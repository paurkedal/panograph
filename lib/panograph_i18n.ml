(* Copyright (C) 2014--2022  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the LGPL-3.0 Linking Exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * and the LGPL-3.0 Linking Exception along with this library.  If not, see
 * <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.
 *)

open Printf
open Unprime_list
open Unprime_string

let invalid_arg_f fmt = ksprintf invalid_arg fmt

type lang = Iso639.Lang.t

module Json_lang = Deriving_Json.Convert (struct
  type a = string
  type b = lang
  let t = Deriving_Json.Json_string.t
  let from_ = Iso639.Lang.of_string_exn
  let to_ = Iso639.Lang.to_string
end)
let lang_json = Json_lang.t
let lang_to_json = Json_lang.write
let lang_of_json = Json_lang.read

module Lang = struct

  type t = lang

  let of_string_opt s =
    (match String.length s with
     | 2 -> Iso639.Lang.of_iso639p1 s
     | 3 -> Iso639.Lang.of_string s
     | _ -> None)

  let of_string s =
    (match of_string_opt s with
     | Some lang -> lang
     | None ->
        invalid_arg_f "lang_of_string: %s is not an ISO 639 language code." s)

  let to_string lang =
    (match Iso639.Lang.to_iso639p1 lang with
     | Some s -> s
     | None -> Iso639.Lang.to_string lang)

  let equal = Iso639.Lang.equal
  let compare = Iso639.Lang.compare
end

module Lang_map = Prime_enummap.Make (Lang)

type twine = string Lang_map.t

module Twine = struct
  type t = twine

  let make lms =
    List.fold (fun (lang, msg) -> Lang_map.add lang msg) lms Lang_map.empty

  let equal = Lang_map.equal (=)

  let compare = Lang_map.compare String.compare

  let rec to_string ~langs tw =
    match langs with
    | [] -> raise Not_found
    | lang :: langs -> try Lang_map.find lang tw
                       with Not_found -> to_string ~langs tw

  type sym_patch = string Lang_map.t * string Lang_map.t
                 * (string * string) Lang_map.t

  let sym_diff = Lang_map.split_union (fun _ a b -> a, b)

  let sym_patch_theirs (mR, mA, mM) tw =
    tw |> Lang_map.fold (fun l _ -> Lang_map.remove l) mR
       |> Lang_map.fold (fun l (_, s) -> Lang_map.add l s) mM
       |> Lang_map.fold (fun l s -> Lang_map.add l s) mA

  let sym_patch_ours (mR, mA, mM) tw =
    let remove l s m =
      try if Lang_map.find l m = s then Lang_map.remove l m else m
      with Not_found -> m in
    let modify l (sR, s) m =
      if try Lang_map.find l m = sR with Not_found -> true
      then Lang_map.add l s m else m in
    let add l s m =
      if Lang_map.mem l m then m else Lang_map.add l s m in
    tw |> Lang_map.fold remove mR
       |> Lang_map.fold modify mM
       |> Lang_map.fold add mA

  let sym_patch ?(strategy = `Theirs) =
    match strategy with
    | `Theirs -> sym_patch_theirs
    | `Ours -> sym_patch_ours
end

let check_utf8_exn =
  let check_code_point char_pos byte_pos = function
   | `Uchar c ->
      let fail what =
        ksprintf failwith
          "Unicode %s code point U+%04x at position %d is not allowed."
          what (Uchar.to_int c) char_pos
      in
      (match Uchar.to_int c with
       | 0x0009 | 0x000a | 0x000d -> char_pos + 1
       | 0xfffc -> fail "object replacement character"
       | 0xfffd -> fail "replacement character"
       | _ ->
          (match Uucp.Gc.general_category c with
           | `Cc -> fail "control"
           | `Cf -> fail "format"
           | `Cs -> fail "surrogate"
           | `Co -> fail "private use"
           | `Cn -> fail "unassigned"
           | _ -> char_pos + 1))
   | `Malformed _ ->
      ksprintf failwith "Malformed UTF-8 sequence starting at byte %d." byte_pos
  in
  fun s -> ignore (Uutf.String.fold_utf_8 check_code_point 0 s)
