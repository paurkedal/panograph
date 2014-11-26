(* Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Printf
open Unprime
open Unprime_char
open Unprime_list
open Unprime_string

let invalid_arg_f fmt = ksprintf invalid_arg fmt

type lang = int deriving (Json)

module Lang = struct

  type t = lang

  let of_int = ident
  let to_int = ident

  let of_string s =
    let int_of_letter c = Char.code (Char.lowercase c) - 0x60 in
    let len = String.length s in
    if len < 2 || len > 4 || not (String.for_all Char.is_alpha s) then
      invalid_arg_f "lang_of_string: %s is not an ISO 639 language code." s;
    let _, lang =
      String.fold (fun c (k, lang) -> (k - 6, lang lor int_of_letter c lsl k))
		  s (18, 0) in
    lang

  let to_string lang =
    let letter_of_int i = Char.chr (i + 0x60) in
    let len =
      if lang land 0xfff = 0 then if lang land 0x03ffff = 0 then 1 else 2
			     else if lang land 0x00003f = 0 then 3 else 4 in
    String.sample (fun i -> letter_of_int (lang lsr (18 - 6 * i) land 0x3f)) len

  let equal = (=)
  let compare = compare
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
    tw |> Lang_map.fold (fun l s -> Lang_map.remove l) mR
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
      if Lang_map.contains l m then m else Lang_map.add l s m in
    tw |> Lang_map.fold remove mR
       |> Lang_map.fold modify mM
       |> Lang_map.fold add mA

  let sym_patch ?(strategy = `Theirs) =
    match strategy with
    | `Theirs -> sym_patch_theirs
    | `Ours -> sym_patch_ours
end
