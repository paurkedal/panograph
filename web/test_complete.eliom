(* Copyright (C) 2017  Petter A. Urkedal <paurkedal@gmail.com>
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

[%%shared.start]
open Eliom_content.Html
open Panograph_types

[%%client.start]
open Lwt.Infix
open Unprime_char
open Unprime_string
open Printf

module Dep = Panui_complete

let rec ordinal = function
 | 0 -> "zero"
 | 1 -> "one"
 | 2 -> "two"
 | 3 -> "three"
 | 4 -> "four"
 | 5 -> "five"
 | 6 -> "six"
 | 7 -> "seven"
 | 8 -> "eight"
 | 9 -> "nine"
 | 10 -> "ten"
 | 11 -> "eleven"
 | 12 -> "twelve"
 | 13 -> "thirteen"
 | 14 -> "fourteen"
 | 15 -> "fifteen"
 | 16 -> "sixteen"
 | 17 -> "seventeen"
 | 18 -> "eighteen"
 | 19 -> "nineteen"
 | 20 -> "twenty"
 | 30 -> "thirty"
 | 40 -> "forty"
 | 50 -> "fifty"
 | 60 -> "sixty"
 | 70 -> "seventy"
 | 80 -> "eighty"
 | 90 -> "ninety"
 | x when 20 <= x && x < 99 -> ordinal (x / 10 * 10) ^ ordinal (x mod 10)
 | _ -> "many"

let choices = Prime_list.sample (fun i -> (ordinal i, i)) 100

let complete pfx =
  Lwt_js.sleep 0.5 >|= fun () ->
  if String.exists (fun c -> not (Char.is_alpha c)) pfx
  then (Panui_result.invalid_input "non-alpha")
  else (Ok (List.filter (fun (s, _) -> String.has_prefix pfx s) choices))

let emit elem (label, value) =
  Manip.removeChildren elem;
  Manip.appendChild elem (D.span [D.pcdata (sprintf "%S, %d" label value)]);
  Lwt.return (Ok ())

let complete_opt pfx =
  Lwt_js.sleep 0.5 >|= fun () ->
  if String.exists (fun c -> not (Char.is_alpha c)) pfx
    then (Panui_result.invalid_input "non-alpha") else
  let choices =
    if pfx = "" then
      ["", None]
    else
      choices
        |> List.filter (fun (s, _) -> String.has_prefix pfx s)
        |> List.map (fun (s, v) -> (s, Some v)) in
  Ok choices

let complete_int32_opt pfx =
  Lwt_js.sleep 0.5 >|= fun () ->
  if String.exists (fun c -> not (Char.is_alpha c)) pfx
    then (Panui_result.invalid_input "non-alpha") else
  let choices =
    if pfx = "" then
      []
    else
      choices
        |> List.filter (fun (s, _) -> String.has_prefix pfx s)
        |> List.map (fun (s, v) -> (s, Int32.of_int v)) in
  Ok choices

let emit_opt elem choice =
  Manip.removeChildren elem;
  let text =
    (match choice with
     | Some (label, value) -> sprintf "%S, %d" label value
     | None -> "(none)") in
  Manip.appendChild elem (D.span [D.pcdata text]);
  Lwt.return (Ok ())

[%%server.start]

let get_handler arg () =
  let result =
    (match arg with
     | None -> "unspecified"
     | Some arg -> Int32.to_string arg) in
  Lwt.return [F.p [F.pcdata "Post result: "; F.pcdata result]]

let get_service =
  let open Eliom_service in
  Test_app.create_page
    ~path:(Path ["complete_get"])
    ~meth:(Get Eliom_parameter.(opt (int32 "arg")))
    ~title:"Get Result"
    get_handler

let handler () () =
  let log_elem = D.span [] in
  let log_elem_opt = D.span [] in
  let elem, _ =
    Panui_complete.labelled_int
      ~has_feedback:false
      ~complete:[%client complete]
      ~emit:[%client emit ~%log_elem] ("zero", 0) in
  let elem_opt, _ =
    Panui_complete.labelled_int_option
      ~has_feedback:false
      ~complete:[%client complete]
      ~emit:[%client emit_opt ~%log_elem_opt] None in
  Lwt.return [
    D.p [
      elem; log_elem; D.br ();
      elem_opt; log_elem_opt
    ];
    D.Form.get_form get_service
      (fun arg_name ->
        let elt, h =
          Panui_complete.labelled_int32_option
            ~complete:[%client complete_int32_opt]
            ~name:arg_name (Some ("one", 1l)) in
        [elt; D.Form.input ~input_type:`Submit D.Form.string]);
  ]
