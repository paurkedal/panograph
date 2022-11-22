(* Copyright (C) 2017--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

[%%shared.start]
open Eliom_content.Html

[%%client.start]
open Js_of_ocaml_lwt
open Lwt.Infix
open Unprime_char
open Unprime_string

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

let emit to_string elem value =
  Manip.removeChildren elem;
  Manip.appendChild elem (D.span [D.txt (to_string value)]);
  Lwt.return (Ok ())

let string_complete pfx =
  Lwt_js.sleep 0.5 >|= fun () ->
  if String.exists (fun c -> not (Char.is_ascii_alpha c)) pfx
  then (Panui_result.invalid_input "non-alpha")
  else (Ok (List.filter (String.has_prefix pfx) (List.map fst choices)))

let labelled_int_complete pfx =
  Lwt_js.sleep 0.5 >|= fun () ->
  if String.exists (fun c -> not (Char.is_ascii_alpha c)) pfx
  then (Panui_result.invalid_input "non-alpha")
  else (Ok (List.filter (fun (s, _) -> String.has_prefix pfx s) choices))

let labelled_int32_complete pfx =
  Lwt_js.sleep 0.5 >|= fun () ->
  if String.exists (fun c -> not (Char.is_ascii_alpha c)) pfx then
    (Panui_result.invalid_input "non-alpha") else
  let choices =
    if pfx = "" then [] else
    choices
      |> List.filter (fun (s, _) -> String.has_prefix pfx s)
      |> List.map (fun (s, v) -> (s, Int32.of_int v)) in
  Ok choices

[%%server.start]

let get_handler arg () =
  let result =
    (match arg with
     | None -> "unspecified"
     | Some arg -> Int32.to_string arg) in
  Lwt.return [F.p [F.txt "Post result: "; F.txt result]]

let get_service =
  let open Eliom_service in
  Test_app.create_page
    ~path:(Path ["complete_get"])
    ~meth:(Get Eliom_parameter.(opt (int32 "arg")))
    ~title:"Get Result"
    get_handler

let handler () () =

  let string_option_log_elem = D.span [] in
  let string_option_elem, _ =
    Panui_complete.string_option
      ~has_feedback:false
      ~complete:[%client string_complete]
      ~emit:[%client emit [%show: string option] ~%string_option_log_elem]
      None in

  let labelled_int_log_elem = D.span [] in
  let labelled_int_elem, _ =
    Panui_complete.labelled_int
      ~has_feedback:false
      ~complete:[%client labelled_int_complete]
      ~emit:[%client emit [%show: string * int] ~%labelled_int_log_elem]
      ("zero", 0) in

  let labelled_int_option_log_elem = D.span [] in
  let labelled_int_option_elem, _ =
    Panui_complete.labelled_int_option
      ~has_feedback:false
      ~complete:[%client labelled_int_complete]
      ~emit:[%client
        emit [%show: (string * int) option] ~%labelled_int_option_log_elem]
      None in

  Lwt.return [
    D.p [
      string_option_elem; string_option_log_elem; D.br ();
      labelled_int_elem; labelled_int_log_elem; D.br ();
      labelled_int_option_elem; labelled_int_option_log_elem
    ];
    D.Form.get_form ~service:get_service
      (fun arg_name ->
        let elt, _h =
          Panui_complete.labelled_int32_option
            ~complete:[%client labelled_int32_complete]
            ~name:arg_name (Some ("one", 1l)) in
        [elt; D.Form.input ~input_type:`Submit D.Form.string]);
  ]
