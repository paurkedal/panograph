(* Copyright (C) 2015--2017  Petter A. Urkedal <paurkedal@gmail.com>
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

open Eliom_content.Html
open Panui_completion
open Panograph_prereq

[%%shared
  [@@@ocaml.warning "-3"] (* Cf panui_completion.eliom *)
  open Panograph_types
  open Unprime_char
  open Unprime_string
]

[%%client
  module Dep0 = Panui_completion

  let rec diff = function
    | [_] -> []
    | x :: x' :: xs -> (x' - x) :: diff (x' :: xs)
    | [] -> raise Not_found

  let rec quot = function
    | [_] -> []
    | x :: x' :: xs when x <> 0 && x' mod x = 0 -> (x' / x) :: quot (x' :: xs)
    | _ -> raise Not_found

  let rec predict = function
    | [] -> []
    | x :: xs when List.for_all ((=) x) xs -> [x]
    | xs ->
      let ys0 = try predict (diff xs) with Not_found -> [] in
      let ys1 = try predict (quot xs) with Not_found -> [] in
      let x = List.hd (List.rev xs) in
      List.sort_uniq compare (List.map ((+) x) ys0 @ List.map (( * ) x) ys1)

  let state, set_state = React.S.create "1 2 4"

  let int_list_of_string s =
    let xs = String.chop_consecutive Char.is_space s in
    List.map int_of_string xs

  let string_of_int_list zs =
    String.concat " " (List.map string_of_int zs)

  let commit s =
    Lwt_js.sleep 1.0 >>
    try ignore (int_list_of_string s); set_state s; Lwt.return (Ok ())
    with Failure msg -> Lwt.return (Error ("Invalid: " ^ msg))

  let fetch s =
    let xs = int_list_of_string s in
    let ys = predict xs in
    let zss = List.map (fun y -> xs @ [y]) ys in
    Lwt_js.sleep 1.0 >>
    Lwt.return (Ok (List.map string_of_int_list zss))
]

let handler () () =
  let value = "1 2 4" in
  let elem, absorb =
    string_completion_input ~value [%client fetch] [%client commit] in
  ignore_cv [%client  Lwt_react.S.keep (React.S.trace ~%absorb state) ];
  Lwt.return [D.p [elem]]
