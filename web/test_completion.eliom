(* Copyright (C) 2015  Petter A. Urkedal <paurkedal@gmail.com>
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

open Eliom_content.Html5
open Panui_completion

{shared{
  open Panograph_types
  open Unprime_char
  open Unprime_string
}}

{client{
  module Dep0 = Panui_completion

  let rec diff = function
    | [_] -> []
    | x :: x' :: xs -> (x' - x) :: diff (x' :: xs)
    | [] -> raise Not_found

  let rec divf = function
    | [_] -> []
    | x :: x' :: xs when x <> 0 && x' mod x = 0 -> (x' / x) :: divf (x' :: xs)
    | _ -> raise Not_found

  let rec predict = function
    | [] -> []
    | x :: (_ :: _ as xs) when List.for_all ((=) x) xs -> [x]
    | xs ->
      let ys0 = try predict (diff xs) with Not_found -> [] in
      let ys1 = try predict (divf xs) with Not_found -> [] in
      let x = List.hd (List.rev xs) in
      List.map ((+) x) ys0 @ List.map (( * ) x) ys1
}}

let fetch = {{fun s ->
  let xs = String.chop_consecutive Char.is_space s in
  let xs = List.map int_of_string xs in
  let ys = predict xs in
  let zss = List.map (fun y -> xs @ [y]) ys in
  Lwt.return
    (List.map (fun zs -> String.concat " " (List.map string_of_int zs)) zss)
}}

let commit = {{fun s ->
  Lwt.return Ack_ok
}}

let render () =
  let value = "1 2 4" in
  let elem, absorb = string_completion_input ~value fetch commit in
  D.p [elem]
