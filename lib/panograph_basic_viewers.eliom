(* Copyright (C) 2015  Petter Urkedal <paurkedal@gmail.com>
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

{shared{
  open Eliom_content.Html5
  open Eliom_lib
  open Unprime_option
}}

{client{
  let with_setter elem (to_string : 'a -> string) (value : 'a option) =
    let set x = Manip.replaceChildren elem [D.pcdata (to_string x)] in
    Option.iter set value; set
}}

{shared{
  let string_viewer ?a ~(to_string : (string -> string) client_value)
		    ?(value : string option) () =
    let span = D.span ?a [] in
    let g = {string -> unit{with_setter %span %to_string %value}} in
    span, g

  let int_viewer ?a ~(to_string : (int -> string) client_value)
		 ?(value : int option) () =
    let span = D.span ?a [] in
    let g = {int -> unit{with_setter %span %to_string %value}} in
    span, g

  let float_viewer ?a ~(to_string : (float -> string) client_value)
		   ?(value : float option) () =
    let span = D.span ?a [] in
    let g = {float -> unit{with_setter %span %to_string %value}} in
    span, g

  let string_option_viewer
	?a ~(to_string : (string option -> string) client_value)
	?(value : string option option) () =
    let span = D.span ?a [] in
    let g = {string option -> unit{with_setter %span %to_string %value}} in
    span, g

  let int_option_viewer
	?a ~(to_string : (int option -> string) client_value)
	?(value : int option option) () =
    let span = D.span ?a [] in
    let g = {int option -> unit{with_setter %span %to_string %value}} in
    span, g

  let float_option_viewer
	?a ~(to_string : (float option -> string) client_value)
	?(value : float option option) () =
    let span = D.span ?a [] in
    let g = {float option -> unit{with_setter %span %to_string %value}} in
    span, g
}}
