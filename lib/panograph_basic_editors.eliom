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

{shared{
  open Eliom_content
  open Panograph_types
  open Unprime
  open Unprime_option
}}

{client{
  let outfit_interactive ~to_string ~of_string ?value input_dom patch_out =
    let saved_title = ref (Js.to_string input_dom##title) in
    Lwt_js_events.(async @@ fun () ->
      changes input_dom @@ fun _ _ ->
      input_dom##classList##add(Js.string "dirty");
      match_lwt
	try patch_out (of_string (Js.to_string input_dom##value))
	with Invalid_argument _ | Failure _ ->
	  Lwt.return (Ack_error "Invalid input.")
      with
      | Ack_ok ->
	input_dom##classList##remove(Js.string "error");
	Lwt.return_unit
      | Ack_error msg ->
	saved_title := Js.to_string input_dom##title;
	input_dom##classList##add(Js.string "error");
	input_dom##title <- Js.string ("Error: " ^ msg ^ "\n" ^ !saved_title);
	Lwt.return_unit);
    let patch_in v =
      input_dom##classList##remove(Js.string "dirty");
      if Js.to_bool (input_dom##classList##contains(Js.string "error")) then
      begin
	input_dom##classList##remove(Js.string "error");
	input_dom##title <- Js.string !saved_title
      end;
      input_dom##value <- Js.string (to_string v) in
    Option.iter patch_in value;
    patch_in

  let outfit_input_editor ~to_string ~of_string ?value input patch_out =
    outfit_interactive ~to_string ~of_string ?value
		       (Html5.To_dom.of_input input) patch_out

  let outfit_select ~to_string ~of_string ?value select patch_out =
    outfit_interactive ~to_string ~of_string ?value
		       (Html5.To_dom.of_select select) patch_out
}}

{shared{
  open Html5

  let string_editor ?a ?(value : string option)
		    (patch_out : (string -> ack Lwt.t) client_value) =
    let input = D.input ~input_type:`Text ?a () in
    let patch_in = {string -> unit{
      outfit_input_editor ~to_string:ident ~of_string:ident
			  ?value:%value %input %patch_out
    }} in
    input, patch_in

  let int_editor ?a ?(value : int option)
		 (patch_out : (int -> ack Lwt.t) client_value) =
    let input = D.input ~input_type:`Text ?a () in
    let patch_in = {int -> unit{
      outfit_input_editor ~to_string:string_of_int ~of_string:int_of_string
			  ?value:%value %input %patch_out
    }} in
    input, patch_in

  let float_editor ?a ?(value : float option)
		   (patch_out : (float -> ack Lwt.t) client_value) =
    let input = D.input ~input_type:`Text ?a () in
    let patch_in = {float -> unit{
      outfit_input_editor ~to_string:string_of_float ~of_string:float_of_string
			  ?value:%value %input %patch_out
    }} in
    input, patch_in

  let string_option_editor
	?a ?(value : string option option)
	(patch_out : (string option -> ack Lwt.t) client_value) =
    let input = D.input ~input_type:`Text ?a () in
    let patch_in = {string option -> unit{
      let to_string = function None -> "" | Some x -> x in
      let of_string = function "" -> None | x -> Some x in
      outfit_input_editor ~to_string ~of_string ?value:%value %input %patch_out
    }} in
    input, patch_in

  let int_option_editor
	?a ?(value : int option option)
	(patch_out : (int option -> ack Lwt.t) client_value) =
    let input = D.input ~input_type:`Text ?a () in
    let patch_in = {int option -> unit{
      let to_string = function None -> "" | Some x -> string_of_int x in
      let of_string = function "" -> None | x -> Some (int_of_string x) in
      outfit_input_editor ~to_string ~of_string ?value:%value %input %patch_out
    }} in
    input, patch_in

  let float_option_editor
	?a ?(value : float option option)
	(patch_out : (float option -> ack Lwt.t) client_value) =
    let input = D.input ~input_type:`Text ?a () in
    let patch_in = {float option -> unit{
      let to_string = function None -> "" | Some x -> string_of_float x in
      let of_string = function "" -> None | x -> Some (float_of_string x) in
      outfit_input_editor ~to_string ~of_string ?value:%value %input %patch_out
    }} in
    input, patch_in

  let string_option_menu ?a ~values ?(value : string option = None)
	(patch_out : (string option -> ack Lwt.t) client_value) =
    let make_option label = D.option ~a:[D.a_value label] (D.pcdata label) in
    let options = D.option ~a:[D.a_value "__none__"] (D.pcdata "-") ::
		  List.map make_option values in
    let select = D.Raw.select ?a options in
    let patch_in = {string option -> unit{
      let of_string = function "__none__" -> None | s -> Some s in
      let to_string = function None -> "__none__" | Some s -> s in
      outfit_select ~to_string ~of_string ~value:%value %select %patch_out
    }} in
    select, patch_in
}}
