(* Copyright (C) 2015--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

[%%shared
  open Eliom_content.Html
  open Eliom_lib
  open Unprime
  open Unprime_option
]

[%%client
  let with_setter elem (to_string : 'a -> string) (value : 'a option) =
    let set x = Manip.replaceChildren elem [D.txt (to_string x)] in
    Option.iter set value; set

  let with_opt_setter elem (to_string : 'a -> string)
                      (value : 'a option option) =
    let set = function
      | None ->
        Manip.Class.add elem "none";
        Manip.removeChildren elem
      | Some x ->
        Manip.Class.remove elem "none";
        Manip.replaceChildren elem [D.txt (to_string x)] in
    Option.iter set value; set
]

[%%shared
  let string_viewer
        ?a
        ?(to_string : (string -> string) Eliom_client_value.t = [%client ident])
        ?(value : string option) () =
    let span = D.span ?a [] in
    let g : (string -> unit) Eliom_client_value.t =
      [%client with_setter ~%(span : [`Span] elt) ~%to_string ~%value] in
    span, g

  let int_viewer
      ?a
      ?(to_string : (int -> string) Eliom_client_value.t = [%client string_of_int])
      ?(value : int option) () =
    let span = D.span ?a [] in
    let g : (int -> unit) Eliom_client_value.t =
      [%client with_setter ~%(span : [`Span] elt) ~%to_string ~%value] in
    span, g

  let int32_viewer
      ?a
      ?(to_string : (int32 -> string) Eliom_client_value.t = [%client Int32.to_string])
      ?(value : int32 option) () =
    let span = D.span ?a [] in
    let g : (int32 -> unit) Eliom_client_value.t =
      [%client with_setter ~%(span : [`Span] elt) ~%to_string ~%value] in
    span, g

  let int64_viewer
      ?a
      ?(to_string : (int64 -> string) Eliom_client_value.t = [%client Int64.to_string])
      ?(value : int64 option) () =
    let span = D.span ?a [] in
    let g : (int64 -> unit) Eliom_client_value.t =
      [%client with_setter ~%(span : [`Span] elt) ~%to_string ~%value] in
    span, g

  let float_viewer
      ?a
      ?(to_string : (float -> string) Eliom_client_value.t = [%client string_of_float])
      ?(value : float option) () =
    let span = D.span ?a [] in
    let g : (float -> unit) Eliom_client_value.t =
      [%client with_setter ~%(span : [`Span] elt) ~%to_string ~%value] in
    span, g

  let string_option_viewer
        ?a
        ?(to_string : (string -> string) Eliom_client_value.t = [%client ident])
        ?(value : string option option) () =
    let span = D.span ?a [] in
    let g : (string option -> unit) Eliom_client_value.t =
      [%client with_opt_setter ~%(span : [`Span] elt) ~%to_string ~%value] in
    span, g

  let int_option_viewer
        ?a
        ?(to_string : (int -> string) Eliom_client_value.t = [%client string_of_int])
        ?(value : int option option) () =
    let span = D.span ?a [] in
    let g : (int option -> unit) Eliom_client_value.t =
      [%client with_opt_setter ~%(span : [`Span] elt) ~%to_string ~%value] in
    span, g

  let int32_option_viewer
        ?a
        ?(to_string : (int32 -> string) Eliom_client_value.t =
            [%client Int32.to_string])
        ?(value : int32 option option) () =
    let span = D.span ?a [] in
    let g : (int32 option -> unit) Eliom_client_value.t =
      [%client with_opt_setter ~%(span : [`Span] elt) ~%to_string ~%value] in
    span, g

  let int64_option_viewer
        ?a
        ?(to_string : (int64 -> string) Eliom_client_value.t =
            [%client Int64.to_string])
        ?(value : int64 option option) () =
    let span = D.span ?a [] in
    let g : (int64 option -> unit) Eliom_client_value.t =
      [%client with_opt_setter ~%(span : [`Span] elt) ~%to_string ~%value] in
    span, g

  let float_option_viewer
        ?a
        ?(to_string : (float -> string) Eliom_client_value.t = [%client string_of_float])
        ?(value : float option option) () =
    let span = D.span ?a [] in
    let g : (float option -> unit) Eliom_client_value.t =
      [%client with_opt_setter ~%(span : [`Span] elt) ~%to_string ~%value] in
    span, g
]
