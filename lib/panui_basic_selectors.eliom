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
  open Panograph_prereq
  open Panograph_types
  open Panui_content
  open Unprime
]
[%%client
  open Pandom_interactive
]

[%%client

  let string_of_string_option = string_of_option ident
  let string_option_of_string = option_of_string ident
  let string_of_bool_option = string_of_option string_of_bool
  let bool_option_of_string = option_of_string bool_of_string
  let string_of_int_option = string_of_option string_of_int
  let int_option_of_string = option_of_string int_of_string
  let string_of_int32_option = string_of_option Int32.to_string
  let int32_option_of_string = option_of_string Int32.of_string
  let string_of_int64_option = string_of_option Int64.to_string
  let int64_option_of_string = option_of_string Int64.of_string
]

[%%shared
  let string_option_selector ?a ~selection ?(value : string option option)
      (emit : (string option -> unit Panui_result.t Lwt.t) Eliom_client_value.t) =
    let elem =
      D.select ?a (selection : string option Selection.t :> _ elt list) in
    let absorb : (string option -> unit) Eliom_client_value.t = [%client
      outfit_select
        ~to_string:string_of_string_option
        ~of_string:string_option_of_string
        ?value:~%value ~%(elem : [`Select] elt) ~%emit
    ] in
    elem, absorb

  let bool_option_selector ?a ~selection ?(value : bool option option)
      (emit : (bool option -> unit Panui_result.t Lwt.t) Eliom_client_value.t) =
    let elem =
      D.select ?a (selection : bool option Selection.t :> _ elt list) in
    let absorb : (bool option -> unit) Eliom_client_value.t = [%client
      outfit_select
        ~to_string:string_of_bool_option
        ~of_string:bool_option_of_string
        ?value:~%value ~%(elem : [`Select] elt) ~%emit
    ] in
    elem, absorb

  let int_option_selector ?a ~selection ?(value : int option option)
      (emit : (int option -> unit Panui_result.t Lwt.t) Eliom_client_value.t) =
    let elem =
      D.select ?a (selection : int option Selection.t :> _ elt list) in
    let absorb : (int option -> unit) Eliom_client_value.t = [%client
      outfit_select
        ~to_string:string_of_int_option
        ~of_string:int_option_of_string
        ?value:~%value ~%(elem : [`Select] elt) ~%emit
    ] in
    elem, absorb

  let int32_option_selector ?a ~selection ?(value : int32 option option)
      (emit : (int32 option -> unit Panui_result.t Lwt.t) Eliom_client_value.t) =
    let elem =
      D.select ?a (selection : int32 option Selection.t :> _ elt list) in
    let absorb : (int32 option -> unit) Eliom_client_value.t = [%client
      outfit_select
        ~to_string:string_of_int32_option
        ~of_string:int32_option_of_string
        ?value:~%value ~%(elem : [`Select] elt) ~%emit
    ] in
    elem, absorb

  let int64_option_selector ?a ~selection ?(value : int64 option option)
      (emit : (int64 option -> unit Panui_result.t Lwt.t) Eliom_client_value.t) =
    let elem =
      D.select ?a (selection : int64 option Selection.t :> _ elt list) in
    let absorb : (int64 option -> unit) Eliom_client_value.t = [%client
      outfit_select
        ~to_string:string_of_int64_option
        ~of_string:int64_option_of_string
        ?value:~%value ~%(elem : [`Select] elt) ~%emit
    ] in
    elem, absorb
]
