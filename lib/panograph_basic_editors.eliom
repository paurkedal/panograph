(* Copyright (C) 2014--2016  Petter A. Urkedal <paurkedal@gmail.com>
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
  open Panograph_prereq
  open Panograph_types
  open Unprime
  open Unprime_option
  open Unprime_string
}}

{client{
  open Pandom_interactive
  open Panograph_common

  let string_of_bool_option = function
    | None -> ""
    | Some false -> "false"
    | Some true -> "true"

  let bool_option_of_string = function
    | "false" -> Some false
    | "true" -> Some true
    | "" -> None
    | _ -> invalid_arg "bool_option_of_string"

  let string_of_int_option = string_of_option string_of_int
  let int_option_of_string = option_of_string int_of_string
  let string_of_int32_option = string_of_option Int32.to_string
  let int32_option_of_string = option_of_string Int32.of_string
  let string_of_int64_option = string_of_option Int64.to_string
  let int64_option_of_string = option_of_string Int64.of_string
}}

{shared{
  open Html5

  let string_ident_cv = {string -> string{ident}}

  let string_editor ?(a = [])
                    ?(to_string = string_ident_cv)
                    ?(of_string = string_ident_cv)
                    ?(value : string option)
                    (patch_out : (string -> ack Lwt.t) client_value) =
    let input = D.Raw.input ~a:(D.a_input_type `Text :: a) () in
    let patch_in = {string -> unit{
      outfit_input ~to_string:%to_string ~of_string:%of_string
                   ?value:%value %input %patch_out
    }} in
    input, patch_in

  let int_editor ?(a = [])
                 ?(to_string = {int -> string{string_of_int}})
                 ?(of_string = {string -> int{int_of_string}})
                 ?(value : int option)
                 (patch_out : (int -> ack Lwt.t) client_value) =
    let input = D.Raw.input ~a:(D.a_input_type `Text :: a) () in
    let patch_in = {int -> unit{
      outfit_input ~to_string:%to_string ~of_string:%of_string
                   ?value:%value %input %patch_out
    }} in
    input, patch_in

  let int32_editor ?(a = [])
                   ?(to_string = {int32 -> string{Int32.to_string}})
                   ?(of_string = {string -> int32{Int32.of_string}})
                   ?(value : int32 option)
                 (patch_out : (int32 -> ack Lwt.t) client_value) =
    let input = D.Raw.input ~a:(D.a_input_type `Text :: a) () in
    let patch_in = {int32 -> unit{
      outfit_input ~to_string:%to_string ~of_string:%of_string
                   ?value:%value %input %patch_out
    }} in
    input, patch_in

  let int64_editor ?(a = [])
                   ?(to_string = {int64 -> string{Int64.to_string}})
                   ?(of_string = {string -> int64{Int64.of_string}})
                   ?(value : int64 option)
                 (patch_out : (int64 -> ack Lwt.t) client_value) =
    let input = D.Raw.input ~a:(D.a_input_type `Text :: a) () in
    let patch_in = {int64 -> unit{
      outfit_input ~to_string:%to_string ~of_string:%of_string
                   ?value:%value %input %patch_out
    }} in
    input, patch_in

  let float_editor ?(a = [])
                   ?(to_string = {float -> string{string_of_float}})
                   ?(of_string = {string -> float{float_of_string}})
                   ?(value : float option)
                   (patch_out : (float -> ack Lwt.t) client_value) =
    let input = D.Raw.input ~a:(D.a_input_type `Text :: a) () in
    let patch_in = {float -> unit{
      outfit_input ~to_string:%to_string ~of_string:%of_string
                   ?value:%value %input %patch_out
    }} in
    input, patch_in

  let string_option_editor
        ?(a = [])
        ?(to_string = string_ident_cv)
        ?(of_string = string_ident_cv)
        ?(value : string option option)
        (patch_out : (string option -> ack Lwt.t) client_value) =
    let input = D.Raw.input ~a:(D.a_input_type `Text :: a) () in
    let patch_in = {string option -> unit{
      let to_string = string_of_option %to_string in
      let of_string = option_of_string %of_string in
      outfit_input ~to_string ~of_string ?value:%value %input %patch_out
    }} in
    input, patch_in

  let string_option_textarea
        ?a
        ?(to_string = string_ident_cv)
        ?(of_string = string_ident_cv)
        ?(value : string option option)
        (patch_out : (string option -> ack Lwt.t) client_value) =
    let input = D.Raw.textarea ?a (D.pcdata "") in
    let patch_in = {string option -> unit{
      let to_string = string_of_option %to_string in
      let of_string = option_of_string %of_string in
      outfit_textarea ~to_string ~of_string ?value:%value %input %patch_out
    }} in
    input, patch_in

  let int_option_editor
        ?(a = [])
        ?(to_string = {int -> string{string_of_int}})
        ?(of_string = {string -> int{int_of_string}})
        ?(value : int option option)
        (patch_out : (int option -> ack Lwt.t) client_value) =
    let input = D.Raw.input ~a:(D.a_input_type `Text :: a) () in
    let patch_in = {int option -> unit{
      let to_string = string_of_option %to_string in
      let of_string = option_of_string %of_string in
      outfit_input ~to_string ~of_string ?value:%value %input %patch_out
    }} in
    input, patch_in

  let int32_option_editor
        ?(a = [])
        ?(to_string = {int32 -> string{Int32.to_string}})
        ?(of_string = {string -> int32{Int32.of_string}})
        ?(value : int32 option option)
        (patch_out : (int32 option -> ack Lwt.t) client_value) =
    let input = D.Raw.input ~a:(D.a_input_type `Text :: a) () in
    let patch_in = {int32 option -> unit{
      let to_string = string_of_option %to_string in
      let of_string = option_of_string %of_string in
      outfit_input ~to_string ~of_string ?value:%value %input %patch_out
    }} in
    input, patch_in

  let int64_option_editor
        ?(a = [])
        ?(to_string = {int64 -> string{Int64.to_string}})
        ?(of_string = {string -> int64{Int64.of_string}})
        ?(value : int64 option option)
        (patch_out : (int64 option -> ack Lwt.t) client_value) =
    let input = D.Raw.input ~a:(D.a_input_type `Text :: a) () in
    let patch_in = {int64 option -> unit{
      let to_string = string_of_option %to_string in
      let of_string = option_of_string %of_string in
      outfit_input ~to_string ~of_string ?value:%value %input %patch_out
    }} in
    input, patch_in

  let float_option_editor
        ?(a = [])
        ?(to_string = {float -> string{string_of_float}})
        ?(of_string = {string -> float{float_of_string}})
        ?(value : float option option)
        (patch_out : (float option -> ack Lwt.t) client_value) =
    let input = D.Raw.input ~a:(D.a_input_type `Text :: a) () in
    let patch_in = {float option -> unit{
      let to_string = string_of_option %to_string in
      let of_string = option_of_string %of_string in
      outfit_input ~to_string ~of_string ?value:%value %input %patch_out
    }} in
    input, patch_in

  let bool_option_selector ?a ?none_label ~false_label ~true_label
                           ?(value : bool option option)
                           (emit : (bool option -> ack Lwt.t) client_value) =
    let none_label =
      match none_label with
      | None -> "<" ^ false_label ^ "|" ^ true_label ^ ">"
      | Some label -> label in
    let elem = D.Raw.select ?a [
      D.Raw.option ~a:[D.a_value ""] (D.pcdata none_label);
      D.Raw.option ~a:[D.a_value "false"] (D.pcdata false_label);
      D.Raw.option ~a:[D.a_value "true"] (D.pcdata true_label);
    ] in
    let absorb = {bool option -> unit{
      outfit_select
        ~to_string:string_of_bool_option ~of_string:bool_option_of_string
        ?value:%value %elem %emit
    }} in
    elem, absorb

  let mk_option_selector conv ?a ?none_label ~items () =
    let none_label =
      match none_label with
      | None -> "<none>"
      | Some label -> label in
    let mk_option (value, label, enabled) =
      let s = conv value in
      let a = if enabled then [D.a_value s]
                         else [D.a_value s; D.a_disabled `Disabled] in
      D.Raw.option ~a (D.pcdata label) in
    let mk_optgroup (label_opt, subitems) =
      let suboptions = List.map mk_option subitems in
      match label_opt with
      | None -> suboptions
      | Some label -> [D.Raw.optgroup ~label suboptions] in
    D.Raw.select ?a (D.Raw.option ~a:[D.a_value ""] (D.pcdata none_label) ::
                     List.flatten (List.map mk_optgroup items))

  let int_option_selector ?a ?none_label ~items ?(value : int option option)
                          (emit : (int option -> ack Lwt.t) client_value) =
    let elem = mk_option_selector string_of_int ?a ?none_label ~items () in
    let absorb = {int option -> unit{
      outfit_select
        ~to_string:string_of_int_option
        ~of_string:int_option_of_string
        ?value:%value %elem %emit
    }} in
    elem, absorb

  let int32_option_selector ?a ?none_label ~items ?(value : int32 option option)
                            (emit : (int32 option -> ack Lwt.t) client_value) =
    let elem = mk_option_selector Int32.to_string ?a ?none_label ~items () in
    let absorb = {int32 option -> unit{
      outfit_select
        ~to_string:string_of_int32_option
        ~of_string:int32_option_of_string
        ?value:%value %elem %emit
    }} in
    elem, absorb

  let int64_option_selector ?a ?none_label ~items ?(value : int64 option option)
                            (emit : (int64 option -> ack Lwt.t) client_value) =
    let elem = mk_option_selector Int64.to_string ?a ?none_label ~items () in
    let absorb = {int64 option -> unit{
      outfit_select
        ~to_string:string_of_int64_option
        ~of_string:int64_option_of_string
        ?value:%value %elem %emit
    }} in
    elem, absorb

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

  let bool_checkbox ?(a = []) ?(value = false)
                    (patch_out : (bool -> ack Lwt.t) client_value) =
    let open Html5 in
    let a = if value then D.a_checked `Checked :: a else a in
    let input = D.Raw.input ~a:(D.a_input_type `Checkbox :: a) () in
    let patch_in = {bool -> unit{
      (* TODO: Use Pandom_interactive.outfit_checkbox if this is kept. *)
      let open Html5 in
      let input_dom = To_dom.of_input %input in
      Lwt_js_events.(async @@ fun () ->
        changes input_dom @@ fun _ _ ->
        Pandom_style.set_dirty input_dom;
        match_lwt %patch_out (Js.to_bool input_dom##checked) with
        | Ack_ok ->
          Pandom_style.clear_error input_dom;
          Lwt.return_unit
        | Ack_error msg ->
          Pandom_style.set_error msg input_dom;
          Lwt.return_unit);
      let patch_in v =
        Pandom_style.clear_error input_dom;
        Pandom_style.clear_dirty input_dom;
        input_dom##checked <- Js.bool v in
      patch_in
    }} in
    input, patch_in
}}
