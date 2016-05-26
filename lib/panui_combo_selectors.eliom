(* Copyright (C) 2015--2016  Petter A. Urkedal <paurkedal@gmail.com>
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

[%%shared
  open Eliom_content.Html5
  open Panograph_basic_editors
  open Panograph_types
  open Panui_basic_selectors
  open Unprime
]

[%%client
  let combo_absorb elem inl_absorb inr_absorb = function
    | None ->
      Manip.Class.add elem "other";
      inl_absorb None; inr_absorb None
    | Some (Inl x) ->
      Manip.Class.remove elem "other";
      inl_absorb (Some x); inr_absorb None
    | Some (Inr x) ->
      Manip.Class.add elem "other";
      inl_absorb None; inr_absorb (Some x)
]

[%%shared
  let int_string_option_combo_selector
        ?(a = [])
        ?inl_a
        ?inr_a
        ~inl_selection
        ?inr_to_string
        ?inr_of_string
        ?(value : (int, string) either option = None)
        emit =
    let inl_value = match value with Some (Inl x) -> Some x | _ -> None in
    let inr_value = match value with Some (Inr x) -> Some x | _ -> None in
    let inl_emit = [%client
      let emit : (int, string) either option -> ack Lwt.t = ~%emit in
      function None -> emit None | Some x -> emit (Some (Inl x))
    ] in
    let inr_emit = [%client
      let emit : (int, string) either option -> ack Lwt.t = ~%emit in
      function None -> emit None | Some x -> emit (Some (Inr x))
    ] in
    let inl_elem, inl_absorb =
      int_option_selector ?a:inl_a ~selection:inl_selection
                            ~value:inl_value inl_emit in
    let inr_elem, inr_absorb =
      string_option_editor ?a:inr_a
                           ?to_string:inr_to_string ?of_string:inr_of_string
                           ~value:inr_value inr_emit in
    let cls_other = if inl_value = None then ["other"] else [] in
    let a = D.a_class ("pan-comboselect" :: cls_other) :: a in
    let elem = D.span ~a [inl_elem; inr_elem] in
    let absorb = [%client combo_absorb ~%elem ~%inl_absorb ~%inr_absorb] in
    elem, absorb

  let int32_string_option_combo_selector
        ?(a = [])
        ?inl_a
        ?inr_a
        ~inl_selection
        ?inr_to_string
        ?inr_of_string
        ?(value : (int32, string) either option = None)
        emit =
    let inl_value = match value with Some (Inl x) -> Some x | _ -> None in
    let inr_value = match value with Some (Inr x) -> Some x | _ -> None in
    let inl_emit = [%client
      let emit : (int32, string) either option -> ack Lwt.t = ~%emit in
      function None -> emit None | Some x -> emit (Some (Inl x))
    ] in
    let inr_emit = [%client
      let emit : (int32, string) either option -> ack Lwt.t = ~%emit in
      function None -> emit None | Some x -> emit (Some (Inr x))
    ] in
    let inl_elem, inl_absorb =
      int32_option_selector ?a:inl_a ~selection:inl_selection
                            ~value:inl_value inl_emit in
    let inr_elem, inr_absorb =
      string_option_editor ?a:inr_a
                           ?to_string:inr_to_string ?of_string:inr_of_string
                           ~value:inr_value inr_emit in
    let cls_other = if inl_value = None then ["other"] else [] in
    let a = D.a_class ("pan-comboselect" :: cls_other) :: a in
    let elem = D.span ~a [inl_elem; inr_elem] in
    let absorb = [%client combo_absorb ~%elem ~%inl_absorb ~%inr_absorb] in
    elem, absorb

  let int64_string_option_combo_selector
        ?(a = [])
        ?inl_a
        ?inr_a
        ~inl_selection
        ?inr_to_string
        ?inr_of_string
        ?(value : (int64, string) either option = None)
        emit =
    let inl_value = match value with Some (Inl x) -> Some x | _ -> None in
    let inr_value = match value with Some (Inr x) -> Some x | _ -> None in
    let inl_emit = [%client
      let emit : (int64, string) either option -> ack Lwt.t = ~%emit in
      function None -> emit None | Some x -> emit (Some (Inl x))
    ] in
    let inr_emit = [%client
      let emit : (int64, string) either option -> ack Lwt.t = ~%emit in
      function None -> emit None | Some x -> emit (Some (Inr x))
    ] in
    let inl_elem, inl_absorb =
      int64_option_selector ?a:inl_a ~selection:inl_selection
                            ~value:inl_value inl_emit in
    let inr_elem, inr_absorb =
      string_option_editor ?a:inr_a
                           ?to_string:inr_to_string ?of_string:inr_of_string
                           ~value:inr_value inr_emit in
    let cls_other = if inl_value = None then ["other"] else [] in
    let a = D.a_class ("pan-comboselect" :: cls_other) :: a in
    let elem = D.span ~a [inl_elem; inr_elem] in
    let absorb = [%client combo_absorb ~%elem ~%inl_absorb ~%inr_absorb] in
    elem, absorb
]
