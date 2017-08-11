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

[%%shared
  (* This module is deprecated and ui_result spills into inferred, making it
   * hard to silence the warning selectively. *)
  [@@@ocaml.warning "-3"]

  open Eliom_content.Html
  open Lwt.Infix
  open Panograph_types
  open Printf
  open Unprime
  open Unprime_option
]

let%client string_completion_client input_elem choices_elem value commit fetch =
  let stored_value = ref (Option.get_or "" value) in
  let input_dom = To_dom.of_input input_elem in
  let choices_dom = To_dom.of_span choices_elem in

  let committing_choice = ref false in
  Lwt_js_events.(async @@ fun () -> mousedowns choices_dom @@ fun _ _ ->
    committing_choice := true;
    Lwt_js.sleep 0.01 >|= fun () -> committing_choice := false);

  Lwt_js_events.(async @@ fun () -> blurs input_dom @@ fun _ _ ->
    if not !committing_choice then Pandom_style.set_hidden choices_dom;
    Lwt.return_unit);

  let on_commit v =
    Pandom_style.set_hidden choices_dom;
    if v = !stored_value then begin
      input_dom##.value := Js.string v;
      Lwt.return_unit
    end else begin
      Pandom_style.set_dirty input_dom;
      (match%lwt commit v with
       | Ok () ->
          Pandom_style.clear_error input_dom;
          Lwt.return_unit
       | Error msg ->
          Pandom_style.clear_dirty input_dom;
          Pandom_style.set_error msg input_dom;
          Lwt.return_unit)
    end in

  let make_choice v =
    let choice_elem = D.span ~a:[D.a_class ["pan-choice"]] [D.pcdata v] in
    let choice_dom = To_dom.of_span choice_elem in
    let on_choice_click _ _ = on_commit v in
    Lwt_js_events.(async @@ fun () -> clicks choice_dom on_choice_click);
    choice_elem in

  let update_choices = Pwt.async_updater @@ fun () ->
    (match%lwt fetch (Js.to_string input_dom##.value) with
     | Ok completions ->
        Pandom_style.clear_hidden choices_dom;
        Pandom_style.clear_error input_dom;
        let choices = List.map make_choice completions in
        Manip.replaceChildren choices_elem choices;
        Lwt.return_unit
     | Error msg ->
        Pandom_style.set_error msg input_dom;
        Lwt.return_unit) in

  let on_input_input _ _ = update_choices (); Lwt.return_unit in

  let on_input_change _ _ =
    if !committing_choice then Lwt.return_unit else begin
      Pandom_style.clear_error input_dom;
      on_commit (Js.to_string input_dom##.value)
    end in

  Lwt_js_events.(async @@ fun () -> inputs input_dom on_input_input);
  Lwt_js_events.(async @@ fun () -> changes input_dom on_input_change);

  fun v ->
    Pandom_style.clear_dirty input_dom;
    Pandom_style.clear_error input_dom;
    stored_value := v;
    input_dom##.value := Js.string v

let%shared string_completion_input
    ?(value : string option)
    (fetch : (string -> string list ui_result Lwt.t) Eliom_client_value.t)
    (commit : (string -> unit ui_result Lwt.t) Eliom_client_value.t) =

  let input_elem = D.input ~a:[D.a_input_type `Text] () in
  let choices_elem = D.span ~a:[D.a_class ["pan-choices"]] [] in

  let absorb : (string -> unit) Eliom_client_value.t = [%client
    string_completion_client
      ~%(input_elem : [`Input] elt)
      ~%(choices_elem : [`Span] elt)
      ~%value ~%commit ~%fetch
  ] in

  D.span ~a:[D.a_class ["pan-completion-input"]]
    [input_elem; D.span ~a:[D.a_class ["pan-dropdown"]] [choices_elem]],
  absorb
