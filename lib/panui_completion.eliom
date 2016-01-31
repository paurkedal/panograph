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

{shared{
  open Eliom_content.Html5
  open Lwt.Infix
  open Panograph_types
  open Printf
  open Unprime
  open Unprime_option
}}

{client{
  let string_completion_client input_elem choices_elem value commit fetch =
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
	input_dom##value <- Js.string v;
	Lwt.return_unit
      end else begin
	Pandom_style.set_dirty input_dom;
	match_lwt commit v with
	| Ack_ok ->
	  Pandom_style.clear_error input_dom;
	  Lwt.return_unit
	| Ack_error msg ->
	  Pandom_style.clear_dirty input_dom;
	  Pandom_style.set_error msg input_dom;
	  Lwt.return_unit
      end in

    let make_choice v =
      let choice_elem = D.span ~a:[D.a_class ["pan-choice"]] [D.pcdata v] in
      let choice_dom = To_dom.of_span choice_elem in
      let on_choice_click _ _ = on_commit v in
      Lwt_js_events.(async @@ fun () -> clicks choice_dom on_choice_click);
      choice_elem in

    let update_choices = Pwt.async_updater @@ fun () ->
      try_lwt
	lwt completions = fetch (Js.to_string input_dom##value) in
	Pandom_style.clear_hidden choices_dom;
	Pandom_style.clear_error input_dom;
	let choices = List.map make_choice completions in
	Manip.replaceChildren choices_elem choices;
	Lwt.return_unit
      with Eliom_lib.Exception_on_server _ ->
	Pandom_style.set_error "Cannot provide completions for this input."
			       input_dom;
	Lwt.return_unit in

    let on_input_input _ _ = update_choices (); Lwt.return_unit in

    let on_input_change _ _ =
      if !committing_choice then Lwt.return_unit else begin
	Pandom_style.clear_error input_dom;
	on_commit (Js.to_string input_dom##value)
      end in

    Lwt_js_events.(async @@ fun () -> inputs input_dom on_input_input);
    Lwt_js_events.(async @@ fun () -> changes input_dom on_input_change);

    fun v ->
      Pandom_style.clear_dirty input_dom;
      Pandom_style.clear_error input_dom;
      stored_value := v;
      input_dom##value <- Js.string v
}}

{shared{
  let string_completion_input
	?(value : string option)
	(fetch : (string -> string list Lwt.t) client_value)
	(commit : (string -> ack Lwt.t) client_value) =

    let input_elem = D.Raw.input ~a:[D.a_input_type `Text] () in
    let choices_elem = D.span ~a:[D.a_class ["pan-choices"]] [] in

    let absorb = {string -> unit{
      string_completion_client %input_elem %choices_elem %value %commit %fetch
    }} in

    D.span ~a:[D.a_class ["pan-completion-input"]]
      [input_elem; D.span ~a:[D.a_class ["pan-dropdown"]] [choices_elem]],
    absorb
}}
