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
  open Eliom_lib
  open Eliom_content
  open Panograph_i18n
  open Panograph_lib
  open Panograph_lwt

  type twine_editor_out = [`Add of lang * string | `Remove of lang]
			  deriving (Json)
  type twine_editor_in = twine_editor_out

  type twine_editor = {
    twe_container_dom : Dom_html.element Js.t;
    twe_emit : twine_editor_out -> unit;
    mutable twe_map : (Dom_html.element Js.t * Dom_html.inputElement Js.t)
		      Lang_map.t;
  }
}}

{client{
  open Unprime
  open Unprime_option

  let remove_translation twe lang =
    let item_dom, _ = Lang_map.find lang twe.twe_map in
    Dom.removeChild twe.twe_container_dom item_dom;
    twe.twe_map <- Lang_map.remove lang twe.twe_map;
    twe.twe_emit (`Remove lang)

  let add_translation (twe : twine_editor) lang msg =
    let open Html5 in
    try
      let _, inp_dom = Lang_map.find lang twe.twe_map in
      inp_dom##value <- Js.string msg
    with Not_found ->
      let inp = D.input ~input_type:`Text ~value:msg () in
      let inp_dom = To_dom.of_input inp in
      let remove_button = D.button ~button_type:`Button [D.pcdata "−"] in
      let item = D.span [D.span [D.pcdata (Lang.to_string lang)];
			 inp; remove_button] in
      let next = Option.map (fst *< snd)
			    (Lang_map.succ_binding_o twe.twe_map lang) in
      let item_dom = To_dom.of_span item in
      Dom.insertBefore twe.twe_container_dom item_dom (Js.Opt.option next);
      twe.twe_map <- Lang_map.add lang (item_dom, inp_dom) twe.twe_map;
      Lwt_js_events.(async @@ fun () ->
	changes inp_dom @@ fun _ _ ->
	twe.twe_emit (`Add (lang, Js.to_string (To_dom.of_input inp)##value));
	Lwt.return_unit);
      Lwt_js_events.(async @@ fun () ->
	clicks (To_dom.of_button remove_button) @@ fun _ _ ->
	remove_translation twe lang;
	Lwt.return_unit)

  let handle twe = function
    | `Add (lang, msg) -> add_translation twe lang msg
    | `Remove lang -> remove_translation twe lang
}}

{shared{
  let twine_editor ?value:(tw : Twine.t = Twine.make []) () =
    let open Html5 in
    let add_input =
      D.input ~input_type:`Text
	      ~a:[D.a_class ["lang"]; D.a_size 2; D.a_maxlength 2] () in
    let add_button = D.button ~button_type:`Button [D.pcdata "+"] in
    let add_span = D.span ~a:[D.a_class ["ui"]] [add_input; add_button] in
    let trans_span = D.span ~a:[D.a_class ["main"]] [] in
    let outer = D.span ~a:[D.a_class ["twine-editor"]] [trans_span; add_span] in
    let comm = {twine_editor_in Emitter.tangle{
      let open Html5 in
      let emitter, emit = Emitter.create () in
      let add_input_dom = To_dom.of_input %add_input in
      let twe =
	{ twe_container_dom = To_dom.of_element %trans_span;
	  twe_emit = emit;
	  twe_map = Lang_map.empty } in
      Lang_map.iter (add_translation twe) %tw;
      Lwt_js_events.(async @@ fun () ->
	clicks (To_dom.of_element %add_button) @@ fun _ _ ->
	add_translation twe (Lang.of_string (Js.to_string add_input_dom##value))
			"";
	add_input_dom##value <- Js.string "";
	Lwt.return_unit);
      emitter, handle twe
    }} in
    outer,
    {twine_editor_in Emitter.t{fst %comm}},
    {twine_editor_in -> unit{snd %comm}}
}}
