(* Copyright (C) 2014--2022  Petter A. Urkedal <paurkedal@gmail.com>
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
  open Eliom_content
  open Panograph_i18n

  type twine_editor_out = [`Add of lang * string | `Remove of lang]
                          [@@deriving json]
  type twine_editor_in = twine_editor_out
]
[%%client
  open Eliom_lib
  open Js_of_ocaml
  open Js_of_ocaml_lwt

  type twine_editor = {
    twe_container_dom: Dom_html.element Js.t;
    twe_error_dom: Dom_html.element Js.t;
    twe_patch_out: twine_editor_out -> unit Panui_result.t Lwt.t;
    mutable twe_map: (Dom_html.element Js.t * Dom_html.inputElement Js.t)
                     Lang_map.t;
  }
]

[%%client
  open Js_of_ocaml
  open Unprime_option

  let set_error twe err =
    let msg = Panui_error.message err in
    twe.twe_error_dom##.innerHTML := Js.string "";
    Dom.appendChild twe.twe_error_dom
                    (Dom_html.document##createTextNode(Js.string msg));
    twe.twe_error_dom##.style##.visibility := Js.string "visible";
    twe.twe_container_dom##.classList##add(Js.string "error")

  let clear_error twe =
    twe.twe_error_dom##.style##.visibility := Js.string "hidden";
    twe.twe_error_dom##.innerHTML := Js.string "";
    twe.twe_container_dom##.classList##remove(Js.string "error")

  let patch_out twe p =
    twe.twe_patch_out p >|=
    (function
     | Ok () -> clear_error twe
     | Error err -> set_error twe err)

  let remove_translation twe lang =
    let item_dom, _ = Lang_map.find lang twe.twe_map in
    Dom.removeChild twe.twe_container_dom item_dom;
    twe.twe_map <- Lang_map.remove lang twe.twe_map

  let add_translation (twe : twine_editor) lang msg =
    let open Html in
    try
      let _, inp_dom = Lang_map.find lang twe.twe_map in
      inp_dom##.classList##remove (Js.string "dirty");
      inp_dom##.value := Js.string msg
    with Not_found ->
      let inp = D.input ~a:[D.a_input_type `Text; D.a_value msg] () in
      let inp_dom = To_dom.of_input inp in
      let remove_button =
        D.button ~a:[D.a_button_type `Button] [D.txt "−"] in
      let remove_dom = To_dom.of_button remove_button in
      let item = D.span [D.span [D.txt (Lang.to_string lang)];
                         inp; remove_button] in
      let next = Option.map (fst % snd)
                            (Lang_map.succ_binding twe.twe_map lang) in
      let item_dom = To_dom.of_span item in
      Dom.insertBefore twe.twe_container_dom item_dom (Js.Opt.option next);
      twe.twe_map <- Lang_map.add lang (item_dom, inp_dom) twe.twe_map;
      Lwt_js_events.(async @@ fun () ->
        changes inp_dom @@ fun _ _ ->
        let msg = Js.to_string (To_dom.of_input inp)##.value in
        inp_dom##.classList##add(Js.string "dirty");
        if msg = "" then patch_out twe (`Remove lang)
                    else patch_out twe (`Add (lang, msg)));
      Lwt_js_events.(async @@ fun () ->
        clicks remove_dom @@ fun _ _ ->
        remove_dom##.classList##add(Js.string "dirty");
        patch_out twe (`Remove lang))

  let patch_in twe p =
    match p with
    | `Add (lang, msg) -> add_translation twe lang msg
    | `Remove lang -> remove_translation twe lang
]

[%%shared
  let twine_editor ?value:(tw : Twine.t = Twine.make [])
      (patch_out :
        (twine_editor_out -> unit Panui_result.t Lwt.t) Eliom_client_value.t) =
    let open Html in
    let add_input =
      D.input ~a:[D.a_input_type `Text; D.a_class ["lang"];
                  D.a_size 2; D.a_maxlength 2] () in
    let add_button = D.button ~a:[D.a_button_type `Button] [D.txt "+"] in
    let add_span = D.span ~a:[D.a_class ["ui"]] [add_input; add_button] in
    let trans_span = D.span ~a:[D.a_class ["main"]] [] in
    let error_span = D.span ~a:[D.a_class ["error"]] [] in
    let outer = D.span ~a:[D.a_class ["twine-editor"]]
                       [trans_span; add_span; error_span] in
    let patch_in : (twine_editor_in -> unit) Eliom_client_value.t = [%client
      let open Html in
      let add_input_dom = To_dom.of_input ~%(add_input : [`Input] elt) in
      let error_dom = To_dom.of_element ~%(error_span : [`Span] elt) in
      error_dom##.style##.visibility := Js.string "hidden";
      let twe =
        { twe_container_dom = To_dom.of_element ~%(trans_span : [`Span] elt);
          twe_error_dom = To_dom.of_element ~%(error_span : [`Span] elt);
          twe_patch_out = ~%patch_out;
          twe_map = Lang_map.empty } in
      Lang_map.iter (add_translation twe) ~%tw;
      Lwt_js_events.(async @@ fun () ->
        clicks (To_dom.of_element ~%(add_button : [`Button] elt)) @@ fun _ _ ->
        add_translation twe
          (Lang.of_string (Js.to_string add_input_dom##.value)) "";
        add_input_dom##.value := Js.string "";
        Lwt.return_unit);
      patch_in twe
    ] in
    outer, patch_in
]
