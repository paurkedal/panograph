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

open Eliom_content.Html5
open Html5_types
open Pwt_infix
open Unprime

let ok_label = [D.pcdata "Ok"]
let cancel_label = [D.pcdata "Cancel"]

let close_all_r = ref (fun () -> ())
let modal_veil = D.div ~a:[D.a_class ["pan-dialog-veil"];
                           D.a_onclick (fun _ -> !close_all_r ())] []
let modal_dialogs = ref []

module Modal_dialog = struct

  type t = div elt

  let open_veil () = Manip.appendToBody modal_veil
  let close_veil () = Manip.removeSelf modal_veil

  let open_bare ?(on_cancel = ident) content =
    if !modal_dialogs = [] then open_veil ();
    let elem = D.div ~a:[D.a_class ["pan-dialog-box"]] content in
    Manip.appendToBody elem;
    modal_dialogs := (elem, on_cancel) :: !modal_dialogs;
    elem

  let open_std ?on_cancel content buttons =
    open_bare ?on_cancel [
      D.div ~a:[D.a_class ["pan-dialog-content"]] content;
      D.div ~a:[D.a_class ["pan-dialog-buttons"]] buttons;
    ]

  let rec close elem =
    match !modal_dialogs with
    | (elem', on_cancel) :: elems ->
      modal_dialogs := elems;
      Manip.removeSelf elem';
      if elem' != elem then (on_cancel (); close elem);
      if !modal_dialogs = [] then close_veil ()
    | [] ->
      Eliom_lib.debug "Modal dialog was already closed."

  let close_all () =
    List.iter (fun (elem, on_cancel) -> Manip.removeSelf elem; on_cancel ())
              !modal_dialogs;
    modal_dialogs := [];
    close_veil ()

  let () = close_all_r := close_all
end

let dialog_lwt mapping default content =
  let make_button (label, _) =
    D.Raw.button ~a:[D.a_button_type `Button] label in
  let buttons = List.map make_button mapping in
  let close_waiter, close_wakener = Lwt.wait () in
  let dialog =
    Modal_dialog.open_std ~on_cancel:(fun () -> Lwt.wakeup close_wakener ())
                          content buttons in
  let make_action button (_, value) =
    Lwt_js_events.click (To_dom.of_button button) >|= fun _ ->
    Modal_dialog.close dialog; value in
  let actions = List.map2 make_action buttons mapping in
  Lwt.choose ((close_waiter >|= konst default) :: actions)

let acknowledge_lwt ?(ok = ok_label) content =
  dialog_lwt [ok, ()] () content

let confirm_lwt ?(ok = ok_label) ?(cancel = cancel_label) content =
  dialog_lwt [ok, true; cancel, false] false content
