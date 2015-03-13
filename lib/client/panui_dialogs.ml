(* Copyright (C) 2015  Petter Urkedal <paurkedal@gmail.com>
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

let modal_veil = D.div ~a:[D.a_class ["pan-dialog-veil"]] []
let modal_dialogs = ref []

module Modal_dialog = struct

  type t = div elt

  let open_veil () = Manip.appendToBody modal_veil
  let close_veil () = Manip.removeSelf modal_veil

  let open_bare ?(on_close = ident) content =
    if !modal_dialogs = [] then open_veil ();
    let elem = D.div ~a:[D.a_class ["pan-dialog-box"]] content in
    Manip.appendToBody elem;
    modal_dialogs := (elem, on_close) :: !modal_dialogs;
    elem

  let open_std ?(on_close = ident) content buttons =
    open_bare [
      D.div ~a:[D.a_class ["pan-dialog-content"]] content;
      D.div ~a:[D.a_class ["pan-dialog-buttons"]] buttons;
    ]

  let rec close elem =
    match !modal_dialogs with
    | (elem', on_close) :: elems ->
      Manip.removeSelf elem'; on_close ();
      modal_dialogs := elems;
      if !modal_dialogs = [] then close_veil ();
      if elem' != elem then close elem
    | [] ->
      Eliom_lib.debug "Modal dialog was already closed."

  let close_all () =
    List.iter (fun (elem, on_close) -> Manip.removeSelf elem; on_close ())
	      !modal_dialogs;
    modal_dialogs := [];
    close_veil ()

end

let confirm_lwt ?(ok = ok_label) ?(cancel = cancel_label) content =
  let ok_button = D.button ~button_type:`Button ok in
  let cancel_button = D.button ~button_type:`Button cancel in
  let close_waiter, close_wakener = Lwt.wait () in
  let dialog =
    Modal_dialog.open_std ~on_close:(Lwt.wakeup close_wakener)
			  content [ok_button; cancel_button] in
  Lwt.choose [
    ( Lwt_js_events.click (To_dom.of_button ok_button) >|= fun _ ->
      Modal_dialog.close dialog; true );
    ( Lwt_js_events.click (To_dom.of_button cancel_button) >|= fun _ ->
      Modal_dialog.close dialog; false );
    ( close_waiter >|= fun _ -> false );
  ]
