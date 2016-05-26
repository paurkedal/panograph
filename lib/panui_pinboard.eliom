(* Copyright (C) 2016  Petter A. Urkedal <paurkedal@gmail.com>
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

[%%shared.start]
open Eliom_content.Html5
open Lwt.Infix
open Unprime
open Unprime_option

type item = Html5_types.tr elt

type t = {
  table : 'a. ([> Html5_types.table] as 'a) elt;
  hover_mutex : Lwt_mutex.t client_value;
}

type subject_content = Html5_types.flow5

let create ?(freeze_on_hover = true) ?(freeze_timeout = 0.2) () =
  let table = D.table ~a:[D.a_class ["pan-pinboard"]] [] in
  let hover_mutex = [%client Lwt_mutex.create ()] in
  if freeze_on_hover then ignore [%client
    let table_dom = To_dom.of_element ~%table in
    let hover_mutex = ~%hover_mutex in
    let rec not_hovering () =
      let%lwt _ = Lwt_js_events.mouseover table_dom in
      Lwt_mutex.lock hover_mutex >>
      hovering ()
    and hovering () =
      let%lwt _ = Lwt_js_events.mouseout table_dom in
      let%lwt reentered = Lwt.pick [
        Lwt_js_events.mouseover table_dom >|= konst true;
        Lwt_js.sleep ~%freeze_timeout >> Lwt.return_false;
      ] in
      if reentered then hovering () else begin
        Lwt_mutex.unlock hover_mutex;
        not_hovering ()
      end in
    Lwt.async not_hovering
  ];
  {table; hover_mutex}

let ui (pinboard : t) = pinboard.table

[%%client.start]

let unpin item pinboard = Manip.removeChild pinboard.table item

let pin ~(subject : [< subject_content] elt list)
        ?(level = Lwt_log_js.Info) ?timeout pinboard =
  let unpin_button = D.button [D.pcdata "✗"] in
  let item = D.tr [
    D.td ~a:[D.a_class [Lwt_log_js.string_of_level level]] [unpin_button];
    D.td subject
  ] in
  Manip.appendChild pinboard.table item;
  let waiter, wakener = Lwt.wait () in
  Option.iter
    (fun t ->
      Lwt.async begin fun () ->
        Lwt.pick [
          Lwt_js.sleep t >> Lwt_mutex.with_lock pinboard.hover_mutex Lwt.return;
          waiter
        ] >>
        Lwt.return (unpin item pinboard)
      end)
    timeout;
  Lwt.async begin fun () ->
    Lwt_js_events.clicks (To_dom.of_element unpin_button)
      (fun _ _ -> Lwt.wakeup wakener (); Lwt.return_unit)
  end;
  (item : item)
