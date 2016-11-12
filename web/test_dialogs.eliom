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

[%%server
  open Panograph_prereq
]
[%%shared
  open Eliom_content.Html5
]
[%%client
  open Panui_dialogs
]

let render () =
  let open_acknowledge =
    D.Raw.button ~a:[F.a_button_type `Button] [D.pcdata "Acknowledge"] in
  let open_confirm =
    D.Raw.button ~a:[F.a_button_type `Button] [D.pcdata "Confirm"] in
  ignore_cv [%client
    Lwt.async begin fun () ->
      Lwt_js_events.clicks
        (To_dom.of_button ~%(open_acknowledge : [`Button] elt)) @@ fun _ _ ->
      let%lwt () = acknowledge_lwt [D.p [D.pcdata "Seen it?"]] in
      Manip.appendToBody (D.div [D.pcdata "It's seen."]);
      Lwt.return_unit
    end;
    Lwt.async begin fun () ->
      Lwt_js_events.clicks
        (To_dom.of_button ~%(open_confirm : [`Button] elt)) @@ fun _ _ ->
      let%lwt ans = confirm_lwt [D.p [D.pcdata "Do it?"]] in
      if ans then Manip.appendToBody (D.div [D.pcdata "It's done."])
             else Manip.appendToBody (D.div [D.pcdata "It's not done."]);
      Lwt.return_unit
    end
  ];
  D.div [
    D.p [
      D.pcdata "In user interface design, a modal window is a graphical control element subordinate to an application's main window which creates a mode where the main window can't be used. The modal window is a child window that requires users to interact with it before it can return to operating the parent application, thus preventing the workflow on the application main window. Modal windows are often called heavy windows or modal dialogs because the window is often used to display a dialog box.";
      D.i [D.pcdata " -- Wikipedia"]
    ];
    D.p [open_acknowledge; open_confirm];
    D.h2 [D.pcdata "Log"];
  ]
