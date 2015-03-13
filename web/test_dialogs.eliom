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

{shared{
  open Eliom_content.Html5
}}
{client{
  open Panui_dialogs
}}

let render () =
  let trigger = D.button ~button_type:`Button [D.pcdata "Try!"] in
  ignore {unit{Lwt.async @@ fun () ->
    Lwt_js_events.clicks (To_dom.of_button %trigger) begin fun _ _ ->
      lwt ans = confirm_lwt [
	D.p [D.pcdata "How about it?"];
      ] in
      if ans then Manip.appendToBody (D.div [D.pcdata "It's done."]);
      Lwt.return_unit
    end
  }};
  D.div [
    D.h1 [D.pcdata "Dialog Test"];
    D.p [trigger];
    D.h2 [D.pcdata "Confirmations"];
  ]
