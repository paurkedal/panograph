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
  open Printf
]

[%%client
  let classify ks = String.concat "_" (List.rev ks)
  let chan = Pandom_weakchan.create classify

  let rec make ks =
    let add_input = D.Raw.input ~a:[D.a_input_type `Text] () in
    let add_input_dom = To_dom.of_input add_input in
    let on_add _ =
      let k = Js.to_string add_input_dom##.value in
      add_input_dom##.value := Js.string "";
      Pandom_weakchan.send chan ks (`Add k) in
    let add_button =
      D.button ~a:[D.a_button_type `Button; D.a_onclick on_add]
               [D.pcdata "add"] in
    let ul = D.ul [D.li [add_input; add_button]] in
    let on_msg = function
      | `Add k ->
        let ul' = make (k :: ks) in
        Manip.appendChild ul (D.li [D.b [D.pcdata k]; ul']) in
    ignore (Pandom_weakchan.subscribe_class chan (To_dom.of_ul ul) ks on_msg);
    ul
]

let render () = C.node [%client make []]
