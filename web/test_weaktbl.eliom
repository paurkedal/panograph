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
  open Eliom_content
}}

{client{
  let wt = Pandom_weaktbl.create ()

  let do_iter _ =
    Pandom_weaktbl.iter (fun _ i -> Eliom_lib.debug "Found item %d" i) wt

  let render () =
    let open Html5 in
    let item0 = D.li [D.pcdata "item 11"] in
    let item1 = D.li [D.pcdata "item 12"] in
    let item2 = D.li [D.pcdata "item 13"] in
    Pandom_weaktbl.add wt (To_dom.of_element item0) 11;
    Pandom_weaktbl.add wt (To_dom.of_element item1) 12;
    Pandom_weaktbl.add wt (To_dom.of_element item2) 13;
    let iter_button =
      D.Raw.button ~a:[D.a_button_type `Button; D.a_onclick do_iter]
                   [D.pcdata "iter"] in
    D.div [D.ul [item0; item2]; iter_button]
}}

let render () = Html5.C.node {{render ()}}
