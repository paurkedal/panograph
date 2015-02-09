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
  open Eliom_content
  open Operated_types
  open Presentation_sigs
  open Presentation_types
  open Unprime
  open Unprime_list
  open Unprime_option

  module String_set = Prime_enumset.Make (String)
  module String_set_p = Presenting_set.Make (String_set)
}}

let s_r = Eliom_reference.Volatile.eref ~scope:`Site
  (List.fold String_set.add ["One"; "Two"] String_set.empty)

let ev, emit = React.E.create ()
let ev = React.E.fmap ident ev

let patch ds =
  let dp, s' = String_set_p.change ds (Eliom_reference.Volatile.get s_r) in
  Eliom_reference.Volatile.set s_r s';
  emit dp;
  Lwt.return_unit

let patch_c = server_function Json.t<string set_patch> patch

let render () =
  let open Html5 in
  let p = String_set_p.present (Eliom_reference.Volatile.get s_r) in
  let ev_c = Eliom_react.Down.of_react ev in
  let ul = {{
    let open Html5 in
    let open Operated_html5 in

    let input = D.input ~input_type:`Text () in
    let input_dom = Html5.To_dom.of_input input in
    let on_add _ =
      let s = Js.to_string (input_dom##value) in
      Lwt.async (fun () -> %patch_c (Set_add s)) in
    let add_button = D.button ~button_type:`Button ~a:[D.a_onclick on_add]
			      [D.pcdata "+"] in
    let add_li = D.li [input; D.pcdata " "; add_button] in
    let make_li x =
      let delete _ = Lwt.async (fun () -> %patch_c (Set_remove x)) in
      let delete_button = D.button ~button_type:`Button ~a:[D.a_onclick delete]
				   [D.pcdata "−"] in
      absurd, [D.li [D.pcdata x; D.pcdata " "; delete_button]] in
    let patch_ul, ul = O.ul ~intro:[add_li] make_li %p in
    Lwt_react.E.keep (React.E.trace patch_ul %ev_c);
    ul
  }} in
  C.node ul
