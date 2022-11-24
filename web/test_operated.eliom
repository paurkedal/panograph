(* Copyright (C) 2015--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

[%%server
  open Eliom_client
  open Unprime_list
]
[%%shared
  open Eliom_content
  open Presentation_types

  module String_set = struct
    include Prime_enumset.Make (String)
    type rep = string list [@@deriving json]
    let to_json buf xs = rep_to_json buf (elements xs) [@@warning "-32"]
    let of_json json = of_ordered_elements (rep_of_json json) [@@warning "-32"]
  end
  module String_set_p = Presenting_set.Make (String_set)

  module String_map = Prime_enummap.Make (String)
  module Main_p = Presenting_map.Make (String_map) (String_set_p)
]
[%%client
  open Js_of_ocaml
  open Unprime
]

let s_r = Eliom_reference.Volatile.eref ~scope:`Site @@
  List.fold
    (fun (k, xs) ->
      String_map.add k (List.fold String_set.add xs String_set.empty))
    [ "fruit", ["apple"; "pear"];
      "berry", ["banana"; "cherry"] ]
    String_map.empty

let ev, emit = React.E.create ()

let patch ds =
  let dp, s' = Main_p.change ds (Eliom_reference.Volatile.get s_r) in
  Eliom_reference.Volatile.set s_r s';
  emit dp;
  Lwt.return_unit

let%client patch
  : (string, String_set.t, string set_patch) map_patch -> unit Lwt.t =
  ~%(server_function [%json: (string, String_set.t, string set_patch) map_patch]
                     patch)

[%%client
  open Html
  open Operated_html5

  let make_item (k, p) =
    let intro =
      let input = D.Raw.input ~a:[D.a_input_type `Text] () in
      let input_dom = Html.To_dom.of_input input in
      let on_add _ =
        let s = Js.to_string (input_dom##.value) in
        Lwt.async (fun () -> patch (Map_at (k, Set_add s))) in
      let add_button =
        D.Raw.button ~a:[D.a_button_type `Button; D.a_onclick on_add]
                     [D.txt "+"] in
      [D.li [input; D.txt " "; add_button]] in
    let make_li x =
      let delete _ =
        Lwt.async (fun () -> patch (Map_at (k, Set_remove x))) in
      let delete_button =
        D.Raw.button ~a:[D.a_button_type `Button; D.a_onclick delete]
                     [D.txt "−"] in
      absurd, [D.li [D.txt x; D.txt " "; delete_button]] in
    let patch_ul, ul = O.ul ~intro make_li p in
    patch_ul, [D.dt [D.txt k]; D.dd [ul]]

  let make_dl p =
    let intro =
      let input = D.Raw.input ~a:[D.a_input_type `Text] () in
      let input_dom = Html.To_dom.of_input input in
      let on_add _ =
        let k = Js.to_string (input_dom##.value) in
        Lwt.async (fun () -> patch (Map_add (k, String_set.empty))) in
      let add_button =
        D.Raw.button ~a:[D.a_button_type `Button; D.a_onclick on_add]
                     [D.txt "+"] in
      [D.dt [input]; D.dd [add_button]] in
    O.dl ~intro make_item p
]

let handler () () =
  let open Html in
  let p = Main_p.present (Eliom_reference.Volatile.get s_r) in
  let ev_c = Eliom_react.Down.of_react ev in
  let dl = [%client
    let patch, dl = make_dl ~%p in
    Lwt_react.E.keep (React.E.trace patch ~%ev_c);
    dl
  ] in
  Lwt.return [(C.node dl : [`Dl] elt :> [> `Dl] elt)]
