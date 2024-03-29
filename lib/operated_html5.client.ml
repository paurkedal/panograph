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

open Eliom_content
open Js_of_ocaml
open Unprime_enumlist

module O = struct

  type ('attr, 'ev, 'ep, 'eu, 'cv, 'cp, 'cu) star =
    ?a: (('attr Html.attrib) list) ->
    ?intro: 'eu Html.elt list ->
    ('ev -> ('ep -> unit) * 'eu Html.elt list) ->
    'cv -> ('cp -> unit) * 'cu Html.elt

  let list_op f container_dom ys =
    let open Html in
    let state = ref (Enumlist.of_list ys) in
    let rec find_next pos =
      if pos = Enumlist.length !state then Js.Opt.empty else
      match Enumlist.get !state pos with
      | _, [] -> find_next (pos + 1)
      | _, item :: _items -> Js.Opt.return (To_dom.of_element item) in
    let insert_at pos item =
      let item_dom = To_dom.of_element item in
      Dom.insertBefore container_dom item_dom (find_next pos) in
    let delete item =
      let item_dom = To_dom.of_element item in
      Dom.removeChild container_dom item_dom in
    function
    | `Insert (pos, x) ->
      let y = f x in
      List.iter (insert_at pos) (snd y);
      state := Enumlist.insert pos y !state
    | `Delete pos ->
      List.iter delete (snd (Enumlist.get !state pos));
      state := Enumlist.delete pos !state
    | `Move (pos, pos') ->
      let f, items = Enumlist.get !state pos in
      List.iter delete items;
      state := Enumlist.delete pos !state;
      List.iter (insert_at pos') items;
      state := Enumlist.insert pos (f, items) !state
    | `Update (pos, dx) ->
      fst (Enumlist.get !state pos) dx

  let p ?a ?(intro = []) f xs =
    let ys = List.map f xs in
    let elem = Html.D.p ?a (intro @ List.flatten (List.map snd ys)) in
    (list_op f (Html.To_dom.of_element elem) ys, elem)

  let pre ?a ?(intro = []) f xs =
    let ys = List.map f xs in
    let elem = Html.D.pre ?a (intro @ List.flatten (List.map snd ys)) in
    (list_op f (Html.To_dom.of_element elem) ys, elem)

  let div ?a ?(intro = []) f xs =
    let ys = List.map f xs in
    let elem = Html.D.div ?a (intro @ List.flatten (List.map snd ys)) in
    (list_op f (Html.To_dom.of_element elem) ys, elem)

  let span ?a ?(intro = []) f xs =
    let ys = List.map f xs in
    let elem = Html.D.span ?a (intro @ List.flatten (List.map snd ys)) in
    (list_op f (Html.To_dom.of_element elem) ys, elem)

  let ul ?a ?(intro = []) f xs =
    let ys = List.map f xs in
    let elem = Html.D.ul ?a (intro @ List.flatten (List.map snd ys)) in
    (list_op f (Html.To_dom.of_element elem) ys, elem)

  let ol ?a ?(intro = []) f xs =
    let ys = List.map f xs in
    let elem = Html.D.ol ?a (intro @ List.flatten (List.map snd ys)) in
    (list_op f (Html.To_dom.of_element elem) ys, elem)

  let dl ?a ?(intro = []) f xs =
    let ys = List.map f xs in
    let elem = Html.D.dl ?a (intro @ List.flatten (List.map snd ys)) in
    (list_op f (Html.To_dom.of_element elem) ys, elem)

  let table ?caption ?columns ?thead ?tfoot ?a ?(intro = []) f xs =
    let ys = List.map f xs in
    let trs = intro @ List.flatten (List.map snd ys) in
    let elem = Html.D.table ?caption ?columns ?thead ?tfoot ?a trs in
    (list_op f (Html.To_dom.of_element elem) ys, elem)

  let tr ?a ?(intro = []) f xs =
    let ys = List.map f xs in
    let elem = Html.D.tr ?a (intro @ List.flatten (List.map snd ys)) in
    (list_op f (Html.To_dom.of_element elem) ys, elem)

  let select ?a ?(intro = []) f xs =
    let ys = List.map f xs in
    let elem = Html.D.Raw.select ?a (intro @ List.flatten (List.map snd ys)) in
    (list_op f (Html.To_dom.of_element elem) ys, elem)

  let optgroup ~label ?a ?(intro = []) f xs =
    let ys = List.map f xs in
    let elem = Html.D.Raw.optgroup ~label ?a
                                    (intro @ List.flatten (List.map snd ys)) in
    (list_op f (Html.To_dom.of_element elem) ys, elem)
end
