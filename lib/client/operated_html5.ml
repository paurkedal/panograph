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

open Eliom_content
open Html5_types
open Operated_types
open Unprime_enumlist

module O = struct

  type ('attr, 'ev, 'ep, 'eu, 'cv, 'cp, 'cu) star =
    ?a: (('attr Html5.attrib) list) ->
    ?intro: 'eu Html5.elt list ->
    ('ev -> ('ep -> unit) * 'eu Html5.elt list) ->
    'cv -> ('cp -> unit) * 'cu Html5.elt

  let list_op f state container_dom ys op =
    let open Html5 in
    let rec find_next pos =
      if pos = Enumlist.length !state then Js.Opt.empty else
      match Enumlist.get pos !state with
      | _, [] -> find_next (pos + 1)
      | _, item :: items -> Js.Opt.return (To_dom.of_element item) in
    let insert_at pos item =
      let item_dom = To_dom.of_element item in
      Dom.insertBefore container_dom item_dom (find_next pos) in
    let delete item =
      let item_dom = To_dom.of_element item in
      Dom.removeChild container_dom item_dom in
    match op with
    | Grid1_insert (pos, x) ->
      let y = f x in
      List.iter (insert_at pos) (snd y);
      state := Enumlist.insert pos y !state
    | Grid1_delete pos ->
      List.iter delete (snd (Enumlist.get pos !state));
      state := Enumlist.delete pos !state
    | Grid1_move (pos, pos') ->
      let f, items = Enumlist.get pos !state in
      List.iter delete items;
      state := Enumlist.delete pos !state;
      List.iter (insert_at pos') items;
      state := Enumlist.insert pos (f, items) !state
    | Grid1_at (pos, dx) ->
      fst (Enumlist.get pos !state) dx

  let div ?a ?(intro = []) f xs =
    let ys = List.map f xs in
    let state = ref (Enumlist.of_list ys) in
    let div = Html5.D.div (intro @ List.flatten (List.map snd ys)) in
    (list_op f state (Html5.To_dom.of_span div) ys, div)

  let span ?a ?(intro = []) f xs =
    let ys = List.map f xs in
    let state = ref (Enumlist.of_list ys) in
    let span = Html5.D.span (intro @ List.flatten (List.map snd ys)) in
    (list_op f state (Html5.To_dom.of_span span) ys, span)

  let ul ?a ?(intro = []) f xs =
    let ys = List.map f xs in
    let state = ref (Enumlist.of_list ys) in
    let ul = Html5.D.ul (intro @ List.flatten (List.map snd ys)) in
    (list_op f state (Html5.To_dom.of_ul ul) ys, ul)

  let ol ?a ?(intro = []) f xs =
    let ys = List.map f xs in
    let state = ref (Enumlist.of_list ys) in
    let ol = Html5.D.ol (intro @ List.flatten (List.map snd ys)) in
    (list_op f state (Html5.To_dom.of_ol ol) ys, ol)

  let dl ?a ?(intro = []) f xs =
    let ys = List.map f xs in
    let state = ref (Enumlist.of_list ys) in
    let dl = Html5.D.dl (intro @ List.flatten (List.map snd ys)) in
    (list_op f state (Html5.To_dom.of_dl dl) ys, dl)

end
