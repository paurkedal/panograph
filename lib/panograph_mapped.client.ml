(* Copyright (C) 2014--2022  Petter A. Urkedal <paurkedal@gmail.com>
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
open Eliom_lib
open Panograph_common
open Panograph_intf
open Panograph_types
open Unprime
open Unprime_list
open Unprime_option

module Mapped_PE
        (Key : Map.OrderedType)
        (Key_SV : SNAPSHOT_VIEWER with type value = Key.t)
        (Elt_PE : RETRACTABLE_PATCH_EDITOR)
        (Container : CONTAINER with type item_ui = Key_SV.ui * Elt_PE.ui) =
struct
  type shape = {
    key_sv_shape: Key_SV.shape;
    elt_pe_shape: Elt_PE.shape;
    container_shape: Container.shape;
  }
  type ui = Container.ui
  type value = (Key_SV.value * Elt_PE.value) list

  type patch_out =
    [`Patch of Key_SV.value * Elt_PE.patch_out]

  type patch_in =
    [ `Add of Key_SV.value * Elt_PE.value
    | `Remove of Key_SV.value
    | `Patch of Key_SV.value * Key_SV.value option * Elt_PE.patch_in ]

  module Map = Prime_enummap.Make (Key)

  type elt = {
    e_key: Key_SV.value ref;
    e_key_sv: Key_SV.t;
    e_elt_pe: Elt_PE.t;
    e_item: Container.item;
  }

  type t = {
    w_shape: shape;
    w_container: Container.t;
    mutable w_map: elt Map.t;
    w_on_patch: (patch_out -> unit Panui_result.t Lwt.t) option;
  }

  let default_shape = {
    key_sv_shape = Key_SV.default_shape;
    elt_pe_shape = Elt_PE.default_shape;
    container_shape = Container.default_shape;
  }

  let add_elt w k elt =
    w.w_map <- Map.add k elt w.w_map;
    let before = Option.map (fun b -> (snd b).e_item)
                            (Map.succ_binding w.w_map k) in
    Container.append ?before w.w_container elt.e_item

  let add_binding w (k, v) =
    if Map.mem k w.w_map then
      error "Conflicting add to mapped collection."
    else begin
      let e_key = ref k in
      let on_elt_patch on_patch p = on_patch (`Patch (!e_key, p)) in
      let key_sv, key_ui =
        Key_SV.create ~shape:w.w_shape.key_sv_shape k in
      let elt_pe, elt_ui =
        Elt_PE.create ~shape:w.w_shape.elt_pe_shape
                      ?on_patch:(Option.map on_elt_patch w.w_on_patch) v in
      let item = Container.create_item ~shape:w.w_shape.container_shape
                                       (key_ui, elt_ui) in
      add_elt w k {e_key; e_key_sv = key_sv; e_elt_pe = elt_pe; e_item = item}
    end

  let remove_key w k =
    try
      let elt = Map.find k w.w_map in
      w.w_map <- Map.remove k w.w_map;
      Container.remove w.w_container elt.e_item
    with Not_found ->
      error "Cannot find element to remove from mapped collection."

  let patch_elt w k k' p =
    try
      match k' with
      | None -> Elt_PE.patch (Map.find k w.w_map).e_elt_pe p
      | Some k' ->
        let elt = Map.find k w.w_map in
        if Map.mem k' w.w_map then
          error "Conflict for incomping patch to mapped collection."
        else begin
          Container.remove w.w_container elt.e_item;
          w.w_map <- Map.remove k w.w_map;
          Elt_PE.patch elt.e_elt_pe p;
          Key_SV.set elt.e_key_sv k';
          elt.e_key := k';
          add_elt w k' elt
        end
    with Not_found ->
      error "Cannot find element to patch in mapped collection."

  let patch w = function
    | `Add (k, v) -> add_binding w (k, v)
    | `Remove k -> remove_key w k
    | `Patch (k, k', p) -> patch_elt w k k' p

  let create ?(shape = default_shape) ?on_patch init =
    let container, container_ui =
      Container.create ~shape:shape.container_shape () in
    let w =
      { w_shape = shape;
        w_container = container;
        w_map = Map.empty;
        w_on_patch = on_patch; } in
    List.iter (add_binding w) init;
    w, container_ui
end

module Ul_mapped_container = struct
  include Basic_shape
  type ui = Html_types.flow5 Html.elt
  type t = ui
  type item_ui = Html_types.flow5 Html.elt * Html_types.flow5 Html.elt
  type item = Html_types.ul_content Html.elt
  type static_ui = Prime.counit

  let default_shape = make_default_shape ["mapped"]

  let create ?(shape = default_shape) ?static () =
    let a = attribs_of_shape shape in
    let ui = Html.D.ul ~a [] in
    ui, ui

  let create_item ?(shape = default_shape) (key_ui, elt_ui) =
    Html.D.(li [key_ui; txt ": "; elt_ui])

  let append ?before ul li = Html.Manip.appendChild ?before ul li
  let remove ul li = Html.Manip.removeChild ul li
end

module Table_mapped_container = struct
  include Basic_shape
  type ui = Html_types.flow5 Html.elt
  type t = ui
  type item_ui = Html_types.flow5 Html.elt list
               * Html_types.flow5 Html.elt list
  type item = Html_types.table_content Html.elt
  type static_ui = Prime.counit

  let default_shape = make_default_shape ["mapped"]

  let create ?(shape = default_shape) ?static () =
    let a = attribs_of_shape shape in
    let ui = Html.D.table ~a [] in
    ui, ui

  let create_item ?shape (key_ui, elt_ui) =
    Html.D.(tr (td key_ui :: List.map (fun el -> td [el]) elt_ui))
  let append ?before table tr = Html.Manip.appendChild ?before table tr
  let remove table tr = Html.Manip.removeChild table tr
end
