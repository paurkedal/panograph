(* Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
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

open Eliom_lib
open Panograph_intf
open Unprime_option

module Mapped_PE
	(Key : Map.OrderedType)
	(Key_SV : SNAPSHOT_VIEWER with type value = Key.t)
	(Elt_PE : RETRACTABLE_PATCH_EDITOR)
	(Container : CONTAINER with type item_ui = Key_SV.ui * Elt_PE.ui) =
struct
  type shape = {
    key_sv_shape : Key_SV.shape;
    elt_pe_shape : Elt_PE.shape;
    container_shape : Container.shape;
  }
  type ui = Container.ui
  type value = (Key_SV.value * Elt_PE.value) list
  type patch_out = [`Patch of Key_SV.value * Elt_PE.patch_out]
  type patch_in = [ `Add of Key_SV.value * Elt_PE.value
		  | `Remove of Key_SV.value
		  | `Patch of Key_SV.value * Key_SV.value option
			    * Elt_PE.patch_in ]

  module Map = Prime_enummap.Make (Key)

  type elt = {
    e_key : Key_SV.value ref;
    e_key_sv : Key_SV.t;
    e_elt_pe : Elt_PE.t;
    e_item : Container.item;
  }

  type t = {
    w_shape : shape;
    w_container : Container.t;
    mutable w_map : elt Map.t;
    w_on_patch : (patch_out -> ack Lwt.t) option;
  }

  let ui w = Container.ui w.w_container

  let add_elt w k elt =
    w.w_map <- Map.add k elt w.w_map;
    let before =
      Option.found (fun () -> (snd (Map.succ_binding_e w.w_map k)).e_item) in
    Container.append ?before w.w_container elt.e_item

  let add_binding w (k, v) =
    if Map.contains k w.w_map then
      error "Conflicting add to mapped collection."
    else begin
      let e_key = ref k in
      let on_elt_patch on_patch p = on_patch (`Patch (!e_key, p)) in
      let key_sv = Key_SV.create ~init:k w.w_shape.key_sv_shape in
      let elt_pe =
	Elt_PE.create ~init:v ?on_patch:(Option.map on_elt_patch w.w_on_patch)
		      w.w_shape.elt_pe_shape in
      let item = Container.create_item (Key_SV.ui key_sv, Elt_PE.ui elt_pe)
				       w.w_shape.container_shape in
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
	if Map.contains k' w.w_map then
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

  let create ~init ?on_patch shape =
    let container = Container.create shape.container_shape in
    let w =
      { w_shape = shape;
	w_container = container;
	w_map = Map.empty;
	w_on_patch = on_patch; } in
    List.iter (add_binding w) init;
    w
end
