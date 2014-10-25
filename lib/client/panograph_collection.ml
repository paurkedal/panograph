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

open Panograph_common
open Panograph_intf
open Eliom_content
open Eliom_lib
open Unprime_option

let label_for_remove = [Html5.F.(b [pcdata "−"])]
let label_for_add = [Html5.F.(b [pcdata "+"])]

module Collection_editor
	(Elt_PE : RETRACTABLE_PATCH_EDITOR)
	(Elt_SE : SNAPSHOT_EDITOR with type value = Elt_PE.value)
	(Container : CONTAINER with type item_ui = Elt_PE.ui * controls_ui
				and type aux_ui = Elt_SE.ui * controls_ui) =
struct
  type shape = {
    elt_pe_shape : Elt_PE.shape;
    elt_se_shape : Elt_SE.shape;
    container_shape : Container.shape;
  }
  type ui = Container.ui
  type value = Elt_PE.value list
  type patch_out = [ `Add of Elt_PE.value | `Remove of Elt_PE.key
		   | `Patch of Elt_PE.patch_out ]
  type patch_in = [ `Add of Elt_PE.value | `Remove of Elt_PE.key
		  | `Patch of Elt_PE.patch_in ]

  module Set = Prime_retraction.Make (struct
    type key = Elt_PE.key
    type t = Elt_PE.t * Container.item
    let compare_key k (y, _) = Elt_PE.compare_key k y
    let compare (x, _) (y, _) = Elt_PE.compare x y
  end)

  type t = {
    w_shape : shape;
    w_container : Container.t;
    mutable w_set : Set.t;
    w_on_patch : (patch_out -> ack Lwt.t) option;
  }

  let ui w = Container.ui w.w_container

  let add_elt w ((_, item) as elt) =
    w.w_set <- Set.add elt w.w_set;
    let before = Option.found (fun () -> snd (Set.elt_succ_e w.w_set elt)) in
    Container.append ?before w.w_container item

  let add_value w v =
    if Set.contains (Elt_PE.key_of_value v) w.w_set then
      error "Collection_editor: Conflicting add."
    else begin
      let on_elt_patch on_patch p =
	match Elt_PE.key_of_patch_out p with
	| k, None -> on_patch (`Patch p)
	| k, Some k' ->
	  if Set.contains k' w.w_set then
	    Lwt.return
	      (Ack_error "The changed item conflicts with another item.")
	  else
	    on_patch (`Patch p) in
      let elt_pe =
	Elt_PE.create ~init:v ?on_patch:(Option.map on_elt_patch w.w_on_patch)
		      w.w_shape.elt_pe_shape in
      let remove_button =
	match w.w_on_patch with
	| Some on_patch ->
	  let on_remove () = on_patch (`Remove (Elt_PE.key_of_t elt_pe)) in
	  [make_button on_remove label_for_remove]
	| None -> [] in
      let item = Container.create_item (Elt_PE.ui elt_pe, remove_button)
				       w.w_shape.container_shape in
      add_elt w (elt_pe, item)
    end

  let remove_key w k =
    try
      let _, item = Set.find_e k w.w_set in
      w.w_set <- Set.remove k w.w_set;
      Container.remove w.w_container item
    with Not_found -> ()

  let patch_elt w p =
    try
      match Elt_PE.key_of_patch_in p with
      | k, None -> Elt_PE.patch (fst (Set.find_e k w.w_set)) p
      | k, Some k' ->
	let (elt, item) = Set.find_e k w.w_set in
	if Set.contains k' w.w_set then
	  error "Collection_editor: Conflict for incoming patch."
	else begin
	  Container.remove w.w_container item;
	  w.w_set <- Set.remove k w.w_set;
	  Elt_PE.patch elt p;
	  add_elt w (elt, item)
	end
    with Not_found ->
      error "Collection_editor: Element to patch not found."

  let patch w = function
    | `Add elt -> add_value w elt
    | `Remove k -> remove_key w k
    | `Patch elt_patch -> patch_elt w elt_patch

  let create ~init ?on_patch shape =
    let aux =
      match on_patch with
      | None -> None
      | Some on_patch ->
	let add_se = Elt_SE.create shape.elt_se_shape in
	let on_add () = on_patch (`Add (Elt_SE.snapshot add_se)) in
	let add_button = make_button on_add label_for_add in
	Some (Elt_SE.ui add_se, [add_button]) in
    let container = Container.create ?aux shape.container_shape in
    let w =
      { w_shape = shape;
	w_container = container;
	w_set = Set.empty;
	w_on_patch = on_patch; } in
    List.iter (add_value w) init;
    w

end