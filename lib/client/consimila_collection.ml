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

open Consimila_intf
open Eliom_lib
open Unprime_option
let (>>=) = Lwt.(>>=)

module Collection_editor
	(Container : CONTAINER)
	(Elt : RETRACTABLE_PATCH_EDITOR with type ui = Container.elt_ui)
	(New : SNAPSHOT_EDITOR with type ui = Container.elt_ui
				and type value = Elt.value) =
struct
  type shape = {
    container_shape : Container.shape;
    elt_shape : Elt.shape;
    new_shape : New.shape;
  }
  type ui = Container.ui
  type value = Elt.value list
  type patch_out = [ `Add of Elt.value | `Remove of Elt.key
		   | `Patch of Elt.patch_out ]
  type patch_in = [ `Add of Elt.value | `Remove of Elt.key
		  | `Patch of Elt.patch_in ]

  module Items = Prime_retraction.Make (struct
    type key = Elt.key
    type t = Elt.t * Container.item_ui
    let compare_key k (y, _) = Elt.compare_key k y
    let compare (x, _) (y, _) = Elt.compare x y
  end)

  type t = {
    w_shape : shape;
    w_ui : ui;
    mutable w_items : Items.t;
    w_on_patch : (patch_out -> ack Lwt.t) option;
  }

  let ui w = w.w_ui

  let add_item w ((_, item_ui) as item) =
    w.w_items <- Items.add item w.w_items;
    match Items.get_o (Items.locate_elt_e item w.w_items + 1) w.w_items with
    | None -> Container.append w.w_ui item_ui
    | Some (_, succ_ui) -> Container.insert w.w_ui succ_ui item_ui

  let add_value w v =
    if Items.contains (Elt.key_of_value v) w.w_items then
      error "Collection_editor: Conflicting add."
    else begin
      let on_elt_patch on_patch p =
	match Elt.key_of_patch_out p with
	| k, None -> on_patch (`Patch p)
	| k, Some k' ->
	  if Items.contains k' w.w_items then
	    Lwt.return
	      (Ack_error "The changed item conflicts with another item.")
	  else
	    on_patch (`Patch p) in
      let elt =
	Elt.create ~init:v ?on_patch:(Option.map on_elt_patch w.w_on_patch)
		   w.w_shape.elt_shape in
      let item_ui =
	Container.create_item w.w_shape.container_shape (Elt.ui elt) in
      add_item w (elt, item_ui)
    end

  let remove_key w k =
    try
      let _, item_ui = Items.find_e k w.w_items in
      w.w_items <- Items.remove k w.w_items;
      Container.remove w.w_ui item_ui
    with Not_found -> ()

  let patch_elt w p =
    try
      match Elt.key_of_patch_in p with
      | k, None -> Elt.patch (fst (Items.find_e k w.w_items)) p
      | k, Some k' ->
	let (elt, item_ui) = Items.find_e k w.w_items in
	if Items.contains k' w.w_items then
	  error "Collection_editor: Conflict for incoming patch."
	else begin
	  Container.remove w.w_ui item_ui;
	  w.w_items <- Items.remove k w.w_items;
	  Elt.patch elt p;
	  add_item w (elt, item_ui)
	end
    with Not_found ->
      error "Collection_editor: Element to patch not found."

  let patch w = function
    | `Add elt -> add_value w elt
    | `Remove k -> remove_key w k
    | `Patch elt_patch -> patch_elt w elt_patch

  let create ~init ?on_patch shape =
    let ui = Container.create shape.container_shape in
    let w =
      { w_shape = shape;
	w_ui = ui;
	w_items = Items.empty;
	w_on_patch = on_patch; } in
    List.iter (add_value w) init;
    w

end
