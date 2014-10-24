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

open Panograph_intf
open Eliom_lib
open Unprime_option
let (>>=) = Lwt.(>>=)

module Collection_editor
	(Container : CONTAINER)
	(Elt_PE : RETRACTABLE_PATCH_EDITOR with type ui = Container.elt_pe_ui)
	(Elt_SE : SNAPSHOT_EDITOR with type ui = Container.elt_se_ui
				   and type value = Elt_PE.value) =
struct
  type shape = {
    container_shape : Container.shape;
    elt_pe_shape : Elt_PE.shape;
    elt_se_shape : Elt_SE.shape;
  }
  type ui = Container.ui
  type value = Elt_PE.value list
  type patch_out = [ `Add of Elt_PE.value | `Remove of Elt_PE.key
		   | `Patch of Elt_PE.patch_out ]
  type patch_in = [ `Add of Elt_PE.value | `Remove of Elt_PE.key
		  | `Patch of Elt_PE.patch_in ]

  module Items = Prime_retraction.Make (struct
    type key = Elt_PE.key
    type t = Elt_PE.t * Container.item_ui
    let compare_key k (y, _) = Elt_PE.compare_key k y
    let compare (x, _) (y, _) = Elt_PE.compare x y
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
    let before =
      Option.map snd (Items.get_o (Items.locate_elt_e item w.w_items + 1)
		 w.w_items) in
    Container.append ?before w.w_ui item_ui

  let add_value w v =
    if Items.contains (Elt_PE.key_of_value v) w.w_items then
      error "Collection_editor: Conflicting add."
    else begin
      let on_elt_patch on_patch p =
	match Elt_PE.key_of_patch_out p with
	| k, None -> on_patch (`Patch p)
	| k, Some k' ->
	  if Items.contains k' w.w_items then
	    Lwt.return
	      (Ack_error "The changed item conflicts with another item.")
	  else
	    on_patch (`Patch p) in
      let elt =
	Elt_PE.create ~init:v ?on_patch:(Option.map on_elt_patch w.w_on_patch)
		      w.w_shape.elt_pe_shape in
      let on_remove on_patch () = on_patch (`Remove (Elt_PE.key_of_t elt)) in
      let item_ui =
	Container.create_item ~edit_ui:(Elt_PE.ui elt)
			      ?on_remove:(Option.map on_remove w.w_on_patch)
			      w.w_shape.container_shape in
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
      match Elt_PE.key_of_patch_in p with
      | k, None -> Elt_PE.patch (fst (Items.find_e k w.w_items)) p
      | k, Some k' ->
	let (elt, item_ui) = Items.find_e k w.w_items in
	if Items.contains k' w.w_items then
	  error "Collection_editor: Conflict for incoming patch."
	else begin
	  Container.remove w.w_ui item_ui;
	  w.w_items <- Items.remove k w.w_items;
	  Elt_PE.patch elt p;
	  add_item w (elt, item_ui)
	end
    with Not_found ->
      error "Collection_editor: Element to patch not found."

  let patch w = function
    | `Add elt -> add_value w elt
    | `Remove k -> remove_key w k
    | `Patch elt_patch -> patch_elt w elt_patch

  let create ~init ?on_patch shape =
    let add_se = Elt_SE.create shape.elt_se_shape in
    let on_add =
      Option.map (fun on_patch () -> on_patch (`Add (Elt_SE.snapshot add_se)))
		 on_patch in
    let ui = Container.create ~add_ui:(Elt_SE.ui add_se) ?on_add
			      shape.container_shape in
    let w =
      { w_shape = shape;
	w_ui = ui;
	w_items = Items.empty;
	w_on_patch = on_patch; } in
    List.iter (add_value w) init;
    w

end
