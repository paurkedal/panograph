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
		   | `Patch of Elt.key * Elt.patch_out ]
  type patch_in = [ `Add of Elt.value | `Remove of Elt.key
		  | `Patch of Elt.key * Elt.patch_in ]

  module Elts = Prime_retraction.Make (Elt)

  type t = {
    w_shape : shape;
    w_ui : ui;
    mutable w_elts : Elts.t;
    w_on_patch : (patch_out -> ack Lwt.t) option;
  }

  let ui w = w.w_ui

  let add_elt w elt =
    let elt_ui = Elt.ui elt in
    w.w_elts <- Elts.add elt w.w_elts;
    begin match Elts.get_o (Elts.locate_elt_e elt w.w_elts + 1) w.w_elts with
    | None -> Container.prepend w.w_ui elt_ui
    | Some succ_elt -> Container.insert w.w_ui (Elt.ui succ_elt) elt_ui
    end

  let add_value w v =
    let key = Elt.key_of_value v in
    if not (Elts.contains key w.w_elts) then begin
      let on_elt_patch =
	Option.map (fun on_patch p -> on_patch (`Patch (key, p)))
		   w.w_on_patch in
      let elt = Elt.create ~init:v ?on_patch:on_elt_patch w.w_shape.elt_shape in
      add_elt w elt
    end

  let remove_key w k =
    try
      let elt = Elts.find_e k w.w_elts in
      w.w_elts <- Elts.remove k w.w_elts;
      Container.remove w.w_ui (Elt.ui elt)
    with Not_found -> ()

  let patch_elt w k p =
    try
      let elt = Elts.find_e k w.w_elts in
      if Elt.affects_key p then begin
	Container.remove w.w_ui (Elt.ui elt);
	w.w_elts <- Elts.remove k w.w_elts;
	Elt.patch elt p;
	add_elt w elt
      end else
	Elt.patch elt p
    with Not_found -> ()

  let patch w = function
    | `Add elt -> add_value w elt
    | `Remove key -> remove_key w key
    | `Patch (key, elt_patch) -> patch_elt w key elt_patch

  let create ~init ?on_patch shape =
    let ui = Container.create shape.container_shape in
    let w =
      { w_shape = shape;
	w_ui = ui;
	w_elts = Elts.empty;
	w_on_patch = on_patch; } in
    List.iter (add_value w) init;
    w

end
