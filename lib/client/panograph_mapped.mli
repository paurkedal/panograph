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

open Eliom_content
open Panograph_intf

module Mapped_PE
	(Key : Map.OrderedType)
	(Key_SV : SNAPSHOT_VIEWER with type value = Key.t)
	(Elt_PE : RETRACTABLE_PATCH_EDITOR)
	(Container : CONTAINER with type item_ui = Key_SV.ui * Elt_PE.ui) :
sig
  type shape = {
    key_sv_shape : Key_SV.shape;
    elt_pe_shape : Elt_PE.shape;
    container_shape : Container.shape;
  }
  include PATCH_EDITOR
    with type shape := shape
     and type ui = Container.ui
     and type value = (Key_SV.value * Elt_PE.value) list
     and type patch_out = [ `Patch of Key_SV.value * Elt_PE.patch_out ]
     and type patch_in = [ `Add of Key_SV.value * Elt_PE.value
			 | `Remove of Key_SV.value
			 | `Patch of Key_SV.value * Key_SV.value option
				   * Elt_PE.patch_in ]
end

module Mapped_container_shape : sig
  type t = {
    a_id : string option;
    a_class : string list;
  }
  val default : t
end

module Ul_mapped_container : CONTAINER
  with type shape = Mapped_container_shape.t
   and type item_ui = Html5_types.flow5 Html5.elt * Html5_types.flow5 Html5.elt
   and type ui = Html5_types.flow5 Html5.elt

module Table_mapped_container : CONTAINER
  with type shape = Mapped_container_shape.t
   and type item_ui = Html5_types.flow5 Html5.elt list
		    * Html5_types.flow5 Html5.elt list
   and type ui = Html5_types.flow5 Html5.elt
