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

module Collection_editor
	(Container : CONTAINER)
	(Elt_PE : RETRACTABLE_PATCH_EDITOR with type ui = Container.elt_pe_ui)
	(Elt_SE : SNAPSHOT_EDITOR with type ui = Container.elt_se_ui
				   and type value = Elt_PE.value) :
sig
  type shape = {
    container_shape : Container.shape;
    elt_pe_shape : Elt_PE.shape;
    elt_se_shape : Elt_SE.shape;
  }
  include PATCH_EDITOR
    with type shape := shape
     and type ui = Container.ui
     and type value = Elt_PE.value list
     and type patch_out = [ `Add of Elt_PE.value | `Remove of Elt_PE.key
			  | `Patch of Elt_PE.patch_out ]
     and type patch_in = [ `Add of Elt_PE.value | `Remove of Elt_PE.key
			 | `Patch of Elt_PE.patch_in ]
end
