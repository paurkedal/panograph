(* Copyright (C) 2014--2016  Petter A. Urkedal <paurkedal@gmail.com>
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
open Panograph_common
open Panograph_intf

module Collection_PE
        (Elt_PE : RETRACTABLE_PATCH_EDITOR)
        (Elt_SE : SNAPSHOT_EDITOR with type value = Elt_PE.value)
        (Container : CONTAINER with type item_ui = Elt_PE.ui * controls_ui
                                and type static_ui = Elt_SE.ui * controls_ui) :
sig
  type shape = {
    elt_pe_shape : Elt_PE.shape;
    elt_se_shape : Elt_SE.shape;
    container_shape : Container.shape;
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

module Ul_collection_container : sig
  include BASIC_SHAPE_TYPE
  include CONTAINER
    with type shape := shape
     and type item_ui = Html_types.flow5 Html.elt * controls_ui
     and type static_ui = Html_types.flow5 Html.elt * controls_ui
     and type ui = Html_types.flow5 Html.elt
end

module Table_collection_container : sig
  include BASIC_SHAPE_TYPE
  include CONTAINER
    with type shape := shape
     and type item_ui = Html_types.flow5 Html.elt list * controls_ui
     and type static_ui = Html_types.flow5 Html.elt list * controls_ui
     and type ui = Html_types.flow5 Html.elt
end
