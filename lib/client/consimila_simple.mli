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
open Consimila_intf

module Simple_shape : sig
  type t
  val make : ?a: Html5_types.input_attrib Html5.attrib list -> unit -> t
end

module type SIMPLE_PATCH_EDITOR = sig

  type value

  include RETRACTABLE_PATCH_EDITOR
     with type value := value
      and type key = value
      and type patch_out = [`Set of value]
      and type patch_in = [`Set of value]
      and type shape = Simple_shape.t
      and type ui = Html5_types.flow5 Html5.elt
end

module type SIMPLE_SNAPSHOT_EDITOR =
  SNAPSHOT_EDITOR
    with type shape = Simple_shape.t
     and type ui = Html5_types.flow5 Html5.elt

module Simple_patch_editor (Value : STRINGABLE) :
  SIMPLE_PATCH_EDITOR with type value = Value.t

module Simple_snapshot_editor (Value : STRINGABLE) :
  SIMPLE_SNAPSHOT_EDITOR with type value = Value.t

module String_PE : SIMPLE_PATCH_EDITOR with type value = string
module Int_PE : SIMPLE_PATCH_EDITOR with type value = int
module Float_PE : SIMPLE_PATCH_EDITOR with type value = float

module String_SE : SIMPLE_SNAPSHOT_EDITOR with type value = string
module Int_SE : SIMPLE_SNAPSHOT_EDITOR with type value = int
module Float_SE : SIMPLE_SNAPSHOT_EDITOR with type value = float
