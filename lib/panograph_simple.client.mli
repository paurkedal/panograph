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
open Panograph_i18n
open Panograph_intf

module type SIMPLE_VALUE = sig
  include STRINGABLE
  val css_classes : string list
end

module type SIMPLE_SNAPSHOT_VIEWER = sig
  include BASIC_SHAPE_TYPE
  include SNAPSHOT_VIEWER
    with type shape := shape
     and type ui = Html5_types.flow5 Html5.elt
end

module Simple_SV (Value : SIMPLE_VALUE) :
  SIMPLE_SNAPSHOT_VIEWER with type value = Value.t

module Simple_shape : sig
  type t
  val make : ?a: Html5_types.input_attrib Html5.attrib list -> unit -> t
end

module type SIMPLE_PATCH_EDITOR = sig

  type value

  include BASIC_SHAPE_TYPE
  include RETRACTABLE_PATCH_EDITOR
     with type value := value
      and type key = value
      and type patch_out = [`Change of value * value]
      and type patch_in = [`Change of value * value]
      and type shape := shape
      and type ui = Html5_types.flow5 Html5.elt
end

module type SIMPLE_SNAPSHOT_EDITOR = sig
  include BASIC_SHAPE_TYPE
  include SNAPSHOT_EDITOR
    with type shape := shape
     and type ui = Html5_types.flow5 Html5.elt
end

module Simple_PE (Value : SIMPLE_VALUE) :
  SIMPLE_PATCH_EDITOR with type value = Value.t

module Simple_SE (Value : SIMPLE_VALUE) :
  SIMPLE_SNAPSHOT_EDITOR with type value = Value.t

module String_SV : SIMPLE_SNAPSHOT_VIEWER with type value = string
module Int_SV : SIMPLE_SNAPSHOT_VIEWER with type value = int
module Float_SV : SIMPLE_SNAPSHOT_VIEWER with type value = float

module String_PE : SIMPLE_PATCH_EDITOR with type value = string
module Int_PE : SIMPLE_PATCH_EDITOR with type value = int
module Float_PE : SIMPLE_PATCH_EDITOR with type value = float
module String_option_PE : SIMPLE_PATCH_EDITOR with type value = string option
module Int_option_PE : SIMPLE_PATCH_EDITOR with type value = int option
module Float_option_PE : SIMPLE_PATCH_EDITOR with type value = float option

module String_SE : SIMPLE_SNAPSHOT_EDITOR with type value = string
module Int_SE : SIMPLE_SNAPSHOT_EDITOR with type value = int
module Float_SE : SIMPLE_SNAPSHOT_EDITOR with type value = float
module String_option_SE : SIMPLE_SNAPSHOT_EDITOR with type value = string option
module Int_option_SE : SIMPLE_SNAPSHOT_EDITOR with type value = int option
module Float_option_SE : SIMPLE_SNAPSHOT_EDITOR with type value = float option

module Lang_SV : SIMPLE_SNAPSHOT_VIEWER with type value = lang
module Lang_SE : SIMPLE_SNAPSHOT_EDITOR with type value = lang
module Lang_PE : SIMPLE_PATCH_EDITOR with type value = lang
