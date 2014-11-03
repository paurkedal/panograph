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

module type SPAN_TREE = sig
  type t
  type tabular
  val is_root : t -> bool
  val is_leaf : t -> bool
  val up : t -> t option
  val first : t -> t option
  val last : t -> t option
  val next : t -> t option
  val prev : t -> t option
  val add_first : tabular -> t -> t
  val add_last : tabular -> t -> t
  val add_before : tabular -> t -> t
  val add_after : tabular -> t -> t
  val delete : tabular -> t -> unit
end

module Tabular : sig
  type t

  module Rowspan : SPAN_TREE with type tabular := t
  module Colspan : SPAN_TREE with type tabular := t

  val create : unit -> t
  val ui : t -> [> `Table] Html5.elt
  val root_rowspan : t -> Rowspan.t
  val root_colspan : t -> Colspan.t

  val refine : t -> int -> int -> Rowspan.t -> Colspan.t -> unit
  val draw_th : t -> Rowspan.t -> Colspan.t ->
		Html5_types.th_content Html5.elt list -> unit
  val draw_td : t -> Rowspan.t -> Colspan.t ->
		Html5_types.td_content Html5.elt list -> unit
end
