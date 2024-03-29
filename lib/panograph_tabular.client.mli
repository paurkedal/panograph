(* Copyright (C) 2014--2022  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the LGPL-3.0 Linking Exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * and the LGPL-3.0 Linking Exception along with this library.  If not, see
 * <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.
 *)

(** Dynamic table with nested row and column spans. *)

open Eliom_content
open Js_of_ocaml

module type SPAN_TREE = sig
  type t
  type tabular

  val level : t -> int
  val is_root : t -> bool
  val is_leaf : t -> bool
  val is_first : t -> bool
  val is_last : t -> bool
  val is_only : t -> bool

  val up : t -> t option
  val first : t -> t option
  val last : t -> t option
  val next : t -> t option
  val prev : t -> t option

  val first_leaf : t -> t
  val last_leaf : t -> t

  val fold : ?depth: int -> (t -> 'a -> 'a) -> t -> 'a -> 'a
  val iter : ?depth: int -> (t -> unit) -> t -> unit
  val iteri : ?depth: int -> (int -> t -> unit) -> t -> unit
  val iterp : depth: int -> (int list -> t -> unit) -> t -> unit
  val exists : ?depth: int -> (t -> bool) -> t -> bool

  val add_first : tabular -> ?css_class: string -> t -> t
  val add_last : tabular -> ?css_class: string -> t -> t
  val add_before : tabular -> ?css_class: string -> t -> t
  val add_after : tabular -> ?css_class: string -> t -> t
  val delete : tabular -> t -> unit
end

module Tabular : sig
  type t

  type state =
    | Leaf
    | Split of int * int
    | Invalid

  module Rowspan : SPAN_TREE with type tabular := t
  module Colspan : SPAN_TREE with type tabular := t

  val create : ?a: [< Html_types.table_attrib] Html.attrib list ->
               ?root_css_class: string -> unit -> t
  val validate : t -> unit
  val ui : t -> [> `Table] Html.elt
  val root_rowspan : t -> Rowspan.t
  val root_colspan : t -> Colspan.t

  val state : t -> Rowspan.t -> Colspan.t -> state
  val refine : t -> int -> int -> Rowspan.t -> Colspan.t -> unit
  val draw : t -> Rowspan.t -> Colspan.t ->
             [< Html_types.tr_content] Html.elt -> unit
  val draw_th : t -> Rowspan.t -> Colspan.t ->
                ?a: [< Html_types.th_attrib] Html.attrib list ->
                [< Html_types.th_content] Html.elt list -> unit
  val draw_td : t -> Rowspan.t -> Colspan.t ->
                ?a: [< Html_types.td_attrib] Html.attrib list ->
                [< Html_types.td_content] Html.elt list -> unit

  val find_tr :
    t -> Rowspan.t -> Dom_html.tableRowElement Js.t option
  val find_td :
    t -> Rowspan.t -> Colspan.t -> Dom_html.tableCellElement Js.t option
end
