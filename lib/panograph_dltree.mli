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

module Dltree : sig
  type 'a t

  val make : 'a -> 'a t

  val is_root : 'a t -> bool
  val is_leaf : 'a t -> bool
  val is_first : 'a t -> bool
  val is_last : 'a t -> bool
  val is_only : 'a t -> bool

  val get : 'a t -> 'a
  val set : 'a -> 'a t -> unit

  val up : 'a t -> 'a t option
  val first : 'a t -> 'a t option
  val last : 'a t -> 'a t option
  val next : 'a t -> 'a t option
  val prev : 'a t -> 'a t option

  val level : 'a t -> int
  val left_compare : 'a t -> 'a t -> int

  val first_leaf : 'a t -> 'a t
  val last_leaf : 'a t -> 'a t

  val fold : ?depth: int -> ('a t -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val iter : ?depth: int ->('a t -> unit) -> 'a t -> unit
  val exists : ?depth: int -> ('a t -> bool) -> 'a t -> bool
  val fold_ancestors : ('a t -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val iter_ancestors : ('a t -> unit) -> 'a t -> unit

  val add_first : 'a -> 'a t -> 'a t
  val add_last : 'a -> 'a t -> 'a t
  val add_before : 'a -> 'a t -> 'a t
  val add_after : 'a -> 'a t -> 'a t
  val delete_subtree : 'a t -> unit
end
