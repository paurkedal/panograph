(* Copyright (C) 2015--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

open Operated_types
open Presentation_types
open Unprime

module type PRESENTABLE_SET = sig
  type elt
  type t
  val locate : elt -> t -> bool * int
  val add : elt -> t -> t
  val remove : elt -> t -> t
  val elements : t -> elt list
end

module Make (Set : PRESENTABLE_SET) = struct
  type t = Set.t
  type dt = Set.elt set_patch
  type p = Set.elt list
  type dp = (Set.elt, counit) grid1_op

  let present = Set.elements

  let change = function
    | Set_add e -> fun s ->
      let is_present, pos = Set.locate e s in
      if is_present then raise (Conflict "Element to insert exists.");
      `Insert (pos, e), Set.add e s
    | Set_remove e -> fun s ->
      let is_present, pos = Set.locate e s in
      if not is_present then raise (Conflict "Element to remove is missing.");
      `Delete pos, Set.remove e s
end
