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
open Presentation_sigs
open Presentation_types

module type PRESENTABLE_MAP = sig
  type key
  type 'a t
  val find : key -> 'a t -> 'a
  val locate : key -> 'a t -> bool * int
  val add : key -> 'a -> 'a t -> 'a t
  val remove : key -> 'a t -> 'a t
  val bindings : 'a t -> (key * 'a) list
end

module Make (Map : PRESENTABLE_MAP) (E : PRESENTATION) :
  PRESENTATION
    with type t = E.t Map.t
     and type dt = (Map.key, E.t, E.dt) map_patch
     and type p = (Map.key * E.p) list
     and type dp = (Map.key * E.p, E.dp) grid1_op
