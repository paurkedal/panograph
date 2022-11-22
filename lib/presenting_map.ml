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

module Make (Map : PRESENTABLE_MAP) (Elt : PRESENTATION) = struct
  type t = Elt.t Map.t
  type dt = (Map.key, Elt.t, Elt.dt) map_patch
  type p = (Map.key * Elt.p) list
  type dp = (Map.key * Elt.p, Elt.dp) grid1_op

  let present m = List.map (fun (k, e) -> (k, Elt.present e)) (Map.bindings m)

  let change = function
    | Map_add (k, e) -> fun m ->
      let is_present, pos = Map.locate k m in
      if is_present then raise (Conflict "Key already bound in map.");
      `Insert (pos, (k, Elt.present e)), Map.add k e m
    | Map_remove k -> fun m ->
      let is_present, pos = Map.locate k m in
      if not is_present then raise (Conflict "Missing key to remove.");
      `Delete pos, Map.remove k m
    | Map_at (k, p) -> fun m ->
      let is_present, pos = Map.locate k m in
      if not is_present then raise (Conflict "Missing key to modify.");
      let e = Map.find k m in
      let c, e' = Elt.change p e in
      `Update (pos, c), Map.add k e' m
end
