(* Copyright (C) 2015  Petter Urkedal <paurkedal@gmail.com>
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
  type key = Map.key
  type t = Elt.t Map.t
  type dt = (Map.key, Elt.t, Elt.dt) map_patch
  type p = (Map.key * Elt.p) list
  type dp = (Map.key * Elt.p, Elt.dp) grid1_op

  let present m = List.map (fun (k, e) -> (k, Elt.present e)) (Map.bindings m)

  let change = function
    | Map_add (k, e) -> fun m ->
      let is_present, pos = Map.locate k m in
      if is_present then raise (Conflict "Key already bound in map.");
      Grid1_insert (pos, (k, Elt.present e)), Map.add k e m
    | Map_remove k -> fun m ->
      let is_present, pos = Map.locate k m in
      if not is_present then raise (Conflict "Missing key to remove.");
      Grid1_delete pos, Map.remove k m
    | Map_at (k, p) -> fun m ->
      let is_present, pos = Map.locate k m in
      if not is_present then raise (Conflict "Missing key to modify.");
      let e = Map.find k m in
      let c, e' = Elt.change p e in
      Grid1_at (pos, c), Map.add k e' m
end
