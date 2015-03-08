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

module type S = sig
  type key
  type 'a t
  val create : int -> 'a t
  val event : 'a t -> key -> 'a React.E.t
  val event_opt : 'a t -> key -> 'a React.E.t option
  val emit : 'a t -> key -> 'a -> unit
  val size : 'a t -> int
end

type ex_epair =
  Ex_epair : 'a React.E.t * ('a -> unit) -> ex_epair

let ex_epair_dummy = Ex_epair (React.E.never, (fun _ -> assert false))

let enclose x () = ignore x

module Make (Key : Hashtbl.HashedType) = struct

  type key = Key.t

  module Node = struct
    type t = Key.t * ex_epair
    let hash (key, _) = Key.hash key
    let equal (k1, _) (k2, _) = Key.equal k1 k2
  end

  module Wt = Weak.Make (Node)

  type 'a t = Wt.t

  let create = Wt.create

  let event (wt : 'a t) key : 'a React.E.t =
    let (_, Ex_epair (event, _)) =
      try
	Wt.find wt (key, ex_epair_dummy)
      with Not_found ->
	let event, emit = React.E.create () in
	let node = (key, Ex_epair (event, emit)) in
	let `R _ = React.E.retain event (enclose node) in
	Wt.add wt node; node in
    Obj.magic event (*[1]*)

  let event_opt (wt : 'a t) key : 'a React.E.t option =
    try
      let (_, Ex_epair (event, _)) = Wt.find wt (key, ex_epair_dummy) in
      Some (Obj.magic event : 'a React.E.t)
    with Not_found ->
      None

  let emit (wt : 'a t) key (x : 'a) =
    try
      let (_, Ex_epair (_, emit)) = Wt.find wt (key, ex_epair_dummy) in
      Obj.magic emit x (*[1]*)
    with Not_found ->
      ()

  let size wt = Wt.count wt
end

(* [1] The Weak module has monomorphic elements, so we need to introduce
 * polymorphism by swearing. *)
