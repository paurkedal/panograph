(* Copyright (C) 2015--2021  Petter A. Urkedal <paurkedal@gmail.com>
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
  val emit : 'a t -> key -> ?step: React.Step.t -> 'a -> unit
  val size : 'a t -> int
end

exception Retained : 'a -> exn

let never_emit ?step x = assert false

module Make (Key : Hashtbl.HashedType) = struct
  type key = Key.t
  type 'a node = Key.t * 'a React.E.t * (?step: React.step -> 'a -> unit)

  type _ t =
    Pack : (module Weak.S with type data = 'a node and type t = 'wt) * 'wt ->
           'a t

  let create (type a) n : a t =
    let module Wt =
      Weak.Make (struct
        type t = a node
        let hash (k, _, _) = Key.hash k
        let equal (k1, _, _) (k2, _, _) = Key.equal k1 k2
      end) in
    Pack ((module Wt), Wt.create n)

  let event (type a) (Pack ((module Wt), wt) : a t) key =
    try
      let _, ev, _ = Wt.find wt (key, React.E.never, never_emit) in
      ev
    with Not_found ->
      let ev, emit = React.E.create () in
      let node = (key, ev, emit) in
      let `R _ = React.E.retain ev (fun () -> raise (Retained node)) in
      Wt.add wt node; ev

  let event_opt (type a) (Pack ((module Wt), et) : a t) key =
    try
      let _, ev, _ = Wt.find et (key, React.E.never, never_emit) in
      Some ev
    with Not_found ->
      None

  let emit (type a) (Pack ((module Wt), wt) : a t) key ?step (x : a) =
    try
      let _, _, emit = Wt.find wt (key, React.E.never, never_emit) in
      emit ?step x
    with Not_found -> ()

  let size (type a) (Pack ((module Wt), wt) : a t) = Wt.count wt
end
