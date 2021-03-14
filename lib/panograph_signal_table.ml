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

open Unprime_option

module type S = sig
  type key
  type 'a t
  val create : int -> 'a t
  val signal : 'a t -> key -> 'a -> 'a React.S.t
  val signal_opt : 'a t -> key -> 'a React.S.t option
  val value_opt : 'a t -> key -> 'a option
  val set : 'a t -> key -> 'a -> unit
  val size : 'a t -> int
end

exception Retained : 'a -> exn

module Make (Key : Hashtbl.HashedType) = struct

  type key = Key.t

  type 'a node =
    | Key of Key.t
    | Node of Key.t * 'a React.S.t * (?step: React.step -> 'a -> unit)

  let node_key = function
   | Key k -> k
   | Node (k, _, _) -> k

  type _ t =
    Pack : (module Weak.S with type data = 'a node and type t = 'wt) * 'wt ->
           'a t

  let create (type a) n : a t =
    let module Wt =
      Weak.Make (struct
        type t = a node
        let hash node = Key.hash (node_key node)
        let equal node1 node2 = Key.equal (node_key node1) (node_key node2)
      end)
    in
    Pack ((module Wt), Wt.create n)

  let signal (type a) (Pack ((module Wt), wt) : a t) key value =
    (match Wt.find wt (Key key) with
     | Key _ -> assert false
     | Node (_, sn, _) -> sn
     | exception Not_found ->
        let sn, send = React.S.create value in
        let node = Node (key, sn, send) in
        let `R _ = React.S.retain sn (fun () -> raise (Retained node)) in
        Wt.add wt node; sn)

  let signal_opt (type a) (Pack ((module Wt), wt) : a t) key =
    (match Wt.find wt (Key key) with
     | Key _ -> assert false
     | Node (_, sn, _) -> Some sn
     | exception Not_found -> None)

  let value_opt (wt : 'a t) key = Option.map React.S.value (signal_opt wt key)

  let set (type a) (Pack ((module Wt), wt) : a t)  key value =
    (match Wt.find wt (Key key) with
     | Key _ -> assert false
     | Node (_, _, set) -> set value
     | exception Not_found -> ())

  let size (type a) (Pack ((module Wt), wt) : a t) = Wt.count wt
end
