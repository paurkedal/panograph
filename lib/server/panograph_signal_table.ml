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

open Unprime_option

type ex_spair =
  Ex_spair : 'a React.S.t * ('a -> unit) -> ex_spair

let dummy = Ex_spair (React.S.const (), (fun () -> assert false))

let enclose x () = ignore x

module Make (Key : Hashtbl.HashedType) = struct

  module Node = struct
    type t = Key.t * ex_spair
    let hash (key, _) = Key.hash key
    let equal (k1, _) (k2, _) = Key.equal k1 k2
  end

  module Wt = Weak.Make (Node)

  type 'a t = Wt.t

  let create = Wt.create

  let signal (wt : 'a t) key x : 'a React.S.t =
    let (_, Ex_spair (signal, _)) =
      try
	Wt.find wt (key, dummy)
      with Not_found ->
	let signal, set = React.S.create x in
	let node = (key, Ex_spair (signal, set)) in
	let `R _ = React.S.retain signal (enclose node) in
	Wt.add wt node; node in
    Obj.magic signal

  let signal_opt (wt : 'a t) key : 'a React.S.t option =
    try
      let (_, Ex_spair (signal, _)) = Wt.find wt (key, dummy) in
      Some (Obj.magic signal : 'a React.S.t)
    with Not_found ->
      None

  let value_opt (wt : 'a t) key = Option.map React.S.value (signal_opt wt key)

  let set (wt : 'a t) key (x : 'a) =
    try
      let (_, Ex_spair (_, set)) = Wt.find wt (key, dummy) in
      (Obj.magic set : 'a -> unit) x
    with Not_found ->
      ()

  let size = Wt.count
end
