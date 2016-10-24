(* Copyright (C) 2016  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Dispatch events indexed by integers. *)

type 'a t

val create : int React.signal -> (int * 'a) React.event -> 'a t
(** [create m e] is a dispatcher of events [e] with modulus [2^m]. The modulus
    only affects efficiency. Optimally the range of indices is dense, and [2^m]
    is on the order of the largest index. *)

val get : 'a t -> int -> 'a React.event
(** [get a i] receives every event of [a] with index [i]. *)

val hold : ?eq: ('a -> 'a -> bool) -> 'a t -> int -> 'a -> 'a React.signal
(** [signal a i x] is [React.S.hold x (get a i)]. *)
