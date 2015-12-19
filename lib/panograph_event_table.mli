(* Copyright (C) 2015  Petter A. Urkedal <paurkedal@gmail.com>
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

(** A weak table of events. *)

module type S = sig
  type key
  type 'a t

  val create : int -> 'a t
  (** [create size] creates a weak event hash table with initial size [size].
      For best performance, use round the size to the nearest prime. *)

  val event : 'a t -> key -> 'a React.E.t
  (** [event wt k] returns an event which will receive [x] whenever [emit k x]
      is called.  A new event will be added to the table at [k] if not
      present. *)

  val event_opt : 'a t -> key -> 'a React.E.t option
  (** [event_opt wt k] is [Some (event wt k)] if [k] is already associated
      with an event, otherwise [None]. *)

  val emit : 'a t -> key -> 'a -> unit
  (** [emit wt k x] sends [x] to the event at [k] in [wt] if present,
      otherwise does nothing. *)

  val size : 'a t -> int
  (** [size wt] is the current size of the hash table. *)
end

module Make (Key : Hashtbl.HashedType) : S with type key = Key.t
