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

(** Messaging system with listeners bound to DOM elements. *)

type ('k, 'a) t
(** A channel dispatching on ['k] and delivering messages of type ['a]. *)

type subscription
(** A handle to unsubscribe a listener from a channel. *)

val create : ('k -> string) -> ('k, 'a) t
(** Given a function [classify], which mangles a key to an identifier, [create
    classify] creates a channel which dispatches messages by the domain of
    [classify]. *)

val subscribe_class : ('k, 'a) t -> #Dom_html.element Js.t ->
		      'k -> ('a -> unit) -> subscription
(** [subscribe_class chan el k f] adds [f] as a listener for messages on
    [chan] classified by [k] and returns a handle to use for unsubscribing.
    This function adds a CSS class to [el] to identify the listener, and the
    listener will only receive messages when [el] is in the current DOM
    document. *)

val subscribe_id : ('k, 'a) t -> #Dom_html.element Js.t ->
		   'k -> ('a -> unit) -> subscription
(** [subscribe_class chan el k f] adds [f] as a listener for messages on
    [chan] classified by [k] and returns a handle to use for unsubscribing.
    This function sets the [id] attribute of [el], and the listener will only
    receive messages when [el] is in the current DOM document. *)

val unsubscribe : subscription -> unit
(** [unsubscribe h] removes the subscription which returned [h]. *)

val send : ('k, 'a) t -> 'k -> 'a -> unit
(** [send chan k v] calls [f v] for each [f] subscribed to [chan] under the
    key [k]. *)
