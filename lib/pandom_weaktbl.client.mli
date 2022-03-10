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

(** Semi-weak imperative maps over DOM elements.

    JavaScript does not have weak references, but we can attach a value to a DOM
    element and find it later whenever the element is still in the current
    document. *)

open Js_of_ocaml

type ('e, 'a) t constraint 'e = #Dom_html.element Js.t

val create : unit -> ('e, 'a) t
(** This constructor essentially creates a unique identifier which will be used
    to locate the inserted elements within the DOM. The DOM itself is contains
    the collection. *)

val add : ('e, 'a) t -> 'e -> 'a -> unit
(** [add wt k v] adds [v] to the element [k] under an anonymous key associated
    with [wt]. *)

val remove : ('e, 'a) t -> 'e -> unit
(** [remove wt k] removes any value stored on [k] as part of [wt]. *)

val find : ('e, 'a) t -> 'e -> 'a option
(** [find wt k] is the value, if any, stored on [k] as part of [wt]. *)

val iter : ?top: #Dom_html.element Js.t ->
           ('e -> 'a -> unit) -> ('e, 'a) t -> unit
(** [iter f wt] calls [f k v] for each binding of [wt]
    @param top Delimits the iteration to [top] and elements below. *)

val fold : ?top: #Dom_html.element Js.t ->
           ('e -> 'a -> 'b -> 'b) -> ('e, 'a) t -> 'b -> 'b
(** [fold f wt] is the composition of [f k v] over bindings of [wt].
    @param top Delimits the composition to [top] and elements below. *)

val for_all : ?top: #Dom_html.element Js.t ->
              ('e -> 'a -> bool) -> ('e, 'a) t -> bool
(** [for_all f wt] is true iff [f k v] is true for all bindings of [wt].
    @param top Delimits the conjunction to [top] and elements below. *)

val exists : ?top: #Dom_html.element Js.t ->
             ('e -> 'a -> bool) -> ('e, 'a) t -> bool
(** [exists f w] is true iff [f k v] is true for at least one binding of [wt].
    @param top Delimits the disjunction to [top] and elements below. *)
