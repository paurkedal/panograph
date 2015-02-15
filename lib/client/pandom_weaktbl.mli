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

type ('e, 'a) t constraint 'e = #Dom_html.element Js.t

val create : unit -> ('e, 'a) t

val add : ('e, 'a) t -> 'e -> 'a -> unit

val remove : ('e, 'a) t -> 'e -> unit

val find : ('e, 'a) t -> 'e -> 'a option

val iter : ?top: #Dom_html.element Js.t ->
	   ('e -> 'a -> unit) -> ('e, 'a) t -> unit

val fold : ?top: #Dom_html.element Js.t ->
	   ('e -> 'a -> 'b -> 'b) -> ('e, 'a) t -> 'b -> 'b

val for_all : ?top: #Dom_html.element Js.t ->
	      ('e -> 'a -> bool) -> ('e, 'a) t -> bool

val exists : ?top: #Dom_html.element Js.t ->
	     ('e -> 'a -> bool) -> ('e, 'a) t -> bool
