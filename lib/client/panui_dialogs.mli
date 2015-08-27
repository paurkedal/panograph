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

open Eliom_content.Html5
open Html5_types

module Modal_dialog : sig

  type t

  val open_bare :
	?on_cancel: (unit -> unit) ->
	[< div_content_fun] elt list -> t

  val open_std :
	?on_cancel: (unit -> unit) ->
	[< div_content_fun] elt list ->
	[< div_content_fun] elt list -> t

  val close : t -> unit

  val close_all : unit -> unit

end

val acknowledge_lwt :
      ?ok: button_content_fun elt list ->
      [< div_content_fun] elt list -> unit Lwt.t

val confirm_lwt :
      ?ok: button_content_fun elt list ->
      ?cancel: button_content_fun elt list ->
      [< div_content_fun] elt list -> bool Lwt.t
