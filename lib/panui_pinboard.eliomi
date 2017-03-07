(* Copyright (C) 2016--2017  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Transient messaging. *)

[%%shared.start]
open Eliom_content.Html

type item
type t

type subject_content = Html_types.flow5

val create : ?freeze_on_hover: bool -> ?freeze_timeout: float -> unit -> t

val ui : t -> [> Html_types.table] elt

[%%client.start]

val pin : subject: [< subject_content] elt list ->
          ?level: Lwt_log_js.level ->
          ?timeout: float ->
          t -> item

val unpin : item -> t -> unit
