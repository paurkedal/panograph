(* Copyright (C) 2017--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Helpers for [('a, ]{!Panui_error.t}[) result]. *)

type 'a t = ('a, Panui_error.t) result

val ok : 'a -> 'a t

val error : ?tags: string list -> ?doc: string -> string -> 'a t

val error_f :
  ?tags: string list -> ?doc: string ->
  ('a, Format.formatter, unit, 'b t) format4 -> 'a

val catch : ?tags: string list -> ?doc: string -> msg: string ->
            ?report: (exn -> unit) ->
            (unit -> 'a) -> 'a t

val catch_lwt : ?tags: string list -> ?doc: string -> msg: string ->
                ?report: (exn -> unit Lwt.t) ->
                (unit -> 'a Lwt.t) -> 'a t Lwt.t

val invalid_input : ?doc: string -> string -> 'a t

val missing_input : ?doc: string -> string -> 'a t

val convert_input : ?tags: string list -> ?doc: string -> ?msg: string ->
                    ('a -> 'b) -> 'a -> 'b t

val require_input : ?tags: string list -> ?doc: string -> ?msg: string ->
                    'a option -> 'a t

val of_msg : ('a, [< `Msg of string]) result -> 'a t
