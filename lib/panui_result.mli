(* Copyright (C) 2017  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Helpers for [('a, ]{!Panui_error.t}[) result]. *)

type 'a t = ('a, Panui_error.t) result

val ok : 'a -> 'a t

val error : ?tags: string list -> ?doc: string -> string -> 'a t

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