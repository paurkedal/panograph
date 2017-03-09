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

(** Descriptive errors.

    This module provides an abstract type to represent informative descriptions
    of errors.  In more complex cases of error handling, this is meant as the
    final stage where an explanation is prepared for the end-user. *)

type t

val create : ?tags: string list -> ?doc: string -> string -> t
(** [create ?tags ?doc msg] creates an error concisely described by [msg], which
    should be a one-liner, or at most, short paragraph. [?doc] provides further
    documentation related to the error, and [?tags] serve to classify the error.

    {b Note}: Some kind of markdown format may be implemented for the [doc] and
    [msg] arguments, so don't pass funny characters there yet. *)

val tags : t -> string list

val message : t -> string

val doc : t -> string option
