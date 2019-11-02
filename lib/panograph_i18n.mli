(* Copyright (C) 2014--2019  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Internationalization and localization. *)

type lang = Iso639.Lang.t [@@deriving json]

module Lang : sig
  type t = lang

  val of_string : string -> lang
  val to_string : lang -> string
  val equal : lang -> lang -> bool
  val compare : lang -> lang -> int
end

module Lang_map : Prime_enummap.S with type key = lang

type twine = string Lang_map.t

module Twine : sig
  type t = twine
  val make : (lang * string) list -> t
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val to_string : langs: lang list -> t -> string

  type sym_patch = string Lang_map.t * string Lang_map.t
                 * (string * string) Lang_map.t
  val sym_diff : t -> t -> sym_patch
  val sym_patch : ?strategy: [`Theirs | `Ours] -> sym_patch -> t -> t
end

val check_utf8_exn : string -> unit
