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

[%%shared.start]

open Html_types

module type Selection = sig

  type ('a, +'tag) elt

  val group : ?disabled: bool -> string ->
              ('a, [< `Option]) elt list -> ('a, [> `Optgroup]) elt

  val (^:) : string ->
             ('a, [< `Option]) elt list -> ('a, [> `Optgroup]) elt

  val unsafe : ?disabled: bool -> string ->
               string -> ('a, [> selectoption]) elt

  val conv : ('a -> string) ->
             ?disabled: bool -> string ->
             'a -> ('a, [> selectoption]) elt

  val string : ?disabled: bool -> string ->
               string -> (string, [> selectoption]) elt

  val bool : ?disabled: bool -> string ->
             bool -> (bool, [> selectoption]) elt

  val int : ?disabled: bool -> string ->
            int -> (int, [> selectoption]) elt

  val int32 : ?disabled: bool -> string ->
              int32 -> (int32, [> selectoption]) elt

  val int64 : ?disabled: bool -> string ->
              int64 -> (int64, [> selectoption]) elt

  val float : ?disabled: bool -> string ->
              float -> (float, [> selectoption]) elt

  val option : (?disabled: bool -> string ->
                'a -> ('a, [> selectoption] as 'tag) elt) ->
               ?disabled: bool -> string ->
               'a option -> ('a option, [> selectoption] as 'tag) elt

  val none : ?disabled: bool -> string ->
             ('a option, [> selectoption]) elt

  val some : (?disabled: bool -> string ->
              'a -> ('a, [> selectoption] as 'tag) elt) ->
             ?disabled: bool -> string ->
             'a -> ('a option, [> selectoption] as 'tag) elt
end
