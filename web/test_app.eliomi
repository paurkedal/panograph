(* Copyright (C) 2022  Petter A. Urkedal <paurkedal@gmail.com>
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

open Eliom_content
open Eliom_service

val create_page :
  path: ('att, 'co, 'gp_) path_option ->
  meth: ('m, 'gp, 'gn, 'pp, 'pn,
         [< `WithSuffix | `WithoutSuffix] as 'a, 'gp_) meth ->
  title: string ->
  ('gp -> 'pp -> Html_types.body_content_fun Html.elt list Lwt.t) ->
  ('gp, 'pp, 'm, 'att, 'co, non_ext, reg, 'a, 'gn, 'pn, non_ocaml) t

val create_test :
  name: string ->
  ?title: string ->
  (unit -> unit -> Html_types.body_content_fun Html.elt list Lwt.t) ->
  string * (unit, unit, get, att, non_co, non_ext, reg,
            [`WithoutSuffix], unit, unit, non_ocaml) t
