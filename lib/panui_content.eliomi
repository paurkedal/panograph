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

{shared{
  open Panui_content_intf

  module Selection : sig
    type ('a, +'tag) elt = private 'tag Eliom_content.Html5.elt
    type 'a t = ('a, Html5_types.select_content_fun) elt list
    module F : Selection with type ('a, 'tag) elt := ('a, 'tag) elt
    module D : Selection with type ('a, 'tag) elt := ('a, 'tag) elt
  end
}}
