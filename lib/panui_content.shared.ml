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

open Unprime

module Make_selection (Html : module type of Eliom_content.Html.F.Raw) = struct
  let group ?(disabled = false) label opts =
    let a = if disabled then [Html.a_disabled ()] else [] in
    Html.optgroup ~label ~a opts

  let (^:) label opts = group label opts

  let unsafe ?(disabled = false) label value =
    let a = [Html.a_value value] in
    let a = if disabled then Html.a_disabled () :: a else a in
    Html.option ~a (Html.txt label)

  let conv to_string ?disabled label x = unsafe ?disabled label (to_string x)
  let string = conv ident
  let bool = conv string_of_bool
  let int = conv string_of_int
  let int32 = conv Int32.to_string
  let int64 = conv Int64.to_string
  let float = conv string_of_float
  let none ?disabled label = unsafe ?disabled label ""
  let some f ?disabled label x = f ?disabled label x
  let option f ?disabled label = function
    | None -> none ?disabled label
    | Some x -> f ?disabled label x
end

module Selection = struct
  type ('a, 'tag) elt = 'tag Eliom_content.Html.elt
  type 'a t = ('a, Html_types.select_content_fun) elt list
  module F = Make_selection (Eliom_content.Html.F.Raw)
  module D = Make_selection (Eliom_content.Html.D.Raw)
end
