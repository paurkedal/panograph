(* Copyright (C) 2015--2016  Petter A. Urkedal <paurkedal@gmail.com>
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

[%%shared

  open Panui_content_intf
  open Unprime

  module type Make_selection_param = sig

    module Svg :
      Svg_sigs.T
        with module Xml := Eliom_content.Xml
        with type +'a elt = 'a Eliom_content.Svg.elt
         and type 'a wrap = 'a
         and type 'a list_wrap = 'a list
         and type +'a attrib = 'a Eliom_content.Svg.attrib

    module Html5 :
      Html5_sigs.T
        with module Xml := Eliom_content.Xml
        with module Svg := Svg
         and type +'a elt = 'a Eliom_content.Html5.elt
         and type 'a wrap = 'a
         and type 'a list_wrap = 'a list
         and type +'a attrib = 'a Eliom_content.Html5.attrib
  end

  module Make_selection (Content : Make_selection_param) = struct
    include Content

    let group ?(disabled = false) label opts =
      let a = if disabled then [Html5.a_disabled `Disabled] else [] in
      Html5.optgroup ~label ~a opts

    let (^:) label opts = group label opts

    let unsafe ?(disabled = false) label value =
      let a = [Html5.a_value value] in
      let a = if disabled then Html5.a_disabled `Disabled :: a else a in
      Html5.option ~a (Html5.pcdata label)

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
    type ('a, 'tag) elt = 'tag Eliom_content.Html5.elt
    type 'a t = ('a, Html5_types.select_content_fun) elt list
    module F =
      Make_selection (struct
        module Svg = Eliom_content.Svg.F
        module Html5 = Eliom_content.Html5.F.Raw
      end)
    module D =
      Make_selection (struct
        module Svg = Eliom_content.Svg.D
        module Html5 = Eliom_content.Html5.D.Raw
      end)
  end

]
