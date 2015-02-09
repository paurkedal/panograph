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

open Eliom_content
open Html5_types
open Operated_types

module O : sig

  type ('attr, 'ev, 'ep, 'eu, 'cv, 'cp, 'cu) star =
    ?a: ('attr Html5.attrib) list ->
    ?intro: 'eu Html5.elt list ->
    ('ev -> ('ep -> unit) * 'eu Html5.elt list) ->
    'cv -> ('cp -> unit) * 'cu Html5.elt

  val div : ([< div_attrib],
	    'ev, 'ep, [< div_content_fun],
	    'ev list, ('ev, 'ep) grid1_op, [> div]) star

  val span : ([< span_attrib],
	    'ev, 'ep, [< span_content_fun],
	    'ev list, ('ev, 'ep) grid1_op, [> span]) star

  val ul : ([< ul_attrib],
	    'ev, 'ep, [< ul_content_fun],
	    'ev list, ('ev, 'ep) grid1_op, [> ul]) star

  val ol : ([< ol_attrib],
	    'ev, 'ep, [< ol_content_fun],
	    'ev list, ('ev, 'ep) grid1_op, [> ol]) star

end
