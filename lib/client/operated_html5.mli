(* Copyright (C) 2015  Petter A. Urkedal <paurkedal@gmail.com>
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

  val p : ([< p_attrib],
	   'ev, 'ep, [< p_content_fun],
	   'ev list, ('ev, 'ep) grid1_op, [> p]) star

  val pre : ([< pre_attrib],
	     'ev, 'ep, [< pre_content_fun],
	     'ev list, ('ev, 'ep) grid1_op, [> pre]) star

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

  val dl : ([< dl_attrib],
	    'ev, 'ep, [< dl_content_fun],
	    'ev list, ('ev, 'ep) grid1_op, [> dl]) star

  val table :
    ?caption: [< caption] Html5.elt ->
    ?columns: [< colgroup] Html5.elt list ->
    ?thead: [< thead] Html5.elt ->
    ?tfoot: [< tfoot] Html5.elt ->
    ([< table_attrib],
     'ev, 'ep, [< table_content_fun],
     'ev list, ('ev, 'ep) grid1_op, [> table]) star

  val tr : ([< tr_attrib],
	    'ev, 'ep, [< tr_content_fun],
	    'ev list, ('ev, 'ep) grid1_op, [> tr]) star

  val select : ([< select_attrib],
		'ev, 'ep, [< select_content_fun],
		'ev list, ('ev, 'ep) grid1_op, [> select]) star

  val optgroup : label: string ->
		 ([< optgroup_attrib],
		  'ev, 'ep, [< optgroup_content_fun],
		  'ev list, ('ev, 'ep) grid1_op, [> optgroup]) star
end
