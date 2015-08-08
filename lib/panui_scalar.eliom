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

{shared{
  open Eliom_content.Html5
  open Panograph_prereq
  open Panograph_types
  open Unprime
}}

{server{
  type 'a handle
}}

{client{
  class type ['a] handle = object
    method show : unit
    method hide : unit
    method edit_on : ('a -> ack Lwt.t) -> unit
    method edit_off : unit
    method get : 'a
    method set : 'a -> unit
  end

  class ['a] input_handle (to_string : 'a -> string)
			  (of_string : string -> 'a)
			  (init : 'a) el =
  object (self)
    val mutable value = init
    val mutable absorb = fun _ -> ()

    method show = Manip.Class.remove el "pan-hidden"
    method hide = Manip.Class.add el "pan-hidden"

    method edit_on f =
      let inp = D.input ~input_type:`Text () in
      absorb <- Pandom_interactive.outfit_input ~to_string ~of_string inp f;
      Manip.replaceChildren el [inp]

    method edit_off =
      absorb <- (fun x -> Manip.replaceChildren el [D.pcdata (to_string x)]);
      absorb value

    method get = value

    method set x = absorb x

    initializer self#edit_off
  end
}}

{shared{
  type ('a, 'attrib) t =
	?to_string: ('a -> string) client_value ->
	?of_string: (string -> 'a) client_value ->
	?a: 'attrib attrib list ->
	'a -> Html5_types.span elt * 'a handle client_value
      constraint 'attrib = [< Html5_types.common]

  let string : (string, 'attrib) t =
    fun ?(to_string = {{ident}})
	?(of_string = {{ident}})
	?a init ->
    let el = D.span ?a [D.pcdata init] in
    let h : string handle client_value =
      {{new input_handle %to_string %of_string %init %el}} in
    el, h

  let int : (int, 'attrib) t =
    fun ?(to_string = {{string_of_int}})
	?(of_string = {{int_of_string}})
	?a init ->
    let el = D.span ?a [D.pcdata (string_of_int init)] in
    let h : int handle client_value =
      {{new input_handle %to_string %of_string %init %el}} in
    el, h

  let int32 : (int32, 'attrib) t =
    fun ?(to_string = {{Int32.to_string}})
	?(of_string = {{Int32.of_string}})
	?a init ->
    let el = D.span ?a [D.pcdata (Int32.to_string init)] in
    let h : int32 handle client_value =
      {{new input_handle %to_string %of_string %init %el}} in
    el, h

  let int64 : (int64, 'attrib) t =
    fun ?(to_string = {{Int64.to_string}})
	?(of_string = {{Int64.of_string}})
	?a init ->
    let el = D.span ?a [D.pcdata (Int64.to_string init)] in
    let h : int64 handle client_value =
      {{new input_handle %to_string %of_string %init %el}} in
    el, h

  let float : (float, 'attrib) t =
    fun ?(to_string = {{string_of_float}})
	?(of_string = {{float_of_string}})
	?a init ->
    let el = D.span ?a [D.pcdata (string_of_float init)] in
    let h : float handle client_value =
      {{new input_handle %to_string %of_string %init %el}} in
    el, h

  let string_option : (string option, 'attrib) t =
    fun ?(to_string = {{string_of_option ident}})
	?(of_string = {{option_of_string ident}})
	?a init ->
    let el = D.span ?a [D.pcdata (string_of_option ident init)] in
    let h : string option handle client_value =
      {{new input_handle %to_string %of_string %init %el}} in
    el, h

  let int_option : (int option, 'attrib) t =
    fun ?(to_string = {{string_of_option string_of_int}})
	?(of_string = {{option_of_string int_of_string}})
	?a init ->
    let el = D.span ?a [D.pcdata (string_of_option string_of_int init)] in
    let h : int option handle client_value =
      {{new input_handle %to_string %of_string %init %el}} in
    el, h

  let int32_option : (int32 option, 'attrib) t =
    fun ?(to_string = {{string_of_option Int32.to_string}})
	?(of_string = {{option_of_string Int32.of_string}})
	?a init ->
    let el = D.span ?a [D.pcdata (string_of_option Int32.to_string init)] in
    let h : int32 option handle client_value =
      {{new input_handle %to_string %of_string %init %el}} in
    el, h

  let int64_option : (int64 option, 'attrib) t =
    fun ?(to_string = {{string_of_option Int64.to_string}})
	?(of_string = {{option_of_string Int64.of_string}})
	?a init ->
    let el = D.span ?a [D.pcdata (string_of_option Int64.to_string init)] in
    let h : int64 option handle client_value =
      {{new input_handle %to_string %of_string %init %el}} in
    el, h

  let float_option : (float option, 'attrib) t =
    fun ?(to_string = {{string_of_option string_of_float}})
	?(of_string = {{option_of_string float_of_string}})
	?a init ->
    let el = D.span ?a [D.pcdata (string_of_option string_of_float init)] in
    let h : float option handle client_value =
      {{new input_handle %to_string %of_string %init %el}} in
    el, h
}}
