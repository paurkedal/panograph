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

{shared{
  type ('a, 'item) item =
    | Item of string * bool * 'a
    | Item_group of string * bool * ('a, [`Item]) item list
    constraint 'item = [< `Item | `Group]

  let item ?(enabled = true) label value =
    Item (label, enabled, value)

  let item_group ?(enabled = true) label items =
    Item_group (label, enabled, items)
}}

{client{
  open Pandom_interactive

  class type ['a] handle = object
    method show : unit
    method hide : unit
    method edit_on : ('a -> ack Lwt.t) -> unit
    method edit_off : unit
    method get : 'a
    method set : 'a -> unit
  end

  class common_handle el = object (self)
    method show = Manip.Class.remove el "pan-hidden"
    method hide = Manip.Class.add el "pan-hidden"
  end

  class ['a] input_handle (to_string : 'a -> string)
			  (of_string : string -> 'a)
			  (emit : ('a -> ack Lwt.t) option)
			  (error : (string option -> unit) option)
			  (init : 'a) el =
  object (self)
    inherit common_handle el

    val mutable value = init
    val mutable absorb = fun _ -> ()

    method edit_on f =
      let inp = D.input ~input_type:`Text () in
      absorb <- outfit_input ~to_string ~of_string ?error ~value inp f;
      Manip.replaceChildren el [inp]

    method edit_off =
      absorb <- (fun x -> Manip.replaceChildren el [D.pcdata (to_string x)]);
      absorb value

    method get = value

    method set x = value <- x; absorb x

    initializer
      match emit with None -> self#edit_off | Some f -> self#edit_on f
  end

  class ['a] select_handle
      (items : ('a, 'item) item list)
      (to_string : 'a -> string)
      (of_string : string -> 'a)
      (emit : ('a -> ack Lwt.t) option)
      (error : (string option -> unit) option)
      (init : 'a) el =
    let label_by_value = Hashtbl.create 11 in
    let options =
      let mk_option label enabled value =
	Hashtbl.add label_by_value value label;
	let a = [D.a_value (to_string value)] in
	let a = if enabled then a else D.a_disabled `Disabled :: a in
	D.option ~a (D.pcdata label) in
      let mk0 = function
	| Item (label, enabled, value) -> mk_option label enabled value
	| Item_group _ -> assert false in
      let mk1 = function
	| Item (label, enabled, value) -> mk_option label enabled value
	| Item_group (label, enabled, items) ->
	  let a = if enabled then [] else [D.a_disabled `Disabled] in
	  D.optgroup ~label ~a (List.map mk0 items) in
      List.map mk1 items in
    let select = D.Raw.select options in
  object (self)
    inherit common_handle el

    val mutable value = init
    val mutable absorb = fun _ -> ()

    method edit_on f =
      absorb <- outfit_select ~to_string ~of_string ?error ~value select f;
      Manip.replaceChildren el [select]

    method edit_off =
      absorb <- begin fun x ->
	let label = Hashtbl.find label_by_value value in
	Manip.replaceChildren el [D.pcdata label]
      end;
      absorb value

    method get = value
    method set x = value <- x; absorb x

    initializer
      match emit with None -> self#edit_off | Some f -> self#edit_on f
  end

  let make_handle = function
    | None -> new input_handle
    | Some items -> new select_handle items
}}

{shared{
  type ('a, 'attrib, 'item) t =
	?to_string: ('a -> string) client_value ->
	?of_string: (string -> 'a) client_value ->
	?items: ('a, 'item) item list ->
	?emit: ('a -> ack Lwt.t) client_value ->
	?error: (string option -> unit) client_value ->
	?a: 'attrib attrib list ->
	'a -> Html5_types.span elt * 'a handle client_value
      constraint 'attrib = [< Html5_types.common]
      constraint 'item = [< `Item | `Group]

  let bool : (bool, 'attrib, 'item) t =
    fun ?(to_string = {{string_of_bool}})
	?(of_string = {{bool_of_string}})
	?(items = [item "true" true; item "false" false])
	?emit ?error
	?a init ->
    let el = D.span ?a [D.pcdata (string_of_bool init)] in
    let h : bool handle client_value =
      {{make_handle (Some %items)
		    %to_string %of_string %emit %error %init %el}} in
    el, h

  let string : (string, 'attrib, 'item) t =
    fun ?(to_string = {{ident}})
	?(of_string = {{ident}})
	?items
	?emit ?error
	?a init ->
    let el = D.span ?a [D.pcdata init] in
    let h : string handle client_value =
      {{make_handle %items %to_string %of_string %emit %error %init %el}} in
    el, h

  let int : (int, 'attrib, 'item) t =
    fun ?(to_string = {{string_of_int}})
	?(of_string = {{int_of_string}})
	?items
	?emit ?error
	?a init ->
    let el = D.span ?a [D.pcdata (string_of_int init)] in
    let h : int handle client_value =
      {{make_handle %items %to_string %of_string %emit %error %init %el}} in
    el, h

  let int32 : (int32, 'attrib, 'item) t =
    fun ?(to_string = {{Int32.to_string}})
	?(of_string = {{Int32.of_string}})
	?items
	?emit ?error
	?a init ->
    let el = D.span ?a [D.pcdata (Int32.to_string init)] in
    let h : int32 handle client_value =
      {{make_handle %items %to_string %of_string %emit %error %init %el}} in
    el, h

  let int64 : (int64, 'attrib, 'item) t =
    fun ?(to_string = {{Int64.to_string}})
	?(of_string = {{Int64.of_string}})
	?items
	?emit ?error
	?a init ->
    let el = D.span ?a [D.pcdata (Int64.to_string init)] in
    let h : int64 handle client_value =
      {{make_handle %items %to_string %of_string %emit %error %init %el}} in
    el, h

  let float : (float, 'attrib, 'item) t =
    fun ?(to_string = {{string_of_float}})
	?(of_string = {{float_of_string}})
	?items
	?emit ?error
	?a init ->
    let el = D.span ?a [D.pcdata (string_of_float init)] in
    let h : float handle client_value =
      {{make_handle %items %to_string %of_string %emit %error %init %el}} in
    el, h

  let bool_option : (bool option, 'attrib, 'item) t =
    fun ?(to_string = {{string_of_option string_of_bool}})
	?(of_string = {{option_of_string bool_of_string}})
	?(items = [item "" None; item "true" (Some true);
				 item "false" (Some false)])
	?emit ?error
	?a init ->
    let el = D.span ?a [D.pcdata (string_of_option string_of_bool init)] in
    let h : bool option handle client_value =
      {{make_handle (Some %items)
		    %to_string %of_string %emit %error %init %el}} in
    el, h

  let string_option : (string option, 'attrib, 'item) t =
    fun ?(to_string = {{string_of_option ident}})
	?(of_string = {{option_of_string ident}})
	?items
	?emit ?error
	?a init ->
    let el = D.span ?a [D.pcdata (string_of_option ident init)] in
    let h : string option handle client_value =
      {{make_handle %items %to_string %of_string %emit %error %init %el}} in
    el, h

  let int_option : (int option, 'attrib, 'item) t =
    fun ?(to_string = {{string_of_option string_of_int}})
	?(of_string = {{option_of_string int_of_string}})
	?items
	?emit ?error
	?a init ->
    let el = D.span ?a [D.pcdata (string_of_option string_of_int init)] in
    let h : int option handle client_value =
      {{make_handle %items %to_string %of_string %emit %error %init %el}} in
    el, h

  let int32_option : (int32 option, 'attrib, 'item) t =
    fun ?(to_string = {{string_of_option Int32.to_string}})
	?(of_string = {{option_of_string Int32.of_string}})
	?items
	?emit ?error
	?a init ->
    let el = D.span ?a [D.pcdata (string_of_option Int32.to_string init)] in
    let h : int32 option handle client_value =
      {{make_handle %items %to_string %of_string %emit %error %init %el}} in
    el, h

  let int64_option : (int64 option, 'attrib, 'item) t =
    fun ?(to_string = {{string_of_option Int64.to_string}})
	?(of_string = {{option_of_string Int64.of_string}})
	?items
	?emit ?error
	?a init ->
    let el = D.span ?a [D.pcdata (string_of_option Int64.to_string init)] in
    let h : int64 option handle client_value =
      {{make_handle %items %to_string %of_string %emit %error %init %el}} in
    el, h

  let float_option : (float option, 'attrib, 'item) t =
    fun ?(to_string = {{string_of_option string_of_float}})
	?(of_string = {{option_of_string float_of_string}})
	?items
	?emit ?error
	?a init ->
    let el = D.span ?a [D.pcdata (string_of_option string_of_float init)] in
    let h : float option handle client_value =
      {{make_handle %items %to_string %of_string %emit %error %init %el}} in
    el, h
}}
