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
  type ('a, 'opt) opt =
    | Opt of string option * bool * 'a
    | Optgroup of string * bool * ('a, [`Opt]) opt list
    constraint 'opt = [< `Opt | `Optgroup]

  let opt ?(enabled = true) label value = Opt (Some label, enabled, value)
  let optv ?(enabled = true) value = Opt (None, enabled, value)
  let optgroup ?(enabled = true) label opts = Optgroup (label, enabled, opts)
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
      (opts : ('a, 'opt) opt list)
      (to_string : 'a -> string)
      (of_string : string -> 'a)
      (emit : ('a -> ack Lwt.t) option)
      (error : (string option -> unit) option)
      (init : 'a) el =
    let label_by_value = Hashtbl.create 11 in
    let options =
      let mk_option label enabled value =
	let label = match label with Some s -> s | None -> to_string value in
	Hashtbl.add label_by_value value label;
	let a = [D.a_value (to_string value)] in
	let a = if enabled then a else D.a_disabled `Disabled :: a in
	D.option ~a (D.pcdata label) in
      let mk0 = function
	| Opt (label, enabled, value) -> mk_option label enabled value
	| Optgroup _ -> assert false in
      let mk1 = function
	| Opt (label, enabled, value) -> mk_option label enabled value
	| Optgroup (label, enabled, opts) ->
	  let a = if enabled then [] else [D.a_disabled `Disabled] in
	  D.optgroup ~label ~a (List.map mk0 opts) in
      List.map mk1 opts in
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
    | Some opts -> new select_handle opts

  let add_input_with_handle ~to_string ~of_string ?opts ?emit ?error init el =
    make_handle opts to_string of_string emit error init el
}}

{shared{
  type ('a, 'opt, 'attrib, 'elt) t =
	?to_string: ('a -> string) client_value ->
	?of_string: (string -> 'a) client_value ->
	?opts: ('a, 'opt) opt list ->
	?emit: ('a -> ack Lwt.t) client_value ->
	?error: (string option -> unit) client_value ->
	?a: 'attrib attrib list ->
	'a -> 'elt elt * 'a handle client_value
      constraint 'attrib = [< Html5_types.common > `Class]
      constraint 'opt = [< `Opt | `Optgroup]
      constraint 'elt = [> `Span]

  let bool : (bool, 'opt, 'attrib, 'elt) t =
    fun ?(to_string = {{string_of_bool}})
	?(of_string = {{bool_of_string}})
	?(opts = [opt "true" true; opt "false" false])
	?emit ?error
	?(a = [D.a_class ["pan-scalar"; "bool"]]) init ->
    let el = D.span ~a [D.pcdata (string_of_bool init)] in
    let h : bool handle client_value =
      {{make_handle (Some %opts)
		    %to_string %of_string %emit %error %init %el}} in
    el, h

  let string : (string, 'opt, 'attrib, 'elt) t =
    fun ?(to_string = {{ident}})
	?(of_string = {{ident}})
	?opts
	?emit ?error
	?(a = [D.a_class ["pan-scalar"; "string"]]) init ->
    let el = D.span ~a [D.pcdata init] in
    let h : string handle client_value =
      {{make_handle %opts %to_string %of_string %emit %error %init %el}} in
    el, h

  let int : (int, 'opt, 'attrib, 'elt) t =
    fun ?(to_string = {{string_of_int}})
	?(of_string = {{int_of_string}})
	?opts
	?emit ?error
	?(a = [D.a_class ["pan-scalar"; "int"]]) init ->
    let el = D.span ~a [D.pcdata (string_of_int init)] in
    let h : int handle client_value =
      {{make_handle %opts %to_string %of_string %emit %error %init %el}} in
    el, h

  let int32 : (int32, 'opt, 'attrib, 'elt) t =
    fun ?(to_string = {{Int32.to_string}})
	?(of_string = {{Int32.of_string}})
	?opts
	?emit ?error
	?(a = [D.a_class ["pan-scalar"; "int32"]]) init ->
    let el = D.span ~a [D.pcdata (Int32.to_string init)] in
    let h : int32 handle client_value =
      {{make_handle %opts %to_string %of_string %emit %error %init %el}} in
    el, h

  let int64 : (int64, 'opt, 'attrib, 'elt) t =
    fun ?(to_string = {{Int64.to_string}})
	?(of_string = {{Int64.of_string}})
	?opts
	?emit ?error
	?(a = [D.a_class ["pan-scalar"; "int64"]]) init ->
    let el = D.span ~a [D.pcdata (Int64.to_string init)] in
    let h : int64 handle client_value =
      {{make_handle %opts %to_string %of_string %emit %error %init %el}} in
    el, h

  let float : (float, 'opt, 'attrib, 'elt) t =
    fun ?(to_string = {{string_of_float}})
	?(of_string = {{float_of_string}})
	?opts
	?emit ?error
	?(a = [D.a_class ["pan-scalar"; "float"]]) init ->
    let el = D.span ~a [D.pcdata (string_of_float init)] in
    let h : float handle client_value =
      {{make_handle %opts %to_string %of_string %emit %error %init %el}} in
    el, h

  let bool_option : (bool option, 'opt, 'attrib, 'elt) t =
    fun ?(to_string = {{string_of_option string_of_bool}})
	?(of_string = {{option_of_string bool_of_string}})
	?(opts = [opt "" None; opt "true" (Some true);
			       opt "false" (Some false)])
	?emit ?error
	?(a = [D.a_class ["pan-scalar"; "bool"; "option"]]) init ->
    let el = D.span ~a [D.pcdata (string_of_option string_of_bool init)] in
    let h : bool option handle client_value =
      {{make_handle (Some %opts)
		    %to_string %of_string %emit %error %init %el}} in
    el, h

  let string_option : (string option, 'opt, 'attrib, 'elt) t =
    fun ?(to_string = {{string_of_option ident}})
	?(of_string = {{option_of_string ident}})
	?opts
	?emit ?error
	?(a = [D.a_class ["pan-scalar"; "string"; "option"]])  init ->
    let el = D.span ~a [D.pcdata (string_of_option ident init)] in
    let h : string option handle client_value =
      {{make_handle %opts %to_string %of_string %emit %error %init %el}} in
    el, h

  let int_option : (int option, 'opt, 'attrib, 'elt) t =
    fun ?(to_string = {{string_of_option string_of_int}})
	?(of_string = {{option_of_string int_of_string}})
	?opts
	?emit ?error
	?(a = [D.a_class ["pan-scalar"; "int"; "option"]]) init ->
    let el = D.span ~a [D.pcdata (string_of_option string_of_int init)] in
    let h : int option handle client_value =
      {{make_handle %opts %to_string %of_string %emit %error %init %el}} in
    el, h

  let int32_option : (int32 option, 'opt, 'attrib, 'elt) t =
    fun ?(to_string = {{string_of_option Int32.to_string}})
	?(of_string = {{option_of_string Int32.of_string}})
	?opts
	?emit ?error
	?(a = [D.a_class ["pan-scalar"; "int32"; "option"]]) init ->
    let el = D.span ~a [D.pcdata (string_of_option Int32.to_string init)] in
    let h : int32 option handle client_value =
      {{make_handle %opts %to_string %of_string %emit %error %init %el}} in
    el, h

  let int64_option : (int64 option, 'opt, 'attrib, 'elt) t =
    fun ?(to_string = {{string_of_option Int64.to_string}})
	?(of_string = {{option_of_string Int64.of_string}})
	?opts
	?emit ?error
	?(a = [D.a_class ["pan-scalar"; "int64"; "option"]]) init ->
    let el = D.span ~a [D.pcdata (string_of_option Int64.to_string init)] in
    let h : int64 option handle client_value =
      {{make_handle %opts %to_string %of_string %emit %error %init %el}} in
    el, h

  let float_option : (float option, 'opt, 'attrib, 'elt) t =
    fun ?(to_string = {{string_of_option string_of_float}})
	?(of_string = {{option_of_string float_of_string}})
	?opts
	?emit ?error
	?(a = [D.a_class ["pan-scalar"; "float"; "option"]]) init ->
    let el = D.span ~a [D.pcdata (string_of_option string_of_float init)] in
    let h : float option handle client_value =
      {{make_handle %opts %to_string %of_string %emit %error %init %el}} in
    el, h
}}
