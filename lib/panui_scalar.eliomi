(* Copyright (C) 2015--2021  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Editors and viewers for elementary types. *)

[%%shared.start]

open Eliom_content.Html
open Panograph_types

type ('a, +'opt) opt constraint 'opt = [< `Opt | `Optgroup]

type common_input_attrib =
  [ Html_types.common | `Autofocus | `Disabled | `Size | `Required ]

[%%server.start]

type 'a handle

[%%client.start]

class type ['a] handle = object
  method show : unit
  method hide : unit
  method edit_on : ('a -> unit Panui_result.t Lwt.t) -> unit
  method edit_off : unit
  method get : 'a
  method set : 'a -> unit
end

type input_or_select_parent = [
  | `Abbr | `Article | `Aside | `Blockquote | `B | `Body
  | `Caption | `Cite | `Code
  | `Dd | `Del | `Details | `Dfn | `Div | `Dt | `Em
  | `Figcaption | `Figure | `Form
  | `H1 | `H2 | `H3 | `H4 | `H5 | `H6
  | `I | `Ins | `Li | `Main | `Mark | `Nav | `P | `Pre | `Q
  | `Samp | `Section | `Small | `Span | `Strong | `Sub | `Summary | `Sup
  | `Td | `Th | `Time | `U | `Var
]

val inject_input :
  to_string: ('a -> string) ->
  of_string: (string -> 'a) ->
  ?emit: ('a -> unit Panui_result.t Lwt.t) ->
  ?error: (string option -> unit) ->
  ?a: [< Html_types.input_attrib] attrib list ->
  'a -> [< input_or_select_parent] elt ->
  'a handle

val inject_textarea :
  to_string: ('a -> string) ->
  of_string: (string -> 'a) ->
  ?to_html: ('a -> [< Html_types.div_content] elt list) ->
  ?emit: ('a -> unit Panui_result.t Lwt.t) ->
  ?error: (string option -> unit) ->
  ?a: [< Html_types.textarea_attrib] attrib list ->
  'a -> [< input_or_select_parent] elt ->
  'a handle

val inject_select :
  to_string: ('a -> string) ->
  of_string: (string -> 'a) ->
  opts: ('a, 'opt) opt list ->
  ?emit: ('a -> unit Panui_result.t Lwt.t) ->
  ?error: (string option -> unit) ->
  ?a: [< Html_types.select_attrib] attrib list ->
  'a -> [< input_or_select_parent] elt ->
  'a handle

val inject_input_or_select :
  to_string: ('a -> string) ->
  of_string: (string -> 'a) ->
  ?opts: ('a, 'opt) opt list ->
  ?emit: ('a -> unit Panui_result.t Lwt.t) ->
  ?error: (string option -> unit) ->
  ?a: [< common_input_attrib] attrib list ->
  'a -> [< input_or_select_parent] elt -> 'a handle

val add_input_with_handle :
      to_string: ('a -> string) ->
      of_string: (string -> 'a) ->
      ?opts: ('a, 'opt) opt list ->
      ?emit: ('a -> unit Panui_result.t Lwt.t) ->
      ?error: (string option -> unit) ->
      ?a: [< common_input_attrib] attrib list ->
      'a -> [< `Span | `Div | `Td] elt -> 'a handle
[@@deprecated "Renamed to inject_input_or_select."]

[%%shared.start]

val opt : ?enabled: bool -> string -> 'a -> ('a, [> `Opt]) opt
val optv : ?enabled: bool -> 'a -> ('a, [> `Opt]) opt
val optgroup : ?enabled: bool -> string ->
               ('a, [`Opt]) opt list -> ('a, [> `Optgroup]) opt

type ('a, 'opt, 'attrib, 'input_attrib, 'elt) t =
      ?to_string: ('a -> string) Eliom_client_value.t ->
      ?of_string: (string -> 'a) Eliom_client_value.t ->
      ?opts: ('a, 'opt) opt list ->
      ?emit: ('a -> unit Panui_result.t Lwt.t) Eliom_client_value.t ->
      ?error: (string option -> unit) Eliom_client_value.t ->
      ?a: 'attrib attrib list ->
      ?input_a: 'input_attrib attrib list ->
      'a -> 'elt elt * 'a handle Eliom_client_value.t
    constraint 'attrib = [< Html_types.common > `Class]
    constraint 'input_attrib = [< common_input_attrib]
    constraint 'opt = [< `Opt | `Optgroup]
    constraint 'elt = [> `Span]

val bool : (bool, 'opt, 'attrib, 'input_attrib, 'elt) t
val string : (string, 'opt, 'attrib, 'input_attrib, 'elt) t
val int : (int, 'opt, 'attrib, 'input_attrib, 'elt) t
val int32 : (int32, 'opt, 'attrib, 'input_attrib, 'elt) t
val int64 : (int64, 'opt, 'attrib, 'input_attrib, 'elt) t
val float : (float, 'opt, 'attrib, 'input_attrib, 'elt) t

val bool_option : (bool option, 'opt, 'attrib, 'input_attrib, 'elt) t
val string_option : (string option, 'opt, 'attrib, 'input_attrib, 'elt) t
val int_option : (int option, 'opt, 'attrib, 'input_attrib, 'elt) t
val int32_option : (int32 option, 'opt, 'attrib, 'input_attrib, 'elt) t
val int64_option : (int64 option, 'opt, 'attrib, 'input_attrib, 'elt) t
val float_option : (float option, 'opt, 'attrib, 'input_attrib, 'elt) t

val textarea :
  ?to_string: (string -> string) Eliom_client_value.t ->
  ?of_string: (string -> string) Eliom_client_value.t ->
  ?to_html:
    (string -> [< Html_types.div_content] elt list Eliom_client_value.t) ->
  ?emit: (string -> unit Panui_result.t Lwt.t) Eliom_client_value.t ->
  ?error: (string option -> unit) Eliom_client_value.t ->
  ?a: [< Html_types.div_attrib > `Class] attrib list ->
  ?input_a: [< Html_types.textarea_attrib] attrib list ->
  string -> [> Html_types.div] elt * string handle Eliom_client_value.t

val textarea_option :
  ?to_string: (string option -> string) Eliom_client_value.t ->
  ?of_string: (string -> string option) Eliom_client_value.t ->
  ?to_html:
    (string option ->
      [< Html_types.div_content] elt list Eliom_client_value.t) ->
  ?emit: (string option -> unit Panui_result.t Lwt.t) Eliom_client_value.t ->
  ?error: (string option -> unit) Eliom_client_value.t ->
  ?a: [< Html_types.div_attrib > `Class] attrib list ->
  ?input_a: [< Html_types.textarea_attrib] attrib list ->
  string option ->
  [> Html_types.div] elt * string option handle Eliom_client_value.t
