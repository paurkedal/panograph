(* Copyright (C) 2014--2016  Petter A. Urkedal <paurkedal@gmail.com>
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

open Eliom_client
open Eliom_content
open Panograph_basic_editors
open Panograph_prereq
open Panograph_types
open Unprime_option

[%%client
  module Dep_sca = Panui_scalar
  open Panograph_types
]

let string_stream, string_out' = Lwt_stream.create ()
let string_comet = Eliom_comet.Channel.create ~scope:`Site string_stream
let string_out = server_function [%json: string] @@ fun x ->
  Lwt_log.debug_f "Received string \"%s\"." x >>
  Lwt_unix.sleep 0.3 >>
  (string_out' (Some (String.uppercase x)); Lwt.return Ack_ok)

let int_stream, int_out' = Lwt_stream.create ()
let int_comet = Eliom_comet.Channel.create ~scope:`Site int_stream
let int_out = server_function [%json: int] @@ fun x ->
  Lwt_log.debug_f "Received int %d." x >>
  Lwt_unix.sleep 0.3 >>
  (int_out' (Some (2 * x)); Lwt.return Ack_ok)

let float_stream, float_out' = Lwt_stream.create ()
let float_comet = Eliom_comet.Channel.create ~scope:`Site float_stream
let float_out = server_function [%json: float] @@ fun x ->
  Lwt_log.debug_f "Received float %g." x >>
  Lwt_unix.sleep 0.3 >>
  (float_out' (Some (1.0 /. x)); Lwt.return Ack_ok)

let bool_stream, bool_out' = Lwt_stream.create ()
let bool_comet = Eliom_comet.Channel.create ~scope:`Site bool_stream
let bool_out = server_function [%json: bool] @@ fun x ->
  Lwt_log.debug_f "Received bool %b." x >>
  Lwt_unix.sleep 0.3 >>
  (bool_out' (Some x); Lwt.return Ack_ok)

let bool_option_stream, bool_option_out' = Lwt_stream.create ()
let bool_option_comet =
  Eliom_comet.Channel.create ~scope:`Site bool_option_stream
let bool_option_out = server_function [%json: bool option] @@ fun x_opt ->
  begin match x_opt with
  | None -> Lwt_log.debug_f "Received bool option None."
  | Some x -> Lwt_log.debug_f "Received bool option %b." x
  end >>
  Lwt_unix.sleep 0.3 >>
  (bool_option_out' (Some (Option.map not x_opt)); Lwt.return Ack_ok)

let int_option_stream, int_option_out' = Lwt_stream.create ()
let int_option_comet = Eliom_comet.Channel.create ~scope:`Site int_option_stream
let int_option_out = server_function [%json: int option] @@ fun x_opt ->
  begin match x_opt with
  | None -> Lwt_log.debug "Received int option None."
  | Some x -> Lwt_log.debug_f "Received int option Some %d." x
  end >>
  Lwt_unix.sleep 0.3 >>
  (int_option_out' (Some (Option.map succ x_opt)); Lwt.return Ack_ok)

let render () =
  let open Html in

  let string_ed, h = Panui_scalar.string "" in
  ignore_cv [%client
    (~%h#edit_on : (string -> ack Lwt.t) -> unit) ~%string_out;
    Lwt.async (fun () -> Lwt_stream.iter ~%h#set ~%string_comet) ];

  let int_ed, h = Panui_scalar.int 0 in
  ignore_cv [%client
    (~%h#edit_on : (int -> ack Lwt.t) -> unit) ~%int_out;
                Lwt.async (fun () -> Lwt_stream.iter ~%h#set ~%int_comet) ];

  let int_select_ed, h =
    let opts = Panui_scalar.[opt "Zero" 0; opt "One" 1;
                             opt "Two" 2; opt "Three" 3] in
    Panui_scalar.int ~opts 0 in
  ignore_cv [%client
    (~%h#edit_on : (int -> ack Lwt.t) -> unit) ~%int_out;
                Lwt.async (fun () -> Lwt_stream.iter ~%h#set ~%int_comet) ];

  let float_ed, h = Panui_scalar.float 0.0 in
  ignore_cv [%client
    (~%h#edit_on : (float -> ack Lwt.t) -> unit) ~%float_out;
                Lwt.async (fun () -> Lwt_stream.iter ~%h#set ~%float_comet) ];

  let bool1_ed, h = Panui_scalar.bool false in
  ignore_cv [%client
    (~%h#edit_on : (bool -> ack Lwt.t) -> unit) ~%bool_out;
                Lwt.async (fun () -> Lwt_stream.iter ~%h#set ~%bool_comet) ];

  let bool2_ed, h =
    Panui_scalar.(bool ~opts:[opt "yes" true; opt "no" false] false) in
  ignore_cv [%client
    (~%h#edit_on : (bool -> ack Lwt.t) -> unit) ~%bool_out;
                Lwt.async (fun () -> Lwt_stream.iter ~%h#set ~%bool_comet) ];

  let bool_option_ed, h = Panui_scalar.bool_option None in
  ignore_cv [%client
    (~%h#edit_on : (bool option -> ack Lwt.t) -> unit) ~%bool_option_out;
    Lwt.async (fun () -> Lwt_stream.iter ~%h#set ~%bool_option_comet)
  ];

  let int_option_ed, h = Panui_scalar.int_option None in
  ignore_cv [%client
    (~%h#edit_on : (int option -> ack Lwt.t) -> unit) ~%int_option_out;
    Lwt.async @@ fun () -> Lwt_stream.iter ~%h#set ~%int_option_comet ];

  D.div [
    D.h2 [D.pcdata "Server Side Inputs"];
    D.ul [
      D.li [D.pcdata "string: "; string_ed];
      D.li [D.pcdata "int: "; int_ed];
      D.li [D.pcdata "int select: "; int_select_ed];
      D.li [D.pcdata "float: "; float_ed];
      D.li [D.pcdata "bool: "; bool1_ed];
      D.li [D.pcdata "bool: "; bool2_ed];
      D.li [D.pcdata "bool option: "; bool_option_ed];
      D.li [D.pcdata "int option: "; int_option_ed];
    ]
  ]
