(* Copyright (C) 2014--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

open Eliom_client
open Eliom_content
open Lwt.Infix
open Panograph_prereq
open Unprime_option

let string_stream, string_out' = Lwt_stream.create ()
let string_comet = Eliom_comet.Channel.create ~scope:`Site string_stream
let string_out = server_function [%json: string] @@ fun x ->
  Lwt_log.debug_f "Received string \"%s\"." x >>= fun () ->
  Lwt_unix.sleep 0.3 >>= fun () ->
  string_out' (Some (String.uppercase_ascii x));
  Lwt.return (Panui_result.ok ())

let int_stream, int_out' = Lwt_stream.create ()
let int_comet = Eliom_comet.Channel.create ~scope:`Site int_stream
let int_out = server_function [%json: int] @@ fun x ->
  Lwt_log.debug_f "Received int %d." x >>= fun () ->
  Lwt_unix.sleep 0.3 >>= fun () ->
  int_out' (Some (2 * x));
  Lwt.return (Panui_result.ok ())

let float_stream, float_out' = Lwt_stream.create ()
let float_comet = Eliom_comet.Channel.create ~scope:`Site float_stream
let float_out = server_function [%json: float] @@ fun x ->
  Lwt_log.debug_f "Received float %g." x >>= fun () ->
  Lwt_unix.sleep 0.3 >>= fun () ->
  float_out' (Some (1.0 /. x));
  Lwt.return (Panui_result.ok ())

let bool_stream, bool_out' = Lwt_stream.create ()
let bool_comet = Eliom_comet.Channel.create ~scope:`Site bool_stream
let bool_out = server_function [%json: bool] @@ fun x ->
  Lwt_log.debug_f "Received bool %b." x >>= fun () ->
  Lwt_unix.sleep 0.3 >>= fun () ->
  bool_out' (Some x);
  Lwt.return (Panui_result.ok ())

let bool_option_stream, bool_option_out' = Lwt_stream.create ()
let bool_option_comet =
  Eliom_comet.Channel.create ~scope:`Site bool_option_stream
let bool_option_out = server_function [%json: bool option] @@ fun x_opt ->
  (match x_opt with
   | None -> Lwt_log.debug_f "Received bool option None."
   | Some x -> Lwt_log.debug_f "Received bool option %b." x) >>= fun () ->
  Lwt_unix.sleep 0.3 >>= fun () ->
  bool_option_out' (Some (Option.map not x_opt));
  Lwt.return (Panui_result.ok ())

let int_option_stream, int_option_out' = Lwt_stream.create ()
let int_option_comet = Eliom_comet.Channel.create ~scope:`Site int_option_stream
let int_option_out = server_function [%json: int option] @@ fun x_opt ->
  (match x_opt with
   | None -> Lwt_log.debug "Received int option None."
   | Some x -> Lwt_log.debug_f "Received int option Some %d." x) >>= fun () ->
  Lwt_unix.sleep 0.3 >>= fun () ->
  int_option_out' (Some (Option.map succ x_opt));
  Lwt.return (Panui_result.ok ())

let textarea_stream, textarea_out' = Lwt_stream.create ()
let textarea_comet = Eliom_comet.Channel.create ~scope:`Site textarea_stream
let textarea_out = server_function [%json: string] @@ fun x ->
  Lwt_log.debug_f "Received string \"%s\"." x >>= fun () ->
  Lwt_unix.sleep 0.3 >>= fun () ->
  textarea_out' (Some (String.uppercase_ascii x));
  Lwt.return (Panui_result.ok ())

let int_select_opts =
  Panui_scalar.[opt "Zero" 0; opt "One" 1; opt "Two" 2; opt "Three" 3]

let handler () () =
  let open Html in

  let string_ed, h = Panui_scalar.string "" in
  ignore_cv [%client
    (~%h#edit_on : (string -> unit Panui_result.t Lwt.t) -> unit) ~%string_out;
    Lwt.async (fun () -> Lwt_stream.iter ~%h#set ~%string_comet) ];

  let int_ed, h = Panui_scalar.int 0 in
  ignore_cv [%client
    (~%h#edit_on : (int -> unit Panui_result.t Lwt.t) -> unit) ~%int_out;
                Lwt.async (fun () -> Lwt_stream.iter ~%h#set ~%int_comet) ];

  let int_select_ed, h = Panui_scalar.int ~opts:int_select_opts 0 in
  ignore_cv [%client
    (~%h#edit_on : (int -> unit Panui_result.t Lwt.t) -> unit) ~%int_out;
                Lwt.async (fun () -> Lwt_stream.iter ~%h#set ~%int_comet) ];

  let int_select_view, h = Panui_scalar.int ~opts:int_select_opts 0 in
  ignore_cv [%client Lwt.async (fun () -> Lwt_stream.iter ~%h#set ~%int_comet)];

  let float_ed, h = Panui_scalar.float 0.0 in
  ignore_cv [%client
    (~%h#edit_on : (float -> unit Panui_result.t Lwt.t) -> unit) ~%float_out;
                Lwt.async (fun () -> Lwt_stream.iter ~%h#set ~%float_comet) ];

  let bool1_ed, h = Panui_scalar.bool false in
  ignore_cv [%client
    (~%h#edit_on : (bool -> unit Panui_result.t Lwt.t) -> unit) ~%bool_out;
                Lwt.async (fun () -> Lwt_stream.iter ~%h#set ~%bool_comet) ];

  let bool2_ed, h =
    Panui_scalar.(bool ~opts:[opt "yes" true; opt "no" false] false) in
  ignore_cv [%client
    (~%h#edit_on : (bool -> unit Panui_result.t Lwt.t) -> unit) ~%bool_out;
                Lwt.async (fun () -> Lwt_stream.iter ~%h#set ~%bool_comet) ];

  let bool_option_ed, h = Panui_scalar.bool_option None in
  ignore_cv [%client
    (~%h#edit_on :
      (bool option -> unit Panui_result.t Lwt.t) -> unit) ~%bool_option_out;
    Lwt.async (fun () -> Lwt_stream.iter ~%h#set ~%bool_option_comet)
  ];

  let bool_option_view, h = Panui_scalar.bool_option None in
  ignore_cv [%client
    Lwt.async (fun () -> Lwt_stream.iter ~%h#set ~%bool_option_comet) ];

  let int_option_ed, h = Panui_scalar.int_option None in
  ignore_cv [%client
    (~%h#edit_on :
      (int option -> unit Panui_result.t Lwt.t) -> unit) ~%int_option_out;
    Lwt.async @@ fun () -> Lwt_stream.iter ~%h#set ~%int_option_comet ];

  let int_option_view, h = Panui_scalar.int_option None in
  ignore_cv [%client
    Lwt.async @@ fun () -> Lwt_stream.iter ~%h#set ~%int_option_comet ];

  let textarea_ed, h = Panui_scalar.string_textarea "" in
  ignore_cv [%client
    (~%h#edit_on : (string -> unit Panui_result.t Lwt.t) -> unit)
      ~%textarea_out;
    Lwt.async (fun () -> Lwt_stream.iter ~%h#set ~%textarea_comet) ];

  Lwt.return [
    D.h2 [D.txt "Server Side Inputs"];
    D.h3 [D.txt "On-Liners"];
    D.ul [
      D.li [D.txt "string: "; string_ed];
      D.li [D.txt "int: "; int_ed];
      D.li [D.txt "int select: "; int_select_ed];
      D.li [D.txt "int select: "; int_select_view];
      D.li [D.txt "float: "; float_ed];
      D.li [D.txt "bool: "; bool1_ed];
      D.li [D.txt "bool: "; bool2_ed];
      D.li [D.txt "bool option: "; bool_option_ed];
      D.li [D.txt "bool option: "; bool_option_view];
      D.li [D.txt "int option: "; int_option_ed];
      D.li [D.txt "int option: "; int_option_view];
    ];
    D.h3 [D.txt "Text Area"];
    textarea_ed;
  ]

[%%client.start]
[@@@warning "-33"]
open Panui_scalar
