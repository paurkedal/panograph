(* Copyright (C) 2014--2021  Petter A. Urkedal <paurkedal@gmail.com>
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
open Lwt.Infix
open Panograph_basic_editors [@@ocaml.warning "-3"]
open Panograph_prereq
open Panograph_types
open Unprime_option

[%%client
  module Dep_pbe = Panograph_basic_editors [@@ocaml.warning "-3"]
]

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

let int_option_stream, int_option_out' = Lwt_stream.create ()
let int_option_comet = Eliom_comet.Channel.create ~scope:`Site int_option_stream
let int_option_out = server_function [%json: int option] @@ fun x_opt ->
  begin match x_opt with
  | None -> Lwt_log.debug "Received int option None."
  | Some x -> Lwt_log.debug_f "Received int option Some %d." x
  end >>= fun () ->
  Lwt_unix.sleep 0.3 >>= fun () ->
  int_option_out' (Some (Option.map succ x_opt));
  Lwt.return (Panui_result.ok ())

let bool_option_stream, bool_option_out' = Lwt_stream.create ()
let bool_option_comet =
  Eliom_comet.Channel.create ~scope:`Site bool_option_stream
let bool_option_out = server_function [%json: bool option] @@ fun x_opt ->
  begin match x_opt with
  | None -> Lwt_log.debug "Received bool option None from select."
  | Some x -> Lwt_log.debug_f "Received bool option %b from select." x
  end >>= fun () ->
  Lwt_unix.sleep 0.3 >>= fun () ->
  bool_option_out' (Some (Option.map not x_opt));
  Lwt.return (Panui_result.ok ())

let int_option_stream, int_option_out' = Lwt_stream.create ()
let int_option_comet =
  Eliom_comet.Channel.create ~scope:`Site int_option_stream
let int_option_out = server_function [%json: int option] @@ fun x_opt ->
  begin match x_opt with
  | None -> Lwt_log.debug "Received int option None from select."
  | Some x -> Lwt_log.debug_f "Received int option %d from select." x
  end >>= fun () ->
  Lwt_unix.sleep 0.3 >>= fun () ->
  int_option_out' (Some (Option.map succ x_opt));
  Lwt.return (Panui_result.ok ())

let string_option_menu_stream, string_option_menu_out' = Lwt_stream.create ()
let string_option_menu_comet =
  Eliom_comet.Channel.create ~scope:`Site string_option_menu_stream
let string_option_menu_out =
        server_function [%json: string option] @@ fun x_opt ->
  begin match x_opt with
  | None -> Lwt_log.debug "Selected string option None."
  | Some x -> Lwt_log.debug_f "Selected string option Some %s." x
  end >>= fun () ->
  Lwt_unix.sleep 0.3 >>= fun () ->
  let swap = function "Sun" -> "Earth" | "Earth" -> "Sun" | x -> x in
  string_option_menu_out' (Some (Option.map swap x_opt));
  Lwt.return (Panui_result.ok ())

let string_option_textarea_stream, string_option_textarea_out' = Lwt_stream.create ()
let string_option_textarea_comet =
  Eliom_comet.Channel.create ~scope:`Site string_option_textarea_stream
let string_option_textarea_out =
        server_function [%json: string option] @@ fun x_opt ->
  begin match x_opt with
  | None -> Lwt_log.debug "Selected string option None."
  | Some x -> Lwt_log.debug_f "Selected string option Some %s." x
  end >>= fun () ->
  Lwt_unix.sleep 0.3 >>= fun () ->
  let tr x = String.uppercase_ascii x in
  string_option_textarea_out' (Some (Option.map tr x_opt));
  Lwt.return (Panui_result.ok ())

let handler () () =
  let open Html in

  let string_ed, string_in =
    string_editor [%client ~%string_out] [@@ocaml.warning "-3"] in
  ignore_cv [%client Lwt.async (fun () -> Lwt_stream.iter ~%string_in ~%string_comet)];

  let int_ed, int_in =
    int_editor [%client ~%int_out ] [@@ocaml.warning "-3"] in
  ignore_cv [%client Lwt.async (fun () -> Lwt_stream.iter ~%int_in ~%int_comet)];

  let float_ed, float_in =
    float_editor [%client ~%float_out ] [@@ocaml.warning "-3"] in
  ignore_cv [%client Lwt.async (fun () -> Lwt_stream.iter ~%float_in ~%float_comet)];

  let int_option_ed, int_option_in =
    int_option_editor [%client ~%int_option_out] [@@ocaml.warning "-3"] in
  ignore_cv [%client Lwt.async @@ fun () ->
    Lwt_stream.iter ~%int_option_in ~%int_option_comet];

  let bool_option_ed, bool_option_in =
    bool_option_selector ~false_label:"false" ~true_label:"true"
                         [%client  ~%bool_option_out] [@@ocaml.warning "-3"] in
  ignore_cv [%client Lwt.async @@ fun () ->
    Lwt_stream.iter ~%bool_option_in ~%bool_option_comet];

  let int_option_ed, int_option_in =
    int_option_selector ~items:[None, [0, "zero", true; 1, "one", true];
                                Some "g", [2, "two", true; 3, "three", false]]
                        [%client ~%int_option_out] [@@ocaml.warning "-3"] in
  ignore_cv [%client Lwt.async @@ fun () ->
    Lwt_stream.iter ~%int_option_in ~%int_option_comet];

  let string_option_menu, string_option_menu_in =
    string_option_menu ~values:["Earth"; "Sun"; "The Milky Way"]
      [%client ~%string_option_menu_out] [@@ocaml.warning "-3"] in
  ignore_cv [%client Lwt.async @@ fun () ->
    Lwt_stream.iter ~%string_option_menu_in ~%string_option_menu_comet];

  let string_option_textarea, string_option_textarea_in =
    string_option_textarea [%client ~%string_option_textarea_out]
      [@@ocaml.warning "-3"] in
  ignore_cv [%client Lwt.async @@ fun () ->
    Lwt_stream.iter ~%string_option_textarea_in ~%string_option_textarea_comet];

  Lwt.return [
    D.h2 [D.txt "Server Side Inputs"];
    D.ul [
      D.li [string_ed];
      D.li [int_ed];
      D.li [float_ed];
      D.li [int_option_ed];
      D.li [bool_option_ed];
      D.li [string_option_menu];
      D.li [string_option_textarea];
    ]
  ]
