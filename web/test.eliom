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

open Eliom_content.Html

module C : sig
  include module type of C
  val div : [`Div] elt Eliom_client_value.t -> [> `Div] elt
  val table : [`Table] elt Eliom_client_value.t -> [> `Table] elt
  val ul : [`Ul] elt Eliom_client_value.t -> [> `Ul] elt
end = struct
  include C
  let div c = (C.node c : [`Div] elt :> [> `Div] elt)
  let table c = (C.node c : [`Table] elt :> [> `Table] elt)
  let ul c = (C.node c : [`Ul] elt :> [> `Ul] elt)
end

let create_test_c_div ~name (f : (unit -> [< `Div] elt) Eliom_client_value.t) =
  Test_app.create_test ~name (fun () () -> Lwt.return [C.div [%client ~%f ()]])
let create_test_c_table ~name (f : (unit -> [< `Table] elt) Eliom_client_value.t) =
  Test_app.create_test ~name (fun () () -> Lwt.return [C.table [%client ~%f ()]])
let create_test_c_ul ~name (f : (unit -> [< `Ul] elt) Eliom_client_value.t) =
  Test_app.create_test ~name (fun () () -> Lwt.return [C.ul [%client ~%f ()]])

let test_services = Test_app.[
  create_test ~name:"complete" Test_complete.handler;
  create_test ~name:"content_with_edit" Test_content_with_edit.handler;
  create_test ~name:"dialogs" Test_dialogs.handler;
  create_test_c_div ~name:"finalizer" [%client Test_finalizer.render];
  create_test_c_div ~name:"inputs" [%client Test_inputs.render];
  create_test_c_table ~name:"tabular1" [%client Test_tabular1.render];
  create_test_c_table ~name:"tabular2" [%client Test_tabular2.render];
  create_test_c_table ~name:"tabular3" [%client Test_tabular3.render];
  create_test ~name:"operated" Test_operated.handler;
  create_test ~name:"pinboard" Test_pinboard.handler;
  create_test ~name:"scalar" Test_scalar.handler;
  create_test_c_ul ~name:"weakchan" [%client Test_weakchan.render];
  create_test_c_div ~name:"weaktbl" [%client Test_weaktbl.render];
  create_test ~name:"twine_editor" Test_twine_editor.handler;
]

let main_handler () () =
  let test_service_item (name, service) =
    F.li [F.a ~service [F.txt name] ()] in
  Lwt.return [F.ul (List.map test_service_item test_services)]

let _main_service =
  let open Eliom_service in
  Test_app.create_page
    ~path:(Path []) ~meth:(Get Eliom_parameter.unit)
    ~title:"Panograph Tests" main_handler

[%%client.start]
[@@@warning "-33"]
open Test_complete
open Test_content_with_edit
open Test_dialogs
open Test_finalizer
open Test_twine_editor
open Test_operated
open Test_pinboard
open Test_scalar
open Test_weakchan
open Test_weaktbl
