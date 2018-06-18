(* Copyright (C) 2014--2018  Petter A. Urkedal <paurkedal@gmail.com>
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

open Eliom_content.Html

module%client Linked = struct
  module Test_complete = Test_complete
  module Test_content_with_edit = Test_content_with_edit
  module Test_dialogs = Test_dialogs
  module Test_finalizer = Test_finalizer
  module Test_basic_editors = Test_basic_editors
  module Test_twine_editor = Test_twine_editor
  module Test_operated = Test_operated
  module Test_pinboard = Test_pinboard
  module Test_scalar = Test_scalar
  module Test_weakchan = Test_weakchan
  module Test_weaktbl = Test_weaktbl
end

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

let create_test_c_div name (f : (unit -> [< `Div] elt) Eliom_client_value.t) =
  Test_app.create_test name (fun () () -> Lwt.return [C.div [%client ~%f ()]])
let create_test_c_table name (f : (unit -> [< `Table] elt) Eliom_client_value.t) =
  Test_app.create_test name (fun () () -> Lwt.return [C.table [%client ~%f ()]])
let create_test_c_ul name (f : (unit -> [< `Ul] elt) Eliom_client_value.t) =
  Test_app.create_test name (fun () () -> Lwt.return [C.ul [%client ~%f ()]])

let test_services = Test_app.[
  create_test "complete" Test_complete.handler;
  create_test "content_with_edit" Test_content_with_edit.handler;
  create_test "dialogs" Test_dialogs.handler;
  create_test_c_div "finalizer" [%client Test_finalizer.render];
  create_test_c_div "inputs" [%client Test_inputs.render];
  create_test_c_table "tabular1" [%client Test_tabular1.render];
  create_test_c_table "tabular2" [%client Test_tabular2.render];
  create_test_c_table "tabular3" [%client Test_tabular3.render];
  create_test "basic_editors" Test_basic_editors.handler;
  create_test "operated" Test_operated.handler;
  create_test "pinboard" Test_pinboard.handler;
  create_test "scalar" Test_scalar.handler;
  create_test_c_ul "weakchan" [%client Test_weakchan.render];
  create_test_c_div "weaktbl" [%client Test_weaktbl.render];
  create_test "twine_editor" Test_twine_editor.handler;
]

let main_handler () () =
  let test_service_item (name, service) =
    F.li [F.a ~service [F.pcdata name] ()] in
  Lwt.return [F.ul (List.map test_service_item test_services)]

let main_service =
  let open Eliom_service in
  Test_app.create_page
    ~path:(Path []) ~meth:(Get Eliom_parameter.unit)
    ~title:"Panograph Tests" main_handler
