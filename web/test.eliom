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

{shared{
  open Eliom_content
  open Unprime_list
  open Unprime_option
}}
{client{
  module Dep_com = Test_completion
  module Dep_cse = Test_combo_selectors
  module Dep_cwe = Test_content_with_edit
  module Dep_dia = Test_dialogs
  module Dep_fin = Test_finalizer
  module Dep_tbe = Test_basic_editors
  module Dep_tte = Test_twine_editor
  module Dep_opr = Test_operated
  module Dep_sca = Test_scalar
  module Dep_twc = Test_weakchan
  module Dep_twt = Test_weaktbl
}}

{server{
  let lwt_log_rules =
    try Some (Sys.getenv "LWT_LOG_JS") with Not_found ->
    try Some (Sys.getenv "LWT_LOG") with Not_found ->
    None
}}
{client{
  let () =
    Lwt_log_js.(Section.set_level (Section.make "eliom:client") Warning);
    Option.iter Lwt_log_js.load_rules %lwt_log_rules
}}

module App = Eliom_registration.App (struct let application_name = "test" end)

let css = [["css"; "panograph.css"]; ["css"; "panograph-test.css"]]

let simple_handler title render () () =
  let open Html5 in
  Lwt.return @@
    Eliom_tools.D.html ~title ~css
      (D.body [D.h1 [D.pcdata title]; render ()])

let make_test_service (name, render) =
  let open Html5 in
  name,
  App.register_service ~path:[name] ~get_params:Eliom_parameter.unit
    (simple_handler name render)

let test_services = List.map make_test_service [
  "combo_selectors", Test_combo_selectors.render;
  "completion", Test_completion.render;
  "content_with_edit", Test_content_with_edit.render;
  "dialogs", Test_dialogs.render;
  "finalizer", Test_finalizer.render;
  "inputs", (fun () -> Html5.C.node {{Test_inputs.render ()}});
  "tabular1", (fun () -> Html5.C.node {{Test_tabular1.render ()}});
  "tabular2", (fun () -> Html5.C.node {{Test_tabular2.render ()}});
  "tabular3", (fun () -> Html5.C.node {{Test_tabular3.render ()}});
  "basic_editors", Test_basic_editors.render;
  "operated", Test_operated.render;
  "scalar", Test_scalar.render;
  "weakchan", Test_weakchan.render;
  "weaktbl", Test_weaktbl.render;
  "twine_editor", Test_twine_editor.render;
]

let main_handler () () =
  let open Html5 in
  let test_service_item (name, service) =
    F.li [F.a ~service [F.pcdata name] ()] in
  Lwt.return @@
    Eliom_tools.D.html
      ~title:"Panograph Test" ~css
      (F.body [
        F.h1 [F.pcdata "Panograph Test"];
        F.ul (List.map test_service_item test_services);
      ])

let main_service =
  App.register_service ~path:[] ~get_params:Eliom_parameter.unit main_handler
