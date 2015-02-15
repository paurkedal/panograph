(* Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
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
  module Dep_tbe = Test_basic_editors
  module Dep_ted = Test_editable
  module Dep_tte = Test_twine_editor
  module Dep_opr = Test_operated
  module Dep_twt = Test_weaktbl
}}

module App = Eliom_registration.App (struct let application_name = "test" end)

let simple_handler title render () () =
  let open Html5 in
  Lwt.return @@
    Eliom_tools.D.html ~title ~css:[["css"; "panograph.css"]]
      (D.body [D.h1 [D.pcdata title]; render ()])

let make_test_service (name, render) =
  let open Html5 in
  name,
  App.register_service ~path:[name] ~get_params:Eliom_parameter.unit
    (simple_handler name render)

let test_services = List.map make_test_service [
  "editable", Test_editable.render;
  "inputs", (fun () -> Html5.C.node {{Test_inputs.render ()}});
  "tabular1", (fun () -> Html5.C.node {{Test_tabular1.render ()}});
  "tabular2", (fun () -> Html5.C.node {{Test_tabular2.render ()}});
  "tabular3", (fun () -> Html5.C.node {{Test_tabular3.render ()}});
  "basic_editors", Test_basic_editors.render;
  "operated", Test_operated.render;
  "weaktbl", Test_weaktbl.render;
  "twine_editor", Test_twine_editor.render;
]

let main_handler () () =
  let open Html5 in
  let test_service_item (name, service) =
    F.li [F.a ~service [F.pcdata name] ()] in
  Lwt.return @@
    Eliom_tools.D.html
      ~title:"Panograph Test"
      ~css:[["css"; "panograph.css"]]
      (F.body [
	F.h1 [F.pcdata "Panograph Test"];
	F.ul (List.map test_service_item test_services);
      ])

let main_service =
  App.register_service ~path:[] ~get_params:Eliom_parameter.unit main_handler
