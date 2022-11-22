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

[%%shared
  open Eliom_content.Html
  open Eliom_lib
]

[%%server
  let lwt_log_rules =
    try Some (Sys.getenv "LWT_LOG_JS") with Not_found ->
    try Some (Sys.getenv "LWT_LOG") with Not_found ->
    None
]
[%%client
  let () =
    Lwt_log.(Section.set_level (Section.make "eliom:client") Warning);
    Option.iter Lwt_log.load_rules ~%lwt_log_rules
]

include Eliom_registration.App
  (struct
    let application_name = "test"
    let global_data_path = None
  end)

let default_css = [["css"; "panograph.css"]; ["css"; "panograph-test.css"]]

let create_page ~path ~meth ~title inner =
  create ~path ~meth
    (fun get post ->
      inner get post >|= fun contents ->
      Eliom_tools.D.html ~title ~css:default_css
        (D.body (D.h1 [D.txt title] :: contents)))

let create_test ~name ?title inner =
  let open Eliom_service in
  let title =
    (match title with
     | Some title -> title
     | None ->
        name |> String.map (function '_' -> ' ' | c -> c)
             |> String.capitalize_ascii) in
  let service =
    create_page ~path:(Path [name]) ~meth:(Get Eliom_parameter.unit)
                ~title:name inner in
  (title, service)
