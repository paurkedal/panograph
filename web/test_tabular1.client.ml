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

open Eliom_content
open Js_of_ocaml_lwt
open Panograph_tabular

let render () =
  let open Html in
  let tab = Tabular.create ~a:[F.a_class ["tabular1"]]
                           ~root_css_class:"root" () in
  let rs_root = Tabular.root_rowspan tab in
  let   rs_head = Tabular.Rowspan.add_first tab ~css_class:"rhead" rs_root in
  let   rs_body = Tabular.Rowspan.add_last tab ~css_class:"rbody" rs_root in
  let   rs_foot = Tabular.Rowspan.add_last tab ~css_class:"rfoot" rs_root in
  let cs_root = Tabular.root_colspan tab in
  let   cs_head = Tabular.Colspan.add_first tab ~css_class:"chead" cs_root in
  let   cs_body = Tabular.Colspan.add_last tab ~css_class:"cbody" cs_root in

  Tabular.refine tab 1 1 rs_root cs_root;
  Tabular.draw_th tab rs_body cs_head [D.txt "M = "];

  let     csX = Tabular.Colspan.add_last tab cs_body in
  Tabular.refine tab 0 1 rs_head cs_body;
  let     csY = Tabular.Colspan.add_last tab cs_body in
  let     csZ = Tabular.Colspan.add_last tab cs_body in
  Tabular.draw_th tab rs_head csX [D.txt "X"];
  Tabular.draw_th tab rs_head csY [D.txt "Y"];
  Tabular.draw_th tab rs_head csZ [D.txt "Z"];
  Tabular.refine tab 1 1 rs_body cs_body;
  let     rs1 = Tabular.Rowspan.add_first tab rs_body in
  let     rs2 = Tabular.Rowspan.add_last tab rs_body in
  let draw3 rs x y z =
    Tabular.draw_td tab rs csX [D.txt (string_of_float x)];
    Tabular.draw_td tab rs csY [D.txt (string_of_float y)];
    Tabular.draw_td tab rs csZ [D.txt (string_of_float z)] in
  draw3 rs1    1.0    0.5 0.1;
  draw3 rs2 ~-.0.5    1.0 0.2;

  Tabular.draw_td tab rs_foot cs_body [D.txt "foot"];

  let csT = Tabular.Colspan.add_first tab cs_body in
  Tabular.draw_td tab rs1 csT [D.txt "a"];
  Tabular.draw_td tab rs2 csT [D.txt "b"];

  Eliom_lib.debug "here";
  let rs3 = Tabular.Rowspan.add_last tab rs_body in
  draw3 rs3 ~-.0.1 ~-.0.2 1.0;
  let csT0 = Tabular.Colspan.add_last tab csT in
  let csT1 = Tabular.Colspan.add_last tab csT in
  Tabular.refine tab 0 1 rs3 csT;

  Tabular.draw_td tab rs3 csT0 [D.txt "c₀"];
  Tabular.draw_td tab rs3 csT1 [D.txt "c₁"];

  let mk_remove_rs rs cs =
    let button = D.Raw.button ~a:[D.a_button_type `Button] [D.txt "-"] in
    Tabular.draw_td tab rs cs [button];
    Lwt_js_events.(async @@ fun () ->
      clicks (To_dom.of_button button)
             (fun _ _ -> Tabular.Rowspan.delete tab rs; Lwt.return_unit)) in

  let cs_ctrl2 = Tabular.Colspan.add_last tab cs_root in
  Tabular.refine tab 1 0 rs_body cs_ctrl2;
  mk_remove_rs rs1 cs_ctrl2;
  mk_remove_rs rs2 cs_ctrl2;
  mk_remove_rs rs3 cs_ctrl2;

  let cs_ctrl1 = Tabular.Colspan.add_last tab cs_root in
  mk_remove_rs rs_head cs_ctrl1;
  mk_remove_rs rs_body cs_ctrl1;
  mk_remove_rs rs_foot cs_ctrl1;

  let mk_remove_cs rs cs =
    let button = D.Raw.button ~a:[D.a_button_type `Button] [D.txt "-"] in
    Tabular.draw_td tab rs cs [button];
    Lwt_js_events.(async @@ fun () ->
      clicks (To_dom.of_button button)
             (fun _ _ -> Tabular.Colspan.delete tab cs; Lwt.return_unit)) in

  let rs_ctrl2 = Tabular.Rowspan.add_last tab rs_root in
  Tabular.refine tab 0 1 rs_ctrl2 cs_body;
  mk_remove_cs rs_ctrl2 csT;
  mk_remove_cs rs_ctrl2 csX;
  mk_remove_cs rs_ctrl2 csY;
  mk_remove_cs rs_ctrl2 csZ;

  let rs_ctrl1 = Tabular.Rowspan.add_last tab rs_root in
  mk_remove_cs rs_ctrl1 cs_head;
  mk_remove_cs rs_ctrl1 cs_body;

  Tabular.validate tab;
  Tabular.ui tab
