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

open Eliom_content
open Panograph_tabular

let render () =
  let open Html5 in
  let tab = Tabular.create () in
  let rs_root = Tabular.root_rowspan tab in
  let   rs_head = Tabular.Rowspan.add_first tab rs_root in
  let   rs_body = Tabular.Rowspan.add_last tab rs_root in
  let   rs_foot = Tabular.Rowspan.add_last tab rs_root in
  let cs_root = Tabular.root_colspan tab in
  let   cs_head = Tabular.Colspan.add_first tab cs_root in
  let   cs_body = Tabular.Colspan.add_last tab cs_root in

  Tabular.refine tab 1 1 rs_root cs_root;
  Tabular.draw_th tab rs_body cs_head [D.pcdata "M = "];

  let     csX = Tabular.Colspan.add_last tab cs_body in
  Tabular.refine tab 0 1 rs_head cs_body;
  let     csY = Tabular.Colspan.add_last tab cs_body in
  let     csZ = Tabular.Colspan.add_last tab cs_body in
  Tabular.draw_th tab rs_head csX [D.pcdata "X"];
  Tabular.draw_th tab rs_head csY [D.pcdata "Y"];
  Tabular.draw_th tab rs_head csZ [D.pcdata "Z"];
  Tabular.refine tab 1 1 rs_body cs_body;
  let     rs1 = Tabular.Rowspan.add_first tab rs_body in
  let     rs2 = Tabular.Rowspan.add_last tab rs_body in
  let draw3 rs x y z =
    Tabular.draw_td tab rs csX [D.pcdata (string_of_float x)];
    Tabular.draw_td tab rs csY [D.pcdata (string_of_float y)];
    Tabular.draw_td tab rs csZ [D.pcdata (string_of_float z)] in
  draw3 rs1    1.0    0.5 0.1;
  draw3 rs2 ~-.0.5    1.0 0.2;

  Tabular.draw_td tab rs_foot cs_body [D.pcdata "foot"];

  let csT = Tabular.Colspan.add_first tab cs_body in
  Tabular.draw_td tab rs1 csT [D.pcdata "a"];
  Tabular.draw_td tab rs2 csT [D.pcdata "b"];

  Eliom_lib.debug "here";
  let rs3 = Tabular.Rowspan.add_last tab rs_body in
  draw3 rs3 ~-.0.1 ~-.0.2 1.0;

  Tabular.draw_td tab rs3 csT [D.pcdata "c"];

  let mk_remove_rs rs cs =
    let button = D.button ~button_type:`Button [F.pcdata "-"] in
    Tabular.draw_td tab rs cs [button];
    Lwt_js_events.(async @@ fun () ->
      clicks (To_dom.of_button button)
	     (fun _ _ -> Tabular.Rowspan.delete tab rs; Lwt.return_unit)) in

  let cs_ctrl2 = Tabular.Colspan.add_last tab cs_root in
  Tabular.refine tab 1 0 rs_body cs_ctrl2;
  mk_remove_rs rs_head cs_ctrl2;
  mk_remove_rs rs1 cs_ctrl2;
  mk_remove_rs rs2 cs_ctrl2;
  mk_remove_rs rs3 cs_ctrl2;
  mk_remove_rs rs_foot cs_ctrl2;

  let cs_ctrl1 = Tabular.Colspan.add_last tab cs_root in
  mk_remove_rs rs_head cs_ctrl1;
  mk_remove_rs rs_body cs_ctrl1;
  mk_remove_rs rs_foot cs_ctrl1;

  let mk_remove_cs rs cs =
    let button = D.button ~button_type:`Button [F.pcdata "-"] in
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
