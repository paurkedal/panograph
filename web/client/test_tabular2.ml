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

open Eliom_content
open Panograph_tabular
open Printf
open Unprime
open Unprime_list

let tile ns ms =
  let tab = Tabular.create () in
  let rs = Tabular.root_rowspan tab in
  let cs = Tabular.root_colspan tab in
  Eliom_lib.debug "** Tiling [%s], [%s]"
                  (String.concat ", " (List.map string_of_int ns))
                  (String.concat ", " (List.map string_of_int ms));
  let rec add_rows rs = function
    | [] -> ()
    | n :: ns ->
      for i = 0 to n - 1 do
        let rs' = Tabular.Rowspan.add_last tab rs in
        add_rows rs' ns
      done in
  let rec add_cols cs = function
    | [] -> ()
    | m :: ms ->
      for j = 0 to m - 1 do
        let cs' = Tabular.Colspan.add_last tab cs in
        add_cols cs' ms
      done in
  Eliom_lib.debug "** Adding rows and columns.";
  add_rows rs ns;
  add_cols cs ms;
  Eliom_lib.debug "** Refining.";
  Tabular.refine tab (List.length ns) (List.length ms) rs cs;
  Eliom_lib.debug "** Drawing.";
  Tabular.Rowspan.iterp ~depth:(List.length ns)
    (fun pr rs ->
      Tabular.Colspan.iterp ~depth:(List.length ms)
        (fun pc cs ->
          let open Html5 in
          let sr = String.concat "." (List.map string_of_int pr) in
          let sc = String.concat "." (List.map string_of_int pc) in
          let label = [F.pcdata sr; F.pcdata ", "; F.pcdata sc] in
          Tabular.draw_td tab rs cs label)
        cs)
    rs;
  Tabular.ui tab

let render () =
  let open Html5 in
  let tab = Tabular.create () in
  let rs = Tabular.root_rowspan tab in
  let cs = Tabular.root_colspan tab in
  Tabular.refine tab 1 1 rs cs;
  for lr = 0 to 3 do ignore (Tabular.Rowspan.add_last tab rs) done;
  for lc = 0 to 3 do ignore (Tabular.Colspan.add_last tab cs) done;
  Tabular.Rowspan.iteri
    (fun n rs ->
      Tabular.Colspan.iteri
        (fun m cs ->
          if n <> 0 || m <> 0 then
            Tabular.draw_td tab rs cs
              [tile (List.sample (konst 2) n) (List.sample (konst 2) m)])
        cs)
    rs;
  Tabular.ui tab
