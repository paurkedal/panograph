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

open Panograph_tabular
open Unprime_option

let (>>=) = Lwt.(>>=)

let rec nth_rs i rs =
  if i = 0 then rs else
  nth_rs (i - 1) (Option.get (Tabular.Rowspan.next rs))

let rec nth_cs i cs =
  if i = 0 then cs else
  nth_cs (i - 1) (Option.get (Tabular.Colspan.next cs))

let rec random_rowspan rs =
  if Tabular.Rowspan.is_leaf rs then rs else
  match Random.int 2 with
  | 0 -> rs
  | _ ->
    let n = Tabular.Rowspan.fold (fun _ -> succ) rs 0 in
    let i = Random.int n in
    random_rowspan (nth_rs i (Option.get (Tabular.Rowspan.first rs)))

let rec random_colspan cs =
  if Tabular.Colspan.is_leaf cs then cs else
  match Random.int 2 with
  | 0 -> cs
  | _ ->
    let n = Tabular.Colspan.fold (fun _ -> succ) cs 0 in
    let i = Random.int n in
    random_colspan (nth_cs i (Option.get (Tabular.Colspan.first cs)))

let rec count_seq f x n =
  match f x with
  | None -> n
  | Some x' -> count_seq f x' (n + 1)

let rowspan_path rs =
  let rec loop acc rs =
    match Tabular.Rowspan.up rs with
    | None -> acc
    | Some rs_u -> loop (count_seq Tabular.Rowspan.prev rs 0 :: acc) rs_u in
  String.concat "." (List.map string_of_int (loop [] rs))

let colspan_path cs =
  let rec loop acc cs =
    match Tabular.Colspan.up cs with
    | None -> acc
    | Some cs_u -> loop (count_seq Tabular.Colspan.prev cs 0 :: acc) cs_u in
  String.concat "." (List.map string_of_int (loop [] cs))

let add_row tab =
  let rs = random_rowspan (Tabular.root_rowspan tab) in
  let debug fn = Eliom_lib.debug "** %s %s" fn (rowspan_path rs) in
  match Random.int (if Tabular.Rowspan.is_root rs then 2 else 4) with
  | 0 -> debug "Rowspan.add_first";  Tabular.Rowspan.add_first tab rs
  | 1 -> debug "Rowspan.add_last";   Tabular.Rowspan.add_last tab rs
  | 2 -> debug "Rowspan.add_before"; Tabular.Rowspan.add_before tab rs
  | 3 -> debug "Rowspan.add_after";  Tabular.Rowspan.add_after tab rs
  | _ -> assert false

let add_column tab =
  let cs = random_colspan (Tabular.root_colspan tab) in
  let debug fn = Eliom_lib.debug "** %s %s" fn (colspan_path cs) in
  match Random.int (if Tabular.Colspan.is_root cs then 2 else 4) with
  | 0 -> debug "Colspan.add_first";  Tabular.Colspan.add_first tab cs
  | 1 -> debug "Colspan.add_last";   Tabular.Colspan.add_last tab cs
  | 2 -> debug "Colspan.add_before"; Tabular.Colspan.add_before tab cs
  | 3 -> debug "Colspan.add_after";  Tabular.Colspan.add_after tab cs
  | _ -> assert false

let remove_row tab =
  let rs = random_rowspan (Tabular.root_rowspan tab) in
  if not (Tabular.Rowspan.is_root rs) then begin
    Eliom_lib.debug "** remove rowspan %s" (rowspan_path rs);
    Tabular.Rowspan.delete tab rs
  end

let remove_column tab =
  let cs = random_colspan (Tabular.root_colspan tab) in
  if not (Tabular.Colspan.is_root cs) then begin
    Eliom_lib.debug "** remove colspan %s" (colspan_path cs);
    Tabular.Colspan.delete tab cs
  end

let refine tab =
  let rs = random_rowspan (Tabular.root_rowspan tab) in
  let cs = random_colspan (Tabular.root_colspan tab) in
  match Tabular.state tab rs cs with
  | Tabular.Leaf ->
    let lr, lc = Random.int 3, Random.int 3 in
    if lr <> 0 || lc <> 0 then begin
      Eliom_lib.debug "** refine %d %d" lr lc;
      Tabular.refine tab lr lc rs cs
    end
  | Tabular.Split _ | Tabular.Invalid -> ()

let mutate tab =
  let coin = Random.int 20 - 2 in
  if coin = -2 then remove_row tab else
  if coin = -1 then remove_column tab else
  if coin < 4 then ignore (add_row tab) else
  if coin < 8 then ignore (add_column tab) else
  refine tab

let render () =
  let tab = Tabular.create () in
  let rec mutate_loop () =
    Lwt_js.sleep 0.01 >>= fun () ->
    mutate tab; Tabular.validate tab; mutate_loop () in
  Lwt.async mutate_loop;
  Tabular.ui tab
