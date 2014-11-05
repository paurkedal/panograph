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
open Panograph_dltree
open Unprime
open Unprime_option

module type SPAN_TREE = sig
  type t
  type tabular
  val is_root : t -> bool
  val is_leaf : t -> bool
  val up : t -> t option
  val first : t -> t option
  val last : t -> t option
  val next : t -> t option
  val prev : t -> t option
  val add_first : tabular -> t -> t
  val add_last : tabular -> t -> t
  val add_before : tabular -> t -> t
  val add_after : tabular -> t -> t
  val delete : tabular -> t -> unit
end

module Tabular = struct

  type blockstate =
    | Single of Dom_html.tableCellElement Js.t
    | Refined of int * int

  type block = {
    mutable blk_state : blockstate;
    blk_rs : rowspan_node Dltree.t;
    blk_cs : colspan_node Dltree.t;
  }
  and rowspan_node = {
    mutable rsn_span : int;
    rsn_blocks : (int, block) Hashtbl.t;
  }
  and colspan_node = {
    csn_id : int;			(* Unique to each span. *)
    mutable csn_span : int;
  }

  module Rs_order = struct
    type t = rowspan_node Dltree.t
    let compare = Dltree.left_compare
  end

  module Cs_order = struct
    type t = colspan_node Dltree.t
    let compare = Dltree.left_compare
  end

  module Rs_map = Prime_enummap.Make (Rs_order)
  module Cs_map = Prime_enummap.Make (Cs_order)

  type tr_node = {
    tn_tr : Dom_html.tableRowElement Js.t;
    mutable tn_tcs : Dom_html.tableCellElement Js.t Cs_map.t;
  }

  type t = {
    tab_table : Dom_html.tableElement Js.t;
    tab_root_rs : rowspan_node Dltree.t;
    tab_root_cs : colspan_node Dltree.t;
    mutable tab_tns : tr_node Rs_map.t;
    mutable tab_next_col_id : int;
  }

  let validate_tn tab rs tn =
    let i = ref 0 in
    Cs_map.iter (fun cs tc -> assert (tc##cellIndex = !i); incr i) tn.tn_tcs;
    assert (!i = tn.tn_tr##cells##length)
  let validate tab = Rs_map.iter (validate_tn tab) tab.tab_tns

  let insert_row tab rs =
    assert (Dltree.is_leaf rs);
    let found, pos = Rs_map.locate rs tab.tab_tns in
    assert (not found);
    let tr = tab.tab_table##insertRow(pos) in
    let tn = {tn_tr = tr; tn_tcs = Cs_map.empty} in
    tab.tab_tns <- Rs_map.add rs tn tab.tab_tns;
    tn

  let remove_row tab rs =
    assert (Dltree.is_leaf rs);
    let found, pos = Rs_map.locate rs tab.tab_tns in
    assert found;
    let tn = Rs_map.find rs tab.tab_tns in
    assert (pos = tn.tn_tr##rowIndex);
    tab.tab_table##deleteRow(pos);
    tab.tab_tns <- Rs_map.remove rs tab.tab_tns

  let insert_cell' tn cs =
    let found, pos = Cs_map.locate cs tn.tn_tcs in
    assert (not found);
    let tc = tn.tn_tr##insertCell(pos) in
    tn.tn_tcs <- Cs_map.add cs tc tn.tn_tcs;
    tc

  let remove_cell' tn cs =
    let found, pos = Cs_map.locate cs tn.tn_tcs in
    assert found;
    let tc = Cs_map.find cs tn.tn_tcs in
    assert (pos = tc##cellIndex);
    tn.tn_tr##deleteCell(pos);
    tn.tn_tcs <- Cs_map.remove cs tn.tn_tcs

  let move_cell' tn_old tn_new cs =
    let found_old, pos_old = Cs_map.locate cs tn_old.tn_tcs in
    let found_new, pos_new = Cs_map.locate cs tn_new.tn_tcs in
    assert found_old;
    assert (not found_new);
    let tc = Cs_map.find cs tn_old.tn_tcs in
    let tc'_opt = Cs_map.get_o pos_new tn_new.tn_tcs in
    tn_old.tn_tcs <- Cs_map.remove cs tn_old.tn_tcs;
    Dom.removeChild tn_old.tn_tr tc;
    tn_new.tn_tcs <- Cs_map.add cs tc tn_new.tn_tcs;
    Dom.insertBefore tn_new.tn_tr tc (Js.Opt.option tc'_opt)

  let insert_cell tab rs cs =
    let rs_leaf = Dltree.first_leaf rs in
    assert (Rs_map.contains rs_leaf tab.tab_tns);
    let tn = Rs_map.find rs_leaf tab.tab_tns in
    let tc = insert_cell' tn cs in
    tc##rowSpan <- (Dltree.get rs).rsn_span;
    tc##colSpan <- (Dltree.get cs).csn_span;
    tc

  let remove_cell tab rs cs =
    let rs_leaf = Dltree.first_leaf rs in
    assert (Rs_map.contains rs_leaf tab.tab_tns);
    let tn = Rs_map.find rs_leaf tab.tab_tns in
    remove_cell' tn cs

  let for_subblocks rs cs f =
    let rsn, csn = Dltree.(get rs, get cs) in
    assert (Hashtbl.mem rsn.rsn_blocks csn.csn_id);
    let blk = Hashtbl.find rsn.rsn_blocks csn.csn_id in
    match blk.blk_state with
    | Single tc -> invalid_arg "Tabular.for_subblocks"
    | Refined (lr, lc) ->
      let rec loop_cs lc cs =
	let rec loop_rs lr rs =
	  if lr > 0 then Dltree.iter (loop_rs (lr - 1)) rs
		    else f rs cs in
	if lc > 0 then Dltree.iter (loop_cs (lc - 1)) cs
		  else loop_rs lr rs in
      loop_cs lc cs

  let refine tab lr lc cov_rs cov_cs =
    let cov_rsn, cov_csn = Dltree.(get cov_rs, get cov_cs) in
    if not (Hashtbl.mem cov_rsn.rsn_blocks cov_csn.csn_id) then
      invalid_arg "Tabular.refine: Blockspan between or below a refinement.";
    let cov_blk = Hashtbl.find cov_rsn.rsn_blocks cov_csn.csn_id in
    match cov_blk.blk_state with
    | Refined _ -> invalid_arg "Tabular.refine: Already refined."
    | Single tc ->
      remove_cell tab cov_rs cov_cs;
      cov_blk.blk_state <- Refined (lr, lc);
      for_subblocks cov_rs cov_cs
	(fun sub_rs sub_cs ->
	  let sub_rsn, sub_csn = Dltree.(get sub_rs, get sub_cs) in
	  let sub_tc = insert_cell tab sub_rs sub_cs in
	  let sub_blk = {
	    blk_state = Single sub_tc;
	    blk_rs = sub_rs;
	    blk_cs = sub_cs;
	  } in
	  Hashtbl.add sub_rsn.rsn_blocks sub_csn.csn_id sub_blk)

  (* Find the inner cover of (rs, cs) spanning exactly rs. *)
  let rec blk_covering_rs rs cs =
    let rsn, csn = Dltree.(get rs, get cs) in
    try Some (Hashtbl.find rsn.rsn_blocks csn.csn_id)
    with Not_found ->
      Option.search (blk_covering_rs rs) (Dltree.up cs)

  (* Find the inner cover of (rs, cs) spanning exactly cs. *)
  let rec blk_covering_cs rs cs =
    let rsn, csn = Dltree.(get rs, get cs) in
    try Some (Hashtbl.find rsn.rsn_blocks csn.csn_id)
    with Not_found ->
      Option.search (fun rs -> blk_covering_cs rs cs) (Dltree.up rs)

  (* Finds the block covering (rs_inner, cs_inner) if any. *)
  let rec find_cover rs_inner cs_inner =
    let rec loop_cs cs =
      let csn = Dltree.get cs in
      let rec loop_rs rs =
	let rsn = Dltree.get rs in
	if Hashtbl.mem rsn.rsn_blocks csn.csn_id
	then Some (ident (rs, cs))
	else Option.search loop_rs (Dltree.up rs) in
      match loop_rs rs_inner with
      | Some _ as bs_opt -> bs_opt
      | None -> Option.search loop_cs (Dltree.up cs) in
    loop_cs cs_inner

  let fill_cell tab rs cs =
    let rsn, csn = Dltree.get rs, Dltree.get cs in
    let tc = insert_cell tab rs cs in
    tc##rowSpan <- rsn.rsn_span;
    tc##colSpan <- csn.csn_span;
    Eliom_lib.debug "fill_cell, span = (%d, %d)" rsn.rsn_span csn.csn_span;
    let blk = {
      blk_state = Single tc;
      blk_rs = rs;
      blk_cs = cs;
    } in
    Hashtbl.add rsn.rsn_blocks csn.csn_id blk

  let tr_of_node node =
    Js.coerce_opt (Dom_html.CoerceTo.element node) Dom_html.CoerceTo.tr
		  (fun _ -> assert false)

  (* We just added new_rs, extend rowspans or allocate new cells. *)
  let alloc_row ?migrate tab new_rs =

    (* First fix the ancestor rowspans if this is not the first subspan. *)
    if not (Dltree.is_only new_rs) then
      Dltree.iter_ancestors
	(fun rs ->
	  let rsn = Dltree.get rs in
	  Eliom_lib.debug "rsn.rsn_rowspan <- %d" (rsn.rsn_span + 1);
	  rsn.rsn_span <- rsn.rsn_span + 1)
	new_rs;

    (* Then allocate or extend the cells. *)
    let rec loop_cs lc cs =
      if lc > 0 then Dltree.iter (loop_cs (lc - 1)) cs else
      match blk_covering_cs new_rs cs with
      | None ->
	fill_cell tab new_rs cs
      | Some blk ->
	let rsn = Dltree.get blk.blk_rs in
	begin match blk.blk_state with
	| Single tc ->
	  Option.iter
	    (fun (tn_old, tn_new) ->
	      let tc_tr = tr_of_node (Js.Opt.get (tc##parentNode)
						 (fun _ -> assert false)) in
	      if tc_tr == tn_old.tn_tr then
		move_cell' tn_old tn_new cs)
	    migrate;
	  Eliom_lib.debug "tc##rowSpan <- %d" rsn.rsn_span;
	  tc##rowSpan <- rsn.rsn_span
	| Refined (lr, lc) ->
	  Eliom_lib.debug "refined at (%d, %d) by (%d, %d)"
			  (Dltree.level blk.blk_rs) (Dltree.level blk.blk_cs)
			  lr lc;
	  if lc > 0 then loop_cs lc cs
		    else fill_cell tab new_rs cs
	end in
    Eliom_lib.debug "alloc_row, %b" (Dltree.is_only new_rs);
    loop_cs 0 tab.tab_root_cs

  (* We just added new_cs, extend colspans or allocate new cells. *)
  let alloc_column tab new_cs =

    (* First fix the ancestor colspans if this is not the first subspan. *)
    if not (Dltree.is_only new_cs) then
      Dltree.iter_ancestors
	(fun cs ->
	  let csn = Dltree.get cs in
	  Eliom_lib.debug "csn.csn_colspan <- %d" (csn.csn_span + 1);
	  csn.csn_span <- csn.csn_span + 1)
	new_cs;

    (* Then allocate the cells. *)
    let rec loop_rs lr rs =
      if lr > 0 then Dltree.iter (loop_rs (lr - 1)) rs else
      match blk_covering_rs rs new_cs with
      | None ->
	fill_cell tab rs new_cs
      | Some blk ->
	let csn = Dltree.get blk.blk_cs in
	begin match blk.blk_state with
	| Single tc ->
	  Eliom_lib.debug "tc##colSpan <- %d" csn.csn_span;
	  tc##colSpan <- csn.csn_span
	| Refined (lr, lc) ->
	  Eliom_lib.debug "refined at (%d, %d) by (%d, %d)"
			  (Dltree.level blk.blk_rs) (Dltree.level blk.blk_cs)
			  lr lc;
	  if lr > 0 then loop_rs lr rs
		    else fill_cell tab rs new_cs
	end in
    Eliom_lib.debug "alloc_col, %b" (Dltree.is_only new_cs);
    loop_rs 0 tab.tab_root_rs

  module Rowspan = struct
    type t = rowspan_node Dltree.t
    let is_root = Dltree.is_root
    let is_leaf = Dltree.is_leaf
    let up = Dltree.up
    let first = Dltree.first
    let last = Dltree.last
    let next = Dltree.next
    let prev = Dltree.prev

    let make_rsn () = { rsn_span = 1; rsn_blocks = Hashtbl.create 11; }

    let add_first tab rs_u =
      match Dltree.first rs_u with
      | None ->
	assert (Rs_map.contains rs_u tab.tab_tns);
	let tn = Rs_map.find rs_u tab.tab_tns in
	let rsn = make_rsn () in
	let rs = Dltree.add_first rsn rs_u in
	tab.tab_tns <- Rs_map.add rs tn (Rs_map.remove rs_u tab.tab_tns);
	alloc_row tab rs;
	rs
      | Some rs1 ->
	assert (Rs_map.contains rs1 tab.tab_tns);
	let tn1 = Rs_map.find rs1 tab.tab_tns in
	let rsn0 = make_rsn () in
	let rs0 = Dltree.add_first rsn0 rs_u in
	let tn0 = insert_row tab rs0 in
	alloc_row ~migrate:(tn1, tn0) tab rs0;
	rs0

    let add_before tab rs_n =
      match Dltree.up rs_n with
      | None -> invalid_arg "Tabular.Rowspan.add_before"
      | Some rs_u ->
	if Dltree.is_first rs_n then add_first tab rs_u else
	begin
	  let rs = Dltree.add_before (make_rsn ()) rs_n in
	  let _ = insert_row tab rs in
	  alloc_row tab rs;
	  rs
	end

    let add_after tab rs_p =
      let rs = Dltree.add_after (make_rsn ()) rs_p in
      let _ = insert_row tab rs in
      alloc_row tab rs;
      rs

    let add_last tab rs =
      match Dltree.last rs with
      | None -> add_first tab rs
      | Some last_rs -> add_after tab last_rs

    let delete tab rs =
      assert false (* FIXME *)
  end

  module Colspan = struct
    type t = colspan_node Dltree.t
    let is_root = Dltree.is_root
    let is_leaf = Dltree.is_leaf
    let up = Dltree.up
    let first = Dltree.first
    let last = Dltree.last
    let next = Dltree.next
    let prev = Dltree.prev

    let make_csn tab =
      let csn_id = tab.tab_next_col_id in
      tab.tab_next_col_id <- succ tab.tab_next_col_id;
      {csn_id; csn_span = 1}

    let add_first tab cs_up =
      let cs = Dltree.add_first (make_csn tab) cs_up in
      alloc_column tab cs; cs

    let add_last tab cs_up =
      let cs = Dltree.add_last (make_csn tab) cs_up in
      alloc_column tab cs; cs

    let add_before tab cs_n =
      let cs = Dltree.add_before (make_csn tab) cs_n in
      alloc_column tab cs; cs

    let add_after tab cs_p =
      let cs = Dltree.add_after (make_csn tab) cs_p in
      alloc_column tab cs; cs

    let delete tab cs =
      assert false (* FIXME *)
  end

  let get_tc tab rs cs =
    let rsn, csn = Dltree.(get rs, get cs) in
    try
      let blk = Hashtbl.find rsn.rsn_blocks csn.csn_id in
      match blk.blk_state with
      | Single tc -> tc
      | Refined _ ->
	invalid_arg "Tabular.get_tc: This cell is refined."
    with Not_found ->
      invalid_arg "Tabular.get_tc: Not refined at this cell."

  let set_tc tab rs cs tc' =
    let rsn, csn = Dltree.(get rs, get cs) in
    try
      let blk = Hashtbl.find rsn.rsn_blocks csn.csn_id in
      match blk.blk_state with
      | Single tc ->
	assert (tc##rowSpan = rsn.rsn_span);
	assert (tc##colSpan = csn.csn_span);
	tc'##rowSpan <- rsn.rsn_span;
	tc'##colSpan <- csn.csn_span;
	let rs_leaf = Dltree.first_leaf rs in
	assert (Rs_map.contains rs_leaf tab.tab_tns);
	let tn = Rs_map.find rs_leaf tab.tab_tns in
	assert (Cs_map.contains cs tn.tn_tcs);
	assert (Cs_map.find cs tn.tn_tcs == tc);
	blk.blk_state <- Single tc';
	tn.tn_tcs <- Cs_map.add cs tc' tn.tn_tcs;
	Dom.replaceChild tn.tn_tr tc' tc
      | Refined _ ->
	invalid_arg "Tabular.set_tc: This cell is refined."
    with Not_found ->
      invalid_arg "Tabular.set_tc: Not refined at this cell."

  (* Cf https://github.com/ocsigen/js_of_ocaml/pull/240 *)
  let coerce_to_tc : #Dom_html.element Js.t -> #Dom_html.tableCellElement Js.t =
    Js.Unsafe.coerce

  let draw_td tab rs cs content =
    let td = Html5.D.td content in
    set_tc tab rs cs (coerce_to_tc (Html5.To_dom.of_td td))

  let draw_th tab rs cs content =
    let th = Html5.D.th content in
    set_tc tab rs cs (coerce_to_tc (Html5.To_dom.of_th th))

  let create () =
    let root_rsn = {rsn_span = 1; rsn_blocks = Hashtbl.create 11} in
    let root_csn = {csn_span = 1; csn_id = 0} in
    let tab =
      { tab_table = Dom_html.createTable Dom_html.document;
	tab_root_cs = Dltree.make root_csn;
	tab_root_rs = Dltree.make root_rsn;
	tab_next_col_id = 1;
	tab_tns = Rs_map.empty; } in
    let tn = insert_row tab tab.tab_root_rs in
    let tc = tn.tn_tr##insertCell(0) in
    tn.tn_tcs <- Cs_map.add tab.tab_root_cs tc tn.tn_tcs;
    let rec blk =
      { blk_state = Single tc;
	blk_rs = tab.tab_root_rs;
	blk_cs = tab.tab_root_cs; } in
    Hashtbl.add root_rsn.rsn_blocks 0 blk;
    tab

  let ui t = Html5.Of_dom.of_table t.tab_table
  let root_rowspan t = t.tab_root_rs
  let root_colspan t = t.tab_root_cs

end
