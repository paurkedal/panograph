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
open Panograph_dltree
open Unprime
open Unprime_option

module type SPAN_TREE = sig
  type t
  type tabular

  val level : t -> int
  val is_root : t -> bool
  val is_leaf : t -> bool
  val is_first : t -> bool
  val is_last : t -> bool
  val is_only : t -> bool

  val up : t -> t option
  val first : t -> t option
  val last : t -> t option
  val next : t -> t option
  val prev : t -> t option

  val first_leaf : t -> t
  val last_leaf : t -> t

  val fold : ?depth: int -> (t -> 'a -> 'a) -> t -> 'a -> 'a
  val iter : ?depth: int -> (t -> unit) -> t -> unit
  val iteri : ?depth: int -> (int -> t -> unit) -> t -> unit
  val iterp : depth: int -> (int list -> t -> unit) -> t -> unit
  val exists : ?depth: int -> (t -> bool) -> t -> bool

  val add_first : tabular -> ?css_class: string -> t -> t
  val add_last : tabular -> ?css_class: string -> t -> t
  val add_before : tabular -> ?css_class: string -> t -> t
  val add_after : tabular -> ?css_class: string -> t -> t
  val delete : tabular -> t -> unit
end

module Tabular = struct

  type blockstate =
    | Empty of int * int * Dom_html.tableCellElement Js.t
    | Single of Dom_html.tableCellElement Js.t
    | Refining of int * int * Dom_html.tableCellElement Js.t
    | Refined of int * int

  type block = {
    mutable blk_state : blockstate;
    blk_rs : rowspan_node Dltree.t;
    blk_cs : colspan_node Dltree.t;
  }
  and rowspan_node = {
    mutable rsn_span : int;
    rsn_blocks : (int, block) Hashtbl.t;
    rsn_css_class : string option;
  }
  and colspan_node = {
    csn_id : int;                       (* Unique to each span. *)
    mutable csn_span : int;
    csn_css_class : string option;
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

  let has_subblock ?skip_rs ?skip_cs lr lc rs cs =
    Dltree.exists ~depth:lr (fun rs' -> Option.for_all ((!=) rs') skip_rs) rs &&
    Dltree.exists ~depth:lc (fun cs' -> Option.for_all ((!=) cs') skip_cs) cs

  let is_leaf ?skip c =
    match skip with
    | None -> Dltree.is_leaf c
    | Some c' -> Dltree.for_all ((==) c') c

  let for_subblocks ?skip_rs ?skip_cs cov_lr cov_lc cov_rs cov_cs f =
    let rec loop_cs sub_lc sub_cs =
      let rec loop_rs sub_lr sub_rs =
        if not (Option.exists ((==) sub_rs) skip_rs) then begin
          if sub_lr = 0 || is_leaf ?skip:skip_rs sub_rs
          then f sub_lr sub_lc sub_rs sub_cs
          else Dltree.iter (loop_rs (sub_lr - 1)) sub_rs
        end in
      if not (Option.exists ((==) sub_cs) skip_cs) then begin
        if sub_lc = 0 || is_leaf ?skip:skip_cs sub_cs
        then loop_rs cov_lr cov_rs
        else Dltree.iter (loop_cs (sub_lc - 1)) sub_cs
      end in
    loop_cs cov_lc cov_cs

  let validate tab =
    let root_rsn = Dltree.get tab.tab_root_rs in
    let root_csn = Dltree.get tab.tab_root_cs in
    let widths = Array.make root_rsn.rsn_span 0 in
    let i = ref 0 in
    let validate_tn rs tn =
      let j = ref 0 in
      Cs_map.iter
        (fun cs tc ->
          assert (tc##.cellIndex = !j);
          for k = 0 to tc##.rowSpan - 1 do
            widths.(!i + k) <- widths.(!i + k) + tc##.colSpan
          done;
          incr j)
        tn.tn_tcs;
      assert (!j = tn.tn_tr##.cells##.length);
      incr i in
    Rs_map.iter validate_tn tab.tab_tns;
    if Prime_array.exists ((<>) root_csn.csn_span) widths then begin
      Eliom_lib.debug "Wrong width, should be %d:" root_csn.csn_span;
      for i = 0 to Array.length widths - 1 do
        Eliom_lib.debug "widths.(%d) = %d" i widths.(i)
      done;
      assert false
    end;
    let rec assert_empty_at_rs rs cs =
      let rsn, csn = Dltree.(get rs, get cs) in
      assert (not (Hashtbl.mem rsn.rsn_blocks csn.csn_id));
      Dltree.iter (assert_empty_at_rs rs) cs in
    let rec assert_empty rs cs =
      assert_empty_at_rs rs cs;
      Dltree.iter (fun rs -> assert_empty rs cs) rs in
    let rec assert_empty_below rs cs =
      Dltree.iter (assert_empty rs) cs;
      Dltree.iter (fun rs -> assert_empty_at_rs rs cs) rs in
    let rec validate_block rem_lr rem_lc rs cs =
      let rsn, csn = Dltree.(get rs, get cs) in
      let blk = Hashtbl.find rsn.rsn_blocks csn.csn_id in
      match blk.blk_state with
      | Single tc ->
        assert (rem_lr = 0 && rem_lc = 0);
        assert (tc##.rowSpan = rsn.rsn_span);
        assert (tc##.colSpan = csn.csn_span);
        assert_empty_below rs cs
      | Empty (rem_lr', rem_lc', tc) ->
        assert (rem_lr' = rem_lr);
        assert (rem_lc' = rem_lc);
        assert_empty_below rs cs
      | Refining (lr, lc, tc) ->
        assert (rem_lr = 0 && rem_lc = 0);
        assert (lr > 0 || lc > 0);
        assert (not (has_subblock lr lc rs cs));
        assert (tc##.rowSpan = rsn.rsn_span);
        assert (tc##.colSpan = csn.csn_span);
        assert_empty_below rs cs
      | Refined (lr, lc) ->
        assert (rem_lr = 0 && rem_lc = 0);
        assert (lr = 0 || not (Dltree.is_leaf rs));
        assert (lc = 0 || not (Dltree.is_leaf cs));
        for_subblocks lr lc rs cs validate_block in
    validate_block 0 0 tab.tab_root_rs tab.tab_root_cs

  let switch_class b cls tc =
    if b then tc##.classList##add(Js.string cls)
         else tc##.classList##remove(Js.string cls)

  let update_cell_rs_attribs rs tc =
    tc##.rowSpan := (Dltree.get rs).rsn_span;
    let rec clear sfx rs =
      Option.iter (fun cls -> tc##.classList##remove(Js.string (cls ^ sfx)))
                  (Dltree.get rs).rsn_css_class;
      Option.iter (clear sfx) (Dltree.up rs) in
    let rec update_fr rs =
      match Dltree.up rs with
      | None -> ()
      | Some up_rs when Dltree.is_first rs ->
        Option.iter (fun cls -> tc##.classList##add(Js.string (cls ^ "-fr")))
                    (Dltree.get up_rs).rsn_css_class;
        update_fr up_rs
      | Some up_rs -> clear "-fr" up_rs in
    let rec update_lr rs =
      match Dltree.up rs with
      | None -> ()
      | Some up_rs when Dltree.is_last rs ->
        Option.iter (fun cls -> tc##.classList##add(Js.string (cls ^ "-lr")))
                    (Dltree.get up_rs).rsn_css_class;
        update_lr up_rs
      | Some up_rs -> clear "-lr" up_rs in
    update_fr rs;
    update_lr rs

  let update_cell_cs_attribs cs tc =
    tc##.colSpan := (Dltree.get cs).csn_span;
    let rec clear sfx cs =
      Option.iter (fun cls -> tc##.classList##remove(Js.string (cls ^ sfx)))
                  (Dltree.get cs).csn_css_class;
      Option.iter (clear sfx) (Dltree.up cs) in
    let rec update_fc cs =
      match Dltree.up cs with
      | None -> ()
      | Some up_cs when Dltree.is_first cs ->
        Option.iter (fun cls -> tc##.classList##add(Js.string (cls ^ "-fc")))
                    (Dltree.get up_cs).csn_css_class;
        update_fc up_cs
      | Some up_cs -> clear "-fc" up_cs in
    let rec update_lc cs =
      match Dltree.up cs with
      | None -> ()
      | Some up_cs when Dltree.is_last cs ->
        Option.iter (fun cls -> tc##.classList##add(Js.string (cls ^ "-lc")))
                    (Dltree.get up_cs).csn_css_class;
        update_lc up_cs
      | Some up_cs -> clear "-lc" up_cs in
    update_fc cs;
    update_lc cs

  let init_cell_attribs rs cs tc =
    Option.iter (fun c -> tc##.classList##add(Js.string c))
                (Dltree.get rs).rsn_css_class;
    Option.iter (fun c -> tc##.classList##add(Js.string c))
                (Dltree.get cs).csn_css_class;
    let rec loop_up_rs rs =
      match Dltree.up rs with
      | None -> ()
      | Some up_rs ->
        if not (Dltree.is_root up_rs) then
          Option.iter (fun c -> tc##.classList##add(Js.string (c ^ "-r")))
                      (Dltree.get up_rs).rsn_css_class;
        loop_up_rs up_rs in
    let rec loop_up_cs cs =
      match Dltree.up cs with
      | None -> ()
      | Some up_cs ->
        if not (Dltree.is_root up_cs) then
          Option.iter (fun c -> tc##.classList##add(Js.string (c ^ "-c")))
                      (Dltree.get up_cs).csn_css_class;
        loop_up_cs up_cs in
    loop_up_rs rs;
    loop_up_cs cs;
    update_cell_rs_attribs rs tc;
    update_cell_cs_attribs cs tc

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
    assert (pos = tn.tn_tr##.rowIndex);
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
    assert (pos = tc##.cellIndex);
    tn.tn_tr##deleteCell(pos);
    tn.tn_tcs <- Cs_map.remove cs tn.tn_tcs

  let move_cell' tn_old tn_new cs =
    let found_old, pos_old = Cs_map.locate cs tn_old.tn_tcs in
    let found_new, pos_new = Cs_map.locate cs tn_new.tn_tcs in
    assert found_old;
    assert (not found_new);
    let tc = Cs_map.find cs tn_old.tn_tcs in
    let tc'_opt =
      if pos_new = Cs_map.cardinal tn_new.tn_tcs then None else
      Some (Cs_map.get tn_new.tn_tcs pos_new) in
    tn_old.tn_tcs <- Cs_map.remove cs tn_old.tn_tcs;
    Dom.removeChild tn_old.tn_tr tc;
    tn_new.tn_tcs <- Cs_map.add cs tc tn_new.tn_tcs;
    Dom.insertBefore tn_new.tn_tr tc (Js.Opt.option tc'_opt)

  let insert_cell tab rs cs =
    let rs_leaf = Dltree.first_leaf rs in
    assert (Rs_map.contains rs_leaf tab.tab_tns);
    let tn = Rs_map.find rs_leaf tab.tab_tns in
    let tc = insert_cell' tn cs in
    init_cell_attribs rs cs tc;
    tc

  let remove_cell tab rs cs =
    let rs_leaf = Dltree.first_leaf rs in
    assert (Rs_map.contains rs_leaf tab.tab_tns);
    let tn = Rs_map.find rs_leaf tab.tab_tns in
    remove_cell' tn cs

  let tr_of_node node =
    Js.coerce_opt (Dom_html.CoerceTo.element node) Dom_html.CoerceTo.tr
                  (fun _ -> assert false)

  let maybe_move_cell tc cs (tn_old, tn_new) =
    let tc_tr =
      tr_of_node (Js.Opt.get (tc##.parentNode) (fun _ -> assert false)) in
    if tc_tr == tn_old.tn_tr then
      move_cell' tn_old tn_new cs

  (* Refining -> Refined *)
  let add_refinement tab cov_blk =
    let cov_rs, cov_cs = cov_blk.blk_rs, cov_blk.blk_cs in
    match cov_blk.blk_state with
    | Refined _ | Single _ | Empty _ -> assert false
    | Refining (cov_lr, cov_lc, tc) ->
      assert (has_subblock cov_lr cov_lc cov_rs cov_cs);
      remove_cell tab cov_rs cov_cs;
      cov_blk.blk_state <- Refined (cov_lr, cov_lc);
      for_subblocks cov_lr cov_lc cov_rs cov_cs
        (fun rem_lr rem_lc sub_rs sub_cs ->
          let sub_rsn, sub_csn = Dltree.(get sub_rs, get sub_cs) in
          let sub_tc = insert_cell tab sub_rs sub_cs in
          let sub_blk = {
            blk_state = if rem_lr = 0 && rem_lc = 0
                        then Single sub_tc
                        else Empty (rem_lr, rem_lc, sub_tc);
            blk_rs = sub_rs;
            blk_cs = sub_cs;
          } in
          Hashtbl.add sub_rsn.rsn_blocks sub_csn.csn_id sub_blk)

  (* Single -> Refined | Refining *)
  let refine tab cov_lr cov_lc cov_rs cov_cs =
    let cov_rsn, cov_csn = Dltree.(get cov_rs, get cov_cs) in
    if not (Hashtbl.mem cov_rsn.rsn_blocks cov_csn.csn_id) then
      invalid_arg "Tabular.refine: Blockspan between or below a refinement.";
    let cov_blk = Hashtbl.find cov_rsn.rsn_blocks cov_csn.csn_id in
    match cov_blk.blk_state with
    | Refined _ -> invalid_arg "Tabular.refine: Already refined."
    | Refining _ -> invalid_arg "Tabular.refine: Already refined (but empty)."
    | Empty _ -> invalid_arg "Tabular.refine: Already refined (partial)."
    | Single tc ->
      cov_blk.blk_state <- Refining (cov_lr, cov_lc, tc);
      if has_subblock cov_lr cov_lc cov_rs cov_cs then
        add_refinement tab cov_blk

  (* Refined -> Refining *)
  let drop_refinement ?transfer ?skip_rs ?skip_cs tab cov_blk =
    let cov_rs, cov_cs = cov_blk.blk_rs, cov_blk.blk_cs in
    match cov_blk.blk_state with
    | Refining _ | Single _ | Empty _ -> assert false
    | Refined (cov_lr, cov_lc) ->
      for_subblocks ?skip_rs ?skip_cs cov_lr cov_lc cov_rs cov_cs
        (fun sub_lr sub_lc sub_rs sub_cs ->
          let sub_rsn, sub_csn = Dltree.(get sub_rs, get sub_cs) in
          assert (Hashtbl.mem sub_rsn.rsn_blocks sub_csn.csn_id);
          Hashtbl.remove sub_rsn.rsn_blocks sub_csn.csn_id;
          remove_cell tab sub_rs sub_cs);
      let cov_tc = insert_cell tab cov_rs cov_cs in
      cov_blk.blk_state <- Refining (cov_lr, cov_lc, cov_tc);
      Option.iter (maybe_move_cell cov_tc cov_cs) transfer

  (* Refined | Refining -> Single *)
  let unrefine tab cov_rs cov_cs =
    let cov_rsn, cov_csn = Dltree.(get cov_rs, get cov_cs) in
    if not (Hashtbl.mem cov_rsn.rsn_blocks cov_csn.csn_id) then
      invalid_arg "Tabular.unrefine: Blockspan between or below a refinement.";
    let cov_blk = Hashtbl.find cov_rsn.rsn_blocks cov_csn.csn_id in
    begin match cov_blk.blk_state with
    | Refined (cov_lr, cov_lc) -> drop_refinement tab cov_blk
    | Refining (cov_lr, cov_lc, cov_tc) -> ()
    | Empty _ -> invalid_arg "Tabular.unrefined: Between refinements."
    | Single _ -> invalid_arg "Tabular.unrefine: Not refined."
    end;
    match cov_blk.blk_state with
    | Refining (cov_lr, cov_lc, cov_tc) -> cov_blk.blk_state <- Single cov_tc
    | _ -> assert false

  (* Find the inner cover of (rs, cs) spanning exactly rs. *)
  let rec cover_at_rs rs cs =
    let rsn, csn = Dltree.(get rs, get cs) in
    try Some (Hashtbl.find rsn.rsn_blocks csn.csn_id)
    with Not_found ->
      Option.search (cover_at_rs rs) (Dltree.up cs)

  (* Find the inner cover of (rs, cs) spanning exactly cs. *)
  let rec cover_at_cs rs cs =
    let rsn, csn = Dltree.(get rs, get cs) in
    try Some (Hashtbl.find rsn.rsn_blocks csn.csn_id)
    with Not_found ->
      Option.search (fun rs -> cover_at_cs rs cs) (Dltree.up rs)

  let is_alloc tab rs cs =
    let rsn, csn = Dltree.get rs, Dltree.get cs in
    Hashtbl.mem rsn.rsn_blocks csn.csn_id

  let fill_cell ?(rem_lr = 0) ?(rem_lc = 0) tab rs cs =
    let rsn, csn = Dltree.get rs, Dltree.get cs in
    assert (rem_lr >= 0 && rem_lc >= 0);
    let tc = insert_cell tab rs cs in
    init_cell_attribs rs cs tc;
    let blk = {
      blk_state = if rem_lr = 0 && rem_lc = 0
                  then Single tc
                  else Empty (rem_lr, rem_lc, tc);
      blk_rs = rs;
      blk_cs = cs;
    } in
    Hashtbl.add rsn.rsn_blocks csn.csn_id blk

  let unfill_cell tab rs cs =
    let rsn, csn = Dltree.(get rs, get cs) in
    Hashtbl.remove rsn.rsn_blocks csn.csn_id;
    remove_cell tab rs cs

  let rec unfill tab rs cs =
    let rsn, csn = Dltree.(get rs, get cs) in
    assert (Hashtbl.mem rsn.rsn_blocks csn.csn_id);
    let blk = Hashtbl.find rsn.rsn_blocks csn.csn_id in
    begin match blk.blk_state with
    | Refined (lr, lc) -> drop_refinement tab blk
    | _ -> ()
    end;
    unfill_cell tab rs cs

  let update_rs_attribs tab rs =
    let rec loop_cs rem_lc div_cs =
      if rem_lc > 0 && not (Dltree.is_leaf div_cs) then
        Dltree.iter (loop_cs (rem_lc - 1)) div_cs else
      match cover_at_cs rs div_cs with
      | None -> ()
      | Some cov_blk ->
        begin match cov_blk.blk_state with
        | Empty (_, _, tc) | Single tc | Refining (_, _, tc) ->
          update_cell_rs_attribs cov_blk.blk_rs tc
        | Refined (lr, lc) ->
          if lc > 0 && not (Dltree.is_leaf div_cs) then loop_cs lc div_cs
        end in
    loop_cs 0 tab.tab_root_cs

  let update_cs_attribs tab cs =
    let rec loop_rs rem_lr div_rs =
      if rem_lr > 0 && not (Dltree.is_leaf div_rs) then
        Dltree.iter (loop_rs (rem_lr - 1)) div_rs else
      match cover_at_rs div_rs cs with
      | None -> ()
      | Some cov_blk ->
        begin match cov_blk.blk_state with
        | Empty (_, _, tc) | Single tc | Refining (_, _, tc) ->
          update_cell_cs_attribs cov_blk.blk_cs tc
        | Refined (lr, lc) ->
          if lr > 0 && not (Dltree.is_leaf div_rs) then loop_rs lr div_rs
        end in
    loop_rs 0 tab.tab_root_rs

  (* We just added new_rs, extend rowspans or allocate new cells. *)
  let alloc_rs ?transfer tab new_rs =

    (* First fix the ancestor rowspans if this is not the first subspan. *)
    if not (Dltree.is_only new_rs) then
      Dltree.iter_ancestors
        (fun rs ->
          let rsn = Dltree.get rs in
          rsn.rsn_span <- rsn.rsn_span + 1)
        new_rs;

    (* Then allocate or extend the cells. *)
    let rec loop_cs next_lr rem_lc div_cs =
      if rem_lc > 0 && not (Dltree.is_leaf div_cs) then
        Dltree.iter (loop_cs next_lr (rem_lc - 1)) div_cs else
      match cover_at_cs new_rs div_cs with
      | None -> (* At top rowspan within this colspan. *)
        let rem_lr = next_lr - Dltree.level new_rs in
        assert (rem_lr >= 0);
        fill_cell ~rem_lr ~rem_lc tab new_rs div_cs
      | Some cov_blk ->
        let cov_rs = cov_blk.blk_rs in
        assert (cov_rs != new_rs);
        assert (cov_blk.blk_cs == div_cs);
        begin match cov_blk.blk_state with
        | Empty (rem_lr', rem_lc', tc) ->
          Option.iter (maybe_move_cell tc div_cs) transfer;
          if rem_lr' = 0 then
            update_cell_rs_attribs cov_rs tc
          else begin
            assert (Dltree.level new_rs = Dltree.level cov_rs + 1);
            assert (rem_lc = rem_lc');
            unfill_cell tab cov_rs div_cs;
            let rem_lr = rem_lr'
                       - (Dltree.level new_rs - Dltree.level cov_rs) in
            assert (rem_lr >= 0);
            fill_cell ~rem_lr ~rem_lc tab new_rs div_cs
          end
        | Single tc ->
          assert (rem_lc = 0);
          Option.iter (maybe_move_cell tc div_cs) transfer;
          update_cell_rs_attribs cov_rs tc
        | Refining (lr, lc, tc) ->
          assert (rem_lc = 0);
          Option.iter (maybe_move_cell tc div_cs) transfer;
          update_cell_rs_attribs cov_rs tc;
          if has_subblock lr lc cov_rs div_cs then add_refinement tab cov_blk
        | Refined (lr, lc) ->
          assert (rem_lc = 0);
          if lc > 0 && not (Dltree.is_leaf div_cs) then
            loop_cs (Dltree.level cov_rs + lr) lc div_cs
          else begin
            let rem_lc = Dltree.level div_cs + lc - Dltree.level div_cs in
            let rem_lr = Dltree.level cov_rs + lr - Dltree.level new_rs in
            assert (rem_lr >= 0);
            fill_cell ~rem_lr ~rem_lc tab new_rs div_cs
          end
        end in
    loop_cs 0 0 tab.tab_root_cs

  let dealloc_rs ?transfer tab old_rs =

    if not (Dltree.is_only old_rs) then
      Dltree.iter_ancestors
        (fun rs -> let rsn = Dltree.get rs in rsn.rsn_span <- rsn.rsn_span - 1)
        old_rs;

    let rs = Option.get (Dltree.up old_rs) in

    let rec loop_cs next_lr rem_lc div_cs =
      if rem_lc > 0 && not (Dltree.is_leaf div_cs) then
        Dltree.iter (loop_cs next_lr (rem_lc - 1)) div_cs else
      match cover_at_cs rs div_cs with
      | None -> if is_alloc tab old_rs div_cs then unfill tab old_rs div_cs
      | Some cov_blk ->
        let cov_rs = cov_blk.blk_rs in
        begin match cov_blk.blk_state with
        | Single tc | Empty (_, _, tc) | Refining (_, _, tc) ->
          Option.iter (maybe_move_cell tc div_cs) transfer;
          update_cell_rs_attribs cov_rs tc
        | Refined (lr, lc) ->
          assert (rem_lc = 0);
          if not (has_subblock ~skip_rs:old_rs lr lc cov_rs div_cs) then begin
            drop_refinement ?transfer tab cov_blk
          end else if lc > 0 && not (Dltree.is_leaf div_cs) then
            loop_cs (Dltree.level cov_rs + lr) lc div_cs
          else if Dltree.level old_rs = Dltree.level cov_rs + lr then begin
            assert (lr > 0);
            assert (lr > 1 || not (Dltree.is_only old_rs));
            unfill tab old_rs div_cs;
          end
        end in
    loop_cs 0 0 tab.tab_root_cs

  (* We just added new_cs, extend colspans or allocate new cells. *)
  let alloc_cs tab new_cs =

    (* First fix the ancestor colspans if this is not the first subspan. *)
    if not (Dltree.is_only new_cs) then
      Dltree.iter_ancestors
        (fun cs ->
          let csn = Dltree.get cs in
          csn.csn_span <- csn.csn_span + 1)
        new_cs;

    (* Then allocate or extend the cells. *)
    let rec loop_rs rem_lr next_lc div_rs =
      if rem_lr > 0 && not (Dltree.is_leaf div_rs) then
        Dltree.iter (loop_rs (rem_lr - 1) next_lc) div_rs else
      match cover_at_rs div_rs new_cs with
      | None -> (* At top colspan within this rowspan. *)
        let rem_lc = next_lc - Dltree.level new_cs in
        assert (rem_lc >= 0);
        fill_cell ~rem_lr ~rem_lc tab div_rs new_cs
      | Some cov_blk ->
        let cov_cs = cov_blk.blk_cs in
        assert (cov_cs != new_cs);
        assert (cov_blk.blk_rs == div_rs);
        begin match cov_blk.blk_state with
        | Empty (rem_lr', rem_lc', tc) ->
          if rem_lc' = 0 then
            update_cell_cs_attribs cov_cs tc
          else begin
            assert (Dltree.level new_cs = Dltree.level cov_cs + 1);
            assert (rem_lr = rem_lr');
            unfill_cell tab div_rs cov_cs;
            let rem_lc = rem_lc'
                       - (Dltree.level new_cs - Dltree.level cov_cs) in
            assert (rem_lc >= 0);
            fill_cell ~rem_lr ~rem_lc tab div_rs new_cs
          end
        | Single tc ->
          assert (rem_lr = 0);
          update_cell_cs_attribs cov_cs tc
        | Refining (lr, lc, tc) ->
          assert (rem_lr = 0);
          update_cell_cs_attribs cov_cs tc;
          if has_subblock lr lc div_rs cov_cs then add_refinement tab cov_blk
        | Refined (lr, lc) ->
          assert (rem_lr = 0);
          if lr > 0 && not (Dltree.is_leaf div_rs) then
            loop_rs lr (Dltree.level cov_cs + lc) div_rs
          else begin
            let rem_lr = Dltree.level div_rs + lr - Dltree.level div_rs in
            let rem_lc = Dltree.level cov_cs + lc - Dltree.level new_cs in
            assert (rem_lc >= 0);
            fill_cell ~rem_lr ~rem_lc tab div_rs new_cs
          end
        end in
    loop_rs 0 0 tab.tab_root_rs

  let dealloc_cs tab old_cs =

    if not (Dltree.is_only old_cs) then
      Dltree.iter_ancestors
        (fun cs -> let csn = Dltree.get cs in csn.csn_span <- csn.csn_span - 1)
        old_cs;

    let cs = Option.get (Dltree.up old_cs) in

    let rec loop_rs next_lc rem_lr div_rs =
      if rem_lr > 0 && not (Dltree.is_leaf div_rs) then
        Dltree.iter (loop_rs next_lc (rem_lr - 1)) div_rs else
      match cover_at_rs div_rs cs with
      | None -> if is_alloc tab div_rs old_cs then unfill tab div_rs old_cs
      | Some cov_blk ->
        let cov_cs = cov_blk.blk_cs in
        begin match cov_blk.blk_state with
        | Single tc | Empty (_, _, tc) | Refining (_, _, tc) ->
          update_cell_cs_attribs cov_cs tc
        | Refined (lr, lc) ->
          assert (rem_lr = 0);
          if not (has_subblock ~skip_cs:old_cs lr lc div_rs cov_cs) then
            drop_refinement tab cov_blk
          else if lr > 0 && not (Dltree.is_leaf div_rs) then
            loop_rs (Dltree.level cov_cs + lc) lr div_rs
          else if Dltree.level old_cs = Dltree.level cov_cs + lc then begin
            assert (lc > 0);
            assert (lc > 1 || not (Dltree.is_only old_cs));
            unfill tab div_rs old_cs
          end
        end in
    loop_rs 0 0 tab.tab_root_rs

  module Rowspan = struct
    type t = rowspan_node Dltree.t
    let level = Dltree.level
    let is_root = Dltree.is_root
    let is_leaf = Dltree.is_leaf
    let is_first = Dltree.is_first
    let is_last = Dltree.is_last
    let is_only = Dltree.is_only
    let up = Dltree.up
    let first = Dltree.first
    let last = Dltree.last
    let next = Dltree.next
    let prev = Dltree.prev
    let first_leaf = Dltree.first_leaf
    let last_leaf = Dltree.last_leaf
    let fold = Dltree.fold
    let iter = Dltree.iter
    let iteri = Dltree.iteri
    let iterp = Dltree.iterp
    let exists = Dltree.exists

    let make_rsn css_class =
      { rsn_span = 1;
        rsn_blocks = Hashtbl.create 11;
        rsn_css_class = css_class; }

    let add_first tab ?css_class rs_u =
      match Dltree.first rs_u with
      | None ->
        assert (Rs_map.contains rs_u tab.tab_tns);
        let tn = Rs_map.find rs_u tab.tab_tns in
        let rsn = make_rsn css_class in
        let rs = Dltree.add_first rsn rs_u in
        tab.tab_tns <- Rs_map.add rs tn (Rs_map.remove rs_u tab.tab_tns);
        alloc_rs tab rs;
        rs
      | Some rs1' ->
        let rs1 = Dltree.first_leaf rs1' in
        assert (Rs_map.contains rs1 tab.tab_tns);
        let tn1 = Rs_map.find rs1 tab.tab_tns in
        let rsn0 = make_rsn css_class in
        let rs0 = Dltree.add_first rsn0 rs_u in
        let tn0 = insert_row tab rs0 in
        alloc_rs ~transfer:(tn1, tn0) tab rs0;
        update_rs_attribs tab rs1';
        rs0

    let add_before tab ?css_class rs_n =
      match Dltree.up rs_n with
      | None -> invalid_arg "Tabular.Rowspan.add_before"
      | Some rs_u ->
        if Dltree.is_first rs_n then add_first tab ?css_class rs_u else
        begin
          let rs = Dltree.add_before (make_rsn css_class) rs_n in
          let _ = insert_row tab rs in
          alloc_rs tab rs;
          rs
        end

    let add_after tab ?css_class rs_p =
      let rs = Dltree.add_after (make_rsn css_class) rs_p in
      let _ = insert_row tab rs in
      alloc_rs tab rs;
      if Dltree.is_last rs then update_rs_attribs tab rs_p;
      rs

    let add_last tab ?css_class rs =
      match Dltree.last rs with
      | None -> add_first tab ?css_class rs
      | Some last_rs -> add_after tab ?css_class last_rs

    let rec delete tab rs =
      let rs_u =
        match Dltree.up rs with
        | None -> invalid_arg "Tabular.Rowspan.delete: Cannot delete root."
        | Some rs_u -> rs_u in
      while not (Dltree.is_leaf rs) do
        Option.iter (delete tab) (Dltree.last rs)
      done;
      begin match Dltree.is_first rs, Dltree.is_last rs with
      | true, true ->
        assert (Rs_map.contains rs tab.tab_tns);
        let tn = Rs_map.find rs tab.tab_tns in
        dealloc_rs tab rs;
        tab.tab_tns <- Rs_map.add rs_u tn (Rs_map.remove rs tab.tab_tns);
        Dltree.delete_subtree rs
      | true, false ->
        assert (Rs_map.contains rs tab.tab_tns);
        let tn0 = Rs_map.find rs tab.tab_tns in
        let rs1 = Dltree.first_leaf (Option.get (Dltree.next rs)) in
        assert (Rs_map.contains rs1 tab.tab_tns);
        let tn1 = Rs_map.find rs1 tab.tab_tns in
        dealloc_rs ~transfer:(tn0, tn1) tab rs;
        remove_row tab rs;
        Dltree.delete_subtree rs;
        update_rs_attribs tab rs1
      | false, true ->
        let rs1 = Option.get (Dltree.prev rs) in
        dealloc_rs tab rs;
        remove_row tab rs;
        Dltree.delete_subtree rs;
        update_rs_attribs tab rs1
      | false, false ->
        dealloc_rs tab rs;
        remove_row tab rs;
        Dltree.delete_subtree rs
      end;
  end

  module Colspan = struct
    type t = colspan_node Dltree.t
    let level = Dltree.level
    let is_root = Dltree.is_root
    let is_leaf = Dltree.is_leaf
    let is_first = Dltree.is_first
    let is_last = Dltree.is_last
    let is_only = Dltree.is_only
    let up = Dltree.up
    let first = Dltree.first
    let last = Dltree.last
    let next = Dltree.next
    let prev = Dltree.prev
    let first_leaf = Dltree.first_leaf
    let last_leaf = Dltree.last_leaf
    let fold = Dltree.fold
    let iter = Dltree.iter
    let iteri = Dltree.iteri
    let iterp = Dltree.iterp
    let exists = Dltree.exists

    let make_csn tab css_class =
      let csn_id = tab.tab_next_col_id in
      tab.tab_next_col_id <- succ tab.tab_next_col_id;
      { csn_id;
        csn_span = 1;
        csn_css_class = css_class }

    let add_first tab ?css_class cs_up =
      let cs = Dltree.add_first (make_csn tab css_class) cs_up in
      alloc_cs tab cs;
      Option.iter (update_cs_attribs tab <@ Dltree.first_leaf) (Dltree.next cs);
      cs

    let add_last tab ?css_class cs_up =
      let cs = Dltree.add_last (make_csn tab css_class) cs_up in
      alloc_cs tab cs;
      Option.iter (update_cs_attribs tab <@ Dltree.last_leaf) (Dltree.prev cs);
      cs

    let add_before tab ?css_class cs_n =
      let cs = Dltree.add_before (make_csn tab css_class) cs_n in
      alloc_cs tab cs;
      if Dltree.is_first cs then update_cs_attribs tab (Dltree.first_leaf cs_n);
      cs

    let add_after tab ?css_class cs_p =
      let cs = Dltree.add_after (make_csn tab css_class) cs_p in
      alloc_cs tab cs;
      if Dltree.is_last cs then update_cs_attribs tab (Dltree.last_leaf cs_p);
      cs

    let rec delete tab cs =
      while not (Dltree.is_leaf cs) do
        Option.iter (delete tab) (Dltree.last cs)
      done;
      dealloc_cs tab cs;
      let sibl = Dltree.(prev cs, next cs) in
      Dltree.delete_subtree cs;
      match sibl with
      | None, Some cs_n -> update_cs_attribs tab (Dltree.first_leaf cs_n)
      | Some cs_p, None -> update_cs_attribs tab (Dltree.last_leaf cs_p)
      | None, None | Some _, Some _ -> ()
  end

  let get_tc tab rs cs =
    let rsn, csn = Dltree.(get rs, get cs) in
    try
      let blk = Hashtbl.find rsn.rsn_blocks csn.csn_id in
      match blk.blk_state with
      | Single tc -> tc
      | Empty _ | Refining _ | Refined _ ->
        invalid_arg "Tabular.get_tc: This cell is refined."
    with Not_found ->
      invalid_arg "Tabular.get_tc: Not refined at this cell."

  let set_tc tab rs cs tc' =
    let rsn, csn = Dltree.(get rs, get cs) in
    try
      let blk = Hashtbl.find rsn.rsn_blocks csn.csn_id in
      match blk.blk_state with
      | Single tc ->
        assert (tc##.rowSpan = rsn.rsn_span);
        assert (tc##.colSpan = csn.csn_span);
        init_cell_attribs rs cs tc';
        let rs_leaf = Dltree.first_leaf rs in
        assert (Rs_map.contains rs_leaf tab.tab_tns);
        let tn = Rs_map.find rs_leaf tab.tab_tns in
        assert (Cs_map.contains cs tn.tn_tcs);
        assert (Cs_map.find cs tn.tn_tcs == tc);
        blk.blk_state <- Single tc';
        tn.tn_tcs <- Cs_map.add cs tc' tn.tn_tcs;
        Dom.replaceChild tn.tn_tr tc' tc
      | Empty _ | Refining _ | Refined _ ->
        invalid_arg "Tabular.set_tc: This cell is refined."
    with Not_found ->
      invalid_arg "Tabular.set_tc: Not refined at this cell."

  (* Cf https://github.com/ocsigen/js_of_ocaml/pull/240 *)
  let coerce_to_tc : #Dom_html.element Js.t -> #Dom_html.tableCellElement Js.t =
    Js.Unsafe.coerce

  let draw tab rs cs el =
    set_tc tab rs cs (coerce_to_tc (Html.To_dom.of_element el))

  let draw_td tab rs cs ?a content =
    let td = Html.D.td ?a content in
    set_tc tab rs cs (coerce_to_tc (Html.To_dom.of_td td))

  let draw_th tab rs cs ?a content =
    let th = Html.D.th ?a content in
    set_tc tab rs cs (coerce_to_tc (Html.To_dom.of_th th))

  let create ?a ?root_css_class () =
    let table = Html.D.table ?a [] in
    let root_rsn =
      { rsn_span = 1; rsn_blocks = Hashtbl.create 11;
        rsn_css_class = root_css_class } in
    let root_csn =
      { csn_span = 1; csn_id = 0;
        csn_css_class = root_css_class; } in
    let tab =
      { tab_table = Html.To_dom.of_table table;
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

  type state =
    | Leaf
    | Split of int * int
    | Invalid

  let state tab rs cs =
    let rsn, csn = Dltree.(get rs, get cs) in
    try
      let blk = Hashtbl.find rsn.rsn_blocks csn.csn_id in
      match blk.blk_state with
      | Empty _ -> Invalid
      | Single _ -> Leaf
      | Refined (lr, lc) | Refining (lr, lc, _) -> Split (lr, lc)
    with Not_found -> Invalid

  let ui t = Html.Of_dom.of_table t.tab_table
  let root_rowspan t = t.tab_root_rs
  let root_colspan t = t.tab_root_cs

end
