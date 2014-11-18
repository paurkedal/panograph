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
open Panograph_intf
open Panograph_simple
open Panograph_mapped
open Panograph_collection

let (>|=) = Lwt.(>|=)

module Int_order = struct
  type t = int
  let compare = compare
end

module Int_ul_CPE =
  Collection_PE (Int_PE) (Int_SE) (Ul_collection_container)
module Int_ul_MPE =
  Mapped_PE (Int_order) (Int_SV) (Int_PE) (Ul_mapped_container)

let test_int_editor () =
  let ev, send_ev = React.E.create () in
  let open Html5 in
  let on_patch p = Lwt_js.sleep 1.0 >> (send_ev p; Lwt.return Ack_ok) in
  let w, ui =
    Int_PE.create ~shape:Simple_shape.(make ~a:[F.a_title "test"] ())
		  ~on_patch 19 in
  Lwt_react.E.keep (Lwt_react.E.map (Int_PE.patch w) ev);
  ui

let test_float_editor () =
  let ev, send_ev = React.E.create () in
  let open Html5 in
  let on_patch p = Lwt_js.sleep 1.0 >> (send_ev p; Lwt.return Ack_ok) in
  let w, ui = Float_PE.create ~on_patch 0.01 in
  Lwt_react.E.keep (Lwt_react.E.map (Float_PE.patch w) ev);
  ui

let test_int_ul () =
  let ev, send_ev = React.E.create () in
  let on_patch p =
    Lwt.(async (fun () -> Lwt_js.sleep 0.33 >|= fun () -> send_ev p));
    Lwt.return Ack_ok in
  let init = [5; 7; 3; 11; 17; 13] in
  let coll_pe, coll_ui = Int_ul_CPE.create ~on_patch init in
  let on_mapped_patch (`Patch (k, (`Change (v, v')))) =
    send_ev (`Patch (`Change (k, - v')));
    Lwt.return Ack_ok in
  let mapped_pe, mapped_ui =
    Int_ul_MPE.create ~on_patch:on_mapped_patch
		      (List.map (fun k -> k, -k) init) in
  let update p =
    Int_ul_CPE.patch coll_pe p;
    let p' =
      match p with
      | `Add v -> `Add (v, -v)
      | `Remove v -> `Remove v
      | `Patch (`Change (v, v')) ->
	if v = v' then `Patch (v, None, `Change (-v, -v'))
		  else `Patch (v, Some v', `Change (-v, -v')) in
    Int_ul_MPE.patch mapped_pe p' in
  Lwt_react.E.keep (Lwt_react.E.map update ev);
  Html5.D.div [coll_ui; mapped_ui]

let render () =
  Html5.D.div [
    test_int_editor ();
    test_float_editor ();
    test_int_ul ();
  ]
