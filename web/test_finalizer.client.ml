(* Copyright (C) 2016--2017  Petter A. Urkedal <paurkedal@gmail.com>
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

open Eliom_content.Html
open Unprime_option

let created_count = ref 0
let finalized_count = ref 0

let create () =
  let is_finalized = ref false in
  let finalize () =
    assert (not !is_finalized);
    is_finalized := true;
    incr finalized_count in
  incr created_count;
  let node = D.span ~a:[D.a_class ["nesting"]] [] in
  Pandom_finalizer.add finalize (To_dom.of_span node);
  node

let rec mutate d node =
  let c = Random.int 32 in
  let n = Manip.childLength node in
  if c < n then
    if d >= 8 || Random.bool () then
      Manip.removeChild node (Option.get (Manip.nth node c))
    else
      mutate (succ d) (Option.get (Manip.nth node c))
  else
    Manip.appendChild node (create ())

let rec update stat node =
  (* Pandom_finalizer.run (); *)
  mutate 0 node;
  let reachable_count =
    Dom_html.document##(getElementsByClassName(Js.string "-pan-finalized-"))
                     ##.length in
  Manip.replaceChildren stat [
    D.pcdata "Created ";
    D.pcdata (string_of_int !created_count);
    D.pcdata ", finalized ";
    D.pcdata (string_of_int !finalized_count);
    D.pcdata ", can reach ";
    D.pcdata (string_of_int reachable_count);
  ];
  Lwt_js.sleep 0.01 >> update stat node

let render () =
  Pandom_finalizer.enable 100;
  let stat = D.p [] in
  let node = create () in
  let tree = D.p [node] in
  Lwt.async (fun () -> update stat node);
  D.div [stat; tree]
