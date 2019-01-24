(* Copyright (C) 2015--2019  Petter A. Urkedal <paurkedal@gmail.com>
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

open Js_of_ocaml
open Printf
open Unprime

type ('e, 'a) t = string constraint 'e = #Dom_html.element Js.t

let last_serial = ref 0
let create () = incr last_serial; sprintf "__weaktbl%d" !last_serial

let elements ?top wt =
  let nodelist =
    let sel = Js.string ("." ^ wt) in
    match top with
    | None -> Dom_html.document##querySelectorAll(sel)
    | Some elem -> elem##querySelectorAll(sel) in
  (Js.Unsafe.coerce nodelist :> #Dom_html.element Dom.nodeList Js.t)

let unsafe_find wt e = Js.Unsafe.get e wt

let add wt e x =
  Js.Unsafe.set e wt x;
  e##.classList##add(Js.string wt)

let remove wt e =
  e##.classList##remove(Js.string wt);
  Js.Unsafe.delete e wt

let find wt e =
  if Js.to_bool (e##.classList##contains(Js.string wt))
  then Some (unsafe_find wt e)
  else None

let iter ?top f wt =
  let es = elements ?top wt in
  for i = 0 to es##.length - 1 do
    Js.Opt.iter (es##item(i)) @@ fun e ->
    f e (unsafe_find wt e)
  done

let fold ?top f wt acc =
  let es = elements ?top wt in
  let acc_r = ref acc in
  for i = 0 to es##.length - 1 do
    Js.Opt.iter (es##item(i)) @@ fun e ->
    acc_r := f e (unsafe_find wt e) !acc_r
  done;
  !acc_r

let for_all ?top f wt =
  let es = elements ?top wt in
  let rec loop i =
    i < es##.length
      && Js.Opt.case (es##item(i)) (konst true)
                     (fun e -> f e (unsafe_find wt e))
      && loop (succ i) in
  loop 0

let exists ?top f wt =
  let es = elements ?top wt in
  let rec loop i =
    i = es##.length
      || Js.Opt.case (es##item(i)) (konst false)
                     (fun e -> f e (unsafe_find wt e))
      || loop (succ i) in
  loop 0
