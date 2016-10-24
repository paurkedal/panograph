(* Copyright (C) 2016  Petter A. Urkedal <paurkedal@gmail.com>
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

open Unprime

type 'a t = (int * 'a) React.event array list React.signal

let never_eq _ _ = false

let create d_sn ev =

  let rec shrink dd = function
   | [] -> assert false
   | arr :: arrs when dd > 0 ->
      Array.iter React.E.stop arr;
      shrink (dd - 1) arrs
   | arrs ->
      assert (dd = 0); arrs in

  let rec grow d_new d arr arrs =
    if d = d_new then arr :: arrs else
    let mask = 1 lsl d - 1 in
    let size' = 1 lsl (d + 1) in
    let mask' = size' - 1 in
    let event i =
      let cond (j, _) = j land mask' = i in
      React.E.filter cond arr.(i land mask) in
    let arr' = Array.init size' event in
    grow d_new (d + 1) arr' (arr :: arrs) in

  let resize arrs d_new =
    let d = Prime_int.ceil_log2 (Array.length (List.hd arrs)) in
    if d > d_new then shrink (d - d_new) arrs else
    if d < d_new then grow d_new d (List.hd arrs) (List.tl arrs) else
    arrs in

  let d_sn = React.S.l1 (max 0) d_sn in
  let init = grow (React.S.value d_sn) 0 [|ev|] [] in
  React.S.fold ~eq:(==) resize init (React.S.changes d_sn)

let bind_event_signal sn f =
  let f' x = React.S.hold ~eq:never_eq None (React.E.Option.some (f x)) in
  React.E.fmap ident (React.S.changes (React.S.bind ~eq:never_eq sn f'))

let get arrs_sn i =
  bind_event_signal arrs_sn @@ fun arrs ->
  let arr = List.hd arrs in
  React.E.fmap
    (fun (j, x) -> if j = i then Some x else None)
    arr.(i mod Array.length arr)

let hold ?eq arrs_sn i c = React.S.hold ?eq c (get arrs_sn i)
