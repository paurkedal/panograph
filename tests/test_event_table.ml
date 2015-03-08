(* Copyright (C) 2015  Petter Urkedal <paurkedal@gmail.com>
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

module String_hashable = struct
  type t = string
  let hash = Hashtbl.hash
  let equal = (=)
end

module Et = Panograph_event_table.Make (String_hashable)

let test1 et =
  let a_ev = Et.event et "a" in
  let b_ev = Et.event et "b" in
  let a_r = ref 0 in
  let a_ev = React.E.trace (fun x -> a_r := !a_r + x) a_ev in
  let b_ev = React.E.trace (fun _ -> assert false) b_ev in
  assert (Et.size et = 2);
  Et.emit et "a" 1;
  Gc.full_major ();
  Et.emit et "a" 10;
  Et.emit et "a" 100;
  assert (!a_r = 111);
  assert (Et.size et = 2);
  ignore a_ev;
  ignore b_ev

let run () =
  let et = Et.create 23 in
  assert (Et.size et = 0);
  Et.emit et "a" 0;
  Et.emit et "b" 0;
  assert (Et.size et = 0);
  test1 et;
  Gc.full_major ();
  Et.emit et "b" 0;
  assert (Et.size et = 0)