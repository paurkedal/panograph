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

module Event_array = Panreact_event_array

type watch = {
  value_sn : int React.S.t;
  count_sn : int React.S.t;
  mutable value_ck : int;
  mutable count_ck : int;
}

let check watches =
  for i = 0 to Array.length watches - 1 do
    assert (React.S.value watches.(i).value_sn = watches.(i).value_ck);
    assert (React.S.value watches.(i).count_sn = watches.(i).count_ck)
  done

let run () =
  let d_max = 6 in
  let n_watch = 2 lsl d_max in
  let depth_sn, set_depth = React.S.create 4 in
  let elt_ev, send_elt = React.E.create () in
  let ea = Event_array.create depth_sn elt_ev in
  let mk_watch i =
    let value_sn = Event_array.hold ea i (-1) in
    let count_sn = React.S.fold (fun c _ -> succ c) 0 (Event_array.get ea i) in
    {value_sn; count_sn; value_ck = -1; count_ck = 0} in
  let watches = Array.init n_watch mk_watch in
  for round = 1 to 500 do
    let d = Random.int (d_max + 1) in
    set_depth d;
    let repeat = Random.int (1 lsl (Random.int (d + 1))) in
    for _ = 0 to repeat - 1 do
      let i = Random.int (2 lsl d) in
      let v = Random.int 16 in
      send_elt (i, v);
      watches.(i).value_ck <- v;
      watches.(i).count_ck <- watches.(i).count_ck + 1
    done;
    check watches
  done
