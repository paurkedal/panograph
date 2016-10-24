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

let check watchers expected =
  for i = 0 to Array.length watchers - 1 do
    assert (React.S.value watchers.(i) = expected.(i))
  done

let run () =
  let d_max = 7 in
  let n_watch = 1 lsl d_max in
  let depth_sn, set_depth = React.S.create 4 in
  let elt_ev, send_elt = React.E.create () in
  let disp = Event_array.create depth_sn elt_ev in
  let watchers =
    Array.init n_watch (fun i -> React.S.hold 0 (Event_array.get disp i)) in
  let expected = Array.make n_watch 0 in
  for round = 0 to 999 do
    let d = Random.int (d_max + 1) in
    set_depth d;
    let count = 1 lsl (Random.int (d + 1)) in
    for _ = 0 to count - 1 do
      let i = Random.int (1 lsl d) in
      expected.(i) <- i + round;
      send_elt (i, i + round)
    done;
    check watchers expected
  done
