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

open Lwt

type 'a t = ('a Lwt.t * 'a Lwt.u) ref
type 'a tangle = 'a t * ('a -> unit)

let create () =
  let ev_r = ref (Lwt.task ()) in
  let emit x =
    let ev = !ev_r in
    ev_r := Lwt.task ();
    Lwt.wakeup (snd ev) x in
  ev_r, emit

let next ev_r = fst !ev_r

let iter_s h ev_r =
  let rec loop () = next ev_r >>= h >>= loop in
  loop ()

let iter h ev_r =
  let rec loop () = next ev_r >>= fun x -> Lwt.return (h x) >>= loop in
  loop ()
