(* Copyright (C) 2015--2016  Petter A. Urkedal <paurkedal@gmail.com>
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

open Lwt.Infix
open Printf

let when_s c f = if c then f () else Lwt.return_unit

let failwith s = Lwt.fail (Failure s)
let failwith_f fmt = ksprintf failwith fmt
let invalid_arg s = Lwt.fail (Invalid_argument s)
let invalid_arg_f fmt = ksprintf invalid_arg fmt

let async_updater f =
  let cond = Lwt_condition.create () in
  let state = ref None in
  let is_finalised = ref false in
  let rec loop () =
    match !state with
    | None ->
      if !is_finalised then Lwt.return_unit
                       else Lwt_condition.wait cond >> loop ()
    | Some x ->
      state := None;
      f x >> loop () in
  let g x =
    state := Some x;
    Lwt_condition.signal cond () in
  Lwt.async loop;
  Gc.finalise (fun _ -> is_finalised := true; Lwt_condition.signal cond ()) g;
  g
