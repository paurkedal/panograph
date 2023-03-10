(* Copyright (C) 2015--2023  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the LGPL-3.0 Linking Exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * and the LGPL-3.0 Linking Exception along with this library.  If not, see
 * <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.
 *)

open Lwt
open Unprime_list

let rec fold_s f = function
  | [] -> return
  | x :: xs -> fun acc -> bind (f x acc) (fold_s f xs)

let rec find_map_s f = function
  | [] -> return None
  | x :: xs ->
    bind (f x) (function Some _ as r -> return r | None -> find_map_s f xs)

let rec find_map_p f = function
  | [] -> return None
  | x :: xs ->
    let m = find_map_s f xs in
    bind (f x) (function Some _ as r -> return r | None -> m)

let flatten_map_s f xs =
  let rec loop acc = function
    | [] -> Lwt.return (List.rev acc)
    | x :: xs ->
      let%lwt ys = f x in
      loop (List.rev_append ys acc) xs in
  loop [] xs

let flatten_map_p f xs =
  Lwt_list.rev_map_p f xs >|= fun yss -> List.(fold rev_append) yss []

(* deprecated *)
let search_s = find_map_s
let search_p = find_map_p
