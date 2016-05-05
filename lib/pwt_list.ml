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

open Lwt

let rec fold_s f = function
  | [] -> return
  | x :: xs -> fun acc -> bind (f x acc) (fold_s f xs)

let rec search_s f = function
  | [] -> return None
  | x :: xs ->
    bind (f x) (function Some _ as r -> return r | None -> search_s f xs)

let rec search_p f = function
  | [] -> return None
  | x :: xs ->
    let m = search_s f xs in
    bind (f x) (function Some _ as r -> return r | None -> m)

let rec fmap_s f = function
  | [] -> return []
  | x :: xs ->
    let%lwt yo = f x in
    let%lwt ys = fmap_s f xs in
    return (match yo with None -> ys | Some y -> y :: ys)

let rec fmap_p f = function
  | [] -> return []
  | x :: xs ->
    let%lwt yo = f x and ys = fmap_p f xs in
    return (match yo with None -> ys | Some y -> y :: ys)
