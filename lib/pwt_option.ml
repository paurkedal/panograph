(* Copyright (C) 2015--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

let iter_s f = function None -> return () | Some x -> f x
let fold_s f = function None -> return | Some x -> f x
let for_all_s f = function None -> return true | Some x -> f x
let exists_s f = function None -> return false | Some x -> f x

let filter_s f = function
  | None -> return None
  | Some x ->
    bind (f x) (fun c -> return (if c then Some x else None))

let map_s f = function
  | None -> return None
  | Some x -> bind (f x) (fun r -> return (Some r))

let fmap_s f = function
  | None -> return None
  | Some x -> f x
