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

open Lwt

let iter_s f ht =
  Hashtbl.fold (fun k v intro -> intro >> f k v) ht return_unit

let iter_p f ht =
  let thunks = Hashtbl.fold (fun k v acc -> f k v :: acc) ht [] in
  Lwt.join thunks

let fold_s f ht acc =
  Hashtbl.fold (fun k v intro -> intro >>= f k v) ht (return acc)
