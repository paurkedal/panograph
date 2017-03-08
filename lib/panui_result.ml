(* Copyright (C) 2017  Petter A. Urkedal <paurkedal@gmail.com>
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

type 'a t = ('a, Panui_error.t) result

let ok x = Ok x

let error ?tags ?doc msg = Error (Panui_error.create ?tags ?doc msg)

let catch ?tags ?doc ~msg ?report f =
  try Ok (f ()) with exn ->
  (match report with
   | None -> ()
   | Some report -> report exn);
  error ?tags ?doc msg

let catch_lwt ?tags ?doc ~msg ?report f =
  try%lwt f () >|= ok with exn ->
  (match report with
   | None -> Lwt.return_unit
   | Some report -> report exn) >|= fun () ->
  error ?tags ?doc msg

let invalid_input ?doc msg = error ~tags:["origin:user"] ?doc msg
let missing_input ?doc msg = error ~tags:["origin:user"] ?doc msg

let convert_input ?(tags = ["origin:user"]) ?doc ?(msg = "Invalid input") f x =
  (try Ok (f x) with
   | Invalid_argument _
   | Failure _ -> error ~tags ?doc msg)

let require_input ?(tags = ["origin:user"]) ?doc ?(msg = "Missing input") =
  (function
   | Some x -> Ok x
   | None -> error ~tags ?doc msg)
