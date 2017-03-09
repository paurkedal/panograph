(* Copyright (C) 2014--2017  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Common type definitions. *)

type ('a, 'b) either = Inl of 'a | Inr of 'b [@@deriving json]

type ui_error = string
  [@@ocaml.deprecated "Replaced by Panui_error.t"]
type 'a ui_result = ('a, ui_error) result
  [@@ocaml.deprecated "Replaced by Panui_result.t"]

type ack =
  | Ack_ok
  | Ack_error of string
(** @deprecated This is only used for some older widget, and will be phased out.
    Newer widgets use [result]. *)
