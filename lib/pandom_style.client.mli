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

(** Stylistic tweaks of elements *)

val hidden_class : Js.js_string Js.t

val error_class : Js.js_string Js.t
(** Class used to indicate that an input has a invalid value or failed to be
    commited for other reasons. *)

val dirty_class : Js.js_string Js.t
(** Class used to indicate that an input has not been committed. *)

val set_hidden : #Dom_html.element Js.t -> unit
val clear_hidden : #Dom_html.element Js.t -> unit

val set_error : string -> #Dom_html.element Js.t -> unit
(** [set_error msg elem] adjusts [elem] to indicate a failure with a tooltip
    reporting [msg]. *)

val clear_error : #Dom_html.element Js.t -> unit
(** [clear_error elem] clears the failure indicator of [elem], if present, and
    resets the original tooltip. *)

val flash_error : string -> #Dom_html.element Js.t -> unit Lwt.t
(** [flash_error msg elem] calls {!set_error} and {!clear_error} with a brief
    pause. *)

val set_dirty : #Dom_html.element Js.t -> unit
(** [set_dirty elem] adds {!dirty_class} to [elem]. *)

val clear_dirty : #Dom_html.element Js.t -> unit
(** [clear_dirty elem] removes {!dirty_class} from [elem]. *)
