(* Copyright (C) 2016--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Finalizers for DOM Elements using the document as GC root. *)

open Js_of_ocaml

val add : (unit -> unit) -> #Dom_html.element Js.t -> unit
(** [add f elt] causes [f ()] to be called from the next call to [run ()]
    after [elt] is no longer linked to the current document. *)

val run : unit -> unit
(** [run ()] immediately runs all finalizers.  You should generally use
    {!trigger} or {!enable} instead. *)

val trigger : unit -> unit
(** [trigger ()] requests [run ()] to be called asynchronously, ignoring any
    additional requests until the acutal run happens. *)

val enable : int -> unit
(** [enable n] requests an automatic call to {!trigger} on about every [n]
    call to {!add}. *)

val disable : unit -> unit
(** [disable ()] disables the automatic trigger. *)
