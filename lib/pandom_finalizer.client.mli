(* Copyright (C) 2016--2019  Petter A. Urkedal <paurkedal@gmail.com>
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
