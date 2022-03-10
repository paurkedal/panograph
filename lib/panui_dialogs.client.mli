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

open Eliom_content.Html
open Html_types

module Modal_dialog : sig

  type t

  val open_bare :
    ?on_cancel: (unit -> unit) ->
    [< div_content_fun] elt list -> t
  (** [open_bare content] adds a dialog box containing [content] to the root
      document. The box overlaps part of the content of the window, which is
      shaded to indicate a modal dialog. The box can be removed with {!close}.

      @param on_cancel will be called when the dialog box is closed. *)

  val open_std :
    ?on_cancel: (unit -> unit) ->
    [< div_content_fun] elt list ->
    [< div_content_fun] elt list -> t
  (** [open_std content footer] creates a dialog box like {!open_bare} but split
      vertically into [content] which is meant for text and [footer] which is
      meant for buttons.

      @param on_cancel will be called when the dialog box is closed. *)

  val close : t -> unit
  (** [close dialog] removes [dialog] from the root document. *)

  val close_all : unit -> unit
  (** [close_all ()] removes all dialog boxes from the root document. *)

end

val dialog_lwt :
  (button_content_fun elt list * 'a) list -> 'a ->
  [< div_content_fun] elt list -> 'a Lwt.t
(** [dialog_lwt [l1, v1; ...; lN, vN] v0 content] creates a dialog displaying
    [content] with [N] buttons labeled [l1, ..., lN] with associated values
    [v1, ..., vN], respectively, and waits for user input.  Pressing a button
    causes the associated value to be return, and pressing outside the dialog
    causes [v0] to be returned. *)

val acknowledge_lwt :
  ?ok: button_content_fun elt list ->
  [< div_content_fun] elt list -> unit Lwt.t
(** [acknowledge_lwt content] creates a dialog box with [content] and an single
    button used to close the it, and return a promise which will be resolved
    when closed.

    @param ok is the content of the button, by default "Ok". *)

val confirm_lwt :
  ?ok: button_content_fun elt list ->
  ?cancel: button_content_fun elt list ->
  [< div_content_fun] elt list -> bool Lwt.t
(** [confirm_lwt] creates a dialog box with [content] and two buttons which both
    close it, and returns a promise which will be resolved to [true] or [false]
    when the first or second button is used, respectively.

    @param ok is the content of the first button, by default "Ok".
    @param cancel is the content of the second button, by default "Cancel". *)
