(* Copyright (C) 2015--2017  Petter A. Urkedal <paurkedal@gmail.com>
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
open Unprime_string

let hidden_class = Js.string "pan-hidden"
let error_prefix = "** "
let error_suffix = " **\n"
let error_class = Js.string "pan-error"
let dirty_class = Js.string "pan-dirty"

let set_error msg dom =
  if not (Js.to_bool (dom##.classList##contains(error_class))) then begin
    dom##.classList##add(error_class);
    let orig_title = Js.to_string dom##.title in
    dom##.title := Js.string (error_prefix ^ msg ^ error_suffix ^ orig_title)
  end

let set_error_v2 err dom =
  set_error (Panui_error.message err) dom

let clear_error dom =
  if Js.to_bool (dom##.classList##contains(error_class)) then begin
    dom##.classList##remove(error_class);
    match String.cut_affix error_suffix (Js.to_string dom##.title) with
    | Some (_ as msg, orig_title) when String.has_prefix error_prefix msg ->
      dom##.title := Js.string orig_title
    | _ -> ()
  end

let flash_error msg dom =
  set_error msg dom;
  Lwt_js.sleep 4.0 >|= fun () ->
  clear_error dom

let set_dirty dom = dom##.classList##add(dirty_class)
let clear_dirty dom = dom##.classList##remove(dirty_class)
let set_hidden dom = dom##.classList##add(hidden_class)
let clear_hidden dom = dom##.classList##remove(hidden_class)
