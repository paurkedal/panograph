(* Copyright (C) 2015--2019  Petter A. Urkedal <paurkedal@gmail.com>
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

open Js_of_ocaml
open Printf

type ('k, 'a) t = {
  prefix : string;
  classify : 'k -> string;
}

type subscription = unit -> unit

let next_channel_no = ref 0

let create classify =
  let prefix = sprintf "-pan-wc%d-" !next_channel_no in
  incr next_channel_no;
  {prefix; classify}

let classifier chan k = Js.string (chan.prefix ^ chan.classify k)

let subscribe_class chan e k f =
  let cls = classifier chan k in
  e##.classList##add(cls);
  Js.Unsafe.set e cls f;
  (fun () -> e##.classList##remove(cls); Js.Unsafe.delete e cls)

let subscribe_id chan e k f =
  let cls = classifier chan k in
  e##.id := cls;
  Js.Unsafe.set e cls f;
  (fun () -> e##.id := Js.string ""; Js.Unsafe.delete e cls)

let unsubscribe g = g ()

let send (chan : ('k, 'a) t) (k : 'k) (v : 'a) =
  let cls = classifier chan k in
  let send_to e = (Js.Unsafe.get e cls : 'a -> unit) v in
  Js.Opt.iter (Dom_html.document##getElementById cls) send_to;
  let els = Dom_html.document##getElementsByClassName cls in
  for i = 0 to els##.length - 1 do
    send_to (els##item i)
  done
