(* Copyright (C) 2016--2020  Petter A. Urkedal <paurkedal@gmail.com>
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

let section = Lwt_log_js.Section.make "panograph:finalizer"

let magic = "-pan-finalized-"
let magic_js = Js.string magic

type t = {
  finalize: unit -> unit;
  mutable mark: bool;
  mutable next: t;
}

let rec chain = {
  finalize = (fun () -> assert false);
  mark = false;
  next = chain;
}

let run () =
  let rec clear_marks d n =
    if d == chain then n else begin
      d.mark <- false;
      clear_marks d.next (succ n)
    end in
  let finalizer_count = clear_marks chain.next 0 in

  let es = Dom_html.document##getElementsByClassName(Js.string magic) in
  for i = 0 to es##.length - 1 do
    (Js.Unsafe.get (es##item i) magic).mark <- true
  done;

  let rec finalize_unmarked slot n =
    if slot.next == chain then n else begin
      if not slot.next.mark then begin
        slot.next.finalize ();
        slot.next <- slot.next.next;
        finalize_unmarked slot (succ n)
      end else
        finalize_unmarked slot.next n
    end in
  let finalized_count = finalize_unmarked chain 0 in

  Lwt_log_js.ign_info_f ~section "Ran %d of %d finalizers."
                        finalized_count finalizer_count

let triggered = ref false

let trigger () =
  if not !triggered then begin
    triggered := true;
    Lwt.async begin fun () ->
      run ();
      triggered := false;
      Lwt.return_unit
    end
  end

let autotrigger_count = ref 0
let enable n = autotrigger_count := n
let disable () = autotrigger_count := 0

let add_count = ref 0
let add f (e : #Dom_html.element Js.t) =
  incr add_count;
  if !autotrigger_count > 0 && !add_count mod !autotrigger_count = 0 then
    trigger ();
  let finalize =
    if Js.to_bool (e##.classList##contains magic_js) then
      let g : unit -> unit = Js.Unsafe.get e magic in
      (fun () -> g (); f ())
    else
      (e##.classList##add magic_js; f) in
  let d = {finalize; mark = true; next = chain.next} in
  chain.next <- d;
  Js.Unsafe.set e magic d
