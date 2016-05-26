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

open Eliom_content.Html5
open Panograph_basic_editors
open Panui_combo_selectors
open Panui_content

[%%shared
  open Panograph_types
]
[%%client
  module Dep = Panui_combo_selectors

  let ev, emit_ev =
    (React.E.create () : (float, float) either option React.event * _)

  let emit x =
    Lwt_js.sleep 0.4 >>
    (emit_ev x; Lwt.return Ack_ok)
]

let render () =
  let selection = Selection.F.([
    none "";
    some int32 "Number two" 2l;
    some int32 "Number three" 3l;
    some int32 "Number five" 5l;
    some int32 "Number seven" 7l;
    group "Above ten" [
      some int32 "Number eleven" 11l;
      some int32 "Number thirteen" 13l;
      some int32 "Number seventeen" 17l;
    ];
  ]) in
  let elem, absorb =
    int32_string_option_combo_selector ~inl_selection:selection
                                       [%client emit] in
  let emitI =
    [%client function Some x -> emit (Some (Inl x)) | None -> emit None] in
  let emitS =
    [%client function Some x -> emit (Some (Inr x)) | None -> emit None] in
  let elemI, absorbI = int32_option_editor emitI in
  let elemS, absorbS = string_option_editor emitS in
  let absorbI = [%client
    let absorbI : int32 option -> unit = ~%absorbI in
    let absorbS : int32 option -> unit = ~%absorbS in
    function
    | Some (Inl x) -> absorbI (Some x); absorbS None
    | Some (Inr x) -> absorbS (Some x); absorbI None
    | _ -> absorbI None; absorbS None
  ] in
  ignore [%client
    Lwt_react.E.keep (React.E.trace ~%absorb ev);
    Lwt_react.E.keep (React.E.trace ~%absorbI ev)
  ];
  D.div [elem; D.pcdata " = "; elemI; D.pcdata " | "; elemS]
