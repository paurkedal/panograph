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

[%%server
  open Panograph_prereq
]
[%%shared
  open Lwt.Infix
  open Eliom_content.Html
]

[%%client
  open Js_of_ocaml_lwt

  let pcdata_f fmt = Printf.kprintf F.txt fmt

  let levels = Lwt_log_js.[|Debug; Info; Notice; Warning; Error; Fatal|]

  let rec populate pinboard i =
    let timeout = Random.float 4.0 in
    Panui_pinboard.pin ~subject:[pcdata_f "Item %d pinned for %g s" i timeout]
                       ~level:levels.(Random.int (Array.length levels))
                       ~timeout pinboard |> ignore;
    Lwt_js.sleep (Random.float 1.0) >>= fun () ->
    populate pinboard (succ i)
]

let handler () () =
  let pinboard = Panui_pinboard.create () in
  ignore_cv [%client Lwt.async (fun () -> populate ~%pinboard 0)];
  Lwt.return [Panui_pinboard.ui pinboard]
