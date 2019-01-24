(* Copyright (C) 2014--2019  Petter A. Urkedal <paurkedal@gmail.com>
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

open Eliom_content.Html
open Js_of_ocaml
open Panograph_common
open Panograph_i18n
open Panograph_intf
open Panograph_types
open Unprime
open Unprime_option

module type SIMPLE_VALUE = sig
  include STRINGABLE
  val css_classes : string list
end

module type SIMPLE_SNAPSHOT_VIEWER = sig
  include BASIC_SHAPE_TYPE
  include SNAPSHOT_VIEWER
    with type shape := shape
     and type ui = Html_types.flow5 elt
end

module Simple_SV (Value : SIMPLE_VALUE) = struct
  include Basic_shape

  type value = Value.t
  type ui = Html_types.flow5 elt
  type t = ui * ui

  let default_shape = make_default_shape ("SV" :: Value.css_classes)

  let create ?(shape = default_shape) v =
    let c = D.txt (Value.to_string v) in
    let p = D.span ~a:(attribs_of_shape shape) [c] in
    (p, c), p

  let set (p, c) v = Manip.replaceChildren p [D.txt (Value.to_string v)]
end

module Simple_shape = struct
  type t = {
    input_a : Html_types.input_attrib attrib list;
  }
  let make ?(a = []) () = {input_a = a}
end

module type SIMPLE_PATCH_EDITOR = sig
  type value
  include BASIC_SHAPE_TYPE
  include RETRACTABLE_PATCH_EDITOR
     with type value := value
      and type key = value
      and type patch_out = [`Change of value * value]
      and type patch_in = [`Change of value * value]
      and type shape := shape
      and type ui = Html_types.flow5 elt
end

module type SIMPLE_SNAPSHOT_EDITOR = sig
  include BASIC_SHAPE_TYPE
  include SNAPSHOT_EDITOR
    with type shape := shape
     and type ui = Html_types.flow5 elt
end

module Simple_PE (Value : SIMPLE_VALUE) = struct
  include Basic_shape

  type value = Value.t
  type patch_out = [`Change of value * value]
  type patch_in = patch_out

  type ui = Html_types.flow5 elt
  type t = {
    w_dom : Dom_html.inputElement Js.t;
    w_saved_title : string;
    mutable w_value : value; (* as received *)
  }

  let default_shape = make_default_shape ("PE" :: Value.css_classes)

  let has_class w cls =
    Js.to_bool (w.w_dom##.classList##contains(Js.string cls))

  let set_dirty w =
    if not (has_class w "dirty") then
      w.w_dom##.classList##add(Js.string "dirty")

  let clear_dirty w =
    w.w_dom##.classList##remove(Js.string "dirty")

  let set_error w msg =
    if not (has_class w "error") then
      w.w_dom##.classList##add(Js.string "error");
    w.w_dom##.title := Js.string (msg ^ "\n" ^ w.w_saved_title)

  let clear_error w =
    w.w_dom##.classList##remove(Js.string "error");
    w.w_dom##.title := Js.string w.w_saved_title

  let patch w (`Change (_, x)) =
    clear_error w;
    w.w_value <- x;
    w.w_dom##.value := Js.string (Value.to_string x);
    clear_dirty w

  let get {w_dom} = Value.of_string (Js.to_string w_dom##.value)

  let create ?(shape = default_shape) ?on_patch init =
    let inp =
      D.Raw.input ~a:(D.a_input_type `Text :: attribs_of_shape shape) () in
    let w_dom = To_dom.of_input inp in
    let w = {w_dom; w_saved_title = Js.to_string w_dom##.title;
             w_value = init} in
    w_dom##.value := Js.string (Value.to_string init);
    Option.iter (fun on_patch ->
      let on_change _ _ =
        try
          set_dirty w;
          match%lwt on_patch (`Change (w.w_value, get w)) with
          | Ok () -> Lwt.return_unit
          | Error err -> set_error w (Panui_error.message err); Lwt.return_unit
        with
        | Invalid_input msg ->
          clear_dirty w; set_error w msg; Lwt.return_unit
        | Failure _ | Invalid_argument _ ->
          clear_dirty w; set_error w "Invalid input."; Lwt.return_unit in
      Lwt_js_events.(async @@ fun () -> changes w.w_dom on_change)) on_patch;
    w, (inp :> ui)

  type key = value
  let key_of_t w = w.w_value
  let key_of_value x = x
  let key_of_patch_in (`Change (x, x')) = if x = x' then x, None else x, Some x'
  let key_of_patch_out = key_of_patch_in
  let compare_key k w = Pervasives.compare k w.w_value
  let compare wA wB = Pervasives.compare wA.w_value wB.w_value
end

module Simple_SE (Value : SIMPLE_VALUE) = struct
  include Basic_shape

  type value = Value.t
  type ui = Html_types.flow5 elt
  type t = {
    w_dom : Dom_html.inputElement Js.t;
    w_saved_title : string;
  }

  let default_shape = make_default_shape ("SE" :: Value.css_classes)

  let snapshot w = Value.of_string (Js.to_string w.w_dom##.value)

  let set_error w msg =
    w.w_dom##.classList##add(Js.string "error");
    w.w_dom##.title := Js.string (msg ^ "\n" ^ w.w_saved_title)
  let clear_error w =
    w.w_dom##.classList##remove(Js.string "error");
    w.w_dom##.title := Js.string w.w_saved_title

  let create ?(shape = default_shape) ?init () =
    let inp =
      D.Raw.input ~a:(D.a_input_type `Text :: attribs_of_shape shape) () in
    let w_dom = To_dom.of_input inp in
    let w = {w_dom; w_saved_title = Js.to_string w_dom##.title} in
    Option.iter (fun x -> w_dom##.value := Js.string (Value.to_string x)) init;
    let on_change _ _ =
      Lwt.wrap begin fun () ->
        try clear_error w;
            ignore (Value.of_string (Js.to_string w_dom##.value))
        with
        | Invalid_input msg -> set_error w msg
        | Failure _ | Invalid_argument _ -> set_error w "Invalid input."
      end in
    Lwt_js_events.(async @@ fun () -> changes w.w_dom on_change);
    w, (inp :> ui)
end

module String_str = struct
  type t = string
  let of_string = ident
  let to_string = ident
  let css_classes = ["string"]
end

module Int_str = struct
  type t = int
  let of_string = int_of_string
  let to_string = string_of_int
  let css_classes = ["int"]
end

module Float_str = struct
  type t = float
  let of_string = float_of_string
  let to_string = string_of_float
  let css_classes = ["float"]
end

module Option_str (X : SIMPLE_VALUE) = struct
  type t = X.t option
  let of_string = function "" -> None | s -> Some (X.of_string s)
  let to_string = function None -> "" | Some x -> X.to_string x
  let css_classes = "option" :: X.css_classes
end

module String_option_str = Option_str (String_str)
module Int_option_str = Option_str (Int_str)
module Float_option_str = Option_str (Float_str)

module String_SV = Simple_SV (String_str)
module Int_SV = Simple_SV (Int_str)
module Float_SV = Simple_SV (Float_str)

module String_PE = Simple_PE (String_str)
module Int_PE = Simple_PE (Int_str)
module Float_PE = Simple_PE (Float_str)
module String_option_PE = Simple_PE (String_option_str)
module Int_option_PE = Simple_PE (Int_option_str)
module Float_option_PE = Simple_PE (Float_option_str)

module String_SE = Simple_SE (String_str)
module Int_SE = Simple_SE (Int_str)
module Float_SE = Simple_SE (Float_str)
module String_option_SE = Simple_SE (String_option_str)
module Int_option_SE = Simple_SE (Int_option_str)
module Float_option_SE = Simple_SE (Float_option_str)

module Lang_V = struct
  include Lang
  let css_classes = ["lang"]
end

module Lang_SV = Simple_SV (Lang_V)
module Lang_SE = Simple_SE (Lang_V)
module Lang_PE = Simple_PE (Lang_V)
