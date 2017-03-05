(* Copyright (C) 2017  Petter A. Urkedal <paurkedal@gmail.com>
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

[%%shared
  open Eliom_content.Html
  open Lwt.Infix
  open Panograph_types
  open Printf
  open Unprime
  open Unprime_option
]

class type%client ['a] handle = object
  method get : 'a
  method set : 'a -> unit
end

class%client virtual ['a, 'b] base_handle
  has_feedback input_elem choices_elem init_choice emit complete =
  let has_feedback = has_feedback <> Some false in
object (self)
  val input_dom = To_dom.of_input input_elem
  val choices_dom = To_dom.of_span choices_elem

  val mutable current_choice : 'a = init_choice
  val mutable expecting_select = false

  method virtual private of_label : string -> 'a option
  method virtual private to_label : 'a -> string
  method virtual private receive_completions : 'b -> 'a list

  method private commit choice =
    Pandom_style.set_hidden choices_dom;
    input_dom##.value := Js.string (self#to_label choice);
    if choice = current_choice then Lwt.return_unit else
    begin
      current_choice <- choice;
      if has_feedback then Pandom_style.set_dirty input_dom;
      (match%lwt emit choice with
       | Ok () ->
          Pandom_style.clear_error input_dom;
          Lwt.return_unit
       | Error msg ->
          if has_feedback then Pandom_style.clear_dirty input_dom;
          Pandom_style.set_error msg input_dom;
          Lwt.return_unit)
    end

  method private cancel =
    if not expecting_select then begin
      Pandom_style.set_hidden choices_dom;
      input_dom##.value := Js.string (self#to_label current_choice)
    end

  method private commit_exact =
    if expecting_select then Lwt.return_unit else
    let label = Js.to_string input_dom##.value in
    (match self#of_label label with
     | None -> Lwt.return_unit
     | Some choice ->
        Pandom_style.clear_error input_dom;
        self#commit choice)

  method private make_choice choice =
    let label_text, label_class =
      (match self#to_label choice with
       | "" -> ("(none)", ["pan-none"])
       | label -> (label, [])) in
    let choice_elem =
      D.span ~a:[D.a_class ("pan-choice" :: label_class)]
             [D.pcdata label_text] in
    let choice_dom = To_dom.of_span choice_elem in
    let on_choice_click _ _ =
      expecting_select <- false;
      self#commit choice in
    Lwt_js_events.(async @@ fun () -> clicks choice_dom on_choice_click);
    choice_elem

  method private suggest = () |> Pwt.async_updater @@ fun () ->
    (match%lwt complete (Js.to_string input_dom##.value) with
     | Ok completions' ->
        let completions = self#receive_completions completions' in
        Pandom_style.clear_hidden choices_dom;
        Pandom_style.clear_error input_dom;
        let choices = List.map self#make_choice completions in
        Manip.replaceChildren choices_elem choices;
        Lwt.return_unit
     | Error msg ->
        Pandom_style.set_error msg input_dom;
        Lwt.return_unit)

  method private expect_select =
    expecting_select <- true;
    Lwt_js.sleep 0.01 >|= fun () ->
    expecting_select <- false

  initializer
    let open Lwt_js_events in
    async (fun () -> mousedowns choices_dom (fun _ _ -> self#expect_select));
    async (fun () -> blurs input_dom (fun _ _ -> Lwt.return self#cancel));
    async (fun () -> focuses input_dom (fun _ _ -> Lwt.return self#suggest));
    async (fun () -> inputs input_dom (fun _ _ -> Lwt.return self#suggest));
    async (fun () -> changes input_dom (fun _ _ -> self#commit_exact));
    input_dom##.value := Js.string (self#to_label init_choice)

  method get = current_choice

  method set choice =
    if has_feedback then begin
      Pandom_style.clear_dirty input_dom;
      Pandom_style.clear_error input_dom
    end;
    current_choice <- choice;
    input_dom##.value := Js.string (self#to_label choice)
end

class%client string_req_handle
  has_feedback input_elem choices_elem init_choice emit complete =
object
  inherit [string, string list] base_handle
    has_feedback input_elem choices_elem init_choice emit complete

  val mutable completions : string list = []

  method private to_label choice =
    choice
  method private of_label label =
    if List.mem label completions then Some label else None
  method private receive_completions cs =
    completions <- cs;
    cs
end

class%client string_opt_handle
  has_feedback input_elem choices_elem init_choice emit complete =
object
  inherit [string option, string list] base_handle
    has_feedback input_elem choices_elem init_choice emit complete

  val mutable completions : string list = []

  method private to_label = function
   | None -> ""
   | Some choice -> choice
  method private of_label = function
   | "" -> Some None
   | label when List.mem label completions -> Some (Some label)
   | _ -> None
  method private receive_completions cs =
    completions <- cs;
    None :: List.map Option.some cs
end

class%client ['a] labelled_req_handle
  has_feedback input_elem choices_elem init_choice emit complete =
object
  inherit [string * 'a, (string * 'a) list] base_handle
    has_feedback input_elem choices_elem init_choice emit complete

  val mutable completions : 'b = []

  method private to_label choice = fst choice
  method private of_label label =
    try Some (label, List.assoc label completions)
    with Not_found -> None
  method private receive_completions cs =
    completions <- cs;
    cs
end

class%client ['a] labelled_opt_handle
  has_feedback input_elem choices_elem init_choice emit complete =
object
  inherit [(string * 'a) option, (string * 'a) list] base_handle
    has_feedback input_elem choices_elem init_choice emit complete

  val mutable completions : 'b = []

  method private to_label = function
   | None -> ""
   | Some choice -> fst choice
  method private of_label = function
   | "" -> Some None
   | label ->
      try Some (Some (label, List.assoc label completions))
      with Not_found -> None
  method private receive_completions cs =
    completions <- cs;
    None :: List.map Option.some cs
end


type%server 'a handle

[%%shared.start]

type ('a, 'b, 'attrib, 'elt) t =
    ?has_feedback: bool ->
    complete: (string -> 'b ui_result Lwt.t) Eliom_client_value.t ->
    emit: ('a -> unit ui_result Lwt.t) Eliom_client_value.t ->
    ?a: 'attrib attrib list ->
    'a -> 'elt elt * 'a handle Eliom_client_value.t
  constraint 'attrib = [< Html_types.common > `Class]
  constraint 'elt = [> `Span]

type ('a, 'attrib, 'elt) t_req = ('a, 'a list, 'attrib, 'elt) t
type ('a, 'attrib, 'elt) t_opt = ('a option, 'a list, 'attrib, 'elt) t

let make_elem ?(a = []) () =
  let input_elem = D.input ~a:[D.a_input_type `Text] () in
  let choices_elem = D.span ~a:[D.a_class ["pan-choices"]] [] in
  let main_elem = D.span ~a:(D.a_class ["pan-completion-input"] :: a) [
    input_elem;
    D.span ~a:[D.a_class ["pan-dropdown"]] [choices_elem];
  ] in
  (input_elem, choices_elem, main_elem)

let string : (string, 'attrib, 'elt) t_req =
  fun ?has_feedback ~complete ~emit ?a choice ->
  let input_elem, choices_elem, main_elem = make_elem ?a () in
  let h : string handle Eliom_client_value.t =
    [%client
      new string_req_handle ~%has_feedback
        ~%(input_elem : [`Input] elt) ~%(choices_elem : [`Span] elt)
        ~%choice ~%emit ~%complete] in
  (main_elem, h)

let string_option : (string, 'attrib, 'elt) t_opt =
  fun ?has_feedback ~complete ~emit ?a choice ->
  let input_elem, choices_elem, main_elem = make_elem ?a () in
  let h : string option handle Eliom_client_value.t =
    [%client
      new string_opt_handle ~%has_feedback
        ~%(input_elem : [`Input] elt) ~%(choices_elem : [`Span] elt)
        ~%choice ~%emit ~%complete] in
  (main_elem, h)

let labelled_int : (string * int, 'attrib, 'elt) t_req =
  fun ?has_feedback ~complete ~emit ?a choice ->
  let input_elem, choices_elem, main_elem = make_elem ?a () in
  let h : (string * int) handle Eliom_client_value.t =
    [%client
      new labelled_req_handle ~%has_feedback
        ~%(input_elem : [`Input] elt) ~%(choices_elem : [`Span] elt)
        ~%choice ~%emit ~%complete] in
  (main_elem, h)

let labelled_int_option : (string * int, 'attrib, 'elt) t_opt =
  fun ?has_feedback ~complete ~emit ?a choice ->
  let input_elem, choices_elem, main_elem = make_elem ?a () in
  let h : (string * int) option handle Eliom_client_value.t =
    [%client
      new labelled_opt_handle ~%has_feedback
        ~%(input_elem : [`Input] elt) ~%(choices_elem : [`Span] elt)
        ~%choice ~%emit ~%complete] in
  (main_elem, h)

let labelled_int32 : (string * int32, 'attrib, 'elt) t_req =
  fun ?has_feedback ~complete ~emit ?a choice ->
  let input_elem, choices_elem, main_elem = make_elem ?a () in
  let h : (string * int32) handle Eliom_client_value.t =
    [%client
      new labelled_req_handle ~%has_feedback
        ~%(input_elem : [`Input] elt) ~%(choices_elem : [`Span] elt)
        ~%choice ~%emit ~%complete] in
  (main_elem, h)

let labelled_int32_option : (string * int32, 'attrib, 'elt) t_opt =
  fun ?has_feedback ~complete ~emit ?a choice ->
  let input_elem, choices_elem, main_elem = make_elem ?a () in
  let h : (string * int32) option handle Eliom_client_value.t =
    [%client
      new labelled_opt_handle ~%has_feedback
        ~%(input_elem : [`Input] elt) ~%(choices_elem : [`Span] elt)
        ~%choice ~%emit ~%complete] in
  (main_elem, h)

let labelled_int64 : (string * int64, 'attrib, 'elt) t_req =
  fun ?has_feedback ~complete ~emit ?a choice ->
  let input_elem, choices_elem, main_elem = make_elem ?a () in
  let h : (string * int64) handle Eliom_client_value.t =
    [%client
      new labelled_req_handle ~%has_feedback
        ~%(input_elem : [`Input] elt) ~%(choices_elem : [`Span] elt)
        ~%choice ~%emit ~%complete] in
  (main_elem, h)

let labelled_int64_option : (string * int64, 'attrib, 'elt) t_opt =
  fun ?has_feedback ~complete ~emit ?a choice ->
  let input_elem, choices_elem, main_elem = make_elem ?a () in
  let h : (string * int64) option handle Eliom_client_value.t =
    [%client
      new labelled_opt_handle ~%has_feedback
        ~%(input_elem : [`Input] elt) ~%(choices_elem : [`Span] elt)
        ~%choice ~%emit ~%complete] in
  (main_elem, h)
