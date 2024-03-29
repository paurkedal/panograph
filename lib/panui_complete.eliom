(* Copyright (C) 2017--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

[%%shared
  open Eliom_content.Html
  open Unprime_option
]
[%%client
  open Js_of_ocaml
  open Js_of_ocaml_lwt
  open Lwt.Infix
  open Panograph_prereq
]

class type%client ['a] handle = object
  method get : 'a
  method set : 'a -> unit
end

class%client virtual ['a, 'b] base_handle
    has_feedback input_elem choices_elem init_choice emit complete =
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
      (match emit with
       | None -> Lwt.return_unit;
       | Some emit ->
          (match%lwt emit choice with
           | Ok () ->
              Pandom_style.clear_error input_dom;
              Lwt.return_unit
           | Error err ->
              if has_feedback then Pandom_style.clear_dirty input_dom;
              Pandom_style.set_error_v2 err input_dom;
              Lwt.return_unit))
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
             [D.txt label_text] in
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
     | Error err ->
        Pandom_style.set_error_v2 err input_dom;
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
  let has_feedback =
    (match has_feedback, emit with
     | Some has_feedback, _ -> has_feedback
     | None, Some _ -> true
     | None, None -> false) in
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
  let has_feedback =
    (match has_feedback, emit with
     | Some has_feedback, _ -> has_feedback
     | None, Some _ -> true
     | None, None -> false) in
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
    List.map Option.some cs
end

class%client ['a] labelled_req_handle
    has_feedback input_elem form_input_elem choices_elem
    init_choice emit complete to_string =
  let has_feedback =
    (match has_feedback, emit with
     | Some has_feedback, _ -> has_feedback
     | None, Some _ -> true
     | None, None -> false) in
  let emit =
    (match form_input_elem with
     | None -> emit
     | Some form_input_elem ->
        let dom = To_dom.of_input form_input_elem in
        let form_input_emit x = dom##.value := Js.string (to_string x) in
        (match emit with
         | None -> Some (fun x -> form_input_emit x; Lwt.return (Ok ()))
         | Some emit -> Some (fun x -> form_input_emit x; emit x))) in
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
    has_feedback input_elem form_input_elem choices_elem
    init_choice emit complete to_string =
  let has_feedback =
    (match has_feedback, emit with
     | Some has_feedback, _ -> has_feedback
     | None, Some _ -> true
     | None, None -> false) in
  let emit =
    (match form_input_elem with
     | None -> emit
     | Some form_input_elem ->
        let dom = To_dom.of_input form_input_elem in
        let form_input_emit x = dom##.value := Js.string (to_string x) in
        (match emit with
         | None -> Some (fun x -> form_input_emit x; Lwt.return (Ok ()))
         | Some emit -> Some (fun x -> form_input_emit x; emit x))) in
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
    List.map Option.some cs
end


type%server 'a handle

[%%shared.start]

type ('a, 'b, 'c, 'attrib, 'elt) t =
    ?has_feedback: bool ->
    complete: (string -> 'b Panui_result.t Lwt.t) Eliom_client_value.t ->
    ?name: 'c Eliom_parameter.param_name ->
    ?emit: ('a -> unit Panui_result.t Lwt.t) Eliom_client_value.t ->
    ?a: 'attrib attrib list ->
    'a -> 'elt elt * 'a handle Eliom_client_value.t
  constraint 'attrib = [< Html_types.common > `Class]
  constraint 'elt = [> `Span]

type ('a, 'attrib, 'elt) simple_req =
  ('a, 'a list, [`One of 'a], 'attrib, 'elt) t

type ('a, 'attrib, 'elt) simple_opt =
  ('a option, 'a list, [`One of 'a], 'attrib, 'elt) t

type ('a, 'attrib, 'elt) labelled_req =
  (string * 'a, (string * 'a) list, [`One of 'a], 'attrib, 'elt) t

type ('a, 'attrib, 'elt) labelled_opt =
  ((string * 'a) option, (string * 'a) list, [`One of 'a], 'attrib, 'elt) t


let make_string ?(a = []) ?name param =
  let input_elem =
    (match name with
     | None -> D.input ~a:[D.a_input_type `Text] ()
     | Some name ->
        D.Form.input ~a ~input_type:`Text ~name param) in
  let choices_elem = D.span ~a:[D.a_class ["pan-choices"]] [] in
  let main_elem = D.span ~a:(D.a_class ["pan-completion-input"] :: a) [
    input_elem;
    D.span ~a:[D.a_class ["pan-dropdown"]] [choices_elem];
  ] in
  (input_elem, choices_elem, main_elem)

let make_labelled ?(a = []) ?name ?value param =
  let input_elem = D.input ~a:[D.a_input_type `Text] () in
  let choices_elem = D.span ~a:[D.a_class ["pan-choices"]] [] in
  let main_elem, form_input_elem =
    (match name with
     | None ->
        let main_elem =
          D.span ~a:(D.a_class ["pan-completion-input"] :: a) [
            input_elem;
            D.span ~a:[D.a_class ["pan-dropdown"]] [choices_elem];
          ] in
        (main_elem, None)
     | Some name ->
        let form_input_elem =
          D.Form.input ~a ~input_type:`Hidden ~name ?value param in
        let main_elem =
          D.span ~a:(D.a_class ["pan-completion-input"] :: a) [
            input_elem; form_input_elem;
            D.span ~a:[D.a_class ["pan-dropdown"]] [choices_elem];
          ] in
        (main_elem, Some form_input_elem)) in
  (input_elem, form_input_elem, choices_elem, main_elem)

let string : (string, 'attrib, 'elt) simple_req =
  fun ?has_feedback ~complete ?name ?emit ?a choice ->
  let input_elem, choices_elem, main_elem =
    make_string ?a ?name D.Form.string in
  let h : string handle Eliom_client_value.t =
    [%client
      new string_req_handle ~%has_feedback
        ~%(input_elem : [`Input] elt)
        ~%(choices_elem : [`Span] elt)
        ~%choice ~%emit ~%complete] in
  (main_elem, h)

let string_option : (string, 'attrib, 'elt) simple_opt =
  fun ?has_feedback ~complete ?name ?emit ?a choice ->
  let input_elem, choices_elem, main_elem =
    make_string ?a ?name D.Form.string in
  let h : string option handle Eliom_client_value.t =
    [%client
      new string_opt_handle ~%has_feedback
        ~%(input_elem : [`Input] elt)
        ~%(choices_elem : [`Span] elt)
        ~%choice ~%emit ~%complete] in
  (main_elem, h)

let labelled_int : (int, 'attrib, 'elt) labelled_req =
  fun ?has_feedback ~complete ?name ?emit ?a choice ->
  let input_elem, form_input_elem, choices_elem, main_elem =
    make_labelled ?a ?name ~value:(snd choice) D.Form.int in
  let h : (string * int) handle Eliom_client_value.t =
    [%client
      new labelled_req_handle ~%has_feedback
        ~%(input_elem : [`Input] elt)
        ~%(form_input_elem : [`Input] elt option)
        ~%(choices_elem : [`Span] elt)
        ~%choice ~%emit ~%complete (string_of_int % snd)] in
  (main_elem, h)

let labelled_int_option : (int, 'attrib, 'elt) labelled_opt =
  fun ?has_feedback ~complete ?name ?emit ?a choice ->
  let input_elem, form_input_elem, choices_elem, main_elem =
    make_labelled ?a ?name ?value:(Option.map snd choice) D.Form.int in
  let h : (string * int) option handle Eliom_client_value.t =
    [%client
      new labelled_opt_handle ~%has_feedback
        ~%(input_elem : [`Input] elt)
        ~%(form_input_elem : [`Input] elt option)
        ~%(choices_elem : [`Span] elt)
        ~%choice ~%emit ~%complete (string_of_option (string_of_int % snd))] in
  (main_elem, h)

let labelled_int32 : (int32, 'attrib, 'elt) labelled_req =
  fun ?has_feedback ~complete ?name ?emit ?a choice ->
  let input_elem, form_input_elem, choices_elem, main_elem =
    make_labelled ?a ?name ~value:(snd choice) D.Form.int32 in
  let h : (string * int32) handle Eliom_client_value.t =
    [%client
      new labelled_req_handle ~%has_feedback
        ~%(input_elem : [`Input] elt)
        ~%(form_input_elem : [`Input] elt option)
        ~%(choices_elem : [`Span] elt)
        ~%choice ~%emit ~%complete (Int32.to_string % snd)] in
  (main_elem, h)

let labelled_int32_option : (int32, 'attrib, 'elt) labelled_opt =
  fun ?has_feedback ~complete ?name ?emit ?a choice ->
  let input_elem, form_input_elem, choices_elem, main_elem =
    make_labelled ?a ?name ?value:(Option.map snd choice) D.Form.int32 in
  let h : (string * int32) option handle Eliom_client_value.t =
    [%client
      new labelled_opt_handle ~%has_feedback
        ~%(input_elem : [`Input] elt)
        ~%(form_input_elem : [`Input] elt option)
        ~%(choices_elem : [`Span] elt)
        ~%choice ~%emit ~%complete (string_of_option (Int32.to_string % snd))] in
  (main_elem, h)

let labelled_int64 : (int64, 'attrib, 'elt) labelled_req =
  fun ?has_feedback ~complete ?name ?emit ?a choice ->
  let input_elem, form_input_elem, choices_elem, main_elem =
    make_labelled ?a ?name ~value:(snd choice) D.Form.int64 in
  let h : (string * int64) handle Eliom_client_value.t =
    [%client
      new labelled_req_handle ~%has_feedback
        ~%(input_elem : [`Input] elt)
        ~%(form_input_elem : [`Input] elt option)
        ~%(choices_elem : [`Span] elt)
        ~%choice ~%emit ~%complete (Int64.to_string % snd)] in
  (main_elem, h)

let labelled_int64_option : (int64, 'attrib, 'elt) labelled_opt =
  fun ?has_feedback ~complete ?name ?emit ?a choice ->
  let input_elem, form_input_elem, choices_elem, main_elem =
    make_labelled ?a ?name ?value:(Option.map snd choice) D.Form.int64 in
  let h : (string * int64) option handle Eliom_client_value.t =
    [%client
      new labelled_opt_handle ~%has_feedback
        ~%(input_elem : [`Input] elt)
        ~%(form_input_elem : [`Input] elt option)
        ~%(choices_elem : [`Span] elt)
        ~%choice ~%emit ~%complete (string_of_option (Int64.to_string % snd))] in
  (main_elem, h)
