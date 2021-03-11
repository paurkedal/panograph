(* Copyright (C) 2015--2021  Petter A. Urkedal <paurkedal@gmail.com>
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
  open Panograph_i18n
  open Panograph_prereq
  open Panograph_types
  open Unprime
  open Unprime_option
]

[%%server
  type 'a handle
]

[%%shared
  type ('a, +'opt) opt =
    | Opt of string option * bool * 'a
    | Optgroup of string * bool * ('a, [`Opt]) opt list
    constraint 'opt = [< `Opt | `Optgroup]

  let opt ?(enabled = true) label value = Opt (Some label, enabled, value)
  let optv ?(enabled = true) value = Opt (None, enabled, value)
  let optgroup ?(enabled = true) label opts = Optgroup (label, enabled, opts)

  type common_input_attrib =
    [ Html_types.common | `Autofocus | `Disabled | `Size | `Required ]
]

[%%client
  open Js_of_ocaml
  open Pandom_interactive
  open Panograph_common

  let checked_utf8 s =
    (match check_utf8_exn s with
     | () -> s
     | exception (Failure msg) -> raise (Invalid_input msg))

  let render_text text =
    (try check_utf8_exn text with Failure msg -> raise (Invalid_input msg));
    String.split_on_char '\n' text
      |> List.map (fun line -> [F.txt line; F.br ()])
      |> List.flatten

  class type ['a] handle = object
    method show : unit
    method hide : unit
    method edit_on : ('a -> unit Panui_result.t Lwt.t) -> unit
    method edit_off : unit
    method get : 'a
    method set : 'a -> unit
  end

  class common_handle el = object (self)
    method show = Manip.Class.remove el "pan-hidden"
    method hide = Manip.Class.add el "pan-hidden"
  end

  type input_or_select_parent = [
    | `Abbr | `Article | `Aside | `Blockquote | `B | `Body
    | `Caption | `Cite | `Code
    | `Dd | `Del | `Details | `Dfn | `Div | `Dt | `Em
    | `Figcaption | `Figure | `Form
    | `H1 | `H2 | `H3 | `H4 | `H5 | `H6
    | `I | `Ins | `Li | `Main | `Mark | `Nav | `P | `Pre | `Q
    | `Samp | `Section | `Small | `Span | `Strong | `Sub | `Summary | `Sup
    | `Td | `Th | `Time | `U | `Var
  ]

  class ['a] input_handle
      ~(to_string : 'a -> string)
      ~(of_string : string -> 'a)
      ?(emit : ('a -> unit Panui_result.t Lwt.t) option)
      ?(error : (string option -> unit) option)
      ?(a : [< Html_types.input_attrib] attrib list = [])
      (init : 'a) (el : [< input_or_select_parent] elt) =
  object (self)
    inherit common_handle el

    val mutable value = init
    val mutable absorb = fun _ -> ()

    method edit_on f =
      let inp = D.input ~a:(F.a_input_type `Text :: a) () in
      absorb <- outfit_input ~to_string ~of_string ?error ~value inp f;
      Manip.replaceChildren el [inp]

    method edit_off =
      absorb <- (fun x -> Manip.replaceChildren el [D.txt (to_string x)]);
      absorb value

    method get = value

    method set x = value <- x; absorb x

    initializer
      match emit with None -> self#edit_off | Some f -> self#edit_on f
  end

  class ['a] textarea_handle
    ~(to_string : 'a -> string)
    ~(of_string : string -> 'a)
    ?(to_html : (('a -> [< Html_types.div_content] elt list) option))
    ?(emit : ('a -> unit Panui_result.t Lwt.t) option)
    ?(error : (string option -> unit) option)
    ?(a : [< Html_types.textarea_attrib] attrib list option)
    (init : 'a) (el : [< input_or_select_parent] elt) =
  object (self)
    inherit common_handle el

    val mutable value = init
    val mutable absorb = fun _ -> ()

    method edit_on f =
      let inp = D.textarea ?a (D.txt (to_string init)) in
      absorb <- outfit_textarea ~to_string ~of_string ?error ~value inp f;
      Manip.replaceChildren el [inp]

    method edit_off =
      absorb <-
        (match to_html with
         | Some to_html -> fun x ->
            Manip.replaceChildren el (to_html x)
         | None -> fun x ->
            Manip.replaceChildren el (render_text (to_string x)));
      absorb value

    method get = value

    method set x = value <- x; absorb x

    initializer
      match emit with None -> self#edit_off | Some f -> self#edit_on f
  end

  class checkbox_handle
      ?(emit : (bool -> unit Panui_result.t Lwt.t) option)
      ?(error : (string option -> unit) option)
      ?(a : [< Html_types.input_attrib] attrib list = [])
      (init : bool) (el : [< input_or_select_parent] elt) =
  object (self)
    inherit common_handle el

    val mutable value = init
    val mutable absorb = fun _ -> ()

    method edit_on f =
      let a = if init then (D.a_checked () :: a) else a in
      let inp = D.input ~a:(F.a_input_type `Checkbox :: a) () in
      absorb <- outfit_checkbox ?error ~value inp f;
      Manip.replaceChildren el [inp]

    method edit_off =
      (* TODO: May want to revise how to indicate checked values i read-only
       * mode, one option being to use a disabled checkbox. *)
      let to_string = function false -> "✗" | true -> "✓" in
      absorb <- (fun x -> Manip.replaceChildren el [D.txt (to_string x)]);
      absorb value

    method get = value

    method set x = value <- x; absorb x

    initializer
      match emit with None -> self#edit_off | Some f -> self#edit_on f
  end

  class ['a] select_handle
      ~(to_string : 'a -> string)
      ~(of_string : string -> 'a)
      ~(opts : ('a, 'opt) opt list)
      ?(emit : ('a -> unit Panui_result.t Lwt.t) option)
      ?(error : (string option -> unit) option)
      ?(a : [< Html_types.select_attrib] attrib list option)
      (init : 'a) (el : [< input_or_select_parent] elt) =
  object (self)
    inherit common_handle el

    val label_by_value : ('a, string) Hashtbl.t = Hashtbl.create 11

    val mutable value = init
    val mutable absorb = fun _ -> ()

    val mutable active_unknown_option = None
    val mutable active_select = None

    method private init_label_by_value =
      let mk_option label value =
        Hashtbl.add label_by_value value
          (match label with Some label -> label | None -> to_string value)
      in
      let mk0 = function
        | Opt (label, _, value) -> mk_option label value
        | Optgroup _ -> assert false
      in
      let mk1 = function
        | Opt (label, _, value) -> mk_option label value
        | Optgroup (_, _, opts) -> List.iter mk0 opts
      in
      List.iter mk1 opts

    method private enable_select =
      let make_options () =
        let mk_option label enabled value =
          let label = match label with Some s -> s | None -> to_string value in
          let a = [D.a_value (to_string value)] in
          let a = if enabled then a else D.a_disabled () :: a in
          D.option ~a (D.txt label)
        in
        let mk0 = function
          | Opt (label, enabled, value) -> mk_option label enabled value
          | Optgroup _ -> assert false
        in
        let mk1 = function
          | Opt (label, enabled, value) -> mk_option label enabled value
          | Optgroup (label, enabled, opts) ->
            let a = if enabled then [] else [D.a_disabled ()] in
            D.optgroup ~label ~a (List.map mk0 opts)
        in
        List.map mk1 opts
      in
      (match active_select with
       | Some select -> select
       | None ->
          let select = D.select ?a (make_options ()) in
          active_select <- Some select;
          select)

    method private disable_select =
      active_unknown_option <- None;
      active_select <- None

    method private enable_select_unknown x =
      (match active_select, active_unknown_option with
       | Some select, None ->
          let unknown_option =
            D.option ~a:[D.a_value "__pan_unknown__"; D.a_class ["pan-error"]]
              (D.txt ("invalid (" ^ to_string x ^ ")"))
          in
          active_unknown_option <- Some unknown_option;
          Manip.insertFirstChild select unknown_option;
          Pandom_style.set_error
            "The currently selected option is no longer available."
            (To_dom.of_select select);
          (To_dom.of_select select)##.value := Js.string "__pan_unknown__"
       | Some _, Some _ -> ()
       | None, _ -> assert false)

    method private disable_select_unknown =
      (match active_select, active_unknown_option with
       | Some select, Some unknown_option ->
          active_unknown_option <- None;
          Pandom_style.clear_error (To_dom.of_select select);
          Manip.removeChild select unknown_option
       | Some _, None -> ()
       | None, _ -> assert false)

    method edit_on f =
      let select = self#enable_select in
      let absorb' = outfit_select ~to_string ~of_string ?error select f in
      absorb <- begin fun x ->
        self#disable_select_unknown;
        if Hashtbl.mem label_by_value x then
          absorb' x
        else
          self#enable_select_unknown x
      end;
      Manip.replaceChildren el [select];
      absorb value

    method edit_off =
      self#disable_select;
      absorb <- begin fun x ->
        let content = match Hashtbl.find_opt label_by_value x with
         | Some label -> [D.txt label]
         | None ->
            [D.span ~a:[D.a_class ["pan-error"]]
              [D.txt ("invalid (" ^ to_string x ^ ")")]]
        in
        Manip.replaceChildren el content
      end;
      absorb value

    method get = value

    method set x = absorb x; value <- x

    initializer
      self#init_label_by_value;
      match emit with None -> self#edit_off | Some f -> self#edit_on f
  end

  let inject_input ~to_string ~of_string ?emit ?error ?a =
    let a = (a :> Html_types.input_attrib attrib list option) in
    new input_handle ~to_string ~of_string ?emit ?error ?a

  let inject_textarea = new textarea_handle

  let inject_checkbox ?emit ?error ?a init el =
    let a = (a :> Html_types.input_attrib attrib list option) in
    new checkbox_handle ?emit ?error ?a init el

  let inject_select = new select_handle

  let inject_input_or_select
        ~to_string ~of_string ?opts ?emit ?error ?a init el =
    (match opts with
     | None ->
        inject_input ~to_string ~of_string ?emit ?error ?a init el
     | Some opts ->
        new select_handle ~opts ~to_string ~of_string ?emit ?error ?a init el)

  let add_input_with_handle = inject_input_or_select
]

[%%shared
  type ('a, 'opt, 'attrib, 'inner_attrib, 'elt) t =
        ?to_string: ('a -> string) Eliom_client_value.t ->
        ?of_string: (string -> 'a) Eliom_client_value.t ->
        ?opts: ('a, 'opt) opt list ->
        ?emit: ('a -> unit Panui_result.t Lwt.t) Eliom_client_value.t ->
        ?error: (string option -> unit) Eliom_client_value.t ->
        ?a: 'attrib attrib list ->
        ?input_a: 'inner_attrib attrib list ->
        'a -> 'elt elt * 'a handle Eliom_client_value.t
      constraint 'attrib = [< Html_types.common > `Class]
      constraint 'inner_attrib = [< common_input_attrib]
      constraint 'opt = [< `Opt | `Optgroup]
      constraint 'elt = [> `Span]

  let bool_opts =
    [opt "true" true; opt "false" false]

  let bool_option_opts =
    [opt "" None; opt "true" (Some true); opt "false" (Some false)]

  let bool : (bool, 'opt, 'attrib, 'inner_attrib, 'elt) t =
    fun ?to_string ?of_string ?opts ?emit ?error
        ?(a = [D.a_class ["pan-scalar"; "bool"]]) ?input_a init ->
    let el = D.span ~a [D.txt (string_of_bool init)] in
    let h : bool handle Eliom_client_value.t =
      match to_string, of_string, opts with
      | None, None, None ->
        [%client
          inject_checkbox
            ?emit:~%emit ?error:~%error ?a:~%input_a
            ~%init ~%(el : [`Span] elt)]
      | _ ->
        let to_string = Option.get_or [%client string_of_bool] to_string in
        let of_string = Option.get_or [%client bool_of_string] of_string in
        let opts = Option.get_or bool_opts opts in
        [%client
          inject_select
            ~opts:~%(opts :> (bool, [`Opt | `Optgroup]) opt list)
            ~to_string:~%to_string ~of_string:~%of_string
            ?emit:~%emit ?error:~%error ?a:~%input_a
            ~%init ~%(el : [`Span] elt)]
    in
    (el, h)

  let string : (string, 'opt, 'attrib, 'inner_attrib, 'elt) t =
    fun ?(to_string = [%client ident])
        ?(of_string = [%client checked_utf8])
        ?opts ?emit ?error
        ?(a = [D.a_class ["pan-scalar"; "string"]]) ?input_a init ->
    let el = D.span ~a [D.txt init] in
    let h : string handle Eliom_client_value.t =
      [%client
        inject_input_or_select
          ?opts:~%(opts :> (string, [`Opt | `Optgroup]) opt list option)
          ~to_string:~%to_string ~of_string:~%of_string
          ?emit:~%emit ?error:~%error ?a:~%input_a
          ~%init ~%(el : [`Span] elt)]
    in
    (el, h)

  let int : (int, 'opt, 'attrib, 'inner_attrib, 'elt) t =
    fun ?(to_string = [%client string_of_int])
        ?(of_string = [%client int_of_string])
        ?opts ?emit ?error
        ?(a = [D.a_class ["pan-scalar"; "int"]]) ?input_a init ->
    let el = D.span ~a [D.txt (string_of_int init)] in
    let h : int handle Eliom_client_value.t =
      [%client
        inject_input_or_select
          ?opts:~%(opts :> (int, [`Opt | `Optgroup]) opt list option)
          ~to_string:~%to_string ~of_string:~%of_string
          ?emit:~%emit ?error:~%error ?a:~%input_a
          ~%init ~%(el : [`Span] elt)]
    in
    (el, h)

  let int32 : (int32, 'opt, 'attrib, 'inner_attrib, 'elt) t =
    fun ?(to_string = [%client Int32.to_string])
        ?(of_string = [%client Int32.of_string])
        ?opts ?emit ?error
        ?(a = [D.a_class ["pan-scalar"; "int32"]]) ?input_a init ->
    let el = D.span ~a [D.txt (Int32.to_string init)] in
    let h : int32 handle Eliom_client_value.t =
      [%client
        inject_input_or_select
          ?opts:~%(opts :> (_, [`Opt | `Optgroup]) opt list option)
          ~to_string:~%to_string ~of_string:~%of_string
          ?emit:~%emit ?error:~%error ?a:~%input_a
          ~%init ~%(el : [`Span] elt)]
    in
    (el, h)

  let int64 : (int64, 'opt, 'attrib, 'inner_attrib, 'elt) t =
    fun ?(to_string = [%client Int64.to_string])
        ?(of_string = [%client Int64.of_string])
        ?opts ?emit ?error
        ?(a = [D.a_class ["pan-scalar"; "int64"]]) ?input_a init ->
    let el = D.span ~a [D.txt (Int64.to_string init)] in
    let h : int64 handle Eliom_client_value.t =
      [%client
        inject_input_or_select
          ?opts:~%(opts :> (_, [`Opt | `Optgroup]) opt list option)
          ~to_string:~%to_string ~of_string:~%of_string
          ?emit:~%emit ?error:~%error ?a:~%input_a
          ~%init ~%(el : [`Span] elt)]
    in
    (el, h)

  let float : (float, 'opt, 'attrib, 'inner_attrib, 'elt) t =
    fun ?(to_string = [%client string_of_float])
        ?(of_string = [%client float_of_string])
        ?opts ?emit ?error
        ?(a = [D.a_class ["pan-scalar"; "float"]]) ?input_a init ->
    let el = D.span ~a [D.txt (string_of_float init)] in
    let h : float handle Eliom_client_value.t =
      [%client
        inject_input_or_select
          ?opts:~%(opts :> (_, [`Opt | `Optgroup]) opt list option)
          ~to_string:~%to_string ~of_string:~%of_string
          ?emit:~%emit ?error:~%error ?a:~%input_a
          ~%init ~%(el : [`Span] elt)] in
    (el, h)

  let bool_option : (bool option, 'opt, 'attrib, 'inner_attrib, 'elt) t =
    fun ?(to_string = [%client string_of_option string_of_bool])
        ?(of_string = [%client option_of_string bool_of_string])
        ?(opts = bool_option_opts) ?emit ?error
        ?(a = [D.a_class ["pan-scalar"; "bool"; "option"]]) ?input_a
        init ->
    let el = D.span ~a [D.txt (string_of_option string_of_bool init)] in
    let h : bool option handle Eliom_client_value.t =
      [%client
        inject_select
          ~opts:~%(opts :> (_, [`Opt | `Optgroup]) opt list)
          ~to_string:~%to_string ~of_string:~%of_string
          ?emit:~%emit ?error:~%error ?a:~%input_a
          ~%init ~%(el : [`Span] elt)]
    in
    (el, h)

  let string_option : (string option, 'opt, 'attrib, 'inner_attrib, 'elt) t =
    fun ?(to_string = [%client string_of_option ident])
        ?(of_string = [%client option_of_string checked_utf8])
        ?opts
        ?emit ?error
        ?(a = [D.a_class ["pan-scalar"; "string"; "option"]]) ?input_a
        init ->
    let el = D.span ~a [D.txt (string_of_option ident init)] in
    let h : string option handle Eliom_client_value.t =
      [%client
        inject_input_or_select
          ?opts:~%(opts :> (_, [`Opt | `Optgroup]) opt list option)
          ~to_string:~%to_string ~of_string:~%of_string
          ?emit:~%emit ?error:~%error ?a:~%input_a
          ~%init ~%(el : [`Span] elt)]
    in
    (el, h)

  let int_option : (int option, 'opt, 'attrib, 'inner_attrib, 'elt) t =
    fun ?(to_string = [%client string_of_option string_of_int])
        ?(of_string = [%client option_of_string int_of_string])
        ?opts
        ?emit ?error
        ?(a = [D.a_class ["pan-scalar"; "int"; "option"]]) ?input_a
        init ->
    let el = D.span ~a [D.txt (string_of_option string_of_int init)] in
    let h : int option handle Eliom_client_value.t =
      [%client
        inject_input_or_select
          ?opts:~%(opts :> (_, [`Opt | `Optgroup]) opt list option)
          ~to_string:~%to_string ~of_string:~%of_string
          ?emit:~%emit ?error:~%error ?a:~%input_a
          ~%init ~%(el : [`Span] elt)]
    in
    (el, h)

  let int32_option : (int32 option, 'opt, 'attrib, 'inner_attrib, 'elt) t =
    fun ?(to_string = [%client string_of_option Int32.to_string])
        ?(of_string = [%client option_of_string Int32.of_string])
        ?opts ?emit ?error
        ?(a = [D.a_class ["pan-scalar"; "int32"; "option"]]) ?input_a
        init ->
    let el = D.span ~a [D.txt (string_of_option Int32.to_string init)] in
    let h : int32 option handle Eliom_client_value.t =
      [%client
        inject_input_or_select
          ?opts:~%(opts :> (_, [`Opt | `Optgroup]) opt list option)
          ~to_string:~%to_string ~of_string:~%of_string
          ?emit:~%emit ?error:~%error ?a:~%input_a
          ~%init ~%(el : [`Span] elt) ]
    in
    (el, h)

  let int64_option : (int64 option, 'opt, 'attrib, 'inner_attrib, 'elt) t =
    fun ?(to_string = [%client string_of_option Int64.to_string])
        ?(of_string = [%client option_of_string Int64.of_string])
        ?opts ?emit ?error
        ?(a = [D.a_class ["pan-scalar"; "int64"; "option"]]) ?input_a
        init ->
    let el = D.span ~a [D.txt (string_of_option Int64.to_string init)] in
    let h : int64 option handle Eliom_client_value.t =
      [%client
        inject_input_or_select
          ?opts:~%(opts :> (_, [`Opt | `Optgroup]) opt list option)
          ~to_string:~%to_string ~of_string:~%of_string
          ?emit:~%emit ?error:~%error ?a:~%input_a
          ~%init ~%(el : [`Span] elt)]
    in
    (el, h)

  let float_option : (float option, 'opt, 'attrib, 'inner_attrib, 'elt) t =
    fun ?(to_string = [%client string_of_option string_of_float])
        ?(of_string = [%client option_of_string float_of_string])
        ?opts ?emit ?error
        ?(a = [D.a_class ["pan-scalar"; "float"; "option"]]) ?input_a
        init ->
    let el = D.span ~a [D.txt (string_of_option string_of_float init)] in
    let h : float option handle Eliom_client_value.t =
      [%client
        inject_input_or_select
          ?opts:~%(opts :> (_, [`Opt | `Optgroup]) opt list option)
          ~to_string:~%to_string ~of_string:~%of_string
          ?emit:~%emit ?error:~%error ?a:~%input_a
          ~%init ~%(el : [`Span] elt)]
    in
    (el, h)

  let string_textarea :
        ?to_string: (string -> string) Eliom_client_value.t ->
        ?of_string: (string -> string) Eliom_client_value.t ->
        ?to_html:
          (string ->
            [< Html_types.div_content] elt list Eliom_client_value.t) ->
        ?emit: ((string -> unit Panui_result.t Lwt.t) Eliom_client_value.t) ->
        ?error: ((string option -> unit) Eliom_client_value.t) ->
        ?a: [< Html_types.div_attrib > `Class] attrib list ->
        ?input_a: [< Html_types.textarea_attrib] attrib list ->
        string -> [> `Div] elt * string handle Eliom_client_value.t =
    fun ?(to_string = [%client ident])
        ?(of_string = [%client checked_utf8])
        ?to_html
        ?emit ?error ?(a = [D.a_class ["pan-scalar"]]) ?input_a init ->
    let el = D.div ~a [] in
    let h : 'a handle Eliom_client_value.t =
      [%client
        inject_textarea
          ~to_string:~%to_string ~of_string:~%of_string ?to_html:~%to_html
          ?emit:~%emit ?error:~%error ?a:~%input_a
          ~%init ~%(el : [`Div] elt)]
    in
    (el, h)

  let string_option_textarea :
        ?to_string: (string option -> string) Eliom_client_value.t ->
        ?of_string: (string -> string option) Eliom_client_value.t ->
        ?to_html:
          (string option ->
            [< Html_types.div_content] elt list Eliom_client_value.t) ->
        ?emit:
          ((string option -> unit Panui_result.t Lwt.t) Eliom_client_value.t) ->
        ?error: ((string option -> unit) Eliom_client_value.t) ->
        ?a: [< Html_types.div_attrib > `Class] attrib list ->
        ?input_a: [< Html_types.textarea_attrib] attrib list ->
        string option ->
        [> `Div] elt * string option handle Eliom_client_value.t =
    fun ?(to_string = [%client string_of_option ident])
        ?(of_string = [%client option_of_string checked_utf8])
        ?to_html
        ?emit ?error ?(a = [D.a_class ["pan-scalar"]]) ?input_a init ->
    let el = D.div ~a [] in
    let h : 'a handle Eliom_client_value.t =
      [%client
        inject_textarea
          ~to_string:~%to_string ~of_string:~%of_string ?to_html:~%to_html
          ?emit:~%emit ?error:~%error ?a:~%input_a
          ~%init ~%(el : [`Div] elt)]
    in
    (el, h)
]
