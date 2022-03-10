(* Copyright (C) 2014--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

open Unprime

let ord_step = 256

module Dltree = struct

  type 'a t = {
    mutable up: 'a t;
    mutable down: 'a t;
    mutable prev: 'a t;
    mutable next: 'a t;
    mutable value: 'a;
    level: int;
    mutable ord: int;
  }

  let is_root c = c.up == c
  let is_head c = c.up == c || c.up.down == c
  let is_leaf c = c.down == c
  let is_first c = c.up.down == c
  let is_last c = is_first c.next
  let is_only c = c.next == c

  let make x =
    let rec r = {up = r; down = r; prev = r; next = r; value = x;
                 level = 0; ord = 0} in
    r

  let get c = c.value
  let set x c = c.value <- x
  let up c = if c.up == c then None else Some c.up
  let first c = if c.down == c then None else Some c.down
  let last c = if c.down == c then None else Some c.down.prev
  let next c = let n = c.next in if is_head n then None else Some n
  let prev c = if is_head c then None else Some c.prev

  let level c = c.level

  let rec left_compare cA cB =
    if cA.level < cB.level then
      let o = left_compare cA cB.up in
      if o = 0 then -1 else o else
    if cA.level > cB.level then
      let o = left_compare cA.up cB in
      if o = 0 then 1 else o else
    if cA.level > 0 then
      let o = left_compare cA.up cB.up in
      if o = 0 then compare cA.ord cB.ord else o else
    compare cA.ord cB.ord

  let rec first_leaf c = if is_leaf c then c else first_leaf c.down
  let rec last_leaf c = if is_leaf c then c else last_leaf c.down.prev

  let rec fold ?(depth = 1) f u =
    if depth = 0 then f u else
    let rec loop = function
     | None -> ident
     | Some c -> fun acc -> loop (next c) (fold ~depth:(depth - 1) f c acc)
    in
    loop (first u)

  let rec iter ?(depth = 1) f u =
    if depth = 0 then f u else
    let rec loop = function
     | None -> ()
     | Some c -> iter ~depth:(depth - 1) f c; loop (next c)
    in
    loop (first u)

  let iteri ?depth f u =
    ignore (fold ?depth (fun c i -> f i c; i + 1) u 0 : int)

  let rec iterp' ~depth ~path f u =
    if depth = 0 then f (List.rev path) u else
    let rec loop i = function
     | None -> ()
     | Some c ->
        iterp' ~depth:(depth - 1) ~path:(i :: path) f c;
        loop (i + 1) (next c)
    in
    loop 0 (first u)
  let iterp ~depth f u = iterp' ~depth ~path:[] f u

  let rec exists ?(depth = 1) f u =
    if depth = 0 then f u else
    let rec loop = function
     | None -> false
     | Some c -> exists ~depth:(depth - 1) f c || loop (next c)
    in
    loop (first u)

  let rec for_all ?(depth = 1) f u =
    if depth = 0 then f u else
    let rec loop = function
     | None -> true
     | Some c -> for_all ~depth:(depth - 1) f c && loop (next c)
    in
    loop (first u)

  let fold_ancestors f c =
    let rec loop = function
     | None -> ident
     | Some c -> fun acc -> loop (up c) (f c acc)
    in
    loop (up c)

  let iter_ancestors f c =
    let rec loop = function
     | None -> ()
     | Some c -> f c; loop (up c)
    in
    loop (up c)

  (* TODO: Also check ord against min_int and max_int. *)

  let add_last value u =
    if u.down == u then begin
      let rec c = {up = u; down = c; prev = c; next = c; value;
                   level = u.level + 1; ord = 0} in
      u.down <- c;
      c
    end else begin
      let n = u.down in
      let p = n.prev in
      if n.prev.ord + ord_step < n.prev.ord then
        failwith "Panograph_dltree.add_last: overflow.";
      let rec c = {up = u; down = c; prev = p; next = n; value;
                   level = n.level; ord = n.prev.ord + ord_step} in
      n.prev.next <- c;
      n.prev <- c;
      c
    end

  let add_first x u =
    let c = add_last x u in
    if c.next.ord - ord_step > c.next.ord then
        failwith "Panograph_dltree.add_first: overflow.";
    u.down <- c;
    c.ord <- c.next.ord - ord_step;
    c

  let add_between value p n =
    let u = p.up in
    if p.ord + 1 = n.ord then begin
      let h = u.down in
      let rec reorder ord c =
        c.ord <- ord;
        if c.next != h then reorder (ord + ord_step) c.next
      in
      reorder 0 h
    end;
    let ord = (p.ord + n.ord) asr 1 in
    assert (p.ord < ord && ord < n.ord);
    let rec c = {up = u; down = c; prev = p; next = n; value;
                 level = p.level; ord} in
    p.next <- c;
    n.prev <- c;
    c

  let add_after value p =
    let u = p.up in
    if u == p then invalid_arg "Dltree.add_after: Root node.";
    if is_last p then add_last value u else add_between value p p.next

  let add_before value n =
    let u = n.up in
    if u == n then invalid_arg "Dltree.add_before: Root node.";
    if is_first n then add_first value u else add_between value n.prev n

  let delete_subtree c =
    let u = c.up in
    if u == c then
      invalid_arg "Dltree.delete: Root node."
    else if c.next == c then
      u.down <- u
    else begin
      if u.down == c then u.down <- c.next;
      c.next.prev <- c.prev;
      c.prev.next <- c.next
    end

end
