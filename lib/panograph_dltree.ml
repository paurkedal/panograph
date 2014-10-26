(* Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
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

module Dltree = struct

  type 'a t = {
    mutable up : 'a t;
    mutable down : 'a t;
    mutable prev : 'a t;
    mutable next : 'a t;
    mutable value : 'a;
  }

  let is_root c = c.up == c
  let is_head c = c.up == c || c.up.down == c
  let is_leaf c = c.down == c

  let make x =
    let rec r = {up = r; down = r; prev = r; next = r; value = x} in
    r

  let get c = c.value
  let set x c = c.value <- x
  let up c = if c.up == c then None else Some c.up
  let first c = if c.down == c then None else Some c.down
  let last c = if c.down == c then None else Some c.down.prev
  let next c = let n = c.next in if is_head n then None else Some n
  let prev c = if is_head c then None else Some c.prev

  let add_last x u =
    if u.down == u then begin
      let rec c = {up = u; down = c; prev = c; next = c; value = x} in
      u.down <- c;
      c
    end else begin
      let n = u.down in
      let rec c = {up = u; down = c; prev = n.prev; next = n; value = x} in
      n.prev.next <- c;
      n.prev <- c;
      c
    end

  let add_first x u =
    let c = add_last x u in
    u.down <- c;
    c

  let add_after x p =
    let u = p.up in
    if u == p then invalid_arg "Dltree.add_after: Root node.";
    let n = p.next in
    let rec c = {up = u; down = c; prev = p; next = n; value = x} in
    p.next <- c;
    n.prev <- c;
    c

  let add_before x n =
    let u = n.up in
    if u == n then invalid_arg "Dltree.add_before: Root node.";
    let p = n.prev in
    let rec c = {up = u; down = c; prev = p; next = n; value = x} in
    p.next <- c;
    n.prev <- c;
    if u.down == n then u.down <- c;
    c

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
