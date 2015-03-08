(* Copyright (C) 2014--2015  Petter Urkedal <paurkedal@gmail.com>
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

open Panograph_dltree
open Unprime_option

let (=?) n_opt n' =
  match n_opt with
  | None -> false
  | Some n -> n == n'

let (=??) n_opt n'_opt =
  match n_opt, n'_opt with
  | None, None -> true
  | Some n, Some n' -> n == n'
  | _ -> false

let test_add_first x u =
  let c = Dltree.add_first x u in
  assert (Dltree.get c = x);
  assert (not (Dltree.is_root c));
  assert (Dltree.is_leaf c);
  assert (Dltree.prev c = None);
  assert (Dltree.first u =? c);
  assert (Dltree.up c =? u);
  assert (Dltree.first u =? c);
  assert (Dltree.left_compare u c < 0);
  assert (Dltree.left_compare c u > 0);
  c

let test_add_last x u =
  let c = Dltree.add_last x u in
  assert (Dltree.get c = x);
  assert (not (Dltree.is_root c));
  assert (Dltree.is_leaf c);
  assert (Dltree.next c = None);
  assert (Dltree.last u =? c);
  assert (Dltree.up c =? u);
  assert (Dltree.last u =? c);
  assert (Dltree.left_compare u c < 0);
  assert (Dltree.left_compare c u > 0);
  c

let test_add_before x n =
  let c = Dltree.add_before x n in
  assert (Dltree.get c = x);
  assert (not (Dltree.is_root c));
  assert (Dltree.is_leaf c);
  assert (not (Dltree.is_only c));
  assert (Dltree.prev n =? c);
  assert (Dltree.next c =? n);
  assert (Dltree.up c =?? Dltree.up n);
  assert (Dltree.left_compare c n < 0);
  assert (Dltree.left_compare n c > 0);
  c

let test_add_after x p =
  let c = Dltree.add_after x p in
  assert (Dltree.get c = x);
  assert (not (Dltree.is_root c));
  assert (Dltree.is_leaf c);
  assert (not (Dltree.is_only c));
  assert (Dltree.next p =? c);
  assert (Dltree.prev c =? p);
  assert (Dltree.up c =?? Dltree.up p);
  assert (Dltree.left_compare c p > 0);
  assert (Dltree.left_compare p c < 0);
  c

let test_delete c =
  let u_opt = Dltree.up c in
  let n_opt = Dltree.next c in
  let p_opt = Dltree.prev c in
  Dltree.delete_subtree c;
  match u_opt, p_opt, n_opt with
  | None, _, _ -> assert false
  | Some u, None, None -> assert (Dltree.first u = None)
  | Some u, None, Some n -> assert (Dltree.first u =? n)
  | Some u, Some p, None -> assert (Dltree.last u =? p)
  | Some u, Some p, Some n -> ()

let rec nth i c = if i = 0 then c else nth (i - 1) (Option.get (Dltree.next c))

let check_onelevel p xs =
  let rec check_from next c_opt = function
    | [] -> assert (c_opt = None)
    | x :: xs ->
      begin match c_opt with
      | None -> assert false
      | Some c -> assert (Dltree.get c = x); check_from next (next c) xs
      end in
  check_from Dltree.next (Dltree.first p) xs;
  check_from Dltree.prev (Dltree.last p) (List.rev xs)

let test_sub p =
  let n20 = test_add_first 20 p in
  assert (Dltree.is_only n20);
  assert (not (Dltree.is_leaf p));
  let n30 = test_add_last 30 p in
  let n10 = test_add_first 10 p in
  assert (Dltree.first p =? n10);
  assert (Dltree.last p =? n30);
  let n21 = test_add_after 21 n20 in
  let n19 = test_add_before 19 n20 in
  ignore n19;
  let n09 = test_add_before 9 n10 in
  let n31 = test_add_after 31 n30 in
  assert (Dltree.first p =? n09);
  assert (Dltree.last p =? n31);
  check_onelevel p [9; 10; 19; 20; 21; 30; 31];
  test_delete n19;
  check_onelevel p [9; 10; 20; 21; 30; 31];
  test_delete n21;
  test_delete n09;
  test_delete n31;
  check_onelevel p [10; 20; 30];
  ()

let rec pick_random c =
  if Dltree.is_leaf c then c else
  if Random.int 2 = 0 then c else
  let n = Dltree.fold (fun _ -> succ) c 0 in
  pick_random (nth (Random.int n) (Option.get (Dltree.first c)))

let add_random a =
  let c = pick_random a in
  match Random.int (if Dltree.is_root c then 2 else 4) with
  | 0 -> test_add_first 0 c
  | 1 -> test_add_last 0 c
  | 2 -> test_add_before 0 c
  | 3 -> test_add_after 0 c
  | _ -> assert false

let remove_random a =
  let c = pick_random a in
  if not (Dltree.is_root c) then Dltree.delete_subtree c

let mutate c =
  if Random.int 8 = 0
  then remove_random c
  else ignore (add_random c : int Dltree.t)

let rec enumerate i c =
  Dltree.set !i c; incr i;
  Dltree.iter (enumerate i) c

let test_random () =
  for i = 0 to 99 do
    let c = Dltree.make 0 in
    for j = 0 to 99 do
      mutate c
    done;
    enumerate (ref 0) c;
    for j = 0 to 99 do
      let a = pick_random c in
      let b = pick_random c in
      assert (Dltree.left_compare a b = compare (Dltree.get a) (Dltree.get b))
    done
  done

let test_exists () =
  let root = Dltree.make 0 in
  let a = test_add_first 1 root in
  let aa = test_add_first 2 a in
  assert (Dltree.exists ((==) a) root);
  assert (not (Dltree.exists ((==) aa) root));
  assert (not (Dltree.exists ((!=) a) root));
  assert (Dltree.exists ~depth:2 ((==) aa) root);
  assert (not (Dltree.exists ~depth:2 ((==) a) root));
  assert (not (Dltree.exists ~depth:2 ((!=) aa) root))

let run () =
  let root = Dltree.make 0 in
  assert (Dltree.is_root root);
  assert (Dltree.is_leaf root);
  let n99 = test_add_first 99 root in
  assert (Dltree.is_root root);
  assert (not (Dltree.is_leaf root));
  test_delete n99;
  assert (Dltree.is_root root);
  assert (Dltree.is_leaf root);
  assert (Dltree.first root = None);
  assert (Dltree.last root = None);

  test_exists ();
  test_sub root;
  test_sub (Option.get (Dltree.first root));
  test_random ()
