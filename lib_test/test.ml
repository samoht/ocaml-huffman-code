(*
 * Copyright (c) 2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Printf

let pp_str fmt =
  let b = Buffer.create 20 in                         (* for thread safety. *)
  let ppf = Format.formatter_of_buffer b in
  let k ppf = Format.pp_print_flush ppf (); Buffer.contents b in
  Format.kfprintf k ppf fmt

let rec cmp_list fn l1 l2 = match l1, l2 with
  | [], [] -> true
  | [], _ | _ , [] -> false
  | h1::t1, h2::t2 -> fn h1 h1 && cmp_list fn t1 t2

let printer_list fn l = String.concat ", " (List.map fn l)

let fail = OUnit.assert_string

let check_tree msg t1 t2 =
  let cmp x y = Huffman_code.compare_tree Pervasives.compare x y = 0 in
  let printer x = pp_str "%a" (Huffman_code.pp_tree Format.pp_print_int) x in
  OUnit.assert_equal ~cmp ~printer ~msg t1 t2

let check_code msg c1 c2 =
  let cmp x y = Huffman_code.compare_code x y = 0 in
  let printer x = pp_str "%a" Huffman_code.pp_code x in
  OUnit.assert_equal ~cmp ~printer ~msg c1 c2

let int = Format.pp_print_int
let char = Format.pp_print_char

let check_codes pp_a msg c1s c2s =
  let sort = List.sort (fun (_, x) (_, y) -> compare x y) in
  let cmp (x, y) (u, v) = Huffman_code.compare_code x u = 0 && y = v in
  let cmp x y = cmp_list cmp (sort x) (sort y) in
  let printer (x, y) = pp_str "(%a %a)" Huffman_code.pp_code x pp_a y in
  let printer = printer_list printer in
  OUnit.assert_equal ~cmp ~printer ~msg c1s c2s

let c x = Huffman_code.code_of_bool_list (List.map (fun x -> x <> 0) x)

let c1 = [
  c [0;0]  , 'A';
  c [1]    , 'B';
  c [0;1;1], 'C';
  c [0;1;0], 'D';
]

let c2 =  [
  c [1;0]  , 'A';
  c [0]    , 'B';
  c [1;1;0], 'C';
  c [1;1;1], 'D';
]

let get fn x = match fn x with
  | None   -> failwith "exn"
  | Some x -> x

(* distrib from the original paper *)
let d = [
  0.20, 1 , [1;0];
  0.18, 2 , [0;0;0];
  0.10, 3 , [0;1;1];
  0.10, 4 , [1;1;0];
  0.10, 5 , [1;1;1];
  0.06, 6 , [0;1;0;1];
  0.06, 7 , [0;0;1;0;0];
  0.04, 8 , [0;0;1;0;1];
  0.04, 9 , [0;1;0;0;0];
  0.04, 10, [0;1;0;0;1];
  0.04, 11, [0;0;1;1;0];
  0.03, 12, [0;0;1;1;1;0];
  0.01, 13, [0;0;1;1;1;1];
]

let mk i n =
  let rec aux acc = function
  | 0 -> acc
  | n -> aux (i :: acc) (n - 1)
  in
  aux [] n

let i =
  List.fold_left (fun acc (d, n, _) ->
      mk n (int_of_float (d *. 100.)) @ acc
    ) [] d

let tree =
  let check_codes msg c () =
    let t  = get Huffman_code.tree_of_codes c in
    let c' = Huffman_code.codes_of_tree t in
    check_codes char msg c c'
  in [
    "c1", `Quick, check_codes "c1" c1;
    "c2", `Quick, check_codes "c2" c2;
  ]

let cot = Huffman_code.codes_of_tree

let distrib =
  let p = List.map (fun (d, x, _) -> d, x) d in
  let c = List.map (fun (_, x, y) -> c y, x) d in
  let c' = cot @@ Huffman_code.tree_of_distrib p in
  [ "tree", `Quick, fun () -> check_codes int "distrib" c c' ]

let create =
  let c = List.map (fun (_, x, y) -> c y, x) d in
  let c' = cot @@ Huffman_code.encode i in
  [ "create", `Quick, fun () -> check_codes int "create" c c' ]

let lenghts =
  let c = List.map (fun (_, x, y) -> c y, x) d in
  let l = List.map (fun (c, x) -> Huffman_code.code_length c, x) c in
  let c' = cot @@ get Huffman_code.tree_of_code_lengths l in
  [ "lengths", `Quick, fun () -> check_codes int "lengths" c c' ]

let () =
  Alcotest.run "huffman-tree" [
    "tree"   , tree;
    "distrib", distrib;
    "create" , create;
    "lengths", lenghts;
  ]
