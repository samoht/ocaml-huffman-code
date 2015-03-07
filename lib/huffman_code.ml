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

type 'a tree =
  | Node of 'a tree * 'a tree
  | Leaf of 'a
  | Empty

let rec pp_tree pp_a fmt t =
  let pp = pp_tree pp_a in
  match t with
  | Node (x, y) -> Format.fprintf fmt "Node (@[<hov 2>@,%a,@ %a@])" pp x pp y
  | Leaf x      -> Format.fprintf fmt "Leaf (@[<hov 2>%a@])" pp_a x
  | Empty       -> Format.fprintf fmt "Empty"

let rec compare_tree cmp x y =
  let compare = compare_tree cmp in
  match x, y with
  | Empty      , Empty       -> 0
  | Leaf x     , Leaf y      -> cmp x y
  | Node (x, y), Node (u, w) ->
    begin match compare x u with
      | 0 -> compare y w
      | i -> i
    end
  | Empty , _      -> -1
  | _     , Empty -> 1
  | Leaf _, _      -> -1
  | _     , Leaf _ -> 1

let empty = Empty

let node ?(canonical=true) x y = match x, y with
  | Node _, Leaf _
  | Empty , Leaf _ when canonical -> Node (y, x)
  | _  -> Node (x, y)

let leaf x = Leaf x

type code = bool list
let code_of_bool_list x = x
let bool_list_of_code x = x
let code_length = List.length
let compare_code = Pervasives.compare

let pp_code fmt code =
  Format.fprintf fmt "[@[<hov 2>@;";
  List.iter (fun b -> Format.fprintf fmt "%d@ " @@ if b then 1 else 0) code;
  Format.fprintf fmt "@]]"

let err fmt =
  let b = Buffer.create 20 in                         (* for thread safety. *)
  let ppf = Format.formatter_of_buffer b in
  let k ppf = Format.pp_print_flush ppf (); failwith (Buffer.contents b) in
  Format.kfprintf k ppf fmt

let err_leaf = err "%a: cannot insert nodes in value position" pp_code
let err_node = err "%a: cannot insert values in intermediate nodes" pp_code

let insert t code x =
  let rec aux t c = match t, c with
    | Empty     , []   -> Leaf x
    | Empty     , false::c -> Node (aux Empty c, Empty)
    | Empty     , true ::c -> Node (Empty, aux Empty c)
    | Leaf _    , _        -> err_leaf code
    | Node _    , []       -> err_node code
    | Node (l,r), false::c -> Node (aux l c, r)
    | Node (l,r), true ::c -> Node (l, aux r c)
  in
  aux t code

let decode t code =
  let rec aux = function
    | []     , Leaf x      -> Some x
    | false::c, Node (l,_) -> aux (c, l)
    | true ::c, Node (_,r) -> aux (c, r)
    | _ -> None
  in
  aux (code,t )

let tree_of_codes xl =
  try
    let t = List.fold_left (fun t (code, x) -> insert t code x) Empty xl in
    Some t
  with Failure _ ->
    None

let codes_of_tree t =
  let rec aux acc = function
    | []               -> List.rev acc
    | (_, Empty)  :: t -> aux acc t
    | (c, Leaf x) :: t -> aux ((c, x) :: acc) t
    | (c, Node (l, r)) :: t ->
      let l = false :: c, l in
      let r = true :: c, r in
      aux acc (l :: r :: t)
  in
  let codes = aux [] [[], t] in
  List.map (fun (c, x) -> List.rev c, x) codes

let insert_w (w, _ as e) wl =
  let rec aux acc wl = match wl with
    | [] -> List.rev_append acc [e]
    | (x, _ as i) :: t ->
      if w <= x then
        List.rev_append acc (e :: wl)
      else
        aux (i :: acc) t
  in
  aux [] wl

let compare_w (w1, _) (w2, _) = Pervasives.compare w1 w2

let tree_of_distrib ?(canonical=true) xs =
  let rec aux = function
    | []     -> failwith "Empty"
    | [_, t] -> t
    | (w1,x1)::(w2,x2)::t -> aux (insert_w (w1 +. w2, node ~canonical x1 x2) t)
  in
  let xs = List.rev (List.sort compare_w xs) in
  let xs = List.map (fun (w, x) -> w, leaf x) xs in
  aux xs

let encode xs =
  let distrib = Hashtbl.create (List.length xs) in
  let get x =
    try Hashtbl.find distrib x
    with Not_found -> 0.
  in
  let incr x = Hashtbl.replace distrib x (get x +. 1.) in
  List.iter incr xs;
  let d = Hashtbl.fold (fun x d acc -> (d, x) :: acc) distrib [] in
  tree_of_distrib d

let rec zero = function
  | 0 -> []
  | n -> false :: zero (n-1)

let code_of_be_int ~len n =
  let rec aux len = function
    | 0 -> zero len
    | 1 -> true :: zero (len - 1)
    | n ->
      if n mod 2 = 0 then false :: aux (len - 1) (n / 2)
      else true :: aux (len - 1) ((n-1) / 2)
  in
  List.rev (aux len n)

let be_int_of_code c =
  let rec aux acc n = function
    | []         -> acc
    | false :: t -> aux acc (n*2) t
    | true  :: t -> aux (n + acc) (n*2) t
  in
  aux 0 1 (List.rev c)

let codes_of_lengths ls =
  let len = List.length ls in

  (* 1) Count the number of codes for each code length. Let
     bl_count[N] be the number of codes of length N, N >= 1. *)
  let bl_count = Array.make len 0 in
  List.iter (fun (n, _) -> bl_count.(n) <- bl_count.(n) + 1) ls;

  (* 2) Find the numerical value of the smallest code for each code
     length *)
  let code = ref 0 in
  let max_bits = List.fold_left (fun acc (x, _) -> max acc x) 0 ls in
  let next_code = Array.make (max_bits + 1) 0 in
  for bits = 1 to max_bits do
    code := (!code + bl_count.(bits-1)) lsl 1;
    next_code.(bits) <- !code
  done;

  (* 3) Assign numerical values to all codes, using consecutive values
     for all codes of the same length with the base values determined
     at step 2. *)
  let length = Array.of_list ls in
  let length = Array.map fst length in
  let code = Array.make len 0 in
  for n = 0 to len - 1 do
    let len = length.(n) in
    if len <> 0 then (
      code.(n) <- next_code.(len);
      next_code.(len) <- next_code.(len) + 1
    );
  done;
  List.mapi (fun i (len, e) -> code_of_be_int ~len code.(i), e) ls

let tree_of_code_lengths ls =
  tree_of_codes (codes_of_lengths ls)
