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

(** Implementation of Huffman codes. *)

(** {1 Huffman trees} *)

type 'a tree
(** The type for Huffman trees. *)

val compare_tree: ('a -> 'a -> int) -> 'a tree -> 'a tree -> int
(** Compare two trees. *)

val pp_tree: (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a tree -> unit
(** Pretty-printing. *)

val empty: 'a tree
(** The empty tree. *)

val leaf: 'a -> 'a tree
(** [leaf a] is the leaf containing [a]. *)

val node: ?canonical:bool -> 'a tree -> 'a tree -> 'a tree
(** Build a tree node. If [canonical] is set, the parameter might be
    switched to ensure that the right is never a leaf if the left
    child is a node or empty. *)

(** {1 Huffman codes} *)

type code
(** The type for Huffman codes. *)

val pp_code: Format.formatter -> code -> unit
(** Pretty-print a code. *)

val code_of_be_int: len:int -> int -> code
(** [code_of_int ~len i] uses [i]'s big-endian binary representation
    as code to build a code of size [len]. *)

val be_int_of_code: code -> int
(** [int_of_code c] interprets [c] as a big-endian binary integer. *)

val code_of_bool_list: bool list -> code
(** Convert a list of bools to a code. *)

val bool_list_of_code: code -> bool list
(** Convert a code to a list of bools. *)

val code_length: code -> int
(** [length c] is [c]'s length. *)

val compare_code: code -> code -> int
(** Compare two codes. *)

(** {1 Conversions} *)

val tree_of_codes: (code * 'a) list -> 'a tree option
(** [of_codes c] is the Huffman tree for the codes [c]. Return [None]
    is the codes are not consistent. *)

val tree_of_code_lengths: (int * 'a)  list -> 'a tree option
(** [of_code_length ls] is the canonical Huffman tree having codes of
    lenghts [ls]. Return [None] if no valid Huffman tree exists for
    the given code lengths. *)

val codes_of_tree: 'a tree -> (code * 'a) list
(** [to_codes t] is the list of codes encoded in the Huffman tree
    [t]. *)

val tree_of_distrib: ?canonical:bool -> (float * 'a) list -> 'a tree
(** [of_distrib w] is the Huffman tree built using the weigh
    distribution [w]. If [canonical] is set, returns a canonical form
    with:

    {ul
    {- No node has a node as left child and a leaf as right child. }
    {- If a node has two leaves as child, the left one comes first in [w].}
*)

(** {1 Coding and decoding} *)

val encode: 'a list -> 'a tree
(** [encode l] is the Huffman tree for the input [l]. It first build
    the distribution of [l] and then call {!of_distrib}. *)

val decode: 'a tree -> code -> 'a option
(** Find an element in the tree. *)
