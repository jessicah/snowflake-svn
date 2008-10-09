
(* An implementation of the trie data structure.

A trie is a tree which normally represents strings which
might have common prefixes. Given three strings abc, abd
and ac, the trie might look like:

    |
    a
  -----
  |   |
  b   c
 ---
 | |
 c d

with one node for each prefix of the string (one for the
empty string, one for a, one for ab etc). Tries are quite
space-efficient if there's a lot of duplication in the
prefixes. You can efficiently produce a trie containing all
the endings of some prefix (for example, from a you can produce
a tree of bc, bd and c). You can also find out if some word is
in the trie, and add words later on.

The tarfs driver uses tries to store the list of files.
Apparently they're useful for routing, too. *)

module type Ordering =
	sig
		(* The type of strings. *)
		type t

		(* The type of element indices. *)
		type index

		(* A key to be associated with each string. *)
		type key

		(* A function to produce lists of indexes for the trie. *)
		val to_index : t -> index list
		val from_index : index list -> t
	end

module Make : functor (O : Ordering) ->
	sig
		(* The type of strings. *)
		type elt

		(* The type of keys associated with the strings. *)
		type key

		(* The type of tries. *)
		type t

		(* The type of indexes. *)
		type index

		(* Produce an empty trie. *)
		val empty : unit -> t

		(* Insert a word into a trie. *)
		val insert : elt -> key -> t -> unit

		(* Insert a word into a trie, given as an index list. *)
		val insert_direct : index list -> key -> t -> unit

		(* Remove a word from a trie. *)
		val delete : elt -> t -> unit

		(* Remove a word from a trie, given as an index list. *)
		val delete_direct : index list -> t -> unit

		(* Return the trie containing all words beginning with
		   prefix, with the prefix removed. *)
		val restrict : elt -> t -> t

		(* The same as above, with an index list. *)
		val restrict_direct : index list -> t -> t

		(* Return the key associated with the empty string, or
		   raise Not_found. This is mainly useful with restrict -
		   in fact find is implemented using this. *)
		val find_empty : t -> key

		(* Return the key associated with some string, or
		   raise Not_found. *)
		val find : elt -> t -> key

		(* The same as above, with an index list. *)
		val find_direct : index list -> t -> key

		(* Perform a function on every string in the trie.
		   Call find_empty on the given string to get the key. *)
		val iter : (t -> unit) -> t -> unit
		val iter_string : (elt -> unit) -> t -> unit
		val fold : (elt -> 'a) -> t -> 'a list
	end with type elt = O.t with type key = O.key with type index = O.index
