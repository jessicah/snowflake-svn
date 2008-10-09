
module type Ordering =
	sig
		type t
		type index
		type key
		val to_index : t -> index list
		val from_index : index list -> t
	end

module Make = functor(O : Ordering) ->
	struct
		type elt = O.t
		type index = O.index
		type key = O.key
		type t = {
			mutable children : (index, t) Hashtbl.t;
			mutable key : key option
		}

		let
		rec empty () = { children = Hashtbl.create 3; key = None }
		and insert e = insert_direct (O.to_index e)
		and delete e = delete_direct (O.to_index e)
		and restrict e = restrict_direct (O.to_index e)
		and find_empty trie = match trie.key with
			  Some k -> k
			| None -> raise Not_found
		and find e = find_direct (O.to_index e)

		(* Find the trie at idx, and then read its empty string's key. *)
		and find_direct idx trie = find_empty (restrict_direct idx trie)

		and insert_direct idx k = replace_direct idx (Some k)
		(* This doesn't delete unneeded nodes, but never mind. *)
		and delete_direct idx = replace_direct idx None

		and replace_direct idx k trie =
			let traverse t i = try
					Hashtbl.find t.children i
				with Not_found ->
					let t' = empty () in
					Hashtbl.add t.children i t';
					t'
			in let node = List.fold_left traverse trie idx in
			node.key <- k

		and restrict_direct idx trie =
			let traverse t i = Hashtbl.find t.children i in
			List.fold_left traverse trie idx

		and iter f trie =
			let recurse a b = iter f b in
			Hashtbl.iter recurse trie.children;
			match trie.key with
				  Some k -> f trie
				| None -> ()

		and iter_string f trie = iter_string_from [] f trie
		and iter_string_from rev_prefix f trie =
			let recurse a b = iter_string_from (a::rev_prefix) f b in
			Hashtbl.iter recurse trie.children;
			match trie.key with
				  Some k -> f (O.from_index (List.rev rev_prefix))
				| None -> ()
		and fold f trie =
			let rec fold list trie =
				Hashtbl.fold (fun index t accu -> let index = list @ [index] in (f (O.from_index index)) :: (fold index t) @ accu) trie.children []
			in fold [] trie
	end
