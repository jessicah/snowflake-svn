
module Testing = struct

	let tar_file = lazy begin
			let data = Multiboot.open_module () in
			let str = String.create (Bigarray.Array1.dim data) in
			for i = 0 to String.length str - 1 do
				str.[i] <- data.{i}
			done;
			TarFile.open_tar_file str (* make this work with IO and avoid alloc'ing tar file again *)
		end

	let objs = lazy begin
			Array.map begin fun filename ->
				filename, ElfParsing.parse filename (Bitstring.bitstring_of_string
					(TarFile.read_file (Lazy.force tar_file) filename))
			end LinkerTest.input_files
		end
end

(*
		According to what I read by Ian Lance Taylor
		(http://www.airs.com/blog/archives/39), there are four steps
		that need to be done:
		
		1.	Read the input object files. Determine the length and type
			of the contents. Read the symbols.
		2.	Build a symbol table containing all the symbols, linking
			undefined symbols to their definitions.
		3.	Decide where all the contents should go in the output
			executable file, which means deciding where they should go
			in memory when the program runs.
		4.	Read the contents data and the relocations. Apply the
			relocations to the contents. Write the result to the
			output file.
	*)
	
(*

Section is struct {
	string name;
	address vaddr;
	address size;
	uint align;
	
	blob data
}

Symbol is struct {
	string name
	address value
	address size
	Section sect
	
	bool is_defined
	Symbol linked_symbol # if this symbol is undefined, this is the defined symbol it's linked to
}

# sections in the final output file
sections = new Hashtable of Sections;

foreach(section_in_input_files) {
	# Where sect is the current section
	
	# minimum section alignment
	if sections[sect.name].align < sect.align
		sections[sect.name].align = sect.align
	
	# Update the input section's address to the offset within the final object file
	# sect.base must be aligned to the section's minimum alignment (or better)
	# I've got no idea how non-zero address work out, so i'm ignoring them here
	section.vaddr = align(sections[section.name].size, sect.align)
	
	# Update the size of the output section
	sections[section.name].size = section.vaddr + section.size
}

# do "linking undefined symbols to their definitions." here

# Fix the address of every symbol
foreach(defined_symbol_in_input_files) {
	# sym is the symbol
	# sect is the symbol's section in the object file (same as sect above)
	
	sym.value += sect.vaddr + section.real_base_link_address
}

# do step 3 here, pretty much a case of just sorting sections by vaddr and ensuring no overlaps

# step 4!

foreach(section in sections) {
	section.blob = allocate(section.size)
	
	# Input sections that are the same as, or will be merged in to this section
	foreach(input_sections) {
		# input_section.vaddr is actually the offset within the final output section
		copy(input_section.data to (section.data + offset input_section.vaddr))
	}
}

# do relocations here

*)

(* Types and globals *)

type section = {
		section_name : string; (* not sure if this is strictly needed here *)
		mutable section_vaddr : int;
		mutable section_size  : int;
		mutable section_align : int;
		mutable section_data  : Bitstring.t;
	}

type symbol = {
		symbol_name  : string;
		symbol_value : int;
		symbol_size  : int;
		symbol_section : section;
		mutable symbol_is_defined : bool;
		mutable symbol_linked_to  : symbol option; (* when undefined, this is the symbol it maps to *)
	}

let sections = Hashtbl.create 7 (* key: string (name), value: section *)

(* Step 0: Load the list of files from disk, and pass to step 1 :P *)

let rec step0 () =
	(* this would actually take a list of files or something; we'd also have a VFS *)
	Vt100.printf "Loading object and archive files...\n";
	step1 (Lazy.force Testing.objs)

(* Step 1: Read the input object files. Determine the length and type
			of the contents. Read the symbols. *)
and step1 arr = (* arr : (string * ElfTypes.t) array *)
	Vt100.printf "Don't have any code yet; stopping...\n";
	()
