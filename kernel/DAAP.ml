
module S = ExtString.String

let rec replace_all s f r = match S.replace s f r with
	| false, n -> n
	| _, n -> replace_all n f r

(* daap format: 4-byte tag : 4-byte length : length bytes data *)

let parse_tag bits = bitmatch bits with {
		tag : 32 : string;
		length : 32;
		data : (Int32.to_int length) * 8 : bitstring;
		rest : -1 : bitstring
	} -> tag, data, rest

let rec parse_list bits acc =
	let (tag,data,rest) = parse_tag bits in
	if Bitstring.bitstring_length rest = 0 then
		(tag,data) :: acc
	else
		parse_list rest ((tag,data)::acc)

let parse_byte bits = bitmatch bits with {
		b : 8 : int
	} -> b

let parse_int bits = bitmatch bits with {
		num : 32
	} -> num

let parse_version bits = bitmatch bits with {
		a : 8; b : 8; c : 8; d : 8
	} -> d, c, b, a

let print_tag_data tag data = match tag with
	| "msdc" -> Printf.sprintf "%ld" (parse_int data)
	| "minm" -> replace_all (Bitstring.string_of_bitstring data) "\xE2\x80\x99" "'"
	| "mpro" | "apro" -> let a,b,c,d = parse_version data in
		Printf.sprintf "%d.%d.%d.%d" a b c d
	| _ -> "<unknown>"

let print_tag (tag,data) map = match List.mem_assoc tag map with
	| true ->
		Vt100.printf "  %s: %s\n"
			(List.assoc tag map)
			(print_tag_data tag data)
	| _ -> ()

let features = [
		"msix", "index"; "msex", "extensions"; "msup", "update";
		"msal", "auto-logout"; "mslr", "requires-logon";
		"msqy", "query"; "msrs", "resolve"; "msbr", "browse";
		"mspi", "persistent-ids"
	]

let rec supported_features acc = function
| [] -> acc
| (tag, data) :: xs ->
	if List.mem_assoc tag features && parse_byte data <> 0 then
		supported_features (List.assoc tag features::acc) xs
	else
		supported_features acc xs

let parse_server_info server_info =
	let (tag,data,rest) = parse_tag (Bitstring.bitstring_of_string server_info) in
	if String.compare tag "msrv" <> 0 then
		failwith "daap: expected server info response";
	if Bitstring.bitstring_length rest <> 0 then
		Vt100.printf "daap: warning, trailing data found\n";
	let params = List.rev (parse_list data []) in
	let features = supported_features [] params in
	Vt100.printf "Server Info:\n";
	List.iter begin fun tuple ->
		print_tag tuple ["mpro", "M-Version"; "apro", "A-Version"; "minm", "Library Name"; "msdc", "Database Count"]
	end params;
	Vt100.printf "  Supported Features:";
	List.iter (Vt100.printf " %s") features;
	Vt100.printf "\n";
