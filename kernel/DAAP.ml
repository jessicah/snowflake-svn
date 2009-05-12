
module S = ExtString.String

let rec replace_all s f r = match S.replace s f r with
	| false, n -> n
	| _, n -> replace_all n f r

(* daap format: 4-byte tag : 4-byte length : length bytes data *)

type kind = Byte | Short | Int | Long | String | Date | Version | List

(* lookup table of types *)
let types = [
	"mstt", Int;
	"miid", Int;
	"minm", String;
	"mikd", Byte;
	"mper", Long;
	"mcon", List;
	"mcti", Int;
	"mpco", Int;
	"msts", String;
	"mimc", Int;
	"mrco", Int;
	"mtco", Int;
	"mlcl", List;
	"mlit", List;
	"mbcl", List;
	"mdcl", List;
	"msrv", List;
	(*msaud*)
	"mslr", Byte;
	"mpro", Version;
	"apro", Version;
	"msal", Byte;
	"msup", Byte;
	"mspi", Byte;
	"msex", Byte;
	"msbr", Byte;
	"msqy", Byte;
	"msix", Byte;
	"msrs", Byte;
	"mstm", Int;
	"msdc", Int;
	"mccr", List;
	"mcnm", Int;
	"mcna", String;
	"mcty", Short;
	"mlog", List;
	"mlid", Int;
	"mupd", List;
	"musr", Int;
	"muty", Byte;
	"mudl", List;
	"avdb", List;
	"abro", List;
	"abal", List;
	"abar", List;
	"abcp", List;
	"abgn", List;
	"adbs", List;
	"asal", String;
	"asar", String;
	"asbt", Short;
	"asbr", Short;
	"ascm", String;
	"asco", Byte;
	"asda", Date;
	"asdm", Date;
	"asdc", Short;
	"asdn", Short;
	"asdb", Byte;
	"aseq", String;
	"asfm", String;
	"asgn", String;
	"asdt", String;
	"asrv", Byte;
	"assr", Int;
	"assz", Int;
	"asst", Int;
	"assp", Int;
	"astm", Int;
	"astc", Short;
	"astn", Short;
	"asur", Byte;
	"asyr", Short;
	"asdk", Byte;
	"asul", String;
	"aply", List;
	"apbl", Byte;
	"apso", List;
	"prsv", List;
	"arif", List;
	"aeNV", Int;
	"aeSP", Byte;
]

type result = B of int | Sh of int | I of int32 | L of int64 | S of string | D of Bitstring.t | V of int * int | Ls of (string * result) list | U of Bitstring.t

let parse bits = bitmatch bits with {
		tag : 32 : string;
		length : 32;
		data : (Int32.to_int length) * 8 : bitstring;
		rest : -1 : bitstring
	} -> tag, data, rest

let rec parse_kind tag data =
	if List.mem_assoc tag types then
		match List.assoc tag types with
		| Byte -> parse_byte tag data
		| Short -> parse_short tag data
		| Int -> parse_int tag data
		| Long -> parse_long tag data
		| String -> parse_string tag data
		| List -> parse_list tag data
		| Date -> parse_date tag data
		| Version -> parse_version tag data
	else
		tag, U data
and parse_byte tag bits = bitmatch bits with {
		b : 8 : int
	} -> tag, B b
and parse_short tag bits = bitmatch bits with {
		sh : 16 : int
	} -> tag, Sh sh
and parse_int tag bits = bitmatch bits with {
		i : 32
	} -> tag, I i
and parse_long tag bits = bitmatch bits with {
		l : 64
	} -> tag, L l
and parse_string tag bits =
	tag, S (Bitstring.string_of_bitstring bits)
and parse_list tag bits =
	tag, Ls (parse_list_acc bits [])
and parse_list_acc bits acc =
	if Bitstring.bitstring_length bits = 0 then acc
	else begin
		let tag, data, rest = parse bits in
		parse_list_acc rest ((parse_kind tag data) :: acc)
	end
and parse_date tag bits =
	tag, D bits
and parse_version tag bits = bitmatch bits with {
		minor : 16 : int; major : 16 : int
	} -> tag, V (major, minor)

let rec kind_to_string = function
	| B i | Sh i -> Printf.sprintf "%d" i
	| I i -> Printf.sprintf "%ld" i
	| L i -> Printf.sprintf "%Ld" i
	| S s -> replace_all s "\xE2\x80\x99" "'"
	| D b -> Bitstring.string_of_bitstring b
	| V (maj,min) -> Printf.sprintf "%d.%d" maj min
	| Ls l ->
		let ll = List.map kind_to_string (List.map snd l) in
		Printf.sprintf "[%s]" (String.concat ", " ll)
	| U b -> Bitstring.string_of_bitstring b

let rec print (tag,kind) fields =
	if List.mem_assoc tag fields then begin
		Vt100.printf "  %s: %s\n" (List.assoc tag fields) (kind_to_string kind)
	end else begin match kind with
	| Ls l -> List.iter (fun x -> print x fields) l
	| _ -> ()
	end

let rec find_list t = function
	| [] -> raise Not_found
	| (tag,kind) :: xs when tag = t -> kind
	| (_,Ls l) :: xs -> begin
			try find_list t l
			with Not_found -> find_list t xs
		end
	| _ :: xs -> find_list t xs
	| _ -> raise Not_found
let find (tag,kind) t =
	if String.compare tag t = 0 then kind
	else begin match kind with
	| Ls l -> find_list t l
	| _ -> raise Not_found
	end

(*let features = [
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
		supported_features acc xs*)

let parse_server_info server_info =
	let tag, data, rest = parse (Bitstring.bitstring_of_string server_info) in
	if String.compare tag "msrv" <> 0 then
		failwith "daap: expected server info response";
	if Bitstring.bitstring_length rest <> 0 then
		Vt100.printf "daap: warning, trailing data found\n";
	let daap = parse_kind tag data in
	Vt100.printf "Server Info:\n";
	print daap [
		"mpro", "M-Version";
		"apro", "A-Version";
		"minm", "Library Name";
		"msdc", "Database Count";
	]

let parse_login login =
	let tag, data, rest = parse (Bitstring.bitstring_of_string login) in
	if String.compare tag "mlog" <> 0 then
		failwith "daap: expected login response";
	let daap = parse_kind tag data in
	Vt100.printf "Login Response:\n";
	print daap [
		"mstt", "Status";
		"mlid", "Session ID";
	];
	let I id = find daap "mlid" in
	id

let parse_update update =
	let tag, data, rest = parse (Bitstring.bitstring_of_string update) in
	if String.compare tag "mupd" <> 0 then
		failwith "daap: expected update response";
	let daap = parse_kind tag data in
	let I id = find daap "musr" in
	id

let output_databases database_list =
	let tag, data, rest = parse (Bitstring.bitstring_of_string database_list) in
	if String.compare tag "avdb" <> 0 then
		failwith "daap: expected database list";
	let daap = parse_kind tag data in
	let Ls records = find daap "mlcl" in
	Vt100.printf "Databases:\n";
	List.iter begin fun daap ->
		print daap [
			"miid", "ID";
			"minm", "Name";
			"mimc", "Songs";
			"mctc", "Playlists";
		]
	end records
