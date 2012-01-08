
(* distcc client, protocol version 1 *)

open Printf

let airUnbuntu = NetworkProtocolStack.IPv4.Addr (192, 168, 1, 64)

let distcc_port = 3632

(*
 client tokens:
	DIST <version>
	ARGC <nargs>
	ARGV <len> <bytes>
	DOTI <len> <bytes>
 server tokens:
	DONE <version>
	STAT <status>
	SERR <len> <bytes>
	SOUT <len> <bytes>
	DOTO <len> <bytes>
*)

(* returns status * output; status = exitcode << 8 | termsignal *)
let submit cmd args content =
	(* reader is an IO input *)
	let socket, writer, reader = TCP.open_readbuffer airUnbuntu distcc_port in
	let write_packet token param = function
		| None -> kprintf writer "%s%08x" token param
		| Some body ->
			kprintf writer "%s%08x%s" token param body
	in
	(* submit the command *)
	write_packet "DIST" 1 None;
	write_packet "ARGC" (Array.length args + 1) None;
	write_packet "ARGV" (String.length cmd) (Some cmd);
	Array.iter (fun arg ->
		write_packet "ARGV" (String.length arg) (Some arg)
		) args;
	write_packet "DOTI" (String.length content) (Some content);
	(* now we have to get the response... *)
	let read_packet () =
		let token = String.make 4 '\000' in
		let param = String.make 8 '\000' in
		IO.really_input reader token 0 4;
		IO.really_input reader param 0 8;
		let param = Scanf.sscanf param "%08x" (fun x -> x) in
		match token, param with
		| "DONE", version when version <> 1 ->
			failwith (sprintf "protocol mismatch: 1 <> %d" version)
		| "SERR", length
		| "SOUT", length
		| "DOTO", length when length > 0 ->
			let buffer = String.make length '\000' in
			IO.really_input reader buffer 0 length;
			token, param, Some buffer
		| _ -> token, param, None
	in
	let header = read_packet () in
	let status = read_packet () in
	let stderr = read_packet () in
	let stdout = read_packet () in
	let output = read_packet () in
	begin match status with
	| _, status, _ ->
		printf "exit code: %d\n" (status asr 8)
	end;
	begin match stderr with
	| _, _, None -> printf "no error output\n"
	| _, _, Some msg -> printf "stderr:\n%s\n" msg
	end;
	begin match stdout with
	| _, _, None -> printf "no output message\n"
	| _, _, Some msg -> printf "stdout:\n%s\n" msg
	end;
	begin match output with
	| _, _, None -> printf "empty output\n"
	| _, len, Some _ -> printf "output size is %d bytes\n" len
	end;
	let (_,_,output) = output in
	let (_,status,_) = status in
	status, output

open Shell
open Arg

let test_submit () =
	printf "submitting job to distcc node...\n";
	let status, output = submit "gcc" [| "-c"; "-o"; "hello.o"; "hello.S" |]
	".global __entrypoint\n.extern thread_exit\n.section .text\n.set STACKSIZE, 0x40000\n__entrypoint:\n\tmov $(stack + STACKSIZE), %esp\n\n" in
	printf "all done!"

let () =
	add_command "distcc" test_submit []
