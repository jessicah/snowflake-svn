
open ExtString

let rec skip_headers input = match IO.read_line input with
	| "" -> ()
	| _ -> skip_headers input

(* a very naive HTTP client :P *)
let request path headers ip port =
	let ip = NetworkStack.Helpers.ip_addr ip in
	let writer, input = TCP.open_channel ip port in
	let headers = String.concat "\r\n" headers in
	if String.length headers > 0 then
		Printf.kprintf writer "GET %s HTTP/1.0\r\n%s\r\n\r\n" path headers
	else
		Printf.kprintf writer "GET %s HTTP/1.0\r\n\r\n" path;
	let status = IO.read_line input in
	if not (String.starts_with status "HTTP/1.0 200 OK") && not (String.starts_with status "HTTP/1.1 200 OK") then
		Printf.kprintf failwith "http: %s\n" status;
	skip_headers input;
	(* return the response :P *)
	IO.read_all input
	(* since it's HTTP/1.0, the connection should close for us... *)
