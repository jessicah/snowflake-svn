
open ExtString

(* a very naive HTTP client :P *)
let request path headers ip port =
	let ip = NetworkStack.Helpers.ip_addr ip in
	let writer, queue = TCP.connect ip port in
	let headers = String.concat "\r\n" headers in
	if String.length headers > 0 then
		Printf.kprintf writer "GET %s HTTP/1.0\r\n%s\r\n\r\n" path headers
	else
		Printf.kprintf writer "GET %s HTTP/1.0\r\n\r\n" path;
	while Queue.is_empty queue do
		Thread.yield ();
	done;
	let headers = Queue.take queue in
	(* check first line is HTTP/1.x 200 OK *)
	let ic = IO.input_string headers in
	let first_line = IO.read_line ic in
	if not (String.starts_with first_line "HTTP/1.0 200 OK") && not (String.starts_with first_line "HTTP/1.1 200 OK") then
		Printf.kprintf failwith "http: %s\n" first_line;
	while Queue.is_empty queue do
		Thread.yield ();
	done;
	(* return the response :P *)
	Queue.take queue
	(* since it's HTTP/1.0, the connection should close for us... *)
