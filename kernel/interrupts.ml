
external unsafe_lock : Mutex.t -> unit = "caml_mutex_unsafe_lock"
external unsafe_unlock : Mutex.t -> unit = "caml_mutex_unsafe_unlock"

let create irq cb =
	let m = Mutex.create () in
	Mutex.lock m; (* lock it so handler is blocked *)
	let t = Thread.create (fun () ->
		while true do
			(* acquire locked mutex *)
			unsafe_lock m;
			cb ();
		done
	) () in
	let u () = unsafe_unlock m in
	ignore (Sys.signal irq (Sys.Signal_handle (fun _ -> u ())))
