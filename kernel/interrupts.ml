
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
			(* signal interrupt controller that we're done *)
			Asm.out8 0x20 0x20;
			if irq > 7 then
				Asm.out8 0xA0 0x20;
		done
	) () in
	let u () = unsafe_unlock m in
	ignore (Sys.signal irq (Sys.Signal_handle (fun _ -> u ())))
