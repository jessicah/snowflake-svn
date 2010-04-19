
external unsafe_lock : Mutex.t -> unit = "caml_mutex_unsafe_lock"
external unsafe_unlock : Mutex.t -> unit = "caml_mutex_unsafe_unlock"

let create irq cb =
	let m = Mutex.create () in
	Mutex.lock m; (* lock it so handler is blocked *)
	let _ = Thread.create (fun () ->
		while true do
			(* acquire locked mutex *)
			unsafe_lock m;
			cb ();
			(* signal interrupt controller that we're done *)
			Asm.out8 0x20 0x20;
			if irq > 7 then
				Asm.out8 0xA0 0x20;
		done
	) () (Printf.sprintf "irq %d" irq) in
	let u () = unsafe_unlock m in
	ignore (Sys.signal irq (Sys.Signal_handle (fun _ -> u ())))

open Gc
let print_stat () =
	let st = stat () in
  Debug.printf "minor_words: %Ld\n" (Int64.of_float st.minor_words);
  Debug.printf "promoted_words: %Ld\n" (Int64.of_float st.promoted_words);
  Debug.printf "major_words: %Ld\n" (Int64.of_float st.major_words);
  Debug.printf "minor_collections: %d\n" st.minor_collections;
  Debug.printf "major_collections: %d\n" st.major_collections;
  Debug.printf "heap_words: %d\n" st.heap_words;
  Debug.printf "heap_chunks: %d\n" st.heap_chunks;
  Debug.printf "top_heap_words: %d\n" st.top_heap_words;
  Debug.printf "live_words: %d\n" st.live_words;
  Debug.printf "live_blocks: %d\n" st.live_blocks;
  Debug.printf "free_words: %d\n" st.free_words;
  Debug.printf "free_blocks: %d\n" st.free_blocks;
  Debug.printf "largest_free: %d\n" st.largest_free;
  Debug.printf "fragments: %d\n" st.fragments;
  Debug.printf "compactions: %d\n" st.compactions
  
let create_i irq cb =
	let m = Mutex.create () in
	Mutex.lock m; (* lock it so handler is blocked *)
	let _ = Thread.create (fun () ->
		while true do
			Printf.kprintf Debug.log "irq %02d: waiting" irq;
			(* acquire locked mutex *)
			unsafe_lock m;
			Printf.kprintf Debug.log "irq %02d: running" irq;
			cb ();
			Printf.kprintf Debug.log "irq %02d: finished" irq;
			Asm.out8 0x20 0x20;
			if irq > 7 then
				Asm.out8 0xA0 0x20;
			print_stat ();
		done
	) () (Printf.sprintf "irq %02d" irq) in
	let u () =
		Printf.kprintf Debug.log "irq %02d: signalling" irq;
		unsafe_unlock m
	in
	ignore (Sys.signal irq (Sys.Signal_handle (fun _ -> u ())))

