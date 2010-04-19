
(* very simple mvar *)

type 'a t = {
	mutable data : 'a option;
	mutex : Mutex.t;
	condvar : Condition.t;
}

let create () = {
		data = None;
		mutex = Mutex.create ();
		condvar = Condition.create ();
	}

let rec put item mvar =
	Mutex.lock mvar.mutex;
	match mvar.data with
		| None ->
			mvar.data <- Some item;
			Mutex.unlock mvar.mutex;
			Condition.signal mvar.condvar;
		| Some _ ->
			Condition.wait mvar.condvar mvar.mutex;
			Mutex.unlock mvar.mutex;
			put item mvar

let rec get mvar =
	Mutex.lock mvar.mutex;
	match mvar.data with
		| None ->
			Condition.wait mvar.condvar mvar.mutex;
			Mutex.unlock mvar.mutex;
			get mvar
		| Some item ->
			mvar.data <- None;
			Mutex.unlock mvar.mutex;
			Condition.signal mvar.condvar;
			item

(* try yield instead of blocking first *)

let put item mvar =
	let rec loop n =
		if n = 0 then ()
		else if mvar.data <> None then (Thread.yield(); loop (n-1))
		else ()
	in loop 100;
	put item mvar

let get mvar =
	let rec loop n =
		if n = 0 then ()
		else if mvar.data = None then (Thread.yield(); loop (n-1))
		else ()
	in loop 100;
	get mvar
