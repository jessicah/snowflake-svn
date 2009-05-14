
(* AudioMixer *)

(* Provides an interface to a sound device *)

type bit_rate
	= Bits8
	| Bits16

type sample_rate
	= KHertz11
	| KHertz22
	| KHertz44
	| KHertz48

type format = int * int * int

module Wave : sig
	type t
	
	val read : BlockIO.input -> t
end

(*val resample : Wave.t -> int -> (unit -> int)*)

type output = {
	format: format;
	output: BlockIO.input -> unit;
}

(* Pretty much allows for only one audio device atm *)

val play : Wave.t -> unit
val play_raw : BlockIO.input -> unit
val stop : unit -> unit

val register_device : output -> unit
