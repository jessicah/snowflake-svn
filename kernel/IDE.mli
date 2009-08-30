
val init : unit -> unit

type t

val read_disk : t -> int -> int -> string

type controller = Primary | Secondary
type device = Master | Slave

val disk_to_id : controller -> device -> t

(* raises Not_found *)
val get : controller -> device -> t