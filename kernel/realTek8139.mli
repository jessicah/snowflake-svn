
(* RTL8139 Driver *)

val create : PCI.device -> (unit -> string) * (string -> unit) * int list
