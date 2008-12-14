
(* device manager *)

open PCI

val scan_pci_bus : unit -> unit

val add_driver : string -> (device -> unit) -> int -> int -> unit
