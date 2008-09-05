(*
 * Std - Additional functions
 * Copyright (C) 2003 Nicolas Cannasse
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** Additional functions. *)

val string_of_char : char -> string
(** creates a string from a char. *)

external identity : 'a -> 'a = "%identity"
(** the identity function. *)

val unique : unit -> int
(** returns an unique identifier every time it is called. *)

val dump : 'a -> string
(** reprensent a runtime value as a string. *)

val finally : (unit -> unit) -> ('a -> 'b) -> 'a -> 'b 
(** finally [fend f x] calls [f x] and then [fend()] even if [f x] raised
	an exception. *)
