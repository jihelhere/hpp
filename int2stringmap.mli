(*
 * Copyright (C) 2014  Joseph Le Roux
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Core.Std
open Sexp

module Int2StringMap :
  sig
    type t with sexp
    val empty : unit -> t
    val str2int : t -> string -> int
    val str2int_safe : t -> string -> int
    val int2str : t -> int -> string

    val get_size : t -> int

    val load_from_sexp : t -> Sexp.t -> unit
  end