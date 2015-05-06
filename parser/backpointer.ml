(*
 * Copyright (C) 2015  Joseph Le Roux
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

open Rule

module type BackPointer =
sig
  type t

  val unused  : t
  val lexical : Rule.t -> int -> t
  val unary   : Rule.t -> t
  val binary  : Rule.t -> int -> int -> t
  val  is_unused : t-> bool

  val get_adress_list : t -> int -> Rule.t * (int list)
end

module CKYBackPointer : BackPointer =
struct
  type t =
    | X
    | L of Rule.t * int
    | B of Rule.t * int * int
    | U of Rule.t

  let unused = X
  let lexical r tid = L (r,tid)
  let unary  r = U r
  let binary rule lcell_idx rcell_idx = B (rule, lcell_idx, rcell_idx)


  let get_adress_list t entry_id =
    match t with
    | X -> failwith "unused backpointer"
    | L (r,tid) -> r,[tid]
    | U r -> r,[entry_id]
    | B (r,lcell,rcell) -> r,[lcell;rcell]


  let is_unused t =
    match t with
    | X -> true
    |_ -> false
end
