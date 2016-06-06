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

open Core.Std

open Int2stringmap
open Rule


type t

type lexical_index = ((int*float*float*Rule.t) list) Array.t

type index = {lhs:int; score:float; logscore: float; rule:Rule.t}
type unary_index =  (index list) Array.t


module type Rule_trie =
sig
  type info = int * float * float * Rule.t [@@deriving sexp, compare]
  type t3   = info list [@@deriving sexp, compare]
  type t2   = (int * t3) list [@@deriving sexp, compare]
  type t    = (int * t2) list [@@deriving sexp, compare]
end

module Rule_trie  : Rule_trie

val nts : t -> int
val lexical_index : t -> lexical_index
val unary_frompos : t -> unary_index
val unary_index : t -> unary_index
val binary_index : t -> Rule_trie.t

val priors : t -> (int,float) Hashtbl.t
val word_map : t -> Int2StringMap.t
val nt_map : t -> Int2StringMap.t


val from_model_file : bool -> string -> t
