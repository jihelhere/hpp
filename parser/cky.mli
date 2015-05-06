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

open Ckygram
open Ptbtree
open Rule

module type Cell =
sig
  type entry
  type t

  val create_empty : int -> t
  val add_lexical:   t -> int -> float -> unit
  val add_unary  :   t -> int -> float -> unit
  val add_binary :   t -> int -> float -> unit

  val decode_lexical : t -> int -> Rule.t -> int -> float -> bool
  val decode_unary   : t -> Rule.t -> int -> float -> float -> unit
  val decode_binary  : t -> Rule.t -> int -> int -> int -> float -> float -> unit

  val length : t -> int
  val get : t -> int -> entry
  val is_empty_entry : entry -> bool
  val get_entry_inside_score_exn : entry -> float
  val get_entry_max_score_exn : entry -> float
  val get_entry_inside_score : entry -> float option
  val get_entry_max_score : entry -> float option

  val transfer_inside : t -> unit
  val transfer_max : t -> unit

  val iteri : t -> f:(int -> entry ->unit) -> unit
  val prune_priors : t -> (int,float) Hashtbl.t -> threshold:float -> bool
  val prune_group :  t -> (int,float) Hashtbl.t -> size:int -> bool

  val reset : t -> int -> unit

  val stat : int -> int -> t -> unit

  val extract_tree : Ckygram.t ->
    (string * int * (int * float * float * Rule.t) list) array ->
    t Array.t -> int -> Ptbtree.string_tree
end

module BaseCell : Cell


module type Parser =
sig
  val parse_file : Ckygram.t -> bool -> string -> unit
  val parse_wordlist : Ckygram.t -> bool -> string list -> unit
  val test_forest : Ckygram.t -> bool -> Ptbtree.string_tree -> int * int
end

module MakeCKY : functor (C : Cell) -> Parser
