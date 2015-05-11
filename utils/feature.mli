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

open Conll
open Core.Std


module type Feature =
sig

  module C : ConllType

  type t

  val of_int : int -> t
  val to_int : t -> int

  val is_valid : int -> t -> bool

  val get_all_features :  (int,int) Hashtbl.t -> int ->
                         (int option -> int option) -> C.t array -> int -> unit


  val compute_score_difference : float array -> C.t array -> C.t array -> int -> C.t -> C.t -> float

  val prune_features : int -> int

  val template_feature_map_to_sexp : unit -> Sexp.t
  val save_template_feature_map_to_file : string -> unit

  val load_template_map_from_sexp : Sexp.t -> unit

  val collect_features_on_corpus : only_gold:bool -> C.sentence list -> unit
end
