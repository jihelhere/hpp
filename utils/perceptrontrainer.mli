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
open Conll
open Feature

module type Decoder =
sig
 module C :  ConllType
  module Feature : Feature with module C = C

  val decode : float array -> C.t array -> C.t array
  val  compute_score_difference : C.t -> C.t -> float
end



module type OnlineTrainer =
sig
    val train :
      train_filename:string ->
      dev_filename:string  option ->
      test_filename:string option ->
      max_iter:int ->
      feature_threshold:int ->
      verbose:bool ->
      float array
    val name : string
  end


module PerceptronTrainer (Co : ConllType) (D: Decoder with module C = Co) : OnlineTrainer
module MiraTrainer (Co : ConllType) (D: Decoder with module C = Co) : OnlineTrainer



(* module TrainSelecter (C : ConllType) (P : Decoder with module C = C) : *)
(* sig *)
(*   val create_known_table : unit -> (module OnlineTrainer) String.Table.t *)
(* end *)
