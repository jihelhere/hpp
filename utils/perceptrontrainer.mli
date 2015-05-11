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
open Decoder
open Eval

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


module PerceptronTrainer (Co : ConllType) (E : Eval with module C = Co) (D: Decoder with module C = Co) : OnlineTrainer
module MiraTrainer (Co : ConllType) (E : Eval with module C = Co) (D: Decoder with module C = Co) : OnlineTrainer



module TrainSelecter (C : ConllType)(E : Eval with module C = C)(P : Decoder with module C = C) :
sig
  val create_known_table : unit -> (module OnlineTrainer) String.Table.t
end
