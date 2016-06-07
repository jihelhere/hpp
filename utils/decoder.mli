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
open Conll
open Feature

module type Decoder =
sig
  module C :  ConllType
  module Feature : Feature with module C = C


  val decode : float array -> C.t array -> C.t array
  val forced_decode : float array -> C.t array -> C.t array
  (* val constrained_decode : float array -> C.t array -> C.t array *)
  val decode_corpus : filename:string -> feature_weights:float array -> corpus:C.corpus -> verbose:bool -> evaluation:bool -> unit

  val get_feature_differences :  (int, int) Hashtbl.t -> int -> C.t array -> C.t array -> unit

end
