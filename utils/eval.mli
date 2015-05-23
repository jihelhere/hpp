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

module type Eval =
sig

  module C : ConllType
  type t
  val empty : t

  val reset : t -> unit

  (* return Score (or 1 - Loss) and exact match*)
  val to_score : t -> (float * float)

  val update : t -> ref_sentence:C.t array -> hyp_sentence:C.t array -> unit

  val to_string : t -> string

end
