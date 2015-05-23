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


let delta = function
  | true ->  1
  | false -> 0


module Eval_Tag (C : ConllType) = struct
  module C = C
  type t =
      {
        mutable total_pos   : int;
        mutable correct_pos : int;
        mutable exact        : int;
        mutable instances    : int;
      }
  let empty = {total_pos = 0;
              correct_pos = 0;
              exact = 0;
              instances = 0}

  let reset t =
    t.total_pos <- 0;
    t.correct_pos <- 0;
    t.exact  <- 0;
    t.instances <- 0

  (* return UAS and exact match*)
  let to_score t =
    ((Float.of_int t.correct_pos) /. (Float.of_int t.total_pos),
     (Float.of_int t.exact)        /. (Float.of_int t.instances))


  let update t ~ref_sentence ~hyp_sentence =
    t.total_pos <- t.total_pos + (Array.length ref_sentence);
    let perfect = ref true in
    for i = 0 to (Array.length ref_sentence) - 1 do
      if(C.same_prediction ref_sentence.(i) hyp_sentence.(i))
      then t.correct_pos <- t.correct_pos + 1
      else perfect := false
    done;
    t.instances <- t.instances + 1;
    t.exact     <- t.exact + (delta !perfect)

  let to_string t =
    let (acc,exact) = to_score t in
    Printf.sprintf "%d sentences Acc:%2.2f EX:%2.2f\r%!"
                   t.instances (100.0 *. acc) (100.0 *. exact)

end
