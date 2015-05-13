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
open Conlltag
open Featuretag
open Templatetag

(* open Featuretag *)



module Sequence_Decoder =
struct

  module C = Conll_Tag
  module T = Template_Tag
  module Feature = Feature_Tag

  let decode params sent =
    let nb_labels = Conll_Tag.get_number_pos () in
    let n = Array.length sent in

    (*build uni/bi scores*)
    let hyp_scores = Array.create ~len:(n*nb_labels*nb_labels) 0.0 in
    for i = 0 to (n-1) do
      let index = i * nb_labels * nb_labels in
      for pos = 0 to (nb_labels - 1) do
        let uni_score = Feature.get_uni_score params sent i pos in
        for ppos = 0 to (nb_labels - 1) do
          let bi_score =
            if (i > 0) || (ppos = C.prediction C.start)
            then Feature.get_bi_score params sent i ppos pos
            else 0.0 in
          Array.unsafe_set hyp_scores (index + ppos * nb_labels + pos) (uni_score +. bi_score)
        done
      done
    done;

  (*best path scores*)
    let scores = Array.create ~len:(n*nb_labels) 0.0 in
  (* backpointers*)
    let bp = Array.create ~len:(n*nb_labels) (-1) in

    (* fill score chart (partial paths score) + backpointers *)
    for i = 0 to (n-1) do
      for pos = 0 to nb_labels - 1 do
        let bst_score = ref Float.neg_infinity in
        let bst_ppos = ref (-1) in
        for ppos = 0 to nb_labels - 1 do
          let cur_score = Array.unsafe_get hyp_scores (i * nb_labels * nb_labels + ppos * nb_labels + pos) +.
            if i > 0 then Array.unsafe_get scores ( (i-1)*nb_labels + ppos)
            else 0.0 in
          if cur_score > !bst_score
          then
            (
              bst_score := cur_score;
              bst_ppos  := ppos
            )
        done;
        (
          Array.unsafe_set scores (i*nb_labels + pos) !bst_score;
          Array.unsafe_set bp (i*nb_labels + pos) !bst_ppos
        )
      done
    done;


    (* compute path scores (add the missing POS -> STOP edge) *)
    let last = n-1 in
    let stop_pos = C.prediction C.stop in
    let bst_score = ref Float.neg_infinity in
    let bst_ppos = ref (-1) in
    for last_pos = 0 to nb_labels - 1 do
      let cur_score = if last > 0 then Array.unsafe_get scores (last*nb_labels + last_pos) else 0.0 in
      let cur_score = cur_score +. Feature.get_bi_score params sent n last_pos stop_pos in

      if cur_score > !bst_score
      then
        (
          bst_score := cur_score;
          bst_ppos  := last_pos
        )
    done;

    (* set last pos from previous iteration *)
    let output = Array.copy sent in
    Array.replace output last ~f:(fun t -> C.set_prediction t !bst_ppos);


    (* go backward in bp chain *)
    let rec aux i =
      if i < 0 then ()
      else
        (Array.replace output i ~f:(fun t ->
          let next_pred = C.prediction (Array.unsafe_get output (i+1)) in
          C.set_prediction t (Array.unsafe_get bp ((i+1)*nb_labels + next_pred))
         );
         aux (i-1))

    in
    aux (last-1);

    (* for i = 0 to n-1 do *)

    (*   printf "%d %d\n%!" (C.prediction (Array.unsafe_get sent i)) (C.prediction (Array.unsafe_get output i)) *)
    (* done; *)
    (* printf "\n%!"; *)

    output


  let decode_corpus ~filename ~feature_weights ~corpus ~verbose =
    let decode_func = decode  feature_weights in
    let oc = Out_channel.create filename in
    let t1 = Time.now () in
    let () =
      List.iter (Conll_Tag.corpus_to_list corpus)
        ~f:(fun s ->
          let ref_a = Conll_Tag.prepare_sentence_for_decoder s in
          let hyp_a = decode_func ref_a in
          Array.iter hyp_a ~f:(fun tok -> Printf.fprintf oc "%s\n" (Conll_Tag.to_string tok));
          fprintf oc "\n"
        ) in
    let t2 = Time.now () in
    (if verbose
     then
       printf "Decoding time: %s\n" (Core.Span.to_string (Time.diff t2 t1)));
    Out_channel.close oc


end
