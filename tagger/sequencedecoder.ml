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

open Evaltag

(** Sequence Decoder :
   A Viterbi decoder for trigram labelers
*)


module Sequence_Decoder =
struct

  (** Access Conll-like data
  *)
  module C = Conll_Tag

  (** Access feature template extraction
  *)
  module T = Template_Tag


  (** Access feature hash
  *)
  module F = Feature_Tag
  module Feature = F


  (** Access evaluation metrics for tagging
  *)
  module E = Eval_Tag


  (** The general decoding function
      @param size_func a function returning an array of authorized tags for a token
      @param params the weight vector
      @param sent an array of conll tokens
  *)
  let decode_general size_func params sent =

    (* let () = printf "entering decode\n%!" in *)
    let nb_hv = !T.nb_hidden_vars in
    let n = Array.length sent in

    let nb_hv_sq = nb_hv * nb_hv in
    let nb_hv_cu = nb_hv_sq * nb_hv in

    (*best path scores*)
    let path_scores = Array.init n
      ~f:(fun i ->
        let s1 = Array.length (size_func i) in
        let s2 = if i = 0 then 1 else Array.length (size_func (i-1)) in
        Array.create ~len:(s1*s2*nb_hv_sq) Float.neg_infinity) in

    (* backpointers*)
    let bp  = Array.init n
      ~f:(fun i ->
        if i < 2 then Array.empty()
        else
          let s1 = Array.length (size_func i) in
          let s2 = Array.length (size_func (i-1)) in
          Array.create ~len:(s1*s2*nb_hv_sq) (-1,-1)
      ) in


    (* case i = 0 *)
    (* assume only start at position 0 *)
    let () =
      let rec build_path_0 path_entry  lv =
        if lv < 0 then ()
        else
          let offset = lv * nb_hv in
          let uni_score = 0.0 in
          let current_score = Array.unsafe_get path_entry offset in
          let () =
            if uni_score > current_score
            then Array.unsafe_set path_entry offset uni_score (* no bp because we are at 0 *)
          in
          build_path_0 path_entry (lv-1)
      in
      build_path_0 (Array.unsafe_get path_scores 0) (nb_hv-1);

    in
    (* case i = 1 *)
    (* assume only start at position 0 *)
    let () =
      let path_entry = Array.unsafe_get path_scores 1 in
      let previous_path_entry = Array.unsafe_get path_scores 0 in
      let valid_pos_current = size_func 1 in
      for curpos_idx = 0 to (Array.length valid_pos_current) - 1 do
        for lv = 0 to nb_hv - 1 do
          for plv = 0 to nb_hv -1 do

            let score = 0.0 in
            let offset = curpos_idx * nb_hv_sq + lv * nb_hv + plv in
            let current_score = Array.unsafe_get path_entry offset in
            if score > current_score
            then
              (* no bp because we are at 1 *)
              Array.unsafe_set path_entry offset score
          done
        done
      done
    in

    (*could be specific cases for i = 2 and i = |sent| ...*)


    let rec fill_path i =
      if i = n then ()
      else
        let path_entry = Array.unsafe_get path_scores i in
        let bp_entry = Array.unsafe_get bp i in
        let previous_path_entry = Array.unsafe_get path_scores (i-1) in

        let valid_current_pos = size_func i in
        let size_current = Array.length valid_current_pos in
        let valid_left_pos = size_func (i-1) in
        let size_left = Array.length valid_left_pos in
        let valid_left_left_pos = size_func (i-2) in
        let size_left_left = Array.length valid_left_left_pos in

        for curpos_idx = 0 to size_current - 1 do
          let curpos = Array.unsafe_get valid_current_pos curpos_idx in
          let current_path_offset = curpos_idx * size_left * nb_hv_sq in

          for lv = 0 to nb_hv - 1 do
            (* no unigram for END token *)
            let uni_score = if i = n - 1 then 0.0 else F.get_uni_score params sent i curpos lv in

            let current_path_offset = current_path_offset + lv * nb_hv in

            for leftpos_idx = 0 to size_left - 1 do
              let leftpos = Array.unsafe_get valid_left_pos leftpos_idx in
              let current_path_offset = current_path_offset + leftpos_idx * nb_hv_sq in
              let previous_path_offset = leftpos_idx * size_left_left * nb_hv_sq in

              for plv = 0 to nb_hv - 1 do

                let bi_score = F.get_bi_score params sent i leftpos plv curpos lv in
                let score = uni_score +. bi_score in

                let current_path_offset = current_path_offset + plv in
                let previous_path_offset = previous_path_offset + plv * nb_hv in

                for leftleftpos_idx = 0 to size_left_left - 1 do
                  let leftleftpos = Array.unsafe_get valid_left_left_pos leftleftpos_idx in

                  let previous_path_offset =  previous_path_offset + leftleftpos_idx * nb_hv_sq in

                  for pplv = 0 to nb_hv - 1 do
                    let tri_score = F.get_tri_score params sent i leftleftpos pplv leftpos plv curpos lv in
                    (* let tri_score = 0.0 in *)

                    let score = score +. tri_score
                      +. (Array.unsafe_get previous_path_entry (previous_path_offset + pplv)) in
                    let current_score = Array.unsafe_get path_entry current_path_offset in
                    if score > current_score
                    then
                      begin
                        Array.unsafe_set path_entry current_path_offset score;
                        Array.unsafe_set bp_entry current_path_offset (leftleftpos_idx,pplv)
                      end
                  done
                done
              done
            done
          done
        done;
        fill_path (i+1)
    in
    fill_path 2;


    (* get the path_scores (last tier) *)
    (* this assumes that only stop is possible at last position *)

    (* assert((Array.length (size_func 0)) = 1); *)
    (* assert((Array.length (size_func 1)) = 1); *)
    (* assert((Array.length (size_func (n-1))) = 1); *)

    let stop_pos = C.prediction C.stop in
    let path_entry = Array.unsafe_get path_scores (n-1) in
    let find_max lv ((b,_)as acc) score = if score > b then (score,lv) else acc in

    let _,bst_int = Array.foldi path_entry ~init:(Float.neg_infinity,-1) ~f:find_max in
    (* printf "size stop: %d\n%!" (Array.length (size_func (n-1))); *)
    (* let () = assert(Array.length (size_func (n-1)) = 1) in *)

    (*from bst_int recover y_{n-2} and latent variables*)
    (* printf "bst_int %d\n%!" bst_int; *)
    let last_valid_pos = size_func (n-2) in
    (* let size_last = Array.length last_valid_pos in *)
    let bst_plat = bst_int mod nb_hv in
    let bst_int = bst_int / nb_hv in
    let bst_lat = bst_int mod nb_hv in
    let bst_int =  bst_int / nb_hv in
    (* printf "%d %d\n%!" size_last bst_int; *)
    let bst_ppos = Array.unsafe_get last_valid_pos bst_int in

    (* set last pos from previous iteration *)
    let output = Array.copy sent in
    Array.replace output (n-1) ~f:(fun t -> Conll_Tag.set_latentprediction t stop_pos bst_lat);
    Array.replace output (n-2) ~f:(fun t -> Conll_Tag.set_latentprediction t bst_ppos bst_plat);

    (* go backward in bp chain *)
    let rec bpchain i nextpos_idx nextlat nextnextpos_idx nextnextlat =
      if i < 0 then ()
      else
        begin
          (* printf "i: %d\n%!" i; *)
          let bp_entry = Array.unsafe_get bp (i+2) in
          let nextpos_size = Array.length (size_func (i+1)) in
          (* printf "nextpos_size %d nextpos_idx %d\n%!" nextpos_size nextpos_idx; *)
          (* printf "bp_entry %d index %d\n%!" (Array.length bp_entry) (nextnextpos_idx * nextpos_size * nb_hv_sq *)
          (*                                                            + nextpos_idx * nb_hv_sq + nextnextlat * nb_hv + nextlat); *)
          let pos_idx,lat = Array.unsafe_get bp_entry (nextnextpos_idx * nextpos_size * nb_hv_sq
                                                + nextpos_idx * nb_hv_sq + nextnextlat * nb_hv + nextlat) in
          (* printf "pos_idx %d\n%!" pos_idx; *)
          let pos = Array.unsafe_get (size_func i) pos_idx in

          let () = Array.replace output i ~f:(fun t -> C.set_latentprediction t pos lat) in
          bpchain (i-1) pos_idx lat nextpos_idx nextlat
        end
    in
    bpchain (n-3) bst_int bst_plat 0  bst_lat;
    output


  (** Use POS seen in train to filter search space
      @param params weight vector
      @param sent sentence

  *)
  let decode params sent =
    let pruner = C.collect_word_tags () in
    let poslist = C.collect_unk_tags () in
    let sent_valid_tags = Array.map sent
      ~f:(fun tok ->
        let f = C.get_form_id tok in
        if f < (Array.length pruner) && (not (Array.is_empty (Array.unsafe_get pruner f)))
        then Array.unsafe_get pruner f
        else poslist) in
    let get_valid_tags i = Array.unsafe_get sent_valid_tags i
    in
    decode_general get_valid_tags params sent


  (*sent must be pos tagged*)
  (** Use goldpos to filter search space
      @param params weight vector
      @param sent sentence

  *)
  let constrained_decode params sent =
    let sent_valid_tags = Array.map sent ~f:(fun tok -> [|C.prediction tok|]) in
    let get_valid_tags i = Array.unsafe_get sent_valid_tags i in
    decode_general get_valid_tags params sent


  (** Read, parse to file and possibly evaluate a conll file
      @param filename name of output file
      @param feature_weights feature weight vector
      @param corpus the corpus of sentences to read and parse
      @param verbose verbose mode
      @param evaluation and eval type instance

  *)
  let decode_corpus ~filename ~feature_weights ~corpus ~verbose ~evaluation =
    let decode_func = decode  feature_weights in
    let oc = Out_channel.create filename in
    let evaluator = E.empty in
    let t1 = Time.now () in
    let () =
      List.iter (C.corpus_to_list corpus)
        ~f:(fun s ->
          let ref_sentence = C.prepare_sentence_for_decoder s in
          let hyp_sentence = decode_func ref_sentence in
          if evaluation then E.update evaluator ~ref_sentence ~hyp_sentence;
          (* Array.rev_inplace hyp_sentence; (\* right to left *\) *)
          Array.iter hyp_sentence ~f:(fun tok ->
            if (C.same_prediction tok C.start) || (C.same_prediction tok C.stop) then ()
            else Printf.fprintf oc "%s\n" (C.to_string tok));
          fprintf oc "\n"
        ) in
    (if verbose
     then
        let t2 = Time.now () in
        printf "Decoding time: %s\n" (Core.Span.to_string (Time.diff t2 t1)));
    if evaluation
    then fprintf Out_channel.stdout "%s\n%!" (E.to_string evaluator);
    Out_channel.close oc


  let opt_oper oper = function
    | None -> Some (oper 0 1) (* init: +/- 1 *)
    | Some x -> Some (oper x 1) (* update: x +/- 1 *)



  (**

  *)
  let get_feature_differences (htbl : (int,int) Hashtbl.t)
      (size : int) ref_sent  hyp_sent =


    let rec loop_on_seq i =
      (* let comp_func = C.same_fine_prediction in *)
      let comp_func = C.same_prediction in
      if i < Array.length ref_sent
      then
        let () =
          if
            i > 1 &&
              not (comp_func ref_sent.(i) hyp_sent.(i))
          then
            (
              F.get_uni_features  htbl size (opt_oper (+)) ref_sent i;
              F.get_uni_features  htbl size (opt_oper (-)) hyp_sent i
            )
          else
            ();

          if i > 1 &&
            i > 0 &&
            ((not (comp_func ref_sent.(i) hyp_sent.(i)))
             || (not (comp_func ref_sent.(i-1) hyp_sent.(i-1))))
          then
            (
              F.get_bi_features  htbl size (opt_oper (+)) ref_sent i;
              F.get_bi_features  htbl size (opt_oper (-)) hyp_sent i
            )
          else
            ();

          if i > 1 &&  ((not (comp_func ref_sent.(i) hyp_sent.(i)))
                        || (not (comp_func ref_sent.(i-1) hyp_sent.(i-1)))
                        || (not (comp_func ref_sent.(i-2) hyp_sent.(i-2))))
          then
            (
              F.get_tri_features  htbl size (opt_oper (+)) ref_sent i;
              F.get_tri_features  htbl size (opt_oper (-)) hyp_sent i
            )
          else
            ();
          ()
        in
        loop_on_seq (i+1)
    in
    let () = loop_on_seq 0 in

    ()
end
