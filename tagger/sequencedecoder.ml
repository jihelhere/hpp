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



  (*Stage 2016: remove hidden variables *)
  let () = T.nb_hidden_vars := 1

  (** The general decoding function
      @param valid_pos_func a function returning an array of authorized tags for a token
      @param params the weight vector
      @param sent an array of conll tokens
  *)
  let decode_general valid_pos_func params sent =

    (* let () = printf "entering decode\n%!" in *)
    let n = Array.length sent in

    (*best path scores*)
    let path_scores = Array.init n
        ~f:(fun i ->
            match i with
            | 0 -> Array.create ~len:1 Float.neg_infinity
            | 1 -> Array.create ~len:1 Float.neg_infinity
            | _ ->
              let s1 = Array.length (valid_pos_func i) in
              let s2 = Array.length (valid_pos_func (i-1)) in
              Array.create ~len:(s1*s2) Float.neg_infinity) in

    (* backpointers*)
    let bp  = Array.init n
      ~f:(fun i ->
        if i < 2 then Array.empty()
        else
          let s1 = Array.length (valid_pos_func i) in
          let s2 = Array.length (valid_pos_func (i-1)) in
          Array.create ~len:(s1*s2) (-1)
      ) in


    (* FOR LOOP implementation *)
    (* faster than rec. functions. Why?? *)
    (* todo stage 2016 : benchmark and rewrite with functions *)

    (* case i = 0 *)
    (* assume only start at position 0 *)
    let () =
      let build_path_0 path_entry =
        let curpos = Array.unsafe_get (valid_pos_func 0) 0 in (* TODO:  directly get START *)
        let uni_score = F.get_uni_score params sent 0 curpos in
        let current_score = Array.unsafe_get path_entry 0 in
        if uni_score > current_score
        then Array.unsafe_set path_entry 0 uni_score (* no bp because we are at 0 *)
        else ()
      in
      build_path_0 (Array.unsafe_get path_scores 0);

    in
    (* case i = 1 *)
    (* assume only start at positions 0 and for 1 *)
    let () =
      let path_entry = Array.unsafe_get path_scores 1 in
      let previous_path_entry = Array.unsafe_get path_scores 0 in
      let valid_pos_current = valid_pos_func 1 in
      let curpos_idx = 0 in
      let curpos = Array.unsafe_get valid_pos_current curpos_idx in
      let uni_score = F.get_uni_score params sent 1 curpos in
      let leftpos = Array.unsafe_get (valid_pos_func 0) 0 in
      let score = uni_score +. F.get_bi_score params sent 1 leftpos curpos in
      let score = score +. (Array.unsafe_get previous_path_entry 0) in
      let current_score = Array.unsafe_get path_entry 0 in
      if score > current_score
      then
        (* no bp because we are at 1 *)
        Array.unsafe_set path_entry 0 score
    in

    (*could be specific cases for i = 2 and i = |sent| ...*)


    let rec fill_path i =
      if i = n then ()
      else
        let path_entry = Array.unsafe_get path_scores i in
        let bp_entry = Array.unsafe_get bp i in
        let previous_path_entry = Array.unsafe_get path_scores (i-1) in

        let valid_current_pos = valid_pos_func i in
        let size_current = Array.length valid_current_pos in
        let valid_left_pos = valid_pos_func (i-1) in
        let size_left = Array.length valid_left_pos in
        let valid_left_left_pos = valid_pos_func (i-2) in
        let size_left_left = Array.length valid_left_left_pos in

        for curpos_idx = 0 to size_current - 1 do
          let curpos = Array.unsafe_get valid_current_pos curpos_idx in
          let current_path_offset = curpos_idx * size_left in

          let uni_score = F.get_uni_score params sent i curpos in
          let current_path_offset = current_path_offset in

          for leftpos_idx = 0 to size_left - 1 do
            let leftpos = Array.unsafe_get valid_left_pos leftpos_idx in
            let current_path_offset = current_path_offset + leftpos_idx in
            let previous_path_offset = leftpos_idx * size_left_left in

            let bi_score = F.get_bi_score params sent i leftpos curpos in
            let score = uni_score +. bi_score in

            let current_path_offset = current_path_offset in
            let previous_path_offset = previous_path_offset in

            for leftleftpos_idx = 0 to size_left_left - 1 do
              let leftleftpos = Array.unsafe_get valid_left_left_pos leftleftpos_idx in

              let previous_path_offset =  previous_path_offset + leftleftpos_idx in


              let tri_score = F.get_tri_score params sent i leftleftpos leftpos curpos in
              (* let tri_score = 0.0 in *)

              let score = score +. tri_score +. (Array.unsafe_get previous_path_entry previous_path_offset) in
              let current_score = Array.unsafe_get path_entry current_path_offset in
              if score > current_score
              then
                begin
                  Array.unsafe_set path_entry current_path_offset score;
                  Array.unsafe_set bp_entry current_path_offset (leftleftpos_idx)
                end
            done
          done
        done;
        fill_path (i+1)
    in
    fill_path 2;


    (* get the path_scores (last tier) *)
    (* this assumes that only stop is possible at last position *)

    (* assert((Array.length (valid_pos_func 0)) = 1); *)
    (* assert((Array.length (valid_pos_func 1)) = 1); *)
    (* assert((Array.length (valid_pos_func (n-1))) = 1); *)

    let stop_pos = C.prediction C.stop in
    let path_entry = Array.unsafe_get path_scores (n-1) in
    let find_max lv ((b,_)as acc) score = if score > b then (score,lv) else acc in

    let _,bst_int = Array.foldi path_entry ~init:(Float.neg_infinity,-1) ~f:find_max in
    (* printf "size stop: %d\n%!" (Array.length (valid_pos_func (n-1))); *)
    (* let () = assert(Array.length (valid_pos_func (n-1)) = 1) in *)

    (*from bst_int recover y_{n-2} and latent variables*)
    (* printf "bst_int %d\n%!" bst_int; *)
    let last_valid_pos = valid_pos_func (n-2) in
    (* let size_last = Array.length last_valid_pos in *)
    (* printf "%d %d\n%!" size_last bst_int; *)

    let bst_ppos = Array.unsafe_get last_valid_pos bst_int in

    (* set last pos from previous iteration *)
    let output = Array.copy sent in
    Array.replace output (n-1) ~f:(fun t -> Conll_Tag.set_prediction t stop_pos);
    Array.replace output (n-2) ~f:(fun t -> Conll_Tag.set_prediction t bst_ppos);

    (* Go backward in bp chain *)
    let rec bpchain i nextpos_idx nextnextpos_idx =
      if i < 0 then ()
      else
        begin
          (* printf "i: %d\n%!" i; *)
          let bp_entry = Array.unsafe_get bp (i+2) in
          let nextpos_size = Array.length (valid_pos_func (i+1)) in
          (* printf "nextpos_size %d nextpos_idx %d\n%!" nextpos_size nextpos_idx; *)
          (* printf "bp_entry %d index %d\n%!" (Array.length bp_entry) (nextnextpos_idx * nextpos_size *)
          (*                                                            + nextpos_idx ); *)
          let pos_idx = Array.unsafe_get bp_entry (nextnextpos_idx * nextpos_size + nextpos_idx ) in
          (* printf "pos_idx %d\n%!" pos_idx; *)
          let pos = Array.unsafe_get (valid_pos_func i) pos_idx in

          let () = Array.replace output i ~f:(fun t -> C.set_prediction t pos) in
          bpchain (i-1) pos_idx nextpos_idx
        end
    in
    bpchain (n-3) bst_int 0;
    output


  let get_all_pos sent =
    (* let r = Random.bool () in *)
    let pruner = C.collect_word_tags () in
    let poslist = C.collect_unk_tags () in
    let sent_valid_tags = Array.map sent
        ~f:(fun tok ->
            let f = C.get_form_id tok in
            if f < (Array.length pruner)
            && (not (Array.is_empty (Array.unsafe_get pruner f)))
            (* && (r ||f = 0 || f = 1) *)
            then Array.unsafe_get pruner f
            else poslist) in
    (* Is this better than a hash map? *)
    Array.unsafe_get sent_valid_tags

  let get_all_lats sent =
    Array.unsafe_get
      (Array.init (Array.length sent)
         ~f:(fun _ -> Array.init !T.nb_hidden_vars ~f:(fun i -> i)))

  (*sent must be pos tagged*)
  (** Use goldpos to filter search space
      @param params weight vector
      @param sent sentence
  *)
  let forced_decode params sent =
    let sent_valid_tags = Array.map sent ~f:(fun tok -> [|C.prediction tok|]) in
    let get_valid_tags i = Array.unsafe_get sent_valid_tags i in
    decode_general get_valid_tags params sent


  (** Use POS seen in train to filter search space
      @param params weight vector
      @param sent sentence
  *)
  let decode params sent =
    decode_general (get_all_pos sent) params sent




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



  (** Collect all features from
      @note 'all' has not precise meaning, check subroutine call
      @param htbl the hashtable of feature -> count
  *)
  let get_feature_differences (htbl : (int,int) Hashtbl.t)
      (size : int) ref_sent  hyp_sent =

    let rec report_all_latent i max =
      if i >= max (* Array.length ref_sent *) then ()
      else
        let () =
          begin
            if true
                (* not(C.same_prediction  (Array.unsafe_get ref_sent i) (Array.unsafe_get hyp_sent i)) *)
            then
              (F.get_uni_features htbl size (opt_oper (+)) ref_sent i;
               F.get_uni_features htbl size (opt_oper (-)) hyp_sent i)
            else
              ();
            if i > 0
            (* && (C.same_prediction  (Array.unsafe_get ref_sent (i-1)) (Array.unsafe_get hyp_sent (i-1))) *)
            (* && not(C.same_prediction  (Array.unsafe_get ref_sent i) (Array.unsafe_get hyp_sent i)) *)
            then
              (F.get_bi_features htbl size (opt_oper (+)) ref_sent i;
               F.get_bi_features htbl size (opt_oper (-)) hyp_sent i)
            else
              ();
            if i > 1
            (* && (C.same_prediction  (Array.unsafe_get ref_sent (i-2)) (Array.unsafe_get hyp_sent (i-2))) *)
            (* && (C.same_prediction  (Array.unsafe_get ref_sent (i-1)) (Array.unsafe_get hyp_sent (i-1))) *)
            (* && not(C.same_prediction  (Array.unsafe_get ref_sent i) (Array.unsafe_get hyp_sent i)) *)
            then
              (F.get_tri_features htbl size (opt_oper (+)) ref_sent i;
               F.get_tri_features htbl size (opt_oper (-)) hyp_sent i)
            else
              ()
          end
        in
        report_all_latent (i+1) max
    in

    let obs_first =
      Array.findi ref_sent ~f:(fun i reftok ->
        let hyptok = Array.unsafe_get hyp_sent i in
        not (
          (* C.same_fine_prediction *)
            C.same_prediction
            reftok hyptok
        )) in

    let latobs_first =
      Array.findi ref_sent ~f:(fun i reftok ->
        let hyptok = Array.unsafe_get hyp_sent i in
        not (
          C.same_fine_prediction
            (* C.same_prediction *)
            reftok hyptok
        )) in

    let ()  =
      match (obs_first,latobs_first) with
      | Some(i,r),Some(j,h) when i = j ->
        if C.same_prediction r h && not (C.same_fine_prediction r h)
        then printf "obs and latobs are different\n%!"
      | _ -> ()
  in


    match Array.findi ref_sent ~f:(fun i reftok ->
        let hyptok = Array.unsafe_get hyp_sent i in
        not (
          (* C.same_fine_prediction *)
            C.same_prediction
            reftok hyptok
        )) with
    | None -> ()
    | Some (i,_) ->
      (* let reftok = ref_sent.(i) in *)
      (* let hyptok = hyp_sent.(i) in *)
      (* let () = printf "%d %s (%s,%d) (%s,%d)\n%!" *)
      (*     i (C.get_form reftok) *)
      (*     (C.get_pos reftok) (C.latent_prediction reftok) *)
      (*     (C.get_pos hyptok) (C.latent_prediction hyptok) in *)
      report_all_latent 0 (Array.length ref_sent)

end
