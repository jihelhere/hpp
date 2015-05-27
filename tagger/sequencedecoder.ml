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



module Sequence_Decoder =
struct

  module C = Conll_Tag
  module T = Template_Tag
  module F = Feature_Tag
  module Feature = F

  module E = Eval_Tag(C)



  (* sentence unigram scores aka node scores *)
  let build_uni_scores params sent sent_size nb_hv size_func =
    let uni_scores = Array.init sent_size ~f:(fun i ->
      let s = size_func i |> Array.length in
      Array.create ~len:(nb_hv * s) Float.neg_infinity)
    in

    let rec iter_current_latent entry uni_score_fun pos pos_idx lv =
      if lv < 0 then ()
      else
        begin
          let uni_score = uni_score_fun pos lv in
          Array.unsafe_set entry (pos_idx * nb_hv + lv) uni_score;
          iter_current_latent entry uni_score_fun pos pos_idx (lv-1)
        end
    in

    let rec fill_uni_scores sent uni_scores i =
      if i < 0 then ()
      else
        begin
          let uni_score_fun = F.get_uni_score params sent i in
          let valid_pos_list = size_func i in
          let uni_entry = Array.unsafe_get uni_scores i in
          Array.iteri valid_pos_list
            ~f:(fun pos_idx pos -> iter_current_latent uni_entry uni_score_fun pos pos_idx (nb_hv-1));
          fill_uni_scores sent uni_scores (i-1)
        end
    in
    fill_uni_scores sent uni_scores (sent_size - 1);
    uni_scores


  (* bigram scores *)
  let build_bi_scores params sent sent_size nb_hv size_func =
    let bi_scores = Array.init sent_size ~f:(fun i ->
      let s1 = size_func i     |> Array.length in
      let s2 = size_func (i-1) |> Array.length in
      Array.create ~len:(nb_hv * nb_hv * s1 * s2) Float.neg_infinity) in

    let nb_hv_sq = nb_hv * nb_hv in

    let rec iter_previous_latent hyp_entry offset bi_fun curpos leftpos lv plv =
      if plv < 0 then ()
      else
        begin
            (* printf "plv: %d\n%!" plv; *)
          let bi_score = bi_fun leftpos plv curpos lv in
          Array.unsafe_set hyp_entry (offset + plv) bi_score;
          iter_previous_latent hyp_entry offset bi_fun curpos leftpos lv (plv - 1)
        end
    in

    let iter_previous entry left_valid_pos_list curpos curpos_idx lv bi_fun =
      let l2 = Array.length left_valid_pos_list in
      let offset = curpos_idx * l2 * nb_hv_sq + lv * nb_hv in
      Array.iteri left_valid_pos_list
        ~f:(fun leftpos_idx leftpos ->
          let offset = offset + leftpos_idx * nb_hv_sq in
            (* printf "leftpos_idx: %d\n%!" leftpos_idx; *)
          iter_previous_latent entry offset bi_fun curpos leftpos lv (nb_hv -1)
        )
    in

      let rec iter_current_latent entry bi_fun left_valid_pos_list curpos curpos_idx lv =
        if lv < 0 then ()
        else
          begin
            (* printf "curpos_idx: %d\tlv: %d\n%!" curpos_idx lv; *)
            iter_previous entry left_valid_pos_list curpos curpos_idx lv bi_fun;
            iter_current_latent entry bi_fun left_valid_pos_list curpos curpos_idx (lv-1)
          end
      in

    let rec fill_bi_scores sent bi_scores i =
      if i < 0 then ()
      else
        begin
          (* printf "%d\n%!" i; *)
          let bi_score_fun  = F.get_bi_score params sent i in
          let valid_pos_list = size_func i in
          let left_valid_pos_list = size_func (i-1) in
          let bi_entry = Array.unsafe_get bi_scores i in
          Array.iteri valid_pos_list
            ~f:(fun curpos_idx curpos -> iter_current_latent bi_entry bi_score_fun left_valid_pos_list curpos curpos_idx (nb_hv-1));
          fill_bi_scores sent bi_scores (i-1)
        end
     in
    fill_bi_scores sent bi_scores (sent_size - 1);
    bi_scores



  (* (\* trigram scores *\) *)
  (* let build_tri_scores params sent sent_size nb_hv size_func = *)
  (*   let tri_scores = Array.init sent_size ~f:(fun i -> *)
  (*     let s1 = size_func i     |> Array.length in *)
  (*     let s2 = size_func (i-1) |> Array.length in *)
  (*     let s3 = size_func (i-2) |> Array.length in *)
  (*     Array.create ~len:(nb_hv * nb_hv * nb_hv * s1 * s2 * s3) Float.neg_infinity) in *)

  (*   let nb_hv_sq = nb_hv * nb_hv in *)
  (*   let nb_hv_cu = nb_hv_sq * nb_hv in *)

  (*   let rec iter_previous_latent entry left_left_valid_pos_list offset tri_fun curpos leftpos lv plv = *)
  (*     if plv < 0 then () *)
  (*     else *)
  (*       begin *)
  (*         (\* printf "plv: %d\n%!" plv; *\) *)
  (*         Array.iteri left_valid_pos_list *)

  (*         iter_previous_latent hyp_entry offset bi_fun curpos leftpos lv (plv - 1) *)
  (*       end *)
  (*   in *)

  (*   let iter_previous entry left_left_valid_pos_list left_valid_pos_list curpos curpos_idx lv tri_fun = *)
  (*     let l2 = Array.length left_valid_pos_list in *)
  (*     let l3 = Array.length left_left_valid_pos_list in *)
  (*     let offset = curpos_idx * l2 * l3 * nb_hv_cu + lv * nb_hv_sq in *)
  (*     Array.iteri left_valid_pos_list *)
  (*       ~f:(fun leftpos_idx leftpos -> *)
  (*         let offset = offset + leftpos_idx * l3 * nb_hv_cu in *)
  (*         iter_previous_latent entry offset left_left_valid_pos_list tri_fun curpos leftpos lv (nb_hv -1) *)
  (*       ) *)
  (*   in *)

  (*   let rec iter_current_latent entry tri_fun left_left_valid_pos_list left_valid_pos_list curpos curpos_idx lv = *)
  (*     if lv < 0 then () *)
  (*     else *)
  (*       begin *)
  (*           (\* printf "curpos_idx: %d\tlv: %d\n%!" curpos_idx lv; *\) *)
  (*         iter_previous entry left_left_valid_pos_list left_valid_pos_list curpos curpos_idx lv tri_fun; *)
  (*         iter_current_latent entry tri_fun left_left_valid_pos_list left_valid_pos_list curpos curpos_idx (lv-1) *)
  (*       end *)
  (*   in *)

  (*   let rec fill_tri_scores sent tri_scores i = *)
  (*     if i < 0 then () *)
  (*     else *)
  (*       begin *)
  (*         (\* printf "%d\n%!" i; *\) *)
  (*         let tri_score_fun  = F.get_tri_score params sent i in *)

  (*         let valid_pos_list = size_func i in *)
  (*         let left_valid_pos_list = size_func (i-1) in *)
  (*         let left_left_valid_pos_list = size_func (i-1) in *)

  (*         let tri_entry = Array.unsafe_get tri_scores i in *)
  (*         Array.iteri valid_pos_list *)
  (*           ~f:(fun curpos_idx curpos -> iter_current_latent tri_entry tri_score_fun left_left_valid_pos_list left_valid_pos_list curpos curpos_idx (nb_hv-1)); *)
  (*         fill_tri_scores sent tri_scores (i-1) *)
  (*       end *)
  (*    in *)
  (*   fill_bi_scores sent tri_scores (sent_size - 1); *)
  (*   tri_scores *)



  let decode_general size_func params sent =

    (* let () = printf "entering decode\n%!" in *)
    let nb_labels = C.get_number_pos () in
    let nb_hv = !T.nb_hidden_vars in
    let n = Array.length sent in

    let uni_scores =  build_uni_scores params sent n nb_hv size_func in
    let bi_scores =  build_bi_scores params sent n nb_hv size_func in

    (*best path scores*)
    let path_scores = Array.init n
      ~f:(fun i -> let s1 = size_func i |> Array.length in
                   Array.create ~len:(s1*nb_hv) Float.neg_infinity) in

    (* backpointers*)
    let bp = Array.create ~len:(n*nb_labels*nb_hv) (-1,-1) in




    let rec fill_path_scores_left_latent i uni_score bi_entry offset leftpos_idx plv lv best_score bestplv =
      if plv >= nb_hv then (best_score,bestplv)
      else
        let score = uni_score +. Array.unsafe_get bi_entry (offset + lv * nb_hv + plv) in
        let score = score +. if i > 0
          then Array.unsafe_get (Array.unsafe_get path_scores (i-1)) (leftpos_idx * nb_hv  + plv)
          else F.get_uni_score params sent i 0 lv in
        if score > best_score
        then fill_path_scores_left_latent i uni_score bi_entry offset leftpos_idx (plv+1) lv score plv
        else fill_path_scores_left_latent i uni_score bi_entry offset leftpos_idx (plv+1) lv best_score bestplv

    in

    let rec fill_path_scores_current_latent i uni_entry bi_entry path_score_entry left_valid_pos_list curpos_idx curpos lv =
      if lv >= nb_hv then ()
      else
        let uni_score = Array.unsafe_get uni_entry (curpos_idx * nb_hv + lv) in
        let l2 = Array.length left_valid_pos_list in
        let offset = curpos_idx * l2 in
        let best_score, best_leftpos, best_plv =
          Array.foldi left_valid_pos_list
            ~init:(Float.neg_infinity,-1,-1)
            ~f:(fun leftpos_idx ((best_score,_,_) as acc) leftpos ->
              let offset = (offset + leftpos_idx) * nb_hv * nb_hv in
              let score, plv = fill_path_scores_left_latent i uni_score bi_entry offset leftpos_idx 0 lv Float.neg_infinity (-1) in
              if score > best_score
              then (score, leftpos, plv)
              else acc
            )
        in
        let current_score = Array.unsafe_get path_score_entry (curpos_idx * nb_hv + lv) in
        if best_score > current_score
        then
          begin
            Array.unsafe_set path_score_entry (curpos_idx * nb_hv + lv) best_score;
            Array.unsafe_set bp (i*nb_labels*nb_hv + curpos * nb_hv + lv) (best_leftpos,best_plv)
          end;
        fill_path_scores_current_latent i uni_entry bi_entry path_score_entry left_valid_pos_list curpos_idx curpos (lv+1)

    in

    let fill_path_scores_current path_score_entry i valid_pos_list =
      let uni_entry = Array.unsafe_get uni_scores i in
      let bi_entry = Array.unsafe_get bi_scores i in
      let left_valid_pos_list = size_func (i-1) in
      Array.iteri valid_pos_list
        ~f:(fun curpos_idx curpos ->
          fill_path_scores_current_latent i uni_entry bi_entry path_score_entry left_valid_pos_list curpos_idx curpos 0
        )
    in

    let rec fill_path_scores i =
      if i >= n then ()
      else
        let valid_pos_list = size_func i in
        let entry = Array.unsafe_get path_scores i in
        fill_path_scores_current entry i valid_pos_list;
        fill_path_scores (i+1)
    in
    fill_path_scores 0;

    (* compute path scores (add the missing POS -> STOP edge) *)
    let find_best_final_transition stop_pos last_pos_list =
      let e = Array.unsafe_get path_scores (n-1) in
      Array.foldi last_pos_list
        ~init:(Float.neg_infinity,-1,-1,-1)
        ~f:(fun last_pos_idx ((b,_,_,_)as acc) last_pos ->
          let rec aux_find_best_final_transition plv lv best_score bestlv bestplv =
            if plv >= nb_hv then (best_score, bestplv, bestlv)
            else
              if lv >= nb_hv then aux_find_best_final_transition (plv+1) 0 best_score bestlv bestplv
              else
                let score = (Array.unsafe_get e (last_pos_idx*nb_hv + plv)) +. (F.get_bi_score params sent n last_pos plv stop_pos lv)

                in
                if score > best_score
                then aux_find_best_final_transition plv (lv+1) score lv plv
                else aux_find_best_final_transition plv (lv+1) best_score bestlv bestplv
          in
          let bc,plc,lc = aux_find_best_final_transition 0 0 Float.neg_infinity (-1) (-1) in
          if bc > b
          then
            (bc,last_pos,plc,lc)
          else acc
        )
    in

    let _,bst_ppos,bst_plat, bst_lat =  find_best_final_transition (C.prediction C.stop) (size_func (n-1)) in


  (* set last pos from previous iteration *)
  let output = Array.copy sent in
    Array.replace output (n-1) ~f:(fun t -> Conll_Tag.set_latentprediction t bst_ppos bst_plat);


    (* go backward in bp chain *)
    let rec aux i nextlat =
      if i < 0 then ()
      else
        let next_pred = C.prediction (Array.unsafe_get output (i+1)) in
        let pos,lat = Array.unsafe_get bp ((i+1)*nb_labels*nb_hv + next_pred*nb_hv + nextlat) in
        let () = Array.replace output i ~f:(fun t -> C.set_latentprediction t pos lat) in
        aux (i-1) lat
    in
    aux (n-2) bst_plat;

     let first = Array.unsafe_get output 0 in
     (* printf "%d %d\n%!" (C.prediction first) (C.latent_prediction first); *)

    (output,
     snd (Array.unsafe_get bp ((C.prediction first)*nb_hv + (C.latent_prediction first))),
     bst_lat)



  let decode params sent =
    let pruner = C.collect_word_tags () in
    let poslist = C.collect_unk_tags () in
    let sent_valid_tags = Array.map sent
      ~f:(fun tok ->
        let f = C.get_form_id tok in
        if f < (Array.length pruner) && (not (Array.is_empty (Array.unsafe_get pruner f)))
        then Array.unsafe_get pruner f
        else poslist) in
    let get_valid_tags i =
      if i < 0 then [|0|]
      else Array.unsafe_get sent_valid_tags i
    in
    decode_general get_valid_tags params sent


  (*sent must be pos tagged*)
  let constrained_decode params sent =
    let sent_valid_tags = Array.map sent
      ~f:(fun tok -> [|C.prediction tok|])
    in
    let get_valid_tags i =
      if i < 0 then [|0|]
      else Array.unsafe_get sent_valid_tags i
    in
    decode_general get_valid_tags params sent



  (* (\*sent must be pos tagged*\) *)
  (* let constrained_decode params sent = *)
  (*   (\* let () = printf "entering constrained decode\n%!" in *\) *)
  (*   let nb_hv = !T.nb_hidden_vars in *)
  (*   let n = Array.length sent in *)

  (*   (\*build uni/bi scores*\) *)
  (*   let hyp_scores = Array.create ~len:(n*nb_hv*nb_hv) 0.0 in *)
  (*   for i = 0 to (n-1) do *)
  (*     let index = i * nb_hv * nb_hv in *)
  (*     let pos = C.prediction sent.(i) in *)
  (*     for latvar = 0 to (nb_hv -1) do *)
  (*       let uni_score = F.get_uni_score params sent i pos latvar in *)
  (*       let ppos = if i = 0 then C.prediction C.start else C.prediction sent.(i-1) in *)
  (*       for platvar = 0 to nb_hv -1 do *)
  (*         let bi_score = F.get_bi_score params sent i ppos platvar pos latvar in *)
  (*         Array.unsafe_set hyp_scores (index + platvar * nb_hv + latvar) (uni_score +. bi_score) *)
  (*       done *)
  (*     done *)
  (*   done; *)


  (*   (\* let () = printf "built scores constrained decode\n%!" in *\) *)
  (* (\*best path scores*\) *)
  (*   let scores = Array.create ~len:(n*nb_hv) 0.0 in *)
  (* (\* backpointers*\) *)
  (*   let bp = Array.create ~len:(n*nb_hv) (-1) in *)

  (*   (\* fill score chart (partial paths score) + backpointers *\) *)
  (*   for i = 0 to (n-1) do *)
  (*     for lat = 0 to nb_hv - 1 do *)
  (*       let bst_score = ref Float.neg_infinity in *)
  (*       let bst_plat = ref (-1) in *)
  (*       for plat = 0  to nb_hv - 1 do *)
  (*         let cur_score = Array.unsafe_get hyp_scores (i * nb_hv * nb_hv + plat * nb_hv + lat) *)
  (*           +. *)
  (*             if i > 0 *)
  (*             then Array.unsafe_get scores ((i-1) * nb_hv + plat) *)
  (*             else F.get_uni_score params sent i 0 lat in *)
  (*         if cur_score > !bst_score *)
  (*         then *)
  (*           ( *)
  (*             bst_score := cur_score; *)
  (*             bst_plat := plat *)
  (*           ) *)
  (*       done; *)
  (*       ( *)
  (*         Array.unsafe_set scores (i*nb_hv + lat) !bst_score; *)
  (*         Array.unsafe_set bp (i*nb_hv + lat ) (!bst_plat) *)
  (*       ) *)
  (*     done *)
  (*   done; *)


  (*   (\* let () = printf "filled path chart constrained decode\n%!" in *\) *)

  (*   (\* compute path scores (add the missing POS -> STOP edge) *\) *)
  (*   let last = n-1 in *)
  (*   let stop_pos = C.prediction C.stop in *)
  (*   let bst_score = ref Float.neg_infinity in *)
  (*   let bst_plat = ref (-1) in *)
  (*   let bst_lat  = ref (-1) in *)
  (*   let last_pos = C.prediction sent.(last) in *)
  (*   for last_lat = 0 to nb_hv - 1 do *)
  (*     let cur_score = if last > 0 then Array.unsafe_get scores (last*nb_hv + last_lat) else 0.0 in *)
  (*     for stop_lat = 0 to nb_hv -1 do *)
  (*       let cur_score = cur_score +. F.get_bi_score params sent n last_pos last_lat stop_pos stop_lat *)
  (*         +. F.get_uni_score params sent n stop_pos stop_lat in *)
  (*       if cur_score > !bst_score *)
  (*       then *)
  (*         ( *)
  (*           bst_score := cur_score; *)
  (*           bst_plat  := last_lat; *)
  (*           bst_lat  := stop_lat *)
  (*         ) *)
  (*     done *)
  (*   done; *)


  (*   (\* let () = printf "computed last constrained decode\n%!" in *\) *)

  (*   (\* set last pos from previous iteration *\) *)
  (*   let output = Array.copy sent in *)
  (*   Array.replace output last ~f:(fun t -> Conll_Tag.set_latentprediction t (C.prediction t) !bst_plat); *)


  (*   (\* go backward in bp chain *\) *)
  (*   let rec aux i nextlat = *)
  (*     if i < 0 then () *)
  (*     else *)
  (*       let lat = Array.unsafe_get bp ((i+1)*nb_hv + nextlat) in *)
  (*       let () = Array.replace output i ~f:(fun t -> C.set_latentprediction t (C.prediction t) lat) in *)
  (*       aux (i-1) lat *)
  (*   in *)
  (*   aux (last-1) !bst_plat; *)

  (*   (\* for i = 0 to n-1 do *\) *)

  (*   (\*   printf "%d %d\n%!" (C.prediction (Array.unsafe_get sent i)) (C.prediction (Array.unsafe_get output i)) *\) *)
  (*   (\* done; *\) *)
  (*   (\* printf "\n%!"; *\) *)

  (*   let first = Array.unsafe_get output 0 in *)

  (*   (\* let () = printf "rewind bps constrained decode\n%!" in *\) *)


  (*   (output, *)
  (*    Array.unsafe_get bp (C.latent_prediction first), *)
  (*    !bst_lat) *)

  let decode_corpus ~filename ~feature_weights ~corpus ~verbose ~evaluation =
    let decode_func = decode  feature_weights in
    let oc = Out_channel.create filename in
    let evaluator = E.empty in
    let t1 = Time.now () in
    let () =
      List.iter (C.corpus_to_list corpus)
        ~f:(fun s ->
          let ref_sentence = C.prepare_sentence_for_decoder s in
          let (hyp_sentence,_,_) = decode_func ref_sentence in
          if evaluation then E.update evaluator ~ref_sentence ~hyp_sentence;
          Array.iter hyp_sentence ~f:(fun tok -> Printf.fprintf oc "%s\n" (C.to_string tok));
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

  let get_feature_differences (htbl : (int,int) Hashtbl.t)
      (size : int) (ref_sent, (rfl : int), (rel : int)) (hyp_sent,hfl,hel) =


    let rec loop_on_seq i =
      if i < Array.length ref_sent
      then
        let () =
          (* if  not (C.same_fine_prediction ref_sent.(i) hyp_sent.(i)) *)
                      if  not (C.same_prediction ref_sent.(i) hyp_sent.(i))
          then
            (
              F.get_uni_features  htbl size (opt_oper (+)) ref_sent i;
              F.get_uni_features  htbl size (opt_oper (-)) hyp_sent i
            )
          else ();
          if i > 0 && (* ((not (C.same_fine_prediction ref_sent.(i) hyp_sent.(i))) *)
                      (*  || (not (C.same_fine_prediction ref_sent.(i-1) hyp_sent.(i-1)))) *)
            ((not (C.same_prediction ref_sent.(i) hyp_sent.(i)))
                       || (not (C.same_prediction ref_sent.(i-1) hyp_sent.(i-1))))
          then
            (
              F.get_bi_features  htbl size (opt_oper (+)) ref_sent i;
              F.get_bi_features  htbl size (opt_oper (-)) hyp_sent i
            )
          else ()

        in
        loop_on_seq (i+1)
    in
    let () = loop_on_seq 0 in

    if (* rfl <> hfl || *) (not (C.same_fine_prediction ref_sent.(0) hyp_sent.(0)))
    then
      (
        F.get_bi_features_first  htbl size (opt_oper (+)) ref_sent rfl;
        F.get_bi_features_first  htbl size (opt_oper (-)) hyp_sent hfl
      )
    else ();

    (* if rfl <> hfl *)
    (* then *)
    (*   ( *)
    (*     F.get_uni_features_start  htbl size (opt_oper (+)) ref_sent rfl; *)
    (*     F.get_uni_features_start  htbl size (opt_oper (-)) hyp_sent hfl *)
    (*   ) *)

    (* else (); *)


    (* if rel <> hel *)
    (* then *)
    (*   ( *)
    (*     F.get_bi_features_stop  htbl size (opt_oper (+)) ref_sent rel; *)
    (*     F.get_bi_features_stop  htbl size (opt_oper (-)) hyp_sent hel; *)
    (*     F.get_uni_features_stop  htbl size (opt_oper (+)) ref_sent rel; *)
    (*     F.get_uni_features_stop  htbl size (opt_oper (-)) hyp_sent hel *)
    (*   ); *)

    ()



end
