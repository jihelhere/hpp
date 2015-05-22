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
  module F = Feature_Tag
  module Feature = F




  let decode params sent =
    let pruner  = C.collect_word_tags () in
    let poslist = C.collect_unk_tags () in

    (* let () = printf "entering decode\n%!" in *)
    let nb_labels = C.get_number_pos () in
    let nb_hv = !T.nb_hidden_vars in
    let n = Array.length sent in

    let sent_valid_tags = Array.map sent
      ~f:(fun tok ->
        let f = C.get_form_id tok in
        if f < (Array.length pruner) && (not (Array.is_empty (Array.unsafe_get pruner f)))
        then Array.unsafe_get pruner f
        else poslist)
    in
    let get_valid_tags i =
      if i < 0 then [|0|]
      else Array.unsafe_get sent_valid_tags i
    in

    (*build uni/bi scores*)
    let hyp_scores = Array.init n ~f:(fun i ->
      let s1 = get_valid_tags i     |> Array.length in
      let s2 = get_valid_tags (i-1) |> Array.length in
      Array.create ~len:(nb_hv * nb_hv * s1 * s2) Float.neg_infinity) in

    let rec fill_hyp_scores sent hyp_scores n i =

      let rec iter_previous_latent hyp_entry offset uni_score bi_fun curpos leftpos lv plv =
        if plv >= nb_hv then ()
        else
          let bi_score = bi_fun leftpos plv curpos lv in
          Array.unsafe_set hyp_entry (offset + plv) (uni_score +. bi_score);
          iter_previous_latent hyp_entry offset uni_score bi_fun curpos leftpos lv (plv + 1)
      in

      let iter_previous entry i curpos curpos_idx lv uni_score bi_fun =
        let left_valid_pos_list = get_valid_tags (i-1) in
        let l2 = Array.length left_valid_pos_list in
        let nb_hv_sq = nb_hv * nb_hv in
        let offset = curpos_idx * l2 * nb_hv_sq + lv * nb_hv in
        Array.iteri left_valid_pos_list
          ~f:(fun leftpos_idx leftpos ->
            let offset = offset + leftpos_idx * nb_hv_sq in
            iter_previous_latent entry offset uni_score bi_fun curpos leftpos lv 0
          )
      in

      let rec iter_current_latent entry uni_fun bi_fun curpos curpos_idx lv =
        if lv >= nb_hv then ()
        else
          let uni_score = uni_fun curpos lv in
          iter_previous entry i curpos curpos_idx lv uni_score bi_fun;
          iter_current_latent entry uni_fun bi_fun curpos curpos_idx (lv+1)
      in

      let iter_current hyp_scores i cur_pos_list uni_fun bi_fun =
        let hyp_entry = Array.unsafe_get hyp_scores i in
        Array.iteri cur_pos_list ~f:(fun curpos_idx curpos -> iter_current_latent hyp_entry uni_fun bi_fun curpos curpos_idx 0)
      in

      if i >= n then ()
      else
        let uni_score_fun = F.get_uni_score params sent i in
        let bi_score_fun  = F.get_bi_score params sent i in
        let valid_pos_list = get_valid_tags i in

        iter_current hyp_scores i valid_pos_list uni_score_fun bi_score_fun;
        fill_hyp_scores sent hyp_scores n (i+1)
     in
    fill_hyp_scores sent hyp_scores n 0;

    (*best path scores*)
    let path_scores = Array.init n
      ~f:(fun i -> let s1 = get_valid_tags i |> Array.length in
                   Array.create ~len:(s1*nb_hv) Float.neg_infinity) in
    (* backpointers*)
    let bp = Array.create ~len:(n*nb_labels*nb_hv) (-1,-1) in
    (* let bp = Array.init n ~f:(fun i -> *)
    (*   let s1 = get_valid_tags i |> List.length in *)
    (*   Array.create ~len:(s1 * nb_hv) (-1,-1)) in *)


    let rec fill_path_scores i =

      let rec fill_path_scores_left_latent offset leftpos_idx plv lv best_score bestplv =
        if plv >= nb_hv then (best_score,bestplv)
        else
          let hyp_entry = Array.unsafe_get hyp_scores i in
          let score = Array.unsafe_get hyp_entry (offset + lv * nb_hv + plv) in
          let score = score +. if i > 0
            then Array.unsafe_get (Array.unsafe_get path_scores (i-1)) (leftpos_idx * nb_hv  + plv) else 0.0 in
          if score > best_score
          then fill_path_scores_left_latent offset leftpos_idx (plv+1) lv score plv
          else fill_path_scores_left_latent offset leftpos_idx (plv+1) lv best_score bestplv
      in

      let rec fill_path_scores_current_latent path_score_entry left_valid_pos_list curpos_idx curpos lv =
        if lv >= nb_hv then ()
        else
          let l2 = Array.length left_valid_pos_list in
          let best_score, best_leftpos, best_plv =
            Array.foldi left_valid_pos_list
              ~init:(Float.neg_infinity,-1,-1)
              ~f:(fun leftpos_idx ((best_score,_,_) as acc) leftpos ->
                let offset = (curpos_idx * l2 + leftpos_idx) * nb_hv * nb_hv in
                let score, plv = fill_path_scores_left_latent offset leftpos_idx 0 lv Float.neg_infinity (-1) in
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
          fill_path_scores_current_latent path_score_entry left_valid_pos_list curpos_idx curpos (lv+1)

      in

      let fill_path_scores_current path_score_entry i valid_pos_list =
        let left_valid_pos_list = get_valid_tags (i-1) in
        Array.iteri valid_pos_list
          ~f:(fun curpos_idx curpos ->
            fill_path_scores_current_latent path_score_entry left_valid_pos_list curpos_idx curpos 0
          )
      in
      if i >= n then ()
      else
        let valid_pos_list = get_valid_tags i in
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
                let score = (Array.unsafe_get e (last_pos_idx*nb_hv + plv)) +. (F.get_bi_score params sent n last_pos plv stop_pos lv) in
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

    let _,bst_ppos,bst_plat, bst_lat =  find_best_final_transition (C.prediction C.stop) (get_valid_tags (n-1)) in


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




  (*       let decode_old params sent = *)

  (*   let pruner  = C.collect_word_tags () in *)
  (*   let poslist = C.collect_unk_tags () in *)

  (*   (\* let () = printf "entering decode\n%!" in *\) *)
  (*   let nb_labels = C.get_number_pos () in *)
  (*   let nb_hv = T.nb_hidden_vars in *)
  (*   let n = Array.length sent in *)

  (*   (\*build uni/bi scores*\) *)
  (*   let hyp_scores = Array.create ~len:(n*nb_labels*nb_labels*nb_hv*nb_hv) Float.neg_infinity in *)

  (*   let i_incr =    nb_labels * nb_hv * nb_labels * nb_hv in *)
  (*   let ppos_incr =             nb_hv * nb_labels * nb_hv in *)
  (*   let plat_incr =                     nb_labels * nb_hv in *)
  (*   let pos_incr =                                  nb_hv in *)

  (*   let sent_valid_tags = Array.map sent ~f:(fun tok -> *)
  (*     let f = C.get_form_id tok in *)
  (*     if f < (Array.length pruner) && (Array.unsafe_get pruner f) <> [] *)
  (*     then Array.unsafe_get pruner f *)
  (*     else poslist) *)
  (*   in *)

  (*   let rec iter_previous i pos lat offset uni_score bi_score_fun ppos_list plat = *)
  (*     match ppos_list with *)
  (*     | [] -> () *)
  (*     | ppos::rest -> *)
  (*     if plat >= nb_hv then iter_previous i pos lat offset uni_score bi_score_fun rest 0 *)
  (*     else *)
  (*       let ppos_offset = ppos * ppos_incr in *)
  (*       let plat_offset = plat * plat_incr in *)
  (*       let bi_score = bi_score_fun ppos plat pos lat in *)
  (*       Array.unsafe_set hyp_scores (offset + ppos_offset + plat_offset) (uni_score +. bi_score); *)
  (*       iter_previous i pos lat offset uni_score bi_score_fun ppos_list (plat+1) *)
  (*   in *)

  (*   let rec iter_current i valid_pos_list lat offset uni_score_fun bi_score_fun = *)
  (*     match valid_pos_list with *)
  (*     | [] -> () *)
  (*     | pos::rest -> *)
  (*     if lat >= nb_hv then iter_current i rest 0 offset uni_score_fun bi_score_fun *)
  (*     else *)
  (*       let pos_offset = pos * pos_incr in *)
  (*       let lat_offset = lat in *)
  (*       let uni_score = uni_score_fun pos lat in *)

  (*       let left_valid_pos_list = (\* poslist *\) *)
  (*         if i = 0 then [C.prediction C.start] *)
  (*         else Array.unsafe_get sent_valid_tags (i-1) *)
  (*       in *)
  (*       iter_previous i pos lat (offset + pos_offset + lat_offset) uni_score bi_score_fun left_valid_pos_list 0; *)
  (*       iter_current i valid_pos_list (lat+1) offset uni_score_fun bi_score_fun *)
  (*   in *)

  (*   let rec fill_hyp_scores i= *)
  (*     if i >= n then () *)
  (*     else *)
  (*       let i_offset = i * i_incr in *)
  (*       let uni_score_fun = F.get_uni_score params sent i in *)
  (*       let bi_score_fun  = F.get_bi_score params sent i in *)

  (*       let valid_pos_list = (\* poslist *\) *)
  (*         Array.unsafe_get sent_valid_tags i *)
  (*       in *)
  (*       iter_current i valid_pos_list 0 i_offset uni_score_fun bi_score_fun; *)
  (*       fill_hyp_scores (i+1) *)
  (*   in *)
  (*   let () = fill_hyp_scores 0 in *)


  (* (\*best path scores*\) *)
  (*   let path_scores = Array.create ~len:(n*nb_labels*nb_hv) Float.neg_infinity in *)
  (* (\* backpointers*\) *)
  (*   let bp = Array.create ~len:(n*nb_labels*nb_hv) (-1,-1) in *)

  (*   (\* fill score chart (partial paths score) + backpointers *\) *)
  (*   let rec find_best_transition i pos lat ppos_list plat  bst_score bst_ppos bst_plat = *)
  (*     match ppos_list with *)
  (*     | [] -> (bst_score,bst_ppos,bst_plat) *)
  (*     | ppos::rest -> *)
  (*       if plat >= nb_hv then find_best_transition i pos lat rest 0 bst_score bst_ppos bst_plat *)
  (*       else *)
  (*         let cur_score = Array.unsafe_get hyp_scores *)
  (*           (i * i_incr + ppos * ppos_incr + plat * plat_incr + pos * pos_incr + lat) +. *)
  (*           if i > 0 then Array.unsafe_get path_scores ( (i-1)*nb_labels * nb_hv + ppos * nb_hv + plat) *)
  (*           else 0.0 in *)
  (*         if cur_score > bst_score *)
  (*         then find_best_transition i pos lat ppos_list (plat+1)  cur_score ppos plat *)
  (*         else find_best_transition i pos lat ppos_list (plat+1)  bst_score bst_ppos bst_plat *)
  (*    in *)

  (*   let rec fill_path_scores_current i valid_pos_list lat = *)
  (*     match valid_pos_list with *)
  (*     | [] -> () *)
  (*     | pos::rest -> *)
  (*        if lat >= nb_hv then fill_path_scores_current i rest 0 *)
  (*        else *)
  (*          let left_valid_pos_list = (\* poslist *\) *)
  (*            if i = 0 then [C.prediction C.start] *)
  (*            else Array.unsafe_get sent_valid_tags (i-1) *)
  (*          in *)

  (*          let bst_score,bst_ppos,bst_plat = find_best_transition i pos lat left_valid_pos_list 0 Float.neg_infinity (-1) (-1) in *)
  (*          Array.unsafe_set path_scores (i*nb_labels*nb_hv + pos*nb_hv + lat) bst_score; *)
  (*          Array.unsafe_set bp (i*nb_labels*nb_hv + pos*nb_hv + lat ) (bst_ppos,bst_plat); *)
  (*       fill_path_scores_current i valid_pos_list (lat+1) *)
  (*   in *)
  (*   let rec fill_path_scores i = *)
  (*     if i >= n then () *)
  (*     else *)
  (*       let valid_pos_list = (\* poslist *\) *)
  (*         Array.unsafe_get sent_valid_tags i *)
  (*       in *)
  (*       fill_path_scores_current i valid_pos_list 0; *)
  (*       fill_path_scores (i+1) *)
  (*   in *)
  (*   fill_path_scores 0; *)



  (*   (\* compute path scores (add the missing POS -> STOP edge) *\) *)
  (*   let rec find_best_final_transition n stop_pos stop_lat ppos_list plat bst_score bst_ppos bst_plat bst_lat = *)
  (*     match ppos_list with *)
  (*     | [] -> (bst_score,bst_ppos,bst_plat,bst_lat) *)
  (*     | ppos::rest -> *)
  (*        if stop_lat >= nb_hv *)
  (*        then find_best_final_transition n stop_pos 0 rest 0 bst_score bst_ppos bst_plat bst_lat *)
  (*        else *)
  (*          if plat >= nb_hv then find_best_final_transition n stop_pos (stop_lat+1) ppos_list 0 bst_score bst_ppos bst_plat bst_lat *)
  (*          else *)
  (*            let cur_score = if n > 1 *)
  (*              then Array.unsafe_get path_scores ((n-1)*nb_labels * nb_hv + ppos * nb_hv + plat) *)
  (*              else 0.0 in *)
  (*            let cur_score = cur_score +. F.get_bi_score params sent n ppos plat stop_pos stop_lat in *)
  (*            if cur_score > bst_score *)
  (*            then find_best_final_transition n stop_pos stop_lat ppos_list (plat+1) cur_score ppos plat stop_lat *)
  (*            else find_best_final_transition n stop_pos stop_lat ppos_list (plat+1) bst_score bst_ppos bst_plat bst_lat *)
  (*   in *)

  (*   let ppos_list = Array.unsafe_get sent_valid_tags (n-1) *)
  (*   in *)

  (*   let _,bst_ppos,bst_plat, bst_lat = *)
  (*     find_best_final_transition n (C.prediction C.stop) 0 ppos_list 0 Float.neg_infinity (-1) (-1) (-1) in *)

  (*   (\* set last pos from previous iteration *\) *)
  (*   let output = Array.copy sent in *)
  (*   Array.replace output (n-1) ~f:(fun t -> Conll_Tag.set_latentprediction t bst_ppos bst_plat); *)


  (*   (\* go backward in bp chain *\) *)
  (*   let rec aux i nextlat = *)
  (*     if i < 0 then () *)
  (*     else *)
  (*       let next_pred = C.prediction (Array.unsafe_get output (i+1)) in *)
  (*       let pos,lat = Array.unsafe_get bp ((i+1)*nb_labels*nb_hv + next_pred*nb_hv + nextlat) in *)
  (*       let () = Array.replace output i ~f:(fun t -> C.set_latentprediction t pos lat) in *)
  (*       aux (i-1) lat *)
  (*   in *)
  (*   aux (n-2) bst_plat; *)

  (*   (\* for i = 0 to n-1 do *\) *)

  (*   (\*   printf "%d %d\n%!" (C.prediction (Array.unsafe_get sent i)) (C.prediction (Array.unsafe_get output i)) *\) *)
  (*   (\* done; *\) *)
  (*   (\* printf "\n%!"; *\) *)

  (*   let first = Array.unsafe_get output 0 in *)

  (*   (output, *)
  (*    snd (Array.unsafe_get bp ((C.prediction first)*nb_hv + (C.latent_prediction first))), *)
  (*    bst_lat) *)



  (*sent must be pos tagged*)
  let constrained_decode params sent =
    (* let () = printf "entering constrained decode\n%!" in *)
    let nb_hv = !T.nb_hidden_vars in
    let n = Array.length sent in

    (*build uni/bi scores*)
    let hyp_scores = Array.create ~len:(n*nb_hv*nb_hv) 0.0 in
    for i = 0 to (n-1) do
      let index = i * nb_hv * nb_hv in
      let pos = C.prediction sent.(i) in
      for latvar = 0 to (nb_hv -1) do
        let uni_score = F.get_uni_score params sent i pos latvar in
        let ppos = if i = 0 then C.prediction C.start else C.prediction sent.(i-1) in
        for platvar = 0 to nb_hv -1 do
          let bi_score = F.get_bi_score params sent i ppos platvar pos latvar in
          Array.unsafe_set hyp_scores (index + platvar * nb_hv + latvar) (uni_score +. bi_score)
        done
      done
    done;


    (* let () = printf "built scores constrained decode\n%!" in *)
  (*best path scores*)
    let scores = Array.create ~len:(n*nb_hv) 0.0 in
  (* backpointers*)
    let bp = Array.create ~len:(n*nb_hv) (-1) in

    (* fill score chart (partial paths score) + backpointers *)
    for i = 0 to (n-1) do
      for lat = 0 to nb_hv - 1 do
        let bst_score = ref Float.neg_infinity in
        let bst_plat = ref (-1) in
        for plat = 0  to nb_hv - 1 do
          let cur_score = Array.unsafe_get hyp_scores (i * nb_hv * nb_hv + plat * nb_hv + lat)
            +. if i > 0 then Array.unsafe_get scores ((i-1) * nb_hv + plat) else 0.0 in
          if cur_score > !bst_score
          then
            (
              bst_score := cur_score;
              bst_plat := plat
            )
        done;
        (
          Array.unsafe_set scores (i*nb_hv + lat) !bst_score;
          Array.unsafe_set bp (i*nb_hv + lat ) (!bst_plat)
        )
      done
    done;


    (* let () = printf "filled path chart constrained decode\n%!" in *)

    (* compute path scores (add the missing POS -> STOP edge) *)
    let last = n-1 in
    let stop_pos = C.prediction C.stop in
    let bst_score = ref Float.neg_infinity in
    let bst_plat = ref (-1) in
    let bst_lat  = ref (-1) in
    let last_pos = C.prediction sent.(last) in
    for last_lat = 0 to nb_hv - 1 do
      let cur_score = if last > 0 then Array.unsafe_get scores (last*nb_hv + last_lat) else 0.0 in
      for stop_lat = 0 to nb_hv -1 do
        let cur_score = cur_score +. F.get_bi_score params sent n last_pos last_lat stop_pos stop_lat in
        if cur_score > !bst_score
        then
          (
            bst_score := cur_score;
            bst_plat  := last_lat;
            bst_lat  := stop_lat
          )
      done
    done;


    (* let () = printf "computed last constrained decode\n%!" in *)

    (* set last pos from previous iteration *)
    let output = Array.copy sent in
    Array.replace output last ~f:(fun t -> Conll_Tag.set_latentprediction t (C.prediction t) !bst_plat);


    (* go backward in bp chain *)
    let rec aux i nextlat =
      if i < 0 then ()
      else
        let lat = Array.unsafe_get bp ((i+1)*nb_hv + nextlat) in
        let () = Array.replace output i ~f:(fun t -> C.set_latentprediction t (C.prediction t) lat) in
        aux (i-1) lat
    in
    aux (last-1) !bst_plat;

    (* for i = 0 to n-1 do *)

    (*   printf "%d %d\n%!" (C.prediction (Array.unsafe_get sent i)) (C.prediction (Array.unsafe_get output i)) *)
    (* done; *)
    (* printf "\n%!"; *)

    let first = Array.unsafe_get output 0 in

    (* let () = printf "rewind bps constrained decode\n%!" in *)


    (output,
     Array.unsafe_get bp (C.latent_prediction first),
     !bst_lat)

  let decode_corpus ~filename ~feature_weights ~corpus ~verbose =
    let decode_func = decode  feature_weights in
    let oc = Out_channel.create filename in
    let t1 = Time.now () in
    let () =
      List.iter (C.corpus_to_list corpus)
        ~f:(fun s ->
          let ref_a = C.prepare_sentence_for_decoder s in
          let (hyp_a,lat_start,lat_stop) = decode_func ref_a in
          Array.iter hyp_a ~f:(fun tok -> Printf.fprintf oc "%s\n" (C.to_string tok));
          fprintf oc "\n"
        ) in
    let t2 = Time.now () in
    (if verbose
     then
       printf "Decoding time: %s\n" (Core.Span.to_string (Time.diff t2 t1)));
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
          if  not (C.same_fine_prediction ref_sent.(i) hyp_sent.(i))
          then
            (
              F.get_uni_features  htbl size (opt_oper (+)) ref_sent i;
              F.get_uni_features  htbl size (opt_oper (-)) hyp_sent i
            )
          else ();
          if i > 0 && ((not (C.same_fine_prediction ref_sent.(i) hyp_sent.(i)))
                       || (not (C.same_fine_prediction ref_sent.(i-1) hyp_sent.(i-1))))
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

    if rfl <> hfl || (not (C.same_fine_prediction ref_sent.(0) hyp_sent.(0)))
    then
      (
        F.get_bi_features_first  htbl size (opt_oper (+)) ref_sent rfl;
        F.get_bi_features_first  htbl size (opt_oper (-)) hyp_sent hfl
      )
    else ();

    if rel <> hel
    then
      (
        F.get_bi_features_stop  htbl size (opt_oper (+)) ref_sent rel;
        F.get_bi_features_stop  htbl size (opt_oper (-)) hyp_sent hel
      );

    ()



end
