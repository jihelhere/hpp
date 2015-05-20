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


  let decode params (pruner,poslist) sent =

    (* let () = printf "entering decode\n%!" in *)
    let nb_labels = C.get_number_pos () in
    let nb_hv = T.nb_hidden_vars in
    let n = Array.length sent in

    (*build uni/bi scores*)
    let hyp_scores = Array.create ~len:(n*nb_labels*nb_labels*nb_hv*nb_hv) Float.neg_infinity in

    let i_incr =    nb_labels * nb_hv * nb_labels * nb_hv in
    let ppos_incr =             nb_hv * nb_labels * nb_hv in
    let plat_incr =                     nb_labels * nb_hv in
    let pos_incr =                                  nb_hv in

    let sent_valid_tags = Array.map sent ~f:(fun tok ->
      let f = C.get_form_id tok in
      if f < (Array.length pruner) && (Array.unsafe_get pruner f) <> []
      then Array.unsafe_get pruner f
      else poslist)
    in

    let rec iter_previous i pos lat offset uni_score bi_score_fun ppos_list plat =
      match ppos_list with
      | [] -> ()
      | ppos::rest ->
      if plat >= nb_hv then iter_previous i pos lat offset uni_score bi_score_fun rest 0
      else
        let ppos_offset = ppos * ppos_incr in
        let plat_offset = plat * plat_incr in
        let bi_score = bi_score_fun ppos plat pos lat in
        Array.unsafe_set hyp_scores (offset + ppos_offset + plat_offset) (uni_score +. bi_score);
        iter_previous i pos lat offset uni_score bi_score_fun ppos_list (plat+1)
    in

    let rec iter_current i valid_pos_list lat offset uni_score_fun bi_score_fun =
      match valid_pos_list with
      | [] -> ()
      | pos::rest ->
      if lat >= nb_hv then iter_current i rest 0 offset uni_score_fun bi_score_fun
      else
        let pos_offset = pos * pos_incr in
        let lat_offset = lat in
        let uni_score = uni_score_fun pos lat in

        let left_valid_pos_list = (* poslist *)
          if i = 0 then [C.prediction C.start]
          else Array.unsafe_get sent_valid_tags (i-1)
        in
        iter_previous i pos lat (offset + pos_offset + lat_offset) uni_score bi_score_fun left_valid_pos_list 0;
        iter_current i valid_pos_list (lat+1) offset uni_score_fun bi_score_fun
    in

    let rec fill_hyp_scores i=
      if i >= n then ()
      else
        let i_offset = i * i_incr in
        let uni_score_fun = F.get_uni_score params sent i in
        let bi_score_fun  = F.get_bi_score params sent i in

        let valid_pos_list = (* poslist *)
          Array.unsafe_get sent_valid_tags i
        in
        iter_current i valid_pos_list 0 i_offset uni_score_fun bi_score_fun;
        fill_hyp_scores (i+1)
    in
    let () = fill_hyp_scores 0 in


  (*best path scores*)
    let path_scores = Array.create ~len:(n*nb_labels*nb_hv) Float.neg_infinity in
  (* backpointers*)
    let bp = Array.create ~len:(n*nb_labels*nb_hv) (-1,-1) in

    (* fill score chart (partial paths score) + backpointers *)
    let rec find_best_transition i pos lat ppos_list plat  bst_score bst_ppos bst_plat =
      match ppos_list with
      | [] -> (bst_score,bst_ppos,bst_plat)
      | ppos::rest ->
        if plat >= nb_hv then find_best_transition i pos lat rest 0 bst_score bst_ppos bst_plat
        else
          let cur_score = Array.unsafe_get hyp_scores
            (i * i_incr + ppos * ppos_incr + plat * plat_incr + pos * pos_incr + lat) +.
            if i > 0 then Array.unsafe_get path_scores ( (i-1)*nb_labels * nb_hv + ppos * nb_hv + plat)
            else 0.0 in
          if cur_score > bst_score
          then find_best_transition i pos lat ppos_list (plat+1)  cur_score ppos plat
          else find_best_transition i pos lat ppos_list (plat+1)  bst_score bst_ppos bst_plat
     in

    let rec fill_path_scores_current i valid_pos_list lat =
      match valid_pos_list with
      | [] -> ()
      | pos::rest ->
         if lat >= nb_hv then fill_path_scores_current i rest 0
         else
           let left_valid_pos_list = (* poslist *)
             if i = 0 then [C.prediction C.start]
             else Array.unsafe_get sent_valid_tags (i-1)
           in

           let bst_score,bst_ppos,bst_plat = find_best_transition i pos lat left_valid_pos_list 0 Float.neg_infinity (-1) (-1) in
           Array.unsafe_set path_scores (i*nb_labels*nb_hv + pos*nb_hv + lat) bst_score;
           Array.unsafe_set bp (i*nb_labels*nb_hv + pos*nb_hv + lat ) (bst_ppos,bst_plat);
        fill_path_scores_current i valid_pos_list (lat+1)
    in
    let rec fill_path_scores i =
      if i >= n then ()
      else
        let valid_pos_list = (* poslist *)
          Array.unsafe_get sent_valid_tags i
        in
        fill_path_scores_current i valid_pos_list 0;
        fill_path_scores (i+1)
    in
    fill_path_scores 0;



    (* compute path scores (add the missing POS -> STOP edge) *)
    let rec find_best_final_transition n stop_pos stop_lat ppos_list plat bst_score bst_ppos bst_plat bst_lat =
      match ppos_list with
      | [] -> (bst_score,bst_ppos,bst_plat,bst_lat)
      | ppos::rest ->
         if stop_lat >= nb_hv
         then find_best_final_transition n stop_pos 0 rest 0 bst_score bst_ppos bst_plat bst_lat
         else
           if plat >= nb_hv then find_best_final_transition n stop_pos (stop_lat+1) ppos_list 0 bst_score bst_ppos bst_plat bst_lat
           else
             let cur_score = if n > 1
               then Array.unsafe_get path_scores ((n-1)*nb_labels * nb_hv + ppos * nb_hv + plat)
               else 0.0 in
             let cur_score = cur_score +. F.get_bi_score params sent n ppos plat stop_pos stop_lat in
             if cur_score > bst_score
             then find_best_final_transition n stop_pos stop_lat ppos_list (plat+1) cur_score ppos plat stop_lat
             else find_best_final_transition n stop_pos stop_lat ppos_list (plat+1) bst_score bst_ppos bst_plat bst_lat
    in

    let ppos_list = Array.unsafe_get sent_valid_tags (n-1)
    in

    let _,bst_ppos,bst_plat, bst_lat =
      find_best_final_transition n (C.prediction C.stop) 0 ppos_list 0 Float.neg_infinity (-1) (-1) (-1) in

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

    (* for i = 0 to n-1 do *)

    (*   printf "%d %d\n%!" (C.prediction (Array.unsafe_get sent i)) (C.prediction (Array.unsafe_get output i)) *)
    (* done; *)
    (* printf "\n%!"; *)

    let first = Array.unsafe_get output 0 in

    (output,
     snd (Array.unsafe_get bp ((C.prediction first)*nb_hv + (C.latent_prediction first))),
     bst_lat)



  (*sent must be pos tagged*)
  let constrained_decode params sent =
    (* let () = printf "entering constrained decode\n%!" in *)
    let nb_hv = T.nb_hidden_vars in
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



  let decode_corpus ~filename ~feature_weights ~corpus ~verbose ~pruner =
    let decode_func = decode  feature_weights in
    let oc = Out_channel.create filename in
    let t1 = Time.now () in
    let () =
      List.iter (C.corpus_to_list corpus)
        ~f:(fun s ->
          let ref_a = C.prepare_sentence_for_decoder s in
          let (hyp_a,lat_start,lat_stop) = decode_func pruner ref_a in
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
