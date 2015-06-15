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

open Int2stringmap
open Templatetag
open Conlltag

module Feature_Tag = struct

  module C = Conll_Tag
  module T = Template_Tag

  type t = int

  let of_int t = t
  let to_int t = t

  let template_feature_map = Hashtbl.create ~hashable:T.hashable ()
  let counter = ref 0

  let of_template template =
    match Hashtbl.find template_feature_map template with
    | None ->
       let d = !counter in
       let () = counter  := !counter + 1 in
       let _ = Hashtbl.add template_feature_map ~key:template ~data:d in
       d
    | Some d -> d

  let of_template_valid template =
    match Hashtbl.find template_feature_map template with
    | None -> -1
    | Some d -> d

  let is_valid feature_vector_size t =
    t < feature_vector_size

  let get_uni_features htbl feature_vector_size opt_oper sent i =
    let is_valid i = i > 0 && i < feature_vector_size in
    let tok = (Array.unsafe_get sent i) in
    let pos = C.prediction tok in
    let lat = C.latent_prediction tok in
    T.fill_hash_table_uni_part  htbl is_valid of_template_valid opt_oper sent i pos lat

  let get_uni_features_start htbl feature_vector_size opt_oper sent lat =
    let is_valid i = i > 0 && i < feature_vector_size in
    let tok = C.start in
    let pos = C.prediction tok in
    T.fill_hash_table_uni_part  htbl is_valid of_template_valid opt_oper sent 0 pos lat

  let get_uni_features_stop htbl feature_vector_size opt_oper sent lat =
    let is_valid i = i > 0 && i < feature_vector_size in
    let tok = C.stop in
    let pos = C.prediction tok in
    T.fill_hash_table_uni_part  htbl is_valid of_template_valid opt_oper sent ((Array.length sent)-1) pos lat

  let get_bi_features htbl feature_vector_size opt_oper sent i =
    let is_valid i = i > 0 && i < feature_vector_size in
    let tok = (Array.unsafe_get sent i) in
    let ptok = (Array.unsafe_get sent (i-1)) in
    let pos = C.prediction tok in
    let lat = C.latent_prediction tok  in
    let ppos = C.prediction ptok  in
    let plat = C.latent_prediction ptok in
    T.fill_hash_table_bi_part  htbl is_valid of_template_valid opt_oper sent i ppos plat pos lat

  let get_bi_features_first htbl feature_vector_size opt_oper sent plat =
    let is_valid i = i > 0 && i < feature_vector_size in
    let first = Array.unsafe_get sent 0 in
    let pos = C.prediction first in
    let lat = C.latent_prediction first in
    let ppos = C.prediction C.start in
    T.fill_hash_table_bi_part  htbl is_valid of_template_valid opt_oper sent 0 ppos plat pos lat


  let get_bi_features_stop htbl feature_vector_size opt_oper sent lat =
    let is_valid i = i > 0 && i < feature_vector_size in
    let pos = C.prediction C.stop in
    let last = Array.unsafe_get sent ((Array.length sent)-1) in
    let ppos = C.prediction last in
    let plat = C.latent_prediction last in
    T.fill_hash_table_bi_part  htbl is_valid of_template_valid opt_oper sent 0 ppos plat pos lat


  let get_tri_features htbl feature_vector_size opt_oper sent i =
    let is_valid i = i > 0 && i < feature_vector_size in
    let tok = Array.unsafe_get sent i in
    let ptok = Array.unsafe_get sent (i-1) in
    let pptok = Array.unsafe_get sent (i-2) in
    let pos = C.prediction tok in
    let lat = C.latent_prediction tok  in
    let ppos = C.prediction ptok  in
    let plat = C.latent_prediction ptok in
    let pppos = C.prediction pptok  in
    let pplat = C.latent_prediction pptok in
    T.fill_hash_table_tri_part  htbl is_valid of_template_valid opt_oper sent i pppos pplat ppos plat pos lat


  let get_all_features _h _s _o _r _i = failwith "not implemented yet"
  (* let get_all_features  htbl feature_vector_size opt_oper sent d = *)
  (*   let is_valid i = i > 0 && i < feature_vector_size in *)

  (*   let pos = C.prediction (Array.unsafe_get sent d) in *)
  (*   let ppos = if d = 0 then - 1 else C.prediction (Array.unsafe_get sent (d-1)) in *)
  (*   let _ : float = T.fill_hash_table  htbl is_valid of_template_valid opt_oper sent d ppos pos in *)
  (*   () *)


  let fun_score feature_vector template =
    let idx = of_template_valid template in
    if idx >= 0 (* && idx < Array.length feature_vector *)
    then Array.unsafe_get feature_vector idx
    else 0.0

  let get_uni_score feature_vector sent i pos lat =
    T.make_template_uni (fun_score feature_vector) sent i pos lat

  let get_bi_score feature_vector sent i ppos plat pos lat =
    T.make_template_bi (fun_score feature_vector) sent i ppos plat pos lat

  let get_tri_score feature_vector sent i pppos pplat ppos plat pos lat =
    T.make_template_tri (fun_score feature_vector) sent i pppos pplat ppos plat pos lat

  let prune_features threshold =
    let () = Printf.printf "feature table contains %d entries\n" (Hashtbl.length T.table_collect_templates) in

    (* remove features below threshold *)
    let () =
      Hashtbl.filter_inplace T.table_collect_templates
                             ~f:(fun nb -> nb >= threshold)
    in
    let res =     Hashtbl.length T.table_collect_templates in

    (* frequent features first *)
    let () = Hashtbl.to_alist T.table_collect_templates
             |> List.sort ~cmp:(fun (_,c) (_,c') -> Int.descending c c')
             |> List.iter ~f:(fun (t,_) -> let _ = of_template t in ())
    in
    (* free memory *)
    let () = Hashtbl.clear T.table_collect_templates in
    let () = T.reset_table_collect_template () in
    let () = Gc.compact () in
    res

  let template_feature_map_to_sexp () =
    let a x = Sexp.Atom x and l x = Sexp.List x in
    l [
        a "template_feature_map";
        Hashtbl.sexp_of_t T.sexp_of_t Int.sexp_of_t template_feature_map;
      ]

  let save_template_feature_map_to_file filename =
    template_feature_map_to_sexp ()
    |> Sexp.save_hum filename
    (* let oc = Out_channel.create filename in *)
    (* Marshal.to_channel oc template_feature_map [Marshal.Closures]; *)
    (* Out_channel.close oc *)


  let load_template_map_from_sexp sexp =
    let rec add_pairs l =
      match l with
      | [] -> ()
      | Sexp.List([Sexp.Atom key; Sexp.Atom value])::tl ->
         let _ = Hashtbl.add template_feature_map ~key:(T.of_int (Int.of_string key))
                             ~data:(Int.of_string value)
         in
         add_pairs tl
      | _ -> assert(false)
    in
    match sexp with
      Sexp.List(pairs) -> add_pairs pairs
    | _ -> assert(false)


  let collect_features_on_corpus ~only_gold corpus ~verbose =
    let instances = ref 0 in
    let () = Printf.printf "\nCollecting features:\n%!" in

    List.iter corpus
      ~f:(fun s ->
        instances := !instances + 1;
        if verbose then printf "Sentence %d\r%!" !instances;
        C.prepare_sentence_for_decoder s |> T.collect_templates ~only_gold
      );
    printf "\nDone:\n"

  let compute_score_difference _params _refs _hyps _i _refp _hypp  = failwith "not implemented"
    (* (get_uni_score params refs i (C.prediction refp)) *)
    (* +. *)
    (*   (if i > 0 *)
    (*    then (get_bi_score params refs i (C.prediction (Array.unsafe_get refs (i-1))) (C.prediction refp)) *)
    (*    else (get_bi_score params refs i (C.prediction C.start) (C.prediction refp) )) *)
    (* +. (\*not sure about this one*\) *)
    (*   (if i = (Array.length refs) -1 *)
    (*    then (get_bi_score params refs (Array.length refs) (C.prediction refp) (C.prediction C.stop)) *)
    (*    else 0.0) *)
    (* -. *)
    (*   (get_uni_score params hyps i (C.prediction hypp)) *)
    (* -. *)
    (*   (if i > 0 *)
    (*    then (get_bi_score params hyps i (C.prediction (Array.unsafe_get hyps (i-1))) (C.prediction hypp)) *)
    (*    else (get_bi_score params hyps i (C.prediction C.start) (C.prediction hypp))) *)
    (* -. (\*not sure about this one*\) *)
    (*   (if i = (Array.length hyps) -1 *)
    (*    then (get_bi_score params hyps (Array.length hyps) (C.prediction hypp) (C.prediction C.stop)) *)
    (*    else 0.0) *)



end
