
open Core.Std

open Int2stringmap
open Template
open Conll

module Feature (C : ConllType) (T : Template with module C = C) = struct

  module C = C

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

  let get_all_features  htbl feature_vector_size opt_oper sent d =
    let is_valid i = i > 0 && i < feature_vector_size in
    (* let _ = T.fill_hash_table  htbl is_valid of_template_valid opt_oper sent h d in *)
    ()


  let fun_score feature_vector template =
    let idx = of_template_valid template in
    if idx >= 0 (* && idx < Array.length feature_vector *)
    then Array.unsafe_get feature_vector idx
    else 0.0

  (* let get_head_score feature_vector sent h td = *)
  (*   Template.make_template_tok_head false (fun_score feature_vector) sent h td *)
  (* let get_mod_score  feature_vector sent m td = *)
  (*   Template.make_template_tok_mod  false (fun_score feature_vector) sent m td *)
  (* let get_dep_score  feature_vector sent h m = *)
  (*   Template.make_template_dep  (fun_score feature_vector) sent h m *)


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


  let collect_features_on_corpus ~only_gold corpus =
    let instances = ref 0 in
    let () = Printf.printf "\nCollecting features:\n" in

    List.iter corpus
              ~f:
              (
                fun s ->
                instances := !instances + 1;
                Printf.printf "Sentence %d\r%!" !instances;

                C.prepare_sentence_for_decoder s
                |> T.collect_templates ~only_gold
              )

  let compute_score_difference refp hypp  = 0.0

end
