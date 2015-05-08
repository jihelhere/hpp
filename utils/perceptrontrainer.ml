(*
 * Copyright (C) 2014  Joseph Le Roux
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
open Eval
open Decoder
open Feature

module type Updater =
sig
    module C : ConllType

    type t
    val create : total:int -> size:int -> t
    val update :  t -> int -> int -> int -> C.t array ->  C.t array -> unit
    val get_weights : t -> float array
    val average : t -> float array
  end

module OnlineMarginTrainer =
  functor (C : ConllType )
    -> functor (E : Eval with module C = C)
        -> functor (U : Updater with module C = E.C)
            -> functor (D : Decoder with module C = U.C) ->
struct

    (* a triplet of functions *)
    (* first one prints UAS/LAS info : will be called after each training example *)
    (* second one resets eval variables : will be called before each pass *)
    (* third one returns final UAS score *)
    let eval_dep_parse () =
      let eval = E.empty in
      let update_stats ref_sent hyp_sent =
        E.update eval ref_sent hyp_sent;
        Printf.printf "%s\r%!" (E.to_string eval) in
      let reset_stats () = E.reset eval in
      let final_score () = E.to_score eval |> fst in
      (update_stats, reset_stats, final_score)

    let train_and_eval feature_weights fun_update
        (fun_eval, fun_reset_eval, final_score) corpus =
      let my_decoder = D.decode feature_weights in

      fun_reset_eval ();
      List.iteri corpus
        ~f:(fun i s ->
          (* let () = Printf.printf "sentence: %d\n%!" i in *)
          let ref_a = C.prepare_sentence_for_decoder s in
          let hyp_a = my_decoder ref_a in
          fun_eval ref_a hyp_a;
          fun_update i ref_a hyp_a
        );
      final_score ()


    let train_epoch ~update_func ~feature_weights ~corpus =
      train_and_eval feature_weights update_func (eval_dep_parse ())
                     corpus

    let dont_update_model = fun _ _ _ -> ()

    let eval_epoch ~feature_weights ~corpus =
      train_and_eval feature_weights dont_update_model (eval_dep_parse ()) corpus

    let eval_print_epoch ~filename ~feature_weights ~corpus =
      let oc = Out_channel.create filename in
      let (update_stats, reset_stats, final_score) = eval_dep_parse ()
      in
      let update_stats_print sentence hypothesis =
        update_stats sentence hypothesis;
        Array.iter hypothesis ~f:(fun tok -> Printf.fprintf oc "%s\n%!" (C.to_string tok));
        Printf.fprintf oc "\n%!"
      in
      (* REMEMBER TO ALWAYS TYPE UNUSED RESULTS -> *)
      let (_unused_score : float) = train_and_eval feature_weights dont_update_model (update_stats_print, reset_stats, final_score) corpus
      in
      Out_channel.close oc


    let train  ~train_filename ~dev_filename ~test_filename ~
               max_iter ~feature_threshold ~verbose =



      let filename_to_list x = x |> C.do_read_file |> C.corpus_to_list in
      let open Option.Monad_infix in
      let filename_to_list_opt x = x >>= (fun df -> Some (filename_to_list df)) in

      let train_instances = filename_to_list train_filename    in
      let dev_instances   = filename_to_list_opt dev_filename  in
      let test_instances  = filename_to_list_opt test_filename in


      (* collect features on corpus and filter out rare ones *)
      D.Feature.collect_features_on_corpus ~only_gold:true train_instances;
      let size = D.Feature.prune_features feature_threshold in
      Printf.printf "Nb features: %d\n" size;

      (* Initialize feature arrays *)
      let best_feature_vector = Array.create ~len:size 0.0 in
      let updater = U.create ~total:(List.length train_instances) ~size in

      let rec main_loop train_instances best best_score epoch =
        if epoch > max_iter then best
        else
          let () =
            Printf.printf("\nIteration: %d\nTraining\n%!") epoch
          in

          let (_ : float) = train_epoch ~update_func:(U.update updater max_iter epoch)
            ~feature_weights:(U.get_weights updater)
            ~corpus:train_instances
          in


          let average = U.average updater in

          let dev_score =
            match dev_instances with
            | None -> 0.0
            | Some dis ->
               Printf.printf("\nDev:\n%!");
               eval_epoch ~feature_weights:average ~corpus:dis
          in

          let train_instances = List.permute train_instances in
          (* made any progress ? *)
          if dev_score >= best_score
          then main_loop train_instances average dev_score (epoch+1)
          else main_loop train_instances best best_score (epoch+1)

      in
      let train_instances = List.permute train_instances in
      let final = main_loop train_instances best_feature_vector 0.0 1
      in
      let () =
        match test_instances with
        | None -> ()
        | Some tis ->
           Printf.printf("\nTest:\n%!");
           eval_print_epoch ~filename:"test.results" (* TODO give a proper filename *)
                            ~feature_weights:final
                            ~corpus:tis
           ;
           Printf.printf("\n%!")
      in
      final
  end


module PerceptronTrainer(Co : ConllType) (E : Eval with module C = Co) (D : Decoder with module C = Co) =
  struct

    module UpdatePerceptron : Updater with module C = D.C =
    struct
      type t = {size: int;
                mutable counter: int;
                weights: float Array.t;
                average_weights: float Array.t
               }

      module C = Co

      let create ~total:_ ~size = { size = size; counter = 0;
                                    weights = Array.create ~len:size 0.0;
                                    average_weights = Array.create ~len:size 0.0}



      let incr_examples t = t.counter <- t.counter +1



        (* compute perceptron update*)
        let update  t _ _ _ ref_sentence hyp_sentence =
          let htbl = Hashtbl.create ~hashable:Int.hashable () in

          let opt_oper oper = function
            | None -> Some (oper 0 1) (* init: +/- 1 *)
            | Some x -> Some (oper x 1) (* update: x +/- 1 *)
          in

          (* get feature counts where solutions are different *)
          let rec loop_on_seq i =
            if i < Array.length ref_sentence
            then
              let () =
                if  not (Co.same_prediction ref_sentence.(i) hyp_sentence.(i))
                then
                  (
                    D.Feature.get_all_features  htbl t.size (opt_oper (+)) ref_sentence i;
                    D.Feature.get_all_features  htbl t.size (opt_oper (-)) hyp_sentence i
                  )
              in
              loop_on_seq (i+1)
          in
          let () = loop_on_seq 1 in

          (* update model *)
          incr_examples t;
          Hashtbl.iter htbl
            ~f:(fun ~key:fi ~data:count ->
              if count <> 0
              then
                (t.weights.(fi)         <- t.weights.(fi)         +. (Float.of_int count);
                 t.average_weights.(fi) <- t.average_weights.(fi) +. (Float.of_int (t.counter * count)))
            )

        let average t =
          Array.init t.size ~f:(fun i -> t.weights.(i) -. (t.average_weights.(i) /. (Float.of_int t.counter)))

        let get_weights t =
          t.weights
      end

    include OnlineMarginTrainer(Co)(E)(UpdatePerceptron)(D)

    let name = "perceptron"
  end



module MiraTrainer (Co : ConllType) (E : Eval with module C = Co) (D : Decoder with module C = Co) =
  struct
     module UpdateOneBestMira : Updater with module C = D.C =
     struct
       module C = Co
        type t = {size: int;
                  mutable counter: int;
                  examples: int;
                  weights: float Array.t;
                  average_weights: float Array.t;
                  clip: float;}
        let create ~total ~size = { size = size;
                                    counter = 0;
                                    examples = total;
                                    weights = Array.create ~len:size 0.0;
                                    average_weights = Array.create ~len:size 0.0;
                                    clip = 0.01;}

        let incr_examples t = t.counter <- t.counter +1



        (* compute mira update*)
        let update  t max_iter epoch num ref_sentence hyp_sentence =
          let htbl = Hashtbl.create ~hashable:Int.hashable () in

          let opt_oper oper = function
            | None -> Some (oper 0 1) (* init: +/- 1 *)
            | Some x -> Some (oper x 1) (* update: x +/- 1 *)
          in
          let opt_add = opt_oper (+) and opt_sub = opt_oper (-) in


          (* get feature counts where solutions are different *)
          let rec loop_on_arcs i =
            if i < Array.length ref_sentence
            then
              let () =
                if not(C.same_prediction ref_sentence.(i) hyp_sentence.(i))
                then
                  (
                    D.Feature.get_all_features  htbl t.size opt_add ref_sentence i;
                    D.Feature.get_all_features  htbl t.size opt_sub hyp_sentence i
                  )
              in
              loop_on_arcs (i+1)
          in
          let () = loop_on_arcs 1 in

          (* update model *)
          incr_examples t;

          let norm = Hashtbl.fold htbl ~init:0 ~f:(fun ~key:_ ~data:c acc -> acc + c*c) |> Float.of_int in

          (*TODO: this is UAS loss, need to parameterize with something else *)
          let (loss,diff_score) =
            Array.fold2_exn ref_sentence hyp_sentence
                            ~init:(0,0.0)
                            ~f:(fun (loss',diff_score) re hy ->
                                if C.same_prediction re  hy
                                then (loss',diff_score)
                                else
                                  let ds = D.Feature.compute_score_difference re hy
                                  in
                                  (loss'+1,diff_score +. ds)) in

          (* let () = *)
          (*   printf "diff_score (should be negative): %f\n%!" diff_score in *)
          let delta = ((Float.of_int loss) -. diff_score) /. norm in
          (* let () = if delta > 0.0 *)
          (*          then Printf.printf "positive\n%!" in *)
          let alpha = Float.max 0.0 delta |> Float.min t.clip in
          (* let alpha = 1. in *)
          (* double avgUpdate(double(loop * num_examples - (num_examples * ((iteration + 1) - 1) + (num )) + 1)); *)
          let avgalpha = max_iter * t.examples - (t.examples * (epoch-1) + num) + 1 |> Float.of_int |> ( *. ) alpha in

          Hashtbl.iter htbl
                       ~f:(fun ~key:fi ~data:count ->
                           if count <> 0
                           then
                             (t.weights.(fi)         <- t.weights.(fi)         +. ((Float.of_int count) *. alpha) ;
                              t.average_weights.(fi) <- t.average_weights.(fi) +. ((Float.of_int count) *. avgalpha)
                             )
                          )

        let average t = Array.copy t.average_weights
        let get_weights t = t.weights
      end
    include OnlineMarginTrainer(Co)(E)(UpdateOneBestMira)(D)

    let name = "mira"
  end


module type OnlineTrainer =
  sig
    val train :
      train_filename:  string ->
      dev_filename:string  option ->
      test_filename: string option ->
      max_iter:int ->
      feature_threshold:int ->
      verbose:bool ->
      float array
    val name : string
  end


(* module TrainSelecter (C : ConllType) (D : Decoder with module C = C) = *)
(*   struct *)


(*     let perceptron = (module PerceptronTrainer(C)(D) : OnlineTrainer) *)
(*     let mira = (module MiraTrainer(C)(D) : OnlineTrainer) *)

(*     let known_trainers = [ perceptron; mira;] *)


(*     let create_known_table () = *)
(*       let known  = String.Table.create () in *)
(*       List.iter known_trainers *)
(*                 ~f:(fun ((module Q : OnlineTrainer) as q) -> *)
(*                     Hashtbl.replace known ~key:Q.name ~data:q); *)
(*       known *)

(*   end *)