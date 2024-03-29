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
open Eval
open Decoder
open Feature

module type Updater =
sig
    module C : ConllType
    type t
    val create : random_init:bool -> total:int -> size:int -> t
    val update :  t -> int -> int -> int -> C.t array ->  C.t array  -> unit
    val weights : t -> float array
    val average : t -> float array

    val init_iteration : t -> unit
    val finish_iteration : t -> bool -> unit
  end

module OnlineMarginTrainer
  (C : ConllType)
  (E : Eval with module C = C)
  (U : Updater with module C = E.C)
  (D : Decoder with module C = U.C) =
struct

    (* a triplet of functions *)
    (* first one prints score info : will be called after each training example *)
    (* second one resets eval variables : will be called before each pass *)
    (* third one returns final score *)
  let eval_task verbose () =

    let print_count = ref 0 in
    let print_cond () = if verbose then true
      else (print_count := !print_count + 1; (* false) *) (!print_count mod 100) = 0)
    in
      let eval = E.empty in
      let update_stats ref_sentence hyp_sentence =
        E.update eval ~ref_sentence ~hyp_sentence;
        if print_cond () then fprintf Out_channel.stdout "%s\r%!" (E.to_string eval)
      in
      let reset_stats () = E.reset eval in
      let final_score () =  fprintf Out_channel.stdout "%s\n%!" (E.to_string eval); E.to_score eval |> fst in
      (update_stats, reset_stats, final_score)

    let train_and_eval feature_weights fun_decode fun_ref_decode fun_update
        (fun_eval, fun_reset_eval, final_score) corpus =
      let my_decoder = fun_decode feature_weights in
      let ref_decoder = fun_ref_decode feature_weights in

      fun_reset_eval ();
      List.iteri corpus
        ~f:(fun i s ->
          (* let () = Printf.printf "sentence: %d\n%!" i in *)
          let ref_a = C.prepare_sentence_for_decoder s in
          let hyp_a = my_decoder ref_a in
          let ref_a = ref_decoder ref_a in
          fun_eval ref_a hyp_a;
          fun_update i ref_a hyp_a
        );
      final_score ()


    let train_epoch ~decode_func ~update_func ~feature_weights ~corpus ~verbose =
      train_and_eval feature_weights (decode_func) (D.forced_decode) update_func (eval_task verbose ()) corpus

    let dont_update_model = fun _ _ _ -> ()

    let eval_epoch ~feature_weights ~corpus ~verbose =
      train_and_eval feature_weights (D.decode) (D.forced_decode) dont_update_model (eval_task verbose ()) corpus

    let eval_print_epoch ~filename ~feature_weights ~corpus ~verbose =
      let oc = Out_channel.create filename in
      let (update_stats, reset_stats, final_score) = eval_task verbose ()
      in
      let update_stats_print sentence hypothesis =
        update_stats sentence hypothesis;
        Array.iter hypothesis
          ~f:(fun tok -> Printf.fprintf oc "%s\n%!" (C.to_string tok));
        Printf.fprintf oc "\n%!"
      in
      (* REMEMBER TO ALWAYS TYPE UNUSED RESULTS -> *)
      let (_unused_score : float) = train_and_eval feature_weights (D.decode) (D.forced_decode) dont_update_model (update_stats_print, reset_stats, final_score) corpus
      in
      Out_channel.close oc


    let train  ~train_filename ~dev_filename ~test_filename
        ~max_iter ~feature_threshold
        ~random_init ~restart_freq
        ~verbose =

      let filename_to_list x b = x |> C.do_read_file ~collect_word:b ~verbose |> C.corpus_to_list in
      let open Option.Monad_infix in
      let filename_to_list_opt x b = x >>= (fun df -> Some (filename_to_list df b)) in

      fprintf Out_channel.stdout "Reading files: \n%!";
      let train_instances = filename_to_list     train_filename    true   in
      let dev_instances   = filename_to_list_opt dev_filename      false in
      let test_instances  = filename_to_list_opt test_filename     false in

      (* collect features on corpus and filter out rare ones *)
      D.Feature.collect_features_on_corpus ~only_gold:true train_instances ~verbose;
      let size = D.Feature.prune_features feature_threshold in
      fprintf Out_channel.stdout "Nb features: %d\n%!" size;

      (* Initialize feature arrays *)
      let best_feature_vector = Array.create ~len:size 0.0 in
      let updater = U.create ~random_init ~total:(List.length train_instances) ~size in

      let rec main_loop train_instances best best_score epoch =
        if epoch >= max_iter then (best,best_score)
        else
          let () = fprintf Out_channel.stdout "\nIteration: %d\nTraining\n%!" epoch in
          let () = U.init_iteration updater in

          let (_ : float) = train_epoch ~verbose
              ~decode_func:
                (* D.decode *)
                (match epoch with
                 (* | n when n mod 40 = 10 -> D.constrained_decode *)
                 (* | n when n mod 40 = 11 -> D.constrained_decode *)
                 (* | n when n mod 20 = 4 -> D.constrained_decode *)
                 (* | n when n mod 20 < 2 -> D.constrained_decode *)
                 | _ -> D.decode
                )
                (* (if epoch < 5 then D.constrained_decode else D.decode) *)
              ~update_func:(U.update updater max_iter epoch)
              ~feature_weights:(U.weights updater)
              ~corpus:train_instances
          in


          let reinit_from_average = (epoch > 0) && (restart_freq > 0) && (epoch mod restart_freq = 0) in
          let () = U.finish_iteration updater reinit_from_average in
          let average = if reinit_from_average then Array.copy (U.weights updater) else U.average updater in

          let dev_score =
            match dev_instances with
            | None -> 0.0
            | Some dis ->
               fprintf Out_channel.stdout "\nDev:\n%!";
              eval_epoch ~feature_weights:average ~corpus:dis ~verbose
          in

          let train_instances = List.permute train_instances in
          (* made any progress ? *)
          if dev_score >= best_score
          then main_loop train_instances average dev_score  (epoch+1)
          else main_loop train_instances best    best_score (epoch+1)

      in
      let train_instances = List.permute train_instances in
      let final,final_score = main_loop train_instances best_feature_vector Float.neg_infinity 0
      in
      fprintf Out_channel.stdout "Best score on Dev: %f\n%!" final_score;
      let () =
        match test_instances with
        | None -> ()
        | Some tis ->
           fprintf Out_channel.stdout "\nTest:\n%!";
           eval_print_epoch ~filename:"test.results" (* TODO give a proper filename *)
             ~feature_weights:final
             ~corpus:tis
             ~verbose
           ;
           fprintf Out_channel.stdout "\n%!"
      in
      final
  end

module PerceptronTrainer(Co : ConllType) (E : Eval with module C = Co) (D : Decoder with module C = Co) =
  struct

    module UpdatePerceptron : Updater with module C = D.C =
    struct
      type t = {size: int;
                total: int;
                mutable counter: int;
                model: float Array.t;
                sum: float Array.t;
                iter_tmp: float Array.t;
               }

      module C = Co

      let create ~random_init ~total ~size =
        let w = Array.create ~len:size 0.0 in
        let () = if random_init
          then
            let rec init i =
              if i >= (size-1) then ()
              else
                begin
                  let s = Random.bool () in
                  let f = Random.float 1.0 in
                  let f = if s then f else (-.f) in
                  Array.unsafe_set w i f;
                  (* Array.unsafe_set w (i+1) (-.f); *)
                  init (i+1)
                end
            in init 0

          (*   begin *)
          (*     for i = 0 to (size/2) - 1 do *)
          (*       let f = Random.float 0.05 in *)
          (*       Array.unsafe_set w i f; *)
          (*       Array.unsafe_set w (size-i-1) (-.f) *)
          (*     done; *)
          (*     Array.permute w *)
          (*   end *)
          (* else () *)
        in
        { size = size;
          total = total;
          counter = 0;
          model = w;
          sum = Array.create ~len:size 0.0;
          iter_tmp = Array.create ~len:size 0.0;
        }

      let incr_examples t = t.counter <- t.counter +1

      (* compute perceptron update*)
      let update  t _max_iteration _iteration idx_sentence ref_sentence hyp_sentence =

        let htbl = Hashtbl.create ~hashable:Int.hashable () in

        let () = D.get_feature_differences htbl t.size ref_sentence hyp_sentence in

        (* update model *)
        incr_examples t;

        Hashtbl.iteri htbl
          ~f:(fun ~key:fi ~data:count ->
            if count <> 0
            then
              (
                t.model.(fi)    <- t.model.(fi)    +. (Float.of_int count);
                t.iter_tmp.(fi) <- t.iter_tmp.(fi) +. (Float.of_int ((t.total - idx_sentence) * count))
              )
          )

      let average t =
        Array.init t.size ~f:(fun i -> t.sum.(i) /. (Float.of_int t.counter))

      let weights t = t.model

      let init_iteration t =
        Array.iteri t.model
          ~f:(fun i w -> Array.unsafe_set t.iter_tmp i ((Float.of_int t.total) *. w))

      let finish_iteration t reinit_from_average =
        Array.iteri t.iter_tmp ~f:(fun i w -> Array.replace t.sum i ~f:(fun v -> v +. w));
        if reinit_from_average
        then
          Array.iteri t.sum ~f:(fun i w -> Array.unsafe_set t.model i (w /. (Float.of_int t.counter)))
        else
          ()
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
        let create ~random_init:_ ~total ~size = { size = size;
                                    counter = 0;
                                    examples = total;
                                    weights = Array.create ~len:size 0.0;
                                    average_weights = Array.create ~len:size 0.0;
                                    clip = Float.infinity;}

        let incr_examples t = t.counter <- t.counter +1

        let init_iteration _t = failwith "not implemented"
        let finish_iteration _t = failwith "not implemented"


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
          let () = loop_on_arcs 0 in

          (* update model *)
          incr_examples t;

          let norm = Hashtbl.fold htbl ~init:0 ~f:(fun ~key:_ ~data:c acc -> acc + c*c) |> Float.of_int in


          (* TODO:  PROBLEM the score difference is not computed correctly*)
          let (_,loss,diff_score) =
            Array.fold2_exn ref_sentence hyp_sentence
                            ~init:(0,0,0.0)
                            ~f:(fun (i,loss',diff_score) re hy ->
                                if C.same_prediction re  hy
                                then (i+1,loss',diff_score)
                                else
                                  let ds = D.Feature.compute_score_difference t.weights ref_sentence hyp_sentence i re hy
                                  in
                                  (i+1,loss'+1,diff_score +. ds)) in

          (* let () = *)
          (*   printf "diff_score (should be negative): %f\n%!" diff_score in *)
          let delta = ((Float.of_int loss) -. diff_score) /. norm in
          (* let () = if delta > 0.0 *)
          (*          then Printf.printf "positive\n%!" in *)
          let alpha = Float.max 0.0 delta |> Float.min t.clip in
          (* let alpha = 1. in *)
          (* double avgUpdate(double(loop * num_examples - (num_examples * ((iteration + 1) - 1) + (num )) + 1)); *)
          let avgalpha = max_iter * t.examples - (t.examples * (epoch-1) + num) + 1 |> Float.of_int |> ( *. ) alpha in

          Hashtbl.iteri htbl
                       ~f:(fun ~key:fi ~data:count ->
                           if count <> 0
                           then
                             (t.weights.(fi)         <- t.weights.(fi)         +. ((Float.of_int count) *. alpha) ;
                              t.average_weights.(fi) <- t.average_weights.(fi) +. ((Float.of_int count) *. avgalpha)
                             )
                          )

        let average t = Array.copy t.average_weights
        let weights t = t.weights

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
      random_init:bool ->
      restart_freq:int ->
      verbose:bool ->
      float array
    val name : string
  end


module TrainSelecter (C : ConllType) (E : Eval with module C = C) (D : Decoder with module C = C) =
  struct


    let perceptron = (module PerceptronTrainer(C)(E)(D) : OnlineTrainer)
    let mira = (module MiraTrainer(C)(E)(D) : OnlineTrainer)

    let known_trainers = [ perceptron; mira;]


    let create_known_table () =
      let known  = String.Table.create () in
      List.iter known_trainers
                ~f:(fun ((module Q : OnlineTrainer) as q) ->
                    Hashtbl.set known ~key:Q.name ~data:q);
      known

  end
