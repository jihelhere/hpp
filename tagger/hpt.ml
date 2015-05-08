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

open Perceptrontrainer
open Templatetag
open Featuretag

module Sequence =
struct

  module C = Conll_Tag
  module T = Template_Tag
  module Feature = Feature(C)(Template_Tag)


type cell = {uni_score: float; bi_score:float}
type tier = cell array
type hypotheses = tier array

let build_hypotheses nb_labels sent =

  let n = Array.length sent in
  let hyp =
    Array.init n
      ~f:(fun _ ->
        Array.init nb_labels ~f:(fun _ -> {uni_score=0.0; bi_score=0.0})
      )
  in
  hyp



let decode params input = input
let compute_score_difference t1 t2 = 0.0

end


(* module TS = TrainSelecter(Conll_Tag)(Sequence) *)
module Trainer = PerceptronTrainer(Conll_Tag)(Sequence)



let train =

  (* let trainer_table = TS.create_known_table () in *)

  Command.basic
    ~summary:"Train and eval  structured perceptron tagger"
    Command.Spec.
  (
    empty
    +> flag "-t" (required file) ~doc: "filename Training set    (CONLL format)"
    +> flag "-d" (optional file) ~doc: "filename Development set (CONLL format)"
    +> flag "-f" (optional file) ~doc: "filename Test set        (CONLL format)"
    +> flag "-i" (optional_with_default 10 int)  ~doc: "int Iterations"
    +> flag "-n" (optional_with_default 5 int)  ~doc: "int Feature threshold"
    +> flag "-m" (required file) ~doc: "filename Output model name"
    +> flag "-a" (optional_with_default "mira" string) ~doc: "string Trainer name : \"perceptron\" or \"mira\" [default]"
    +> flag "-v" (no_arg) ~doc: " Verbose mode"
  )
    (fun train_filename dev_filename test_filename
         max_iter feature_threshold model algo verbose () ->

           (* let (module Trainer : OnlineTrainer) = *)
           (*   match Hashtbl.find trainer_table algo with *)
           (*   | None -> failwith "unknown training algorithm" *)
           (*   | Some x -> x *)
           (* in *)


           let m = Trainer.train ~train_filename ~dev_filename ~test_filename
             ~max_iter ~feature_threshold
             ~verbose

in ()

    (* |> Model.make label_prune_table distance_prune_table MyParser.name labeled *)
        (* |> Model.save ~filename:model *)

    )

(* let predict = *)
(*   Command.basic *)
(*     ~summary:"Parse with a 1st order dependency parser" *)
(*     Command.Spec. *)
(*   ( *)
(*     empty *)
(*     +> flag "-m" (required file) ~doc: "filename Model file" *)
(*     +> flag "-f" (required file) ~doc: "filename Input file  (CONLL format)" *)
(*     +> flag "-o" (required file) ~doc: "filename Output file (CONLL format)" *)
(*     +> flag "-v" (no_arg) ~doc: " Verbose mode" *)
(*   ) *)
(*     (fun model_fname input_fname output_fname  verbose () -> *)

(*      let (feature_weights,prune_pos, prune_distance, parser_name, labeled) = *)
(*        Model.load model_fname |> Model.get_data in *)
(*      let corpus = Conll.do_read_file input_fname |> Conll.corpus_to_list in *)

(*      let (module Parser : Parser) = *)
(*        match Hashtbl.find parser_selecter parser_name with *)
(*        | None -> failwith "unknown parsing algorithm" *)
(*        | Some x -> x *)
(*      in *)

(*      Parser.parse_corpus *)
(*        ~filename:output_fname ~feature_weights ~corpus *)
(*        ~verbose ~prune_pos ~prune_distance ~labeled *)
(*     ) *)

let command =
  Command.group
    ~summary:"A vanilla 1st order dependency parser"
    ~readme:(fun () -> "More detailed information")
    [("train",train)
    (* ; ("predict",predict) *)
    ]

let () = Command.run
           ~version:"0.0.1" ~build_info:"JLR"
           command
