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

open Modeltag
open Sequencedecoder
open Conlltag
open Evaltag

module TS = TrainSelecter(Conll_Tag)(Eval_Tag(Conll_Tag))(Sequence_Decoder)

let train =

  let trainer_table = TS.create_known_table () in

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
      +> flag "-r" (no_arg) ~doc: " Random initialization of weight vector"
      +> flag "-g" (optional_with_default (-1) int) ~doc: "int Restart from average frequency (negative is never)"
      +> flag "-v" (no_arg) ~doc: " Verbose mode"
    )
    (fun train_filename dev_filename test_filename
      max_iter feature_threshold model algo
      random_init restart_freq
      verbose () ->

        let (module Trainer : OnlineTrainer) =
          match Hashtbl.find trainer_table algo with
          | None -> failwith "unknown training algorithm"
          | Some x -> x
        in

        if verbose then printf "verbose mode\n%!" else printf "not verbose mode\n%!";

        Trainer.train ~train_filename ~dev_filename ~test_filename
          ~max_iter ~feature_threshold ~random_init ~restart_freq
          ~verbose
        |> Model.make
        |> Model.save ~filename:model

    )

let predict =
  Command.basic
    ~summary:"Run structured perceptron tagger"
    Command.Spec.
  (
    empty
    +> flag "-m" (required file) ~doc: "filename Model file"
    +> flag "-f" (required file) ~doc: "filename Input file  (CONLL format)"
    +> flag "-o" (required file) ~doc: "filename Output file (CONLL format)"
    +> flag "-v" (no_arg) ~doc: " Verbose mode"
  )
    (fun model_fname input_fname output_fname  verbose () ->

     let feature_weights = Model.load model_fname |> Model.get_data in
     let corpus = Conll_Tag.do_read_file input_fname in

     Sequence_Decoder.decode_corpus
       ~filename:output_fname ~feature_weights ~corpus
       ~verbose ~pruner:([||],[])
    )

let command =
  Command.group
    ~summary:"A vanilla 1st order dependency parser"
    ~readme:(fun () -> "More detailed information")
    [("train",train); ("predict",predict)]

let () = Command.run
  ~version:"0.0.1" ~build_info:"JLR"
  command
