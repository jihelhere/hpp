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
open Templatetag

module TS = TrainSelecter(Conll_Tag)(Eval_Tag)(Sequence_Decoder)

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
      +> flag "-a" (optional_with_default "perceptron" string) ~doc: "string Trainer name : \"perceptron\" [default] or \"mira\""
      +> flag "-r" (no_arg) ~doc: " Random initialization of weight vector"
      +> flag "-g" (optional_with_default (-1) int) ~doc: "int Restart from average frequency (negative is never)"
      +> flag "-h" (optional_with_default (1) int) ~doc: "int Number of latent variables per label"
      (* +> flag "-l" (optional_with_default true bool) ~doc: "bool left to right reading [default]" *)
      +> flag "-v" (no_arg) ~doc: " Verbose mode"
    )
    (fun train_filename dev_filename test_filename
      max_iter feature_threshold model algo
      random_init restart_freq nb_hidden_vars
      (* left_to_right *)
      verbose () ->

        let (module Trainer : OnlineTrainer) =
          match Hashtbl.find trainer_table algo with
          | None -> failwith "unknown training algorithm"
          | Some x -> x
        in

        let () = Template_Tag.nb_hidden_vars := nb_hidden_vars in

        Trainer.train ~train_filename ~dev_filename ~test_filename
          ~max_iter ~feature_threshold ~random_init ~restart_freq
          ~verbose
        |> (fun x -> Model.make x !Template_Tag.nb_hidden_vars)
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
    +> flag "-e" (no_arg) ~doc: " Evaluation mode"
    +> flag "-v" (no_arg) ~doc: " Verbose mode"
  )
    (fun model_fname input_fname output_fname  evaluation verbose () ->

     let feature_weights,nb_hidden_vars = Model.load model_fname |> Model.get_data in
     let corpus = Conll_Tag.do_read_file input_fname ~collect_word:false ~verbose in

     let () = Template_Tag.nb_hidden_vars := nb_hidden_vars in

     Sequence_Decoder.decode_corpus
       ~filename:output_fname ~feature_weights ~corpus
       ~evaluation
       ~verbose
    )

let command =
  (* let c = Gc.get () in *)
  (* let () = fprintf Out_channel.stderr "minor heap size before : %d \n%!" c.minor_heap_size in *)
  (* let () = Gc.tune ~minor_heap_size:(262144 * 64) () in *)
  (* let c = Gc.get () in *)
  (* let () = fprintf Out_channel.stderr "minor heap size after : %d \n%!" c.minor_heap_size in *)
  (* let () = Gc.tune ~major_heap_increment:(1000448 * 32) () in *)
  Command.group
    ~summary:"A POS tagger based on latent structured perceptron"
    ~readme:(fun () -> "More detailed information")
    [("train",train); ("predict",predict)]

let () = Command.run
  ~version:"0.0.1" ~build_info:"JLR"
  command
