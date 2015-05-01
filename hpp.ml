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

open Ptbtree
open Treebank

open Rule
open Int2stringmap
open Sexp

module CKY = Grammar.MakeCKY(Grammar.BaseCell)


let train =
  Command.basic
    ~summary:"Train and eval hpp"
    Command.Spec.
    (
      empty
      +> flag "-t" (required file) ~doc: "filename Training set    (PTBformat)"
      (* +> flag "-d" (optional file) ~doc: "filename Development set (PTB format)" *)
      (* +> flag "-f" (optional file) ~doc: "filename Test set        (PTB format)" *)
      (* +> flag "-i" (optional_with_default 10 int)  ~doc: "int Iterations" *)
      (* +> flag "-n" (optional_with_default 5 int)  ~doc: "int Feature threshold" *)
      +> flag "-m" (required file) ~doc: "filename Output model name"
      (* +> flag "-a" (optional_with_default "mira" string) ~doc: "string Trainer name : \"perceptron\" or \"mira\" [default]" *)
      +> flag "-v" (no_arg) ~doc: " Verbose mode"
    )
    (fun train_filename
      (* dev_filename test_filename *)
      (* max_iter feature_threshold *)
      model
       (* algo *)
      verbose
      () ->

        let _nb_pos,priors, hgram = Treebank.process_file verbose train_filename 5 in
        let () = if verbose then fprintf Out_channel.stderr "save to file\n%!"  in
        let priors_sexp = Hashtbl.sexp_of_t (Int.sexp_of_t) (Float.sexp_of_t) priors in
        let hgram_sexp = Hashtbl.sexp_of_t (Rule.sexp_of_t) (Float.sexp_of_t) hgram in
        let sexp = Sexp.List
          [Int2StringMap.sexp_of_t Ptbtree.nt_map; Int2StringMap.sexp_of_t Ptbtree.w_map; priors_sexp; hgram_sexp] in
        Sexp.save_hum model sexp
    )

let parse =
  Command.basic
    ~summary:"Parse with hpp"
    Command.Spec.
    (
      empty
      +> flag "-f" (required file) ~doc: "filename Test set (tok format)" (* TODO:  make it optional and able to read stdin *)
      +> flag "-m" (required file) ~doc: "filename Model name"
      +> flag "-v" (no_arg) ~doc: " Verbose mode"
    )
    (fun file
      model
      verbose
      () ->

        let grammar = Ckygram.from_model_file verbose model in
        let () = CKY.parse_file grammar file in
        ()
    )



let command =
      (* let c = Gc.get () in *)
      (* let () = printf "minor heap size before : %d \n%!" c.minor_heap_size in *)
      (* let () = Gc.tune ~minor_heap_size:(262144 * 32) () in *)
      (* let c = Gc.get () in *)
      (* let () = printf "minor heap size after : %d \n%!" c.minor_heap_size in *)
      (* let () = Gc.tune ~major_heap_increment:(1000448 * 8) () in *)
  Command.group
    ~summary:"HP parser"
    ~readme:(fun () -> "More detailed information")
    [("train",train); ("parse",parse)]

let () = Command.run
  ~version:"0.0.1" ~build_info:"JLR"
  command
