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

open Lexer
open Lexing

open Ptbtree
open Rule
open Int2stringmap
open Sexp


module CKY = Grammar.MakeCKY(Grammar.HistCell(Grammar.CKYBackPointer))

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Ptbparser.treel Lexer.read lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    []
  | Ptbparser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)


let parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | [] -> []
  | l ->
     let () = fprintf Out_channel.stderr "Successful Parse: %d trees\n%!" (List.length l) in
     l


let loop filename =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  let l = parse_and_print lexbuf in
  let () = In_channel.close inx in
  l


let read_treebank filename =
  let tree_list = loop filename |> List.map ~f:Ptbtree.process_tree in
  tree_list

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


        let () = if verbose then fprintf Out_channel.stderr "read treebank\n%!"  in
        let l = read_treebank train_filename in
        (* let () = List.iter l ~f:(fun t -> printf "%s\n%!" (Ptbtree.to_string t)) in *)

        let () = if verbose then fprintf Out_channel.stderr "mask rare token\n%!"  in
        let l' = Ptbtree.replace_rares_simple 5 l in
        (* let () = List.iter l' ~f:(fun t -> printf "%s\n%!" (Ptbtree.to_string t)) in *)

        let () = if verbose then fprintf Out_channel.stderr "encode strings\n%!"  in
        let il = List.map ~f:Ptbtree.convert_string_ptb l' in
        (* let l'' = List.map ~f:Ptbtree.convert_int_ptb il in *)
        (* let () = List.iter l'' ~f:(fun t -> printf "%s\n%!"
           (Ptbtree.to_string t)) in *)

        let () = if verbose then fprintf Out_channel.stderr "compute pcfg weights/priors\n%!"  in
        let priors, hgram = Rule.create_pcfg il in

        let () = if verbose then fprintf Out_channel.stderr "save to file\n%!"  in
        let priors_sexp = Hashtbl.sexp_of_t (Int.sexp_of_t) (Float.sexp_of_t) priors in
        let hgram_sexp = Hashtbl.sexp_of_t (Rule.sexp_of_t) (Float.sexp_of_t) hgram in
        let sexp = Sexp.List [Int2StringMap.sexp_of_t Ptbtree.nt_map; Int2StringMap.sexp_of_t Ptbtree.w_map; priors_sexp; hgram_sexp] in
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

        let () = if verbose then fprintf Out_channel.stderr "loading model\n%!"  in
        let (nt_map, w_map, priors, rules) =
        match Sexp.load_sexp model with
        | Sexp.List([n;w;p;g]) -> (n,w,p,g)
        | _ -> failwith "unable to load model"
        in
        let () = Int2StringMap.load_from_sexp Ptbtree.nt_map nt_map in
        let () = Int2StringMap.load_from_sexp Ptbtree.w_map w_map in
        let priors = Rule.priors_of_sexp priors in
        let rules = Rule.gram_of_sexp rules in
        let grammar = Grammar.Cky_gram.initialize Ptbtree.nt_map Ptbtree.w_map rules priors in
        let () = CKY.parse_file Ptbtree.w_map grammar file in
        ()



    )



let command =
  Command.group
    ~summary:"HP parser"
    ~readme:(fun () -> "More detailed information")
    [("train",train); ("parse",parse)]

let () = Command.run
           ~version:"0.0.1" ~build_info:"JLR"
           command
