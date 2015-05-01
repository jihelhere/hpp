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



let process_file verbose filename unk_threshold =

  let nb_pos,tree_list =
  (if verbose then fprintf Out_channel.stderr "read treebank\n%!" ;
  read_treebank filename) |>
      (if verbose then fprintf Out_channel.stderr "mask rare tokens\n%!";
       Ptbtree.replace_rares_simple unk_threshold) |>
          (if verbose then fprintf Out_channel.stderr "encode strings\n%!";
           Ptbtree.convert_string_trees)
  in

  let () = if verbose then fprintf Out_channel.stderr "compute pcfg weights/priors\n%!"  in
  let priors, hgram = Rule.create_pcfg tree_list in
  (nb_pos,priors,hgram)
