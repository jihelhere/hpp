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
module Regex = Re2.Regex


module Conll_Tag =
struct

  type t =
    {
      idx        : int;
      form       : int;
      pos        : int;
      latpos     : int;
      prefix     : int;
      suffix     : int;
    }

  let same_prediction t1 t2 = (t1.pos = t2.pos)
  let same_fine_prediction t1 t2 = (t1.pos = t2.pos) && (t1.latpos = t2.latpos)
  let prediction t = t.pos
  let latent_prediction t = t.latpos


  let set_prediction t i = {t with pos=i}
  let set_latentprediction t i l = {t with pos=i;latpos=l}

  let form_map = Int2StringMap.empty ()
  let pos_map = Int2StringMap.empty ()


  (* TODO: command line argument to set this *)
  let prefix_length = 1
  let suffix_length = 3

  let prefix_map = Int2StringMap.empty ()
  let suffix_map = Int2StringMap.empty ()

  let get_number_form () = Int2StringMap.get_size form_map
  let get_number_pos () = Int2StringMap.get_size pos_map

  let get_string_prefix string = String.prefix string prefix_length
  let get_string_suffix string = String.prefix string suffix_length


  let line_to_conll_token line =
    let lt = Regex.split (Regex.create_exn "\t") line in
    let get_field n = match (List.nth lt n) with
      | None -> failwith "missing field"
      | (Some v) -> v
    in
    {
      idx        = int_of_string (get_field 0);
      form       = Int2StringMap.str2int form_map       (get_field 1);
      pos        = Int2StringMap.str2int pos_map (get_field 2);
      latpos     = -1;
      prefix = Int2StringMap.str2int prefix_map (get_string_prefix (get_field 1));
      suffix = Int2StringMap.str2int suffix_map (get_string_suffix (get_field 1));
    }


  let start = {idx = 0;
              form = Int2StringMap.str2int form_map "__START__";
              pos = Int2StringMap.str2int pos_map "__START__";
              latpos = -1;
              prefix = Int2StringMap.str2int prefix_map (get_string_prefix "__START__");
              suffix = Int2StringMap.str2int suffix_map (get_string_suffix "__START__");
              }

  let stop = {idx = 0;
               form = Int2StringMap.str2int form_map "__STOP__";
               pos = Int2StringMap.str2int pos_map "__STOP__";
               latpos = -1;
               prefix = Int2StringMap.str2int prefix_map (get_string_prefix "__STOP__");
               suffix = Int2StringMap.str2int suffix_map (get_string_suffix "__STOP__");
              }

  let digit = {idx = 0;
               form = Int2StringMap.str2int form_map "__DIGIT__";
               pos = Int2StringMap.str2int pos_map "__DIGIT__";
               latpos = -1;
               prefix = Int2StringMap.str2int prefix_map (get_string_prefix "__DIGIT__");
               suffix = Int2StringMap.str2int suffix_map (get_string_suffix "__DIGIT__");
              }


  let get_idx t = t.idx
  let get_form t = Int2StringMap.int2str form_map t.form
  let get_pos t = Int2StringMap.int2str pos_map t.pos

  let get_form_id t = t.form
  let get_pos_id t = t.pos

  let get_prefix_id t = t.suffix
  let get_suffix_id t = t.suffix

  let get_form_id t = t.form
  let get_pos_id t = t.pos
  let get_prefix_id t = t.prefix
  let get_suffix_id t = t.suffix

  let get_number_form () = Int2StringMap.get_size form_map
  let get_number_pos () = Int2StringMap.get_size pos_map

  let to_string t =
    Printf.sprintf "%d\t%s\t%s"
      (get_idx t)
      (get_form t)
      (get_pos t)

  let is_digit t =
    let first = (get_form t).[0] in
    first >= '0' && first <= '9'



  type sentence = t list
  type corpus = sentence list

  let corpus_to_list corpus = corpus
  let list_to_corpus corpus = corpus

  let sentence_to_list sentence = sentence
  let list_to_sentence sentence = sentence


  let prepare_sentence_for_decoder sentence =
    Array.of_list sentence


  let load_map_from_sexp sexp =
    match sexp with
    | Sexp.List ([Sexp.Atom name; map]) ->
       (match name with
        | "form_map"         -> Int2StringMap.load_from_sexp form_map map
        | "pos_map"          -> Int2StringMap.load_from_sexp pos_map map
        | "prefix_map"       -> Int2StringMap.load_from_sexp prefix_map map
        | "suffix_map"       -> Int2StringMap.load_from_sexp suffix_map map
        | _ -> assert(false)
       )
    | _ -> assert(false)


  let do_write_file corpus file =
   Out_channel.with_file file
                         ~f: (fun oc ->
                              List.iter corpus
                                        ~f:(fun sent ->
                                            List.iter
                                              sent ~f:(fun t -> Printf.fprintf oc "%s\n" (to_string t))
                                            ; Printf.fprintf oc "\n"
                                           ); Out_channel.close oc
                             )


  let all_string_tables_to_sexp () =
    let a x = Sexp.Atom x and l x = Sexp.List x in
    l [ a "corpus";
        l [ a "form_map" ; Int2StringMap.sexp_of_t form_map];
        l [ a "pos_map" ; Int2StringMap.sexp_of_t pos_map];
        l [ a "prefix_map" ; Int2StringMap.sexp_of_t prefix_map];
        l [ a "suffix_map" ; Int2StringMap.sexp_of_t suffix_map]
      ]

  let save_all_string_tables_to_file filename =
    all_string_tables_to_sexp ()
    |> Sexp.save_hum filename


  let do_read_file_unordered file =
    let i = ref 0 in
    fst (In_channel.with_file file ~f:
                              (fun ic ->
                               In_channel.fold_lines ic ~init:([],[])
                                                     ~f:(fun (acc_corpus,acc_sent) line ->
                                                         if line = ""
                                                         then
                                                           let _ = i := !i + 1 in
                                                           let _ = Printf.printf "Loading %d sentences\r%!" !i in
                                                           (acc_sent::acc_corpus, [])
                                                         else (acc_corpus, (line_to_conll_token line)::acc_sent))
                              )
        )

  (* let do_read_ordered file = *)
  let do_read_file file  =
    let i = ref 0 in
    List.rev
      (fst (In_channel.with_file file ~f:
                                 (fun ic ->
                                  In_channel.fold_lines ic ~init:([],[])
                                                        ~f:(fun (acc_corpus,acc_sent) line ->
                                                            if line = ""
                                                            then
                                                              let _ = i := !i + 1 in
                                                              let _ = Printf.printf "Loading %d sentences\r%!" !i in
                                                              ((List.rev acc_sent)::acc_corpus, [])
                                                            else (acc_corpus, (line_to_conll_token line)::acc_sent))
                                 )
           )
      )




end
