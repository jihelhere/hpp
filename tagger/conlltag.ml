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
      orig_form     : string;
      form          : int;
      pos           : int;
      latpos        : int;
      prefix        : int;
      suffix        : int;
      has_digit     : bool;
      has_uppercase : bool;
      all_uppercase : bool;
      has_hyphen    : bool;
      prefix_list   : int list;
      suffix_list   : int list;
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
  let prefix_length = 10
  let suffix_length = 10

  let prefix_map = Int2StringMap.empty ()
  let suffix_map = Int2StringMap.empty ()

  let get_number_form () = Int2StringMap.get_size form_map
  (* let get_number_pos () = Int2StringMap.get_size pos_map *)

  let get_string_prefix string = String.prefix string prefix_length
  let get_string_suffix string = String.prefix string suffix_length


  let p_has_hyphen t = String.contains t '-'
  let p_has_digit t =  String.exists t ~f:(fun c -> c >= '0' && c <= '9')
  let p_has_uppercase t = not (String.lowercase t = t)
  let p_all_uppercase t = (String.uppercase t = t)

  let p_extract_pref str =
    let rec aux str len acc =
      if len > prefix_length then acc
      else aux str (len+1) ((String.prefix str len)::acc)
    in
    List.rev(aux str 1 [])


  let p_extract_suf str =
    let rec aux str len acc =
      if len > suffix_length then acc
      else aux str (len+1) ((String.suffix str len)::acc)
    in
    List.rev(aux str 1 [])



  let start =
    let str = "__START__" in
    {
      orig_form = str;
      form = Int2StringMap.str2int form_map str;
      pos = Int2StringMap.str2int pos_map str;
      latpos = -1;
      prefix = Int2StringMap.str2int prefix_map (get_string_prefix str);
      suffix = Int2StringMap.str2int suffix_map (get_string_suffix str);
      has_digit = false;
      has_uppercase = false;
      all_uppercase = false;
      has_hyphen = false;
      prefix_list = List.map (p_extract_pref str) ~f:(fun s -> Int2StringMap.str2int prefix_map s);
      suffix_list = List.map (p_extract_suf str)  ~f:(fun s -> Int2StringMap.str2int suffix_map s);
    }

  let stop =
    let str = "__STOP__" in
    {
      orig_form = str;
      form = Int2StringMap.str2int form_map str;
      pos = Int2StringMap.str2int pos_map str;
      latpos = -1;
      prefix = Int2StringMap.str2int prefix_map (get_string_prefix str);
      suffix = Int2StringMap.str2int suffix_map (get_string_suffix str);
      has_digit = false;
      has_uppercase = false;
      all_uppercase = false;
      has_hyphen = false;
      prefix_list = List.map (p_extract_pref str) ~f:(fun s -> Int2StringMap.str2int prefix_map s);
      suffix_list = List.map (p_extract_suf str)  ~f:(fun s -> Int2StringMap.str2int suffix_map s);
    }

  let unknown =
    let str = "__UNK__" in
    {
      orig_form = str;
      form = Int2StringMap.str2int form_map str;
      pos = Int2StringMap.str2int pos_map str;
      latpos = -1;
      prefix = Int2StringMap.str2int prefix_map (get_string_prefix str);
      suffix = Int2StringMap.str2int suffix_map (get_string_suffix str);
      has_digit = false;
      has_uppercase = false;
      all_uppercase = false;
      has_hyphen = false;
      prefix_list = List.map (p_extract_pref str) ~f:(fun s -> Int2StringMap.str2int prefix_map s);
      suffix_list = List.map (p_extract_suf str)  ~f:(fun s -> Int2StringMap.str2int suffix_map s);
    }


  let separator_regex = Regex.create_exn "\t"
  let word_freq = Hashtbl.create ~hashable:String.hashable ()
  let pruner = ref   (Array.empty ())
  let pos_list = ref (Array.empty ())

  let line_to_conll_token line =
    let lt = Regex.split separator_regex line in
    (* let lt = String.split line ~on:'\t' in *)
    let get_field n = match (List.nth lt n) with
      | None -> failwith "missing field"
      | (Some v) -> v
    in
    let form = get_field 1 in
    let form_id = match Hashtbl.find word_freq form with
      | Some x when x > 1 -> Int2StringMap.str2int form_map  (String.lowercase form)
      | _ -> unknown.form in
    {
      orig_form = form;
      form       =  form_id;
      pos        = Int2StringMap.str2int pos_map (get_field 2);
      latpos     = -1;
      prefix = Int2StringMap.str2int prefix_map (get_string_prefix form);
      suffix = Int2StringMap.str2int suffix_map (get_string_suffix form);
      has_digit = p_has_digit form;
      has_uppercase = p_has_uppercase form;
      all_uppercase = p_all_uppercase form;
      has_hyphen = p_has_hyphen form;
      prefix_list = List.map (p_extract_pref form) ~f:(fun s -> Int2StringMap.str2int prefix_map s);
      suffix_list = List.map (p_extract_suf form)  ~f:(fun s -> Int2StringMap.str2int suffix_map s);
    }





  let get_idx _t = 0
  let get_form t = Int2StringMap.int2str form_map t.form
  let get_pos t = Int2StringMap.int2str pos_map t.pos

  (* let get_form_id t = t.form *)
  (* let get_pos_id t = t.pos *)

  (* let get_prefix_id t = t.prefix *)
  (* let get_suffix_id t = t.suffix *)

  let get_form_id t = t.form
  let get_pos_id t = t.pos
  let get_prefix_id t = t.prefix
  let get_suffix_id t = t.suffix

  let get_prefix_list t = t.prefix_list
  let get_suffix_list t = t.suffix_list



  let get_number_form () = Int2StringMap.get_size form_map
  let get_number_pos () = Int2StringMap.get_size pos_map

  let to_string t = Printf.sprintf "%s\t%s" t.orig_form (get_pos t)


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
    let read_hash map s =
      let rec add_pairs l =
        match l with
        | [] -> ()
        | Sexp.List([Sexp.Atom h1;Sexp.Atom h2])::q ->
             let _ = Hashtbl.add map ~key:h1 ~data:(Int.of_string h2) in
             add_pairs q
        | _ -> failwith "add_pairs"
      in
      match s with
    | Sexp.List(q) -> add_pairs q
    | _ -> failwith "t_of_sexp"
    in

    match sexp with
    | Sexp.List ([Sexp.Atom name; map]) ->
       (match name with
        | "form_map"     -> Int2StringMap.load_from_sexp form_map map
        | "pos_map"      -> Int2StringMap.load_from_sexp pos_map map
        | "prefix_map"   -> Int2StringMap.load_from_sexp prefix_map map
        | "suffix_map"   -> Int2StringMap.load_from_sexp suffix_map map
        | "word_freq"    -> read_hash word_freq map
        | "pruner"       -> pruner := Array.t_of_sexp (Array.t_of_sexp Int.t_of_sexp) map
        | "pos_list"     -> pos_list :=  Array.t_of_sexp Int.t_of_sexp map
        | _ -> assert(false)
       )
    | _ -> assert(false)


  let do_write_file corpus file =
   Out_channel.with_file file
                         ~f: (fun oc ->
                              List.iter corpus
                                        ~f:(fun sent ->
                                            List.iteri
                                              sent ~f:(fun i t -> Printf.fprintf oc "%d\t%s\n" (i+1) (to_string t))
                                            ; Printf.fprintf oc "\n"
                                           ); Out_channel.close oc
                             )


  let all_string_tables_to_sexp () =
    let a x = Sexp.Atom x and l x = Sexp.List x in
    l [ a "corpus";
        l [ a "form_map"   ; Int2StringMap.sexp_of_t form_map];
        l [ a "pos_map"    ; Int2StringMap.sexp_of_t pos_map];
        l [ a "prefix_map" ; Int2StringMap.sexp_of_t prefix_map];
        l [ a "suffix_map" ; Int2StringMap.sexp_of_t suffix_map];
        l [ a "word_freq"  ; Hashtbl.sexp_of_t String.sexp_of_t Int.sexp_of_t word_freq];
        l [ a "pruner"     ; Array.sexp_of_t (fun l -> Array.sexp_of_t Int.sexp_of_t l) !pruner];
        l [ a "pos_list"   ; Array.sexp_of_t Int.sexp_of_t !pos_list]
      ]

  let save_all_string_tables_to_file filename =
    all_string_tables_to_sexp ()
    |> Sexp.save_hum filename

  (* let do_read_file_unordered file = *)
  (*   (\* let i = ref 0 in *\) *)
  (*   fst (In_channel.with_file file ~f: *)
  (*                             (fun ic -> *)
  (*                              In_channel.fold_lines ic ~init:([],[]) *)
  (*                                                    ~f:(fun (acc_corpus,acc_sent) line -> *)
  (*                                                        if line = "" *)
  (*                                                        then *)
  (*                                                          (\* let _ = i := !i + 1 in *\) *)
  (*                                                          (\* let _ = Printf.printf "Loading %d sentences\r%!" !i in *\) *)
  (*                                                          (acc_sent::acc_corpus, []) *)
  (*                                                        else (acc_corpus, (line_to_conll_token line)::acc_sent)) *)
  (*                             ) *)
  (*       ) *)


  let collect_word_freq filename =
    In_channel.with_file filename
      ~f:(fun ic ->
        In_channel.iter_lines ic
          ~f:(fun line ->
            if line = ""  then ()
            else
               match Regex.split separator_regex line with
               | [_;form;_] -> Hashtbl.change word_freq  form (function | None -> Some 1 | Some x -> Some(x+1))
               | _ -> failwith "??"

          )
      )

        let p_collect_word_tags sentences =
    let a = Array.init (get_number_form ()) ~f:(fun _ -> []) in
    let () = List.iter sentences
      ~f:(fun sentence ->
        List.iter sentence
          ~f:(fun tok ->
            let l = Array.unsafe_get a tok.form in
            if (List.exists l ~f:((=) tok.pos))
            then
              ()
            else
              Array.unsafe_set a tok.form (tok.pos::l)

          )
      )
    in
    pruner :=  Array.map a ~f:(fun l -> List.sort l ~cmp:Int.compare |> Array.of_list)

  let collect_word_tags () = !pruner


  let p_collect_unk_tags () =
    let i = get_number_pos () in
    let rec incr i acc =
      if i < 0 then acc
      else incr (i-1) (i::acc)
    in
    let l = incr (i-1) [] in
    (* List.iter l ~f:(fun e -> printf "%d " e);printf "\n%!"; *)
    pos_list := Array.of_list l


  let collect_unk_tags () = !pos_list


  (* let do_read_ordered file = *)
  let do_read_file file  ~collect_word =
    let () = if collect_word
      then collect_word_freq file
    in
    let i = ref 0 in
    let sentences =
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
    in
    if collect_word
    then
      begin
        p_collect_word_tags sentences;
        p_collect_unk_tags ()
      end;
  sentences





  let has_hyphen t = t.has_hyphen
  let has_digit t =  t.has_digit
  let has_uppercase t = t.has_uppercase
  let all_uppercase t = t.all_uppercase

end
