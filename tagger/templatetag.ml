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
open Sexp

open Conlltag


module Template_Tag =
  struct

    module C = Conll_Tag

    let nb_hidden_vars = ref 4

    module T = struct
      type t = int with sexp

      let compare = Int.compare

      let hash i = Int.hash i

                            (* let u32rot x k = *)
                            (*   Int.bit_or (Int.shift_left x k) (Int.shift_right x (32 - k)) *)

                            (* (\* let u32mix (a,b,c) = *\) *)
                            (* (\*   let a = a - c in *\) *)
                            (* (\*   let a = Int.bit_xor a (u32rot c 4) in *\) *)
                            (* (\*   let c = c + b in *\) *)

                            (* (\*   let b = b - a in *\) *)
                            (* (\*   let b = Int.bit_xor b (u32rot a 6) in *\) *)
                            (* (\*   let a = a + c in *\) *)

                            (* (\*   let c = c - b in *\) *)
                            (* (\*   let c = Int.bit_xor c (u32rot b 8) in *\) *)
                            (* (\*   let b = b + a in *\) *)

                            (* (\*   let a = a - c in *\) *)
                            (* (\*   let a = Int.bit_xor a (u32rot c 16) in *\) *)
                            (* (\*   let c = c + b in *\) *)

                            (* (\*   let b = b - a in *\) *)
                            (* (\*   let b = Int.bit_xor b (u32rot a 19) in *\) *)
                            (* (\*   let a = a + c in *\) *)

                            (* (\*   let c = c - b in *\) *)
                            (* (\*   let c = Int.bit_xor c (u32rot b 4) in *\) *)
                            (* (\*   let b = b + a in *\) *)

                            (* (\*   (a,b,c) *\) *)



                            (* let hash k = *)
                            (*   let a = 0x31415926 + (Int.bit_and k 0xffffffff) in *)
                            (*   let b = 0x27182818 + (Int.shift_right k 32) in *)
                            (*   let c = 0xdeadbeef in *)

                            (*   let c = Int.bit_xor c b in *)
                            (*   let c = c - (u32rot b 14) in *)

                            (*   let a = Int.bit_xor a c in *)
                            (*   let a = a - (u32rot c 11) in *)

                            (*   let b = Int.bit_xor b a in *)
                            (*   let b = b - (u32rot a 25) in *)

                            (*   let c = Int.bit_xor c b in *)
                            (*   let c = c - (u32rot b 16) in *)

                            (*   let a = Int.bit_xor a c in *)
                            (*   let a = a - (u32rot c 4) in *)

                            (*   let b = Int.bit_xor b a in *)
                            (*   let b = b - (u32rot a 14) in *)

                            (*   let c = Int.bit_xor c b in *)
                            (*   let c = c - (u32rot b 24) in *)

                            (*   c *)
                            (* let compare = Int.compare *)
    end
    include Comparable.Make(T)
    include Hashable.Make(T)

    type t = T.t with sexp

    type uni_template_type =
      | U_Bias (* bias *)
      | U_Word (* current word *)
      | U_Pref (* prefix *)
      | U_Suf (* suffix *)
      | U_PWord (* previous word *)
      | U_PSuf (* previous suffix *)
      | U_PPWord (* previous previous word *)
      | U_NWord (* next word *)
      | U_NSuf (* next suffix *)
      | U_NNWord (* next next word *)
      | U_Digit
      | U_Hyphen
      | U_Upper
      | U_AllUpper
      | U_Digit_Hyphen_Upper


    type bi_template_type =
      | B_Bias (* bias *)
      | B_Word (* current word *)
      | B_Pref (* prefix *)
      | B_Suf (* suffix *)

      (*not really bigram...*)
      | B_PPos (* previous pos alone *)
      | B_PPos_Word (* previous pos + word *)

    let incr_opt opt =
      match opt with
        None -> Some 1
      | Some x -> Some (x + 1)

    let invalid_tag  = 0xFF
    let invalid_word = 0xFFFF

    let of_template_type  tt =
      ((Obj.magic (Obj.repr tt)) : int)

    let ft tt tag1 =
      Int.shift_left (of_template_type tt) 56
      |> Int.bit_or tag1

    let ftt tt tag1 tag2 =
      Int.shift_left (of_template_type tt) 56
      |> Int.bit_or tag1
      |> Int.bit_or (Int.shift_left tag2 8)

    let fttt tt tag1 tag2 tag3 =
      Int.shift_left (of_template_type tt) 56
      |> Int.bit_or tag1
      |> Int.bit_or (Int.shift_left tag2 8)
      |> Int.bit_or (Int.shift_left tag3 16)

    let ftttt tt tag1 tag2 tag3 tag4 =
      Int.shift_left (of_template_type tt) 56
      |> Int.bit_or tag1
      |> Int.bit_or (Int.shift_left tag2 8)
      |> Int.bit_or (Int.shift_left tag3 16)
      |> Int.bit_or (Int.shift_left tag4 24)

    let fttttt tt tag1 tag2 tag3 tag4 tag5 =
      Int.shift_left (of_template_type tt) 56
      |> Int.bit_or tag1
      |> Int.bit_or (Int.shift_left tag2 8)
      |> Int.bit_or (Int.shift_left tag3 16)
      |> Int.bit_or (Int.shift_left tag4 24)
      |> Int.bit_or (Int.shift_left tag5 32)

    let ftttttt tt tag1 tag2 tag3 tag4 tag5 tag6 =
      Int.shift_left (of_template_type tt) 56
      |> Int.bit_or tag1
      |> Int.bit_or (Int.shift_left tag2 8)
      |> Int.bit_or (Int.shift_left tag3 16)
      |> Int.bit_or (Int.shift_left tag4 24)
      |> Int.bit_or (Int.shift_left tag5 32)
      |> Int.bit_or (Int.shift_left tag6 40)

    let fttttttt tt tag1 tag2 tag3 tag4 tag5 tag6 tag7 =
      Int.shift_left (of_template_type tt) 56
      |> Int.bit_or tag1
      |> Int.bit_or (Int.shift_left tag2 8)
      |> Int.bit_or (Int.shift_left tag3 16)
      |> Int.bit_or (Int.shift_left tag4 24)
      |> Int.bit_or (Int.shift_left tag5 32)
      |> Int.bit_or (Int.shift_left tag6 40)
      |> Int.bit_or (Int.shift_left tag7 48)

    (* assume less than 2^16 words *)

    (* let fw tt word1 = *)
    (*   Int.shift_left (of_template_type tt) 56 *)
    (*   |> Int.bit_or word1 *)

    (* let fww tt word1 word2 = *)
    (*   Int.shift_left (of_template_type tt) 56 *)
    (*   |> Int.bit_or word1 *)
    (*   |> Int.bit_or (Int.shift_left word2 16) *)

    (* let fwww tt word1 word2 word3 = *)
    (*   Int.shift_left (of_template_type tt) 56 *)
    (*   |> Int.bit_or word1 *)
    (*   |> Int.bit_or (Int.shift_left word2 16) *)
    (*   |> Int.bit_or (Int.shift_left word3 32) *)

    let fwt tt word1 tag1 =
      Int.shift_left (of_template_type tt) 56
      |> Int.bit_or word1
      |> Int.bit_or (Int.shift_left tag1 16)

    let fwtt tt word1 tag1 tag2 =
      Int.shift_left (of_template_type tt) 56
      |> Int.bit_or word1
      |> Int.bit_or (Int.shift_left tag1 16)
      |> Int.bit_or (Int.shift_left tag2 24)

    let fwttt tt word1 tag1 tag2 tag3 =
      Int.shift_left (of_template_type tt) 56
      |> Int.bit_or word1
      |> Int.bit_or (Int.shift_left tag1 16)
      |> Int.bit_or (Int.shift_left tag2 24)
      |> Int.bit_or (Int.shift_left tag3 32)

    (* let fwtttt tt word1 tag1 tag2 tag3 tag4 = *)
    (*   Int.shift_left (of_template_type tt) 56 *)
    (*   |> Int.bit_or word1 *)
    (*   |> Int.bit_or (Int.shift_left tag1 16) *)
    (*   |> Int.bit_or (Int.shift_left tag2 24) *)
    (*   |> Int.bit_or (Int.shift_left tag3 32) *)
    (*   |> Int.bit_or (Int.shift_left tag4 40) *)

    let fwwt tt word1 word2 tag1 =
      Int.shift_left (of_template_type tt) 56
      |> Int.bit_or word1
      |> Int.bit_or (Int.shift_left word2 16)
      |> Int.bit_or (Int.shift_left tag1 32)

    let fwwtt tt word1 word2 tag1 tag2 =
      Int.shift_left (of_template_type tt) 56
      |> Int.bit_or word1
      |> Int.bit_or (Int.shift_left word2 16)
      |> Int.bit_or (Int.shift_left tag1 32)
      |> Int.bit_or (Int.shift_left tag2 40)

    let fwwttt tt word1 word2 tag1 tag2 tag3 =
      Int.shift_left (of_template_type tt) 56
      |> Int.bit_or word1
      |> Int.bit_or (Int.shift_left word2 16)
      |> Int.bit_or (Int.shift_left tag1 32)
      |> Int.bit_or (Int.shift_left tag2 40)
      |> Int.bit_or (Int.shift_left tag3 48)

    (* let fwwwt tt word1 word2 word3 tag1 = *)
    (*   Int.shift_left (of_template_type tt) 56 *)
    (*   |> Int.bit_or word1 *)
    (*   |> Int.bit_or (Int.shift_left word2 16) *)
    (*   |> Int.bit_or (Int.shift_left word3 32) *)
    (*   |> Int.bit_or (Int.shift_left tag1 48) *)

    let delta = function
      | false -> 0
      | true -> 1

    let combine_pos_lat pos latvar =
      pos * !nb_hidden_vars + latvar

    let retrieve_pos comb =
      comb / !nb_hidden_vars

    let retrieve_latvar comb =
      comb mod !nb_hidden_vars



    let make_template_uni fun_score array_sentence i =
      (* let init = delta (i = 0) in *)
      let tok = Array.unsafe_get array_sentence i in
      let word = C.get_form_id tok in

      let pref_list = C.get_prefix_list tok in
      let suf_list =  C.get_suffix_list tok in

      let digit = C.has_digit tok in
      let hyphen= C.has_hyphen tok in
      let upper = i > 0 && C.has_uppercase tok in
      let all_uppercase = C.all_uppercase tok in
      let digit_hyphen_upper = digit && hyphen && upper in


      let ptok = if i = 0 then C.start else Array.unsafe_get array_sentence (i-1) in
      let pword = C.get_form_id ptok in

      let pptok = if i < 2 then C.start else Array.unsafe_get array_sentence (i-2) in
      let ppword = C.get_form_id pptok in

      let ntok = if i >= (Array.length array_sentence) - 1 then C.stop else Array.unsafe_get array_sentence (i+1) in
      let nword = C.get_form_id ntok in

      let nntok = if i >= (Array.length array_sentence) - 2 then C.stop else Array.unsafe_get array_sentence (i+2) in
      let nnword = C.get_form_id nntok in

      fun pos _lat ->
        (* let pos = combine_pos_lat pos _lat in *)
        0.0
        +. (fun_score (ft U_Bias pos))
        +. (fun_score (fwt U_Word word pos))
        +. (fun_score (fwt U_PWord pword pos))
        +. (fun_score (fwt U_PPWord ppword pos))
        +. (fun_score (fwt U_NWord nword pos))
        +. (fun_score (fwt U_NNWord nnword pos))
        +. (fun_score (ftt U_Digit (delta digit) pos))
        +. (fun_score (ftt U_Hyphen (delta hyphen) pos))
        +. (fun_score (ftt U_Upper (delta upper) pos))

        +. (fun_score (ftt U_AllUpper (delta all_uppercase) pos))
        +. (fun_score (ftt U_Digit_Hyphen_Upper (delta digit_hyphen_upper) pos))

        +. List.foldi pref_list ~init:0.0 ~f:(fun i acc p -> acc +. (fun_score (fwtt U_Pref p i pos)))
        +. List.foldi suf_list  ~init:0.0 ~f:(fun i acc p -> acc +. (fun_score (fwtt U_Suf  p i pos)))


    let make_template_bi fun_score _array_sentence _i ppos latvar_ppos pos latvar_pos =

      let ppos = combine_pos_lat ppos latvar_ppos in
      let pos  = combine_pos_lat pos latvar_pos in

      (* let init = delta (i = 0) in *)
      (* let tok = if i >= Array.length array_sentence then C.stop else Array.unsafe_get array_sentence i in *)
      (* let tok = if C.is_digit tok then C.digit else tok in *)
      (* let word = C.get_form_id tok in *)
      (* let pref = C.get_prefix_id tok in *)
      (* let suf = C.get_suffix_id tok in *)
      0.0
      +. (fun_score (ftt B_Bias ppos pos))
    (*   +. (fun_score (ft B_PPos ppos)) *)
    (*   +. (fun_score (fwt B_PPos_Word word ppos)) *)
    (*   +. (fun_score (fwtt B_Word word ppos pos)) *)
    (*   (\* +. (fun_score (fwtt B_Pref pref ppos pos)) *\) *)
    (* (\* +. (fun_score (fwtt B_Suf suf  ppos pos)) *\) *)

      (* +. (fun_score (fttt B_Bias ppos pos init)) *)
      (* +. (fun_score (ftt B_PPos ppos init)) *)
      (* +. (fun_score (fwtt B_PPos_Word word ppos init)) *)
      (* +. (fun_score (fwttt B_Word word ppos pos init)) *)
      (* +. (fun_score (fwttt B_Pref pref ppos pos init)) *)
      (* +. (fun_score (fwttt B_Suf suf  ppos pos init)) *)

    let of_int t = t

    let table_collect_templates = Table.create ()

    let collect_template t  =
      Hashtbl.change table_collect_templates t incr_opt


    let reset_table_collect_template () =
      Hashtbl.clear table_collect_templates


    let collect_templates ~only_gold sentence =
      let local_table_collect_templates = Table.create () in
      let ct t =
        Hashtbl.change local_table_collect_templates t incr_opt;
        0.0
      in
      (if only_gold
       then
          let start_pos = C.prediction C.start in
          (for i = 0 to (Array.length sentence) - 1 do
              let pos = C.prediction (Array.unsafe_get sentence i) in
              let ppos = if i = 0 then start_pos else C.prediction (Array.unsafe_get sentence (i-1)) in
              for lv = 0 to !nb_hidden_vars - 1 do
                let (_: float) = make_template_uni ct sentence i pos lv in
                for plv = 0 to !nb_hidden_vars - 1 do
                  let (_: float) = make_template_bi ct sentence i ppos plv pos lv in
                  ()
                done
              done;
              ()
           done);
          let stop_pos = C.prediction C.stop in
          let last = Array.length sentence - 1 in
          let last_pos = C.prediction (Array.unsafe_get sentence last) in
          for lv = 0 to !nb_hidden_vars -1 do
            for plv = 0 to !nb_hidden_vars -1 do
              let (_: float) = make_template_bi ct sentence (last+1)  last_pos plv stop_pos lv in
              ()
            done;
          done;
          ()
       else
          failwith "Not implemented yet"
      )
      ;
      Hashtbl.iter local_table_collect_templates
        ~f:(fun ~key:k ~data:_ -> collect_template k)

    (* (\* These 2 functions should be factorized at some point *\) *)
    (* let compute_score  weight_vector template_to_index_fun = *)
    (*   let max_size = Array.length weight_vector in *)
    (*   let fun_score t = *)
    (*     let idx = template_to_index_fun t in *)
    (*     if idx >= 0 && idx < max_size *)
    (*     then Array.unsafe_get weight_vector idx *)
    (*     else 0.0 *)
    (*   in *)
    (*   fun sent i ppos pos -> *)
    (*     (make_template_uni fun_score sent i pos) +. *)
    (*       (if ppos = (-1) then 0.0 else make_template_bi fun_score sent i ppos pos) *)
    (*     +. (if i = (Array.length sent) -1 *)
    (*       then make_template_bi fun_score sent (i+1) pos (C.prediction C.stop) *)
    (*       else 0.0 *)
    (*     ) *)

    (* let fill_hash_table  htbl is_valid template_to_index_fun oper = *)
    (*   let fun_score t = *)
    (*     let idx = (template_to_index_fun t) in *)
    (*     let () = (if is_valid idx then  Hashtbl.change htbl (template_to_index_fun t) oper) *)
    (*     in 0.0 *)
    (*   in *)
    (*   fun sent i ppos pos -> *)
    (*     let _:float = (make_template_uni fun_score sent i pos) in *)
    (*     if ppos = (-1) then 0.0 *)
  (*     else (make_template_bi fun_score sent i ppos pos) *)

    let fill_hash_table_uni_part  htbl is_valid template_to_index_fun oper =
      let fun_score t =
        let idx = (template_to_index_fun t) in
        let () = (if is_valid idx then  Hashtbl.change htbl (template_to_index_fun t) oper)
        in 0.0
      in
      fun sent i pos lat ->
        let _:float = (make_template_uni fun_score sent i pos lat) in ()

    let fill_hash_table_bi_part  htbl is_valid template_to_index_fun oper =
      let fun_score t =
        let idx = (template_to_index_fun t) in
        let () = (if is_valid idx then  Hashtbl.change htbl (template_to_index_fun t) oper)
        in 0.0
      in
      fun sent i ppos plat pos lat ->
        let _:float = (make_template_bi fun_score sent i ppos plat pos lat) in ()
  end
