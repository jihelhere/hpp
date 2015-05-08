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



    let max_distance = 0xFF
    let max_cross = 0xF

    let bin_distance dist =
      if dist > 40 then 6
      else if dist > 30 then 5
      else if dist > 20 then 4
      else if dist > 10 then 3
      else if dist > 5  then 2
      else if dist > 2  then 1
      else 0

    let between_verb  = 0
    let between_punc  = 1
    let between_coord = 2

    type dep_role =
      | Head
      | Modifier

    type template_type =
      | TW0              (* word *)
      | TWP1
      | TWP2
      | TWN1
      | TWN2
      (* | TWP01 *)
      (* | TWN01 *)
      (* | TWP012 *)
      (* | TWN012 *)
      | TL0              (* lemma *)
      | TLP1
      | TLP2
      | TLN1
      | TLN2
      (* | TLP01 *)
      (* | TLN01 *)
      (* | TLP012 *)
      (* | TLN012 *)
      | TT0              (* pos *)
      | TTP1
      | TTP2
      | TTN1
      | TTN2
      (* | TTP01 *)
      (* | TTN01 *)
      (* | TTP012 *)
      (* | TTN012 *)
      | TQ0              (* cpos *)
      | TQP1
      | TQP2
      | TQN1
      | TQN2
      | TQP01
      | TQN01
      | TQP012
      | TQN012
      | TP0              (* word + pos *)
      (* | TPP1 *)
      (* | TPP2 *)
      (* | TPN1 *)
      (* | TPN2 *)
      (* | TPP01 *)
      (* | TPN01 *)
      (* | TPP012 *)
      (* | TPN012 *)
      | TB0              (* word + cpos *)
      | TBP1
      | TBP2
      | TBN1
      | TBN2
      (* | TBP01 *)
      (* | TBN01 *)
      (* | TBP012 *)
      (* | TBN012 *)
      (* | TX0              (\* prefix *\) *)
      (* | TXP1 *)
      (* | TXP2 *)
      (* | TXN1 *)
      (* | TXN2 *)
      (* | TY0              (\* suffix *\) *)
      (* | TYP1 *)
      (* | TYP2 *)
      (* | TYN1 *)
      (* | TYN2 *)
      | HWMW             (* dep word *)
      (* | HWMW_HPW *)
      (* | HWMW_HNW *)
      (* | HWMW_MPW *)
      (* | HWMW_MNW *)
      (* | HLML             (\* dep lemma *\) *)
      (* | HLML_HPL *)
      (* | HLML_HNL *)
      (* | HLML_MPL *)
      (* | HLML_MNL *)
      | HTMT             (* dep pos *)
      | HTMT_HPT
      | HTMT_HNT
      | HTMT_MPT
      | HTMT_MNT
      | HTMT_NNTT
      | HTMT_PNTT
      | HTMT_NPTT
      | HTMT_PPTT
      | HTMT_PNTT_PNTT
      (* | HTMT_HPW *)
      (* | HTMT_HNW *)
      (* | HTMT_MPW *)
      (* | HTMT_MNW *)
      (* | HWMW_HNT *)
      (* | HWMW_HPT *)
      (* | HWMW_MNT *)
      (* | HWMW_MPT *)
      (* | HWMW_NNTT *)
      (* | HWMW_NPTT *)
      (* | HWMW_PNTT *)
      (* | HWMW_PPTT *)
      (* | HBMB_HNB *)
      (* | HBMB_MNB *)
      (* | HBMB_HPB *)
      (* | HBMB_MPB *)
      (* | HQMQ           (\* dep cpos *\) *)
      (* | HQMQ_HPQ *)
      (* | HQMQ_HNQ *)
      (* | HQMQ_MPQ *)
      (* | HQMQ_MNQ *)
      | HWMT           (* mix *)
      | HTMW
      (* | HTMW_HNT *)
      (* | HTMW_HPT *)
      (* | HTMW_MNT *)
      (* | HTMW_MPT *)
      (* | HTMW_NNTT *)
      (* | HTMW_NPTT *)
      (* | HTMW_PNTT *)
      (* | HTMW_PPTT *)
      (* | HWMT_HNT *)
      (* | HWMT_HPT *)
      (* | HWMT_MNT *)
      (* | HWMT_MPT *)
      (* | HWMT_NNTT *)
      (* | HWMT_NPTT *)
      (* | HWMT_PNTT *)
      (* | HWMT_PPTT *)
      | HTMB
      (* | HWMB *)
      | HBMT
      (* | HBMW *)
      | HBMB
      (* | HWMQ *)
      (* | HQMW *)
      (* | HQMR *)
      (* | HWMR *)
      (* | HRMQ *)
      (* | HRMW *)
      (* | HRMR *)
      (* | HTMX *)
      (* | HXMT *)
      (* | HTMY *)
      (* | HYMT *)
      (* | HTMXT *)
      (* | HXTMT *)
      (* | HTMYT *)
      (* | HYTMT *)
      (* | HWMX *)
      (* | HXMW *)
      (* | HWMY *)
      (* | HYMW *)
      (* | HWMXT *)
      (* | HXTMW *)
      (* | HWMYT *)
      (* | HYTMW *)
      | DISTEXACT
      (* | DISTBIN *)
      (* | DISTBIN_HWMW *)
      (* | DISTBIN_HLML *)
      (* | DISTBIN_HTMT *)
      (* | DISTBIN_HQMQ *)
      (* | BW *)
      (* | HWMW_BW *)
      (* | BT *)
      | HTMT_BT
      | HWMT_BT
      | HTMW_BT
      (* | BQ *)
      (* | HQMQ_BQ *)
      | HWMW_BT
      (* | HWMW_BQ *)
      | BETWEEN
      | HTMT_BETWEEN
      (* | HTMT_BIN *)
      (* | HTMT_BPU *)
      (* | HTMT_BVB *)
      (* | hqmq_BCC *)
      (* | HQMQ_BIN *)
      (* | HQMQ_BPU *)
      (* | HQMQ_BVB *)
      | TF0
      | TFQ0
      | TWF0
      | HF_MF
      | HF_MQ
      | HF_MFQ
      | HQ_MF
      | HFQ_MF
      | HFQ_MFQ
      | HFQ_MQ
      | HQ_MFQ
      | DIR_BIAS

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

    (* let fwt tt word1 tag1 = *)
    (*   Int.shift_left (of_template_type tt) 56 *)
    (*   |> Int.bit_or word1 *)
    (*   |> Int.bit_or (Int.shift_left tag1 16) *)

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

    let make_template_tok labeled fun_score array_sentence idx role_symb tag_dir =

      (* let _ = assert(tag_dir <= 1) in *)
      (* let tag_dir = 0 in *)

      let perform =
      match role_symb with
      | Head -> true
      | Modifier -> labeled
    in
    if perform
    then
      let role = ((Obj.magic (Obj.repr role_symb)) : int) in
      let tok = Array.unsafe_get array_sentence idx in
      let word = C.get_form_id tok in
      (* let prefix = C.get_prefix_id tok in *)
      (* let suffix = C.get_suffix_id tok in *)


    (* let (wordp1,lemmap1, posp1,cposp1, form_cposp1) = *)
    (*   (if idx < 1 *)
    (*    then *)
    (*      (invalid_word, invalid_word, *)
    (*       invalid_tag, invalid_tag, *)
    (*       invalid_word) *)
    (*    else *)
    (*      let v = Array.unsafe_get array_sentence (idx - 1) in *)
    (*      (C.get_form_id        v, *)
    (*       C.get_lemma_id       v, *)
    (*       C.get_pos_id         v, *)
    (*       C.get_coarse_pos_id  v, *)
    (*       C.get_form_cpos_id   v *)
    (*      ) *)
    (*   ) *)
    (* in *)
    (* let (wordp2, lemmap2, posp2, cposp2, form_cposp2) = *)
    (*   (if idx < 2 *)
    (*    then *)
    (*      (invalid_word, invalid_word, *)
    (*       invalid_tag, invalid_tag, *)
    (*       invalid_word) *)
    (*    else *)
    (*      let v = Array.unsafe_get array_sentence (idx - 2) in *)
    (*      (C.get_form_id        v, *)
    (*       C.get_lemma_id       v, *)
    (*       C.get_pos_id         v, *)
    (*       C.get_coarse_pos_id  v, *)
    (*       C.get_form_cpos_id   v *)
    (*      ) *)

    (*   ) *)
    (* in *)
    (* let (wordn1,lemman1, posn1,cposn1, form_cposn1) = *)
    (*   (if idx > (Array.length array_sentence) - 2 *)
    (*    then *)
    (*      (invalid_word, invalid_word, *)
    (*       invalid_tag, invalid_tag, *)
    (*       invalid_word) *)
    (*    else *)
    (*      let v = Array.unsafe_get array_sentence (idx + 1) in *)
    (*      (C.get_form_id        v, *)
    (*       C.get_lemma_id       v, *)
    (*       C.get_pos_id         v, *)
    (*       C.get_coarse_pos_id  v, *)
    (*       C.get_form_cpos_id   v *)
    (*      ) *)
    (*   ) *)
    (* in *)
    (* let (wordn2,lemman2, posn2,cposn2, form_cposn2) = *)
    (*   (if idx > (Array.length array_sentence) - 3 *)
    (*    then *)
    (*      (invalid_word, invalid_word, *)
    (*       invalid_tag, invalid_tag, *)
    (*       invalid_word) *)
    (*    else *)
    (*      let v = Array.unsafe_get array_sentence (idx + 2) in *)
    (*      (C.get_form_id        v, *)
    (*       C.get_lemma_id       v, *)
    (*       C.get_pos_id         v, *)
    (*       C.get_coarse_pos_id  v, *)
    (*       C.get_form_cpos_id   v *)
    (*      ) *)
    (*   ) *)
    (* in *)

    (*   (fun_score (fwtt TW0 word role tag_dir)) *)
    (*   +. (fun_score (fwtt TL0 lemma role tag_dir)) *)
    (*   +. (fun_score (fttt TT0 pos role tag_dir)) *)
    (*   +. (fun_score (fttt TQ0 cpos role tag_dir)) *)

    (*   +. (fun_score (fwtt TB0 form_cpos role tag_dir)) *)
    (*   +. (fun_score (fwtt  TF0  feat role tag_dir)) *)
    (*   +. (fun_score (fwwtt TWF0 word feat role tag_dir)) *)
    (*   +. (fun_score (fwttt TFQ0 feat cpos role tag_dir)) *)


    (*   +. (fun_score (fwtt TWP1 wordp1 role tag_dir)) *)
    (*   +. (fun_score (fwtt TWN1 wordn1 role tag_dir)) *)
    (*   +. (fun_score (fttt TQP1 cposp1 role tag_dir)) *)
    (*   +. (fun_score (fttt TQN1 cposn1 role tag_dir)) *)
    (*   +. (fun_score (fwtt TLP1 lemmap1 role tag_dir)) *)
    (*   +. (fun_score (fwtt TLN1 lemman1 role tag_dir)) *)
    (*   +. (fun_score (fttt TTP1 posp1 role tag_dir)) *)
    (*   +. (fun_score (fttt TTN1 posn1 role tag_dir)) *)
    (*   +. (fun_score (fwtt TBP1 form_cposp1 role tag_dir)) *)
    (*   +. (fun_score (fwtt TBN1 form_cposn1 role tag_dir)) *)

    (*   +. (fun_score (fwtt TWP2 wordp2 role tag_dir)) *)
    (*   +. (fun_score (fwtt TWN2 wordn2 role tag_dir)) *)
    (*   +. (fun_score (fttt TQP2 cposp2 role tag_dir)) *)
    (*   +. (fun_score (fttt TQN2 cposn2 role tag_dir)) *)
    (*   +. (fun_score (fttt TTP2 posp2 role tag_dir)) *)
    (*   +. (fun_score (fttt TTN2 posn2 role tag_dir)) *)
    (*   +. (fun_score (fwtt TLP2 lemmap2 role tag_dir)) *)
    (*   +. (fun_score (fwtt TLN2 lemman2 role tag_dir)) *)
    (*   +. (fun_score (fwtt TBP2 form_cposp2 role tag_dir)) *)
    (*   +. (fun_score (fwtt TBN2 form_cposn2 role tag_dir)) *)

    (*   +. (fun_score (ftttt TQP01 cpos cposp1 role tag_dir)) *)
    (*   +. (fun_score (ftttt TQN01 cpos cposn1 role tag_dir)) *)

    (*   +. (fun_score (fttttt TQP012 cpos cposp1 cposp2 role tag_dir)) *)
    (*   +. (fun_score (fttttt TQN012 cpos cposn1 cposn2 role tag_dir)) *)

    (*        (\* +. (fun_score (fwwt TWP01 word wordp1 role)) *\) *)
    (*        (\* +. (fun_score (fwwt TWN01 word wordn1 role)) *\) *)

    (*        (\* +. (fun_score (fwwwt TWP012 word wordp1 wordp2 role)) *\) *)
    (*        (\* +. (fun_score (fwwwt TWN012 word wordn1 wordn2 role)) *\) *)

    (*        (\* +. (fun_score (fwwt TLP01 lemma lemmap1 role)) *\) *)
    (*        (\* +. (fun_score (fwwt TLN01 lemma lemman1 role)) *\) *)

    (*        (\* +. (fun_score (fwwwt TLP012 lemma lemmap1 lemmap2 role)) *\) *)
    (*        (\* +. (fun_score (fwwwt TLN012 lemma lemman1 lemman2 role)) *\) *)

    (*        (\* +. (fun_score (fttt TTP01 pos posp1 role)) *\) *)
    (*        (\* +. (fun_score (fttt TTN01 pos posn1 role)) *\) *)

    (*        (\* +. (fun_score (ftttt TTP012 pos posp1 posp2 role)) *\) *)
    (*        (\* +. (fun_score (ftttt TTN012 pos posn1 posn2 role)) *\) *)

    (*        (\* +. (fun_score (fwt TP0 form_pos role)) *\) *)
    (*        (\* +. (fun_score (fwt TPP1 form_posp1 role)) *\) *)
    (*        (\* +. (fun_score (fwt TPP2 form_posp2 role)) *\) *)
    (*        (\* +. (fun_score (fwt TPN1 form_posn1 role)) *\) *)
    (*        (\* +. (fun_score (fwt TPN2 form_posn2 role)) *\) *)

    (*        (\* +. (fun_score (fwwt TPP01 form_pos form_posp1 role)) *\) *)
    (*        (\* +. (fun_score (fwwt TPN01 form_pos form_posn1 role)) *\) *)


    (*        (\* +. (fun_score (fwwwt TPP012 form_pos form_posp1 form_posp2 role)) *\) *)
    (*        (\* +. (fun_score (fwwwt TPN012 form_pos form_posn1 form_posn2 role)) *\) *)

    (*        (\* +. (fun_score (fwwt TBP01 form_cpos form_cposp1 role)) *\) *)
    (*        (\* +. (fun_score (fwwt TBN01 form_cpos form_cposn1 role)) *\) *)

    (*        (\* +. (fun_score (fwwwt TBP012 form_cpos form_cposp1 form_cposp2 role)) *\) *)
    (*        (\* +. (fun_score (fwwwt TBN012 form_cpos form_cposn1 form_cposn2 role)) *\) *)

    (*        (\* +. (fun_score (fwt TX0 prefix role)) *\) *)
    (*        (\* +. (fun_score (fwt TXP1 prefp1 role)) *\) *)
    (*        (\* +. (fun_score (fwt TXP2 prefp2 role)) *\) *)
    (*        (\* +. (fun_score (fwt TXN1 prefn1 role)) *\) *)
    (*        (\* +. (fun_score (fwt TXN2 prefn2 role)) *\) *)

    (*        (\* +. (fun_score (fwt TY0 suffix role)) *\) *)
    (*        (\* +. (fun_score (fwt TYP1 sufp1 role)) *\) *)
    (*        (\* +. (fun_score (fwt TYP2 sufp2 role)) *\) *)
    (*        (\* +. (fun_score (fwt TYN1 sufn1 role)) *\) *)
    (*        (\* +. (fun_score (fwt TYN2 sufn2 role)) *\) *)

    (*        (\* +. (fun_score (fwt  TF0  feat role)) *\) *)
    (*        (\* +. (fun_score (fwtt TFQ0 feat pos role)) *\) *)
    (*        (\* +. (fun_score (fwwt TWF0 word feat role)) *\) *)
    (* else *)
      0.0
    else 0.0


    let make_template_tok_head labeled fun_score sentence head_idx tag_dir=
      make_template_tok labeled fun_score sentence head_idx Head tag_dir

    let make_template_tok_mod labeled fun_score sentence mod_idx tag_dir =
      make_template_tok labeled fun_score sentence mod_idx Modifier tag_dir

    let make_template_dep fun_score array_sentence head_idx dep_idx =
      let head = Array.unsafe_get array_sentence head_idx in
      let dep  = Array.unsafe_get array_sentence dep_idx in
      let hform = C.get_form_id head in
      let dform = C.get_form_id dep in


      (* let tmp_dist = Int.abs (head_idx - dep_idx) in *)
      (* let dist = (if tmp_dist > max_distance then max_distance else tmp_dist) in *)
      (* let my_dist = bin_distance dist in *)
      (* let tag_dir = (if head_idx > dep_idx then 0 else 1) in *)

      (* let (min,max) = (if head_idx < dep_idx then (head_idx,dep_idx) else (dep_idx, head_idx)) in *)
      (* let rec compute_between (nb_punct,nb_verb, nb_coord) idx = *)
      (*   if idx = max then (nb_punct,nb_verb,nb_coord) *)
      (*   else *)
      (*     let b = Array.unsafe_get array_sentence idx in *)
      (*     compute_between (nb_punct + (dirac (C.is_punct b)), *)
      (*                      nb_verb  + (dirac (C.is_verb b)), *)
      (*                      nb_coord + (dirac (C.is_coord b)) *)
      (*                     ) (idx + 1) *)
      (* in *)
      (* let (nb_punct, nb_verb, nb_coord) = compute_between (0,0,0) (min+1) in *)
      (* let nb_punct = Int.min nb_punct max_cross in *)
      (* let nb_verb = Int.min  nb_verb max_cross in *)
      (* let nb_coord = Int.min nb_coord max_cross in *)

      (* let between_punct = Int.shift_left nb_punct 4 |> Int.bit_or between_punc in *)
      (* let between_verb  = Int.shift_left nb_verb 4  |> Int.bit_or between_verb in *)
      (* let between_coord = Int.shift_left nb_coord 4 |> Int.bit_or between_coord in *)


      (* let dposp = (if dep_idx < 1 *)
      (*              then invalid_tag *)
      (*              else *)
      (*                let v = Array.unsafe_get array_sentence dep_idx in *)
      (*                C.get_pos_id v *)
      (*             ) *)
      (* in *)
      (* let hposp = (if head_idx < 1 *)
      (*              then invalid_tag *)
      (*              else *)
      (*                let v = Array.unsafe_get array_sentence head_idx in *)
      (*                C.get_pos_id v *)
      (*             ) *)

      (* in *)
      (* let dposn = (if dep_idx > (Array.length array_sentence) - 2 *)
      (*              then invalid_tag *)
      (*              else *)
      (*                let v = Array.unsafe_get array_sentence dep_idx in *)
      (*                C.get_pos_id v *)
      (*             ) *)
      (* in *)
      (* let hposn = (if head_idx > (Array.length array_sentence) - 2 *)
      (*              then invalid_tag *)
      (*              else *)
      (*                let v = Array.unsafe_get array_sentence head_idx in *)
      (*                C.get_pos_id v *)
      (*             ) *)
      (* in *)

      (* let res = *)
      (*   (fun_score (ft DIR_BIAS tag_dir)) *)
      (*   +. (fun_score (fwwt HWMW hform dform tag_dir)) *)
      (*   +. (fun_score (fttt HTMT hpos dpos tag_dir)) *)

      (*   +. (fun_score (fwtt HWMT hform dpos tag_dir)) *)
      (*   +. (fun_score (fwtt HTMW dform hpos tag_dir)) *)
      (*   +. (fun_score (fwtt HTMB dformpos hpos tag_dir)) *)
      (*   +. (fun_score (fwtt HBMT hformpos dpos tag_dir)) *)
      (*   +. (fun_score (fwwt HBMB hformpos dformpos tag_dir)) *)

      (*   +. (fun_score (fwwt HF_MF     hfeat dfeat tag_dir)) *)
      (*   +. (fun_score (fwwt HF_MQ     hfeat dpos tag_dir)) *)
      (*   +. (fun_score (fwwtt HF_MFQ   hfeat dfeat dpos tag_dir)) *)
      (*   +. (fun_score (fwwt HQ_MF     dfeat hpos tag_dir)) *)
      (*   +. (fun_score (fwwtt HFQ_MF   hfeat dfeat hpos tag_dir)) *)
      (*   +. (fun_score (fwwttt HFQ_MFQ hfeat dfeat hpos dpos tag_dir)) *)
      (*   +. (fun_score (fwttt HFQ_MQ   hfeat hpos dpos tag_dir)) *)
      (*   +. (fun_score (fwwtt HQ_MFQ   dfeat hpos dpos tag_dir)) *)


      (*   (\* +. (fun_score (fwww HWMW_HNW hform dform hformn)) *\) *)
      (*   (\* +. (fun_score (fwww HWMW_MNW hform dform dformn)) *\) *)
      (*   (\* +. (fun_score (fwww HWMW_HPW hform dform hformp)) *\) *)
      (*   (\* +. (fun_score (fwww HWMW_MPW hform dform dformp)) *\) *)
      (*   (\* +. (fun_score (fwwt DISTBIN_HWMW hform dform my_dist)) *\) *)


      (*   (\* +. *\) *)
      (*   (\*       (fun_score (fww HLML hlemma dlemma)) *\) *)
      (*   (\*       +. (fun_score (fwww HLML_HNL hlemma dlemma hlemman)) *\) *)
      (*   (\*       +. (fun_score (fwww HLML_MNL hlemma dlemma dlemman)) *\) *)
      (*   (\*       +. (fun_score (fwww HLML_HPL hlemma dlemma hlemmap)) *\) *)
      (*   (\*       +. (fun_score (fwww HLML_MPL hlemma dlemma dlemmap)) *\) *)
      (*   (\*       +. (fun_score (fwwt DISTBIN_HLML hlemma dlemma my_dist)) *\) *)


      (*   +. (fun_score (ftttt HTMT_HNT hpos dpos hposn tag_dir)) *)
      (*   +. (fun_score (ftttt HTMT_MNT hpos dpos dposn tag_dir)) *)
      (*   +. (fun_score (ftttt HTMT_HPT hpos dpos hposp tag_dir)) *)
      (*   +. (fun_score (ftttt HTMT_MPT hpos dpos dposp tag_dir)) *)

      (*   (\* +. (fun_score (fwtt HTMT_HNW hformn hpos dpos)) *\) *)
      (*   (\* +. (fun_score (fwtt HTMT_MNW dformn hpos dpos)) *\) *)
      (*   (\* +. (fun_score (fwtt HTMT_HPW hformp hpos dpos)) *\) *)
      (*   (\* +. (fun_score (fwtt HTMT_MPW dformp hpos dpos)) *\) *)


      (*   +. (fun_score (fttttt HTMT_NNTT hpos dpos hposn dposn tag_dir)) *)
      (*   +. (fun_score (fttttt HTMT_NPTT hpos dpos hposn dposp tag_dir)) *)
      (*   +. (fun_score (fttttt HTMT_PPTT hpos dpos hposp dposp tag_dir)) *)
      (*   +. (fun_score (fttttt HTMT_PNTT hpos dpos hposp dposn tag_dir)) *)
      (*   +. (fun_score (fttttttt HTMT_PNTT_PNTT hpos dpos hposp dposp hposn dposn tag_dir)) *)

      (*   (\* adjacent context *\) *)
      (*   +. *)
      (*     (if head_idx <> 0 && head_idx = dep_idx - 1 *)
      (*      then *)
      (*        (fun_score (fttttt HTMT_MNT hpos dpos dposn tag_dir 0x01)) *)
      (*        +. (fun_score (fttttt HTMT_HPT hpos dpos hposp tag_dir 0x01)) *)
      (*        +. (fun_score (ftttttt HTMT_PNTT hpos dpos hposp dposn tag_dir 0x01)) *)
      (*      else 0.0 *)
      (*     ) *)
      (*   +. *)
      (*     (if head_idx <> 0 && head_idx = dep_idx + 1 *)
      (*      then *)
      (*        (fun_score (fttttt HTMT_MPT hpos dpos dposp tag_dir 0x01)) *)
      (*        +. (fun_score (fttttt HTMT_HNT hpos dpos hposn tag_dir 0x01)) *)
      (*        +. (fun_score (ftttttt HTMT_NPTT hpos dpos hposn dposp tag_dir 0x01)) *)
      (*      else 0.0 *)
      (*     ) *)



      (*   (\* +. (fun_score (fwwt HWMW_HNT hform dform hposn)) *\) *)
      (*   (\* +. (fun_score (fwwt HWMW_MNT hform dform dposn)) *\) *)
      (*   (\* +. (fun_score (fwwt HWMW_HPT hform dform hposp)) *\) *)
      (*   (\* +. (fun_score (fwwt HWMW_MPT hform dform dposp)) *\) *)

      (*   (\* +. (fun_score (fwwtt HWMW_NNTT hform dform hposn dposn)) *\) *)
      (*   (\* +. (fun_score (fwwtt HWMW_NPTT hform dform hposn dposp)) *\) *)
      (*   (\* +. (fun_score (fwwtt HWMW_PPTT hform dform hposp dposp)) *\) *)
      (*   (\* +. (fun_score (fwwtt HWMW_PNTT hform dform hposp dposn)) *\) *)

      (*   (\* +. (fun_score (fwwtt  HTMW_HNT  dform hpos hposn tag_dir)) *\) *)
      (*   (\* +. (fun_score (fwwtt  HTMW_MNT  dform hpos dposn tag_dir)) *\) *)
      (*   (\* +. (fun_score (fwwtt  HTMW_HPT  dform hpos hposp tag_dir)) *\) *)
      (*   (\* +. (fun_score (fwwtt  HTMW_MPT  dform hpos dposp tag_dir)) *\) *)
      (*   (\* +. (fun_score (fwwttt HTMW_NNTT dform hpos hposn dposn tag_dir)) *\) *)
      (*   (\* +. (fun_score (fwwttt HTMW_NPTT dform hpos hposn dposp tag_dir)) *\) *)
      (*   (\* +. (fun_score (fwwttt HTMW_PPTT dform hpos hposp dposp tag_dir)) *\) *)
      (*   (\* +. (fun_score (fwwttt HTMW_PNTT dform hpos hposp dposn tag_dir)) *\) *)

      (*   (\* +. (fun_score (fwttt  HWMT_HNT  hform dpos hposn tag_dir)) *\) *)
      (*   (\* +. (fun_score (fwttt  HWMT_MNT  hform dpos dposn tag_dir)) *\) *)
      (*   (\* +. (fun_score (fwttt  HWMT_HPT  hform dpos hposp tag_dir)) *\) *)
      (*   (\* +. (fun_score (fwttt  HWMT_MPT  hform dpos dposp tag_dir)) *\) *)
      (*   (\* +. (fun_score (fwtttt HWMT_NNTT hform dpos hposn dposn tag_dir)) *\) *)
      (*   (\* +. (fun_score (fwtttt HWMT_NPTT hform dpos hposn dposp tag_dir)) *\) *)
      (*   (\* +. (fun_score (fwtttt HWMT_PPTT hform dpos hposp dposp tag_dir)) *\) *)
      (*   (\* +. (fun_score (fwtttt HWMT_PNTT hform dpos hposp dposn tag_dir)) *\) *)

      (*   (\* +. (fun_score (fww HWMB hform dformpos)) *\) *)
      (*   (\* +. (fun_score (fww HBMW hformpos dform)) *\) *)
      (*   (\* +. (fun_score (fww HBMB hformpos dformpos)) *\) *)

      (*   (\* +. (fun_score (fwww HBMB_HNB hformpos dformpos hformposn)) *\) *)
      (*   (\* +. (fun_score (fwww HBMB_HPB hformpos dformpos hformposp)) *\) *)
      (*   (\* +. (fun_score (fwww HBMB_MNB hformpos dformpos dformposn)) *\) *)
      (*   (\* +. (fun_score (fwww HBMB_MPB hformpos dformpos dformposp)) *\) *)

      (*   (\* +. (fun_score (fwt HWMQ hform dcpos)) *\) *)
      (*   (\* +. (fun_score (fwt HQMW dform hcpos)) *\) *)

      (*   (\* +. (fun_score (fwt HQMR dformcpos hcpos)) *\) *)
      (*   (\* +. (fun_score (fww HWMR hform dformcpos)) *\) *)
      (*   (\* +. (fun_score (fwt HRMQ hformcpos dcpos)) *\) *)
      (*   (\* +. (fun_score (fww HRMW hformcpos dform)) *\) *)
      (*   (\* +. (fun_score (fww HRMR hformcpos dformcpos)) *\) *)

      (*   (\* +. (fun_score (fttt HQMQ_HNQ hcpos dcpos hcposn)) *\) *)
      (*   (\* +. (fun_score (fttt HQMQ_MNQ hcpos dcpos dcposn)) *\) *)
      (*   (\* +. (fun_score (fttt HQMQ_HPQ hcpos dcpos hcposp)) *\) *)
      (*   (\* +. (fun_score (fttt HQMQ_MPQ hcpos dcpos dcposp)) *\) *)
      (*        (\* +. (fun_score (fttt DISTBIN_HQMQ hcpos dcpos my_dist)) *\) *)

      (*   +. (fun_score (ftt DISTEXACT dist tag_dir)) *)

      (* in *)
      (* let rec all_bin_dist acc bd = *)
      (*   if bd < 0 then acc *)
      (*   else *)
      (*     all_bin_dist (acc *)
      (*                   +. fun_score (ftt DIR_BIAS bd tag_dir) *)
      (*                   +. fun_score (ftttt HTMT hpos dpos bd tag_dir) *)
      (*                   +. fun_score (ftttt TP0 hpos bd 0 tag_dir) *)
      (*                   +. fun_score (ftttt TP0 dpos bd 1 tag_dir) *)
      (*                  ) (bd -1) *)
      (* in *)
      (* let res = res +. all_bin_dist 0.0 my_dist *)

      (*           +. (fun_score (ftt BETWEEN between_verb tag_dir)) *)
      (*           +. (fun_score (ftt BETWEEN between_punct tag_dir)) *)
      (*           +. (fun_score (ftt BETWEEN between_coord tag_dir)) *)
      (*           +. (fun_score (ftttt HTMT_BETWEEN hpos dpos between_verb tag_dir)) *)
      (*           +. (fun_score (ftttt HTMT_BETWEEN hpos dpos between_punct tag_dir)) *)
      (*           +. (fun_score (ftttt HTMT_BETWEEN hpos dpos between_coord tag_dir)) *)

      (* in *)
      (* let rec compute_between acc idx = *)
      (*   if idx = max then acc *)
      (*   else *)
      (*     let b = Array.unsafe_get array_sentence idx in *)
      (*     (\* let bform = C.get_form_id  b in *\) *)
      (*     let bpos  = C.get_pos_id b in *)
      (*     (\* let bcpos  = C.get_coarse_pos_id b in *\) *)

      (*     compute_between *)
      (*       (acc *)
      (*        +. (fun_score (ftttt HTMT_BT hpos dpos bpos tag_dir)) *)
      (*        +. (fun_score (fwwtt HWMW_BT hform dform bpos tag_dir)) *)
      (*        +. (fun_score (fwttt HWMT_BT hform dpos bpos tag_dir)) *)
      (*        +. (fun_score (fwttt HTMW_BT dform hpos bpos tag_dir)) *)
      (*       ) (idx + 1) *)
      (* in *)
      (* compute_between res (min+1) *)

      (* (\* +. (fun_score (ft BCC nb_coord)) *\) *)
      (* (\* +. (fun_score (ft BVB nb_verb)) *\) *)
      (* (\* +. (fun_score (ft BPU nb_punct)) *\) *)
      (* (\* +. (fun_score (ft BIN nb_prep)) *\) *)
      (* (\* +. (fun_score (fwwt HWMW_BCC hform dform nb_coord)) *\) *)
      (* (\* +. (fun_score (fwwt HWMW_BVB hform dform nb_verb)) *\) *)
      (* (\* +. (fun_score (fwwt HWMW_BPU hform dform nb_punct)) *\) *)
      (* (\* +. (fun_score (fwwt HWMW_BIN hform dform nb_prep)) *\) *)
      (* (\* +. (fun_score (fttt HTMT_BCC hpos dpos nb_coord)) *\) *)
      (* (\* +. (fun_score (fttt HTMT_BVB hpos dpos nb_verb)) *\) *)
      (* (\* +. (fun_score (fttt HTMT_BPU hpos dpos nb_punct)) *\) *)
      (* (\* +. (fun_score (fttt HTMT_BIN hpos dpos nb_prep)) *\) *)
      (* (\* +. (fun_score (fttt HQMQ_BCC hcpos dcpos nb_coord)) *\) *)
      (* (\* +. (fun_score (fttt HQMQ_BVB hcpos dcpos nb_verb)) *\) *)
      (* (\* +. (fun_score (fttt HQMQ_BPU hcpos dcpos nb_punct)) *\) *)
      (* (\* +. (fun_score (fttt HQMQ_BIN hcpos dcpos nb_prep)) *\) *)

      (* (\* +. (fun_score (fwtt HTMX dpref hpos tag_dir)) *\) *)
      (* (\* +. (fun_score (fwtt HXMT hpref dpos tag_dir)) *\) *)
      (* (\* +. (fun_score (fwtt HTMY dsuf  hpos tag_dir)) *\) *)
      (* (\* +. (fun_score (fwtt HYMT hsuf  dpos tag_dir)) *\) *)

      (* (\* +. (fun_score (fwwt HWMX dpref hform tag_dir)) *\) *)
      (* (\* +. (fun_score (fwwt HXMW hpref dform tag_dir)) *\) *)
      (* (\* +. (fun_score (fwwt HWMY dsuf  hform tag_dir)) *\) *)
      (* (\* +. (fun_score (fwwt HYMW hsuf  dform tag_dir)) *\) *)

      (* (\* +. (fun_score (fwttt HTMXT dpref hpos dpos tag_dir)) *\) *)
      (* (\* +. (fun_score (fwttt HXTMT hpref hpos dpos tag_dir)) *\) *)
      (* (\* +. (fun_score (fwttt HTMYT dsuf  hpos dpos tag_dir)) *\) *)
      (* (\* +. (fun_score (fwttt HYTMT hsuf  hpos dpos tag_dir)) *\) *)

      (* (\* +. (fun_score (fwwtt HWMXT dpref hform dpos tag_dir)) *\) *)
      (* (\* +. (fun_score (fwwtt HXTMW hpref dform hpos tag_dir)) *\) *)
      (* (\* +. (fun_score (fwwtt HWMYT dsuf  hform dpos tag_dir)) *\) *)
      (* +. (fun_score (fwwtt HYTMW hsuf  dform hpos tag_dir)) *)
      0.0


    let global_template_list  =
      [
        (fun ct sentence h d ->
         let tag_dir = (if h > d then 0 else 1) in
         (make_template_tok false ct sentence h Head tag_dir)
         +. (make_template_tok false ct sentence d Modifier tag_dir)) ;
        make_template_dep ;
      ]


    (* include Int *)
    (* let hash = Int.hash *)
    (* let sexp_of_t = Int.sexp_of_t *)

    let of_int t = t

    let table_collect_templates = Table.create ()

    let collect_template t  =
      let _ = Hashtbl.change table_collect_templates t incr_opt
      in  ()

    let reset_table_collect_template () =
      Hashtbl.clear table_collect_templates


    let collect_templates ~only_gold sentence =
      let local_table_collect_templates = Table.create () in
      let ct t =
        let _ = Hashtbl.change local_table_collect_templates t incr_opt
        in  0.0
      in
      (if only_gold
       then
         (for i = 1 to (Array.length sentence) - 1 do
            (* let head_idx = C.get_head (Array.unsafe_get sentence i) in *)
            (* let tag_dir = (if head_idx > i then 0 else 1) in *)
            (* let (_:float) = make_template_tok false ct sentence i Modifier tag_dir in *)
            (* let (_:float) = make_template_tok false ct sentence head_idx Head tag_dir in *)
            (* let (_:float) = make_template_dep ct sentence head_idx i in *)
            ()
          done)
       else
         (for i = 0 to (Array.length sentence) - 1 do
            (* let (_:float) = make_template_tok false ct sentence i Head 0 in *)
            (* let (_:float) = make_template_tok false ct sentence i Modifier 0 in *)
            (* let (_:float) = make_template_tok false ct sentence i Head 1 in *)
            (* let (_:float) = make_template_tok false ct sentence i Modifier 1 in *)
            ()
          done;
          for h = 0 to (Array.length sentence) - 1 do
            for d = 1 to (Array.length sentence) - 1 do
              if h <> d then
                let _ = make_template_dep ct sentence h d in
                ()
            done
          done))
      ;
        Hashtbl.iter local_table_collect_templates
                     ~f:(fun ~key:k ~data:_ -> collect_template k)

    (* These 2 functions should be factorized at some point *)
    let compute_score  weight_vector template_to_index_fun =
      let max_size = Array.length weight_vector in
      let fun_score t =
        let idx = template_to_index_fun t in
        if idx >= 0 && idx < max_size
        then Array.unsafe_get weight_vector idx
        else 0.0
      in
      List.fold_left (global_template_list )
                     ~init:(fun _ _ _ -> 0.0)
                     ~f:
                     (fun acc f ->
                      fun sentence head dep ->
                      (acc sentence head dep) +. (f fun_score sentence head dep))

    let fill_hash_table  htbl is_valid template_to_index_fun oper =
      let fun_score t =
        let idx = (template_to_index_fun t) in
        let () = (if is_valid idx then  Hashtbl.change htbl (template_to_index_fun t) oper)
        in 0.0
      in
      List.fold_left (global_template_list )
                     ~init: (fun _ _ _ -> 0.0)
                     ~f:
                     (fun acc f ->
                      fun sentence head dep ->
                      let (_:float) = acc sentence head dep in
                      f fun_score sentence head dep
                     )
  end
