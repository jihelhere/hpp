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


module Conll_Tag :
  sig
    type t
    type sentence
    type corpus

    val same_prediction : t -> t -> bool
    val same_fine_prediction : t -> t -> bool
    val prediction : t -> int
    val latent_prediction : t -> int
    val set_prediction : t -> int -> t
    val set_latentprediction : t -> int -> int -> t
    val prepare_sentence_for_decoder : sentence -> t array

    val corpus_to_list : corpus -> sentence list
    val list_to_corpus : sentence list -> corpus
    val sentence_to_list : sentence -> t list
    val list_to_sentence : t list -> sentence

    val start : t
    val stop : t
    val unknown : t

    val is_digit : t -> bool

    val get_form : t -> string
    val get_pos : t -> string
    val get_idx : t -> int

    val get_form_id : t -> int
    val get_pos_id : t -> int

    val get_prefix_id : t -> int
    val get_suffix_id : t -> int


    val get_number_form : unit -> int
    val get_number_pos : unit -> int

    val to_string : t -> string

    val line_to_conll_token : string -> t
    val do_read_file : string -> collect_word:bool -> verbose:bool -> corpus
    val do_write_file : corpus -> string -> unit

    val all_string_tables_to_sexp : unit -> Sexp.t
    val save_all_string_tables_to_file : string -> unit

    val load_map_from_sexp : Sexp.t -> unit

    val collect_word_tags : unit -> int array array
    val collect_unk_tags :  unit  -> int array

    val has_hyphen : t -> bool
    val has_digit : t -> bool
    val has_uppercase : t -> bool
    val all_uppercase : t -> bool

    val get_prefix_list : t -> int list
    val get_suffix_list : t -> int list

  end
