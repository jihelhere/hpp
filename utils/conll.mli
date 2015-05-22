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

module type ConllType =
sig
    type t
    type sentence
    type corpus

    val prediction : t -> int

    val same_prediction : t -> t -> bool
    val prepare_sentence_for_decoder : sentence -> t array
    val to_string : t -> string

    val do_read_file : string -> collect_word:bool -> verbose:bool -> corpus
    val corpus_to_list : corpus -> sentence list

end
