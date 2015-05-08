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
open Conll

module type Template =
sig
  type t with sexp
  include Hashable.S with type t := t
  module C : ConllType

  val of_int : int -> t

  val table_collect_templates : (t,int) Hashtbl.t
  val reset_table_collect_template : unit -> unit

  (* val compute_score :  float array -> (t -> int) -> (Conll.t array -> int -> int -> float) *)
  val collect_templates : only_gold:bool -> C.t array -> unit
  val fill_hash_table :  (int, int) Table.hashtbl -> (int ->bool) ->
    (t -> int) -> (int option -> int option) -> C.t array -> int -> int -> float

  (* val make_template_tok_head :  bool -> (t -> float) -> Conll.t array -> int -> int -> float *)
  (* val make_template_tok_mod  :  bool -> (t -> float) -> Conll.t array -> int -> int -> float *)
  (* val make_template_dep :       (t -> float) -> Conll.t array -> int -> int -> float *)

end
