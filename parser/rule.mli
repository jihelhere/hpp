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

module Rule : sig
  type t =
    | Bin of int * int * int
    | Una of int * int * int list
    | Lex of int * int [@@deriving sexp, compare]
  val create_pcfg : Ptbtree.int_tree list -> (int,float) Hashtbl.t * (t,float) Hashtbl.t
  val priors_of_sexp : Sexp.t -> (int,float) Hashtbl.t
  val gram_of_sexp : Sexp.t -> (t,float) Hashtbl.t
end
