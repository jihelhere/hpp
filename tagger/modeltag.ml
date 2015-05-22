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
open Featuretag
open Conlltag

module Model =
  struct
    type t =
      {
        weights : float array;
        nb_latvars : int;
      }


    let load_corpus sexp =
      match sexp with
      | Sexp.List (Sexp.Atom "corpus"::map_list) ->
         List.iter ~f:Conll_Tag.load_map_from_sexp map_list
      | _ -> assert(false)

    let load_template_map sexp =
      match sexp with
      |Sexp.List ([Sexp.Atom "template_feature_map";map]) ->
        Feature_Tag.load_template_map_from_sexp map
      | _ -> assert(false)

    let load_weights sexp =
      let rec loop array idx l =
        match l with
        | [] -> ()
        | (Sexp.Atom value)::tl -> array.(idx) <- Float.of_string value ;
                                   loop array (idx+1) tl
        | _ -> assert(false)
      in
      match sexp with
      | Sexp.List([(Sexp.Atom "weights");(Sexp.Atom size);Sexp.List l]) ->
         let weights = Array.create ~len:(Int.of_string size) 0.0 in
         loop weights 0 l;
         weights
      | _ -> assert(false)


    let compute_norm weights =
      Array.fold weights ~init:0.0 ~f:(fun acc e -> acc +. (e *. e)) |> sqrt

    let normalize_weights weights =
      let norm = compute_norm weights in
      Array.init (Array.length weights)
        ~f:(fun i -> weights.(i) /. norm)

    let load model =
      let sexp = Sexp.load_sexp model in
      match sexp with
      | Sexp.List([corpus; template_map; feature_weights; Sexp.Atom nbh]) ->
         load_corpus corpus;
         load_template_map template_map;
         let w = load_weights feature_weights in
         let w = normalize_weights w in
         (* Printf.printf "OK: %f\n%!" (compute_norm w); *)
         {weights = w; nb_latvars = Int.of_string nbh}
      | _ -> assert(false)


    let get_weights t = t.weights
    let get_data t = (t.weights, t.nb_latvars)

    let make weights nb_latvars =
      {
        weights = weights;
        nb_latvars = nb_latvars
      }

    let save ~filename t =
      let a x = Sexp.Atom x and l x = Sexp.List x in
      Sexp.save_hum filename
                    (l [ Conll_Tag.all_string_tables_to_sexp ();
                         Feature_Tag.template_feature_map_to_sexp ();
                         l [ a "weights" ;
                             a (Int.to_string (Array.length t.weights));
                             (Array.sexp_of_t Float.sexp_of_t t.weights)
                           ];
                         a (Int.to_string t.nb_latvars)
                       ])
  end
