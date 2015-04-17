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
open Tree
open Ptbtree

module Rule : sig
  type t =
    | Bin of int * int * int
    | Una of int * int * int list
    | Lex of int * int with sexp, compare

  val create_pcfg : Ptbtree.int_tree list -> (int,float) Hashtbl.t * (t,float) Hashtbl.t
  val priors_of_sexp : Sexp.t -> (int,float) Hashtbl.t
  val gram_of_sexp : Sexp.t -> (t,float) Hashtbl.t


  include Hashable.S with type t:= t
end = struct
  module T = struct
    type t =
      | Bin of int * int * int
      | Una of int * int * int list
      | Lex of int * int
    with sexp, compare

    let hash t =
      match t with
      | Bin(l,r1,r2) ->    (Int.hash l) lxor (Int.hash r1) lxor (Int.hash r2)
      | Una(l,r1,_path) -> (Int.hash l) lxor (Int.hash r1) (* and path ??*)
      | Lex(l,w)  ->       (Int.hash l) lxor (Int.hash w)
  end
  include T
  include Hashable.Make(T)

  type pcfg_rule = {rule: t; weight: float}

  let get_lhs t =
    match t with
    | Bin(l,_,_) -> l
    | Una(l,_,_) ->   l
    | Lex(l,_)  ->  l


  let rec compose_unaries path_length punaries =

    let rec compute_new originals candidates =
      match originals with
      | [] -> []
      | ( (Una(l,r,_),_) as original)::t ->
         let (replace,noreplace) = List.partition_tf candidates
           ~f:(function
           | Una(l',r',_),_ -> l=l' && r=r'
           | _ -> false) in
         let max = List.fold replace ~init:original
           ~f:(fun (max,pr) (r,pr') -> if pr' > pr then (r,pr') else (max,pr)) in
         max::(compute_new t noreplace)
      | _ -> failwith "nope"
    in

    if path_length = 0 then punaries
    else
      let newly_created =
        List.fold punaries ~init:[]
          ~f:(fun acc (u,pr) ->
            match u with
            | Una(l,r,pa) ->
               let pred = (function | (Una(l',_,_),_) -> l' = r | _ -> false ) in
               let compatibles = List.filter punaries ~f:pred in
               let compounds = List.fold
                 ~f:(fun acc' prule ->
                   match prule with
                   | (Una(l',r',pa'),pr') -> (Una(l,r',pa@l'::pa'), pr*.pr')::acc'
                   | _ -> acc' (*exception??*)
                 )
                 ~init:[] compatibles
               in
               compounds@acc
            | _ -> failwith "ill-formed rule"
          )
      in
      let new_punaries = compute_new punaries newly_created in
      compose_unaries (path_length - 1) new_punaries




  let create_pcfg tb =
    let get_daughter_id d =
      match d with
      | Tree.Node(q,_) -> q
      | Tree.Leaf(q,_) -> q
    in
    let get_rules t =
      let rec aux t =
        match t with
        | Tree.Leaf(p,w) -> [Lex(p,w)]
        | Tree.Node(n,[d]) ->
           let dn = get_daughter_id d in
           (Una(n,dn,[]))::(aux d)
        | Tree.Node(n,[d1;d2]) ->
           let dn1 = get_daughter_id d1 in
           let dn2 = get_daughter_id d2 in
           (Bin(n,dn1,dn2))::(aux d1)@(aux d2)
        | _ -> failwith "ill-formed tree"
      in aux t
    in
    let lrules = List.fold ~f:(fun acc t -> (get_rules t)@acc) ~init:[] tb in
    let rule_counts = Hashtbl.create ~hashable: hashable () in
    let lhs_counts = Hashtbl.create ~hashable: Int.hashable () in

    let incr_count = function
      | Some x -> Some(x+1)
      | None -> Some 1
    in
    let () =
      List.iter ~f:(fun rule ->
        let () = Hashtbl.change rule_counts rule incr_count in
        let lhs = get_lhs rule in
        let () = Hashtbl.change lhs_counts lhs incr_count in ()
      ) lrules
    in
    let total_nts = Hashtbl.fold ~f:(fun ~key:_ ~data:i acc -> i + acc) ~init:0 lhs_counts |> Float.of_int in
    let priors = Hashtbl.map lhs_counts ~f:(fun i -> Float.of_int(i) /. total_nts) in

    let pcfg_simple = Hashtbl.mapi rule_counts
      ~f:(fun ~key:rule ~data:count ->
        let lhs = get_lhs rule in
        Float.of_int(count) /. Float.of_int(Hashtbl.find_exn lhs_counts lhs)
      ) in

    let unaries,binlex = Hashtbl.partitioni_tf pcfg_simple
      ~f:(fun ~key:rule ~data:_ ->
        match rule with
        | Una(_,_,_) -> true
        | _ -> false
      ) in

    let unaries_simple = Hashtbl.to_alist unaries in

    let unaries_complete = compose_unaries 1 unaries_simple in
    let unaries_complete_hash = Hashtbl.of_alist_exn ~hashable: hashable unaries_complete in
    let pcfg = Hashtbl.merge unaries_complete_hash binlex
      ~f:(fun ~key:_  data ->
        match data with
        | `Both (_,_) -> failwith "both"
        | `Left a -> Some a
        | `Right b-> Some b
      )
    in
    (priors,pcfg)

  let priors_of_sexp sexp =
    let priors = Hashtbl.create ~hashable: Int.hashable () in
    let rec add_pairs l =
      match l with
      | [] -> priors
      | Sexp.List([Sexp.Atom h1;Sexp.Atom h2])::q ->
         let _ = Hashtbl.add priors ~key:(Int.of_string h1) ~data:(Float.of_string h2) in
         add_pairs q
      | _ -> failwith "add_pairs"
    in
    match sexp with
    | Sexp.List (q) -> add_pairs q
    | _ -> failwith "t_of_sexp"


  let gram_of_sexp sexp =
    let gram = Hashtbl.create ~hashable: hashable () in
    let rec add_pairs l =
      match l with
      | [] -> gram
      | Sexp.List([Sexp.List lr;Sexp.Atom h2])::q ->
         let _ = Hashtbl.add gram ~key:(t_of_sexp (Sexp.List lr)) ~data:(Float.of_string h2) in
         add_pairs q
      | _ -> failwith "add_pairs"
    in
    match sexp with
    | Sexp.List (q) -> add_pairs q
    | _ -> failwith "t_of_sexp"



end
