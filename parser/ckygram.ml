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
open Rule

  (* not really a trie because internal nodes do not have info *)
  (* all paths have the same length *)
module type Rule_trie =
sig
  type info = int * float * float * Rule.t with sexp, compare
  type t3   = info list with sexp, compare
  type t2   = (int * t3) list with sexp, compare
  type t    = (int * t2) list with sexp, compare
end

module Rule_trie =
  struct
    type info = int * float * float * Rule.t with sexp, compare
    type t3   = info list with sexp, compare
    type t2   = (int * t3) list with sexp, compare
    type t    = (int * t2) list with sexp, compare

    let (empty : t) = []

    let extract_rhs1 bin_rule_score_list =
      List.map bin_rule_score_list
        ~f:(fun (bin_rule,_) ->
          match bin_rule with
          | Rule.Bin(_,rhs1,_) ->
             rhs1
          | _ -> failwith "not a binary rule"
        )
    |> List.sort ~cmp:Int.compare
    |> List.remove_consecutive_duplicates ~equal:Int.equal


    let extract_rhs2 bin_rule_score_list =
      List.map bin_rule_score_list
        ~f:(fun (bin_rule,_) ->
          match bin_rule with
          | Rule.Bin(_,_,rhs2) ->
             rhs2
          | _ -> failwith "not a binary rule"
        )
    |> List.sort ~cmp:Int.compare
    |> List.remove_consecutive_duplicates ~equal:Int.equal


    let filter_rhs1 my_rhs1 bin_rule_score_list =
      List.fold bin_rule_score_list
        ~init:([],[])
        ~f:(fun (acc_pos,acc_neg) (bin_rule,score) ->
          match bin_rule with
          | Rule.Bin(_,rhs1,_) ->
             if rhs1 = my_rhs1
             then ((bin_rule,score)::acc_pos, acc_neg)
             else (acc_pos, (bin_rule,score)::acc_neg)
          | _ -> failwith "not a binary rule"
        )

    let filter_rhs2 my_rhs2 bin_rule_score_list =
      List.fold bin_rule_score_list
        ~init:([],[])
        ~f:(fun (acc_pos,acc_neg) (bin_rule,score) ->
          match bin_rule with
          | Rule.Bin(_,_,rhs2) ->
             if rhs2 = my_rhs2
             then ((bin_rule,score)::acc_pos, acc_neg)
             else (acc_pos, (bin_rule,score)::acc_neg)
          | _ -> failwith "not a binary rule"
        )

    let create bin_rule_score_list =
      let rhs1s = extract_rhs1 bin_rule_score_list in
      let (res :t) = List.map rhs1s
        ~f:(fun rhs1 ->
          let (rules,_) = filter_rhs1 rhs1 bin_rule_score_list in
          let rhs2s = extract_rhs2 rules in
          let (assoc : t2) = List.map rhs2s
            ~f:(fun rhs2 ->
              let (rrules,_) = filter_rhs2 rhs2 rules in
              let (triples : t3) = List.map rrules
                ~f:(fun (rule,score) ->
                  match rule with
                  | Rule.Bin(lhs,_,_) ->
                     (lhs,score, log score, rule)
                  | _ -> failwith "not a binary rule"
                ) in
              (rhs2,triples)
            )
          in
          rhs1, assoc
        )
      in
      res
  end

  (* rhs -> lhs * prob * rule *)
  type index = {lhs:int; score:float; logscore: float; rule:Rule.t}
  type unary_index =  (index list) Array.t

  (* word -> pos * prob * rule *)
  type lexical_index = ((int*float*float*Rule.t) list) Array.t

  (* (\* rhs1 -> rhs2 -> lhs * prob * rule *\) *)
  (* type binary_index  =   ((int*float* Rule.t) list) Array.t *)

  type t = { priors: (int,float) Hashtbl.t;
             lex: lexical_index;
             una: unary_index;
             unapos: unary_index;
             bin: Rule_trie.t;
             nts: int;
             ts:  int;
             nt_map: Int2StringMap.t;
             w_map: Int2StringMap.t;
           }

  let empty nt_map w_map =
    let nts = Int2StringMap.get_size nt_map in
    let ts =  Int2StringMap.get_size w_map in

    { priors = Hashtbl.create ~hashable: Int.hashable ();
      lex =  Array.create ~len:ts [];
      una =  Array.create ~len:nts [];
      unapos =  Array.create ~len:nts [];
      bin =  Rule_trie.empty;
      nt_map= nt_map;
      w_map= w_map;
      nts = nts;
      ts =  ts;
    }


  let initialize nt_map w_map rules p =

    let gram = empty nt_map w_map in
    let () = Hashtbl.merge_into ~src:p ~dst: gram.priors ~f:(fun ~key: _ d _ -> Some d)
    in

    let poslist = Hashtbl.fold rules ~init:[] ~f:(fun ~key:rule ~data:_ poslist ->
      match rule with
      | Rule.Lex (l,_) -> l::poslist
      | _ -> poslist) in


    Hashtbl.iter rules
      ~f:(fun ~key:rule ~data:prob ->
        match rule with
        | Rule.Lex(p,w) -> gram.lex.(w) <- (p,prob,log prob,rule)::gram.lex.(w)
        | Rule.Una(l,r,_) -> if List.exists poslist ~f:(fun p -> p = r)
          then gram.unapos.(r) <- {lhs = l; score = prob; logscore= log prob; rule = rule}::gram.unapos.(r)
          else gram.una.(r) <- {lhs = l; score = prob; logscore=log prob; rule = rule}::gram.una.(r)
        | Rule.Bin(_,_,_) -> () (* binary rules are processed below *)
      )
    ;

    let bins = Hashtbl.fold rules ~init:[]
      ~f:(fun ~key:rule ~data:prob acc ->
        match rule with
        | Rule.Bin(_,_,_) -> (rule,prob)::acc
        | _ -> acc
      )
    in
    { gram with bin = Rule_trie.create bins}


  let from_model_file verbose filename =
    let () = if verbose then fprintf Out_channel.stderr "loading model\n%!"  in

    let nt_map = Int2StringMap.empty () in
    let w_map = Int2StringMap.empty () in

    let (priors, rules) =
      match Sexp.load_sexp filename with
      | Sexp.List([n;w;p;g]) ->
         let () = Int2StringMap.load_from_sexp nt_map n in
         let () = Int2StringMap.load_from_sexp w_map w in
         let priors = Rule.priors_of_sexp p in
         let rules = Rule.gram_of_sexp g in
         (priors,rules)
      | _ -> failwith "unable to load model"
    in
    initialize nt_map w_map rules priors


  let nts t = t.nts
  let lexical_index t = t.lex
  let unary_frompos t = t.unapos
  let unary_index t = t.una
  let binary_index t = t.bin

  let priors t = t.priors
  let word_map t = t.w_map
  let nt_map t = t.nt_map
