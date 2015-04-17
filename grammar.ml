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
open Rule
open Int2stringmap
module Regex = Re2.Regex

(* not really a trie because internal nodes do not have info *)
(* all paths have the same length *)
module Rule_trie =
struct
  type info = int * float * Rule.t with sexp, compare
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
                   (lhs,score, rule)
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


(* TODO proper initialization *)
module Cky_gram =
struct
  (* rhs -> lhs * prob * rule *)
  type index = {lhs:int; score:float; rule:Rule.t}
  type unary_index =  (index list) Array.t

  (* word -> pos * prob * rule *)
  type lexical_index = ((int*float* Rule.t) list) Array.t

  (* (\* rhs1 -> rhs2 -> lhs * prob * rule *\) *)
  (* type binary_index  =   ((int*float* Rule.t) list) Array.t *)

  type t = { priors: (int,float) Hashtbl.t;
             lex: lexical_index;
             una: unary_index;
             unapos: unary_index;
             bin: Rule_trie.t;
             nts: int;
             ts:  int}

  let empty n_nt n_w = { priors = Hashtbl.create ~hashable: Int.hashable ();
                         lex =  Array.create ~len:n_w [];
                         una =  Array.create ~len:n_nt [];
                         unapos =  Array.create ~len:n_nt [];
                         bin =  Rule_trie.empty;
                         nts = n_nt; ts = n_w
                       }


  let initialize nt_map w_map rules p =

    let n_nts =Int2StringMap.get_size nt_map in
    let n_ws = Int2StringMap.get_size w_map in
  (* fprintf Out_channel.stderr "%d non-terminals\t%d terminals\n" n_nts n_ws; *)

    let gram = empty n_nts n_ws in
    let () = Hashtbl.merge_into ~src:p ~dst: gram.priors ~f:(fun ~key: _ d _ -> Some d)
    in

    let poslist = Hashtbl.fold rules ~init:[] ~f:(fun ~key:rule ~data:_ poslist ->
      match rule with
      | Rule.Lex (l,_) -> l::poslist
      | _ -> poslist) in


    Hashtbl.iter rules
      ~f:(fun ~key:rule ~data:prob ->
        match rule with
        | Rule.Lex(p,w) -> gram.lex.(w) <- (p,prob,rule)::gram.lex.(w)
        | Rule.Una(l,r,_) -> if List.exists poslist ~f:(fun p -> p = r)
          then gram.unapos.(r) <- {lhs = l; score = prob; rule = rule}::gram.unapos.(r)
          else gram.una.(r) <- {lhs = l; score = prob; rule = rule}::gram.una.(r)
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
end

open Cky_gram

module CKY =
struct

  type backpointer =
    | N
    | B of int * int * int * int (* left_cell_id * left_nt_id * right_cell_id * right_nt_id *)
    | U of int * int (* cell_id * nt_id *) (* TODO:  cell_id is  redundant *)

  type cell = ((float * ((Rule.t * backpointer) list)) option) Array.t

  (* type entry = {empty: bool; score:float; hist:(Rule.t * backpointer) list} *)
  (* type cell =  entry Array.t *)

  (* let empty_entry () = {empty = true; score = Float.neg_infinity;
     hist = []} *)
  let empty_entry () = None
  let empty_cell n = Array.init n ~f:(fun _ -> empty_entry ())

  let split_regex = Regex.create_exn " +"

  (* (\* 400 words / 100 nt symbols maximum *\) *)
  (* let (chart : (cell Array.t)) = Array.init (400 * 401 / 2) ~f:(fun _ -> empty_cell 100) *)

  (* let reinit_chart size nts = *)
  (*   let iter_size id_size = *)
  (*     if id_size = size *)
  (*     then () *)
  (*     else *)
  (*       let cell = Array.unsafe_get chart id_size in *)
  (*       Array.fill cell ~pos:0 ~len:nts None *)
  (*   in *)
  (*   iter_size 0 *)


  let parse_line w_map gram line =

    (* Hashtbl.iter gram.priors *)
    (*   ~f:(fun ~key:nt ~data: pr -> *)
    (*     fprintf Out_channel.stderr "prior: %d %f\n%!" nt pr *)
    (*   ); *)

    let line = String.strip line in
    let tokens = Array.of_list
      (List.map (Regex.split split_regex line)
         ~f:(fun str ->
           match Int2StringMap.str2int_safe w_map str with
           | -1 -> (str, Int2StringMap.str2int_safe w_map "UNK") (* TODO:  prepare for other jokers *)
           | n ->(str, n)
         )
      ) in

    (* Array.iteri tokens *)
    (*   ~f:(fun i (str,id) -> *)
    (*     fprintf Out_channel.stderr "token %d %s %d  %s\n%!" i str id (Int2StringMap.int2str w_map id) *)
    (*   ); *)


    let n = Array.length tokens in
    let access i j = i + ((j-i)*(2*n-(j-i)+ 1))/2 in
    let size = n*(n+1)/2 in

    (* fprintf Out_channel.stderr "size chart: %d\n%!" size; *)
    let chart = Array.init size ~f:(fun _ -> empty_cell gram.nts) in
    (* let () = reinit_chart size gram.nts in *)


    (* initialize charts *)
    for i = 0 to (n-1) do
      let cell_i = access i i in
      (* fprintf Out_channel.stderr "position is %d -> in chart %d\n%!" i cell_i ; *)
      let w_id = snd tokens.(i) in

      (* fprintf Out_channel.stderr "token is %d (%s)\n%!" w_id (fst tokens.(i)); *)

      let cell = Array.unsafe_get chart cell_i in

      (* Array.iteri cell *)
      (*   ~f:(fun j entry -> *)
      (*     match entry with *)
      (*     | None ->  fprintf Out_channel.stderr "entry %d is empty\n%!" j *)
      (*     | match -> _ -> fprintf Out_channel.stderr "entry %d is not empty\n%!" j *)
      (*   ); *)


      List.iter gram.lex.(w_id)
        ~f:(fun (pos,prob,rule) ->

        let s = Some(prob, [(rule,N)])
        in
        (* let s = Some (prob,[]) in *)
        (* fprintf Out_channel.stderr "pos %d id %d score %f\n%!" i pos prob; *)
        Array.unsafe_set cell pos s;
        List.iter gram.unapos.(pos)
          ~f:(fun {lhs = lhs; score = prob_lhs; rule = rule_lhs} ->
            let bp = rule_lhs, U(cell_i, pos) in
            let newval =
              match Array.unsafe_get cell lhs with
                | None -> Some(prob_lhs, [bp])
                | Some(prob', l') -> Some(prob_lhs +. prob', bp::l')
            in Array.unsafe_set cell lhs newval
          )
      )
    done;

    for width = 1 to n do
    (* fprintf Out_channel.stderr "width: %d\n%!" width; *)
      for start = 0 to n-1 do
        let lend = start + width in
        if lend < n then
          let cell_id = (access start lend) in
          let cell = Array.unsafe_get chart cell_id in

          (* fprintf Out_channel.stderr "cell: %d %d\n%!" start lend; *)

          (* binary rules *)
          for m = start to lend - 1 do

            let left_cell_id = access start m in
            let left_cell = Array.unsafe_get chart left_cell_id in

            (*     Array.iteri left_cell *)
            (* ~f:(fun j entry -> *)
            (*   match entry with *)
            (*   | None ->  fprintf Out_channel.stderr "entry %d is empty\n%!" j *)
            (*   |_ -> fprintf Out_channel.stderr "entry %d is not empty\n%!" j *)
            (* ); *)

            let right_cell_id = access (m+1) lend in
            let right_cell= Array.unsafe_get chart right_cell_id in

            (*     Array.iteri right_cell *)
            (* ~f:(fun j entry -> *)
            (*   match entry with *)
            (*   | match ->  None ->  fprintf Out_channel.stderr "entry %d is empty\n%!" j *)
            (*   |_ -> fprintf Out_channel.stderr "entry %d is not empty\n%!" j *)
            (* ); *)


            List.iter gram.bin
              ~f:(fun (r1,info1) ->
                match Array.unsafe_get left_cell r1 with
                | None -> ()
                | Some(left_score,_) ->
                   List.iter info1
                    ~f:(fun (r2,info2) ->
                      match Array.unsafe_get right_cell r2 with
                      | None -> ()
                      | Some(right_score,_) ->
                         let lr_score = left_score *. right_score in
                         List.iter  info2
                           ~f:(fun (lhs,rule_score,rule) ->
                             let score = lr_score *. rule_score in
                             let bp = (rule, B(left_cell_id,r1, right_cell_id, r2)) in
                             let new_val =
                               match Array.unsafe_get cell lhs with
                               | None ->
                                  Some(score,[bp])
                                (* Some(score,[]) *)
                               | Some(score',l') ->
                                  Some(score +. score', bp::l')
                              (* Some(score +. score', []) *)
                             in
                             Array.unsafe_set cell lhs new_val
                           )
                    )
              )

          done;
        (* [?  |  ?]et () = printf "here1\n%!" in *)
          (*unary rules*)
          let unary_cell = empty_cell (Array.length cell) in
          Array.iteri cell
            ~f:(fun i entry ->
              match entry with
              | None -> ()
              |Some(score, _) ->
                 List.iter gram.una.(i)
                   ~f:(fun {lhs = lhs; score = rule_score; rule = rule} ->
                     let update_score = score *. rule_score in
                   let bp = (rule, U(cell_id, i)) in
                     let new_val =
                       match Array.unsafe_get unary_cell lhs with
                       | None ->
                          Some (update_score, [bp])
                          (* some (update_score, []) *)
                       | Some(score', l) ->
                          Some(score' +. update_score, bp::l)
                          (* Some(score' +. update_score, []) *)
                     in
                     Array.unsafe_set unary_cell lhs new_val
                   )
            );

          Array.iteri unary_cell
            ~f:( fun i uentry ->
              match uentry with
              | None -> ()
              | Some(uscore,uhists) ->
                 let new_val =
                   match Array.unsafe_get cell i with
                   | None -> uentry
                   | Some(score,hists) ->
                      Some(score +. uscore, List.rev_append uhists hists)
                      (* Some(score +. uscore, []) *)
                 in
                 Array.unsafe_set cell i new_val
            );

    (* Array.iteri cell *)
    (*   ~f:(fun j entry -> *)
    (*     match entry with *)
    (*     | None ->  fprintf Out_channel.stderr "entry %d is empty\n%!" j *)
    (*     |_ -> fprintf Out_channel.stderr "entry %d is not empty\n%!" j *)
    (*   ); *)

        (* (\* pruning cell inside * priors : and keep the top 20 ??? *\) *)
        (* (\* fprintf Out_channel.stderr "pruning:\n"; *\) *)
        (* let entries2remove = *)
        (*   Array.foldi cell ~init:[] *)
        (*     ~f:(fun lhs acc entry  -> *)
        (*       match entry with *)
        (*       | None -> acc *)
        (*       | Some (score,_) -> *)
        (*          (\* fprintf Out_channel.stderr "%f %f %f\n" (score) (Hashtbl.find_exn gram.priors lhs) (score *. Hashtbl.find_exn gram.priors lhs); *\) *)
        (*          (score *. Hashtbl.find_exn gram.priors lhs, lhs)::acc *)
        (*     ) *)
        (*     |> List.sort ~cmp: (fun (score1,_) (score2,_) -> Float.compare score2 score1) *)
        (*     |> (fun l -> List.drop l 10) *)


          (* pruning cell inside * priors : and keep the entries >
           \alpha max *)
          (* fprintf Out_channel.stderr "pruning:\n"; *)
          let entries2remove =
            let (entries,max_score) =
              Array.foldi cell ~init:([],0.0)
                ~f:(fun lhs (acc,max) entry  ->
                  match entry with
                  | None -> (acc,max)
                  | Some (score,_) ->
                     let t = score *. Hashtbl.find_exn gram.priors lhs in
                     let max' = if t > max then t else max in
                     (* fprintf Out_channel.stderr "%f %f %f\n" (score) (Hashtbl.find_exn gram.priors lhs) (score *. Hashtbl.find_exn gram.priors lhs); *)
                     ((t, lhs)::acc,max')
                )
            in
            let threshold = 0.0005 in
            let max_score = max_score *. threshold in
            List.filter entries
              ~f:(fun (score,_) -> score < max_score)

          in
          List.iter entries2remove
            ~f:(fun (_,lhs) ->
              Array.unsafe_set cell lhs None
            )

      done;
    done;

    (* let () = printf "here\n%!" in *)
    ()

  let parse_file w_map gram file =

    (In_channel.with_file file ~f:
       (fun ic ->
         let i = ref 0 in
         fprintf Out_channel.stderr "start\n%!";
         In_channel.iter_lines ic
           ~f:(fun line ->
             parse_line w_map gram line;
             i := !i + 1;
             if (!i mod 50) = 0
             then fprintf Out_channel.stderr "sentence: %d\n%!" !i;
           )
       )
    )
end
