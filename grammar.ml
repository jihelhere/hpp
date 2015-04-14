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

(* this would work of all type of rules *)
(* not really a trie because internal nodes do not have info *)
(* all paths have the same length *)
module Rule_trie =
struct
  type t =
    | Leaf of float * Rule.t
    | Node of (int * t) list with sexp, compare

  let empty = Node []

  let add_rule t r pr =
  let rec  add_rulelist t l r pr =
    match l with
    | [] -> Leaf (pr,r)
    | hd::tl ->
       match t with
       | Leaf(_,_) -> failwith "ill-formed trie"
       | Node ds ->
          let (before,after) = List.split_while ds ~f:(fun (e,_) -> e < hd) in
          match after with
          | [] -> Node (ds@[(hd, add_rulelist empty tl r pr)])
          | (e,f)::es ->
             if e > hd
             then
               Node (before@((hd, add_rulelist empty tl r pr)::after))
             else (*e == hd *)
               Node (before@((hd, add_rulelist f tl r pr)::es))
  in
  match r with
  | Rule.Bin(l,r1,r2) -> add_rulelist t [r1;r2;l] r pr
  | _ -> failwith "todo"

end

(* rhs -> lhs * prob * rule *)
type unary_index =  ((int*float* Rule.t) list) Array.t

(* word -> pos * prob * rule *)
type lexical_index = ((int*float* Rule.t) list) Array.t

(* rhs1 -> rhs2 -> lhs * prob * rule *)
type binary_index  =   ((int*float* Rule.t) list) Array.t

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
        then gram.unapos.(r) <- (l,prob,rule)::gram.unapos.(r)
        else gram.una.(r) <- (l,prob,rule)::gram.una.(r)
      | Rule.Bin(_,_,_) -> () (* binary rules are processed below *)
    )
  ;

  let bins = Hashtbl.fold rules ~init:Rule_trie.empty
    ~f:(fun ~key:rule ~data:prob trie ->
      match rule with
      | Rule.Bin(_,_,_) -> Rule_trie.add_rule trie rule prob
      | _ -> trie
    )
  in
  { gram with bin = bins }


(*************************)
type backpointer =
  | N
  | B of int * int * int * int (* left_cell_id * left_nt_id * right_cell_id * right_nt_id *)
  | U of int * int (* cell_id * nt_id *) (* TODO:  cell_id is  redundant *)

type cell = ((float * ((Rule.t * backpointer) list)) option) Array.t


let empty_cell n = Array.create ~len: n None

let split_regex = Regex.create_exn " +"


(* 400 words / 100 nt symbols maximum *)
let (chart : (cell Array.t)) = Array.init (400 * 401 / 2) ~f:(fun _ -> empty_cell 100)

let reinit_chart size nts =
  let rec iter_nts cell id_nts =
    if id_nts = nts
    then ()
    else
      let () = Array.unsafe_set cell id_nts None in
      iter_nts cell (id_nts + 1)
  in
  let rec iter_size id_size =
    if id_size = size
    then ()
    else
      let cell = Array.unsafe_get chart id_size in
      let () = iter_nts cell 0 in
      iter_size (id_size + 1) in
  iter_size 0



let parse_line nt_map w_map gram line =

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
  (* let chart = Array.init size ~f:(fun _ -> empty_cell gram.nts) in *)
  let () = reinit_chart size gram.nts in


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
    (*     |_ -> fprintf Out_channel.stderr "entry %d is not empty\n%!" j *)
    (*   ); *)


    List.iter gram.lex.(w_id)
      ~f:(fun (pos,prob,rule) ->
        (* let s = Some (prob,[(rule,N)]) in *)
        let s = Some (prob,[]) in
        (* fprintf Out_channel.stderr "pos %d id %d score %f\n%!" i pos prob; *)
        Array.unsafe_set cell pos s;
        List.iter gram.unapos.(pos)
          ~f:(fun (lhs,prob_lhs,rule_lhs) ->
            let newval =
              match Array.unsafe_get cell lhs with
              | None ->
                 (* fprintf Out_channel.stderr "unary empty pos %d rule %d -> %d  score %f\n%!" i lhs pos prob; *)
                 (* Some(prob_lhs,[rule_lhs,U(cell_i, pos)]) *)
                 Some(prob_lhs,[])
              | Some(prob',l') ->
                 (* fprintf Out_channel.stderr "unary not empty pos %d rule %d -> %d  prev. score %f rule score %f new score %f \n%!" i lhs pos prob' prob_lhs (prob_lhs +. prob'); *)
                 (* Some(prob_lhs +. prob',(rule_lhs,U(cell_i, pos))::l') *)
                   Some(prob_lhs +. prob',[])
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
      (*   | None ->  fprintf Out_channel.stderr "entry %d is empty\n%!" j *)
      (*   |_ -> fprintf Out_channel.stderr "entry %d is not empty\n%!" j *)
      (* ); *)



          let extract_indexed_list n = match n with Rule_trie.Node l -> l | _ -> failwith "problem with bin rules" in

          List.iter (extract_indexed_list gram.bin)
            ~f:(fun (r1,info1) ->
              match Array.unsafe_get left_cell r1 with
                  | None -> ()
                  | Some(left_score,_) ->
                     List.iter (extract_indexed_list info1)
                       ~f:(fun (r2,info2) ->
                         match Array.unsafe_get right_cell r2 with
                              | None -> ()
                              | Some(right_score,_) ->
                                 let lr_score = left_score *. right_score in
                                 List.iter (extract_indexed_list info2)
                                   ~f:(fun (lhs,info3) ->
                                     match info3 with
                                     | Rule_trie.Leaf(rule_score,rule) ->
                                        let score = lr_score *. rule_score in
                                        let bp = (rule, B(left_cell_id,r1, right_cell_id, r2)) in
                                        let new_val =
                                          match Array.unsafe_get cell lhs with
                                          | None -> (* Some(score,[bp]) *)
                                             Some(score,[])
                                          | Some(score',l') -> (* Some(score +. score', bp::l') *)
                                             Some(score +. score', [])
                                        in
                                        Array.unsafe_set cell lhs new_val

                                     | _ -> failwith "problem with bin rules (last level)"

                                   )
                       )
            )

        done;
          (* let () = printf "here1\n%!" in *)
          (*unary rules*)
        let unary_cell = empty_cell (Array.length cell) in
        Array.iteri cell
          ~f:(fun i entry ->
            match entry with
            | None -> ()
            |Some(score, _) ->
               List.iter gram.una.(i)
                 ~f:(fun (lhs,rule_score,rule) ->
                   let update_score = score *. rule_score in
                   let bp = (rule, U(cell_id, i)) in
                   let new_val =
                     match Array.unsafe_get unary_cell lhs with
                     | None -> (* Some (update_score, [bp]) *)
                        Some (update_score, [])
                     | Some(score', l) -> (* Some(score' +. update_score, bp::l) *)
                        Some(score' +. update_score, [])
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
                 | Some(score,hists) -> (* Some(score +. uscore, List.rev_append uhists hists) *)
                    Some(score +. uscore, [])
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
          let threshold = 0.0000 in
          let max_score = max_score *. threshold in
          List.filter entries
          ~f:(fun (score,lhs) -> score < max_score)

        in
        List.iter entries2remove
          ~f:(fun (_,lhs) ->
            Array.unsafe_set cell lhs None
          )



    done;
  done;


  (* let () = printf "here\n%!" in *)
  ()




let parse_file nt_map w_map gram file =
  let i = ref 0 in
  (In_channel.with_file file ~f:
     (fun ic ->
       In_channel.iter_lines ic
         ~f:(fun line ->
           let () = i := !i + 1 in
           fprintf Out_channel.stderr "sentence: %d\n%!" !i;
           parse_line nt_map w_map gram line
         )
     )
  )
