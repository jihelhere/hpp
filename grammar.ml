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



(* TODO proper initialization *)
module Cky_gram =
struct

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


module type BackPointer =
sig
  type t

  val lexical : t
  val unary   : int -> int -> t
  val binary  : int -> int -> int -> int -> t
end

module type Cell =
sig
  type entry
  type t

  val create_empty : int -> t
  val add_lexical:   t -> int -> float -> Rule.t -> unit
  val add_unary  :   t -> int -> float -> Rule.t -> int -> unit
  val add_binary :   t -> int -> float -> Rule.t -> int -> int -> int -> int -> unit


  val length : t -> int
  val get : t -> int -> entry
  val is_empty_entry : entry -> bool
  val get_entry_inside_score : entry -> float

  val transfer_inside : t -> unit

  val iteri : t -> f:(int -> entry ->unit) -> unit
  val prune_priors : t -> (int,float) Hashtbl.t -> threshold:float -> unit
  val prune_group :  t -> (int,float) Hashtbl.t -> size:int -> unit

  val reset : t -> int -> unit

  val stat : int -> int -> t -> unit
end

module HistCell (BP : BackPointer) : Cell=
struct
  type scores = {mutable inside_score:float;
                 mutable unary_inside_score:float}
  type entry = {mutable init:bool;
                mutable scores:scores;
                mutable uhist: (Rule.t * BP.t) list;
                mutable bhist: (Rule.t * BP.t) list;
                mutable lhist: Rule.t option
               }
  type t = entry Array.t

  let empty_entry () = {init=false;
                        scores={inside_score=0.0; unary_inside_score=0.0};
                        bhist=[]; lhist=None; uhist=[];
                       }
  let create_empty n = Array.init n ~f:(fun _ -> empty_entry ())

  (* let add_lexical t pos score rule  = *)
  (*   let entry = {ventry with *)
  (*     init =true; *)
  (*     scores = {vscores with inside_score=score}; *)
  (*     lhist=Some rule} in *)
  (*   Array.unsafe_set t pos entry *)


  let add_lexical t pos score rule  =
    let entry = Array.unsafe_get t pos in
    entry.init <- true;
    entry.scores.inside_score <- score
    ;
    entry.lhist <- Some rule


  (* let add_unary (t : t) lhs score rule celli = *)
  (*   let rbp = rule, (BP.unary celli lhs) in *)
  (*   let newval = *)
  (*     let oldval = Array.unsafe_get t lhs in *)
  (*     if oldval.init then {oldval with *)
  (*       scores={oldval.scores with unary_inside_score=oldval.scores.unary_inside_score +. score}; *)
  (*       uhist=rbp::oldval.uhist} *)
  (*     else {ventry with *)
  (*       init=true; *)
  (*       scores={inside_score=oldval.scores.inside_score; unary_inside_score=score}; *)
  (*       uhist=[rbp]} *)
  (*   in Array.unsafe_set t lhs newval *)


  let add_unary (t : t) lhs score rule celli =
    let entry = Array.unsafe_get t lhs in
    let rbp = rule, (BP.unary celli lhs) in
    entry.init <- true;
    entry.scores.unary_inside_score <- entry.scores.unary_inside_score +. score
    ;
    entry.uhist <- rbp::entry.uhist


  (* let add_binary (t : t) lhs score rule lcelli llhs rcelli rlhs = *)
  (*   let rbp = rule, (BP.binary lcelli llhs rcelli rlhs) in *)
  (*   let newval = *)
  (*     let oldval = Array.unsafe_get t lhs in *)
  (*     if oldval.init then {oldval with *)
  (*       scores={oldval.scores with inside_score=score +. oldval.scores.inside_score}; *)
  (*       bhist=rbp::oldval.bhist} *)
  (*     else {ventry with *)
  (*       init=true; *)
  (*       scores={inside_score=score; unary_inside_score=oldval.scores.unary_inside_score}; *)
  (*       bhist=[rbp]} *)
  (*   in Array.unsafe_set t lhs newval *)


  let add_binary (t : t) lhs score rule lcelli llhs rcelli rlhs =
    let entry =  Array.unsafe_get t lhs in
    let rbp = rule, (BP.binary lcelli llhs rcelli rlhs) in
    entry.init <- true;
    entry.scores.inside_score <- entry.scores.inside_score +. score
    ;
    entry.bhist <- rbp::entry.bhist

  let length t = Array.length t

  let is_empty_entry entry = entry.init = false

  let get_entry_inside_score entry = entry.scores.inside_score

  let get t lhs = Array.unsafe_get t lhs

  let iteri t ~f = Array.iteri t ~f

  let nb_active t = Array.fold t ~init:0
    ~f:(fun acc entry ->
      if entry.init then acc+1 else acc)

  (* pruning cell inside * priors : and keep the entries >  \alpha max *)
  let prune_priors t priors ~threshold =

    (* let a1 = Float.of_int(nb_active t) in *)


    let (entries,max_score) =
      Array.foldi t ~init:([],0.0)
        ~f:(fun lhs (acc,m) entry  ->
          if not entry.init then (acc,m)
          else
            let tmp = entry.scores.inside_score *. Hashtbl.find_exn priors lhs in
            let max' = max tmp m in
            ((tmp, lhs)::acc,max')
        )
    in
    let max_score = max_score *. threshold in
    List.iter entries
      ~f:(fun (score,lhs) -> if score < max_score then t.(lhs).init <- false)
  (* ; *)
  (* let a2 = Float.of_int(nb_active t) in *)
  (* if a1 <> 0.0 *)
  (* then  printf "after pruning: %f\n%!"  (100.0 *. a2/. a1) *)
  (* else  printf "cell was empty\n%!" *)

  (* pruning cell inside * priors : and keep the top ~size ??? *)
  let prune_group t priors ~size =
    let entries2remove =
      Array.foldi t ~init:[]
        ~f:(fun lhs acc entry  ->
          if not entry.init then acc
          else (entry.scores.inside_score *. Hashtbl.find_exn priors lhs, lhs)::acc
        )
           |> List.sort ~cmp: (fun (score1,_) (score2,_) -> Float.compare score2 score1)
           |> (fun l -> List.drop l size)
    in
    List.iter entries2remove
      ~f:(fun (_,lhs) ->
        Array.unsafe_set t lhs (empty_entry ())
      )


  let reset t size =
    (*not sure this avoids copying*)
    Array.fill t ~pos:0 ~len:size (empty_entry ())

  (* let transfer_inside t = *)
  (*   Array.iteri t *)
  (*     ~f:(fun lhs entry -> *)
  (*       if entry.init *)
  (*       then *)
  (*         Array.unsafe_set t lhs {entry with scores={inside_score=entry.scores.inside_score +. entry.scores.unary_inside_score; *)
  (*                                                    unary_inside_score = 0.0}} *)
  (*     ) *)


  let transfer_inside t =
    Array.iter t
      ~f:(fun entry ->
        if entry.init
        then
          (entry.scores.inside_score <-entry.scores.inside_score +. entry.scores.unary_inside_score;
           entry.scores.unary_inside_score <- 0.0)
      )


  let stat s e t =
 (* () *)
    let empty = Array.fold t ~init:true
      ~f:(fun acc entry ->
        if not acc then
          false
        else
          if entry.init then
            false
          else
            true
      )
    in
    if empty
    then Printf.printf "cell (%d,%d) is empty\n%!" s e
    else
      Array.iteri t
        ~f:(fun i entry ->
          if entry.init
          then
      (* Printf.printf "entry (%d,%d) %d: score %f, hist. length: (%d,%d,%d)\n%!" *)
      (*   s e i entry.scores.inside_score *)
      (*   (List.length entry.bhist) (if entry.lhist = None then 0 else 1) (List.length entry.uhist) *)
            Printf.printf "entry (%d,%d) %d: score %f\n%!"
              s e i entry.scores.inside_score
        )

end



module BasicCell : Cell=
struct
  type entry = float  option
  type t = entry Array.t

  let empty_entry () = None
  let create_empty n = Array.init n ~f:(fun _ -> empty_entry ())

  let add_lexical t pos score _rule  =
    let entry = Some score in
    Array.unsafe_set t pos entry

  let add (t : t) lhs score _rule _bp =
    let newval =
      match Array.unsafe_get t lhs with
      | None -> Some score
      | Some score' -> Some (score +. score')
    in Array.unsafe_set t lhs newval

  let add_unary  t lhs score _rule _celli                      = add t lhs score () ()
  let add_binary t lhs score _rule _lcelli _llhs _rcelli _rlhs = add t lhs score () ()


  let length t = Array.length t

  let is_empty_entry entry = entry = None

  let get_entry_inside_score entry =
    match entry with
      None -> failwith "I get dead"
    | Some s -> s


  let get t lhs = Array.unsafe_get t lhs

  let iteri t ~f =
    Array.iteri t ~f

  (* pruning cell inside * priors : and keep the entries >  \alpha max *)
  let prune_priors t priors ~threshold =
    let entries2remove =
      let (entries,max_score) =
        Array.foldi t ~init:([],0.0)
          ~f:(fun lhs (acc,max) entry  ->
            match entry with
            | None -> (acc,max)
            | Some score ->
               let tmp = score *. Hashtbl.find_exn priors lhs in
               let max' = if tmp > max then tmp else max in
               (* fprintf Out_channel.stderr "%f %f %f\n"
                  (score) (Hashtbl.find_exn gram.priors lhs) (score *.
                  Hashtbl.find_exn gram.priors lhs); *)
               ((tmp, lhs)::acc,max')
          )
      in
      let max_score = max_score *. threshold in
      List.filter entries
        ~f:(fun (score,_) -> score < max_score)
    in
    List.iter entries2remove
      ~f:(fun (_,lhs) ->
        Array.unsafe_set t lhs None
      )


  (* pruning cell inside * priors : and keep the top 20 ??? *)
  let prune_group t priors ~size =
    let entries2remove =
      Array.foldi t ~init:[]
        ~f:(fun lhs acc entry  ->
          match entry with
          | None -> acc
          | Some score ->
             (* fprintf Out_channel.stderr "%f %f %f\n" (score) (Hashtbl.find_exn gram.priors lhs) (score *. Hashtbl.find_exn gram.priors lhs); *)
             (score *. Hashtbl.find_exn priors lhs, lhs)::acc
        )
    |> List.sort ~cmp: (fun (score1,_) (score2,_) -> Float.compare score2 score1)
    |> (fun l -> List.drop l size)
    in
    List.iter entries2remove
      ~f:(fun (_,lhs) ->
        Array.unsafe_set t lhs None
      )


  let reset t size =
    Array.fill t ~pos:0 ~len:size None


  let transfer_inside _t =
    failwith "not implemented"

  let stat _s _e _t =
    ()

end




module CKYBackPointer : BackPointer =
struct
  type t =
    | N
    | B of int * int * int * int (* left_cell_id * left_nt_id * right_cell_id * right_nt_id *)
    | U of int * int (* cell_id * nt_id *) (* TODO:  cell_id is  redundant *)

  let lexical = N
  let unary  cell_idx pos =
    U(cell_idx,pos)

  let binary lcell_idx lpos rcell_idx rpos =
    B(lcell_idx, lpos, rcell_idx, rpos)
end



module MakeCKY (Cell : Cell)=
struct

  let split_regex = Regex.create_exn " +"

  (* (\* 400 words / 100 nt symbols maximum *\) *)
  (* let chart = Array.init (400 * 401 / 2) ~f:(fun _ -> Cell.create_empty 100) *)

  (* let reinit_chart size nts = *)
  (*   let rec iter_size id_size = *)
  (*     if id_size = size *)
  (*     then () *)
  (*     else *)
  (*       let cell = Array.unsafe_get chart id_size in *)
  (*       Cell.reset cell nts; *)
  (*       iter_size (id_size + 1) *)
  (*   in *)
  (*   iter_size 0 *)


  let build_forest gram tokens =

    let n = Array.length tokens in

    let access i j = i + ((j-i)*(2*n-(j-i)+ 1))/2 in
    (* equivalent to access i i*)
    let access_lex i = i in

    let size = n*(n+1)/2 in

    (* fprintf Out_channel.stderr "size chart: %d\n%!" size; *)
    let chart = Array.init size ~f:(fun _ -> Cell.create_empty gram.nts) in
    (* let () = reinit_chart size gram.nts in *)


    (* initialize charts *)
    Array.iteri tokens
      ~f:(fun i token ->
        let cell_i = access_lex i in
        let w_id = snd token in
        let cell = Array.unsafe_get chart cell_i in

        List.iter gram.lex.(w_id)
          ~f:(fun (pos,prob,rule) ->
            (* add T -> w *)
            Cell.add_lexical cell pos prob rule;

            (* add rules L -> T *)
            List.iter gram.unapos.(pos)
              ~f:(fun index_ur -> Cell.add_unary cell index_ur.lhs index_ur.score index_ur.rule cell_i)
          );
        Cell.transfer_inside cell
      );



    let process_binaries cell start lend =
      let add_to_cell = Cell.add_binary cell
      in
      let rec recprocess_binaries m =
        if m = lend then ()
        else
          let left_cell_id = access start m in
          let left_cell = Array.unsafe_get chart left_cell_id in
          let right_cell_id = access (m+1) lend in
          let right_cell= Array.unsafe_get chart right_cell_id in
          let left_get = Cell.get left_cell in
          let right_get = Cell.get right_cell in
          let () =
            List.iter gram.bin
              ~f:(fun (r1,info1) ->
                let lentry = left_get r1 in
                if Cell.is_empty_entry lentry then ()
                else
                  let left_score = Cell.get_entry_inside_score lentry in
                  List.iter info1
                    ~f:(fun (r2,info2) ->
                      let rentry = right_get r2 in
                      if Cell.is_empty_entry rentry then ()
                      else
                        let right_score = Cell.get_entry_inside_score rentry in
                        let lr_score = left_score *. right_score in
                        List.iter  info2
                          ~f:(fun (lhs,rule_score,rule) ->
                            let score = lr_score *. rule_score in
                            add_to_cell lhs score rule left_cell_id r1 right_cell_id r2
                          )
                    )
              )
          in
          recprocess_binaries (m+1)
      in
      recprocess_binaries start
    in

    (* let process_unary cell = *)
    (*   let unary_cell = Cell.create_empty (Cell.length cell) in *)
    (*   let add_to_cell = Cell.add_unary unary_cell *)
    (*   in *)
    (*   Cell.iteri cell *)
    (*     ~f:(fun i entry -> *)
    (*       if Cell.is_empty_entry entry *)
    (*       then () *)
    (*       else *)
    (*         let score = Cell.get_entry_score entry in *)
    (*         List.iter gram.una.(i) *)
    (*           ~f:(fun unary_index -> *)
    (*             let update_score = score *. unary_index.score in *)
    (*             add_to_cell unary_index.lhs update_score unary_index.rule i *)
    (*           ) *)
    (*     ); *)
    (*   Cell.add_in_place cell unary_cell *)
    (* in *)

    let process_unary cell =
      Cell.iteri cell
        ~f:(fun rhs entry ->
          if Cell.is_empty_entry entry
          then ()
          else
            let score = Cell.get_entry_inside_score entry in
            List.iter gram.una.(rhs)
              ~f:(fun unary_index ->
                let unary_score = score *. unary_index.score in
                Cell.add_unary cell unary_index.lhs unary_score unary_index.rule rhs
              )
        );
      Cell.transfer_inside cell
    in


    let visit_spans sent_length width =
      let rec rec_visit_spans start =

        if start = sent_length then ()
        else
          let lend = start + width in
          if lend < sent_length then
            let cell_id = (access start lend) in
            let cell = Array.unsafe_get chart cell_id in
            let () =
              (* binary rules *)
              process_binaries cell start lend;

              (*unary rules*)
              process_unary cell;

              (* fprintf Out_channel.stderr "pruning:\n"; *)
              Cell.prune_priors cell gram.priors ~threshold:0.001

            (* ; *)
            (* Cell.stat start lend cell *)

            in
            rec_visit_spans (start + 1)
      in
      rec_visit_spans 0
    in

    let iter_widths sentence_length =
      let rec aux width =
        if width > n then ()
        else
          let () = visit_spans sentence_length width in
          aux (width + 1)
      in
      aux 0
    in
    iter_widths n




  let parse_line w_map gram line =

    (* Hashtbl.iter gram.priors *)
    (*   ~f:(fun ~key:nt ~data: pr -> *)
    (*     fprintf Out_channel.stderr "prior: %d %f\n%!" nt pr *)
    (*   ); *)

    let line = String.strip line in
    let tokens =
      List.map (Regex.split split_regex line)
        ~f:(fun str ->
          match Int2StringMap.str2int_safe w_map str with
          | -1 -> (str, Int2StringMap.str2int_safe w_map "UNK") (* TODO:  prepare for other jokers *)
          | n ->(str, n)
        )
      |> Array.of_list
    in

    (* Array.iteri tokens *)
    (*   ~f:(fun i (str,id) -> *)
    (*     fprintf Out_channel.stderr "token %d %s %d  %s\n%!" i str id (Int2StringMap.int2str w_map id) *)
    (*   ); *)
    build_forest gram tokens



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
