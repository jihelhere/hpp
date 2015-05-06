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
module Regex = Re2.Regex


open Rule
open Int2stringmap
open Tree
open Ptbtree
open Ckygram
open Backpointer

module type Cell =
sig
  type entry
  type t

  val create_empty : int -> t
  val add_lexical:   t -> int -> float -> unit
  val add_unary  :   t -> int -> float -> unit
  val add_binary :   t -> int -> float -> unit

  val decode_lexical : t -> int -> Rule.t -> int -> float -> bool
  val decode_unary   : t -> Rule.t -> int -> float -> float -> unit
  val decode_binary  : t -> Rule.t -> int -> int -> int -> float -> float -> unit


  val length : t -> int
  val get : t -> int -> entry
  val is_empty_entry : entry -> bool
  val get_entry_inside_score_exn : entry -> float

  val get_entry_max_score_exn : entry -> float
  val get_entry_inside_score : entry -> float option
  val get_entry_max_score : entry -> float option

  val transfer_inside : t -> unit
  val transfer_max : t -> unit

  val iteri : t -> f:(int -> entry ->unit) -> unit
  val prune_priors : t -> (int,float) Hashtbl.t -> threshold:float -> bool
  val prune_group :  t -> (int,float) Hashtbl.t -> size:int -> bool

  val reset : t -> int -> unit

  val stat : int -> int -> t -> unit

  val extract_tree : Ckygram.t ->
    (string * int * (int * float * float * Rule.t) list) array ->
    t Array.t -> int -> Ptbtree.string_tree
end

module type Parser =
sig
  val parse_file : Ckygram.t -> bool -> string -> unit
  val parse_wordlist : Ckygram.t -> bool -> string list -> unit
  val test_forest : Ckygram.t -> bool -> Ptbtree.string_tree -> int * int
end


module BaseCell =
struct
  type scores = {inside_score:float;
                 unary_inside_score:float;
                 max_score:float;
                 unary_max_score:float
                }
  type entry = (scores * CKYBackPointer.t * CKYBackPointer.t) option

  type t = entry Array.t

  let vscores = {inside_score=0.0; unary_inside_score=0.0;
                 max_score=Float.neg_infinity;
                 unary_max_score=Float.neg_infinity;
                }

  let empty_entry () = None
  (* let create_empty n = Array.init n ~f:(fun _ -> empty_entry ()) *)
  let create_empty n = Array.create ~len:n None

  let add_lexical t pos score =
    let entry = Some ({vscores with inside_score=score}, CKYBackPointer.unused, CKYBackPointer.unused) in
    Array.unsafe_set t pos entry

  let decode_lexical t tok_id rule pos logscore =
    match Array.unsafe_get t pos with
    | None -> false
    | Some _ ->
       Array.unsafe_set t pos (Some ({vscores with max_score=logscore},
                                     CKYBackPointer.lexical rule tok_id,
                                     CKYBackPointer.unused
       ));
      true

  let add_unary t lhs score =
    let newval =
      match Array.unsafe_get t lhs with
      | None -> Some ({vscores with unary_inside_score =  score}, CKYBackPointer.unused, CKYBackPointer.unused)
      | Some ({unary_inside_score;_} as old,bp1,bp2) -> Some ({old with unary_inside_score = unary_inside_score +. score},bp1,bp2)
    in Array.unsafe_set t lhs newval

  (* TODO:  check *)
  let decode_unary t rule lhs logrhsscore logrulescore =
    match Array.unsafe_get t lhs with
    | None -> ()
    | Some ({unary_max_score;_} as old,bp,_) ->
       let score = logrhsscore +. logrulescore in
       if score > unary_max_score
       then Array.unsafe_set t lhs (Some ({old with unary_max_score=score},bp,CKYBackPointer.unary rule))

  let add_binary t lhs score =
    let newval =
      match Array.unsafe_get t lhs with
      | None -> Some ({vscores with inside_score =  score},CKYBackPointer.unused, CKYBackPointer.unused)
      | Some ({inside_score;_},bp1,bp2) -> Some ({vscores with inside_score = inside_score +. score},bp1,bp2)
    in Array.unsafe_set t lhs newval


  let decode_binary t rule lhs cell1_id cell2_id loglrscore logrulescore =
    match Array.unsafe_get t lhs with
    | None -> ()
    | Some ({max_score;_},_,_) ->
       let score = loglrscore +. logrulescore in
       if score > max_score
       then Array.unsafe_set t lhs (Some ({vscores with max_score=score},CKYBackPointer.binary rule cell1_id cell2_id,CKYBackPointer.unused))


  let length t = Array.length t

  let is_empty_entry entry = entry = None

  let get_entry_inside_score_exn entry =
    match entry with
    | None -> failwith "inside empty"
    | Some ({inside_score;_},_,_) -> inside_score

  let get_entry_max_score_exn entry =
    match entry with
    | None -> failwith "max empty"
    | Some ({max_score;_},_,_) -> max_score


  let get_entry_inside_score entry =
    match entry with
    | None -> None
    | Some ({inside_score;_},_,_) -> Some inside_score

  let get_entry_max_score entry =
    match entry with
    | None -> None
    | Some ({max_score;_},_,_) -> Some max_score

  let get t lhs = Array.unsafe_get t lhs

  let iteri t ~f = Array.iteri t ~f

  (* let nb_active t = Array.fold t ~init:0 *)
  (*   ~f:(fun acc entry -> *)
  (*     if entry <> None then acc+1 else acc) *)

  (* pruning cell inside * priors : and keep the entries >  \alpha max *)
  let prune_priors t priors ~threshold =

    (* let a1 = Float.of_int(nb_active t) in *)

    let (entries,max_score,closed) =
      Array.foldi t ~init:([],0.0,true)
        ~f:(fun lhs ((acc,m,_) as old) entry  ->
          match entry with
          | None -> old
          | Some ({inside_score;_},_,_) ->
             let tmp = inside_score *. Hashtbl.find_exn priors lhs in
             let max' = max tmp m in
             ((tmp, lhs)::acc,max',false)
        )
    in
    let () = if closed
      then ()
      else
        let max_score = max_score *. threshold in
        List.iter entries
          ~f:(fun (score,lhs) -> if score < max_score then
              Array.unsafe_set t lhs None
          )
    (* ; *)
    (* let a2 = Float.of_int(nb_active t) in *)
    (* if a1 <> 0.0 *)
    (* then  printf "after pruning: %f\n%!"  (100.0 *. a2/. a1) *)
    (* else  printf "cell was empty\n%!" *)
    in
    closed

  (* pruning cell inside * priors : and keep the top ~size ??? *)
  let prune_group t priors ~size =
    let entries,closed =
      Array.foldi t ~init:([],true)
        ~f:(fun lhs ((acc,_) as old) entry  ->
          match entry with
          | None -> old
          | Some ({inside_score;_},_,_) ->
             (inside_score *. Hashtbl.find_exn priors lhs, lhs)::acc,false
        )
    in
    let () = if closed
      then ()
      else
        let entries2remove = List.sort ~cmp: (fun (score1,_) (score2,_) -> Float.compare score2 score1) entries |> (fun l -> List.drop l size)
        in
        List.iter entries2remove
          ~f:(fun (_,lhs) ->
            Array.unsafe_set t lhs None
          )
    in
    closed


  let reset t size =
    (*not sure this avoids copying*)
    Array.fill t ~pos:0 ~len:size (empty_entry ())

  let transfer_inside t =
    Array.iteri t
      ~f:(fun lhs entry ->
        match entry with
        | None -> ()
        |Some ({inside_score; unary_inside_score;_},bp1,bp2) ->
           Array.unsafe_set t lhs
             (Some ({vscores with inside_score=inside_score +. unary_inside_score;},bp1,bp2))
      )



  let transfer_max t =
    Array.iteri t
      ~f:(fun lhs entry ->
        match entry with
        | None -> ()
        |Some (({max_score; unary_max_score;_} as old),bp1,bp2) ->
           if unary_max_score > max_score
           then Array.unsafe_set t lhs (Some ({vscores with max_score=unary_max_score},bp1,bp2))
           else Array.unsafe_set t lhs (Some (old,bp1,CKYBackPointer.unused))
      )



  let stat s e t =
    (* () *)
    let empty = Array.fold t ~init:true
      ~f:(fun acc entry ->
        if not acc then
          false
        else
          match entry with
          | None -> true
          | _ -> false
      )
    in
    if empty
    then Printf.printf "cell (%d,%d) is empty\n%!" s e
    else
      Array.iteri t
        ~f:(fun i entry ->
          match entry with
          | None -> ()
          | Some ({inside_score;_},_bp1,_bp2) ->
             Printf.printf "entry (%d,%d) %d: score %f\n%!" s e i inside_score
        )


  let extract_tree gram tokens chart root_entry_idx =
    let rec recextract_tree lhs entry_idx license_unary =
      match Array.unsafe_get (Array.unsafe_get chart entry_idx) lhs with
      | Some (_,bp1,bp2) ->
         let bp = if license_unary && (not (CKYBackPointer.is_unused bp2))
           then
             (
               (* printf "bp2\n%!"; *)
              bp2)
           else (
             (* printf "bp1\n%!"; *)
             bp1)
         in
         begin
           (* printf "%d\n%!" entry_idx; *)
         match CKYBackPointer.get_adress_list bp entry_idx with
         | Rule.Lex(pos,_),[tok_id] ->
            begin match tokens.(tok_id) with (w,_,_) ->
              (* printf "lexical %s\n%!" w; *)
              Tree.Leaf(Int2StringMap.int2str (Ckygram.nt_map gram) pos, w) end
         | Rule.Una(lhs,rhs,path),_ ->
            (* printf "una %d %d\n%!" lhs rhs; *)
            let t' = recextract_tree rhs entry_idx false in
            let t'' = List.fold path ~init:t'
              ~f:(fun tree i -> Tree.Node(Int2StringMap.int2str (Ckygram.nt_map gram) i, [tree])) in
            Tree.Node(Int2StringMap.int2str (Ckygram.nt_map gram) lhs, [t''])
         | Rule.Bin(lhs,rhs1,rhs2),[lcell_id;rcell_id] ->
            (* printf "bin %d %d %d\n%!" lhs rhs1 rhs2; *)
            Tree.Node(Int2StringMap.int2str (Ckygram.nt_map gram) lhs,
                      [recextract_tree rhs1 lcell_id true; recextract_tree rhs2 rcell_id true])
         |_ -> failwith "ill formed bp"
         end
      | None -> failwith "ill-formed forest"

    in
    recextract_tree 29 (*TOP*) root_entry_idx true


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


  let init_chart una_pos_indexes chart tokens =
    Array.iteri tokens
      ~f:(fun i (_,_,lex_indexes) ->
        let cell = Array.unsafe_get chart i in

        List.iter lex_indexes
          ~f:(fun (pos,prob,_,_) ->
            (* add T -> w *)
            Cell.add_lexical cell pos prob;

            (* add rules L -> T *)
            List.iter (Array.unsafe_get una_pos_indexes pos)
              ~f:(fun index_ur -> Cell.add_unary cell index_ur.lhs (prob *.index_ur.score))
          )
        ;
        Cell.transfer_inside cell
      )





  let build_forest gram tokens =

    let n = Array.length tokens in
    let access i j =
      let diff = j - i in
      i + diff * (2*n-diff+ 1)/2 in
    (* equivalent to access i i*)
    (* let access_lex i = i in *)

    let size = n*(n+1)/2 in

    (* fprintf Out_channel.stderr "size chart: %d\n%!" size; *)

    (* TODO: it should be possible to do all the parsing in ~f
       below *)
    let nts = Ckygram.nts gram in
    let chart = Array.init size ~f:(fun _ -> Cell.create_empty nts) in
    (* let () = reinit_chart size gram.nts in *)

    let opencells = Array.create ~len:size true in

    let unapos = Ckygram.unary_frompos gram in
    let bin = Ckygram.binary_index gram in
    let una = Ckygram.unary_index gram in
    let priors = Ckygram.priors gram in

    (* initialize charts *)
    init_chart unapos chart tokens;



    let process_binaries cell start lend =
      (* let add_to_cell = Cell.add_binary cell *)
      (* in *)
      let rec recprocess_binaries m =
        if m = lend then ()
        else
          let left_cell_id = access start m in
          (if Array.unsafe_get opencells left_cell_id
           then
              let right_cell_id = access (m+1) lend in
              (if Array.unsafe_get opencells right_cell_id
               then
                  let left_cell = Array.unsafe_get chart left_cell_id in
                  let right_cell= Array.unsafe_get chart right_cell_id in
                  let left_get = Cell.get left_cell in
                  let right_get = Cell.get right_cell in
                  List.iter bin
                    ~f:(fun (r1,info1) ->
                      let lentry = left_get r1 in
                      match Cell.get_entry_inside_score lentry with
                      | None -> ()
                      |Some left_score ->
                         List.iter info1
                           ~f:(fun (r2,info2) ->
                             let rentry = right_get r2 in
                             match Cell.get_entry_inside_score rentry with
                             | None -> ()
                             | Some right_score ->
                                let lr_score = left_score *. right_score in
                                List.iter  info2
                                  ~f:(fun (lhs,rule_score,_,_) ->
                                    let score = lr_score *. rule_score in
                                    Cell.add_binary cell lhs score
                                  )
                           )
                    )
               else ()
              )
           else ()
          )
          ;
          recprocess_binaries (m+1)
      in
      recprocess_binaries start
    in

    let process_unary cell =
      Cell.iteri cell
        ~f:(fun rhs entry ->
          match Cell.get_entry_inside_score entry with
          | None -> ()
          | Some score ->
             List.iter (Array.unsafe_get una rhs)
               ~f:(fun unary_index ->
                 let unary_score = score *. unary_index.score in
                 Cell.add_unary cell unary_index.lhs unary_score
               )
        );
      Cell.transfer_inside cell
    in


    let visit_spans sent_length width threshold =
      let rec rec_visit_spans start =

        if start = sent_length then ()
        else
          let lend = start + width in
          if lend < sent_length then
            let cell_id = (access start lend) in
            (if Array.unsafe_get opencells cell_id
             then
                let cell = Array.unsafe_get chart cell_id in
                (* binary rules *)
                process_binaries cell start lend;

                (*unary rules*)
                process_unary cell;

                (* fprintf Out_channel.stderr "pruning:\n"; *)
                let closed = Cell.prune_priors cell priors ~threshold in
                Array.unsafe_set opencells cell_id (not closed)
            );
            (* ; *)
            (* Cell.stat start lend cell *)
            rec_visit_spans (start + 1)
      in
      rec_visit_spans 0
    in

    let iter_widths sentence_length =
      let threshold= if sentence_length < 3 then 0.0 else 0.0015 in
      let rec aux width =
        if width > n then ()
        else
          let () = visit_spans sentence_length width threshold in
          aux (width + 1)
      in
      aux 0
    in
    iter_widths n;
    (chart,opencells)


  let decode gram tokens chart opencells =
    (* TODO: reomve this repeat from build_forest *)
    let n = Array.length tokens in
    let access i j =
      let diff = j - i in
      i + diff * (2*n-diff+ 1)/2 in
    (* equivalent to access i i*)
    (* let access_lex i = i in *)

    let unapos = Ckygram.unary_frompos gram in
    let bin = Ckygram.binary_index gram in
    let una = Ckygram.unary_index gram in

    (* initialize charts *)
    Array.iteri tokens
      ~f:(fun i (_,_,indexes) ->
        let cell = Array.unsafe_get chart i in
        List.iter indexes
          ~f:(fun (pos,_,logprob,lexrule) ->
            (* add T -> w *)
            if Cell.decode_lexical cell i lexrule pos logprob
            then
               (* rules L -> T *)
              List.iter (Array.unsafe_get unapos pos)
                ~f:(fun index_ur ->
                  Cell.decode_unary cell index_ur.rule index_ur.lhs logprob index_ur.logscore)
          );
        Cell.transfer_max cell
      );

    let process_binaries cell start lend =
      (* let add_to_cell = Cell.decode_binary cell *)
      (* in *)
      let rec recprocess_binaries m =
        if m = lend then ()
        else
          let left_cell_id = access start m in
          (if Array.unsafe_get opencells left_cell_id
           then
              let right_cell_id = access (m+1) lend in
              (if Array.unsafe_get opencells right_cell_id
               then
                  let left_cell = Array.unsafe_get chart left_cell_id in
                  let right_cell= Array.unsafe_get chart right_cell_id in
                  let left_get = Cell.get left_cell in
                  let right_get = Cell.get right_cell in
                  List.iter bin
                    ~f:(fun (r1,info1) ->
                      let lentry = left_get r1 in
                      match Cell.get_entry_max_score lentry with
                      | None -> ()
                      | Some left_score ->
                         List.iter info1
                           ~f:(fun (r2,info2) ->
                             let rentry = right_get r2 in
                             match Cell.get_entry_max_score rentry with
                             | None -> ()
                             | Some right_score ->
                                let lr_score = left_score +. right_score in
                                List.iter  info2
                                  ~f:(fun (lhs,_,logrule_score,rule) ->
                                    Cell.decode_binary cell rule lhs left_cell_id right_cell_id lr_score logrule_score
                                  )
                           )
                    )
               else ()
              )
           else ()
          )
          ;
          recprocess_binaries (m+1)
      in
      recprocess_binaries start
    in

    let process_unary cell =
      let process_entry rhs entry =
        match Cell.get_entry_max_score entry with
        | None -> ()
        | Some score ->
           List.iter (Array.unsafe_get una rhs)
             ~f:(fun unary_index ->
               Cell.decode_unary cell unary_index.rule unary_index.lhs score unary_index.logscore
             )
      in
      Cell.iteri cell ~f:process_entry
      ;
      Cell.transfer_max cell
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
              (if Array.unsafe_get opencells cell_id
               then
                  (* binary rules *)
                  process_binaries cell start lend;

               (*unary rules*)
               process_unary cell
              )


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
      aux 1
    in
    iter_widths n

    ;

    let root_cell_id = access 0 (n-1) in
    let root_cell = Array.get chart root_cell_id in
    let root_entry = Cell.get  root_cell 29 (*TOP*) in
    match Cell.get_entry_max_score root_entry with
    | None -> printf "-inf : (())\n%!"
    | Some max_score ->
       printf "%f : %s\n%!" max_score
         (if max_score = Float.neg_infinity
          then "(())"
          else
             let tree = Cell.extract_tree gram tokens chart  root_cell_id |> Ptbtree.unbinarize in
             Ptbtree.to_string tree
         )

  let tokenize_simple gram tokens =
    let word_map = Ckygram.word_map gram in
    let lex = Ckygram.lexical_index gram in
    List.map tokens
      ~f:(fun str ->
        match Int2StringMap.str2int_safe word_map str with
        | -1 ->
           let unk_id = Int2StringMap.str2int word_map "UNK" in
           (str, unk_id, Array.unsafe_get lex unk_id) (* TODO:  prepare for other jokers *)
        | n ->(str, n, Array.unsafe_get lex n)
      )
    |> Array.of_list


  let tokenize_tagged gram tokens =
    let word_map = Ckygram.word_map gram in
    let nt_map = Ckygram.nt_map gram in
    let lex = Ckygram.lexical_index gram in
    let rec process l =
      match l with
      | [] -> []
      | hw::hp::tl ->
         begin
           let w =
             match Int2StringMap.str2int_safe word_map hw with
             | -1 -> Int2StringMap.str2int_safe word_map "UNK" (* TODO:  prepare for other jokers *)
             | n -> n
           in
           let p = Int2StringMap.str2int nt_map hp in
           let l = Array.unsafe_get lex w in
           match List.find l ~f:(fun (a,_,_,_) -> a = p) with
           | None -> (hw,w,l)::(process tl)
           | Some i -> (hw,w,[i])::(process tl)
         end
      | _ -> failwith "ill-formed input"
    in
    process tokens |> Array.of_list


  let parse_line gram tok_func line =

    let line = String.strip line in
    let tokens = tok_func gram (Regex.split split_regex line) in

    (* Array.iteri tokens *)
    (*   ~f:(fun i (str,id) -> *)
    (*     fprintf Out_channel.stderr "token %d %s %d  %s\n%!" i str id (Int2StringMap.int2str w_map id) *)
    (*   ); *)
    let chart,opencells = build_forest gram tokens in
    decode gram tokens chart opencells

  let parse_file gram tagged file =
    let tok_func =
      if tagged
      then tokenize_tagged
      else tokenize_simple
    in
    In_channel.with_file file ~f:
      (fun ic ->
        let i = ref 0 in
        fprintf Out_channel.stderr "start\n%!";
        In_channel.iter_lines ic
          ~f:(fun line ->

            parse_line gram tok_func line;
            i := !i + 1;
            if (!i mod 50) = 0
            then fprintf Out_channel.stderr "sentence: %d\n%!" !i;
          )
      )


  let parse_wordlist gram tagged word_list =
    let tok_func =
      if tagged
      then tokenize_tagged
      else tokenize_simple
    in
    let tokens = tok_func gram word_list in
    let chart,opencells = build_forest gram tokens in
    decode gram tokens chart opencells






  let test_forest gram _tagged tree =
    let rec get_list_labeled_const atree =
      match atree with
      | Tree.Leaf((pos,(b,e)),_) -> [(pos,b,e)]
      | Tree.Node((lhs,(b,e)),l) ->
         match l with
         | [n1;n2] -> (lhs,b,e)::(get_list_labeled_const n1)@(get_list_labeled_const n2)
         | [n] -> get_list_labeled_const n
         | _ -> failwith "ill-formed tree"
    in
    let atree = Ptbtree.annotate tree in
    let l = get_list_labeled_const atree in
    let word_list = Ptbtree.yield tree in
    let tokens = tokenize_simple gram word_list in
    let chart,_opencells = build_forest gram tokens in

    let n = Array.length tokens in
    let access i j =
      let diff = j - i in
      i + diff * (2*n-diff+ 1)/2 in

    let matched = List.fold l ~init:0
      ~f:(fun acc (lhs,i,j) ->
        let cell_id = access i j in
        let cell = Array.unsafe_get chart cell_id in
        let lhs_id = Int2StringMap.str2int (Ckygram.nt_map gram) lhs in
        let entry = Cell.get cell lhs_id in
        if Cell.is_empty_entry entry then acc else acc+1
      ) in
    (* printf "matched: %f%%\n%!"  (100.0 *. (Float.of_int matched) /.
       (Float.of_int (List.length l))) *)
    (matched, List.length l)







end
