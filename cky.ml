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

open Ckygram


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
  val add_lexical:   t -> int -> float -> unit
  val add_unary  :   t -> int -> float -> unit
  val add_binary :   t -> int -> float -> unit

  val decode_lexical : t -> int -> float -> unit
  val decode_unary   : t -> int -> float -> float -> unit
  val decode_binary  : t -> int -> float -> float -> unit

  val length : t -> int
  val get : t -> int -> entry
  val is_empty_entry : entry -> bool
  val get_entry_inside_score : entry -> float
  val get_entry_max_score : entry -> float

  val transfer_inside : t -> unit
  val transfer_max : t -> unit

  val iteri : t -> f:(int -> entry ->unit) -> unit
  val prune_priors : t -> (int,float) Hashtbl.t -> threshold:float -> bool
  val prune_group :  t -> (int,float) Hashtbl.t -> size:int -> bool

  val reset : t -> int -> unit

  val stat : int -> int -> t -> unit
end

module BaseCell : Cell=
struct
  type scores = {inside_score:float;
                 unary_inside_score:float;
                 max_score:float;
                 unary_max_score:float
                }
  type entry = scores option

    (* {init:bool; *)
    (*             scores:scores; *)
    (*             (\* uhist: (Rule.t * BP.t) list; *\) *)
    (*             (\* bhist: (Rule.t * BP.t) list; *\) *)
    (*             (\* lhist: Rule.t option *\) *)
    (*            }

 *)
  type t = entry Array.t

  let vscores = {inside_score=0.0; unary_inside_score=0.0;
                 max_score=Float.neg_infinity;
                 unary_max_score=Float.neg_infinity;
                }
  (* let ventry = {init=false; *)
  (*               scores={inside_score=0.0; unary_inside_score=0.0}; *)
  (*               (\* bhist=[]; lhist=None; uhist=[]; *\) *)
  (*              } *)

  let empty_entry () = None
  let create_empty n = Array.init n ~f:(fun _ -> empty_entry ())

  let add_lexical t pos score =
    let entry = Some {vscores with inside_score=score} in
    Array.unsafe_set t pos entry

  let decode_lexical t pos logscore =
    match Array.unsafe_get t pos with
    | None -> ()
    | Some ({max_score;_} as old) ->
       if logscore > max_score
       then Array.unsafe_set t pos (Some {old with max_score=logscore})

  let add_unary t lhs score =
    let newval =
      match Array.unsafe_get t lhs with
      | None -> Some {vscores with unary_inside_score =  score}
      | Some ({unary_inside_score;_} as old) -> Some {old with unary_inside_score = unary_inside_score +. score}
    in Array.unsafe_set t lhs newval

  let decode_unary t lhs logrhsscore logrulescore =
    match Array.unsafe_get t lhs with
    | None -> ()
    | Some ({unary_max_score;_} as old) ->
       let score = logrhsscore +. logrulescore in
       if score > unary_max_score
       then Array.unsafe_set t lhs (Some {old with unary_max_score=score})

  let add_binary t lhs score =
    let newval =
      match Array.unsafe_get t lhs with
      | None -> Some {vscores with inside_score =  score}
      | Some ({inside_score;_} as old) -> Some {old with inside_score = inside_score +. score}
    in Array.unsafe_set t lhs newval


  let decode_binary t lhs loglrscore logrulescore =
    match Array.unsafe_get t lhs with
    | None -> ()
    | Some ({max_score;_} as old) ->
      let score = loglrscore +. logrulescore in
      if score > max_score
      then Array.unsafe_set t lhs (Some {old with max_score=score})


  let length t = Array.length t

  let is_empty_entry entry = entry = None

  let get_entry_inside_score entry =
    match entry with
    | None -> failwith "inside empty"
    | Some {inside_score;_} -> inside_score

  let get_entry_max_score entry =
    match entry with
    | None -> failwith "max empty"
    | Some {max_score;_} -> max_score

  let get t lhs = Array.unsafe_get t lhs

  let iteri t ~f = Array.iteri t ~f

  let nb_active t = Array.fold t ~init:0
    ~f:(fun acc entry ->
      if entry <> None then acc+1 else acc)

  (* pruning cell inside * priors : and keep the entries >  \alpha max *)
  let prune_priors t priors ~threshold =

    (* let a1 = Float.of_int(nb_active t) in *)

    let (entries,max_score,closed) =
      Array.foldi t ~init:([],0.0,true)
        ~f:(fun lhs (acc,m,cl) entry  ->
          match entry with
          | None -> (acc,m,cl)
          | Some {inside_score;_} ->
             let tmp = inside_score *. Hashtbl.find_exn priors lhs in
             let max' = max tmp m in
             ((tmp, lhs)::acc,max',false)
        )
    in
    let () = if not closed
      then
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
        ~f:(fun lhs (acc,cl) entry  ->
          match entry with
          | None -> (acc,cl)
          | Some {inside_score;_} ->
             (inside_score *. Hashtbl.find_exn priors lhs, lhs)::acc,false
        )
    in
    let () = if not closed
      then
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
        |Some {inside_score; unary_inside_score;_} ->
           Array.unsafe_set t lhs
             (Some {vscores with inside_score=inside_score +. unary_inside_score;})
      )



  let transfer_max t =
    Array.iteri t
      ~f:(fun lhs entry ->
        match entry with
        | None -> ()
        |Some ({max_score; unary_max_score;_} as old) ->
           match Float.compare max_score unary_max_score with
           | 0 -> ()
           | -1 ->
              Array.unsafe_set t lhs
                (Some {old with max_score=unary_max_score; unary_inside_score=Float.neg_infinity})
           | 1 ->
              Array.unsafe_set t lhs
                (Some {old with unary_inside_score=Float.neg_infinity})
           | _ -> assert false
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
          |Some{inside_score;_} ->
             Printf.printf "entry (%d,%d) %d: score %f\n%!" s e i inside_score
        )
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

    (* TODO: it should be possible to do all the parsing in ~f
       below *)
    let nts = Ckygram.nts gram in
    let chart = Array.init size ~f:(fun _ -> Cell.create_empty nts) in
    (* let () = reinit_chart size gram.nts in *)

    let opencells = Array.create ~len:size true in

    let lex = Ckygram.lexical_index gram in
    let unapos = Ckygram.unary_frompos gram in
    let bin = Ckygram.binary_index gram in
    let una = Ckygram.unary_index gram in
    let priors = Ckygram.priors gram in

    (* initialize charts *)
    Array.iteri tokens
      ~f:(fun i token ->
        let cell_i = access_lex i in
        let w_id = snd token in
        let cell = Array.unsafe_get chart cell_i in

        List.iter (Array.unsafe_get lex w_id)
          ~f:(fun (pos,prob,_,_) ->
            (* add T -> w *)
            Cell.add_lexical cell pos prob;

            (* add rules L -> T *)
            List.iter (Array.unsafe_get unapos pos)
              ~f:(fun index_ur -> Cell.add_unary cell index_ur.lhs (prob *.index_ur.score))
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
                            ~f:(fun (lhs,rule_score,_,_) ->
                              let score = lr_score *. rule_score in
                              add_to_cell lhs score
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
          if Cell.is_empty_entry entry
          then ()
          else
            let score = Cell.get_entry_inside_score entry in
            List.iter (Array.unsafe_get una rhs)
              ~f:(fun unary_index ->
                let unary_score = score *. unary_index.score in
                Cell.add_unary cell unary_index.lhs unary_score
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
            (if Array.unsafe_get opencells cell_id
            then
              let cell = Array.unsafe_get chart cell_id in
              (* binary rules *)
              process_binaries cell start lend;

              (*unary rules*)
              process_unary cell;

              (* fprintf Out_channel.stderr "pruning:\n"; *)
              let closed = Cell.prune_priors cell priors ~threshold:0.001 in
              Array.unsafe_set opencells cell_id (not closed)
            );
            (* ; *)
            (* Cell.stat start lend cell *)
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
    iter_widths n;
    (chart,opencells)


  let decode gram tokens chart opencells =
    (* TODO: reomve this repeat from build_forest *)
    let n = Array.length tokens in
    let access i j = i + ((j-i)*(2*n-(j-i)+ 1))/2 in
    (* equivalent to access i i*)
    let access_lex i = i in

    let lex = Ckygram.lexical_index gram in
    let unapos = Ckygram.unary_frompos gram in
    let bin = Ckygram.binary_index gram in
    let una = Ckygram.unary_index gram in

    (* initialize charts *)
    Array.iteri tokens
      ~f:(fun i token ->
        let cell_i = access_lex i in
        let w_id = snd token in
        let cell = Array.unsafe_get chart cell_i in
        List.iter (Array.unsafe_get lex w_id)
          ~f:(fun (pos,_,logprob,_) ->
            (* add T -> w *)
            if not (Cell.is_empty_entry (Cell.get cell pos))
            then
              (Cell.decode_lexical cell pos logprob;
               (* rules L -> T *)
               List.iter (Array.unsafe_get unapos pos)
                 ~f:(fun index_ur ->
                   Cell.decode_unary cell index_ur.lhs logprob index_ur.logscore)
              )
          );
        Cell.transfer_max cell
      );



    let process_binaries cell start lend =
      let add_to_cell = Cell.decode_binary cell
      in
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
                if Cell.is_empty_entry lentry then ()
                else
                  let left_score = Cell.get_entry_max_score lentry in
                  List.iter info1
                    ~f:(fun (r2,info2) ->
                      let rentry = right_get r2 in
                      if Cell.is_empty_entry rentry then ()
                      else
                        let right_score = Cell.get_entry_max_score rentry in
                        let lr_score = left_score +. right_score in
                        List.iter  info2
                          ~f:(fun (lhs,_,logrule_score,_) ->
                            add_to_cell lhs lr_score logrule_score
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
          if Cell.is_empty_entry entry
          then ()
          else
            let score = Cell.get_entry_max_score entry in
            List.iter (Array.unsafe_get una rhs)
              ~f:(fun unary_index ->
                Cell.decode_unary cell unary_index.lhs score unary_index.logscore
              )
        );
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
      aux 0
    in
    iter_widths n

    (* ; *)

    (* let root_cell = Array.get chart (access 0 (n-1)) in *)
    (* let root_entry = Cell.get  root_cell 29 (\*TOP*\) in *)
    (* if not (Cell.is_empty_entry root_entry) *)
    (* then *)
    (*   printf "parsed %f\n%!" (Cell.get_entry_max_score root_entry) *)
    (* else *)
    (*   printf "not parsed\n%!" *)


  let parse_line gram line =

    (* Hashtbl.iter gram.priors *)
    (*   ~f:(fun ~key:nt ~data: pr -> *)
    (*     fprintf Out_channel.stderr "prior: %d %f\n%!" nt pr *)
    (*   ); *)

    let line = String.strip line in
    let word_map = Ckygram.word_map gram in
    let tokens =
      List.map (Regex.split split_regex line)
        ~f:(fun str ->
          match Int2StringMap.str2int_safe word_map str with
          | -1 -> (str, Int2StringMap.str2int_safe word_map "UNK") (* TODO:  prepare for other jokers *)
          | n ->(str, n)
        )
      |> Array.of_list
    in

    (* Array.iteri tokens *)
    (*   ~f:(fun i (str,id) -> *)
    (*     fprintf Out_channel.stderr "token %d %s %d  %s\n%!" i str id (Int2StringMap.int2str w_map id) *)
    (*   ); *)
    let chart,opencells = build_forest gram tokens in
    decode gram tokens chart opencells

  let parse_file gram file =

    (In_channel.with_file file ~f:
       (fun ic ->
         let i = ref 0 in
         fprintf Out_channel.stderr "start\n%!";
         In_channel.iter_lines ic
           ~f:(fun line ->
             parse_line gram line;
             i := !i + 1;
             if (!i mod 50) = 0
             then fprintf Out_channel.stderr "sentence: %d\n%!" !i;
           )
       )
    )
end
