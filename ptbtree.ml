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
open Tree

module Ptbtree =
struct
  type string_tree = (string,string) Tree.t

  let add_top_node t =
    Tree.Node("TOP", [t])

  let yield t =
    let rec aux acc t =
      match t with
      | Tree.Leaf(_,w) -> w::acc
      | Tree.Node(_,l) -> List.fold ~f:(fun a n -> aux a n) ~init:acc l
    in
    List.rev (aux [] t)

  (* TODO:  take a regex parameter (treebanks/functions...) *)
  let regex = Regex.create_exn "[-=].+$"
  let rec clean_nodes t =
    let clean s =
      if s.[0] = '-' then s
      else
        match (Regex.rewrite regex  ~template: "" s) with
        | Ok n ->  n
        | Error _ -> s
    in
    match t with
    | Tree.Leaf(p,w) -> Tree.Leaf((clean p),w)
    | Tree.Node(n,l) -> Tree.Node((clean n), List.map ~f:clean_nodes l)

  let rec remove_trace t =
    let trace_node n =
      match n with
    | Tree.Leaf("-NONE-",_) -> true
    | Tree.Node("-NONE-",_) -> true
    |_ -> false
    in
    match t with
    | Tree.Node(n,l) ->
       let l' = (List.map ~f:remove_trace l) in
       if List.for_all ~f:trace_node l'
       then Tree.Node("-NONE-", [])
       else Tree.Node(n, List.filter ~f:(fun n -> not (trace_node n)) l')
    | _ -> t

  let rec binarize_right t =
    match t with
    | Tree.Leaf(_,_) -> t
    | Tree.Node(n,l) ->
       if (List.length l) <= 2
       then Tree.Node(n, List.map ~f:binarize_right l)
       else
         let hd = List.hd_exn l in
         let tl = List.tl_exn l in
         let new_name = if n.[0] = '@' then n else sprintf "@%s" n in
         Tree.Node(n, List.map ~f:binarize_right [ hd; Tree.Node(new_name, tl)])

  let rec unbinarize t =
    match t with
    | Tree.Leaf(_,_) -> t
    | Tree.Node(n, l) ->
       Tree.Node(n,
       List.fold
         ~f:(fun acc d ->
           match d with
           | Tree.Node(nd,ld) ->
              let ld' = List.map ~f:unbinarize ld in
              if nd.[0] = '@' then ld'@acc else Tree.Node(nd, ld')::acc
           | _ ->  d::acc
         )
         ~init:[]
         l)

  let process_tree t =
    clean_nodes t |> remove_trace |> binarize_right |> add_top_node

  let to_string t =
    let rec rec_to_string t =
      match t with
      | Tree.Leaf(p,s) -> sprintf "(%s %s)" p s
      | Tree.Node(n,l) -> sprintf "(%s %s)" n
                                        (List.fold
                                          ~f:(fun acc d ->
                                            String.concat [acc ; (rec_to_string d)]
                                          )
                                          ~init: "" l)
    in
    sprintf "(%s)" (rec_to_string t)


  let calculate_word_counts tl =
    let tbl = Hashtbl.create ~hashable:String.hashable () in
    let () = List.iter ~f:(fun t ->
      let y = yield t in
      List.iter ~f:(fun w ->
        Hashtbl.change tbl w
          (function
          | Some x -> Some(x+1)
          | None -> Some 1
          )
      ) y
    ) tl in
    tbl

  let replace_rares repl_fun threshold tl =
    let tbl = calculate_word_counts tl in
    let rec aux t =
      match t with
      | Tree.Leaf(nt,w) ->
         (match Hashtbl.find tbl w with
          | None -> Tree.Leaf(nt, repl_fun w)
          | Some i when i < threshold -> Tree.Leaf(nt,repl_fun w)
          | Some _ -> t
         )
      | Tree.Node(n,l) -> Tree.Node(n, List.map ~f:aux l)
    in
    List.map ~f:aux tl

  let replace_rares_simple t l = replace_rares (fun _ -> "UNK") t l


  (************************************)
  type int_tree = (int,int) Tree.t

  open Int2stringmap


  let nt_map = Int2StringMap.empty ()
  let w_map =  Int2StringMap.empty ()

  let rec convert_string_ptb t =
    match t with
    | Tree.Leaf(p,w) -> Tree.Leaf(Int2StringMap.str2int nt_map p, Int2StringMap.str2int w_map w)
    | Tree.Node(n,l) -> Tree.Node(Int2StringMap.str2int nt_map n, List.map ~f:convert_string_ptb l)

  let rec convert_int_ptb t =
    match t with
    | Tree.Leaf(p,w) -> Tree.Leaf(Int2StringMap.int2str nt_map p, Int2StringMap.int2str w_map w)
    | Tree.Node(n,l) -> Tree.Node(Int2StringMap.int2str nt_map n, List.map ~f:convert_int_ptb l)


  (***********************************)
  type annotation = {b: int; e: int}
  type annotated_tree = (int * annotation, int * annotation) Tree.t

  let annotate tree =
    let rec rec_annotate tree b =
      match tree with
      | Tree.Leaf(nt,t) -> Tree.Leaf((nt,(b,b)), (t,(b,b)))
      | Tree.Node(n,l) ->
         let e,al =
           List.fold l
           ~init:(b,[])
           ~f:(fun (b',l') tree ->
             let n = List.length (yield tree) in
             let tree' = rec_annotate tree b' in
             (b'+ n), tree'::l'
           ) in
         Tree.Node((n,(b,e)),List.rev al)
    in
    rec_annotate tree 0

end
