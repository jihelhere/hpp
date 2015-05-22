(*
 * Copyright (C) 2014  Joseph Le Roux
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

(* A module to model a bidirectional string/int encoding *)
module Int2StringMap = struct

  type t = {
      int2str : (int,string) Hashtbl.t;
      str2int : (string,int) Hashtbl.t;
      mutable counter : int;
      mutable closed  : bool;
  }

  (* thunk it *)
  let empty () = {
    int2str = Hashtbl.create ~hashable:Int.hashable ();
    str2int = Hashtbl.create ~hashable:String.hashable ();
    counter = 0;
    closed =  false;
  }

  let close t = t.closed <- true


  (* can modify the first argument *)
  let str2int t str =
    match Hashtbl.find t.str2int str with
    | Some x -> x
    | None ->
       if not t.closed
       then
         let d = t.counter in
         let () = t.counter  <- t.counter + 1 in
         let _ = Hashtbl.add t.int2str ~key: d ~data: str in
         let _ = Hashtbl.add t.str2int ~key: str ~data: d in
         d
       else
         -1

  (* cannot modify the first argument *)
  let int2str t i =
    match Hashtbl.find t.int2str i with
    | Some x -> x
    | None ->  failwith "Not Found"        (* TODO:  replace with closed condition*)

  let get_size t =
    t.counter

  let sexp_of_t t =
    let a x = Sexp.Atom x and l x = Sexp.List x in
    l [ a "int2str" ;
        l (Hashtbl.fold t.int2str ~init:[]
                        ~f:(fun ~key:i ~data:s acc ->
                            (l [a (string_of_int i) ; a s])::acc
                           )
          )
      ]

  let load_from_sexp t s =
    let rec add_pairs l =
      match l with
      | [] -> ()
      | Sexp.List([Sexp.Atom h1;Sexp.Atom h2])::q ->
         let _ = Hashtbl.add t.int2str ~key:(Int.of_string h1) ~data:h2 in
         let _ = Hashtbl.add t.str2int ~key:h2 ~data:(Int.of_string h1) in
         let () = t.counter <- t.counter +1 in
         add_pairs q
      | _ -> failwith "add_pairs"
    in
    match s with
    | Sexp.List ([Sexp.Atom "int2str";Sexp.List(q)]) -> add_pairs q
    | _ -> failwith "t_of_sexp"

  let t_of_sexp s =
    let t = empty () in
    load_from_sexp t s; t

end
