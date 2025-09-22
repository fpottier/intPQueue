(******************************************************************************)
(*                                                                            *)
(*                                  IntPQueue                                 *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*       Copyright 2025--2025 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(*                                                                            *)
(******************************************************************************)

(* As a reference implementation, one might (naively) wish to use an
   efficient priority queue, such as the one offered by the module
   [Pqueue] in OCaml's standard library, starting with OCaml 5.4.0.
   However, this is not suitable. [remove] is non-deterministic. Once
   the candidate implementation has chosen which element should be
   extracted, the reference implementation must obey this choice. *)

(* We use a map of priorities to nonempty lists of elements (with
   possible duplicate elements). It is important to not tolerate
   empty lists; this way, [min_binding] returns an element with
   minimum priority. *)

module M = Map.Make(Int)

let create () =
  ref M.empty

let add q x p =
  let xs = match M.find_opt p !q with None -> [] | Some xs -> xs in
  q := M.add p (x :: xs) !q

let rec list_remove x xs =
  match xs with
  | [] ->
      assert false
  | y :: xs ->
      if x = y then xs else y :: list_remove x xs

let extract q (ox : int option) =
  (* [extract] is non-deterministic. Here, [ox] is the result produced by
     the candidate implementation. We must determine whether this result
     is valid and simulate a removal operation that produces precisely
     this result. *)
  if M.is_empty !q then
    match ox with
    | None ->
        Monolith.Valid None
    | Some x ->
        let cause _doc = PPrint.utf8format "(* candidate finds %d *)" x in
        Monolith.Invalid cause
  else
    match ox with
    | None ->
        let cause _doc = PPrint.utf8format "(* candidate returns None *)" in
        Monolith.Invalid cause
    | Some x ->
        (* Check that there exists an element [x] with minimum priority
           in the queue [q] and remove it. *)
        match M.min_binding_opt !q with
        | Some (p, xs) when List.mem x xs ->
            let xs = list_remove x xs in
            q := if xs = [] then M.remove p !q else M.add p xs !q;
            Monolith.Valid (Some x)
        | _ ->
            let cause _doc = PPrint.utf8format "(* candidate finds %d *)" x in
            Monolith.Invalid cause

let cardinal q =
  M.fold (fun _p xs c -> List.length xs + c) !q 0

let is_empty q =
  cardinal q = 0
