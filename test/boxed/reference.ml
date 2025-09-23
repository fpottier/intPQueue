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

let format = PPrint.utf8format

module C = IntPQueue.Boxed

(* As a reference implementation, one might (naively) wish to use an
   efficient priority queue, such as the one offered by the module
   [Pqueue] in OCaml's standard library, starting with OCaml 5.4.0.
   However, this is not suitable. [extract] is non-deterministic. Once
   the candidate implementation has chosen which element should be
   extracted, the reference implementation must obey this choice. *)

(* We use a map of priorities to nonempty lists of boxes. It is
   important to not tolerate empty lists; this way, [min_binding]
   returns an element with minimum priority. *)

module M = Map.Make(Int)

type elt =
  int

type box =
  { payload: elt; mutable priority: int; mutable busy: t option }

and t =
  box list M.t ref

let box x =
  { payload = x; priority = 0; busy = None }

let payload box =
  box.payload

let priority box =
  box.priority

let busy box =
  match box.busy with None -> false | Some _ -> true

let mem q box =
  match box.busy with None -> false | Some q' -> q == q'
(*
  Another implementation that does not rely on [busy]:
  match M.find_opt box.priority !q with
  | None ->
      false
  | Some boxes ->
      List.memq box boxes
 *)

let create () : t =
  ref M.empty

let add q box i =
  assert (not (busy box));
  assert (not (mem q box));
  let boxes = match M.find_opt i !q with None -> [] | Some boxes -> boxes in
  q := M.add i (box :: boxes) !q;
  box.priority <- i;
  box.busy <- Some q

let rec list_remove equiv x ys =
  match ys with
  | [] ->
      raise Not_found
  | y :: ys ->
      if equiv x y then
        y, ys
      else
        let z, ys = list_remove equiv x ys in
        z, y :: ys

let same_payload (cbox : elt C.box) (rbox : box) =
  C.payload cbox = rbox.payload

let extract (q : t) (obox : elt C.box option) =
  (* [extract] is non-deterministic. Here, [obox] is the result produced by
     the candidate implementation. We must determine whether this result
     is valid and simulate a removal operation that produces precisely
     this result. *)
  if M.is_empty !q then
    match obox with
    | None ->
        Monolith.Valid None
    | Some box ->
        let cause _doc =
          format
            "(* candidate finds box with payload %d, yet queue should be empty *)"
            (C.payload box)
        in
        Monolith.Invalid cause
  else
    match obox with
    | None ->
        let cause _doc = format "(* candidate returns None *)" in
        Monolith.Invalid cause
    | Some cbox ->
        (* Check that there exists an element [x] with minimum priority
           in the queue [q] and remove it. *)
        let (i, boxes) = M.min_binding !q in
        (* Now we have a potential difficulty. [box] is a candidate box; it is
           the result of the candidate implementation. On the other hand,
           [boxes] is a list of reference boxes. How are we supposed to tell
           which box in the list [boxes] corresponds to [box]? The answer is,
           we cannot tell, unless we use the payload as a way of comparing a
           reference box and a candidate box. This works provided unique
           payloads. This is a hack, and demonstrates a shortcoming of
           Monolith; but for now, this will do. *)
        match list_remove same_payload cbox boxes with
        | box, boxes ->
            q := if boxes = [] then M.remove i !q else M.add i boxes !q;
            box.busy <- None;
            Monolith.Valid (Some box)
        | exception Not_found ->
            let cause _doc =
              format
                "(* candidate finds a box with payload %d, which does not have minimum priority *)"
                (C.payload cbox)
            in
            Monolith.Invalid cause

let cardinal q =
  M.fold (fun _p xs c -> List.length xs + c) !q 0

let is_empty q =
  cardinal q = 0
