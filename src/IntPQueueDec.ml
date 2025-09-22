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

(* This is a variant of IntPQueue, which supports changing the priority
   of an element while it is in the queue, testing whether an element is
   in the queue, and removing an element from the queue. To this end, at
   each level in the main array, instead of a stack of elements, we use
   a circular doubly-linked list of boxes, where each box holds an
   element. *)

module MyArray = Hector.Poly

type priority =
  int

let fail =
  invalid_arg

(* -------------------------------------------------------------------------- *)

(** A box holds an element and a priority. Furthermore, it participates
    in a circular doubly-linked list.

    A box may or may not be currently part of a priority queue. If it is
    currently part of a priority queue, then the [queue] field points to
    this queue, the [priority] field reflects its priority in the queue,
    and (via the fields [prev] and [next]) this box is part of the
    circular doubly-linked list associated with [queue] and [priority].
    If a box is not currently part of a priority queue, then its [queue]
    field is [None] and the fields [prev] and [next] point to this box
    itself. The [priority] field is the box's priority as last set. *)
type 'a box = {
  payload: 'a;
  mutable priority: int;
  mutable queue: 'a t option;
  mutable prev: 'a box;
  mutable next: 'a box;
}

(**A circular doubly-linked list of boxes, also known as a ring, is either
   empty or represented by a pointer to an arbitrary box in the list. *)
and 'a ring =
  'a box option

(**A priority queue.*)
and 'a t = {

  (* A priority queue is represented as a vector, indexed by priorities, of
     rings. There is no bound on the size of the main vector -- its size is
     increased if needed. It is up to the user to use priorities of
     reasonable magnitude. *)
  a: 'a ring MyArray.t;

  (* The index [best] is comprised between 0 (included) and the length of the
     array [a] (excluded). It can be the index of the lowest nonempty stack,
     if there is one; or it can be lower. In other words, from the index 0
     to the index [best] (excluded), every stack is empty. *)
  mutable best: int;

  (* Current number of elements in the queue. Used in [extract] to stop the
     search for a nonempty bucket. *)
  mutable cardinal: int;

}

(* -------------------------------------------------------------------------- *)

(* Checking well-formedness. (Debugging only.) *)

(* [iter ring yield] enumerates all boxes in the ring [ring]. *)

let iter (ring : 'a ring) (yield : 'a box -> unit) =
  match ring with
  | None ->
      ()
  | Some start ->
      yield start;
      let current = ref start.next in
      while !current != start do
        yield !current;
        current := !current.next
      done

(* [mem ring box] tests whether the box [box] appears in the ring [ring]. *)

let mem ring box =
  let exception Found in
  match iter ring (fun box' -> if box == box' then raise Found) with
  | exception Found -> true
  | ()              -> false

(* [check q] checks that the queue [q] is well-formed. *)

let check q =
  assert (0 <= q.best && q.best <= MyArray.length q.a);
  for i = 0 to q.best - 1 do
    let xs = MyArray.get q.a i in
    assert (xs = None);
  done;
  let c = ref 0 in
  for i = q.best to MyArray.length q.a - 1 do
    let ring = MyArray.get q.a i in
    iter ring @@ fun box ->
      assert (box.priority = i);
      assert (match box.queue with None -> false | Some q' -> q == q');
      assert (box.prev.next == box);
      assert (box.next.prev == box);
      incr c
  done;
  assert (q.cardinal = !c)

(* [check_box box] checks that the box [box] is well-formed. *)

let check_box box =
  match box.queue with
  | Some q ->
      let i = box.priority in
      assert (0 <= i && i < MyArray.length q.a);
      let ring = MyArray.get q.a i in
      assert (mem ring box)
  | None ->
      assert (box.next == box);
      assert (box.prev == box)

(* -------------------------------------------------------------------------- *)

(* Operations on boxes. *)

(* [box x] creates a new box whose payload is [x]. *)

let box x =
  let payload = x
  and priority = 0 (* dummy *)
  and queue = None in
  let rec box = { payload; priority; queue; prev = box; next = box } in
  box

let[@inline] payload box =
  box.payload

let[@inline] priority box =
  box.priority

let[@inline] queue box =
  box.queue

(* -------------------------------------------------------------------------- *)

(* Internal operations on rings. *)

module Ring = struct

  (* [insert ring box] inserts the box [box] into the ring [ring]. The box's
     [prev] and [next] fields need not be valid; they are overwritten. The
     fields other than [prev] and [next] are unaffected. The Boolean result
     indicates whether the ring was empty. *)

  let[@inline] insert (ring : 'a ring) (box : 'a box) : bool =
    match ring with
    | None ->
        true
    | Some start ->
        (* Make [box] the predecessor of [start] in the ring. *)
        box.next <- start;
        box.prev <- start.prev;
        start.prev.next <- box;
        start.prev <- box;
        false

  (* [half_extract box] extracts the box [box] out of its ring. This box's
     neighbors in the ring are updated, so the ring shrinks, but the box's
     [prev] and [next] fields are not updated. They must be updated before
     this box is returned to the user. *)

  let[@inline] half_extract (box : 'a box) : unit =
    let prev = box.prev
    and next = box.next in
    next.prev <- prev;
    prev.next <- next

  (* [extract box] extracts the box [box] out of its ring. This box becomes
     isolated in a ring of length 1. The fields other than [prev] and [next]
     are unaffected. *)

  let[@inline] extract (box : 'a box) : unit =
    half_extract box;
    box.next <- box;
    box.prev <- box

end (* Ring *)

(* -------------------------------------------------------------------------- *)

let create () =
  (* Set up the main array so that it initially has 16 priority levels. When
     a new level is added, it must be initialized with an empty circular
     list. *)
  let a = MyArray.make 16 None in
  { a; best = 0; cardinal = 0 }

let[@inline] grow q priority =
  if MyArray.length q.a <= priority then begin
    MyArray.ensure_capacity q.a (priority + 1);
    while MyArray.length q.a <= priority do
      MyArray.push q.a None
    done
  end

let add q box priority =
  match box.queue with
  | Some _ ->
      fail "add: this box is already a member of a priority queue"
  | None ->
      assert (0 <= priority);
      q.cardinal <- q.cardinal + 1;
      (* Grow the main array if necessary. *)
      grow q priority;
      assert (priority < MyArray.length q.a);
      (* Find out which ring we should insert into. *)
      let ring = MyArray.unsafe_get q.a priority in
      (* Insert. *)
      if Ring.insert ring box then
        MyArray.unsafe_set q.a priority (Some box);
      box.priority <- priority;
      box.queue <- Some q;
      (* Decrease [q.best], if necessary, so as not to miss the new element. In
         the special case of Dijkstra's algorithm or A*, this never happens. *)
      if priority < q.best then
        q.best <- priority

let[@inline] is_empty q =
  q.cardinal = 0

let[@inline] cardinal q =
  q.cardinal

let rec extract_nonempty q =
  assert (0 < q.cardinal);
  assert (0 <= q.best && q.best < MyArray.length q.a);
  (* Look for the next nonempty bucket. We know there is one. This may seem
     inefficient, because it is a linear search. However, in applications
     where [q.best] never decreases, the cumulated cost of this loop is the
     maximum priority ever used, which is good. *)
  let ring = MyArray.unsafe_get q.a q.best in
  match ring with
  | None ->
      q.best <- q.best + 1;
      extract_nonempty q
  | Some start ->
      q.cardinal <- q.cardinal - 1;
      let box =
        if start.next == start then begin
          (* This ring becomes empty. *)
          MyArray.unsafe_set q.a q.best None;
          start
        end
        else
          (* This ring does not become empty. Instead of extracting [start],
             we extract the box [start.prev], so [start] remains a member of
             the ring. Therefore there is no need to update the main array. *)
          let box = start.prev in
          Ring.extract box;
          box
      in
      box.queue <- None;
      box

let[@inline] extract q =
  if q.cardinal = 0 then
    None
  else
    Some (extract_nonempty q)

let remove box =
  match box.queue with
  | None ->
      fail "remove: this box is not currently a member of any priority queue"
  | Some q ->
      assert (0 < q.cardinal);
      let i = box.priority in
      let ring = MyArray.unsafe_get q.a i in
      match ring with
      | None ->
          assert false
      | Some start ->
          q.cardinal <- q.cardinal - 1;
          let prev = start.prev in
          Ring.extract box;
          box.queue <- None;
          if start == box then
            MyArray.unsafe_set q.a i (if prev == box then None else Some prev)

(* [update box priority] is equivalent to [remove box; add q box priority],
   where [box.queue = Some q]. By composing the two operations, we are able
   to avoid some writes to memory. To begin with, if [priority] is equal to
   [box.priority] then there is nothing to do. If the two priorities differ,
   then 1- there is no need to update [q.cardinal]; 2- there is no need to
   update [box.queue]; 3- the box can be extracted using [Ring.half_extract]
   instead of [Ring.extract]; the fields [box.prev] and [box.next] are then
   updated by [Ring.insert]. *)

let update box priority =
  match box.queue with
  | None ->
      fail "update: this box is not currently a member of any priority queue"
  | Some q ->
      let i = box.priority in
      if i <> priority then
        let ring = MyArray.unsafe_get q.a i in
        match ring with
        | None ->
            assert false
        | Some start ->
            let prev = start.prev in
            Ring.half_extract box;
            if start == box then
              MyArray.unsafe_set q.a i (if prev == box then None else Some prev);
            grow q priority;
            assert (priority < MyArray.length q.a);
            let ring = MyArray.unsafe_get q.a priority in
            if Ring.insert ring box then
              MyArray.unsafe_set q.a priority (Some box);
            box.priority <- priority;
            if priority < q.best then
              q.best <- priority

let repeat q f =
  while q.cardinal > 0 do
    let x = extract_nonempty q in
    f x
  done
