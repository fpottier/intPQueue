(******************************************************************************)
(*                                                                            *)
(*                                  IntPQueue                                 *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*       Copyright 2024--2024 Inria. All rights reserved. This file is        *)
(*       distributed under the terms of the GNU Library General Public        *)
(*       License, with an exception, as described in the file LICENSE.        *)
(*                                                                            *)
(******************************************************************************)

module MyArray = Hector.Poly
module MyStack = Hector.Poly

type 'a t = {

  (* A priority queue is represented as a vector, indexed by priorities, of
     stacks. There is no bound on the size of the main vector -- its size is
     increased if needed. It is up to the user to use priorities of reasonable
     magnitude. *)
  a: 'a MyStack.t MyArray.t;

  (* The index [best] is comprised between 0 (included) and the length of the
     array [a] (excluded). It can be the index of the lowest nonempty stack,
     if there is one; or it can be lower. In other words, from the index 0
     to the index [best] (excluded), every stack is empty. *)
  mutable best: int;

  (* Current number of elements in the queue. Used in [remove] to stop the
     search for a nonempty bucket. *)
  mutable cardinal: int;

}

let check q =
  assert (0 <= q.best && q.best <= MyArray.length q.a);
  for i = 0 to q.best - 1 do
    let xs = MyArray.get q.a i in
    assert (MyStack.length xs = 0);
  done;
  let c = ref 0 in
  for i = q.best to MyArray.length q.a - 1 do
    let xs = MyArray.get q.a i in
    c := !c + MyStack.length xs
  done;
  assert (q.cardinal = !c)

let create () =
  (* Set up the main array so that it initially has 16 priority levels. When
     a new level is added, it must be initialized with a fresh empty stack. *)
  let a = MyArray.init 16 (fun _i -> MyStack.create()) in
  { a; best = 0; cardinal = 0 }

let add q x priority =
  assert (0 <= priority);
  q.cardinal <- q.cardinal + 1;
  (* Grow the main array if necessary. *)
  if MyArray.length q.a <= priority then begin
    MyArray.ensure_capacity q.a (priority + 1);
    while MyArray.length q.a <= priority do
      MyArray.push q.a (MyStack.create())
    done
  end;
  assert (priority < MyArray.length q.a);
  (* Find out which stack we should push into. *)
  let xs = MyArray.unsafe_get q.a priority in
  (* Push. *)
  MyStack.push xs x;
  (* Decrease [q.best], if necessary, so as not to miss the new element. In
     the special case of Dijkstra's algorithm or A*, this never happens. *)
  if priority < q.best then
    q.best <- priority

let[@inline] is_empty q =
  q.cardinal = 0

let[@inline] cardinal q =
  q.cardinal

let rec remove_nonempty q =
  assert (0 < q.cardinal);
  assert (0 <= q.best && q.best < MyArray.length q.a);
  (* Look for the next nonempty bucket. We know there is one. This may seem
     inefficient, because it is a linear search. However, in applications
     where [q.best] never decreases, the cumulated cost of this loop is the
     maximum priority ever used, which is good. *)
  let xs = MyArray.unsafe_get q.a q.best in
  if MyStack.length xs = 0 then begin
    (* As noted below, [MyStack.pop] does not physically shrink the stack.
       When we find that a priority level has become empty, we physically
       empty it, so as to free the (possibly large) space that it takes up.
       This strategy is good when the client is Dijkstra's algorithm or A*. *)
    MyStack.reset xs;
    q.best <- q.best + 1;
    remove_nonempty q
  end
  else begin
    q.cardinal <- q.cardinal - 1;
    MyStack.pop xs
    (* Note: [MyStack.pop] does not shrink the physical array underlying the
       stack. This is good, because we are likely to push new elements into
       this stack. *)
  end

let remove q =
  if q.cardinal = 0 then
    None
  else
    Some (remove_nonempty q)

let repeat q f =
  while q.cardinal > 0 do
    let x = remove_nonempty q in
    f x
  done
