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

(**This module implements a fast and compact priority queue whose keys are
   nonnegative integers. The user must use only {i low} priorities, because
   the space occupied by the priority queue is linear in the magnitude of
   the priorities. *)

(**The type of priority queues. *)
type 'a t

(**[create()] creates an empty priority queue. *)
val create: unit -> 'a t

(**[add q x p] inserts the element [x] with priority [p] into the queue [q]. *)
val add: 'a t -> 'a -> int -> unit

(**[extract q] extracts an element with minimum priority out of the queue [q]
   and returns it. *)
val extract: 'a t -> 'a option

(**[is_empty q] tests whether the queue [q] is empty. *)
val is_empty: 'a t -> bool

(**[cardinal q] returns the number of elements in the queue [q]. *)
val cardinal: 'a t -> int

(**[repeat q f] repeatedly extracts an element with minimum priority out of [q]
   and passes it to [f] (which may insert new elements into [q]), until [q] is
   exhausted. *)
val repeat: 'a t -> ('a -> unit) -> unit

(**/**)
(**[check] is used only during testing. *)
val check : 'a t -> unit
