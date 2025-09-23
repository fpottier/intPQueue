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

(**A priority is a nonnegative integer. *)
type priority =
  int

(**A box carries a payload, that is, a value of type ['a]. This payload
   cannot be modified. Furthermore, a box has an inherent priority, a
   nonnegative integer value. The priority of a box is modified by certain
   operations, such as {!add} and {!update}. Finally, at any point in time,
   a box is either stand-alone or a member of a priority queue. *)
type 'a box

(**A priority queue can be thought of as a set of boxes. *)
type 'a t

(**[box x] creates a new box whose payload is [x] and whose priority is
   unspecified. *)
val box: 'a -> 'a box

(**[payload box] returns the payload of the box [box]. *)
val payload: 'a box -> 'a

(**[priority box] returns the current priority of the box [box]. If this box
   is currently a member of a queue [q], then this is its current priority
   in the queue [q]. If this box is currently isolated, then this is the
   box's last known priority, as set by {!add} or {!update}. *)
val priority: 'a box -> priority

(**[busy box] determines whether the box [box] is currently a member of
   some priority queue. *)
val busy: 'a box -> bool

(**[mem q box] determines whether the box [box] is currently a member of
   the priority queue [q]. *)
val mem: 'a t -> 'a box -> bool

(**[create()] creates a new empty priority queue. *)
val create: unit -> 'a t

(**[add q box p] sets the priority of the box [box] to [p] and inserts this
   box into the queue [q]. This box must be isolated, that is, not already a
   member of a priority queue. *)
val add: 'a t -> 'a box -> priority -> unit

(**[extract q] extracts a box with minimum priority out of the queue [q]
   and returns it. *)
val extract: 'a t -> 'a box option

(**[remove q box] extracts the box [box] out of the priority queue [q].
   This box must be a member of the queue [q]. *)
val remove: 'a t -> 'a box -> unit

(**[update q box i] sets the priority of the box [box] to [i]. This box
   must be a member of the queue [q]. The call [update box p]
   is then equivalent to [remove box; add q box p].

   If the condition [mem q box] is violated, then [update q box i] cannot be
   expected to fail: it can seem to silently succeed. To avoid this problem,
   it is recommended to write [assert (mem q box); update q box i]. *)
val update: 'a t -> 'a box -> priority -> unit

(**[is_empty q] tests whether the queue [q] is empty. *)
val is_empty: 'a t -> bool

(**[cardinal q] returns the number of boxes in the queue [q]. *)
val cardinal: 'a t -> int

(**[repeat q f] repeatedly extracts a box with minimum priority out of [q]
   and passes it to [f] (which may insert new boxes into [q]), until [q] is
   exhausted. *)
val repeat: 'a t -> ('a box -> unit) -> unit

(**/**)

(**[check] is used only during testing. *)
val check: 'a t -> unit
