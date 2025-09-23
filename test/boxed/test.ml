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

open Monolith

(* This is the reference implementation. *)
module R = Reference

(* This is the candidate implementation. *)
module C = IntPQueue.Boxed

let () =
  dprintf "          open IntPQueue.Boxed;;\n"

(* -------------------------------------------------------------------------- *)

(* The abstract type [t]. *)

(* This type is equipped with a well-formedness check,
   which ignores the model (the reference side). *)

let check _model =
  C.check,
  constant "check"

let t =
  declare_abstract_type ~check ()

(* -------------------------------------------------------------------------- *)

(* The abstract type [box]. *)

(* This type does not have a well-formedness check. *)

let box =
  declare_abstract_type ()

(* -------------------------------------------------------------------------- *)

(* Random priorities and sequentially-generated elements. *)

(* Our reference implementation of [extract] relies on the fact that we use
   unique (sequentially generated) elements. Indeed, this allows us to compare
   a reference box and a candidate box by comparing their payloads. *)

let elt =
  sequential()

let priority =
  lt 16

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

let lone box =
  not (R.busy box)

let () =

  let spec = elt ^> box in
  declare "box" spec R.box C.box;

  let spec = box ^> elt in
  declare "payload" spec R.payload C.payload;

  let spec = box ^> priority in
  declare "priority" spec R.priority C.priority;

  let spec = box ^> bool in
  declare "busy" spec R.busy C.busy;

  let spec = unit ^> t in
  declare "create" spec R.create C.create;

  let spec = t ^> box ^> bool in
  declare "mem" spec R.mem C.mem;

  let spec = t ^> lone % box ^> priority ^> unit in
  declare "add" spec R.add C.add;

  let spec = t ^> nondet (option box) in
  declare "extract" spec R.extract C.extract;

  let spec = t ^>> fun q -> R.mem q % box ^> unit in
  declare "remove" spec R.remove C.remove;

  let spec = t ^>> fun q -> R.mem q % box ^> priority ^> unit in
  declare "update" spec R.update C.update;

  let spec = t ^> bool in
  declare "is_empty" spec R.is_empty C.is_empty;

  let spec = t ^> int in
  declare "cardinal" spec R.cardinal C.cardinal;

  (* [repeat] is not tested. *)

  ()

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  let fuel = 32 in
  main fuel
