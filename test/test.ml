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

open Monolith

(* This is the reference implementation. *)
module R = Reference

(* This is the candidate implementation. *)
module C = IntPQueue

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

(* Random priorities and elements. *)

let elt =
  lt 16

let prio =
  lt 16

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

let () =

  let spec = unit ^> t in
  declare "create" spec R.create C.create;

  let spec = t ^> elt ^> prio ^> unit in
  declare "add" spec R.add C.add;

  let spec = t ^> nondet (option elt) in
  declare "remove" spec R.remove C.remove;

  let spec = t ^> bool in
  declare "is_empty" spec R.is_empty C.is_empty;

  let spec = t ^> int in
  declare "cardinal" spec R.cardinal C.cardinal;

  (* [repeat] is not tested. *)

  ()

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  dprintf "          open IntPQueue;;\n"

let () =
  let fuel = 32 in
  main fuel
