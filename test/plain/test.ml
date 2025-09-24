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
module C = IntPQueue.Plain

let () =
  dprintf "          open IntPQueue.Plain;;\n"

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

let priority =
  lt 16

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

let () =

  let spec = unit ^> t in
  declare "create" spec R.create C.create;

  let spec = t ^> elt ^> priority ^> unit in
  declare "add" spec R.add C.add;

  let spec = t ^> nondet (option elt) in
  declare "extract" spec R.extract C.extract;

  (* [extract'] is not tested. *)

  let spec = t ^> bool in
  declare "is_empty" spec R.is_empty C.is_empty;

  let spec = t ^> int in
  declare "cardinal" spec R.cardinal C.cardinal;

  (* [repeat] is not tested. *)

  let spec = t ^> unit in
  declare "reset" spec R.reset C.reset;

  ()

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  let fuel = 32 in
  main fuel
