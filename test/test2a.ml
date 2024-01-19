(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** hack of Definition of the OCaml AST *)


type newrec_flag2 = Asttypes.rec_flag = Nonrecursive | Recursive
                                        
and
  newrec_flag3 = Nonrecursive2 | Recursive2
  
and
  newrec_flag4 = Nonrecursive1 | Recursive1
  

