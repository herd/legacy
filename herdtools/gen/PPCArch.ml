(*********************************************************************)
(*                         Diy                                       *)
(*                                                                   *)
(*   Jade Alglave, Luc Maranget INRIA Paris-Rocquencourt, France.    *)
(*                                                                   *)
(*  Copyright 2010 Institut National de Recherche en Informatique et *)
(*  en Automatique. All rights reserved. This file is distributed    *)
(*  under the terms of the Lesser GNU General Public License.        *)
(*********************************************************************)

module type Config = sig
  val eieio : bool
  val naturalsize : MachSize.sz
end

(* Default: know about eieio and word size *)

module Config =
  struct
    let eieio = true
    let naturalsize = MachSize.Word
  end

module Make(C:Config)  =
  struct
    include PPCBase
    let tr_endian x = MachSize.tr_endian C.naturalsize x
    include MachAtom.Make
        (struct
          let naturalsize = Some C.naturalsize
          let endian = MachSize.Big
        end)


(**********)
(* Fences *)
(**********)

    type fence = Sync | LwSync | ISync | Eieio

    let is_isync = function
      | ISync -> true
      | _ -> false

    let compare_fence = compare

    let strong = Sync

    let pp_fence = function
      | Sync -> "Sync"
      | LwSync -> "LwSync"
      | ISync -> "ISync"
      | Eieio -> "Eieio"

    let fold_cumul_fences =
      if C.eieio then fun f r -> f Sync (f LwSync (f Eieio r))
      else fun f r -> f Sync (f LwSync r)

    let fold_all_fences f r = f ISync (fold_cumul_fences f r)

    let fold_some_fences = fold_cumul_fences

    open Code

    let orders f d1 d2 = match f,d1,d2 with
    | Sync,_,_ -> true
    | LwSync,W,R -> false
    | LwSync,_,_ -> true
    | ISync,_,_ -> false
    | Eieio,W,W -> true
    | Eieio,_,_ -> false


(********)
(* Deps *)
(********)
    include Dep

    let pp_dp = function
      | ADDR -> "Addr"
      | DATA -> "Data"
      | CTRL -> "Ctrl"
      | CTRLISYNC -> "CtrlIsync"

    include
        ArchExtra.Make
        (struct
          type arch_reg = reg
          let is_symbolic = function
            | Symbolic_reg _ -> true
            | _ -> false
          let pp_reg = pp_reg
          let free_registers = allowed_for_symb
        end)
  end
