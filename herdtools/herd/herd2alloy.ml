(*********************************************************************)
(*                        Herd                                       *)
(*                                                                   *)
(* Luc Maranget, INRIA Paris-Rocquencourt, France.                   *)
(* Jade Alglave, University College London, UK.                      *)
(* John Wickerson, Imperial College London, UK.                      *)
(*                                                                   *)
(*  Copyright 2013 Institut National de Recherche en Informatique et *)
(*  en Automatique and the authors. All rights reserved.             *)
(*  This file is distributed  under the terms of the Lesser GNU      *)
(*  General Public License.                                          *)
(*********************************************************************)


open AST
open Printf

let with_sc = ref false

let in_ignore_section = ref false 

let rec fprintf_iter s f chan = function
  | [] -> ()
  | [x] -> fprintf chan "%a" f x
  | x::xs -> fprintf chan "%a%s%a" f x s (fprintf_iter s f) xs
       
let rec fprintf_list s f chan = function
  | [] -> ()
  | [x] -> fprintf chan "%a" f x
  | x::xs -> fprintf chan "(%s %a %a)" s f x (fprintf_list s f) xs

let rec fprintf_list_infix s f chan = function
  | [] -> ()
  | [x] -> fprintf chan "%a" f x
  | x::xs -> 
    fprintf chan "(%a %s %a)" 
      f x s (fprintf_list_infix s f) xs

let alloy_of_konst chan = function
  | Empty SET -> fprintf chan "emps"
  | Empty RLN -> fprintf chan "empr"

let alloy_of_dir = function
  | Write -> "x.W"
  | Read -> "x.R"
  | WriteRead -> "x.(W+R)"
  | Atomic -> "x.A"
  | Plain -> "x.(ev-A)"
  | Unv_Set -> "x.ev"
		 
let rec alloy_of_op2 (use_preds:bool) args chan es = function
  | Union ->
     fprintf_list_infix "+" (alloy_of_exp use_preds args) chan es
  | Inter ->
     fprintf_list_infix "&" (alloy_of_exp use_preds args) chan es
  | Diff ->
     fprintf_list_infix "-" (alloy_of_exp use_preds args) chan es
  | Seq ->
     fprintf_list_infix "." (alloy_of_exp use_preds args) chan es
  | Cartesian ->
     fprintf_list_infix "->" (alloy_of_exp use_preds args) chan es
  | _ -> Warn.fatal "alloy_of_op2"

and alloy_of_op1 (use_preds:bool) args chan e = function
  | Plus ->
     fprintf chan "(^%a)" (alloy_of_exp use_preds args) e
  | Star ->
     fprintf chan "(*%a)" (alloy_of_exp use_preds args) e
  | Opt ->
     fprintf chan "(rc[%a])" (alloy_of_exp use_preds args) e
  | Select (d1,d2) ->
     fprintf chan "((%s -> %s) & %a)"
	     (alloy_of_dir d1) (alloy_of_dir d2)
	     (alloy_of_exp use_preds args) e 
  | Inv ->
     fprintf chan "(~%a)" (alloy_of_exp use_preds args) e
  | Square ->
     fprintf chan "(%a -> %a)"
	     (alloy_of_exp use_preds args) e
	     (alloy_of_exp use_preds args) e
  | Ext ->
     fprintf chan "(%a - x.sthd)" (alloy_of_exp use_preds args) e
  | Int ->
     fprintf chan "(%a & x.sthd)" (alloy_of_exp use_preds args) e
  | NoId ->
     fprintf chan "(%a - iden)" (alloy_of_exp use_preds args) e
  | Set_to_rln ->
     fprintf chan "(stor[%a])" (alloy_of_exp use_preds args) e
  | Comp SET ->
     fprintf chan "(x.ev - %a)" (alloy_of_exp use_preds args) e
  | Comp RLN ->
     fprintf chan "((x.ev -> x.ev) - %a)"
	     (alloy_of_exp use_preds args) e
  | _ -> Warn.fatal "Unknown operator in herd2alloy"
		    
and alloy_of_var (use_preds:bool) args chan x = 
  match x with
  | "asw" | "lo" | "acq" | "rel"
  | "sc" | "R" | "W" | "F" | "A"
  | "con"
    -> fprintf chan "(x.%s)" x
  | "thd" -> fprintf chan "(x.sthd)"
  | "loc" -> fprintf chan "(x.sloc)"
  | "rf" -> fprintf chan "rf"
  | "rfe" -> fprintf chan "(rf - x.sthd)"
  | "co" -> fprintf chan "co"
  | "coe" -> fprintf chan "(co - x.sthd)"
  | "fr" -> fprintf chan "(fr[x,rf,co])"
  | "fre" -> fprintf chan "(fr[x,rf,co] - x.sthd)"
  | "atom" ->
     fprintf chan "(none -> none)" (* ignore atomic stuff for now *)
  | "po-loc" -> fprintf chan "(x.sb & x.sloc)"
  | "po" | "sb" -> fprintf chan "(x.sb)"
  | "addr" -> fprintf chan "(x.ad)"
  | "data" -> fprintf chan "(x.dd)"
  | "ctrl" -> fprintf chan "(x.cd)"
  | "nonatomicloc" -> fprintf chan "(x.naL)"
  | "MFENCE" -> fprintf chan "(x.F)"
  | "S" -> fprintf chan "s"
  | "I" -> fprintf chan "I"
  | "M" | "_" -> fprintf chan "x.ev"
  | "id" -> fprintf chan "iden"
  | _ -> 
    let x = Str.global_replace (Str.regexp_string "-") "_" x in
    if List.mem x args or (not use_preds) then
      fprintf chan "%s" x
    else
      fprintf chan "(%s[x,rf,co%s])" x (if !with_sc then ",s" else "")

and alloy_of_exp (use_preds : bool) args chan = function
  | Konst (_,k) -> alloy_of_konst chan k
  | Var (_,x) -> alloy_of_var use_preds args chan x
  | Op1 (_,op1, e) -> alloy_of_op1 use_preds args chan e op1
  | Op (_,op2, es) -> alloy_of_op2 use_preds args chan es op2
  | App (_,_,_) -> fprintf chan "Functions not done yet"
  | Bind _ -> fprintf chan "Bindings not done yet"
  | BindRec _ -> fprintf chan "Recursive bindings not done yet"
  | Fun _ -> fprintf chan "Local functions not done yet"
  | _ -> Warn.fatal "explicitset/match/tag etc. in herd2alloy"

and alloy_of_binding (use_preds : bool) chan (x, e) = 
  match e with
  | Fun (_,_,_,_,_) ->
     fprintf chan "Functions not done yet"
  | _ ->
     if x = "alloy_ignore_section_begin" then
       in_ignore_section := true
     else if x = "alloy_ignore_section_end" then
       in_ignore_section := false
     else if not !in_ignore_section then
       if use_preds then
	 fprintf chan "fun %s[x : BasicExec, rf, co%s : E -> E] : E -> E {\n  %a\n}\n\n" 
		 x
		 (if !with_sc then ", s" else "")
		 (alloy_of_exp true []) e
       else
	 fprintf chan "let %s = %a |\n" x (alloy_of_exp false []) e

let alloy_of_test = function
  | Acyclic -> "is_acyclic"
  | Irreflexive -> "irreflexive"
  | TestEmpty -> "is_empty"

let provides : string list ref = ref []
let requires : string list ref = ref []
let seen_requires_clause : bool ref = ref false
					  

let print_derived_relations b chan = function
  | Let (_,bs) -> List.iter (alloy_of_binding b chan) bs
  | _ -> ()

let print_predicates chan = function
  | Test (_,_, test, exp, name, test_type) ->
     let name = begin
	 match name with 
	 | None ->
	    Warn.user_error "You need to give each constraint a name!"
         | Some name -> name 
       end
     in
    fprintf chan "pred %s[x : BasicExec, rf, co%s : E -> E] {\n  %s[%a]\n}\n\n" 
	    name
	    (if !with_sc then ", s" else "")
	    (alloy_of_test test)
	    (alloy_of_exp true []) exp;
    begin match test_type with
    | Provides -> 
       if (!seen_requires_clause) then
	 Warn.user_error "Provides-clause follows requires-clause!";
       provides := name :: (!provides)
    | Requires ->
       seen_requires_clause := true;
       requires := name :: (!requires)
    end
  | _ -> ()

let print_provide_predicates chan = function
  | Test (_,_, test, exp, name, Provides) ->
     let name = begin
	 match name with 
	 | None ->
	    Warn.user_error "You need to give each constraint a name!"
         | Some name -> name 
       end
     in
     fprintf chan "let %s = %s[%a] |\n" 
	     name
	     (alloy_of_test test)
	     (alloy_of_exp false []) exp;
     provides := name :: (!provides)
  | _ -> ()

let print_require_predicates chan = function
  | Test (_,_, test, exp, name, Requires) ->
     let name = begin
	 match name with 
	 | None ->
	    Warn.user_error "You need to give each constraint a name!"
         | Some name -> name 
       end
     in
    fprintf chan "let %s = %s[%a] |\n" 
	    name
	    (alloy_of_test test)
	    (alloy_of_exp false []) exp;
    requires := name :: (!requires)
  | _ -> ()

let alloy_of_prog chan (with_sc_arg:bool) (use_preds:bool) prog =

  with_sc := with_sc_arg;
  
  match use_preds with
  | true -> begin
 
      List.iter (print_derived_relations true chan) prog;
      
      List.iter (print_predicates chan) prog;
      
      fprintf chan "pred consistent[x : BasicExec, rf, co : E -> E] {\n      ";
      if !with_sc then
	fprintf chan "  some s : E -> E when ax_wfS[x,s] |\n      ";
      (*  fprintf chan "  some y : Extras | wf_Extras[x,y%s] &&\n    " (if !with_sc then ",s" else ""); *)

      fprintf chan "no none\n";

      List.iter (fun s -> fprintf chan "&& %s[x,rf,co%s]\n" s (if !with_sc then ",s" else "")) (List.rev !provides);
      
      fprintf chan "\n}\n\n";
      
      fprintf chan "pred not_faulty[x : BasicExec, rf : E -> E] {\n      ";

      fprintf chan "no none\n";

      List.iter (fun s -> fprintf chan "&& %s[x,rf,none->none%s]\n" s (if !with_sc then ",none->none" else "")) (List.rev !requires);
      
      fprintf chan "\n}\n\n"
    end
	      
  | false -> begin

      fprintf chan "fun hbfun[x : BasicExec, rf : E -> E] : E -> E {\n      ";

      fprintf chan "let co = none->none |\n";
      
      if !with_sc then fprintf chan "let s = none->none |\n";

      List.iter (print_derived_relations false chan) prog;

      fprintf chan "hb\n";
      
      fprintf chan "\n}\n\n";

      fprintf chan "pred consistent[x : BasicExec, rf, co : E -> E] {\n      ";
      if !with_sc then
	fprintf chan "  some s : E -> E when ax_wfS[x,s] |\n      ";

      List.iter (print_derived_relations false chan) prog;

      List.iter (print_provide_predicates chan) prog;

      fprintf chan "no none\n";

      List.iter (fun s -> fprintf chan "&& %s\n" s) (List.rev !provides);
      
      fprintf chan "\n}\n\n";

      fprintf chan "pred not_faulty[x : BasicExec, rf : E -> E] {\n      ";
      fprintf chan "let co = none->none |\n";

      if !with_sc then fprintf chan "let s = none->none |\n";

      List.iter (print_derived_relations false chan) prog;

      List.iter (print_require_predicates chan) prog;

      fprintf chan "no none\n";

      List.iter (fun s -> fprintf chan "&& %s\n" s) (List.rev !requires);

      fprintf chan "\n}\n\n"
      
      
    end
  
