open AST
open Printf

let rec fprintf_list_infix s f c = function
  | [] -> ()
  | [x] -> fprintf c "%a" f x
  | x::xs -> 
    fprintf c "%a %s %a" 
      f x s (fprintf_list_infix s f) xs

let rec list_iter_alt f inbetween = function
  | [] -> ()
  | [x] -> f x
  | x :: xs -> f x; inbetween (); list_iter_alt f inbetween xs

let paren b c f = 
  if b then begin 
    fprintf c "("; f (); fprintf c ")"
  end else f ()

let tex_of_konst c = function
  | Empty SET -> fprintf c "\\{\\}"
  | Empty RLN -> fprintf c "\\emptyset"

let rec tex_of_op2 n c es op2 = 
  paren (n >= 2) c (fun () -> match op2 with
  | Union -> fprintf_list_infix "\\cup" (tex_of_exp 2) c es
  | Inter -> fprintf_list_infix "\\cap" (tex_of_exp 2) c es
  | Diff -> fprintf_list_infix "\\setminus" (tex_of_exp 2) c es
  | Seq -> fprintf_list_infix "\\semicolon" (tex_of_exp 2) c es
  | Cartesian -> fprintf_list_infix "\\times" (tex_of_exp 2) c es)

and string_of_dir = function
  | Write -> "W" 
  | Read -> "R"
  | WriteRead -> "M"
  | Atomic -> "A"
  | Plain -> "P"
  | Unv_Set -> "\\_"
  | Bar_Set -> "B"

and tex_of_op1 n c e op1 = 
  paren (n >= 3 && op1 != Set_to_rln) c (fun () -> match op1 with
  | Plus -> fprintf c "%a^+" (tex_of_exp 3) e
  | Star -> fprintf c "%a^*" (tex_of_exp 3) e
  | Opt -> fprintf c "%a^?" (tex_of_exp 3) e
  | Select (d1,d2) -> 
    fprintf c "\\mathrm{%s%s}(%a)"
      (string_of_dir d1)
      (string_of_dir d2)
      (tex_of_exp 0) e
  | Inv -> fprintf c "%a^{-1}" (tex_of_exp 3) e
  | Square -> fprintf c "%a^{2}" (tex_of_exp 3) e
  | Ext -> fprintf c "\\mathrm{ext}(%a)" (tex_of_exp 0) e
  | Int -> fprintf c "\\mathrm{int}(%a)" (tex_of_exp 0) e
  | NoId -> fprintf c "\\mathrm{noid}(%a)" (tex_of_exp 0) e
  | Set_to_rln -> fprintf c "[%a]" (tex_of_exp 0) e
  | Comp _ -> fprintf c "%a^\\mathsf{c}" (tex_of_exp 3) e)

and comma c () = fprintf c ","

and tex_of_exp n c = function
  | Konst k -> tex_of_konst c k
  | Var x -> tex_of_var c x
  | Op1 (op1, e) -> tex_of_op1 n c e op1
  | Op (op2, es) -> tex_of_op2 n c es op2
  | App (e,es) -> 
    paren (n > 2) c (fun () ->
        tex_of_exp 0 c e;
        paren true c (fun () -> 
            list_iter_alt (tex_of_exp 0 c) (comma c) es))
  | Bind _ -> fprintf c "\\mbox{\\color{red}[Local bindings not done yet]}"
  | BindRec _ -> fprintf c "\\mbox{\\color{red}[Local bindings not done yet]}"
  | Fun (xs,e) -> 
    paren (n > 1) c (fun () ->
      fprintf c "\\lambda %a \\ldotp %a" 
        (tex_of_formals false) xs
        (tex_of_exp 1) e)

and tex_of_formals b c = function
  | [] -> paren true c (fun () -> ())
  | [x] -> paren b c (fun () -> tex_of_var c x)
  | xs -> paren true c (fun () -> list_iter_alt (tex_of_var c) (comma c) xs)
    
and tex_of_var c x = fprintf c "\\var{%s}" x

and tex_of_name c x = fprintf c "\\name{%s}" x

and tex_of_binding c (x, e) = 
  begin match e with
  | Fun (xs,e) ->
    fprintf c "$%a%a = %a$" 
      tex_of_var x 
      (tex_of_formals true) xs
      (tex_of_exp 0) e
  | _ ->
    fprintf c "$%a = %a$" 
      tex_of_var x 
      (tex_of_exp 0) e
  end

let tex_of_test = function
  | Acyclic -> "\\kwd{acyclic}"
  | Irreflexive -> "\\kwd{irreflexive}"
  | TestEmpty -> "\\kwd{empty}"

let tex_of_test_type = function
  | Provides -> ""
  | Requires -> "\\KWD{undefined\\_unless}~"

let tex_of_ins c = function
  | Let bs -> 
    fprintf c "\\KWD{let}~"; 
    list_iter_alt (tex_of_binding c) (fun () -> fprintf c "~\\KWD{and}~") bs
  | Rec bs -> 
    fprintf c "\\KWD{let}~\\KWD{rec}~"; 
    list_iter_alt (tex_of_binding c) (fun () -> fprintf c "~\\KWD{and}~") bs 
  | Test (_, test, exp, name, test_type) -> 
    fprintf c "%s%s~$%a$"
      (tex_of_test_type test_type)
      (tex_of_test test)
      (tex_of_exp 0) exp;
    begin match name with 
    | None -> () 
    | Some name -> fprintf c "~\\kwd{as}~%a" tex_of_name name end;
  | UnShow xs ->
    fprintf c "\\KWD{unshow}~$";
    list_iter_alt (tex_of_var c) (fun () -> fprintf c ",") xs;
    fprintf c "$"
  | Show xs -> 
    fprintf c "\\KWD{show}~$";
    list_iter_alt (tex_of_var c) (fun () -> fprintf c ",") xs;
    fprintf c "$"
  | ShowAs (exp,name) -> 
    fprintf c "\\KWD{show}~$%a$~\\kwd{as}~$\\mathrm{%a}$" 
      (tex_of_exp 0) exp 
      tex_of_name name
  | Latex s -> 
    fprintf c "\\entercomment\n";
    fprintf c "\\noindent %s\n" s;
    fprintf c "\\exitcomment\n"

let tex_of_prog c name prog = 
  fprintf c "\\documentclass[12pt]{article}\n";
  fprintf c "\\input{herd2tex}\n";
  fprintf c "\\begin{document}\n";
  fprintf c "\\begin{source}\n";
  fprintf c "\\noindent \\modelname{%s}\n\n" name;
  List.iter (fprintf c "\\noindent %a\n\n" tex_of_ins) prog;
  fprintf c "\\end{source}\n";
  fprintf c "\\end{document}\n"
