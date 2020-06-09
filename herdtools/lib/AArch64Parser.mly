/*********************************************************************/
/*                        DIY                                        */
/*                                                                   */
/* Luc Maranget, INRIA Paris-Rocquencourt, France.                   */
/*                                                                   */
/*  Copyright 2015 Institut National de Recherche en Informatique et */
/*  en Automatique and the authors. All rights reserved.             */
/*  This file is distributed  under the terms of the Lesser GNU      */
/*  General Public License.                                          */
/*********************************************************************/

%{
open AArch64Base
%}

%token EOF
%token <AArch64Base.reg> ARCH_XREG
%token <string> SYMB_XREG
%token <AArch64Base.reg> ARCH_WREG
%token <string> SYMB_WREG
%token <int> NUM
%token <string> NAME
%token <int> PROC

%token SEMI COMMA PIPE COLON LBRK RBRK
%token SXTW

/* Instructions */
%token B BEQ BNE CBZ CBNZ
%token LDR STR LDAR LDXR LDAXR STLR STXR STLXR CMP
%token MOV ADD EOR SUBS
%token DMB DSB ISB
%token SY ST LD
%token OSH OSHST OSHLD
%token ISH ISHST ISHLD
%token NSH NSHST NSHLD

%type <int list * (AArch64Base.pseudo) list list * MiscParser.gpu_data option> main 
%start  main

%nonassoc SEMI
%%
main:
| semi_opt proc_list iol_list EOF { $2,$3,None }

semi_opt:
| { () }
| SEMI { () }

proc_list:
| PROC SEMI
    {[$1]}

| PROC PIPE proc_list  { $1::$3 }

iol_list :
|  instr_option_list SEMI
    {[$1]}
|  instr_option_list SEMI iol_list {$1::$3}

instr_option_list :
  | instr_option
      {[$1]}
  | instr_option PIPE instr_option_list 
      {$1::$3}

instr_option :
|            { Nop }
| NAME COLON instr_option { Label ($1,$3) }
| instr      { Instruction $1}

reg:
| SYMB_XREG { V64,Symbolic_reg $1 }
| ARCH_XREG { V64,$1 }
| SYMB_WREG { V32,Symbolic_reg $1 }
| ARCH_WREG { V32,$1 }

xreg:
| SYMB_XREG { Symbolic_reg $1 }
| ARCH_XREG { $1 }

wreg:
| SYMB_WREG { Symbolic_reg $1 }
| ARCH_WREG { $1 }

k:
| NUM { $1 }

kr:
| k { K $1 }
| xreg { RV (V64,$1) }
| wreg COMMA SXTW { RV (V32,$1) }

kr0:
| { K 0 }
| COMMA kr { $2 }

kwr:
| k { K $1 }
| wreg { RV (V32,$1) }

instr:
/* Branch */
| B NAME { I_B $2 }
| BEQ NAME { I_BC (EQ,$2) }
| BNE NAME { I_BC (NE,$2) }
| CBZ reg COMMA NAME   { let v,r = $2 in I_CBZ (v,r,$4) }
| CBNZ reg COMMA NAME  { let v,r = $2 in I_CBNZ (v,r,$4) }
/* Memory */
| LDR reg COMMA LBRK xreg kr0 RBRK
  { let v,r = $2 in I_LDR (v,r,$5,$6) }
| LDAR reg COMMA LBRK xreg RBRK
  { let v,r = $2 in I_LDAR (v,AA,r,$5) }
| LDXR reg COMMA LBRK xreg RBRK
  { let v,r = $2 in I_LDAR (v,XX,r,$5) }
| LDAXR reg COMMA LBRK xreg RBRK
  { let v,r = $2 in I_LDAR (v,AX,r,$5) }
| STR reg COMMA LBRK xreg kr0 RBRK
  { let v,r = $2 in I_STR (v,r,$5,$6) }
| STLR reg COMMA LBRK xreg RBRK
  { let v,r = $2 in I_STLR (v,r,$5) }
| STXR wreg COMMA reg COMMA LBRK xreg RBRK
  { let v,r = $4 in I_STXR (v,YY,$2,r,$7) }
| STLXR wreg COMMA reg COMMA LBRK xreg RBRK
  { let v,r = $4 in I_STXR (v,LY,$2,r,$7) }

/* Operations */
| MOV reg COMMA k
  { let v,r = $2 in I_MOV (v,r,$4) }
| SXTW xreg COMMA wreg
  { I_SXTW ($2,$4) }
| ADD xreg COMMA xreg COMMA kr
  { I_OP3 (V64,ADD,$2,$4,$6) }
| ADD wreg COMMA wreg COMMA kwr
  { I_OP3 (V32,ADD,$2,$4,$6) }
| EOR xreg COMMA xreg COMMA kr
  { I_OP3 (V64,EOR,$2,$4,$6) }
| EOR wreg COMMA wreg COMMA kwr
  { I_OP3 (V32,EOR,$2,$4,$6) }
| SUBS xreg COMMA xreg COMMA kr
  { I_OP3 (V64,SUBS,$2,$4,$6) }
| SUBS wreg COMMA wreg COMMA kwr
  { I_OP3 (V32,SUBS,$2,$4,$6) }
| CMP wreg COMMA k
  { I_OP3 (V32,SUBS,ZR,$2,K $4) }
| CMP xreg COMMA k
  { I_OP3 (V64,SUBS,ZR,$2,K $4) }
/* Fences */
| DMB fenceopt
  { let d,t = $2 in I_FENCE (DMB (d,t)) }
| DSB fenceopt
  { let d,t = $2 in I_FENCE (DSB (d,t)) }
| ISB
  { I_FENCE ISB }

fenceopt:
| SY
  { SY,FULL }
| ST
  { SY,ST }
| LD
  { SY,LD }
| OSH
  { OSH,FULL }
| OSHST
  { OSH,ST }
| OSHLD
  { OSH,LD }
| ISH
  { ISH,FULL }
| ISHST
  { ISH,ST }
| ISHLD
  { ISH,LD }
| NSH
  { NSH,FULL }
| NSHST
  { NSH,ST }
| NSHLD
  { NSH,LD}
