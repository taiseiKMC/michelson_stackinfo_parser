%{
open Syntax
%}

%token LPAREN RPAREN
%token CLPAREN CRPAREN
%token SLPAREN SRPAREN
%token SEMI COLON COMMA
%token START STOP
%token PAIR

%token <int> INTV
%token <Syntax.str> STR
%token <Syntax.prim> PRIM

%start toplevel
%type <Syntax.code> toplevel
%%

toplevel :
    e=Code SEMI SEMI { Exp e }

Code :
    e=Prim { e }
  | e=Block { e }

NoInfoCode :
    CLPAREN e=Prims CRPAREN { NISeq e }
  | p=PRIM { NIPrim (p, []) }

Block :
    CLPAREN e=Prims CRPAREN COMMA info=Info { Seq (e, info) }
  | CLPAREN e=Prims CRPAREN { NISeq e }
  | CLPAREN CRPAREN { NISeq [] }
  | CLPAREN CRPAREN COMMA info=Info { Seq ([], info) }

Prims :
    e=Prim { [e] }
  | e1=Prim SEMI e2=Prims { e1::e2 }

Prim :
    p=PRIM a=Args COMMA info=Info { Prim (p, a, info) }
  | p=PRIM COMMA info=Info { Prim (p, [], info) }
  | p=PRIM { NIPrim (p, []) }
  | p=PRIM a=Args { NIPrim (p, a) }
  | LPAREN e=Prim RPAREN { e }

Args :
    h=Args t=Arg { h @ [t] }
  | e=Arg { [e] }

Arg :
    e=AType { ArgTy e }
  | LPAREN e=Type RPAREN { ArgTy e }
  | e=Value { Arg e }
  | LPAREN e=Code RPAREN { ArgCode e }
  | e=NoInfoCode { ArgCode e }

Info :
    l=Location COMMA bef=Stack COMMA aft=Stack { (l, bef, aft) }
  | LPAREN e=Info RPAREN { e }

Location :
    START COLON LPAREN s=Position RPAREN COMMA STOP COLON LPAREN t=Position RPAREN { (s, t) }
  | LPAREN e=Location RPAREN { e }

Position :
    name=STR COLON v=INTV COMMA t=Position { (name, v)::t }
  | name=STR COLON v=INTV { [(name, v)] }

Stack :
    SLPAREN tys=TypeAry SRPAREN { tys }

TypeAry :
    h=Type COLON t=TypeAry { h::t }
  | ty=Type { [ty] }

Type :
    f=Type x=AType { App (f, x) }
  | e=AType { e }

AType :
    s=STR { Type s }
  | LPAREN ty=Type RPAREN { ty }

Value :
    s=STR { STRV s }
  | i=INTV { INTV i }
  | PAIR v1=Value v2=Value { PairV (v1, v2) }
  | LPAREN v=Value RPAREN { v }