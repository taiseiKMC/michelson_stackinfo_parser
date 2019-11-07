{
let reservedWords = [
  (* Keywords *)
  ("start", Parser.START);
  ("stop", Parser.STOP);
  ("Pair", Parser.PAIR);
]
}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }

| "-"? ['0'-'9']+
    { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }
| "{" { Parser.CLPAREN }
| "}" { Parser.CRPAREN }
| "[" { Parser.SLPAREN }
| "]" { Parser.SRPAREN }
| ";" { Parser.SEMI }
| ":" { Parser.COLON }
| "," { Parser.COMMA }

| ['A'-'Z'] ['A'-'Z' '0'-'9' '_' '\'']*
    { let str = Lexing.lexeme lexbuf in
      try
        List.assoc str reservedWords
      with
      _ -> Parser.PRIM str
     }

| ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
    { let str = Lexing.lexeme lexbuf in
      try
        List.assoc str reservedWords
      with
      _ -> Parser.STR str
     }

| '"' ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']* '"'
    { Parser.STR (Lexing.lexeme lexbuf) }
| eof { exit 0 }


