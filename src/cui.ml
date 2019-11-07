

let read_eval_print inchannel =
  print_string "# ";
  flush stdout;
  let decl = Parser.toplevel Lexer.main (Lexing.from_channel inchannel) in
  Syntax.print_code Format.std_formatter decl;
  print_newline ()

