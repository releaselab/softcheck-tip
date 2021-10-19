{
  open P
  open Lexing

  exception Lexing_error of string

  let kwd_tbl = ["malloc", MALLOC; "input", INPUT; "while", WHILE;
    "if", IF; "else", ELSE; "var", VAR; "return", RETURN; "null", NULL;
    "output", OUTPUT]

  let id_or_kwd s = try List.assoc s kwd_tbl with _ -> IDENT s
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = letter (letter | digit | '_')*
let integer = digit+
let space = ' ' | '\t'

rule next_token = parse
  | '\n'          { new_line lexbuf; next_token lexbuf }
  | space+        { next_token lexbuf }
  | ident as id   { id_or_kwd id }
  | '+'           { PLUS }
  | '-'           { MINUS }
  | '*'           { TIMES }
  | '/'           { DIV }
  | '='           { EQ }
  | "=="          { EQQ }
  | ">"           { GT }
  | '&'           { REF}
  | '('           { LP }
  | ')'           { RP }
  | '{'           { LB }
  | '}'           { RB }
  | ','           { COMMA }
  | ';'           { SEMICOLON }
  | integer as s  { try NUM (int_of_string s)
    with _ -> raise (Lexing_error ("constant too large: " ^ s)) }
  | eof           { EOF }
  | _ as c        { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }
