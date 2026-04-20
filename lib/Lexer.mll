{
  open Parser

  let keyword_or_ident = function
    | "true"  -> BOOL true
    | "false" -> BOOL false
    | "def"   -> DEF
    | s       -> IDENT s
}

let whitespace = [' ' '\t' '\r']+
let digit      = ['0'-'9']
let integer    = digit+
let ident_head = ['a'-'z' 'A'-'Z' '_']
let ident_tail = ['a'-'z' 'A'-'Z' '0'-'9' '_' '-' '?' '!']
let ident      = ident_head ident_tail*
let op_char    = ['+' '-' '*' '/' '<' '>' '=' '!' '@' '&' '|' '^' '~' '%']
let op         = op_char+

rule token = parse
  | whitespace          { token lexbuf }
  | '\n'                { Lexing.new_line lexbuf; token lexbuf }
  | "(*"                { comment 1 lexbuf }
  | integer as n        { INT (int_of_string n) }
  | "->"                { ARROW }
  | ident as s          { keyword_or_ident s }
  | op as s             { IDENT s }
  | '('                 { LPAREN }
  | ')'                 { RPAREN }
  | '['                 { LBRACK }
  | ']'                 { RBRACK }
  | '{'                 { LBRACE }
  | '}'                 { RBRACE }
  | ';'                 { SEMI }
  | eof                 { EOF }
  | _ as c              { failwith (Printf.sprintf "unexpected character: %C" c) }

and comment depth = parse
  | "(*"                { comment (depth + 1) lexbuf }
  | "*)"                { if depth = 1 then token lexbuf
                          else comment (depth - 1) lexbuf }
  | '\n'                { Lexing.new_line lexbuf; comment depth lexbuf }
  | eof                 { failwith "unterminated comment" }
  | _                   { comment depth lexbuf }
