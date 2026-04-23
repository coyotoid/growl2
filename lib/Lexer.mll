{
  open Parser

  let keyword_or_ident = function
    | "true"  -> BOOL true
    | "false" -> BOOL false
    | "def"   -> DEF
    | s       -> WORD s
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
  | '('                 { comment 1 lexbuf }
  | '"'                 { string (Buffer.create 16) lexbuf }
  | integer as n        { INT (int_of_string n) }
  | "->"                { ARROW }
  | ident as s          { keyword_or_ident s }
  | (ident as s) ':'    { COMMAND s }
  | op as s             { WORD s }
  | '['                 { LBRACK }
  | ']'                 { RBRACK }
  | '{'                 { LBRACE }
  | '}'                 { RBRACE }
  | ';'                 { SEMI }
  | eof                 { EOF }
  | _ as c              { failwith (Printf.sprintf "unexpected character: %C" c) }

and string buf = parse
  | '"'                 { STRING (Buffer.contents buf) }
  | '\\' 'n'            { Buffer.add_char buf '\n'; string buf lexbuf }
  | '\\' 't'            { Buffer.add_char buf '\t'; string buf lexbuf }
  | '\\' 'r'            { Buffer.add_char buf '\r'; string buf lexbuf }
  | '\\' '\\'           { Buffer.add_char buf '\\'; string buf lexbuf }
  | '\\' '"'            { Buffer.add_char buf '"'; string buf lexbuf }
  | '\\' _ as c         { failwith (Printf.sprintf "invalid escape sequence: %s" c) }
  | '\n'                { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; 
                          string buf lexbuf }
  | eof                 { failwith "unterminated string literal" }
  | _ as c              { Buffer.add_char buf c; string buf lexbuf }

and comment depth = parse
  | "("                 { comment (depth + 1) lexbuf }
  | ")"                 { if depth = 1 then token lexbuf
                          else comment (depth - 1) lexbuf }
  | '\n'                { Lexing.new_line lexbuf; comment depth lexbuf }
  | eof                 { failwith "unterminated comment" }
  | _                   { comment depth lexbuf }
