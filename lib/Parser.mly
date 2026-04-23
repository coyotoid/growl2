%{
  open Ast
  module S = Span.Spanned

  let mk_span (s : Lexing.position) (e : Lexing.position) =
    let to_pos p =
      Span.{ line = p.Lexing.pos_lnum; col = p.Lexing.pos_cnum - p.Lexing.pos_bol; byte = p.Lexing.pos_cnum }
    in
    Span.{ filename = s.pos_fname; lo = to_pos s; hi = to_pos e; }

  let spanned v s e = S.{ value = v; span = mk_span s e }

  let cat t rest = 
    match rest.S.value with
    | Id -> t
    | _ -> S.{ value = Cat (t, rest); span = Span.merge t.S.span rest.S.span }
%}

%token <int>    INT
%token <bool>   BOOL
%token <string> STRING
%token <string> COMMAND
%token <string> WORD
%token DEF
%token ARROW
%token SEMI
%token LBRACE RBRACE
%token LPAREN RPAREN
%token LBRACK RBRACK
%token EOF

%start <Ast.program> program

%%

program:
  | ds = list(def) EOF { ds }

def:
  | DEF name = WORD LBRACE body = terms RBRACE
    { spanned
        { name = spanned name $startpos(name) $endpos(name); body }
        $startpos $endpos }
  | DEF name = WORD LBRACE error RBRACE
    { spanned
        { name = spanned name $startpos(name) $endpos(name)
        ; body = spanned Id $startpos $endpos }
        $startpos $endpos }
  | DEF error RBRACE
    { spanned
        { name = spanned "_" $startpos $endpos
        ; body = spanned Id $startpos $endpos }
        $startpos $endpos }

terms:
  | { spanned Id $startpos $endpos }
  | ARROW name = WORD SEMI rest = terms
    { spanned
        (Bind (spanned name $startpos(name) $endpos(name), rest))
        $startpos $endpos }
  | ARROW error SEMI rest = terms
    { rest }
  | t = term rest = terms
    { cat t rest }

command:
  | name = COMMAND body = terms SEMI
    { Command (spanned name $startpos(name) $endpos(name), body) }

term:
  | n = INT
    { spanned (Lit (`Int n)) $startpos $endpos }
  | b = BOOL
    { spanned (Lit (`Bool b)) $startpos $endpos }
  | s = STRING
    { spanned (Lit (`String s)) $startpos $endpos }
  | name = WORD
    { spanned (Word name) $startpos $endpos }
  | LBRACK body = terms RBRACK
    { spanned (Quote body) $startpos $endpos }
  | LBRACK body = terms error
    { spanned (Quote body) $startpos $endpos }
  | cmd = command
    { spanned cmd $startpos $endpos }

