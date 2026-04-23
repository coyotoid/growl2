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
%token <string> STACK_VAR
%token DEF
%token ARROW
%token COMMA
%token SEMI
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACK RBRACK
%token EOF

%start <Ast.program> program

%%

program:
  | ds = list(def) EOF { ds }

def:
  | DEF name = WORD LBRACE body = terms RBRACE
    { spanned
        { name = spanned name $startpos(name) $endpos(name); annot = None; body }
        $startpos $endpos }
  | DEF name = WORD LBRACK annot = annot RBRACK LBRACE body = terms RBRACE
    { spanned
        { name = spanned name $startpos(name) $endpos(name)
        ; annot = Some annot
        ; body }
        $startpos $endpos }
  | DEF name = WORD LBRACE error RBRACE
    { spanned
        { name = spanned name $startpos(name) $endpos(name)
        ; annot = None
        ; body = spanned Id $startpos $endpos }
        $startpos $endpos }
  | DEF error RBRACE
    { spanned
        { name = spanned "" $startpos $endpos
        ; annot = None
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
  | lit = literal
    { lit }
  | name = WORD
    { spanned (Word name) $startpos $endpos }
  | LBRACK body = terms RBRACK
    { spanned (Quote body) $startpos $endpos }
  | LBRACK body = terms error
    { spanned (Quote body) $startpos $endpos }
  | cmd = command
    { spanned cmd $startpos $endpos }

literal:
  | i = INT
    { spanned (Lit (`Int i)) $startpos $endpos }
  | b = BOOL
    { spanned (Lit (`Bool b)) $startpos $endpos }
  | s = STRING
    { spanned (Lit (`String s)) $startpos $endpos }
  | LBRACE l = list(literal) RBRACE
    { spanned (List l) $startpos $endpos }

type_atom:
  | name = WORD
    { Type_ast.ty_of_string name }
  | t = type_atom name = WORD
    { Type_ast.TCon (name, [t]) }
  | LPAREN ts = separated_list(COMMA, type_atom) RPAREN name = WORD
    { Type_ast.TCon (name, ts) }
  | LBRACK annot = annot RBRACK
    { Type_ast.TFunc annot }

stack_atom:
  | t = type_atom { Type_ast.SItem t }
  | v = STACK_VAR { Type_ast.SRest v }

annot:
  | inputs = separated_list(COMMA, stack_atom) ARROW outputs = separated_list(COMMA, stack_atom)
    { Type_ast.{ inputs; outputs } }