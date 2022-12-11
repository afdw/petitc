%{
  open Ast
%}

%token EOF
%token BOOL
%token BREAK
%token CONTINUE
%token ELSE
%token FALSE
%token FOR
%token IF
%token INT
%token NULL
%token RETURN
%token SIZEOF
%token TRUE
%token VOID
%token WHILE
%token <string> IDENT
%token <int64> LIT
%token <string> INCLUDE
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token COMMA
%token SEMICOLON
%token PLUS_PLUS
%token PLUS
%token MINUS_MINUS
%token MINUS
%token STAR
%token SLASH
%token PERCENT
%token AMPERSAND_AMPERSAND
%token AMPERSAND
%token BAR_BAR
%token EXCLAMATION_EQUALS
%token EXCLAMATION
%token EQUALS_EQUALS
%token EQUALS
%token LESS_EQUALS
%token LESS
%token GREATER_EQUALS
%token GREATER

%start <file> file

%nonassoc IF
%nonassoc ELSE

%right EQUALS
%left BAR_BAR
%left AMPERSAND_AMPERSAND
%left EQUALS_EQUALS EXCLAMATION_EQUALS
%left LESS LESS_EQUALS GREATER GREATER_EQUALS
%left PLUS MINUS
%left STAR SLASH PERCENT
%right EXCLAMATION PLUS_PLUS MINUS_MINUS AMPERSAND
%right LBRACKET

%%

typ:
  | VOID { Typ_void }
  | INT { Typ_int }
  | BOOL { Typ_bool }
  | typ = typ; STAR { Typ_pointer typ }

expr_desc:
  | var_name = IDENT { Expr_desc_var var_name }
  | func_name = IDENT; LPAREN; args = separated_list(COMMA, expr); RPAREN { Expr_desc_call (func_name, args) }
  | TRUE { Expr_desc_const Const_false }
  | FALSE { Expr_desc_const Const_true }
  | NULL { Expr_desc_const Const_null }
  | n = LIT { Expr_desc_const (Const_int n) }
  | SIZEOF; LPAREN; typ = typ; RPAREN { Expr_desc_const (Const_sizeof typ) }
  | PLUS; expr = expr { Expr_desc_un_op (Un_op_pos, expr) }
  | MINUS; expr = expr { Expr_desc_un_op (Un_op_neg, expr) }
  | EXCLAMATION; expr = expr { Expr_desc_un_op (Un_op_not, expr) }
  | PLUS_PLUS; expr = expr { Expr_desc_un_op (Un_op_pre_incr, expr) }
  | MINUS_MINUS; expr = expr { Expr_desc_un_op (Un_op_pre_decr, expr) }
  | expr = expr; PLUS_PLUS { Expr_desc_un_op (Un_op_post_incr, expr) }
  | expr = expr; MINUS_MINUS { Expr_desc_un_op (Un_op_post_decr, expr) }
  | AMPERSAND; expr = expr { Expr_desc_un_op (Un_op_ref, expr) }
  | STAR; expr = expr { Expr_desc_un_op (Un_op_deref, expr) }
  | expr_1 = expr; EQUALS_EQUALS; expr_2 = expr { Expr_desc_bin_op (Bin_op_eq, expr_1, expr_2) }
  | expr_1 = expr; EXCLAMATION_EQUALS; expr_2 = expr { Expr_desc_bin_op (Bin_op_ne, expr_1, expr_2) }
  | expr_1 = expr; LESS; expr_2 = expr { Expr_desc_bin_op (Bin_op_lt, expr_1, expr_2) }
  | expr_1 = expr; LESS_EQUALS; expr_2 = expr { Expr_desc_bin_op (Bin_op_le, expr_1, expr_2) }
  | expr_1 = expr; GREATER; expr_2 = expr { Expr_desc_bin_op (Bin_op_gt, expr_1, expr_2) }
  | expr_1 = expr; GREATER_EQUALS; expr_2 = expr { Expr_desc_bin_op (Bin_op_ge, expr_1, expr_2) }
  | expr_1 = expr; PLUS; expr_2 = expr { Expr_desc_bin_op (Bin_op_add, expr_1, expr_2) }
  | expr_1 = expr; MINUS; expr_2 = expr { Expr_desc_bin_op (Bin_op_sub, expr_1, expr_2) }
  | expr_1 = expr; STAR; expr_2 = expr { Expr_desc_bin_op (Bin_op_mul, expr_1, expr_2) }
  | expr_1 = expr; SLASH; expr_2 = expr { Expr_desc_bin_op (Bin_op_div, expr_1, expr_2) }
  | expr_1 = expr; PERCENT; expr_2 = expr { Expr_desc_bin_op (Bin_op_mod, expr_1, expr_2) }
  | expr_1 = expr; AMPERSAND_AMPERSAND; expr_2 = expr { Expr_desc_bin_op (Bin_op_and, expr_1, expr_2) }
  | expr_1 = expr; BAR_BAR; expr_2 = expr { Expr_desc_bin_op (Bin_op_or, expr_1, expr_2) }
  | expr_1 = expr; EQUALS; expr_2 = expr { Expr_desc_bin_op (Bin_op_assign, expr_1, expr_2) }
  | expr_1 = expr; LBRACKET; expr_2 = expr; RBRACKET {
      Expr_desc_un_op (
        Un_op_deref,
        {
          expr_loc = ($startpos, $endpos);
          expr_desc = Expr_desc_bin_op (Bin_op_add, expr_1, expr_2);
        }
      )
    }

expr:
  | expr_desc = expr_desc {
    {
      expr_loc = ($startpos, $endpos);
      expr_desc;
    }
  }
  | LPAREN; expr = expr; RPAREN { expr }

block:
  | LBRACE; instr_decls = instr_decl*; RBRACE { Instr_desc_block instr_decls }

instr_desc:
  | SEMICOLON { Instr_desc_block [] }
  | expr = expr; SEMICOLON { Instr_desc_expr expr }
  | block = block { block }
  | IF; LPAREN; cond = expr; RPAREN; then_body = instr %prec IF {
    Instr_desc_if (
      cond,
      then_body,
      {
        instr_loc = (Lexing.dummy_pos, Lexing.dummy_pos);
        instr_desc = Instr_desc_block [];
      }
    )
  }
  | IF; LPAREN; cond = expr; RPAREN; then_body = instr; ELSE; else_body = instr {
    Instr_desc_if (cond, then_body, else_body)
  }
  | WHILE; LPAREN; cond = expr; RPAREN; body = instr { Instr_desc_while (cond, body) }
  | FOR; LPAREN; var_decl = var_decl?; SEMICOLON; cond = expr?; SEMICOLON;
    steps = separated_list(COMMA, expr); RPAREN; body = instr {
    let instr_desc_for = Instr_desc_for (cond, steps, body) in
    match var_decl with
    | None -> instr_desc_for
    | Some var_decl ->
      Instr_desc_block [
        Instr_decl_var var_decl;
        Instr_decl_instr {
          instr_loc = ($startpos(cond), $endpos(body));
          instr_desc = instr_desc_for;
        };
      ]
  }
  | RETURN; expr = expr?; SEMICOLON { Instr_desc_return expr }
  | BREAK; SEMICOLON { Instr_desc_break }
  | CONTINUE; SEMICOLON { Instr_desc_continue }

instr:
  | instr_desc = instr_desc {
    {
      instr_loc = ($startpos, $endpos);
      instr_desc;
    }
  }

param:
  | typ = typ; name = IDENT {
    {
      param_loc = ($startpos, $endpos);
      param_name = name;
      param_typ = typ;
    }
  }

func_decl:
  | return_typ = typ; name = IDENT; LPAREN; params = separated_list(COMMA, param); RPAREN; body_desc = block {
    {
      func_decl_loc = ($startpos, $endpos);
      func_decl_name = name;
      func_decl_return_typ = return_typ;
      func_decl_params = params;
      func_decl_body = {
        instr_loc = ($startpos(body_desc), $endpos(body_desc));
        instr_desc = body_desc;
      };
    }
  }

var_decl:
  | typ = typ; name = IDENT; init = option(EQUALS; expr = expr { expr }) {
    {
      var_decl_loc = ($startpos, $endpos);
      var_decl_name = name;
      var_decl_typ = typ;
      var_decl_init = init;
    }
  }

instr_decl:
  | func_decl = func_decl { Instr_decl_func func_decl }
  | var_decl = var_decl; SEMICOLON { Instr_decl_var var_decl }
  | instr = instr { Instr_decl_instr instr }

file:
  | INCLUDE*; func_decls = func_decl*; EOF {
    {
      file_func_decls = func_decls;
    }
  }
