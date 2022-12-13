type loc =
  (Lexing.position [@printer Utils.pp_position]) *
  (Lexing.position [@printer Utils.pp_position])
  [@@deriving show]

type typ =
  | Typ_void
  | Typ_int
  | Typ_bool
  | Typ_pointer of typ
  [@@deriving show]

type const =
  | Const_true
  | Const_false
  | Const_null
  | Const_int of int64
  | Const_sizeof of typ
  [@@deriving show]

type un_op =
  | Un_op_pos
  | Un_op_neg
  | Un_op_not
  | Un_op_pre_incr
  | Un_op_pre_decr
  | Un_op_post_incr
  | Un_op_post_decr
  | Un_op_ref
  | Un_op_deref
  [@@deriving show]

type bin_op =
  | Bin_op_eq
  | Bin_op_ne
  | Bin_op_lt
  | Bin_op_le
  | Bin_op_gt
  | Bin_op_ge
  | Bin_op_add
  | Bin_op_sub
  | Bin_op_mul
  | Bin_op_div
  | Bin_op_mod
  | Bin_op_and
  | Bin_op_or
  | Bin_op_assign
  [@@deriving show]

type expr_desc =
  | Expr_desc_var of string
  | Expr_desc_call of string * expr list
  | Expr_desc_const of const
  | Expr_desc_un_op of un_op * expr
  | Expr_desc_bin_op of bin_op * expr * expr
  [@@deriving show]

and expr = {
  expr_loc : loc;
  expr_desc : expr_desc;
} [@@deriving show]

type instr_desc =
  | Instr_desc_expr of expr
  | Instr_desc_block of instr_decl list
  | Instr_desc_if of expr * instr * instr
  | Instr_desc_while of expr * instr
  | Instr_desc_for of expr * expr list * instr
  | Instr_desc_return of expr option
  | Instr_desc_break
  | Instr_desc_continue
  [@@deriving show]

and instr = {
  instr_loc : loc;
  instr_desc : instr_desc;
} [@@deriving show]

and param = {
  param_loc : loc;
  param_name : string;
  param_typ : typ;
} [@@deriving show]

and func_decl = {
  func_decl_loc : loc;
  func_decl_name : string;
  func_decl_return_typ : typ;
  func_decl_params : param list;
  func_decl_body : instr;
} [@@deriving show]

and var_decl = {
  var_decl_loc : loc;
  var_decl_name : string;
  var_decl_typ : typ;
  var_decl_init : expr option;
} [@@deriving show]

and instr_decl =
  | Instr_decl_func of func_decl
  | Instr_decl_var of var_decl
  | Instr_decl_instr of instr
  [@@deriving show]

type file = {
  file_loc : loc;
  file_func_decls : func_decl list;
} [@@deriving show]
