type path = {
  path_components : string list;
}

let pp_path (formatter : Format.formatter) (path : path) : unit =
  Format.pp_print_list
    ~pp_sep:(fun formatter () -> Format.pp_print_string formatter "::")
    Format.pp_print_string
    formatter
    path.path_components

let show_path (path : path) : string =
  Format.asprintf "%a" pp_path path

exception Error of Lexing.position * path * string

type typ =
  | Typ_void
  | Typ_int
  | Typ_bool
  | Typ_pointer of typ

let rec pp_typ (formatter : Format.formatter) (typ : typ) : unit =
  match typ with
  | Typ_void -> Format.fprintf formatter "void"
  | Typ_int -> Format.fprintf formatter "int"
  | Typ_bool -> Format.fprintf formatter "bool"
  | Typ_pointer typ -> Format.fprintf formatter "%a*" pp_typ typ

let show_typ (typ : typ) : string =
  Format.asprintf "%a" pp_typ typ

let typ_equivalent (typ_1 : typ) (typ_2 : typ) : bool =
  match typ_1, typ_2 with
  | _, _ when typ_1 = typ_2 -> true
  | Typ_int, Typ_bool -> true
  | Typ_bool, Typ_int -> true
  | Typ_pointer Typ_void, _ -> true
  | _, Typ_pointer Typ_void -> true
  | _, _ -> false

let rec typeck_ast_typ (ast_typ : Ast.typ) : typ =
  match ast_typ with
  | Ast.Typ_void -> Typ_void
  | Ast.Typ_int -> Typ_int
  | Ast.Typ_bool -> Typ_bool
  | Ast.Typ_pointer ast_typ -> Typ_pointer (ast_typ |> typeck_ast_typ)

type binding =
  | Binding_var of { path : path; typ : typ }
  | Binding_funct of { path : path; return_typ : typ; param_typs : typ list }

type env = {
  env_scope_path : path;
  env_bindings : (string * binding) list;
}

type const =
  | Const_null
  | Const_int of int64
  [@@deriving show]

type un_op =
  | Un_op_not
  | Un_op_pre_incr
  | Un_op_pre_decr
  | Un_op_post_incr
  | Un_op_post_decr
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
  | Expr_desc_var of path
  | Expr_desc_call of path * expr list
  | Expr_desc_const of const
  | Expr_desc_un_op of un_op * expr
  | Expr_desc_bin_op of bin_op * expr * expr
  [@@deriving show]

and expr = {
  expr_typ : typ;
  expr_desc : expr_desc;
} [@@deriving show]

let expr_as_lvalue (expr : expr) : expr option =
  match expr.expr_desc with
  | Expr_desc_un_op (Un_op_deref, expr) -> Some expr
  | _ -> None

let rec typeck_ast_expr (env : env) (ast_expr : Ast.expr) : expr =
  match ast_expr.expr_desc with
  | Ast.Expr_desc_var var_name -> (
    match env.env_bindings |> List.assoc_opt var_name with
    | None ->
      raise (Error (fst ast_expr.expr_loc, env.env_scope_path, Format.sprintf "variable %s not found" var_name))
    | Some (Binding_var { path; typ }) ->
      {
        expr_typ = typ;
        expr_desc = Expr_desc_un_op (
          Un_op_deref,
          { expr_typ = Typ_pointer typ; expr_desc = Expr_desc_var path }
        );
      }
    | Some _ ->
      raise (Error (fst ast_expr.expr_loc, env.env_scope_path, Format.sprintf "%s is not a variable" var_name))
  )
  | Ast.Expr_desc_call (var_name, arg_ast_exprs) -> (
    match env.env_bindings |> List.assoc_opt var_name with
    | None ->
      raise (Error (fst ast_expr.expr_loc, env.env_scope_path, Format.sprintf "function %s not found" var_name))
    | Some (Binding_funct { path; return_typ; param_typs }) ->
      let args = arg_ast_exprs |> List.map (typeck_ast_expr env) in
      if List.length args <> List.length param_typs then
        raise (Error (
          fst ast_expr.expr_loc,
          env.env_scope_path,
          Format.asprintf "wrong number of arguments to the function %a" pp_path path
        ));
      for i = 0 to List.length param_typs - 1 do
        let expected_typ = List.nth param_typs i in
        let actual_typ = (List.nth args i).expr_typ in
        if not (typ_equivalent expected_typ actual_typ) then
          raise (Error (
            fst ast_expr.expr_loc,
            env.env_scope_path,
            Format.asprintf "argument #%d to the function %a has type %a instead of %a"
              i pp_path path pp_typ actual_typ pp_typ expected_typ
          ));
      done;
      { expr_typ = return_typ; expr_desc = Expr_desc_call (path, args) }
    | Some _ ->
      raise (Error (fst ast_expr.expr_loc, env.env_scope_path, Format.sprintf "%s is not a function" var_name))
  )
  | Ast.Expr_desc_const Ast.Const_true -> { expr_typ = Typ_bool; expr_desc = Expr_desc_const (Const_int 1L) }
  | Ast.Expr_desc_const Ast.Const_false -> { expr_typ = Typ_bool; expr_desc = Expr_desc_const (Const_int 0L) }
  | Ast.Expr_desc_const Ast.Const_null -> { expr_typ = Typ_pointer Typ_void; expr_desc = Expr_desc_const Const_null }
  | Ast.Expr_desc_const (Ast.Const_int n) -> { expr_typ = Typ_int; expr_desc = Expr_desc_const (Const_int n) }
  | Ast.Expr_desc_const (Ast.Const_sizeof _) -> { expr_typ = Typ_int; expr_desc = Expr_desc_const (Const_int 8L) }
  | Ast.Expr_desc_un_op (Ast.Un_op_pos as ast_un_op, ast_expr_1)
  | Ast.Expr_desc_un_op (Ast.Un_op_neg as ast_un_op, ast_expr_1) -> (
    let expr_1 = ast_expr_1 |> typeck_ast_expr env in
    if not (typ_equivalent Typ_int expr_1.expr_typ) then
      raise (Error (
        fst ast_expr.expr_loc,
        env.env_scope_path,
        Format.asprintf "argument to the unary `%s` operator has type %a instead of %a"
          (
            match ast_un_op with
            | Ast.Un_op_pos -> "+"
            | Ast.Un_op_neg -> "-"
            | _ -> assert false
          )
          pp_typ expr_1.expr_typ pp_typ Typ_int
      ));
    match ast_un_op with
    | Ast.Un_op_pos -> expr_1
    | Ast.Un_op_neg ->
      {
        expr_typ = Typ_int;
        expr_desc = Expr_desc_bin_op (
          Bin_op_sub,
          { expr_typ = Typ_int; expr_desc = Expr_desc_const (Const_int 0L) },
          expr_1
        );
      }
    | _ -> assert false
  )
  | Ast.Expr_desc_un_op (Ast.Un_op_not, ast_expr_1) ->
    let expr_1 = ast_expr_1 |> typeck_ast_expr env in
    if typ_equivalent Typ_void expr_1.expr_typ then
      raise (Error (
        fst ast_expr.expr_loc,
        env.env_scope_path,
        "argument to the `!` operator has a void type"
      ));
    { expr_typ = Typ_int; expr_desc = Expr_desc_un_op (Un_op_not, expr_1) }
  | Ast.Expr_desc_un_op (Ast.Un_op_pre_incr as ast_un_op, ast_expr_1)
  | Ast.Expr_desc_un_op (Ast.Un_op_pre_decr as ast_un_op, ast_expr_1)
  | Ast.Expr_desc_un_op (Ast.Un_op_post_incr as ast_un_op, ast_expr_1)
  | Ast.Expr_desc_un_op (Ast.Un_op_post_decr as ast_un_op, ast_expr_1) -> (
    let expr_1 = ast_expr_1 |> typeck_ast_expr env in
    match expr_as_lvalue expr_1 with
    | Some expr_1_pointer ->
      {
        expr_typ = expr_1.expr_typ;
        expr_desc = Expr_desc_un_op (
          (
            match ast_un_op with
            | Ast.Un_op_pre_incr -> Un_op_pre_incr
            | Ast.Un_op_pre_decr -> Un_op_pre_decr
            | Ast.Un_op_post_incr -> Un_op_post_incr
            | Ast.Un_op_post_decr -> Un_op_post_decr
            | _ -> assert false
          ),
          expr_1_pointer
        );
      }
    | None ->
      raise (Error (
        fst ast_expr.expr_loc,
        env.env_scope_path,
        Format.asprintf "argument to the %s operator is not an lvalue"
          (
            match ast_un_op with
            | Ast.Un_op_pre_incr -> "pre-increment"
            | Ast.Un_op_pre_decr -> "pre-decrement"
            | Ast.Un_op_post_incr -> "pose-increment"
            | Ast.Un_op_post_decr -> "pose-decrement"
            | _ -> assert false
          )
      ))
  )
  | Ast.Expr_desc_un_op (Ast.Un_op_ref, ast_expr_1) -> (
    let expr_1 = ast_expr_1 |> typeck_ast_expr env in
    match expr_as_lvalue expr_1 with
    | Some expr_1_pointer -> expr_1_pointer
    | _ ->
      raise (Error (
        fst ast_expr.expr_loc,
        env.env_scope_path,
        "argument to the reference operator is not an lvalue"
      ))
  )
  | Ast.Expr_desc_un_op (Ast.Un_op_deref, ast_expr_1) -> (
    let expr_1 = ast_expr_1 |> typeck_ast_expr env in
    match expr_1.expr_typ with
    | Typ_pointer typ when not (typ_equivalent typ Typ_void) ->
      { expr_typ = typ; expr_desc = Expr_desc_un_op (Un_op_deref, expr_1) }
    | _ ->
      raise (Error (
        fst ast_expr.expr_loc,
        env.env_scope_path,
        Format.asprintf "argument to the dereference operator has type %a instead of a non-void pointer"
          pp_typ expr_1.expr_typ
      ))
  )
  | Ast.Expr_desc_bin_op (Ast.Bin_op_eq as ast_bin_op, ast_expr_1, ast_expr_2)
  | Ast.Expr_desc_bin_op (Ast.Bin_op_ne as ast_bin_op, ast_expr_1, ast_expr_2)
  | Ast.Expr_desc_bin_op (Ast.Bin_op_lt as ast_bin_op, ast_expr_1, ast_expr_2)
  | Ast.Expr_desc_bin_op (Ast.Bin_op_le as ast_bin_op, ast_expr_1, ast_expr_2)
  | Ast.Expr_desc_bin_op (Ast.Bin_op_gt as ast_bin_op, ast_expr_1, ast_expr_2)
  | Ast.Expr_desc_bin_op (Ast.Bin_op_ge as ast_bin_op, ast_expr_1, ast_expr_2) ->
    let expr_1 = ast_expr_1 |> typeck_ast_expr env in
    let expr_2 = ast_expr_2 |> typeck_ast_expr env in
    if not (typ_equivalent expr_1.expr_typ expr_2.expr_typ) || typ_equivalent Typ_void expr_1.expr_typ then
      raise (Error (
        fst ast_expr.expr_loc,
        env.env_scope_path,
        Format.asprintf "arguments to the `%s` operator has types %a and %a which are void or not compatible"
          (
            match ast_bin_op with
            | Ast.Bin_op_eq -> "=="
            | Ast.Bin_op_ne -> "!="
            | Ast.Bin_op_lt -> "<"
            | Ast.Bin_op_le -> "<="
            | Ast.Bin_op_gt -> ">"
            | Ast.Bin_op_ge -> ">="
            | _ -> assert false
          )
          pp_typ expr_1.expr_typ pp_typ expr_2.expr_typ
      ));
      {
        expr_typ = Typ_int;
        expr_desc = Expr_desc_bin_op (
          (
            match ast_bin_op with
            | Ast.Bin_op_eq -> Bin_op_eq
            | Ast.Bin_op_ne -> Bin_op_ne
            | Ast.Bin_op_lt -> Bin_op_lt
            | Ast.Bin_op_le -> Bin_op_le
            | Ast.Bin_op_gt -> Bin_op_gt
            | Ast.Bin_op_ge -> Bin_op_ge
            | _ -> assert false
          ),
          expr_1,
          expr_2
        );
      }
  | Ast.Expr_desc_bin_op (Ast.Bin_op_add as ast_bin_op, ast_expr_1, ast_expr_2)
  | Ast.Expr_desc_bin_op (Ast.Bin_op_sub as ast_bin_op, ast_expr_1, ast_expr_2)
  | Ast.Expr_desc_bin_op (Ast.Bin_op_mul as ast_bin_op, ast_expr_1, ast_expr_2)
  | Ast.Expr_desc_bin_op (Ast.Bin_op_div as ast_bin_op, ast_expr_1, ast_expr_2)
  | Ast.Expr_desc_bin_op (Ast.Bin_op_mod as ast_bin_op, ast_expr_1, ast_expr_2)
  | Ast.Expr_desc_bin_op (Ast.Bin_op_and as ast_bin_op, ast_expr_1, ast_expr_2)
  | Ast.Expr_desc_bin_op (Ast.Bin_op_or as ast_bin_op, ast_expr_1, ast_expr_2) -> (
    let expr_1 = ast_expr_1 |> typeck_ast_expr env in
    let expr_2 = ast_expr_2 |> typeck_ast_expr env in
    match ast_bin_op, expr_1.expr_typ, expr_2.expr_typ with
    | _, _, _ when typ_equivalent expr_1.expr_typ expr_2.expr_typ && typ_equivalent Typ_int expr_1.expr_typ ->
      {
        expr_typ = Typ_int;
        expr_desc = Expr_desc_bin_op (
          (
            match ast_bin_op with
            | Ast.Bin_op_add -> Bin_op_add
            | Ast.Bin_op_sub -> Bin_op_sub
            | Ast.Bin_op_mul -> Bin_op_mul
            | Ast.Bin_op_div -> Bin_op_div
            | Ast.Bin_op_mod -> Bin_op_mod
            | Ast.Bin_op_and -> Bin_op_and
            | Ast.Bin_op_or -> Bin_op_or
            | _ -> assert false
          ),
          expr_1,
          expr_2
        );
      }
    | Ast.Bin_op_add, _, Typ_pointer typ
    | Ast.Bin_op_sub, _, Typ_pointer typ
      when typ_equivalent Typ_int expr_1.expr_typ ->
      {
        expr_typ = Typ_pointer typ;
        expr_desc = Expr_desc_bin_op (
          (
            match ast_bin_op with
            | Ast.Bin_op_add -> Bin_op_add
            | Ast.Bin_op_sub -> Bin_op_sub
            | _ -> assert false
          ),
          {
            expr_typ = Typ_int;
            expr_desc = Expr_desc_bin_op (
              Bin_op_mul,
              expr_1,
              { expr_typ = Typ_int; expr_desc = Expr_desc_const (Const_int 8L) }
            );
          },
          expr_2
        );
      }
    | Ast.Bin_op_add, Typ_pointer typ, _
    | Ast.Bin_op_sub, Typ_pointer typ, _
      when typ_equivalent Typ_int expr_2.expr_typ ->
      {
        expr_typ = Typ_pointer typ;
        expr_desc = Expr_desc_bin_op (
          (
            match ast_bin_op with
            | Ast.Bin_op_add -> Bin_op_add
            | Ast.Bin_op_sub -> Bin_op_sub
            | _ -> assert false
          ),
          expr_1,
          {
            expr_typ = Typ_int;
            expr_desc = Expr_desc_bin_op (
              Bin_op_mul,
              expr_2,
              { expr_typ = Typ_int; expr_desc = Expr_desc_const (Const_int 8L) }
            );
          }
        );
      }
    | Ast.Bin_op_sub, Typ_pointer typ_1, Typ_pointer typ_2 when typ_1 = typ_2 ->
      {
        expr_typ = Typ_int;
        expr_desc = Expr_desc_bin_op (
          Bin_op_div,
          { expr_typ = Typ_int; expr_desc = Expr_desc_bin_op (Bin_op_sub, expr_1, expr_2) },
          { expr_typ = Typ_int; expr_desc = Expr_desc_const (Const_int 8L) }
        );
      }
    | _, _, _ ->
      raise (Error (
        fst ast_expr.expr_loc,
        env.env_scope_path,
        Format.asprintf "arguments to the `%s` operator has types %a and %a which are not compatible"
          (
            match ast_bin_op with
            | Ast.Bin_op_add -> "+"
            | Ast.Bin_op_sub -> "-"
            | Ast.Bin_op_mul -> "*"
            | Ast.Bin_op_div -> "/"
            | Ast.Bin_op_mod -> "%"
            | Ast.Bin_op_and -> "&&"
            | Ast.Bin_op_or -> "||"
            | _ -> assert false
          )
          pp_typ expr_1.expr_typ pp_typ expr_2.expr_typ
      ))
  )
  | Ast.Expr_desc_bin_op (Ast.Bin_op_assign, ast_expr_1, ast_expr_2) ->
    let expr_1 = ast_expr_1 |> typeck_ast_expr env in
    let expr_2 = ast_expr_2 |> typeck_ast_expr env in
    match expr_as_lvalue expr_1 with
    | Some expr_1_pointer ->
      if not (typ_equivalent expr_1.expr_typ expr_2.expr_typ) then
        raise (Error (
          fst ast_expr.expr_loc,
          env.env_scope_path,
          Format.asprintf "arguments to the assignement operator has types %a and %a which are not compatible"
            pp_typ expr_1.expr_typ pp_typ expr_2.expr_typ
        ));
      { expr_typ = expr_1.expr_typ; expr_desc = Expr_desc_bin_op (Bin_op_assign, expr_1_pointer, expr_2) }
    | _ ->
      raise (Error (
        fst ast_expr.expr_loc,
        env.env_scope_path,
        "argument to the reference operator is not an lvalue"
      ));
