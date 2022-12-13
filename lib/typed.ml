type path = {
  path_components : string list;
}

let empty_path =
  { path_components = [] }

let path_append (component : string) (path : path) =
  { path_components = path.path_components @ [component] }

let simplified_pp_path = ref false

let pp_path (formatter : Format.formatter) (path : path) : unit =
  if !simplified_pp_path && path.path_components <> [] then
    Format.fprintf formatter "%s" (path.path_components |> List.rev |> List.hd)
  else
    Format.pp_print_list
      ~pp_sep:(fun formatter () -> Format.fprintf formatter "::@,")
      Format.pp_print_string
      formatter
      path.path_components

let show_path (path : path) : string =
  Format.asprintf "%a" pp_path path

let pp_path_strict (formatter : Format.formatter) (path : path) : unit =
  Format.pp_print_list
    ~pp_sep:(fun formatter () -> Format.fprintf formatter "::")
    Format.pp_print_string
    formatter
    path.path_components

let show_path_strict (path : path) : string =
  Format.asprintf "%a" pp_path_strict path

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
  | Typ_pointer Typ_void, Typ_pointer _ -> true
  | Typ_pointer _, Typ_pointer Typ_void -> true
  | _, _ -> false

let rec typeck_ast_typ (ast_typ : Ast.typ) : typ =
  match ast_typ with
  | Ast.Typ_void -> Typ_void
  | Ast.Typ_int -> Typ_int
  | Ast.Typ_bool -> Typ_bool
  | Ast.Typ_pointer ast_typ -> Typ_pointer (ast_typ |> typeck_ast_typ)

type binding =
  | Binding_var of { path : path; typ : typ }
  | Binding_func of { path : path; return_typ : typ; param_typs : typ list }

let pp_binding (formatter : Format.formatter) (binding : binding) : unit =
  match binding with
  | Binding_var { path; typ } ->
    Format.fprintf formatter "%a %a"
      pp_typ typ
      pp_path path
  | Binding_func { path; return_typ; param_typs } ->
    Format.fprintf formatter "%a %a(%a)"
      pp_typ return_typ
      pp_path path
      (Format.pp_print_list ~pp_sep:(fun formatter () -> Format.fprintf formatter ",@ ") pp_typ) param_typs

let show_binding (binding : binding) : string =
  Format.asprintf "%a" pp_binding binding

type env = {
  env_scope_path : path;
  env_bindings : (string * binding) list;
  env_used_idents : (string * (Lexing.position [@printer Utils.pp_position])) list;
} [@@deriving show]

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

and expr = {
  expr_typ : typ;
  expr_desc : expr_desc;
}

let rec pp_expr_desc (formatter : Format.formatter) (expr_desc : expr_desc) : unit =
  match expr_desc with
  | Expr_desc_var path -> Format.fprintf formatter "%a" pp_path path
  | Expr_desc_call (path, arg_exprs) ->
    Format.fprintf formatter "%a(%a)"
    pp_path path
    (
      Format.pp_print_list
        ~pp_sep:(fun formatter () -> Format.fprintf formatter ",@ ")
        pp_expr
    ) arg_exprs
  | Expr_desc_const Const_null -> Format.fprintf formatter "NULL"
  | Expr_desc_const (Const_int n) -> Format.fprintf formatter "%Ld" n
  | Expr_desc_un_op (Un_op_not, expr_1) -> Format.fprintf formatter "!%a" pp_expr expr_1
  | Expr_desc_un_op (Un_op_pre_incr, expr_1) -> Format.fprintf formatter "--%a" pp_expr expr_1
  | Expr_desc_un_op (Un_op_pre_decr, expr_1) -> Format.fprintf formatter "--%a" pp_expr expr_1
  | Expr_desc_un_op (Un_op_post_incr, expr_1) -> Format.fprintf formatter "%a++" pp_expr expr_1
  | Expr_desc_un_op (Un_op_post_decr, expr_1) -> Format.fprintf formatter "%a++" pp_expr expr_1
  | Expr_desc_un_op (Un_op_deref, expr_1) -> Format.fprintf formatter "*%a" pp_expr expr_1
  | Expr_desc_bin_op (Bin_op_eq, expr_1, expr_2) ->
    Format.fprintf formatter "%a == %a" pp_expr expr_1 pp_expr expr_2
  | Expr_desc_bin_op (Bin_op_ne, expr_1, expr_2) ->
    Format.fprintf formatter "%a != %a" pp_expr expr_1 pp_expr expr_2
  | Expr_desc_bin_op (Bin_op_lt, expr_1, expr_2) ->
    Format.fprintf formatter "%a < %a" pp_expr expr_1 pp_expr expr_2
  | Expr_desc_bin_op (Bin_op_le, expr_1, expr_2) ->
    Format.fprintf formatter "%a <= %a" pp_expr expr_1 pp_expr expr_2
  | Expr_desc_bin_op (Bin_op_gt, expr_1, expr_2) ->
    Format.fprintf formatter "%a > %a" pp_expr expr_1 pp_expr expr_2
  | Expr_desc_bin_op (Bin_op_ge, expr_1, expr_2) ->
    Format.fprintf formatter "%a >= %a" pp_expr expr_1 pp_expr expr_2
  | Expr_desc_bin_op (Bin_op_add, expr_1, expr_2) ->
    Format.fprintf formatter "%a + %a" pp_expr expr_1 pp_expr expr_2
  | Expr_desc_bin_op (Bin_op_sub, expr_1, expr_2) ->
    Format.fprintf formatter "%a - %a" pp_expr expr_1 pp_expr expr_2
  | Expr_desc_bin_op (Bin_op_mul, expr_1, expr_2) ->
    Format.fprintf formatter "%a * %a" pp_expr expr_1 pp_expr expr_2
  | Expr_desc_bin_op (Bin_op_div, expr_1, expr_2) ->
    Format.fprintf formatter "%a / %a" pp_expr expr_1 pp_expr expr_2
  | Expr_desc_bin_op (Bin_op_mod, expr_1, expr_2) ->
    Format.fprintf formatter "%a %% %a" pp_expr expr_1 pp_expr expr_2
  | Expr_desc_bin_op (Bin_op_and, expr_1, expr_2) ->
    Format.fprintf formatter "%a && %a" pp_expr expr_1 pp_expr expr_2
  | Expr_desc_bin_op (Bin_op_or, expr_1, expr_2) ->
    Format.fprintf formatter "%a || %a" pp_expr expr_1 pp_expr expr_2
  | Expr_desc_bin_op (Bin_op_assign, expr_1, expr_2) ->
    Format.fprintf formatter "%a = %a" pp_expr expr_1 pp_expr expr_2

and pp_expr (formatter : Format.formatter) (expr : expr) : unit =
  Format.fprintf formatter "(@[%a@]@ : %a)" pp_expr_desc expr.expr_desc pp_typ expr.expr_typ

let show_expr_desc (expr_desc : expr_desc) : string =
  Format.asprintf "%a" pp_expr_desc expr_desc

let show_expr (expr : expr) : string =
  Format.asprintf "%a" pp_expr expr

let expr_as_lvalue (expr : expr) : expr option =
  match expr.expr_desc with
  | Expr_desc_un_op (Un_op_deref, expr) -> Some expr
  | _ -> None

let rec typeck_ast_expr (env : env) (ast_expr : Ast.expr) : expr =
  match ast_expr.expr_desc with
  | Ast.Expr_desc_var var_name -> (
    match env.env_bindings |> List.assoc_opt var_name with
    | None ->
      raise (Error (fst ast_expr.expr_loc, env.env_scope_path, Format.sprintf "variable `%s` not found" var_name))
    | Some (Binding_var { path; typ }) ->
      {
        expr_typ = typ;
        expr_desc = Expr_desc_un_op (
          Un_op_deref,
          { expr_typ = Typ_pointer typ; expr_desc = Expr_desc_var path }
        );
      }
    | Some _ ->
      raise (Error (fst ast_expr.expr_loc, env.env_scope_path, Format.sprintf "`%s` is not a variable" var_name))
  )
  | Ast.Expr_desc_call (var_name, arg_ast_exprs) -> (
    match env.env_bindings |> List.assoc_opt var_name with
    | None ->
      raise (Error (fst ast_expr.expr_loc, env.env_scope_path, Format.sprintf "function `%s` not found" var_name))
    | Some (Binding_func { path; return_typ; param_typs }) ->
      let args = arg_ast_exprs |> List.map (typeck_ast_expr env) in
      if List.length args <> List.length param_typs then
        raise (Error (
          fst ast_expr.expr_loc,
          env.env_scope_path,
          Format.asprintf "wrong number of arguments to the function `%a`" pp_path path
        ));
      for i = 0 to List.length param_typs - 1 do
        let expected_typ = List.nth param_typs i in
        let actual_typ = (List.nth args i).expr_typ in
        if not (typ_equivalent expected_typ actual_typ) then
          raise (Error (
            fst ast_expr.expr_loc,
            env.env_scope_path,
            Format.asprintf "argument #%d to the function `%a`@ has type %a instead of %a"
              i pp_path path pp_typ actual_typ pp_typ expected_typ
          ));
      done;
      { expr_typ = return_typ; expr_desc = Expr_desc_call (path, args) }
    | Some _ ->
      raise (Error (fst ast_expr.expr_loc, env.env_scope_path, Format.sprintf "`%s` is not a function" var_name))
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
        Format.asprintf "argument to the unary `%s` operator@ has type %a instead of %a"
          (
            match ast_un_op with
            | Ast.Un_op_pos -> "+"
            | Ast.Un_op_neg -> "-"
            | _ -> assert false
          )
          pp_typ expr_1.expr_typ
          pp_typ Typ_int
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
        Format.asprintf "arguments to the `%s` operator@ has types %a and %a which are void or not compatible"
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
          pp_typ expr_1.expr_typ
          pp_typ expr_2.expr_typ
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
        Format.asprintf "arguments to the `%s` operator@ has types %a and %a which are not compatible"
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
          pp_typ expr_1.expr_typ
          pp_typ expr_2.expr_typ
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
          Format.asprintf "arguments to the assignement operator@ has types %a and %a which are not compatible"
            pp_typ expr_1.expr_typ
            pp_typ expr_2.expr_typ
        ));
      { expr_typ = expr_1.expr_typ; expr_desc = Expr_desc_bin_op (Bin_op_assign, expr_1_pointer, expr_2) }
    | _ ->
      raise (Error (
        fst ast_expr.expr_loc,
        env.env_scope_path,
        "argument to the reference operator is not an lvalue"
      ));

type action =
  | Action_goto of path
  | Action_if of expr * path * path
  | Action_return of expr option
  | Action_unreachable
  [@@deriving show]

type block = {
  block_exprs : expr list;
  block_action : action;
} [@@deriving show]

type var_decl = {
  var_decl_typ : typ;
} [@@deriving show]

type func_decl = {
  func_decl_return_typ : typ;
  func_decl_params : (path * var_decl) list;
  func_decl_vars : (path * var_decl) list;
  func_decl_blocks : (path * block) list;
  func_decl_entry_block_path : path;
} [@@deriving show]

type program = {
  program_funcs : (path * func_decl) list;
} [@@deriving show]

type 'a cumulation = {
  cumulation_data : 'a;
  cumulation_vars : (path * var_decl) list;
  cumulation_funcs : (path * func_decl) list;
  cumulation_blocks : (path * block) list;
} [@@deriving map]

let cumulation_of_data (type a) (data : a) =
  {
    cumulation_data = data;
    cumulation_vars = [];
    cumulation_funcs = [];
    cumulation_blocks = [];
  }

let combine_cumulation (type a) (type b)
  (cumulation_1 : a cumulation)  (cumulation_2 : b cumulation) : (a * b) cumulation =
  assert (
    Utils.lists_disjoint
      (cumulation_1.cumulation_vars |> List.map fst)
      (cumulation_2.cumulation_vars |> List.map fst)
  );
  assert (
    Utils.lists_disjoint
      (cumulation_1.cumulation_funcs |> List.map fst)
      (cumulation_2.cumulation_funcs |> List.map fst)
  );
  assert (
    Utils.lists_disjoint
      (cumulation_1.cumulation_blocks |> List.map fst)
      (cumulation_2.cumulation_blocks |> List.map fst)
  );
  {
    cumulation_data = (cumulation_1.cumulation_data, cumulation_2.cumulation_data);
    cumulation_vars = cumulation_1.cumulation_vars @ cumulation_2.cumulation_vars;
    cumulation_funcs = cumulation_1.cumulation_funcs @ cumulation_2.cumulation_funcs;
    cumulation_blocks = cumulation_1.cumulation_blocks @ cumulation_2.cumulation_blocks;
  }

type continuation = {
  continuation_return_typ : typ;
  continuation_fallthrough_block_path : path;
  continuation_break_block_path : path option;
  continuation_continue_block_path : path option;
}

let rec typeck_ast_instr (env : env) (continuation : continuation)
  (ast_instr : Ast.instr) : path cumulation =
  match ast_instr.instr_desc with
  | Ast.Instr_desc_expr ast_expr ->
    let renamed_env = {
      env with
      env_scope_path = env.env_scope_path |> path_append "instr_expr";
    } in
    let expr = ast_expr |> typeck_ast_expr renamed_env in
    let expr_block_path = renamed_env.env_scope_path |> path_append "expr" in
    let expr_block = {
      block_exprs = [expr];
      block_action = Action_goto continuation.continuation_fallthrough_block_path;
    } in
    {
      (cumulation_of_data expr_block_path) with
      cumulation_blocks = [(expr_block_path, expr_block)];
    }
  | Instr_desc_block ast_instr_decls ->
    let renamed_env = {
      env with
      env_scope_path = env.env_scope_path |> path_append "block_expr";
    } in
    let new_env = {
      renamed_env with
      env_used_idents = []
    } in
    ast_instr_decls
      |> typeck_ast_instr_decls new_env continuation
      |> map_cumulation fst
  | Instr_desc_if (cond_ast_expr, then_ast_instr, else_ast_instr) ->
    let renamed_env = {
      env with
      env_scope_path = env.env_scope_path |> path_append "instr_if";
    } in
    let cond_expr = cond_ast_expr |> typeck_ast_expr renamed_env in
    if typ_equivalent Typ_void cond_expr.expr_typ then
      raise (Error (fst ast_instr.instr_loc, renamed_env.env_scope_path, "the `if` condition has a void type"));
    let then_cumulation = then_ast_instr |> typeck_ast_instr {
      renamed_env with
      env_scope_path = renamed_env.env_scope_path |> path_append "then";
    } continuation in
    let else_cumulation = else_ast_instr |> typeck_ast_instr {
      renamed_env with
      env_scope_path = renamed_env.env_scope_path |> path_append "else";
    } continuation in
    let combined_cumulation = combine_cumulation then_cumulation else_cumulation in
    let (then_block_path, else_block_path) = combined_cumulation.cumulation_data in
    let cond_block_path = renamed_env.env_scope_path |> path_append "cond" in
    let cond_block = {
      block_exprs = [];
      block_action = Action_if (cond_expr, then_block_path, else_block_path);
    } in
    combine_cumulation
      combined_cumulation
      {
        (cumulation_of_data cond_block_path) with
        cumulation_blocks = [(cond_block_path, cond_block)];
      }
      |> map_cumulation snd
  | Instr_desc_while (cond_ast_expr, body_ast_instr) ->
    let renamed_env = {
      env with
      env_scope_path = env.env_scope_path |> path_append "instr_while";
    } in
    let cond_expr = cond_ast_expr |> typeck_ast_expr renamed_env in
    if typ_equivalent Typ_void cond_expr.expr_typ then
      raise (Error (fst ast_instr.instr_loc, renamed_env.env_scope_path, "the `while` condition has a void type"));
    let cond_block_path = renamed_env.env_scope_path |> path_append "cond" in
    let end_block_path = renamed_env.env_scope_path |> path_append "end" in
    let end_block = {
      block_exprs = [];
      block_action = Action_goto (continuation.continuation_fallthrough_block_path);
    } in
    let body_cumulation = body_ast_instr |> typeck_ast_instr {
      renamed_env with
      env_scope_path = renamed_env.env_scope_path |> path_append "body";
    } {
      continuation with
      continuation_fallthrough_block_path = cond_block_path;
      continuation_break_block_path = Some end_block_path;
      continuation_continue_block_path = Some cond_block_path;
    } in
    let body_block_path = body_cumulation.cumulation_data in
    let cond_block = {
      block_exprs = [];
      block_action = Action_if (cond_expr, body_block_path, end_block_path);
    } in
    combine_cumulation
      body_cumulation
      {
        (cumulation_of_data cond_block_path) with
        cumulation_blocks = [(cond_block_path, cond_block); (end_block_path, end_block)];
      }
      |> map_cumulation snd
  | Instr_desc_for (cond_ast_expr, step_ast_exprs, body_ast_instr) ->
    let renamed_env = {
      env with
      env_scope_path = env.env_scope_path |> path_append "instr_for";
    } in
    let cond_expr = cond_ast_expr |> typeck_ast_expr renamed_env in
    if typ_equivalent Typ_void cond_expr.expr_typ then
      raise (Error (fst ast_instr.instr_loc, renamed_env.env_scope_path, "the `for` condition has a void type"));
    let cond_block_path = renamed_env.env_scope_path |> path_append "cond" in
    let step_exprs = step_ast_exprs |> List.map (typeck_ast_expr env) in
    let steps_block_path = renamed_env.env_scope_path |> path_append "steps" in
    let steps_block = {
      block_exprs = step_exprs;
      block_action = Action_goto cond_block_path;
    } in
    let end_block_path = renamed_env.env_scope_path |> path_append "end" in
    let end_block = {
      block_exprs = [];
      block_action = Action_goto (continuation.continuation_fallthrough_block_path);
    } in
    let body_cumulation = body_ast_instr |> typeck_ast_instr {
      renamed_env with
      env_scope_path = renamed_env.env_scope_path |> path_append "body";
    } {
      continuation with
      continuation_fallthrough_block_path = steps_block_path;
      continuation_break_block_path = Some end_block_path;
      continuation_continue_block_path = Some cond_block_path;
    } in
    let body_block_path = body_cumulation.cumulation_data in
    let cond_block = {
      block_exprs = [];
      block_action = Action_if (cond_expr, body_block_path, end_block_path);
    } in
    combine_cumulation
      body_cumulation
      {
        (cumulation_of_data cond_block_path) with
        cumulation_blocks = [
          (cond_block_path, cond_block);
          (steps_block_path, steps_block);
          (end_block_path, end_block);
        ];
      }
      |> map_cumulation snd
  | Instr_desc_return ast_expr -> (
    let renamed_env = {
      env with
      env_scope_path = env.env_scope_path |> path_append "instr_return";
    } in
    match ast_expr with
    | None ->
      if continuation.continuation_return_typ <> Typ_void then
        raise (Error (
          fst ast_instr.instr_loc,
          renamed_env.env_scope_path,
          Format.asprintf "value required in `return` from a function of return type `%a`"
            pp_typ continuation.continuation_return_typ
        ));
      let return_block_path = renamed_env.env_scope_path |> path_append "return" in
      let return_block = {
        block_exprs = [];
        block_action = Action_return None;
      } in
      {
        (cumulation_of_data return_block_path) with
        cumulation_blocks = [(return_block_path, return_block)];
      }
    | Some ast_expr ->
      let expr = ast_expr |> typeck_ast_expr renamed_env in
      if not (typ_equivalent continuation.continuation_return_typ expr.expr_typ) then
        raise (Error (
          fst ast_instr.instr_loc,
          renamed_env.env_scope_path,
          Format.asprintf "value of type `%a` in `return` from a function of return type `%a`"
            pp_typ expr.expr_typ
            pp_typ continuation.continuation_return_typ
        ));
      let return_block_path = renamed_env.env_scope_path |> path_append "return" in
      let return_block = {
        block_exprs = [];
        block_action = Action_return (Some expr);
      } in
      {
        (cumulation_of_data return_block_path) with
        cumulation_blocks = [(return_block_path, return_block)];
      }
  )
  | Instr_desc_break -> (
    let renamed_env = {
      env with
      env_scope_path = env.env_scope_path |> path_append "instr_break";
    } in
    match continuation.continuation_break_block_path with
    | None ->
      raise (Error (fst ast_instr.instr_loc, renamed_env.env_scope_path, "nothing to break"))
    | Some break_block_path ->
      let goto_block_path = renamed_env.env_scope_path |> path_append "goto" in
      let goto_block = {
        block_exprs = [];
        block_action = Action_goto break_block_path;
      } in
      {
        (cumulation_of_data goto_block_path) with
        cumulation_blocks = [(goto_block_path, goto_block)];
      }
  )
  | Instr_desc_continue -> (
    let renamed_env = {
      env with
      env_scope_path = env.env_scope_path |> path_append "instr_continue";
    } in
    match continuation.continuation_continue_block_path with
    | None ->
      raise (Error (fst ast_instr.instr_loc, renamed_env.env_scope_path, "nothing to continue"))
    | Some continue_block_path ->
      let goto_block_path = renamed_env.env_scope_path |> path_append "goto" in
      let goto_block = {
        block_exprs = [];
        block_action = Action_goto continue_block_path;
      } in
      {
        (cumulation_of_data goto_block_path) with
        cumulation_blocks = [(goto_block_path, goto_block)];
      }
  )

and typeck_ast_func_decl (env : env) (ast_func_decl : Ast.func_decl) : env cumulation =
  match env.env_used_idents |> List.assoc_opt ast_func_decl.func_decl_name with
  | None ->
    let return_typ = ast_func_decl.func_decl_return_typ |> typeck_ast_typ in
    let param_typs = ast_func_decl.func_decl_params |> List.map (fun (ast_param : Ast.param) ->
      ast_param.param_typ|> typeck_ast_typ
    ) in
    let path = env.env_scope_path |> path_append ("func_" ^ ast_func_decl.func_decl_name) in
    let extended_env = {
      env with
      env_bindings =
        (ast_func_decl.func_decl_name, Binding_func { path; return_typ; param_typs }) :: env.env_bindings;
      env_used_idents = (ast_func_decl.func_decl_name, fst ast_func_decl.func_decl_loc) :: env.env_used_idents;
    } in
    let inner_env = {
      extended_env with
      env_scope_path = env.env_scope_path |> path_append ("decl_" ^ ast_func_decl.func_decl_name);
      env_used_idents = [];
    } in
    let param_inst_decls = ast_func_decl.func_decl_params |> List.map (fun (ast_param : Ast.param) ->
      Ast.Instr_decl_var {
        Ast.var_decl_loc = ast_param.param_loc;
        Ast.var_decl_name = ast_param.param_name;
        Ast.var_decl_typ = ast_param.param_typ;
        Ast.var_decl_init = None;
      }
    ) in
    let param_ast_instrs = param_inst_decls @ [Ast.Instr_decl_instr ast_func_decl.func_decl_body] in
    let func_end_block_path = path |> path_append "func_end" in
    let func_end_block = {
      block_exprs = [];
      block_action = if typ_equivalent Typ_void return_typ then Action_return None else Action_unreachable;
    } in
    let {
      cumulation_data = (entry_block_path, end_env);
      cumulation_vars = vars;
      cumulation_funcs = funcs;
      cumulation_blocks = blocks;
    } = param_ast_instrs |> typeck_ast_instr_decls inner_env {
      continuation_return_typ = return_typ;
      continuation_fallthrough_block_path = func_end_block_path;
      continuation_break_block_path = None;
      continuation_continue_block_path = None;
    } in
    let param_paths = ast_func_decl.func_decl_params |> List.map (fun (ast_param : Ast.param) ->
      match end_env.env_bindings |> List.assoc ast_param.param_name with
      | Binding_var { path; _ } -> path
      | Binding_func _ -> assert false
    ) in
    let func_decl = {
      func_decl_return_typ = return_typ;
      func_decl_params = vars |> List.filter (fun (path, _) -> param_paths |> List.mem path);
      func_decl_vars = vars |> List.filter (fun (path, _) -> not (param_paths |> List.mem path));
      func_decl_blocks = (func_end_block_path, func_end_block) :: blocks;
      func_decl_entry_block_path = entry_block_path;
    } in
    {
      (cumulation_of_data extended_env) with
      cumulation_funcs = (path, func_decl) :: funcs;
    }
  | Some position ->
    raise (Error (
      fst ast_func_decl.func_decl_loc,
      env.env_scope_path,
      Format.asprintf "identifier `%s` is already used for `%a` defined at: %a"
        ast_func_decl.func_decl_name
        pp_binding (env.env_bindings |> List.assoc ast_func_decl.func_decl_name)
        Utils.pp_position position
    ))

and typeck_ast_var_decl (env : env) (next_block_path : path)
  (ast_var_decl : Ast.var_decl) : (path * env) cumulation =
  match env.env_used_idents |> List.assoc_opt ast_var_decl.var_decl_name with
  | None -> (
    let typ = ast_var_decl.var_decl_typ |> typeck_ast_typ in
    if typ_equivalent Typ_void typ then
      raise (Error (
        fst ast_var_decl.var_decl_loc,
        env.env_scope_path,
        Format.asprintf "the variable `%s` has a void type"
          ast_var_decl.var_decl_name
      ));
    let path = env.env_scope_path |> path_append ("var_" ^ ast_var_decl.var_decl_name) in
    let extended_env = {
      env_scope_path = env.env_scope_path |> path_append ("with_" ^ ast_var_decl.var_decl_name);
      env_bindings = (ast_var_decl.var_decl_name, Binding_var { path; typ }) :: env.env_bindings;
      env_used_idents = (ast_var_decl.var_decl_name, fst ast_var_decl.var_decl_loc) :: env.env_used_idents;
    } in
    let inner_env = {
      extended_env with
      env_scope_path = env.env_scope_path |> path_append ("decl_" ^ ast_var_decl.var_decl_name);
    } in
    let result_cumulation = match ast_var_decl.var_decl_init with
    | None -> cumulation_of_data (next_block_path, extended_env)
    | Some init_ast_expr ->
      let init_expr = init_ast_expr |> typeck_ast_expr inner_env in
      if not (typ_equivalent typ init_expr.expr_typ) then
        raise (Error (
          fst ast_var_decl.var_decl_loc,
          env.env_scope_path,
          Format.asprintf "initializing expression of the variable `%s`@ has type %a instead of %a"
            ast_var_decl.var_decl_name
            pp_typ init_expr.expr_typ
            pp_typ typ
        ));
      let assign_expr = {
        expr_typ = init_expr.expr_typ;
        expr_desc = Expr_desc_bin_op (
          Bin_op_assign,
          {
            expr_typ = Typ_pointer typ;
            expr_desc = Expr_desc_var path;
          },
          init_expr
        )
      } in
      let init_block_path = env.env_scope_path |> path_append ("init_" ^ ast_var_decl.var_decl_name) in
      let init_block = {
        block_exprs = [assign_expr];
        block_action = Action_goto next_block_path;
      } in
      {
        (cumulation_of_data (init_block_path, extended_env)) with
        cumulation_blocks = [(init_block_path, init_block)];
      } in
    let var_decl = {
      var_decl_typ = typ;
    } in
    {
      result_cumulation with
      cumulation_vars = [(path, var_decl)];
    }
  )
  | Some position ->
    raise (Error (
      fst ast_var_decl.var_decl_loc,
      env.env_scope_path,
      Format.asprintf "identifier `%s`@ is already used for `%a` defined at: %a"
        ast_var_decl.var_decl_name
        pp_binding (env.env_bindings |> List.assoc ast_var_decl.var_decl_name)
        Utils.pp_position position
    ))

and typeck_ast_instr_decl (env : env) (continuation : continuation)
  (ast_instr_decl : Ast.instr_decl) : (path * env) cumulation =
  match ast_instr_decl with
  | Ast.Instr_decl_func ast_func_decl ->
    ast_func_decl
     |> typeck_ast_func_decl env
     |> map_cumulation (fun env -> (continuation.continuation_fallthrough_block_path, env))
  | Ast.Instr_decl_var ast_var_decl ->
    ast_var_decl
      |> typeck_ast_var_decl env continuation.continuation_fallthrough_block_path
  | Ast.Instr_decl_instr ast_instr ->
    ast_instr
      |> typeck_ast_instr env continuation
      |> map_cumulation (fun block_path -> (block_path, env))

and typeck_ast_instr_decls (env : env) (continuation : continuation)
  (ast_instr_decls : Ast.instr_decl list) : (path * env) cumulation =
  if ast_instr_decls = []
  then cumulation_of_data (continuation.continuation_fallthrough_block_path, env)
  else
    let result_cumulation = ast_instr_decls
      |> List.mapi (fun i ast_instr_decl -> (i, ast_instr_decl))
      |> List.fold_left (fun cumulation (i, ast_instr_decl) ->
        let current_env = (if i = 0 then env else cumulation.cumulation_data |> List.rev |> List.hd |> snd) in
        let scope_path_without_suffix =
          if i <> 0 &&
            current_env.env_scope_path.path_components <> [] &&
            current_env.env_scope_path.path_components |> List.rev |> List.hd = ("group_" ^ string_of_int (i - 1))
          then { path_components = current_env.env_scope_path.path_components |> List.rev |> List.tl |> List.rev }
          else current_env.env_scope_path in
        combine_cumulation
          cumulation
          (
            ast_instr_decl |> typeck_ast_instr_decl
              {
                current_env with
                env_scope_path = (scope_path_without_suffix |> path_append ("group_" ^ string_of_int i));
              }
              {
                continuation with
                continuation_fallthrough_block_path =
                  env.env_scope_path |> path_append ("group_end_" ^ string_of_int i);
              }
          )
          |> map_cumulation (fun (rest, (block_path, env)) -> rest @ [(block_path, env)])
      ) (cumulation_of_data []) in
    ast_instr_decls
      |> List.mapi (fun i _ ->
        {
          (cumulation_of_data ()) with
          cumulation_blocks = [(
            env.env_scope_path |> path_append ("group_end_" ^ string_of_int i),
            {
              block_exprs = [];
              block_action = Action_goto (
                if i = List.length ast_instr_decls - 1
                then continuation.continuation_fallthrough_block_path
                else List.nth result_cumulation.cumulation_data (i + 1) |> fst
              );
            }
          )];
        }
      )
      |> List.fold_left (fun cumulation_1 cumulation_2 ->
        combine_cumulation cumulation_1 cumulation_2 |> map_cumulation fst
      ) (result_cumulation |> map_cumulation (fun list ->
        (list |> List.hd |> fst, list |> List.rev |> List.hd |> snd)
      ))

let typeck_ast_file (ast_file : Ast.file) : program =
  let builtin_position = { Lexing.dummy_pos with pos_fname = "<builtin>" } in
  let builtin_path = empty_path |> path_append "builtin" in
  let global_env = {
    env_scope_path = empty_path;
    env_bindings = [
      (
        "malloc",
        Binding_func {
          path = builtin_path |> path_append "func_malloc";
          return_typ = Typ_pointer Typ_void;
          param_typs = [Typ_int];
        }
      );
      (
        "putchar",
        Binding_func {
          path = builtin_path |> path_append "func_putchar";
          return_typ = Typ_int;
          param_typs = [Typ_int];
        }
      );
    ];
    env_used_idents = [("malloc", builtin_position); ("putchar", builtin_position)];
  } in
  let result_cumulation = ast_file.file_func_decls
    |> List.map (fun func_decl -> Ast.Instr_decl_func func_decl)
    |> typeck_ast_instr_decls global_env {
      continuation_return_typ = Typ_void;
      continuation_fallthrough_block_path = (global_env.env_scope_path |> path_append "file_end");
      continuation_break_block_path = None;
      continuation_continue_block_path = None;
     } in
  {
    program_funcs = result_cumulation.cumulation_funcs;
  }

let pp_program_graph (formatter : Format.formatter) (program : program) : unit =
  simplified_pp_path := true;
  Format.fprintf formatter "digraph {@.";
  program.program_funcs |> List.iteri (fun i (path, func_decl) ->
    Format.fprintf formatter "    subgraph \"cluster_%d\" {@." i;
    let pp_path_var_decl =
      Format.pp_print_list
        ~pp_sep:(fun formatter () -> Format.fprintf formatter ",@ ")
        (fun formatter (path, var_decl) ->
          Format.fprintf formatter "%a %a" pp_typ var_decl.var_decl_typ pp_path path
        ) in
    let label =
      Format.asprintf "%a %a(%a) [%a]"
        pp_typ func_decl.func_decl_return_typ
        pp_path path
        pp_path_var_decl func_decl.func_decl_params
        pp_path_var_decl func_decl.func_decl_vars in
    Format.fprintf formatter "        label = \"%s\";@." label;
    let source_path = path |> path_append "source" in
    Format.fprintf formatter "        \"%a\"[style = invisible];@." pp_path_strict source_path;
    Format.fprintf formatter "        \"%a\" -> \"%a\"@."
      pp_path_strict source_path
      pp_path_strict func_decl.func_decl_entry_block_path;
    func_decl.func_decl_blocks |> List.iter (fun (block_path, block) ->
      let replace_breaks str = str |> Str.global_replace (Str.regexp "\n") "<br/>\n" in
      let label =
        Format.asprintf "<table border=\"0\" cellborder=\"1\" cellspacing=\"0\"><tr><td>%s</td></tr>
        <tr><td></td></tr>%a</table>"
          (Format.asprintf "%a" pp_path block_path |> replace_breaks)
          (
            Format.pp_print_list
            (fun formatter expr ->
              Format.fprintf formatter "<tr><td>%s</td></tr>" (Format.asprintf "%a" pp_expr expr |> replace_breaks)
            )
          ) block.block_exprs in
      Format.fprintf formatter "        \"%a\"[shape=plaintext; label = <%s>];@." pp_path_strict block_path label;
      match block.block_action with
      | Action_goto target_block_path ->
        Format.fprintf formatter "        \"%a\" -> \"%a\";@."
          pp_path_strict block_path
          pp_path_strict target_block_path;
      | Action_if (cond_expr, then_block_path, else_block_path) ->
        Format.fprintf formatter "        \"%a\" -> \"%a\" [label=\"%s ?= true\"];@."
          pp_path_strict block_path
          pp_path_strict then_block_path
          (Format.asprintf "%a" pp_expr cond_expr);
        Format.fprintf formatter "        \"%a\" -> \"%a\" [label=\"%s ?= false\"];@."
          pp_path_strict block_path
          pp_path_strict else_block_path
          (Format.asprintf "%a" pp_expr cond_expr);
      | Action_return return_expr -> (
        let sink_path = block_path |> path_append "sink" in
        Format.fprintf formatter "        \"%a\"[style = invisible];@." pp_path_strict sink_path;
        match return_expr with
        | None ->
          Format.fprintf formatter "        \"%a\" -> \"%a\" [label=\"return\"];@."
            pp_path_strict block_path
            pp_path_strict sink_path
        | Some return_expr ->
          Format.fprintf formatter "        \"%a\" -> \"%a\" [label=\"return %s\"];@."
            pp_path_strict block_path
            pp_path_strict sink_path
            (Format.asprintf "%a" pp_expr return_expr)
      )
      | Action_unreachable ->
        let sink_path = block_path |> path_append "sink" in
        Format.fprintf formatter "        \"%a\"[style = invisible];@." pp_path_strict sink_path;
        Format.fprintf formatter "        \"%a\" -> \"%a\" [label=\"unreachable\"];@."
          pp_path_strict block_path
          pp_path_strict sink_path
    );
    Format.fprintf formatter "    }@.";
  );
  Format.fprintf formatter "}@.";
  simplified_pp_path := false

let show_program_graph (program : program) : string =
  Format.asprintf "%a" pp_program_graph program
