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

type const =
  | Const_null
  | Const_int of int64
  [@@deriving show]

type un_op =
  | Un_op_nonnull
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
  | Expr_desc_un_op (Un_op_nonnull, expr_1) -> Format.fprintf formatter "?%a" pp_expr expr_1
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
