open Typed

let emit_path (path : path) : string =
  "___" ^ (path.path_components |> String.concat "___")

let rec emit_expr (var_offsets : (path * int) list) (expr : expr) : string =
  match expr.expr_desc with
  | Expr_desc_var (path, depth_difference) ->
    "    movq %rbp, %rax\n" ^
    (List.init depth_difference (fun _ ->
      "    movq 16(%rax), %rax\n"
    ) |> String.concat "") ^
    "    leaq " ^ (string_of_int (var_offsets |> List.assoc path)) ^ "(%rax), %rax\n" ^
    "    pushq %rax\n"
  | Expr_desc_call (path, depth_difference, arg_exprs) ->
    let args_code = arg_exprs |> List.map (emit_expr var_offsets) |> String.concat "" in
    args_code ^
    "    movq %rbp, %rax\n" ^
    (List.init depth_difference (fun _ ->
      "    movq 16(%rax), %rax\n"
    ) |> String.concat "") ^
    "    pushq %rax\n" ^
    "    call " ^ (path |> emit_path) ^ "\n" ^
    "    addq $" ^ (string_of_int (((arg_exprs |> List.length) + 1) * 8)) ^ ", %rsp\n" ^
    "    pushq %rax\n"
  | Expr_desc_const Const_null ->
    "    pushq $0\n"
  | Expr_desc_const (Const_int n) ->
    "    pushq $" ^ (Int64.to_string n) ^ "\n"
  | Expr_desc_un_op (un_op, expr_1) ->
    (expr_1 |> emit_expr var_offsets) ^
    "    popq %rax\n" ^ (
      match un_op with
      | Un_op_nonnull ->
        "    testq %rax, %rax\n" ^
        "    setnz %rax\n"
      | Un_op_not ->
        "    testq %rax, %rax\n" ^
        "    setz %rax\n"
      | Un_op_pre_incr n ->
        "    movq (%rax), %rbx\n" ^
        "    addq $" ^ (string_of_int n) ^ ", %rbx\n" ^
        "    movq %rbx, (%rax)\n" ^
        "    movq %rbx, %rax\n" ^
        "    subq $" ^ (string_of_int n) ^ ", %rax\n"
      | Un_op_pre_decr n ->
        "    movq (%rax), %rbx\n" ^
        "    subq $" ^ (string_of_int n) ^ ", %rbx\n" ^
        "    movq %rbx, (%rax)\n" ^
        "    movq %rbx, %rax\n" ^
        "    addq $" ^ (string_of_int n) ^ ", %rax\n"
      | Un_op_post_incr n ->
        "    movq (%rax), %rbx\n" ^
        "    addq $" ^ (string_of_int n) ^ ", %rbx\n" ^
        "    movq %rbx, (%rax)\n" ^
        "    movq %rbx, %rax\n"
      | Un_op_post_decr n ->
        "    movq (%rax), %rbx\n" ^
        "    subq $" ^ (string_of_int n) ^ ", %rbx\n" ^
        "    movq %rbx, (%rax)\n" ^
        "    movq %rbx, %rax\n"
      | Un_op_deref ->
        "    movq (%rax), %rax\n"
    ) ^
    "    pushq %rax\n"
  | Expr_desc_bin_op (bin_op, expr_1, expr_2) ->
    (expr_1 |> emit_expr var_offsets) ^
    (expr_2 |> emit_expr var_offsets) ^
    "    popq %rbx\n" ^
    "    popq %rax\n" ^ (
      match bin_op with
      | Bin_op_eq ->
        "    cmpq %rbx, %rax\n" ^
        "    sete %al\n" ^
        "    andq $1, %rax\n"
      | Bin_op_ne ->
        "    cmpq %rbx, %rax\n" ^
        "    setne %al\n" ^
        "    andq $1, %rax\n"
      | Bin_op_lt ->
        "    cmpq %rbx, %rax\n" ^
        "    setl %al\n" ^
        "    andq $1, %rax\n"
      | Bin_op_le ->
        "    cmpq %rbx, %rax\n" ^
        "    setle %al\n" ^
        "    andq $1, %rax\n"
      | Bin_op_gt ->
        "    cmpq %rbx, %rax\n" ^
        "    setg %al\n" ^
        "    andq $1, %rax\n"
      | Bin_op_ge ->
        "    cmpq %rbx, %rax\n" ^
        "    setge %al\n" ^
        "    andq $1, %rax\n"
      | Bin_op_add ->
        "    addq %rbx, %rax\n"
      | Bin_op_sub ->
        "    subq %rbx, %rax\n"
      | Bin_op_mul ->
        "    imulq %rbx, %rax\n"
      | Bin_op_div ->
        "    cqto\n" ^
        "    idivq %rbx\n"
      | Bin_op_mod ->
        "    cqto\n" ^
        "    idivq %rbx\n" ^
        "    movq %rdx, %rax\n"
      | Bin_op_and ->
        "    testq %rax, %rax\n" ^
        "    setnz %rax\n" ^
        "    testq %rbx, %rbx\n" ^
        "    setnz %rbx\n" ^
        "    andq %rbx, %rax\n" ^
        "    andq $1, %rax\n"
      | Bin_op_or ->
        "    testq %rax, %rax\n" ^
        "    setnz %rax\n" ^
        "    testq %rbx, %rbx\n" ^
        "    setnz %rbx\n" ^
        "    orq %rbx, %rax\n" ^
        "    andq $1, %rax\n"
      | Bin_op_assign ->
        "    movq %rbx, (%rax)\n" ^
        "    movq %rbx, %rax\n"
    ) ^
    "    pushq %rax\n"

let emit_action (var_offsets : (path * int) list) (action : action) : string =
  match action with
  | Action_goto path ->
    "    jmp " ^ (path |> emit_path) ^ "\n"
  | Action_if (expr, then_path, else_path) ->
    (expr |> emit_expr var_offsets) ^
    "    popq %rax\n" ^
    "    testq %rax, %rax\n" ^
    "    jnz " ^ (then_path |> emit_path) ^ "\n" ^
    "    jmp " ^ (else_path |> emit_path) ^ "\n"
  | Action_return expr -> (
    match expr with
    | Some expr ->
      (expr |> emit_expr var_offsets) ^
      "    popq %rax\n" ^
      "    movq %rbp, %rsp\n" ^
      "    popq %rbp\n" ^
      "    ret\n"
    | None ->
      "    movq %rbp, %rsp\n" ^
      "    popq %rbp\n" ^
      "    ret\n"
  )
  | Action_unreachable ->
    "    ud2\n"

let emit_block (var_offsets : (path * int) list) (block : block) : string =
  let exprs_code =
    block.block_exprs
      |> List.map (fun expr ->
        (expr |> emit_expr var_offsets) ^
        "    addq $8, %rsp\n"
      )
      |> String.concat "" in
  exprs_code ^ (block.block_action |> emit_action var_offsets)

let emit_func_decl (var_offsets : (path * int) list) (func_decl : func_decl) : string =
  let blocks_code =
    func_decl.func_decl_blocks
      |> List.map (fun (path, block) ->
        "  " ^ (path |> emit_path) ^ ":\n" ^ (block |> emit_block var_offsets)
      )
      |> String.concat "\n" in
  "  pushq %rbp\n" ^
  "  movq %rsp, %rbp\n" ^
  "  subq $" ^ (string_of_int ((func_decl.func_decl_vars |> List.length) * 8)) ^ ", %rsp\n" ^
  "  jmp " ^ (func_decl.func_decl_entry_block_path |> emit_path) ^ "\n\n" ^
  blocks_code

let emit_program (program : program) : string =
  let var_offsets =
    program.program_funcs
      |> List.map (fun (_, func_decl) ->
        (func_decl.func_decl_params |> List.mapi (fun i (path, _) ->
          (path, ((func_decl.func_decl_params |> List.length) - i + 2) * 8))
        ) @
        (func_decl.func_decl_vars |> List.mapi (fun i (path, _) -> (path, -(i + 1) * 8)))
      )
      |> List.fold_left (@) [] in
  let funcs_code =
    program.program_funcs
      |> List.map (fun (path, func_decl) ->
        (path |> emit_path) ^ ":\n" ^ (func_decl |> emit_func_decl var_offsets)
      )
      |> String.concat "\n" in
  let builtin_path = empty_path |> path_append "builtin" in
  ".text\n" ^
  ".globl main\n\n" ^
  funcs_code ^ "\n" ^
  (builtin_path |> path_append "func_malloc" |> emit_path) ^ ":\n" ^
  "  pushq %rbp\n" ^
  "  movq %rsp, %rbp\n" ^
  "  movq 24(%rsp), %rdi\n" ^
  "  andq $0xfffffffffffffff0, %rsp\n" ^
  "  call malloc\n" ^
  "  movq %rbp, %rsp\n" ^
  "  popq %rbp\n" ^
  "  ret\n\n" ^
  (builtin_path |> path_append "func_putchar" |> emit_path) ^ ":\n" ^
  "  pushq %rbp\n" ^
  "  movq %rsp, %rbp\n" ^
  "  movq 24(%rsp), %rdi\n" ^
  "  andq $0xfffffffffffffff0, %rsp\n" ^
  "  call putchar\n" ^
  "  movq %rbp, %rsp\n" ^
  "  popq %rbp\n" ^
  "  ret\n\n" ^
  "main:\n" ^
  "  pushq %rbp\n" ^
  "  movq %rsp, %rbp\n" ^
  "  call " ^ (program.program_main_func_path |> emit_path) ^ "\n" ^
  "  movq %rbp, %rsp\n" ^
  "  popq %rbp\n" ^
  "  ret\n"
