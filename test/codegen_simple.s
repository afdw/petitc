.text
.globl main

___group_0___func_main:
  pushq %rbp
  movq %rsp, %rbp
  subq $8, %rsp
  jmp ___group_0___decl_main___group_0___block_expr___group_0___instr_expr___expr

  ___group_0___func_main___func_end:
    ud2

  ___group_0___decl_main___group_0___block_expr___group_0___instr_expr___expr:
    pushq $6
    pushq $6
    popq %rbx
    popq %rax
    imulq %rbx, %rax
    pushq %rax
    pushq $6
    popq %rbx
    popq %rax
    addq %rbx, %rax
    pushq %rax
    call ___builtin___func_putchar
    addq $8, %rsp
    pushq %rax
    addq $8, %rsp
    jmp ___group_0___decl_main___group_0___block_expr___group_end_0

  ___group_0___decl_main___group_0___block_expr___group_1___instr_expr___expr:
    pushq $236
    pushq $7
    popq %rbx
    popq %rax
    cqto
    idivq %rbx
    movq %rdx, %rax
    pushq %rax
    pushq $5214
    pushq $140
    popq %rbx
    popq %rax
    cqto
    idivq %rbx
    pushq %rax
    popq %rbx
    popq %rax
    addq %rbx, %rax
    pushq %rax
    call ___builtin___func_putchar
    addq $8, %rsp
    pushq %rax
    addq $8, %rsp
    jmp ___group_0___decl_main___group_0___block_expr___group_end_1

  ___group_0___decl_main___group_0___block_expr___group_2___block_expr___group_0___init_i:
    leaq -8(%rbp), %rax
    pushq %rax
    pushq $0
    popq %rbx
    popq %rax
    movq %rbx, (%rax)
    movq %rbx, %rax
    pushq %rax
    addq $8, %rsp
    jmp ___group_0___decl_main___group_0___block_expr___group_2___block_expr___group_end_0

  ___group_0___decl_main___group_0___block_expr___group_2___block_expr___group_0___with_i___group_1___instr_for___body___block_expr___group_0___instr_expr___expr:
    pushq $42
    call ___builtin___func_putchar
    addq $8, %rsp
    pushq %rax
    addq $8, %rsp
    jmp ___group_0___decl_main___group_0___block_expr___group_2___block_expr___group_0___with_i___group_1___instr_for___body___block_expr___group_end_0

  ___group_0___decl_main___group_0___block_expr___group_2___block_expr___group_0___with_i___group_1___instr_for___body___block_expr___group_end_0:
    jmp ___group_0___decl_main___group_0___block_expr___group_2___block_expr___group_0___with_i___group_1___instr_for___steps

  ___group_0___decl_main___group_0___block_expr___group_2___block_expr___group_0___with_i___group_1___instr_for___cond:
    leaq -8(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    pushq $3
    popq %rbx
    popq %rax
    cmpq %rbx, %rax
    setl %al
    andq $1, %rax
    pushq %rax
    popq %rax
    testq %rax, %rax
    jnz ___group_0___decl_main___group_0___block_expr___group_2___block_expr___group_0___with_i___group_1___instr_for___body___block_expr___group_0___instr_expr___expr
    jmp ___group_0___decl_main___group_0___block_expr___group_2___block_expr___group_0___with_i___group_1___instr_for___end

  ___group_0___decl_main___group_0___block_expr___group_2___block_expr___group_0___with_i___group_1___instr_for___steps:
    leaq -8(%rbp), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rbx
    addq $1, %rbx
    movq %rbx, (%rax)
    movq %rbx, %rax
    pushq %rax
    addq $8, %rsp
    jmp ___group_0___decl_main___group_0___block_expr___group_2___block_expr___group_0___with_i___group_1___instr_for___cond

  ___group_0___decl_main___group_0___block_expr___group_2___block_expr___group_0___with_i___group_1___instr_for___end:
    jmp ___group_0___decl_main___group_0___block_expr___group_2___block_expr___group_end_1

  ___group_0___decl_main___group_0___block_expr___group_2___block_expr___group_end_0:
    jmp ___group_0___decl_main___group_0___block_expr___group_2___block_expr___group_0___with_i___group_1___instr_for___cond

  ___group_0___decl_main___group_0___block_expr___group_2___block_expr___group_end_1:
    jmp ___group_0___decl_main___group_0___block_expr___group_end_2

  ___group_0___decl_main___group_0___block_expr___group_3___instr_if___then___block_expr___group_0___instr_expr___expr:
    pushq $42
    call ___builtin___func_putchar
    addq $8, %rsp
    pushq %rax
    addq $8, %rsp
    jmp ___group_0___decl_main___group_0___block_expr___group_3___instr_if___then___block_expr___group_end_0

  ___group_0___decl_main___group_0___block_expr___group_3___instr_if___then___block_expr___group_end_0:
    jmp ___group_0___decl_main___group_0___block_expr___group_end_3

  ___group_0___decl_main___group_0___block_expr___group_3___instr_if___else___block_expr___group_0___instr_expr___expr:
    pushq $43
    call ___builtin___func_putchar
    addq $8, %rsp
    pushq %rax
    addq $8, %rsp
    jmp ___group_0___decl_main___group_0___block_expr___group_3___instr_if___else___block_expr___group_end_0

  ___group_0___decl_main___group_0___block_expr___group_3___instr_if___else___block_expr___group_end_0:
    jmp ___group_0___decl_main___group_0___block_expr___group_end_3

  ___group_0___decl_main___group_0___block_expr___group_3___instr_if___cond:
    pushq $1
    popq %rax
    testq %rax, %rax
    jnz ___group_0___decl_main___group_0___block_expr___group_3___instr_if___then___block_expr___group_0___instr_expr___expr
    jmp ___group_0___decl_main___group_0___block_expr___group_3___instr_if___else___block_expr___group_0___instr_expr___expr

  ___group_0___decl_main___group_0___block_expr___group_4___instr_expr___expr:
    pushq $10
    call ___builtin___func_putchar
    addq $8, %rsp
    pushq %rax
    addq $8, %rsp
    jmp ___group_0___decl_main___group_0___block_expr___group_end_4

  ___group_0___decl_main___group_0___block_expr___group_5___instr_return___return:
    pushq $0
    popq %rax
    movq %rbp, %rsp
    popq %rbp
    ret

  ___group_0___decl_main___group_0___block_expr___group_end_0:
    jmp ___group_0___decl_main___group_0___block_expr___group_1___instr_expr___expr

  ___group_0___decl_main___group_0___block_expr___group_end_1:
    jmp ___group_0___decl_main___group_0___block_expr___group_2___block_expr___group_0___init_i

  ___group_0___decl_main___group_0___block_expr___group_end_2:
    jmp ___group_0___decl_main___group_0___block_expr___group_3___instr_if___cond

  ___group_0___decl_main___group_0___block_expr___group_end_3:
    jmp ___group_0___decl_main___group_0___block_expr___group_4___instr_expr___expr

  ___group_0___decl_main___group_0___block_expr___group_end_4:
    jmp ___group_0___decl_main___group_0___block_expr___group_5___instr_return___return

  ___group_0___decl_main___group_0___block_expr___group_end_5:
    jmp ___group_0___decl_main___group_end_0

  ___group_0___decl_main___group_end_0:
    jmp ___group_0___func_main___func_end

___builtin___func_malloc:
  pushq %rbp
  movq %rsp, %rbp
  movq 16(%rsp), %rdi
  andq $0xfffffffffffffff0, %rsp
  call malloc
  movq %rbp, %rsp
  popq %rbp
  ret

___builtin___func_putchar:
  pushq %rbp
  movq %rsp, %rbp
  movq 16(%rsp), %rdi
  andq $0xfffffffffffffff0, %rsp
  call putchar
  movq %rbp, %rsp
  popq %rbp
  ret

main:
  pushq %rbp
  movq %rsp, %rbp
  call ___group_0___func_main
  movq %rbp, %rsp
  popq %rbp
  ret
