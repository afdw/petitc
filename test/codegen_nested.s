.text
.globl main

___group_0___func_main:
  pushq %rbp
  movq %rsp, %rbp
  subq $8, %rsp
  jmp ___group_0___decl_main___group_0___block_expr___group_end_0

  ___group_0___func_main___func_end:
    ud2

  ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_2___instr_expr___expr:
    pushq $10
    movq %rbp, %rax
    pushq %rax
    call ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___func_fib
    addq $16, %rsp
    pushq %rax
    addq $8, %rsp
    jmp ___group_0___decl_main___group_0___block_expr___group_end_2

  ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_3___instr_expr___expr:
    movq %rbp, %rax
    leaq -8(%rax), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    movq %rbp, %rax
    pushq %rax
    call ___builtin___func_putchar
    addq $16, %rsp
    pushq %rax
    addq $8, %rsp
    jmp ___group_0___decl_main___group_0___block_expr___group_end_3

  ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_4___instr_expr___expr:
    pushq $10
    movq %rbp, %rax
    pushq %rax
    call ___builtin___func_putchar
    addq $16, %rsp
    pushq %rax
    addq $8, %rsp
    jmp ___group_0___decl_main___group_0___block_expr___group_end_4

  ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_5___instr_return___return:
    pushq $0
    popq %rax
    movq %rbp, %rsp
    popq %rbp
    ret

  ___group_0___decl_main___group_0___block_expr___group_end_0:
    jmp ___group_0___decl_main___group_0___block_expr___group_end_1

  ___group_0___decl_main___group_0___block_expr___group_end_1:
    jmp ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_2___instr_expr___expr

  ___group_0___decl_main___group_0___block_expr___group_end_2:
    jmp ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_3___instr_expr___expr

  ___group_0___decl_main___group_0___block_expr___group_end_3:
    jmp ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_4___instr_expr___expr

  ___group_0___decl_main___group_0___block_expr___group_end_4:
    jmp ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_5___instr_return___return

  ___group_0___decl_main___group_0___block_expr___group_end_5:
    jmp ___group_0___decl_main___group_end_0

  ___group_0___decl_main___group_end_0:
    jmp ___group_0___func_main___func_end

___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___func_fib:
  pushq %rbp
  movq %rsp, %rbp
  subq $0, %rsp
  jmp ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_end_0

  ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___func_fib___func_end:
    movq %rbp, %rsp
    popq %rbp
    ret

  ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_0___with_n___group_1___block_expr___group_1___instr_if___then___instr_expr___expr:
    movq %rbp, %rax
    movq 16(%rax), %rax
    leaq -8(%rax), %rax
    pushq %rax
    movq %rbp, %rax
    leaq 24(%rax), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    popq %rbx
    popq %rax
    movq %rbx, (%rax)
    movq %rbx, %rax
    pushq %rax
    addq $8, %rsp
    jmp ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_0___with_n___group_1___block_expr___group_end_1

  ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_0___with_n___group_1___block_expr___group_1___instr_if___else___instr_expr___expr:
    movq %rbp, %rax
    pushq %rax
    call ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_0___with_n___group_1___block_expr___group_0___func_mksum
    addq $8, %rsp
    pushq %rax
    addq $8, %rsp
    jmp ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_0___with_n___group_1___block_expr___group_end_1

  ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_0___with_n___group_1___block_expr___group_1___instr_if___cond:
    movq %rbp, %rax
    leaq 24(%rax), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    pushq $1
    popq %rbx
    popq %rax
    cmpq %rbx, %rax
    setle %al
    andq $1, %rax
    pushq %rax
    popq %rax
    testq %rax, %rax
    jnz ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_0___with_n___group_1___block_expr___group_1___instr_if___then___instr_expr___expr
    jmp ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_0___with_n___group_1___block_expr___group_1___instr_if___else___instr_expr___expr

  ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_0___with_n___group_1___block_expr___group_end_0:
    jmp ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_0___with_n___group_1___block_expr___group_1___instr_if___cond

  ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_0___with_n___group_1___block_expr___group_end_1:
    jmp ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_end_1

  ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_end_0:
    jmp ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_0___with_n___group_1___block_expr___group_end_0

  ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_end_1:
    jmp ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___func_fib___func_end

___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_0___with_n___group_1___block_expr___group_0___func_mksum:
  pushq %rbp
  movq %rsp, %rbp
  subq $8, %rsp
  jmp ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_0___with_n___group_1___block_expr___group_0___decl_mksum___group_0___block_expr___group_0___instr_expr___expr

  ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_0___with_n___group_1___block_expr___group_0___func_mksum___func_end:
    movq %rbp, %rsp
    popq %rbp
    ret

  ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_0___with_n___group_1___block_expr___group_0___decl_mksum___group_0___block_expr___group_0___instr_expr___expr:
    movq %rbp, %rax
    movq 16(%rax), %rax
    leaq 24(%rax), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    pushq $2
    popq %rbx
    popq %rax
    subq %rbx, %rax
    pushq %rax
    movq %rbp, %rax
    movq 16(%rax), %rax
    movq 16(%rax), %rax
    pushq %rax
    call ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___func_fib
    addq $16, %rsp
    pushq %rax
    addq $8, %rsp
    jmp ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_0___with_n___group_1___block_expr___group_0___decl_mksum___group_0___block_expr___group_end_0

  ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_0___with_n___group_1___block_expr___group_0___decl_mksum___group_0___block_expr___group_1___init_tmp:
    movq %rbp, %rax
    leaq -8(%rax), %rax
    pushq %rax
    movq %rbp, %rax
    movq 16(%rax), %rax
    movq 16(%rax), %rax
    leaq -8(%rax), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    popq %rbx
    popq %rax
    movq %rbx, (%rax)
    movq %rbx, %rax
    pushq %rax
    addq $8, %rsp
    jmp ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_0___with_n___group_1___block_expr___group_0___decl_mksum___group_0___block_expr___group_end_1

  ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_0___with_n___group_1___block_expr___group_0___decl_mksum___group_0___block_expr___group_1___with_tmp___group_2___instr_expr___expr:
    movq %rbp, %rax
    movq 16(%rax), %rax
    leaq 24(%rax), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    pushq $1
    popq %rbx
    popq %rax
    subq %rbx, %rax
    pushq %rax
    movq %rbp, %rax
    movq 16(%rax), %rax
    movq 16(%rax), %rax
    pushq %rax
    call ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___func_fib
    addq $16, %rsp
    pushq %rax
    addq $8, %rsp
    jmp ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_0___with_n___group_1___block_expr___group_0___decl_mksum___group_0___block_expr___group_end_2

  ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_0___with_n___group_1___block_expr___group_0___decl_mksum___group_0___block_expr___group_1___with_tmp___group_3___instr_expr___expr:
    movq %rbp, %rax
    movq 16(%rax), %rax
    movq 16(%rax), %rax
    leaq -8(%rax), %rax
    pushq %rax
    movq %rbp, %rax
    movq 16(%rax), %rax
    movq 16(%rax), %rax
    leaq -8(%rax), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    movq %rbp, %rax
    leaq -8(%rax), %rax
    pushq %rax
    popq %rax
    movq (%rax), %rax
    pushq %rax
    popq %rbx
    popq %rax
    addq %rbx, %rax
    pushq %rax
    popq %rbx
    popq %rax
    movq %rbx, (%rax)
    movq %rbx, %rax
    pushq %rax
    addq $8, %rsp
    jmp ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_0___with_n___group_1___block_expr___group_0___decl_mksum___group_0___block_expr___group_end_3

  ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_0___with_n___group_1___block_expr___group_0___decl_mksum___group_0___block_expr___group_end_0:
    jmp ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_0___with_n___group_1___block_expr___group_0___decl_mksum___group_0___block_expr___group_1___init_tmp

  ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_0___with_n___group_1___block_expr___group_0___decl_mksum___group_0___block_expr___group_end_1:
    jmp ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_0___with_n___group_1___block_expr___group_0___decl_mksum___group_0___block_expr___group_1___with_tmp___group_2___instr_expr___expr

  ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_0___with_n___group_1___block_expr___group_0___decl_mksum___group_0___block_expr___group_end_2:
    jmp ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_0___with_n___group_1___block_expr___group_0___decl_mksum___group_0___block_expr___group_1___with_tmp___group_3___instr_expr___expr

  ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_0___with_n___group_1___block_expr___group_0___decl_mksum___group_0___block_expr___group_end_3:
    jmp ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_0___with_n___group_1___block_expr___group_0___decl_mksum___group_end_0

  ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_0___with_n___group_1___block_expr___group_0___decl_mksum___group_end_0:
    jmp ___group_0___decl_main___group_0___block_expr___group_0___with_f___group_1___decl_fib___group_0___with_n___group_1___block_expr___group_0___func_mksum___func_end

___builtin___func_malloc:
  pushq %rbp
  movq %rsp, %rbp
  movq 24(%rsp), %rdi
  andq $0xfffffffffffffff0, %rsp
  call malloc
  movq %rbp, %rsp
  popq %rbp
  ret

___builtin___func_putchar:
  pushq %rbp
  movq %rsp, %rbp
  movq 24(%rsp), %rdi
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
