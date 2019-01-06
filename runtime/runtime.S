  .data
Lfmt_int:
  .asciz "%lld\n"
Lfmt_flt:
  .asciz "%f\n"
Lfmt_ptr:
  .asciz "%p\n"
Ltype_error:
  .asciz "What are we doing here, JavaScript?"

  .text
  .global __print$
__print$:
  pushq %rbp
  movq %rsp, %rbp
  movq 24(%rbp), %rax
  cmpq $1, %rax
  jle  1f
  leaq Lfmt_ptr(%rip), %rdi
  movq 16(%rbp), %rsi
  movq $0, %rax
  jmp 3f
1:
  cmpq $0, %rax
  je  0f
  movaps 16(%rbp), %xmm0
  leaq Lfmt_flt(%rip), %rdi
  movq $1, %rax
  jmp 3f
0:
  movq 16(%rbp), %rsi
  leaq Lfmt_int(%rip), %rdi
  movq $0, %rax
3:
  callq _printf
  xorq %rax, %rax
  xorq %rbx, %rbx
  popq %rbp
  retq

  .global __add$
__add$:
  cmpq $1, %rax
  je 1f
  jl 2f
  jmp _type_error
2:
  cmpq $1, %rsi
  je 3f
  jl 4f
  jmp _type_error
1:
  cmpq $1, %rsi
  je 5f
  jl 6f
  jmp _type_error
3:
  cvtsi2sd %rbx, %xmm1
  movq %rdi, %xmm0
  addsd %xmm1, %xmm0
  movq %rsi, %rax
  movq %xmm0, %rbx
  retq
4:
  addq %rdi, %rbx
  retq
5:
  movq %rbx, %xmm0
  movq %rdi, %xmm1
  addsd %xmm0, %xmm1
  movq %xmm1, %rbx
  retq
6:
  cvtsi2sd %rdi, %xmm0
  movq %rbx, %xmm1
  addsd %xmm0, %xmm1
  movq %xmm1, %rbx
  retq

  .global __sub$
__sub$:
  cmpq $1, %rax
  je 1f
  jl 2f
  jmp _type_error
2:
  cmpq $1, %rsi
  je 3f
  jl 4f
  jmp _type_error
1:
  cmpq $1, %rsi
  je 5f
  jl 6f
  jmp _type_error
3:
  cvtsi2sd %rbx, %xmm1
  movq %rdi, %xmm0
  subsd %xmm1, %xmm0
  movq %rsi, %rax
  movq %xmm0, %rbx
  retq
4:
  subq %rbx, %rdi
  movq %rdi, %rbx
  retq
5:
  movq %rbx, %xmm0
  movq %rdi, %xmm1
  subsd %xmm0, %xmm1
  movq %xmm1, %rbx
  retq
6:
  cvtsi2sd %rdi, %xmm0
  movq %rbx, %xmm1
  subsd %xmm1, %xmm0
  movq %xmm0, %rbx
  retq

  .global __mul$
__mul$:
  cmpq $1, %rax
  je 1f
  jl 2f
  jmp _type_error
2:
  cmpq $1, %rsi
  je 3f
  jl 4f
  jmp _type_error
1:
  cmpq $1, %rsi
  je 5f
  jl 6f
  jmp _type_error
3:
  cvtsi2sd %rbx, %xmm1
  movq %rdi, %xmm0
  mulsd %xmm1, %xmm0
  movq %rsi, %rax
  movq %xmm0, %rbx
  retq
4:
  xchg %rax, %rbx
  imulq %rdi
  xchg %rbx, %rax
  retq
5:
  movq %rbx, %xmm0
  movq %rdi, %xmm1
  mulsd %xmm0, %xmm1
  movq %xmm1, %rbx
  retq
6:
  cvtsi2sd %rdi, %xmm0
  movq %rbx, %xmm1
  mulsd %xmm1, %xmm0
  movq %xmm0, %rbx
  retq

  .globl __div$
__div$:
  cmpq $1, %rax
  je 1f
  jl 2f
  jmp _type_error
2:
  cmpq $1, %rsi
  je 3f
  jl 4f
  jmp _type_error
1:
  cmpq $1, %rsi
  je 5f
  jl 6f
  jmp _type_error
3:
  cvtsi2sd %rbx, %xmm1
  movq %rdi, %xmm0
  divsd %xmm1, %xmm0
  movq %rsi, %rax
  movq %xmm0, %rbx
  retq
4:
  xchg %rax, %rdi
  xorq %rdx, %rdx
  idivq %rbx
  movq %rax, %rbx
  xchg %rax, %rdi
  retq
5:
  movq %rbx, %xmm0
  movq %rdi, %xmm1
  divsd %xmm0, %xmm1
  movq %xmm1, %rbx
  retq
6:
  cvtsi2sd %rdi, %xmm0
  movq %rbx, %xmm1
  divsd %xmm1, %xmm0
  movq %xmm0, %rbx
  retq

_type_error:
  pushq %rbp
  movq %rsp, %rbp
  leaq Ltype_error(%rip), %rdi
  callq _puts
  popq %rbp
  jmp _abort

  .global _main
_main:
  pushq %rbp
  movq %rsp, %rbp
  callq main
  movq $0, %rax
  popq %rbp
  retq
