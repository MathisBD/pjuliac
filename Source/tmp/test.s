	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $16, %rdi
	call malloc
	movq $2, 0(%rax)
	movq $2, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call malloc
	movq $2, 0(%rax)
	movq $62, 8(%rax)
	pushq %rax
	popq %r9
	movq 0(%r9), %rdx
	popq %r8
	movq 0(%r8), %rcx
	cmpq $2, %rcx
	jne .Lerror3
	cmpq $2, %rdx
	jne .Lerror3
	jmp .L4
.Lerror3:
	movq $.S5, %rdi
	call error
.L4:
	movq 8(%r9), %r9
	movq 8(%r8), %r8
	cmpq $0, %r9
	jge .L5
	movq $.S4, %rdi
	call error
.L5:
	movq %r8, %rdi
	movq %r9, %rsi
	call pow
	movq %rax, %r8
	pushq %r8
	movq $16, %rdi
	call malloc
	movq $2, 0(%rax)
	popq %r8
	movq %r8, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call malloc
	movq $2, 0(%rax)
	movq $2, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call malloc
	movq $2, 0(%rax)
	movq $62, 8(%rax)
	pushq %rax
	popq %r9
	movq 0(%r9), %rdx
	popq %r8
	movq 0(%r8), %rcx
	cmpq $2, %rcx
	jne .Lerror2
	cmpq $2, %rdx
	jne .Lerror2
	jmp .L2
.Lerror2:
	movq $.S3, %rdi
	call error
.L2:
	movq 8(%r9), %r9
	movq 8(%r8), %r8
	cmpq $0, %r9
	jge .L3
	movq $.S2, %rdi
	call error
.L3:
	movq %r8, %rdi
	movq %r9, %rsi
	call pow
	movq %rax, %r8
	pushq %r8
	movq $16, %rdi
	call malloc
	movq $2, 0(%rax)
	popq %r8
	movq %r8, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call malloc
	movq $2, 0(%rax)
	movq $1, 8(%rax)
	pushq %rax
	popq %r9
	movq 0(%r9), %rdx
	popq %r8
	movq 0(%r8), %rcx
	cmpq $2, %rcx
	jne .Lerror1
	cmpq $2, %rdx
	jne .Lerror1
	jmp .L1
.Lerror1:
	movq $.S1, %rdi
	call error
.L1:
	movq 8(%r9), %r9
	movq 8(%r8), %r8
	subq %r9, %r8
	pushq %r8
	movq $16, %rdi
	call malloc
	movq $2, 0(%rax)
	popq %r8
	movq %r8, 8(%rax)
	pushq %rax
	popq %r9
	movq 0(%r9), %rdx
	popq %r8
	movq 0(%r8), %rcx
	cmpq $2, %rcx
	jne .Lerror0
	cmpq $2, %rdx
	jne .Lerror0
	jmp .L0
.Lerror0:
	movq $.S0, %rdi
	call error
.L0:
	movq 8(%r9), %r9
	movq 8(%r8), %r8
	addq %r9, %r8
	pushq %r8
	movq $16, %rdi
	call malloc
	movq $2, 0(%rax)
	popq %r8
	movq %r8, 8(%rax)
	pushq %rax
	movq 0(%rsp), %r8
	movq %r8, x
	addq $8, %rsp
	movq $16, %rdi
	call malloc
	movq $3, 0(%rax)
	movq $.S7, 8(%rax)
	pushq %rax
	movq x, %r8
	cmpq $1, %r8
	jne .L6
	movq $.S6, %rdi
	call error
.L6:
	pushq %r8
	popq %rdi
	call print
	popq %rdi
	call print
	movq $8, %rdi
	call malloc
	movq $0, 0(%rax)
	pushq %rax
	addq $8, %rsp
	leave
	movq $0, %rax
	ret

pow:
	movq $1, %rax
	jmp .L14
.L12:
	testq $1, %rsi
	jz .L13
	imulq %rdi, %rax
.L13:
	imulq %rdi, %rdi
	shrq $1, %rsi
.L14:
	testq %rsi, %rsi
	jnz .L12
	ret

error:
	movq %rdi, %rsi
	movq $.S13, %rdi
	movq $0, %rax
	call printf
	movq $1, %rdi
	call exit

print:
	movq 0(%rdi), %rcx
	cmpq $0, %rcx
	je .L7
	cmpq $1, %rcx
	je .L8
	cmpq $2, %rcx
	je .L9
	cmpq $3, %rcx
	je .L10
	movq $.S12, %rdi
	call error
.L7:
	movq $.S11, %rdi
	movq $0, %rax
	call printf
	ret
.L8:
	movq 8(%rdi), %rax
	movq $.S9, %rdi
	testq %rax, %rax
	jnz .L11
	movq $.S10, %rdi
.L11:
	movq $0, %rax
	call printf
	ret
.L9:
	movq 8(%rdi), %rsi
	movq $.S8, %rdi
	movq $0, %rax
	call printf
	ret
.L10:
	movq 8(%rdi), %rdi
	movq $0, %rax
	call printf
	ret

	.data
x:
	.quad 1
.S0:
	.string "invalid argument types for operator +"
.S1:
	.string "invalid argument types for operator -"
.S2:
	.string "negative exponent"
.S3:
	.string "invalid argument types for operator ^"
.S4:
	.string "negative exponent"
.S5:
	.string "invalid argument types for operator ^"
.S6:
	.string "uninitialized variable x"
.S7:
	.string "\n"
.S8:
	.string "%lld"
.S9:
	.string "true"
.S10:
	.string "false"
.S11:
	.string "nothing"
.S12:
	.string "can't print value"
.S13:
	.string "runtime error: %s\n"
