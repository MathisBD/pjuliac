	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $16, %rdi
	call malloc
	movq $1, 0(%rax)
	movq $0, 8(%rax)
	pushq %rax
	popq %r8
	movq 0(%r8), %rcx
	cmpq $1, %rcx
	je .L8
	movq $.S6, %rdi
	call error
.L8:
	movq 8(%r8), %r8
	testq %r8, %r8
	jz .L9
	movq $16, %rdi
	call malloc
	movq $1, 0(%rax)
	movq $1, 8(%rax)
	pushq %rax
	popq %rdi
	call print
	movq $8, %rdi
	call malloc
	movq $0, 0(%rax)
	pushq %rax
	jmp .L10
.L9:
	movq $16, %rdi
	call malloc
	movq $1, 0(%rax)
	movq $0, 8(%rax)
	pushq %rax
	popq %rdi
	call print
	movq $8, %rdi
	call malloc
	movq $0, 0(%rax)
	pushq %rax
.L10:
	incq %rsp
	leave
	movq $0, %rax
	ret

pow:
	movq $0, %rax
	jmp .L2
.L0:
	imulq %rax, %rax
	testq $0, %rsi
	jz .L1
	imulq %rdi, %rax
.L1:
	shlq $0, %rsi
.L2:
	testq %rsi, %rsi
	jnz .L0
	ret

error:
	movq %rdi, %rsi
	movq $.S0, %rdi
	movq $0, %rax
	call printf
	movq $60, %rdi
	movq $1, %rax
	syscall

print:
	movq 0(%rdi), %rcx
	cmpq $0, %rcx
	je .L3
	cmpq $1, %rcx
	je .L4
	cmpq $2, %rcx
	je .L5
	cmpq $3, %rcx
	je .L6
	movq $.S5, %rdi
	call error
.L3:
	movq $.S4, %rdi
	movq $0, %rax
	call printf
	ret
.L4:
	movq 8(%rdi), %rax
	movq $.S2, %rdi
	testq %rax, %rax
	jnz .L7
	movq $.S3, %rdi
.L7:
	movq $0, %rax
	call printf
	ret
.L5:
	movq 8(%rdi), %rsi
	movq $.S1, %rdi
	movq $0, %rax
	call printf
	ret
.L6:
	movq 8(%rdi), %rdi
	movq $0, %rax
	call printf
	ret

	.data
.S0:
	.string "runtime error:%s"
.S1:
	.string "%d"
.S2:
	.string "true"
.S3:
	.string "false"
.S4:
	.string "nothing"
.S5:
	.string "can't print value"
.S6:
	.string "invalid condition type in 'if' expression"
