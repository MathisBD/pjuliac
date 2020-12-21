	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $16, %rdi
	call malloc
	movq $3, 0(%rax)
	movq $.S1, 8(%rax)
	pushq %rax
	movq 0(%rsp), %rax
	movq 0(%rax), %rax
	call f_Any
	jmp .L0
.L0:
	addq $8, %rsp
	movq %rax, %rdi
	call print
	movq $16, %rdi
	call malloc
	movq $3, 0(%rax)
	movq $.S0, 8(%rax)
	movq %rax, %rdi
	call print
	movq $8, %rdi
	call malloc
	movq $0, 0(%rax)
	addq $0, %rsp
	leave
	movq $0, %rax
	ret

f_Any:
	pushq %rbp
	movq %rsp, %rbp
	movq 16(%rbp), %rax
	leave
	ret
	leave
	ret

error:
	movq %rdi, %rsi
	movq $.S2, %rdi
	movq $0, %rax
	call printf
	movq $1, %rdi
	call exit

pow:
	movq $1, %rax
	jmp .L3
.L1:
	testq $1, %rsi
	jz .L2
	imulq %rdi, %rax
.L2:
	imulq %rdi, %rdi
	shrq $1, %rsi
.L3:
	testq %rsi, %rsi
	jnz .L1
	ret

print:
	movq 0(%rdi), %rcx
	cmpq $0, %rcx
	je .L4
	cmpq $1, %rcx
	je .L5
	cmpq $2, %rcx
	je .L6
	cmpq $3, %rcx
	je .L7
	movq $.S7, %rdi
	call error
.L4:
	movq $.S6, %rdi
	movq $0, %rax
	call printf
	ret
.L5:
	movq 8(%rdi), %rax
	movq $.S4, %rdi
	testq %rax, %rax
	jnz .L8
	movq $.S5, %rdi
.L8:
	movq $0, %rax
	call printf
	ret
.L6:
	movq 8(%rdi), %rsi
	movq $.S3, %rdi
	movq $0, %rax
	call printf
	ret
.L7:
	movq 8(%rdi), %rdi
	movq $0, %rax
	call printf
	ret

	.data
.S0:
	.string "\n"
.S1:
	.string "hello"
.S2:
	.string "runtime error: %s\n"
.S3:
	.string "%lld"
.S4:
	.string "true"
.S5:
	.string "false"
.S6:
	.string "nothing"
.S7:
	.string "can't print value"
