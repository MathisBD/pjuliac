	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	leave
	movq $0, %rax
	ret

haha_Int64:
	pushq %rbp
	movq %rsp, %rbp
	movq $8, %rdi
	call malloc
	movq $0, 0(%rax)
	leave
	ret

haha_Bool:
	pushq %rbp
	movq %rsp, %rbp
	movq $8, %rdi
	call malloc
	movq $0, 0(%rax)
	leave
	ret

haha_String:
	pushq %rbp
	movq %rsp, %rbp
	movq $8, %rdi
	call malloc
	movq $0, 0(%rax)
	leave
	ret

f_Any:
	pushq %rbp
	movq %rsp, %rbp
	movq 16(%rbp), %rax
	pushq %rax
	movq 0(%rsp), %rax
	movq 0(%rax), %rax
	cmpq $2, %rax
	je .L1
	cmpq $1, %rax
	je .L2
	cmpq $3, %rax
	je .L3
	movq $.S0, %rdi
	call error
.L1:
	call haha_Int64
	jmp .L0
.L2:
	call haha_Bool
	jmp .L0
.L3:
	call haha_String
	jmp .L0
.L0:
	addq $8, %rsp
	leave
	ret

error:
	movq %rdi, %rsi
	movq $.S1, %rdi
	movq $0, %rax
	call printf
	movq $1, %rdi
	call exit

pow:
	movq $1, %rax
	jmp .L6
.L4:
	testq $1, %rsi
	jz .L5
	imulq %rdi, %rax
.L5:
	imulq %rdi, %rdi
	shrq $1, %rsi
.L6:
	testq %rsi, %rsi
	jnz .L4
	ret

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
	movq $.S6, %rdi
	call error
.L7:
	movq $.S5, %rdi
	movq $0, %rax
	call printf
	ret
.L8:
	movq 8(%rdi), %rax
	movq $.S3, %rdi
	testq %rax, %rax
	jnz .L11
	movq $.S4, %rdi
.L11:
	movq $0, %rax
	call printf
	ret
.L9:
	movq 8(%rdi), %rsi
	movq $.S2, %rdi
	movq $0, %rax
	call printf
	ret
.L10:
	movq 8(%rdi), %rdi
	movq $0, %rax
	call printf
	ret

	.data
.S0:
	.string "no overload of function haha matches the given argument types"
.S1:
	.string "runtime error: %s\n"
.S2:
	.string "%lld"
.S3:
	.string "true"
.S4:
	.string "false"
.S5:
	.string "nothing"
.S6:
	.string "can't print value"
