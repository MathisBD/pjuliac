	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $16, %rdi
	call malloc
	movq $3, 0(%rax)
	movq $.S7, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call malloc
	movq $3, 0(%rax)
	movq $.S6, 8(%rax)
	pushq %rax
	popq %rdi
	call print
	popq %rdi
	call print
	movq $8, %rdi
	call malloc
	movq $0, 0(%rax)
	pushq %rax
	addq $8, %rsp
	pushq $1
	movq $16, %rdi
	call malloc
	movq $2, 0(%rax)
	movq $2, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call malloc
	movq $2, 0(%rax)
	movq $4, 8(%rax)
	pushq %rax
	popq %r9
	popq %r8
	movq 0(%r8), %rcx
	movq 0(%r9), %rdx
	cmpq $2, %rcx
	je .L8
	movq $.S13, %rdi
	call error
.L8:
	cmpq $2, %rdx
	je .L9
	movq $.S12, %rdi
	call error
.L9:
	movq 8(%r8), %r8
	movq 8(%r9), %r9
	pushq %r8
	pushq %r9
	movq $16, %rdi
	call malloc
	movq $2, 0(%rax)
	movq %rax, -8(%rbp)
	jmp .L11
.L10:
	movq 8(%rsp), %r8
	movq -8(%rbp), %rcx
	movq %r8, 8(%rcx)
	movq %rcx, -8(%rbp)
	movq $16, %rdi
	call malloc
	movq $3, 0(%rax)
	movq $.S11, 8(%rax)
	pushq %rax
	movq -8(%rbp), %r8
	cmpq $1, %r8
	jne .L13
	movq $.S10, %rdi
	call error
.L13:
	pushq %r8
	movq $16, %rdi
	call malloc
	movq $2, 0(%rax)
	movq $2, 8(%rax)
	pushq %rax
	popq %r9
	movq 0(%r9), %rdx
	popq %r8
	movq 0(%r8), %rcx
	cmpq $2, %rcx
	jne .Lerror0
	cmpq $2, %rdx
	jne .Lerror0
	jmp .L12
.Lerror0:
	movq $.S9, %rdi
	call error
.L12:
	movq 8(%r9), %r9
	movq 8(%r8), %r8
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
	movq $3, 0(%rax)
	movq $.S8, 8(%rax)
	pushq %rax
	popq %rdi
	call print
	popq %rdi
	call print
	popq %rdi
	call print
	movq $8, %rdi
	call malloc
	movq $0, 0(%rax)
	pushq %rax
	addq $8, %rsp
	incq 8(%rsp)
.L11:
	movq 8(%rsp), %r8
	movq 0(%rsp), %r9
	cmpq %r8, %r9
	jge .L10
	addq $16, %rsp
	leave
	movq $0, %rax
	ret

pow:
	movq $1, %rax
	jmp .L2
.L0:
	testq $1, %rsi
	jz .L1
	imulq %rdi, %rax
.L1:
	imulq %rdi, %rdi
	shrq $1, %rsi
.L2:
	testq %rsi, %rsi
	jnz .L0
	ret

error:
	movq %rdi, %rsi
	movq $.S0, %rdi
	movq $0, %rax
	call printf
	movq $1, %rdi
	call exit

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
	.string "runtime error: %s\n"
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
	.string "hello"
.S7:
	.string "\n"
.S8:
	.string "i^2 = "
.S9:
	.string "invalid argument types for operator ^"
.S10:
	.string "unitinialized variable i"
.S11:
	.string "\n"
.S12:
	.string "invalid 'for' loop bound type"
.S13:
	.string "invalid 'for' loop bound type"
