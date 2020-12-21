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
	movq $1, 8(%rax)
	popq %r8
	movq 0(%r8), %rcx
	movq 0(%rax), %rdx
	cmpq $2, %rcx
	jne .Lerror0
	cmpq $2, %rdx
	jne .Lerror0
	jmp .L0
.Lerror0:
	movq $.S1, %rdi
	call error
.L0:
	movq 8(%rax), %rax
	movq 8(%r8), %r8
	addq %r8, %rax
	movq %rax, %rbx
	movq $16, %rdi
	call malloc
	movq $2, 0(%rax)
	movq %rbx, 8(%rax)
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
	movq $16, %rdi
	call malloc
	movq $2, 0(%rax)
	movq $1, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call malloc
	movq $2, 0(%rax)
	movq $4, 8(%rax)
	popq %r8
	movq 0(%r8), %rcx
	movq 0(%rax), %rdx
	cmpq $2, %rcx
	jne .Lerror1
	cmpq $2, %rdx
	jne .Lerror1
	jmp .L1
.Lerror1:
	movq $.S3, %rdi
	call error
.L1:
	movq 8(%rax), %rax
	movq 8(%r8), %r8
	subq %r8, %rax
	movq %rax, %rbx
	movq $16, %rdi
	call malloc
	movq $2, 0(%rax)
	movq %rbx, 8(%rax)
	movq %rax, %rdi
	call print
	movq $16, %rdi
	call malloc
	movq $3, 0(%rax)
	movq $.S2, 8(%rax)
	movq %rax, %rdi
	call print
	movq $8, %rdi
	call malloc
	movq $0, 0(%rax)
	addq $0, %rsp
	movq $16, %rdi
	call malloc
	movq $2, 0(%rax)
	movq $4, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call malloc
	movq $2, 0(%rax)
	movq $11, 8(%rax)
	popq %r8
	movq 0(%r8), %rcx
	movq 0(%rax), %rdx
	cmpq $2, %rcx
	jne .Lerror2
	cmpq $2, %rdx
	jne .Lerror2
	jmp .L2
.Lerror2:
	movq $.S6, %rdi
	call error
.L2:
	movq 8(%rax), %rax
	movq 8(%r8), %r8
	testq %r8, %r8
	jnz .L3
	movq $.S5, %rdi
	call error
.L3:
	movq $0, %rdx
	idivq %r8
	movq %rdx, %rax
	movq %rax, %rbx
	movq $16, %rdi
	call malloc
	movq $2, 0(%rax)
	movq %rbx, 8(%rax)
	movq %rax, %rdi
	call print
	movq $16, %rdi
	call malloc
	movq $3, 0(%rax)
	movq $.S4, 8(%rax)
	movq %rax, %rdi
	call print
	movq $8, %rdi
	call malloc
	movq $0, 0(%rax)
	addq $0, %rsp
	movq $16, %rdi
	call malloc
	movq $2, 0(%rax)
	movq $3, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call malloc
	movq $2, 0(%rax)
	movq $1, 8(%rax)
	popq %r8
	movq 0(%r8), %rcx
	movq 0(%rax), %rdx
	cmpq $2, %rcx
	jne .Lerror3
	cmpq $2, %rdx
	jne .Lerror3
	jmp .L4
.Lerror3:
	movq $.S8, %rdi
	call error
.L4:
	movq 8(%rax), %rax
	movq 8(%r8), %r8
	imulq %r8, %rax
	movq %rax, %rbx
	movq $16, %rdi
	call malloc
	movq $2, 0(%rax)
	movq %rbx, 8(%rax)
	movq %rax, %rdi
	call print
	movq $16, %rdi
	call malloc
	movq $3, 0(%rax)
	movq $.S7, 8(%rax)
	movq %rax, %rdi
	call print
	movq $8, %rdi
	call malloc
	movq $0, 0(%rax)
	addq $0, %rsp
	leave
	movq $0, %rax
	ret

error:
	movq %rdi, %rsi
	movq $.S9, %rdi
	movq $0, %rax
	call printf
	movq $1, %rdi
	call exit

pow:
	movq $1, %rax
	jmp .L7
.L5:
	testq $1, %rsi
	jz .L6
	imulq %rdi, %rax
.L6:
	imulq %rdi, %rdi
	shrq $1, %rsi
.L7:
	testq %rsi, %rsi
	jnz .L5
	ret

print:
	movq 0(%rdi), %rcx
	cmpq $0, %rcx
	je .L8
	cmpq $1, %rcx
	je .L9
	cmpq $2, %rcx
	je .L10
	cmpq $3, %rcx
	je .L11
	movq $.S14, %rdi
	call error
.L8:
	movq $.S13, %rdi
	movq $0, %rax
	call printf
	ret
.L9:
	movq 8(%rdi), %rax
	movq $.S11, %rdi
	testq %rax, %rax
	jnz .L12
	movq $.S12, %rdi
.L12:
	movq $0, %rax
	call printf
	ret
.L10:
	movq 8(%rdi), %rsi
	movq $.S10, %rdi
	movq $0, %rax
	call printf
	ret
.L11:
	movq 8(%rdi), %rdi
	movq $0, %rax
	call printf
	ret

	.data
.S0:
	.string "\n"
.S1:
	.string "invalid argument types for operator +"
.S2:
	.string "\n"
.S3:
	.string "invalid argument types for operator -"
.S4:
	.string "\n"
.S5:
	.string "modulo by zero"
.S6:
	.string "invalid argument types for operator %"
.S7:
	.string "\n"
.S8:
	.string "invalid argument types for operator *"
.S9:
	.string "runtime error: %s\n"
.S10:
	.string "%lld"
.S11:
	.string "true"
.S12:
	.string "false"
.S13:
	.string "nothing"
.S14:
	.string "can't print value"
