	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $16, %rdi
	call malloc
	movq $3, 0(%rax)
	movq $.S6, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call malloc
	movq $2, 0(%rax)
	movq $13, 8(%rax)
	pushq %rax
	movq 0(%rsp), %rax
	movq 0(%rax), %rax
	cmpq $2, %rax
	je .L3
	movq $.S5, %rdi
	call error
.L3:
	movq 8(%rsp), %rax
	movq 0(%rax), %rax
	cmpq $3, %rax
	je .L4
	movq $.S4, %rdi
	call error
.L4:
	movq $24, %rdi
	call malloc
	movq $4, 0(%rax)
	movq 0(%rsp), %r8
	movq %r8, 8(%rax)
	movq 8(%rsp), %r8
	movq %r8, 16(%rax)
	jmp .L2
.L2:
	addq $16, %rsp
	movq %rax, bob
	addq $0, %rsp
	movq bob, %rax
	cmpq $1, %rax
	jne .L7
	movq $.S9, %rdi
	call error
.L7:
	movq 0(%rax), %rcx
	cmpq $4, %rcx
	je .L5
	movq $.S8, %rdi
	call error
.L5:
	pushq %rax
	movq $16, %rdi
	call malloc
	movq $2, 0(%rax)
	movq $42, 8(%rax)
	movq 0(%rax), %rcx
	cmpq $2, %rcx
	je .L6
	movq $.S7, %rdi
	call error
.L6:
	popq %r8
	movq %rax, 8(%r8)
	movq %rax, x
	addq $0, %rsp
	movq x, %rax
	cmpq $1, %rax
	jne .L8
	movq $.S11, %rdi
	call error
.L8:
	movq %rax, %rdi
	call print
	movq $16, %rdi
	call malloc
	movq $3, 0(%rax)
	movq $.S10, 8(%rax)
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
	movq 0(%rax), %rcx
	cmpq $4, %rcx
	je .L1
	movq $.S3, %rdi
	call error
.L1:
	movq 8(%rax), %rax
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
	movq 16(%rbp), %rax
	movq 0(%rax), %rcx
	cmpq $4, %rcx
	je .L0
	movq $.S1, %rdi
	call error
.L0:
	movq 16(%rax), %rax
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
	leave
	ret

error:
	movq %rdi, %rsi
	movq $.S12, %rdi
	movq $0, %rax
	call printf
	movq $1, %rdi
	call exit

pow:
	movq $1, %rax
	jmp .L11
.L9:
	testq $1, %rsi
	jz .L10
	imulq %rdi, %rax
.L10:
	imulq %rdi, %rdi
	shrq $1, %rsi
.L11:
	testq %rsi, %rsi
	jnz .L9
	ret

print:
	movq 0(%rdi), %rcx
	cmpq $0, %rcx
	je .L12
	cmpq $1, %rcx
	je .L13
	cmpq $2, %rcx
	je .L14
	cmpq $3, %rcx
	je .L15
	movq $.S17, %rdi
	call error
.L12:
	movq $.S16, %rdi
	movq $0, %rax
	call printf
	ret
.L13:
	movq 8(%rdi), %rax
	movq $.S14, %rdi
	testq %rax, %rax
	jnz .L16
	movq $.S15, %rdi
.L16:
	movq $0, %rax
	call printf
	ret
.L14:
	movq 8(%rdi), %rsi
	movq $.S13, %rdi
	movq $0, %rax
	call printf
	ret
.L15:
	movq 8(%rdi), %rdi
	movq $0, %rax
	call printf
	ret

	.data
.S0:
	.string "\n"
.S1:
	.string "can't access field name: expected an instance of struct Kid"
.S2:
	.string "\n"
.S3:
	.string "can't access field age: expected an instance of struct Kid"
bob:
	.quad 1
.S4:
	.string "no overload of function Kid matches the given argument types"
.S5:
	.string "no overload of function Kid matches the given argument types"
.S6:
	.string "Bobby"
x:
	.quad 1
.S7:
	.string "can't assign to field age : expected a value of type Int64"
.S8:
	.string "can't assign to field age: expected an instance of struct Kid"
.S9:
	.string "uninitialized variable bob"
.S10:
	.string "\n"
.S11:
	.string "uninitialized variable x"
.S12:
	.string "runtime error: %s\n"
.S13:
	.string "%lld"
.S14:
	.string "true"
.S15:
	.string "false"
.S16:
	.string "nothing"
.S17:
	.string "can't print value"
