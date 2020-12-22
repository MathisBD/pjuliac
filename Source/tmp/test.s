	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $8, %rdi
	call malloc
	movq $0, 0(%rax)
	movq %rax, nothing
	movq bob, %rax
	cmpq $1, %rax
	jne .L2
	movq $.S2, %rdi
	call error
.L2:
	movq 0(%rax), %rcx
	cmpq $4, %rcx
	je .L0
	movq $.S1, %rdi
	call error
.L0:
	pushq %rax
	movq $16, %rdi
	call malloc
	movq $2, 0(%rax)
	movq $13, 8(%rax)
	movq 0(%rax), %rcx
	cmpq $2, %rcx
	je .L1
	movq $.S0, %rdi
	call error
.L1:
	popq %r8
	movq %rax, 8(%r8)
	addq $0, %rsp
	movq $16, %rdi
	call malloc
	movq $3, 0(%rax)
	movq $.S5, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call malloc
	movq $2, 0(%rax)
	movq $13, 8(%rax)
	pushq %rax
	movq 0(%rsp), %rax
	movq 0(%rax), %rax
	cmpq $2, %rax
	je .L4
	movq $.S4, %rdi
	call error
.L4:
	movq 8(%rsp), %rax
	movq 0(%rax), %rax
	cmpq $3, %rax
	je .L5
	movq $.S3, %rdi
	call error
.L5:
	movq $24, %rdi
	call malloc
	movq $4, 0(%rax)
	movq 0(%rsp), %r8
	movq %r8, 8(%rax)
	movq 8(%rsp), %r8
	movq %r8, 16(%rax)
	jmp .L3
.L3:
	addq $16, %rsp
	movq %rax, bob
	addq $0, %rsp
	leave
	movq $0, %rax
	ret

error:
	movq %rdi, %rsi
	movq $.S6, %rdi
	movq $0, %rax
	call printf
	movq $1, %rdi
	call exit

pow:
	movq $1, %rax
	jmp .L8
.L6:
	testq $1, %rsi
	jz .L7
	imulq %rdi, %rax
.L7:
	imulq %rdi, %rdi
	shrq $1, %rsi
.L8:
	testq %rsi, %rsi
	jnz .L6
	ret

print:
	movq 0(%rdi), %rcx
	cmpq $0, %rcx
	je .L9
	cmpq $1, %rcx
	je .L10
	cmpq $2, %rcx
	je .L11
	cmpq $3, %rcx
	je .L12
	movq $.S11, %rdi
	call error
.L9:
	movq $.S10, %rdi
	movq $0, %rax
	call printf
	ret
.L10:
	movq 8(%rdi), %rax
	movq $.S8, %rdi
	testq %rax, %rax
	jnz .L13
	movq $.S9, %rdi
.L13:
	movq $0, %rax
	call printf
	ret
.L11:
	movq 8(%rdi), %rsi
	movq $.S7, %rdi
	movq $0, %rax
	call printf
	ret
.L12:
	movq 8(%rdi), %rdi
	movq $0, %rax
	call printf
	ret

equal:
	movq 0(%rdi), %rcx
	movq 0(%rsi), %rdx
	cmpq %rcx, %rdx
	jne .L15
	cmpq $0, %rcx
	je .L14
	cmpq $1, %rcx
	je .L16
	cmpq $2, %rcx
	je .L17
	cmpq $3, %rcx
	je .L18
	jmp .L19
.L16:
.L17:
	movq 8(%rdi), %rdi
	movq 8(%rsi), %rsi
	cmpq %rdi, %rsi
	je .L14
	jmp .L15
.L18:
	movq 8(%rdi), %rdi
	movq 8(%rsi), %rsi
.L20:
	movzbq 0(%rdi), %r8
	movzbq 0(%rsi), %r9
	cmpq %r8, %r9
	jne .L15
	testq %r8, %r8
	jz .L14
	incq %rdi
	incq %rsi
	jmp .L20
.L19:
	movq $.S12, %rdi
	call error
.L14:
	movq $16, %rdi
	call malloc
	movq $1, 0(%rax)
	movq $1, 8(%rax)
	ret
.L15:
	movq $16, %rdi
	call malloc
	movq $1, 0(%rax)
	movq $0, 8(%rax)
	ret

	.data
struct_m_table:
	.byte 0, 0, 0, 0, 1, 0

struct_fc_table:
	.quad 0, 0, 0, 0, 2, 3

nothing:
	.quad 0
.S0:
	.string "can't assign to field age : expected a value of type Int64"
.S1:
	.string "can't assign to field age: expected an instance of struct Kid"
.S2:
	.string "uninitialized variable bob"
bob:
	.quad 1
.S3:
	.string "no overload of function Kid matches the given argument types"
.S4:
	.string "no overload of function Kid matches the given argument types"
.S5:
	.string "Bobby"
.S6:
	.string "runtime error: %s\n"
.S7:
	.string "%lld"
.S8:
	.string "true"
.S9:
	.string "false"
.S10:
	.string "nothing"
.S11:
	.string "can't print value"
.S12:
	.string "equality not implemented"
