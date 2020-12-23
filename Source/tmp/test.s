	.text
	.globl	main
main:
	pushq %rbp
	movq %rsp, %rbp
	movq $8, %rdi
	call malloc
	movq $0, 0(%rax)
	movq %rax, nothing
	movq nothing, %rax
	cmpq $1, %rax
	jne .L3
	movq $.S3, %rdi
	call error
.L3:
	pushq %rax
	movq $16, %rdi
	call malloc
	movq $3, 0(%rax)
	movq $.S2, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call malloc
	movq $2, 0(%rax)
	movq $13, 8(%rax)
	pushq %rax
	movq 0(%rsp), %rax
	movq 0(%rax), %rax
	cmpq $2, %rax
	je .L1
	movq $.S1, %rdi
	call error
.L1:
	movq 8(%rsp), %rax
	movq 0(%rax), %rax
	movq 16(%rsp), %rax
	movq 0(%rax), %rax
	cmpq $0, %rax
	je .L2
	movq $.S0, %rdi
	call error
.L2:
	movq $32, %rdi
	call malloc
	movq $4, 0(%rax)
	movq 0(%rsp), %r8
	movq %r8, 8(%rax)
	movq 8(%rsp), %r8
	movq %r8, 16(%rax)
	movq 16(%rsp), %r8
	movq %r8, 24(%rax)
	jmp .L0
.L0:
	addq $24, %rsp
	movq %rax, mike
	addq $0, %rsp
	movq nothing, %rax
	cmpq $1, %rax
	jne .L7
	movq $.S7, %rdi
	call error
.L7:
	pushq %rax
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
	je .L5
	movq $.S5, %rdi
	call error
.L5:
	movq 8(%rsp), %rax
	movq 0(%rax), %rax
	movq 16(%rsp), %rax
	movq 0(%rax), %rax
	cmpq $0, %rax
	je .L6
	movq $.S4, %rdi
	call error
.L6:
	movq $32, %rdi
	call malloc
	movq $4, 0(%rax)
	movq 0(%rsp), %r8
	movq %r8, 8(%rax)
	movq 8(%rsp), %r8
	movq %r8, 16(%rax)
	movq 16(%rsp), %r8
	movq %r8, 24(%rax)
	jmp .L4
.L4:
	addq $24, %rsp
	movq %rax, mike2
	addq $0, %rsp
	movq mike, %rax
	cmpq $1, %rax
	jne .L13
	movq $.S13, %rdi
	call error
.L13:
	pushq %rax
	movq $16, %rdi
	call malloc
	movq $2, 0(%rax)
	movq $40, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call malloc
	movq $3, 0(%rax)
	movq $.S12, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call malloc
	movq $2, 0(%rax)
	movq $13, 8(%rax)
	pushq %rax
	movq 0(%rsp), %rax
	movq 0(%rax), %rax
	cmpq $2, %rax
	je .L9
	movq $.S11, %rdi
	call error
.L9:
	movq 8(%rsp), %rax
	movq 0(%rax), %rax
	cmpq $3, %rax
	je .L10
	movq $.S10, %rdi
	call error
.L10:
	movq 16(%rsp), %rax
	movq 0(%rax), %rax
	cmpq $2, %rax
	je .L11
	movq $.S9, %rdi
	call error
.L11:
	movq 24(%rsp), %rax
	movq 0(%rax), %rax
	cmpq $4, %rax
	je .L12
	movq $.S8, %rdi
	call error
.L12:
	movq $40, %rdi
	call malloc
	movq $5, 0(%rax)
	movq 0(%rsp), %r8
	movq %r8, 8(%rax)
	movq 8(%rsp), %r8
	movq %r8, 16(%rax)
	movq 16(%rsp), %r8
	movq %r8, 24(%rax)
	movq 24(%rsp), %r8
	movq %r8, 32(%rax)
	jmp .L8
.L8:
	addq $32, %rsp
	movq %rax, bob
	addq $0, %rsp
	movq mike2, %rax
	cmpq $1, %rax
	jne .L19
	movq $.S19, %rdi
	call error
.L19:
	pushq %rax
	movq $16, %rdi
	call malloc
	movq $2, 0(%rax)
	movq $40, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call malloc
	movq $3, 0(%rax)
	movq $.S18, 8(%rax)
	pushq %rax
	movq $16, %rdi
	call malloc
	movq $2, 0(%rax)
	movq $13, 8(%rax)
	pushq %rax
	movq 0(%rsp), %rax
	movq 0(%rax), %rax
	cmpq $2, %rax
	je .L15
	movq $.S17, %rdi
	call error
.L15:
	movq 8(%rsp), %rax
	movq 0(%rax), %rax
	cmpq $3, %rax
	je .L16
	movq $.S16, %rdi
	call error
.L16:
	movq 16(%rsp), %rax
	movq 0(%rax), %rax
	cmpq $2, %rax
	je .L17
	movq $.S15, %rdi
	call error
.L17:
	movq 24(%rsp), %rax
	movq 0(%rax), %rax
	cmpq $4, %rax
	je .L18
	movq $.S14, %rdi
	call error
.L18:
	movq $40, %rdi
	call malloc
	movq $5, 0(%rax)
	movq 0(%rsp), %r8
	movq %r8, 8(%rax)
	movq 8(%rsp), %r8
	movq %r8, 16(%rax)
	movq 16(%rsp), %r8
	movq %r8, 24(%rax)
	movq 24(%rsp), %r8
	movq %r8, 32(%rax)
	jmp .L14
.L14:
	addq $32, %rsp
	movq %rax, bob2
	addq $0, %rsp
	movq bob, %rax
	cmpq $1, %rax
	jne .L21
	movq $.S22, %rdi
	call error
.L21:
	pushq %rax
	movq bob2, %rax
	cmpq $1, %rax
	jne .L20
	movq $.S21, %rdi
	call error
.L20:
	popq %rdi
	movq %rax, %rsi
	call equal
	movq %rax, %rdi
	call print
	movq $16, %rdi
	call malloc
	movq $3, 0(%rax)
	movq $.S20, 8(%rax)
	movq %rax, %rdi
	call print
	movq nothing, %rax
	addq $0, %rsp
	leave
	movq $0, %rax
	ret

error:
	pushq %rbp
	movq %rsp, %rbp
	movq %rdi, %rsi
	movq $.S23, %rdi
	movq $0, %rax
	call printf
	movq $1, %rdi
	call exit

pow:
	pushq %rbp
	movq %rsp, %rbp
	movq $1, %rax
	jmp .L24
.L22:
	testq $1, %rsi
	jz .L23
	imulq %rdi, %rax
.L23:
	imulq %rdi, %rdi
	shrq $1, %rsi
.L24:
	testq %rsi, %rsi
	jnz .L22
	leave
	ret

print:
	pushq %rbp
	movq %rsp, %rbp
	movq 0(%rdi), %rcx
	cmpq $0, %rcx
	je .L25
	cmpq $1, %rcx
	je .L26
	cmpq $2, %rcx
	je .L27
	cmpq $3, %rcx
	je .L28
	movq $.S28, %rdi
	call error
.L25:
	movq $.S27, %rdi
	movq $0, %rax
	call printf
	leave
	ret
.L26:
	movq 8(%rdi), %rax
	movq $.S25, %rdi
	testq %rax, %rax
	jnz .L29
	movq $.S26, %rdi
.L29:
	movq $0, %rax
	call printf
	leave
	ret
.L27:
	movq 8(%rdi), %rsi
	movq $.S24, %rdi
	movq $0, %rax
	call printf
	leave
	ret
.L28:
	movq 8(%rdi), %rdi
	movq $0, %rax
	call printf
	leave
	ret

equal:
	pushq %rbp
	movq %rsp, %rbp
	movq 0(%rdi), %rcx
	movq 0(%rsi), %rdx
	cmpq %rcx, %rdx
	jne .L31
	cmpq $0, %rcx
	je .L30
	cmpq $1, %rcx
	je .L32
	cmpq $2, %rcx
	je .L33
	cmpq $3, %rcx
	je .L34
	jmp .L35
.L32:
.L33:
	movq 8(%rdi), %rdi
	movq 8(%rsi), %rsi
	cmpq %rdi, %rsi
	je .L30
	jmp .L31
.L34:
	movq 8(%rdi), %rdi
	movq 8(%rsi), %rsi
.L39:
	movzbq 0(%rdi), %r8
	movzbq 0(%rsi), %r9
	cmpq %r8, %r9
	jne .L31
	testq %r8, %r8
	jz .L30
	incq %rdi
	incq %rsi
	jmp .L39
.L35:
	movq $struct_m_table, %r8
	movb -4(%r8,%rcx,1), %r8b
	testb %r8b, %r8b
	jz .L36
	cmpq %rdi, %rsi
	je .L30
	jmp .L31
.L36:
	movq $struct_fc_table, %r8
	movq -32(%r8,%rcx,8), %r8
	pushq %rsi
	pushq %rdi
	pushq %r8
	pushq $1
	jmp .L38
.L37:
	movq 16(%rsp), %rdi
	movq 24(%rsp), %rsi
	movq 0(%rdi,%r8,8), %rdi
	movq 0(%rsi,%r8,8), %rsi
	call equal
	movq 8(%rax), %rax
	testq %rax, %rax
	jz .L31
	incq 0(%rsp)
.L38:
	movq 0(%rsp), %r8
	movq 8(%rsp), %r9
	cmpq %r8, %r9
	jge .L37
	jmp .L30
.L30:
	movq $16, %rdi
	call malloc
	movq $1, 0(%rax)
	movq $1, 8(%rax)
	leave
	ret
.L31:
	movq $16, %rdi
	call malloc
	movq $1, 0(%rax)
	movq $0, 8(%rax)
	leave
	ret

	.data
struct_m_table:
	.byte 0, 0

struct_fc_table:
	.quad 3, 4

nothing:
	.quad 0
mike:
	.quad 1
.S0:
	.string "no overload of function Kid matches the given argument types"
.S1:
	.string "no overload of function Kid matches the given argument types"
.S2:
	.string "Mike"
.S3:
	.string "uninitialized variable nothing"
mike2:
	.quad 1
.S4:
	.string "no overload of function Kid matches the given argument types"
.S5:
	.string "no overload of function Kid matches the given argument types"
.S6:
	.string "Mike"
.S7:
	.string "uninitialized variable nothing"
bob:
	.quad 1
.S8:
	.string "no overload of function Student matches the given argument types"
.S9:
	.string "no overload of function Student matches the given argument types"
.S10:
	.string "no overload of function Student matches the given argument types"
.S11:
	.string "no overload of function Student matches the given argument types"
.S12:
	.string "Bobby"
.S13:
	.string "uninitialized variable mike"
bob2:
	.quad 1
.S14:
	.string "no overload of function Student matches the given argument types"
.S15:
	.string "no overload of function Student matches the given argument types"
.S16:
	.string "no overload of function Student matches the given argument types"
.S17:
	.string "no overload of function Student matches the given argument types"
.S18:
	.string "Bobby"
.S19:
	.string "uninitialized variable mike2"
.S20:
	.string "\n"
.S21:
	.string "uninitialized variable bob2"
.S22:
	.string "uninitialized variable bob"
.S23:
	.string "runtime error: %s\n"
.S24:
	.string "%lld"
.S25:
	.string "true"
.S26:
	.string "false"
.S27:
	.string "nothing"
.S28:
	.string "can't print value"
