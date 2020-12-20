    .globl main
    .text
main:
    pushq %rbp
    movq %rsp, %rbp


	movq $.S0, %rdi
	movq $0, %rax
	call printf

    movq $1, %rdi
    call exit


    leave
    movq $0, %rax
    ret

.S0:
	.string "runtime error\n"
