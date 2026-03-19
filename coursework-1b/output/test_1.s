	.text
	.globl	main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	leaq	-24(%rbp), %rax
	movq	%rax, -24(%rbp)
	movq	$17, %rax
	movq	-24(%rbp), %rbx
	movq	%rax, (%rbx)
	leaq	-40(%rbp), %rax
	movq	%rax, -40(%rbp)
	movq	-24(%rbp), %rax
	movq	-40(%rbp), %rbx
	movq	%rax, (%rbx)
	movq	-40(%rbp), %rax
	movq	(%rax), -56(%rbp)
	movq	-56(%rbp), %rax
	movq	(%rax), -64(%rbp)
	movq	-64(%rbp), %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	