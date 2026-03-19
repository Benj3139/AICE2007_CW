	.text
	.globl	foo
foo:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	leaq	-24(%rbp), %rax
	movq	%rax, -24(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rbx
	addq	%rbx, %rax
	movq	%rax, -32(%rbp)
	movq	-32(%rbp), %rax
	movq	-24(%rbp), %rbx
	movq	%rax, (%rbx)
	movq	-24(%rbp), %rax
	movq	(%rax), -48(%rbp)
	movq	-48(%rbp), %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	.text
	.globl	main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$40, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	leaq	foo(%rip), %rdi
	callq	ll_callback
	movq	%rax, -24(%rbp)
	movq	-24(%rbp), %rdi
	callq	ll_ltoa
	movq	%rax, -32(%rbp)
	movq	-32(%rbp), %rdi
	callq	ll_puts
	movq	$0, %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	