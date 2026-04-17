	.data
	.globl	gint
gint:
	.quad	42
	.data
	.globl	v1
v1:
	.quad	0
	.quad	gint
	.data
	.globl	v2
v2:
	.quad	1
	.quad	0
	.data
	.globl	gstr
gstr:
	.asciz	"hello, world!"
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
	leaq	v2(%rip), %rax
	movq	$0, %rbx
	imulq	$16, %rbx
	addq	%rbx, %rax
	movq	%rax, -32(%rbp)
	movq	$5, %rax
	movq	-32(%rbp), %rbx
	movq	%rax, (%rbx)
	leaq	v2(%rip), -48(%rbp)
	movq	-48(%rbp), %rdi
	callq	foo
	movq	-32(%rbp), %rax
	movq	%rax, -64(%rbp)
	movq	-64(%rbp), %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	.text
	.globl	foo
foo:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	movq	-8(%rbp), %rax
	movq	$0, %rbx
	imulq	$16, %rbx
	addq	%rbx, %rax
	movq	%rax, -16(%rbp)
	movq	$6, %rax
	movq	-16(%rbp), %rbx
	movq	%rax, (%rbx)
	movq	-16(%rbp), %rax
	movq	%rax, -32(%rbp)
	movq	%rbp, %rsp
	popq	%rbp
	retq	