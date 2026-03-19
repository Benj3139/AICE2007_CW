	.text
	.globl	factorial
factorial:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$120, %rsp
	movq	%rdi, -8(%rbp)
	leaq	-16(%rbp), %rax
	movq	%rax, -16(%rbp)
	leaq	-24(%rbp), %rax
	movq	%rax, -24(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rbx
	movq	%rax, (%rbx)
	movq	$1, %rax
	movq	-24(%rbp), %rbx
	movq	%rax, (%rbx)
	jmp	factorial.start
	.text
factorial.start:
	movq	-16(%rbp), %rax
	movq	(%rax), -48(%rbp)
	movq	-48(%rbp), %rax
	movq	$0, %rbx
	cmpq	%rbx, %rax
	setg	%r8b
	movq	%r8 , %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	cmpq	$0, %rax
	jne	factorial.then
	jmp	factorial.end
	.text
factorial.then:
	movq	-24(%rbp), %rax
	movq	(%rax), -64(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), -72(%rbp)
	movq	-64(%rbp), %rax
	movq	-72(%rbp), %rbx
	imulq	%rbx, %rax
	movq	%rax, -80(%rbp)
	movq	-80(%rbp), %rax
	movq	-24(%rbp), %rbx
	movq	%rax, (%rbx)
	movq	-16(%rbp), %rax
	movq	(%rax), -96(%rbp)
	movq	-96(%rbp), %rax
	movq	$1, %rbx
	subq	%rbx, %rax
	movq	%rax, -104(%rbp)
	movq	-104(%rbp), %rax
	movq	-16(%rbp), %rbx
	movq	%rax, (%rbx)
	jmp	factorial.start
	.text
factorial.end:
	movq	-24(%rbp), %rax
	movq	(%rax), -120(%rbp)
	movq	-120(%rbp), %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	.text
	.globl	factorial2
factorial2:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$120, %rsp
	movq	%rdi, -8(%rbp)
	leaq	-16(%rbp), %rax
	movq	%rax, -16(%rbp)
	leaq	-24(%rbp), %rax
	movq	%rax, -24(%rbp)
	movq	-8(%rbp), %rax
	movq	-16(%rbp), %rbx
	movq	%rax, (%rbx)
	movq	$1, %rax
	movq	-24(%rbp), %rbx
	movq	%rax, (%rbx)
	jmp	factorial2.start
	.text
factorial2.start:
	movq	-16(%rbp), %rax
	movq	(%rax), -48(%rbp)
	movq	-48(%rbp), %rax
	movq	$0, %rbx
	cmpq	%rbx, %rax
	setg	%r8b
	movq	%r8 , %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	cmpq	$0, %rax
	jne	factorial2.then
	jmp	factorial2.end
	.text
factorial2.then:
	movq	-24(%rbp), %rax
	movq	(%rax), -64(%rbp)
	movq	-16(%rbp), %rax
	movq	(%rax), -72(%rbp)
	movq	-64(%rbp), %rax
	movq	-72(%rbp), %rbx
	imulq	%rbx, %rax
	movq	%rax, -80(%rbp)
	movq	-80(%rbp), %rax
	movq	-24(%rbp), %rbx
	movq	%rax, (%rbx)
	movq	-16(%rbp), %rax
	movq	(%rax), -96(%rbp)
	movq	-96(%rbp), %rax
	movq	$1, %rbx
	subq	%rbx, %rax
	movq	%rax, -104(%rbp)
	movq	-104(%rbp), %rax
	movq	-16(%rbp), %rbx
	movq	%rax, (%rbx)
	jmp	factorial2.start
	.text
factorial2.end:
	movq	-24(%rbp), %rax
	movq	(%rax), -120(%rbp)
	movq	-120(%rbp), %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	
	.text
	.globl	main
main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$56, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	leaq	-24(%rbp), %rax
	movq	%rax, -24(%rbp)
	movq	$0, %rax
	movq	-24(%rbp), %rbx
	movq	%rax, (%rbx)
	movq	$5, %rdi
	callq	factorial
	movq	%rax, -40(%rbp)
	movq	$5, %rdi
	callq	factorial2
	movq	%rax, -48(%rbp)
	movq	-40(%rbp), %rax
	movq	-48(%rbp), %rbx
	addq	%rbx, %rax
	movq	%rax, -56(%rbp)
	movq	-56(%rbp), %rax
	movq	%rbp, %rsp
	popq	%rbp
	retq	