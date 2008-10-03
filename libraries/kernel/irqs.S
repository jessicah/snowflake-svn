
.global irq0
.global irq1
.global irq2
.global irq3
.global irq4
.global irq5
.global irq6
.global irq7
.global irq8
.global irq9
.global irq10
.global irq11
.global irq12
.global irq13
.global irq14
.global irq15

.extern signal_handlers
.extern thread_schedule
.extern thread_exit
.extern caml_young_limit
.extern caml_young_end

#define IRQ(a,b) 							\
irq##a:												\
	pusha;												\
	movl $signal_handlers, %eax;	\
	push $##a;										\
	call *##b##(%eax);						\
	addl $4, %esp;								\
	popa;												\
	iret

irq0:
	pusha
	movl caml_young_end, %eax
	movl %eax, caml_young_limit
	movl $signal_handlers, %eax
	push $0
	call *0(%eax)
	addl $4, %esp
	call thread_yield
	popa
	iret

.global _thread_switch_stacks
_thread_switch_stacks:
	/* Create stack frame */
	push	%ebp
	movl	%esp, %ebp
	
	/* Save callee-save registers
	 * %ebp is saved by creating the stack frame above */
	push	%ebx
	push	%esi
	push	%edi
	
	/* Save current esp */
	movl	12(%ebp), %eax
	movl	%esp, (%eax)
	
	/* Switch to the new stack */
	movl	8(%ebp), %esp
	
	/* Restore callee-save registers */
	pop	%edi
	pop	%esi
	pop	%ebx
	
	/* Destroy stack frame & return */
	pop	%ebp
	ret

IRQ(1,4)
IRQ(2,8)
IRQ(3,12)
IRQ(4,16)
IRQ(5,20)
IRQ(6,24)
IRQ(7,28)
IRQ(8,32)
IRQ(9,36)
IRQ(10,40)
IRQ(11,44)
IRQ(12,48)
IRQ(13,52)
IRQ(14,56)
IRQ(15,60)
