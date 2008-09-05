
.global irq0
.global irq16
.global thread_start
.global do_context_switch

.section .data
do_context_switch:
.long 0

.section .text

.extern thread_schedule
.extern thread_exit
.extern caml_young_end
.extern caml_young_limit

irq0:
	push %eax
	movl $1, do_context_switch
	movl caml_young_end, %eax
	movl %eax, caml_young_limit
	mov $0x20, %al
	outb $0x20
	pop %eax
	iret

irq16: # invoked via software interrupt
	pusha
	push %esp
	call thread_schedule
	mov %eax, %esp
	popa
	iret

thread_start:
	push %ecx
	call *%eax
	push %eax
	call thread_exit
