
.global _exit

.section .text

_exit:
	mov $1, %eax
	mov $42, %ebx
	int $0x80
