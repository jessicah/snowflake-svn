
.global _start

.section .text

_start:
	mov $1, %eax
	mov $42, %ebx
	int $0x80
