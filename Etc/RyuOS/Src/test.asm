[org 0]
[bits 16]
	jmp 0x07c0:start
start:
	mov ax, 0xb800
	mov es, ax
	mov byte[es:0], 'a'
	mov byte[es:1], 0x07
	jmp $

	times 510 - ($ - $$) db 0
	dw 0xAA55