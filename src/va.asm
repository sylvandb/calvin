; ======================================================================== ;
;
; Copyright (c) 1997  Paul Vojta
;
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to
; deal in the Software without restriction, including without limitation the
; rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
; sell copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
;
; The above copyright notice and this permission notice shall be included in
; all copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
; PAUL VOJTA BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;
; ======================================================================== ;

;	VA	Assembly portion of VI.

	public	_setjmp
	public	_raise_err
	public	_beep
	public	_beep2
	public	_chirp
	public	_ting
	public	_farmemmove
	public	_farmemchr
	public	_farmemichr
	public	_farmemrchr
	public	_farmemrichr
	public	_farmemcmp
	public	_farmemicmp
	public	_farread
	public	_farwrite
	public	_farmalloc
	public	_farfree

DGROUP	group	_DATA
_DATA	segment word public 'DATA'
	extrn	_errno:word
	extrn	_errbuf:word
_DATA	ends

_TEXT	segment byte public 'CODE'
	assume	cs:_TEXT, ds:DGROUP, ss:DGROUP
_setjmp:pop	cx		;return address
	pop	bx		;buffer address
	push	bx
	mov	[bx],cx
	mov	[bx+2],sp
	mov	[bx+4],bp
	mov	[bx+6],si
	mov	[bx+8],di
	xor	ax,ax
	jmp	cx		;return 0

;	flush keyboard buffer and jump to marked error location

_raise_err:
	mov	bx,offset _errbuf
	mov	ah,1		;check for character waiting
	int	16h
	jz	re2		;if no character waiting
	mov	ah,0
	int	16h
	jmp	_raise_err

re2:	mov	sp,[bx+2]	;jump to prearranged location
	mov	bp,[bx+4]
	mov	si,[bx+6]
	mov	di,[bx+8]
	mov	ax,1		;return 1
	jmp	[bx]

;	TING	Beep the speaker just barely (margin bell).
;	CHIRP	Beep the speaker in an audibly pleasant manner.
;	BEEP	Beep the speaker in a slightly less pleasant manner.
;	BEEP2	Beep the speaker with a distinctive two-tone.

;	I/O ports.

timer	equ	40h
port_b	equ	61h

_ting:	mov	bx,1000		;timer count (inverse frequency)
	mov	cx,120		;loop count (duration)
	jmp	short bp1

_chirp:	mov	bx,3000		;timer count
	mov	cx,120		;loop count
	jmp	short bp1

_beep2:	call	_chirp		;first do a normal beep
	mov	bx,2600		;timer count
	mov	cx,120
	jmp	short bp1	;then do a normal beep

_beep:	mov	bx,2200		;timer count
	mov	cx,250

;	Main beep routine.  bx and cx should be set up by now.

bp1:	mov	al,0b6h		;010110110B:  select timer 2, lsb, msb, binary
	out	timer+3,al
	xchg	ax,bx		;timer count
	out	timer+2,al
	mov	al,ah
	out	timer+2,al
	in	al,port_b	;set speaker bit
	or	al,3
	out	port_b,al
	mov	al,-1
bp2:	mov	ah,al
	in	al,timer+2	;least significant byte
	nop
	nop
	in	al,timer+2	;most significant byte
	cmp	al,ah
	jbe	bp2		;if no change
	loop	bp2		;count one more

	in	al,port_b	;turn it off
	and	al,0fdh
	out	port_b,al
	ret

;	FARMEMMOVE	Move memory, correctly.

_farmemmove:
	mov	bx,sp
	push	si
	push	di
	mov	cx,[bx+10]	;byte count
	jcxz	fm5		;if nothing to do
	les	di,[bx+2]	;destination
	mov	ax,es
	cmp	ax,[bx+8]	;compare segment registers
	lds	si,[bx+6]	;source
	jne	fm3		;if forward move OK
	cmp	si,di
	jae	fm3		;if OK

;	Backward move.

	std
	add	si,cx
	add	di,cx
	dec	si
	dec	di
	test	si,1
	jz	fm1		;if word aligned
	movsb
	dec	cx
fm1:	dec	si
	dec	di
	shr	cx,1
	rep	movsw
	jnc	fm2		;if no odd byte
	inc	si
	inc	di
	movsb
fm2:	cld
	jmp	short fm5

;	Forward move.

fm3:	test	si,1
	jz	fm4		;if word aligned
	movsb
	dec	cx
fm4:	shr	cx,1
	rep	movsw
	jnc	fm5		;if no odd byte
	movsb

;	Done.

fm5:	pop	di
	pop	si
	push	ss
	pop	ds
	ret

;	FARMEMCHR - Far equivalent of memchr()

_farmemchr:
	pop	bx
	pop	dx		;get arguments
	pop	es
	pop	ax
	pop	cx
	push	cx
	push	ax
	push	es
	push	dx
	push	bx
mc1:	push	di
	mov	di,dx
	jcxz	mc2		;if not found
	repnz	scasb
	jnz	mc2		;if not found
	dec	di
	xchg	ax,di
	mov	dx,es
	pop	di
	ret

mc2:	pop	di
	xor	ax,ax		;return NULL
	xor	dx,dx
	ret

;	FARMEMICHR - Far equivalent of memichr()

_farmemichr:
	mov	bx,sp
	mov	al,[bx+6]
	or	al,20h
	sub	al,'a'
	cmp	al,'z'-'a'
	ja	_farmemchr	;if not letter
	push	si
	mov	cx,[bx+8]
	mov	ah,[bx+6]
	or	ah,20h
	lds	si,[bx+2]
	jcxz	mc4		;if not found
mc3:	lodsb
	or	al,20h
	cmp	al,ah
	loopnz	mc3
	jnz	mc4		;if not found
	dec	si
	mov	ax,si
	mov	dx,ds
	jmp	short mc5

mc4:	xor	ax,ax		;not found:  return 0
	xor	dx,dx
mc5:	pop	si
	push	ss
	pop	ds
	ret

;	FARMEMRCHR - Far equivalent of memrchr()

_farmemrchr:
	pop	bx
	pop	dx		;get arguments
	pop	es
	pop	ax
	pop	cx
	push	cx
	push	ax
	push	es
	push	dx
	push	bx
	push	di
	mov	di,dx
	jcxz	mc2		;if not found
	add	di,cx
	dec	di
	std
	repnz	scasb
	cld
	jnz	mc2		;if not found
	inc	di
	xchg	ax,di
	mov	dx,es
	pop	di
	ret

;	FARMEMRICHR - Far equivalent of memrichr()

_farmemrichr:
	mov	bx,sp
	mov	al,[bx+6]
	or	al,20h
	sub	al,'a'
	cmp	al,'z'-'a'
	ja	_farmemrchr	;if not letter
	push	si
	mov	cx,[bx+8]
	mov	ah,[bx+6]
	or	ah,20h
	lds	si,[bx+2]
	jcxz	mc4		;if not found
	add	si,cx
	dec	si
	std
mc6:	lodsb
	or	al,20h
	cmp	al,ah
	loopnz	mc6
	cld
	jnz	mc4		;if not found
	inc	si
	mov	ax,si
	mov	dx,ds
	jmp	mc5

;	FARMEMCMP - Far equivalent of memcmp()

_farmemcmp:
	mov	bx,sp
	push	si
	push	di
	mov	cx,[bx+10]
	les	di,[bx+6]
	lds	si,[bx+2]
	xor	ax,ax
	repz	cmpsb
	jz	mc7		;if equal
	mov	al,[si-1]
	sub	al,es:[di-1]
	cbw
mc7:	pop	di
	pop	si
	push	ss
	pop	ds
	ret

;	FARMEMICMP - Far equivalent of memicmp()
;	Note:  we only care if the answer = or <> 0, so it doesn't matter
;	if 'a' is counted as greater than 'Z'.

_farmemicmp:
	mov	bx,sp
	push	si
	push	di
	mov	cx,[bx+10]
	les	di,[bx+6]
	dec	di
	lds	si,[bx+2]
	jcxz	mc9		;if null length
mc8:	lodsb
	inc	di
	xor	al,es:[di]
	loopz	mc8		;if OK
	cmp	al,20h
	jne	mc10		;if no match
	dec	si
	lodsb
	or	al,20h
	sub	al,'a'
	cmp	al,'z'-'a'
	ja	mc10		;if no match
	inc	cx
	loop	mc8		;it matched
mc9:	xor	ax,ax		;return zero if entire string matched
	jmp	mc7

mc10:	dec	si
	lodsb
	sub	al,es:[di]
	cbw
	jmp	mc7		;return the difference (maybe zero)

;	FARREAD	Read into far memory.

_farread:
	pop	ax		;return address
	pop	bx		;file handle
	pop	dx		;buffer address
	pop	ds
	pop	cx		;byte count
	push	cx
	push	ds
	push	dx
	push	bx
	push	ax

	mov	ah,3fh		;DOS read function
	jmp	short fw1

;	FARWRITE - Write from far memory.

_farwrite:
	pop	ax		;return address
	pop	bx		;file handle
	pop	dx		;buffer address
	pop	ds
	pop	cx		;byte count
	push	cx
	push	ds
	push	dx
	push	bx
	push	ax

	mov	ah,40h		;DOS write function
fw1:	int	21h
	push	ss		;restore ds
	pop	ds
	jnc	fw2		;if OK
	mov	_errno,ax
	mov	ax,-1
fw2:	ret

;	FARMALLOC - Allocate far memory.

_farmalloc:
	pop	ax
	pop	bx		;get length (we only take words...)
	push	bx
	push	ax

	mov	ah,48h		;allocate memory
	dec	bx
	jl	fma1		;if length too small
	mov	cl,4
	shr	bx,cl
	inc	bx
	int	21h		;call to DOS
	jnc	fma2		;if no error
fma1:	xor	ax,ax		;return NULL for error
fma2:	xchg	ax,dx		;return seg:0
	xor	ax,ax
	ret

;	FARFREE	Undo the above.

_farfree:
	mov	ah,49h		;free allocated memory
	mov	bx,sp
	mov	es,[bx+4]	;the argument
	int	21h
	ret

_TEXT	ends
	end
