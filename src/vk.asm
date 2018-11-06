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

	public	_inkey_c
	public	_inkey_i
	public	_inkey_1
	public	_inkey_wait
	public	_setdosesc

	extrn	_raise_err	:near
	extrn	_chirp		:near
	extrn	activpg		:byte

;	INKEY	Get the next keystroke.  This will always be either an ASCII
;		character, or one of the following special codes.

LF	equ	10
X_END	equ	256+0
X_CHAR	equ	256+1
X_WORD	equ	256+2
X_LINE	equ	256+3
CTRL_V	equ	256+4		;for internal use only
ESCAPE	equ	256+5		;ditto

_DATA	segment word public 'DATA'

;	Translation tables.

;	Conversions for command mode.

table_c	dw	4700h,'H'		;'Home' key
	dw	4800h,'k'		;Up arrow
	dw	4900h,''		;'PgUp'
	dw	4b00h,'h'		;Left arrow
	dw	4d00h,' '		;Right arrow
	dw	4f00h,'L'		;'End'
	dw	5000h,'j'		;Down arrow
	dw	5100h,''		;'PgDn'
	dw	5200h,'i'		;'Ins'
	dw	5300h,'x'		;'Del'
	dw	7300h,'b'		;Ctrl-left
	dw	7400h,'w'		;Ctrl-right
	dw	7500h,'D'		;Ctrl-End
	dw	7700h,12		;Ctrl-Home (= clear screen)
table_c_len	equ	14

;	Conversions for input modes.

table_i	dw	1c0dh,LF		;new line
	dw	0e08h,X_CHAR		;delete character
	dw	4b00h,X_CHAR		;delete character (numeric keypad)
	dw	5300h,X_CHAR		;delete character (other kbds)
	dw	1117h,X_WORD		;delete word
esc_u	dw	1615h,X_LINE		;delete line (ESC or ^U)
	dw	11bh,X_END		;end of input mode (this must not
					;precede the line delete entry)
	dw	4f00h,X_END		;'End' key
	dw	5200h,X_END		;'Ins' key
	dw	2f16h,CTRL_V		;Control-V
table_i_len	equ	10

;	Conversions for one-character modes.

table_1	dw	1c0dh,LF		;new line
	dw	2f16h,CTRL_V		;Control-V
	dw	11bh,ESCAPE		;Esc key
table_1_len	equ	3

;	Data area for saving a waited-for character.

sc_avail db	0			;if available
sc_value dw	0			;its value

_DATA	ends

;	These routines have three variants, each of which does different things.

;	INKEY_C	Get one key for command mode.  Special actions are:
;		Full mapping of keyboard keys.
;		Ctrl-V	==> nothing special
;		Ascii	==> return it
;		Other	==> chirp and start over

_inkey_c:
	mov	bx,offset table_c	;key mapping table
	mov	cx,table_c_len
	call	mapit		
	jc	_inkey_c	;if error
	ret

;	INKEY_I	Get one key, for input (line 24) or insert mode.  Special
;		actions are:
;		Partial mapping of keyboard keys.
;		Ctrl-V	==> process it
;		Ascii	==> return it
;		Other	==> chirp and start over

_inkey_i:
	mov	bx,offset table_i	;key mapping
	mov	cx,table_i_len
	call	mapit
	jc	_inkey_i	;if error
	ret

;	INKEY_1	Get one key, for 'r' or 'f' commands.  Special actions are:
;		esc key	==> call raise_err
;		CR key	==> '^J'
;		Ctrl-V	==> process it
;		Ascii	==> return it
;		Other	==> chirp and call raise_err

_inkey_1:
	mov	bx,offset table_1	;key mapping
	mov	cx,table_1_len
	call	mapit
	jc	raise		;if error
	ret

raise:	jmp	_raise_err



;	MAPIT	Perform keyboard mapping.  On entry, SI = table address,
;		CX = number of entries in the table to try.

mapit:	cmp	sc_avail,0
	jz	mp1a		;if no saved character
	mov	sc_avail,0	;clear the flag
	mov	ax,sc_value
	jmp	short mp1
mp1a:	mov	ah,0		;keyboard i/o:  get character
	int	16h
mp1:	cmp	ax,[bx]
	je	mp6		;if match
	add	bx,4
	loop	mp1

mp2:	cmp	al,0
	je	mp4		;if extended code
	mov	ah,0		;clear upper part

mp3:	clc			;normal return
	ret

mp4:	or	ax,ax
	jz	mp3		;if '\0'
	call	_chirp		;error beep
mp5:	stc
	ret

mp6:	mov	ax,[bx+2]	;match found:  get the new value
	cmp	ax,CTRL_V
	jb	mp3		;if not CTRL_V or ESCAPE
	ja	mp5		;if ESCAPE

;	Do Control-V processing

	mov	ah,8		;read current character/attribute
	mov	bh,activpg
	push	bp
	int	10h
	pop	bp
	xchg	ax,dx		;save it in DX
	mov	ax,10*256+'^'	;write '^' there
	mov	cx,1
	int	10h
	mov	ah,0		;read from keyboard
	int	16h		;(sc_avail not applicable here)
	push	ax		;stash the result
	xchg	ax,dx
	mov	ah,10		;put the old character back on the screen
	int	10h
	pop	ax		;unstash the result
	jmp	mp2		;return an ascii character

;	INKEY_WAIT	Wait for a keystroke, and then return.

_inkey_wait:
	cmp	sc_avail,0
	jnz	ip1		;if we already waited for it
	mov	ah,0		;read from keyboard
	int	16h
	inc	sc_avail
	mov	sc_value,ax
ip1:	ret

;	SETDOSESC	Set use of Ctrl-U/Escape for delete line.

_setdosesc:
	pop	bx,ax
	push	ax,bx		;al = argument
	test	al,1
	mov	ax,11bh		;true ==> Escape key
	jnz	sde1		;if true
	mov	ax,1615h	;false ==> Ctrl-U
sde1:	mov	esc_u,ax
	ret

	end
