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

_TEXT	segment	byte public 'CODE'

	public	_invid
	public	_endvid
	public	_cshape
	public	_putcur
	public	_writes
	public	_mvup
	public	_mvdn
	public	_clearrows
	public	_clrel
	public	__IOERROR

	extrn	_farmalloc:near
	extrn	_farfree:near

TAB	equ	9
MAXROWS	equ	50

_TEXT	ends
_DATA	segment word public 'DATA'

	public	activpg
	public	_sys_errlist
	public	_sys_nerr
	public	__doserrno
	public	__dosErrorToSV
	public	_attrs

	extrn	_ROWS:word
	extrn	_COLS:word
	extrn	_window:word
	extrn	_pos2480:word
	extrn	_pos2580:word
	extrn	_botcurs:word
	extrn	_botlines:word
	extrn	_botattr:word
	extrn	_visual:byte
	extrn	_errno:word
	extrn	__psp:word
	extrn	_printable:word
	extrn	_tabstop:word

savloc	dw	0,0		;location of saved data
savcur	dw	0		;saved cursor position
activpg	db	0		;active display page
crspos	dw	450h,0		;offset, segment of cursor location
scaninf	db	7,0		;bottom scan line and (3=mono, 0=CGA)
vidseg	dw	0b800h		;segment of video card
old24	dw	0,0		;old INT 24 address

_DATA	ends
_TEXT	segment	byte public 'CODE'

;	INVID	Initialize video.  Also initializes the variables ROWS, COLS,
;		window, pos2480, pos2580, botcurs, botlines, visual.
;	void	invid(int vidcompat);

_invid:
	mov	ax,3524h	;get critical error interrupt vector
	int	21h
	mov	old24,bx
	mov	old24+2,es
	mov	ax,2524h	;set new value
	mov	ds,cs
	mov	dx,offset int24
	int	21h
	mov	ds,ss
	mov	word scaninf,7		;initialize some data
	mov	word vidseg,0b800h

;	Switch to alternate screen, if possible.

	pop	ax,cx		;cx = argument
	push	cx,ax

	push	bp
	mov	ah,15		;get current video state
	int	10h
	mov	dl,24		;dl = number of rows - 1
	cmp	al,7
	je	inv1		;if B/W text card
	push	ax
	push	cx
	mov	ax,1130h	;get number of rows
	int	10h
	pop	cx
	pop	ax
	cmp	dl,MAXROWS
	jb	inv2		;if short enough
	mov	dl,MAXROWS-1	;cut it off
	jmp	short inv2

inv1:	mov	word scaninf,30ch	;adjust data for mono card
	mov	word vidseg,0b000h

inv2:	push	ax
	mov	al,ah
	mov	byte _COLS,al	;save number of columns
	mul	dl
	mov	_pos2480,ax
	add	ax,_COLS
	mov	_pos2580,ax
	mov	_botcurs,ax
	mov	word _botlines,0
	mov	byte _window,dl
	inc	dx
	mov	byte _ROWS,dl
	pop	ax
	cmp	al,7
	je	inv4		;if mono card
	cmp	al,3
	jbe	inv3		;if CGA text mode

;	Miscellaneous screen:  switch modes.

	mov	ax,2		;ah=0, al=2--set b&w 80 char mode
	int	10h
	mov	ax,500h		;ah=5, al=0--set active page 0
	int	10h

;	CGA card:  switch to alternate screen

inv3:	or	cx,cx
	jnz	inv4		;if not allowed
	mov	ax,501h		;select active display page #1
	int	10h
	inc	byte activpg
	add	word crspos,2
	mov	ax,600h		;clear page
	mov	bh,7
	xor	cx,cx
	mov	dh,byte _window
	mov	dl,byte _COLS
	dec	dx
	int	10h
	jmp	short inv5

;	B/W card:  try to save screen

inv4:	dec	cx
	jg	inv5		;if not allowed
	mov	ax,_pos2580
	shl	ax,1
	xor	dx,dx
	push	dx
	push	ax
	call	_farmalloc	;get far memory for screen storage
	pop	bx,bx
	or	dx,dx
	jz	inv5		;if no room
	mov	savloc,ax
	mov	savloc+2,dx

;	Save screen

	cld
	push	si,di
	mov	cx,_pos2580
	mov	es,dx
	xchg	di,ax
	mov	ds,vidseg	;segment for mono card
	xor	si,si
	rep	movsw		;move the screenful of data
	pop	di,si
	mov	ds,ss		;restore ds
	mov	ah,3		;get cursor position
	mov	bh,0
	int	10h
	mov	savcur,dx

;	Return.

inv5:	mov	_visual,1
	mov	ax,word _attrs	;ATTRNORM
	mov	_botattr,ax
	pop	bp
	ret

;	ENDVID	End special video mode.  Restore the screen if possible.
;	void	endvid(void);

_endvid:push	bp
	cmp	byte activpg,0
	jnz	ev1		;if alternate display page
	cmp	word savloc+2,0
	jz	ev2		;if not saved

;	Recover saved page.

	cld
	push	si,di
	mov	cx,_pos2580
	mov	es,vidseg	;video segment
	xor	di,di
	lds	si,savloc
	rep	movsw		;copy screen back
	pop	di,si
	mov	ds,ss		;restore ds
	mov	ah,2		;set cursor position
	mov	bh,0		;display page
	mov	dx,savcur
	int	10h
	xor	ax,ax
	xchg	ax,savloc+2
	push	ax		;free the memory
	push	savloc
	call	_farfree
	pop	cx,cx
	jmp	short ev2	;done

;	Modes 2 or 3:  alternate page.

ev1:	mov	ax,500h		;select active display page 0
	int	10h
	dec	byte activpg	;restore initial values
	sub	word crspos,2

ev2:	mov	ax,2524h	;restore int 24 vector
	lds	dx,old24
	int	21h
	mov	ds,ss
	mov	_visual,0
	pop	bp
	ret

;	CSHAPE	Set cursor shape.  Parameter is 0 for full, 1 for half, or
;		2 for normal.
;	void	cshape(int i);

_TEXT	ends
_DATA	segment word public 'DATA'

toplin	db	0,3,6,0,6,11	;scan line table

_DATA	ends
_TEXT	segment	byte public 'CODE'

_cshape:mov	ah,1		;set cursor type
	mov	bx,sp
	mov	bx,[bx+2]	;parameter
	mov	cx,word scaninf	;cl = lower scan line; ch = 3 if mono, else 0
	add	bl,ch		;optionally add 3
	mov	ch,toplin[bx]	;upper scan line number
	int	10h
	ret

;	PUTCUR	Put cursor in given position.
;	void	putcur(int pos);

_putcur:mov	bx,sp
	mov	ax,[bx+2]	;argument
	mov	bh,byte _COLS
	div	bh		;split into row and column numbers
	xchg	al,ah
	xchg	ax,dx
	mov	ah,2		;set cursor position
	mov	bh,activpg	;page number
	int	10h		;call BIOS
	ret

;	WRITES	Write string to screen.  Returns position of next character.
;	int	writes(char far *str, int len, int base, int pos, char attr);

_TEXT	ends
_DATA	segment word public 'DATA'

colbase	dw	0		;position of start of this line

_DATA	ends
_TEXT	segment	byte public 'CODE'

_writes:cld
	push	bp		;save bp
	mov	bp,sp
	push	si,di		;save si, di
	les	si,[bp+4]	;si = string offset
	mov	di,[bp+8]	;di = string length
	mov	ax,[bp+10]	;ax = location of start of line
	mov	colbase,ax
	mov	ax,[bp+12]	;ax = beginning cursor position
	mov	bx,[bp+14]	;bl = attribute
	mov	bh,activpg	;bh = active display page

	mov	cl,byte _COLS	;put cursor into row,col format
	div	cl
	xchg	al,ah
	xchg	ax,dx
	call	upcur		;set cursor
	mov	cx,1		;this will stay in effect
	or	di,di

;	Main loop.

wr1:	jg	wr2		;if more to print

;	quit

	pop	di,si,bp
	mov	al,byte _COLS	;return position of next character
	mul	dh
	mov	dh,0
	add	ax,dx
	ret

wr2:	es lodsb		;get next character
	cmp	al,' '
	jb	wr4		;if control char.
	cmp	al,byte _printable
	jbe	wr7		;if printing char.

;	High character.  Print \xxx.

	push	ax
	mov	al,'\'
	call	co
	pop	ax
	test	al,40h
	push	ax
	mov	al,'2'
	jz	wr3		;if not \3xx
	inc	ax
wr3:	call	co
	pop	ax
	push	ax
	shr	ax,1
	shr	ax,1
	shr	ax,1
	and	ax,7
	add	al,'0'
	call	co
	pop	ax
	and	ax,7
	add	ax,9*256+'0'
	jmp	short wr7

;	Handle lower order characters.

wr4:	cmp	al,0
	je	wr5		;if '\0'
	cmp	al,TAB
	jne	wr6		;if not tab
	mov	al,byte _COLS
	mul	dh
	add	al,dl
	adc	ah,0
	sub	ax,colbase
	push	dx
	cwd
	mov	cx,_tabstop
	div	cx
	sub	cx,dx
	pop	dx
	add	dx,cx		;update cursor
	mov	ax,9*256+' '
	int	10h
	mov	cx,1		;restore cx
	jmp	short wr8

;	'\0'

wr5:	mov	al,'\'
	call	co
	mov	al,'0'
	jmp	short wr7

;	Control character.

wr6:	push	ax
	mov	ax,'^'
	call	co
	pop	ax
	add	ax,'A'-1

;	Normal printing character.

wr7:	mov	ah,9
	int	10h
	inc	dx		;update cursor
wr8:	cmp	dl,byte _COLS
	jb	wr9		;if not new row
	inc	dh
	sub	dl,byte _COLS
wr9:	push	es		;update the cursor
	les	bp,crspos
	mov	es:[bp],dx
	pop	es
	dec	di
	jmp	wr1		;loop back

;	CO	Character output and update cursor.
;	UPCUR	Update cursor.

co:	mov	ah,9		;print the char.
	int	10h
	inc	dl		;update cursor
upcur:	push	es
	les	bp,crspos
	mov	es:[bp],dx
	pop	es
	ret

;	CLEARROWS - Clear lines in screen.
;	void	clearrows(int startrow, int stoprow+1);

_clearrows:
	mov	ax,600h		;video function 6, move 0 lines (= clear)
	mov	bx,sp
	jmp	short mv1

;	MVUP	Move blocks up.
;	void	mvup(int startrow, int stoprow+1, int nrows);

_mvup:	mov	ah,6	;video function 6
	jmp	short mv

;	MVDN	Move blocks down.
;	void	mvdn(int startrow, int stoprow+1, int nrows);

_mvdn:	mov	ah,7	;video function 7
	jmp	short mv

;	MV	Common move routine.  Function code should be loaded into AH.
;		Calling procedure should not alter stack.

mv:	mov	bx,sp
	mov	al,[bx+6]	;number of lines to move
	cmp	al,0
	je	ret		;if no motion (prevent clearing)
mv1:	mov	ch,[bx+2]	;upper row number
	xor	cl,cl
	mov	dh,[bx+4]	;lower row number + 1
	dec	dh
	mov	dl,byte _COLS	;col. 79
	dec	dx
	mov	bh,dh		;check for excessive move
	sub	bh,ch		;number of lines
	cmp	bh,al
	jge	mv2		;if not moving too many lines
	xor	al,al		;clear the lines
mv2:	mov	bh,7		;fill attribute byte
	push	bp
	int	10h
	pop	bp
	ret

;	CLREL	Clear to end of line.  No arguments--uses cursor position.

_clrel:	push	bp
	mov	ah,3		;read cursor position (into dx)
	mov	bh,activpg	;active page number
	int	10h
	mov	ax,600h		;scroll active page up 0 lines
	mov	bh,7		;fill attribute
	mov	cx,dx
	mov	dl,byte _COLS	;col. 79
	dec	dx
	int	10h
	pop	bp
	ret

;	INT24	Critical error handler.

harderr	db	7fh

int24:	add	sp,8		;remove INT 24 stuff and AX from stack
	xchg	ax,di		;al = error code
	pop	bx
	pop	cx
	pop	dx
	pop	si
	pop	di
	pop	bp
	pop	ds
	pop	es
	mov	cs:harderr,al
	mov	ax,3
	cld
	stc
	retf	2

__IOERROR:
	pop	bx
	pop	ax		;argument
	push	bx		;restore return address
	or	ax,ax
	jge	I1		;if >= 0
	neg	ax
	cmp	ax,35
	jg	I2		;if > 35
	mov	bx,-1		;value of _doserrno
	jmp	I4

I1:	cmp	ax,59h
	jb	I3		;if in range
I2:	mov	ax,57h
I3:	xchg	ax,bx
	mov	al,__dosErrorToSV[bx]	;al = errno, bx = doserrno
I4:	cbw
	mov	__doserrno,bx
	mov	bx,7fh
	xchg	bl,cs:harderr
	cmp	bl,0dh
	jae	I5		;if no INT 24 error
	lea	ax,[bx+36]
I5:	mov	_errno,ax
	mov	ax,-1
	ret

_TEXT	ends
_DATA	segment word public 'DATA'

_sys_nerr dw	36+13

msg0	db	'Error 0',0
msg1	db	'Invalid function number',0
msg2	db	'No such file or directory',0
msg3	db	'Path not found',0
msg4	db	'Too many open files',0
msg5	db	'Permission denied',0
msg6	db	'Invalid handle',0
msg7	db	'Memory arena trashed',0
msg8	db	'Not enough memory',0
msg9	db	'Invalid memory block address',0
msg10	db	'Invalid environment',0
msg11	db	'Invalid format',0
msg12	db	'Invalid access code',0
msg13	db	'Invalid data',0
msg14	equ	0
msg15	db	'No such device',0
msg16	db	'Attempted to remove current directory',0
msg17	db	'Not same device',0
msg18	db	'No more files',0
msg19	db	'Invalid argument',0
msg20	db	'Arg list too long',0
msg21	db	'Exec format error',0
msg22	db	'Cross-device link',0
msg33	db	'Math argument',0
msg34	db	'Result too large',0
msg35	db	'File already exists',0

cmsg0	db	'Attempt to write on write-protected diskette',0
cmsg1	db	'Unknown unit',0
cmsg2	db	'Drive not ready',0
cmsg3	db	'Unknown command',0
cmsg4	db	'Data error (CRC)',0
cmsg5	db	'Bad request structure length',0
cmsg6	db	'Seek error',0
cmsg7	db	'Unknown media type',0
cmsg8	db	'Sector not found',0
cmsg9	db	'Printer out of paper',0
cmsg10	db	'Write failure',0
cmsg11	db	'Read failure',0
cmsg12	db	'General failure',0

_sys_errlist dw	msg0,msg1,msg2,msg3,msg4,msg5,msg6,msg7,msg8,msg9
	dw	msg10,msg11,msg12,msg13,msg14,msg15,msg16,msg17,msg18,msg19
	dw	msg20,msg21,msg22,0,0,0,0,0,0,0,0,0,0,msg33,msg34,msg35

	dw	cmsg0,cmsg1,cmsg2,cmsg3,cmsg4,cmsg5,cmsg6,cmsg7,cmsg8,cmsg9
	dw	cmsg10,cmsg11,cmsg12

__doserrno	dw	0

__dosErrorToSV	db	0,19,2,2,4,5,6,8,8,8,20,21,5,19,-1,22,5,17,2,-1,-1,-1
	db	-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,5,5,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
	db	-1,-1,-1,-1,-1,-1,15,-1,35,2,-1,15,-1,-1,-1,-1,19,-1,-1,2,2,5,15
	db	2,-1,-1,-1,19,-1,-1,-1,-1,-1,-1,-1,-1,35,-1,-1,-1,-1,35,-1,19,-1

_attrs	dw	7,70h,15		;initialize attributes for video

_DATA	ends
	end
