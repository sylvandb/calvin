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

;	system3

;	Call the DOS command processor to process a command.

;	int system3(const char *cmd, int fd_in, int fd_out,
;		Boolean thoroughshell);

;	This processes pipes, redirection, and internal and external commands.
;	It calls internal commands by calling COMMAND.COM with the /C switch and
;	calls external commands directly, so that their exit codes can be
;	returned.  (COMMAND.COM does not return exit codes, so internal commands
;	will never return an exit code.)

;	The value returned is:
;		-1..-18		Standard DOS error codes 1..18
;		0..ffh		Program returned this exit code
;		100h		Program was stopped by Control-Break
;		200h		Program was stopped by I/O error

;	Much of this code is already present in some form in COMMAND.COM; it
;	would have been much easier if this had been available as a subfunction
;	of the EXEC call.  Ahem.  :-( .

;	Compile with A86 and use with Turbo C.

CR	equ	13
LF	equ	10
TAB	equ	9


	public	_system3
	public	getenv0

DGROUP	group	_DATA,_BSS
_DATA	segment word public 'DATA'

	extrn	_errno:word
	extrn	__psp:word	;initial value of DS
	extrn	__exitbuf:word	;address of stuff to call when doing exit

pipe1	db	'_:/%PIPE1.$$$',0
zero	equ	pipe1+13
pipe2	db	'_:/%PIPE2.$$$',0
comspec	db	'COMSPEC'
path	db	'PATH'
intcmds	db	'BREAK',0,'CD',0,'CHDIR',0,'CLS',0,'COPY',0,'CTTY',0,'DATE',0
	db	'DEL',0,'DIR',0,'ECHO',0,'ERASE',0,'EXIT',0,'FOR',0,'GOTO',0
	db	'IF',0,'MD',0,'MKDIR',0,'PATH',0,'PAUSE',0,'PROMPT',0,'RD',0
	db	'REM',0,'REN',0,'RENAME',0,'RMDIR',0,'SET',0,'SHIFT',0,'TIME',0
	db	'TYPE',0,'VER',0,'VERIFY',0,'VOL',0,0

_DATA	ends

;	Stack frame.

dosver	equ	[bp-1]		;(byte) DOS version
swchar	equ	[bp-2]		;(byte) switch character (usually '/')
innam	equ	[bp-4]		;(word) address of string of '<' file
pipnam	equ	[bp-6]		;(word) address of string of pipe file
outnam	equ	[bp-8]		;(word) address of string of '>' file
appflg	equ	[bp-9]		;(byte) nonzero if '>>' was used
delflg	equ	[bp-10]		;(byte) nonzero if input file is to be deleted
inhndl	equ	[bp-11]		;(byte) internal handle for orig. stdin
outhndl	equ	[bp-12]		;(byte) internal handle for orig. stdout
;	equ	[bp-13]		;(byte) unused
errhndl	equ	[bp-14]		;(byte) internal handle for orig. stderr
avail	equ	[bp-16]		;(word) next available byte + 1 in upper part of
				;string area
next	equ	[bp-18]		;(word) next pipe to process
bgnpath	equ	[bp-20]		;(word) address of first char. of path name
bgnfil	equ	[bp-22]		;(word) addr of first char. of filename proper
endfil	equ	[bp-24]		;(word) addr of last char. of filename proper
envpos	equ	[bp-26]		;(word) current position in path string in
envseg	equ	[bp-28]		;(word)  environment.

fcb2	equ	[bp-44]		;second FCB to put in new process
fcb1	equ	[bp-60]		;first ...
block	equ	[bp-74]		;block for EXEC call
strs	equ	[bp-202]	;storage area for strings
strs6	equ	[bp-208]	;same as above with 6 extra bytes padding

fcb1a	equ	[bp-59]		;fcb1+1
block2	equ	[bp-72]		;block+2


savebp	dw	0		;keep these in the code segment
savess	dw	0

_system3:call	word [__exitbuf]

;	Set up stack frame

	push	bp		;set up stack frame
	push	si
	push	di
	mov	bp,sp
	push	ss
	pop	es
	mov	cs:savebp,bp
	mov	cs:savess,ss
	mov	ah,19h		;get current disk
	int	21h
	add	al,'A'
	mov	pipe1,al
	mov	pipe2,al
	mov	ax,3700h	;get switch character
	int	21h
	mov	ah,30h		;get DOS version
	int	21h
	mov	dh,al
	push	dx		;save swchar & dosver
	xor	ax,ax
	push	ax		;innam
	push	ax		;pipnam
	push	ax		;outnam
	push	ax		;appflg & delflg
	dec	ax
	push	ax		;inhndl & outhndl
	push	ax		;errhndl
	lea	ax,block
	push	ax		;avail
	sub	sp,12+32	;next, bgnpath, bgnfil, endfil, envpos, envseg,
				;fcb1, fcb2.

;	Set up block.

	push	ds
	lea	ax,fcb2
	push	ax
	push	ds
	lea	ax,fcb1
	push	ax
	push	ds
	push	ax		;address of 80h stuff; fill it later
	mov	ds,__psp	;save environment segment
	push	[2ch]
	sub	sp,86h

;	Take care of secondary arguments.

	call	toggargs	;toggle arguments
	jl	sy0a		;if no stdout argument
	mov	al,-1
	xchg	al,[1ah]	;close stderr
	mov	errhndl,al
	mov	ah,46h		;force a duplicate of a handle
	mov	bl,1
	mov	cx,2
	int	21h
sy0a:	push	ss		;restore ds
	pop	ds

;	Take care of quotes, '<', '>', and pipes.

	mov	si,[bp+8]	;get argument
	lea	di,strs
	dec	di
sy0:	mov	al,0
	stosb
sy1:	call	skipsep		;Loop over pipes.  Skip separators.
sy2:	lodsb
	cmp	al,'"'
	je	sy3		;if quote
	cmp	al,'<'
	je	sy5
	cmp	al,'>'
	je	sy6
	cmp	al,'|'
	je	sy9
sy2a:	stosb
	cmp	al,0
	jne	sy2		;if not end of string
	jmp	short sy10

;	Quotes.

sy3:	stosb
	lodsb
	cmp	al,0
	je	sy4		;if odd number of quotes
	cmp	al,'"'
	jne	sy3		;if not end
	jmp	sy2a

sy4:	dec	si		;back up to last quote
	dec	di
	cmp	byte [si-1],'"'
	jne	sy4		;if not the end
	inc	di
	jmp	sy2

;	'<' and '>'

sy5:	lea	bx,innam
	jmp	short sy8

sy6:	cmp	al,byte [si]
	mov	al,0		;append flag
	jne	sy7		;if not '>>'
	lodsb			;skip second '>' and set appflg
sy7:	mov	appflg,al
	lea	bx,outnam

;	Save file name and put address in [bx].

sy8:	call	skipsep
	call	len2sep
	push	di
	mov	di,avail	;allocate space for name
	sub	di,cx
	dec	di
	mov	avail,di
	mov	[bx],di
	rep	movsb		;move the name
	mov	al,0
	stosb
	pop	di
	jmp	sy2

;	Pipes.  Store them as juxtaposed null-terminated strings.

sy9:	cmp	byte [di-1],0
	je	sy1		;if empty pipe
	jmp	sy0

sy10:	stosb			;put an extra 0 for absolute end
	lea	si,strs

;	Begin execution loop.
;	At this point si = address of next command.

sy11:	mov	di,si		;find end of current string
	mov	al,0
	mov	cx,-1
	repnz	scasb
	mov	es,__psp
	mov	dx,innam
	or	dx,dx
	jz	sy12		;if no input file given

;	Open input file.

	mov	ax,3d00h	;open for reading
	int	21h
	jc	err_j1		;if error
	xchg	ax,bx
	mov	al,-1		;force it into handle 0
	xchg	al,es:[bx+18h]
	xchg	al,es:[18h]
	mov	inhndl,al

;	Open output or pipe file.

sy12:	cmp	byte [di],0
	jne	sy13		;if we are piping
	mov	dx,outnam
	mov	pipnam,dx
	or	dx,dx
	jz	sy18		;if no output redirection
	cmp	byte appflg,0
	jz	sy15		;if no appending
	mov	ax,3d01h	;open for writing
	int	21h
	jc	err_j1
	xchg	ax,bx
	mov	ax,4202h	;LSEEK
	xor	cx,cx
	xor	dx,dx
	int	21h
	jmp	short sy17

;	Open pipe file.

sy13:	mov	dx,offset pipe1
	cmp	dx,innam
	jne	sy14		;if not duplicate
	mov	dx,offset pipe2
sy14:	mov	pipnam,dx
	cmp	byte dosver,2
	je	sy15		;if DOS 2
	mov	ah,5ah		;create unique file
	mov	bx,dx
	mov	byte [bx+3],0
	jmp	short sy16

err_j1:	jmp	err

sy15:	mov	ah,3ch		;create file
sy16:	xor	cx,cx		;attribute
	int	21h
	jc	err_j1
	xchg	ax,bx

sy17:	mov	al,-1		;move it to handle #1
	xchg	al,es:[bx+18h]
	xchg	al,es:[19h]
	mov	outhndl,al

sy18:	push	ss
	pop	es
	mov	next,di

;	Process the input line.  At this point si points to the beginning of the
;	current line and di to the next.

;	Check for:	1.  Drive change.
;			2.  Internal command.
;			3.  External command with explicit drive or path.
;			4.  External command.

	push	si
	cmp	byte [bp+14],0	;should we always use COMSPEC?
	jnz	sy23		;if we always want to process as internal cmd
	mov	dl,swchar
	mov	ax,2900h	;parse filename
	lea	di,fcb1
	int	21h
	or	al,al
	mov	ax,15		;invalid drive
	js	err_j1
	mov	ax,2		;file not found
	jnz	err_j1
	cmp	byte [di],0
	je	sy19		;if no drive given
	cmp	byte [di+1],' '
	jne	sy24		;if part of filename given
	mov	al,[si]
	call	ifslash
	je	sy24		;if slash, then part of a filename is here

;	Change current drive.

	mov	ah,0eh		;select disk
	mov	dl,[di]
	dec	dx
	int	21h
	jmp	done		;done

;	Check for internal command.

sy19:	mov	al,[si]
	call	ifslash
	jne	sy20		;if not a slash, then this is not option 3.
	inc	byte [di]
sy20:	mov	di,offset intcmds
	mov	al,0

sy21:	cmp	al,[di]
	je	sy24		;if end of list
	mov	cx,8
	lea	si,fcb1a	;offset fcb1+1
	repe	cmpsb
	dec	di
	cmp	byte [si-1],' '
	jne	sy22		;if unequal
	scasb
	je	sy23		;if [di]=0, then they match
sy22:	repnz	scasb		;skip to next string
	jmp	sy21

;	Process internal command.

sy23:	pop	si
	sub	si,4
	mov	di,si
	mov	al,' '		;prepend " /C "
	mov	ah,swchar
	stosw
	mov	ax,' C'
	stosw
	call	do80h
	mov	si,offset comspec
	mov	cx,7
	call	getenv0		;get name of COMMAND.COM
	mov	ax,4b00h	;EXEC
	push	ds		;xchg ds,es
	push	es
	pop	ds
	pop	es
	mov	dx,di
	lea	bx,block
	int	21h
	mov	bp,cs:savebp
	mov	ss,cs:savess
	lea	sp,strs6
	if c jmp err		;if error
	jmp	done

;	Process external command.  At this point, the start address of the
;	string is on top of the stack, and the first byte of FCB1 is zero if
;	paths are to be searched.

sy24:	mov	di,offset zero
	cmp	byte fcb1,0
	jnz	sy25		;if drive or path given
	mov	si,offset path
	mov	cx,4
	call	getenv0
sy25:	pop	si		;start of string
	mov	envpos,di	;save environment position
	mov	envseg,es
	push	ss		;restore es
	pop	es
	call	len2sep
	lea	di,[si-6]
	mov	bgnfil,di	;beginning of file name
	mov	bgnpath,di
	rep	movsb		;move filename over
	mov	endfil,di	;end of file name

;	Set up FCB's & parameter string.

	push	si
	call	swskip		;skip switches
	mov	ax,2901h	;parse filename
	lea	di,fcb1
	int	21h
sy26:	lodsb			;skip till separator
	cmp	al,dl
	je	sy27		;if switchar
	cmp	al,0
	je	sy27		;if end
	call	ifsep
	jne	sy26		;if not separator
sy27:	dec	si
	call	swskip
	mov	ax,2901h	;parse filename
	lea	di,fcb2
	int	21h
	pop	si		;back to start of string
	call	do80h		;do parameter string

;	Loop over paths.  At this point, the path is prepended to the file name
;	and bgnpath is set accordingly.  ds=es=ss.

sy28:	mov	di,endfil	;add .COM
	mov	ax,'C.'
	stosw
	mov	ax,'MO'
	stosw
	mov	al,0
	stosb

sy29:	mov	ax,4b00h	;EXEC
	lea	bx,block
	mov	dx,bgnpath
	int	21h
	mov	bp,cs:savebp
	mov	ss,cs:savess
	mov	sp,bgnpath
	if nc jmp done		;if done
	cmp	ax,2
	je	sy29a		;if file not found
	cmp	ax,3		;path not found
	jne	err		;if other error
sy29a:	and	sp,-2
	push	ss
	pop	ds
	push	ss
	pop	es
	mov	di,endfil
	inc	di
	cmp	byte [di],'C'
	jne	sy30		;if we just did .EXE
	mov	ax,'XE'
	stosw
	stosb
	jmp	sy29		;go back and try .EXE

;	Try another path.

sy30:	mov	ds,envseg	;environment position
	mov	si,envpos
	call	skipsep
	cmp	al,0
	mov	ax,2		;file not found
	je	err		;if no more paths
	mov	cx,si		;get length of next path
sy31:	lodsb
	cmp	al,0
	je	sy32		;if end of string
	call	ifsep
	jne	sy31		;if not separator
sy32:	dec	si
	mov	envpos,si
sy33:	dec	si
	cmp	byte [si],'/'
	je	sy33		;skip trailing slashes
	cmp	byte [si],'\'
	je	sy33		;skip trailing slashes
	inc	si
	xchg	cx,si
	sub	cx,si		;cx = length
	mov	di,bgnfil	;prepend the next path
	dec	di
	sub	di,cx
	mov	sp,di
	and	sp,-2
	mov	bgnpath,di
	rep	movsb
	mov	al,'/'
	stosb
	push	ss		;restore ds
	pop	ds
	jmp	sy28		;try again

;	ERR	Process errors.  Return -ax.

err:	push	ss
	pop	ds
	lea	sp,fcb2
	mov	_errno,ax
	mov	ax,-1
	push	ax
	call	closio		;close I/O redirection
err1:	mov	si,next
	cmp	byte [si],0
	je	err2		;if the output file wasn't a pipe
	mov	dx,innam
	call	delpipe
err2:	pop	ax
	jmp	short quit

;	DONE	Done with this pipe.

done:	push	ss
	pop	ds
	lea	sp,strs6
	call	closio
	mov	ah,4dh		;get return code
	int	21h
	cmp	ah,0
	je	done1		;if normal termination
	push	ax		;error quit
	jmp	err1

done1:	mov	si,next
	cmp	byte [si],0
	je	quit		;if no more pipes
	jmp	sy11

quit:	mov	ds,__psp
	push	ax		;save return value
	call	toggargs	;put back the file descriptors
	jl	q1		;if no stdout argument
	mov	ah,3eh		;close file
	mov	bx,2		;stderr
	int	21h
	mov	al,errhndl	;restore previous value
	mov	[1ah],al
q1:	pop	ax		;restore return value
	mov	sp,bp
	push	ss		;restore ds
	pop	ds
	pop	di
	pop	si
	pop	bp
	ret

;	SWSKIP	Skip switches.  Assumes CLD and DL=swchar.

swskip:	call	skipsep
	cmp	al,dl
	jne	ss1		;if not switchar
	inc	si		;skip it
	call	skipsep
	lodsb
	cmp	al,0
	jne	swskip		;if not end of string
	dec	si
ss1:	ret

;	SKIPSEP	Skip separators.

skipsep:lodsb
	call	ifsep
	je	skipsep
	dec	si
	ret

;	LEN2SEP	Compute length till the next separator.  Answer is in CX.
;		Assumes dl = swchar; uses al.

len2sep:mov	cx,si
len1:	lodsb
	cmp	al,0
	je	len3
	cmp	dl,'/'
	jne	len2		;if switchar <> '/'
	cmp	al,dl
	je	len3		;quit if al=switchar='/'
len2:	call	ifsep
	jne	len1		;if not done
len3:	dec	si
	xchg	cx,si
	sub	cx,si
	ret

;	IFSEP	Compare AL with separators ' ', '\t', ',', ';', '='.

ifsep:	cmp	al,' '
	je	if1
	cmp	al,TAB
	je	if1
	cmp	al,','
	je	if1
	cmp	al,';'
	je	if1
	cmp	al,'='
if1:	ret

;	IFSLASH	Compare AL with '\' and '/' (the latter only if swchar!='/').

ifslash:cmp	dl,'/'
	je	ifsl1
	cmp	al,'/'
	je	ifsl2
ifsl1:	cmp	al,'\'
ifsl2:	ret

;	DO80H	Fix up the part of BLOCK dealing with the string to be put at
;		80h.  Initially si = address of beginning of string.
;		Alters bx, si.

do80h:	mov	bx,next
	dec	bx
	mov	byte [bx],CR
	sub	bx,si		;bx = length
	dec	si
	mov	[si],bl
	mov	block2,si
	ret

;	CLOSIO	Close pipe files.
;	DELPIPE	Delete the input file if it is called for.

closio:	mov	es,__psp
	mov	dx,pipnam
	or	dx,dx
	jz	cio1		;if no output file
	mov	ah,3eh		;close file
	mov	bx,1
	int	21h
	mov	al,-1		;restore internal handle
	xchg	al,outhndl
	mov	es:[19h],al

cio1:	xchg	dx,innam
	or	dx,dx
	jz	cio2		;if no input file
	mov	ah,3eh		;close file
	xor	bx,bx
	int	21h
	mov	al,-1		;restore internal handle
	xchg	al,inhndl
	mov	es:[18h],al

cio2:	push	ss
	pop	es
	mov	al,1
	xchg	al,delflg
	or	al,al
	jz	dp1		;if it is not to be deleted

delpipe:mov	ah,41h		;delete file
	int	21h

dp1:	ret

;	TOGGARGS - Toggle arguments.  Upon exit, flags indicate whether stderr
;	should be done, too.  Also, it assumes that ds = __psp.

toggargs:mov	bl,byte [bp+10]	;get second argument
	mov	bh,0
	cmp	bl,bh
	jl	togg1		;if no stdin argument
	mov	al,[18h]	;toggle input fd's
	xchg	al,[bx+18h]
	mov	[18h],al
togg1:	mov	bl,byte [bp+12]	;get third argument
	cmp	bl,bh
	jl	ret		;if no stdout argument
	mov	al,[19h]	;toggle fd's
	xchg	al,[bx+18h]
	mov	[19h],al
	ret			;return with flags intact


;	GETENV0	Primitive operations to get an environment string.

;	Entry	si	Address of variable name (capitalized)
;		cx	Length of variable name
;		CLD

;	Exit	es:di	Address of string (if found) or second 0 byte at end of
;			environment (if not found).

getenv0:mov	es,__psp
	mov	es,es:[2ch]	;get environment segment
	xor	di,di
	mov	al,0

ge1:	cmp	al,es:[di]	;beginning of loop
	je	ge4		;if end of environment
	push	cx
	push	si
	repe	cmpsb		;compare strings
	pop	si
	pop	cx
	jne	ge2		;if not equal
	cmp	byte es:[di],'='
	je	ge3		;if match
ge2:	scasb			;skip to end of string
	jne	ge2
	jmp	ge1		;loop back

ge3:	inc	di		;found it
ge4:	ret

	end
