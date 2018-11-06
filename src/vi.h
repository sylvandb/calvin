/*========================================================================*\

Copyright (c) 1997  Paul Vojta

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to
deal in the Software without restriction, including without limitation the
rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
sell copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
PAUL VOJTA BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

\*========================================================================*/

#include <string.h>
#include <stdlib.h>

#ifndef	EXTERN
#define	EXTERN		extern
#define	INIT(x)
#endif	EXTERN

typedef	short		smalint;	/* lines and chars within slab */
typedef	long		bigint;		/* lines and chars within file */
typedef	unsigned long	ubigint;	/* unsigned version of the above */
typedef	short		slabnum;
typedef	void		*caddr_t;
typedef	unsigned char	ubyte;
typedef	signed char	sbyte;
typedef	unsigned short	word;

typedef	signed char	Boolean;
#define	True		1
#define	False		0
#define	Maybe		(-1)
#define	Null		0

#define	MAXINT	32767

#define	XtNumber(x)	(sizeof(x) / sizeof(*x))
#define	bzero(s, len)	(void) memset(s, 0, len);
#define	CONTROL(x)	(x - '@')

#ifdef	TEST
#define	SLABLEN	28
#define	SLABMAX	32
#define	SLABSHIFT 5
#define	MAX_N_SLABS 2
#define	TESTSWAP
#define	MAXNEAR	44
#define	DEFAULTDIR	"c:/tmp"
#else
#define	SLABLEN	4000
#define	SLABMAX	4096
#define	SLABSHIFT 12
#define	MAXNEAR	8088
#endif

	/* needed for TC library */
#define	BINARY
#define	read		_read
#define	write		_write

#define	SEGS
typedef	struct {
	int	ip, sp, bp, si, di;
}	jmp_buf[1];
int	cdecl	setjmp(jmp_buf jmpb);

#define	OPTLEN		40
#define	BANGLEN		128	/* max length of shell command */
#define	MAXROWS		50
#define	MEM_MISER		/* try to free memory before shelling out */
#define	SHELLVAR	"COMSPEC"
#define	THOROUGHSHELL

#ifndef	SEGS
#define	far
#define	farread		read
#define	farwrite	write
#define	farmemmove	memmove
#define	farmemchr	memchr
#define	farmemichr	memichr
#define	farmemrchr	memrchr
#define	farmemrichr	memrichr
#define	farmemcmp	memcmp
#define	farmemicmp	memicmp
#else	SEGS
int	cdecl	farread(int handle, void far *buf, unsigned len);
int	cdecl	farwrite(int handle, const void far *buf, unsigned len);
void	cdecl	farmemmove(void far *d, const void far *s, size_t n);
void far *	cdecl	farmemchr(const void far *s, int c, size_t n);
void far *	cdecl	farmemrchr(const void far *s, int c, size_t n);
void far *	cdecl	farmemichr(const void far *s, int c, size_t n);
void far *	cdecl	farmemrichr(const void far *s, int c, size_t n);
int	cdecl	farmemcmp(const void far *s1, const void far *s2, size_t n);
int	cdecl	farmemicmp(const void far *s1, const void far *s2, size_t n);
#endif	SEGS

typedef	ubyte far *	slabseg;
#define	SSPTR(ss)	(ss)

typedef	struct {
	slabseg	slab;
	smalint	len;
	smalint	nlines;		/* count of number of newlines */
	short	fslot;
	Boolean	dirty;
	}
		slabrec, *slabptr;

#define	getdata(dst, s, s1, len)	farmemmove(dst, SSPTR(s) + (s1), len)
#define	putdata(d, d1, src, len)	farmemmove(SSPTR(d) + (d1), src, len)
#define	movedata(d, d1, s, s1, len) \
			farmemmove(SSPTR(d) + (d1), SSPTR(s) + (s1), len)

/*
 *	Position stuff.
 */

typedef	struct	posrec {
	bigint	byte;		/* within file */
	bigint	line;
	slabnum	slabno;
	smalint	offset;		/* within slab */
	smalint	slabline;
}
	*posptr;

				/* position of first byte */
EXTERN	struct posrec	pos0;	/* INIT({0, 0, 0, 0, 0}) */

/*
 *	Screen stuff
 */

EXTERN	Boolean	screengood	INIT(False);	/* is the screen right? */
EXTERN	int	halfscrn	INIT(12);	/* scroll amount for ^D/^U */
EXTERN	int	curcol;		/* current column number */

EXTERN	bigint	baslin		INIT(1);	/* first line no. displayed */
EXTERN	int	ldisp		INIT(0);	/* number of lines on screen */
EXTERN	struct ldata {
	int		lrow;
	struct posrec	lpos;		/* only byte, slabno, offset are used */
}
	linedata[MAXROWS + 1];		/* data on nth line on screen */

/*
 *	Buffers for yanking and for storing the main file
 */

typedef	struct bufrec {
	Boolean	slabs;
	Boolean	linebuf;
	bigint	nbytes;
	bigint	nlines;
	ubyte	*addr;
	smalint	len;
	struct bufrec *prev, *next;
}
	*bufptr;

EXTERN	struct bufrec	buffers[30];	/* 'yank' text buffers */

#define	ZEROBUF	buffers		/* default buffer to use */
#define	XBUF1	(&buffers[27])	/* extra buffer for undo, etc. */
#define	XBUF2	(&buffers[28])	/* extra buffer for undo, etc. */
#define	MAINBUF	(&buffers[29])	/* buffer to store the file in */

#define	YKSPACE	30000		/* amount of room to allocate for buffer info */
EXTERN	ubyte	*bufspace;	/* matrix to put the above in */
EXTERN	struct bufrec	lastbuf;
EXTERN	int	ykroom	INIT(YKSPACE);

/*
 *	Variables to do with undoing
 */

EXTERN	bufptr	ubuf;		/* buffer storing deleted text for 'u' cmd */
EXTERN	bigint	ybyte1, ybyte2;	/* inclusive positions of last inserted text */
EXTERN	bigint	cbyte1, cbyte2;	/* before and after cursor positions */
EXTERN	bufptr	ybuf		INIT(Null);	/* buffer no. of above text */
EXTERN	Boolean	ylines;		/* if last insertion was a line operation */
EXTERN	Boolean	old_modif;	/* previous value of modif flag */
EXTERN	int	old_curcol;	/* previous value of current cursor column */
EXTERN	Boolean	old_eolateof;

/*
 *	Stuff for moving around in the file (seekpos, seeknext, seekprev)
 */

EXTERN	bigint	sk_byte	INIT(0);	/* byte of start of this chunk*/
EXTERN	int	sk_slabno;		/* corresponds to sk_byte */
EXTERN	int	sk_offset;		/* corresponds to sk_byte */
EXTERN	ubyte far *sk_addr;		/* addr & len of this chunk */
EXTERN	int	sk_len;
EXTERN	ubyte far *sk_end;		/* end + 1 of this chunk */

/*
 *	Stuff for motion commands
 */

#define	NOLOC	'\n'

struct locrec {
	char	ltype;	/* type of locate command */
	Boolean	lineloc;/* if it's a line motion */
	union {
		int	mindex;		/* index of mark to refer to */
		int	linepart;	/* number of lines in additon */
	} x;
	int	sslen;	/* length of following string */
	ubyte	sstr[80];	/* string used with '/' or '?' */
	ubyte	fchar;		/* 'f' character */
};

EXTERN	char	loctyp;		/* f, F, t, or T */
EXTERN	ubyte	locchar		INIT(NOLOC);	/* character used with above */
EXTERN	int	locdir		INIT(0);	/* direction for / or ? */
EXTERN	int	loclen;		/* length for the following */
EXTERN	ubyte	locpat[80];	/* pattern for above */

typedef	struct markrec {	/* structure for marking a position */
	bufptr	buffer;
	bigint	byte;
}
	*markptr;

EXTERN	struct markrec	markpos[27];

#define	X_END	256+0
#define	X_CHAR	256+1
#define	X_WORD	256+2
#define	X_LINE	256+3
#define	X_WORD2	256+4		/* internal state for gpinput() */

EXTERN	int	argc;
EXTERN	char	**argv;
EXTERN	int	argindex	INIT(0);
EXTERN	Boolean	edited		INIT(True);	/* [not edited] flag */
EXTERN	Boolean	modif		INIT(False);	/* has file been modified? */
EXTERN	struct posrec	cp;	/* current position */
EXTERN	Boolean	heapfull;	/* set if memory fills up */

extern	int	cdecl	attrs[3];	/* initialized in vv.asm */

#define	ATTRNORM attrs[0]
#define	ATTRHIGH attrs[1]
#define	ATTRBOLD attrs[2]

EXTERN	int	cdecl	botcurs;	/* cursor position at bottom */
	/* number of lines at the bottom of the screen */
EXTERN	int	cdecl	botlines;
	/* attribute to use for next call to botputs */
EXTERN	Boolean	cdecl	botdirty	INIT(False);	/* if bottom is dirty */
EXTERN	int	cdecl	botattr;
EXTERN	int	cdecl	window;		/* number of text lines on the screen */
#define	window1	ROWS		/* number of lines on the screen (used) */
EXTERN	int	cdecl	pos2480;	/* window * COLS */
EXTERN	int	cdecl	pos2580;	/* window1 * COLS */
EXTERN	int	cdecl	ROWS;		/* total number of rows on the screen */
EXTERN	int	cdecl	COLS;		/* number of columns */

EXTERN	struct {		/* info on last buffer-changing command */
		char	cmd;	/* may be one of cdiIaApPrJ or '.' */
		bufptr	buffer;
		bigint	rep;
		struct locrec	loc;
		ubyte	rchar;	/* used for 'r' */
	} dot;

/* global :set options */
EXTERN	Boolean	startup		INIT(True);
EXTERN	Boolean	cdecl	visual	INIT(False);
EXTERN	int	marginbell	INIT(0);
EXTERN	int	cdecl	printable	INIT('~');
EXTERN	Boolean	showmode	INIT(False);
EXTERN	char	nomode[]	INIT("              ");
#define	MODEWID	14
EXTERN	int	cdecl	tabstop		INIT(8);
EXTERN	int	vidcompat	INIT(0);
EXTERN	Boolean	eolateof	INIT(True);

#ifdef	DEFAULTDIR
EXTERN	char	directory[OPTLEN]	INIT(DEFAULTDIR);
#else
EXTERN	char	directory[OPTLEN];
#endif
EXTERN	int	swaphandle	INIT(-1);

EXTERN	void far * cdecl (*mem_chr)(const void far *s, int c, size_t n)
				INIT(farmemchr);
EXTERN	void far * cdecl (*memr_chr)(const void far *s, int c, size_t n)
				INIT(farmemrchr);
EXTERN	int cdecl (*mem_cmp)(const void far *s1, const void far *s2, size_t n)
				INIT(farmemcmp);

/*
 *	External procedures.
 */

EXTERN	jmp_buf	cdecl	errbuf;

typedef	enum {full, half, normal} curstyp;

void	cdecl	invid(int vc);
void	cdecl	endvid(void);
void	cdecl	cshape(curstyp typ);
int	cdecl	writes(const ubyte far *str, int len, int base, int pos,
		int attr);
void	cdecl	putcur(int pos);
void	cdecl	mvup(int topline, int botline, int nlines);
void	cdecl	mvdn(int topline, int botline, int nlines);
void	cdecl	clearrows(int topline, int botline);
void	cdecl	clrel(void);
void	cdecl	raise_err(void);
void	cdecl	beep(void);
void	cdecl	beep2(void);
void	cdecl	chirp(void);
void	cdecl	ting(void);
char	cdecl	inkey_c(void);
word	cdecl	inkey_i(void);
ubyte	cdecl	inkey_1(void);
void	cdecl	inkey_wait(void);
void	cdecl	setdosesc(Boolean);

/*
 *	Utility functions.
 */

/*	The following are used for gpinput	*/
EXTERN	word	gpi_key;
EXTERN	ubyte	far *gpi_ptr0;		/* address where to put stuff */
EXTERN	ubyte	far *gpi_ptr;		/* next address to put stuff */
EXTERN	ubyte	far *gpi_maxptr;		/* last + 1 usable address */
EXTERN	Boolean	gpi_first;		/* if gpi_ptr0 points to bgn of line */
EXTERN	int	gpi_colbase;
EXTERN	int	gpi_col0;
EXTERN	int	gpi_col;
EXTERN	int	gpi_colend;
EXTERN	int	gpi_coljunk;		/* last+1 character of deleted stuff */

char	*formname(char *result, const char *extn);
void	initswap(void);
void	rmfiles(void);
void	raise_mem_err(void);
void	bufrealloc(bufptr buffer, int newlen);
void	prefree(bufptr buffer, int len);
slabseg	newslab(void);
short	newfslot(void);
void	freefslot(short fsl);
void	free_ss(slabseg ss);
void	free_sl(slabptr sl);
void	loadslab(slabptr sl);
#ifdef	TESTSWAP
void	oktoswap(void);
#else
#define	oktoswap()
#endif
#ifdef	MEM_MISER
void	pre_shell(void);
#else
#define	pre_shell()
#endif
void	freebuf(bufptr buffer);
bufptr	mvbuf(bufptr dst, bufptr src);
int	countlines(ubyte far *addr, int len);
void	slabify(bufptr buffer);
void	addslab(bufptr buffer, ubyte far *addr, int len, int nlines);
void	catslab(bufptr buffer, slabseg ss, int len);
void	addbytes(bufptr buffer, ubyte far *addr, int len, int nlines);
bigint	posadjust(bigint add, posptr pos);
ubyte far *seekpos(const posptr pos);
ubyte far *seekprev(void);
ubyte far *seeknext(void);
void	findline(bigint lineno, posptr pos);
void	findbyte(bigint byteno, posptr pos);
void	first(posptr pos);
int	colof(const struct posrec *pos);
void	setcol(void);
void	setcp(bigint lineno);
ubyte	currchar(void);
void	ring_raise(void);
void	openup(void);
void	botco(ubyte c);
void	botnewline(void);
void	botend(void);
void	botputs(const char *s);
void	botputd(long i);
void	errmes(const char *s);
void	msg_raise(const char *s);
void	botperror(char *msg);
void	chkbottom(void);
Boolean	isletter(ubyte c);
Boolean	isother(ubyte c);
void	gpiscroll(int j);
void	gpinput(void);
int	in24(char c, ubyte *instring);
void	chkrep(char *cp, bigint *rep);
bigint	getnum(char **pp);
int	getletter(ubyte c);
void	markhere(int i);

/*
 *	Screen functions.
 */

int	clen(ubyte c, int col);
int	slen(const ubyte far *strp, int len, int col);
int	xslen(const posptr pos, bigint byteno, int col);
void	lrowset(bigint lineno);
void	lrowfind(bigint lineno, int row);
void	xwrites(posptr pos1, posptr pos2, int base, int pos);
void	filldisp(int l1, int l2);
void	dispend(void);
void	fillend(void);
void	dispset(void);
void	rollfind(void);
void	cfroll(bigint lineno, int row, int rep);
int	makerow(int lineind);
void	doshowmode(const char *m);

/*
 *	String searching functions.
 */

ubyte	*parsesearch(ubyte *dest, int *destlenp, ubyte *p, ubyte *p_end,
    char c_end);
void	parsloc(char c, struct locrec *loc, char cmd);
void	dosearch(int dir, posptr pos, Boolean *wrap_p);
void	doloc(bigint n, struct locrec *loc, posptr pos, int incl, char cmd,
    Boolean *wrap, int *newcolp);
#ifdef	CHECK
void	checkpos(posptr pos);
#endif

/*
 *	Text modification functions.
 */

void	insmode(const posptr endp, Boolean linemode, const char *modestr);
void	dochange(bufptr ibuf, bufptr dbuf, posptr pos2, Boolean scfix,
    Boolean bottom);

/*
 *	Command functions.
 */

void	fixybuf(void);
void	yank_cmd(bigint rep, bufptr buffer, char fstloc);
void	chg_cmd(bigint rep, bufptr buffer, char cmd, char fstloc);
void	bang_cmd(bigint rep, bufptr buffer, Boolean dotting);
void	ope_cmd(bigint lineno);
void	ins_cmd(char cmd, Boolean dotting);
void	put_cmd(bufptr buffer, Boolean after);
void	repl_cmd(int rep, Boolean dotting);
void	join_cmd(void);
void	undo(void);
void	motion(bigint rep, char c);

/*
 *	Line editor functions.
 */

void	status(void);
void	ex_version(void);
void	bangfilter(const char *cmd, posptr pos1, posptr pos2, bufptr answbuf,
		Boolean warn, Boolean keep);
void	excommand(char *str);
