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

#include "vi.h"
#include <stdio.h>
#include <alloc.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <io.h>
#include <errno.h>

extern	char *	cdecl	sys_errlist[];
extern	int	cdecl	sys_nerr;

static	slabseg	avail_slab	= Null;		/* head of free slab list */

static	short	firstfs		= 0;		/* first fslab possibly free */
static	ubyte	fsused[(YKSPACE / sizeof(slabrec) + 7) / 8];
						/* bits set if fslab used */
#ifdef	MAX_N_SLABS
static	int	slabsleft	= MAX_N_SLABS;
#else
static	Boolean	memfull		= False;
#endif

static	void	int_raise(const char *);

/*
 *	vi uses a simple algorithm to consider which buffers to swap out:
 *	it just goes through every possible buffer circularly until it finds
 *	one which is not currently in use.
 */

static	bufptr	swapbuf		= buffers;
static	smalint	swapoffset	= 0;
static	slabseg	dontswapme;

/*
 *	FORMNAME - Create file name.
 */

char	*formname(char *result, const char *extn) {
	char	*p;

#ifndef	DEFAULTDIR
	if (*directory == '\0')
	    p = stpcpy(result, "%%");
	else
#endif
	{
	    p = stpcpy(result, directory);
#ifdef	__MSDOS__
	    if (p[-1] != '/' && p[-1] != '\\' && p[-1] != ':')
#else
	    if (p[-1] != '/')
#endif
		*p++ = '/';
	}
	(void) strcpy(stpcpy(p, "vi."), extn);
	return result;
}

/*
 *	INITSWAP - Initialize swap file.
 */

void	initswap(void) {
	char	swapname[OPTLEN + 7];

	swaphandle = open(formname(swapname, "dat"), O_RDWR | O_CREAT,
	    S_IREAD | S_IWRITE);
	if (swaphandle == -1) {
	    perror("swap file");
	    exit(1);
	}
}

/*
 *	RMFILES	Remove swap file.
 */

void	rmfiles(void) {
	char	swapname[OPTLEN + 7];

	if (swaphandle != -1) {
	    close(swaphandle);
	    unlink(formname(swapname, "dat"));
	}
}

/*
 *	RAISE_MEM_ERR - Raise out-of-memory error.
 */

void	raise_mem_err(void) {
	heapfull = True;
	msg_raise("Out of memory--save what you have, immediately");
}

/*
 *	BUFREALLOC - Change the size of a buffer.  If it can't fit, either
 *		slabify some other buffer or slabify this one.  In the latter
 *		case, don't add any space.
 */

void	bufrealloc(bufptr buffer, int newlen) {
	ubyte	*addr1;
	bufptr	buf1, buf2;
	int	maxlen;

	if (newlen == 0) {	/* free it */
	    if (buffer->addr != Null) {
		buffer->next->prev = buffer->prev;
		buffer->prev->next = buffer->next;
		ykroom += buffer->len;
	    }
	    bzero(buffer, sizeof(*buffer));
	    return;
	}
	if (!buffer->slabs && newlen > MAXNEAR) {
	    slabify(buffer);
	    return;
	}
	/* Ensure that there exists enough space, somewhere */
	while (ykroom < newlen - buffer->len) {
	    maxlen = 0;		/* find longest non-slabified buffer */
	    for (buf1 = &lastbuf; (buf1 = buf1->next) != &lastbuf;)
		if (!buf1->slabs && buf1->len > maxlen) {
		    buf2 = buf1;
		    maxlen = buf2->len;
		}
	    if (newlen >= maxlen && !buffer->slabs) {
		buf2 = buffer;
		maxlen = newlen;
	    }
	    if (maxlen <= sizeof(slabrec)) raise_mem_err();
	    slabify(buf2);
	    if (buf2 == buffer) return;
	}
	if (buffer->addr == Null) {	/* link it into the list */
	    (buffer->next = lastbuf.next) -> prev = buffer;
	    buffer->prev = &lastbuf;
	    lastbuf.next = buffer;
	    buffer->addr = bufspace;
	}
	if (buffer->addr + newlen > buffer->next->addr) {
		/* can't expand here; look for a large gap */
	    buf1 = &lastbuf;
	    addr1 = bufspace;
	    do {
		buf2 = buf1->next;
		if (addr1 + newlen <= buf2->addr) {
		    memcpy(addr1, buffer->addr, buffer->len);
		    buffer->addr = addr1;
		    if (buffer != buf2) {
			buffer->next->prev = buffer->prev;
			buffer->prev->next = buffer->next;
			buf1->next = buf2->prev = buffer;
			buffer->prev = buf1;
			buffer->next = buf2;
		    }
		    break;
		}
		buf1 = buf2;
		addr1 = buf1->addr + buf1->len;
	    }
	    while (buf1 != &lastbuf);
	}
	if (buffer->addr + newlen > buffer->next->addr)
		/* pack the successors */
	    for (buf2 = &lastbuf; (buf1 = buf2->prev) != &lastbuf; buf2 = buf1)
	    {
		addr1 = buf2->addr - buf1->len;
		if (addr1 != buf1->addr) {
		    memmove(addr1, buf1->addr, buf1->len);
		    buf1->addr = addr1;
		}
	    }
	if (buffer->addr + newlen > buffer->next->addr)
		/* pack the predecessors */
	    for (buf1 = &lastbuf, addr1 = bufspace;;) {
		buf1 = buf1->next;
		if (addr1 != buf1->addr) {
		    memmove(addr1, buf1->addr, buf1->len);
		    buf1->addr = addr1;
		}
		if (buf1 == buffer) break;
		addr1 += buf1->len;
	    }
	if (buffer->addr + newlen > buffer->next->addr)
	    int_raise("Internal error--bufrealloc");
	if (buffer->slabs && buffer->len < newlen)
	    bzero(buffer->addr + buffer->len, newlen - buffer->len);
	ykroom += buffer->len - newlen;
	buffer->len = newlen;
}

/*
 *	PREFREE	Free space at the beginning of a buffer.
 */

void	prefree(bufptr buffer, int len) {

	buffer->addr += len;
	buffer->len -= len;
	ykroom += len;
}

/*
 *	SWAPOUT	Swap slab out to disk.
 */

static	void	swapout(slabptr sl) {
	if (sl->dirty &&
		(lseek(swaphandle, (long) sl->fslot << SLABSHIFT,
		    SEEK_SET) == -1 ||
		farwrite(swaphandle, sl->slab, SLABMAX) < SLABMAX))
	    if (errno == 0) errmes("Out of swap space");
	    else botperror("Writing swap file:  ");
	sl->slab = Null;
	sl->dirty = False;
}

/*
 *	NEWSLAB	Allocate a slab.
 */

slabseg	newslab(void) {
	slabseg	ss;

	ss = avail_slab;
	if (ss != Null)
	    avail_slab = *((slabseg far *) avail_slab);
#ifdef	MAX_N_SLABS
	else if (slabsleft == 0 ||
	    (--slabsleft, (ss = farmalloc(SLABMAX))) == Null)
#else
	else if (memfull || (ss = farmalloc(SLABMAX)) == Null)
#endif
	{
#ifndef	MAX_N_SLABS
	    memfull = True;
#endif
		/* swap a slab out to disk */
#ifndef	DEFAULTDIR
	    if (swaphandle == -1) raise_mem_err();
#endif
	    for (;;) {
		slabptr	sl;
		slabptr	sl_end;

		if (!swapbuf->slabs || swapoffset >= swapbuf->len) {
		    if (++swapbuf >= buffers + XtNumber(buffers))
			swapbuf = buffers;
		    swapoffset = 0;
		    continue;
		}
		sl = (slabptr) (swapbuf->addr + swapoffset);
		sl_end = (slabptr) (swapbuf->addr + swapbuf->len);
		/* this may be a null loop, but we know that ss == Null */
		while (sl < sl_end) {
		    ss = sl->slab;
		    if (ss == dontswapme) ss = Null;
		    if (ss != Null) break;
		    ++sl;
		}
		swapoffset = (ubyte *) (sl + 1) - swapbuf->addr;
		if (ss != Null) {
		    swapout(sl);
		    break;
		}
	    }
	}
	return ss;
}

/*
 *	NEWFSLOT - Get a new slot in the file.
 */

short	newfslot(void) {
	register ubyte *fsp;
	ubyte	mask;

	fsp = fsused + (firstfs >> 3);
	while (*fsp == 0xff) ++fsp;
#ifdef	TEST
	if (fsp >= fsused + XtNumber(fsused))
	    int_raise("Internal error--newfslot");
#endif	TEST
	firstfs = (fsp - fsused) << 3;
	mask = 1;
	while ((*fsp & mask) != 0) mask <<= 1, ++firstfs;
	*fsp |= mask;
	return firstfs;
}

/*
 *	FREEFSLOT - Free an fslot.
 */

void	freefslot(short fsl) {
	fsused[fsl >> 3] &= ~(1 << (fsl & 7));
	if (fsl < firstfs) firstfs = fsl;
}

/*
 *	FREESLAB - Release a slab.
 */

void	free_ss(slabseg ss) {
	if (ss != Null) {
	    *((slabseg far *) ss) = avail_slab;
	    avail_slab = ss;
	}
}

void	free_sl(slabptr sl) {
	free_ss(sl->slab);
	freefslot(sl->fslot);
}

/*
 *	LOADSLAB - Make sure the given slab is in memory.
 */

void	loadslab(slabptr sl) {

	if (sl->slab == Null) {
	    sl->slab = newslab();
	    if (lseek(swaphandle, (long) sl->fslot << SLABSHIFT, SEEK_SET) == -1
		    || farread(swaphandle, sl->slab, SLABMAX) == -1)
		botperror("Reading swap file:  ");
	}
	dontswapme = sl->slab;
}

#ifdef	TESTSWAP
void	oktoswap(void) {
	dontswapme = Null;
}
#endif

#ifdef	MEM_MISER
/*
 *	pre_shell - Free up memory prior to shelling out to another process
 */

void	pre_shell(void) {
	bufptr	buffer;
	slabptr	sl;
	slabptr	sl_end;
	slabseg	ss;

#ifndef	DEFAULT_DIR
	if (*directory != '\0')
#endif
	for (buffer = buffers; buffer < buffers + XtNumber(buffers); ++buffer)
	    if (buffer->slabs) {
		sl_end = (slabptr) (buffer->addr + buffer->len);
		for (sl = (slabptr) buffer->addr; sl < sl_end; ++sl)
		    if ((ss = sl->slab) != Null) {
			swapout(sl);
			farfree(ss);
		    }
	    }
	while (avail_slab != Null) {
	    ss = avail_slab;
	    avail_slab = *((slabseg far *) avail_slab);
	    farfree(ss);
	}
#ifdef	MAX_N_SLABS
	slabsleft = MAX_N_SLABS;
#else
	memfull = False;
#endif
}
#endif	/* MEM_MISER */

/*
 *	FREEBUF	Release a whole buffer's worth of information.
 */

void	freebuf(bufptr buffer) {
	markptr	markp;

	if (buffer->slabs) {
	    slabptr sl = (slabptr) buffer->addr;
	    slabptr sl_end = (slabptr) (buffer->addr + buffer->len);

	    for (; sl < sl_end; ++sl) free_sl(sl);
	}
	bufrealloc(buffer, 0);
	for (markp = markpos; markp < markpos + XtNumber(markpos); ++markp)
	    if (markp->buffer == buffer) markp->buffer = Null;
}

/*
 *	MVBUF	Move a buffer to a different location.
 */

bufptr	mvbuf(bufptr dst, bufptr src) {

	freebuf(dst);
	*dst = *src;
	bzero(src, sizeof(*src));
	dst->prev->next = dst->next->prev = dst;
	return dst;
}

/*
 *	COUNTLINES - Count the number of '\n' characters in the given address
 *	range.
 */

int	countlines(ubyte far *addr, int len) {
	int	n	= 0;
	ubyte	far *p	= addr;
	ubyte	far *p_end	= addr + len;

	for (;;) {
	    p = farmemchr(p, '\n', p_end - p);
	    if (p == Null) return n;
	    ++p;
	    ++n;
	}
}

/*
 *	MKSLAB	Make a slab and put its record in the given spot.
 *		CAUTION:  sl may overlap addr.
 */

static	void	mkslab(slabptr sl, ubyte far *addr, int len, int nlines) {
	slabseg	ss;

	ss = newslab();
	farmemmove(SSPTR(ss), addr, len);
	sl->nlines = (nlines >= 0 ? nlines : countlines(addr, len));
	sl->len = len;
	sl->dirty = True;
	sl->slab = ss;
	sl->fslot = newfslot();
}

/*
 *	SLABIFY	Change a simple buffer to a slabby one.
 *		It's OK if the buffer doesn't exist yet.
 */

void	slabify(bufptr buffer) {
	slabptr	sl;
	ubyte	*addr;
	ubyte	*lastaddr;
	int	oldlen	= buffer->len;
	int	len;

	if (oldlen < sizeof(slabrec) && oldlen != 0)
	    bufrealloc(buffer, sizeof(slabrec));
	addr = buffer->addr;
	lastaddr = addr + oldlen;
	buffer->slabs = True;
	ykroom += buffer->len;
	buffer->len = 0;
	for (sl = (slabptr) addr; addr < lastaddr; ++sl) {
	    slabrec slr;

	    len = lastaddr - addr;
	    if (len > SLABMAX) len = SLABLEN;
	    mkslab(&slr, addr, len, -1);
	    bufrealloc(buffer, buffer->len + sizeof(slabrec));
	    *sl = slr;
	    addr += len;
	}
}

/*
 *	ADDSLAB	Add the given slab to the given buffer.
 */

void	addslab(bufptr buffer, ubyte far *addr, int len, int nlines) {
	slabptr	sl;

	if (len == 0) return;
	if (!buffer->slabs) slabify(buffer);
	bufrealloc(buffer, buffer->len + sizeof(slabrec));
	sl = (slabptr) (buffer->addr + buffer->len) - 1;
	mkslab(sl, addr, len, nlines);
	buffer->nlines += sl->nlines;
	buffer->nbytes += len;
}

/*
 *	CATSLAB	Add the given slab to the given buffer.  No copying is done.
 */

void	catslab(bufptr buffer, slabseg ss, int len) {
	slabptr	sl;

	if (!buffer->slabs) slabify(buffer);
	bufrealloc(buffer, buffer->len + sizeof(slabrec));
	sl = (slabptr) (buffer->addr + buffer->len) - 1;
	sl->slab = ss;
	buffer->nlines += (sl->nlines = countlines(SSPTR(ss), len));
	buffer->nbytes += (sl->len = len);
	sl->dirty = True;
	sl->fslot = newfslot();
}

/*
 *	ADDBYTES - Add the given number of characters to the given buffer.
 */

void	addbytes(bufptr buffer, ubyte far *addr, int len, int nlines) {

	if (len == 0) return;
	if (nlines < 0) nlines = countlines(addr, len);
	if (!buffer->slabs) bufrealloc(buffer, buffer->len + len);
	if (!buffer->slabs) {
	    farmemmove(buffer->addr + buffer->len - len, addr, len);
	    buffer->nlines += nlines;
	    buffer->nbytes += len;
	}
	else {
	    slabptr sl = (slabptr) (buffer->addr + buffer->len) - 1;
	    int l1;
	    int	nl;

	    l1 = buffer->len == 0 ? SLABMAX : sl->len;
	    l1 = (l1 + len > SLABMAX ? SLABLEN : SLABMAX) - l1;
	    if (l1 > 0) {
		if (l1 > len) l1 = len;
		loadslab(sl);
		farmemmove(SSPTR(sl->slab) + sl->len, addr, l1);
		nl = countlines(addr, l1);
		addr += l1;
		len -= l1;
		sl->len += l1;
		buffer->nbytes += l1;
		nlines -= nl;
		sl->nlines += nl;
		buffer->nlines += nl;
		sl->dirty = True;
	    }
	    while (len > SLABMAX) {
		nl = countlines(addr, SLABLEN);
		addslab(buffer, addr, SLABLEN, nl);
		addr += SLABLEN;
		len -= SLABLEN;
		nlines -= nl;
	    }
	    addslab(buffer, addr, len, nlines);
	}
}

/*
 *	POSADJUST - Adjust the position by the given number of bytes.
 *		Returns the number added to slabline.
 */

bigint	posadjust(bigint add, posptr pos) {
	slabptr	sl;
	bigint	off1	= pos->offset + add;
	bigint	linoff	= 0;

	sl = (slabptr) (MAINBUF->addr + pos->slabno);
	while (off1 < 0) {
	    --sl;
	    off1 += sl->len;
	    linoff += sl->nlines;
	}
	while (off1 >= sl->len &&
		sl + 1 < (slabptr) (MAINBUF->addr + MAINBUF->len)) {
	    off1 -= sl->len;
	    linoff -= sl->nlines;
	    ++sl;
	}
	pos->slabno = (ubyte *) sl - MAINBUF->addr;
	pos->slabline += linoff;
	pos->offset = off1;
	pos->byte += add;
	return linoff;
}

/*
 *	SEEKPOS	Locate an array containing the given byte of the file.
 *		Uses pos->{byte,slabno,offset}.
 */

ubyte far *seekpos(const posptr pos) {
	slabptr	sl;

	loadslab(sl = (slabptr) (MAINBUF->addr + pos->slabno));
	sk_byte = pos->byte - pos->offset;
	sk_slabno = pos->slabno;
	sk_offset = 0;
	sk_addr = SSPTR(sl->slab);
	sk_len = sl->len;
	sk_end = SSPTR(sl->slab) + sk_len;
	return sk_addr + pos->offset;
}

/*
 *	SEEKPREV - Get the previous chunk of the file.
 */

ubyte far *seekprev(void) {
	struct posrec	pos;

	if (sk_byte == 0) return Null;
	pos.byte = sk_byte;
	pos.slabno = sk_slabno;
	pos.offset = sk_offset;
	(void) posadjust(-1, &pos);
	return seekpos(&pos) + 1;
}

/*
 *	SEEKNEXT - Get the next chunk of the file.
 */

ubyte far *seeknext(void) {
	struct posrec	pos;

	pos.byte = sk_byte;
	pos.slabno = sk_slabno;
	pos.offset = sk_offset;
	(void) posadjust(sk_len, &pos);
	if (pos.byte >= MAINBUF->nbytes) return Null;
	return seekpos(&pos);
}

/*
 *	SKIPLINES - Return number of bytes to skip until the given number of \n
 *		characters are found.
 */

static	int	skiplines(ubyte far *addr, int len0, int lines0,
		int len, int lines, int lskip) {
	ubyte far *p	= addr;
	ubyte far *p_end = addr + len;
	static	ubyte	interr[]	= "Internal error--skiplines.";

	if (lskip == 0) return 0;
	if (len0 != 0)
	    if (lskip <= lines0) {
		p_end = addr + len0;
		lines = lines0;
	    }
	    else {
		p += len0;
		lines -= lines0;
		lskip -= lines0;
	    }
	if (lskip * 2 <= lines)
	    while (--lskip >= 0) {
		p = farmemchr(p, '\n', p_end - p);
		if (p == Null) {
		    beep2();
		    errmes(interr);
		    return 0;
		}
		++p;
	    }
	else {
	    while (lskip <= lines) {
		++lskip;
		p_end = farmemrchr(p, '\n', p_end - p);
		if (p_end == Null) {
		    beep2();
		    errmes(interr);
		    return 0;
		}
	    }
	    p = p_end + 1;
	}
	return p - addr;
}

/*
 *	CLOSEST	Find the closest of three numbers.  Omit the middle one if the
 *		associated number is zero.
 */

static	void	*closest(bigint val, bigint val1, void *dat1,
		bigint val2, void *dat2, bigint val3, void *dat3) {
	if (val2 != 0)
	    if (val <= val2) {
		val3 = val2;
		dat3 = dat2;
	    }
	    else {
		val1 = val2;
		dat1 = dat2;
	    }
	return 2 * val > val1 + val3 ? dat3 : dat1;
}

/*
 *	FINDLINE - Find the position of the given line.
 */

void	findline(bigint lineno, posptr pos) {
	slabptr	sl;
	struct posrec	posend, *posp;
	bigint	byte, lin;

	if (lineno == 0) {
	    *pos = pos0;
	    return;
	}
	if ((ubigint) lineno > (ubigint) MAINBUF->nlines)
	    int_raise("Internal error--findline");
	posend.byte = MAINBUF->nbytes;
	posend.line = MAINBUF->nlines;
	posend.slabno = MAINBUF->len - sizeof(slabrec);
	sl = (slabptr) (MAINBUF->addr + posend.slabno);
	posend.offset = sl->len;
	posend.slabline = sl->nlines;
	posp = closest(lineno, 1, &pos0, cp.line, &cp,
	    MAINBUF->nlines, &posend);
	sl = (slabptr) (MAINBUF->addr + posp->slabno);
	byte = posp->byte - posp->offset;
	lin = posp->line - posp->slabline;
	if (lin < lineno)
	    while (lin + sl->nlines < lineno) {
		byte += sl->len;
		lin += sl->nlines;
		++sl;
	    }
	else
	    while (lin >= lineno) {
		--sl;
		byte -= sl->len;
		lin -= sl->nlines;
	    }
	loadslab(sl);
		/* Caution:  pos may point to cp */
	pos->offset = skiplines(SSPTR(sl->slab),
	    sl == (slabptr) (MAINBUF->addr + cp.slabno) ? cp.offset : 0,
	    cp.slabline, sl->len, sl->nlines, lin = lineno - lin);
	pos->byte = byte + pos->offset;
	if (pos->offset == sl->len &&
		sl + 1 < (slabptr) (MAINBUF->addr + MAINBUF->len))
	    pos->offset = lin = 0, ++sl;
	pos->line = lineno;
	pos->slabno = (ubyte *) sl - MAINBUF->addr;
	pos->slabline = lin;
}

/*
 *	FINDBYTE - Find the position of the given byte.  Does not do any line
 *		counting.
 */

void	findbyte(bigint byteno, posptr pos) {
	int	offset;
	struct posrec	posend;
	slabptr	sl;
	bigint	lin;

	if ((ubigint) byteno > (ubigint) MAINBUF->nbytes)
	    int_raise("Internal error--findbyte");
	posend.line = MAINBUF->nlines;
	posend.byte = MAINBUF->nbytes;
	posend.slabno = MAINBUF->len - sizeof(slabrec);
	sl = (slabptr) (MAINBUF->addr + posend.slabno);
	posend.offset = sl->len;
	posend.slabline = sl->nlines;
	*pos = *(posptr) closest(byteno, 0, &pos0, cp.byte, &cp,
	    MAINBUF->nbytes, &posend);
	lin = pos->line - pos->slabline;
	lin -= posadjust(byteno - pos->byte, pos);
	loadslab(sl = (slabptr) (MAINBUF->addr + pos->slabno));
	offset = pos->offset;
	pos->line = lin +
	    (pos->slabline = 2 * offset <= sl->len
		? countlines(SSPTR(sl->slab), offset)
		: sl->nlines
		    - countlines(SSPTR(sl->slab) + offset, sl->len - offset));
	pos->byte = byteno;
}

/*
 *	FIRST	Move to the first nonwhite character of the line.  Assumes that
 *		we are already at the first character of the line.
 */

void	first(posptr pos) {
	int	col	= 0;
	ubyte far *p;

	p = seekpos(pos);
	while (*p == ' ' || *p == '\t') {
	    ++col;
	    ++p;
	    if (p >= sk_end) p = seeknext();
	}
	if (*p == '\n' && col > 0) --col;
	(void) posadjust(col, pos);
}

/*
 *	CURRCHAR - Returns the current character.
 */

ubyte	currchar(void) {
	register slabptr sl = (slabptr) (MAINBUF->addr + cp.slabno);

	loadslab(sl);
	return SSPTR(sl->slab)[cp.offset];
}

/*
 *	COLOF	Get column number of given position.
 */

int	colof(const struct posrec *pos) {
	struct posrec	pos1;
	int	col;
	ubyte	c;

	findline(pos->line, &pos1);
	col = xslen(&pos1, pos->byte, 0);
	c = currchar();
	if (c != '\n') col += clen(c, col) - 1;
	return col;
}

/*
 *	SETCOL	Set column number for cursor direction keys.
 */

void	setcol(void) {

	curcol = colof(&cp);
}

/*
 *	SETCP	Set CP to point to the first nonwhite character of the given
 *		line.
 */

void	setcp(bigint lineno) {

	findline(lineno, &cp);
	first(&cp);
	setcol();
}

/*
 *	RING_RAISE - Ring bell and raise exception.
 */

void	ring_raise(void) {

	beep();
	raise_err();
}

/*
 *	OPENUP	Add a line at the bottom of the screen.
 */

void	openup(void) {

	if (botlines < window1) ++botlines;
	mvup(window1 - botlines, window1, 1);
	botcurs -= COLS;
}

/*
 *	BOTCO	Output a character on the bottom line.
 */

void	botco(ubyte c) {
	int	wid;

	if (!visual) (void) write(1, &c, 1);
	else {
	    wid = clen(c, botcurs);
	    if (botcurs + wid > pos2580) openup();
	    (void) writes(&c, 1, (window1 - botlines) * COLS, botcurs, botattr);
	    botcurs += wid;
	    putcur(botcurs);
	}
}

/*
 *	BOTNEWLINE   Print a new line on the terminal.
 */

void	botnewline(void) {
	if (!visual)
#ifdef	BINARY
	    (void) write(1, "\r\n", 2);
#else
	    botco('\n');
#endif
	else botcurs = pos2580;
}

/*
 *	BOTEND	Print a new line in ex mode, or a space in visual mode.
 */

void	botend(void) {
	if (!visual) botnewline();
	else botco(' ');
}

/*
 *	BOTPUTS	Output a string on the bottom line.
 */

void	botputs(const char *s) {
	int	wid;
	int	len;

	len = strlen(s);
	if (s[len - 1] == '\n') --len;
	if (!visual) (void) write(1, s, len);
	else {
	    wid = slen(s, len, botcurs);
	    while (botcurs + wid > pos2580) openup();
	    (void) writes(s, len, (window1 - botlines) * COLS, botcurs,
		botattr);
	    botcurs += wid;
	    putcur(botcurs);
	}
	if (s[len]) botend();
}

/*
 *	BOTPUTD	Output an integer on the bottom line.
 */

void	botputd(long i) {
	char	s[33];

	ltoa(i, s, 10);
	botputs(s);
}

/*
 *	ERRMES	Display error message.
 */

void	errmes(const char *s) {

	botattr = ATTRHIGH;
	botputs(s);
	botattr = ATTRNORM;
}

/*
 *	MSG_RAISE - Print the message and raise an exception.
 */

void	msg_raise(const char *s) {

	errmes(s);
	raise_err();
}

/*
 *	INT_RAISE - Beep, print the message, and raise an exception.
 */

static	void	int_raise(const char *s) {

	beep2();
	errmes(s);
	raise_err();
}

/*
 *	BOTPERROR - perror() for messages at the bottom of the screen.
 */

void	botperror(char *msg) {

	botattr = ATTRHIGH;
	if (msg != Null) botputs(msg);
	botputs(errno >= sys_nerr ? "Unknown error" : sys_errlist[errno]);
	botattr = ATTRNORM;
	botend();
}

/*
 *	CHKBOTTOM - Check for bottom line overflow.
 */

void	chkbottom(void) {
	int	i;

	if (botlines > 1) {
	    botcurs = pos2580;
	    errmes("[Hit return to continue]");
	    (void) inkey_c();
	    botnewline();
	    if (!visual) {
		invid(vidcompat);
		screengood = False;
	    }
	    if (screengood) {
		for (i = ldisp;
		    i > 0 && linedata[i].lrow > window1 - botlines;
		    --i) ;
		filldisp(i, ldisp);
		dispend();
	    }
	    clearrows(window, window1);
	}
	else if (botlines == 1)
	    botdirty = True;

	botlines = 0;
	botcurs = pos2580;
}

/*
 *	ISLETTER - Return true if the character is a letter, digit, or
 *		underscore.
 */

Boolean	isletter(ubyte c) {

	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
	    (c >= '0' && c <= '9') || c == '_';
}

/*
 *	ISOTHER	Return true if the character is not a letter, digit, underscore,
 *		or white space.
 */

Boolean	isother(ubyte c) {

	return (c != '\n' && c != ' ' && c != '\t' && !isletter(c));
}

/*
 *	GPUNJUNK - Remove deleted characters from screen (used in conjunction
 *		with GPINPUT, next).
 */

static	void	gpunjunk(void) {

	putcur(gpi_col);
	while (gpi_coljunk > gpi_col) {
	    --gpi_coljunk;
	    (void) writes(" ", 1, gpi_colbase, gpi_coljunk, ATTRNORM);
	}
}

/*
 *	GPISCROLL - Scroll gpirec by that many characters.
 */

void	gpiscroll(int j) {

	gpi_colbase -= j;
	gpi_col0 -= j;
	gpi_col -= j;
	gpi_coljunk -= j;
}

/*
 *	GPINPUT	General purpose input routine.  Takes care of a lot of things.
 *		Return conditions:
 *			!gpi_first && deleting character/word/line;
 *			delete character/word/line at beginning of line;
 *			carriage return;
 *			end key;
 *			normal character moves past end of allowed area.
 */

void	gpinput(void) {
	int	col1;

    for (;;) {
	if (gpi_key == '\n') return;
	if (gpi_key == X_END) break;
	if (gpi_key >= X_CHAR) {	/* delete something */
	    if (gpi_key == X_CHAR) {
		if (gpi_ptr == gpi_ptr0) return;
		--gpi_ptr;
	    }
	    else {	/* repeatable operation */
		if (gpi_key == X_LINE) gpi_ptr = gpi_ptr0;
		else {	/* deleting word */
		    static	Boolean	(*is_let)(ubyte);

		    if (gpi_key == X_WORD) {
			while (gpi_ptr != gpi_ptr0) {
			    --gpi_ptr;
			    if (*gpi_ptr != ' ' && *gpi_ptr != '\t') {
				is_let = isletter(*gpi_ptr) ? isletter
				    : isother;
				gpi_key = X_WORD2;
				++gpi_ptr;
				break;
			    }
			}
		    }
		    if (gpi_key == X_WORD2) {
			while (gpi_ptr != gpi_ptr0) {
			    --gpi_ptr;
			    if (!(*is_let)(*gpi_ptr)) {
				gpi_key = X_CHAR;
				++gpi_ptr;
				break;
			    }
			}
		    }
		}
		if (gpi_ptr == gpi_ptr0 && !gpi_first) return;
	    }
	    gpi_col = gpi_col0 + slen(gpi_ptr0, gpi_ptr - gpi_ptr0,
		    gpi_col0 - gpi_colbase);
	    gpunjunk();		/* if the option is set */
	}
	else {	/* normal character */
	    if (gpi_ptr >= gpi_maxptr) return;
	    col1 = gpi_col + clen((char) gpi_key, gpi_col - gpi_colbase);
	    if (col1 > gpi_colend) return;
	    *gpi_ptr = (ubyte) gpi_key;
	    (void) writes(gpi_ptr, 1, gpi_colbase, gpi_col, ATTRNORM);
	    ++gpi_ptr;
	    if (gpi_coljunk < col1) gpi_coljunk = col1;
	    if (gpi_col - gpi_colbase < marginbell &&
		col1 - gpi_colbase >= marginbell) ting();
	    gpi_col = col1;
	}
	putcur(gpi_col);
	gpi_key = inkey_i();
    }
    gpunjunk();
}

/*
 *	IN24	Input line on line 24.  Returns length of string.  Escape causes
 *		an exception to be raised.
 */

int	in24(char c, ubyte *instring) {

	botco(c);
	botcurs = pos2580;
	botlines = 0;
	botdirty = True;
	gpi_ptr0 = gpi_ptr = instring;
	gpi_maxptr = instring + COLS;
	gpi_first = True;
	gpi_key = inkey_i();
	gpi_colbase = pos2480;
	gpi_col = gpi_col0 = pos2480 + 1;
	gpi_colend = pos2580;
	gpi_coljunk = 0;
	for (;;) {
	    gpinput();
	    if (gpi_key == X_END || gpi_key == '\n') {
		gpunjunk();
		putcur(pos2480);
		return gpi_ptr - instring;
	    }
	    if (gpi_ptr == instring) {
		gpunjunk();
		raise_err();
	    }
	    beep();	/* if normal character, this means hit end of screen */
	    gpi_key = inkey_i();
	}
}

/*
 *	CHKREP	Process repetition count.  First parameter is next character to
 *		process; second character is rep. count.
 */

void	chkrep(char *cp, bigint *rep) {
	char	c1;
	bigint	rep1;

	c1 = *cp;
	if (c1 >= '1' && c1 <= '9') {
	    rep1 = 0;
	    do {
		rep1 = rep1 * 10 + c1 - '0';
		c1 = inkey_c();
	    }
	    while (c1 >= '0' && c1 <= '9');
	    *cp = c1;
	    *rep = rep1;
	}
}

/*
 *	GETNUM	Get decimal number from command line.
 */

bigint	getnum(char **pp) {
	bigint	val;
	int	i;

	val = 0;
	while ((i = **pp - '0') >= 0 && i <= 9) {
	    ++(*pp);
	    val = val * 10 + i;
	}
	return val;
}

/*
 *	GETLETTER - Get letter argument for ", `, or '.  Return 0 if the
 *		keystroke matches the argument.
 */

int	getletter(ubyte c) {
	ubyte	c1;

	c1 = inkey_1();
	if (c1 >= 'a' && c1 <= 'z') return c1 - ('a' - 1);
	if (c1 != c) ring_raise();
	return 0;
}

/*
 *	MARKHERE - Save the current location in the mark number  I.
 */

void	markhere(int i) {

	markpos[i].buffer = MAINBUF;
	markpos[i].byte = cp.byte;
}
