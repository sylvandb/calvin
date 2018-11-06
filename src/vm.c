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

/*
 *	Variables common to makeroom and insmode
 */

static	int	cline;		/* cp.line - baslin */
static	int	col00;		/* initial column number */
static	slabseg	ss;		/* next slab to be added to XBUF1 */

/*
 *	MAKEROOM Make room in INSBUF for at least one more character.
 */

static	void	makeroom(void) {
	ubyte	*oldaddr;

	if (XBUF1->slabs) catslab(XBUF1, ss, gpi_maxptr - SSPTR(ss));
	else {
	    XBUF1->nbytes = XBUF1->len;
	    XBUF1->nlines = countlines(XBUF1->addr, XBUF1->len);
	    oldaddr = XBUF1->addr;
	    bufrealloc(XBUF1, XBUF1->len + 1);
	    if (!XBUF1->slabs) {
		gpi_ptr0 = (gpi_ptr0 - oldaddr) + XBUF1->addr;
		gpi_ptr = (gpi_ptr - oldaddr) + XBUF1->addr;
		gpi_maxptr = XBUF1->addr + MAXNEAR;
		if (gpi_maxptr > XBUF1->next->addr)
		    gpi_maxptr = XBUF1->next->addr;
		bufrealloc(XBUF1, gpi_maxptr - XBUF1->addr);
		return;
	    }
	}

	ss = newslab();
	gpi_ptr0 = gpi_ptr = SSPTR(ss);
	gpi_maxptr = gpi_ptr0 + SLABLEN;
	gpi_col0 = gpi_col;
	gpi_first = False;
}

/*
 *	SETSTATS - Set certain variables to correct values:  gpi_ptr0, gpi_col0,
 *		and gpi_first.
 */

static	void
setstats(void) {
	ubyte far *p;
	slabptr	sl;

	gpi_col0 = 0;
	gpi_first = True;
	if (XBUF1->slabs) {
	    gpi_ptr0 = SSPTR(ss);
	    p = farmemrchr(gpi_ptr0, '\n', gpi_ptr - gpi_ptr0);
	    if (p != Null) gpi_ptr0 = p + 1;
	    else {
		for (sl = (slabptr) (XBUF1->addr + XBUF1->len) - 1;;) {
			/* find start of line */
		    oktoswap();
		    loadslab(sl);
		    p = farmemrchr(SSPTR(sl->slab), '\n', sl->len);
		    if (p != Null) {
			++p;
			break;
		    }
		    if (sl == (slabptr) XBUF1->addr) {
			p = SSPTR(sl->slab);
			gpi_col0 = col00;
			break;
		    }
		    --sl;
		}
		for (;;) {
		    gpi_col0 += slen(p, SSPTR(sl->slab) + sl->len - p,
			gpi_col0);
		    ++sl;
		    if (sl >= (slabptr) (XBUF1->addr + XBUF1->len)) break;
		    oktoswap();
		    loadslab(sl);
		    p = SSPTR(sl->slab);
		}
		gpi_ptr0 = SSPTR(ss);
		gpi_first = False;
	    }
	}
	else {	/* !XBUF1->slabs */
	    p = farmemrchr(XBUF1->addr, '\n', gpi_ptr - XBUF1->addr);
	    if (p != Null) gpi_ptr0 = p + 1;
	    else {
		gpi_ptr0 = XBUF1->addr;
		gpi_col0 = col00;
	    }
	}
	gpi_col0 += gpi_colbase;
}

/*
 *	GETACHAR - Get an input character.
 */

static	void	getachar(void) {
	putcur(gpi_col);
	gpi_key = inkey_i();
}

/*
 *	INSMODE	Process insert mode.  Returns the inserted text in XBUF1.  It
 *		manages the screen in such a way that the text starting with  CP
 *		and ending with  ENDP  (inclusive) is deleted (from the screen).
 *		It does not modify the main buffer in any way.
 */

void	insmode(const posptr endp, Boolean linemode, const char *modestr) {
	int	coltail	= MAXINT;
	int	colmax;
	int	col1, col2;
	struct posrec tailend;
	Boolean	hastail	= False;
	jmp_buf errbufsav;
	int	i, j;

	if (ybuf == XBUF1) ybuf = Null;
	freebuf(XBUF1);
	gpi_ptr0 = gpi_ptr = Null;
	makeroom();
	cline = cp.line - baslin;
	i = ldisp;
	if (endp->line - baslin <= ldisp) i = endp->line - baslin - linemode;
	if (linemode) {
	    col00 = 0;
	    if (i >= cline && i < ldisp) {
		coltail = xslen(&linedata[i].lpos, endp->byte - 1, 0);
		if (coltail == 0) coltail = 1;
		coltail += linedata[i].lrow * COLS;
	    }
	}
	else {
	    col00 = xslen(&linedata[cline].lpos, cp.byte, 0);
	    if (i < ldisp) {
		tailend = linedata[i + 1].lpos;
		if (tailend.byte != endp->byte + 1) hastail = True;
		coltail = linedata[i].lrow * COLS +
		    xslen(&linedata[i].lpos, endp->byte, 0);
	    }
	    else findline(endp->line + 1, &tailend);
	}
	if (i >= ldisp) {	/* off the screen */
	    ldisp = cline + 1;
	    linedata[ldisp].lrow = window;
	}
	else {
	    memmove(linedata + cline + 1, linedata + i + 1,
		(ldisp - i) * sizeof(*linedata));
	    ldisp -= i - cline;
	    if (i < cline && makerow(cline) != 0) --cline;	/* open mode */
	}
	gpi_colbase = linedata[cline].lrow * COLS;
	gpi_col = gpi_col0 = gpi_coljunk = gpi_colbase + col00;
	gpi_first = True;
	if (coltail < MAXINT && coltail > gpi_col)
	    (void) writes("$", 1, 0, coltail - 1, ATTRNORM);
	if (!hastail) coltail = MAXINT;	/* and vice versa */
	doshowmode(modestr);
	cshape(half);
	getachar();
	*errbufsav = *errbuf;
	if (setjmp(errbuf)) {	/* if we ran out of space */
	    *errbuf = *errbufsav;
	    screengood = False;
	}
	else for (;;) {
	    colmax = linedata[cline + 1].lrow * COLS;
	    gpi_colend = colmax;
	    if (coltail < colmax) gpi_colend = coltail;
	    gpinput();			/* does most of the work */
	    if (gpi_key >= X_END && gpi_ptr == gpi_ptr0 && !gpi_first) {
			/* back 1 slab*/
		slabptr	sl;

		free_ss(ss);
		loadslab(sl = (slabptr) (XBUF1->addr + XBUF1->len) - 1);
		XBUF1->nbytes -= sl->len;
		XBUF1->nlines -= sl->nlines;
		ss = sl->slab;
		gpi_ptr = gpi_maxptr = SSPTR(ss) + sl->len;
		freefslot(sl->fslot);
		bufrealloc(XBUF1, XBUF1->len - sizeof(slabrec));
		if (!XBUF1->slabs) {	/* if no full slabs left */
		    bufrealloc(XBUF1, gpi_ptr - SSPTR(ss));
		    farmemmove(XBUF1->addr, SSPTR(ss), XBUF1->len);
		    free_ss(ss);
		    gpi_ptr = gpi_maxptr = XBUF1->addr + XBUF1->len;
		}
		setstats();
		continue;
	    }
	    if (gpi_key == '\n') {	/* new line */
		if (gpi_ptr >= gpi_maxptr) makeroom();
		*gpi_ptr++ = '\n';
		++ldisp;
		memmove(linedata + cline + 1, linedata + cline,
		    (ldisp - cline) * sizeof(*linedata));
		++cline;
		j = gpi_col - gpi_colbase;
		if (j != 0) --j;
		j = j / COLS + 1;	/* no. of rows in previous line */
		linedata[cline].lrow += j;
		gpi_ptr0 = gpi_ptr;
		gpi_colbase = linedata[cline].lrow * COLS;
		if (gpi_col < gpi_colbase) clrel();
		if (gpi_colbase >= colmax) {
		    j = makerow(cline);
		    gpiscroll(j);
		    if (hastail) coltail -= j;
		    colmax += COLS - j;
		    if (j != 0) --cline;
		}
		col1 = gpi_col = gpi_col0 = gpi_colbase;
		gpi_first = True;
	    }
	    else if (gpi_key < X_END)	/* process character overflow */
		if (gpi_ptr >= gpi_maxptr) {
		    makeroom();
		    continue;
		}
		else
		    col1 = gpi_col + clen(gpi_key, gpi_col - gpi_colbase);
	    else if (gpi_key == X_END) {
		col1 = col2 = gpi_col;
		if (!hastail) break;
		coltail = -1;		/* force rewriting of tail */
	    }
	    else if (gpi_key == X_LINE) {
		getachar();
		continue;
	    }
	    else {	/* backspace to previous line */
		if (gpi_ptr == (XBUF1->slabs ? SSPTR(ss) : XBUF1->addr) ||
			gpi_col == 0) {
		    beep();
		    getachar();
		    continue;
		}
		--gpi_ptr;
		if (gpi_coljunk < gpi_col) gpi_coljunk = gpi_col;
		memmove(linedata + cline, linedata + cline + 1,
		    (ldisp - cline) * sizeof(*linedata));
		--cline;
		--ldisp;
		gpi_colbase = linedata[cline].lrow * COLS;
		setstats();
		col1 = gpi_col = gpi_col0 + slen(gpi_ptr0, gpi_ptr - gpi_ptr0,
			gpi_col0 - gpi_colbase);
	    }
	    /* col1 is current column position */
	    col2 = hastail ?
		gpi_colbase + xslen(endp, tailend.byte - 1, col1 - gpi_colbase)
		: col1;
	    if (col2 > colmax) {
		do {
		    j = makerow(cline);
		    gpiscroll(j);
		    if (hastail) coltail -= j;
		    col1 -= j;
		    col2 -= j;
		    colmax += COLS - j;
		    if (j != 0) --cline;
		}
		while (colmax < col2);
		gpi_colbase = linedata[cline].lrow * COLS;
	    }
	    if (col1 > coltail) {	/* rewrite tail */
		xwrites(endp, &tailend, gpi_colbase, col1);
		coltail = col1;
	    }
	    if (gpi_key == X_END) break;
	    if (gpi_key > X_END || gpi_key == '\n') getachar();
	}

	if (linemode) {
	    if (gpi_ptr >= gpi_maxptr) makeroom();
	    *gpi_ptr++ = '\n';
	    XBUF1->linebuf = True;
	}
	if (XBUF1->slabs) catslab(XBUF1, ss, gpi_ptr - SSPTR(ss));
	else {
	    bufrealloc(XBUF1, gpi_ptr - XBUF1->addr);
	    XBUF1->nbytes = XBUF1->len;
	    XBUF1->nlines = countlines(XBUF1->addr, XBUF1->len);
	}
	*errbuf = *errbufsav;
	doshowmode(nomode);
	cshape(normal);
	if (!(linemode || hastail || cline < ldisp - 1)) {
		/* tail may have been off screen; redraw it now. */
	    linedata[ldisp].lpos = tailend;
	    col2 = gpi_colbase +
		xslen(endp, tailend.byte - 1, col1 - gpi_colbase);
	    while (col2 > pos2480) {
		j = makerow(cline);
		gpi_colbase -= j;
		col1 -= j;
		col2 -= j;
		--cline;
	    }
	    xwrites(endp, &tailend, gpi_colbase, col1);
	}
	i = gpi_colbase / COLS;
	j = col2;
	if (j != gpi_colbase) --j;
	j = j / COLS + 1;
	if (j * COLS > col2) {
	    putcur(col2);
	    clrel();
	}
	i = linedata[cline + 1].lrow - j;
	if (i > 0) {	/* adjust lrow */
	    mvup(j, window, i);
	    for (j = cline + 1; j <= ldisp; ++j) linedata[j].lrow -= i;
	}
}

/*
 *	UNFRAG	Combine slabno and slabno + 1, if possible.
 */

static	void	unfrag(bufptr buffer, int slabno) {
	slabptr	sl	= (slabptr) (buffer->addr + slabno);

	if (sl + 1 >= (slabptr) (buffer->addr + buffer->len)) return;
	if (sl->len + sl[1].len > SLABMAX) return;
	loadslab(sl + 1);
	loadslab(sl);
	movedata(sl->slab, sl->len, sl[1].slab, 0, sl[1].len);
	free_sl(sl + 1);
	sl->len += sl[1].len;
	sl->nlines += sl[1].nlines;
	sl->dirty = True;
	sl += 2;
	memmove(sl - 1, sl, buffer->addr + buffer->len - (ubyte *) sl);
	bufrealloc(buffer, buffer->len - sizeof(slabrec));
}

#ifdef	CHECK

void	checkmainbuf(bufptr buf, char buf_id) {
	slabptr	sl;
	int	i;
	int	j;
	Boolean	err	= False;

	if (buf->slabs) {
	    int nb = 0;
	    int nl = 0;

	    for (sl = (slabptr) buf->addr, i = 0;
		    sl < (slabptr) (buf->addr + buf->len); ++sl, ++i) {
		loadslab(sl);
		j = countlines(SSPTR(sl->slab), sl->len);
		if (j != sl->nlines) {
		    botputd(sl->nlines);
		    botputs(" S/B ");
		    botputd(j);
		    botputs(" at ");
		    botco(buf_id);
		    botputd(i);
		    botnewline();
		    err = True;
		}
		nb += sl->len;
		nl += j;
	    }
	    if (nb != (int) buf->nbytes) {
		botputd((int) buf->nbytes);
		botputs(" total bytes S/B ");
		botputd(nb);
		botputs(" at ");
		botco(buf_id);
		botnewline();
		err = True;
	    }
	    if (nl != (int) buf->nlines) {
		botputd((int) buf->nlines);
		botputs(" total lines S/B ");
		botputd(nl);
		botputs(" at ");
		botco(buf_id);
		botnewline();
		err = True;
	    }
	}
	else {
	    j = countlines(buf->addr, buf->len);
	    if (j != buf->nlines) {
		botputd(buf->nlines);
		botputs(" S/B ");
		botputd(j);
		botputs(" at ");
		botco(buf_id);
		botnewline();
		err = True;
	    }
	}
	if (err) beep2();
}

#endif	/* CHECK */

/*
 *	DOCHANGE - The basic file changing procedure.  Deletes text from  CP  to
 *		POS2  inclusive and inserts the text in the insertion buffer
 *		(ibuf). Deleted text is placed into the buffer  dbuf.  There
 *		must be an insert buffer; just plain deleting should be done
 *		by using an empty buffer. This procedure assumes that  CP<=POS2.
 *		It optionally fixes the screen.  The cursor is put at the
 *		beginning or end of the added text, depending on the argument
 *		BOTTOM.
 */

void	dochange(bufptr ibuf, bufptr dbuf, posptr pos2, Boolean scfix,
    Boolean bottom) {
	bigint	line1	= cp.line;
	bigint	oldbyte2 = pos2->byte;
	bigint	oldline2 = pos2->line;
	bigint	newline2;
	bigint	bytebias;
	jmp_buf errbufsav;
	int	slabno2	= pos2->slabno;
	slabptr	sl;
	int	slabno;
	int	offset;
	int	slabline;
	int	len;
	markptr	markp;
	int	oldrow1, oldrow2, newrow1, newrow2;
	int	lnend;
	int	i;

#ifdef	CHECK
	checkmainbuf(ibuf, 'i');
#endif
	if (ybuf != dbuf) fixybuf();
	*errbufsav = *errbuf;
	if (setjmp(errbuf)) {	/* if we ran out of space */
	    slabptr sl1;

	    *errbuf = *errbufsav;
	    screengood = False;
	    MAINBUF->nbytes = MAINBUF->nlines = 0;
	    for (sl = sl1 = (slabptr) MAINBUF->addr;
		sl < (slabptr) (MAINBUF->addr + MAINBUF->len); ++sl) {
		if (sl->len == 0) continue;
		if (sl->slab) sl->nlines = countlines(SSPTR(sl->slab), sl->len);
		MAINBUF->nbytes += sl->len;
		MAINBUF->nlines += sl->nlines;
		*sl1++ = *sl;
	    }
	    bufrealloc(MAINBUF, (ubyte *) sl1 - MAINBUF->addr);
	    cp = pos0;
	    findline(line1, &cp);
	    raise_err();
	}
	/*
	 *	do the deletion
	 */
	freebuf(dbuf);
	dbuf->linebuf = ylines = ibuf->linebuf;
	slabno = cp.slabno;
	offset = cp.offset;
	slabline = cp.slabline;
		/* move fragment of first slab */
	if (slabno < slabno2 && offset > 0) {
	    loadslab(sl = (slabptr) (MAINBUF->addr + slabno));
	    addbytes(dbuf, SSPTR(sl->slab) + offset, sl->len - offset,
		sl->nlines - slabline);
	    slabno += sizeof(slabrec);
	    offset = slabline = 0;
	}
	if (slabno < slabno2) {	/* move full slabs in middle */
	    slabify(dbuf);
	    i = dbuf->len;
	    bufrealloc(dbuf, i + slabno2 - slabno);
	    memcpy(dbuf->addr + i, MAINBUF->addr + slabno, slabno2 - slabno);
	    for (sl = (slabptr) (MAINBUF->addr + slabno);
		    sl < (slabptr) (MAINBUF->addr + slabno2); ++sl) {
		dbuf->nbytes += sl->len;
		dbuf->nlines += sl->nlines;
	    }
		/* prevent things from getting junked */
	    bzero(MAINBUF->addr + slabno, slabno2 - slabno);
	    unfrag(dbuf, 0);
	}
	loadslab(sl = (slabptr) (MAINBUF->addr + slabno2));	/* last bit */
	addbytes(dbuf, SSPTR(sl->slab) + offset, pos2->offset - offset,
	    pos2->slabline - slabline);
		/* Some accounting */
	bytebias = ibuf->nbytes - dbuf->nbytes;
	MAINBUF->nbytes += bytebias;
	MAINBUF->nlines += ibuf->nlines - dbuf->nlines;
	if (MAINBUF->nbytes == 0) {
	    botputs("No lines in buffer.");
	    addbytes(ibuf, "\n", 1, 1);
	    ++MAINBUF->nbytes;
	    ++MAINBUF->nlines;
	}
	ybuf = ibuf;
	ybyte1 = cp.byte;
	ybyte2 = cp.byte + ibuf->nbytes;
	newline2 = cp.line + ibuf->nlines;
	/*
	 *	The main buffer now has a gaping hole where the old slabs were.
	 *	Let's go about filling it.
	 */
	if (cp.slabno == slabno2) {	/* small amount was deleted */
	    sl = (slabptr) (MAINBUF->addr + cp.slabno);
	    if (!ibuf->slabs &&
		ibuf->nbytes <= SLABMAX - sl->len + pos2->offset - cp.offset) {
		loadslab(sl);
		movedata(sl->slab, cp.offset + ibuf->len,
		    sl->slab, pos2->offset, sl->len - pos2->offset);
		putdata(sl->slab, cp.offset, ibuf->addr, ibuf->len);
		sl->len += cp.offset + ibuf->len - pos2->offset;
		sl->nlines += cp.slabline + (int) ibuf->nlines - pos2->slabline;
		sl->dirty = True;
	    }
	    else {	/* split this slab into two pieces */
		loadslab(sl = (slabptr) (MAINBUF->addr + slabno2));
		sl->dirty = True;
		bufrealloc(MAINBUF, MAINBUF->len + sizeof(slabrec));
		slabno += sizeof(slabrec);
		slabno2 += sizeof(slabrec);
		sl = (slabptr) (MAINBUF->addr + slabno2);
		memmove(sl, sl - 1, MAINBUF->len - slabno2);
		sl->slab = Null;
		sl->slab = newslab();
		sl->fslot = newfslot();
		/* sl->dirty = True; (inherited from sl[-1]) */
		movedata(sl->slab, pos2->offset, sl[-1].slab, pos2->offset,
		    sl->len - pos2->offset);
	    }
	}
	if (cp.slabno < slabno2) {	/* if more to be done */
	    sl = (slabptr) (MAINBUF->addr + cp.slabno);
	    sl->len = cp.offset;
	    sl->nlines = cp.slabline;
	    sl->dirty = True;
	    if (!ibuf->slabs) {
		if (slabno != cp.slabno) {	/* add to first slab */
		    len = SLABMAX - cp.offset;
		    if (len > ibuf->len) len = ibuf->len;
		    loadslab(sl);
		    putdata(sl->slab, cp.offset, ibuf->addr, len);
		    sl->len += len;
		    sl->nlines += countlines(ibuf->addr, len);
		    prefree(ibuf, len);
		}
			/* add to last slab */
		sl = (slabptr) (MAINBUF->addr + slabno2);
		len = SLABMAX - sl->len + pos2->offset;
		if (len > ibuf->len) len = ibuf->len;
		loadslab(sl);
		movedata(sl->slab, len, sl->slab, pos2->offset,
			i = sl->len - pos2->offset);
		putdata(sl->slab, 0, ibuf->addr + ibuf->len - len, len);
		sl->len = len + i;
		sl->nlines += countlines(ibuf->addr + ibuf->len - len, len) -
		    pos2->slabline;
		sl->dirty = True;
		bufrealloc(ibuf, ibuf->len - len);
			/* if there's more */
		if (ibuf->len != 0) slabify(ibuf);
	    }
	    else {	/* ibuf->slabs is true */
		sl = (slabptr) (MAINBUF->addr + slabno2);
		loadslab(sl);
		movedata(sl->slab, 0, sl->slab, pos2->offset,
		    sl->len -= pos2->offset);
		sl->nlines -= pos2->slabline;
		sl->dirty = True;
	    }
	    i = ibuf->len - (slabno2 - slabno);
	    if (i < 0) memmove(MAINBUF->addr + slabno2 + i,
		MAINBUF->addr + slabno2, MAINBUF->len - slabno2);
	    bufrealloc(MAINBUF, MAINBUF->len + i);
	    if (i > 0) memmove(MAINBUF->addr + slabno2 + i,
		MAINBUF->addr + slabno2, MAINBUF->len - i - slabno2);
	    if (ibuf->len > 0) {
		memcpy(MAINBUF->addr + slabno, ibuf->addr, ibuf->len);
		unfrag(MAINBUF, slabno + ibuf->len - sizeof(slabrec));
	    }
	    unfrag(MAINBUF, cp.slabno);
	}
	bufrealloc(ibuf, 0);	/* don't destroy mark data */
	*errbuf = *errbufsav;
	/*
	 *	set current pointer
	 */
	old_curcol = curcol;
	old_eolateof = eolateof;
	if (ybyte2 == MAINBUF->nbytes) eolateof = True;
	cp = pos0;
	if (bottom == Maybe) findbyte(cbyte1, &cp);
	else {
	    cbyte2 = -3;
	    if (ylines)
		if (!bottom && line1 < MAINBUF->nlines)
		    setcp(line1);
		else cbyte2 = ybyte2 - 2;
	    else if (!bottom) {
		findbyte(ybyte1, &cp);
		if (currchar() == '\n') cbyte2 = ybyte1 - 1;
		else setcol();
	    }
	    else cbyte2 = ybyte2 - 1;
	    if (cbyte2 != -3) {
		if (cbyte2 < 0) cp = pos0;
		else {
		    findbyte(cbyte2, &cp);
		    if (currchar() == '\n') {
			(void) posadjust(+1, &cp);
			++cp.line;
			++cp.slabline;
		    }
		}
		setcol();
	    }
	}
	cbyte2 = cp.byte;
		/* one last bit of housekeeping */
	old_modif = modif;
	modif = True;
	ubuf = dbuf;
#ifdef	CHECK
	checkmainbuf(MAINBUF, 'M');
	checkmainbuf(dbuf, 'd');
#endif
	/*
	 *	fix marks
	 */
	for (markp = markpos; markp < markpos + XtNumber(markpos); ++markp)
	    if (markp->buffer == MAINBUF) {
		if (markp->byte > ybyte1)
		    if (markp->byte >= oldbyte2) markp->byte += bytebias;
		    else {
			markp->buffer = dbuf;
			markp->byte -= ybyte1;
		    }
	    }
	    else if (markp->buffer == ibuf) {
		markp->buffer = MAINBUF;
		markp->byte += ybyte1;
	    }
	/*
	 *	All that remains is to fix the screen.
	 *	First check degenerate cases.
	 */
	if (!scfix) {
	    for (i = 0; i <= ldisp; ++i)
		findline(baslin + i, &linedata[i].lpos);
	    fillend();
	    return;
	}
	if (!screengood) {
	    if (baslin > line1) {
		if (baslin >= oldline2) baslin += newline2 - oldline2;
		else baslin = line1;
	    }
	    return;
	}
	if (line1 > baslin + ldisp) return;
	if (oldline2 < baslin) {
	    baslin += newline2 - oldline2;
	    for (i = 0; i <= ldisp; ++i)
		findline(baslin + i, &linedata[i].lpos);
	    return;
	}
	/*
	 *	Determine what on the screen is still good
	 */
	if (!ylines) {
	    ++oldline2;		/* these now refer to first unchanged lines */
	    ++newline2;
	}
	lnend = oldline2 - baslin < ldisp ? oldline2 - baslin : ldisp;
	oldrow2 = linedata[lnend].lrow;
	lnend = ldisp - lnend;	/* number of good lines after modification */
	if (line1 >= baslin) {
	    oldrow1 = linedata[line1 - baslin].lrow;
	    lrowset(baslin);
	}
	else {
	    oldrow1 = 0;
	    lrowfind(newline2, oldrow2);
	}
	/*
	 *	Now we have:
	 *	oldrow1	Number of possibly unchanged rows at top
	 *	oldrow2	Row number of first unchanged line
	 *	lnend	Lines at end of screen
	 */
	screengood = False;
	rollfind();
	if (baslin > newline2 || baslin + ldisp < line1) return;
	screengood = True;
	/*
	 *	All combinations of moving the top stuff up/down, moving the
	 *	bottom stuff up/down, and not having either of the above may
	 *	occur.  Therefore we move things away from the middle first.
	 */
	newrow1 = line1 < baslin ? 0 : linedata[line1 - baslin].lrow;
	if (newrow1 < oldrow1) mvup(0, oldrow1, oldrow1 - newrow1);
	newrow2 = newline2 > baslin + ldisp ? window :
	    linedata[newline2 - baslin].lrow;
	if (newrow2 >= oldrow2) mvdn(oldrow2, window, newrow2 - oldrow2);
	else {		/* move bottom stuff up */
	    mvup(newrow2, window, oldrow2 - newrow2);
	    filldisp(newline2 - baslin + lnend, ldisp);
	}
	if (newrow1 > oldrow1) {	/* move top stuff down */
	    mvdn(0, newrow1, newrow1 - oldrow1);
	    filldisp(0, line1 - baslin);
	}
	/* fill in the middle */
	filldisp(line1 < baslin ? 0 : line1 - baslin,
	    newline2 - baslin > ldisp ? ldisp : newline2 - baslin);
	fillend();
}
