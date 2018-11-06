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
 *	CLEN	Return length of character when printed at given column.
 */

int	clen(ubyte c, int col) {

	if (c < ' ')
	    if (c != '\t') return 2;
	    else return tabstop - col % tabstop;
	else if (c <= printable) return 1;
	else return 4;
}

/*
 *	SLEN	Return length of string, starting at given column.
 */

int	slen(const ubyte far *strp, int len, int col) {
	register int	bias;
	register const ubyte far *p;
	const ubyte far	*p_end	= strp + len;

	bias = col;
	for (p = strp; p < p_end; ++p) {
	    ubyte c = *p;

	    if (c < ' ') bias += clen(c, p - strp + bias) - 1;
	    else if (c > printable) bias += 3;
	}
	return len + bias - col;
}

/*
 *	XSLEN	Return last + 1 column of the given string, starting at given
 *		column.  This is a string in MAINBUF.
 */

int	xslen(const posptr pos, bigint byteno, int col) {
	ubyte far *p;
	int	lenleft;
	int	len;

	oktoswap();
	p = seekpos(pos);
	lenleft = byteno - pos->byte;
	while (lenleft > 0) {
	    len = sk_end - p;
	    if (len > lenleft) len = lenleft;
	    col += slen(p, len, col);
	    p += len;
	    lenleft -= len;
	    if (p >= sk_end) {
		oktoswap();
		p = seeknext();
	    }
	}
	return col;
}

/*
 *	LROWFILL - Add more lines at the bottom of the screen, if possible.
 */

static	void	lrowfill(void) {
	struct ldata	*lp	= linedata + ldisp;
	int		row	= lp->lrow;
	slabptr	sl;
	bigint	byt1;		/* byte number of beginning of slab */
	ubyte far *p;
	ubyte far *p1;
	int	len;

	loadslab(sl = (slabptr) (MAINBUF->addr + lp->lpos.slabno));
	byt1 = lp->lpos.byte - lp->lpos.offset;
	p = SSPTR(sl->slab) + lp->lpos.offset;
	while (baslin + ldisp < MAINBUF->nlines) {
	    len = 0;
	    for (;;) {	/* loop over slabs */
		p1 = farmemchr(p, '\n', SSPTR(sl->slab) + sl->len - p);
		len += slen(p,
		    (p1 == Null ? SSPTR(sl->slab) + sl->len : p1) - p, len);
		if (p1 != Null) break;
		byt1 += sl->len;
		loadslab(++sl);
		p = SSPTR(sl->slab);
	    }
	    if (len == 0) len = 1;
	    row += (len - 1) / COLS + 1;
	    if (ldisp > 0 && row > window) break;
	    p = p1 + 1;
	    ++ldisp;
	    ++lp;
	    lp->lrow = row;
	    lp->lpos.slabno = (ubyte *) sl - MAINBUF->addr;
	    lp->lpos.offset = p - SSPTR(sl->slab);
	    lp->lpos.byte = byt1 + lp->lpos.offset;
	}
}

/*
 *	LROWSET	Set up the matrix LROW starting at given line.  Also sets BASLIN
 *		and LDISP.
 */

void	lrowset(bigint lineno) {

	findline(baslin = lineno, &linedata[0].lpos);
	ldisp = 0;
	lrowfill();
};

/*
 *	LROWFIND - Set LROW so that the given line occurs at <= the given row,
 *		but as close as possible.
 */

void	lrowfind(bigint lineno, int row) {
	int	nrows;
	int	ind;

	baslin = lineno - row;
	if (baslin < 0) baslin = 0;
	lrowset(baslin);
	for (;;) {
	    ind = lineno - baslin;
	    if (ind > ldisp) nrows = window - row + ind - ldisp;
	    else nrows = linedata[ind].lrow - row;
	    if (nrows <= 0) break;
	    ind = nrows;
	    if (ind > ldisp) ind = ldisp;
	    while (linedata[ind - 1].lrow >= nrows) --ind;
	    lrowset(baslin + ind);
	}
};

/*
 *	XWRITES	Write the (possibly partial) line on the screen.
 */

void	xwrites(posptr pos1, posptr pos2, int base, int pos) {
	slabptr	sl	= (slabptr) (MAINBUF->addr + pos1->slabno);
	slabptr	sl_end	= (slabptr) (MAINBUF->addr + pos2->slabno);
	int	offset2;
	ubyte far *p;
	ubyte far *p1;

	loadslab(sl);
	p = SSPTR(sl->slab) + pos1->offset;
	if ((offset2 = pos2->offset) <= 0) offset2 = (--sl_end)->len;
	--offset2;
	for (;;) {
	    p1 = SSPTR(sl->slab) + (sl < sl_end ? sl->len : offset2);
	    pos = writes(p, p1 - p, base, pos, ATTRNORM);
	    if (sl >= sl_end) break;
	    ++sl;
	    loadslab(sl);
	    p = SSPTR(sl->slab);
	}
	if (pos == base || pos % COLS != 0) clrel();
}

/*
 *	FILLDISP - Fill in given lines of display.
 */

void	filldisp(int l1, int l2) {
	struct ldata	*lp;
	int	base;

	if (!screengood) return;
	for (lp = linedata + l1; lp < linedata + l2; ++lp) {
	    base = lp->lrow * COLS;
	    xwrites(&lp->lpos, &lp[1].lpos, base, base);
	}
};

/*
 *	DISPEND	Put funny lines at end of screen.
 */

void	dispend(void) {
	ubyte	c;
	int	i;

	if (screengood) {
	    i = linedata[ldisp].lrow;
	    if (i < window) {
		clearrows(i, window);
		c = '~';
		if (baslin + ldisp < MAINBUF->nlines) c = '@';
		for (; i < window; ++i)
		    (void) writes(&c, 1, 0, i * COLS, ATTRNORM);
	    }
	}
};

/*
 *	FILLEND	Add lines at the bottom of the screen, if possible.
 */

void	fillend(void) {
	int	oldisp	= ldisp;

	lrowfill();
	filldisp(oldisp, ldisp);
	dispend();
};

/*
 *	DISPSET	Set display to put current line at middle of screen.
 */

void	dispset(void) {

	lrowfind(cp.line, 11);
	filldisp(0, ldisp);
	dispend();
};

/*
 *	ROLLFIND - Set display to put current line on the screen, rolling if
 *	possible.
 */

void	rollfind(void) {
	bigint	obas;
	int	opos;

	if (cp.line < baslin) {	/* scroll backwards */
	    obas = baslin;
	    lrowset(cp.line);
	    if (obas > baslin + ldisp ||
		     linedata[obas - baslin].lrow >= window - 1) {
		dispset();
		return;
	    }
	    if (screengood) {
		mvdn(0, window, linedata[obas - baslin].lrow);
		filldisp(0, obas - baslin);
	    }
	}
	else {	/* roll up */
	    if (cp.line < baslin + ldisp) return;
	    if (cp.line > baslin + ldisp + window - 2) {
		dispset();
		return;
	    }
	    obas = baslin + ldisp;
	    opos = linedata[ldisp].lrow;
	    lrowfind(cp.line + 1, window);
	    if (baslin >= obas - 1) {
		dispset();
		return;
	    }
	    if (screengood) {
		mvup(0, window, opos - linedata[obas - baslin].lrow);
		filldisp(obas - baslin, ldisp);
	    }
	}
	dispend();
};

/*
 *	CFROLL	Programmed roll, using LROWFIND first.  If rep>0 then redraw the
 *		entire screen.
 */

void	cfroll(bigint lineno, int row, int rep) {
	bigint	obas;
	int	oldisp;
	int	orow;

	if (lineno < 0 || lineno > MAINBUF->nlines ||
		(lineno >= MAINBUF->nlines && row == 0))
	    ring_raise();
	obas = baslin;
	oldisp = ldisp;
	orow = linedata[ldisp].lrow;
	lrowfind(lineno, row);
	if (baslin + ldisp <= obas || obas + oldisp <= baslin || rep > 0)
		 /* no overlap */
	    filldisp(0, ldisp);
	else if (baslin <= obas) {	/* move stuff down */
	    mvdn(0, linedata[ldisp].lrow, linedata[obas - baslin].lrow);
	    filldisp(0, obas - baslin);
	}
	else {				/* move stuff up */
	    mvup(0, window, orow - linedata[obas + oldisp - baslin].lrow);
	    filldisp(obas + oldisp - baslin, ldisp);
	}
	dispend();
	if (cp.line >= baslin + ldisp) setcp(baslin + ldisp - 1);
	if (cp.line < baslin) setcp(baslin);
};

/*
 *	MAKEROW	Create an empty row on the screen, at the given line.
 *		Returns COLS * number of lines scrolled.
 */

int	makerow(int lineind) {
	int	nl;
	int	i;

	if (linedata[lineind + 1].lrow >= window) {	/* scroll it */
	    nl = linedata[1].lrow;
	    memmove(linedata, linedata + 1, ldisp * sizeof(*linedata));
	    for (i = 0; i < ldisp; ++i) linedata[i].lrow -= nl;
	    --ldisp;
	    ++baslin;
	    mvup(0, window, nl);
	    ++linedata[ldisp].lrow;
	}
	else {
	    nl = 0;
	    mvdn(linedata[lineind + 1].lrow, window, 1);
	    for (i = ldisp; i > lineind; --i) ++linedata[i].lrow;
	    if (linedata[ldisp].lrow > window) --ldisp;
	}
	dispend();
	return nl * COLS;
};

/*
 *	DOSHOWMODE - If showmode is set, then put the mode name in the correct
 *		position on the screen.
 */

void	doshowmode(const char *m) {

	if (showmode)
	    (void) writes(m, strlen(m), 0, pos2580 - MODEWID, ATTRNORM);
}
