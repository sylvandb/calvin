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
 *	YANK	Make a copy of some text.
 */

static	void	yank(bufptr buffer, posptr pos1, posptr pos2, Boolean linebuf) {
	int	slabno, slabend;
	slabptr	sl;
	int	skip;
	int	len;
	int	nlines;

	freebuf(buffer);
	buffer->linebuf = linebuf;
	slabno = pos1->slabno;
	slabend = pos2->slabno;
	skip = pos1->offset;
	nlines = pos1->slabline;
	for (; slabno <= slabend; slabno += sizeof(slabrec)) {
	    sl = (slabptr) (MAINBUF->addr + slabno);
	    if (slabno < slabend) {
		len = sl->len - skip;
		nlines = sl->nlines - nlines;
	    }
	    else {
		len = pos2->offset - skip;
		nlines = pos2->slabline - nlines;
	    }
	    loadslab(sl);
	    if (buffer->slabs)
		addslab(buffer, SSPTR(sl->slab) + skip, len, nlines);
	    else addbytes(buffer, SSPTR(sl->slab) + skip, len, nlines);
	    skip = nlines = 0;
	}
}

/*
 *	FIXYBUF	Fix buffer of last yanked text.
 */

void	fixybuf(void) {
	struct posrec	pos1, pos2;

	if (ybuf != Null) {
	    findbyte(ybyte1, &pos1);
	    findbyte(ybyte2, &pos2);
	    yank(ybuf, &pos1, &pos2, ylines);
	    ybuf = Null;
	}
}

/*
 *	YANK_CMD - Process yank command.
 */

void	yank_cmd(bigint rep, bufptr buffer, char fstloc) {
	struct locrec	loc;
	struct posrec	pos1, pos2;
	Boolean	wrap;
	int	newcol;

	if (ybuf == buffer) ybuf = XBUF1 + (ubuf == XBUF1);
	if (ubuf == buffer) ubuf = mvbuf(XBUF1 + (ybuf == XBUF1), ubuf);
	chkrep(&fstloc, &rep);
	parsloc(fstloc, &loc, 'y');
	doloc(rep, &loc, &pos2, 1, 'y', &wrap, &newcol);
	if (wrap) ring_raise();
	pos1 = cp;
	if (loc.lineloc ? (pos1.line > pos2.line) : (pos1.byte > pos2.byte)) {
			/* if moved backwards */
	    pos1 = pos2;
	    pos2 = cp;
	}
	if (loc.lineloc) {
	    findline(pos1.line, &pos1);
	    findline(pos2.line + 1, &pos2);
	}
	if (loc.ltype == '%') (void) posadjust(1, &pos2);
	yank(buffer, &pos1, &pos2, loc.lineloc);
}

/*
 *	PRE_CHANGE - Prepare for change and '!' filters.
 */

void	pre_change(bigint rep, struct locrec *loc, posptr pos, char cmd) {
	Boolean	wrap;
	int	newcol;

	doloc(rep, loc, pos, 1, cmd, &wrap, &newcol);
	if (wrap) ring_raise();
	cbyte1 = cp.byte;
	if (loc->lineloc) {
	    bigint line1 = cp.line;
	    bigint line2 = pos->line;

	    if (pos->line < cp.line) {	/* if moved backwards */
		line1 = pos->line;
		line2 = cp.line;
	    }
	    findline(line1, &cp);
	    findline(line2 + 1, pos);
	}
	else if (pos->byte < cp.byte) {	/* character motion, moved backwards */
	    struct posrec pos1;

	    pos1 = *pos;
	    *pos = cp;
	    cp = pos1;
	}
	if (loc->ltype == '%') (void) posadjust(1, pos);
}

/*
 *	CHG_CMD	Implement change and delete commands.  The third argument may
 *		also be a dot.
 */

void	chg_cmd(bigint rep, bufptr buffer, char cmd, char fstloc) {
	struct locrec	loc;
	struct posrec	pos;

	if (fstloc == '.') loc = dot.loc;
	else {
	    chkrep(&fstloc, &rep);
	    parsloc(fstloc, &loc, cmd);
	}
	pre_change(rep, &loc, &pos, cmd);
	if (cp.line < baslin && cmd == 'c') rollfind();
	if (fstloc == '.') dochange(XBUF1, buffer, &pos, True, cmd == 'c');
	else {
	    if (cmd == 'c') {
		insmode(&pos, loc.lineloc,
		    loc.lineloc ? "INPUT MODE" : "CHANGE MODE");
		dochange(XBUF1, buffer, &pos, False, True);
	    }
	    else {
		if (ybuf == XBUF1) ybuf = Null;
		freebuf(XBUF1);
		XBUF1->linebuf = loc.lineloc;
		dochange(XBUF1, buffer, &pos, True, False);
	    }
	    dot.cmd = cmd;
	    dot.loc = loc;
	}
	dot.buffer = buffer;
	dot.rep = rep;
}

/*
 *	BANG_CMD - Filter the text through a DOS command.
 */

void	bang_cmd(bigint rep, bufptr buffer, Boolean dotting) {
	char	*cmd	= "!";
	char	str[81];
	struct locrec	loc;
	struct posrec	pos;

	if (!dotting) {
	    ubyte fstloc = inkey_c();

	    chkrep(&fstloc, &rep);
	    parsloc(fstloc, &loc, '!');
	    str[in24('!', str)] = '\0';
	    if (*str == '\0') return;
	    cmd = str;
	}
	pre_change(rep, &loc, &pos, 'c');
	bangfilter(cmd, &cp, &pos, XBUF1, False, True);
	if (ybuf == XBUF1) ybuf = Null;
	dochange(XBUF1, buffer, &pos, True, False);
	dot.cmd = '!';
	dot.buffer = buffer;
	dot.rep = rep;
	dot.loc = loc;
}

/*
 *	OPE_CMD	Process open command.  Inserts before this line.
 */

void	ope_cmd(bigint lineno) {

	cbyte1 = cp.byte;
	findline(lineno, &cp);
	insmode(&cp, True, "OPEN MODE");
	dochange(XBUF1, XBUF2, &cp, False, True);
	dot.buffer = XBUF1;		/* '.' uses 'p' or 'P' */
}

/*
 *	INS_CMD	Process the insert mode commands.  The first parameter is 'a' or
 *		'A' or 'i' or 'I'; the second parameter indicates whether we are
 *		redoing a command.
 */

void	ins_cmd(char cmd, Boolean dotting) {
	Boolean	app	= False;

	cbyte1 = cp.byte;
	switch (cmd) {
	    case 'A':
		findline(cp.line + 1, &cp);
		(void) posadjust(-1, &cp);
		--cp.line;
		--cp.slabline;
		app = True;
		break;
	    case 'a':
		if (currchar() != '\n') (void) posadjust(1, &cp);
		app = True;
		break;
	    case 'I':
		findline(cp.line, &cp);
		first(&cp);
		break;
	    default: ;	/* 'i' */
	}
	if (!dotting) insmode(&cp, False, app ? "APPEND MODE" : "INSERT MODE");
	dochange(XBUF1, XBUF2, &cp, dotting, True);
	dot.cmd = cmd;
}

/*
 *	PUT_CMD	Put command.  We assume that buffer != XBUF2.
 */

void	put_cmd(bufptr buffer, Boolean after) {

	if (ybuf == buffer) fixybuf();
	if (buffer->addr == Null) return;
	cbyte1 = cp.byte;
	if (buffer->linebuf) findline(cp.line + after, &cp);
	else if (after && currchar() != '\n') (void) posadjust(1, &cp);
	dochange(buffer, XBUF2, &cp, True, !buffer->linebuf);
	rollfind();
	dot.cmd = after ? 'p' : 'P';
	dot.buffer = buffer;
}

/*
 *	REPL_CMD - Replace a character.
 */

void	repl_cmd(int rep, Boolean dotting) {
	int	i;

	if (!dotting) {
	    doshowmode("REPLACE 1 CHAR");
	    dot.rchar = inkey_1();
	    doshowmode(nomode);
	}
	/* if (rep == 0) rep = 1;  (done automatically) */
	if (ybuf == XBUF1) ybuf = Null;
	freebuf(XBUF1);
	if (dot.rchar == '\n') addbytes(XBUF1, &dot.rchar, 1, 1);
	else
	    for (i = rep; i > 0; --i)
		addbytes(XBUF1, &dot.rchar, 1, 0);
	parsloc(' ', &dot.loc, 'c');
	chg_cmd(rep, XBUF2, 'c', '.');
	dot.cmd = 'r';
	dot.rep = rep;
}

/*
 *	JOIN_CMD - Join two lines.
 */

void	join_cmd(void) {
	struct posrec	pos2;

	if (cp.line >= MAINBUF->nlines - 1) ring_raise();
	if (ybuf == XBUF1) ybuf = Null;
	freebuf(XBUF1);
	addbytes(XBUF1, " ", 1, 0);
	cbyte1 = cp.byte;
	findline(cp.line + 1, &cp);
	pos2 = cp;
	(void) posadjust(-1, &cp);
	--cp.line;
	--cp.slabline;
	first(&pos2);
	dochange(XBUF1, XBUF2, &pos2, True, False);
	dot.cmd = 'J';
}

/*
 *	UNDO	Undo last command.
 */

void	undo(void) {
	struct posrec	pos2;
	Boolean	old_old_modif = old_modif;
	int	old_old_curcol = old_curcol;
	Boolean	old_old_eolateof = old_eolateof;

	if (ybuf == Null) ring_raise();
	if (cp.byte + 1 < ybyte1 || cp.byte > ybyte2) markhere(0);
	findbyte(ybyte1, &cp);
	findbyte(ybyte2, &pos2);
	dochange(ubuf, ybuf, &pos2, True, Maybe);
	modif = old_old_modif;
	curcol = old_old_curcol;
	eolateof = old_old_eolateof;
	rollfind();
}

/*
 *	MOTION	Process motion command.
 */

void	motion(bigint rep, char c) {
	struct locrec	loc;
	struct posrec	pos;
	Boolean	wrap;

	parsloc(c, &loc, ':');
	doloc(rep, &loc, &pos, 0, ' ', &wrap, &curcol);
	if (wrap) botputs("(wrap)\n");
	if (loc.lineloc) {
	    findline(pos.line, &pos);
	    first(&pos);
#ifdef	CHECK
	    checkpos(&pos);
#endif
	}
	if (strchr("/?nNGHML%`'", loc.ltype) != Null &&
		pos.byte != cp.byte && (!loc.lineloc || pos.line != cp.line))
	    markhere(0);
	cp = pos;
	if (curcol < 0) setcol();
	rollfind();
}
