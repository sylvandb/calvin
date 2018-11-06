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

#define	EXTERN
#define	INIT(x)	=x

#include "vi.h"

/*
 *	COLON	Process "colon" command.
 */

static	void	colon(void) {
	char	str[81];

	str[in24(':', str)] = '\0';
	excommand(str);
}

int	cdecl	main(int largc, char **largv) {
	char	*plus	= Null;
	bigint	rep;
	bufptr	buffer;
	Boolean	dotting;
	char	c, c1;
	char	*p;

	dot.cmd = '.';
	bufspace = malloc(YKSPACE);
	lastbuf.addr = bufspace + YKSPACE;
	lastbuf.next = lastbuf.prev = &lastbuf;
	if ((p = getenv("EXINIT")) != Null)
	    if (!setjmp(errbuf))
		excommand(p);
	/*
	 *	parse command line
	 */
	argc = largc - 1;
	argv = largv + 1;
	if (**argv == '+') {
	    plus = *argv + 1;
	    --argc;
	    ++argv;
	}
	invid(vidcompat);
#ifdef	DEFAULTDIR
	initswap();
#endif
	startup = False;
	ex_version();
	openup();
	botlines = 1;
	if (argc > 1) {
	    botputd(argc);
	    botputs(" files to edit\n");
	}
	/*
	 *	read in file
	 */
	if (argc >= 1) {
	    if (!setjmp(errbuf))
		excommand("rew");
	}
	else {
	    MAINBUF->slabs = True;
	    addbytes(MAINBUF, "\n", 1, 1);
	    /* cp = 0 by default */
	    dispset();
	}
	if (plus != Null)
	    if (!setjmp(errbuf))
		excommand(*plus != '\0' ? plus : "$");
	if (setjmp(errbuf)) {
	    doshowmode(nomode);
	    botnewline();
	}
	heapfull = False;
	for (;;) {		/* loop over commands */
	    chkbottom();
	    if (!screengood) {
		if (baslin >= MAINBUF->nlines) baslin = MAINBUF->nlines - 1;
		lrowset(baslin);
		rollfind();
		screengood = True;
		filldisp(0, ldisp);
		dispend();
	    }
#ifdef	CHECK
	    loadslab((slabptr) (MAINBUF->addr + cp.slabno));
	    if (countlines(SSPTR(((slabptr) (MAINBUF->addr + cp.slabno))->slab),
		cp.offset) != cp.slabline) beep2();
#endif
	    putcur(linedata[cp.line - baslin].lrow * COLS + colof(&cp));
	    if (botdirty) {
		inkey_wait();
		if (!visual) {
		    invid(vidcompat);
		    screengood = False;
		}
		clearrows(window, window1);
		botdirty = False;
	    }
	    c = inkey_c();
	    rep = 0;
	    buffer = ZEROBUF;
	    dotting = False;
	    chkrep(&c, &rep);
	    if (c == '"') {
		buffer = buffers + getletter('a');
		c = inkey_c();
	    }
	    chkrep(&c, &rep);
	    if (c == '.') {
		c = dot.cmd;
		if (rep == 0) rep = dot.rep;
		if (buffer == ZEROBUF) buffer = dot.buffer;
		dotting = True;
	    }
	    if (rep == 0 &&
		    (c == 'r' || c == CONTROL('Y') || c == CONTROL('E')))
		rep = 1;
	    switch (c) {
		case ':':  colon(); break;
		case 'd':
		case 'c':  c1 = '.';
			if (!dotting && (c1 = inkey_c()) == '.') ring_raise();
			chg_cmd(rep, buffer, c, c1);
			break;
		case 'D':  chg_cmd(rep, buffer, 'd', '$'); break;
		case 'C':  chg_cmd(rep, buffer, 'c', '$'); break;
		case 'x':  chg_cmd(rep, buffer, 'd', ' '); break;
		case 'X':  chg_cmd(rep, buffer, 'd', 'h'); break;
		case 's':  chg_cmd(rep, buffer, 'c', ' '); break;
		case 'S':  chg_cmd(rep, buffer, 'c', 'c'); break;
		case '!':  bang_cmd(rep, buffer, dotting); break;
		case 'i':
		case 'a':
		case 'A':
		case 'I':  ins_cmd(c, dotting); break;
		case 'O':  ope_cmd(cp.line); dot.cmd = 'P'; break;
		case 'o':  ope_cmd(cp.line + 1); dot.cmd = 'p'; break;
		case 'r':  repl_cmd(rep, dotting); break;
		case 'J':  join_cmd(); break;
		case 'u':  undo(); break;
		case 'y':  yank_cmd(rep, buffer, inkey_c()); break;
		case 'Y':  yank_cmd(rep, buffer, '$'); break;
		case 'p':  put_cmd(buffer, True); break;
		case 'P':  put_cmd(buffer, False); break;
		case CONTROL('G'):  status(); break;
		case 'm':  markhere(getletter('a')); break;
		case 'z':
		    c = inkey_1();
		    if (strchr("hH\r\n+mM.lL-", c) == Null) ring_raise();
		    if (rep != 0) {
			markhere(0);
			setcp(--rep);
		    }
		    else if (c == '+') {
			rep = baslin + ldisp;
			if (rep == MAINBUF->nlines) --rep;
			setcp(rep);
		    }
		    if (c == 'm' || c == 'M' || c == '.')
			cfroll(cp.line, 11, 0);
		    else if (c == 'l' || c == 'L' || c == '-')
			cfroll(cp.line + 1, window, 0);
		    else cfroll(cp.line, 0, 0);
		    break;
		case 'Z':  if (inkey_1() == 'Z') excommand("x"); break;
		case CONTROL('F'):
		    if (rep > 1)
			while (rep > 1 && ldisp > 3) {
			    lrowfind(baslin + ldisp - 2, 0);
			    --rep;
			}
		    if (ldisp <= 3) cfroll(baslin + 1, 0, rep);
		    else cfroll(baslin + ldisp - 2, 0, rep);
		    break;
		case CONTROL('B'):
		    while (rep > 1 && baslin > 1) {
			if (ldisp <= 1) lrowfind(baslin + 1, window);
			else lrowfind(baslin + 2, window);
			--rep;
		    }
		    if (ldisp <= 1) cfroll(baslin + 1, window, rep);
		    else cfroll(baslin + 2, window, rep);
		    break;
		case CONTROL('D'):
		    if (rep > 0) halfscrn = rep;
		    cfroll(baslin + ldisp, window - halfscrn, 0);
		    break;
		case CONTROL('U'):
		    if (rep > 0) halfscrn = rep;
		    cfroll(baslin, halfscrn, 0);
		    break;
		case CONTROL('E'):  cfroll(baslin + rep, 0, 0); break;
		case CONTROL('Y'):  cfroll(baslin - rep, 0, 0); break;
		case CONTROL('L'):  screengood = False; break;
		case CONTROL('^'):  excommand("e#"); break;
		case '\033':	/* Escape */
		    if (rep == 0 && buffer == ZEROBUF) {
			cshape(full);
			beep();
			cshape(normal);
		    }
		    continue;
		default:  motion(rep, c);	/* motion command */
	    } /* end switch */
	} /* end loop over commands */
}
