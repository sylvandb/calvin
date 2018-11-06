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
 *	PARSESEARCH - Parse in preparation for search command.
 *		dest:	where to put parsed string
 *		destlenp: pointer to integer for returning length of the above
 *		p:	points to first character
 *		len:	length of string pointed to by p
 *		c_end:	'/' or '?'
 */

ubyte	*parsesearch(ubyte *dest, int *destlenp, ubyte *p, ubyte *p_end,
    char c_end) {
	ubyte	*dp	= dest;
	ubyte	c;

	while (p < p_end && (c = *p++) != c_end) {
	    if (c == '\\') {
		if (p >= p_end) ring_raise();
		if ((c = *p++) == '0') c = '\0';
	    }
	    *dp++ = c;
	}
	*destlenp = dp - dest;
	return p;
}

/*
 *	PARSLOC	Parse locate request.
 *		c:	first character
 *		loc:	parse record output
 *		cmd:	character for 'c' or 'd' command.
 */

void	parsloc(char c, struct locrec *loc, char cmd) {

	loc->lineloc = False;
	loc->fchar = NOLOC;	/* an impossible value */
	loc->sslen = 0;
	if (c == '/' || c == '?') {
	    ubyte string1[81];
	    ubyte *string1_end = string1 + in24(c, string1);
	    ubyte *p;

	    p = parsesearch(loc->sstr, &loc->sslen, string1, string1_end, c);
	    if (p < string1_end) {
		loc->lineloc = True;
		loc->x.linepart = 0;
		*string1_end = '\0';
		for (;;) {
		    ubyte c1 = *p++;
		    if (c1 == '\0') break;
		    if (c1 == '+' || c1 == '-') {
			int	num = 1;
			if (*p >= '0' && *p <= '9') num = getnum((char **) &p);
			loc->x.linepart += c1 == '-' ? -num : num;
		    }
		    else if (c1 != ' ' && c1 != '\t') ring_raise();
		}
	    }
	}
	if ((c == '/' || c == '?' || c == 'n' || c == 'N') && loc->sslen == 0 &&
	    locdir == 0)
	    msg_raise("No previous regular expression");
	if (c == 'f' || c == 'F' || c == 't' || c == 'T') {
	    ubyte c1 = inkey_1();
	    if (c1 == '\n') c1 = '\r';
	    loc->fchar = c1;
	}
	if ((c == ';' || c == ',') && locchar == NOLOC) ring_raise();
	if (c == '\'' || c == '`') loc->x.mindex = getletter(c);
	if (c == cmd) c = '_';
	if (strchr("G_\r+-HML'", c) != Null) loc->lineloc = True;
	else if (strchr("hjkl \n\b/?nNfFtT;,$0^wbeWBE%|`'\020\016", c) == Null)
		{
	    if (c != '\033') beep();
	    raise_err();
	}
	loc->ltype = c;
}

/*
 *	Local functions for DOSEARCH.
 */

/*
 *	FWDSRCH	Look for the given string.  Returns:
 *		0	if nothing matches.
 *		x	if x characters match at the end of the second string.
 *			The pattern is considered to have an implicit asterisk
 *			at the end.
 *		
 */

static	int	fwdsrch(ubyte far *addr, ubyte far *addr_end,
			ubyte *pat, int patlen) {

	for (;;) {
	    addr = (*mem_chr)(addr, *pat, addr_end - addr);
	    if (addr == Null) return 0;
	    if (patlen > addr_end - addr) patlen = addr_end - addr;
	    if ((*mem_cmp)(addr, pat, patlen) == 0) return addr_end - addr;
	    ++addr;
	}
}

/*
 *	BAKSRCH	Look (backwards) for the given string.  The value returned means
 *		the same as with FWDSRCH.
 */


static	int	baksrch(ubyte far *addr, ubyte far *addr_end,
			ubyte *pat, int patlen) {
	ubyte	*pat_end;

	--patlen;
	pat_end = pat + patlen;
	for (;;) {
	    addr_end = (*memr_chr)(addr, *pat_end, addr_end - addr);
	    if (addr_end == Null) return 0;
	    if (patlen > addr_end - addr) patlen = addr_end - addr;
	    if ((*mem_cmp)(addr_end - patlen, pat_end - patlen, patlen) == 0)
		return addr_end - addr + 1;
	}
}

/*
 *	SEARCH	Do searching operation, starting from CP.  Search string is in
 *		LOCPAT/LOCLEN.
 */

void	dosearch(int dir, posptr pos, Boolean *wrap_p) {
	ubyte far *p	= seekpos(pos);
	int	state	= 0;
	Boolean	found	= False;
	Boolean	lasttime = False;
	int	len;

	*wrap_p = False;
	if (dir > 0)
	    for (++p;;) {
		if (p < sk_end)
		    for (;;) {
			if (state == 0) {
			    state = fwdsrch(p, sk_end, locpat, loclen);
			    break;
			}
			len = loclen - state;
			if (len > sk_end - p) len = sk_end - p;
			if ((*mem_cmp)(p, locpat + state, len) == 0) {
			    state += (state + len >= loclen ? sk_end - p : len);
			    break;
			}
			state = fwdsrch(locpat + 1, locpat + state,
			    locpat, loclen);
		    }
		if (state >= loclen) {
		    state = sk_end - sk_addr - state;
		    found = True;
		    break;
		}
		if (lasttime) break;
		p = seeknext();
		if (p == Null) {
		    if (*wrap_p) break;
		    state = 0;
		    p = seekpos(&pos0);
		    *wrap_p = True;
		}
		if (*wrap_p && sk_byte + sk_len > pos->byte + loclen) {
		    int i;
		    if ((i = (pos->byte - sk_byte) + loclen) <= 0) break;
		    sk_end = sk_addr + i;
		    lasttime = True;
		}
	    }
	else {		/* backwards search */
	    p += loclen - 1;
	    while (p > sk_end) {	/* skip forward some bytes */
		len = p - sk_end;
		p = seeknext();
		if (p == Null) p = sk_end; else p += len;
	    }
	    for (;;) {
		for (;;) {
		    if (state == 0) {
			state = baksrch(sk_addr, p, locpat, loclen);
			break;
		    }
		    len = loclen - state;
		    if (len > p - sk_addr) len = p - sk_addr;
		    if ((*mem_cmp)(p - len, locpat + loclen - state - len, len)
			    == 0) {
			state += (state + len >= loclen ? p - sk_addr : len);
			break;
		    }
		    state = baksrch(locpat + loclen - state,
			locpat + loclen - 1, locpat, loclen);
		}
		if (state >= loclen) {
		    state -= loclen;
		    found = True;
		    break;
		}
		if (lasttime) break;
		p = seekprev();
		if (p == Null) {
		    struct posrec pos1;

		    if (*wrap_p) break;
		    findbyte(MAINBUF->nbytes - 1, &pos1);
		    p = seekpos(&pos1) + 1;
		    state = 0;
		    *wrap_p = True;
		}
		if (*wrap_p && sk_byte < pos->byte) {
		    sk_addr += (pos->byte - sk_byte);
		    sk_byte = pos->byte;
		    if (sk_addr >= sk_end) break;
		    lasttime = True;
		}
	    }
	}
	if (!found) msg_raise("Pattern not found");
	findbyte(sk_byte + state, pos);
}

/*
 *	Local functions for doloc.
 */

/*
 *	LASTBYTE - Returns a pointer to the last byte in the file.
 *		Assumes that sk_byte, sk_slabno, and sk_offset are set for the
 *		last slab in the file.
 */

static	ubyte far *lastbyte(int incl) {
	ubyte far *p;

	p = sk_end - 1;
	if (incl == 0) {
	    if (p <= sk_addr) p = seekprev();
	    if (p == Null) return sk_addr;
	    if (p[-1] != '\n') --p;
	}
	return p;
}

/*
 *	FINDCOL	Skip the given number of columns in  POS.
 */

static	void	findcol(int newcol, int incl, posptr pos) {
	ubyte far *p;
	int	col;

	p = seekpos(pos);
	col = 0;
	for (;;) {
	    if (*p == '\n') {		/* we hit this before eof */
		if (col != 0) p += incl - 1;
		break;
	    }
	    col += clen(*p, col);
	    if (col > newcol) break;
	    if (++p >= sk_end) p = seeknext();
	}
	(void) posadjust((smalint) (p - sk_addr) + sk_byte - pos->byte, pos);
}

/*
 *	POSFIX	Call posadjust and adjust for lines moved, too.
 */

static	void	posfix(ubyte far *p, bigint lines, posptr pos) {

	(void) posadjust((smalint) (p - sk_addr) + sk_byte - pos->byte, pos);
	pos->line += lines;
	pos->slabline += lines;
}

/*
 *	SKIPWHITE - Skip white characters:  set pointer to first nonwhite.
 */

static	ubyte far *skipwhite(ubyte far *p, bigint *linep) {

	if (p != Null)
	    while (*p == ' ' || *p == '\t' || (*p == '\n' && (++*linep, True)))
		if (++p >= sk_end) {
		    p = seeknext();
		    if (p == Null) break;
		}
	return p;
}

/*
 *	SKIPAN	Skip alphanumeric characters:  set pointer to last+1
 *		alphanumeric character.
 */

static	ubyte far *skipan(ubyte far *p) {
	Boolean	(*is_let)(ubyte);

	if (p != Null) {
	    is_let = (isletter(*p) ? isletter : isother);
	    while ((*is_let)(*p))
		if (++p >= sk_end) {
		    p = seeknext();
		    if (p == Null) break;
		}
	}
	return p;
}

/*
 *	SKIPDARK - Skip nonblank characters.  Conventions are the same as for
 *		SKIPAN.
 */

static	ubyte far *skipdark(ubyte far *p) {

	if (p != Null)
	    while (*p != ' ' && *p != '\t' && *p != '\n')
		if (++p >= sk_end) {
		    p = seeknext();
		    if (p == Null) break;
		}
	return p;
}

/*
 *	BAKWHITE - Skip white characters backwards.  Return if hit beginning of
 *		file.
 */

static	ubyte far *bakwhite(ubyte far *p, bigint *linep) {

	if (p != Null)
	    do {
		if (p <= sk_addr) {
		    p = seekprev();
		    if (p == Null) break;
		}
		--p;
	    }
	    while (*p == ' ' || *p == '\t' || (*p == '\n' && (--*linep, True)));
	return p;
}

/*
 *	FORMATCH - Match parentheses forward.
 */

static	void	formatch(ubyte c1, ubyte c2, posptr pos, ubyte far *p) {
	int	level;
	bigint	lines;
	ubyte far *p1;
	ubyte far *p2;

	level = 0;
	lines = 0;
	for (;;) {
	    p1 = farmemchr(p, '\n', sk_end - p);
	    if (p1 == Null) p1 = sk_end;
	    p2 = farmemchr(p, c1, p1 - p);
	    if (p2 == Null) p2 = p1;
	    p = farmemchr(p, c2, p2 - p);
	    if (p == Null) p = p2;
	    if (p < sk_end) {
		if (*p == '\n') ++lines;
		else if (*p == c1) ++level;
		else if (--level == 0) break;
		++p;
	    }
	    if (p >= sk_end) {
		p = seeknext();
		if (p == Null) break;
	    }
	}
	posfix(p, lines, pos);
}

/*
 *	BAKMATCH - Match parentheses backward.
 */

static	void	bakmatch(ubyte c1, ubyte c2, posptr pos, ubyte far *p) {
	int	level;
	bigint	lines;
	ubyte far *p1;
	ubyte far *p2;

	level = 1;
	lines = 0;
	for (;;) {
	    if (p <= sk_addr) {
		p = seekprev();
		if (p == Null) ring_raise();
	    }
	    p1 = farmemrchr(sk_addr, '\n', p - sk_addr);
	    if (p1 == Null) p1 = sk_addr;
	    p2 = farmemrchr(p1, c1, p - p1);
	    if (p2 == Null) p2 = p1;
	    p = farmemrchr(p2, c2, p - p2);
	    if (p == Null) p = p2;
	    if (*p == '\n') --lines;
	    else if (*p == c1 && --level == 0) break;
	    else if (*p == c2) ++level;
	}
	posfix(p, lines, pos);
}

#ifdef	CHECK
/*
 *	CHECKPOS - Check internal consistency of position record.
 */

void	checkpos(posptr pos) {
	int	flags	= 0;

	if ((ubigint) pos->byte >= (ubigint) MAINBUF->nbytes) flags |= 1;
	if ((unsigned int) pos->slabno >= (unsigned int) MAINBUF->len)
	    flags |= 2;
	else {
	    slabptr sl;
	    slabptr sl1;
	    int	b;
	    int	l;

	    loadslab(sl = (slabptr) (MAINBUF->addr + pos->slabno));
	    if ((unsigned int) pos->offset >= (unsigned int) sl->len)
		flags |= 4;
	    else if (countlines(SSPTR(sl->slab), pos->offset) != pos->slabline)
		flags |= 8;
	    b = l = 0;
	    for (sl1 = (slabptr) MAINBUF->addr; sl1 < sl; ++sl1) {
		b += sl1->len;
		l += sl1->nlines;
	    }
	    if (b + pos->offset != (int) pos->byte) flags |= 16;
	    if (l + pos->slabline != (int) pos->line) flags |= 32;
	}
	if (flags) {
	    botputs("Error in motion command:  ");
	    botputd(flags);
	    botnewline();
	    chirp();
	}
}
#endif	/* CHECK */

/*
 *	DOLOC	Do locate operation.  For line operations, it just sets
 *		pos->line.
 *		n:	repetition count
 *		loc:	locate record to do
 *		pos:	position output.  May not be CP.
 *		incl:	number of extra spaces to include for delete/change cmds
 *			(0 if locate, 1 if delete/change/yank.)
 *		cmd:	command letter, ' ' for just moving.
 *		*wrap_p:  TRUE if search wrapped around.
 *		newcol:	New column number for SETCOL.
 */

void	doloc(bigint n, struct locrec *loc, posptr pos, int incl, char cmd,
	Boolean *wrap_p, int *newcolp) {
	int	newcol;
	int	this_locdir;
	char	c;
	Boolean	show;

	*wrap_p = False;
	*pos = cp;
	newcol = -1;
	c = loc->ltype;
	switch (c) {
	    case ';':  c = loctyp; break;
	    case ',':  c = loctyp ^ ('a' ^ 'A'); break;
	    case '/':  locdir = 1; break;
	    case '?':  locdir = -1; break;
	    case 'N':  this_locdir = -locdir; break;
	}
	if (loc->fchar != NOLOC) {
	    locchar = loc->fchar;
	    loctyp = c;
	}
	show = True;
	if (loc->sslen != 0) {
	    loclen = loc->sslen;
	    memcpy(locpat, loc->sstr, loclen);
	    show = False;
	}
	if (n == 0 && c != 'G') n = 1;
	switch (c) {
	    case 'G':  pos->line = (n == 0 ? MAINBUF->nlines : n) - 1; break;
	    case '_':  --pos->line;
	    case '\r':
	    case '+':  pos->line += n; break;
	    case '-':  pos->line -= n; break;
	    case 'H':  pos->line = baslin + n - 1; break;
	    case 'M':  pos->line = baslin + (ldisp + 1) / 2; break;
	    case 'L':  pos->line = baslin + ldisp - n; break;

	    case CONTROL('N'):
	    case '\n':
	    case 'j':  pos->line += n; newcol = curcol; break;
	    case CONTROL('P'):
	    case 'k':  pos->line -= n; newcol = curcol; break;
	    case '$':  pos->line += n - 1; newcol = MAXINT; break;
	    case '|':  newcol = n - 1; break;

	    case ' ':
#ifdef	TEST9
		    if (n == 99999) {
			findbyte(cp.byte - cp.offset, pos);
			break;
		    }
#endif	TEST9
	    case 'l':  {
		    struct posrec pos2;

		    findline(cp.line + 1, &pos2);
		    if (cp.byte + n >= pos2.byte + (incl - 1)) ring_raise();
		    (void) posadjust(n, pos);
		}
		break;
	    case CONTROL('H'):
	    case 'h':  {
		    struct posrec pos2;

		    findline(cp.line, &pos2);
		    if (cp.byte - n < pos2.byte) ring_raise();
		    (void) posadjust(-n, pos);
		}
		break;

	    case '0':  findline(cp.line, pos); break;
	    case '^':  findline(cp.line, pos); first(pos); break;

	    case '/':
	    case '?':
	    case 'n':
		this_locdir = locdir;
	    case 'N':
		if (show) {
		    botco(this_locdir > 0 ? '/' : '?');
		    --botcurs;
		}
		dosearch(this_locdir, pos, wrap_p);
		if (loc->lineloc) pos->line += loc->x.linepart;
		break;

	    case 'f':
	    case 't':  {
		    int i;
		    ubyte far *p;
		    ubyte far *p1;
		    ubyte far *p2;

		    p = seekpos(&cp);
		    if (*p == '\n') ring_raise();
		    if (c == 't') {
			if (++p >= sk_end) p = seeknext();
			if (*p == '\n') ring_raise();
		    }
		    for (i = n; i > 0; --i) {
			if (++p >= sk_end) p = seeknext();
			for (;;) {
			    p1 = farmemchr(p, '\n', sk_end - p);
			    p2 = farmemchr(p, locchar,
				(p1 != Null ? p1 : sk_end) - p);
			    if (p2 != Null) break;
			    if (p1 != Null) ring_raise();
			    p = seeknext();
			}
			p = p2;
		    }
		    if (c == 't') --p;
		    p += incl;
		    (void) posadjust((smalint)(p - sk_addr) + sk_byte - cp.byte,
			pos);
		}
		break;
	    case 'F':
	    case 'T':  {
		    int i;
		    ubyte far *p;
		    ubyte far *p1;
		    ubyte far *p2;

		    p = seekpos(&cp);
		    if (c == 'T') {
			if (p <= sk_addr) p = seekprev();
			if (p == Null || *--p == '\n') ring_raise();
		    }
		    for (i = n; i > 0; --i) {
			if (p <= sk_addr) p = seekprev();
			for (;;) {
			    if (p == Null) ring_raise();
			    p1 = p2 = farmemrchr(sk_addr, '\n', p - sk_addr);
			    if (p2 == Null) p2 = sk_addr;
			    p2 = farmemrchr(p2, locchar, p - p2);
			    if (p2 != Null) break;
			    if (p1 != Null) ring_raise();
			    p = seekprev();
			}
			p = p2;
		    }
		    if (c == 'T') ++p;
		    (void) posadjust((p - sk_addr) + sk_byte - cp.byte, pos);
		}
		break;

	    case '\'':
	    case '`':  {
		    markptr markp = markpos + loc->x.mindex;

		    if (markp->buffer != MAINBUF) ring_raise();
		    findbyte(markp->byte, pos);
		}
		break;

	    case 'w':  {
		    bigint lines = 0;
		    ubyte far *p = seekpos(&cp);
		    int i;

		    if (cp.byte >= MAINBUF->nbytes - 2) ring_raise();
		    for (i = n; i > 0; --i) {
			p = skipan(p);
			if (cmd == 'c' && i == 1) break;
			p = skipwhite(p, &lines);
		    }
		    if (p == Null) {
			--lines;
			p = lastbyte(incl);
		    }
		    posfix(p, lines, pos);
		}
		break;
	    case 'b':  {
		    bigint lines = 0;
		    ubyte far *p = seekpos(&cp);
		    Boolean (*is_let)(ubyte);
		    int i;

		    if (cp.byte == 0) ring_raise();
		    for (i = n; i > 0; --i) {
			p = bakwhite(p, &lines);
			if (p == Null) break;
			is_let = (isletter(*p) ? isletter : isother);
			for (;;) {
			    if (p <= sk_addr) {
				p = seekprev();
				if (p == Null) break;
			    }
			    if (!(*is_let)(p[-1])) break;
			    --p;
			}
		    }
		    posfix(p != Null ? p : sk_addr, lines, pos);
		}
		break;
	    case 'e':  {
		    ubyte far *p = seekpos(&cp);
		    bigint lines = *p == '\n' ? 1 : 0;
		    int i;

		    if (cp.byte >= MAINBUF->nbytes - 2) ring_raise();
		    if (++p >= sk_end) p = seeknext();
		    for (i = n; i > 0; --i) {
			p = skipwhite(p, &lines);
			p = skipan(p);
		    }
		    if (p == Null) {
			--lines;
			p = lastbyte(incl) + 1 - incl;
		    }
		    posfix(p + incl - 1, lines, pos);
		}
		break;
	    case 'W':  {
		    bigint lines = 0;
		    ubyte far *p = seekpos(&cp);
		    int i;

		    if (cp.byte >= MAINBUF->nbytes - 2) ring_raise();
		    for (i = n; i > 0; --i) {
			p = skipdark(p);
			if (cmd == 'c' && i == 1) break;
			p = skipwhite(p, &lines);
		    }
		    if (p == Null) {
			--lines;
			p = lastbyte(incl);
		    }
		    posfix(p, lines, pos);
		}
		break;
	    case 'B':  {
		    bigint lines = 0;
		    ubyte far *p = seekpos(&cp);
		    int i;

		    if (cp.byte == 0) ring_raise();
		    for (i = n; i > 0; --i) {
			p = bakwhite(p, &lines);
			if (p == Null) break;
			for (;;) {
			    if (p <= sk_addr) {
				p = seekprev();
				if (p == Null) break;
			    }
			    if (p[-1] == '\n' || p[-1] == ' ' || p[-1] == '\t')
				break;
			    --p;
			}
		    }
		    posfix(p != Null ? p : sk_addr, lines, pos);
		}
		break;
	    case 'E':  {
		    ubyte far *p = seekpos(&cp);
		    bigint lines = *p == '\n' ? 1 : 0;
		    int i;

		    if (cp.byte >= MAINBUF->nbytes - 2) ring_raise();
		    if (++p >= sk_end) p = seeknext();
		    for (i = n; i > 0; --i) {
			p = skipwhite(p, &lines);
			p = skipdark(p);
		    }
		    if (p == Null) {
			--lines;
			p = lastbyte(incl) + 1 - incl;
		    }
		    posfix(p + incl - 1, lines, pos);
		}
		break;

	    case '%':  {
		    ubyte far *p = seekpos(&cp);
		    ubyte c;

		    for (;;) {
			c = *p;
			if (c == '(') {formatch('(', ')', pos, p); break;}
			if (c == ')') {bakmatch('(', ')', pos, p); break;}
			if (c == '[') {formatch('[', ']', pos, p); break;}
			if (c == ']') {bakmatch('[', ']', pos, p); break;}
			if (c == '{') {formatch('{', '}', pos, p); break;}
			if (c == '}') {bakmatch('{', '}', pos, p); break;}
			if (c == '\n') ring_raise();
			if (++p >= sk_end) p = seeknext();
		    }
		}
		break;

	    default:   msg_raise("NYI");
	}

	if (pos->line < 0 || pos->line >= MAINBUF->nlines) ring_raise();
	if (newcol >= 0) {
	    findline(pos->line, pos);
	    findcol(newcol, incl, pos);
	}
	*newcolp = newcol;
#ifdef	CHECK
	if (!loc->lineloc) checkpos(pos);
#endif
}
