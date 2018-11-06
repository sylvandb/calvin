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
#include <fcntl.h>
#include <sys/stat.h>
#include <io.h>
#include <errno.h>

#ifdef	THOROUGHSHELL
extern	int	cdecl	system3(const char *command, int fd1, int fd2,
		Boolean thoroughshell);
#else
extern	int	cdecl	system3(const char *command, int fd1, int fd2);
#endif

#define	CPMEOF	26

static	char	*p;
static	char	*pmax;		/* end + 1 of this command */
static	int	ex_argc;
static	bigint	ex_argv[2];
static	Boolean	bang;
static	char	*cmdname;

typedef	char	filename[81];
static	filename edname[2];	/* current and alt file names */
static	bigint	ednumber[2];	/* line number memory for the above */
static	int	edindex	= -1;	/* index of current file name */
static	Boolean	morefiles = False; /* if "n more files ..." issued recently */

/* :set option variables (the local ones) */

static	Boolean	autowrite = False;
#ifdef	BINARY
static	Boolean	binary	= False;
static	Boolean	cpmeof	= False;
#endif
static	Boolean	dosesc	= False;
static	Boolean	ignorecase = False;
#ifdef	THOROUGHSHELL
static	Boolean	thoroughshell = False;
#endif

#ifdef	DEFAULTDIR
char	defaultdir[]	=DEFAULTDIR;
#else
char	defaultdir[]	="";
#endif

/*
 *	ALLOCFILE - Allocate a buffer for the file.  Replaces the system-
 *	provided one.
 */

/*int	filebuffer[174/2+1];

char	*allocfil(void) {
	return (*((int *) filebuffer) == 0) ? (void *) filebuffer : Null;
}
*/

/*
 *	SKIPSPACES - Skip spaces and tabs.
 */

static	void	skipspaces(void) {

	while (*p == ' ' || *p == '\t') ++p;
}

/*
 *	SETPMAX	Find the extent of this ex command.
 */

static	void	setpmax(void) {
	register int	i;
	register char	*p2;

	pmax = p + (i = strlen(p));
	if ((p2 = memchr(p, '|', i)) != Null) pmax = p2;
}

/*
 *	BANGCHECK - Print message and quit unless overridden.
 */

static	void	bangcheck(char *msg) {

	if (!bang) {
	    botattr = ATTRHIGH;
	    botco(' ');
	    botputs(msg);
	    botputs(" (:");
	    botputs(cmdname);
	    botputs("! overrides)");
	    botattr = ATTRNORM;
	    raise_err();
	}
}

/*
 *	EXPECTEND - Issue diagnostic if we're not at the end of a command.
 */

static	void	expectend(void) {

	if (p < pmax) msg_raise("Excess characters at end of command");
}

/*
 *	GETARG	Determine argument.
 */

static	char	*getarg(char *fn, int len) {

	if (len != 0) {
	    if (memchr(fn, ' ', len) != Null || memchr(fn, '\t', len) != Null)
		msg_raise("Too many file names");
	    if (memchr(fn, '*', len) != Null || memchr(fn, '?', len) != Null)
		msg_raise("Wild cards not allowed");
	    memmove(fn - 1, fn, len);
	    --fn;
	    fn[len] = '\0';
	    return fn;
	}
	else {
	    if (edindex < 0) msg_raise("No current filename");
	    return edname[edindex];
	}
}

/*
 *	LINCHAR	Print lines and characters message.
 */

static	void	linchar(bigint nlines, bigint nbytes) {
	botputd(nlines);
	botputs(" line");
	if (nlines != 1) botco('s');
	botputs(", ");
	botputd(nbytes);
	botputs(" character");
	if (nbytes != 1) botco('s');
	botputs(".\n");
}

/*
 *	FDREAD	Read from a given file descriptor into a buffer.  The argument
 *		"show" tells whether to put out the customary message on number
 *		of lines and characters.
 */

static	void	fdread(int fd, bufptr buffer, Boolean show) {
	Boolean	lcl_eolateof;		/* if CR at end of file */
	Boolean	cr_at_end = False;
	bigint	llen;
	int	len;
	slabseg ss = Null;
	jmp_buf errbufsav;

	if (fd != -1) {
	    errno = 0;
	    *errbufsav = *errbuf;
	    if (!setjmp(errbuf)) for (;;) {
		ss = newslab();
		len = farread(fd, SSPTR(ss), SLABLEN);
		if (cr_at_end && (len == 0 || *SSPTR(ss) != '\n'))
		    addbytes(buffer, "\r", 1, 0);
		if (len == 0 || errno != 0) break;
#ifdef	BINARY
		if (!binary) {	/* end-of-line conversion */
		    ubyte far *p;
		    ubyte far *p1;
		    ubyte far *p2;
		    ubyte far *p_end;

		    p = p1 = p2 = SSPTR(ss);
		    p_end = p + len;
		    for (;;) {
			p = farmemchr(p, '\r', p_end - p);
			if (p == Null) p = p_end;
			else if (p >= p_end - 1 || p[1] != '\n') {
			    ++p;
			    continue;
			}
			len = p - p1;
			if (p1 != p2) farmemmove(p2, p1, len);
			p1 = ++p;
			p2 += len;
			if (p1 >= p_end) break;
		    }
		    cr_at_end = False;
		    len = p2 - SSPTR(ss);
		    if (p2[-1] == '\r') {
			cr_at_end = True;
			--len;
		    }
		}
#endif	BINARY
		catslab(buffer, ss, len);
		ss = Null;
	    }
	free_ss(ss);
	llen = tell(fd);
	close(fd);
	*errbuf = *errbufsav;
	if (!heapfull && errno == 0) {	/* trim Control-Z & \n */
#ifdef	BINARY
		register slabptr sl = (slabptr) (buffer->addr + buffer->len);
		ubyte	lastbyte = '\0';
		int	newlen;

		if (binary) {
		    if (buffer->nbytes != 0) {
			--sl;
			loadslab(sl);
			lastbyte = SSPTR(sl->slab)[sl->len - 1];
		    }
		}
		else {
		    while (--sl >= (slabptr) buffer->addr) {
			loadslab(sl);
			while (sl->len > 0 &&
			    (lastbyte = SSPTR(sl->slab)[sl->len - 1])
				== CPMEOF) {
			    --buffer->nbytes;
			    --sl->len;
			}
			if (lastbyte != CPMEOF) break;
			free_sl(sl);
		    }
		    if ((newlen = (ubyte *) (sl + 1) - buffer->addr) <
			    buffer->len)
			bufrealloc(buffer, newlen);
		}
#else	BINARY
		register slabptr sl = (slabptr) (buffer->addr + buffer->len);
		ubyte	lastbyte = '\0';

		if (buffer->nbytes != 0) {
		    --sl;
		    loadslab(sl);
		    lastbyte = SSPTR(sl->slab)[sl->len - 1];
		}
#endif	BINARY
		lcl_eolateof = True;
		if (lastbyte != '\n') {
		    addbytes(buffer, "\n", 1, 1);
		    lcl_eolateof = False;
		}
	    }
	}
	if (heapfull || errno != 0) {
	    freebuf(buffer);
	    MAINBUF->slabs = True;
	    lcl_eolateof = True;
	    if (errno == ENOENT) errno = 0;
	    if (errno != 0) botperror(Null);
	}
	else if (show) {
	    linchar(buffer->nlines, llen);
	    if (!lcl_eolateof) botputs("Incomplete last line.\n");
	}
	if (buffer == MAINBUF) eolateof = lcl_eolateof;
}

/*
 *	FDWRITE	Create and write to a given file descriptor from MAINBUF.
 *		Returns the file descriptor in *fd_ret.
 */

static	void	fdwrite(posptr pos1, posptr pos2, char *arg, int *fd_ret) {
	int	fd;
	slabptr	sl, sl_end;
	int	skip;
	int	len;
#ifdef	BINARY
#define	BUFLEN	512
	int	len1;
	ubyte	buffer[BUFLEN + 1];
	ubyte	*q = buffer;
#endif	BINARY

	*fd_ret = fd = creat(arg, S_IWRITE);
	if (fd == -1) return;
	errno = 0;
	sl = (slabptr) (MAINBUF->addr + pos1->slabno);
	sl_end = (slabptr) (MAINBUF->addr + pos2->slabno);
	skip = pos1->offset;
	for (; sl <= sl_end; ++sl) {
	    len = (sl < sl_end ? sl->len : pos2->offset) - skip;
	    loadslab(sl);
#ifdef	BINARY
	    if (binary) len1 = farwrite(fd, SSPTR(sl->slab) + skip, len);
	    else {
		ubyte far *p;
		ubyte far *p1;
		ubyte far *p2;
		ubyte far *p_end;

		p = p1 = SSPTR(sl->slab) + skip;
		p_end = p + len;
		for (;;) {
		    p2 = farmemchr(p1, '\n', p_end - p1);
		    if (p2 == Null) p1 = p_end;
		    else {
			*p2 = '\r';
			p1 = p2 + 1;
		    }
			/* copy from p to p1 - 1 */
		    if (p1 - p > buffer + BUFLEN - q) {
			if ((len = q - buffer) > 0 &&
			    (len1 = write(fd, buffer, len)) < len) break;
			q = buffer;
		    }
		    if ((len1 = len = p1 - p) > BUFLEN) {
			if ((len1 = farwrite(fd, p, len)) < len) break;
		    }
		    else {
			farmemmove(q, p, len);
			q += len;
		    }
		    if (p2 == Null) break;
		    *p2 = '\n';
		    p = p2;
		}
		if (p2 != Null) *p2 = '\n';
	    }
	    if (errno != 0) return;
	    if (len1 < len) {
		errno = -1;
		return;
	    }
	    skip = 0;
#else	BINARY
	    if (farwrite(fd, SSPTR(sl->slab) + skip, len) < len || errno != 0) {
		if (errno == 0) errno = -1;
		return;
	    }
#endif	BINARY
	}
#ifdef	BINARY
	if (!binary) {
	    if (cpmeof) *q++ = CPMEOF;
	    if (write(fd, buffer, q - buffer) < q - buffer) {
		if (errno == 0) errno = -1;
		return;
	    }
	}
#endif	BINARY
}

/*
 *	BANGFILTER - Filter MAINBUF[pos1..pos2] through the given command and
 *		produce an answer in ANSWBUF.
 */

void	bangfilter(const char *cmd, posptr pos1, posptr pos2, bufptr answbuf,
		Boolean warn, Boolean keep) {
	static	char	oldbang[BANGLEN];
	int	fd1, fd2;	/* file handles */
	char	n1[OPTLEN + 6];		/* input file name */
	char	n2[OPTLEN + 7];		/* output file name */

	if (keep) {
	    if (*cmd != '!') strcpy(oldbang, cmd);
	    else if (*oldbang == '\0')
		msg_raise("No previous command to substitute for !");
	    cmd = oldbang;
	}
	errno = 0;
	fd1 = fd2 = -1;
	if (pos1 != Null) {
	    fdwrite(pos1, pos2, formname(n1, "in"), &fd1);
	    if (errno < 0) msg_raise("Cannot write to pipe file.");
	    if (!errno) lseek(fd1, 0L, SEEK_SET);
	}
	if (!errno) {
	    if (answbuf != Null) fd2 = creat(formname(n2, "out"), S_IWRITE);
	    else {
		endvid();
		botlines = window1;
	    }
	}
	if (!errno) {
	    if (warn) botputs("[No write since last change]\n");
	    pre_shell();
#ifdef	THOROUGHSHELL
	    (void) system3(cmd, fd1, fd2, keep & thoroughshell);
#else
	    (void) system3(cmd, fd1, fd2);
#endif
	}
	if (fd1 >= 0) close(fd1);
	if (pos1 != Null) {
	    int errno_save = errno;

	    unlink(n1);
	    errno = errno_save;
	}
	if (!errno && answbuf != Null) {
	    lseek(fd2, 0L, SEEK_SET);
	    freebuf(answbuf);
	    answbuf->slabs = True;
	    fdread(fd2, answbuf, False);
	}
	if (fd2 >= 0) unlink(n2);
	if (errno) {
	    botperror(Null);
	    raise_err();
	}
}

/*
 *	RDFILE	Read file into buffer.  Returns empty buffer if file not found.
 */

static	void	rdfile(char *arg, bufptr buffer) {
	freebuf(buffer);
	buffer->slabs = True;
	if (*arg == '\0') return;
	botco('"'); botputs(arg); botputs("\":  ");
	fdread(open(arg, O_RDONLY, 0), buffer, True);
}

/*
 *	WTFILE	Write buffer to file.
 */

static	void	wtfile(char *fn, int fnlen, bigint first, bigint last) {
	struct posrec	pos1, pos2;
	char	*arg;
	int	fd;
	bigint	llen;

	findline(first, &pos1);
	findline(last, &pos2);
	if (!eolateof && last >= MAINBUF->nlines) (void) posadjust(-1, &pos2);
	if (fnlen > 0 && *fn == '!') {
	    bangfilter(fn + 1, &pos1, &pos2, Null, False, True);
	    p += strlen(p);	/* that's it for this string of commands */
	    return;
	}
	arg = getarg(fn, fnlen);
	botco('"'); botputs(arg); botputs("\":  ");
	fdwrite(&pos1, &pos2, arg, &fd);
	if (errno == 0) llen = tell(fd);
	if (fd != -1) close(fd);
	if (errno != 0) {
	    int	errnosav = errno;

	    unlink(arg);
	    errno = errnosav;
	    if (errno == -1) msg_raise("Cannot write file");
	    botperror(Null);
	    raise_err();
	}
	linchar(last - first, llen);
	if (first <= 0 && last == MAINBUF->nlines) {
	    edited = True;
	    old_modif = True;
	    modif = False;
	}
}

/*
 *	CMDEDIT	Process :e command.
 */

static	void	cmdedit(char *arg, char *pplus) {
	bigint	lineno;

	fixybuf();
	rdfile(arg, MAINBUF);
	edited = True;
	modif = False;
	if (MAINBUF->len == 0) {	/* file not found or out of memory */
	    addbytes(MAINBUF, "\n", 1, 1);
	    if (errno == 0 && !heapfull && *arg != '\0') botputs("new file.\n");
	    errno = 0;
	    heapfull = False;
	}
	lineno = 0;
	if (edindex >= 0) {
	    ednumber[edindex] = cp.line;
	    if (pplus == Null && strcmp(arg, edname[1 - edindex]) == 0) {
		lineno = ednumber[1 - edindex];
		if (lineno >= MAINBUF->nlines) lineno = MAINBUF->nlines - 1;
	    }
	}
	if (arg == edname[0]) edindex = 0;
	else if (arg == edname[1]) edindex = 1;
	else {
	    edindex = (edindex == 0);
	    strcpy(edname[edindex], arg);
	}
	cp = pos0;	/* don't fool setcp */
	screengood = False;
	setcp(lineno);
	dispset();
}

/*
 *	EX_EDIT	Process :edit command.
 */

static	void	ex_edit(void) {
	char	*pplus = Null;
	int	pluslen;

	if (*p == '+') {
	    pplus = p + 1;
	    p = strchr(p, ' ');
	    if (p == Null) p = pmax;
	    else setpmax();
	    pluslen = p - pplus;
	    if (pluslen == 0) {
		pplus = "$";
		pluslen = 1;
	    }
	    skipspaces();
	}
	if (*p == '#' && pmax == p + 1) {
	    if (edindex < 0 || edname[1 - edindex][0] == '\0')
		msg_raise("No alternate file name");
	    cmdedit(edname[1 - edindex], pplus);
	}
	else cmdedit(getarg(p, pmax - p), pplus);
	if (pplus != Null) {
	    p = pmax - pluslen;
	    memmove(p, pplus, pluslen);
	    pmax = --p;
	}
}

/*
 *	EX_NEXT	:next command.
 */

static	void	ex_next(void) {

	++argindex;
	if (argindex >= argc) msg_raise("No more files");
	cmdedit(argv[argindex], Null);
}

/*
 *	EX_REW	:rewind command.
 */

static	void	ex_rew(void) {

	argindex = -1;
	ex_next();
}

/*
 *	EX_WRITE - Process :write command.
 */

static	void	ex_write(void) {
	if (ex_argc < 2) {
	    ex_argv[1] = MAINBUF->nlines;
	    if (ex_argc < 1) ex_argv[0] = 1;
	}
	wtfile(p, pmax - p, ex_argv[0] - (ex_argv[0] != 0), ex_argv[1]);
}

/*
 *	EX_READ - Process :read command (reads from another file).
 */

static	void	ex_read(void) {

	if (*p == '!') {
	    bangfilter(p + 1, Null, Null, XBUF1, False, True);
	    p += strlen(p);	/* that's it for this string of commands */
	}
	else {
	    rdfile(p, XBUF1);
	    if (XBUF1->len == 0) {
		if (!heapfull && errno == 0) botputs("not found.\n");
		errno = 0;
		heapfull = False;
		return;
	    }
	}
	if (ybuf == XBUF1) ybuf = Null;
	cbyte1 = cp.byte;
	if (ex_argc != 0) --ex_argc;
	findline(ex_argv[ex_argc], &cp);
	XBUF1->linebuf = True;
	dochange(XBUF1, XBUF2, &cp, True, False);
	rollfind();
}

/*
 *	STATUS	Process :file and ^G commands (w/o arguments).
 */

void	status(void) {

	if (edindex >= 0) {
	    botco('"'); botputs(edname[edindex]); botco('"');
	    /* if (!writable) botputs(" [read only]"); */
	    if (!edited) botputs(" [not edited]");
	    if (modif) botputs(" [modified]");
	    botco(':');
	}
	else botputs("No file");
	botputs("  line "); botputd(cp.line + 1);
	botputs(" of "); botputd(MAINBUF->nlines);
	botputs("  --");
	botputd((long) (cp.line + 1) * 100 / MAINBUF->nlines);
	botputs("%--\n");
}

/*
 *	EX_FILE	Process :file command.
 */

static	void	ex_file(void) {

	if (p < pmax) {	/* file argument */
	    if (edindex < 0) edindex = 0;
	    strcpy(edname[edindex], getarg(p, pmax - p));
	    edited = False;
	}
	status();
}
/*
 *	EX_QUIT	Erase funny lines at end of screen and quit.
 */

static	void	ex_quit(void) {
	int	i;

	i = argc - argindex - 1;
	if (i > 0 && !morefiles) {
	    morefiles = True;
	    botattr = ATTRHIGH;
	    botco(' ');
	    botputd(i);
	    botputs(" more file");
	    if (i > 1) botco('s');
	    botputs(" to edit");
	    botattr = ATTRNORM;
	    raise_err();
	}
	i = linedata[ldisp].lrow;
	if (i + botlines < window1) mvup(i, window1, window1 - i - botlines);
	else if (vidcompat == 2) {	/* do a normal scroll */
	    mvup(0, window1, 1);
	    --i;
	}
	putcur((i + botlines) * COLS);
	rmfiles();
	endvid();
	exit(0);
}

/*
 *	EX_WQ	Write and quit.
 */

static	void	ex_wq(void) {

	ex_write();
	ex_quit();
}

/*
 *	EX_X	Process ending command.
 */

static	void	ex_x(void) {

	if (modif) ex_write();
	ex_quit();
}

/*
 *	EX_VERSION - Print copyright message at end of screen.
 */

void	ex_version(void) {

	botattr = ATTRBOLD;
	botputs("Calvin version 2.3.  (C) Copyright 1988-1997 Paul Vojta.");
	botnewline();
	botputs("	For copying conditions, please see the documentation file.");
	botnewline();
	botattr = ATTRNORM;
}

/*
 *	EX_MOVE	Process motion command.
 */

static	void	ex_move(void) {
	if (ex_argc != 0) --ex_argc;
	setcp(ex_argv[ex_argc] - (ex_argv[ex_argc] != 0));
	if (screengood) rollfind();
}

/*
 *	EX_EQUALS - Process "=" command.
 */

static	void	ex_equals(void) {
	if (ex_argc != 0) --ex_argc;
	botputd(ex_argv[ex_argc]);
	botco(' ');
}

/*
 *	BANGIT	Common routine for "!" and :shell commands.
 */

static	void	bangit(char *cmd, Boolean keep) {
	Boolean	warn	= False;

	if (modif)
	    if (autowrite) wtfile(p, 0, 0, MAINBUF->nlines);
	    else warn = True;
	bangfilter(cmd, Null, Null, Null, warn, keep);
}

/*
 *	EX_BANG	Process "!" command.
 */

static	void	ex_bang(void) {
	bangit(p, True);
	p += strlen(p);		/* that's it for this string of commands */
}

/*
 *	EX_SHELL - Process :shell command.
 */

static	void	ex_shell(void) {
	char	cmd[81];
	char	*p;

	strcpy(cmd, getenv(SHELLVAR));
	p = strrchr(cmd, '.');
	if (p != Null) *p = '\0';
	bangit(cmd, False);
}

/*
 *	MATCHSTRING - Find best match from list.
 */

static	char	**matchstring(char **tbl, int tblwid, int tbllen) {
	char	*p0;
	int	len;
	char	**ansptr = Null;
	int	count	= 0;
	int	n;

	p0 = p;
	while (*p >= 'a' && *p <= 'z') ++p;
	len = p - p0;
	if (len == 0) return Null;
	while (--tbllen >= 0) {
	    n = strlen(*tbl);
	    if (n >= len && memcmp(p0, *tbl, len) == 0) {
		if (n == len) return tbl;	/* exact match */
		ansptr = tbl;
		++count;
	    }
	    *((char **) &tbl) += tblwid;
	}
	return count <= 1 ? ansptr : Null;
}

/*
 *	EX_SET	Process :set command.
 */
Boolean	setall	= False;	/* a real kludge */

struct	setrec	{
		char	*setvar;
		enum	{SET_bool, SET_int, SET_str}	set_typ;
		caddr_t	dflt_val;
		void	*val;
		enum	{SETsyn, SETnop, SETall, SETsg, SETic, SETdir, SETesc,
				SETvc}
			set_action;
	} settable[] = {
		{"autowrite",	SET_bool,	False,	&autowrite,	SETnop},
#ifdef	BINARY
		{"binary",	SET_bool,	False,	&binary,	SETnop},
		{"cpmeof",	SET_bool,	False,	&cpmeof,	SETnop},
#endif
		{"directory",	SET_str,	defaultdir, directory,	SETdir},
		{"dosesc",	SET_bool,	False,	&dosesc,	SETesc},
		{"eolateof",	SET_bool, (caddr_t) True, &eolateof,	SETnop},
		{"ignorecase",	SET_bool,	False,	&ignorecase,	SETic},
		{"marginbell",	SET_int,	0,	&marginbell,	SETnop},
		{"printable",	SET_int, (caddr_t) '~',	&printable,	SETsg},
		{"showmode",	SET_bool,	False,	&showmode,	SETnop},
		{"tabstop",	SET_int, (caddr_t) 8,	&tabstop,	SETsg},
#ifdef	THOROUGHSHELL
		{"thoroughshell", SET_bool,	False,	&thoroughshell,	SETnop},
#endif
		{"vidcompat",	SET_int,	0,	&vidcompat,	SETvc},
		{"all",		SET_bool,	False,	&setall,	SETall},
		{"aw",		SET_bool,	False,	&autowrite,	SETsyn},
		{"ic",		SET_bool,	False,	&ignorecase,	SETsyn},
		{"mb",		SET_int,	0,	&marginbell,	SETsyn},
	};

static	char	*show_opt(struct setrec *setptr, char *p) {
	char	s[33];

	if (setptr->set_typ == SET_bool && *((Boolean *) setptr->val) == False)
	    p = stpcpy(p, "no");
	p = stpcpy(p, setptr->setvar);
	if (setptr->set_typ != SET_bool) {
	    *p++ = '=';
	    p = stpcpy(p,
		setptr->set_typ == SET_int
		? ltoa(*((int *) setptr->val), s, 10)
		: (char *) setptr->val);
	}
	return p;
}

static	void	ex_set(void) {
	struct setrec	*setptr;
	Boolean	boolval;
	char	s[81];
	Boolean	written	= False;

	if (*p == '\0' || *p == '|')	/* show altered options */
	    for (setptr = settable; setptr->set_action != SETall; ++setptr) {
		if (setptr->set_typ == SET_str ?
			(strcmp((char *) setptr->val, (char *) setptr->dflt_val)
			    != 0) :
			(setptr->set_typ == SET_bool
			    ? *((Boolean *) setptr->val)
			    : *((int *) setptr->val))
			    != (int) setptr->dflt_val) {
		    if (written) botco(' ');
		    written = True;
		    (void) show_opt(setptr, s);
		    botputs(s);
		}
	    }
	else
	    for (; *p != '\0' && *p != '|'; skipspaces()) {
		boolval = True;
		if (p[0] == 'n' && p[1] == 'o') {
		    boolval = False;
		    p += 2;
		}
		setptr = (struct setrec *) matchstring((char **) settable,
		    sizeof(struct setrec), XtNumber(settable));
		if (setptr == Null)
		    msg_raise(
			"No such option--\"set all\" gives all option values");
		if (setptr->set_action == SETsyn) {	/* process synonyms */
		    void *oval = setptr->val;
		    do --setptr; while (setptr->val != oval);
		}
		skipspaces();
		if (*p == '?') {
		    ++p;
		    if (written) botco(' ');
		    written = True;
		    (void) show_opt(setptr, s);
		    botputs(s);
		    continue;
		}
		if (setptr->set_typ == SET_bool) {
		    if (*((Boolean *) setptr->val) == boolval) continue;
		    *((Boolean *) setptr->val) = boolval;
		}
		else {
		    if (*p == '=') {
			++p;
			skipspaces();
		    }
		    if (setptr->set_typ == SET_int) {
			int	intval;

			intval = getnum(&p);
			if (*((int *) setptr->val) == intval) continue;
			*((int *) setptr->val) = intval;
		    }
		    else {	/* process string */
			char	strval[OPTLEN];
			char	*q	= strval;

			while (p < pmax && *p != ' ' && *p != '\t') {
			    if (q >= strval + OPTLEN - 1)
				msg_raise("String too long");
			    *q++ = *p++;
			}
			*q = '\0';
			if (strcmp((char *) setptr->val, strval) == 0) continue;
			if (setptr->set_action == SETdir) {
#ifdef	DEFAULTDIR
			    if (!startup)
				msg_raise("Too late to change directory.");
#else
			    if (swaphandle != -1)
				msg_raise("Can't change directory twice.");
#endif
			}
			strcpy((char *) setptr->val, strval);
		    } 
		}
		switch (setptr->set_action) {
		    case SETesc:	setdosesc(dosesc); break;
		    case SETic:
			mem_chr = farmemchr;
			memr_chr = farmemrchr;
			mem_cmp = farmemcmp;
			if (ignorecase) {
			    mem_chr = farmemichr;
			    memr_chr = farmemrichr;
			    mem_cmp = farmemicmp;
			}
			break;
		    case SETdir:
#ifndef	DEFAULTDIR
			initswap();
#endif
			break;
		    case SETvc:
				if (visual) {
				    endvid();
				    invid(vidcompat);
				}
				/* control falls through */
		    case SETsg:  screengood = False; break;
		    case SETall: {
			    struct setrec *sp = settable;
			    int rows = ((setptr - settable - 1) / 3 + 1);

			    if (written || (visual && botcurs > pos2480))
				botnewline();
			    written = False;
			    for (; sp < settable + rows; ++sp) {
				char *p;

				p = show_opt(sp, s);
				memset(p, ' ', s + 27 - p);
				p = show_opt(sp + rows, s + 27);
				if (sp + 2 * rows < setptr) {
				    memset(p, ' ', s + 54 - p);
				    (void) show_opt(sp + 2 * rows, s + 54);
				}
				botputs(s);
				botnewline();
			    }
			    setall = False;
			}
			break;
		    case SETnop: ;
		}
	    }
	/* end if */
	if (written) botend();
}

typedef	void	(*excommandproc)(void);

#define	FLG_NOARG	1		/* no arguments allowed */
#define	FLG_NEEDARG	2		/* argument required */
#define	FLG_AW		4		/* autowrite */
#define	FLG_ADDR	8		/* address allowed */

static	struct	cmdrec	{
		char	*cmdnam;
		excommandproc	proc;
		sbyte	flags;
	}
	cmdtable[] = {
		{"edit",	ex_edit,	FLG_AW},
		{"file",	ex_file,	0},
		{"next",	ex_next,	FLG_NOARG | FLG_AW},
		{"quit",	ex_quit,	FLG_NOARG | FLG_AW},
		{"r",		Null,		1},
		{"read",	ex_read,	FLG_ADDR | FLG_NEEDARG},
		{"rewind",	ex_rew,		FLG_NOARG | FLG_AW},
		{"set",		ex_set,		0},
		{"shell",	ex_shell,	FLG_NOARG},
		{"version",	ex_version,	FLG_NOARG},
		{"wq",		ex_wq,		0},
		{"w",		Null,		1},
		{"write",	ex_write,	FLG_ADDR},
		{"x",		ex_x,		0},
	},

	cmdequals = {"=",	ex_equals,	FLG_ADDR | FLG_NOARG},

	cmdbang = {"!",		ex_bang,	FLG_NEEDARG | FLG_ADDR},

	cmdmove = {"",		ex_move,	FLG_ADDR | FLG_NOARG};

/*
 *	EXCOMMAND - Process "ex" command.
 */

void	excommand(char *str) {
	struct cmdrec	*cmdptr;
	bigint	addr;
	int	i;
	char	c;

	p = (char *) str;
	skipspaces();
	if (*p == ':') ++p;
	for (;;) {
	    skipspaces();
	    if (*p == '\0') break;
	    ex_argc = 0;
	    ex_argv[0] = ex_argv[1] = cp.line + 1;
	    while (strchr("1234567890/?'.$+-", c = *p) != Null) {
		addr = cp.line + 1;
		switch (c) {
		    case '\'':  {
			    struct posrec pos;
			    markptr mark = Null;

			    ++p;
			    if (*p == c) mark = markpos;
			    else if ((c = *p) >= 'a' && c <= 'z')
				mark = markpos + (c - ('a' - 1));
			    if (mark == Null || mark->byte < 0)
				msg_raise("No such mark");
			    findbyte(mark->byte, &pos);
			    addr = pos.line + 1;
			    ++p;
			}
			break;
		    case '/':
		    case '?':  {
			    int len;
			    struct posrec pos;
			    Boolean wrap;

			    p = parsesearch(locpat, &len, p + 1, p + strlen(p),
				c);
			    if (len != 0) loclen = len;
			    if (c == '/') {
				findline(cp.line + 1, &pos);
				pos.line += posadjust(-1, &pos);
				locdir = 1;
			    }
			    else {
				findline(cp.line, &pos);
				locdir = -1;
			    }
			    dosearch(locdir, &pos, &wrap);
			    addr = pos.line + 1;
			}
			break;
		    case '$':  addr = MAINBUF->nlines; /* ++p; break; */
		    case '.':  ++p; break;
		    default:  if (c >= '0' && c <= '9') addr = getnum(&p);
		}
		for (;;) {
		    c = *p;
		    if (c == '+') {
			c = *++p;
			if (c >= '0' && c <= '9') addr += getnum(&p);
			else ++addr;
		    }
		    else if (c == '-') {
			c = *++p;
			if (c >= '0' && c <= '9') addr -= getnum(&p);
			else --addr;
		    }
		    else break;
		}
		if (addr < 0)
		    msg_raise("Negative address--first buffer line is 1");
		if (addr > MAINBUF->nlines)
		    msg_raise("Not that many lines in buffer");
		if (ex_argc < 2) ++ex_argc;
		else ex_argv[0] = ex_argv[1];
		ex_argv[ex_argc - 1] = addr;
		skipspaces();
		if (*p != ',') break;
		++p;
	    }
	    if (ex_argc == 2 && ex_argv[0] > ex_argv[1])
		msg_raise("First address exceeds second");
	    setpmax();
	    cmdptr = Null;
	    bang = False;
	    if (*p < 'a' || *p > 'z') {
		if (*p == '=') {
		    ++p;
		    cmdptr = &cmdequals;
		}
		else if (*p == '!') {
		    ++p;
		    cmdptr = &cmdbang;
		}
		else if (p >= pmax) cmdptr = &cmdmove;
	    }
	    else {
		cmdptr = (struct cmdrec *) matchstring((char **) cmdtable,
		    sizeof(struct cmdrec), XtNumber(cmdtable));
		if (*p == '!') {
		    ++p;
		    bang = True;
		}
	    }
	    if (cmdptr == Null) msg_raise("Illegal command");
	    if (cmdptr->proc == Null) cmdptr += cmdptr->flags;	/* synonyms */
	    cmdname = cmdptr->cmdnam;
	    skipspaces();	/* get argument */
	    i = cmdptr->flags;
	    if (i & FLG_NOARG) expectend();
	    if ((i & FLG_NEEDARG) && p >= pmax) msg_raise("Argument missing");
	    if (ex_argc != 0 && !(i & FLG_ADDR))
		msg_raise("No address allowed on this command");
	    if ((i & FLG_AW) && modif && !bang)
		if (autowrite) wtfile(p, 0, 0, MAINBUF->nlines);
		else bangcheck("No write since last change");

	    (*(cmdptr->proc))();		/* this does the command */
	    if (*pmax == '\0') break;
	    p = pmax + 1;
	}
	morefiles = False;
}
