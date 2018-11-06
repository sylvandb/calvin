This ZIP file contains the source files of Calvin, a small partial clone
of the Unix<tm> vi command.

This program compiles under version 2.0 (1988 vintage) of Turbo C, and
version A86 assembler, version 4.02.  I do not guarantee that it will
compile or work with any other C compiler or assembler.  In particular,
some of the *.ASM files _require_ A86.

The files included in this distribution are:

	readme.src	This file.
	makefile	A Makefile.  You may have to adapt it for your
			particular version of make.
	turboc.cfg	Contains some definitions used by Turbo C.  It may
			require adaptation for your system.
	vi.h		C source files
	main.c
	ex.c
	vu.c
	vl.c
	vs.c
	vm.c
	vc.c
	setargv.c	C source file for wild cards in the command line
	va.asm		A86 source files
	vk.asm
	vv.asm
	sys3.asm

These files are copyrighted, but may be freely distributed.

Paul Vojta
vojta@math.berkeley.edu
