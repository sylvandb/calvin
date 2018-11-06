				  RELEASE NOTES
		      CALVIN (FORMERLY FREE VI) VERSION 2.3

This is Calvin, a small partial clone of the Unix<tm> vi screen editor.

This zip file contains the files:

	read.me		This file.
	

NOTES FOR VERSION 2.3:

There is really only one change in this release:  the text on the bottom
line of the screen will not hang around forever.  The only real reason for
this release is make it correspond to the source code, which is now being
released in a separate ZIP file.

NOTES FOR VERSION 2.2:

This is another bug-fix release, again having to do with internal errors.
I hope that they are fixed this time.  It also changes the `undo' command
to act more like the original vi.  In particular, you can move to the location
of the last change in the file by typing `uu'.


NOTES FOR VERSION 2.1:

This is a bug-fix release; this has to do with "internal error..." messages
which Version 2.0 occasionally produced.  These could have been produced by
the following bugs, which have now been fixed:

    1.	If you type 'f' or 't' on a blank line, then it may find the character
	in the next line.
    2.	If you run out of disk space when writing a file or a part of a file,
	then it may turn an end-of-line into a Control-M (carriage return).

I hope that these fixes cut down on the number of internal errors.


NOTES FOR VERSION 2.0:

This is the third public release of Calvin, formerly called Free VI.  The name
has been changed due to the appearance of other vi clones.

This version is a substantial rewrite of Version 1.9a; the most important
changes are:
    1.  There is no longer a 50K file size limit.
    2.  Many more ex ("colon") commands have been added.
    3.  You can now put multiple file names (and wild cards) on the command
	line.  Also, the "+" option and EXINIT command variable are now
	supported.
    4.  Varying display parameters are supported (e.g. EGA 43 line mode).

Rather than list all the changes, I will refer you to the documentation file.

Thanks to mdlawler@bsu-cs.bsu.edu for many helpful suggestions and bug reports.

Calvin is copyrighted, but may be freely distributed.

Paul Vojta
vojta@math.berkeley.edu
