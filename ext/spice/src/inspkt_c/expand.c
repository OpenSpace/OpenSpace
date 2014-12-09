/* expand.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__2 = 2;

/* $Procedure      EXPAND ( Expand a SUBTeX buffer ) */
/* Subroutine */ int expand_(char *buffer, ftnlen buffer_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer card;
    char file[128];
    integer line;
    char cseq[12];
    logical more;
    integer size;
    char text[132];
    extern integer cardc_(char *, ftnlen);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer sizec_(char *, ftnlen);
    extern /* Subroutine */ int nthwd_(char *, integer *, char *, integer *, 
	    ftnlen, ftnlen), scardc_(integer *, char *, ftnlen), swapac_(
	    integer *, integer *, integer *, integer *, char *, ftnlen), 
	    sigerr_(char *, ftnlen), chkout_(char *, ftnlen), rdtext_(char *, 
	    char *, logical *, ftnlen, ftnlen);
    extern logical return_(void);
    integer end;
    logical eof;
    integer loc, new__;

/* $ Abstract */

/*     Expand a buffer containing SUBTeX source lines by replacing */
/*     @input commands with the contents of the indicated files. */

/* $ Disclaimer */

/*     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE */
/*     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S. */
/*     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE */
/*     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE */
/*     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS" */
/*     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY */
/*     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A */
/*     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC */
/*     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE */
/*     SOFTWARE AND RELATED MATERIALS, HOWEVER USED. */

/*     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA */
/*     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT */
/*     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND, */
/*     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS, */
/*     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE */
/*     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY. */

/*     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF */
/*     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY */
/*     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE */
/*     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE. */

/* $ Required_Reading */

/*     SUBTeX */

/* $ Keywords */

/*     SUBTeX */

/* $ Declarations */
/* $ Detailed_Input */

/*     BUFFER      on input is a buffer containing lines of SUBTeX */
/*                 source text, which may include commands of the form */

/*                    @input <filename> */

/* $ Detailed_Output */

/*     BUFFER      on output contains the original lines, with any */
/*                 input lines replaced by the contents of the specified */
/*                 files. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     BUFFER    I/O  Original, expanded SUBTeX source lines. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) If an input file cannot be opened or read, the error */
/*        'SUBTeX(BADINPUTFILE)' is signalled. */

/*     2) If the expanded buffer becomes too large, the error */
/*        'SUBTeX(CANNOTEXPAND)' is signalled. */

/* $ Particulars */

/*     The @input control sequence and the name of the file are */
/*     assumed to be the first two words of the command file. */
/*     For example, */

/*        @input gloss.tex */
/*        @input gloss.tex  % glossary of terms */

/*     are all legitimate input commands; however, */

/*        @input gloss.tex%glossary of terms */

/*     is not; and in the following line */

/*        @input gloss.tex  @input appendix.tex ... @input authors.tex */

/*     all files but the first are ignored. Note that EXPAND does NOT */
/*     assumes a file type of 'TEX' if none is supplied. (This may be */
/*     remedied in later versions. */

/* $ Examples */



/* $ Restrictions */

/*     1) The file name may contain up to 128 characters. */

/*     2) All files are opened with the READONLY qualifier, which is */
/*        specific to VAX Fortran. This must be changed or removed */
/*        when porting to non-VAX environments. */

/* $ Literature_References */

/* $Include SUBTeX.REFS */

/* $ Author_and_Institution */

/*     I.M. Underwood (JPL) */

/* $ Version */

/*     Beta Version 1.0.0, 11-JUN-1988 (IMU) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling */

    if (return_()) {
	return 0;
    } else {
	chkin_("EXPAND", (ftnlen)6);
    }

/*     The size of the buffer determines the number of replacement */
/*     lines that can be added. */

    size = sizec_(buffer, buffer_len);

/*     Rather than trying to nest expansions, we will expand one level */
/*     at a time. */

    more = TRUE_;
    while(more) {

/*        Check each line in the buffer for an input command. */
/*        (Remember that expanding a line can change both the */
/*        cardinality of the buffer and the location of the */
/*        current line.) */

	line = 0;
	more = FALSE_;
	while(line < cardc_(buffer, buffer_len)) {
	    ++line;
	    nthwd_(buffer + (line + 5) * buffer_len, &c__1, cseq, &loc, 
		    buffer_len, (ftnlen)12);
	    if (s_cmp(cseq, "@input", (ftnlen)12, (ftnlen)6) == 0) {

/*              We have a bite. Get the name of the file. */

		nthwd_(buffer + (line + 5) * buffer_len, &c__2, file, &loc, 
			buffer_len, (ftnlen)128);

/*              Put lines read from the file at the end of the buffer. */
/*              We will exchange the new lines for the @input command */
/*              line later. */

		card = cardc_(buffer, buffer_len);
		end = card;
		rdtext_(file, text, &eof, (ftnlen)128, (ftnlen)132);
		while(! eof) {
		    if (end < size) {
			++end;
			s_copy(buffer + (end + 5) * buffer_len, text, 
				buffer_len, (ftnlen)132);
		    } else {
			sigerr_("SUBTeX(CANNOTEXPAND)", (ftnlen)20);
			chkout_("EXPAND", (ftnlen)6);
			return 0;
		    }
		    rdtext_(file, text, &eof, (ftnlen)128, (ftnlen)132);
		}

/*              The big switch. (Remember that the current line */
/*              will also be incremented at the beginning of the */
/*              loop. Also that the command line, now at the */
/*              end of the buffer, is not part of the expanded */
/*              buffer.) */

		new__ = end - card;
		i__1 = card + 1;
		swapac_(&c__1, &line, &new__, &i__1, buffer + buffer_len * 6, 
			buffer_len);
		line = line + new__ - 1;
		i__1 = card + new__ - 1;
		scardc_(&i__1, buffer, buffer_len);

/*              Because the expanded file may contain its own */
/*              input commands, we will need to run through the */
/*              buffer again. */

		more = TRUE_;
	    }
	}
    }
    chkout_("EXPAND", (ftnlen)6);
    return 0;
} /* expand_ */

