/* title.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure      TITLE ( Process a SUBTeX title ) */
/* Subroutine */ int title_(char *source, integer *n, ftnlen source_len)
{
    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char line[132], cseq[12];
    integer l, begin;
    extern /* Subroutine */ int chkin_(char *, ftnlen), tempb_(char *, char *,
	     ftnlen, ftnlen);
    integer pgwid, under, width;
    char token[132];
    integer remain;
    extern /* Subroutine */ int shiftc_(char *, char *, integer *, char *, 
	    char *, ftnlen, ftnlen, ftnlen, ftnlen), params_(char *, char *, 
	    integer *, ftnlen, ftnlen), chkout_(char *, ftnlen), tokens_(char 
	    *, char *, integer *, char *, integer *, ftnlen, ftnlen, ftnlen), 
	    suffix_(char *, integer *, char *, ftnlen, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Process a @chapter, @section, or @subsection control sequence. */

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

/*     SOURCE      are the source lines containing a @chapter, @section, */
/*                 or @subsection control sequence, followed by a title. */

/*     N           is the number of source lines. */

/* $ Detailed_Output */

/*     Processed lines are saved in the temporary buffer. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     SOURCE     I   Source lines. */
/*     N          I   Number of source lines. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Particulars */

/*     Titles are not indented, regardless of the current value of */
/*     LEFTSKIP. Each title is preceded and followed by a blank line. */

/*     A title may be broken into several lines by occurrences of */
/*     the @newline control sequence. */

/* $ Examples */


/* $ Restrictions */

/*     None. */

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
	chkin_("TITLE", (ftnlen)5);
    }

/*     Retrieve the required parameters. */

    params_("GET", "PAGEWIDTH", &width, (ftnlen)3, (ftnlen)9);

/*     The first token should be @chapter, @section, or @subsection. */

    tokens_("NEW", source, n, cseq, &l, (ftnlen)3, source_len, (ftnlen)12);

/*     One difference between the various titles is the number */
/*     of blank lines preceding them. */

    if (s_cmp(cseq, "@chapter", (ftnlen)12, (ftnlen)8) == 0) {
	tempb_("ADD", " ", (ftnlen)3, (ftnlen)1);
    } else if (s_cmp(cseq, "@section", (ftnlen)12, (ftnlen)8) == 0) {
	tempb_("ADD", " ", (ftnlen)3, (ftnlen)1);
    } else if (s_cmp(cseq, "@subsection", (ftnlen)12, (ftnlen)11) == 0) {
	tempb_("ADD", " ", (ftnlen)3, (ftnlen)1);
    }

/*     Keep grabbing tokens until they run out. Start a new line */
/*     whenever: */

/*        - the current line becomes full. */
/*        - the current token is @newline. */

/*     REMAIN is the number of spaces remaining in the current line. */

    pgwid = width;
    begin = 1;
    s_copy(line, " ", (ftnlen)132, (ftnlen)1);
    remain = pgwid;
    s_copy(token, " ", (ftnlen)132, (ftnlen)1);
    tokens_("NEXT", source, n, token, &l, (ftnlen)4, source_len, pgwid);
    while(s_cmp(token, " ", (ftnlen)132, (ftnlen)1) != 0) {
	if (s_cmp(token, "@newline", (ftnlen)132, (ftnlen)8) == 0) {
	    tempb_("ADD", line, (ftnlen)3, (ftnlen)132);
	    s_copy(line, " ", (ftnlen)132, (ftnlen)1);
	    remain = pgwid;
	} else if (l > remain) {
	    tempb_("ADD", line, (ftnlen)3, (ftnlen)132);
	    s_copy(line, " ", (ftnlen)132, (ftnlen)1);
	    remain = pgwid;
	    s_copy(line + (begin - 1), token, 132 - (begin - 1), (ftnlen)132);
	    remain = remain - l - 1;
	} else if (s_cmp(line, " ", (ftnlen)132, (ftnlen)1) == 0) {
	    s_copy(line, token, (ftnlen)132, (ftnlen)132);
	    remain = remain - l - 1;
	} else {
	    suffix_(token, &c__1, line, (ftnlen)132, (ftnlen)132);
	    remain = remain - l - 1;
	}
	tokens_("NEXT", source, n, token, &l, (ftnlen)4, source_len, pgwid);
    }
    if (s_cmp(line, " ", (ftnlen)132, (ftnlen)1) != 0) {
	tempb_("ADD", line, (ftnlen)3, (ftnlen)132);
    }

/*     Other differences are the way they are (or are not) underlined, */
/*     and the number of blank lines following them. */

    if (s_cmp(cseq, "@chapter", (ftnlen)12, (ftnlen)8) == 0) {
	under = width;
	s_copy(line, " ", (ftnlen)132, (ftnlen)1);
	shiftc_(line, "R", &under, "=", line, (ftnlen)132, (ftnlen)1, (ftnlen)
		1, (ftnlen)132);
	tempb_("ADD", line, (ftnlen)3, (ftnlen)132);
/*         LINE  = ' ' */
/*         CALL SHIFTC ( LINE, 'R', UNDER, '-', LINE ) */
/*         CALL TEMPB ( 'ADD', LINE ) */
	tempb_("ADD", " ", (ftnlen)3, (ftnlen)1);
    } else if (s_cmp(cseq, "@section", (ftnlen)12, (ftnlen)8) == 0) {
	under = (integer) ((real) width * .75f);
	s_copy(line, " ", (ftnlen)132, (ftnlen)1);
	shiftc_(line, "R", &under, "-", line, (ftnlen)132, (ftnlen)1, (ftnlen)
		1, (ftnlen)132);
	tempb_("ADD", line, (ftnlen)3, (ftnlen)132);
	tempb_("ADD", " ", (ftnlen)3, (ftnlen)1);
    } else if (s_cmp(cseq, "@subsection", (ftnlen)12, (ftnlen)11) == 0) {
	tempb_("ADD", " ", (ftnlen)3, (ftnlen)1);
    }
    chkout_("TITLE", (ftnlen)5);
    return 0;
} /* title_ */

