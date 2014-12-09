/* list.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__0 = 0;

/* $Procedure      LIST ( Process a SUBTeX list item ) */
/* Subroutine */ int list_(char *source, integer *n, ftnlen source_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char line[132], cseq[12];
    integer l, begin;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer index;
    extern /* Subroutine */ int tempb_(char *, char *, ftnlen, ftnlen);
    integer pgwid, width, iskip;
    char token[132];
    integer lskip, rskip;
    extern /* Subroutine */ int rjust_(char *, char *, ftnlen, ftnlen);
    integer remain, indent;
    char marker[5];
    extern /* Subroutine */ int params_(char *, char *, integer *, ftnlen, 
	    ftnlen), chkout_(char *, ftnlen), tokens_(char *, char *, integer 
	    *, char *, integer *, ftnlen, ftnlen, ftnlen), suffix_(char *, 
	    integer *, char *, ftnlen, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int intstr_(integer *, char *, ftnlen);

/* $ Abstract */

/*     Process a @newlist, @numitem, @symitem, or @paritem control */
/*     sequence. */

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

/*     SOURCE      are the source lines containing a @newlist, @numitem, */
/*                 @symitem, or @paritem control sequence, followed by */
/*                 an associated paragraph of text. */

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
	chkin_("LIST", (ftnlen)4);
    }

/*     Retrieve the required parameters. */

    params_("GET", "PAGEWIDTH", &width, (ftnlen)3, (ftnlen)9);
    params_("GET", "LEFTSKIP", &lskip, (ftnlen)3, (ftnlen)8);
    params_("GET", "RIGHTSKIP", &rskip, (ftnlen)3, (ftnlen)9);
    params_("GET", "ITEMINDENT", &indent, (ftnlen)3, (ftnlen)10);
    params_("GET", "ITEMSKIP", &iskip, (ftnlen)3, (ftnlen)8);

/*     The first token should be a recognized control sequence. */

    tokens_("NEW", source, n, cseq, &l, (ftnlen)3, source_len, (ftnlen)12);

/*     @newlist just resets the list index. That's all. */

    if (s_cmp(cseq, "@newlist", (ftnlen)12, (ftnlen)8) == 0) {
	params_("SET", "LISTINDEX", &c__1, (ftnlen)3, (ftnlen)9);
	chkout_("LIST", (ftnlen)4);
	return 0;
    }

/*     The principal difference between the various items is the */
/*     marker that begins the first line. */

    if (s_cmp(cseq, "@numitem", (ftnlen)12, (ftnlen)8) == 0) {
	params_("GET", "LISTINDEX", &index, (ftnlen)3, (ftnlen)9);
	i__1 = index + 1;
	params_("SET", "LISTINDEX", &i__1, (ftnlen)3, (ftnlen)9);
	intstr_(&index, marker, (ftnlen)5);
	suffix_(".", &c__0, marker, (ftnlen)1, (ftnlen)5);
    } else if (s_cmp(cseq, "@symitem", (ftnlen)12, (ftnlen)8) == 0) {
	s_copy(marker, "--", (ftnlen)5, (ftnlen)2);
    } else if (s_cmp(cseq, "@paritem", (ftnlen)12, (ftnlen)8) == 0) {
	s_copy(marker, " ", (ftnlen)5, (ftnlen)1);
    }

/*     The rest of the text is reformatted into a paragraph of width */

/*        PAGEWIDTH - LEFTSKIP - RIGHTSKIP - ITEMINDENT - ITEMSKIP */

/*     beginning in column */

/*        LEFTSKIP + ITEMINDENT + ITEMSKIP + 1 */

/*     The first line contains the marker, right-justified to column */

/*        LEFTSKIP + ITEMINDENT */

/*     Keep grabbing tokens until the run out. Start a new line whenever */
/*     the current line becomes full. REMAIN is the number of spaces */
/*     remaining in the current line. */

    pgwid = width - lskip - rskip - indent - iskip;
    begin = lskip + indent + iskip + 1;
    remain = pgwid;
    s_copy(line, " ", (ftnlen)132, (ftnlen)1);
    rjust_(marker, line, (ftnlen)5, lskip + indent);
    s_copy(token, " ", (ftnlen)132, (ftnlen)1);
    tokens_("NEXT", source, n, token, &l, (ftnlen)4, source_len, pgwid);
    while(s_cmp(token, " ", (ftnlen)132, (ftnlen)1) != 0) {
	if (l > remain || s_cmp(token, "@newline", (ftnlen)132, (ftnlen)8) == 
		0) {
	    tempb_("ADD", line, (ftnlen)3, (ftnlen)132);
	    s_copy(line, " ", (ftnlen)132, (ftnlen)1);
	    remain = pgwid;
	    s_copy(line + (begin - 1), token, 132 - (begin - 1), (ftnlen)132);
	    remain = remain - l - 1;
	} else if (s_cmp(line + (begin - 1), " ", 132 - (begin - 1), (ftnlen)
		1) == 0) {
	    s_copy(line + (begin - 1), token, 132 - (begin - 1), (ftnlen)132);
	    remain = remain - l - 1;
	} else {
	    suffix_(token, &c__1, line + (begin - 1), (ftnlen)132, 132 - (
		    begin - 1));
	    remain = remain - l - 1;
	}
	tokens_("NEXT", source, n, token, &l, (ftnlen)4, source_len, pgwid);
    }
    if (s_cmp(line, " ", (ftnlen)132, (ftnlen)1) != 0) {
	tempb_("ADD", line, (ftnlen)3, (ftnlen)132);
    }

/*     Every list item is followed by a blank line. */

    tempb_("ADD", " ", (ftnlen)3, (ftnlen)1);
    chkout_("LIST", (ftnlen)4);
    return 0;
} /* list_ */

