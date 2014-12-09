/* proc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure      PROC ( Process a chunk of SUBTeX source ) */
/* Subroutine */ int proc_(char *buffer, integer *first, integer *last, 
	ftnlen buffer_len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char cseq[32];
    extern integer cpos_(char *, char *, integer *, ftnlen, ftnlen);
    extern /* Subroutine */ int size_(char *, ftnlen), list_(char *, integer *
	    , ftnlen);
    integer n, begin;
    extern /* Subroutine */ int chkin_(char *, ftnlen), param_(char *, 
	    integer *, ftnlen), liter_(char *, integer *, ftnlen), title_(
	    char *, integer *, ftnlen);
    extern integer ncpos_(char *, char *, integer *, ftnlen, ftnlen);
    extern /* Subroutine */ int other_(char *, integer *, ftnlen), chkout_(
	    char *, ftnlen);
    extern logical return_(void);
    integer end;
    extern /* Subroutine */ int var_(char *, integer *, ftnlen);

/* $ Abstract */

/*     Process one meaningful unit (chunk) of a SUBTeX source buffer. */

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

/*     BUFFER      is a buffer containing lines of SUBTeX source text. */

/*     FIRST, */
/*     LAST        are the indices of the first and last lines of */
/*                 the chunk to be processed. */

/* $ Detailed_Output */

/*     The processed line is saved in the temporary buffer. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     BUFFER     I   Input source. */
/*     FIRST, */
/*     LAST       I   Bounds of chunk to be processed. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Particulars */



/* $ Examples */



/* $ Restrictions */



/* $ Literature_References */

/* $Include SUBTeX.REFS */

/* $ Author_and_Institution */

/*     I.M. Underwood (JPL) */

/* $ Version */

/*     Beta Version 1.1.0,  8-JAN-1996 (WLT) */

/*        Changed the call to SIZE to remove the extra argument */
/*        that is not needed by the routine */

/*        It was */

/*           CALL SIZE ( BUFFER(FIRST), N ) */

/*        Now it's */

/*           CALL SIZE ( BUFFER(FIRST) ) */

/*     Beta Version 1.0.0, 11-JUN-1988 (IMU) */

/* -& */

/*     Entry points */


/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling */

    if (return_()) {
	return 0;
    } else {
	chkin_("PROC", (ftnlen)4);
    }

/*     The type of processing depends on the type of chunk. */
/*     The type of chunk is indicated by the initial control sequence. */

    begin = ncpos_(buffer + (*first + 5) * buffer_len, "  ", &c__1, 
	    buffer_len, (ftnlen)2);
    end = cpos_(buffer + (*first + 5) * buffer_len, " {", &begin, buffer_len, 
	    (ftnlen)2) - 1;
    s_copy(cseq, buffer + ((*first + 5) * buffer_len + (begin - 1)), (ftnlen)
	    32, end - (begin - 1));
    n = *last - *first + 1;
    if (s_cmp(cseq, "@chapter", (ftnlen)32, (ftnlen)8) == 0 || s_cmp(cseq, 
	    "@section", (ftnlen)32, (ftnlen)8) == 0 || s_cmp(cseq, "@subsect"
	    "ion", (ftnlen)32, (ftnlen)11) == 0) {
	title_(buffer + (*first + 5) * buffer_len, &n, buffer_len);
    } else if (s_cmp(cseq, "@setvarsize", (ftnlen)32, (ftnlen)11) == 0 || 
	    s_cmp(cseq, "@setparamsize", (ftnlen)32, (ftnlen)13) == 0) {
	size_(buffer + (*first + 5) * buffer_len, buffer_len);
    } else if (s_cmp(cseq, "@var", (ftnlen)32, (ftnlen)4) == 0 || s_cmp(cseq, 
	    "@morevar", (ftnlen)32, (ftnlen)8) == 0) {
	var_(buffer + (*first + 5) * buffer_len, &n, buffer_len);
    } else if (s_cmp(cseq, "@param", (ftnlen)32, (ftnlen)6) == 0 || s_cmp(
	    cseq, "@moreparam", (ftnlen)32, (ftnlen)10) == 0) {
	param_(buffer + (*first + 5) * buffer_len, &n, buffer_len);
    } else if (s_cmp(cseq, "@literal", (ftnlen)32, (ftnlen)8) == 0 || s_cmp(
	    cseq, "@literalitem", (ftnlen)32, (ftnlen)12) == 0 || s_cmp(cseq, 
	    "@literalparam", (ftnlen)32, (ftnlen)13) == 0 || s_cmp(cseq, 
	    "@literalvar", (ftnlen)32, (ftnlen)11) == 0 || s_cmp(cseq, "@exl"
	    "iteral", (ftnlen)32, (ftnlen)10) == 0 || s_cmp(cseq, "@exliteral"
	    "item", (ftnlen)32, (ftnlen)14) == 0 || s_cmp(cseq, "@exliteralpa"
	    "ram", (ftnlen)32, (ftnlen)15) == 0 || s_cmp(cseq, "@exliteralvar",
	     (ftnlen)32, (ftnlen)13) == 0) {
	liter_(buffer + (*first + 5) * buffer_len, &n, buffer_len);
    } else if (s_cmp(cseq, "@newlist", (ftnlen)32, (ftnlen)8) == 0 || s_cmp(
	    cseq, "@numitem", (ftnlen)32, (ftnlen)8) == 0 || s_cmp(cseq, 
	    "@paritem", (ftnlen)32, (ftnlen)8) == 0 || s_cmp(cseq, "@symitem",
	     (ftnlen)32, (ftnlen)8) == 0) {
	list_(buffer + (*first + 5) * buffer_len, &n, buffer_len);
    } else {
	other_(buffer + (*first + 5) * buffer_len, &n, buffer_len);
    }
    chkout_("PROC", (ftnlen)4);
    return 0;
} /* proc_ */

