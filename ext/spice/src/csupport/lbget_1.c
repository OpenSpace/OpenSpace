/* lbget_1.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure LBGET ( Line buffer, get ) */
/* Subroutine */ int lbget_1__(integer *pos, integer *ptrs, char *buffer, 
	char *line, logical *found, ftnlen buffer_len, ftnlen line_len)
{
    integer ncom, pcard;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer nline, maxln;
    extern /* Subroutine */ int chkout_(char *, ftnlen), cbget_1__(integer *, 
	    integer *, char *, char *, ftnlen, ftnlen), lbdes_1__(integer *, 
	    integer *, integer *, integer *, integer *);
    extern logical return_(void);
    integer posptr;

/* $ Abstract */

/*     Get (return) the line at a particular position within a */
/*     line buffer. */

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

/*     CB, LB */

/* $ Keywords */

/*     ASCII */
/*     CHARACTER */
/*     STRING */
/*     TEXT */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     POS        I   Position of line. */
/*     PTRS, */
/*     BUFFER     I   Line buffer. */
/*     LINE       O   Line. */
/*     FOUND      O   True if the line was found. */

/* $ Detailed_Input */

/*     POS         is the position of an existing line within a line */
/*                 buffer. */

/*     PTRS, */
/*     BUFFER      are the pointer and character components of a line */
/*                 buffer. */

/* $ Detailed_Output */

/*     LINE        is a copy of the specified line. If LINE is shorter */
/*                 than the stored line, it is truncated. If longer, it */
/*                 is padded with spaces. */

/*     FOUND       is true whenever the specified line exists, and is */
/*                 false otherwise. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) If FOUND is false, LINE is not changed. */

/* $ Particulars */

/*     LBGET is the only way to retrieve lines from a line buffer. */

/* $ Examples */

/*     Let the line buffer (P,B) contain the following lines. */

/*       If you can keep your head when all about you */
/*       Are losing theirs and blaming it on you; */
/*       If you can trust yourself when all men doubt you, */
/*       But make allowance for their doubting too: */
/*       If you can wait and not be tired by waiting, */
/*       Or, being lied about, don't deal in lies, */
/*       Or being hated don't give way to hating, */
/*       And yet don't look too good, nor talk too wise; */

/*     The code fragment */

/*       N = 1 */
/*       CALL LBGET ( N, P, B, LINE, FOUND ) */

/*       DO WHILE ( FOUND ) */
/*          WRITE (*,*) '(', N, ') ', LINE */

/*          N = N + 1 */
/*          CALL LBGET ( N, P, B, LINE, FOUND ) */
/*       END DO */

/*     produces the following output: */

/*       (  1) If you can keep your head when all about you */
/*       (  2) Are losing theirs and blaming it on you; */
/*       (  3) If you can trust yourself when all men doubt you, */
/*       (  4) But make allowance for their doubting too: */
/*       (  5) If you can wait and not be tired by waiting, */
/*       (  6) Or, being lied about, don't deal in lies, */
/*       (  7) Or being hated don't give way to hating, */
/*       (  8) And yet don't look too good, nor talk too wise; */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     Dagny Taggart, (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 19-JAN-1989 (DT) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("LBGET_1", (ftnlen)7);
    }

/*     Recover all the essential control information. */

    lbdes_1__(ptrs, &maxln, &nline, &ncom, &pcard);

/*     What are the endpoints of the stored line? Once we have */
/*     them, we can return the line directly. */

    *found = *pos >= 1 && *pos <= nline;
    if (*found) {
	posptr = (*pos << 1) - 1;
	cbget_1__(&ptrs[posptr + 5], &ptrs[posptr + 6], buffer, line, 
		buffer_len, line_len);
    }
    chkout_("LBGET_1", (ftnlen)7);
    return 0;
} /* lbget_1__ */

