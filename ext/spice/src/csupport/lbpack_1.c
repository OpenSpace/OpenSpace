/* lbpack_1.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure LBPACK ( Line buffer, pack ) */
/* Subroutine */ int lbpack_1__(integer *ptrs, char *buffer, ftnlen 
	buffer_len)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    integer ncom, i__, j, begin, pcard;
    extern /* Subroutine */ int chkin_(char *, ftnlen), maxai_(integer *, 
	    integer *, integer *, integer *);
    integer nline, maxln, offset, intlen;
    extern /* Subroutine */ int chkout_(char *, ftnlen), cbrem_1__(integer *, 
	    integer *, char *, ftnlen), lbdes_1__(integer *, integer *, 
	    integer *, integer *, integer *);
    extern logical return_(void);
    extern /* Subroutine */ int lbupd_1__(integer *, integer *, integer *);
    integer end;
    extern integer sizecb_1__(char *, ftnlen);

/* $ Abstract */

/*     Pack the contents of a line buffer. */

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
/*     PTRS, */
/*     BUFFER    I,O  Line buffer. */

/* $ Detailed_Input */

/*     PTRS, */
/*     BUFFER      are the pointer and character components of a line */
/*                 buffer. */

/* $ Detailed_Output */

/*     PTRS, */
/*     BUFFER      are the pointer and character components of the */
/*                 same line buffer after packing. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Particulars */

/*     As lines are added to and removed from a line buffer, the */
/*     buffer becomes fragmented, with free space spread throughout. */
/*     Occasionally, the LB routines will pull all the current lines */
/*     toward the front of the buffer, accumulating all the free */
/*     space in one contiguous chunk. */

/*     LBPACK is provided mainly for internal use by the LB routines, */
/*     but you may pack a line buffer any time you want. Packing a */
/*     buffer will typically speed up operations that change the contents */
/*     of a buffer, but will have no effect on retrieval operations. */

/* $ Examples */

/*     LBPACK is used by LBINS. */

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
	chkin_("LBPACK_1", (ftnlen)8);
    }

/*     Recover the essential control information. */

    lbdes_1__(ptrs, &maxln, &nline, &ncom, &pcard);

/*     For each interval in the complement... */

    offset = nline << 1;
    i__1 = pcard;
    for (i__ = offset + 1; i__ <= i__1; i__ += 2) {

/*        Remove the contents of the interval from the CB, pulling */
/*        the remaining contents forward. */

	begin = ptrs[i__ + 5];
	end = ptrs[i__ + 6];
	intlen = end - begin + 1;
	if (begin <= end) {
	    cbrem_1__(&begin, &end, buffer, buffer_len);

/*           Adjust the pointers for both the lines and the complement */
/*           intervals that followed the purged interval. */

	    i__2 = pcard;
	    for (j = 1; j <= i__2; ++j) {
		if (ptrs[j + 5] > end) {
		    ptrs[j + 5] -= intlen;
		}
	    }
	}
    }

/*     There is only one interval in the complement now. It begins */
/*     just after the last line, and runs to the end of the buffer. */

    maxai_(&ptrs[6], &offset, &end, &j);
    ptrs[offset + 6] = end + 1;
    ptrs[offset + 7] = sizecb_1__(buffer, buffer_len);
    lbupd_1__(&nline, &c__1, ptrs);
    chkout_("LBPACK_1", (ftnlen)8);
    return 0;
} /* lbpack_1__ */

