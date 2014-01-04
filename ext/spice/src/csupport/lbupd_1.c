/* lbupd_1.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure LBUPD ( Line buffer, update ) */
/* Subroutine */ int lbupd_1__(integer *nline, integer *ncom, integer *ptrs)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer sizei_(integer *);
    extern /* Subroutine */ int scardi_(integer *, integer *), sigerr_(char *,
	     ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen), 
	    errint_(char *, integer *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Update internal information in a line buffer. */

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
/*     NLINE      I   Number of lines stored in the buffer. */
/*     NCOM       I   Number of complement intervals in the buffer. */
/*     PTRS      I,O  Pointer compnent of the buffer. */

/* $ Detailed_Input */

/*     NLINE       is the number of lines stored in the buffer, as */
/*                 the result of some change. */

/*     NCOM        is the number of complement intervals in the buffer, */
/*                 as the result of the same change. */

/*     PTRS        is the pointer component of a line buffer. */

/* $ Detailed_Output */

/*     PTRS        is the updated pointer component of a line buffer. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) The error 'SPICE(LBCORRUPTED)' is signalled whenever any */
/*        of the following conditions is detected. */

/*           -- NLINE is less than zero. */

/*           -- NCOM is less than one. */

/*           -- The sum of NLINE and NCOM is greater than the maximum */
/*              number of lines that can be stored in the buffer. */

/* $ Particulars */

/*     LBUPD is are provided for use by the LB routines in SPICELIB, and */
/*     should not be called directly except by those routines. */

/* $ Examples */

/*     LBUPD is used by LBINS and LBREM. */

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


/*     Standard error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("LBUPD_1", (ftnlen)7);
	if (*nline < 0 || *ncom < 1 || *nline + *ncom << 1 > sizei_(ptrs)) {
	    setmsg_("Tried to store # lines, # holes.", (ftnlen)32);
	    errint_("#", nline, (ftnlen)1);
	    errint_("#", ncom, (ftnlen)1);
	    sigerr_("SPICE(LBCORRUPTED)", (ftnlen)18);
	    chkout_("LBUPD_1", (ftnlen)7);
	    return 0;
	}
    }

/*     Save the current number of lines in element -2. We can infer the */
/*     cardinality of the cell from the total number of intervals. */

    ptrs[3] = *nline;
    i__1 = *nline + *ncom << 1;
    scardi_(&i__1, ptrs);
    chkout_("LBUPD_1", (ftnlen)7);
    return 0;
} /* lbupd_1__ */

