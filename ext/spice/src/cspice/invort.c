/* invort.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      INVORT ( Invert nearly orthogonal matrices ) */
/* Subroutine */ int invort_(doublereal *m, doublereal *mit)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    doublereal temp[9]	/* was [3][3] */;
    integer i__;
    doublereal scale;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static doublereal bound;
    extern doublereal dpmax_(void);
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen), xpose_(
	    doublereal *, doublereal *), unorm_(doublereal *, doublereal *, 
	    doublereal *);
    doublereal length;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), vsclip_(doublereal *, doublereal *), setmsg_(char *, 
	    ftnlen), errint_(char *, integer *, ftnlen);

/* $ Abstract */

/*     Construct the inverse of a 3x3 matrix with orthogonal columns */
/*     and non-zero norms using a numerical stable algorithm. */

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

/*     None. */

/* $ Keywords */

/*     MATRIX */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     M          I   A 3x3 matrix. */
/*     MIT        I   M after transposition and scaling of rows. */

/* $ Detailed_Input */

/*     M          is a 3x3 matrix. */

/* $ Detailed_Output */

/*     MIT        is the matrix obtained by transposing M and dividing */
/*                the rows by squares of their norms. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If any of the columns of M have zero length, the error */
/*        SPICE(ZEROLENGTHCOLUMN) will be signaled. */

/*     2) If any column is too short to allow computation of the */
/*        reciprocal of its length without causing a floating */
/*        point overflow, the error SPICE(COLUMNTOOSMALL) will */
/*        be signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Suppose that M is the matrix */

/*             -                      - */
/*            |   A*u    B*v     C*w   | */
/*            |      1      1       1  | */
/*            |                        | */
/*            |   A*u    B*v     C*w   | */
/*            |      2      2       2  | */
/*            |                        | */
/*            |   A*u    B*v     C*w   | */
/*            |      3      3       3  | */
/*             -                      - */

/*     where the vectors (u , u , u ),  (v , v , v ),  and (w , w , w ) */
/*                         1   2   3      1   2   3          1   2   3 */
/*     are unit vectors. This routine produces the matrix: */


/*             -                      - */
/*            |   a*u    a*u     a*u   | */
/*            |      1      2       3  | */
/*            |                        | */
/*            |   b*v    b*v     b*v   | */
/*            |      1      2       3  | */
/*            |                        | */
/*            |   c*w    c*w     c*w   | */
/*            |      1      2       3  | */
/*             -                      - */

/*     where a = 1/A, b = 1/B, and c = 1/C. */

/* $ Examples */

/*     Suppose that you have a matrix M whose columns are orthogonal */
/*     and have non-zero norm (but not necessarily norm 1).  Then the */
/*     routine INVORT can be used to construct the inverse of M: */

/*        CALL INVORT ( M, INVERS ) */

/*     This method is numerically more robust than calling the */
/*     routine INVERT. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     W.L. Taber     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.1, 14-NOV-2013 (EDW) */

/*        Edit to Abstract. Eliminated unneeded Revisions section. */

/* -    SPICELIB Version 1.1.0, 02-SEP-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in VSCL call. */

/* -    SPICELIB Version 1.0.0, 02-JAN-2002 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Transpose a matrix and invert the lengths of the rows */
/*     Invert a pseudo orthogonal matrix */

/* -& */

/*     SPICELIB functions */


/*     Local Variables */


/*     Saved variables */


/*     Initial values */


/*     Use discovery check-in. */


/*     The first time through, get a copy of DPMAX. */

    if (first) {
	bound = dpmax_();
	first = FALSE_;
    }

/*     For each column, construct a scaled copy. However, make sure */
/*     everything is do-able before trying something. */

    for (i__ = 1; i__ <= 3; ++i__) {
	unorm_(&m[(i__1 = i__ * 3 - 3) < 9 && 0 <= i__1 ? i__1 : s_rnge("m", 
		i__1, "invort_", (ftnlen)208)], &temp[(i__2 = i__ * 3 - 3) < 
		9 && 0 <= i__2 ? i__2 : s_rnge("temp", i__2, "invort_", (
		ftnlen)208)], &length);
	if (length == 0.) {
	    chkin_("INVORT", (ftnlen)6);
	    setmsg_("Column # of the input matrix has a norm of zero. ", (
		    ftnlen)49);
	    errint_("#", &i__, (ftnlen)1);
	    sigerr_("SPICE(ZEROLENGTHCOLUMN)", (ftnlen)23);
	    chkout_("INVORT", (ftnlen)6);
	    return 0;
	}

/*        Make sure we can actually rescale the rows. */

	if (length < 1.) {
	    if (length * bound < 1.) {
		chkin_("INVORT", (ftnlen)6);
		setmsg_("The length of column # is #. This number cannot be "
			"inverted.  For this reason, the scaled transpose of "
			"the input matrix cannot be formed. ", (ftnlen)138);
		errint_("#", &i__, (ftnlen)1);
		errdp_("#", &length, (ftnlen)1);
		sigerr_("SPICE(COLUMNTOOSMALL)", (ftnlen)21);
		chkout_("INVORT", (ftnlen)6);
		return 0;
	    }
	}
	scale = 1. / length;
	vsclip_(&scale, &temp[(i__1 = i__ * 3 - 3) < 9 && 0 <= i__1 ? i__1 : 
		s_rnge("temp", i__1, "invort_", (ftnlen)246)]);
    }

/*     If we make it this far, we just need to transpose TEMP into MIT. */

    xpose_(temp, mit);
    return 0;
} /* invort_ */

