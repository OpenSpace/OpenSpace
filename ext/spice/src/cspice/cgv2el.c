/* cgv2el.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      CGV2EL ( Center and generating vectors to ellipse ) */
/* Subroutine */ int cgv2el_(doublereal *center, doublereal *vec1, doublereal 
	*vec2, doublereal *ellips)
{
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *), chkin_(
	    char *, ftnlen), saelgv_(doublereal *, doublereal *, doublereal *,
	     doublereal *), chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Form a SPICELIB ellipse from a center vector and two generating */
/*     vectors. */

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

/*     ELLIPSES */

/* $ Keywords */

/*     ELLIPSE */
/*     GEOMETRY */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     CENTER, */
/*     VEC1, */
/*     VEC2       I   Center and two generating vectors for an ellipse. */
/*     ELLIPS     O   The SPICELIB ellipse defined by the input vectors. */

/* $ Detailed_Input */

/*     CENTER, */
/*     VEC1, */
/*     VEC2           are a center and two generating vectors defining */
/*                    an ellipse in three-dimensional space.  The */
/*                    ellipse is the set of points */

/*                       CENTER  +  cos(theta) VEC1  +  sin(theta) VEC2 */

/*                    where theta ranges over the interval (-pi, pi]. */
/*                    VEC1 and VEC2 need not be linearly independent. */

/* $ Detailed_Output */

/*     ELLIPS         is the SPICELIB ellipse defined by the input */
/*                    vectors. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If VEC1 and VEC2 are linearly dependent, ELLIPS will be */
/*         degenerate.  SPICELIB ellipses are allowed to represent */
/*         degenerate geometric ellipses. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     SPICELIB ellipses serve to simplify calling sequences and reduce */
/*     the chance for error in declaring and describing argument lists */
/*     involving ellipses. */

/*     The set of ellipse conversion routines is */

/*        CGV2EL ( Center and generating vectors to ellipse ) */
/*        EL2CGV ( Ellipse to center and generating vectors ) */

/* $ Examples */

/*     1)  Find the intersecton of an ellipse with a plane.  The ellipse */
/*         is defined by the vectors CENTER, VEC1, and VEC2.  The plane */
/*         is defined by the normal vector N and the constant C. */

/*            C */
/*            C    Make a SPICELIB ellipse.  Make a plane while */
/*            C    we're at it. */
/*            C */
/*                 CALL CGV2EL ( CENTER, VEC1, VEC2,  ELLIPS  ) */
/*                 CALL NVC2PL ( N,      C,           PLANE   ) */

/*            C */
/*            C    Find the intersection of the ellipse and plane. */
/*            C    NXPTS is the number of intersection points; XPT1 */
/*            C    and XPT2 are the points themselves. */
/*            C */
/*                 CALL INELPL ( ELLIPS, PLANE, NXPTS, XPT1, XPT2 ) */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 02-NOV-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     center and generating vectors to ellipse */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     SPICELIB ellipses contain a center vector, a semi-major */
/*     axis vector, and a semi-minor axis vector.  These are */
/*     located, respectively, in elements */

/*        CTRPOS through CTRPOS + 1 */

/*        MAJPOS through MAJPOS + 1 */

/*        MINPOS through MINPOS + 1 */



/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("CGV2EL", (ftnlen)6);
    }

/*     The center of the ellipse is held in the first three elements. */

    vequ_(center, ellips);

/*     Find the semi-axes of the ellipse.  These may be degenerate. */

    saelgv_(vec1, vec2, &ellips[3], &ellips[6]);
    chkout_("CGV2EL", (ftnlen)6);
    return 0;
} /* cgv2el_ */

