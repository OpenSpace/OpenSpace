/* vproj.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      VPROJ ( Vector projection, 3 dimensions ) */
/* Subroutine */ int vproj_(doublereal *a, doublereal *b, doublereal *p)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Local variables */
    doublereal biga, bigb;
    extern /* Subroutine */ int vscl_(doublereal *, doublereal *, doublereal *
	    );
    extern doublereal vdot_(doublereal *, doublereal *);
    doublereal r__[3], t[3], scale;

/* $ Abstract */

/*     VPROJ finds the projection of one vector onto another vector. */
/*     All vectors are 3-dimensional. */

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

/*     VECTOR */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     A          I   The vector to be projected. */
/*     B          I   The vector onto which A is to be projected. */
/*     P          O   The projection of A onto B. */

/* $ Detailed_Input */

/*     A     is a double precision, 3-dimensional vector.  This */
/*           vector is to be projected onto the vector B. */

/*     B     is a double precision, 3-dimensional vector.  This */
/*           vector is the vector which receives the projection. */

/* $ Detailed_Output */

/*     P     is a double precision, 3-dimensional vector containing the */
/*           projection of A onto B.  (P is necessarily parallel to B.) */
/*           If B is the zero vector then P will be returned as the zero */
/*           vector. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The given any vectors A and B there is a unique decomposition of */
/*     A as a sum V + P such that V  the dot product of V and B is zero, */
/*     and the dot product of P with B is equal the product of the */
/*     lengths of P and B.  P is called the projection of A onto B.  It */
/*     can be expressed mathematically as */

/*        DOT(A,B) */
/*        -------- * B */
/*        DOT(B,B) */

/*     (This is not necessarily the prescription used to compute the */
/*     projection. It is intended only for descriptive purposes.) */

/* $ Examples */

/*     The following table gives sample inputs and results from calling */
/*     VPROJ. */

/*        A                  B           NDIM               P */
/*        ------------------------------------------------------- */
/*        (6, 6, 6)      ( 2, 0, 0)        3            (6, 0, 0) */
/*        (6, 6, 6)      (-3, 0, 0)        3            (6, 0, 0) */
/*        (6, 6, 0)      ( 0, 7, 0)        3            (0, 6, 0) */
/*        (6, 0, 0)      ( 0, 0, 9)        3            (0, 0, 0) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     Any reasonable calculus text (for example Thomas) */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.2, 23-APR-2010 (NJB) */

/*        Header correction: assertions that the output */
/*        can overwrite the input have been removed. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     3-dimensional vector projection */

/* -& */


/* Computing MAX */
    d__1 = abs(a[0]), d__2 = abs(a[1]), d__1 = max(d__1,d__2), d__2 = abs(a[2]
	    );
    biga = max(d__1,d__2);
/* Computing MAX */
    d__1 = abs(b[0]), d__2 = abs(b[1]), d__1 = max(d__1,d__2), d__2 = abs(b[2]
	    );
    bigb = max(d__1,d__2);
    if (biga == 0.) {
	p[0] = 0.;
	p[1] = 0.;
	p[2] = 0.;
	return 0;
    }
    if (bigb == 0.) {
	p[0] = 0.;
	p[1] = 0.;
	p[2] = 0.;
	return 0;
    }
    r__[0] = b[0] / bigb;
    r__[1] = b[1] / bigb;
    r__[2] = b[2] / bigb;
    t[0] = a[0] / biga;
    t[1] = a[1] / biga;
    t[2] = a[2] / biga;
    scale = vdot_(t, r__) * biga / vdot_(r__, r__);
    vscl_(&scale, r__, p);
    return 0;
} /* vproj_ */

