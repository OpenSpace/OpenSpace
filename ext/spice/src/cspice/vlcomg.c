/* vlcomg.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      VLCOMG ( Vector linear combination, general dimension ) */
/* Subroutine */ int vlcomg_(integer *n, doublereal *a, doublereal *v1, 
	doublereal *b, doublereal *v2, doublereal *sum)
{
    /* System generated locals */
    integer v1_dim1, v2_dim1, sum_dim1, i__1, i__2, i__3, i__4;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer i__;

/* $ Abstract */

/*      Compute a vector linear combination of two double precision */
/*      vectors of arbitrary dimension. */

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

/*      VECTOR */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      N          I   Dimension of vector space */
/*      A          I   Coefficient of V1 */
/*      V1         I   Vector in N-space */
/*      B          I   Coefficient of V2 */
/*      V2         I   Vector in N-space */
/*      SUM        O   Linear Vector Combination A*V1 + B*V2 */

/* $ Detailed_Input */

/*      N   This variable contains the dimension of the V1, V2 and SUM. */
/*      A   This double precision variable multiplies V1. */
/*      V1  This is an arbitrary, double precision N-dimensional vector. */
/*      B   This double precision variable multiplies V2. */
/*      V2  This is an arbitrary, double precision N-dimensional vector. */

/* $ Detailed_Output */

/*      SUM   is an arbitrary, double precision N-dimensional vector */
/*            which contains the linear combination A*V1 + B*V2. */

/* $ Parameters */

/*      None. */

/* $ Particulars */

/*      For each index from 1 to N, this routine implements in FORTRAN */
/*      code the expression: */

/*      SUM(I) = A*V1(I) + B*V2(I) */

/*      No error checking is performed to guard against numeric overflow. */

/* $ Examples */

/*      We can easily use this routine to perform vector projections */
/*      to 2-planes in N-space.  Let X be an arbitray N-vector */
/*      and let U and V be orthonormal N-vectors spanning the plane */
/*      of interest.  The projection of X onto this 2-plane, PROJUV can */
/*      be obtained by the following code fragment. */

/*         CALL VLCOMG ( N, VDOT(X,U,N), U,   VDOT(X,V,N), V,   PROJUV ) */

/* $ Restrictions */

/*      No error checking is performed to guard against numeric overflow */
/*      or underflow.  The user is responsible for insuring that the */
/*      input values are reasonable. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*      None */

/* $ Author_and_Institution */

/*      W.L. Taber      (JPL) */

/* $ Literature_References */

/*      None */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     linear combination of two n-dimensional vectors */

/* -& */
/* $ Revisions */

/* -     Beta Version 1.0.1, 1-Feb-1989 (WLT) */

/*      Example section of header upgraded. */

/* -& */
    /* Parameter adjustments */
    sum_dim1 = *n;
    v2_dim1 = *n;
    v1_dim1 = *n;

    /* Function Body */
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sum[(i__2 = i__ - 1) < sum_dim1 && 0 <= i__2 ? i__2 : s_rnge("sum", 
		i__2, "vlcomg_", (ftnlen)150)] = *a * v1[(i__3 = i__ - 1) < 
		v1_dim1 && 0 <= i__3 ? i__3 : s_rnge("v1", i__3, "vlcomg_", (
		ftnlen)150)] + *b * v2[(i__4 = i__ - 1) < v2_dim1 && 0 <= 
		i__4 ? i__4 : s_rnge("v2", i__4, "vlcomg_", (ftnlen)150)];
    }
    return 0;
} /* vlcomg_ */

