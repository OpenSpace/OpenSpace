/* vzerog.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure  VZEROG ( Is a vector the zero vector?---general dim. ) */
logical vzerog_(doublereal *v, integer *ndim)
{
    /* System generated locals */
    integer i__1;
    logical ret_val;

    /* Local variables */
    integer i__;

/* $ Abstract */

/*     Indicate whether a general-dimensional vector is the zero vector. */

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

/*     MATH */
/*     VECTOR */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     V          I   Vector to be tested. */
/*     NDIM       I   Dimension of V. */

/*     The function returns the value .TRUE. if and only if V is the */
/*     zero vector. */

/* $ Detailed_Input */

/*     V, */
/*     NDIM           are, respectively, a vector and its dimension. */

/* $ Detailed_Output */

/*     The function returns the value .TRUE. if and only if V is the */
/*     zero vector. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)   When NDIM is non-positive, this function returns the value */
/*          .FALSE.  (A vector of non-positive dimension cannot be the */
/*          zero vector.) */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This function has the same truth value as the logical expression */

/*        VNORMG ( V, NDIM )  .EQ.  0.D0 */

/*     Replacing the above expression by */

/*        VZEROG ( V, NDIM ) */

/*     has several advantages:  the latter expresses the test more */
/*     clearly, looks better, and doesn't go through the work of scaling, */
/*     squaring, taking a square root, and re-scaling (all of which */
/*     VNORMG must do) just to find out that a vector is non-zero. */

/*     A related function is VZERO, which accepts three-dimensional */
/*     vectors. */

/* $ Examples */

/*     1)  When testing whether a vector is the zero vector, one */
/*         normally constructs tests like */

/*            IF (  VNORMG ( V, NDIM )  .EQ.  0.D0  ) THEN */
/*                        . */
/*                        . */
/*                        . */

/*         These can be replaced with the code */

/*            IF (  VZEROG ( V, NDIM )  ) THEN */
/*                        . */
/*                        . */
/*                        . */

/*     2)  Make sure that a `unit' quaternion is non-zero before */
/*         converting it to a rotation matrix. */

/*            IF (  VZEROG ( Q, 4 )  ) THEN */

/*               [ handle error ] */

/*            ELSE */

/*               CALL VHATG ( Q, 4, Q ) */
/*               CALL Q2M   ( Q, M ) */
/*                        . */
/*                        . */
/*                        . */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     I.M. Underwood (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 18-JUL-1990 (NJB) (IMU) */

/* -& */
/* $ Index_Entries */

/*     test whether an n-dimensional vector is the zero vector */

/* -& */

/*     Local variables */


/*     Leave as soon as we find a non-zero component.  If we get through */
/*     the loop, we have a zero vector, as long as the vector's dimension */
/*     is valid. */

    i__1 = *ndim;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (v[i__ - 1] != 0.) {
	    ret_val = FALSE_;
	    return ret_val;
	}
    }

/*     We have a zero vector if and only if the vector's dimension is at */
/*     least 1. */

    ret_val = *ndim >= 1;
    return ret_val;
} /* vzerog_ */

