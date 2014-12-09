/* vnorm.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      VNORM ( Vector norm, 3 dimensions ) */
doublereal vnorm_(doublereal *v1)
{
    /* System generated locals */
    doublereal ret_val, d__1, d__2, d__3;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    doublereal v1max;

/* $ Abstract */

/*      Compute the magnitude of a double precision, 3-dimensional */
/*      vector. */

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
/*       V1        I     Vector whose magnitude is to be found. */

/* $ Detailed_Input */

/*      V1      This may be any 3-dimensional, double precision vector. */

/* $ Detailed_Output */

/*      VNORM is the magnitude of V1 calculated in a numerically stable */
/*      way. */

/* $ Parameters */

/*     None. */

/* $ Particulars */

/*      VNORM finds the component of V1 whose magnitude is the largest. */
/*      If the absolute magnitude of that component indicates that a */
/*      numeric overflow would occur when it is squared, or if it */
/*      indicates that an underflow would occur when square (giving a */
/*      magnitude of zero) then the following expression is used: */

/*      VNORM = V1MAX * MAGNITUDE OF [ (1/V1MAX)*V1 ] */

/*      Otherwise a simpler expression is used: */

/*      VNORM = MAGNITUDE OF [ V1 ] */

/*      Beyond the logic described above, no further checking of the */
/*      validity of the input is performed. */

/* $ Examples */

/*      The following table show the correlation between various input */
/*      vectors V1 and VNORM: */

/*      V1                                    VNORM */
/*      ----------------------------------------------------------------- */
/*      (1.D0, 2.D0, 2.D0)                     3.D0 */
/*      (5.D0, 12.D0, 0.D0)                   13.D0 */
/*      (-5.D-17, 0.0D0, 12.D-17)             13.D-17 */

/* $ Restrictions */

/*      None. */

/* $ Exceptions */

/*      Error free. */

/* $ Files */

/*      None. */

/* $ Author_and_Institution */

/*      W.M. Owen       (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*         Comment section for permuted index source lines was added */
/*         following the header. */

/* -     SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) */

/* -& */
/* $ Index_Entries */

/*     norm of 3-dimensional vector */

/* -& */

/*  Determine the maximum component of the vector. */

/* Computing MAX */
    d__1 = abs(v1[0]), d__2 = abs(v1[1]), d__1 = max(d__1,d__2), d__2 = abs(
	    v1[2]);
    v1max = max(d__1,d__2);

/*  If the vector is zero, return zero; otherwise normalize first. */
/*  Normalizing helps in the cases where squaring would cause overflow */
/*  or underflow.  In the cases where such is not a problem it not worth */
/*  it to optimize further. */

    if (v1max == 0.) {
	ret_val = 0.;
    } else {
/* Computing 2nd power */
	d__1 = v1[0] / v1max;
/* Computing 2nd power */
	d__2 = v1[1] / v1max;
/* Computing 2nd power */
	d__3 = v1[2] / v1max;
	ret_val = v1max * sqrt(d__1 * d__1 + d__2 * d__2 + d__3 * d__3);
    }

    return ret_val;
} /* vnorm_ */

