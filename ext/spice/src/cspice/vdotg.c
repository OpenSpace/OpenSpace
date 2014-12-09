/* vdotg.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      VDOTG ( Vector dot product, general dimension ) */
doublereal vdotg_(doublereal *v1, doublereal *v2, integer *ndim)
{
    /* System generated locals */
    integer i__1;
    doublereal ret_val;

    /* Local variables */
    integer i__;

/* $ Abstract */

/*      Compute the dot product of two vectors of arbitrary dimension. */

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

/*      None. */

/* $ Keywords */

/*      VECTOR */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*       V1        I     First vector in the dot product. */
/*       V2        I     Second vector in the dot product. */
/*       NDIM      I     Dimension of V1 and V2. */

/*       The function returns the value of the dot product of V1 and V2. */

/* $ Detailed_Input */

/*      V1      This may be any double precision vector of arbitrary */
/*              dimension. */

/*      V2      This may be any double precision vector of arbitrary */
/*              dimension. */

/* $ Detailed_Output */

/*      The function returns the value of the dot product of V1 and V2. */

/* $ Parameters */

/*      None. */

/* $ Particulars */

/*      VDOTG calculates the dot product of V1 and V2 by a simple */
/*      application of the definition.  No error checking is */
/*      performed to prevent or recover from numeric overflow. */

/* $ Examples */

/*      Suppose that given two n-dimensional vectors, we want to change */
/*      one of the vectors until the two vectors are perpendicular. */
/*      The following code fragment demonstrates the use of VDOT to do */
/*      so. */

/*      DOT = VDOTG ( V1, V2, NDIM ) */

/*      DO WHILE ( DOT .NE. 0.0D0 ) */
/*         change one of the vectors */
/*         DOT = VDOTG ( V1, V2, NDIM  ) */
/*      END DO */

/* $ Restrictions */

/*      The user is responsible for determining that the vectors V1 and */
/*      V2 are not so large as to cause numeric overflow.  In most cases */
/*      this won't present a problem. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*      None. */

/* $ Author_and_Institution */

/*      W.M. Owen       (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) */

/* -& */
/* $ Index_Entries */

/*     dot product of n-dimensional vectors */

/* -& */

    ret_val = 0.;
    i__1 = *ndim;
    for (i__ = 1; i__ <= i__1; ++i__) {
	ret_val += v1[i__ - 1] * v2[i__ - 1];
    }
    return ret_val;
} /* vdotg_ */

