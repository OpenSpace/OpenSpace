/* vsubg.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      VSUBG ( Vector subtraction, general dimension ) */
/* Subroutine */ int vsubg_(doublereal *v1, doublereal *v2, integer *ndim, 
	doublereal *vout)
{
    /* System generated locals */
    integer v1_dim1, v2_dim1, vout_dim1, i__1, i__2, i__3, i__4;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer i__;

/* $ Abstract */

/*     Compute the difference between two double precision vectors of */
/*     arbitrary dimension. */

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
/*     V1         I   First vector (minuend). */
/*     V2         I   Second vector (subtrahend). */
/*     NDIM       I   Dimension of V1, V2, and VOUT. */
/*     VOUT       O   Difference vector, V1 - V2. */

/* $ Detailed_Input */

/*     V1      is a double precision vector of arbitrary dimension which */
/*             is the minuend (i.e. first or left-hand member) in the */
/*             vector subtraction. */

/*     V2      is a double precision vector of arbitrary dimension which */
/*             is the subtrahend (i.e. second or right-hand member) in */
/*             the vector subtraction. */

/*     NDIM    is the dimension of V1 and V2 (and VOUT). */

/* $ Detailed_Output */

/*     VOUT    is a double precision vector containing the difference */
/*             V1 - V2. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     For each value of the index I from 1 to NDIM, this subroutine */
/*     performs the following subtraction: */

/*        VOUT(I) = V1(I) - V2(I) */

/*     No error checking is performed to guard against numeric overflow */
/*     or underflow. */

/* $ Examples */

/*     The following table shows the results of VSUBG from various */
/*     inputs. */

/*        V1                V2             NDIM         VOUT */
/*        ----------------------------------------------------------- */
/*        (1, 2, 3, 4)     ( 1, 1, 1, 1 )    4         ( 0, 1, 2, 3 ) */
/*        (1, 2, 3, 4)     (-1,-2,-3,-4 )    4         ( 2, 4, 6, 8 ) */
/*        (1, 2, 3, 4)     (-1, 2,-3, 4 )    4         ( 2, 0, 6, 0 ) */

/* $ Restrictions */

/*     No error checking is performed to guard against numeric overflow. */
/*     The programmer is thus required to insure that the values in V1 */
/*     and V2 are reasonable and will not cause overflow. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.M. Owen       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.3, 23-APR-2010 (NJB) */

/*        Header correction: assertions that the output */
/*        can overwrite the input have been removed. */

/* -    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.1, 9-MAY-1990 (HAN) */

/*        Several errors in the header documentation were corrected. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) */

/* -& */
/* $ Index_Entries */

/*     n-dimensional vector subtraction */

/* -& */
    /* Parameter adjustments */
    vout_dim1 = *ndim;
    v2_dim1 = *ndim;
    v1_dim1 = *ndim;

    /* Function Body */
    i__1 = *ndim;
    for (i__ = 1; i__ <= i__1; ++i__) {
	vout[(i__2 = i__ - 1) < vout_dim1 && 0 <= i__2 ? i__2 : s_rnge("vout",
		 i__2, "vsubg_", (ftnlen)152)] = v1[(i__3 = i__ - 1) < 
		v1_dim1 && 0 <= i__3 ? i__3 : s_rnge("v1", i__3, "vsubg_", (
		ftnlen)152)] - v2[(i__4 = i__ - 1) < v2_dim1 && 0 <= i__4 ? 
		i__4 : s_rnge("v2", i__4, "vsubg_", (ftnlen)152)];
    }
    return 0;
} /* vsubg_ */

