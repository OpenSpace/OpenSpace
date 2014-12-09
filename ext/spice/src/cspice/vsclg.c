/* vsclg.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      VSCLG ( Vector scaling, general dimension ) */
/* Subroutine */ int vsclg_(doublereal *s, doublereal *v1, integer *ndim, 
	doublereal *vout)
{
    /* System generated locals */
    integer v1_dim1, vout_dim1, i__1, i__2, i__3;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer i__;

/* $ Abstract */

/*     Multiply a scalar and a double precision vector of arbitrary */
/*     dimension. */

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
/*     S          I   Scalar to multiply a vector. */
/*     V1         I   Vector to be multiplied. */
/*     NDIM       I   Dimension of V1 (and also VOUT). */
/*     VOUT       O   Product vector, S*V1. */

/* $ Detailed_Input */

/*     S      is a double precision scalar. */

/*     V1     is a double precision vector of arbitrary dimension. */

/*     NDIM   is the dimension of V1 (and VOUT). */

/* $ Detailed_Output */

/*     VOUT   is a double precision vector of arbitrary dimension */
/*            containing the product of the scalar with the vector V1. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     For each value of the index I from 1 to NDIM, this subroutine */
/*     performs the following multiplication */

/*        VOUT(I) = S * V1(I) */

/*     No error checking is performed to guard against numeric overflow */
/*     or underflow. */

/* $ Examples */

/*     The following table shows the results of VSCLG from various */
/*     inputs. */

/*        V1                 S           NDIM        VOUT */
/*        ---------------------------------------------------------- */
/*        (1, 2, -3, 4)      3            4         ( 3,  6, -9, 12) */
/*        (1, 2, -3, 4)      0            4         ( 0,  0,  0,  0) */
/*        (1, 2, -3, 4)     -1            4         (-3, -6,  9,-12) */

/* $ Restrictions */

/*     No error checking is performed to guard against numeric overflow. */
/*     The programmer is thus required to insure that the values in V1 */
/*     and S are reasonable and will not cause overflow. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.M. Owen       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.2, 22-APR-2010 (NJB) */

/*        Header correction: assertions that the output */
/*        can overwrite the input have been removed. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) */

/* -& */
/* $ Index_Entries */

/*     n-dimensional vector scaling */

/* -& */
    /* Parameter adjustments */
    vout_dim1 = *ndim;
    v1_dim1 = *ndim;

    /* Function Body */
    i__1 = *ndim;
    for (i__ = 1; i__ <= i__1; ++i__) {
	vout[(i__2 = i__ - 1) < vout_dim1 && 0 <= i__2 ? i__2 : s_rnge("vout",
		 i__2, "vsclg_", (ftnlen)145)] = *s * v1[(i__3 = i__ - 1) < 
		v1_dim1 && 0 <= i__3 ? i__3 : s_rnge("v1", i__3, "vsclg_", (
		ftnlen)145)];
    }
    return 0;
} /* vsclg_ */

