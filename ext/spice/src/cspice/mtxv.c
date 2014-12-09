/* mtxv.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      MTXV  ( Matrix transpose times vector, 3x3 ) */
/* Subroutine */ int mtxv_(doublereal *matrix, doublereal *vin, doublereal *
	vout)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer i__;
    doublereal prodv[3];

/* $ Abstract */

/*     MTXV multiplies the transpose of a 3x3 matrix on the left with */
/*     a vector on the right. */

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
/*     VECTOR */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O              DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     MATRIX     I   3X3 double precision matrix. */
/*     VIN        I   3-dimensional double precision vector. */
/*     VOUT       O   3-dimensional double precision vector. VOUT is */
/*                    the product MATRIX**T * VIN. */

/* $ Detailed_Input */

/*     MATRIX     is an arbitrary 3x3 double precision matrix. */
/*                Typically, MATRIX will be a rotation matrix since */
/*                then its transpose is its inverse (but this is NOT */
/*                a requirement). */

/*     VIN        is an arbitrary 3-dimensional double precision */
/*                vector. */

/* $ Detailed_Output */

/*     VOUT       is a 3-dimensional double precision vector. VOUT is */
/*                the product VOUT = (MATRIX**T)  x (VIN). */
/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The code reflects precisely the following mathematical expression */

/*        For each value of the subscript I from 1 to 3: */

/*        VOUT(I) = Summation from K=1 to 3 of  ( MATRIX(K,I) * VIN(K) ) */

/*     Note that the reversal of the K and I subscripts in the left-hand */
/*     matrix MATRIX is what makes VOUT the product of the TRANSPOSE of */
/*     and not simply of MATRIX itself. */

/* $ Examples */

/*     Typically the matrix MATRIX will be a rotation matrix. Because */
/*     the transpose of an orthogonal matrix is equivalent to its */
/*     inverse, applying the rotation to the vector is accomplished by */
/*     multiplying the vector by the transpose of the matrix. */

/*                -1 */
/*     Let  MATRIX   * VIN = VOUT. If MATRIX is an orthogonal matrix, */
/*     then  (MATRIX**T) * VIN = VOUT. */


/*     If MATRIX  = |  1.0D0  1.0D0  0.0D0 |   and  VIN = |  5.0D0 | */
/*                  |                      |              |        | */
/*                  | -1.0D0  1.0D0  0.0D0 |              | 10.0D0 | */
/*                  |                      |              |        | */
/*                  |  0.0D0  0.0D0  1.0D0 |              | 15.0D0 | */


/*     then the call */

/*        CALL MTXV ( MATRIX, VIN, VOUT ) */

/*     produces the vector */


/*        VOUT = | -5.0D0 | */
/*               |        | */
/*               | 15.0D0 | */
/*               |        | */
/*               | 15.0D0 | */


/* $ Restrictions */

/*     The user is responsible for checking the magnitudes of the */
/*     elements of MATRIX and VIN so that a floating point overflow does */
/*     not occur. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.M. Owen       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.2, 23-APR-2010 (NJB) */

/*        Header correction: assertions that the output */
/*        can overwrite the input have been removed. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) */

/* -& */
/* $ Index_Entries */

/*     matrix_transpose times 3-dimensional vector */

/* -& */

/*     Local variables */


/*  Perform the matrix-vector multiplication */

    for (i__ = 1; i__ <= 3; ++i__) {
	prodv[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("prodv", i__1,
		 "mtxv_", (ftnlen)179)] = matrix[(i__2 = i__ * 3 - 3) < 9 && 
		0 <= i__2 ? i__2 : s_rnge("matrix", i__2, "mtxv_", (ftnlen)
		179)] * vin[0] + matrix[(i__3 = i__ * 3 - 2) < 9 && 0 <= i__3 
		? i__3 : s_rnge("matrix", i__3, "mtxv_", (ftnlen)179)] * vin[
		1] + matrix[(i__4 = i__ * 3 - 1) < 9 && 0 <= i__4 ? i__4 : 
		s_rnge("matrix", i__4, "mtxv_", (ftnlen)179)] * vin[2];
    }

/*  Move the result into VOUT */

    vout[0] = prodv[0];
    vout[1] = prodv[1];
    vout[2] = prodv[2];
    return 0;
} /* mtxv_ */

