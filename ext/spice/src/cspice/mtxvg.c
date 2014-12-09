/* mtxvg.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure  MTXVG ( Matrix transpose times vector, general dimension ) */
/* Subroutine */ int mtxvg_(doublereal *m1, doublereal *v2, integer *nc1, 
	integer *nr1r2, doublereal *vout)
{
    /* System generated locals */
    integer m1_dim1, m1_dim2, m1_offset, v2_dim1, vout_dim1, i__1, i__2, i__3,
	     i__4;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer i__, k;
    doublereal sum;

/* $ Abstract */

/*      Multiply the transpose of a matrix and a vector of */
/*      arbitrary size. */

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

/*      MATRIX,  VECTOR */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*       M1        I     Left-hand matrix whose transpose is to be */
/*                       multiplied. */
/*       V2        I     Right-hand vector to be multiplied. */
/*       NC1       I     Column dimension of M1 and length of VOUT. */
/*       NR1R2     I     Row dimension of M1 and length of V2. */
/*       VOUT      O     Product vector M1**T * V2. */
/*                       VOUT must NOT overwrite either M1 or V2. */

/* $ Detailed_Input */

/*      M1      This is a double precision matrix of arbitrary size whose */
/*              transpose forms the left-hand matrix of the */
/*              multiplication. */

/*      V2      This is a double precision vector on the right of the */
/*              multiplication. */

/*      NC1     This is the column dimension of M1 and length of VOUT. */

/*      NR1R2   This is the row dimension of M1 and length of V2. */

/* $ Detailed_Output */

/*      VOUT    This is the double precision vector which results from */
/*              the expression */

/*                         T */
/*              VOUT = (M1)  x V2 */

/*              where the T denotes the transpose of M1. */

/* $ Parameters */

/*     None. */

/* $ Particulars */

/*      The code reflects precisely the following mathematical expression */

/*      For each value of the subscript I from 1 to NC1, */

/*      VOUT(I) = Summation from K=1 to NR1R2 of  ( M1(K,I) * V2(K) ) */

/*      Note that the reversal of the K and I subscripts in the left-hand */
/*      matrix M1 is what makes VOUT the product of the TRANSPOSE of M1 */
/*      and not simply of M1 itself. */

/*      Since this subroutine operates on matrices of arbitrary size, it */
/*      is not feasible to buffer intermediate results.  Thus, VOUT */
/*      should NOT overwrite either M1 or V2. */

/* $ Examples */

/*                        | 1  2 | */
/*      Suppose that M1 = | 1  3 | */
/*                        | 1  4 | */

/*                        | 1 | */
/*      and that     V2 = | 2 | */
/*                        | 3 | */

/*      Then calling MTXVG according to the following calling sequence */

/*      CALL MTXVG (M1, V2, 2, 3, VOUT) */

/*      will yield the following vector value for VOUT */

/*      VOUT = | 6  | */
/*             | 20 | */

/* $ Restrictions */

/*      1) The user is responsible for checking the magnitudes of the */
/*      elements of M1 and V2 so that a floating point overflow does */
/*      not occur. */
/*      2) VOUT not overwrite M1 or V2 or else the intermediate */
/*      will affect the final result. */

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

/*     matrix_transpose times n-dimensional vector */

/* -& */

/*     Local variables */


/*  Perform the matrix-vector multiplication */

    /* Parameter adjustments */
    vout_dim1 = *nc1;
    v2_dim1 = *nr1r2;
    m1_dim1 = *nr1r2;
    m1_dim2 = *nc1;
    m1_offset = m1_dim1 + 1;

    /* Function Body */
    i__1 = *nc1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	sum = 0.;
	i__2 = *nr1r2;
	for (k = 1; k <= i__2; ++k) {
	    sum += m1[(i__3 = k + i__ * m1_dim1 - m1_offset) < m1_dim1 * 
		    m1_dim2 && 0 <= i__3 ? i__3 : s_rnge("m1", i__3, "mtxvg_",
		     (ftnlen)183)] * v2[(i__4 = k - 1) < v2_dim1 && 0 <= i__4 
		    ? i__4 : s_rnge("v2", i__4, "mtxvg_", (ftnlen)183)];
	}
	vout[(i__2 = i__ - 1) < vout_dim1 && 0 <= i__2 ? i__2 : s_rnge("vout",
		 i__2, "mtxvg_", (ftnlen)186)] = sum;
    }
    return 0;
} /* mtxvg_ */

