/* mtxmg.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure   MTXMG ( Matrix transpose times matrix, general dimension ) */
/* Subroutine */ int mtxmg_(doublereal *m1, doublereal *m2, integer *nc1, 
	integer *nr1r2, integer *nc2, doublereal *mout)
{
    /* System generated locals */
    integer m1_dim1, m1_dim2, m1_offset, m2_dim1, m2_dim2, m2_offset, 
	    mout_dim1, mout_dim2, mout_offset, i__1, i__2, i__3, i__4, i__5, 
	    i__6, i__7;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer i__, j, k;

/* $ Abstract */

/*      Multiply the transpose of a matrix with another matrix, */
/*      both of arbitrary size. (The dimensions of the matrices must be */
/*      compatible with this multiplication.) */

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

/*      MATRIX */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*       M1        I     Left-hand matrix whose transpose is to be */
/*                       multiplied. */
/*       M2        I     Right-hand matrix to be multiplied. */
/*       NC1       I     Column dimension of M1 and row dimension of */
/*                       MOUT. */
/*       NR1R2     I     Row dimension of M1 and row dimension of M2. */
/*       NC2       I     Column dimension of M2 and column dimension of */
/*                       MOUT. */
/*       MOUT      O     Product matrix M1**T * M2. */
/*                       MOUT must NOT overwrite either M1 or M2. */

/* $ Detailed_Input */

/*      M1      This is an double precision matrix of arbitrary dimension */
/*              whose transpose is the left hand multiplier of a matrix */
/*              multiplication. */
/*      M2      This is an double precision matrix of arbitrary dimension */
/*              whose transpose is the left hand multiplier of a matrix */
/*              multiplication. */
/*      NC1     This is the column dimension of M1 and row dimension of */
/*              MOUT. */
/*      NR1R2   This is the row dimension of both M1 and M2. */
/*      NC2     This is the column dimension of both M2 and MOUT. */

/* $ Detailed_Output */

/*      MOUT is a double precision matrix containing the product */

/*                        T */
/*             MOUT = (M1)   x (M2) */

/*      where the superscript T denotes the transpose of M1. */

/* $ Parameters */

/*     None. */

/* $ Particulars */

/*      The code reflects precisely the following mathematical expression */

/*      For each value of the subscript I from 1 to NC1, and J from 1 */
/*      to NC2: */

/*      MOUT(I,J) = Summation from K=1 to NR1R2 of  ( M1(K,I) * M2(K,J) ) */

/*      Note that the reversal of the K and I subscripts in the left-hand */
/*      matrix M1 is what makes MOUT the product of the TRANSPOSE of M1 */
/*      and not simply of M1 itself. */

/*      Since this subroutine operates on matrices of arbitrary size, it */
/*      is not possible to buffer intermediate results.  Thus, MOUT */
/*      should NOT overwrite either M1 or M2. */

/* $ Examples */

/*      Suppose that M1 = | 1  2  3  0 | */
/*                        | 1  1  1  1 | */

/*      and that     M2 = | 1  2  3 | */
/*                        | 0  0  0 | */

/*      Then calling MTXMG according to the following calling sequence */

/*      CALL MTXMG (M1, M2, 4, 2, 3, MOUT) */

/*      will yield the following value for MOUT */

/*             | 1  2  3 | */
/*      MOUT = | 2  4  6 | */
/*             | 3  6  9 | */
/*             | 0  0  0 | */

/* $ Restrictions */

/*      1) The user is responsible for checking the magnitudes of the */
/*      elements of M1 and M2 so that a floating point overflow does */
/*      not occur. */
/*      2) MOUT must not overwrite M1 or M2 or else the intermediate */
/*      will affect the final result. */

/* $ Exceptions */

/*      Error free. */

/* $ Files */

/*      None */

/* $ Author_and_Institution */

/*      N.J. Bachman    (JPL) */
/*      W.M. Owen       (JPL) */

/* $ Literature_References */

/*      None */

/* $ Version */

/* -     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*         Comment section for permuted index source lines was added */
/*         following the header. */

/* -     SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) */

/* -& */
/* $ Index_Entries */

/*     matrix_transpose times matrix n-dimensional_case */

/* -& */
/* $ Revisions */

/* -     Beta Version 1.1.0, 16-FEB-1989 (NJB) */

/*         Contents of the Exceptions section was changed */
/*         to "error free" to reflect the decision that the */
/*         module will never participate in error handling. */

/*         Declaration of unused variable SUM removed. */

/* -& */

/*  Perform the matrix multiplication */

    /* Parameter adjustments */
    m1_dim1 = *nr1r2;
    m1_dim2 = *nc1;
    m1_offset = m1_dim1 + 1;
    mout_dim1 = *nc1;
    mout_dim2 = *nc2;
    mout_offset = mout_dim1 + 1;
    m2_dim1 = *nr1r2;
    m2_dim2 = *nc2;
    m2_offset = m2_dim1 + 1;

    /* Function Body */
    i__1 = *nc1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = *nc2;
	for (j = 1; j <= i__2; ++j) {
	    mout[(i__3 = i__ + j * mout_dim1 - mout_offset) < mout_dim1 * 
		    mout_dim2 && 0 <= i__3 ? i__3 : s_rnge("mout", i__3, 
		    "mtxmg_", (ftnlen)196)] = 0.;
	    i__3 = *nr1r2;
	    for (k = 1; k <= i__3; ++k) {
		mout[(i__4 = i__ + j * mout_dim1 - mout_offset) < mout_dim1 * 
			mout_dim2 && 0 <= i__4 ? i__4 : s_rnge("mout", i__4, 
			"mtxmg_", (ftnlen)198)] = mout[(i__5 = i__ + j * 
			mout_dim1 - mout_offset) < mout_dim1 * mout_dim2 && 0 
			<= i__5 ? i__5 : s_rnge("mout", i__5, "mtxmg_", (
			ftnlen)198)] + m1[(i__6 = k + i__ * m1_dim1 - 
			m1_offset) < m1_dim1 * m1_dim2 && 0 <= i__6 ? i__6 : 
			s_rnge("m1", i__6, "mtxmg_", (ftnlen)198)] * m2[(i__7 
			= k + j * m2_dim1 - m2_offset) < m2_dim1 * m2_dim2 && 
			0 <= i__7 ? i__7 : s_rnge("m2", i__7, "mtxmg_", (
			ftnlen)198)];
	    }
	}
    }

    return 0;
} /* mtxmg_ */

