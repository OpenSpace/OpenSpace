/* mxmtg.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure  MXMTG  ( Matrix times matrix transpose, general dimension ) */
/* Subroutine */ int mxmtg_(doublereal *m1, doublereal *m2, integer *nr1, 
	integer *nc1c2, integer *nr2, doublereal *mout)
{
    /* System generated locals */
    integer m1_dim1, m1_dim2, m1_offset, m2_dim1, m2_dim2, m2_offset, 
	    mout_dim1, mout_dim2, mout_offset, i__1, i__2, i__3, i__4, i__5;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer i__, j, k;
    doublereal sum;

/* $ Abstract */

/*      Multiply a matrix and the transpose of a matrix, both of */
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

/*      MATRIX */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*       M1        I     Left-hand matrix to be multiplied. */
/*       M2        I     Right-hand matrix whose transpose is to be */
/*                       multiplied. */
/*       NR1       I     Row dimension of M1 and row dimension of MOUT. */
/*       NC1C2     I     Column dimension of M1 and column dimension of */
/*                       M2. */
/*       NR2       I     Row dimension of M2 and column dimension of */
/*                       MOUT. */
/*       MOUT      O     Product matrix M1 * M2**T. */
/*                       MOUT must not overwrite either M1 or M2. */

/* $ Detailed_Input */

/*      M1      M1 may be any double precision matrix of arbitrary size. */

/*      M2      M2 may be any double precision matrix of arbitrary size. */
/*              The number of columns in M2 must match the number of */
/*              columns in M1. */

/*      NR1     The number of rows in both M1 and MOUT. */

/*      NC1C2   The number of columns in M1 and (by necessity) the number */
/*              of columns of M2. */

/*      NR2     The number of rows in both M2 and the number of columns */
/*              in MOUT. */

/* $ Detailed_Output */

/*      MOUT    This is a double precision matrix of dimension NR1 x NR2. */
/*                                                                    T */
/*              MOUT is the product matrix given by MOUT = (M1) x (M2) */
/*              where the superscript "T" denotes the transpose matrix. */

/*              MOUT must not overwrite M1 or M2. */

/* $ Parameters */

/*     None. */

/* $ Particulars */

/*      The code reflects precisely the following mathematical expression */

/*      For each value of the subscript I from 1 to NR1, and J from 1 */
/*      to NR2: */

/*      MOUT(I,J) = Summation from K=1 to NC1C2 of  ( M1(I,K) * M2(J,K) ) */

/*      Notice that the order of the subscripts of M2 are reversed from */
/*      what they would be if this routine merely multiplied M1 and M2. */
/*      It is this transposition of subscripts that makes this routine */
/*      multiply M1 and the TRANPOSE of M2. */

/*      Since this subroutine operates on matrices of arbitrary size, it */
/*      is not feasible to buffer intermediate results.  Thus, MOUT */
/*      should NOT overwrite either M1 or M2. */

/* $ Examples */


/*     Let M1 = | 1.0D0  2.0D0  3.0D0 |      NR1   = 2 */
/*              |                     |      NC1C2 = 3 */
/*              | 3.0D0  2.0D0  1.0D0 |      NR2   = 4 */


/*     Let M2 = | 1.0D0  2.0D0  0.0D0 | */
/*              |                     | */
/*              | 2.0D0  1.0D0  2.0D0 | */
/*              |                     | */
/*              | 1.0D0  2.0D0  0.0D0 | */
/*              |                     | */
/*              | 2.0D0  1.0D0  2.0D0 | */

/*      then the call */

/*      CALL MXMTG ( M1, M2, NR1, NC1C2, NR2, MOUT ) */

/*      produces the matrix */


/*      MOUT = | 5.0D0  10.0D0  5.0D0  10.0D0 | */
/*             |                              | */
/*             | 7.0D0  10.0D0  7.0D0  10.0D0 | */


/* $ Restrictions */

/*      No error checking is performed to prevent numeric overflow or */
/*      underflow. */

/*      No error checking is performed to determine if the input and */
/*      output matrices have, in fact, been correctly dimensioned. */

/*      The user is responsible for checking the magnitudes of the */
/*      elements of M1 and M2 so that a floating point overflow does */
/*      not occur. */

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

/*     matrix times matrix_transpose n-dimensional_case */

/* -& */

/*     Local variables */


/*  Perform the matrix multiplication */

    /* Parameter adjustments */
    m1_dim1 = *nr1;
    m1_dim2 = *nc1c2;
    m1_offset = m1_dim1 + 1;
    mout_dim1 = *nr1;
    mout_dim2 = *nr2;
    mout_offset = mout_dim1 + 1;
    m2_dim1 = *nr2;
    m2_dim2 = *nc1c2;
    m2_offset = m2_dim1 + 1;

    /* Function Body */
    i__1 = *nr1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = *nr2;
	for (j = 1; j <= i__2; ++j) {
	    sum = 0.;
	    i__3 = *nc1c2;
	    for (k = 1; k <= i__3; ++k) {
		sum += m1[(i__4 = i__ + k * m1_dim1 - m1_offset) < m1_dim1 * 
			m1_dim2 && 0 <= i__4 ? i__4 : s_rnge("m1", i__4, 
			"mxmtg_", (ftnlen)206)] * m2[(i__5 = j + k * m2_dim1 
			- m2_offset) < m2_dim1 * m2_dim2 && 0 <= i__5 ? i__5 :
			 s_rnge("m2", i__5, "mxmtg_", (ftnlen)206)];
	    }
	    mout[(i__3 = i__ + j * mout_dim1 - mout_offset) < mout_dim1 * 
		    mout_dim2 && 0 <= i__3 ? i__3 : s_rnge("mout", i__3, 
		    "mxmtg_", (ftnlen)209)] = sum;
	}
    }
    return 0;
} /* mxmtg_ */

