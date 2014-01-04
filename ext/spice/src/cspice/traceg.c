/* traceg.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure  TRACEG ( Trace of a matrix, general dimension ) */
doublereal traceg_(doublereal *matrix, integer *ndim)
{
    /* System generated locals */
    integer matrix_dim1, matrix_dim2, matrix_offset, i__1, i__2;
    doublereal ret_val;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer i__;

/* $ Abstract */

/*      Return the trace of a square matrix of arbitrary dimension. */

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
/*      MATRIX     I     NDIM x NDIM matrix of double precision numbers. */
/*      NDIM       I     Dimension of the matrix. */
/*      TRACEG     O     The trace of MATRIX. */

/* $ Detailed_Input */

/*      MATRIX  is a double precision square matrix of arbitrary */
/*              dimension.  The input matrix must be square or else */
/*              the concept is meaningless. */

/*      NDIM    is the dimension of MATRIX. */

/* $ Detailed_Output */

/*      TRACEG  is the trace of MATRIX, i.e. it is the sum of the */
/*              diagonal elements of MATRIX. */

/* $ Parameters */

/*     None. */

/* $ Particulars */

/*      TRACEG simply executes in FORTRAN code the following loop: */

/*      TRACEG = Summation from I = 1 to NDIM of MATRIX(I,I) */

/*      No error detection or correction is implemented within this */
/*      function. */

/* $ Examples */

/*                            | 3   5   7 | */
/*      Suppose that MATRIX = | 0  -2   8 |  (with NDIM = 3), then */
/*                            | 3   0  -1 | */

/*      TRACEG (MATRIX, 3) = 0.  (which is the sum of 3, -2 and -1). */

/* $ Restrictions */

/*      No checking is performed to guard against floating point overflow */
/*      or underflow.  This routine should probably not be used if the */
/*      input matrix is expected to have large double precision numbers */
/*      along the diagonal. */

/* $ Exceptions */

/*      Error free. */

/* $ Files */

/*      None. */

/* $ Author_and_Institution */

/*      W.L. Taber      (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*         Comment section for permuted index source lines was added */
/*         following the header. */

/* -     SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     trace of a nxn_matrix */

/* -& */
    /* Parameter adjustments */
    matrix_dim1 = *ndim;
    matrix_dim2 = *ndim;
    matrix_offset = matrix_dim1 + 1;

    /* Function Body */
    ret_val = 0.;
    i__1 = *ndim;
    for (i__ = 1; i__ <= i__1; ++i__) {
	ret_val += matrix[(i__2 = i__ + i__ * matrix_dim1 - matrix_offset) < 
		matrix_dim1 * matrix_dim2 && 0 <= i__2 ? i__2 : s_rnge("matr"
		"ix", i__2, "traceg_", (ftnlen)133)];
    }
    return ret_val;
} /* traceg_ */

