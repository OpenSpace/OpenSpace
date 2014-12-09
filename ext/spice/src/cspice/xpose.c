/* xpose.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      XPOSE ( Transpose a matrix, 3x3 ) */
/* Subroutine */ int xpose_(doublereal *m1, doublereal *mout)
{
    doublereal temp;

/* $ Abstract */

/*     Transpose a 3x3 matrix. */

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

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     M1        I     Matrix to be transposed. */
/*     MOUT      O    Transposed matrix (M1)**T. */

/* $ Detailed_Input */

/*     M1      This variable may contain any double precision 3x3 */
/*             matrix. */

/* $ Detailed_Output */

/*     MOUT    This variable is a double precision, 3x3 matrix which */
/*             contains the transpose of M1. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     XPOSE first copies the diagonal elements of M1 to MOUT.  Then */
/*     the off-diagonal elements are transposed using a temporary */
/*     variable in the following order: (1,2) <---> (2,1), */
/*     (1,3) <---> (3,1) and finally (2,3) <---> (3,2). */

/* $ Examples */

/*     Given below is one example of a matrix M1 with the output matrix */
/*     MOUT which is implied by M1. */

/*           | 1  2  3 |                | 1  0  0 | */
/*      M1 = | 0  4  5 |   then  MOUT = | 2  4  6 | */
/*           | 0  6  0 |                | 3  5  0 | */

/* $ Restrictions */

/*     None. */

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

/*     transpose a 3x3_matrix */

/* -& */

/*  Move the three diagonal elements from M1 to MOUT */

    mout[0] = m1[0];
    mout[4] = m1[4];
    mout[8] = m1[8];

/*  Switch the three pairs of off-diagonal elements */

    temp = m1[3];
    mout[3] = m1[1];
    mout[1] = temp;

    temp = m1[6];
    mout[6] = m1[2];
    mout[2] = temp;

    temp = m1[7];
    mout[7] = m1[5];
    mout[5] = temp;

    return 0;
} /* xpose_ */

