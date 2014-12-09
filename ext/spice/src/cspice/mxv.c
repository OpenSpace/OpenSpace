/* mxv.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      MXV ( Matrix times vector, 3x3 ) */
/* Subroutine */ int mxv_(doublereal *matrix, doublereal *vin, doublereal *
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

/*     Multiply a 3x3 double precision matrix with a 3-dimensional */
/*     double precision vector. */

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
/*     MATRIX     I   3x3 double precision matrix. */
/*     VIN        I   3-dimensional double precision vector. */
/*     VOUT       O   3-dimensinoal double precision vector. VOUT is */
/*                    the product MATRIX*VIN. */

/* $ Detailed_Input */

/*     MATRIX     is an arbitrary 3x3 double precision matrix. */

/*     VIN        is an arbitrary 3-dimensional double precision vector. */

/* $ Detailed_Output */

/*     VOUT       is a 3-dimensional double precision vector. VOUT is */
/*                the product MATRIX * V. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The code reflects precisely the following mathematical expression */

/*        For each value of the subscript I from 1 to 3: */

/*        VOUT(I) = Summation from K=1 to 3 of  ( MATRIX(I,K) * VIN(K) ) */

/* $ Examples */

/*      Let */

/*         MATRIX = |  0.0D0  1.0D0  0.0D0 |   and  VIN = | 1.0D0 | */
/*                  |                      |              |       | */
/*                  | -1.0D0  0.0D0  0.0D0 |              | 2.0D0 | */
/*                  |                      |              |       | */
/*                  |  0.0D0  0.0D0  1.0D0 |              | 3.0D0 | */

/*      Then the call, */

/*         CALL MXV ( MATRIX, VIN, VOUT ) */

/*      produces the vector */

/*         VOUT = |  2.0D0 | */
/*                |        | */
/*                | -1.0D0 | */
/*                |        | */
/*                |  3.0D0 | */


/* $ Restrictions */

/*     The user is responsible for checking the magnitudes of the */
/*     elements of MATRIX and VIN so that a floating point overflow does */
/*     not occur. */

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

/*     matrix times 3-dimensional vector */

/* -& */

/*  Perform the matrix-vector multiplication */

    for (i__ = 1; i__ <= 3; ++i__) {
	prodv[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("prodv", i__1,
		 "mxv_", (ftnlen)157)] = matrix[(i__2 = i__ - 1) < 9 && 0 <= 
		i__2 ? i__2 : s_rnge("matrix", i__2, "mxv_", (ftnlen)157)] * 
		vin[0] + matrix[(i__3 = i__ + 2) < 9 && 0 <= i__3 ? i__3 : 
		s_rnge("matrix", i__3, "mxv_", (ftnlen)157)] * vin[1] + 
		matrix[(i__4 = i__ + 5) < 9 && 0 <= i__4 ? i__4 : s_rnge(
		"matrix", i__4, "mxv_", (ftnlen)157)] * vin[2];
    }

/*  Move the buffered vector into the output vector VOUT. */

    vout[0] = prodv[0];
    vout[1] = prodv[1];
    vout[2] = prodv[2];
    return 0;
} /* mxv_ */

