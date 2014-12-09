/* invert.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b2 = 0.;
static integer c__9 = 9;

/* $Procedure      INVERT ( Invert a 3x3 matrix ) */
/* Subroutine */ int invert_(doublereal *m1, doublereal *mout)
{
    doublereal mdet;
    extern /* Subroutine */ int filld_(doublereal *, integer *, doublereal *),
	     vsclg_(doublereal *, doublereal *, integer *, doublereal *);
    doublereal mtemp[9]	/* was [3][3] */, invdet;
    extern doublereal det_(doublereal *);

/* $ Abstract */

/*      Generate the inverse of a 3x3 matrix. */

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

/*     MATRIX,  MATH */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     M1         I   Matrix to be inverted. */
/*     MOUT       O   Inverted matrix (M1)**-1.  If M1 is singular, then */
/*                    MOUT will be the zero matrix. */

/* $ Detailed_Input */

/*     M1    An arbitrary 3x3 matrix.  The limits on the size of */
/*           elements of M1 are determined by the process of calculating */
/*           the cofactors of each element of the matrix.  For a 3x3 */
/*           matrix this amounts to the differencing of two terms, each */
/*           of which consists of the multiplication of two matrix */
/*           elements.  This multiplication must not exceed the range of */
/*           double precision numbers or else an overflow error will */
/*           occur. */

/* $ Detailed_Output */

/*     MOUT  is the inverse of M1 and is calculated explicitly using */
/*           the matrix of cofactors.  MOUT is set to be the zero matrix */
/*           if M1 is singular. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     First the determinant is explicitly calculated using the */
/*     fundamental definition of the determinant.  If this value is less */
/*     that 10**-16 then the matrix is deemed to be singular and the */
/*     output value is filled with zeros.  Otherwise, the output matrix */
/*     is calculated an element at a time by generating the cofactor of */
/*     each element.  Finally, each element in the matrix of cofactors */
/*     is multiplied by the reciprocal of the determinant and the result */
/*     is the inverse of the original matrix. */

/*     NO INTERNAL CHECKING ON THE INPUT MATRIX M1 IS PERFORMED EXCEPT */
/*     ON THE SIZE OF ITS DETERMINANT.  THUS IT IS POSSIBLE TO GENERATE */
/*     A FLOATING POINT OVERFLOW OR UNDERFLOW IN THE PROCESS OF */
/*     CALCULATING THE MATRIX OF COFACTORS. */

/* $ Examples */

/*     Suppose that M1 is given by the following matrix equation: */

/*             | 0   -1    0 | */
/*        M1 = | 0.5  0    0 | */
/*             | 0    0    1 | */

/*     If INVERT is called according to the FORTRAN code: */

/*        CALL INVERT (M1, M1) */

/*     then M1 will be set to be: */

/*             | 0    2    0 | */
/*        M1 = |-1    0    0 | */
/*             | 0    0    1 | */

/* $ Restrictions */

/*     The input matrix must be such that generating the cofactors will */
/*     not cause a floating point overflow or underflow.  The strictness */
/*     of this condition depends, of course, on the computer */
/*     installation and the resultant maximum and minimum values of */
/*     double precision numbers. */

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

/*     invert a 3x3_matrix */

/* -& */

/*  Find the determinant of M1 and check for singularity */

    mdet = det_(m1);
    if (abs(mdet) < 1e-16) {
	filld_(&c_b2, &c__9, mout);
	return 0;
    }

/*  Get the cofactors of each element of M1 */

    mtemp[0] = m1[4] * m1[8] - m1[5] * m1[7];
    mtemp[3] = -(m1[3] * m1[8] - m1[5] * m1[6]);
    mtemp[6] = m1[3] * m1[7] - m1[4] * m1[6];
    mtemp[1] = -(m1[1] * m1[8] - m1[2] * m1[7]);
    mtemp[4] = m1[0] * m1[8] - m1[2] * m1[6];
    mtemp[7] = -(m1[0] * m1[7] - m1[1] * m1[6]);
    mtemp[2] = m1[1] * m1[5] - m1[2] * m1[4];
    mtemp[5] = -(m1[0] * m1[5] - m1[2] * m1[3]);
    mtemp[8] = m1[0] * m1[4] - m1[1] * m1[3];

/*  Multiply the cofactor matrix by 1/MDET to obtain the inverse */

    invdet = 1. / mdet;
    vsclg_(&invdet, mtemp, &c__9, mout);

    return 0;
} /* invert_ */

