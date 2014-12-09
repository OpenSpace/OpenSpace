/* vlcom3.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      VLCOM3 ( Vector linear combination, 3 dimensions ) */
/* Subroutine */ int vlcom3_(doublereal *a, doublereal *v1, doublereal *b, 
	doublereal *v2, doublereal *c__, doublereal *v3, doublereal *sum)
{
/* $ Abstract */

/*      This subroutine computes the vector linear combination */
/*      A*V1 + B*V2 + C*V3 of double precision, 3-dimensional vectors. */

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

/*      None. */

/* $ Keywords */

/*      VECTOR */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      A          I   Coefficient of V1 */
/*      V1         I   Vector in 3-space */
/*      B          I   Coefficient of V2 */
/*      V2         I   Vector in 3-space */
/*      C          I   Coefficient of V3 */
/*      V3         I   Vector in 3-space */
/*      SUM        O   Linear Vector Combination A*V1 + B*V2 + C*V3 */

/* $ Detailed_Input */

/*      A     is a double precision number. */

/*      V1    is a double precision 3-dimensional vector. */

/*      B     is a double precision number. */

/*      V2    is a double precision 3-dimensional vector. */

/*      C     is a double precision number. */

/*      V3    is a double precision 3-dimensional vector. */

/* $ Detailed_Output */

/*      SUM   is a double precision 3-dimensional vector which contains */
/*            the linear combination A*V1 + B*V2 + C*V3 */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*      For each index from 1 to 3, this routine implements in FORTRAN */
/*      code the expression: */

/*      SUM(I) = A*V1(I) + B*V2(I) + C*V3(I) */

/*      No error checking is performed to guard against numeric overflow. */

/* $ Examples */

/*      Often one has the components (A,B,C) of a vector in terms */
/*      of a basis V1, V2, V3.  The vector represented by (A,B,C) can */
/*      be obtained immediately from the call */

/*      CALL VLCOM3 ( A, V1, B, V2, C, V3,   VECTOR ) */

/* $ Restrictions */

/*      None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 1-NOV-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     linear combination of three 3-dimensional vectors */

/* -& */
    sum[0] = *a * v1[0] + *b * v2[0] + *c__ * v3[0];
    sum[1] = *a * v1[1] + *b * v2[1] + *c__ * v3[1];
    sum[2] = *a * v1[2] + *b * v2[2] + *c__ * v3[2];
    return 0;
} /* vlcom3_ */

