/* vlcom.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      VLCOM ( Vector linear combination, 3 dimensions ) */
/* Subroutine */ int vlcom_(doublereal *a, doublereal *v1, doublereal *b, 
	doublereal *v2, doublereal *sum)
{
/* $ Abstract */

/*      Compute a vector linear combination of two double precision, */
/*      3-dimensional vectors. */

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

/*      VECTOR */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      A          I   Coefficient of V1 */
/*      V1         I   Vector in 3-space */
/*      B          I   Coefficient of V2 */
/*      V2         I   Vector in 3-space */
/*      SUM        O   Linear Vector Combination A*V1 + B*V2 */

/* $ Detailed_Input */

/*      A     This double precision variable multiplies V1. */
/*      V1    This is an arbitrary, double precision 3-dimensional */
/*            vector. */
/*      B     This double precision variable multiplies V2. */
/*      V2    This is an arbitrary, double precision 3-dimensional */
/*            vector. */

/* $ Detailed_Output */

/*      SUM   is an arbitrary, double precision 3-dimensional vector */
/*            which contains the linear combination A*V1 + B*V2. */

/* $ Parameters */

/*      None. */

/* $ Particulars */

/*      For each index from 1 to 3, this routine implements in FORTRAN */
/*      code the expression: */

/*      SUM(I) = A*V1(I) + B*V2(I) */

/*      No error checking is performed to guard against numeric overflow. */

/* $ Examples */

/*      To generate a sequence of points on an ellipse with major */
/*      and minor axis vectors MAJOR and MINOR, one could use the */
/*      following code fragment */

/*            STEP = TWOPI()/ N */
/*            ANG  = 0.0D0 */

/*            DO I = 0,N */

/*               CALL VLCOM ( DCOS(ANG),MAJOR,  DSIN(ANG),MINOR,  POINT ) */

/*               do something with the ellipse point just constructed */

/*               ANG = ANG + STEP */

/*            END DO */

/*      As a second example, suppose that U and V are orthonormal vectors */
/*      that form a basis of a plane. Moreover suppose that we wish to */
/*      project a vector X onto this plane, we could use the following */
/*      call inserts this projection into PROJ. */

/*            CALL VLCOM ( VDOT(X,V),V,   VDOT(X,U),U,    PROJ ) */


/* $ Restrictions */

/*      No error checking is performed to guard against numeric overflow */
/*      or underflow.  The user is responsible for insuring that the */
/*      input values are reasonable. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*      None */

/* $ Author_and_Institution */

/*      W.L. Taber      (JPL) */

/* $ Literature_References */

/*      None */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     linear combination of two 3-dimensional vectors */

/* -& */
/* $ Revisions */

/* -     Beta Version 1.0.1, 1-Feb-1989 (WLT) */

/*      Example section of header upgraded. */

/* -& */
    sum[0] = *a * v1[0] + *b * v2[0];
    sum[1] = *a * v1[1] + *b * v2[1];
    sum[2] = *a * v1[2] + *b * v2[2];
    return 0;
} /* vlcom_ */

