/* vscl.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      VSCL ( Vector scaling, 3 dimensions ) */
/* Subroutine */ int vscl_(doublereal *s, doublereal *v1, doublereal *vout)
{
/* $ Abstract */

/*     Multiply a scalar and a 3-dimensional double precision vector. */

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

/*     VECTOR */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     S          I   Scalar to multiply a vector. */
/*     V1         I   Vector to be multiplied. */
/*     VOUT       O   Product vector, S*V1. */

/* $ Detailed_Input */

/*     S    This is a double precision scalar used to multiply the */
/*          vector V1. */

/*     V1   This is a 3-dimensional, double precision vector which is */
/*          to be scaled by S. */

/* $ Detailed_Output */

/*     VOUT   This is a 3-dimensional, double precision vector which */
/*            is the scalar multiple of V1.  VOUT = S*V1. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     VSCL multiplies each component of V1 by S to form the respective */
/*     components of VOUT.  No error checking is performed. */

/* $ Examples */

/*     The following table shows the output VOUT as a function of the */
/*     the inputs V1, and S from the subroutine VSCL. */

/*        V1                   S         VOUT */
/*        ---------------------------------------------- */
/*        (1D0, -2D0, 0D0)   -1D0       (-1D0, 2D0, 0D0) */
/*        (0D0, 0D0, 0D0)     5D0       (0D0, 0D0, 0D0) */

/* $ Restrictions */

/*     The user is responsible for insuring that no floating point */
/*     overflow occurs from multiplying S by any component of V1. No */
/*     error recovery or reporting scheme is incorporated in this */
/*     subroutine. */

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

/*     3-dimensional vector scaling */

/* -& */
    vout[0] = *s * v1[0];
    vout[1] = *s * v1[1];
    vout[2] = *s * v1[2];
    return 0;
} /* vscl_ */

