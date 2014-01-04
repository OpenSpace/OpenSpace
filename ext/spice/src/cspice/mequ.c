/* mequ.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__9 = 9;

/* $Procedure      MEQU  ( Matrix equal to another, 3x3 ) */
/* Subroutine */ int mequ_(doublereal *m1, doublereal *mout)
{
    extern /* Subroutine */ int moved_(doublereal *, integer *, doublereal *);

/* $ Abstract */

/*      Set one double precision 3x3 matrix equal to another. */

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

/*      ASSIGNMENT,  MATRIX */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      M1         I     Input matrix. */
/*      MOUT       O     Output matrix equal to M1. */

/* $ Detailed_Input */

/*      M1      This is an arbitrary input 3x3 matrix.  There are no */
/*              restrictions on what it may contain. */

/* $ Detailed_Output */

/*      MOUT    This 3x3 matrix is set to be equal to M1. */

/* $ Parameters */

/*      None. */

/* $ Particulars */

/*      None. */

/* $ Examples */

/*      If  M1 = | 1.0D0   0.0D0   0.0D0 | */
/*               |                       | */
/*               | 0.0D0   1.0D0   0.0D0 | */
/*               |                       | */
/*               | 0.0D0   0.0D0   1.0D0 | */

/*      the call */

/*      CALL MEQU ( M1, MOUT ) */

/*      produces the matrix */

/*       MOUT =  | 1.0D0   0.0D0   0.0D0 | */
/*               |                       | */
/*               | 0.0D0   1.0D0   0.0D0 | */
/*               |                       | */
/*               | 0.0D0   0.0D0   1.0D0 | */


/* $ Restrictions */

/*      None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*      None. */

/* $ Author_and_Institution */

/*      W.M. Owen       (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) */

/* -& */
/* $ Index_Entries */

/*     equal to another 3x3_matrix */

/* -& */
    moved_(m1, &c__9, mout);

    return 0;
} /* mequ_ */

