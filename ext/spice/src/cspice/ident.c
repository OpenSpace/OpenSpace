/* ident.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      IDENT (Return the 3x3 identity matrix) */
/* Subroutine */ int ident_(doublereal *matrix)
{
/* $ Abstract */

/*    This routine returns the 3x3 identity matrix. */

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
/*     MATRIX     O   is the 3x3 identity matrix */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     MATRIX     is the 3x3 Identity matrix.  That is MATRIX is */
/*                the following */
/*                  _                       _ */
/*                 |  1.0D0   0.0D0   0.0D0  | */
/*                 |  0.0D0   1.0D0   0.0D0  | */
/*                 |  0.0D0   0.0D0   1.0D0  | */
/*                  -                       - */
/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Particulars */

/*     This is a utility routine for obtaining the 3x3 identity matrix */
/*     so that you may avoid having to write the loop or assignments */
/*     needed to get the matrix. */

/* $ Examples */

/*     Suppose that you need to construct the matrix sum */

/*        I + OMEGA */

/*     where OMEGA is some matrix you have already computed. */

/*     The code fragment below shows how you could accomplish this */
/*     with this routine. */

/*        First get the Identity matrix */

/*        DOUBLE PRECISION      I ( 3, 3 ) */

/*        CALL IDENT( I  ) */
/*        CALL VSUMG( I, OMEGA, 9, SUM ) */


/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 5-FEB-1996 (WLT) */


/* -& */
/* $ Index_Entries */

/*     Get the 3x3 identity matrix */

/* -& */
    matrix[0] = 1.;
    matrix[1] = 0.;
    matrix[2] = 0.;
    matrix[3] = 0.;
    matrix[4] = 1.;
    matrix[5] = 0.;
    matrix[6] = 0.;
    matrix[7] = 0.;
    matrix[8] = 1.;
    return 0;
} /* ident_ */

