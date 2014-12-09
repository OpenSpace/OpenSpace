/* invstm.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__6 = 6;
static integer c__3 = 3;

/* $Procedure      INVSTM ( Inverse of state transformation matrix) */
/* Subroutine */ int invstm_(doublereal *mat, doublereal *invmat)
{
    extern /* Subroutine */ int chkin_(char *, ftnlen), chkout_(char *, 
	    ftnlen), xposbl_(doublereal *, integer *, integer *, integer *, 
	    doublereal *);
    extern logical return_(void);

/* $ Abstract */

/*     Return the inverse of a state transformation matrix. */

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

/*     ROTATION */

/* $ Keywords */

/*     MATH */
/*     MATRIX */
/*     TRANSFORMATION */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     MAT        I   A state transformation matrix. */
/*     INVMAT     O   The inverse of MAT. */

/* $ Detailed_Input */

/*     MAT        is a state transformation matrix for converting states */
/*                relative to one frame to states relative to another. */
/*                The state transformation of a state vector, S, is */
/*                performed by the matrix-vector product. */

/*                    MAT * S. */

/*                For MAT to be a "true" state transformation matrix */
/*                it must have the form */

/*                      -            - */
/*                     |       :      | */
/*                     |   R   :   0  | */
/*                     |.......:......| */
/*                     |       :      | */
/*                     |  W*R  :   R  | */
/*                     |       :      | */
/*                      -            - */

/*                where R is a 3x3 rotation matrix and, 0 is the 3x3 zero */
/*                matrix and W is a 3x3 skew-symmetric matrix. */

/*                NOTE: no checks are performed on MAT to ensure that it */
/*                      does indeed have the form described above. */

/* $ Detailed_Output */

/*     INVMAT     is the inverse of MAT under the operation of matrix */
/*                multiplication. */

/*                If MAT has the form described above, then INVMAT has */
/*                the form shown below. */

/*                      -             - */
/*                     |     t  :      | */
/*                     |    R   :   0  | */
/*                     |........:......| */
/*                     |      t :    t | */
/*                     | (W*R)  :   R  | */
/*                     |        :      | */
/*                      -            - */

/*                (The superscript "t" denotes the matrix transpose */
/*                operation.) */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) No checks are performed, to insure that the input matrix is */
/*        indeed a state transformation matrix. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Given a matrix for transforming states relative frame 1 to */
/*     states relative frame 2,  the routine produces the inverse */
/*     matrix.  That is, it returns the matrix for transforming states */
/*     relative to frame 2 to states relative to frame 1. */

/*     This special routine exists because unlike the inverse of a */
/*     rotation matrix, the inverse of a state transformation matrix, */
/*     is NOT simply the transpose of the of the matrix. */

/* $ Examples */

/*     Suppose you had a geometric state, STATE,  of a spacecraft in */
/*     earth bodyfixed coordinates and wished to express this state */
/*     relative to earth centered J2000 coordinates.  The following */
/*     code fragment illustrates how to carry out this computation. */

/*     C */
/*     C     First get the state transformation from J2000 to earth */
/*     C     bodyfixed coordinates at the time of interest ET. */
/*     C */
/*           EARTH = 399 */
/*           J2000 = 'J2000' */

/*           CALL TISBOD ( J2000, EARTH, ET, MAT ) */

/*     C */
/*     C     Get the inverse of MAT */
/*     C */
/*           CALL INVSTM ( MAT,  INVMAT          ) */

/*     C */
/*     C     Transform from bodyfixed state to inertial state. */
/*     C */
/*           CALL MXVG ( INVMAT, STATE, 6, 6, ISTATE ) */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.2, 22-APR-2010 (NJB) */

/*        Header correction: assertions that the output */
/*        can overwrite the input have been removed. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 29-OCT-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     inverse of state transformation matrix */

/* -& */

/*     SPICELIB functions */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("INVSTM", (ftnlen)6);
    }

/*     Not much to this.  Just call the more general routine XPOSBL. */

    xposbl_(mat, &c__6, &c__6, &c__3, invmat);

/*     That's all folks. */

    chkout_("INVSTM", (ftnlen)6);
    return 0;
} /* invstm_ */

