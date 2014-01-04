/* xf2rav.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      XF2RAV ( Transform to rotation and angular velocity) */
/* Subroutine */ int xf2rav_(doublereal *xform, doublereal *rot, doublereal *
	av)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    doublereal drdt[9]	/* was [3][3] */;
    extern /* Subroutine */ int mtxm_(doublereal *, doublereal *, doublereal *
	    );
    integer i__, j;
    doublereal omega[9]	/* was [3][3] */;

/* $ Abstract */

/*     This routine determines from a state transformation matrix */
/*     the associated rotation matrix and angular velocity of the */
/*     rotation. */

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

/*     FRAMES */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     XFORM      I   is a state transformation matrix */
/*     ROT        O   is the rotation associated with XFORM */
/*     AV         O   is the angular velocity associated with XFORM */

/* $ Detailed_Input */

/*     XFORM       is a state transformation matrix from one frame */
/*                 FRAME1 to some other frame FRAME2. */

/* $ Detailed_Output */

/*     ROT         is a rotation that gives the transformation from */
/*                 some frame FRAME1 to another frame FRAME2. */

/*     AV          is the angular velocity of the transformation. */
/*                 In other words, if P is the position of a fixed */
/*                 point in FRAME2, then from the point of view of */
/*                 FRAME1,  P rotates (in a right handed sense) about */
/*                 an axis parallel to AV.  Moreover the rate of rotation */
/*                 in radians per unit time is given by the length of */
/*                 AV. */

/*                 More formally, the velocity V of P in FRAME1 is */
/*                 given by */
/*                                    t */
/*                     V  = AV x ( ROT * P ) */

/*                 The components of AV are given relative to FRAME1. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1) No checks are performed on XFORM to ensure that it is indeed */
/*        a state transformation matrix. */

/* $ Particulars */

/*     This routine is essentially a macro routine for converting */
/*     state transformation matrices into the equivalent representation */
/*     in terms of a rotation and angular velocity. */

/*     This routine is an inverse of the routine RAV2XF. */

/* $ Examples */

/*     Suppose that you wanted to determine the angular velocity */
/*     of the earth with respect to J2000 at a particular epoch ET. */
/*     The following code fragment illustrates a procedure for */
/*     computing the angular velocity. */

/*        CALL TISBOD ( 'J2000', 399, ET, TSIPM ) */


/*        Now get the angular velocity by calling XF2RAV */

/*        CALL XF2RAV ( TSPMI, TPMI, AV ) */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -   SPICELIB Version 1.1.0, 28-JUL-1997 (WLT) */

/*       The example in version 1.0.0 was incorrect.  The example */
/*       in version 1.1.0 fixes the previous problem. */

/* -    SPICELIB Version 1.0.0, 19-SEP-1995 (WLT) */


/* -& */
/* $ Index_Entries */

/*     State transformation to rotation and angular velocity */

/* -& */

/*     A state transformation matrix XFORM has the following form */


/*         [      |     ] */
/*         |  R   |  0  | */
/*         |      |     | */
/*         | -----+-----| */
/*         |  dR  |     | */
/*         |  --  |  R  | */
/*         [  dt  |     ] */


/*     where R is a rotation and dR/dt is the time derivative of that */
/*     rotation.  From this we can immediately read the rotation and */
/*     its derivative. */

    for (i__ = 1; i__ <= 3; ++i__) {
	for (j = 1; j <= 3; ++j) {
	    rot[(i__1 = i__ + j * 3 - 4) < 9 && 0 <= i__1 ? i__1 : s_rnge(
		    "rot", i__1, "xf2rav_", (ftnlen)178)] = xform[(i__2 = i__ 
		    + j * 6 - 7) < 36 && 0 <= i__2 ? i__2 : s_rnge("xform", 
		    i__2, "xf2rav_", (ftnlen)178)];
	    drdt[(i__1 = i__ + j * 3 - 4) < 9 && 0 <= i__1 ? i__1 : s_rnge(
		    "drdt", i__1, "xf2rav_", (ftnlen)179)] = xform[(i__2 = 
		    i__ + 3 + j * 6 - 7) < 36 && 0 <= i__2 ? i__2 : s_rnge(
		    "xform", i__2, "xf2rav_", (ftnlen)179)];
	}
    }

/*     Recall that ROT is a transformation that converts positions */
/*     in some frame FRAME1 to positions in a second frame FRAME2. */

/*     The angular velocity matrix OMEGA (the cross product matrix */
/*     corresponding to AV) has the following property. */

/*     If P is the position of an object that is stationary with */
/*     respect to FRAME2 then the velocity V of that object in FRAME1 */
/*     is given by: */
/*                          t */
/*         V  =  OMEGA * ROT  *  P */

/*     But V is also given by */

/*                    t */
/*               d ROT */
/*         V =   -----  * P */
/*                 dt */

/*     So that */
/*                                  t */
/*                    t        d ROT */
/*         OMEGA * ROT    =   ------- */
/*                               dt */

/*     Hence */
/*                             t */
/*                       d ROT */
/*         OMEGA    =   -------  *  ROT */
/*                         dt */



    mtxm_(drdt, rot, omega);

/*     Recall that OMEGA has the form */

/*         _                     _ */
/*        |                       | */
/*        |   0    -AV(3)  AV(2)  | */
/*        |                       | */
/*        |  AV(3)    0   -AV(1)  | */
/*        |                       | */
/*        | -AV(2)   AV(1)   0    | */
/*        |_                     _| */

    av[0] = omega[5];
    av[1] = omega[6];
    av[2] = omega[1];
    return 0;
} /* xf2rav_ */

