/* rav2xf.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      RAV2XF ( Rotation and angular velocity to transform ) */
/* Subroutine */ int rav2xf_(doublereal *rot, doublereal *av, doublereal *
	xform)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer i__, j;
    doublereal omegat[9]	/* was [3][3] */, drotdt[9]	/* was [3][3] 
	    */;
    extern /* Subroutine */ int mxm_(doublereal *, doublereal *, doublereal *)
	    ;

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
/*     ROT        I   rotation matrix */
/*     AV         I   angular velocity vector */
/*     XFORM      O   state transformation associated with ROT and AV */

/* $ Detailed_Input */

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

/* $ Detailed_Output */

/*     XFORM       is a state transformation matrix associated */
/*                 with ROT and AV.  If S1 is the state of an object */
/*                 with respect to FRAME1, then the state S2 of the */
/*                 object with respect to FRAME2 is given by */

/*                     S2  =  XFORM * S1 */

/*                 where "*" denotes Matrix-Vector multiplication. */


/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1) No checks are performed on ROT to ensure that it is indeed */
/*        a rotation matrix. */

/* $ Particulars */

/*     This routine is essentially a macro routine for converting */
/*     a rotation and angular velocity of the rotation to the */
/*     equivalent state transformation matrix. */

/*     This routine is an inverse of XF2RAV */

/* $ Examples */

/*     Suppose that you wanted to determine state transformation */
/*     matrix from a platform frame to J2000. */

/*     CALL CKGPAV ( CKID, TIME, TOL, 'J2000', ROT, AV, CLKOUT, FND ) */

/*     Recall that ROT and AV are the rotation and angular velocity */
/*     of the transformation from J2000 to the platform frame. */

/*     IF ( FND ) THEN */

/*        First get the state transformation from J2000 to the platform */
/*        frame. */

/*        CALL RAV2XF ( ROT,  AV, J2PLT ) */

/*        Invert the state transformation matrix (using INVSTM) to */
/*        the desired state transformation matrix. */

/*        CALL INVSTM ( J2PLT, XFORM ) */

/*     END IF */

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

/* -   SPICELIB Version 1.0.0, 18-SEP-1995 (WLT) */


/* -& */
/* $ Index_Entries */

/*    State transformation to rotation and angular velocity */

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
/*     rotation.  From this we can immediately fill in most of the */
/*     state transformation matrix. */

    for (i__ = 1; i__ <= 3; ++i__) {
	for (j = 1; j <= 3; ++j) {
	    xform[(i__1 = i__ + j * 6 - 7) < 36 && 0 <= i__1 ? i__1 : s_rnge(
		    "xform", i__1, "rav2xf_", (ftnlen)192)] = rot[(i__2 = i__ 
		    + j * 3 - 4) < 9 && 0 <= i__2 ? i__2 : s_rnge("rot", i__2,
		     "rav2xf_", (ftnlen)192)];
	    xform[(i__1 = i__ + 3 + (j + 3) * 6 - 7) < 36 && 0 <= i__1 ? i__1 
		    : s_rnge("xform", i__1, "rav2xf_", (ftnlen)193)] = rot[(
		    i__2 = i__ + j * 3 - 4) < 9 && 0 <= i__2 ? i__2 : s_rnge(
		    "rot", i__2, "rav2xf_", (ftnlen)193)];
	    xform[(i__1 = i__ + (j + 3) * 6 - 7) < 36 && 0 <= i__1 ? i__1 : 
		    s_rnge("xform", i__1, "rav2xf_", (ftnlen)194)] = 0.;
	}
    }

/*     Now for the rest. */

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

/*          d ROT                 t */
/*          -----   =  ROT * OMEGA */
/*            dt */


/*     From this discussion we can see that we need OMEGA transpose. */
/*     Here it is. */

    omegat[0] = 0.;
    omegat[1] = -av[2];
    omegat[2] = av[1];
    omegat[3] = av[2];
    omegat[4] = 0.;
    omegat[5] = -av[0];
    omegat[6] = -av[1];
    omegat[7] = av[0];
    omegat[8] = 0.;
    mxm_(rot, omegat, drotdt);
    for (i__ = 1; i__ <= 3; ++i__) {
	for (j = 1; j <= 3; ++j) {
	    xform[(i__1 = i__ + 3 + j * 6 - 7) < 36 && 0 <= i__1 ? i__1 : 
		    s_rnge("xform", i__1, "rav2xf_", (ftnlen)252)] = drotdt[(
		    i__2 = i__ + j * 3 - 4) < 9 && 0 <= i__2 ? i__2 : s_rnge(
		    "drotdt", i__2, "rav2xf_", (ftnlen)252)];
	}
    }
    return 0;
} /* rav2xf_ */

