/* dnearp.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b16 = 1.;

/* $Procedure      DNEARP ( Derivative of near point ) */
/* Subroutine */ int dnearp_(doublereal *state, doublereal *a, doublereal *b, 
	doublereal *c__, doublereal *dnear, doublereal *dalt, logical *found)
{
    /* Initialized data */

    static doublereal gradm[9]	/* was [3][3] */ = { 1.,0.,0.,0.,1.,0.,0.,0.,
	    1. };
    static doublereal m[9]	/* was [3][3] */ = { 1.,0.,0.,0.,1.,0.,0.,0.,
	    1. };

    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    doublereal grad[3], temp[3];
    extern doublereal vdot_(doublereal *, doublereal *);
    extern /* Subroutine */ int vsub_(doublereal *, doublereal *, doublereal *
	    );
    extern doublereal vtmv_(doublereal *, doublereal *, doublereal *);
    integer i__;
    doublereal l;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal denom, dterm[3];
    extern /* Subroutine */ int vlcom_(doublereal *, doublereal *, doublereal 
	    *, doublereal *, doublereal *);
    doublereal norml[3];
    extern /* Subroutine */ int unorm_(doublereal *, doublereal *, doublereal 
	    *);
    extern logical failed_(void);
    doublereal length, lprime;
    extern /* Subroutine */ int nearpt_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *), chkout_(
	    char *, ftnlen);
    doublereal zenith[3];
    extern logical return_(void);
    extern /* Subroutine */ int mxv_(doublereal *, doublereal *, doublereal *)
	    ;

/* $ Abstract */

/*     Compute the ellipsoid surface point nearest to a specified */
/*     position; also compute the velocity of this point. */

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

/*     ELLIPSOID, GEOMETRY, DERIVATIVE */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STATE      I   State of an object in body-fixed coordinates. */
/*     A          I   Length of semi-axis parallel to x-axis. */
/*     B          I   Length of semi-axis parallel to y-axis. */
/*     C          I   Length on semi-axis parallel to z-axis. */
/*     DNEAR      O   State of the nearest point on the ellipsoid. */
/*     DALT       O   Altitude and derivative of altitude. */
/*     FOUND      O   Tells whether DNEAR is degenerate. */

/* $ Detailed_Input */

/*     STATE      is a 6-vector giving the position and velocity of */
/*                some object in the body-fixed coordinates of the */
/*                ellipsoid. */

/*                In body-fixed coordinates, the semi-axes of the */
/*                ellipsoid are aligned with the x, y, and z-axes of the */
/*                coordinate system. */

/*     A          is the length of the semi-axis of the ellipsoid */
/*                that is parallel to the x-axis of the body-fixed */
/*                coordinate system. */

/*     B          is the length of the semi-axis of the ellipsoid */
/*                that is parallel to the y-axis of the body-fixed */
/*                coordinate system. */

/*     C          is the length of the semi-axis of the ellipsoid */
/*                that is parallel to the z-axis of the body-fixed */
/*                coordinate system. */

/* $ Detailed_Output */


/*     DNEAR      is the 6-vector giving the position and velocity */
/*                in body-fixed coordinates of the point on the */
/*                ellipsoid, closest to the object whose position */
/*                and velocity are represented by STATE. */

/*                While the position component of DNEAR is always */
/*                meaningful, the velocity component of DNEAR will be */
/*                meaningless if FOUND if .FALSE.  (See the discussion */
/*                of the meaning of FOUND below.) */


/*     DALT       is an array of two double precision numbers.  The */
/*                first gives the altitude of STATE with respect to */
/*                the ellipsoid.  The second gives the rate of */
/*                change of the altitude. */

/*                Note that the rate of change of altitude is meaningful */
/*                if and only if FOUND is .TRUE.  (See the discussion of */
/*                the meaning of FOUND below.) */

/*     FOUND      is a logical flag indicating whether or not the */
/*                velocity portion of DNEAR is meaningful. */
/*                If the velocity portion of DNEAR is meaningful */
/*                FOUND will be returned with a value of .TRUE. */
/*                Under very rare circumstance the velocity of the */
/*                near point is undefined.  Under these circumstances */
/*                FOUND will be returned with the value .FALSE. */

/*                FOUND can be .FALSE. only for states whose position */
/*                components are inside the ellipsoid and then only at */
/*                points on a special surface contained inside the */
/*                ellipsoid called the focal set of the ellipsoid. */

/*                A point in the interior is on this special surface */
/*                only if there are two or more points on the ellipsoid */
/*                that are closest to it.  The origin is such a point */
/*                and the only such point if the ellipsoid is a */
/*                sphere.  For non-spheroidal ellipsoids the focal */
/*                set contains small portions of the planes of */
/*                symmetry of the ellipsoid. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */


/*     1) If the axes are non-positive, a routine in the call tree */
/*        of this routine will diagnose the error. */

/*     2) If an object is passing through the interior of an ellipsoid */
/*        there are points at which there is more than 1 point on */
/*        the ellipsoid that is closest to the object.  At these */
/*        points the velocity of the near point is undefined. (See */
/*        the description of the output variable FOUND). */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     If an object is moving relative to some triaxial body along */
/*     a trajectory C(t) then there is a companion trajectory N(t) */
/*     that gives the point on the ellipsoid that is closest to */
/*     C(t) as a function of t.  The instantaneous position and */
/*     velocity of C(t) (STATE) are sufficient to compute the */
/*     instantaneous position and velocity of N(t) (DNEAR). */

/*     This routine computes DNEAR from STATE.  In addition it returns */
/*     the altitude and rate of change of altitude. */

/*     Note that this routine can compute DNEAR for STATES outside, */
/*     on, or inside the ellipsoid.  However, the velocity of DNEAR */
/*     and derivative of altitude do not exist for a "small" set */
/*     of STATES  in the interior of the ellipsoid. See the */
/*     discussion of FOUND above for a description of this set of */
/*     points. */

/* $ Examples */

/*     Example 1.  Speed of a ground track. */
/*     ======================================= */

/*     Suppose you wish to compute the velocity of the ground track */
/*     of a satellite as it passes over a location on the earth */
/*     and that the moment of passage (ET) has been previously */
/*     determined.  (We assume that the spacecraft is close enough */
/*     to the surface that light time corrections do not matter.) */

/*     We let */

/*        BODY    be the idcode for the body */
/*        FRAME   be the string representing the body's body-fixed frame */
/*        SCID    be the idcode of the spacecraft */

/*     First get the axes of the body. */

/*        CALL BODVCD ( BODY, 'RADII', 3, DIM, ABC  ) */

/*        A = ABC(1) */
/*        B = ABC(2) */
/*        C = ABC(3) */

/*        CALL SPKEZ  ( SCID,  ET,   FRAME,   'NONE', BODY, STATE, LT ) */
/*        CALL DNEARP ( STATE, A, B, C, DNEAR, DALT ) */

/*     DNEAR contains the state of the subspacecraft point. */


/*     Example 2. Doppler shift of an altimeter. */
/*     ========================================= */

/*     Suppose you wish to compute the one-way doppler shift of a radar */
/*     altimeter mounted on board a spacecraft as it passes */
/*     over some region.  Moreover, assume that for your */
/*     purposes it is sufficient to neglect effects of atmosphere, */
/*     topography and antenna pattern for the sake of this */
/*     computation.  We use the same notation as in the previous example. */

/*     First get the axes of the body. */

/*        CALL BODVCD ( BODY, 'RADII', 3, DIM, ABC  ) */

/*        A = ABC(1) */
/*        B = ABC(2) */
/*        C = ABC(3) */

/*        CALL SPKEZ  ( SCID,  ET,   FRAME,   'NONE', BODY, STATE, LT ) */
/*        CALL DNEARP ( STATE, A, B, C, DNEAR, DALT ) */


/*     The change in frequency is given by multiplying SHIFT times the */
/*     carrier frequency */

/*        SHIFT = ( DALT(2) / CLIGHT() ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.2, 26-JUN-2008 (NJB) */

/*        Corrected spelling error in abstract; re-wrote */
/*        abstract text. */

/* -    SPICELIB Version 1.1.1, 24-OCT-2005 (NJB) */

/*        Header update:  changed references to BODVAR to references */
/*        to BODVCD. */

/* -    SPICELIB Version 1.1.0, 05-MAR-1998 (WLT) */

/*        In the previous version of the routine FOUND could be */
/*        returned without being set to TRUE when the velocity */
/*        of the near point and rate of change of altitude */
/*        could be determined.  This error has been corrected. */

/* -    SPICELIB Version 1.0.0, 15-JUN-1995 (WLT) */


/* -& */
/* $ Index_Entries */

/*     Velocity of the nearest point on an ellipsoid */
/*     Rate of change of the altitude over an ellipsoid */
/*     Derivative of altitude over an ellipoid */
/*     Velocity of a ground track */

/* -& */

/*     Spicelib functions */


/*     Local Variables */


/*     Saved Variables */


/*     Initial Values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("DNEARP", (ftnlen)6);

/*     Until we have reason to believe otherwise, we set FOUND to TRUE. */

    *found = TRUE_;

/*     First we need to compute the near point. */

    nearpt_(state, a, b, c__, dnear, dalt);

/*     Make sure nothing went bump in the dark innards of NEARPT. */

    if (failed_()) {
	*found = FALSE_;
	chkout_("DNEARP", (ftnlen)6);
	return 0;
    }

/*     Now for the work of this routine.  We need to compute the */
/*     velocity component of DNEAR. */

/*     In all of the discussions below we let <,> stand for the */
/*     dot product. */

/*     Let P be the position (first three components) of STATE */
/*     and let N be the position (first three components) of DNEAR. */

/*     The surface of the ellipsoid is described as the level set */
/*     f(x,y,z) = 1 for the function f defined by */

/*         f(x,y,z) = x**2/a**2 + y**2/b**2 + z**2/c**2 */

/*     Let GRAD be the "half" gradiant of f. Then for some L */


/*           N + L * GRAD = P                         ( 1 ) */


/*     So that */
/*                < P - N, GRAD > */
/*           L =  -------------- */
/*                < GRAD , GRAD > */

/*                          GRAD */
/*             =  < P - N, ------ >  /  | GRAD | */
/*                         |GRAD| */

/*     Since GRAD is computed at a point on the level set f(x,y,z) = 1 */
/*     we don't have to worry about the magnitude of |GRAD| being */
/*     so small that underflow can occur. */

/*     Note that the half gradiant of f  can be computed by simple */
/*     vector multiplication */

/*                       [ 1/A**2    0       0    ] [ x ] */
/*        GRAD(x,y,z)  = |   0     1/B**2    0    | | y | */
/*                       [   0       0     1/C**2 ] [ z ] */

/*     We call the matrix above GRADM.  The correct off */
/*     diagonal values have been established in the data statement */
/*     following the declaration section of this routine. */

    gradm[0] = 1. / (*a * *a);
    gradm[4] = 1. / (*b * *b);
    gradm[8] = 1. / (*c__ * *c__);
    vsub_(state, dnear, zenith);
    mxv_(gradm, dnear, grad);
    unorm_(grad, norml, &length);
    l = vdot_(zenith, norml) / length;

/*     We can rewrite equation (1) as */

/*        P = N + L * GRADM * N */

/*     from this it follows that */

/*        P' =  N' + L' * GRADM * N */
/*                 + L  * GRADM * N' */

/*           = ( IDENT + L*GRADM ) * N'   + L' * GRADM * N */

/*           = ( IDENT + L*GRADM ) * N'   + L' * GRAD */

/*     where IDENT is the 3x3 identity matrix. */

/*     Let M be the inverse of the matrix IDENT + L*GRADM. (Provided */
/*     of course that all of the diagonal entries are non-zero). */

/*     If we multiply both sides of the equation above by M */
/*     we have */


/*        M*P'  = N'  + L'* M * GRAD                      ( 2 ) */


/*     Recall now that N' is orthogonal to GRAD (N' lies in the */
/*     tangent plane to the ellipsoid at N and GRAD is normal */
/*     to this tangent plane).  Thus */

/*        < GRAD, M*P' > = L' < GRAD, M * GRAD > */

/*     and */

/*                 < GRAD, M*P'   > */
/*        L'   =   ----------------- */
/*                 < GRAD, M*GRAD > */


/*             =   VTMV ( GRAD, M, P' ) / VTMV ( GRAD, M, GRAD ) */

/*     Let's pause now to compute M and L'. */

/*        This is where things could go bad.  M might not exist (which */
/*        indicates STATE is on the focal set of the ellipsoid).  In */
/*        addition it is conceivable that VTMV ( GRAD, M, GRAD ) is */
/*        zero.  This turns out not to be possible.  However, the */
/*        demonstration of this fact requires delving into the details */
/*        of how N was computed by NEARPT.  Rather than spending a */
/*        lot of time explaining the details we will make an */
/*        unnecessary but inexpensive check that we don't divide by */
/*        zero when computing L'. */

    for (i__ = 1; i__ <= 3; ++i__) {
	dterm[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("dterm", i__1,
		 "dnearp_", (ftnlen)458)] = l * gradm[(i__2 = i__ + i__ * 3 - 
		4) < 9 && 0 <= i__2 ? i__2 : s_rnge("gradm", i__2, "dnearp_", 
		(ftnlen)458)] + 1.;
    }
    for (i__ = 1; i__ <= 3; ++i__) {
	if (dterm[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("dterm", 
		i__1, "dnearp_", (ftnlen)463)] != 0.) {
	    m[(i__1 = i__ + i__ * 3 - 4) < 9 && 0 <= i__1 ? i__1 : s_rnge(
		    "m", i__1, "dnearp_", (ftnlen)464)] = 1. / dterm[(i__2 = 
		    i__ - 1) < 3 && 0 <= i__2 ? i__2 : s_rnge("dterm", i__2, 
		    "dnearp_", (ftnlen)464)];
	} else {
	    *found = FALSE_;
	    chkout_("DNEARP", (ftnlen)6);
	    return 0;
	}
    }
    denom = vtmv_(grad, m, grad);
    if (denom == 0.) {
	*found = FALSE_;
	chkout_("DNEARP", (ftnlen)6);
	return 0;
    }
    lprime = vtmv_(grad, m, &state[3]) / denom;

/*     Now that we have L' we can easily compute N'. Rewriting */
/*     equation (2) from above we have. */

/*        N'  = M * ( P' - L'*GRAD ) */

    d__1 = -lprime;
    vlcom_(&c_b16, &state[3], &d__1, grad, temp);
    mxv_(m, temp, &dnear[3]);

/*     Only one thing left to do.  Compute the derivative */
/*     of the altitude ALT.  Recall that */

/*                              GRAD */
/*        ALT     = < P  -  N, ------ > */
/*                             |GRAD| */

/*                             GRAD */
/*        dALT/dt = < P' - N', ------ > */
/*                             |GRAD| */

/*                                        GRAD */
/*                 + < P  -  N, Deriv of{------} > */
/*                                       |GRAD| */

/*     The second term is zero.  To see this note that P - N is parallel */
/*     to GRAD.  Moreover, since GRAD/|GRAD| is a unit vector its */
/*     derivative is necessarily orthogonal to it.  Hence it is */
/*     orthogonal to GRAD and P-N. */

/*     Thus */
/*                              GRAD */
/*        dALT/dt = < P' - N', ------ > */
/*                             |GRAD| */

/*     But as we discussed earlier N' is orthogonal to GRAD.  Thus */

/*                          GRAD */
/*        dALT/dt = < P' , ------ > */
/*                         |GRAD| */

/*     We've already computed GRAD/|GRAD| (NORML). Hence */

/*        dALT/dt = < P', NORML > */

    dalt[1] = vdot_(&state[3], norml);
    chkout_("DNEARP", (ftnlen)6);
    return 0;
} /* dnearp_ */

