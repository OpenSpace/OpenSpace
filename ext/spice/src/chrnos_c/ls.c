/* ls.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__10 = 10;

/* $Procedure    LS  ( Return L_s, planetocentric longitude of the sun ) */
doublereal ls_(integer *body, doublereal *et, char *corr, ftnlen corr_len)
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal ret_val;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    doublereal tipm[9]	/* was [3][3] */;
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *);
    integer i__;
    doublereal x[3], y[3], z__[3];
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal uavel[3], npole[3], state[6], trans[9]	/* was [3][3] */;
    extern /* Subroutine */ int spkez_(integer *, doublereal *, char *, char *
	    , integer *, doublereal *, doublereal *, ftnlen, ftnlen), ucrss_(
	    doublereal *, doublereal *, doublereal *);
    doublereal lt;
    extern /* Subroutine */ int reclat_(doublereal *, doublereal *, 
	    doublereal *, doublereal *), tipbod_(char *, integer *, 
	    doublereal *, doublereal *, ftnlen);
    doublereal radius;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);
    doublereal lat, pos[3];
    extern /* Subroutine */ int mxv_(doublereal *, doublereal *, doublereal *)
	    ;

/* $ Abstract */

/*     Compute L_s, the planetocentric longitude of the sun, as seen */
/*     from a specified body. */

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

/*     GEOMETRY */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     BODY       I   NAIF integer code of central body. */
/*     ET         I   Epoch in ephemeris seconds past J2000. */
/*     CORR       I   Aberration correction. */

/*     The function returns the value of L_s for the specified body */
/*     at the specified time. */

/* $ Detailed_Input */

/*     BODY        is the NAIF integer code of the central body, */
/*                 typically a planet. */

/*     ET          is the epoch in ephemeris seconds past J2000 at which */
/*                 the longitude of the sun (L_s) is to be computed. */

/*     CORR        indicates the aberration corrections to be applied */
/*                 when computing the longitude of the sun.  CORR */
/*                 may be any of the following. */

/*                    'NONE'     Apply no correction. */

/*                    'LT'       Correct the position of the sun, */
/*                               relative to the central body, for */
/*                               planetary (light time) aberration. */

/*                    'LT+S'     Correct the position of the sun, */
/*                               relative to the central body, for */
/*                               planetary and stellar aberrations. */

/* $ Detailed_Output */

/*     The function returns the value of L_s for the specified body */
/*     at the specified time.  This is the longitude of the Sun, */
/*     relative to the central body, in a right-handed frame whose */
/*     basis vectors are defined as follows: */

/*        - The positive Z direction is given by the instantaneous */
/*          angular velocity vector of the orbit of the body about */
/*          the sun. */

/*        - The positive X direction is that of the cross product of the */
/*          instantaneous north spin axis of the body with the positive */
/*          Z direction. */

/*        - The positive Y direction is Z x X. */

/*     Units are radians; the range is -pi to pi.  Longitudes are */
/*     positive east. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If no SPK (ephemeris) file has been loaded prior to calling */
/*        this routine, or if the SPK data has insufficient coverage, an */
/*        error will be diagnosed and signaled by a routine in the call */
/*        tree of this routine. */

/*     2) If a PCK file containing rotational elements for the central */
/*        body has not been loaded prior to calling this routine, an */
/*        error will be diagnosed and signaled by a routine called by a */
/*        routine in the call tree of this routine. */

/*     3) If the instantaneous angular velocity and spin axis of BODY */
/*        are parallel, the return value is unspecified. */

/* $ Files */

/*     1) An SPK file (or file) containing ephemeris data sufficient to */
/*        compute the geometric state of the central body relative to */
/*        the sun at ET must be loaded before this routine is called. If */
/*        light time correction is used, data must be available that */
/*        enable computation of the state the sun relative to the solar */
/*        system barycenter at the light-time corrected epoch.  If */
/*        stellar aberration correction is used, data must be available */
/*        that enable computation of the state the central body relative */
/*        to the solar system barycenter at ET. */

/*     2) A PCK file containing rotational elements for the central body */
/*        must be loaded before this routine is called. */

/* $ Particulars */

/*     The direction of the vernal equinox for the central body is */
/*     determined from the instantaneous equatorial and orbital planes */
/*     of the central body.  This equinox definition is specified in */
/*     reference [1].  The "instantaneous orbital plane" is interpreted */
/*     in this routine as the plane normal to the cross product of the */
/*     position and velocity of the central body relative to the sun. */
/*     A geometric state is used for this normal vector computation. */
/*     The "instantaneous equatorial plane" is that normal to the */
/*     central body's north pole at the requested epoch.  The pole */
/*     direction is determined from rotational elements loaded via */
/*     a PCK file. */

/*     The result returned by this routine will depend on the */
/*     ephemeris data and rotational elements used.  The result may */
/*     differ from that given in any particular version of the */
/*     Astronomical Almanac, due to differences in these input data, */
/*     and due to differences in precision of the computations. */

/* $ Examples */

/*     1) A simple program that computes L_s for Mars.  The geometric */
/*        state of the sun is used. */


/*            PROGRAM MARS_LS */
/*            IMPLICIT NONE */

/*            DOUBLE PRECISION      DPR */

/*            INTEGER               FILSIZ */
/*            PARAMETER           ( FILSIZ = 255 ) */

/*            CHARACTER*(FILSIZ)    PCK */
/*            CHARACTER*(FILSIZ)    SPK */
/*            CHARACTER*(FILSIZ)    LEAP */
/*            CHARACTER*(30)        UTC */
/*            CHARACTER*(15)        CORR */

/*            DOUBLE PRECISION      ET */
/*            DOUBLE PRECISION      LONG */
/*            DOUBLE PRECISION      LS */

/*            INTEGER               BODY */
/*            INTEGER               HANDLE */

/*            DATA  BODY   /  499      / */
/*            DATA  CORR   /  'NONE'   / */


/*            CALL PROMPT ( 'Enter name of leapseconds kernel > ', LEAP ) */
/*            CALL PROMPT ( 'Enter name of PCK file           > ', PCK  ) */
/*            CALL PROMPT ( 'Enter name of SPK file           > ', SPK  ) */

/*            CALL FURNSH ( LEAP ) */
/*            CALL FURNSH ( PCK  ) */
/*            CALL FURNSH ( SPK  ) */

/*            WRITE (*,*) ' ' */
/*            WRITE (*,*) 'Kernels have been loaded.' */
/*            WRITE (*,*) ' ' */

/*            DO WHILE ( .TRUE. ) */

/*               CALL PROMPT ( 'Enter UTC time > ', UTC ) */

/*               CALL UTC2ET ( UTC, ET ) */

/*      C */
/*      C        Convert longitude to degrees and move it into the range */
/*      C        [0, 360). */
/*      C */
/*               LONG = DPR() * LS ( BODY, ET, CORR ) */

/*               IF ( LONG .LT. 0.D0 ) THEN */
/*                  LONG = LONG + 360.D0 */
/*               END IF */

/*               WRITE (*,*) ' ' */
/*               WRITE (*,*) 'Mars L_s (deg.) = ',  LONG */
/*               WRITE (*,*) ' ' */

/*            END DO */

/*            END */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     [1] "The Astronomical Almanac for the Year 2005." U.S. Government */
/*         Printing Office, Washington, D.C., 1984, page L9. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */

/* $ Version */

/* -    Chronos Version 1.1.2, 02-OCT-2006 (BVS) */

/*        Replaced LDPOOL and SPKELF with FURNSH in the Examples */
/*        section. */

/* -    Chronos Version 1.1.1, 07-JAN-2005 (NJB) */

/*        Description of reference frame in Detailed_Output header */
/*        section was corrected.  Miscellaneous other header updates */
/*        were made. */

/* -    Beta Version 1.1.0, 14-DEC-1996 (NJB) */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	ret_val = 0.;
	return ret_val;
    } else {
	chkin_("LS", (ftnlen)2);
    }

/*     Look up the direction of the North pole of the central body. */

    tipbod_("J2000", body, et, tipm, (ftnlen)5);
    for (i__ = 1; i__ <= 3; ++i__) {
	npole[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("npole", i__1,
		 "ls_", (ftnlen)302)] = tipm[(i__2 = i__ * 3 - 1) < 9 && 0 <= 
		i__2 ? i__2 : s_rnge("tipm", i__2, "ls_", (ftnlen)302)];
    }

/*     Get the geometric state of the body relative to the sun. */

    spkez_(body, et, "J2000", "NONE", &c__10, state, &lt, (ftnlen)5, (ftnlen)
	    4);

/*     Get the unit direction vector parallel to the angular velocity */
/*     vector of the orbit.  This is just the unitized cross product of */
/*     position and velocity. */

    ucrss_(state, &state[3], uavel);

/*     We want to form a transformation matrix that maps vectors from */
/*     basis REF to the following frame: */

/*        Z  =  UAVEL */

/*        X  =  NPOLE x UAVEL */

/*        Y  =  Z x X */

/*     We'll find the position of the Sun relative to this frame.  In */
/*     our computations, we want our basis vectors to have unit length. */

    vequ_(uavel, z__);
    ucrss_(npole, z__, x);
    ucrss_(z__, x, y);
    for (i__ = 1; i__ <= 3; ++i__) {
	trans[(i__1 = i__ * 3 - 3) < 9 && 0 <= i__1 ? i__1 : s_rnge("trans", 
		i__1, "ls_", (ftnlen)335)] = x[(i__2 = i__ - 1) < 3 && 0 <= 
		i__2 ? i__2 : s_rnge("x", i__2, "ls_", (ftnlen)335)];
	trans[(i__1 = i__ * 3 - 2) < 9 && 0 <= i__1 ? i__1 : s_rnge("trans", 
		i__1, "ls_", (ftnlen)336)] = y[(i__2 = i__ - 1) < 3 && 0 <= 
		i__2 ? i__2 : s_rnge("y", i__2, "ls_", (ftnlen)336)];
	trans[(i__1 = i__ * 3 - 1) < 9 && 0 <= i__1 ? i__1 : s_rnge("trans", 
		i__1, "ls_", (ftnlen)337)] = z__[(i__2 = i__ - 1) < 3 && 0 <= 
		i__2 ? i__2 : s_rnge("z", i__2, "ls_", (ftnlen)337)];
    }

/*     Get the state of the sun in frame REF.  Since we may be using */
/*     aberration corrections, this is not necessarily the negative of */
/*     the state we've just found. */

    spkez_(&c__10, et, "J2000", corr, body, state, &lt, (ftnlen)5, corr_len);

/*     Now transform the position of the Sun into the "equator and */
/*     equinox" frame. */

    mxv_(trans, state, pos);

/*     Let RECLAT find the longitude LS for us. */

    reclat_(pos, &radius, &ret_val, &lat);
    chkout_("LS", (ftnlen)2);
    return ret_val;
} /* ls_ */

