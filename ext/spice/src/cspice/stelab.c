/* stelab.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;

/* $Procedure      STELAB     ( Stellar Aberration ) */
/* Subroutine */ int stelab_(doublereal *pobj, doublereal *vobs, doublereal *
	appobj)
{
    /* Builtin functions */
    double asin(doublereal);

    /* Local variables */
    extern /* Subroutine */ int vhat_(doublereal *, doublereal *);
    doublereal vbyc[3];
    extern /* Subroutine */ int vscl_(doublereal *, doublereal *, doublereal *
	    );
    extern doublereal vdot_(doublereal *, doublereal *);
    doublereal h__[3], u[3];
    extern /* Subroutine */ int chkin_(char *, ftnlen), moved_(doublereal *, 
	    integer *, doublereal *), errdp_(char *, doublereal *, ftnlen), 
	    vcrss_(doublereal *, doublereal *, doublereal *);
    extern doublereal vnorm_(doublereal *);
    extern /* Subroutine */ int vrotv_(doublereal *, doublereal *, doublereal 
	    *, doublereal *);
    extern doublereal clight_(void);
    doublereal onebyc, sinphi;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    doublereal lensqr;
    extern logical return_(void);
    doublereal phi;

/* $ Abstract */

/*      Correct the apparent position of an object for stellar */
/*      aberration. */

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

/*      EPHEMERIS */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      POBJ       I   Position of an object with respect to the */
/*                     observer. */
/*      VOBS       I   Velocity of the observer with respect to the */
/*                     Solar System barycenter. */
/*      APPOBJ     O   Apparent position of the object with respect to */
/*                     the observer, corrected for stellar aberration. */

/* $ Detailed_Input */

/*      POBJ        is the position (x, y, z, km) of an object with */
/*                  respect to the observer, possibly corrected for */
/*                  light time. */

/*      VOBS        is the velocity (dx/dt, dy/dt, dz/dt, km/sec) */
/*                  of the observer with respect to the Solar System */
/*                  barycenter. */

/* $ Detailed_Output */

/*      APPOBJ      is the apparent position of the object relative */
/*                  to the observer, corrected for stellar aberration. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the velocity of the observer is greater than or equal */
/*        to the speed of light, the error SPICE(VALUEOUTOFRANGE) */
/*        is signaled. */

/* $ Files */

/*      None. */

/* $ Particulars */

/*      Let r be the vector from the observer to the object, and v be */
/*          -                                                    - */
/*      the velocity of the observer with respect to the Solar System */
/*      barycenter. Let w be the angle between them. The aberration */
/*      angle phi is given by */

/*           sin(phi) = v sin(w) / c */

/*      Let h be the vector given by the cross product */
/*          - */

/*            h = r X v */
/*            -   -   - */

/*      Rotate r by phi radians about h to obtain the apparent position */
/*             -                      - */
/*      of the object. */

/* $ Examples */

/*      In the following example, STELAB is used to correct the position */
/*      of a target body for stellar aberration. */


/*          (Previous subroutine calls have loaded the SPK file and */
/*           the leapseconds kernel file.) */


/*      C */
/*      C     Get the geometric state of the observer OBS relative to */
/*      C     the solar system barycenter. */
/*      C */
/*            CALL SPKSSB ( OBS, ET, 'J2000', SOBS ) */

/*      C */
/*      C     Get the light-time corrected position TPOS of the target */
/*      C     body TARG as seen by the observer. Normally we would */
/*      C     call SPKPOS to obtain this vector, but we already have */
/*      C     the state of the observer relative to the solar system */
/*      C     barycenter, so we can avoid looking up that state twice */
/*      C     by calling SPKAPO. */
/*      C */
/*            CALL SPKAPO ( TARG, ET, 'J2000', SOBS, 'LT', TPOS, LT ) */

/*      C */
/*      C     Apply the correction for stellar aberration to the */
/*      C     light-time corrected position of the target body. */
/*      C     The corrected position is returned in the argument */
/*      C     PCORR. */
/*      C */
/*            CALL STELAB ( TPOS, SOBS(4), PCORR ) */


/*      Note that this example is somewhat contrived. The sequence */
/*      of calls above could be replaced by a single call to SPKEZP, */
/*      using the aberration correction flag 'LT+S'. */

/*      For more information on aberration-corrected states or */
/*      positions, see the headers of any of the routines */

/*         SPKEZR */
/*         SPKEZ */
/*         SPKPOS */
/*         SPKEZP */

/* $ Restrictions */

/*      None. */

/* $ Literature_References */

/*      1) W.M. Owen, Jr., JPL IOM #314.8-524, "The Treatment of */
/*         Aberration in Optical Navigation", 8 February 1985. */

/* $ Author_and_Institution */

/*      N.J. Bachman    (JPL) */
/*      H.A. Neilan     (JPL) */
/*      W.L. Taber      (JPL) */
/*      I.M. Underwood  (JPL) */

/* $ Version */

/* -     SPICELIB Version 1.1.1, 8-JAN-2008 (NJB) */

/*         The header example was updated to remove references */
/*         to SPKAPP. */

/* -     SPICELIB Version 1.1.0, 8-FEB-1999 (WLT) */

/*         The example was corrected so that SOBS(4) is passed */
/*         into STELAB instead of STARG(4). */

/* -     SPICELIB Version 1.0.2, 10-MAR-1992 (WLT) */

/*         Comment section for permuted index source lines was added */
/*         following the header. */

/* -     SPICELIB Version 1.0.1, 8-AUG-1990 (HAN) */

/*         Examples section of the header was updated to replace */
/*         calls to the GEF ephemeris readers by calls to the */
/*         new SPK ephemeris reader. */

/* -     SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (WLT) */

/* -& */
/* $ Index_Entries */

/*     stellar aberration */

/* -& */
/* $ Revisions */

/* -     Beta Version 2.1.0, 9-MAR-1989 (HAN) */

/*         Declaration of the variable LIGHT was removed from the code. */
/*         The variable was declared but never used. */

/* -     Beta Version 2.0.0, 28-DEC-1988 (HAN) */

/*         Error handling was added to check the velocity of the */
/*         observer. If the velocity of the observer is greater */
/*         than or equal to the speed of light, the error */
/*         SPICE(VALUEOUTOFRANGE) is signalled. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("STELAB", (ftnlen)6);
    }

/*     We are not going to compute the aberrated vector in exactly the */
/*     way described in the particulars section.  We can combine some */
/*     steps and we take some precautions to prevent floating point */
/*     overflows. */


/*     Get a unit vector that points in the direction of the object */
/*     ( u_obj ). */

    vhat_(pobj, u);

/*     Get the velocity vector scaled with respect to the speed of light */
/*     ( v/c ). */

    onebyc = 1. / clight_();
    vscl_(&onebyc, vobs, vbyc);

/*     If the square of the length of the velocity vector is greater than */
/*     or equal to one, the speed of the observer is greater than or */
/*     equal to the speed of light. The observer speed is definitely out */
/*     of range. Signal an error and check out. */

    lensqr = vdot_(vbyc, vbyc);
    if (lensqr >= 1.) {
	setmsg_("Velocity components of observer were:  dx/dt = *, dy/dt = *"
		", dz/dt = *.", (ftnlen)71);
	errdp_("*", vobs, (ftnlen)1);
	errdp_("*", &vobs[1], (ftnlen)1);
	errdp_("*", &vobs[2], (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("STELAB", (ftnlen)6);
	return 0;
    }

/*     Compute u_obj x (v/c) */

    vcrss_(u, vbyc, h__);

/*     If the magnitude of the vector H is zero, the observer is moving */
/*     along the line of sight to the object, and no correction is */
/*     required. Otherwise, rotate the position of the object by phi */
/*     radians about H to obtain the apparent position. */

    sinphi = vnorm_(h__);
    if (sinphi != 0.) {
	phi = asin(sinphi);
	vrotv_(pobj, h__, &phi, appobj);
    } else {
	moved_(pobj, &c__3, appobj);
    }
    chkout_("STELAB", (ftnlen)6);
    return 0;
} /* stelab_ */

