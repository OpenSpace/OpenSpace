/* stlabx.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      STLABX ( Stellar aberration, transmission case ) */
/* Subroutine */ int stlabx_(doublereal *pobj, doublereal *vobs, doublereal *
	corpos)
{
    extern /* Subroutine */ int chkin_(char *, ftnlen), stelab_(doublereal *, 
	    doublereal *, doublereal *);
    doublereal negvel[3];
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int vminus_(doublereal *, doublereal *);

/* $ Abstract */

/*     Correct the position of a target for the stellar aberration */
/*     effect on radiation transmitted from a specified observer to */
/*     the target. */

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
/*      CORPOS     O   Corrected position of the object. */

/* $ Detailed_Input */

/*      POBJ        is the cartesian position vector of an object with */
/*                  respect to the observer, possibly corrected for */
/*                  light time.  Units are km. */

/*      VOBS        is the cartesian velocity vector of the observer */
/*                  with respect to the Solar System barycenter.  Units */
/*                  are km/s. */

/* $ Detailed_Output */

/*      CORPOS      is the  position of the object relative to the */
/*                  observer, corrected for the stellar aberration */
/*                  effect on radiation directed toward the target.  This */
/*                  correction is the inverse of the usual stellar */
/*                  aberration correction:  the corrected vector */
/*                  indicates the direction in which radiation must be */
/*                  emitted from the observer, as seen in an inertial */
/*                  reference frame having velocity equal to that of the */
/*                  observer, in order to reach the position indicated by */
/*                  the input vector POBJ. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the velocity of the observer is greater than or equal */
/*        to the speed of light, the error is diagnosed by a routine */
/*        called by this routine.  The outputs are undefined. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     In order to transmit radiation from an observer to a specified */
/*     target, the emission direction must be corrected for one way */
/*     light time and for the motion of the observer relative to the */
/*     solar system barycenter.  The correction for the observer's */
/*     motion when transmitting to a target is the inverse of the */
/*     usual stellar aberration correction applied to the light-time */
/*     corrected position of the target as seen by the observer. */

/*     Below is the description of the stellar aberration correction */
/*     used in the SPICELIB routine STELAB (with the notation changed */
/*     slightly): */

/*        Let r be the vector from the observer to the object, and v be */
/*        the velocity of the observer with respect to the Solar System */
/*        barycenter. Let w be the angle between them. The aberration */
/*        angle phi is given by */

/*           sin(phi) = v sin(w) / c */

/*        Let h be the vector given by the cross product */

/*           h = r X v */

/*        Rotate r by phi radians about h to obtain the apparent position */
/*        of the object. */

/*     This routine applies the inverse correction, so here the rotation */
/*     about h is by -phi radians. */

/* $ Examples */

/*     In the following example, STLABX is used to correct the position */
/*     of a target body for the stellar aberration effect on radiation */
/*     transmitted to the target. */

/*          [Previous subroutine calls have loaded an SPK file and */
/*           the leapseconds kernel file.  The SPK file contains */
/*           sufficient data to enable computation of observer and */
/*           target states relative to the solar system barycenter.] */

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
/*            CALL SPKAPO ( TARG, ET, 'J2000', SOBS, 'XLT', TPOS, LT ) */

/*      C */
/*      C     Apply the correction for stellar aberration to the */
/*      C     light-time corrected position of the target body. */
/*      C     The corrected position is returned in the argument */
/*      C     PCORR. */
/*      C */
/*            CALL STLABX ( TPOS, SOBS(4), PCORR ) */


/*      Note that this example is somewhat contrived. The sequence */
/*      of calls above could be replaced by a single call to SPKEZP, */
/*      using the aberration correction flag 'XLT+S'. */

/*      For more information on aberration-corrected states or */
/*      positions, see the headers of any of the routines */

/*         SPKEZR */
/*         SPKEZ */
/*         SPKPOS */
/*         SPKEZP */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     1) W.M. Owen, Jr., JPL IOM #314.8-524, "The Treatment of */
/*        Aberration in Optical Navigation", 8 February 1985. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     W.L. Taber      (JPL) */
/*     I.M. Underwood  (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 8-JAN-2008 (NJB) */

/*        The header example was updated to remove references */
/*        to SPKAPP. */

/* -    SPICELIB Version 1.0.0, 02-JAN-2002 (IMU) (WLT) (NJB) */

/* -& */
/* $ Index_Entries */

/*     stellar aberration for transmission case */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("STLABX", (ftnlen)6);
    }

/*     Obtain the negative of the observer's velocity.  This */
/*     velocity, combined with the target's position, will yield */
/*     the inverse of the usual stellar aberration correction, */
/*     which is exactly what we seek. */

    vminus_(vobs, negvel);
    stelab_(pobj, negvel, corpos);
    chkout_("STLABX", (ftnlen)6);
    return 0;
} /* stlabx_ */

