/* ducrss.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__6 = 6;

/* $Procedure      DUCRSS ( Unit Normalized Cross Product and Derivative ) */
/* Subroutine */ int ducrss_(doublereal *s1, doublereal *s2, doublereal *sout)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Local variables */
    doublereal scls1[6], scls2[6];
    extern /* Subroutine */ int dvhat_(doublereal *, doublereal *), moved_(
	    doublereal *, integer *, doublereal *), vsclg_(doublereal *, 
	    doublereal *, integer *, doublereal *);
    doublereal f1, f2;
    extern /* Subroutine */ int dvcrss_(doublereal *, doublereal *, 
	    doublereal *);
    doublereal tmpsta[6];

/* $ Abstract */

/*     Compute the unit vector parallel to the cross product of */
/*     two 3-dimensional vectors and the derivative of this unit vector. */

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
/*     DERIVATIVE */

/* $ Declarations */
/* $ Brief_I/O */


/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     S1        I   Left hand state for cross product and derivative. */
/*     S2        I   Right hand state for cross product and derivative. */
/*     SOUT      O   Unit vector and derivative of the cross product. */

/* $ Detailed_Input */

/*     S1       This may be any state vector.  Typically, this */
/*              might represent the apparent state of a planet or the */
/*              Sun, which defines the orientation of axes of */
/*              some coordinate system. */

/*     S2       Any state vector. */

/* $ Detailed_Output */

/*     SOUT     This variable represents the unit vector parallel to the */
/*              cross product of the position components of S1 and S2 */
/*              and the derivative of the unit vector. */

/*              If the cross product of the position components is */
/*              the zero vector, then the position component of the */
/*              output will be the zero vector.  The velocity component */
/*              of the output will simply be the derivative of the */
/*              cross product of the position components of S1 and S2. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1) If the position components of S1 and S2 cross together to */
/*        give a zero vector, the position component of the output */
/*        will be the zero vector.  The velocity component of the */
/*        output will simply be the derivative of the cross product */
/*        of the position vectors. */

/*     2) If S1 and S2 are large in magnitude (taken together, */
/*        their magnitude surpasses the limit allowed by the */
/*        computer) then it may be possible to generate a */
/*        floating point overflow from an intermediate */
/*        computation even though the actual cross product and */
/*        derivative may be well within the range of double */
/*        precision numbers. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     DUCRSS calculates the unit vector parallel to the cross product */
/*     of two vectors and the derivative of that unit vector. */

/* $ Examples */

/*     One often constructs non-inertial coordinate frames from */
/*     apparent positions of objects.  However, if one wants to convert */
/*     states in this non-inertial frame to states in an inertial */
/*     reference frame, the derivatives of the axes of the non-inertial */
/*     frame are required.  For example consider an Earth meridian */
/*     frame defined as follows. */

/*        The z-axis of the frame is defined to be the vector */
/*        normal to the plane spanned by the position vectors to the */
/*        apparent Sun and to the apparent body as seen from an observer. */

/*        Let SUN be the apparent state of the Sun and let BODY be the */
/*        apparent state of the body with respect to the observer.  Then */
/*        the unit vector parallel to the z-axis of the Earth meridian */
/*        system and its derivative are given by the call: */

/*        CALL DUCRSS ( SUN, BODY, ZZDOT ) */

/* $ Restrictions */

/*     No checking of S1 or S2 is done to prevent floating point */
/*     overflow. The user is required to determine that the magnitude */
/*     of each component of the states is within an appropriate range */
/*     so as not to cause floating point overflow. In almost every case */
/*     there will be no problem and no checking actually needs to be */
/*     done. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 08-APR-2014 (NJB) */

/*        Now scales inputs to reduce chance of numeric */
/*        overflow. */

/* -    SPICELIB Version 1.1.1, 22-APR-2010 (NJB) */

/*        Header correction: assertions that the output */
/*        can overwrite the input have been removed. */

/* -    SPICELIB Version 1.1.0, 30-AUG-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in DVHAT call. */

/* -    SPICELIB Version 1.0.0, 15-JUN-1995 (WLT) */


/* -& */
/* $ Index_Entries */

/*     Compute a unit cross product and its derivative */

/* -& */

/*     Local variables */


/*     Scale the components of the input states so the states have the */
/*     same direction and angular rates, but their largest position */
/*     components have absolute value equal to 1. Do not modify states */
/*     that have all position components equal to zero. */

/* Computing MAX */
    d__1 = abs(s1[0]), d__2 = abs(s1[1]), d__1 = max(d__1,d__2), d__2 = abs(
	    s1[2]);
    f1 = max(d__1,d__2);
/* Computing MAX */
    d__1 = abs(s2[0]), d__2 = abs(s2[1]), d__1 = max(d__1,d__2), d__2 = abs(
	    s2[2]);
    f2 = max(d__1,d__2);
    if (f1 > 0.) {
	d__1 = 1. / f1;
	vsclg_(&d__1, s1, &c__6, scls1);
    } else {
	moved_(s1, &c__6, scls1);
    }
    if (f2 > 0.) {
	d__1 = 1. / f2;
	vsclg_(&d__1, s2, &c__6, scls2);
    } else {
	moved_(s2, &c__6, scls2);
    }

/*     Not much to this.  Just get the cross product and its derivative. */
/*     Using that, get the associated unit vector and its derivative. */

    dvcrss_(scls1, scls2, tmpsta);
    dvhat_(tmpsta, sout);
    return 0;
} /* ducrss_ */

