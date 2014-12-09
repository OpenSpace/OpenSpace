/* reclat.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      RECLAT ( Rectangular to latitudinal coordinates ) */
/* Subroutine */ int reclat_(doublereal *rectan, doublereal *radius, 
	doublereal *long__, doublereal *lat)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal), atan2(doublereal, doublereal);

    /* Local variables */
    doublereal x, y, z__, big;

/* $ Abstract */

/*     Convert from rectangular coordinates to latitudinal coordinates. */

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

/*     CONVERSION,  COORDINATES */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     RECTAN     I   Rectangular coordinates of the point. */
/*     RADIUS     O   Distance of a point from the origin. */
/*     LONG       O   Longitude of point in radians. */
/*     LAT        O   Latitude of point in radians. */

/* $ Detailed_Input */

/*     RECTAN     The rectangular coordinates of a point. */

/* $ Detailed_Output */

/*     RADIUS     Distance of a point from the origin. */

/*                The units associated with RADIUS are those */
/*                associated with the input RECTAN. */

/*     LONG       Longitude of the input point.  This is the angle */
/*                between the prime meridian and the meridian */
/*                containing the point.  The direction of increasing */
/*                longitude is from the +X axis towards the +Y axis. */

/*                LONG is output in radians.  The range of LONG is */
/*                [ -pi, pi]. */


/*     LAT        Latitude of the input point.  This is the angle from */
/*                the XY plane of the ray from the origin through the */
/*                point. */

/*                LAT is output in radians.  The range of LAT is */
/*                [-pi/2, pi/2]. */

/* $ Parameters */

/*     None. */

/* $ Particulars */

/*     This routine returns the latitudinal coordinates of a point */
/*     whose position is input in rectangular coordinates. */

/*     Latitudinal coordinates are defined by a distance from a central */
/*     reference point, an angle from a reference meridian, and an angle */
/*     above the equator of a sphere centered at the central reference */
/*     point. */

/* $ Exceptions */

/*     Error free. */

/*     1) If the X and Y components of RECTAN are both zero, the */
/*        longitude is set to zero. */

/*     2) If RECTAN is the zero vector, longitude and latitude are */
/*        both set to zero. */

/* $ Files */

/*     None. */

/* $ Examples */

/*     Below are two tables. */

/*     Listed in the first table (under X(1), X(2) and X(3) ) are a */
/*     number of points whose rectangular coordinates are */
/*     taken from the set {-1, 0, 1}. */

/*     The results of the code fragment */

/*              CALL RECLAT ( X, R, LONG, LAT ) */
/*        C */
/*        C     Use the SPICELIB routine CONVRT to convert the angular */
/*        C     quantities to degrees */
/*        C */
/*              CALL CONVRT ( LAT,  'RADIANS', 'DEGREES', LAT  ) */
/*              CALL CONVRT ( LONG, 'RADIANS', 'DEGREES', LONG ) */

/*     are listed to 4 decimal places in the second parallel table under */
/*     R (radius), LONG (longitude), and  LAT (latitude). */


/*       X(1)       X(2)     X(3)        R         LONG      LAT */
/*       --------------------------      -------------------------- */
/*       0.0000     0.0000   0.0000      0.0000    0.0000    0.0000 */
/*       1.0000     0.0000   0.0000      1.0000    0.0000    0.0000 */
/*       0.0000     1.0000   0.0000      1.0000   90.0000    0.0000 */
/*       0.0000     0.0000   1.0000      1.0000    0.0000   90.0000 */
/*      -1.0000     0.0000   0.0000      1.0000  180.0000    0.0000 */
/*       0.0000    -1.0000   0.0000      1.0000  -90.0000    0.0000 */
/*       0.0000     0.0000  -1.0000      1.0000    0.0000  -90.0000 */
/*       1.0000     1.0000   0.0000      1.4142   45.0000    0.0000 */
/*       1.0000     0.0000   1.0000      1.4142    0.0000   45.0000 */
/*       0.0000     1.0000   1.0000      1.4142   90.0000   45.0000 */
/*       1.0000     1.0000   1.0000      1.7320   45.0000   35.2643 */


/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     C.H. Acton      (JPL) */
/*     N.J. Bachman    (JPL) */
/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.2, 30-JUL-2003 (NJB) (CHA) */

/*        Various header changes were made to improve clarity.  Some */
/*        minor header corrections were made. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     rectangular to latitudinal coordinates */

/* -& */
/* $ Revisions */

/* -     Beta Version 1.0.1, 1-Feb-1989 (WLT) */

/*      Example section of header upgraded. */

/* -& */

/*     Store rectangular coordinates in temporary variables */

/* Computing MAX */
    d__1 = abs(rectan[0]), d__2 = abs(rectan[1]), d__1 = max(d__1,d__2), d__2 
	    = abs(rectan[2]);
    big = max(d__1,d__2);
    if (big > 0.) {
	x = rectan[0] / big;
	y = rectan[1] / big;
	z__ = rectan[2] / big;
	*radius = big * sqrt(x * x + y * y + z__ * z__);
	*lat = atan2(z__, sqrt(x * x + y * y));
	x = rectan[0];
	y = rectan[1];
	if (x == 0. && y == 0.) {
	    *long__ = 0.;
	} else {
	    *long__ = atan2(y, x);
	}
    } else {
	*radius = 0.;
	*lat = 0.;
	*long__ = 0.;
    }
    return 0;
} /* reclat_ */

