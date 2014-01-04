/* recsph.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      RECSPH ( Rectangular to spherical coordinates ) */
/* Subroutine */ int recsph_(doublereal *rectan, doublereal *r__, doublereal *
	colat, doublereal *long__)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal), atan2(doublereal, doublereal);

    /* Local variables */
    doublereal x, y, z__, big;

/* $ Abstract */

/*     Convert from rectangular coordinates to spherical coordinates. */

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

/*      CONVERSION,  COORDINATES */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      RECTAN     I   Rectangular coordinates of a point. */
/*      R          O   Distance of the point from the origin. */
/*      COLAT      O   Angle of the point from the positive Z-axis. */
/*      LONG       O   Longitude of the point radians. */

/* $ Detailed_Input */

/*      RECTAN     The rectangular coordinates of a point. */

/* $ Detailed_Output */

/*      R          Distance of the point from the origin. */

/*      COLAT      Angle between the point and the positive z-axis. */

/*      LONG       Longitude of the point in radians.  This is the angle */
/*                 between the positive X-axis and the orthogonal */
/*                 projection of the point onto the XY plane.  LONG */
/*                 increases in the counterclockwise sense about the */
/*                 positive Z-axis.  The range of LONG is: */

/*                    -pi < LONG <= pi */

/* $ Parameters */

/*      None. */

/* $ Particulars */

/*      This routine returns the spherical coordinates of a point */
/*      whose position is input in rectangular coordinates. */

/*      Spherical coordinates are defined by a distance from a central */
/*      reference point, an angle from a reference meridian, and an angle */
/*      from the z-axis. */

/* $ Examples */

/*     Below are two tables. */

/*     Listed in the first table (under X(1), X(2) and X(3) ) are a */
/*     number of points whose rectangular coordinates are */
/*     taken from the set {-1, 0, 1}. */

/*     The result of the code fragment */

/*          CALL RECSPH ( X, R, COLAT, LONG ) */

/*          Use the SPICELIB routine CONVRT to convert the angular */
/*          quantities to degrees */

/*          CALL CONVRT ( COLAT, 'RADIANS', 'DEGREES', COLAT ) */
/*          CALL CONVRT ( LONG,  'RADIANS', 'DEGREES', LONG  ) */

/*     are listed to 4 decimal places in the second parallel table under */
/*     R (radius), COLAT (co-latitude), and  LONG (longitude). */

/*       X(1)       X(2)     X(3)        R         COLAT       LONG */
/*       --------------------------      ---------------------------- */
/*       0.0000     0.0000   0.0000      0.0000     0.0000     0.0000 */
/*       1.0000     0.0000   0.0000      1.0000    90.0000     0.0000 */
/*       0.0000     1.0000   0.0000      1.0000    90.0000    90.0000 */
/*       0.0000     0.0000   1.0000      1.0000     0.0000     0.0000 */
/*      -1.0000     0.0000   0.0000      1.0000    90.0000   180.0000 */
/*       0.0000    -1.0000   0.0000      1.0000    90.0000   -90.0000 */
/*       0.0000     0.0000  -1.0000      1.0000   180.0000     0.0000 */
/*       1.0000     1.0000   0.0000      1.4142    90.0000    45.0000 */
/*       1.0000     0.0000   1.0000      1.4142    45.0000     0.0000 */
/*       0.0000     1.0000   1.0000      1.4142    45.0000    90.0000 */
/*       1.0000     1.0000   1.0000      1.7320    54.7356    45.0000 */

/* $ Restrictions */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.2, 07-JAN-2002 (NJB) */

/*        Fixed description of LONG in Brief_I/O and Detailed_I/O */
/*        header sections. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     rectangular to spherical coordinates */

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
	*r__ = big * sqrt(x * x + y * y + z__ * z__);
	*colat = atan2(sqrt(x * x + y * y), z__);
	x = rectan[0];
	y = rectan[1];
	if (x == 0. && y == 0.) {
	    *long__ = 0.;
	} else {
	    *long__ = atan2(y, x);
	}
    } else {
	*r__ = 0.;
	*colat = 0.;
	*long__ = 0.;
    }
    return 0;
} /* recsph_ */

