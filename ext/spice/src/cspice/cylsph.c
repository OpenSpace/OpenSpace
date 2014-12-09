/* cylsph.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      CYLSPH ( Cylindrical to spherical ) */
/* Subroutine */ int cylsph_(doublereal *r__, doublereal *longc, doublereal *
	z__, doublereal *radius, doublereal *colat, doublereal *long__)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal), atan2(doublereal, doublereal);

    /* Local variables */
    doublereal x, y, rh, th, big;

/* $ Abstract */

/*      Convert from cylindrical to spherical coordinates. */

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
/*      --------  ---  ------------------------------------------------- */
/*      R          I   Distance of point from Z axis. */
/*      LONGC      I   Angle (radians) of point from XZ plane. */
/*      Z          I   Height of point above XY plane. */
/*      RADIUS     O   Distance of point from origin. */
/*      COLAT      O   Polar angle (co-latitude in radians) of point. */
/*      LONG       O   Azimuthal angle (longitude) of point (radians). */

/* $ Detailed_Input */

/*      R          Distance of the point of interest from Z axis. */

/*      LONGC      Cylindrical angle (radians) of the point from the */
/*                 XZ plane. */

/*      Z          Height of the point above XY plane. */

/* $ Detailed_Output */

/*      RADIUS     Distance of the point from origin. */

/*      COLAT      Polar angle (co-latitude in radians) of the point. */

/*      LONG       Azimuthal angle (longitude) of the point (radians). */

/* $ Parameters */

/*      None. */

/* $ Particulars */

/*      This returns the spherical coordinates of a point whose position */
/*      is input through cylindrical coordinates. */

/* $ Examples */


/*      Below are two tables:  The first is a set of input values */
/*      the second is the result of the following sequence of */
/*      calls to Spicelib routines.  Note all input and output angular */
/*      quantities are in degrees. */

/*         CALL CONVRT ( LONGC, 'DEGREES', 'RADIANS', LONGC       ) */

/*         CALL CYLSPH ( R,      LONGC, Z,  RADIUS,   COLAT, LONG ) */

/*         CALL CONVRT ( LONG,  'RADIANS', 'DEGREES', LONG        ) */
/*         CALL CONVRT ( LAT,   'RADIANS', 'DEGREES', LAT         ) */



/*      Inputs:                         Results: */

/*      R        LONGC    Z             RADIUS   LONG     COLAT */
/*      ------   ------   ------        ------   ------   ------ */
/*      1.0000     0       0            1.0000     0       90.00 */
/*      1.0000    90.00    0            1.0000    90.00    90.00 */
/*      1.0000   180.00    1.000        1.4142   180.00    45.00 */
/*      1.0000   180.00   -1.000        1.4142   180.00   135.00 */
/*      0.0000   180.00    1.000        1.0000   180.00     0.00 */
/*      0.0000    33.00    0            0.0000    33.00     0.00 */

/* $ Restrictions */

/*      None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*      None. */

/* $ Author_and_Institution */

/*      W.L. Taber      (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -    SPICELIB Version 1.0.2, 22-AUG-2001 (EDW) */

/*        Corrected ENDIF to END IF. Obsolete Revisions section */
/*        deleted. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     cylindrical to spherical */

/* -& */

/*     Local variables */


/*  Convert to spherical, storing in temporary variables */

/* Computing MAX */
    d__1 = abs(*r__), d__2 = abs(*z__);
    big = max(d__1,d__2);
    if (big == 0.) {
	th = 0.;
	rh = 0.;
    } else {
	x = *r__ / big;
	y = *z__ / big;
	rh = big * sqrt(x * x + y * y);
	th = atan2(*r__, *z__);
    }

/*     Move the results to output variables */

    *long__ = *longc;
    *radius = rh;
    *colat = th;
    return 0;
} /* cylsph_ */

