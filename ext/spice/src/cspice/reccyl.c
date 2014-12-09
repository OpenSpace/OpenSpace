/* reccyl.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      RECCYL ( Rectangular to cylindrical coordinates ) */
/* Subroutine */ int reccyl_(doublereal *rectan, doublereal *r__, doublereal *
	long__, doublereal *z__)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(doublereal), atan2(doublereal, doublereal);

    /* Local variables */
    doublereal x, y;
    extern doublereal twopi_(void);
    doublereal big;

/* $ Abstract */

/*      Convert from rectangular to cylindrical coordinates. */


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

/*      CONVERSION, COORDINATES */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  ------------------------------------------------- */
/*      RECTAN     I   Rectangular coordinates of a point. */
/*      R          O   Distance of the point from Z axis. */
/*      LONG       O   Angle (radians) of the point from XZ plane */
/*      Z          O   Height of the point above XY plane. */

/* $ Detailed_Input */

/*      RECTAN     Rectangular coordinates of the point of interest. */

/* $ Detailed_Output */

/*      R          Distance of the point of interest from Z axis. */

/*      LONG       Cylindrical angle (in radians) of the point of */
/*                 interest from XZ plane. */

/*      Z          Height of the point above XY plane. */

/* $ Parameters */

/*      None. */

/* $ Particulars */

/*      This routine transforms the coordinates of a point from */
/*      rectangular to cylindrical coordinates. */

/* $ Examples */

/*     Below are two tables. */

/*     Listed in the first table (under X(1), X(2) and X(3) ) are a */
/*     number of points whose rectangular coordinates coorindates are */
/*     taken from the set {-1, 0, 1}. */

/*     The result of the code fragment */

/*          CALL RECCYL ( X, R, LONG, Z ) */

/*          Use the SPICELIB routine CONVRT to convert the angular */
/*          quantities to degrees */

/*          CALL CONVRT ( LONG, 'RADIANS', 'DEGREES', LONG ) */

/*     are listed to 4 decimal places in the second parallel table under */
/*     R (radius), LONG (longitude), and  Z (same as rectangular Z */
/*     coordinate). */


/*       X(1)       X(2)     X(3)        R         LONG     Z */
/*       --------------------------      ------------------------- */
/*       0.0000     0.0000   0.0000      0.0000    0.0000   0.0000 */
/*       1.0000     0.0000   0.0000      1.0000    0.0000   0.0000 */
/*       0.0000     1.0000   0.0000      1.0000   90.0000   0.0000 */
/*       0.0000     0.0000   1.0000      0.0000    0.0000   1.0000 */
/*      -1.0000     0.0000   0.0000      1.0000  180.0000   0.0000 */
/*       0.0000    -1.0000   0.0000      1.0000  270.0000   0.0000 */
/*       0.0000     0.0000  -1.0000      0.0000    0.0000  -1.0000 */
/*       1.0000     1.0000   0.0000      1.4142   45.0000   0.0000 */
/*       1.0000     0.0000   1.0000      1.0000    0.0000   1.0000 */
/*       0.0000     1.0000   1.0000      1.0000   90.0000   1.0000 */
/*       1.0000     1.0000   1.0000      1.4142   45.0000   1.0000 */

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

/*     rectangular to cylindrical coordinates */

/* -& */

/*     SPICELIB functions */


/*     Local Variables */


/*     Use temporary variables for computing R. */

/* Computing MAX */
    d__1 = abs(rectan[0]), d__2 = abs(rectan[1]);
    big = max(d__1,d__2);

/*     Convert to cylindrical coordinates */

    *z__ = rectan[2];
    if (big == 0.) {
	*r__ = 0.;
	*long__ = 0.;
    } else {
	x = rectan[0] / big;
	y = rectan[1] / big;
	*r__ = big * sqrt(x * x + y * y);
	*long__ = atan2(y, x);
    }
    if (*long__ < 0.) {
	*long__ += twopi_();
    }
    return 0;
} /* reccyl_ */

