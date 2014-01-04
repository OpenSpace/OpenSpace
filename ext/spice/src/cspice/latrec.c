/* latrec.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      LATREC ( Latitudinal to rectangular coordinates ) */
/* Subroutine */ int latrec_(doublereal *radius, doublereal *long__, 
	doublereal *lat, doublereal *rectan)
{
    /* Builtin functions */
    double cos(doublereal), sin(doublereal);

    /* Local variables */
    doublereal x, y, z__;

/* $ Abstract */

/*     Convert from latitudinal coordinates to rectangular coordinates. */

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
/*     RADIUS     I   Distance of a point from the origin. */
/*     LONG       I   Longitude of point in radians. */
/*     LAT        I   Latitude of point in radians. */
/*     RECTAN     O   Rectangular coordinates of the point. */

/* $ Detailed_Input */

/*     RADIUS     Distance of a point from the origin. */

/*     LONG       Longitude of the input point.  This is the angle */
/*                between the prime meridian and the meridian */
/*                containing the point.  The direction of increasing */
/*                longitude is from the +X axis towards the +Y axis. */

/*                Longitude is measured in radians.  On input, the */
/*                range of longitude is unrestricted. */

/*     LAT        Latitude of the input point.  This is the angle from */
/*                the XY plane of the ray from the origin through the */
/*                point. */

/*                Latitude is measured in radians. On input, the range */
/*                of latitude is unrestricted. */

/* $ Detailed_Output */

/*     RECTAN     The rectangular coordinates of the input point. */
/*                RECTAN is a 3-vector. */

/*                The units associated with RECTAN are those */
/*                associated with the input RADIUS. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine returns the rectangular coordinates of a point */
/*     whose position is input in latitudinal coordinates. */

/*     Latitudinal coordinates are defined by a distance from a central */
/*     reference point, an angle from a reference meridian, and an angle */
/*     above the equator of a sphere centered at the central reference */
/*     point. */

/* $ Examples */

/*     Below are two tables. */

/*     Listed in the first table (under R, LONG and LAT) are */
/*     latitudinal coordinate triples that approximately represent */
/*     points whose rectangular coordinates are taken from the set */
/*     {-1, 0, 1}.  (Angular quantities are given in degrees.) */

/*     The results of the code fragment */

/*          C */
/*          C     Use the SPICELIB routine CONVRT to convert the angular */
/*          C     quantities to radians */
/*          C */
/*                CALL CONVRT ( LAT,  'DEGREES', 'RADIANS', LAT  ) */
/*                CALL CONVRT ( LONG, 'DEGREES', 'RADIANS', LONG ) */

/*                CALL LATREC ( R, LONG, LAT, X ) */


/*     are listed in the second parallel table under X(1), X(2) and X(3). */


/*       R         LONG       LAT           X(1)       X(2)     X(3) */
/*       --------------------------         -------------------------- */
/*       0.0000    0.0000    0.0000         0.0000     0.0000   0.0000 */
/*       1.0000    0.0000    0.0000         1.0000     0.0000   0.0000 */
/*       1.0000   90.0000    0.0000         0.0000     1.0000   0.0000 */
/*       1.0000    0.0000   90.0000         0.0000     0.0000   1.0000 */
/*       1.0000  180.0000    0.0000        -1.0000     0.0000   0.0000 */
/*       1.0000  -90.0000    0.0000         0.0000    -1.0000   0.0000 */
/*       1.0000    0.0000  -90.0000         0.0000     0.0000  -1.0000 */
/*       1.4142   45.0000    0.0000         1.0000     1.0000   0.0000 */
/*       1.4142    0.0000   45.0000         1.0000     0.0000   1.0000 */
/*       1.4142   90.0000   45.0000         0.0000     1.0000   1.0000 */
/*       1.7320   45.0000   35.2643         1.0000     1.0000   1.0000 */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     C.H. Acton      (JPL) */
/*     N.J. Bachman    (JPL) */
/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.2, 29-JUL-2003 (NJB) (CHA) */

/*        Various header changes were made to improve clarity.  Some */
/*        minor header corrections were made. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     latitudinal to rectangular coordinates */

/* -& */
/* $ Revisions */

/* -     Beta Version 1.0.1, 1-Feb-1989 (WLT) */

/*      Example section of header upgraded. */

/* -& */

/*     Convert to rectangular coordinates, storing the results in */
/*     temporary variables. */

    x = *radius * cos(*long__) * cos(*lat);
    y = *radius * sin(*long__) * cos(*lat);
    z__ = *radius * sin(*lat);

/*  Move the results to the output variables. */

    rectan[0] = x;
    rectan[1] = y;
    rectan[2] = z__;
    return 0;
} /* latrec_ */

