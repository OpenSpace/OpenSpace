/* latsph.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      LATSPH ( Latitudinal to spherical coordinates ) */
/* Subroutine */ int latsph_(doublereal *radius, doublereal *long__, 
	doublereal *lat, doublereal *rho, doublereal *colat, doublereal *
	longs)
{
    doublereal ph, th;
    extern doublereal halfpi_(void);

/* $ Abstract */

/*     Convert from latitudinal coordinates to spherical coordinates. */

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
/*      RADIUS     I   Distance of a point from the origin. */
/*      LONG       I   Angle of the point from the XZ plane in radians. */
/*      LAT        I   Angle of the point from the XY plane in radians. */
/*      RHO        O   Distance of the point from the origin. */
/*      COLAT      O   Angle of the point from positive Z axis (radians). */
/*      LONGS      O   Angle of the point from the XZ plane (radians). */

/* $ Detailed_Input */

/*      RADIUS     Distance of a point from the origin. */

/*      LONG       Angle of the point from the XZ plane in radians. */

/*      LAT        Angle of the point from the XY plane in radians. */

/* $ Detailed_Output */

/*      RHO        Distance of the point from the origin. */

/*      COLAT      Angle between the vector from the origin to the point */
/*                 and the positive Z axis in radians. */

/*      LONGS      Angle of the point from the XZ plane (radians). */

/* $ Parameters */

/*      None. */

/* $ Particulars */

/*      This routine returns the spherical coordinates of a point */
/*      whose position is input in latitudinal coordinates. */

/*      Latitudinal coordinates are defined by a distance from a central */
/*      reference point, an angle from a reference meridian, and an angle */
/*      above the equator of a sphere centered at the central reference */
/*      point. */

/*      Spherical coordinates are defined by a distance from a central */
/*      reference point, an angle from a reference meridian, and an angle */
/*      from the z-axis. */

/* $ Examples */

/*      Co-latitude is obtained by subtracting latitude from HALFPI() */
/*      Radius and longitude mean the same thing in both latitudinal */
/*      and spherical coordinates.  The table below lists LAT */
/*      corresponding COLAT in terms of degrees. */

/*             LAT            COLAT */
/*            ------         ------ */
/*              0             90 */
/*             20             70 */
/*             45             45 */
/*            -30            120 */
/*             90              0 */
/*            -45            135 */

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

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     latitudinal to spherical coordinates */

/* -& */
/* $ Revisions */

/* -     Beta Version 1.0.1, 1-Feb-1989 (WLT) */

/*      Example section of header upgraded. */

/* -& */

/*     SPICELIB functions */


/*     Local Variables */


/*     Convert to spherical coordinates, storing the results in */
/*     temporary variables */

    th = halfpi_() - *lat;
    ph = *long__;

/*     Move results to output variables */

    *rho = *radius;
    *colat = th;
    *longs = ph;
    return 0;
} /* latsph_ */

