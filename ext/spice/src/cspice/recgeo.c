/* recgeo.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      RECGEO ( Rectangular to geodetic ) */
/* Subroutine */ int recgeo_(doublereal *rectan, doublereal *re, doublereal *
	f, doublereal *long__, doublereal *lat, doublereal *alt)
{
    doublereal base[3], a, b, c__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errdp_(char *, 
	    doublereal *, ftnlen), reclat_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    doublereal radius, normal[3];
    extern /* Subroutine */ int nearpt_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *), sigerr_(
	    char *, ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen),
	     surfnm_(doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *);
    extern logical return_(void);

/* $ Abstract */

/*     Convert from rectangular coordinates to geodetic coordinates. */

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

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     RECTAN     I   Rectangular coordinates of a point. */
/*     RE         I   Equatorial radius of the reference spheroid. */
/*     F          I   Flattening coefficient. */
/*     LONG       O   Geodetic longitude of the point (radians). */
/*     LAT        O   Geodetic latitude  of the point (radians). */
/*     ALT        O   Altitude of the point above reference spheroid. */

/* $ Detailed_Input */

/*     RECTAN     The rectangular coordinates of a point. */

/*     RE         Equatorial radius of a reference spheroid.  This */
/*                spheroid is a volume of revolution:  its horizontal */
/*                cross sections are circular.  The shape of the */
/*                spheroid is defined by an equatorial radius RE and */
/*                a polar radius RP. */

/*     F          Flattening coefficient = (RE-RP) / RE,  where RP is */
/*                the polar radius of the spheroid. */

/* $ Detailed_Output */

/*     LONG       Geodetic longitude of the input point.  This is the */
/*                angle between the prime meridian and the meridian */
/*                containing RECTAN.  The direction of increasing */
/*                longitude is from the +X axis towards the +Y axis. */

/*                LONG is output in radians.  The range of LONG is */
/*                [-pi, pi]. */

/*     LAT        Geodetic latitude of the input point.  For a point P */
/*                on the reference spheroid, this is the angle between */
/*                the XY plane and the outward normal vector at P. */
/*                For a point P not on the reference spheroid, the */
/*                geodetic latitude is that of the closest point to P on */
/*                the spheroid. */

/*                LAT is output in radians.  The range of LAT is */
/*                [-pi/2, pi/2]. */


/*     ALT        Altitude of point above the reference spheroid. */

/*                The units associated with ALT are those associated */
/*                with the input RECTAN. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the equatorial radius is non-positive, the error */
/*        SPICE(VALUEOUTOFRANGE) is signaled. */

/*     2) If the flattening coefficient is greater than or equal to */
/*        one, the error SPICE(VALUEOUTOFRANGE) is signaled. */

/*     3) For points inside the reference ellipsoid, the nearest */
/*        point on the ellipsoid to RECTAN may not be unique, so */
/*        latitude may not be well-defined. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Given the body-fixed rectangular coordinates of a point, and the */
/*     constants describing the reference spheroid,  this routine */
/*     returns the geodetic coordinates of the point.  The body-fixed */
/*     rectangular frame is that having the x-axis pass through the */
/*     0 degree latitude 0 degree longitude point.  The y-axis passes */
/*     through the 0 degree latitude 90 degree longitude.  The z-axis */
/*     passes through the 90 degree latitude point.  For some bodies */
/*     this coordinate system may not be a right-handed coordinate */
/*     system. */

/* $ Examples */

/*     This routine can be used to convert body fixed rectangular */
/*     coordinates (such as the Satellite Tracking and Data Network */
/*     of 1973) to geodetic coordinates such as those used by the */
/*     United States Geological Survey topographic maps. */

/*     The code would look something like this */

/*     C */
/*     C     Shift the STDN-73 coordinates to line up with the center */
/*     C     of the Clark66 reference system. */
/*     C */
/*           CALL VSUB ( STDNX, OFFSET, X ) */

/*     C */
/*     C     Using the equatorial radius of the Clark66 spheroid */
/*     C     (CLARKR = 6378.2064 km) and the Clark 66 flattening */
/*     C     factor (CLARKF = 1.0D0 / 294.9787D0 ) convert to */
/*     C     geodetic coordinates of the North American Datum of 1927. */
/*     C */
/*           CALL RECGEO ( X, CLARKR, CLARKF, LONG, LAT, ALT ) */



/*     Below are two tables. */

/*     Listed in the first table (under X(1), X(2) and X(3)) are a */
/*     number of points whose rectangular coordinates are */
/*     taken from the set {-1, 0, 1}. */

/*     The results of the code fragment */

/*          CALL RECGEO ( X, CLARKR, CLARKF, LONG, LAT, ALT ) */

/*          Use the SPICELIB routine CONVRT to convert the angular */
/*          quantities to degrees */

/*          CALL CONVRT ( LAT,  'RADIANS', 'DEGREES', LAT  ) */
/*          CALL CONVRT ( LONG, 'RADIANS', 'DEGREES', LONG ) */

/*     are listed to 4 decimal places in the second parallel table under */
/*     LONG (longitude), LAT (latitude), and ALT (altitude). */


/*       X(1)       X(2)     X(3)         LONG      LAT        ALT */
/*       --------------------------       ---------------------------- */
/*       0.0000     0.0000   0.0000       0.0000    90.0000   -6356.5838 */
/*       1.0000     0.0000   0.0000       0.0000     0.0000   -6377.2063 */
/*       0.0000     1.0000   0.0000      90.0000     0.0000   -6377.2063 */
/*       0.0000     0.0000   1.0000       0.0000    90.0000   -6355.5838 */
/*      -1.0000     0.0000   0.0000     180.0000     0.0000   -6377.2063 */
/*       0.0000    -1.0000   0.0000     -90.0000     0.0000   -6377.2063 */
/*       0.0000     0.0000  -1.0000       0.0000   -90.0000   -6355.5838 */
/*       1.0000     1.0000   0.0000      45.0000     0.0000   -6376.7921 */
/*       1.0000     0.0000   1.0000       0.0000    88.7070   -6355.5725 */
/*       0.0000     1.0000   1.0000      90.0000    88.7070   -6355.5725 */
/*       1.0000     1.0000   1.0000      45.0000    88.1713   -6355.5612 */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     See FUNDAMENTALS OF ASTRODYNAMICS, Bate, Mueller, White */
/*     published by Dover for a description of geodetic coordinates. */

/* $ Author_and_Institution */

/*     C.H. Acton      (JPL) */
/*     N.J. Bachman    (JPL) */
/*     H.A. Neilan     (JPL) */
/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.3, 02-JUL-2007 (NJB) */

/*        In Examples section of header, description of right-hand */
/*        table was updated to use correct names of columns. Term */
/*        "bodyfixed" is now hyphenated. */

/* -    SPICELIB Version 1.0.2, 30-JUL-2003 (NJB) (CHA) */

/*        Various header changes were made to improve clarity.  Some */
/*        minor header corrections were made. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     rectangular to geodetic */

/* -& */
/* $ Revisions */

/* -    Beta Version 3.0.1, 9-JUN-1989 (HAN) */

/*        Error handling was added to detect and equatorial radius */
/*        whose value is less than or equal to zero. */

/* -    Beta Version 2.0.0, 21-DEC-1988 (HAN) */

/*        Error handling to detect invalid flattening coefficients */
/*        was added. Because the flattening coefficient is used to */
/*        compute the length of an axis, it must be checked so that */
/*        the length is greater than zero. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("RECGEO", (ftnlen)6);
    }

/*     The equatorial radius must be positive. If not, signal an error */
/*     and check out. */

    if (*re <= 0.) {
	setmsg_("Equatorial radius was *.", (ftnlen)24);
	errdp_("*", re, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("RECGEO", (ftnlen)6);
	return 0;
    }

/*     If the flattening coefficient is greater than one, the length */
/*     of the 'C' axis computed below is negative. If it's equal to one, */
/*     the length of the axis is zero. Either case is a problem, so */
/*     signal an error and check out. */

    if (*f >= 1.) {
	setmsg_("Flattening coefficient was *.", (ftnlen)29);
	errdp_("*", f, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("RECGEO", (ftnlen)6);
	return 0;
    }

/*     Determine the lengths of the axes of the reference ellipsoid. */

    a = *re;
    b = *re;
    c__ = *re - *f * *re;

/*     Find the point on the reference spheroid closes to the input point */

    nearpt_(rectan, &a, &b, &c__, base, alt);

/*     From this closest point determine the surface normal */

    surfnm_(&a, &b, &c__, base, normal);

/*     Using the surface normal, determine the latitude and longitude */
/*     of the input point. */

    reclat_(normal, &radius, long__, lat);
    chkout_("RECGEO", (ftnlen)6);
    return 0;
} /* recgeo_ */

