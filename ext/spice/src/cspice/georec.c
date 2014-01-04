/* georec.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b11 = 1.;

/* $Procedure      GEOREC ( Geodetic to rectangular coordinates ) */
/* Subroutine */ int georec_(doublereal *long__, doublereal *lat, doublereal *
	alt, doublereal *re, doublereal *f, doublereal *rectan)
{
    /* System generated locals */
    doublereal d__1, d__2, d__3, d__4;

    /* Builtin functions */
    double cos(doublereal), sin(doublereal), sqrt(doublereal);

    /* Local variables */
    doublereal base[3], cphi, sphi, scale, x, y;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errdp_(char *, 
	    doublereal *, ftnlen), vlcom_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    doublereal clmbda, rp, slmbda, height, normal[3];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), surfnm_(doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *);
    extern logical return_(void);
    doublereal big;

/* $ Abstract */

/*     Convert geodetic coordinates to rectangular coordinates. */

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
/*     LONG       I   Geodetic longitude of point (radians). */
/*     LAT        I   Geodetic latitude  of point (radians). */
/*     ALT        I   Altitude of point above the reference spheroid. */
/*     RE         I   Equatorial radius of the reference spheroid. */
/*     F          I   Flattening coefficient. */
/*     RECTAN     O   Rectangular coordinates of point. */

/* $ Detailed_Input */

/*     LONG       Geodetic longitude of the input point.  This is the */
/*                angle between the prime meridian and the meridian */
/*                containing RECTAN.  The direction of increasing */
/*                longitude is from the +X axis towards the +Y axis. */

/*                Longitude is measured in radians.  On input, the */
/*                range of longitude is unrestricted. */

/*     LAT        Geodetic latitude of the input point.  For a point P */
/*                on the reference spheroid, this is the angle between */
/*                the XY plane and the outward normal vector at P. */
/*                For a point P not on the reference spheroid, the */
/*                geodetic latitude is that of the closest point to P on */
/*                the spheroid. */

/*                Latitude is measured in radians.  On input, the */
/*                range of latitude is unrestricted. */

/*     ALT        Altitude of point above the reference spheroid. */

/*     RE         Equatorial radius of a reference spheroid.  This */
/*                spheroid is a volume of revolution:  its horizontal */
/*                cross sections are circular.  The shape of the */
/*                spheroid is defined by an equatorial radius RE and */
/*                a polar radius RP. */

/*     F          Flattening coefficient = (RE-RP) / RE,  where RP is */
/*                the polar radius of the spheroid. */

/* $ Detailed_Output */

/*     RECTAN     The rectangular coordinates of a point. */

/*                The units associated with RECTAN are those associated */
/*                with the input ALT. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the flattening coefficient is greater than or equal to */
/*        one, the error SPICE(VALUEOUTOFRANGE) is signaled. */

/*     2) If the equatorial radius is less than or equal to zero, */
/*        the error SPICE(VALUEOUTOFRANGE) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Given the geodetic coordinates of a point, and the constants */
/*     describing the reference spheroid,  this routine returns the */
/*     bodyfixed rectangular coordinates of the point.  The bodyfixed */
/*     rectangular frame is that having the x-axis pass through the */
/*     0 degree latitude 0 degree longitude point.  The y-axis passes */
/*     through the 0 degree latitude 90 degree longitude.  The z-axis */
/*     passes through the 90 degree latitude point.  For some bodies */
/*     this coordinate system may not be a right-handed coordinate */
/*     system. */

/* $ Examples */

/*     This routine can be used to convert body fixed geodetic */
/*     coordinates (such as the used for United States Geological */
/*     Survey topographic maps) to bodyfixed rectangular coordinates */
/*     such as the Satellite Tracking and Data Network of 1973. */

/*     The code would look something like this */

/*        C */
/*        C     Using the equatorial radius of the Clark66 spheroid */
/*        C     (CLARKR = 6378.2064 km) and the Clark 66 flattening */
/*        C     factor (CLARKF = 1.0D0 / 294.9787D0 ) convert to */
/*        C     body fixed rectangular coordinates. */
/*        C */
/*              CALL GEOREC ( LONG, LAT, ALT, CLARKR, CLARKF, X ) */

/*        C */
/*        C     Add the North American Datum of 1927 to STDN 73 center */
/*        C     offset */
/*        C */
/*              CALL VADD   ( X, OFFSET, STDNX ) */


/*     Below are two tables. */

/*     Listed in the first table (under LONG, LAT, and ALT ) are */
/*     geodetic coordinate triples that approximately represent points */
/*     whose rectangular coordinates are taken from the set {-1, 0, 1}. */
/*     (Angular quantities are given in degrees.) */

/*     The result of the code fragment */

/*        C */
/*        C     Use the SPICELIB routine CONVRT to convert the angular */
/*        C     quantities to degrees. */
/*        C */
/*              CALL CONVRT ( LAT,  'DEGREES', 'RADIANS', LAT  ) */
/*              CALL CONVRT ( LONG, 'DEGREES', 'RADIANS', LONG ) */

/*              CALL GEOREC ( LONG, LAT, ALT, CLARKR, CLARKF, X ) */


/*     are listed in the second parallel table under X(1), X(2) and X(3). */


/*       LONG      LAT        ALT            X(1)       X(2)     X(3) */
/*       ------------------------------      -------------------------- */
/*       0.0000    90.0000   -6356.5838      0.0000     0.0000   0.0000 */
/*       0.0000     0.0000   -6377.2063      1.0000     0.0000   0.0000 */
/*      90.0000     0.0000   -6377.2063      0.0000     1.0000   0.0000 */
/*       0.0000    90.0000   -6355.5838      0.0000     0.0000   1.0000 */
/*     180.0000     0.0000   -6377.2063     -1.0000     0.0000   0.0000 */
/*     -90.0000     0.0000   -6377.2063      0.0000    -1.0000   0.0000 */
/*       0.0000   -90.0000   -6355.5838      0.0000     0.0000  -1.0000 */
/*      45.0000     0.0000   -6376.7921      1.0000     1.0000   0.0000 */
/*       0.0000    88.7070   -6355.5725      1.0000     0.0000   1.0000 */
/*      90.0000    88.7070   -6355.5725      0.0000     1.0000   1.0000 */
/*      45.0000    88.1713   -6355.5612      1.0000     1.0000   1.0000 */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     C.H. Acton      (JPL) */
/*     N.J. Bachman    (JPL) */
/*     H.A. Neilan     (JPL) */
/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     See FUNDAMENTALS OF ASTRODYNAMICS, Bate, Mueller, White */
/*     published by Dover for a description of geodetic coordinates. */

/* $ Version */

/* -    SPICELIB Version 1.0.2, 29-JUL-2003 (NJB) (CHA) */

/*        Various header changes were made to improve clarity.  Some */
/*        minor header corrections were made. */

/* -   SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*       Comment section for permuted index source lines was added */
/*       following the header. */

/* -   SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     geodetic to rectangular coordinates */

/* -& */
/* $ Revisions */

/* -    Beta Version 3.0.0, 9-JUN-1989  (HAN) */

/*        Error handling added to detect equatorial radius out of */
/*        range. If the equatorial radius is less than or equal to */
/*        zero, an error is signaled. */

/* -    Beta Version 2.0.0, 21-DEC-1988 (HAN) */

/*        Error handling to detect invalid flattening coefficients */
/*        was added. Because the flattening coefficient is used to */
/*        compute the polar radius, it must be checked so that the */
/*        polar radius greater than zero. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("GEOREC", (ftnlen)6);
    }

/*     The equatorial radius must be greater than zero. */

    if (*re <= 0.) {
	setmsg_("Equatorial radius was *.", (ftnlen)24);
	errdp_("*", re, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("GEOREC", (ftnlen)6);
	return 0;
    }

/*     If the flattening coefficient is greater than one, the polar */
/*     radius computed below is negative. If it's equal to one, the */
/*     polar radius is zero. Either case is a problem, so signal an */
/*     error and check out. */

    if (*f >= 1.) {
	setmsg_("Flattening coefficient was *.", (ftnlen)29);
	errdp_("*", f, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("GEOREC", (ftnlen)6);
	return 0;
    }

/*     Move the altitude to a temporary variable. */

    height = *alt;

/*     Compute the polar radius of the spheroid. */

    rp = *re - *f * *re;

/*     Compute a scale factor needed for finding the rectangular */
/*     coordinates of a point with altitude 0 but the same geodetic */
/*     latitude and longitude as the input point. */

    cphi = cos(*lat);
    sphi = sin(*lat);
    clmbda = cos(*long__);
    slmbda = sin(*long__);
/* Computing MAX */
    d__3 = (d__1 = *re * cphi, abs(d__1)), d__4 = (d__2 = rp * sphi, abs(d__2)
	    );
    big = max(d__3,d__4);
    x = *re * cphi / big;
    y = rp * sphi / big;
    scale = 1. / (big * sqrt(x * x + y * y));

/*     Compute the rectangular coordinates of the point with zero */
/*     altitude. */

    base[0] = scale * *re * *re * clmbda * cphi;
    base[1] = scale * *re * *re * slmbda * cphi;
    base[2] = scale * rp * rp * sphi;

/*     Fetch the normal to the ellipsoid at this point. */

    surfnm_(re, re, &rp, base, normal);

/*     Move along the normal to the input point. */

    vlcom_(&c_b11, base, &height, normal, rectan);
    chkout_("GEOREC", (ftnlen)6);
    return 0;
} /* georec_ */

