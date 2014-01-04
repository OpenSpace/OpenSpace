/* drdgeo.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      DRDGEO ( Derivative of rectangular w.r.t. geodetic ) */
/* Subroutine */ int drdgeo_(doublereal *long__, doublereal *lat, doublereal *
	alt, doublereal *re, doublereal *f, doublereal *jacobi)
{
    /* Builtin functions */
    double cos(doublereal), sin(doublereal), sqrt(doublereal);

    /* Local variables */
    doublereal clat, flat, clon, slat, slon, flat2, g;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errdp_(char *, 
	    doublereal *, ftnlen);
    doublereal g2, dgdlat;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     This routine computes the Jacobian of the transformation from */
/*     geodetic to rectangular coordinates. */

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

/*     COORDINATES */
/*     DERIVATIVES */
/*     MATRIX */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     LONG       I   Geodetic longitude of point (radians). */
/*     LAT        I   Geodetic latitude of point (radians). */
/*     ALT        I   Altitude of point above the reference spheroid. */
/*     RE         I   Equatorial radius of the reference spheroid. */
/*     F          I   Flattening coefficient. */
/*     JACOBI     O   Matrix of partial derivatives. */

/* $ Detailed_Input */

/*     LONG       Geodetic longitude of point (radians). */

/*     LAT        Geodetic latitude  of point (radians). */

/*     ALT        Altitude of point above the reference spheroid. */

/*     RE         Equatorial radius of the reference spheroid. */

/*     F          Flattening coefficient = (RE-RP) / RE,  where RP is */
/*                the polar radius of the spheroid.  (More importantly */
/*                RP = RE*(1-F).) */

/* $ Detailed_Output */

/*     JACOBI     is the matrix of partial derivatives of the conversion */
/*                between geodetic and rectangular coordinates.  It */
/*                has the form */

/*                   .-                              -. */
/*                   |  DX/DLONG   DX/DLAT  DX/DALT   | */
/*                   |  DY/DLONG   DY/DLAT  DY/DALT   | */
/*                   |  DZ/DLONG   DZ/DLAT  DZ/DALT   | */
/*                   `-                              -' */

/*                evaluated at the input values of LONG, LAT and ALT. */

/*                The formulae for computing X, Y, and Z from */
/*                geodetic coordinates are given below. */

/*                   X = [ALT +          RE/G(LAT,F)]*COS(LONG)*COS(LAT) */
/*                   Y = [ALT +          RE/G(LAT,F)]*SIN(LONG)*COS(LAT) */
/*                   Z = [ALT + RE*(1-F)**2/G(LAT,F)]*          SIN(LAT) */

/*                where */

/*                   RE is the polar radius of the reference spheroid. */

/*                   F  is the flattening factor (the polar radius is */
/*                      obtained by multiplying the equatorial radius by */
/*                      1-F). */

/*                   G( LAT, F ) is given by */

/*                      sqrt ( cos(lat)**2 + (1-f)**2 * sin(lat)**2 ) */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the flattening coefficient is greater than or equal to */
/*        one, the error SPICE(VALUEOUTOFRANGE) is signaled. */

/*     2) If the equatorial radius is non-positive, the error */
/*        SPICE(BADRADIUS) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     It is often convenient to describe the motion of an object in */
/*     the geodetic coordinate system.  However, when performing */
/*     vector computations its hard to beat rectangular coordinates. */

/*     To transform states given with respect to geodetic coordinates */
/*     to states with respect to rectangular coordinates, one makes use */
/*     of the Jacobian of the transformation between the two systems. */

/*     Given a state in geodetic coordinates */

/*          ( long, lat, alt, dlong, dlat, dalt ) */

/*     the velocity in rectangular coordinates is given by the matrix */
/*     equation: */

/*                    t          |                                   t */
/*        (dx, dy, dz)   = JACOBI|              * (dlong, dlat, dalt) */
/*                               |(long,lat,alt) */


/*     This routine computes the matrix */

/*              | */
/*        JACOBI| */
/*              |(long,lat,alt) */

/* $ Examples */

/*     Suppose that one has a model that gives radius, longitude and */
/*     latitude as a function of time (long(t), lat(t), alt(t) ) for */
/*     which the derivatives ( dlong/dt, dlat/dt, dalt/dt ) are */
/*     computable. */

/*     To find the velocity of the object in bodyfixed rectangular */
/*     coordinates, one simply multiplies the Jacobian of the */
/*     transformation from geodetic to rectangular coordinates, */
/*     evaluated at (long(t), lat(t), alt(t) ), by the vector of */
/*     derivatives of the geodetic coordinates. */

/*     In code this looks like: */

/*        C */
/*        C     Load the derivatives of long, lat, and alt into the */
/*        C     geodetic velocity vector GEOV. */
/*        C */
/*              GEOV(1) = DLONG_DT ( T ) */
/*              GEOV(2) = DLAT_DT  ( T ) */
/*              GEOV(3) = DALT_DT  ( T ) */

/*        C */
/*        C     Determine the Jacobian of the transformation from */
/*        C     geodetic to rectangular coordinates at the geodetic */
/*        C     coordinates of time T. */
/*        C */
/*              CALL DRDGEO ( LONG(T), LAT(T), ALT(T), RE, F, JACOBI ) */

/*        C */
/*        C     Multiply the Jacobian on the right by the geodetic */
/*        C     velocity to obtain the rectangular velocity RECV. */
/*        C */
/*              CALL MXV ( JACOBI, GEOV, RECV ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 20-JUL-2001 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Jacobian of rectangular w.r.t. geodetic coordinates */

/* -& */
/* $ Revisions */

/*     None. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DRDGEO", (ftnlen)6);
    }

/*     If the flattening coefficient is greater than one, the polar */
/*     radius computed below is negative. If it's equal to one, the */
/*     polar radius is zero. Either case is a problem, so signal an */
/*     error and check out. */

    if (*f >= 1.) {
	setmsg_("Flattening coefficient was *.", (ftnlen)29);
	errdp_("*", f, (ftnlen)1);
	sigerr_("SPICE(VALUEOUTOFRANGE)", (ftnlen)22);
	chkout_("DRDGEO", (ftnlen)6);
	return 0;
    }
    if (*re <= 0.) {
	setmsg_("Equatorial Radius <= 0.0D0. RE = *", (ftnlen)34);
	errdp_("*", re, (ftnlen)1);
	sigerr_("SPICE(BADRADIUS)", (ftnlen)16);
	chkout_("DRDGEO", (ftnlen)6);
	return 0;
    }

/*     For the record, here is a derivation of the formulae for the */
/*     values of x, y and z as a function of longitude, latitude and */
/*     altitude. */

/*     First, let's take the case where the longitude is 0. Moreover, */
/*     lets assume that the length of the equatorial axis is a and */
/*     that the polar axis is b: */

/*        a = re */
/*        b = re * (1-f) */

/*     For any point on the spheroid where y is zero we know that there */
/*     is a unique q in the range (-Pi, Pi] such that */

/*        x = a cos(q) and z = b sin(q). */

/*     The normal to the surface at such a point is given by */

/*           cos(q)     sin(q) */
/*        ( ------- ,  ------- ) */
/*             a          b */

/*     The unit vector in the same direction is */

/*                 b cos(q)                         a sin(q) */
/*        ( --------------------------  ,  -------------------------- ) */
/*             ______________________         ______________________ */
/*            / 2   2        2   2           / 2   2        2   2 */
/*          \/ b cos (q)  + a sin (q)      \/ b cos (q)  + a sin (q) */


/*     The first component of this term is by definition equal to the */
/*     cosine of the geodetic latitude, thus */

/*                                ______________________ */
/*                               / 2   2        2   2 */
/*        b cos(q) = cos(lat)  \/ b cos (q)  + a sin (q) */


/*     This can be transformed to the equation */

/*                                ______________________________ */
/*                               /   2    2     2        2 */
/*        b cos(q) = cos(lat)  \/ ( b  - a  )cos (q)  + a */


/*     Squaring both sides and rearranging terms gives: */

/*         2   2         2         2   2     2        2    2 */
/*        b cos (q) + cos (lat) ( a - b ) cos (q) =  a  cos (lat) */

/*     Thus */
/*                           2    2 */
/*           2              a  cos (lat) */
/*        cos (q)  =  -------------------------- */
/*                     2    2         2   2 */
/*                    b  sin (lat) + a cos (lat) */



/*                             cos (lat) */
/*                 =  ------------------------------ */
/*                       _____________________________ */
/*                      /      2    2           2 */
/*                    \/  (b/a)  sin (lat) + cos (lat) */



/*                             cos (lat) */
/*                 =  --------------------------------- */
/*                       _____________________________ */
/*                      /      2    2           2 */
/*                    \/  (1-f)  sin (lat) + cos (lat) */



/*     From this one can also conclude that */


/*                           (1-f) sin (lat) */
/*        sin(q)   =  ---------------------------------- */
/*                        _____________________________ */
/*                       /      2    2           2 */
/*                     \/  (1-f)  sin (lat) + cos (lat) */



/*     Thus the point on the surface of the spheroid is given by */

/*                            re * cos (lat) */
/*        x_0      =  --------------------------------- */
/*                        _____________________________ */
/*                       /      2    2           2 */
/*                     \/  (1-f)  sin (lat) + cos (lat) */



/*                                  2 */
/*                        re * (1-f) sin (lat) */
/*        z_0      =  ---------------------------------- */
/*                        _____________________________ */
/*                       /      2    2           2 */
/*                     \/  (1-f)  sin (lat) + cos (lat) */


/*     Thus given a point with the same latitude but a non-zero */
/*     longitude, one can conclude that */

/*                         re * cos (long) *cos (lat) */
/*        x_0      =  --------------------------------- */
/*                        _____________________________ */
/*                       /      2    2           2 */
/*                     \/  (1-f)  sin (lat) + cos (lat) */



/*                         re * sin (long) cos (lat) */
/*        y_0      =  --------------------------------- */
/*                        _____________________________ */
/*                       /      2    2           2 */
/*                     \/  (1-f)  sin (lat) + cos (lat) */


/*                                    2 */
/*                          re * (1-f) sin (lat) */
/*        z_0      =  ---------------------------------- */
/*                        _____________________________ */
/*                       /      2    2           2 */
/*                     \/  (1-f)  sin (lat) + cos (lat) */


/*     The unit normal, n, at this point is simply */

/*        ( cos(long)cos(lat),  sin(long)cos(lat),  sin(lat) ) */


/*     Thus for a point at altitude alt, we simply add the vector */

/*        alt*n */

/*     to the vector ( x_0, y_0, z_0 ).  Hence we have */

/*        x = [ alt +          re/g(lat,f) ] * cos(long) * cos(lat) */
/*        y = [ alt +          re/g(lat,f) ] * sin(long) * cos(lat) */
/*        z = [ alt + re*(1-f)**2/g(lat,f) ] *             sin(lat) */


/*     We're going to need the sine and cosine of LAT and LONG many */
/*     times.  We'll just compute them once. */

    clat = cos(*lat);
    clon = cos(*long__);
    slat = sin(*lat);
    slon = sin(*long__);

/*     Referring to the G given in the header we have... */

    flat = 1. - *f;
    flat2 = flat * flat;
    g = sqrt(clat * clat + flat2 * slat * slat);
    g2 = g * g;
    dgdlat = (flat2 - 1.) * slat * clat / g;

/*     Now simply take the partial derivatives of the x,y,z w.r.t. */
/*     long,lat, alt. */

    jacobi[0] = -(*alt + *re / g) * slon * clat;
    jacobi[1] = (*alt + *re / g) * clon * clat;
    jacobi[2] = 0.;
    jacobi[3] = -(*re) * dgdlat / g2 * clon * clat - (*alt + *re / g) * clon *
	     slat;
    jacobi[4] = -(*re) * dgdlat / g2 * slon * clat - (*alt + *re / g) * slon *
	     slat;
    jacobi[5] = -flat2 * *re * dgdlat / g2 * slat + (*alt + flat2 * *re / g) *
	     clat;
    jacobi[6] = clon * clat;
    jacobi[7] = slon * clat;
    jacobi[8] = slat;
    chkout_("DRDGEO", (ftnlen)6);
    return 0;
} /* drdgeo_ */

