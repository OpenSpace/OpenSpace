/* srfrec.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static doublereal c_b5 = 1.;

/* $Procedure      SRFREC ( Surface to rectangular coordinates ) */
/* Subroutine */ int srfrec_(integer *body, doublereal *long__, doublereal *
	lat, doublereal *rectan)
{
    /* Initialized data */

    static doublereal origin[3] = { 0.,0.,0. };

    doublereal uvec[3];
    integer n;
    doublereal radii[3];
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    logical found;
    extern /* Subroutine */ int bodvcd_(integer *, char *, integer *, integer 
	    *, doublereal *, ftnlen), latrec_(doublereal *, doublereal *, 
	    doublereal *, doublereal *), chkout_(char *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int surfpt_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, logical *)
	    ;

/* $ Abstract */

/*     Convert planetocentric latitude and longitude of a surface */
/*     point on a specified body to rectangular coordinates. */

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

/*     KERNEL */
/*     NAIF_IDS */

/* $ Keywords */

/*     CONVERSION */
/*     COORDINATES */
/*     TRANSFORMATION */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     BODY       I   NAIF integer code of an extended body. */
/*     LONG       I   Longitude of point in radians. */
/*     LAT        I   Latitude of point in radians. */
/*     RECTAN     O   Rectangular coordinates of the point. */

/* $ Detailed_Input */

/*     BODY       is the NAIF integer code of an extended body on which */
/*                a surface point of interest is located. The body is */
/*                modeled as a triaxial ellipsoid. */

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

/*      RECTAN    The rectangular coordinates of the input surface */
/*                point.  Units are the same as those used to define the */
/*                radii of BODY.  Normally, these units are km. */

/* $ Parameters */

/*      None. */

/* $ Exceptions */

/*     1)  If radii for BODY are not found in the kernel pool, the error */
/*         will be diagnosed by routines called by this routine. */

/*     2)  If radii for BODY are invalid, the error will be diagnosed by */
/*         routines called by this routine.  The radii should be */
/*         positive. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine returns the rectangular coordinates of a surface */
/*     point on an extended body with known radii, where the location */
/*     of the surface point is specified in planetocentric latitudinal */
/*     coordinates. */

/*     Latitudinal coordinates are defined by a distance from a central */
/*     reference point, an angle from a reference meridian, and an angle */
/*     above the equator of a sphere centered at the central reference */
/*     point.  In this case, the distance from the central reference */
/*     point is not required as an input because the fact that the */
/*     point is on the body's surface allows one to deduce this quantity. */

/*     Below are two tables that demonstrate by example the relationship */
/*     between rectangular and latitudinal coordinates. */

/*     Listed in the first table (under R, LONG and LAT ) are */
/*     latitudinal coordinate triples that approximately represent */
/*     points whose rectangular coordinates are taken from the set */
/*     {-1, 0, 1}.  (Angular quantities are given in degrees.) */


/*          R         LONG       LAT           X(1)       X(2)     X(3) */
/*         --------------------------         -------------------------- */
/*         0.0000    0.0000    0.0000         0.0000     0.0000   0.0000 */
/*         1.0000    0.0000    0.0000         1.0000     0.0000   0.0000 */
/*         1.0000   90.0000    0.0000         0.0000     1.0000   0.0000 */
/*         1.0000    0.0000   90.0000         0.0000     0.0000   1.0000 */
/*         1.0000  180.0000    0.0000        -1.0000     0.0000   0.0000 */
/*         1.0000  -90.0000    0.0000         0.0000    -1.0000   0.0000 */
/*         1.0000    0.0000  -90.0000         0.0000     0.0000  -1.0000 */
/*         1.4142   45.0000    0.0000         1.0000     1.0000   0.0000 */
/*         1.4142    0.0000   45.0000         1.0000     0.0000   1.0000 */
/*         1.4142   90.0000   45.0000         0.0000     1.0000   1.0000 */
/*         1.7320   45.0000   35.2643         1.0000     1.0000   1.0000 */


/*     This routine is related to the SPICELIB routine LATREC, which */
/*     accepts a radius, longitude, and latitude as inputs and produces */
/*     equivalent rectangular coordinates as outputs. */

/* $ Examples */

/*     1)  Find the rectangular coordinates of the point */

/*            100 degrees planetocentric longitude */
/*            -35 degrees planetocentric latitude */

/*         on the Earth; then convert these coordinates back to */
/*         latitudinal coordinates.  We should be able to recover */
/*         our original longitude and latitude values. */


/*                  PROGRAM TEST_SRFREC */
/*                  IMPLICIT NONE */

/*            C */
/*            C     SPICELIB functions */
/*            C */
/*                  DOUBLE PRECISION      DPR */
/*                  DOUBLE PRECISION      RPD */

/*            C */
/*            C     Local variables */
/*            C */
/*                  DOUBLE PRECISION      LAT */
/*                  DOUBLE PRECISION      LONG */
/*                  DOUBLE PRECISION      X     ( 3 ) */
/*                  DOUBLE PRECISION      RADIUS */

/*            C */
/*            C     Load the kernel pool with a PCK file that contains */
/*            C     values for the radii of the Earth. */
/*            C */
/*                  CALL FURNSH ( 'pck00008.tpc' ) */

/*            C */
/*            C     Find X, the rectangular coordinates of the */
/*            C     surface point defined by LAT and LONG.  The */
/*            C     NAIF integer code for the Earth is 399. */
/*            C     (See the NAIF_IDS required reading file for */
/*            C     the complete set of codes.) */
/*            C */
/*                  LONG  =  100.D0 */
/*                  LAT   =  -35.D0 */

/*                  WRITE (*,*) 'Original latitudinal coordinates' */
/*                  WRITE (*,*) ' ' */
/*                  WRITE (*,*) 'Longitude  ', LONG */
/*                  WRITE (*,*) 'Latitude   ', LAT */

/*            C */
/*            C     Convert angles to radians on input to SRFREC. */
/*            C */
/*                  CALL SRFREC ( 399, LONG*RPD(), LAT*RPD(), X ) */

/*                  WRITE (*,*) ' ' */
/*                  WRITE (*,*) ' ' */
/*                  WRITE (*,*) 'Rectangular coordinates ' */
/*                  WRITE (*,*) ' ' */
/*                  WRITE (*,*) X */

/*            C */
/*            C     Now try to recover the original latitudinal */
/*            C     coordinates from the rectangular coordinates */
/*            C     found by SRFREC. */
/*            C */
/*                  CALL RECLAT ( X, RADIUS, LONG, LAT ) */

/*            C */
/*            C     Convert angles to degrees for display. */
/*            C */
/*                  WRITE (*,*) ' ' */
/*                  WRITE (*,*) ' ' */
/*                  WRITE (*,*) 'Latitudinal coordinates recovered '   // */
/*                 .            'from rectangular coordinates' */
/*                  WRITE (*,*) ' ' */
/*                  WRITE (*,*) 'Longitude (deg) ', LONG * DPR() */
/*                  WRITE (*,*) 'Latitude  (deg) ', LAT  * DPR() */
/*                  WRITE (*,*) 'Radius    (km)  ', RADIUS */

/*                  END */


/* $ Restrictions */

/*     1)  A NAIF text kernel containing the body radius definitions */
/*         required by this routine must be loaded into the kernel */
/*         pool prior to any calls to this routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     W.L. Taber     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 03-NOV-2005 (NJB) */

/*        Call to BODVAR was replaced with call to BODVCD. */

/*        Various header updates were made to clarify description */
/*        of routine's functionality.  Example program was updated */
/*        as well. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 03-SEP-1991 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     convert bodyfixed latitudinal coordinates to rectangular */
/*     convert surface latitudinal coordinates to rectangular */
/*     surface point latitudinal coordinates to rectangular */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SRFREC", (ftnlen)6);
    }

/*     Look up the body's radii. */

    bodvcd_(body, "RADII", &c__3, &n, radii, (ftnlen)5);

/*     Find the unit vector pointing from the body center to the */
/*     input surface point. */

    latrec_(&c_b5, long__, lat, uvec);

/*     Find out where the ray defined by this vector intersects the */
/*     surface.  This intercept is the point we're looking for. */

    surfpt_(origin, uvec, radii, &radii[1], &radii[2], rectan, &found);

/*     You can't miss the surface if you're riding a ray out from the */
/*     origin, so we don't check the FOUND flag. */

    chkout_("SRFREC", (ftnlen)6);
    return 0;
} /* srfrec_ */

