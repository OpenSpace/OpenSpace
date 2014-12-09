/* latcyl.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      LATCYL ( Latitudinal to cylindrical coordinates ) */
/* Subroutine */ int latcyl_(doublereal *radius, doublereal *long__, 
	doublereal *lat, doublereal *r__, doublereal *longc, doublereal *z__)
{
    /* Builtin functions */
    double cos(doublereal), sin(doublereal);

    /* Local variables */
    doublereal rh, zz;

/* $ Abstract */

/*     Convert from latitudinal coordinates to cylindrical coordinates. */

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
/*      R          O   Distance of the point from the Z axis. */
/*      LONGC      O   Angle of the point from the XZ plane in radians. */
/*      Z          O   Height of the point above the XY plane. */

/* $ Detailed_Input */

/*      RADIUS     Distance of a point from the origin. */

/*      LONG       Angle of the point from the XZ plane in radians. */

/*      LAT        Angle of the point from the XY plane in radians. */

/* $ Detailed_Output */

/*      R          Distance of the point from the Z axis. */

/*      LONGC      Angle of the point from the XZ plane in radians. */

/*      Z          Height of the point above the XY plane. */

/* $ Parameters */

/*      None. */

/* $ Particulars */

/*      This routine returns the cylindrical coordinates of a point */
/*      whose position is input in latitudinal coordinates. */

/*      Latitudinal coordinates are defined by a distance from a central */
/*      reference point, an angle from a reference meridian, and an angle */
/*      above the equator of a sphere centered at the central reference */
/*      point. */

/* $ Examples */

/*     Other than the obvious conversion between coordinate systems */
/*     this routine could be used to obtain the axial projection */
/*     from a sphere to a cylinder about the z-axis that contains */
/*     the equator of the sphere.  The following code fragment */
/*     illustrates this idea. */

/*           CALL LATCYL ( RADIUS, LONG, LAT, R, LONG, Z ) */
/*           R = RADIUS */

/*     R, LONG, and Z now contain the coordinates of the projected */
/*     point. Such a projection is valuable because it preserves the */
/*     areas between regions on the sphere and their projections to the */
/*     cylinder. */


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

/*     latitudinal to cylindrical coordinates */

/* -& */
/* $ Revisions */

/* -     Beta Version 1.0.1, 1-Feb-1989 (WLT) */

/*      Example section of header upgraded. */

/* -& */

/*     Local variables */


/*     Convert to cylindrical, storing in temporary variables */

    rh = *radius * cos(*lat);
    zz = *radius * sin(*lat);

/*     Move the results to output variables. */

    *longc = *long__;
    *r__ = rh;
    *z__ = zz;

    return 0;
} /* latcyl_ */

