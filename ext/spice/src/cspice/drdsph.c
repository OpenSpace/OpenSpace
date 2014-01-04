/* drdsph.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      DRDSPH ( Derivative of rectangular w.r.t. spherical ) */
/* Subroutine */ int drdsph_(doublereal *r__, doublereal *colat, doublereal *
	long__, doublereal *jacobi)
{
    /* Builtin functions */
    double cos(doublereal), sin(doublereal);

    /* Local variables */
    doublereal clong, slong, ccolat, scolat;

/* $ Abstract */

/*     This routine computes the Jacobian of the transformation from */
/*     spherical to rectangular coordinates. */

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
/*     R          I   Distance of a point from the origin. */
/*     COLAT      I   Angle of the point from the positive Z-axis. */
/*     LONG       I   Angle of the point from the XY plane. */
/*     JACOBI     O   Matrix of partial derivatives. */

/* $ Detailed_Input */

/*     R          Distance of a point from the origin. */

/*     COLAT      Angle between the point and the positive z-axis, in */
/*                radians. */

/*     LONG       Angle of the point from the XZ plane in radians. */
/*                The angle increases in the counterclockwise sense */
/*                about the +Z axis. */

/* $ Detailed_Output */

/*     JACOBI     is the matrix of partial derivatives of the conversion */
/*                between spherical and rectangular coordinates, */
/*                evaluated at the input coordinates.  This matrix has */
/*                the form */

/*                    .-                                   -. */
/*                    |  DX/DR     DX/DCOLAT     DX/DLONG   | */
/*                    |                                     | */
/*                    |  DY/DR     DY/DCOLAT     DY/DLONG   | */
/*                    |                                     | */
/*                    |  DZ/DR     DZ/DCOLAT     DZ/DLONG   | */
/*                    `-                                   -' */

/*               evaluated at the input values of R, LONG and LAT. */
/*               Here X, Y, and Z are given by the familiar formulae */

/*                   X = R*COS(LONG)*SIN(COLAT) */
/*                   Y = R*SIN(LONG)*SIN(COLAT) */
/*                   Z = R*COS(COLAT) */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     It is often convenient to describe the motion of an object in */
/*     the spherical coordinate system.  However, when performing */
/*     vector computations its hard to beat rectangular coordinates. */

/*     To transform states given with respect to spherical coordinates */
/*     to states with respect to rectangular coordinates, one makes use */
/*     of the Jacobian of the transformation between the two systems. */

/*     Given a state in spherical coordinates */

/*          ( r, colat, long, dr, dcolat, dlong ) */

/*     the velocity in rectangular coordinates is given by the matrix */
/*     equation: */
/*                    t          |                                    t */
/*        (dx, dy, dz)   = JACOBI|              * (dr, dcolat, dlong ) */
/*                               |(r,colat,long) */

/*     This routine computes the matrix */

/*              | */
/*        JACOBI| */
/*              |(r,colat,long) */

/* $ Examples */

/*     Suppose that one has a model that gives the radius, colatitude */
/*     and longitude as a function of time (r(t), colat(t), long(t)), */
/*     for which the derivatives ( dr/dt, dcolat/dt, dlong/dt ) are */
/*     computable. */

/*     To find the velocity of the object in bodyfixed rectangular */
/*     coordinates, one simply multiplies the Jacobian of the */
/*     transformation from spherical to rectangular coordinates */
/*     (evaluated at r(t), colat(t), long(t) ) by the vector of */
/*     derivatives of the spherical coordinates. */

/*     In code this looks like: */

/*        C */
/*        C     Load the derivatives of r, colat, and long into the */
/*        C     spherical velocity vector SPHV. */
/*        C */
/*              SPHV(1) = DR_DT     ( T ) */
/*              SPHV(2) = DCOLAT_DT ( T ) */
/*              SPHV(3) = DLONG_DT  ( T ) */

/*        C */
/*        C     Determine the Jacobian of the transformation from */
/*        C     spherical to rectangular coordinates at the given */
/*        C     spherical coordinates at time T. */
/*        C */
/*              CALL DRDSPH ( R(T), COLAT(T), LONG(T), JACOBI ) */

/*        C */
/*        C     Multiply the Jacobian on the left times the spherical */
/*        C     velocity to obtain the rectangular velocity RECV. */
/*        C */
/*              CALL MXV ( JACOBI, SPHV, RECV ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 20-JUL-2001 (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     Jacobian of rectangular w.r.t. spherical coordinates */

/* -& */
/* $ Revisions */

/*     None. */

/* -& */

/*     Local parameters */


/*     Local variables */

    ccolat = cos(*colat);
    scolat = sin(*colat);
    clong = cos(*long__);
    slong = sin(*long__);
    jacobi[0] = clong * scolat;
    jacobi[1] = slong * scolat;
    jacobi[2] = ccolat;
    jacobi[3] = *r__ * clong * ccolat;
    jacobi[4] = *r__ * slong * ccolat;
    jacobi[5] = -(*r__) * scolat;
    jacobi[6] = -(*r__) * slong * scolat;
    jacobi[7] = *r__ * clong * scolat;
    jacobi[8] = 0.;
    return 0;
} /* drdsph_ */

