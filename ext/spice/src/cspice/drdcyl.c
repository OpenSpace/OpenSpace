/* drdcyl.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure DRDCYL (Derivative of rectangular w.r.t. cylindrical) */
/* Subroutine */ int drdcyl_(doublereal *r__, doublereal *long__, doublereal *
	z__, doublereal *jacobi)
{
    /* Builtin functions */
    double cos(doublereal), sin(doublereal);

/* $ Abstract */

/*     This routine computes the Jacobian of the transformation from */
/*     cylindrical to rectangular coordinates. */

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
/*     LONG       I   Angle of the point from the XZ plane in radians. */
/*     Z          I   Height of the point above the XY plane. */
/*     JACOBI     O   Matrix of partial derivatives. */

/* $ Detailed_Input */

/*     R          Distance of the point of interest from Z axis. */

/*     LONG       Cylindrical angle (in radians) of the point of */
/*                interest from the XZ plane.  The angle increases in */
/*                the counterclockwise sense about the +Z axis. */

/*     Z          Height of the point above XY plane. */

/* $ Detailed_Output */

/*     JACOBI     is the matrix of partial derivatives of the conversion */
/*                between cylindrical and rectangular coordinates.  It */
/*                has the form */

/*                   .-                                  -. */
/*                   |  dx/dr     dx/dlong       dx/dz    | */
/*                   |                                    | */
/*                   |  dy/dr     dy/dlong       dy/dz    | */
/*                   |                                    | */
/*                   |  dz/dr     dz/dlong       dz/dz    | */
/*                   `-                                  -' */

/*                evaluated at the input values of R, LONG and Z. */
/*                Here x,y, and z are given by the familiar formulae */

/*                   x = r*cos(long) */
/*                   y = r*sin(long) */
/*                   z = z */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     It is often convenient to describe the motion of an object in */
/*     the cylindrical coordinate system.  However, when performing */
/*     vector computations its hard to beat rectangular coordinates. */

/*     To transform states given with respect to cylindrical coordinates */
/*     to states with respect to rectangular coordinates, one uses */
/*     the Jacobian of the transformation between the two systems. */

/*     Given a state in cylindrical coordinates */

/*        ( r, long, z, dr, dlong, dz ) */

/*     the velocity in rectangular coordinates is given by the matrix */
/*     equation: */
/*                    t          |                           t */
/*        (dx, dy, dz)   = JACOBI|          * (dr, dlong, dz) */
/*                               |(r,long,z) */

/*     This routine computes the matrix */

/*              | */
/*        JACOBI| */
/*              |(r,long,z) */

/* $ Examples */

/*     Suppose that one has a model that gives radius, longitude and */
/*     height as a function of time (r(t), long(t), z(t)) for */
/*     which the derivatives ( dr/dt, dlong/dt, dz/dt ) are computable. */

/*     To find the corresponing velocity in bodyfixed rectangular */
/*     coordinates, one simply multiplies the Jacobian of the */
/*     transformation from cylindrical to rectangular coordinates */
/*     (evaluated at r(t), long(t), z(t) ) by the vector of derivatives */
/*     of the cylindrical coordinates. */

/*     In code this looks like: */

/*        C */
/*        C     Load the derivatives of r, long, and z into the */
/*        C     cylindrical velocity vector SPHV. */
/*        C */
/*              CYLV(1) = DR_DT    ( T ) */
/*              CYLV(2) = DLONG_DT ( T ) */
/*              CYLV(3) = DZ_DT    ( T ) */

/*        C */
/*        C     Determine the Jacobian of the transformation from */
/*        C     cylindrical to rectangular coordinates at the */
/*        C     given cylindrical coordinates at time T. */
/*        C */
/*              CALL DRDCYL ( R(T), LONG(T), Z(T), JACOBI ) */

/*        C */
/*        C     Multiply the Jacobian on the left by the cylindrical */
/*        C     velocity to obtain the rectangular velocity RECV. */
/*        C */
/*              CALL MXV ( JACOBI, CYLV, RECV ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 12-NOV-2013 (EDW) */

/*        Trivial edit to header, deleted trailing whitespace */
/*        on lines. */

/* -    SPICELIB Version 1.0.0, 19-JUL-2001 (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     Jacobian of rectangular w.r.t. cylindrical coordinates */

/* -& */
/* $ Revisions */

/*     None. */

/* -& */

/*     Local parameters */

    jacobi[0] = cos(*long__);
    jacobi[1] = sin(*long__);
    jacobi[2] = 0.;
    jacobi[3] = -sin(*long__) * *r__;
    jacobi[4] = cos(*long__) * *r__;
    jacobi[5] = 0.;
    jacobi[6] = 0.;
    jacobi[7] = 0.;
    jacobi[8] = 1.;
    return 0;
} /* drdcyl_ */

