/* drdlat.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure DRDLAT ( Derivative of rectangular w.r.t. latitudinal ) */
/* Subroutine */ int drdlat_(doublereal *r__, doublereal *long__, doublereal *
	lat, doublereal *jacobi)
{
    /* Builtin functions */
    double cos(doublereal), sin(doublereal);

/* $ Abstract */

/*     Compute the Jacobian of the transformation from latitudinal to */
/*     rectangular coordinates. */

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
/*     RADIUS     I   Distance of a point from the origin. */
/*     LONG       I   Angle of the point from the XZ plane in radians. */
/*     LAT        I   Angle of the point from the XY plane in radians. */
/*     JACOBI     O   Matrix of partial derivatives. */

/* $ Detailed_Input */

/*      RADIUS     Distance of a point from the origin. */

/*      LONG       Angle of the point from the XZ plane in radians. */
/*                 The angle increases in the counterclockwise sense */
/*                 about the +Z axis. */

/*      LAT        Angle of the point from the XY plane in radians. */
/*                 The angle increases in the direction of the +Z axis. */

/* $ Detailed_Output */

/*     JACOBI     is the matrix of partial derivatives of the conversion */
/*                between latitudinal and rectangular coordinates. It has */
/*                the form */

/*                    .-                                 -. */
/*                    |  DX/DR     DX/DLONG     DX/DLAT   | */
/*                    |                                   | */
/*                    |  DY/DR     DY/DLONG     DY/DLAT   | */
/*                    |                                   | */
/*                    |  DZ/DR     DZ/DLONG     DZ/DLAT   | */
/*                    `-                                 -' */

/*               evaluated at the input values of R, LONG and LAT. */
/*               Here X, Y, and Z are given by the familiar formulae */

/*                   X = R * COS(LONG) * COS(LAT) */
/*                   Y = R * SIN(LONG) * COS(LAT) */
/*                   Z = R *             SIN(LAT) */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     It is often convenient to describe the motion of an object */
/*     in latitudinal coordinates. It is also convenient to manipulate */
/*     vectors associated with the object in rectangular coordinates. */

/*     The transformation of a latitudinal state into an equivalent */
/*     rectangular state makes use of the Jacobian of the */
/*     transformation between the two systems. */

/*     Given a state in latitudinal coordinates, */

/*          ( r, long, lat, dr, dlong, dlat ) */

/*     the velocity in rectangular coordinates is given by the matrix */
/*     equation */
/*                    t          |                                t */
/*        (dx, dy, dz)   = JACOBI|             * (dr, dlong, dlat) */
/*                               |(r,long,lat) */

/*     This routine computes the matrix */

/*              | */
/*        JACOBI| */
/*              |(r,long,lat) */

/* $ Examples */

/*     Suppose you have a model that gives radius, longitude, and */
/*     latitude as functions of time (r(t), long(t), lat(t)), and */
/*     that the derivatives (dr/dt, dlong/dt, dlat/dt) are computable. */
/*     To find the velocity of the object in rectangular coordinates, */
/*     multiply the Jacobian of the transformation from latitudinal */
/*     to rectangular (evaluated at r(t), long(t), lat(t)) by the */
/*     vector of derivatives of the latitudinal coordinates. */

/*     This is illustrated by the following code fragment. */

/*        C */
/*        C     Load the derivatives of r, long and lat into the */
/*        C     latitudinal velocity vector LATV. */
/*        C */
/*              LATV(1) = DR_DT    ( T ) */
/*              LATV(2) = DLONG_DT ( T ) */
/*              LATV(3) = DLAT_DT  ( T ) */

/*        C */
/*        C     Determine the Jacobian of the transformation from */
/*        C     latitudinal to rectangular coordinates, using the */
/*        C     latitudinal coordinates at time T. */
/*        C */
/*              CALL DRDLAT ( R(T), LONG(T), LAT(T), JACOBI ) */

/*        C */
/*        C     Multiply the Jacobian by the latitudinal velocity to */
/*        C     obtain the rectangular velocity RECV. */
/*        C */
/*              CALL MXV ( JACOBI, LATV, RECV ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 19-JUL-2001 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Jacobian of rectangular w.r.t. latitudinal coordinates */

/* -& */
/* $ Revisions */

/*     None. */

/* -& */

/*     Local variables */


/*     Construct the matrix directly. */

    jacobi[0] = cos(*long__) * cos(*lat);
    jacobi[1] = sin(*long__) * cos(*lat);
    jacobi[2] = sin(*lat);
    jacobi[3] = -(*r__) * sin(*long__) * cos(*lat);
    jacobi[4] = *r__ * cos(*long__) * cos(*lat);
    jacobi[5] = 0.;
    jacobi[6] = -(*r__) * cos(*long__) * sin(*lat);
    jacobi[7] = -(*r__) * sin(*long__) * sin(*lat);
    jacobi[8] = *r__ * cos(*lat);
    return 0;
} /* drdlat_ */

