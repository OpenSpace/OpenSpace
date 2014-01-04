/* dcyldr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure DCYLDR (Derivative of cylindrical w.r.t. rectangular ) */
/* Subroutine */ int dcyldr_(doublereal *x, doublereal *y, doublereal *z__, 
	doublereal *jacobi)
{
    doublereal long__, r__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), vpack_(doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    doublereal injacb[9]	/* was [3][3] */, rectan[3];
    extern /* Subroutine */ int reccyl_(doublereal *, doublereal *, 
	    doublereal *, doublereal *), drdcyl_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    doublereal zz;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int invort_(doublereal *, doublereal *);

/* $ Abstract */

/*     This routine computes the Jacobian of the transformation from */
/*     rectangular to cylindrical coordinates. */

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
/*     X          I   X-coordinate of point. */
/*     Y          I   Y-coordinate of point. */
/*     Z          I   Z-coordinate of point. */
/*     JACOBI     O   Matrix of partial derivatives. */

/* $ Detailed_Input */

/*     X, */
/*     Y, */
/*     Z          are the rectangular coordinates of the point at */
/*                which the Jacobian of the map from rectangular */
/*                to cylindrical coordinates is desired. */

/* $ Detailed_Output */

/*     JACOBI     is the matrix of partial derivatives of the conversion */
/*                between rectangular and cylindrical coordinates.  It */
/*                has the form */

/*                   .-                               -. */
/*                   |  dr   /dx   dr   /dy  dr   /dz  | */
/*                   |  dlong/dx   dlong/dy  dlong/dz  | */
/*                   |  dz   /dx   dz   /dy  dz   /dz  | */
/*                   `-                               -' */

/*                evaluated at the input values of X, Y, and Z. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the input point is on the Z-axis (X and Y = 0), the */
/*        Jacobian is undefined.  The error SPICE(POINTONZAXIS) */
/*        will be signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     When performing vector calculations with velocities it is */
/*     usually most convenient to work in rectangular coordinates. */
/*     However, once the vector manipulations have been performed, */
/*     it is often desirable to convert the rectangular representations */
/*     into cylindrical coordinates to gain insights about phenomena */
/*     in this coordinate frame. */

/*     To transform rectangular velocities to derivatives of coordinates */
/*     in a cylindrical system, one uses the Jacobian of the */
/*     transformation between the two systems. */

/*     Given a state in rectangular coordinates */

/*        ( x, y, z, dx, dy, dz ) */

/*     the velocity in cylindrical coordinates is given by the matrix */
/*     equation: */

/*                       t          |                     t */
/*        (dr, dlong, dz)   = JACOBI|       * (dx, dy, dz) */
/*                                  |(x,y,z) */

/*     This routine computes the matrix */

/*              | */
/*        JACOBI| */
/*              |(x,y,z) */

/* $ Examples */

/*     Suppose one is given the bodyfixed rectangular state of an object */
/*     (x(t), y(t), z(t), dx(t), dy(t), dz(t)) as a function of time t. */

/*     To find the derivatives of the coordinates of the object in */
/*     bodyfixed cylindrical coordinates, one simply multiplies the */
/*     Jacobian of the transformation from rectangular to cylindrical */
/*     coordinates (evaluated at x(t), y(t), z(t)) by the rectangular */
/*     velocity vector of the object at time t. */

/*     In code this looks like: */

/*        C */
/*        C     Load the rectangular velocity vector vector RECV. */
/*        C */
/*              RECV(1) = DX_DT ( T ) */
/*              RECV(2) = DY_DT ( T ) */
/*              RECV(3) = DZ_DT ( T ) */

/*        C */
/*        C     Determine the Jacobian of the transformation from */
/*        C     rectangular to cylindrical coordinates at the */
/*        C     given rectangular coordinates at time T. */
/*        C */
/*              CALL DCYLDR ( X(T), Y(T), Z(T), JACOBI ) */

/*        C */
/*        C     Multiply the Jacobian on the right by the rectangular */
/*        C     velocity to obtain the cylindrical coordinate derivatives */
/*        C     CYLV. */
/*        C */
/*              CALL MXV ( JACOBI, RECV, CYLV ) */

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

/*     Jacobian of cylindrical w.r.t. rectangular coordinates */

/* -& */
/* $ Revisions */

/*     None. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DCYLDR", (ftnlen)6);
    }

/*     There is a singularity of the Jacobian for points on the z-axis. */

    if (*x == 0. && *y == 0.) {
	setmsg_("The Jacobian of the transformation from rectangular to cyli"
		"ndrical coordinates is not defined for points on the z-axis.",
		 (ftnlen)119);
	sigerr_("SPICE(POINTONZAXIS)", (ftnlen)19);
	chkout_("DCYLDR", (ftnlen)6);
	return 0;
    }

/*     We will get the Jacobian of rectangular to cylindrical by */
/*     implicit differentiation. */

/*     First move the X,Y and Z coordinates into a vector. */

    vpack_(x, y, z__, rectan);

/*     Convert from rectangular to cylindrical coordinates. */

    reccyl_(rectan, &r__, &long__, &zz);

/*     Get the Jacobian from cylindrical to rectangular coordinates at */
/*     R, LONG, Z. */

    drdcyl_(&r__, &long__, &zz, injacb);

/*     Now invert INJACB to get the Jacobian from rectangular to */
/*     cylindrical coordinates. */

    invort_(injacb, jacobi);
    chkout_("DCYLDR", (ftnlen)6);
    return 0;
} /* dcyldr_ */

