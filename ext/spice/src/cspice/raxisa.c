/* raxisa.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      RAXISA ( Rotation axis of a matrix ) */
/* Subroutine */ int raxisa_(doublereal *matrix, doublereal *axis, doublereal 
	*angle)
{
    /* Builtin functions */
    double atan2(doublereal, doublereal);

    /* Local variables */
    extern /* Subroutine */ int vhat_(doublereal *, doublereal *);
    doublereal q[4];
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern doublereal vnorm_(doublereal *);
    extern logical vzero_(doublereal *), failed_(void);
    extern doublereal pi_(void);
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int m2q_(doublereal *, doublereal *);

/* $ Abstract */

/*     Compute the axis of the rotation given by an input matrix */
/*     and the angle of the rotation about that axis. */

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

/*     ROTATION */

/* $ Keywords */

/*     ANGLE,  MATRIX,  ROTATION */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     MATRIX     I   3x3 rotation matrix in double precision. */
/*     AXIS       O   Axis of the rotation. */
/*     ANGLE      O   Angle through which the rotation is performed. */

/* $ Detailed_Input */

/*     MATRIX     is a 3x3 rotation matrix in double precision. */

/* $ Detailed_Output */

/*     AXIS       is a unit vector pointing along the axis of the */
/*                rotation.  In other words, AXIS is a unit eigenvector */
/*                of the input matrix, corresponding to the eigenvalue */
/*                1.  If the input matrix is the identity matrix, AXIS */
/*                will be the vector (0, 0, 1). If the input rotation is */
/*                a rotation by PI radians, both AXIS and -AXIS may be */
/*                regarded as the axis of the rotation. */

/*     ANGLE      is the angle between V and MATRIX*V for any non-zero */
/*                vector V orthogonal to AXIS.  Angle is given in */
/*                radians.  The angle returned will be in the range from */
/*                0 to PI. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the input matrix is not a rotation matrix (where a fairly */
/*        loose tolerance is used to check this) a routine in the */
/*        call tree of this routine will signal an error indicating */
/*        the problem. */

/*     2) If the input matrix is the identity matrix, this routine */
/*        returns an angle of 0.0, and an axis of ( 0.0, 0.0, 1.0 ). */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Every rotation matrix has an axis A such any vector, V, parallel */
/*     to that axis satisfies the equation */

/*        V = MATRIX * V */

/*     This routine returns a unit vector AXIS parallel to the axis of */
/*     the input rotation matrix.  Moreover for any vector W orthogonal */
/*     to the axis of the rotation */

/*        AXIS  and  W x MATRIX*W */

/*        (where "x" denotes the cross product operation) */

/*     will be positive scalar multiples of one another (at least to */
/*     within the ability to make such computations with double */
/*     precision arithmetic, and under the assumption that the MATRIX */
/*     does not represent a rotation by zero or Pi radians). */

/*     The angle returned will be the angle between W and MATRIX*W for */
/*     any vector orthogonal to AXIS. */

/*     If the input matrix is a rotation by 0 or PI radians some choice */
/*     must be made for the AXIS returned.  In the case of a rotation by */
/*     0 radians, AXIS is along the positive z-axis. In the case of a */
/*     rotation by 180 degrees, two choices are */

/* $ Examples */

/*     This routine can be used to numerically approximate the */
/*     instantaneous angular velocity vector of a rotating object. */

/*     Suppose that R(t) is the rotation matrix whose columns represent */
/*     the inertial pointing vectors of the bodyfixed axes of an object */
/*     at time t. */

/*     Then the angular velocity vector points along the vector given */
/*     by: */
/*                             T */
/*         limit  AXIS( R(t+h)R ) */
/*         h-->0 */

/*     And the magnitude of the angular velocity at time t is given by: */

/*                             T */
/*         d ANGLE ( R(t+h)R(t) ) */
/*         ----------------------   at   h = 0 */
/*         dh */

/*     Thus to approximate the angular velocity vector the following */
/*     code fragment will do */

/*            Load t      into the double precision variable T */
/*            Load h      into the double precision variable H */
/*            Load R(t+h) into the 3 by 3 double precision array RTH */
/*            Load R(t)   into the 3 by 3 double precision array RT */
/*               . */
/*               . */
/*               . */
/*            compute the infinitesimal rotation R(t+h)R(t)**T */

/*            CALL MXMT   ( RTH,    RT,   INFROT ) */

/*            compute the AXIS and ANGLE of the infinitesimal rotation */

/*            CALL RAXISA ( INFROT, AXIS, ANGLE  ) */

/*            scale axis to get the angular velocity vector */

/*            CALL VSCL   ( ANGLE/H, AXIS, ANGVEL ) */


/* $ Restrictions */

/*     1) If the input matrix is not a rotation matrix but is close */
/*        enough to pass the tests this routine performs on it, no error */
/*        will be signaled, but the results may have poor accuracy. */

/*     2) The input matrix is taken to be an object that acts on */
/*        (rotates) vectors---it is not regarded as a coordinate */
/*        transformation.  To find the axis and angle of a coordinate */
/*        transformation, input the transpose of that matrix to this */
/*        routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */
/*     F.S. Turner     (JPL) */
/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.1.2, 02-JAN-2008 (EDW) */

/*        Minor edit to the ANGLE declaration strictly */
/*        identifying the constant as a double. */

/*        From: */

/*           ANGLE = 2.0 * DATAN2( VNORM(Q(1)), Q(0) ) */

/*        To: */

/*           ANGLE = 2.D0 * DATAN2( VNORM(Q(1)), Q(0) ) */

/* -    SPICELIB Version 2.1.1, 05-JAN-2005 (NJB) */

/*        Minor edits and formatting changes were made. */

/* -    SPICELIB Version 2.1.0, 30-MAY-2002 (FST) */

/*        This routine now participates in error handling properly. */

/* -    SPICELIB Version 2.0.0, 19-SEP-1999 (WLT) */

/*        The routine was re-written so as to avoid the numerical */
/*        instabilities present in the previous implementation for */
/*        rotations very near zero or 180 degrees. */

/* -    SPICELIB Version 1.1.0, 29-FEB-1996 (KRG) */

/*        The declaration for the SPICELIB function PI is now */
/*        preceded by an EXTERNAL statement declaring PI to be an */
/*        external function. This removes a conflict with any */
/*        compilers that have a PI intrinsic function. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     axis and angle of a rotation matrix */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.1.0, 30-MAY-2002 (FST) */

/*        Calls to CHKIN and CHKOUT in the standard SPICE error */
/*        handling style were added.  Versions prior to 2.0.0 */
/*        were error free, however the call to M2Q introduced in */
/*        version 2.0.0 signals an error if the input matrix is */
/*        not sufficiently close to a rotation. */

/*        Additionally, FAILED is now checked after the call to */
/*        M2Q.  This prevents garbage from being placed into the */
/*        output arguments. */

/* -    SPICELIB Version 2.0.0, 21-SEP-1999 (WLT) */

/*        The routine was re-written so as to avoid the numerical */
/*        instabilities present in the previous implementation for */
/*        rotations very near zero or 180 degrees. */

/* -    SPICELIB Version 1.1.0, 29-FEB-1996 (KRG) */

/*        The declaration for the SPICELIB function PI is now */
/*        preceded by an EXTERNAL statement declaring PI to be an */
/*        external function. This removes a conflict with any */
/*        compilers that have a PI intrinsic function. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -    Beta Version 1.1.0, 3-JAN-1989 (WLT) */

/*     Even though the routine stipulates that the input matrix */
/*     should be a rotation matrix, it might not be.  As a result */
/*     we could have negative numbers showing up where we need */
/*     to take square roots.  This fix simply bounds these values */
/*     so that Fortran intrinsics always get reasonable input values. */

/*     Add and example to the header. */
/* -& */

/*     SPICELIB functions */


/*     Local Variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("RAXISA", (ftnlen)6);
    }

/*     Construct the quaternion corresponding to the input rotation */
/*     matrix */

    m2q_(matrix, q);

/*     Check FAILED and return if an error has occurred. */

    if (failed_()) {
	chkout_("RAXISA", (ftnlen)6);
	return 0;
    }

/*     The quaternion we've just constructed is of the form: */

/*         cos(ANGLE/2) + sin(ANGLE/2) * AXIS */

/*     We take a few precautions to handle the case of an identity */
/*     rotation. */

    if (vzero_(&q[1])) {
	*angle = 0.;
	axis[0] = 0.;
	axis[1] = 0.;
	axis[2] = 1.;
    } else if (q[0] == 0.) {
	*angle = pi_();
	axis[0] = q[1];
	axis[1] = q[2];
	axis[2] = q[3];
    } else {
	vhat_(&q[1], axis);
	*angle = atan2(vnorm_(&q[1]), q[0]) * 2.;
    }
    chkout_("RAXISA", (ftnlen)6);
    return 0;
} /* raxisa_ */

