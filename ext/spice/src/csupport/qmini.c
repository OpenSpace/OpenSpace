/* qmini.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b2 = -1.;
static doublereal c_b3 = 1.;

/* $Procedure    QMINI ( Quaternion linear interpolation ) */
/* Subroutine */ int qmini_(doublereal *init, doublereal *final, doublereal *
	frac, doublereal *qintrp)
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double acos(doublereal), cos(doublereal), sin(doublereal);

    /* Local variables */
    doublereal vmag, axis[3];
    extern /* Subroutine */ int vscl_(doublereal *, doublereal *, doublereal *
	    );
    doublereal q[4], angle;
    extern /* Subroutine */ int unorm_(doublereal *, doublereal *, doublereal 
	    *);
    doublereal qscale[4];
    extern doublereal brcktd_(doublereal *, doublereal *, doublereal *);
    doublereal intang, instar[4];
    extern /* Subroutine */ int vminus_(doublereal *, doublereal *), qxq_(
	    doublereal *, doublereal *, doublereal *);

/* $ Abstract */

/*     Interpolate between two quaternions using a constant angular */
/*     rate. */

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

/*     ROTATIONS */

/* $ Keywords */

/*     MATH */
/*     QUATERNION */
/*     ROTATION */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     INIT       I   Initial quaternion representing a rotation. */
/*     FINAL      I   Final quaternion representing a rotation. */
/*     FRAC       I   Fraction of rotation from INIT to FINAL by which */
/*                    to interpolate. */
/*     QINTRP     O   Linearly interpolated quaternion. */

/* $ Detailed_Input */

/*     INIT, */
/*     FINAL, */
/*     FRAC           are, respectively, two unit quaternions between */
/*                    which to interpolate, and an interpolation */
/*                    fraction.  See the Detailed_Output and Particulars */
/*                    sections for details. */

/* $ Detailed_Output */

/*     QINTRP         is the quaternion resulting from linear */
/*                    interpolation between INIT and FINAL by the */
/*                    fraction FRAC.  By "linear interpolation" we mean */
/*                    the following: */

/*                       We view INIT and FINAL as quaternions */
/*                       representing two values of a time-varying */
/*                       rotation matrix R(t) that rotates at a constant */
/*                       angular velocity (that is, the row vectors of */
/*                       R(t) rotate with constant angular velocity). */
/*                       We can say that */

/*                          INIT  represents R(t0) */
/*                          FINAL represents R(t1) */

/*                       Equivalently, the SPICELIB routine Q2M maps */
/*                       INIT and FINAL to rotation matrices */
/*                       corresponding to R(t0) and R(t1) respectively. */

/*                       "Linear interpolation by the fraction FRAC" */
/*                       means that QINTRP represents the matrix */
/*                       R(t) evaluated at time */

/*                          t = t0   +   FRAC * (t1 - t0) */

/*                       and that the sign of QINTRP is such that */
/*                       QINTRP is closer to both INIT and FINAL */
/*                       than is -QINTRP. */


/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If either of INIT or FINAL is not a unit quaternion, the error */
/*         SPICE(NOTAROTATION) is signaled. */

/*     2)  This routine assumes that the quaternion QUOT defined by */

/*                               * */
/*            QUOT = FINAL * INIT */

/*         has rotation angle THETA radians, where */

/*            0  <  THETA  <  pi */
/*               - */

/*         Above the * superscript denotes quaternion conjugation. */

/*         The caller must test this condition on THETA; it is not */
/*         tested by this routine. A quick check may be performed by */
/*         verifying that */

/*            0  <  QUOT(0) */

/*         Note that this inequality is strict because rotations of */
/*         pi radians cannot be linearly interpolated so as to */
/*         produce a unique result. */

/*         This routine cannot distinguish between rotations of THETA */
/*         radians, where THETA is in the interval [0, pi), and */
/*         rotations of */

/*            THETA   +   2 * k * pi */

/*         radians, where k is any integer. These "large" rotations will */
/*         yield invalid results when interpolated.  You must ensure */
/*         that the inputs you provide to this routine will not be */
/*         subject to this sort of ambiguity.  If in fact you are */
/*         interpolating a time-dependent rotation with constant angular */
/*         velocity AV between times t0 and t1, you must ensure that */

/*            || AV ||  *  |t1 - t0|   <   pi. */

/*         Here we assume that the magnitude of AV is the angular rate */
/*         of the rotation in units of radians per second. */


/*     3)  When FRAC is outside of the interval [0, 1], the process */
/*         performed is "extrapolation", not interpolation.  Such */
/*         values of FRAC are permitted. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     In the discussion below, we assume that the conditions specified */
/*     in item (2) of the Exceptions section have been satisfied. */

/*     As we've said, we view INIT and FINAL as quaternions representing */
/*     two values of a time-varying rotation matrix R(t) that rotates at */
/*     a constant angular velocity; we define R(t), t0, and t1 so that */

/*        INIT  represents  R(t0) */
/*        FINAL represents  R(t1). */

/*     The output quaternion QINTRP represents R(t) evaluated at the */
/*     time */

/*        t0   +   FRAC * (t1 - t0). */

/*     How do we evaluate R at times between t0 and t1? Since the row */
/*     vectors of R are presumed to rotate with constant angular */
/*     velocity, we will first find the rotation axis of the quotient */
/*     rotation Q that maps the row vectors of R from their initial to */
/*     final position.  Since the rows of R are the columns of the */
/*     transpose of R, we can write: */

/*             T               T */
/*        R(t1)   =   Q * R(t0), */

/*     Since */

/*             T            T                  T */
/*        R(t1)   =  ( R(t1)  * R(t0) ) * R(t0) */


/*     we can find Q, as well as a rotation axis A and an angle THETA */
/*     in the range [0, pi] such that Q rotates vectors by THETA */
/*     radians about axis A. */

/*     We'll use the notation */

/*        [ x ] */
/*             N */

/*     to indicate a coordinate system rotation of x radians about the */
/*     vector N.  Having found A and THETA, we can write (note that */
/*     the sign of the rotation angle is negated because we're using */
/*     a coordinate system rotation) */

/*            T                  (t  - t0)                T */
/*        R(t)   =  [ - THETA *  ---------  ]    *   R(t0) */
/*                               (t1 - t0)   A */

/*     Thus R(t) and QINTRP are determined. */

/*     The input argument FRAC plays the role of the quotient */

/*        t  - t0 */
/*        ------- */
/*        t1 - t0 */

/*     shown above. */


/* $ Examples */

/*     1)  Suppose we want to interpolate between quaternions */
/*         Q1 and Q2 that give the orientation of a spacecraft structure */
/*         at times t1 and t2.  We wish to find an approximation of the */
/*         structure's orientation at the midpoint of the time interval */
/*         [t1, t2].  We assume that the angular velocity of the */
/*         structure equals the constant AV between times t1 and t2.  We */
/*         also assume that */

/*            || AV ||  *  (t2 - t1)   <   pi. */

/*         Then the code fragment */

/*            CALL QMINI ( Q1, Q2, 0.5D0, QINTRP, SCLDAV ) */

/*         produces the approximation we desire. */



/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 28-FEB-2008 (NJB) */

/*        The discussion of exception #2 was expanded. */

/* -    SPICELIB Version 1.0.0, 19-JUL-2005 (NJB) */

/* -& */
/* $ Index_Entries */

/*     linear interpolation between quaternions */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Use discovery check-in. */



/*     Find the conjugate INSTAR of the input quaternion INIT. */

    instar[0] = init[0];
    vminus_(&init[1], &instar[1]);

/*     Find the quotient quaternion Q that maps INIT to FINAL. */

    qxq_(final, instar, q);

/*     Extract the rotation angle from Q. Use arccosine for */
/*     speed, sacrificing some accuracy. */

    angle = acos(brcktd_(q, &c_b2, &c_b3)) * 2.;

/*     Create a quaternion QSCALE from the rotation axis of the quotient */
/*     and the scaled rotation angle. */

    intang = *frac * angle / 2.;
    qscale[0] = cos(intang);

/*     Get the unit vector parallel to the vector part of Q. */
/*     UNORM does exactly what we want here, because if the vector */
/*     part of Q is zero, the returned "unit" vector will be the */
/*     zero vector. */

    unorm_(&q[1], axis, &vmag);

/*     Form the vector part of QSCALE. */

    d__1 = sin(intang);
    vscl_(&d__1, axis, &qscale[1]);

/*     Apply QSCALE to INIT to produce the interpolated quaternion we */
/*     seek. */

    qxq_(qscale, init, qintrp);
    return 0;
} /* qmini_ */

