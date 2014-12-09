/* linrot_m.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b3 = 1e-8;

/* $Procedure    LINROT_M ( Linear interpolation between rotations ) */
/* Subroutine */ int linrot_m__(doublereal *init, doublereal *final, 
	doublereal *frac, doublereal *r__, doublereal *scldav)
{
    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    doublereal axis[3];
    extern /* Subroutine */ int vscl_(doublereal *, doublereal *, doublereal *
	    ), mtxm_(doublereal *, doublereal *, doublereal *), mxmt_(
	    doublereal *, doublereal *, doublereal *);
    doublereal m[9]	/* was [3][3] */, angle, delta[9]	/* was [3][3] 
	    */;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern logical isrot_(doublereal *, doublereal *, doublereal *);
    extern /* Subroutine */ int raxisa_(doublereal *, doublereal *, 
	    doublereal *), axisar_(doublereal *, doublereal *, doublereal *), 
	    sigerr_(char *, ftnlen), chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Interpolate between two rotations using a constant angular rate. */

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

/*     MOSPICE */
/*     MATRIX */
/*     ROTATION */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     INIT       I   Initial rotation. */
/*     FINAL      I   Final rotation. */
/*     FRAC       I   Fraction of rotation from INIT to FINAL by which */
/*                    to interpolate. */
/*     R          O   Linearly interpolated rotation. */
/*     SCLDAV     O   Scaled angular velocity of rotation. */

/* $ Detailed_Input */

/*     INIT, */
/*     FINAL, */
/*     FRAC           are, respectively, two rotation matrices between */
/*                    which to interpolate, and an interpolation */
/*                    fraction.  See the $Detailed_Output and */
/*                    $Particulars sections for details. */

/* $ Detailed_Output */

/*     R              is the matrix resulting from linear interpolation */
/*                    between INIT and FINAL by the fraction FRAC.  By */
/*                    `linear interpolation' we mean the following: */

/*                       We view INIT and FINAL as two values of a */
/*                       time-varying rotation matrix R(t) that rotates */
/*                       at a constant angular velocity (that is, the */
/*                       row vectors of R(t) rotate with constant angular */
/*                       velocity).  We can say that */

/*                          INIT   =  R(t0) */
/*                          FINAL  =  R(t1). */

/*                       `Linear interpolation by the fraction FRAC' */
/*                       means that we evalute R(t) at time */

/*                          t0   +   FRAC * (t1 - t0). */


/*     SCLDAV         is a scaled version of the constant angular */
/*                    velocity vector used for interpolation.  When */
/*                    SCLDAV is divided by the scale factor */

/*                       t1 - t0, */

/*                    the result is the constant angular velocity */
/*                    assumed for the rows of R(t) in order to perform */
/*                    linear interpolation. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If either of INIT or FINAL is not a rotation matrix, the error */
/*         SPICE(NOTAROTATION) is signalled. */

/*     2)  This routine assumes that the rotation that maps INIT to FINAL */
/*         has a rotation angle THETA radians, where */

/*            0  <  THETA  <  pi. */
/*               _ */

/*         This routine cannot distinguish between rotations of THETA */
/*         radians, where THETA is in the interval [0, pi), and */
/*         rotations of */

/*            THETA   +   2 * k * pi */

/*         radians, where k is any integer.  These `large' rotations will */
/*         yield invalid results when interpolated.  You must ensure that */
/*         the inputs you provide to this routine will not be subject to */
/*         this sort of ambiguity.  If in fact you are interpolating the */
/*         position of a rotating matrix with constant angular velocity */
/*         AV between times t0 and t1, you must ensure that */

/*            || AV ||  *  |t1 - t0|   <   pi. */

/*         Here we assume that the magnitude of AV is the angular rate */
/*         of the rotating matrix in units of radians per second. */


/*     3)  When FRAC is outside of the interval [0, 1], the process */
/*         performed is `extrapolation', not interpolation.  Such */
/*         values of FRAC are permitted. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     In the discussion below, we assume that the conditions specified */
/*     in item (2) of $ Exceptions have been satisfied. */

/*     As we've said, we view INIT and FINAL as two values of a */
/*     time-varying rotation matrix R(t) that rotates at a constant */
/*     angular velocity; we define R(t), t0, and t1 so that */

/*        INIT   =  R(t0) */
/*        FINAL  =  R(t1). */

/*     The output matrix R is R(t) evaluated at the time */

/*        t0   +   FRAC * (t1 - t0). */

/*     How do we evaluate R at times between t0 and t1?  Since the */
/*     column vectors of R are presumed to rotate with constant */
/*     angular velocity, we will first find the rotation axis of */
/*     the matrix M that maps the row vectors of R from their initial */
/*     to final position.  Since the rows of R are the columns of */
/*     the transpose of R, we can write: */

/*             T               T */
/*        R(t1)   =   M * R(t0), */

/*     or */
/*             T              T */
/*        FINAL   =   M * INIT */

/*     Since */
/*             T            T                T */
/*        FINAL   =  ( FINAL  * INIT ) * INIT */

/*     we can find M, as well as a rotation axis A and an angle THETA */
/*     in the range [0, pi] such that M rotates vectors by THETA */
/*     radians about axis A. */

/*     We'll use the notation */

/*        [ x ] */
/*             N */

/*     to indicate a coordinate system rotation of x radians about the */
/*     vector N.  Having found A and THETA, we can write */

/*            T                 (t  - t0)                 T */
/*        R(t)   =  [  THETA *  ---------  ]     *   R(t0) */
/*                              (t1 - t0)    A */

/*     Thus R(t) is determined. */

/*     The input argument FRAC plays the role of the quotient */

/*        t  - t0 */
/*        ------- */
/*        t1 - t0 */

/*     shown above. */

/*     SCLDAV is a vector parallel to the rotation axis of M and */
/*     having length equal to the rotation angle of M (which is in */
/*     the range [0, pi]). */

/* $ Examples */

/*     1)  Suppose we want to interpolate between two rotation matrices */
/*         R1 and R2 that give the orientation of a spacecraft structure */
/*         at times t1 and t2.  We wish to find an approximation of the */
/*         structure's orientation at the midpoint of the time interval */
/*         [t1, t2].  We assume that the angular velocity of the */
/*         structure equals the constant AV between times t1 and t2.  We */
/*         also assume that */

/*            || AV ||  *  (t2 - t1)   <   pi. */

/*         Then the code fragment */

/*            CALL LINROT_M ( R1, R2, 0.5D0, R, SCLDAV ) */

/*         produces the approximation we desire. */


/*     2)  Suppose R1 is the identity and R2 is */

/*            [ pi/2 ] . */
/*                    3 */

/*         Then the code fragment */

/*            CALL LINROT_M ( R1, R2, FRAC, R, SCLDAV ) */

/*         returns SCLDAV as the vector */

/*            ( 0, 0, pi/2 ). */


/*     3)  As long as R1 and R2 are not equal, the code fragment */

/*            CALL LINROT_M ( R1,     R2,            FRAC,  R,  SCLDAV ) */
/*            CALL AXISAR   ( SCLDAV, VNORM(SCLDAV),  M                ) */
/*            CALL MXMT     ( R1,     M,             R2                ) */

/*         should leave R2 unchanged, except for round-off error. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    MOSPICE Version 1.0.0, 17-APR-1992 (NJB) */

/* -& */
/* $ Index_Entries */

/*     linear interpolation between rotations */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     NTOL and DTOL are tolerances used for determining whether INIT */
/*     and FINAL are rotation matrices.  NTOL is bound on the deviation */
/*     of the norms of the columns of a matrix from 1, and DTOL is a */
/*     bound on the deviation of the determinant of a matrix from 1. */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("LINROT_M", (ftnlen)8);
    }

/*     INIT and FINAL must both be rotation matrices. */

    if (! isrot_(init, &c_b3, &c_b3)) {
	setmsg_("INIT is not a rotation.", (ftnlen)23);
	sigerr_("SPICE(NOTAROTATION)", (ftnlen)19);
	chkout_("LINROT_M", (ftnlen)8);
	return 0;
    } else if (! isrot_(final, &c_b3, &c_b3)) {
	setmsg_("FINAL is not a rotation.", (ftnlen)24);
	sigerr_("SPICE(NOTAROTATION)", (ftnlen)19);
	chkout_("LINROT_M", (ftnlen)8);
	return 0;
    }

/*     Little to do, really.  Define M by */

/*             T              T */
/*        FINAL   =   M * INIT */

/*     Since */
/*             T            T                T */
/*        FINAL   =  ( FINAL  * INIT ) * INIT */

/*     and all of the matrices here are non-singular, we have */

/*                          T */
/*        M       =  ( FINAL  * INIT ) */


/*     Find an axis and angle for the rotation M, and interpolate the */
/*     angle.  Form the interpolated rotation DELTA. */

    mtxm_(final, init, m);
    raxisa_(m, axis, &angle);
    d__1 = angle * *frac;
    axisar_(axis, &d__1, delta);

/*     Since */

/*         T                 T */
/*        R   =  DELTA * INIT */

/*     we can find R directly by computing */

/*                           T */
/*        R   =  INIT * DELTA */

    mxmt_(init, delta, r__);

/*     Finding the `constant' angular velocity vector is easy to do. */

    vscl_(&angle, axis, scldav);
    chkout_("LINROT_M", (ftnlen)8);
    return 0;
} /* linrot_m__ */

