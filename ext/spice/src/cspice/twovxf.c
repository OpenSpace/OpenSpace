/* twovxf.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure TWOVXF ( Two states defining a frame transformation ) */
/* Subroutine */ int twovxf_(doublereal *axdef, integer *indexa, doublereal *
	plndef, integer *indexp, doublereal *xform)
{
    extern /* Subroutine */ int chkin_(char *, ftnlen), zztwovxf_(doublereal *
	    , integer *, doublereal *, integer *, doublereal *);
    doublereal xi[36]	/* was [6][6] */;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int invstm_(doublereal *, doublereal *);

/* $ Abstract */

/*     Find the state transformation from a base frame to the */
/*     right-handed frame defined by two state vectors:  one state */
/*     vector defining a specified axis and a second state vector */
/*     defining a specified coordinate plane. */

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

/*     AXES */
/*     FRAMES */
/*     MATRIX */
/*     TRANSFORMATION */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  ------------------------------------------------- */
/*     AXDEF      I   State defining a principal axis. */
/*     INDEXA     I   Principal axis number of AXDEF (X=1, Y=2, Z=3). */
/*     PLNDEF     I   State defining (with AXDEF) a principal plane. */
/*     INDEXP     I   Second axis number (with INDEXA) of principal */
/*                    plane. */
/*     XFORM      O   Output state transformation matrix. */

/* $ Detailed_Input */

/*     AXDEF      is a "generalized" state vector defining one of the */
/*                principal axes of a reference frame. This vector */
/*                consists of three components of a vector-valued */
/*                function of one independent variable t followed by */
/*                the derivatives of the components with respect to that */
/*                variable: */

/*                   ( a, b, c, da/dt, db/dt, dc/dt ) */

/*                This routine treats the input states as unitless, but */
/*                in most applications the input states represent */
/*                quantities that have associated units. The first three */
/*                components must have the same units, and the units of */
/*                the last three components must be compatible with */
/*                those of the first three:  if the first three */
/*                components of AXDEF */

/*                   ( a, b, c ) */

/*                have units U and t has units T, then the units of */
/*                AXDEF normally would be */

/*                   ( U, U, U, U/T, U/T, U/T ) */

/*                Note that the direction and angular velocity defined */
/*                by AXDEF are actually independent of U, so scaling */
/*                AXDEF doesn't affect the output of this routine. */

/*                AXDEF could represent position and velocity; it could */
/*                also represent velocity and acceleration.  AXDEF could */
/*                for example represent the velocity and acceleration of */
/*                a time-dependent position vector ( x(t), y(t), z(t) ), */
/*                in which case AXDEF would be defined by */

/*                   a     = dx/dt */
/*                   b     = dy/dt */
/*                   c     = dz/dt */

/*                            2      2 */
/*                   da/dt = d x / dt */

/*                            2      2 */
/*                   db/dt = d y / dt */

/*                            2      2 */
/*                   dc/dt = d z / dt */

/*                Below, we'll call the normalized (unit length) version */
/*                of */

/*                   ( a, b, c ) */

/*                the "direction" of AXDEF. */

/*                We call the frame relative to which AXDEF is specified */
/*                the "base frame."  The input state PLNDEF must be */
/*                specified relative to the same base frame. */


/*     INDEXA     is the index of the reference frame axis that is */
/*                parallel to the direction of AXDEF. */

/*                   Value of INDEXA             Axis */

/*                         1                      X */
/*                         2                      Y */
/*                         3                      Z */


/*     PLNDEF     is a state vector defining (with AXDEF) a principal */
/*                plane of the reference frame.  This vector consists */
/*                of three components followed by their derivatives with */
/*                respect to the independent variable t associated with */
/*                AXDEF, so PLNDEF is */

/*                   ( e, f, g, de/dt, df/dt, dg/dt ) */

/*                Below, we'll call the unitized version of */

/*                   ( e, f, g ) */

/*                the "direction" of PLNDEF. */

/*                The second axis of the principal plane containing the */
/*                direction vectors of AXDEF and PLNDEF is perpendicular */
/*                to the first axis and has positive dot product with */
/*                the direction vector of PLNDEF. */

/*                The first three components of PLNDEF must have the */
/*                same units, and the units of the last three components */
/*                must be compatible with those of the first three:  if */
/*                the first three components of PLNDEF */

/*                   ( e, f, g ) */

/*                have units U2 and t has units T, then the units of */
/*                PLNDEF normally would be */

/*                   ( U2, U2, U2, U2/T, U2/T, U2/T ) */

/*                ***For meaningful results, the angular velocities */
/*                   defined by AXDEF and PLNDEF must both have units of */
/*                   1/T.*** */

/*                As with AXDEF, scaling PLNDEF doesn't affect the */
/*                output of this routine. */

/*                AXDEF and PLNDEF must be specified relative to a */
/*                common reference frame, which we call the "base */
/*                frame." */


/*     INDEXP     is the index of  second axis of the principal frame */
/*                determined by AXDEF and PLNDEF.  The association of */
/*                integer values and axes is the same as for INDEXA. */

/* $ Detailed_Output */

/*     XFORM      is the 6x6 matrix that transforms states from the */
/*                frame relative to which AXDEF and PLNDEF are specified */
/*                (the "base frame") to the frame whose axes and */
/*                derivative are determined by AXDEF, PLNDEF, INDEXA and */
/*                INDEXP. */

/*                The matrix XFORM has the structure shown below: */

/*                    -            - */
/*                   |       :      | */
/*                   |   R   :  0   | */
/*                   | ......:......| */
/*                   |       :      | */
/*                   | dR_dt :  R   | */
/*                   |       :      | */
/*                    -            - */

/*                where R is a rotation matrix that is a function of */
/*                the independent variable associated with AXDEF and */
/*                PLNDEF, and where dR_dt is the derivative of R */
/*                with respect to that independent variable. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If INDEXA or INDEXP is not in the set {1,2,3} the error */
/*        SPICE(BADINDEX) will be signaled. */

/*     2) If INDEXA and INDEXP are the same the error */
/*        SPICE(UNDEFINEDFRAME) will be signaled. */

/*     3) If the cross product of the vectors AXDEF and PLNDEF is zero, */
/*        the error SPICE(DEPENDENTVECTORS) will be signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Given two linearly independent state vectors AXDEF and PLNDEF, */
/*     define vectors DIR1 and DIR2 by */

/*        DIR1 = ( AXDEF(1),   AXDEF(2),   AXDEF(3)  ) */
/*        DIR2 = ( PLNDEF(1),  PLNDEF(2),  PLNDEF(3) ) */

/*     Then there is a unique right-handed reference frame F having: */

/*        DIR1 lying along the INDEXA axis. */

/*        DIR2 lying in the INDEXA-INDEXP coordinate plane, such that */
/*        the dot product of DIR2 with the positive INDEXP axis is */
/*        positive. */

/*     This routine determines the 6x6 matrix that transforms states */
/*     from the base frame used to represent the input vectors to the */
/*     the frame F determined by AXDEF and PLNDEF.  Thus a state vector */

/*        S       = ( x, y, z, dx/dt, dy/dt, dz/dt ) */
/*         base */

/*     in the input reference frame will be transformed to */

/*        S       = XFORM * S */
/*         F                 base */

/*     in the frame F determined by AXDEF and PLNDEF. */

/* $ Examples */

/*     The time-dependent Sun-Canopus reference frame associated with a */
/*     spacecraft uses the spacecraft-sun state to define the Z axis and */
/*     the Canopus direction to define the X-Z plane. */

/*     Define an approximate "state vector" for Canopus using the */
/*     J2000-relative, unit direction vector toward Canopus at a */
/*     specified time ET (time is needed to compute proper motion) as */
/*     position and the zero vector as velocity.  Call this state vector */
/*     STCANO.  Let STSUN be the J2000-relative state of the sun */
/*     relative to the spacecraft at ET. */

/*     Then the matrix XFISC that transforms states from J2000 to the */
/*     Sun-Canopus reference frame at ET is returned by the call */

/*        CALL TWOVXF ( STSUN, 3, STCANO, 1, XFISC ) */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     W.M. Owen       (JPL) */
/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB) (WMO) (WLT) */

/* -& */
/* $ Index_Entries */

/*     define a state transformation matrix from two states */

/* -& */

/*     SPICELIB functions */


/*     Local Variables */


/*     Standard SPICE error handling */

    if (return_()) {
	return 0;
    }
    chkin_("TWOVXF", (ftnlen)6);

/*     Get the matrix XI that transforms states from the frame */
/*     defined by AXDEF and PLNDEF to their base frame. */

    zztwovxf_(axdef, indexa, plndef, indexp, xi);

/*     Invert XI. */

    invstm_(xi, xform);
    chkout_("TWOVXF", (ftnlen)6);
    return 0;
} /* twovxf_ */

