/* axisar.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      AXISAR ( Axis and angle to rotation ) */
/* Subroutine */ int axisar_(doublereal *axis, doublereal *angle, doublereal *
	r__)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *);
    integer i__;
    extern /* Subroutine */ int ident_(doublereal *);
    doublereal vtemp[3];
    extern /* Subroutine */ int vrotv_(doublereal *, doublereal *, doublereal 
	    *, doublereal *);

/* $ Abstract */

/*     Construct a rotation matrix that rotates vectors by a specified */
/*     angle about a specified axis. */

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

/*     MATRIX */
/*     ROTATION */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     AXIS       I   Rotation axis. */
/*     ANGLE      I   Rotation angle, in radians. */
/*     R          O   Rotation matrix corresponding to AXIS and ANGLE. */

/* $ Detailed_Input */

/*     AXIS, */
/*     ANGLE          are, respectively, a rotation axis and a rotation */
/*                    angle.  AXIS and ANGLE determine a coordinate */
/*                    transformation whose effect on any vector V is to */
/*                    rotate V by ANGLE radians about the vector AXIS. */

/* $ Detailed_Output */

/*     R              is a rotation matrix representing the coordinate */
/*                    transformation determined by AXIS and ANGLE:  for */
/*                    each vector V, R*V is the vector resulting from */
/*                    rotating V by ANGLE radians about AXIS. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)  If AXIS is the zero vector, the rotation generated is the */
/*         identity.  This is consistent with the specification of VROTV. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     AXISAR can be thought of as a partial inverse of RAXISA.  AXISAR */
/*     really is a `left inverse':  the code fragment */

/*        CALL RAXISA ( R,    AXIS,  ANGLE ) */
/*        CALL AXISAR ( AXIS, ANGLE, R     ) */

/*     preserves R, except for round-off error, as long as R is a */
/*     rotation matrix. */

/*     On the other hand, the code fragment */

/*        CALL AXISAR ( AXIS, ANGLE, R     ) */
/*        CALL RAXISA ( R,    AXIS,  ANGLE ) */

/*     preserves AXIS and ANGLE, except for round-off error, only if */
/*     ANGLE is in the range (0, pi).  So AXISAR is a right inverse */
/*     of RAXISA only over a limited domain. */

/* $ Examples */

/*     1)  A matrix that rotates vectors by pi/2 radians about the z-axis */
/*         can be found using the code fragment */

/*            AXIS(1) = 0.D0 */
/*            AXIS(2) = 0.D0 */
/*            AXIS(3) = 1.D0 */

/*            CALL AXISAR ( AXIS, HALFPI(), R ) */

/*         The returned matrix R will equal */

/*            +-               -+ */
/*            |  0    -1     0  | */
/*            |                 | */
/*            |  1     0     0  |. */
/*            |                 | */
/*            |  0     0     1  | */
/*            +-               -+ */


/*     2)  Linear interpolation between two rotation matrices: */

/*            Let R(t) be a time-varying rotation matrix; R could be */
/*            a C-matrix describing the orientation of a spacecraft */
/*            structure.  Given two points in time t1 and t2 at which */
/*            R(t) is known, and given a third time t3, where */

/*               t1  <  t3  <  t2, */

/*            we can estimate R(t3) by linear interpolation.  In other */
/*            words, we approximate the motion of R by pretending that */
/*            R rotates about a fixed axis at a uniform angular rate */
/*            during the time interval [t1, t2].  More specifically, we */
/*            assume that each column vector of R rotates in this */
/*            fashion.  This procedure will not work if R rotates through */
/*            an angle of pi radians or more during the time interval */
/*            [t1, t2]; an aliasing effect would occur in that case. */

/*            If we let */

/*               R1 = R(t1) */
/*               R2 = R(t2), and */

/*                           -1 */
/*               Q  = R2 * R1  , */

/*            then the rotation axis and angle of Q define the rotation */
/*            that each column of R(t) undergoes from time t1 to time */
/*            t2.  Since R(t) is orthogonal, we can find Q using the */
/*            transpose of R1.  We find the rotation axis and angle via */
/*            RAXISA. */

/*               CALL MXMT   ( R2,   R1,    Q      ) */
/*               CALL RAXISA ( Q,    AXIS,  ANGLE  ) */

/*            Find the fraction of the total rotation angle that R */
/*            rotates through in the time interval [t1, t3]. */

/*               FRAC = ( T3 - T1 )  /  ( T2 - T1 ) */

/*            Finally, find the rotation DELTA that R(t) undergoes */
/*            during the time interval [t1, t3], and apply that rotation */
/*            to R1, yielding R(t3), which we'll call R3. */

/*               CALL AXISAR ( AXIS,   FRAC * ANGLE,  DELTA  ) */
/*               CALL MXM    ( DELTA,  R1,            R3     ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 25-AUG-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in VROTV call. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 30-AUG-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     axis and angle to rotation */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.1.0, 25-AUG-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in VROTV call. */

/*        Identity matrix is now obtained from IDENT. */

/* -& */

/*     Local variables */


/*     First, set R equal to the identity. */

    ident_(r__);

/*     The matrix we want rotates EVERY vector by ANGLE about AXIS. */
/*     In particular, it does so to our basis vectors.  The columns */
/*     of R are the images of the basis vectors under this rotation. */

    for (i__ = 1; i__ <= 3; ++i__) {
	vrotv_(&r__[(i__1 = i__ * 3 - 3) < 9 && 0 <= i__1 ? i__1 : s_rnge(
		"r", i__1, "axisar_", (ftnlen)240)], axis, angle, vtemp);
	vequ_(vtemp, &r__[(i__1 = i__ * 3 - 3) < 9 && 0 <= i__1 ? i__1 : 
		s_rnge("r", i__1, "axisar_", (ftnlen)241)]);
    }
    return 0;
} /* axisar_ */

