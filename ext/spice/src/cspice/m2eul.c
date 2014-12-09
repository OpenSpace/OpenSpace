/* m2eul.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b15 = .1;
static integer c__9 = 9;

/* $Procedure      M2EUL ( Matrix to Euler angles ) */
/* Subroutine */ int m2eul_(doublereal *r__, integer *axis3, integer *axis2, 
	integer *axis1, doublereal *angle3, doublereal *angle2, doublereal *
	angle1)
{
    /* Initialized data */

    static integer next[3] = { 2,3,1 };

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    double acos(doublereal), atan2(doublereal, doublereal), asin(doublereal);

    /* Local variables */
    doublereal sign;
    extern /* Subroutine */ int vhat_(doublereal *, doublereal *), mtxm_(
	    doublereal *, doublereal *, doublereal *);
    integer c__, i__;
    logical degen;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern logical isrot_(doublereal *, doublereal *, doublereal *);
    doublereal change[9]	/* was [3][3] */;
    extern /* Subroutine */ int cleard_(integer *, doublereal *), sigerr_(
	    char *, ftnlen), chkout_(char *, ftnlen);
    doublereal tmpmat[9]	/* was [3][3] */;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    extern logical return_(void);
    doublereal tmprot[9]	/* was [3][3] */;
    extern /* Subroutine */ int mxm_(doublereal *, doublereal *, doublereal *)
	    ;

/* $ Abstract */

/*     Factor a rotation matrix as a product of three rotations about */
/*     specified coordinate axes. */

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

/*     ANGLE */
/*     MATRIX */
/*     ROTATION */
/*     TRANSFORMATION */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     R          I   A rotation matrix to be factored. */
/*     AXIS3, */
/*     AXIS2, */
/*     AXIS1      I   Numbers of third, second, and first rotation axes. */
/*     ANGLE3, */
/*     ANGLE2, */
/*     ANGLE1     O   Third, second, and first Euler angles, in radians. */

/* $ Detailed_Input */

/*     R              is a 3x3 rotation matrix that is to be factored as */
/*                    a product of three rotations about a specified */
/*                    coordinate axes.  The angles of these rotations are */
/*                    called `Euler angles'. */

/*     AXIS3, */
/*     AXIS2, */
/*     AXIS1          are the indices of the rotation axes of the */
/*                    `factor' rotations, whose product is R.  R is */
/*                    factored as */

/*                       R = [ ANGLE3 ]     [ ANGLE2 ]     [ ANGLE1 ]   . */
/*                                    AXIS3          AXIS2         AXIS1 */

/*                    The axis numbers must belong to the set {1, 2, 3}. */
/*                    The second axis number MUST differ from the first */
/*                    and third axis numbers. */

/*                    See the $ Particulars section below for details */
/*                    concerning this notation. */

/* $ Detailed_Output */

/*     ANGLE3, */
/*     ANGLE2, */
/*     ANGLE1         are the Euler angles corresponding to the matrix */
/*                    R and the axes specified by AXIS3, AXIS2, and */
/*                    AXIS1.  These angles satisfy the equality */

/*                       R = [ ANGLE3 ]     [ ANGLE2 ]     [ ANGLE1 ] */
/*                                   AXIS3          AXIS2          AXIS1 */


/*                    See the $ Particulars section below for details */
/*                    concerning this notation. */

/*                    The range of ANGLE3 and ANGLE1 is (-pi, pi]. */

/*                    The range of ANGLE2 depends on the exact set of */
/*                    axes used for the factorization.  For */
/*                    factorizations in which the first and third axes */
/*                    are the same, */

/*                       R = [r]  [s]  [t] , */
/*                              a    b    a */

/*                    the range of ANGLE2 is [0, pi]. */


/*                    For factorizations in which the first and third */
/*                    axes are different, */

/*                       R = [r]  [s]  [t] , */
/*                              a    b    c */

/*                    the range of ANGLE2 is [-pi/2, pi/2]. */

/*                    For rotations such that ANGLE3 and ANGLE1 are not */
/*                    uniquely determined, ANGLE3 will always be set to */
/*                    zero; ANGLE1 is then uniquely determined. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)   If any of AXIS3, AXIS2, or AXIS1 do not have values in */

/*             { 1, 2, 3 }, */

/*          then the error SPICE(BADAXISNUMBERS) is signaled. */

/*     2)   An arbitrary rotation matrix cannot be expressed using */
/*          a sequence of Euler angles unless the second rotation axis */
/*          differs from the other two.  If AXIS2 is equal to AXIS3 or */
/*          AXIS1, then then error SPICE(BADAXISNUMBERS) is signaled. */

/*     3)   If the input matrix R is not a rotation matrix, the error */
/*          SPICE(NOTAROTATION) is signaled. */

/*     4)   If ANGLE3 and ANGLE1 are not uniquely determined, ANGLE3 */
/*          is set to zero, and ANGLE1 is determined. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     A word about notation:  the symbol */

/*        [ x ] */
/*             i */

/*     indicates a coordinate system rotation of x radians about the */
/*     ith coordinate axis.  To be specific, the symbol */

/*        [ x ] */
/*             1 */

/*     indicates a coordinate system rotation of x radians about the */
/*     first, or x-, axis; the corresponding matrix is */

/*        +-                    -+ */
/*        |  1      0       0    | */
/*        |                      | */
/*        |  0    cos(x)  sin(x) |. */
/*        |                      | */
/*        |  0   -sin(x)  cos(x) | */
/*        +-                    -+ */

/*     Remember, this is a COORDINATE SYSTEM rotation by x radians; this */
/*     matrix, when applied to a vector, rotates the vector by -x */
/*     radians, not x radians.  Applying the matrix to a vector yields */
/*     the vector's representation relative to the rotated coordinate */
/*     system. */

/*     The analogous rotation about the second, or y-, axis is */
/*     represented by */

/*        [ x ] */
/*             2 */

/*     which symbolizes the matrix */

/*        +-                    -+ */
/*        | cos(x)   0   -sin(x) | */
/*        |                      | */
/*        |  0       1      0    |, */
/*        |                      | */
/*        | sin(x)   0    cos(x) | */
/*        +-                    -+ */

/*     and the analogous rotation about the third, or z-, axis is */
/*     represented by */

/*        [ x ] */
/*             3 */

/*     which symbolizes the matrix */

/*        +-                    -+ */
/*        |  cos(x)  sin(x)   0  | */
/*        |                      | */
/*        | -sin(x)  cos(x)   0  |. */
/*        |                      | */
/*        |  0        0       1  | */
/*        +-                    -+ */


/*     The input matrix is assumed to be the product of three */
/*     rotation matrices, each one of the form */

/*        +-                    -+ */
/*        |  1      0       0    | */
/*        |                      | */
/*        |  0    cos(r)  sin(r) |     (rotation of r radians about the */
/*        |                      |      x-axis), */
/*        |  0   -sin(r)  cos(r) | */
/*        +-                    -+ */


/*        +-                    -+ */
/*        | cos(s)   0   -sin(s) | */
/*        |                      | */
/*        |  0       1      0    |     (rotation of s radians about the */
/*        |                      |      y-axis), */
/*        | sin(s)   0    cos(s) | */
/*        +-                    -+ */

/*     or */

/*        +-                    -+ */
/*        |  cos(t)  sin(t)   0  | */
/*        |                      | */
/*        | -sin(t)  cos(t)   0  |     (rotation of t radians about the */
/*        |                      |      z-axis), */
/*        |  0        0       1  | */
/*        +-                    -+ */

/*     where the second rotation axis is not equal to the first or */
/*     third.  Any rotation matrix can be factored as a sequence of */
/*     three such rotations, provided that this last criterion is met. */

/*     This routine is related to the SPICELIB routine EUL2M, which */
/*     produces a rotation matrix, given a sequence of Euler angles. */
/*     This routine is a `right inverse' of EUL2M:  the sequence of */
/*     calls */

/*        CALL M2EUL ( R,  AXIS3,   AXIS2,   AXIS1, */
/*       .                 ANGLE3,  ANGLE2,  ANGLE1     ) */

/*        CALL EUL2M (     ANGLE3,  ANGLE2,  ANGLE1, */
/*       .                 AXIS3,   AXIS2,   AXIS1,   R ) */

/*     preserves R, except for round-off error. */


/*     On the other hand, the sequence of calls */

/*        CALL EUL2M (     ANGLE3,  ANGLE2,  ANGLE1, */
/*       .                 AXIS3,   AXIS2,   AXIS1,   R ) */

/*        CALL M2EUL ( R,  AXIS3,   AXIS2,   AXIS1, */
/*       .                 ANGLE3,  ANGLE2,  ANGLE1     ) */


/*     preserve ANGLE3, ANGLE2, and ANGLE1 only if these angles start */
/*     out in the ranges that M2EUL's outputs are restricted to. */

/* $ Examples */

/*     1)  Conversion of instrument pointing from a matrix representation */
/*         to Euler angles: */

/*         Suppose we want to find camera pointing in alpha, delta, and */
/*         kappa, given the inertial-to-camera coordinate transformation */


/*    +-                                                               -+ */
/*    |  0.49127379678135830  0.50872620321864170  0.70699908539882417  | */
/*    |                                                                 | */
/*    | -0.50872620321864193 -0.49127379678135802  0.70699908539882428  | */
/*    |                                                                 | */
/*    |  0.70699908539882406 -0.70699908539882439  0.01745240643728360  | */
/*    +-                                                               -+ */


/*         We want to find angles alpha, delta, kappa such that */

/*            TICAM  =  [ kappa ]  [ pi/2 - delta ]  [ pi/2 + alpha ] . */
/*                               3                 1                 3 */

/*         We can use the following small program to do this computation: */


/*            PROGRAM EX1 */
/*            IMPLICIT NONE */

/*            DOUBLE PRECISION      DPR */
/*            DOUBLE PRECISION      HALFPI */
/*            DOUBLE PRECISION      TWOPI */

/*            DOUBLE PRECISION      ALPHA */
/*            DOUBLE PRECISION      ANG1 */
/*            DOUBLE PRECISION      ANG2 */
/*            DOUBLE PRECISION      DELTA */
/*            DOUBLE PRECISION      KAPPA */
/*            DOUBLE PRECISION      TICAM  ( 3, 3 ) */


/*            DATA TICAM /  0.49127379678135830D0, */
/*           .             -0.50872620321864193D0, */
/*           .              0.70699908539882406D0, */
/*           .              0.50872620321864170D0, */
/*           .             -0.49127379678135802D0, */
/*           .             -0.70699908539882439D0, */
/*           .              0.70699908539882417D0, */
/*           .              0.70699908539882428D0, */
/*           .              0.01745240643728360D0  / */


/*            CALL M2EUL ( TICAM, 3, 1, 3, KAPPA, ANG2, ANG1 ) */

/*            DELTA = HALFPI() - ANG2 */
/*            ALPHA = ANG1     - HALFPI() */

/*            IF ( KAPPA .LT. 0.D0 ) THEN */
/*               KAPPA = KAPPA + TWOPI() */
/*            END IF */

/*            IF ( ALPHA .LT. 0.D0 ) THEN */
/*               ALPHA = ALPHA + TWOPI() */
/*            END IF */

/*            WRITE (*,'(1X,A,F24.14)') 'Alpha (deg): ', DPR() * ALPHA */
/*            WRITE (*,'(1X,A,F24.14)') 'Delta (deg): ', DPR() * DELTA */
/*            WRITE (*,'(1X,A,F24.14)') 'Kappa (deg): ', DPR() * KAPPA */

/*            END */


/*         The program's output should be something like */

/*            Alpha (deg):       315.00000000000000 */
/*            Delta (deg):         1.00000000000000 */
/*            Kappa (deg):        45.00000000000000 */

/*         possibly formatted a little differently, or degraded slightly */
/*         by round-off. */


/*     2)  Conversion of instrument pointing angles from a non-J2000, */
/*         not necessarily inertial frame to J2000-relative RA, Dec, */
/*         and Twist. */

/*         Suppose that we have pointing for some instrument expressed as */

/*            [ gamma ]  [ beta ]  [ alpha ] */
/*                     3         2          3 */

/*         with respect to some coordinate system S.  For example, S */
/*         could be a spacecraft-fixed system. */

/*         We will suppose that the transformation from J2000 */
/*         coordinates to system S coordinates is given by the rotation */
/*         matrix J2S. */

/*         The rows of J2S are the unit basis vectors of system S, given */
/*         in J2000 coordinates. */

/*         We want to express the pointing with respect to the J2000 */
/*         system as the sequence of rotations */

/*            [ kappa ]  [ pi/2 - delta ]  [ pi/2 + alpha ] . */
/*                     3                 1                 3 */

/*         First, we use subroutine EUL2M to form the transformation */
/*         from system S to instrument coordinates S2INST. */

/*            CALL EUL2M ( GAMMA, BETA, ALPHA, 3, 2, 3, S2INST ) */

/*         Next, we form the transformation from J2000 to instrument */
/*         coordinates J2INST. */

/*            CALL MXM ( S2INST, J2S, J2INST ) */

/*         Finally, we express J2INST using the desired Euler angles, as */
/*         in the first example: */

/*            CALL M2EUL ( J2INST, 3, 1, 3, TWIST, ANG2, ANG3 ) */

/*            RA   =  ANG3 - HALFPI() */
/*            DEC  =  HALFPI() - ANG2 */

/*         If we wish to make sure that RA, DEC, and TWIST are in */
/*         the ranges [0, 2pi), [-pi/2, pi/2], and [0, 2pi) */
/*         respectively, we may add the code */

/*            IF ( RA .LT. 0.D0 ) THEN */
/*               RA = RA + TWOPI() */
/*            END IF */

/*            IF ( TWIST .LT. 0.D0 ) THEN */
/*               TWIST = TWIST + TWOPI() */
/*            END IF */

/*         Note that DEC is already in the correct range, since ANG2 */
/*         is in the range [0, pi] when the first and third input axes */
/*         are equal. */

/*         Now RA, DEC, and TWIST express the instrument pointing */
/*         as RA, Dec, and Twist, relative to the J2000 system. */

/*         A warning note:  more than one definition of RA, Dec, and */
/*         Twist is extant.  Before using this example in an application, */
/*         check that the definition given here is consistent with that */
/*         used in your application. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.1, 21-DEC-2006 (NJB) */

/*        Error corrected in header example:  input matrix */
/*        previously did not match shown outputs.  Offending */
/*        example now includes complete program. */

/* -    SPICELIB Version 1.2.0, 15-OCT-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in MXM and MTXM calls.  A short error message cited in */
/*        the Exceptions section of the header failed to match */
/*        the actual short message used; this has been corrected. */

/* -    SPICELIB Version 1.1.2, 13-OCT-2004 (NJB) */

/*        Fixed header typo. */

/* -    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.1.0, 02-NOV-1990 (NJB) */

/*        Header upgraded to describe notation in more detail.  Argument */
/*        names were changed to describe the use of the arguments more */
/*        accurately.  No change in functionality was made; the operation */
/*        of the routine is identical to that of the previous version. */

/* -    SPICELIB Version 1.0.0, 03-SEP-1990 (NJB) */

/* -& */
/* $ Index_Entries */

/*     matrix to euler angles */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.2.0, 26-AUG-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in MXM and MTXM calls.  A short error message cited in */
/*        the Exceptions section of the header  failed to match */
/*        the actual short message used; this has been corrected. */

/* -    SPICELIB Version 1.1.0, 02-NOV-1990 (NJB) */

/*        Argument names were changed to describe the use of the */
/*        arguments more accurately.  The axis and angle numbers */
/*        now decrease, rather than increase, from left to right. */
/*        The current names reflect the order of operator application */
/*        when the Euler angle rotations are applied to a vector:  the */
/*        rightmost matrix */

/*           [ ANGLE1 ] */
/*                     AXIS1 */

/*        is applied to the vector first, followed by */

/*           [ ANGLE2 ] */
/*                     AXIS2 */

/*        and then */

/*           [ ANGLE3 ] */
/*                     AXIS3 */

/*        Previously, the names reflected the order in which the Euler */
/*        angle matrices appear on the page, from left to right.  This */
/*        naming convention was found to be a bit obtuse by a various */
/*        users. */

/*        No change in functionality was made; the operation of the */
/*        routine is identical to that of the previous version. */

/*        Also, the header was upgraded to describe the notation in more */
/*        detail.  The symbol */

/*           [ x ] */
/*                i */

/*        is explained at mind-numbing length.  An example was added */
/*        that shows a specific set of inputs and the resulting output */
/*        matrix. */

/*        The angle sequence notation was changed to be consistent with */
/*        Rotations required reading. */

/*          1-2-3  and  a-b-c */

/*        have been changed to */

/*          3-2-1  and  c-b-a. */

/*       Also, one `)' was changed to a `}'. */

/*       The phrase `first and third' was changed to `first or third' */
/*       in the $ Particulars section, where the criterion for the */
/*       existence of an Euler angle factorization is stated. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     NTOL and DETOL are used to determine whether R is a rotation */
/*     matrix. */

/*     NTOL is the tolerance for the norms of the columns of R. */

/*     DTOL is the tolerance for the determinant of a matrix whose */
/*     columns are the unitized columns of R. */



/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("M2EUL", (ftnlen)5);
    }

/*     The first order of business is to screen out the goofy cases. */

/*     Make sure the axis numbers are all right:  They must belong to */
/*     the set {1, 2, 3}... */

    if (*axis3 < 1 || *axis3 > 3 || (*axis2 < 1 || *axis2 > 3) || (*axis1 < 1 
	    || *axis1 > 3)) {
	setmsg_("Axis numbers are #,  #,  #. ", (ftnlen)28);
	errint_("#", axis3, (ftnlen)1);
	errint_("#", axis2, (ftnlen)1);
	errint_("#", axis1, (ftnlen)1);
	sigerr_("SPICE(BADAXISNUMBERS)", (ftnlen)21);
	chkout_("M2EUL", (ftnlen)5);
	return 0;

/*     ...and the second axis number must differ from its neighbors. */

    } else if (*axis3 == *axis2 || *axis1 == *axis2) {
	setmsg_("Middle axis matches neighbor: # # #.", (ftnlen)36);
	errint_("#", axis3, (ftnlen)1);
	errint_("#", axis2, (ftnlen)1);
	errint_("#", axis1, (ftnlen)1);
	sigerr_("SPICE(BADAXISNUMBERS)", (ftnlen)21);
	chkout_("M2EUL", (ftnlen)5);
	return 0;

/*     R must be a rotation matrix, or we may as well forget it. */

    } else if (! isrot_(r__, &c_b15, &c_b15)) {
	setmsg_("Input matrix is not a rotation.", (ftnlen)31);
	sigerr_("SPICE(NOTAROTATION)", (ftnlen)19);
	chkout_("M2EUL", (ftnlen)5);
	return 0;
    }

/*     AXIS3, AXIS2, AXIS1 and R have passed their tests at this */
/*     point.  We take the liberty of working with TMPROT, a version */
/*     of R that has unitized columns. */

    for (i__ = 1; i__ <= 3; ++i__) {
	vhat_(&r__[(i__1 = i__ * 3 - 3) < 9 && 0 <= i__1 ? i__1 : s_rnge(
		"r", i__1, "m2eul_", (ftnlen)667)], &tmprot[(i__2 = i__ * 3 - 
		3) < 9 && 0 <= i__2 ? i__2 : s_rnge("tmprot", i__2, "m2eul_", 
		(ftnlen)667)]);
    }

/*     We now proceed to recover the promised Euler angles from */
/*     TMPROT. */

/*        The ideas behind our method are explained in excruciating */
/*        detail in the ROTATION required reading, so we'll be terse. */
/*        Nonetheless, a word of explanation is in order. */

/*        The sequence of rotation axes used for the factorization */
/*        belongs to one of two categories:  a-b-a or c-b-a.  We */
/*        wish to handle each of these cases in one shot, rather than */
/*        using different formulas for each sequence to recover the */
/*        Euler angles. */

/*        What we're going to do is use the Euler angle formula for the */
/*        3-1-3 factorization for all of the a-b-a sequences, and the */
/*        formula for the 3-2-1 factorization for all of the c-b-a */
/*        sequences. */

/*        How can we get away with this?  The Euler angle formulas for */
/*        each factorization are different! */

/*        Our trick is to apply a change-of-basis transformation to the */
/*        input matrix R.  For the a-b-a factorizations, we choose a new */
/*        basis such that a rotation of ANGLE3 radians about the basis */
/*        vector indexed by AXIS3 becomes a rotation of ANGLE3 radians */
/*        about the third coordinate axis, and such that a rotation of */
/*        ANGLE2 radians about the basis vector indexed by AXIS2 becomes */
/*        a rotation of ANGLE2 radians about the first coordinate axis. */
/*        So R can be factored as a 3-1-3 rotation relative to the new */
/*        basis, and the Euler angles we obtain are the exact ones we */
/*        require. */

/*        The c-b-a factorizations can be handled in an analogous */
/*        fashion.  We transform R to a basis where the original axis */
/*        sequence becomes a 3-2-1 sequence.  In some cases, the angles */
/*        we obtain will be the negatives of the angles we require.  This */
/*        will happen if and only if the ordered basis (here the e's are */
/*        the standard basis vectors) */

/*            { e        e        e      } */
/*               AXIS3    AXIS2    AXIS1 */

/*        is not right-handed.  An easy test for this condition is that */
/*        AXIS2 is not the successor of AXIS3, where the ordering is */
/*        cyclic. */

    if (*axis3 == *axis1) {

/*        The axis order is a-b-a.  We're going to find a matrix CHANGE */
/*        such that */

/*                 T */
/*           CHANGE  R  CHANGE */

/*        gives us R in the a useful basis, that is, a basis in which */
/*        our original a-b-a rotation is a 3-1-3 rotation, but where the */
/*        rotation angles are unchanged. To achieve this pleasant */
/*        simplification, we set column 3 of CHANGE to to e(AXIS3), */
/*        column 1 of CHANGE to e(AXIS2), and column 2 of CHANGE to */

/*          (+/-) e(C), */

/*        (C is the remaining index) depending on whether */
/*        AXIS3-AXIS2-C is a right-handed sequence of axes:  if it */
/*        is, the sign is positive.  (Here e(1), e(2), e(3) are the */
/*        standard basis vectors.) */

/*        Determine the sign of our third basis vector, so that we can */
/*        ensure that our new basis is right-handed.  The variable NEXT */
/*        is just a little mapping that takes 1 to 2, 2 to 3, and 3 to */
/*        1. */

	if (*axis2 == next[(i__1 = *axis3 - 1) < 3 && 0 <= i__1 ? i__1 : 
		s_rnge("next", i__1, "m2eul_", (ftnlen)746)]) {
	    sign = 1.;
	} else {
	    sign = -1.;
	}

/*        Since the axis indices sum to 6, */

	c__ = 6 - *axis3 - *axis2;

/*        Set up the entries of CHANGE: */

	cleard_(&c__9, change);
	change[(i__1 = *axis3 + 5) < 9 && 0 <= i__1 ? i__1 : s_rnge("change", 
		i__1, "m2eul_", (ftnlen)762)] = 1.;
	change[(i__1 = *axis2 - 1) < 9 && 0 <= i__1 ? i__1 : s_rnge("change", 
		i__1, "m2eul_", (ftnlen)763)] = 1.;
	change[(i__1 = c__ + 2) < 9 && 0 <= i__1 ? i__1 : s_rnge("change", 
		i__1, "m2eul_", (ftnlen)764)] = sign * 1.;

/*        Transform TMPROT. */

	mxm_(tmprot, change, tmpmat);
	mtxm_(change, tmpmat, tmprot);

/*        Now we're ready to find the Euler angles, using a */
/*        3-1-3 factorization.  In general, the matrix product */

/*           [ a1 ]   [ a2 ]   [ a3 ] */
/*                 3        1        3 */

/*        has the form */

/*     +-                                                              -+ */
/*     |         cos(a1)cos(a3)          cos(a1)sin(a3)  sin(a1)sin(a2) | */
/*     | -sin(a1)cos(a2)sin(a3)  +sin(a1)cos(a2)cos(a3)                 | */
/*     |                                                                | */
/*     |        -sin(a1)cos(a3)         -sin(a1)sin(a3)  cos(a1)sin(a2) | */
/*     | -cos(a1)cos(a2)sin(a3)  +cos(a1)cos(a2)cos(a3)                 | */
/*     |                                                                | */
/*     |         sin(a2)sin(a3)         -sin(a2)cos(a3)         cos(a2) | */
/*     +-                                                              -+ */


/*        but if a2 is 0 or pi, the product matrix reduces to */


/*     +-                                                              -+ */
/*     |         cos(a1)cos(a3)          cos(a1)sin(a3)               0 | */
/*     | -sin(a1)cos(a2)sin(a3)  +sin(a1)cos(a2)cos(a3)                 | */
/*     |                                                                | */
/*     |        -sin(a1)cos(a3)         -sin(a1)sin(a3)               0 | */
/*     | -cos(a1)cos(a2)sin(a3)  +cos(a1)cos(a2)cos(a3)                 | */
/*     |                                                                | */
/*     |                      0                       0         cos(a2) | */
/*     +-                                                              -+ */


/*        In this case, a1 and a3 are not uniquely determined.  If we */
/*        arbitrarily set a1 to zero, we arrive at the matrix */

/*           +-                                         -+ */
/*           |         cos(a3)         sin(a3)      0    | */
/*           | -cos(a2)sin(a3)  cos(a2)cos(a3)      0    | */
/*           |               0            0      cos(a2) | */
/*           +-                                         -+ */

/*        We take care of this case first.  We test three conditions */
/*        that are mathematically equivalent, but may not be satisfied */
/*        simultaneously because of round-off: */


	degen = tmprot[6] == 0. && tmprot[7] == 0. || tmprot[2] == 0. && 
		tmprot[5] == 0. || abs(tmprot[8]) == 1.;

/*        In the following block of code, we make use of the fact that */

/*           SIN ( ANGLE2 )   >  0 */
/*                            - */
/*        in choosing the signs of the ATAN2 arguments correctly.  Note */
/*        that ATAN2(x,y) = -ATAN2(-x,-y). */


	if (degen) {
	    *angle3 = 0.;
	    *angle2 = acos(tmprot[8]);
	    *angle1 = atan2(tmprot[3], tmprot[0]);
	} else {

/*           The normal case. */

	    *angle3 = atan2(tmprot[6], tmprot[7]);
	    *angle2 = acos(tmprot[8]);
	    *angle1 = atan2(tmprot[2], -tmprot[5]);
	}
    } else {

/*        The axis order is c-b-a.  We're going to find a matrix CHANGE */
/*        such that */

/*                 T */
/*           CHANGE  R  CHANGE */

/*        gives us R in the a useful basis, that is, a basis in which */
/*        our original c-b-a rotation is a 3-2-1 rotation, but where the */
/*        rotation angles are unchanged, or at worst negated.  To */
/*        achieve this pleasant simplification, we set column 1 of */
/*        CHANGE to to e(AXIS3), column 2 of CHANGE to e(AXIS2), and */
/*        column 3 of CHANGE to */

/*          (+/-) e(AXIS1), */

/*        depending on whether AXIS3-AXIS2-AXIS1 is a right-handed */
/*        sequence of axes:  if it is, the sign is positive.  (Here */
/*        e(1), e(2), e(3) are the standard basis vectors.) */

/*        We must be cautious here, because if AXIS3-AXIS2-AXIS1 is a */
/*        right-handed sequence of axes, all of the rotation angles will */
/*        be the same in our new basis, but if it's a left-handed */
/*        sequence, the third angle will be negated.  Let's get this */
/*        straightened out right now.  The variable NEXT is just a */
/*        little mapping that takes 1 to 2, 2 to 3, and 3 to 1. */

	if (*axis2 == next[(i__1 = *axis3 - 1) < 3 && 0 <= i__1 ? i__1 : 
		s_rnge("next", i__1, "m2eul_", (ftnlen)883)]) {
	    sign = 1.;
	} else {
	    sign = -1.;
	}

/*        Set up the entries of CHANGE: */

	cleard_(&c__9, change);
	change[(i__1 = *axis3 - 1) < 9 && 0 <= i__1 ? i__1 : s_rnge("change", 
		i__1, "m2eul_", (ftnlen)894)] = 1.;
	change[(i__1 = *axis2 + 2) < 9 && 0 <= i__1 ? i__1 : s_rnge("change", 
		i__1, "m2eul_", (ftnlen)895)] = 1.;
	change[(i__1 = *axis1 + 5) < 9 && 0 <= i__1 ? i__1 : s_rnge("change", 
		i__1, "m2eul_", (ftnlen)896)] = sign * 1.;

/*        Transform TMPROT. */

	mxm_(tmprot, change, tmpmat);
	mtxm_(change, tmpmat, tmprot);

/*        Now we're ready to find the Euler angles, using a */
/*        3-2-1 factorization.  In general, the matrix product */

/*           [ a1 ]   [ a2 ]   [ a3 ] */
/*                 1        2        3 */

/*        has the form */


/*     +-                                                              -+ */
/*     |         cos(a2)cos(a3)          cos(a2)sin(a3)        -sin(a2) | */
/*     |                                                                | */
/*     |        -cos(a1)sin(a3)          cos(a1)cos(a3)  sin(a1)cos(a2) | */
/*     | +sin(a1)sin(a2)cos(a3)  +sin(a1)sin(a2)sin(a3)                 | */
/*     |                                                                | */
/*     |         sin(a1)sin(a3)         -sin(a1)cos(a3)  cos(a1)cos(a2) | */
/*     | +cos(a1)sin(a2)cos(a3)  +cos(a1)sin(a2)sin(a3)                 | */
/*     +-                                                              -+ */


/*        but if a2 is -pi/2 or pi/2, the product matrix reduces to */


/*     +-                                                              -+ */
/*     |                      0                       0        -sin(a2) | */
/*     |                                                                | */
/*     |        -cos(a1)sin(a3)          cos(a1)cos(a3)               0 | */
/*     | +sin(a1)sin(a2)cos(a3)  +sin(a1)sin(a2)sin(a3)                 | */
/*     |                                                                | */
/*     |         sin(a1)sin(a3)         -sin(a1)cos(a3)               0 | */
/*     | +cos(a1)sin(a2)cos(a3)  +cos(a1)sin(a2)sin(a3)                 | */
/*     +-                                                              -+ */


/*        In this case, a1 and a3 are not uniquely determined.  If we */
/*        arbitrarily set a1 to zero, we arrive at the matrix */

/*           +-                                              -+ */
/*           |               0                 0    -sin(a2)  | */
/*           |        -sin(a3)           cos(a3)          0   |, */
/*           |  sin(a2)cos(a3)    sin(a2)sin(a3)          0   | */
/*           +-                                              -+ */


/*        We take care of this case first.  We test three conditions */
/*        that are mathematically equivalent, but may not be satisfied */
/*        simultaneously because of round-off: */


	degen = tmprot[0] == 0. && tmprot[3] == 0. || tmprot[7] == 0. && 
		tmprot[8] == 0. || abs(tmprot[6]) == 1.;

/*        In the following block of code, we make use of the fact that */

/*           COS ( ANGLE2 )   >  0 */
/*                            - */
/*        in choosing the signs of the ATAN2 arguments correctly.  Note */
/*        that ATAN2(x,y) = -ATAN2(-x,-y). */


	if (degen) {
	    *angle3 = 0.;
	    *angle2 = asin(-tmprot[6]);
	    *angle1 = sign * atan2(-tmprot[1], tmprot[4]);
	} else {

/*           The normal case. */

	    *angle3 = atan2(tmprot[7], tmprot[8]);
	    *angle2 = asin(-tmprot[6]);
	    *angle1 = sign * atan2(tmprot[3], tmprot[0]);
	}
    }
    chkout_("M2EUL", (ftnlen)5);
    return 0;
} /* m2eul_ */

