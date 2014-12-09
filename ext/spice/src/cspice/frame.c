/* frame.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      FRAME ( Build a right handed coordinate frame ) */
/* Subroutine */ int frame_(doublereal *x, doublereal *y, doublereal *z__)
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    double sqrt(doublereal);
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    doublereal a, b, c__, f;
    integer s1, s2, s3;
    extern /* Subroutine */ int vhatip_(doublereal *);

/* $ Abstract */

/*      Given a vector X, this routine builds a right handed */
/*      orthonormal frame X,Y,Z where the output X is parallel to */
/*      the input X. */

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

/*      AXES,  FRAME */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  ------------------------------------------------ */
/*      X         I/0  Input vector. A parallel unit vector on output. */
/*      Y          O   Unit vector in the plane orthogonal to X. */
/*      Z          O   Unit vector given by X x Y. */

/* $ Detailed_Input */


/*      X      This vector is used to form the first vector of a */
/*             right-handed orthonormal triple. */

/* $ Detailed_Output */

/*      X, */
/*      Y, */
/*      Z      form a right handed orthonormal frame, where X is */
/*             now a unit vector parallel to the original input */
/*             vector in X.  There are no special geometric properties */
/*             connected to Y and Z (other than that they complete the */
/*             right handed frame). */

/* $ Parameters */

/*      None. */

/* $ Particulars */

/*      Given an input vector X, this routine returns unit vectors X, */
/*      Y, and Z such that XYZ forms a right-handed orthonormal frame */
/*      where the output X is parallel to the input X. */

/*      This routine is intended primarily to provide a basis for */
/*      the plane orthogonal to X.  There are no special properties */
/*      associated with Y and Z other than that the resulting XYZ frame */
/*      is right handed and orthonormal.  There are an infinite */
/*      collection of pairs (Y,Z) that could be used to this end. */
/*      Even though for a given X, Y and Z are uniquely */
/*      determined, users */
/*      should regard the pair (Y,Z) as a random selection from this */
/*      infinite collection. */

/*      For instance, when attempting to determine the locus of points */
/*      that make up the limb of a triaxial body, it is a straightforward */
/*      matter to determine the normal to the limb plane.  To find */
/*      the actual parametric equation of the limb one needs to have */
/*      a basis of the plane.  This routine can be used to get a basis */
/*      in which one can describe the curve and from which one can */
/*      then determine the principal axes of the limb ellipse. */

/* $ Examples */

/*      In addition to using a vector to construct a right handed frame */
/*      with the x-axis aligned with the input vector, one can construct */
/*      right handed frames with any of the axes aligned with the input */
/*      vector. */

/*      For example suppose we want a right hand frame XYZ with the */
/*      Z-axis aligned with some vector V.  Assign V to Z */

/*            Z(1) = V(1) */
/*            Z(2) = V(2) */
/*            Z(3) = V(3) */

/*      Then call FRAME with the arguements X,Y,Z cycled so that Z */
/*      appears first. */

/*            CALL FRAME (Z, X, Y) */

/*      The resulting XYZ frame will be orthonormal with Z parallel */
/*      to the vector V. */

/*      To get an XYZ frame with Y parallel to V perform the following */

/*            Y(1) = V(1) */
/*            Y(2) = V(2) */
/*            Y(3) = V(3) */

/*            CALL FRAME (Y, Z, X) */

/* $ Restrictions */

/*      None. */

/* $ Exceptions */

/*     Error Free */

/*     1) If X on input is the zero vector the ``standard'' frame (ijk) */
/*        is returned. */

/* $ Files */

/*      None. */

/* $ Author_and_Institution */

/*      W.L. Taber      (JPL) */
/*      I.M. Underwood  (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -    SPICELIB Version 1.2.0, 02-SEP-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in VHAT call. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     build a right handed coordinate frame */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.2.0, 02-SEP-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in VHAT call. */

/* -    Beta Version 2.0.0, 29-DEC-1988 (WLT) (IMU) */

/*     The routine was modified so that it now accepts any input */
/*     vector in the X slot (it originally was assumed to be a unit */
/*     vector).  Moreover, the original algorithm has been streamlined */
/*     a great deal to take advantage of our knowledge of the */
/*     internal structure of the orthonormal triple. */

/* -& */


/*     Local variables */


/*     First make X into a unit vector. */

    vhatip_(x);

/*     We'll need the squares of the components of X in a bit. */

    a = x[0] * x[0];
    b = x[1] * x[1];
    c__ = x[2] * x[2];

/*     If X is zero, then just return the ijk frame. */

    if (a + b + c__ == 0.) {
	x[0] = 1.;
	x[1] = 0.;
	x[2] = 0.;
	y[0] = 0.;
	y[1] = 1.;
	y[2] = 0.;
	z__[0] = 0.;
	z__[1] = 0.;
	z__[2] = 1.;
	return 0;
    }

/*     If we make it this far, determine which component of X has the */
/*     smallest magnitude.  This component will be zero in Y. The other */
/*     two components of X will put into Y swapped with the sign of */
/*     the first changed.  From there, Z can have only one possible */
/*     set of values which it gets from the smallest component */
/*     of X, the non-zero components of Y and the length of Y. */

    if (a <= b && a <= c__) {
	f = sqrt(b + c__);
	s1 = 1;
	s2 = 2;
	s3 = 3;
    } else if (b <= a && b <= c__) {
	f = sqrt(a + c__);
	s1 = 2;
	s2 = 3;
	s3 = 1;
    } else {
	f = sqrt(a + b);
	s1 = 3;
	s2 = 1;
	s3 = 2;
    }

/*     Note: by construction, F is the magnitude of the large components */
/*     of X.  With this in mind, one can verify by inspection that X, Y */
/*     and Z yield an orthonormal frame.  The right handedness follows */
/*     from the assignment of values to S1, S2 and S3 (they are merely */
/*     cycled from one case to the next). */

    y[(i__1 = s1 - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("y", i__1, "frame_", (
	    ftnlen)285)] = 0.;
    y[(i__1 = s2 - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("y", i__1, "frame_", (
	    ftnlen)286)] = -x[(i__2 = s3 - 1) < 3 && 0 <= i__2 ? i__2 : 
	    s_rnge("x", i__2, "frame_", (ftnlen)286)] / f;
    y[(i__1 = s3 - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("y", i__1, "frame_", (
	    ftnlen)287)] = x[(i__2 = s2 - 1) < 3 && 0 <= i__2 ? i__2 : s_rnge(
	    "x", i__2, "frame_", (ftnlen)287)] / f;
    z__[(i__1 = s1 - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("z", i__1, "frame_", 
	    (ftnlen)289)] = f;
    z__[(i__1 = s2 - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("z", i__1, "frame_", 
	    (ftnlen)290)] = -x[(i__2 = s1 - 1) < 3 && 0 <= i__2 ? i__2 : 
	    s_rnge("x", i__2, "frame_", (ftnlen)290)] * y[(i__3 = s3 - 1) < 3 
	    && 0 <= i__3 ? i__3 : s_rnge("y", i__3, "frame_", (ftnlen)290)];
    z__[(i__1 = s3 - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("z", i__1, "frame_", 
	    (ftnlen)291)] = x[(i__2 = s1 - 1) < 3 && 0 <= i__2 ? i__2 : 
	    s_rnge("x", i__2, "frame_", (ftnlen)291)] * y[(i__3 = s2 - 1) < 3 
	    && 0 <= i__3 ? i__3 : s_rnge("y", i__3, "frame_", (ftnlen)291)];
    return 0;
} /* frame_ */

