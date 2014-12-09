/* rotvec.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ROTVEC ( Transform a vector via a rotation ) */
/* Subroutine */ int rotvec_(doublereal *v1, doublereal *angle, integer *
	iaxis, doublereal *vout)
{
    /* Initialized data */

    static integer indexs[5] = { 3,1,2,3,1 };

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    double sin(doublereal), cos(doublereal);
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    doublereal temp[3], c__, s;
    integer i1, i2, i3, tmp;

/* $ Abstract */

/*     Transform a vector to a new coordinate system rotated by ANGLE */
/*     radians about axis IAXIS.  This transformation rotates V1 by */
/*     -ANGLE radians about the specified axis. */

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

/*     ROTATION */
/*     VECTOR */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     V1         I   Vector whose coordinate system is to be rotated. */
/*     ANGLE      I   Angle of rotation in radians. */
/*     IAXIS      I   Axis of rotation (X=1, Y=2, Z=3). */
/*     VOUT       O   Resulting vector [ANGLE]      * V1 expressed in */
/*                                            IAXIS */
/*                    the new coordinate system. */

/* $ Detailed_Input */

/*     V1      This is a vector (typically representing a vector fixed */
/*             in inertial space) which is to be expressed in another */
/*             coordinate system.  The vector remains fixed but the */
/*             coordinate system changes. */

/*     ANGLE   The angle given in radians, through which the rotation */
/*             is performed. */

/*     IAXIS   The index of the axis of rotation.  The X, Y, and Z */
/*             axes have indices 1, 2 and 3 respectively. */

/* $ Detailed_Output */

/*     VOUT    This is the vector expressed in the new coordinate system */
/*             specified by the angle of rotation and axis. If */
/*             [ANGLE]       represents the rotation matrix described by */
/*                    IAXIS */
/*             the angle and axis, (refer to the routine ROTATE) */
/*             then VOUT = [ANGLE]      * V1 */
/*                                IAXIS */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1) If the axis index is not in the range 1 to 3 it will be treated */
/*        the same as that integer 1, 2, or 3 that is congruent to it mod */
/*        3. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     A rotation about the first, i.e. x-axis, is described by */

/*        |  1        0          0      | */
/*        |  0   cos(theta) sin(theta)  | */
/*        |  0  -sin(theta) cos(theta)  | */

/*     A rotation about the second, i.e. y-axis, is described by */

/*        |  cos(theta)  0  -sin(theta)  | */
/*        |      0       1        0      | */
/*        |  sin(theta)  1   cos(theta)  | */

/*     A rotation about the third, i.e. z-axis, is described by */

/*        |  cos(theta) sin(theta)   0   | */
/*        | -sin(theta) cos(theta)   0   | */
/*        |       0          0       1   | */

/*     ROTVEC decides which form is appropriate according to the value */
/*     of IAXIS and applies the rotation to the input vector. */

/* $ Examples */

/*     Suppose that */

/*        V1 = (1.414, 0, 0), ANGLE = PI/4, IAXIS = 3 */

/*     then after calling ROTVEC according to */

/*        CALL ROTVEC (V1, ANGLE, IAXIS, VOUT) */

/*     VOUT will be equal to (1, -1, 0). */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.M. Owen       (JPL) */
/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.3, 23-APR-2010 (NJB) */

/*        Header correction: assertions that the output */
/*        can overwrite the input have been removed. */

/* -    SPICELIB Version 1.0.2, 04-OCT-1999 (NJB) */

/*        Procedure line and abstract and were changed to dispel the */
/*        impression that the input vector is rotated by +ANGLE */
/*        radians about the specified axis. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) */

/* -& */
/* $ Index_Entries */

/*     rotate a vector */

/* -& */
/* $ Revisions */

/* -    Beta Version 1.1.0, 4-JAN-1989 (WLT) */

/*     Upgrade the routine to work with negative axis indexes.  Also take */
/*     care of the funky way the indices (other than the input) were */
/*     obtained via the MOD function.  It works but isn't as clear */
/*     (or fast) as just reading the axes from data. */

/* -& */

/*  Get the sine and cosine of ANGLE */

    s = sin(*angle);
    c__ = cos(*angle);

/*  Get indices for axes. The first index is for the axis of rotation. */
/*  The next two axes follow in right hand order (XYZ).  First get the */
/*  non-negative value of IAXIS mod 3 . */

    tmp = (*iaxis % 3 + 3) % 3;
    i1 = indexs[(i__1 = tmp) < 5 && 0 <= i__1 ? i__1 : s_rnge("indexs", i__1, 
	    "rotvec_", (ftnlen)215)];
    i2 = indexs[(i__1 = tmp + 1) < 5 && 0 <= i__1 ? i__1 : s_rnge("indexs", 
	    i__1, "rotvec_", (ftnlen)216)];
    i3 = indexs[(i__1 = tmp + 2) < 5 && 0 <= i__1 ? i__1 : s_rnge("indexs", 
	    i__1, "rotvec_", (ftnlen)217)];

/*  The coordinate along the axis of rotation does not change. */

    temp[0] = v1[(i__1 = i1 - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("v1", i__1, 
	    "rotvec_", (ftnlen)221)];
    temp[1] = c__ * v1[(i__1 = i2 - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("v1", 
	    i__1, "rotvec_", (ftnlen)222)] + s * v1[(i__2 = i3 - 1) < 3 && 0 
	    <= i__2 ? i__2 : s_rnge("v1", i__2, "rotvec_", (ftnlen)222)];
    temp[2] = -s * v1[(i__1 = i2 - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("v1", 
	    i__1, "rotvec_", (ftnlen)223)] + c__ * v1[(i__2 = i3 - 1) < 3 && 
	    0 <= i__2 ? i__2 : s_rnge("v1", i__2, "rotvec_", (ftnlen)223)];

/*  Move the buffered vector to the output */

    vout[(i__1 = i1 - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("vout", i__1, "rot"
	    "vec_", (ftnlen)227)] = temp[0];
    vout[(i__1 = i2 - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("vout", i__1, "rot"
	    "vec_", (ftnlen)228)] = temp[1];
    vout[(i__1 = i3 - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("vout", i__1, "rot"
	    "vec_", (ftnlen)229)] = temp[2];

    return 0;
} /* rotvec_ */

