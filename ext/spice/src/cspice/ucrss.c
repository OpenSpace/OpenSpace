/* ucrss.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      UCRSS ( Unitized cross product, 3x3 ) */
/* Subroutine */ int ucrss_(doublereal *v1, doublereal *v2, doublereal *vout)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Local variables */
    doublereal vmag, maxv1, maxv2;
    extern doublereal vnorm_(doublereal *);
    doublereal vcross[3], tv1[3], tv2[3];

/* $ Abstract */

/*      Compute the normalized cross product of two 3-vectors. */

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

/*      VECTOR */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*       V1        I     Left vector for cross product. */
/*       V2        I     Right vector for cross product. */
/*       VOUT      O     Normalized cross product (V1xV2) / |V1xV2|. */

/* $ Detailed_Input */

/*      V1   A 3-vector. */

/*      V2   A 3-vector. */

/* $ Detailed_Output */

/*      VOUT is the result of the computation (V1xV2)/|V1xV2| */

/* $ Parameters */

/*      None. */

/* $ Particulars */

/*      None. */

/* $ Examples */

/*      To get a unit normal to the plane spanned by two vectors */
/*      V1 and V2. Simply call */

/*         CALL UCRSS ( V1, V2, NORMAL ) */

/* $ Restrictions */

/*      None. */

/* $ Exceptions */

/*     Error free. */

/*     1) If the cross product of V1 and V2 yields the zero-vector, then */
/*        the zero-vector is returned instead of a vector of unit length. */

/* $ Files */

/*      None. */

/* $ Author_and_Institution */

/*      W.M. Owen       (JPL) */
/*      W.L. Taber      (JPL) */

/* $ Literature_References */

/*      None */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) */

/* -& */
/* $ Index_Entries */

/*     unitized cross product */

/* -& */
/* $ Revisions */

/* -    Beta Version 1.1.0, 10-JAN-1989 (WLT) */

/*     Error free specification added. In addition the algorithm was made */
/*     more robust in the sense that floating point overflows cannot */
/*     occur. */

/* -& */

/*     Get the biggest component of each of the two vectors. */

/* Computing MAX */
    d__1 = abs(v1[0]), d__2 = abs(v1[1]), d__1 = max(d__1,d__2), d__2 = abs(
	    v1[2]);
    maxv1 = max(d__1,d__2);
/* Computing MAX */
    d__1 = abs(v2[0]), d__2 = abs(v2[1]), d__1 = max(d__1,d__2), d__2 = abs(
	    v2[2]);
    maxv2 = max(d__1,d__2);

/*     Scale V1 and V2 by 1/MAXV1 and 1/MAXV2 respectively */

    if (maxv1 != 0.) {
	tv1[0] = v1[0] / maxv1;
	tv1[1] = v1[1] / maxv1;
	tv1[2] = v1[2] / maxv1;
    } else {
	tv1[0] = 0.;
	tv1[1] = 0.;
	tv1[2] = 0.;
    }
    if (maxv2 != 0.) {
	tv2[0] = v2[0] / maxv2;
	tv2[1] = v2[1] / maxv2;
	tv2[2] = v2[2] / maxv2;
    } else {
	tv2[0] = 0.;
	tv2[1] = 0.;
	tv2[2] = 0.;
    }

/*  Calculate the cross product of V1 and V2 */

    vcross[0] = tv1[1] * tv2[2] - tv1[2] * tv2[1];
    vcross[1] = tv1[2] * tv2[0] - tv1[0] * tv2[2];
    vcross[2] = tv1[0] * tv2[1] - tv1[1] * tv2[0];

/*  Get the magnitude of VCROSS and normalize it */

    vmag = vnorm_(vcross);
    if (vmag > 0.) {
	vout[0] = vcross[0] / vmag;
	vout[1] = vcross[1] / vmag;
	vout[2] = vcross[2] / vmag;
    } else {
	vout[0] = 0.;
	vout[1] = 0.;
	vout[2] = 0.;
    }
    return 0;
} /* ucrss_ */

