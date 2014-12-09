/* unorm.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      UNORM ( Unit vector and norm, 3 dimensional ) */
/* Subroutine */ int unorm_(doublereal *v1, doublereal *vout, doublereal *
	vmag)
{
    extern doublereal vnorm_(doublereal *);

/* $ Abstract */

/*     Normalize a double precision 3-vector and return its magnitude. */

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

/*     VECTOR */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     V1         I   Vector to be normalized. */
/*     VOUT       O   Unit vector V1 / |V1|. */
/*                    If V1 is the zero vector, then VOUT will also */
/*                    be zero. */
/*     VMAG       O   Magnitude of V1, i.e. |V1|. */

/* $ Detailed_Input */

/*     V1      This variable may contain any 3-vector, including the */
/*             zero vector. */

/* $ Detailed_Output */

/*     VOUT    This variable contains the unit vector in the direction */
/*             of V1.  If V1 is the zero vector, then VOUT will also be */
/*             the zero vector. */

/*     VMAG    This is the magnitude of V1. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     UNORM references a function called VNORM (which itself is */
/*     numerically stable) to calculate the norm of the input vector V1. */
/*     If the norm is equal to zero, then each component of the output */
/*     vector VOUT is set to zero.  Otherwise, VOUT is calculated by */
/*     dividing V1 by the norm. */

/* $ Examples */

/*     The following table shows how selected V1 implies VOUT and MAG. */

/*        V1                    VOUT                   MAG */
/*        ------------------    ------------------     ---- */
/*        (5, 12, 0)            (5/13, 12/13, 0)       13 */
/*        (1D-7, 2D-7, 2D-7)    (1/3, 2/3, 2/3)        3D-7 */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.M. Owen       (JPL) */
/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.2, 23-APR-2010 (NJB) */

/*        Header correction: assertions that the output */
/*        can overwrite the input have been removed. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) */

/* -& */
/* $ Index_Entries */

/*     3-dimensional unit vector and norm */

/* -& */
/* $ Revisions */

/* -    Beta Version 1.0.1, 10-JAN-1989 (WLT) */

/*     Error free specification added. */

/* -& */


/*     SPICELIB functions */


/*  Obtain the magnitude of V1 */

    *vmag = vnorm_(v1);

/*   If VMAG is nonzero, then normalize.  Note that this process is */
/*   numerically stable: overflow could only happen if VMAG were small, */
/*   but this could only happen if each component of V1 were small. */
/*   In fact, the magnitude of any vector is never less than the */
/*   magnitude of any component. */

    if (*vmag > 0.) {
	vout[0] = v1[0] / *vmag;
	vout[1] = v1[1] / *vmag;
	vout[2] = v1[2] / *vmag;
    } else {
	vout[0] = 0.;
	vout[1] = 0.;
	vout[2] = 0.;
    }
    return 0;
} /* unorm_ */

