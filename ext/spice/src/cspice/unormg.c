/* unormg.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      UNORMG ( Unit vector and norm, general dimension ) */
/* Subroutine */ int unormg_(doublereal *v1, integer *ndim, doublereal *vout, 
	doublereal *vmag)
{
    /* System generated locals */
    integer v1_dim1, vout_dim1, i__1, i__2, i__3;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer i__;
    extern doublereal vnormg_(doublereal *, integer *);

/* $ Abstract */

/*     Normalize a double precision vector of arbitrary dimension and */
/*     return its magnitude. */

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
/*     NDIM       I   Dimension of V1 (and also VOUT). */
/*     VOUT       O   Unit vector V1 / |V1|. */
/*                    If V1 = 0, VOUT will also be zero. */
/*     VMAG       O   Magnitude of V1, that is, |V1|. */

/* $ Detailed_Input */

/*     V1      This variable may contain any vector of arbitrary */
/*             dimension, including the zero vector. */
/*     NDIM    This is the dimension of V1 and VOUT. */

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

/*     UNORMG references a function called VNORMG (which itself is */
/*     numerically stable) to calculate the norm of the input vector V1. */
/*     If the norm is equal to zero, then each component of the output */
/*     vector VOUT is set to zero.  Otherwise, VOUT is calculated by */
/*     dividing V1 by the norm.  No error detection or correction is */
/*     implemented. */

/* $ Examples */

/*     The following table shows how selected V1 implies VOUT and MAG. */

/*        V1                    NDIM   VOUT                   MAG */
/*        -------------------------------------------------------- */
/*        (5, 12)               2      (5/13, 12/13)          13 */
/*        (1D-7, 2D-7, 2D-7)    3      (1/3, 2/3, 2/3)        3D-7 */

/* $ Restrictions */

/*     No error checking is implemented in this subroutine to guard */
/*     against numeric overflow. */

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

/*     n-dimensional unit vector and norm */

/* -& */
/* $ Revisions */

/* -    Beta Version 1.0.1, 10-JAN-1989 (WLT) */

/*     Error free specification added. */

/* -& */

/*  Obtain the magnitude of V1 */

    /* Parameter adjustments */
    vout_dim1 = *ndim;
    v1_dim1 = *ndim;

    /* Function Body */
    *vmag = vnormg_(v1, ndim);

/*   If VMAG is nonzero, then normalize.  Note that this process is */
/*   numerically stable: overflow could only happen if VMAG were small, */
/*   but this could only happen if each component of V1 were also small. */
/*   In fact, the magnitude of any vector is never less than the */
/*   magnitude of any component. */

    if (*vmag > 0.) {
	i__1 = *ndim;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    vout[(i__2 = i__ - 1) < vout_dim1 && 0 <= i__2 ? i__2 : s_rnge(
		    "vout", i__2, "unormg_", (ftnlen)161)] = v1[(i__3 = i__ - 
		    1) < v1_dim1 && 0 <= i__3 ? i__3 : s_rnge("v1", i__3, 
		    "unormg_", (ftnlen)161)] / *vmag;
	}
    } else {
	i__1 = *ndim;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    vout[(i__2 = i__ - 1) < vout_dim1 && 0 <= i__2 ? i__2 : s_rnge(
		    "vout", i__2, "unormg_", (ftnlen)165)] = 0.;
	}
    }

    return 0;
} /* unormg_ */

