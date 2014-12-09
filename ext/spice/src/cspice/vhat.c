/* vhat.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      VHAT ( "V-Hat", unit vector along V, 3 dimensions ) */
/* Subroutine */ int vhat_(doublereal *v1, doublereal *vout)
{
    doublereal vmag;
    extern doublereal vnorm_(doublereal *);

/* $ Abstract */

/*      Find the unit vector along a double precision 3-dimensional */
/*      vector. */

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
/*       V1        I     Vector to be normalized. */
/*       VOUT      O     Unit vector V1 / |V1|. */
/*                       If V1 = 0, VOUT will also be zero. */
/*                       VOUT can overwrite V1. */

/* $ Detailed_Input */

/*      V1      This is any double precision, 3-dimensional vector.  If */
/*              this vector is the zero vector, this routine will detect */
/*              it, and will not attempt to divide by zero. */

/* $ Detailed_Output */

/*      VOUT    VOUT contains the unit vector in the direction of V1. If */
/*              V1 represents the zero vector, then VOUT will also be the */
/*              zero vector.  VOUT may overwrite V1. */

/* $ Parameters */

/*     None. */

/* $ Particulars */

/*      VHAT determines the magnitude of V1 and then divides each */
/*      component of V1 by the magnitude.  This process is highly stable */
/*      over the whole range of 3-dimensional vectors. */

/* $ Examples */

/*      The following table shows how selected V1 implies VOUT. */

/*      V1                    VOUT */
/*      ------------------    ------------------ */
/*      (5, 12, 0)            (5/13, 12/13, 0) */
/*      (1D-7, 2D-7, 2D-7)    (1/3, 2/3, 2/3) */


/* $ Restrictions */

/*      There is no known case whereby floating point overflow may occur. */
/*      Thus, no error recovery or reporting scheme is incorporated */
/*      into this subroutine. */

/* $ Exceptions */

/*      Error free. */

/* $ Files */

/*      None. */

/* $ Author_and_Institution */

/*      N.J. Bachman    (JPL) */
/*      H.A. Neilan     (JPL) */
/*      W.M. Owen       (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*         Comment section for permuted index source lines was added */
/*         following the header. */

/* -     SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) */

/* -& */
/* $ Index_Entries */

/*     unitize a 3-dimensional vector */

/* -& */
/* $ Revisions */

/* -     Beta Version 1.1.0, 10-FEB-1989 (HAN) (NJB) */

/*         Contents of the Exceptions section was changed */
/*         to "error free" to reflect the decision that the */
/*         module will never participate in error handling. */
/*         Also, the declaration of the unused variable I was */
/*         removed. */
/* -& */

/*  Obtain the magnitude of V1 */

    vmag = vnorm_(v1);

/*   If VMAG is nonzero, then normalize.  Note that this process is */
/*   numerically stable: overflow could only happen if VMAG were small, */
/*   but this could only happen if each component of V1 were small. */
/*   In fact, the magnitude of any vector is never less than the */
/*   magnitude of any component. */

    if (vmag > 0.) {
	vout[0] = v1[0] / vmag;
	vout[1] = v1[1] / vmag;
	vout[2] = v1[2] / vmag;
    } else {
	vout[0] = 0.;
	vout[1] = 0.;
	vout[2] = 0.;
    }
    return 0;
} /* vhat_ */

