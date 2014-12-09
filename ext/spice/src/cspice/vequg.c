/* vequg.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      VEQUG ( Vector equality, general dimension ) */
/* Subroutine */ int vequg_(doublereal *vin, integer *ndim, doublereal *vout)
{
    /* System generated locals */
    integer vin_dim1, vout_dim1, i__1, i__2, i__3;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer i__;

/* $ Abstract */

/*      Make one double precision vector of arbitrary dimension equal */
/*      to another. */

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

/*      ASSIGNMENT,  VECTOR */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*       VIN       I   NDIM-dimensional double precision vector. */
/*       NDIM      I   Dimension of VIN (and also VOUT). */
/*       VOUT      O   NDIM-dimensional double precision vector set */
/*                     equal to VIN. */

/* $ Detailed_Input */

/*      VIN      is a double precision vector of arbitrary dimension. */

/*      NDIM     is the number of components of VIN. */

/* $ Detailed_Output */

/*      VOUT    is a double precision vector set equal to VIN. */

/* $ Parameters */

/*      None. */

/* $ Particulars */

/*      The code simply sets each component of VOUT equal to the */
/*      corresponding component of VIN. */

/* $ Examples */

/*      Let STATE be a state vector. Set ABSTAT equal to STATE, and */
/*      correct ABSTAT for stellar aberration. */

/*      CALL VEQUG  ( STATE,           6, ABSTAT ) */
/*      CALL STELAB ( STATE(1), STATE(4), ABSPOS ) */
/*      CALL VEQU   ( ABSPOS,   ABSTAT(1)        ) */


/*      Note that this routine may be used in place of MOVED, which */
/*      sets each output array element equal to the corresponding */
/*      input array element. */

/* $ Restrictions */

/*      None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*      None. */

/* $ Author_and_Institution */

/*      W.M. Owen       (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) */

/* -& */
/* $ Index_Entries */

/*     assign an n-dimensional vector to another */

/* -& */
    /* Parameter adjustments */
    vout_dim1 = *ndim;
    vin_dim1 = *ndim;

    /* Function Body */
    i__1 = *ndim;
    for (i__ = 1; i__ <= i__1; ++i__) {
	vout[(i__2 = i__ - 1) < vout_dim1 && 0 <= i__2 ? i__2 : s_rnge("vout",
		 i__2, "vequg_", (ftnlen)131)] = vin[(i__3 = i__ - 1) < 
		vin_dim1 && 0 <= i__3 ? i__3 : s_rnge("vin", i__3, "vequg_", (
		ftnlen)131)];
    }
    return 0;
} /* vequg_ */

