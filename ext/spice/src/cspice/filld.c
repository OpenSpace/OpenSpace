/* filld.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      FILLD ( Fill a double precision array ) */
/* Subroutine */ int filld_(doublereal *value, integer *ndim, doublereal *
	array)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i__;

/* $ Abstract */

/*      Fill a double precision array with a specified value. */

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

/*      ARRAY,  ASSIGNMENT */

/* $ Declarations */
/* $ Brief_I/O */

/*  VARIABLE  I/O              DESCRIPTION */
/*  --------  ---  ------------------------------------------------------ */
/*   VALUE     I     Double precision value to be placed in all the */
/*                   elements of ARRAY. */
/*   NDIM      I     The number of elements in ARRAY. */
/*   ARRAY     O     Double precision array which is to be filled. */

/* $ Detailed_Input */

/*      VALUE     is the value to be assigned to the array elements */
/*                1 through NDIM. */

/*      NDIM      is the number of elements in the array. */

/* $ Detailed_Output */

/*      ARRAY     is a double precision array whose elements are to be */
/*                set to VALUE. */

/* $ Parameters */

/*      None. */

/* $ Particulars */

/*      None. */

/* $ Examples */

/*       Let  VALUE = 1.0D0 */
/*            NDIM  =  4 */

/*       then the contents of ARRAY are: */

/*            ARRAY (1) = 1.0D0 */
/*            ARRAY (2) = 1.0D0 */
/*            ARRAY (3) = 1.0D0 */
/*            ARRAY (4) = 1.0D0 */

/* $ Restrictions */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1) If NDIM < 1 the array is not modified. */

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

/*     fill a d.p. array */

/* -& */

/*     Local variables */

    i__1 = *ndim;
    for (i__ = 1; i__ <= i__1; ++i__) {
	array[i__ - 1] = *value;
    }
    return 0;
} /* filld_ */

