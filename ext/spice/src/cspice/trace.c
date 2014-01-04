/* trace.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure                      TRACE ( Trace of a 3x3 matrix ) */
doublereal trace_(doublereal *matrix)
{
    /* System generated locals */
    doublereal ret_val;

/* $ Abstract */

/*      Return the trace of a 3x3 matrix. */

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

/*      MATRIX */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      MATRIX     I     3x3 matrix of double precision numbers. */
/*      TRACE      O     The trace of MATRIX. */

/* $ Detailed_Input */

/*      MATRIX  is a double precision 3x3 matrix. */

/* $ Detailed_Output */

/*      TRACE   is the trace of MATRIX, i.e. it is the sum of the */
/*              diagonal elements of MATRIX. */

/* $ Parameters */

/*     None. */

/* $ Particulars */

/*      TRACE simply executes in FORTRAN code the following loop: */

/*      TRACE = Summation from I = 1 to 3 of MATRIX(I,I) */

/*      No error detection or correction is implemented within this */
/*      function. */

/* $ Examples */

/*                            | 3   5   7 | */
/*      Suppose that MATRIX = | 0  -2   8 |  , then */
/*                            | 4   0  -1 | */

/*      TRACE (MATRIX) = 0.  (which is the sum of 3, -2 and -1). */

/* $ Restrictions */

/*      No checking is performed to guard against floating point overflow */
/*      or underflow.  This routine should probably not be used if the */
/*      input matrix is expected to have large double precision numbers */
/*      along the diagonal. */

/* $ Exceptions */

/*      Error free. */

/* $ Files */

/*      None */

/* $ Author_and_Institution */

/*      W.L. Taber      (JPL) */

/* $ Literature_References */

/*      None */

/* $ Version */

/* -     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*         Comment section for permuted index source lines was added */
/*         following the header. */

/* -     SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     trace of a 3x3_matrix */

/* -& */
    ret_val = matrix[0] + matrix[4] + matrix[8];

    return ret_val;
} /* trace_ */

