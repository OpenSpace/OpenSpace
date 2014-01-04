/* vupack.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      VUPACK ( Unpack three scalar components from a vector ) */
/* Subroutine */ int vupack_(doublereal *v, doublereal *x, doublereal *y, 
	doublereal *z__)
{
/* $ Abstract */

/*      Unpack three scalar components from a vector. */

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
/*      V          I   Input vector. */
/*      X, */
/*      Y, */
/*      Z          O   Scalar components of the vector. */

/* $ Detailed_Input */

/*      V           is a vector with components V(1) = X */
/*                                              V(2) = Y */
/*                                              V(3) = Z */
/* $ Detailed_Output */

/*      X, */
/*      Y, */
/*      Z           are the scalar components of the vector. */

/* $ Parameters */

/*     None. */

/* $ Particulars */

/*      Basically, this is just shorthand notation for the common */
/*      sequence */

/*            X = V(1) */
/*            Y = V(2) */
/*            Z = V(3) */

/*      The routine is useful largely for two reasons. First, it */
/*      reduces the chance that the programmer will make a "cut and */
/*      paste" mistake, like */

/*            X = V(1) */
/*            Y = V(1) */
/*            Z = V(1) */

/*      Second, it makes conversions between equivalent units simpler, */
/*      and clearer. For instance, the sequence */

/*            X = V(1) * RPD */
/*            Y = V(2) * RPD */
/*            Z = V(3) * RPD */

/*      can be replaced by the (nearly) equivalent sequence */

/*            CALL VSCL   ( RPD,  V, V ) */
/*            CALL VUPACK ( V, X, Y, Z ) */

/* $ Examples */

/*      See: Detailed_Description. */

/* $ Restrictions */

/*      None. */

/* $ Exceptions */

/*      Error free. */

/* $ Files */

/*      None. */

/* $ Author_and_Institution */

/*      I.M. Underwood  (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*         Comment section for permuted index source lines was added */
/*         following the header. */

/* -     SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     unpack three scalar components from a vector */

/* -& */

/*     Just shorthand, like it says above. */

    *x = v[0];
    *y = v[1];
    *z__ = v[2];
    return 0;
} /* vupack_ */

