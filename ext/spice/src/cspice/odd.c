/* odd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure             ODD ( Is a number odd? ) */
logical odd_(integer *i__)
{
    /* System generated locals */
    logical ret_val;

/* $ Abstract */

/*     Determine whether an integer is odd. */

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

/*     NUMBERS */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     I          I   The integer in question. */
/*     ODD        O   True if I is odd, otherwise false. */

/* $ Detailed_Input */

/*     I           is the integer to be tested for oddness. */

/* $ Detailed_Output */

/*     ODD is returned as true if I is odd, false if I is even. */

/* $ Parameters */

/*     None. */

/* $ Particulars */

/*     Divide I by two. If the remainder is one, I is odd. */

/* $ Examples */

/*     Let ENDPTS contain a series of endpoints, */

/*        a , b , ..., a , b */
/*         1   1        n   n */

/*     representing an ordered collection of disjoint intervals, */

/*        a   <  b   < a */
/*         i  -   i     i+1 */

/*     The following code fragment uses ODD to determine whether */
/*     an arbitrary value X is contained in any of the intervals. */

/*        CONTAINED = .FALSE. */

/*        DO I = 1, N-1 */
/*           IF ( X .GE. ENDPTS(I)  .AND.  X .LE. ENDPTS(I+1) ) THEN */
/*              CONTAINED = ( ODD ( I ) ) */
/*           END IF */
/*        END DO */

/* $ Restrictions */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Author_and_Institution */

/*     I.M. Underwood  (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.2, 07-NOV-2005 (BVS) */

/*        Fixed a few typos in the header. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     test whether an integer is odd */

/* -& */
/* $ Revisions */

/* -    Beta Version 1.0.1, 27-JAN-1989 (IMU) */

/*        Examples section completed. */

/* -& */

/*     Self-explanatory. */

    ret_val = *i__ % 2 != 0;
    return ret_val;
} /* odd_ */

