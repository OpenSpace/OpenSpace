/* brckti.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure  BRCKTI ( Bracket an integer value within an interval. ) */
integer brckti_(integer *number, integer *end1, integer *end2)
{
    /* System generated locals */
    integer ret_val, i__1, i__2;

/* $ Abstract */

/*      Bracket a number. That is, given a number and an acceptable */
/*      interval, make sure that the number is contained in the */
/*      interval. (If the number is already in the interval, leave it */
/*      alone. If not, set it to the nearest endpoint of the interval.) */

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

/*      INTERVALS,  NUMBERS,  UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      NUMBER     I   Number to be bracketed. */
/*      END1       I   One of the bracketing endpoints for NUMBER. */
/*      END2       I   The other bracketing endpoint for NUMBER. */
/*      BRCKTI     O   Bracketed number. */

/* $ Detailed_Input */

/*      NUMBER      is the number to be bracketed. That is, the */
/*                  value of NUMBER is constrained to lie in the */
/*                  interval bounded bye END1 and END2. */

/*      END1, */
/*      END2        are the lower and upper bounds for NUMBER. The */
/*                  order is not important. */

/* $ Detailed_Output */

/*      BRCKTI      is NUMBER, if it was already in the interval */
/*                  provided. Otherwise it is the value of the nearest */
/*                  bound of the interval. */

/* $ Parameters */

/*      None. */

/* $ Particulars */

/*      This routine provides a shorthand notation for code fragments */
/*      like the following */

/*            IF      ( NUMBER .LT. END1 ) THEN */
/*                                              NUMBER = END1 */
/*            ELSE IF ( NUMBER .GT. END2 ) THEN */
/*                                              NUMBER = END2 */
/*            END IF */

/*      which occur frequently during the processing of program inputs. */

/* $ Examples */

/*      The following illustrate the operation of BRCKTI. */

/*            BRCKTI (  -1,   1,  10 )  =  1 */
/*            BRCKTI (  29,   1,  10 )  = 10 */
/*            BRCKTI (   3, -10,  10 )  =  3 */
/*            BRCKTI (   3, -10,  -1 )  = -1 */

/*      The following code fragment illustrates a typical use for BRCKTI. */

/*            C */
/*            C     Object code must be in the range 701-705. */
/*            C */
/*                  READ (5,*) CODE */
/*                  CODE = BRCKTI ( CODE, 701, 705 ) */

/* $ Restrictions */

/*      None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*      None. */

/* $ Author_and_Institution */

/*      W.L. Taber      (JPL) */
/*      I.M. Underwood  (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) */

/* -& */
/* $ Index_Entries */

/*     bracket an integer value within an interval */

/* -& */
/* $ Revisions */

/* -     Beta Version 1.1.0, 30-DEC-1988 (WLT) */

/*      The routine was modified so that the order of the endpoints */
/*      of the bracketing interval is not needed.  The routine now */
/*      determines which is the left endpoint and which is the */
/*      right and acts appropriately. */

/* -& */

/*     What else is there to say? */

    if (*end1 < *end2) {
/* Computing MAX */
	i__1 = *end1, i__2 = min(*end2,*number);
	ret_val = max(i__1,i__2);
    } else {
/* Computing MAX */
	i__1 = *end2, i__2 = min(*end1,*number);
	ret_val = max(i__1,i__2);
    }
    return ret_val;
} /* brckti_ */

