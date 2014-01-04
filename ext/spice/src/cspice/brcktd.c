/* brcktd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure BRCKTD (Bracket a double precision value within an interval) */
doublereal brcktd_(doublereal *number, doublereal *end1, doublereal *end2)
{
    /* System generated locals */
    doublereal ret_val, d__1, d__2;

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
/*      BRCKTD     O   Bracketed number. */

/* $ Detailed_Input */

/*      NUMBER      is the number to be bracketed. That is, the */
/*                  value of NUMBER is constrained to lie in the */
/*                  interval bounded by END1 and END2. */

/*      END1, */
/*      END2        are the lower and upper bounds for NUMBER. The */
/*                  order is not important. */

/* $ Detailed_Output */

/*      BRCKTD      is NUMBER, if it was already in the interval */
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

/*      The following illustrate the operation of BRCKTD. */

/*            BRCKTD (  -1.D0,   1.D0,  10.D0 )  =  1.D0 */
/*            BRCKTD (  29.D0,   1.D0,  10.D0 )  = 10.D0 */
/*            BRCKTD (   3.D0, -10.D0,  10.D0 )  =  3.D0 */
/*            BRCKTD (   3.D0, -10.D0,  -1.D0 )  = -1.D0 */

/*      The following code fragment illustrates a typical use for BRCKTD. */

/*            C */
/*            C     Star magnitude limit must be in the range 0-10. */
/*            C */
/*                  READ (5,*) MAGLIM */
/*                  MAGLIM = BRCKTD ( MAGLIM, 0.D0, 10.D0 ) */

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

/*     bracket a d.p. value within an interval */

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
	d__1 = *end1, d__2 = min(*end2,*number);
	ret_val = max(d__1,d__2);
    } else {
/* Computing MAX */
	d__1 = *end2, d__2 = min(*end1,*number);
	ret_val = max(d__1,d__2);
    }
    return ret_val;
} /* brcktd_ */

