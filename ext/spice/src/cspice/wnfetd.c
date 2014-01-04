/* wnfetd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      WNFETD ( Fetch an interval from a DP window ) */
/* Subroutine */ int wnfetd_(doublereal *window, integer *n, doublereal *left,
	 doublereal *right)
{
    integer card;
    extern integer cardd_(doublereal *);
    extern /* Subroutine */ int chkin_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);
    integer end;

/* $ Abstract */

/*     Fetch a particular interval from a double precision window. */

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

/*      WINDOWS */

/* $ Keywords */

/*     WINDOWS */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      WINDOW     I   Input window. */
/*      N          I   Index of interval to be fetched. */
/*      LEFT, */
/*      RIGHT      O   Left, right endpoints of the Nth interval. */

/* $ Detailed_Input */

/*      WINDOW      is a window containing zero or more intervals. */

/*      N           is the index of a particular interval within the */
/*                  window. Indices range from 1 to CARD(WINDOW)/2. */

/* $ Detailed_Output */

/*      LEFT, */
/*      RIGHT       are the left and right endpoints of the Nth interval */
/*                  in the input window. If the interval is not found, */
/*                  LEFT and RIGHT are not defined. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If N is less than one, the error SPICE(NOINTERVAL) signals. */

/*     2) If the interval does not exist, i.e. N > CARD(WINDOW)/2, the */
/*         error SPICE(NOINTERVAL) signals. */

/* $ Files */

/*      None. */

/* $ Particulars */

/*      None. */

/* $ Examples */

/*      Let A contain the intervals */

/*            [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ] */

/*      This window has a cardinality of 6, so N may have */
/*      value 1, 2, or 3 ( N =< CARD(WINDOW)/2 ). */

/*      Then the following calls */

/*            CALL WNFETD ( A,  1, LEFT, RIGHT )       [1] */
/*            CALL WNFETD ( A,  2, LEFT, RIGHT )       [2] */
/*            CALL WNFETD ( A,  3, LEFT, RIGHT )       [3] */

/*      yield the following values of LEFT and RIGHT */

/*            LEFT         RIGHT */
/*            ---------    --------- */
/*            1            3 */
/*            7            11 */
/*            23           27 */

/* $ Restrictions */

/*      None. */

/* $ Literature_References */

/*      None. */

/* $ Author_and_Institution */

/*      W.L. Taber      (JPL) */
/*      I.M. Underwood  (JPL) */

/* $ Version */

/* -     SPICELIB Version 1.0.3, 30-JUL-2007 (EDW) */

/*         Removed erroneous description in the Examples section */
/*         indicating "Undefined" as a return state after an error */
/*         event caused by an invalid value of N. */

/* -     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*         Comment section for permuted index source lines was added */
/*         following the header. */

/* -     SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     fetch an interval from a d.p. window */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Set up the error processing. */

    if (return_()) {
	return 0;
    }
    chkin_("WNFETD", (ftnlen)6);


/*     How many endpoints in the window? Enough? Normally, endpoints */
/*     of the Nth interval are stored in elements 2N and 2N-1. */

    card = cardd_(window);
    end = *n << 1;
    if (*n < 1 || card < end) {
	setmsg_("WNFETD: No such interval.", (ftnlen)25);
	sigerr_("SPICE(NOINTERVAL)", (ftnlen)17);
    } else {
	*left = window[end + 4];
	*right = window[end + 5];
    }
    chkout_("WNFETD", (ftnlen)6);
    return 0;
} /* wnfetd_ */

