/* wninsd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;

/* $Procedure      WNINSD ( Insert an interval into a DP window ) */
/* Subroutine */ int wninsd_(doublereal *left, doublereal *right, doublereal *
	window)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2;

    /* Local variables */
    integer card, size, i__, j;
    extern integer cardd_(doublereal *);
    extern /* Subroutine */ int chkin_(char *, ftnlen), errdp_(char *, 
	    doublereal *, ftnlen);
    extern integer sized_(doublereal *);
    extern /* Subroutine */ int scardd_(integer *, doublereal *), excess_(
	    integer *, char *, ftnlen), sigerr_(char *, ftnlen), chkout_(char 
	    *, ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*      Insert an interval into a double precision window. */

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

/*      WINDOWS */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      LEFT, */
/*      RIGHT      I   Left, right endpoints of new interval. */
/*      WINDOW    I,O  Input, output window. */

/* $ Detailed_Input */

/*      LEFT, */
/*      RIGHT       are the left and right endpoints of the interval */
/*                  to be inserted. */

/*      WINDOW      on input, is a window containing zero or more */
/*                  intervals. */

/* $ Detailed_Output */

/*      WINDOW      on output, is the original window following the */
/*                  insertion of the interval from LEFT to RIGHT. */

/* $ Parameters */

/*     None. */

/* $ Particulars */

/*      This routine inserts the interval from LEFT to RIGHT into the */
/*      input window. If the new interval overlaps any of the intervals */
/*      in the window, the intervals are merged. Thus, the cardinality */
/*      of the input window can actually decrease as the result of an */
/*      insertion. However, because inserting an interval that is */
/*      disjoint from the other intervals in the window can increase the */
/*      cardinality of the window, the routine signals an error. */

/*      This is the only unary routine to signal an error. No */
/*      other unary routine can increase the number of intervals in */
/*      the input window. */

/* $ Examples */

/*      Let WINDOW contain the intervals */

/*            [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ] */

/*      Then the following series of calls */

/*            CALL WNINSD ( 5,  5, WINDOW )                  (1) */
/*            CALL WNINSD ( 4,  8, WINDOW )                  (2) */
/*            CALL WNINSD ( 0, 30, WINDOW )                  (3) */

/*      produces the following series of windows */

/*            [ 1,  3 ]  [ 5,  5 ]  [  7, 11 ]  [ 23, 27 ]   (1) */
/*            [ 1,  3 ]  [ 4, 11 ]  [ 23, 27 ]               (2) */
/*            [ 0, 30 ]                                      (3) */

/* $ Exceptions */

/*     1) If LEFT is greater than RIGHT, the error SPICE(BADENDPOINTS) is */
/*        signalled. */

/*     2) If the insertion of the interval causes an excess of elements, */
/*        the error SPICE(WINDOWEXCESS) is signalled. */

/* $ Files */

/*      None. */

/* $ Restrictions */

/*      None. */

/* $ Literature_References */

/*      None. */

/* $ Author_and_Institution */

/*      K.R. Gehringer  (JPL) */
/*      N.J. Bachman    (JPL) */
/*      H.A. Neilan     (JPL) */
/*      W.L. Taber      (JPL) */
/*      I.M. Underwood  (JPL) */

/* $ Version */

/* -     Beta Version 1.3.0, 04-MAR-1993  (KRG) */

/*         There was a bug when moving the intervals in the cell */
/*         to the right when inserting a new interval to the left */
/*         of the left most interval. the incrementing in the DO */
/*         loop was incorrect. */

/*         The loop used to read: */

/*            DO J = I-1, CARD */
/*               WINDOW(J+2) = WINDOW(J) */
/*            END DO */

/*         which squashed everything to the right of the first interval */
/*         with the values of the first interval. */

/*         The loop now reads: */

/*            DO J = CARD, I-1, -1 */
/*               WINDOW(J+2) = WINDOW(J) */
/*            END DO */

/*         which correctly scoots the elements in reverse order, */
/*         preserving their values. */

/* -     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*         Comment section for permuted index source lines was added */
/*         following the header. */

/* -     SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     insert an interval into a d.p. window */

/* -& */
/* $ Revisions */

/* -     Beta Version 1.3.0, 04-MAR-1993  (KRG) */

/*         There was a bug when moving the intervals in the cell */
/*         to the right when inserting a new interval to the left */
/*         of the left most interval. the incrementing in the DO */
/*         loop was incorrect. */

/*         The loop used to read: */

/*            DO J = I-1, CARD */
/*               WINDOW(J+2) = WINDOW(J) */
/*            END DO */

/*         which squashed everything to the right of the first interval */
/*         with the values of the first interval. */

/*         The loop now reads: */

/*            DO J = CARD, I-1, -1 */
/*               WINDOW(J+2) = WINDOW(J) */
/*            END DO */

/*         which correctly scoots the elements in reverse order, */
/*         preserving their values. */

/* -     Beta Version 1.2.0, 27-FEB-1989  (HAN) */

/*         Due to the calling sequence and functionality changes */
/*         in the routine EXCESS, the method of signalling an */
/*         excess of elements needed to be changed. */

/* -     Beta Version 1.1.0, 17-FEB-1989 (HAN) (NJB) */

/*         Contents of the Required_Reading section was */
/*         changed from "None." to "WINDOWS".  Also, the */
/*         declaration of the unused variable K was removed. */
/* -& */

/*     SPICELIB functions */


/*     Local Variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("WNINSD", (ftnlen)6);
    }

/*     Get the size and cardinality of the window. */

    size = sized_(window);
    card = cardd_(window);

/*     Let's try the easy cases first. No input interval? No change. */
/*     Signal that an error has occurred and set the error message. */

    if (*left > *right) {
	setmsg_("Left endpoint was *. Right endpoint was *.", (ftnlen)42);
	errdp_("*", left, (ftnlen)1);
	errdp_("*", right, (ftnlen)1);
	sigerr_("SPICE(BADENDPOINTS)", (ftnlen)19);
	chkout_("WNINSD", (ftnlen)6);
	return 0;

/*     Empty window? Input interval later than the end of the window? */
/*     Just insert the interval, if there's room. */

    } else if (card == 0 || *left > window[card + 5]) {
	if (size >= card + 2) {
	    i__1 = card + 2;
	    scardd_(&i__1, window);
	    window[card + 6] = *left;
	    window[card + 7] = *right;
	} else {
	    excess_(&c__2, "window", (ftnlen)6);
	    sigerr_("SPICE(WINDOWEXCESS)", (ftnlen)19);
	}
	chkout_("WNINSD", (ftnlen)6);
	return 0;
    }

/*     Now on to the tougher cases. */

/*     Skip intervals which lie completely to the left of the input */
/*     interval. (The index I will always point to the right endpoint */
/*     of an interval). */

    i__ = 2;
    while(i__ <= card && window[i__ + 5] < *left) {
	i__ += 2;
    }

/*     There are three ways this can go. The new interval can: */

/*        1) lie entirely between the previous interval and the next. */

/*        2) overlap the next interval, but no others. */

/*        3) overlap more than one interval. */

/*     Only the first case can possibly cause an overflow, since the */
/*     other two cases require existing intervals to be merged. */


/*     Case (1). If there's room, move succeeding intervals back and */
/*     insert the new one. If there isn't room, signal an error. */

    if (*right < window[i__ + 4]) {
	if (size >= card + 2) {
	    i__1 = i__ - 1;
	    for (j = card; j >= i__1; --j) {
		window[j + 7] = window[j + 5];
	    }
	    i__1 = card + 2;
	    scardd_(&i__1, window);
	    window[i__ + 4] = *left;
	    window[i__ + 5] = *right;
	} else {
	    excess_(&c__2, "window", (ftnlen)6);
	    sigerr_("SPICE(WINDOWEXCESS)", (ftnlen)19);
	    chkout_("WNINSD", (ftnlen)6);
	    return 0;
	}

/*     Cases (2) and (3). */

    } else {

/*        The left and right endpoints of the new interval may or */
/*        may not replace the left and right endpoints of the existing */
/*        interval. */

/* Computing MIN */
	d__1 = *left, d__2 = window[i__ + 4];
	window[i__ + 4] = min(d__1,d__2);
/* Computing MAX */
	d__1 = *right, d__2 = window[i__ + 5];
	window[i__ + 5] = max(d__1,d__2);

/*        Skip any intervals contained in the one we modified. */
/*        (Like I, J always points to the right endpoint of an */
/*        interval.) */

	j = i__ + 2;
	while(j <= card && window[j + 5] <= window[i__ + 5]) {
	    j += 2;
	}

/*        If the modified interval extends into the next interval, */
/*        merge the two. (The modified interval grows to the right.) */

	if (j <= card && window[i__ + 5] >= window[j + 4]) {
	    window[i__ + 5] = window[j + 5];
	    j += 2;
	}

/*        Move the rest of the intervals forward to take up the */
/*        spaces left by the absorbed intervals. */

	while(j <= card) {
	    i__ += 2;
	    window[i__ + 4] = window[j + 4];
	    window[i__ + 5] = window[j + 5];
	    j += 2;
	}
	scardd_(&i__, window);
    }
    chkout_("WNINSD", (ftnlen)6);
    return 0;
} /* wninsd_ */

