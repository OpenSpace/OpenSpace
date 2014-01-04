/* wnfild.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      WNFILD ( Fill small gaps in a DP window ) */
/* Subroutine */ int wnfild_(doublereal *small, doublereal *window)
{
    integer card, i__, j;
    extern integer cardd_(doublereal *);
    extern /* Subroutine */ int chkin_(char *, ftnlen), scardd_(integer *, 
	    doublereal *), chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Fill small gaps between adjacent intervals of a double precision */
/*     window. */

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

/*     WINDOWS */

/* $ Keywords */

/*     WINDOWS */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      SMALL      I   Limiting measure of small gaps. */
/*      WINDOW    I,O  Window to be filled. */

/* $ Detailed_Input */

/*      SMALL       is the limiting measure of the small gaps to be */
/*                  filled. Adjacent intervals separated by gaps of */
/*                  measure less than or equal to SMALL are merged. */

/*      WINDOW      on input, is a window containing zero or more */
/*                  intervals. */

/* $ Detailed_Output */

/*      WINDOW      on output, is the original window, after adjacent */
/*                  intervals separated by small gaps have been merged. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*      None. */

/* $ Particulars */

/*      This routine removes small gaps between adjacent intervals */
/*      by merging intervals separated by gaps of measure less than */
/*      or equal to the limiting measure (SMALL). */

/* $ Examples */

/*      Let WINDOW contain the intervals */

/*            [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]  [ 29, 29 ] */

/*      Then the following series of calls */

/*            CALL WNFILD (  1, WINDOW )                       (1) */
/*            CALL WNFILD (  2, WINDOW )                       (2) */
/*            CALL WNFILD (  3, WINDOW )                       (3) */
/*            CALL WNFILD ( 12, WINDOW )                       (4) */

/*      produces the following series of windows */

/*            [ 1,  3 ]  [ 7, 11 ]  [ 23, 27 ]  [ 29, 29 ]     (1) */
/*            [ 1,  3 ]  [ 7, 11 ]  [ 23, 29 ]                 (2) */
/*            [ 1,  3 ]  [ 7, 11 ]  [ 23, 29 ]                 (3) */
/*            [ 1, 29 ]                                        (4) */

/* $ Restrictions */

/*      None. */

/* $ Literature_References */

/*      None. */

/* $ Author_and_Institution */

/*      N.J. Bachman    (JPL) */
/*      H.A. Neilan     (JPL) */
/*      W.L. Taber      (JPL) */
/*      I.M. Underwood  (JPL) */

/* $ Version */

/* -     SPICELIB Version 1.0.3, 29-JUL-2007 (NJB) */

/*         Corrected typo in the previous Version line date string, */
/*         "29-JUL-20022" to "29-JUL-2002." */

/* -     SPICELIB Version 1.0.2, 29-JUL-2002 (NJB) */

/*         Changed gap size from 10 to 12 to correct erroneous example. */


/* -     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*         Comment section for permuted index source lines was added */
/*         following the header. */

/* -     SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     fill small gaps in a d.p. window */

/* -& */
/* $ Revisions */

/* -     Beta Version 1.2.0, 24-FEB-1989  (HAN) */

/*         Added calls to CHKIN and CHKOUT. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("WNFILD", (ftnlen)6);
    }

/*     Get the cardinality of the window. (The size is not important; */
/*     this routine can't create any new intervals.) */

    card = cardd_(window);

/*     Step through the window, looking for the next right endpoint */
/*     more than SMALL away from the following left endpoint. This marks */
/*     the end of the new first interval, and the beginning of the new */
/*     second interval. Keep this up until the last right endpoint has */
/*     been reached. This remains the last right endpoint. */

    if (card > 0) {
	i__ = 2;
	j = 2;
	while(j < card) {
	    if (window[j + 5] + *small < window[j + 6]) {
		window[i__ + 5] = window[j + 5];
		window[i__ + 6] = window[j + 6];
		i__ += 2;
	    }
	    j += 2;
	}
	window[i__ + 5] = window[j + 5];
	scardd_(&i__, window);
    }
    chkout_("WNFILD", (ftnlen)6);
    return 0;
} /* wnfild_ */

