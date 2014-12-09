/* removd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      REMOVD ( Remove an item from a double precision set ) */
/* Subroutine */ int removd_(doublereal *item, doublereal *a)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer card, i__;
    extern integer cardd_(doublereal *);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    logical in;
    extern /* Subroutine */ int scardd_(integer *, doublereal *);
    extern integer bsrchd_(doublereal *, integer *, doublereal *);
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);
    integer loc;

/* $ Abstract */

/*      Remove an item from a double precision set. */

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

/*      SETS */

/* $ Keywords */

/*      CELLS, SETS */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      ITEM       I   Item to be removed. */
/*      A         I/O  Removal set. */
/*      ERROR      O   Error flag. */

/* $ Detailed_Input */

/*      ITEM        is an item which is to be removed from the */
/*                  specified set. ITEM may or may not already */
/*                  be an element of the set. */


/*      A           is a set. */


/*                  On input, A may or may not contain the input item */
/*                  as an element. */

/* $ Detailed_Output */

/*      A           on output contains the difference of the input set */
/*                  and the input item. If the item is not an element of */
/*                  the set, the set is not changed. */

/* $ Parameters */

/*      None. */

/* $ Particulars */

/*      None. */

/* $ Examples */

/*      In the following example, the element 'PLUTO' is removed from */
/*      the character set PLANETS and inserted into the character set */
/*      ASTEROIDS. */

/*            CALL REMOVC ( 'PLUTO', PLANETS          ) */
/*            CALL INSRTC ( 'PLUTO', ASTEROIDS, ERROR ) */

/*      If 'PLUTO' is not an element of PLANETS, then the contents of */
/*      PLANETS are not changed. Similarly, if 'PLUTO' is already an */
/*      element of ASTEROIDS, the contents of ASTEROIDS remain unchanged. */

/*      Because inserting an element into a set can increase the */
/*      cardinality of the set, the insertion routines return an */
/*      error flag. The flag is blank if the set is large enough to */
/*      hold the new element. Otherwise, a message (constructed by */
/*      the cell routine EXCESS) is returned. */

/* $ Restrictions */

/*      None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*      None. */

/* $ Literature_References */

/*      None. */

/* $ Author_and_Institution */

/*      N.J. Bachman    (JPL) */
/*      C.A. Curzon     (JPL) */
/*      W.L. Taber      (JPL) */
/*      I.M. Underwood  (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     remove an item from a d.p. set */

/* -& */
/* $ Revisions */

/* -    Beta Version 2.0.0, 13-MAR-1989 (NJB) */

/*        Now participates in error handling.  References to RETURN, */
/*        CHKIN, and CHKOUT added. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard error handling: */

    if (return_()) {
	return 0;
    } else {
	chkin_("REMOVD", (ftnlen)6);
    }

/*     What is the cardinality of the set? */

    card = cardd_(a);

/*     Determine the location (if any) of the item within the set. */

    loc = bsrchd_(item, &card, &a[6]);

/*     Is the item in the set? If so, it needs to be removed. */

    in = loc > 0;
    if (in) {

/*        Move succeeding elements forward to take up the slack left */
/*        by the departing element. And update the cardinality for */
/*        future reference. */

	i__1 = card - 1;
	for (i__ = loc; i__ <= i__1; ++i__) {
	    a[i__ + 5] = a[i__ + 6];
	}
	i__1 = card - 1;
	scardd_(&i__1, a);
    }
    chkout_("REMOVD", (ftnlen)6);
    return 0;
} /* removd_ */

