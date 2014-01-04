/* insrtc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      INSRTC ( Insert an item into a character set ) */
/* Subroutine */ int insrtc_(char *item, char *a, ftnlen item_len, ftnlen 
	a_len)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer i_len(char *, ftnlen), s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer card, slen, last, size, i__;
    extern integer cardc_(char *, ftnlen);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer sizec_(char *, ftnlen);
    logical in;
    extern /* Subroutine */ int scardc_(integer *, char *, ftnlen);
    extern integer lstlec_(char *, integer *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Insert an item into a character set. */

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

/*     SETS */

/* $ Keywords */

/*     CELLS, SETS */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ITEM       I   Item to be inserted. */
/*     A         I/O  Insertion set. */

/* $ Detailed_Input */

/*     ITEM        is an item which is to be inserted into the */
/*                 specified set. ITEM may or may not already be an */
/*                 element of the set.  If ITEM is longer than the */
/*                 length SLEN of the elements of A, only the substring */
/*                 consisting of the first SLEN characters of ITEM will */
/*                 be inserted into the set; any trailing non-blank */
/*                 characters in ITEM are ignored. */


/*     A           is a set. */

/*                 On input, A may or may not contain the input item */
/*                 as an element. */

/* $ Detailed_Output */

/*     A           on output contains the union of the input set and */
/*                 the singleton set containing the input item, unless */
/*                 there was not sufficient room in the set for the */
/*                 item to be included, in which case the set is not */
/*                 changed and an error is signaled. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the insertion of the item into the set causes an excess */
/*        of elements, the error SPICE(SETEXCESS) is signaled. */

/*     2) If the item to be inserted has greater length than the string */
/*        length of the elements of the set, the item will be truncated */
/*        on the right when it is inserted.  The insertion point of */
/*        the element will be determined by the comparison of the */
/*        truncated item to members of the set.  If, after truncation, */
/*        the item to be inserted matches an element already present */
/*        in the set, no insertion occurs. */

/* $ Files */

/*      None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     In the following example, the element 'PLUTO' is removed from */
/*     the character set PLANETS and inserted into the character set */
/*     ASTEROIDS. */

/*        CALL REMOVC ( 'PLUTO', PLANETS   ) */
/*        CALL INSRTC ( 'PLUTO', ASTEROIDS ) */

/*     If 'PLUTO' is not an element of PLANETS, then the contents of */
/*     PLANETS are not changed. Similarly, if 'PLUTO' is already an */
/*     element of ASTEROIDS, the contents of ASTEROIDS remain unchanged. */

/*     Because inserting an element into a set can increase the */
/*     cardinality of the set, an error may occur in the insertion */
/*     routines. */

/* $ Literature_References */

/*      None. */

/* $ Restrictions */

/*      None. */

/* $ Author_and_Institution */

/*      N.J. Bachman    (JPL) */
/*      C.A. Curzon     (JPL) */
/*      W.L. Taber      (JPL) */
/*      I.M. Underwood  (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 01-NOV-2005 (NJB) */

/*        Bug fix:  when the item to be inserted would, after */
/*        truncation to the set's string length, match an item */
/*        already in the set, no insertion is performed.  Previously */
/*        the truncated string was inserted, corrupting the set. */

/*        Long error message was updated to include size of */
/*        set into which insertion was attempted. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     insert an item into a character set */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 2.0.0, 01-NOV-2005 (NJB) */

/*        Bug fix:  when the item to be inserted would, after */
/*        truncation to the set's string length, match an item */
/*        already in the set, no insertion is performed.  Previously */
/*        the truncated string was inserted, corrupting the set. */

/*        Long error message was updated to include size of */
/*        set into which insertion was attempted. */

/* -    Beta Version 1.1.0, 06-JAN-1989 (NJB) */

/*        Calling protocol of EXCESS changed.  Call to SETMSG removed. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Set up the error processing. */

    if (return_()) {
	return 0;
    }
    chkin_("INSRTC", (ftnlen)6);

/*     What are the size and cardinality of the set? */

    size = sizec_(a, a_len);
    card = cardc_(a, a_len);

/*     When we insert an item into the set, any trailing characters */
/*     that don't fit are truncated.  So in deciding where to insert */
/*     the item, we ignore any characters that won't remain after */
/*     insertion. */

/*     We're going to consider only the initial substring of ITEM */
/*     whose length doesn't exceed the string length of the set's */
/*     members. */

/* Computing MIN */
    i__1 = i_len(item, item_len), i__2 = i_len(a + a_len * 6, a_len);
    slen = min(i__1,i__2);

/*     Find the last element of the set which would come before the */
/*     input item. This will be the item itself, if it is already an */
/*     element of the set. */

    last = lstlec_(item, &card, a + a_len * 6, slen, a_len);

/*     Is the item already in the set? If not, it needs to be inserted. */

    if (last > 0) {
	in = s_cmp(a + (last + 5) * a_len, item, a_len, slen) == 0;
    } else {
	in = FALSE_;
    }
    if (! in) {

/*        If there is room in the set for the new element, then move */
/*        the succeeding elements back to make room. And update the */
/*        cardinality for future reference. */

	if (card < size) {
	    i__1 = last + 1;
	    for (i__ = card; i__ >= i__1; --i__) {
		s_copy(a + (i__ + 6) * a_len, a + (i__ + 5) * a_len, a_len, 
			a_len);
	    }
	    s_copy(a + (last + 6) * a_len, item, a_len, slen);
	    i__1 = card + 1;
	    scardc_(&i__1, a, a_len);
	} else {
	    setmsg_("An element could not be inserted into the set due to la"
		    "ck of space; set size is #.", (ftnlen)82);
	    errint_("#", &size, (ftnlen)1);
	    sigerr_("SPICE(SETEXCESS)", (ftnlen)16);
	}
    }
    chkout_("INSRTC", (ftnlen)6);
    return 0;
} /* insrtc_ */

