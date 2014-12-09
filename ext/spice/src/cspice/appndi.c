/* appndi.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      APPNDI ( Append an item to an integer cell ) */
/* Subroutine */ int appndi_(integer *item, integer *cell)
{
    extern integer cardi_(integer *);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer sizei_(integer *);
    extern /* Subroutine */ int scardi_(integer *, integer *);
    integer nwcard;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Append an item to an integer cell. */

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

/*     CELLS */

/* $ Keywords */

/*     CELLS */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ITEM       I   The item to append. */
/*     CELL      I/O  The cell to which ITEM will be appended. */

/* $ Detailed_Input */

/*     ITEM       is an integer value which is to be appended to CELL. */

/*     CELL       is an integer cell to which ITEM will be appended. */

/* $ Detailed_Output */

/*     CELL       is an integer cell in which the last element is ITEM. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) If the cell is not big enough to accommodate the addition */
/*        of a new element, the error SPICE(CELLTOOSMALL) is signaled. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*      In the following example, the element 34 is appended to */
/*      the integer cell NUMBERS. */

/*      Before appending 34, the cell contains: */

/*      NUMBERS (1) =  1 */
/*      NUMBERS (2) =  1 */
/*      NUMBERS (3) =  2 */
/*      NUMBERS (4) =  3 */
/*      NUMBERS (5) =  5 */
/*      NUMBERS (6) =  8 */
/*      NUMBERS (7) = 13 */
/*      NUMBERS (8) = 21 */

/*      The call */

/*        CALL APPNDI ( 34, NUMBERS ) */

/*      appends the element 34 at the location NUMBERS (9), and the */
/*      cardinality is updated. */

/*      If the cell is not big enough to accommodate the addition of */
/*      the item, an error is signaled. In this case, the cell is not */
/*      altered. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     H.A. Neilan     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.2, 31-JUL-2002 (NJB) */

/*        Corrected miscellaneous typos in header and in the long */
/*        error message text. */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (HAN) */

/* -& */
/* $ Index_Entries */

/*     append an item to an integer cell */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("APPNDI", (ftnlen)6);
    }

/*     Check to see if the cell can accommodate the addition of a */
/*     new item. If there is room, append the item to the cell and */
/*     reset the cardinality. If the cell cannot accommodate the */
/*     addition of a new item, signal an error. */

    nwcard = cardi_(cell) + 1;
    if (nwcard <= sizei_(cell)) {
	cell[nwcard + 5] = *item;
	scardi_(&nwcard, cell);
    } else {
	setmsg_("The cell cannot accommodate the addition of the element *. ",
		 (ftnlen)59);
	errint_("*", item, (ftnlen)1);
	sigerr_("SPICE(CELLTOOSMALL)", (ftnlen)19);
    }
    chkout_("APPNDI", (ftnlen)6);
    return 0;
} /* appndi_ */

