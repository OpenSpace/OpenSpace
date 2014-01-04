/* wncard.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure WNCARD ( Cardinality of a double precision window ) */
integer wncard_(doublereal *window)
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    extern logical even_(integer *);
    extern integer cardd_(doublereal *);
    extern /* Subroutine */ int chkin_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen), 
	    errint_(char *, integer *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Return the cardinality (number of intervals) of a double */
/*     precision window. */

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

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     WINDOW     I   Input window. */

/*     The function returns the cardinality of the input window. */

/* $ Detailed_Input */

/*      WINDOW      is a window containing zero or more intervals. */

/* $ Detailed_Output */

/*     The function returns the cardinality of (number of intervals in) */
/*     the input window. */

/* $ Parameters */

/*      None. */

/* $ Exceptions */

/*     1) If the number of elements in WINDOW is not even */
/*        the error SPICE(INVALIDSIZE) signals. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The window cardinality (WNCARD) function simply wraps a CARD call */
/*     then divides the result by 2. A common error when using the SPICE */
/*     windows function is to use the CARDD value as the number of */
/*     window intervals rather than the CARDD/2 value. */

/* $ Examples */

/*      INTEGER               LBCELL */
/*      PARAMETER           ( LBCELL = -5 ) */

/*      INTEGER               WNSIZE */
/*      PARAMETER           ( WNSIZE = 10 ) */

/*      DOUBLE PRECISION      WINDOW      ( LBCELL:WNSIZE ) */
/*      DOUBLE PRECISION      LEFT */
/*      DOUBLE PRECISION      RIGHT */

/*      INTEGER               WNCARD */
/*      INTEGER               I */

/*      Validate the window with size WNSIZE and zero elements. */

/*      CALL WNVALD( WNSIZE, 0, WINDOW ) */

/*      Insert the intervals */

/*         [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ] */

/*      into WINDOW. */

/*      CALL WNINSD(  1.D0,  3.D0, WINDOW ) */
/*      CALL WNINSD(  7.D0, 11.D0, WINDOW ) */
/*      CALL WNINSD( 23.D0, 27.D0, WINDOW ) */

/*      Loop over the number of intervals in WINDOW, output */
/*      the LEFT and RIGHT endpoints for each interval. */

/*      DO I=1, WNCARD(WINDOW) */

/*         CALL WNFETD( WINDOW, I, LEFT, RIGHT ) */

/*         WRITE(*,*) 'Interval', I, ' [', LEFT, RIGHT, ']' */

/*      END DO */

/*   The code outputs: */

/*      Interval 1 [  1.    3.] */
/*      Interval 2 [  7.   11.] */
/*      Interval 3 [  23.  27.] */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     E.D. Wright    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 24-APR-2010 (EDW) */

/*       Minor edit to code comments eliminating typo. */

/* -    SPICELIB Version 1.0.0, 10-AUG-2007 (EDW) */

/* -& */
/* $ Index_Entries */

/*     cardinality of a d.p. window */

/* -& */

/*     SPICELIB functions */

    if (return_()) {
	ret_val = 0;
	return ret_val;
    } else {
	chkin_("WNCARD", (ftnlen)6);
    }
    ret_val = cardd_(window);

/*     Confirm the cardinality as an even integer. */

    if (! even_(&ret_val)) {
	setmsg_("Invalid window size, a window should have an even number of"
		" elements. The size was #.", (ftnlen)85);
	errint_("#", &ret_val, (ftnlen)1);
	sigerr_("SPICE(INVALIDSIZE)", (ftnlen)18);
	chkout_("WNCARD", (ftnlen)6);
	ret_val = 0;
	return ret_val;
    }

/*     Set return value. Cardinality in a SPICE window sense */
/*     means the number of intervals, half the cell */
/*     cardinality value. */

    ret_val /= 2;
    chkout_("WNCARD", (ftnlen)6);
    return ret_val;
} /* wncard_ */

