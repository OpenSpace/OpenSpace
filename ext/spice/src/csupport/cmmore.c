/* cmmore.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure      CMMORE ( Command Loop---More Commands) */
logical cmmore_(char *commnd, ftnlen commnd_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    integer i__1;
    logical ret_val;

    /* Builtin functions */
    integer i_len(char *, ftnlen), s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static char exit[32];
    static integer i__, r__;
    extern logical nechr_(char *, char *, ftnlen, ftnlen);
    extern integer rtrim_(char *, ftnlen);
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    static integer lc;
    extern /* Subroutine */ int trnlat_(char *, char *, ftnlen, ftnlen), 
	    cmprss_(char *, integer *, char *, char *, ftnlen, ftnlen, ftnlen)
	    ;

/* $ Abstract */

/*    Determine whether or not more command loop processing */
/*    should be performed. */

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

/*     Command Loop */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     COMMND     I   A command to be processed by CMLOOP */

/*     The function returns .TRUE. if the command is not the "exit" */
/*     command. If it is the exit command it returns .FALSE. */

/* $ Detailed_Input */

/*     COMMND     A commmand that should be acted on by CMLOOP */


/* $ Detailed_Output */

/*     The function returns .TRUE. if this is not the exit command. */
/*     The meaning being "there is still more to do in CMLOOP." */

/*     If the input command is equivalent to the exit command */
/*     (Same words when converted to uppercase) The function */
/*     returns .FALSE.  The intended meaning is "there is nothing */
/*     left for CMLOOP to do but cleanup and return." */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Particulars */

/*     This is utility function for use by CMLOOP.  It is the */
/*     function tested each pass through the loop to see if the */
/*     loop has finished its work */

/* $ Examples */

/*     See CMLOOP.  There is no other use for this function. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*       W.L. Taber      (JPL) */

/* $ Literature_References */

/*       None. */

/* $ Version */

/* -    Command Loop Version 1.0.0, 4-AUG-1995 (WLT) */


/* -& */
/* $ Index_Entries */

/*     More command processing required */

/* -& */
/*     SPICELIB Functions */


/*     Local Variables. */


/*     On the first pass we fetch the "exit" command and */
/*     spruce it up a bit for use when comparing with */
/*     the input command. */

    if (first) {
	first = FALSE_;
	trnlat_("EXIT", exit, (ftnlen)4, (ftnlen)32);
	cmprss_(" ", &c__1, exit, exit, (ftnlen)1, (ftnlen)32, (ftnlen)32);
	ljust_(exit, exit, (ftnlen)32, (ftnlen)32);
	r__ = rtrim_(exit, (ftnlen)32);
    }

/*     If the input command is shorter than the non-blank */
/*     length of EXIT, then this cannot be the exit command. */
/*     There is more to do. */

/*     Note we assign a value to CMMORE so that the compiler */
/*     won't have a fit about having a function unassigned. */
/*     The if conditions below ensure that we assign a value */
/*     but most compilers aren't smart enough to figure that */
/*     out. */

    ret_val = TRUE_;
    lc = i_len(commnd, commnd_len);
    if (lc < r__) {
	ret_val = TRUE_;
	return ret_val;
    }

/*     Check to see if the input command matches the exit command. */
/*     We do this a character at a time.  We search from the */
/*     left to right, because most commands are not EXIT and this */
/*     allows us to quit early in the process. */

    i__1 = r__;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (nechr_(commnd + (i__ - 1), exit + (i__ - 1), (ftnlen)1, (ftnlen)1)
		) {
	    ret_val = TRUE_;
	    return ret_val;
	}
    }

/*     It's looking like this might be it.  See if the rest of */
/*     the input command is blank. */

    if (lc == r__) {

/*        We've got an exact match.  There are no more commands */
/*        to look at. */

	ret_val = FALSE_;
    } else if (lc > r__) {

/*        There will be more commands only if the rest of the input */
/*        command is non-blank. */

	i__1 = r__;
	ret_val = s_cmp(commnd + i__1, " ", commnd_len - i__1, (ftnlen)1) != 
		0;
    }
    return ret_val;
} /* cmmore_ */

