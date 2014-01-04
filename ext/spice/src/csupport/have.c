/* have.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static integer c__1 = 1;

/* $Procedure      HAVE ( Do we have an error? ) */
logical have_(char *error, ftnlen error_len)
{
    /* System generated locals */
    integer i__1;
    logical ret_val;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char name__[32];
    integer i__, depth;
    extern /* Subroutine */ int reset_(void);
    extern logical failed_(void);
    extern /* Subroutine */ int trcdep_(integer *), trcnam_(integer *, char *,
	     ftnlen), getlms_(char *, ftnlen), prefix_(char *, integer *, 
	    char *, ftnlen, ftnlen), getsms_(char *, ftnlen), suffix_(char *, 
	    integer *, char *, ftnlen, ftnlen);
    char sms[80];

/* $ Abstract */

/*     Determine if an error has occurred. */

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

/*     ERROR */

/* $ Keywords */

/*     ERROR */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     ERROR     I/O  Error message array. */

/*     The function returns .TRUE. if an error occurred. */

/* $ Detailed_Input */

/*     ERROR     is the character string array containing an error */
/*               message. */

/* $ Detailed_Output */

/*     ERROR     is the character string containing an error message. */
/*               If ERROR was blank on input and an error was detected */
/*               by the SPICELIB error handling mechanism, ERROR contains */
/*               the SPICELIB long error message on output. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     H.A. Neilan    (JPL) */

/* $ Version */

/* -     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 4, 1994 */


/* -    Beta Version 1.0.0, 14-MAY-1992 (HAN) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Check to see if an error occurred. */

    if (*(unsigned char *)&error[0] != ' ' || failed_()) {
	ret_val = TRUE_;
    } else {
	ret_val = FALSE_;
	return ret_val;
    }

/*     If an error was detected by the SPICELIB error handling and */
/*     the ERROR message is blank, we need to get the SPICELIB error */
/*     message. After that, reset the error handling. */

    if (failed_() && s_cmp(error, " ", error_len, (ftnlen)1) == 0) {
	getsms_(sms, (ftnlen)80);
	getlms_(error, error_len);
	prefix_("--", &c__0, error, (ftnlen)2, error_len);
	prefix_(sms, &c__0, error, (ftnlen)80, error_len);
	s_copy(error + error_len, "SPICELIB Trace>", error_len, (ftnlen)15);
	trcdep_(&depth);
	i__1 = depth;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    trcnam_(&i__, name__, (ftnlen)32);
	    if (i__ == 1) {
		suffix_(name__, &c__1, error + error_len, (ftnlen)32, 
			error_len);
	    } else {
		suffix_(name__, &c__0, error + error_len, (ftnlen)32, 
			error_len);
	    }
	    if (i__ != depth) {
		suffix_(":", &c__0, error + error_len, (ftnlen)1, error_len);
	    }
	}
	reset_();

/*     It is possible that FAILED() is true, even though we already */
/*     had a recorded error.  To avoid having this show up in a later */
/*     command, we reset the SPICELIB error handling now.  This isn't */
/*     really a good solution, but a better one doesn't come to mind */
/*     at the moment. */

    } else if (failed_()) {
	reset_();
    }
    return ret_val;
} /* have_ */

