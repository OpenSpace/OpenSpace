/* cnfirm_1.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      CNFIRM_1 ( Return status of a yes/no query ) */
/* Subroutine */ int cnfirm_1__(char *prmpt, logical *torf, ftnlen prmpt_len)
{
    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen);
    logical yesno;
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    char respns[256];
    extern /* Subroutine */ int prompt_(char *, char *, ftnlen, ftnlen);

/* $ Abstract */

/*     Return the .TRUE./.FALSE. status of a query which has a yes/no */
/*     response. */

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

/*     PARSING */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     PRMPT      I   The prompt used to elicit a yes/no response. */
/*     TORF       O   The truth value of a yes/no response. */

/* $ Detailed_Input */

/*     PRMPT    The prompt which is used to elicit a yes/no response. */

/* $ Detailed_Output */

/*     TORF     A logical flag which indicates the truth value of a */
/*              yes/no response to a continue/try again prompt. If the */
/*              response was equivalent to yes, TORF = .TRUE.. If the */
/*              response was equivalent to no, TORF = .FALSE.. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1)   Any input value that is not equivalent to 'Y', 'YES', 'N' */
/*          or 'NO' (or lower case equivalents), will cause the routine */
/*          to redisplay the prompt. A yes/no response MUST be given, */
/*          there are no implicit values for any other response. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Often a program needs to ask whether or not a user wishes */
/*     to exercise some option. This routine simplifies the task */
/*     of converting the answer to a logical value. */

/*     If the response to a yes/no question is logically equivalent */
/*     to 'YES' the variable TORF will be set to a value of .TRUE. */
/*     If the response to a yes/no question is logically equivalent */
/*     to 'NO' the variable TORF will be set to a value of .FALSE. */
/*     Any other response will cause the routine to redisplay the */
/*     prompt. */

/* $ Examples */

/*     Suppose you need to ask a user whether or not diagnostic */
/*     information about the behaviour of a program should be */
/*     written to a file.  Using this routine, you can easily */
/*     take the action desired and avoid the details of parsing */
/*     the user's answer. */

/*        PRMPT = 'Log information to a file? (Yes/No) ' */
/*        OK = .FALSE. */
/*        CALL CONFRM( PRMPT, OK ) */

/*        IF ( OK ) THEN */

/*        ...enable recording diagnostics in the log file. */

/*        ELSE */

/*        ...disable recording of diagnostics. */

/*        END IF */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     K.R. Gehringer (JPL) */
/*     W.L. Taber     (JPL) */

/* $ Version */

/* -     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 9, 1994 */


/* -     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of META/2 */
/*         software as of May 3, 1994 */


/* -    Beta Version 1.0.0, 09-SEP-1992 (KRG) */

/* -& */
/* $ Index_Entries */

/*      prompt with a yes/no query and return logical response */

/* -& */

/*     SPICELIB functions */

/*     None. */


/*     Local Parameters */


/*     Local Variables */


/*     Do while we have not gotten a yes/no response */

    yesno = FALSE_;
    while(! yesno) {

/*        Prompt for a response */

	prompt_(prmpt, respns, prmpt_len, (ftnlen)256);

/*        Left justify the response string, RESPNS, and convert it to */
/*        uppercase. */

	ljust_(respns, respns, (ftnlen)256, (ftnlen)256);
	ucase_(respns, respns, (ftnlen)256, (ftnlen)256);
	if (s_cmp(respns, "Y", (ftnlen)256, (ftnlen)1) == 0 || s_cmp(respns, 
		"YES", (ftnlen)256, (ftnlen)3) == 0) {
	    *torf = TRUE_;
	    yesno = TRUE_;
	} else if (s_cmp(respns, "N", (ftnlen)256, (ftnlen)1) == 0 || s_cmp(
		respns, "NO", (ftnlen)256, (ftnlen)2) == 0) {
	    *torf = FALSE_;
	    yesno = TRUE_;
	}
    }
    return 0;
} /* cnfirm_1__ */

