/* prtrap.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure     PRTRAP */
/* Subroutine */ int prtrap_(char *command, logical *tran, ftnlen command_len)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);

    /* Local variables */
    char word[33*3];
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), ucase_(char *, char *,
	     ftnlen, ftnlen), errch_(char *, char *, ftnlen, ftnlen), nthwd_(
	    char *, integer *, char *, integer *, ftnlen, ftnlen);
    extern integer rtrim_(char *, ftnlen);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    integer loc;

/* $ Abstract */

/*     Determine whether the given command should be trapped (left */
/*     untranslated). */

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

/* $ Keywords */

/*     PERCY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     COMMND     I   PERCY command to be evaluated. */
/*     TRAN       I   True if further translation is needed. */

/* $ Detailed_Input */

/*     COMMAND    is the input PERCY command. The following commands */
/*                should not be translated fully. (A moment's thought */
/*                will show why.) */

/*                        - SHOW SYMBOL <symbol> */

/*                        - INQUIRE <symbol> */

/*                If translation has proceeded far enough for either */
/*                of these statements to be recognized, then it has */
/*                gone far enough. */

/* $ Detailed_Output */

/*     TRAN       is true if further translation of COMMAND is okay. */
/*                If any of the statements mentioned above is recognized, */
/*                TRAN is false. (This will prevent PERCY from trying */
/*                to resolve any more symbols.) */

/* $ Input_Files */

/*     None. */

/* $ Output_Files */

/*     None. */

/* $ Input_Output_Common */

/*     See 'SYMBOLS.INC'. */

/* $ Detailed_Description */

/*     Get the first three words of COMMAND. */

/*         - If the first two words are SHOW SYMBOL, and the */
/*           third word is not blank and does not end with '?', */
/*           then this should be trapped. */

/*         - If the first word is INQUIRE and the second word */
/*           is not blank and does not end with '?', then this */
/*           should be trapped. */

/*     If the statement should be trapped, set TRAN to false and return. */

/* $ Examples */

/*     Command                                 Trap? */
/*     ------------------------------------    ----- */
/*     'SHOW SYMBOL CARROT        '              Y */
/*     'SHOW SYMBOL               '              N */
/*     'SHOW SYMBOL SYMBOL_NAME?  '              N */

/*     'INQUIRE PRIMARY_PLANET    '              N */
/*     'INQUIRE                   '              Y */
/*     'INQUIRE QUERY_NAME?       '              Y */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     W. L. Taber     (JPL) */
/*     I. M. Underwood (JPL) */

/* $ Version_and_Date */

/*     Version 1, 17-SEP-1986 */

/* -& */

/*     Spicelib Functions */


/*     Local variables */


/*     Get the first three words of COMMAND. */

    for (i__ = 1; i__ <= 3; ++i__) {
	nthwd_(command, &i__, word + ((i__1 = i__ - 1) < 3 && 0 <= i__1 ? 
		i__1 : s_rnge("word", i__1, "prtrap_", (ftnlen)144)) * 33, &
		loc, command_len, (ftnlen)33);
	ucase_(word + ((i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
		"word", i__1, "prtrap_", (ftnlen)145)) * 33, word + ((i__2 = 
		i__ - 1) < 3 && 0 <= i__2 ? i__2 : s_rnge("word", i__2, "prt"
		"rap_", (ftnlen)145)) * 33, (ftnlen)33, (ftnlen)33);
    }

/*     Is this a SHOW SYMBOL command? */

    if (s_cmp(word, "SHOW", (ftnlen)33, (ftnlen)4) == 0 && s_cmp(word + 33, 
	    "SYMBOL", (ftnlen)33, (ftnlen)6) == 0) {

/*        The third word must not be blank, and must not end with '?'. */
/*        (WORD is longer than any allowable symbol or query, so there */
/*        should always be a blank at the end.) */

	if (s_cmp(word + 66, " ", (ftnlen)33, (ftnlen)1) != 0) {
	    loc = rtrim_(word + 66, (ftnlen)33);
	    if (*(unsigned char *)&word[loc + 65] != '?') {
		*tran = FALSE_;
		return 0;
	    }
	}

/*     Is this an INQUIRE command? */

    } else if (s_cmp(word, "INQUIRE", (ftnlen)33, (ftnlen)7) == 0) {

/*        The second word must not be blank, and must not end with '?'. */

	if (s_cmp(word + 33, " ", (ftnlen)33, (ftnlen)1) != 0) {
	    loc = rtrim_(word + 33, (ftnlen)33);
	    if (*(unsigned char *)&word[loc + 32] == '?') {
		*tran = FALSE_;
		chkin_("PRTRAP", (ftnlen)6);
		setmsg_("INQUIRE commands must be of the form INQUIRE <symbo"
			"l_name>,  You have INQUIRE # which is inquiring for "
			"the value of a query. This kind of INQUIRE is not su"
			"pported. ", (ftnlen)164);
		errch_("#", word + 33, (ftnlen)1, (ftnlen)33);
		sigerr_("INVALID_INQUIRE", (ftnlen)15);
		chkout_("PRTRAP", (ftnlen)6);
		return 0;
	    }
	}
    }

/*     No reason to trap this. */

    *tran = TRUE_;
    return 0;
} /* prtrap_ */

