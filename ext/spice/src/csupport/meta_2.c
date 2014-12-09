/* meta_2.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__0 = 0;
static integer c__10 = 10;
static logical c_true = TRUE_;
static integer c__6 = 6;
static integer c__5 = 5;
static integer c__32 = 32;
static integer c__9 = 9;
static integer c__3 = 3;

/* $Procedure      META_2 ( Percy's interface to META_0 ) */
/* Subroutine */ int meta_2__0_(int n__, char *command, char *temps, integer *
	ntemps, char *temp, integer *btemp, char *error, ftnlen command_len, 
	ftnlen temps_len, ftnlen temp_len, ftnlen error_len)
{
    /* Initialized data */

    static logical pass1 = TRUE_;
    static char margns[128] = "LEFT 1 RIGHT 75                              "
	    "                                                                "
	    "                   ";
    static char keynam[6*10] = "1     " "2     " "3     " "4     " "5     " 
	    "6     " "7     " "8     " "9     " "10    ";

    /* System generated locals */
    address a__1[5];
    integer i__1, i__2[5];

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_wsle(cilist *), e_wsle(
	    void);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);
    integer do_lio(integer *, integer *, char *, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int getopt_1__(char *, integer *, char *, integer 
	    *, char *, integer *, char *, char *, ftnlen, ftnlen, ftnlen, 
	    ftnlen, ftnlen);
    static integer sbeg;
    static char mode[16], pick[32];
    static integer b, e, i__, j;
    extern integer cardc_(char *, ftnlen);
    extern logical batch_(void);
    static integer score;
    static logical fixit;
    extern integer rtrim_(char *, ftnlen);
    static char style[128];
    static integer m2code;
    static char tryit[600];
    extern /* Subroutine */ int m2gmch_(char *, char *, char *, integer *, 
	    logical *, integer *, logical *, integer *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen, ftnlen), m2rcvr_(integer *, integer *, 
	    char *, ftnlen), scardc_(integer *, char *, ftnlen);
    static integer bscore, cutoff;
    static logical reason;
    extern /* Subroutine */ int prefix_(char *, integer *, char *, ftnlen, 
	    ftnlen), ssizec_(integer *, char *, ftnlen), repsub_(char *, 
	    integer *, integer *, char *, char *, ftnlen, ftnlen, ftnlen);
    static logical intrct;
    extern /* Subroutine */ int suffix_(char *, integer *, char *, ftnlen, 
	    ftnlen);
    static char thnwds[32*7], kwords[32*16];
    extern /* Subroutine */ int cmprss_(char *, integer *, char *, char *, 
	    ftnlen, ftnlen, ftnlen), prepsn_(char *, ftnlen);
    static logical pssthn;
    static char questn[80];
    extern /* Subroutine */ int niceio_3__(char *, integer *, char *, ftnlen, 
	    ftnlen), cnfirm_1__(char *, logical *, ftnlen);

    /* Fortran I/O blocks */
    static cilist io___19 = { 0, 6, 0, 0, 0 };
    static cilist io___20 = { 0, 6, 0, 0, 0 };
    static cilist io___21 = { 0, 6, 0, 0, 0 };
    static cilist io___22 = { 0, 6, 0, 0, 0 };
    static cilist io___23 = { 0, 6, 0, 0, 0 };
    static cilist io___27 = { 0, 6, 0, 0, 0 };
    static cilist io___29 = { 0, 6, 0, 0, 0 };
    static cilist io___30 = { 0, 6, 0, 0, 0 };
    static cilist io___31 = { 0, 6, 0, 0, 0 };


/* $ Abstract */

/*     Given a collection of acceptable syntax's and a statement */
/*     (COMMAND) this routine determines if the statement is */
/*     syntactically correct. */

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

/*     The META/2 Book. */

/* $ Keywords */

/*     COMPARE */
/*     PARSING */
/*     SEARCH */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     COMMAND    I   A candidate PERCY command. */
/*     TEMPS      I   A collection of language definition statements */
/*     NTEMPS     I   The number of definition statements */
/*     TEMP       -   Work space required for comparison of statements. */
/*     BTEMP      O   The first of the def statements that best matches. */
/*     ERROR      O   Non-blank if none of the def's match. */

/* $ Detailed_Input */

/*     COMMAND    A candidate PERCY command. */
/*     TEMPS      A collection of language definition statements */
/*     NTEMPS     The number of definition statements */
/*     TEMP       Work space required for comparison of statements. */
/*                TEMP should be declared to have the same length */
/*                as the character strings that make up TEMPS. */

/* $ Detailed_Output */

/*     BTEMP      The first of the def statements that best matches. */
/*     ERROR      Non-blank if none of the def's match. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Particulars */

/*     Later. */

/* $ Examples */

/*     Later. */

/* $ Restrictions */



/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     H.A. Neilan    (JPL) */
/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */

/* $ Version */

/* -     META/2 Configured Version 3.0.0, 11-AUG-1995 (WLT) */

/*         The control flow through this routine was modified */
/*         so that it will now re-try all templates (starting */
/*         with the best previous match) if a spelling error */
/*         is encountered.  This should fix the confused */
/*         responses that META/2 gave occassionally before. */

/* -     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 9, 1994 */

/* -     META/2 Configured Version 2.0.0, 9-MAY-1994 */

/*         Added a pretty print formatting capability to the */
/*         error diagnostics. */

/* -     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of META/2 */
/*         software as of May 3, 1994 */


/* -    Beta Version 2.0.0, 14-JAN-1993 (HAN) */

/*        Assigned the value 'INTERACTIVE' to the variable MODE, and */
/*        replaced calls to VTLIB routines with calls to more */
/*        portable routines. */

/* -    Beta Version 1.0.0, 13-JUL-1988 (WLT) (IMU) */

/* -& */

/*     Spice Functions */


/*     Local variables. */


/*     Saved variables */


/*     Initial values */

    /* Parameter adjustments */
    if (temps) {
	}
    if (error) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_m2marg;
	}

/* %&END_DECLARATIONS */

/*     Take care of first pass initializations. */

    if (pass1) {
	pass1 = FALSE_;
	ssizec_(&c__1, thnwds, (ftnlen)32);
	scardc_(&c__0, thnwds, (ftnlen)32);
	ssizec_(&c__10, kwords, (ftnlen)32);
	scardc_(&c__0, kwords, (ftnlen)32);

/*        Determine if were in batch or interactive mode. */

	if (batch_()) {
	    s_copy(mode, "BATCH", (ftnlen)16, (ftnlen)5);
	} else {
	    s_copy(mode, "INTERACTIVE", (ftnlen)16, (ftnlen)11);
	}
    }
    intrct = s_cmp(mode, "BATCH", (ftnlen)16, (ftnlen)5) != 0;
    s_copy(style, margns, (ftnlen)128, (ftnlen)128);
    suffix_("NEWLINE /cr VTAB /vt HARDSPACE , ", &c__1, style, (ftnlen)33, (
	    ftnlen)128);
    i__ = 0;
    bscore = -1;
    m2code = -1;
    cutoff = 72;
    reason = TRUE_;

/*     Look through the templates until we get a match or we */
/*     run out of templates to try. */

    i__1 = *ntemps;
    for (i__ = 1; i__ <= i__1; ++i__) {
	score = 0;
	s_copy(temp, temps + (i__ - 1) * temps_len, temp_len, temps_len);
	sbeg = 1;
	m2code = 0;
	m2gmch_(temp, thnwds, command, &sbeg, &reason, &cutoff, &pssthn, &
		m2code, &score, error, temp_len, (ftnlen)32, command_len, 
		error_len);

/*        If M2CODE comes back zero, we are done with the work */
/*        of this routine. */

	if (m2code == 0) {
	    *btemp = i__;
	    return 0;
	}
	if (score > bscore) {
	    bscore = score;
	    *btemp = i__;
	}
    }

/*     If we get here, we know we didn't have a match.  Examine the */
/*     highest scoring template to get available diagnostics */
/*     about the mismatch. */

    s_copy(temp, temps + (*btemp - 1) * temps_len, temp_len, temps_len);
    sbeg = 1;
    fixit = TRUE_;
    m2code = 0;
    m2gmch_(temp, thnwds, command, &sbeg, &c_true, &cutoff, &pssthn, &m2code, 
	    &score, error, temp_len, (ftnlen)32, command_len, error_len);

/*     If we are in interactiive mode and we have a spelling error, we */
/*     can attempt to fix it.  Note this occurs only if the M2CODE */
/*     is less than 100 mod 10000. */

    while(m2code % 10000 < 100 && intrct && fixit) {

/*        Construct a friendly message; display it; and */
/*        get the user's response as to whether or not the */
/*        command should be modified. */

	s_copy(tryit, error, (ftnlen)600, error_len);
	prefix_("Hmmmm.,,,", &c__1, tryit, (ftnlen)9, (ftnlen)600);
	suffix_("/cr/cr I can repair this if you like.", &c__0, tryit, (
		ftnlen)37, (ftnlen)600);
	s_wsle(&io___19);
	e_wsle();
	niceio_3__(tryit, &c__6, style, (ftnlen)600, (ftnlen)128);
	s_wsle(&io___20);
	e_wsle();
	s_wsle(&io___21);
	e_wsle();
	s_wsle(&io___22);
	e_wsle();
	s_wsle(&io___23);
	e_wsle();
	m2rcvr_(&b, &e, kwords, (ftnlen)32);
	if (cardc_(kwords, (ftnlen)32) == 1) {
/* Writing concatenation */
	    i__2[0] = 17, a__1[0] = "Should I change \"";
	    i__2[1] = e - (b - 1), a__1[1] = command + (b - 1);
	    i__2[2] = 6, a__1[2] = "\" to \"";
	    i__2[3] = rtrim_(kwords + 192, (ftnlen)32), a__1[3] = kwords + 
		    192;
	    i__2[4] = 3, a__1[4] = "\" ?";
	    s_cat(questn, a__1, i__2, &c__5, (ftnlen)80);
	    cnfirm_1__(questn, &fixit, rtrim_(questn, (ftnlen)80));
	} else {
	    cnfirm_1__("Should I fix it?", &fixit, (ftnlen)16);
	}

/*        If the user has elected to have us fix the command */
/*        we have a few things to do... */

	if (fixit) {

/*           Look up the suggested fixes.  If there is more than */
/*           one possibility, see which one the user thinks is */
/*           best.  Otherwise, no more questions for now. */

	    m2rcvr_(&b, &e, kwords, (ftnlen)32);
	    if (cardc_(kwords, (ftnlen)32) > 1) {
		i__1 = cardc_(kwords, (ftnlen)32) - 4;
		for (i__ = 1; i__ <= i__1; ++i__) {
		    s_wsle(&io___27);
		    e_wsle();
		}
		i__1 = cardc_(kwords, (ftnlen)32);
		getopt_1__("Which word did you mean?", &i__1, keynam, &c__6, 
			kwords + 192, &c__32, kwords + 192, pick, (ftnlen)24, 
			(ftnlen)6, (ftnlen)32, (ftnlen)32, (ftnlen)32);
	    } else {
		s_copy(pick, kwords + 192, (ftnlen)32, (ftnlen)32);
	    }

/*           Make the requested repairs on the command, and */
/*           redisplay the command. */

	    repsub_(command, &b, &e, pick, command, command_len, (ftnlen)32, 
		    command_len);
	    cmprss_(" ", &c__1, command, command, (ftnlen)1, command_len, 
		    command_len);
	    s_wsle(&io___29);
	    do_lio(&c__9, &c__1, " ", (ftnlen)1);
	    e_wsle();
	    s_wsle(&io___30);
	    do_lio(&c__9, &c__1, " ", (ftnlen)1);
	    e_wsle();
	    niceio_3__(command, &c__6, style, command_len, (ftnlen)128);
	    s_wsle(&io___31);
	    e_wsle();

/*           Look through the templates again until we get a match or we */
/*           run out of templates to try.  Note however, that this time */
/*           we will start in a different spot.  We already have a best */
/*           matching template.  We'll start our search for a match */
/*           there and simulate a circular list of templates so that */
/*           we can examine all of them if necessary. */

	    s_copy(error, " ", error_len, (ftnlen)1);
	    s_copy(error + error_len, " ", error_len, (ftnlen)1);
	    bscore = -1;
	    m2code = -1;
	    cutoff = 72;
	    reason = TRUE_;
	    j = *btemp - 1;
	    i__1 = *ntemps;
	    for (i__ = 1; i__ <= i__1; ++i__) {

/*              Get the index of the next template to examine. */

		++j;
		while(j > *ntemps) {
		    j -= *ntemps;
		}

/*              Set the template, score for this template, spot to */
/*              begin examining it and the M2CODE so far. */

		s_copy(temp, temps + (j - 1) * temps_len, temp_len, temps_len)
			;
		sbeg = 1;
		score = 0;
		m2code = 0;
		m2gmch_(temp, thnwds, command, &sbeg, &reason, &cutoff, &
			pssthn, &m2code, &score, error, temp_len, (ftnlen)32, 
			command_len, error_len);

/*              If we get back a zero M2CODE we've got a match */
/*              This routine's work is done. */

		if (m2code == 0) {
		    *btemp = i__;
		    return 0;
		}

/*              Hmmph.  No match.  See if we've got a better */
/*              matching score so far and then go on to the next */
/*              template if any are left. */

		if (score > bscore) {
		    bscore = score;
		    *btemp = i__;
		}
	    }

/*           If we made it to this point the command doesn't properly */
/*           match any of the templates.  Get the best match and */
/*           determine the diagnostics for this template. */

	    s_copy(temp, temps + (*btemp - 1) * temps_len, temp_len, 
		    temps_len);
	    sbeg = 1;
	    m2code = 0;
	    score = 0;
	    m2gmch_(temp, thnwds, command, &sbeg, &reason, &cutoff, &pssthn, &
		    m2code, &score, error, temp_len, (ftnlen)32, command_len, 
		    error_len);
	}
    }

/*     If you get to this point. We didn't have a match set up */
/*     the second level of mismatch diagnostics using the best */
/*     matching template.  (BTEMP already points to it.) */

    s_copy(temp, temps + (*btemp - 1) * temps_len, temp_len, temps_len);
    cmprss_(" ", &c__1, temp, temp, (ftnlen)1, temp_len, temp_len);
    prepsn_(temp, temp_len);
    prepsn_(error + error_len, error_len);
    prefix_("/cr/cr(-3:-3) ", &c__1, error + error_len, (ftnlen)14, error_len)
	    ;
    prefix_(temp, &c__1, error + error_len, temp_len, error_len);
    prefix_("/cr/cr(3:3) ", &c__1, error + error_len, (ftnlen)12, error_len);
    prefix_("a command with the following syntax:", &c__3, error + error_len, 
	    (ftnlen)36, error_len);
    prefix_("I Believe you were trying to enter", &c__1, error + error_len, (
	    ftnlen)34, error_len);
    prefix_("META/2:", &c__1, error + error_len, (ftnlen)7, error_len);
    return 0;

/*     The following entry point allows user's to adjust the margins */
/*     of the META/2 error messages. */


L_m2marg:
    s_copy(margns, temp, (ftnlen)128, temp_len);
    return 0;
} /* meta_2__ */

/* Subroutine */ int meta_2__(char *command, char *temps, integer *ntemps, 
	char *temp, integer *btemp, char *error, ftnlen command_len, ftnlen 
	temps_len, ftnlen temp_len, ftnlen error_len)
{
    return meta_2__0_(0, command, temps, ntemps, temp, btemp, error, 
	    command_len, temps_len, temp_len, error_len);
    }

/* Subroutine */ int m2marg_(char *temp, ftnlen temp_len)
{
    return meta_2__0_(1, (char *)0, (char *)0, (integer *)0, temp, (integer *)
	    0, (char *)0, (ftnint)0, (ftnint)0, temp_len, (ftnint)0);
    }

