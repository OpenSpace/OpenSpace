/* getopt_1.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__9 = 9;
static integer c__1 = 1;
static integer c__4 = 4;

/* $ Procedure      GETOPT_1 ( Get option string from a specified list ) */

/* Subroutine */ int getopt_1__(char *title, integer *nopt, char *optnam, 
	integer *namlen, char *opttxt, integer *txtlen, char *optval, char *
	option, ftnlen title_len, ftnlen optnam_len, ftnlen opttxt_len, 
	ftnlen optval_len, ftnlen option_len)
{
    /* System generated locals */
    address a__1[2], a__2[4];
    integer i__1[2], i__2, i__3[4];

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);
    integer s_wsle(cilist *), e_wsle(void), do_lio(integer *, integer *, char 
	    *, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    logical done;
    char line[80];
    extern /* Subroutine */ int chkin_(char *, ftnlen), ucase_(char *, char *,
	     ftnlen, ftnlen), repmc_(char *, char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen, ftnlen);
    integer itask;
    extern integer rtrim_(char *, ftnlen);
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int prompt_(char *, char *, ftnlen, ftnlen);
    char msg[80];

    /* Fortran I/O blocks */
    static cilist io___3 = { 0, 6, 0, 0, 0 };
    static cilist io___4 = { 0, 6, 0, 0, 0 };
    static cilist io___5 = { 0, 6, 0, 0, 0 };
    static cilist io___7 = { 0, 6, 0, 0, 0 };
    static cilist io___8 = { 0, 6, 0, 0, 0 };
    static cilist io___10 = { 0, 6, 0, 0, 0 };
    static cilist io___11 = { 0, 6, 0, 0, 0 };
    static cilist io___12 = { 0, 6, 0, 0, 0 };
    static cilist io___13 = { 0, 6, 0, 0, 0 };



/* $ Abstract */

/*     Display a list of options in a standard menu format, and get */
/*     an option from a user returning the corresponding value from */
/*     a specified list of option values. */

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

/*     None. */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     TITLE      I   Title for the option menu. */
/*     NOPT       I   Number of options available. */
/*     OPTNAM     I   Names for the options (the selection names). */
/*     NAMLEN     I   Length of all of the option names. */
/*     OPTTXT     I   Brief text describing an option. */
/*     TXTLEN     I   Length of the descriptive text for all options. */
/*     OPTVAL     I   The value returned when its option is selected. */
/*     OPTION     O   The value of the option selected. */

/* $ Detailed_Input */

/*     TITLE    Title for the option menu. */

/*     NOPT     The number of menu options to be displayed. */

/*     OPTNAM   A list of short (mnemonic) names for the menu options. */
/*              These are the names used to selectan option. */

/*     NAMLEN   The maximum length of the short names for the menu */
/*              options. This number should probably be kept small, */
/*              say 6 characters or less. */

/*     OPTTXT   A list of character strings which contain brief */
/*              descriptions for each of the menu options. These */
/*              character strings should be kept relatively short. */

/*     TXTLEN   The maximum length of the brief descriptions of the */
/*              menu options. This number should probably be relatively */
/*              small small, say 50 characters or less. */

/*     OPTVAL   A list of textual values one of which will be returned */
/*              when a menu option is selected. */

/* $ Detailed_Output */

/*     OPTION   The value of the option selected from the menu, as */
/*              specified by the appropriate value of OPTVAL. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)   If the number of options, NOPT, is not > 0, the error */
/*          SPICE(INVALIDARGUMENT) will be signalled. */

/*     2)   If the length of the option names, NAMLEN, is  not > 0, */
/*          the error SPICE(INVALIDARGUMENT) will be signalled. */

/*     3)   If the length of the option text, TXTLEN, is  not > 0, */
/*          the error SPICE(INVALIDARGUMENT) will be signalled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine will display a menu of options in a standardized */
/*     format, promting for an option to be selected. This routine */
/*     will not return to the caller until one of the supplied options */
/*     has been selected. */

/*     Please note that the lengths of the option names, OPTNAM, and */
/*     the descriptive text for each option, OPTTXT, should be kept */
/*     reasonable, they both need to fit on the same output line with */
/*     a width of 80 characters. 13 characters out of the 80 available */
/*     are used for spacing and menu presentation, so there are 67 */
/*     characters available for the option name and the descriptive text */
/*     combined. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     K.R. Gehringer  (JPL) */

/* $ Version */

/* -     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 9, 1994 */


/* -     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of META/2 */
/*         software as of May 3, 1994 */


/* -    Beta Version 3.0.0, 03-SEP-1992  (KRG) */

/* -& */
/* $ Revisions */

/*     None. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Local variables */


/*     Saved variables */

/*     None. */


/*     Initial values */

/*     None. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("GETOPT_1", (ftnlen)8);
    }

/*     Check to make sure that the number of menu options is positive. */
/*     if it is not, then signal an error with an appropriate error */
/*     message. */

    if (*nopt < 1) {
	setmsg_("The number of options was not positive: #.", (ftnlen)42);
	errint_("#", nopt, (ftnlen)1);
	sigerr_("SPICE(INVALIDARGUMENT)", (ftnlen)22);
	chkout_("GETOPT_1", (ftnlen)8);
	return 0;
    }

/*     Check to make sure that the length of the option names is at */
/*     least 1. If not, then signal an error with an appropriate error */
/*     message. */

    if (*namlen < 1) {
	setmsg_("The length of the option names was not positive: #.", (
		ftnlen)51);
	errint_("#", namlen, (ftnlen)1);
	sigerr_("SPICE(INVALIDARGUMENT)", (ftnlen)22);
	chkout_("GETOPT_1", (ftnlen)8);
	return 0;
    }

/*     Check to make sure that the length of the descriptive text for */
/*     each option is at least 1. If not, then signal an error with an */
/*     appropriate error message. */

    if (*txtlen < 1) {
	setmsg_("The length of the option descriptions was not positive: #.", 
		(ftnlen)58);
	errint_("#", txtlen, (ftnlen)1);
	sigerr_("SPICE(INVALIDARGUMENT)", (ftnlen)22);
	chkout_("GETOPT_1", (ftnlen)8);
	return 0;
    }

/*     Do until we get an option */

    done = FALSE_;
    while(! done) {

/*        Display the menu title if it is non blank */

	if (s_cmp(title, " ", title_len, (ftnlen)1) != 0) {
/* Writing concatenation */
	    i__1[0] = 16, a__1[0] = "                ";
	    i__1[1] = title_len, a__1[1] = title;
	    s_cat(line, a__1, i__1, &c__2, (ftnlen)80);
	    s_wsle(&io___3);
	    e_wsle();
	    s_wsle(&io___4);
	    do_lio(&c__9, &c__1, line, rtrim_(line, (ftnlen)80));
	    e_wsle();
	}

/*        Display the menu and read in an option. */

	s_wsle(&io___5);
	e_wsle();
	i__2 = *nopt;
	for (itask = 1; itask <= i__2; ++itask) {
/* Writing concatenation */
	    i__3[0] = 10, a__2[0] = "        ( ";
	    i__3[1] = *namlen, a__2[1] = optnam + (itask - 1) * optnam_len;
	    i__3[2] = 3, a__2[2] = " ) ";
	    i__3[3] = *txtlen, a__2[3] = opttxt + (itask - 1) * opttxt_len;
	    s_cat(line, a__2, i__3, &c__4, (ftnlen)80);
	    s_wsle(&io___7);
	    do_lio(&c__9, &c__1, line, rtrim_(line, (ftnlen)80));
	    e_wsle();
	}

/*        Initialize the task indicator to zero, invalid task. */

	itask = 0;
	s_wsle(&io___8);
	e_wsle();
	prompt_("    Option: ", line, (ftnlen)12, (ftnlen)80);
	if (s_cmp(line, " ", (ftnlen)80, (ftnlen)1) != 0) {
	    ljust_(line, line, (ftnlen)80, (ftnlen)80);
	    ucase_(line, line, (ftnlen)80, (ftnlen)80);
	    itask = isrchc_(line, nopt, optnam, (*namlen), optnam_len);
	    if (itask == 0) {
		s_copy(msg, "'#' was not a valid option. Please try again.", (
			ftnlen)80, (ftnlen)45);
		repmc_(msg, "#", line, msg, (ftnlen)80, (ftnlen)1, (ftnlen)80,
			 (ftnlen)80);
		s_wsle(&io___10);
		e_wsle();
		s_copy(line, "    *****", (ftnlen)80, (ftnlen)9);
		s_wsle(&io___11);
		do_lio(&c__9, &c__1, line, rtrim_(line, (ftnlen)80));
		e_wsle();
/* Writing concatenation */
		i__1[0] = 10, a__1[0] = "    ***** ";
		i__1[1] = rtrim_(msg, (ftnlen)80), a__1[1] = msg;
		s_cat(line, a__1, i__1, &c__2, (ftnlen)80);
		s_wsle(&io___12);
		do_lio(&c__9, &c__1, line, rtrim_(line, (ftnlen)80));
		e_wsle();
		s_copy(line, "    *****", (ftnlen)80, (ftnlen)9);
		s_wsle(&io___13);
		do_lio(&c__9, &c__1, line, rtrim_(line, (ftnlen)80));
		e_wsle();
	    } else {
		s_copy(option, optval + (itask - 1) * optval_len, option_len, 
			optval_len);
		done = TRUE_;
	    }
	}
    }
    chkout_("GETOPT_1", (ftnlen)8);
    return 0;
} /* getopt_1__ */

