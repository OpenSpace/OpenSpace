/* getopt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__6 = 6;

/* $Procedure     GETOPT ( Get an option from a menu ) */
/* Subroutine */ int getopt_(char *title, integer *nopt, char *optnam, char *
	opttxt, integer *option, ftnlen title_len, ftnlen optnam_len, ftnlen 
	opttxt_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    logical done;
    char line[80];
    integer iopt, i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), ucase_(char *, char *,
	     ftnlen, ftnlen), repmc_(char *, char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen, ftnlen);
    logical okequ;
    extern integer rtrim_(char *, ftnlen);
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    char prmpt[80];
    extern logical failed_(void);
    logical ok, okdigi;
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    logical okalph;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int writln_(char *, integer *, ftnlen), prompt_(
	    char *, char *, ftnlen, ftnlen);
    char msg[80];

/* $ Abstract */

/*     Display a list of options in a standard menu format and get */
/*     an option from a user returning the corresponding index of */
/*     the option selected. */

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
/*     TITLE      I   Title for the menu. */
/*     NOPT       I   Number of options available. */
/*     OPTNAM     I   Names for the options. */
/*     OPTTXT     I   Brief text describing an option. */
/*     OPTVAL     I   The value returned when its option is selected. */
/*     OPTION     O   The number of the option selected. */

/* $ Detailed_Input */

/*     TITLE    Title for the option menu. */

/*     NOPT     The number of menu options to be displayed. */

/*     OPTNAM   A list of single character names for the menu options. */
/*              These are the names used to select an option. The names */
/*              must each be a single alphanumeric character. All names */
/*              must be upper case if they are characters. */

/*              If the option names is a period, '.', then a blank line */
/*              is to be displayed at that position in the menu list. */

/*     OPTTXT   A list of character strings which contain brief */
/*              descriptions for each of the menu options. These */
/*              character strings should be kept relatively short. */

/*     Please note that the lengths of the option names, OPTNAM, and */
/*     the descriptive text for each option, OPTTXT, should be kept */
/*     reasonable, they both need to fit on the same output line with */
/*     a width of 80 characters. 13 characters out of the 80 available */
/*     are used for spacing and menu presentation, so there are 67 */
/*     characters available for the option name and the descriptive text */
/*     combined. */

/* $ Detailed_Output */

/*     OPTION   The index of the option selected from the menu. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)   If the number of options, NOPT, is not > 0, the error */
/*          SPICE(INVALIDARGUMENT) will be signalled. */

/*     2)   If the option names are not all upper case alphanumeric */
/*          characters, the error SPICE(BADOPTIONNAME) will be signalled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine will display a menu of options in a standardized */
/*     format, promting for the selection of one of the listed options. */
/*     This routine will not return to the caller until one of the */
/*     supplied options has been selected or an error occurs. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     This routine makes explicit use fo the ASCII character sequence. */

/* $ Author_and_Institution */

/*     K.R. Gehringer  (JPL) */

/* $ Version */

/* -    Beta Version 4.1.0, 05-JUL-1995  (KRG) */

/*        Removed the initial blank line that was printed before the */
/*        title of the menu. The calling program should determine the */
/*        whitespace requirements for the appearance of the menu */
/*        displayed by this routine. */

/* -    Beta Version 4.0.0, 25-APR-1994  (KRG) */

/*        Modified the routine to output the index into the list of menu */
/*        options rather than a character string representing the option */
/*        selected. Also removed several calling arguments that were not */
/*        needed anymore. */

/*        Added the capability of inserting a blank line into the menu. */
/*        This is done by placing a period, '.', into the option name */
/*        location where the blank line lshould occur. */

/*        Added the missing $ Index_Entries section to the header. */

/*        Clarified a few of the comments in the header. */

/* -    Beta Version 3.0.0, 03-SEP-1992  (KRG) */

/* -& */
/* $ Index_Entries */

/*      display a menu and get a user's selection */

/* -& */
/* $ Revisions */

/* -    Beta Version 4.1.0, 05-JUL-1995  (KRG) */

/*        Removed the initial blank line that was printed before the */
/*        title of the menu. The calling program should determine the */
/*        whitespace requirements for the appearance of the menu */
/*        displayed by this routine. */

/* -    Beta Version 4.0.0, 25-APR-1994  (KRG) */

/*        Modified the routine to output the index into the list of menu */
/*        options rather than a character string representing the option */
/*        selected. Also removed several calling arguments that were not */
/*        needed anymore. */

/*        Added the capability of inserting a blank line into the menu. */
/*        This is done by placing a period, '.', into the option name */
/*        location where the blank line lshould occur. */

/*        Added the missing $ Index_Entries section to the header. */

/*        Clarified a few of the comments in the header. */

/* -    Beta Version 3.0.0, 03-SEP-1992  (KRG) */

/* -& */

/*     SPICELIB functions */


/*     Local Parameters */


/*     Mnemonic for the standard output. */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("GETOPT", (ftnlen)6);
    }

/*     Check to make sure that the number of menu options is positive. */
/*     if it is not, then signal an error with an appropriate error */
/*     message. */

    if (*nopt < 1) {
	setmsg_("The number of options was not positive: #.", (ftnlen)42);
	errint_("#", nopt, (ftnlen)1);
	sigerr_("SPICE(INVALIDARGUMENT)", (ftnlen)22);
	chkout_("GETOPT", (ftnlen)6);
	return 0;
    }

/*     Initialize the option prompt. */

    s_copy(prmpt, " ", (ftnlen)80, (ftnlen)1);
    s_copy(prmpt + 3, "Option: ", (ftnlen)77, (ftnlen)8);

/*     Check to make sure that all of the option names are alphanumeric */
/*     and uppercase. The only exception is the period, which signals a */
/*     blank line. */

    ok = TRUE_;
    i__1 = *nopt;
    for (i__ = 1; i__ <= i__1; ++i__) {
	okdigi = *(unsigned char *)&optnam[(i__ - 1) * optnam_len] >= '0' && *
		(unsigned char *)&optnam[(i__ - 1) * optnam_len] <= '9';
	okalph = *(unsigned char *)&optnam[(i__ - 1) * optnam_len] >= 'A' && *
		(unsigned char *)&optnam[(i__ - 1) * optnam_len] <= 'Z';
	okequ = *(unsigned char *)&optnam[(i__ - 1) * optnam_len] == '.';
	ok = ok && (okdigi || okalph || okequ);
	if (! ok) {
	    setmsg_("An illegal option name was found: option #, name '#'. ", 
		    (ftnlen)54);
	    errint_("#", &i__, (ftnlen)1);
	    sigerr_("SPICE(ILLEGALOPTIONNAME)", (ftnlen)24);
	    chkout_("GETOPT", (ftnlen)6);
	    return 0;
	}
    }

/*     Do until we get a valid option. */

    done = FALSE_;
    while(! done) {

/*        Display the menu title if it is non blank */

	if (s_cmp(title, " ", title_len, (ftnlen)1) != 0) {
	    s_copy(line, " ", (ftnlen)80, (ftnlen)1);
	    s_copy(line + 9, "#", (ftnlen)71, (ftnlen)1);
	    repmc_(line, "#", title, line, (ftnlen)80, (ftnlen)1, title_len, (
		    ftnlen)80);
	    writln_(line, &c__6, (ftnlen)80);
	}

/*        Display the menu and read in an option. */

	writln_(" ", &c__6, (ftnlen)1);
	i__1 = *nopt;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    s_copy(line, " ", (ftnlen)80, (ftnlen)1);
	    if (s_cmp(optnam + (i__ - 1) * optnam_len, ".", optnam_len, (
		    ftnlen)1) != 0) {
		s_copy(line + 3, "( # ) #", (ftnlen)77, (ftnlen)7);
		repmc_(line, "#", optnam + (i__ - 1) * optnam_len, line, (
			ftnlen)80, (ftnlen)1, optnam_len, (ftnlen)80);
		repmc_(line, "#", opttxt + (i__ - 1) * opttxt_len, line, (
			ftnlen)80, (ftnlen)1, opttxt_len, (ftnlen)80);
	    }
	    writln_(line, &c__6, (ftnlen)80);
	}
	writln_(" ", &c__6, (ftnlen)1);
	i__ = rtrim_(prmpt, (ftnlen)80) + 1;
	prompt_(prmpt, line, i__, (ftnlen)80);
	if (failed_()) {
	    chkout_("GETOPT", (ftnlen)6);
	    return 0;
	}

/*        Initialize the option value to zero, invalid option. */

	iopt = 0;
	if (s_cmp(line, " ", (ftnlen)80, (ftnlen)1) == 0) {
	    writln_(" ", &c__6, (ftnlen)1);
	} else {
	    ljust_(line, line, (ftnlen)80, (ftnlen)80);
	    ucase_(line, line, (ftnlen)80, (ftnlen)80);

/*           Check to make sure that the option we got is a valid */
/*           candidate: It must be alpha numeric. */

	    okdigi = *(unsigned char *)line >= '0' && *(unsigned char *)line 
		    <= '9';
	    okalph = *(unsigned char *)line >= 'A' && *(unsigned char *)line 
		    <= 'Z';
	    ok = okdigi || okalph;

/*           If we got a valid candidate for an option, see if it is one */
/*           of the options that we are supplying. */

	    if (ok) {
		iopt = isrchc_(line, nopt, optnam, (ftnlen)1, optnam_len);
		ok = iopt != 0;
	    }
	    if (! ok) {
		s_copy(msg, "'#' was not a valid option. Please try again.", (
			ftnlen)80, (ftnlen)45);
		repmc_(msg, "#", line, msg, (ftnlen)80, (ftnlen)1, (ftnlen)1, 
			(ftnlen)80);
		writln_(" ", &c__6, (ftnlen)1);
		s_copy(line, " ", (ftnlen)80, (ftnlen)1);
		s_copy(line + 3, "***", (ftnlen)77, (ftnlen)3);
		writln_(line, &c__6, (ftnlen)80);
		s_copy(line + 3, "*** #", (ftnlen)77, (ftnlen)5);
		repmc_(line, "#", msg, line, (ftnlen)80, (ftnlen)1, (ftnlen)
			80, (ftnlen)80);
		writln_(line, &c__6, (ftnlen)80);
		s_copy(line + 3, "***", (ftnlen)77, (ftnlen)3);
		writln_(line, &c__6, (ftnlen)80);
		writln_(" ", &c__6, (ftnlen)1);
	    } else {
		*option = iopt;
		done = TRUE_;
	    }
	}
    }
    chkout_("GETOPT", (ftnlen)6);
    return 0;
} /* getopt_ */

