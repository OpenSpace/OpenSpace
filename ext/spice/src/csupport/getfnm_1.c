/* getfnm_1.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__1 = 1;
static integer c__6 = 6;

/* $Procedure GETFNM_1 ( Get a filename from standard input ) */
/* Subroutine */ int getfnm_1__(char *prmpt, char *fstat, char *fname, 
	logical *valid, ftnlen prmpt_len, ftnlen fstat_len, ftnlen fname_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    address a__1[2];
    integer i__1, i__2[2];
    char ch__1[1], ch__2[81];

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);

    /* Local variables */
    extern integer cpos_(char *, char *, integer *, ftnlen, ftnlen);
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), ucase_(char *, char *,
	     ftnlen, ftnlen), errch_(char *, char *, ftnlen, ftnlen), reset_(
	    void);
    extern integer rtrim_(char *, ftnlen);
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    static char badchr[162];
    extern logical failed_(void);
    char oldact[10];
    extern /* Subroutine */ int cnfirm_(char *, logical *, ftnlen), erract_(
	    char *, char *, ftnlen, ftnlen);
    integer length;
    extern integer lastnb_(char *, ftnlen);
    char myfnam[1000];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen);
    logical tryagn, myvlid;
    extern logical exists_(char *, ftnlen), return_(void);
    extern /* Subroutine */ int prompt_(char *, char *, ftnlen, ftnlen), 
	    writln_(char *, integer *, ftnlen);
    char status[3], myprmt[80];

/* $ Abstract */

/*     This routine prompts the user for a valid filename. */

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

/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     PRMPT      I   The prompt to use when asking for the filename. */
/*     FSTAT      I   Status of the file: 'OLD' or 'NEW'. */
/*     FNAME      O   A valid filename typed in by the user. */
/*     VALID      O   A logical flag indicating a valid filename. */
/*     PRMLEN     P   Maximum length allowed for a prompt before */
/*                    truncation. */

/* $ Detailed_Input */

/*     PRMPT    is a character string that will be displayed from the */
/*              current cursor position that informs a user that input */
/*              is expected. Prompts should be fairly short, since we */
/*              need to declare some local storage. The current maximum */
/*              length of a prompt is given by the parameter PRMLEN. */

/*     FSTAT    This is the status of the filename entered. It should */
/*              be 'OLD' when prompting for the filename of a file which */
/*              already exists, and 'NEW' when prompting for the */
/*              filename of a file which does not already exist or is to */
/*              be over written. */

/* $ Detailed_Output */

/*     FNAME    is a character string that contains a valid filename */
/*              typed in by the user. A valid filename is defined */
/*              simply to be a nonblank character string with no */
/*              embedded blanks, nonprinting characters, or characters */
/*              having decimal values > 126. */

/*     VALID    A logical flag which indicates whether or not the */
/*              filename entered is valid, i.e., a nonblank character */
/*              string with no leading or embedded blanks, which */
/*              satisfies the constraints for validity imposed. */

/* $ Parameters */

/*     PRMLEN   The maximum length for an input prompt string. */

/* $ Exceptions */

/*     1) If the input file status is not equal to 'NEW' or 'OLD' after */
/*        being left justified and converted to upper case, the error */
/*        SPICE(INVALIDARGUMENT) will be signalled. The error handling */
/*        is then reset. */

/*     2) If the filename entered at the prompt is blank, the error */
/*        SPICE(BLANKFILENAME) will be signalled. The error handling is */
/*        then reset. */

/*     3) If the filename contains an illegal character, a nonprinting */
/*        character or embedded blanks, the error */
/*        SPICE(ILLEGALCHARACTER) will be signalled. */

/*     4) If the file status is equal to 'OLD' after being left */
/*        justified and converted to upper case and the file specified */
/*        by the filename entered at the prompt does not exist, the */
/*        error SPICE(FILEDOESNOTEXIST) will be signalled. */

/*     5) If the file status is equal to 'NEW' after being left */
/*        justified and converted to upper case and the file specified */
/*        by the filename entered at the prompt already exists, the */
/*        error SPICE(FILEALREADYEXISTS) will be signalled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This is a utility that allows you to "easily" request a valid, */
/*     filename from a program user.  At a high level, it frees you */
/*     from the peculiarities of a particular FORTRAN's implementation */
/*     of cursor control. */

/*     A valid filename is defined as a nonblank character string with */
/*     no embedded blanks, nonprinting characters, or characters with */
/*     decimal values > 126. Leading blanks are removed, and trailing */
/*     blanks are ignored. */

/*     If an invalid filename is entered, this routine provides a */
/*     descriptive error message and halts the execution of the */
/*     process which called it by using a Fortran STOP. */

/* $ Examples */

/*     EXAMPLE 1: */

/*        FNAME = ' ' */
/*        PRMPT = 'Filename? ' */
/*        FSTAT = 'OLD' */

/*        CALL GETFNM_1( PRMPT, FSTAT, FNAME, VALID ) */

/*     The user sees the following displayed on the screen: */

/*        Filename? _ */

/*     where the underbar, '_', represents the cursor position. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     K.R. Gehringer (JPL) */

/* $ Version */

/* -    SPICELIB Version 6.17.0, 10-MAR-2014 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-INTEL. */

/* -    SPICELIB Version 6.16.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-LINUX-64BIT-IFORT. */

/* -    SPICELIB Version 6.15.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-GFORTRAN. */

/* -    SPICELIB Version 6.14.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-64BIT-GFORTRAN. */

/* -    SPICELIB Version 6.13.0, 14-DEC-2010 (EDW) */

/*        Declared PROMPT as EXTERNAL. */

/*        Unfied Version and Revision sections, eliminated Revision */
/*        section. Corrected error in 09-DEC-1999 Version entry. */
/*        Version ID changed to 6.0.9 from 7.0.0. */

/* -    Beta Version 6.12.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL. */

/* -    Beta Version 6.11.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-NATIVE_C. */

/* -    Beta Version 6.10.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-LINUX-64BIT-GFORTRAN. */

/* -    Beta Version 6.9.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-IFORT. */

/* -    Beta Version 6.8.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-GFORTRAN. */

/* -    Beta Version 6.7.0, 18-MAR-2009 (BVS) */

/*        Updated for PC-LINUX-GFORTRAN. */

/* -    Beta Version 6.6.0, 18-MAR-2009 (BVS) */

/*        Updated for MAC-OSX-GFORTRAN. */

/* -    Beta Version 6.5.0, 19-FEB-2008 (BVS) */

/*        Updated for PC-LINUX-IFORT. */

/* -    Beta Version 6.4.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-IFORT. */

/* -    Beta Version 6.3.0, 26-OCT-2005 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-GCC_C. */

/* -    Beta Version 6.2.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN. */

/* -    Beta Version 6.1.1, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    Beta Version 6.1.0, 16-AUG-2000 (WLT) */

/*        Added PC-LINUX environment */

/* -    Beta Version 6.0.9, 09-DEC-1999 (WLT) */

/*        This routine now calls EXPFNM_2 only UNIX environments */

/* -    Beta Version 6.0.0, 20-JAN-1998 (NJB) */

/*        Now calls EXPFNM_2 to attempt to expand environment variables. */

/*        Fixed a typo or two at various places in the header. */

/* -    Beta Version 5.1.0, 31-JAN-1996 (KRG) */

/*        Fixed a pedantic Fortran syntax error dealing with input */
/*        strings that are dimensioned CHARACTER*(*). */

/*        A local character string is now declared, and a parameter, */
/*        PRMLEN, has been added to the interface description for this */
/*        subroutine. PRMLEN defines the maximum length allowed for a */
/*        prompt before it is truncated. */

/* -    Beta Version 5.0.0, 05-JUL-1995 (KRG) */

/*        Modified the routine to handle all of its own error messages */
/*        and error conditions. The routine now signals an error */
/*        immediately resetting the error handling when an exceptional */
/*        condition is encountered. This is done so that input attempts */
/*        may continue until a user decides to stop trying. */

/*        Added several exceptions to the $ Exceptions section of the */
/*        header. */

/* -    Beta Version 4.0.1, 25-APR-1994 (KRG) */

/*        Removed some incorrect comments from the $ Particulars section */
/*        of the header. Something about a looping structure that is not */
/*        a part of the code now, if it ever was. */

/*        Fixed a typo or two at various places in the header. */

/* -    Beta Version 4.0.0, 29-SEP-1993 (KRG) */

/*        Added the character reperesnted by decimal 127 to the BADCHR. */
/*        It should have been there, but it wasn't. */

/* -    Beta Version 3.0.0, 10-SEP-1993 (KRG) */

/*        Made the file status variable FSTAT case insensitive. */

/*        Added code to the  file status .EQ. 'NEW' case to set the */
/*        valid flag to .FALSE. and set an appropriate error message */
/*        about the file already existing. */

/* -    Beta Version 2.0.0, 02-APR-1993 (KRG) */

/*        The variable BADCHR was not saved which caused problems on */
/*        some computers. This variable is now saved. */

/* -    Beta Version 1.0.0, 01-JUN-1992 (KRG) */

/* -& */
/* $ Index_Entries */

/*      prompt for a filename with error handling */

/* -& */

/*     SPICELIB Functions */


/*     Local Parameters */


/*     Maximum length of a filename. */


/*     Length of an error action */


/*     Local Variables */


/*     Saved Variables */


/*     Initial Values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("GETFNM_1", (ftnlen)8);
    }

/*     We are going to be signalling errors and resetting the error */
/*     handling, so we need to be in RETURN mode. First we get the */
/*     current mode and save it, then we set the mode to return. Upon */
/*     leaving the subroutine, we will restore the error handling mode */
/*     that was in effect when we entered. */

    erract_("GET", oldact, (ftnlen)3, (ftnlen)10);
    erract_("SET", "RETURN", (ftnlen)3, (ftnlen)6);

/*     If this is the first time this routine has been called, */
/*     initialize the ``bad character'' string. */

    if (first) {
	first = FALSE_;
	for (i__ = 0; i__ <= 32; ++i__) {
	    i__1 = i__;
	    *(unsigned char *)&ch__1[0] = i__;
	    s_copy(badchr + i__1, ch__1, i__ + 1 - i__1, (ftnlen)1);
	}
	for (i__ = 1; i__ <= 129; ++i__) {
	    i__1 = i__ + 32;
	    *(unsigned char *)&ch__1[0] = i__ + 126;
	    s_copy(badchr + i__1, ch__1, i__ + 33 - i__1, (ftnlen)1);
	}
    }

/*     Left justify and convert the file status to upper case for */
/*     comparisons. */

    ljust_(fstat, status, fstat_len, (ftnlen)3);
    ucase_(status, status, (ftnlen)3, (ftnlen)3);

/*     Check to see if we have a valid status for the filename. */

    if (s_cmp(status, "OLD", (ftnlen)3, (ftnlen)3) != 0 && s_cmp(status, 
	    "NEW", (ftnlen)3, (ftnlen)3) != 0) {
	setmsg_("The file status '#' was not valid. The file status must hav"
		"e a value of 'NEW' or 'OLD'.", (ftnlen)87);
	errch_("#", status, (ftnlen)1, (ftnlen)3);
	sigerr_("SPICE(INVALIDARGUMENT)", (ftnlen)22);
	chkout_("GETFNM_1", (ftnlen)8);
	return 0;
    }

/*     Store the input value for the prompt into our local value. We do */
/*     this for pedantic Fortran compilers that issue warnings for */
/*     CHARACTER*(*) variables used with concatenation. */

    s_copy(myprmt, prmpt, (ftnlen)80, prmpt_len);

/*     Read in a potential filename, and test it for validity. */

    tryagn = TRUE_;
    while(tryagn) {

/*        Set the value of the valid flag to .TRUE.. We assume that the */
/*        name entered will be a valid one. */

	myvlid = TRUE_;

/*        Get the filename. */

	if (s_cmp(myprmt, " ", (ftnlen)80, (ftnlen)1) == 0) {
	    prompt_("Filename? ", myfnam, (ftnlen)10, (ftnlen)1000);
	} else {
/* Writing concatenation */
	    i__2[0] = lastnb_(myprmt, (ftnlen)80), a__1[0] = myprmt;
	    i__2[1] = 1, a__1[1] = " ";
	    s_cat(ch__2, a__1, i__2, &c__2, (ftnlen)81);
	    prompt_(ch__2, myfnam, lastnb_(myprmt, (ftnlen)80) + 1, (ftnlen)
		    1000);
	}
	if (failed_()) {
	    myvlid = FALSE_;
	}
	if (myvlid) {
	    if (s_cmp(myfnam, " ", (ftnlen)1000, (ftnlen)1) == 0) {
		myvlid = FALSE_;
		setmsg_("The filename entered was blank.", (ftnlen)31);
		sigerr_("SPICE(BLANKFILENAME)", (ftnlen)20);
	    }
	}
	if (myvlid) {

/*           Left justify the filename. */

	    ljust_(myfnam, myfnam, (ftnlen)1000, (ftnlen)1000);

/*           Check for bad characters in the filename. */

	    length = lastnb_(myfnam, (ftnlen)1000);
	    i__ = cpos_(myfnam, badchr, &c__1, length, (ftnlen)162);
	    if (i__ > 0) {
		myvlid = FALSE_;
		setmsg_("The filename entered contains non printing characte"
			"rs or embedded blanks.", (ftnlen)73);
		sigerr_("SPICE(ILLEGALCHARACTER)", (ftnlen)23);
	    }
	}
	if (myvlid) {

/*           We know that the filename that was entered was nonblank and */
/*           had no bad characters. So, now we take care of the status */
/*           question. */

	    if (s_cmp(status, "OLD", (ftnlen)3, (ftnlen)3) == 0) {
		if (! exists_(myfnam, rtrim_(myfnam, (ftnlen)1000))) {
		    myvlid = FALSE_;
		    setmsg_("A file with the name '#' does not exist.", (
			    ftnlen)40);
		    errch_("#", myfnam, (ftnlen)1, (ftnlen)1000);
		    sigerr_("SPICE(FILEDOESNOTEXIST)", (ftnlen)23);
		}
	    } else if (s_cmp(status, "NEW", (ftnlen)3, (ftnlen)3) == 0) {
		if (exists_(myfnam, rtrim_(myfnam, (ftnlen)1000))) {
		    myvlid = FALSE_;
		    setmsg_("A file with the name '#' already exists.", (
			    ftnlen)40);
		    errch_("#", myfnam, (ftnlen)1, (ftnlen)1000);
		    sigerr_("SPICE(FILEALREADYEXISTS)", (ftnlen)24);
		}
	    }
	}
	if (myvlid) {
	    tryagn = FALSE_;
	} else {
	    writln_(" ", &c__6, (ftnlen)1);
	    cnfirm_("Try again? (Yes/No) ", &tryagn, (ftnlen)20);
	    writln_(" ", &c__6, (ftnlen)1);
	    if (tryagn) {
		reset_();
	    }
	}
    }

/*     At this point, we have done the best we can. If the status */
/*     was new, we might still have an invalid filename, but the */
/*     exact reasons for its invalidity are system dependent, and */
/*     therefore hard to test. */

    *valid = myvlid;
    if (*valid) {
	s_copy(fname, myfnam, fname_len, rtrim_(myfnam, (ftnlen)1000));
    }

/*     Restore the error action. */

    erract_("SET", oldact, (ftnlen)3, (ftnlen)10);
    chkout_("GETFNM_1", (ftnlen)8);
    return 0;
} /* getfnm_1__ */

