/* getfnm.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__1 = 1;

/* $Procedure GETFNM ( Get a filename from standard input ) */
/* Subroutine */ int getfnm_(char *prmpt, char *fstat, char *fname, logical *
	valid, char *messg, ftnlen prmpt_len, ftnlen fstat_len, ftnlen 
	fname_len, ftnlen messg_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    address a__1[3];
    integer i__1, i__2[3];
    char ch__1[1];

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);

    /* Local variables */
    extern integer cpos_(char *, char *, integer *, ftnlen, ftnlen);
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), ucase_(char *, char *,
	     ftnlen, ftnlen), repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen);
    extern integer rtrim_(char *, ftnlen);
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    static char badchr[162];
    integer length;
    extern integer lastnb_(char *, ftnlen);
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical exists_(char *, ftnlen), return_(void);
    extern /* Subroutine */ int prompt_(char *, char *, ftnlen, ftnlen);
    char status[3];

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
/*     MESSG      O   A descriptive message for an invalid filename. */

/* $ Detailed_Input */

/*     PRMPT    is a character string that will be displayed from the */
/*              active position of the cursor to the end of string */
/*              that lets a user know that input is expected. */

/*     FSTAT    This is the status of the filename entered. It should */
/*              be 'OLD' when prompting for the filename of a file which */
/*              already exists, and 'NEW' when prompting for the filename */
/*              of a file which does not already exist or is to be over */
/*              written. */

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

/*     MESSG    A brief descriptive message which describes why a */
/*              particular filename was not valid. Blank if a valid */
/*              filename is entered. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

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

/*        CALL GETFNM( PRMPT, FSTAT, FNAME, VALID, MESSG ) */

/*     The user sees the following displayed on his screen: */

/*        Filename? _ */

/*     where the underbar, '_', represents the cursor position. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     K.R. Gehringer (JPL) */

/* $ Version */

/* -    SPICELIB Version 5.17.0, 10-MAR-2014 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-INTEL. */

/* -    SPICELIB Version 5.16.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-LINUX-64BIT-IFORT. */

/* -    SPICELIB Version 5.15.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-GFORTRAN. */

/* -    SPICELIB Version 5.14.0, 10-MAR-2014 (BVS) */

/*        Updated for PC-CYGWIN-64BIT-GFORTRAN. */

/* -    SPICELIB Version 5.13.0, 14-DEC-2010 (EDW) */

/*        Declared PROMPT as EXTERNAL. */

/*        Unfied Version and Revision sections, eliminated Revision */
/*        section. Corrected error in 09-DEC-1999 Version entry. */
/*        Version ID changed to 5.0.9 from 7.0.0. */

/* -    Beta Version 5.12.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL. */

/* -    Beta Version 5.11.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-NATIVE_C. */

/* -    Beta Version 5.10.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-LINUX-64BIT-GFORTRAN. */

/* -    Beta Version 5.9.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-IFORT. */

/* -    Beta Version 5.8.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-GFORTRAN. */

/* -    Beta Version 5.7.0, 18-MAR-2009 (BVS) */

/*        Updated for PC-LINUX-GFORTRAN. */

/* -    Beta Version 5.6.0, 18-MAR-2009 (BVS) */

/*        Updated for MAC-OSX-GFORTRAN. */

/* -    Beta Version 5.5.0, 19-FEB-2008 (BVS) */

/*        Updated for PC-LINUX-IFORT. */

/* -    Beta Version 5.4.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-IFORT. */

/* -    Beta Version 5.3.0, 26-OCT-2005 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-GCC_C. */

/* -    Beta Version 5.2.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN. */

/* -    Beta Version 5.1.1, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    Beta Version 5.1.0, 16-AUG-2000 (WLT) */

/*        Added the PC-LINUX environment */

/* -    Beta Version 5.0.9, 09-DEC-1999 (WLT) */

/*        This routine now calls EXPFNM_2 only in UNIX environments */

/* -    Beta Version 5.0.0, 20-JAN-1998 (NJB) */

/*        Now calls EXPFNM_2 to attempt to expand environment variables. */

/*        Fixed a typo or two at various places in the header. */

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

/*        The variable BADCHR was not saved which caused problems on some */
/*        computers. This variable is now saved. */

/* -    Beta Version 1.0.0, 01-JUN-1992 (KRG) */

/* -& */
/* $ Index_Entries */

/*      prompt for a filename with error handling */

/* -& */

/*     SPICELIB Functions */


/*     Local Parameters */


/*     Local Variables */


/*     Saved Variables */


/*     Initial Values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("GETFNM", (ftnlen)6);
    }

/*     If this is the first time this routine has been called, initialize */
/*     the ``bad character'' string. */

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

/*     Set the value of the valid flag to .TRUE.. We might as well assume */
/*     that the name entered will be a valid one. */

    *valid = TRUE_;

/*     Left justify and convert the file status to upper case for */
/*     comparisons. */

    ljust_(fstat, status, fstat_len, (ftnlen)3);
    ucase_(status, status, (ftnlen)3, (ftnlen)3);

/*     Check to see if we have a valid status for the filename. */

    if (s_cmp(status, "OLD", (ftnlen)3, (ftnlen)3) != 0 && s_cmp(status, 
	    "NEW", (ftnlen)3, (ftnlen)3) != 0) {
	*valid = FALSE_;
/* Writing concatenation */
	i__2[0] = 12, a__1[0] = "The status '";
	i__2[1] = 3, a__1[1] = status;
	i__2[2] = 21, a__1[2] = "' was not recognized.";
	s_cat(messg, a__1, i__2, &c__3, messg_len);
	chkout_("GETFNM", (ftnlen)6);
	return 0;
    }

/*     Read in a potential filename, and test it for validity. */

    if (s_cmp(prmpt, " ", prmpt_len, (ftnlen)1) == 0) {
	prompt_("Filename? ", fname, (ftnlen)10, fname_len);
    } else {
	prompt_(prmpt, fname, prmpt_len, fname_len);
    }
    if (s_cmp(fname, " ", fname_len, (ftnlen)1) == 0) {
	*valid = FALSE_;
	s_copy(messg, "A blank filename is not valid.", messg_len, (ftnlen)30)
		;
	chkout_("GETFNM", (ftnlen)6);
	return 0;
    }

/*     Left justify the filename. */

    ljust_(fname, fname, fname_len, fname_len);

/*     Check for bad characters in the filename. */

    length = lastnb_(fname, fname_len);
    i__ = cpos_(fname, badchr, &c__1, length, (ftnlen)162);
    if (i__ > 0) {
	*valid = FALSE_;
	s_copy(messg, "Invalid filename. Illegal character encountered: deci"
		"mal value: #", messg_len, (ftnlen)65);
	i__1 = *(unsigned char *)&fname[i__ - 1];
	repmi_(messg, "#", &i__1, messg, messg_len, (ftnlen)1, messg_len);
	chkout_("GETFNM", (ftnlen)6);
	return 0;
    }

/*     We know that the filename that was entered was nonblank and had */
/*     no bad characters. So, now we take care of the status question. */

    if (s_cmp(status, "OLD", (ftnlen)3, (ftnlen)3) == 0) {
	if (! exists_(fname, rtrim_(fname, fname_len))) {
	    *valid = FALSE_;
	    s_copy(messg, "The file does not exist.", messg_len, (ftnlen)24);
	    chkout_("GETFNM", (ftnlen)6);
	    return 0;
	}
    } else if (s_cmp(status, "NEW", (ftnlen)3, (ftnlen)3) == 0) {
	if (exists_(fname, rtrim_(fname, fname_len))) {
	    *valid = FALSE_;
	    s_copy(messg, "The file already exists.", messg_len, (ftnlen)24);
	    chkout_("GETFNM", (ftnlen)6);
	    return 0;
	}
    }

/*     At this point, we have done the best we can. If the status */
/*     was new, we might still have an invalid filename, but the */
/*     exact reasons for its invalidity are system dependent. */

    chkout_("GETFNM", (ftnlen)6);
    return 0;
} /* getfnm_ */

