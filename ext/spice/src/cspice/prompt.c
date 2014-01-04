/* prompt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure      PROMPT ( Prompt a user for a string ) */
/* Subroutine */ int prompt_(char *prmpt, char *string, ftnlen prmpt_len, 
	ftnlen string_len)
{
    /* System generated locals */
    integer i__1, i__2;
    cilist ci__1;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(void),
	     s_rsfe(cilist *), e_rsfe(void), i_len(char *, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, ftnlen)
	    , setmsg_(char *, ftnlen);
    integer iostat;
    extern /* Subroutine */ int errint_(char *, integer *, ftnlen);

/* $ Abstract */

/*     This routine prompts a user for keyboard input. */

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
/*     PRMPT      I   The prompt to use when asking for input. */
/*     STRING     O   The response typed by a user. */

/* $ Detailed_Input */

/*     PRMPT      is a character string that will be displayed from the */
/*                current cursor position and describes the input that */
/*                the user is expected to enter.  The string PRMPT should */
/*                be relatively short, i.e., 50 or fewer characters, so */
/*                that a response may be typed on the line where the */
/*                prompt appears. */

/*                All characters (including trailing blanks) in PRMPT */
/*                are considered significant and will be displayed. */

/* $ Detailed_Output */

/*     STRING     is a character string that contains the string */
/*                entered by the user. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     This subroutine uses discovery check-in so that it may be called */
/*     after an error has occurred. */

/*     1) If the attempt to write the prompt to the standard output */
/*        device fails, returning an IOSTAT value not equal to zero, the */
/*        error SPICE(WRITEFAILED) will be signalled. */

/*     2) If the attempt to read the response from the standard input */
/*        device fails, returning an IOSTAT value not equal to zero, the */
/*        error SPICE(READFAILED) will be signalled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This is a utility that allows you to "easily" request information */
/*     from a program user.  At a high level, it frees you from the */
/*     peculiarities of a particular implementation of FORTRAN cursor */
/*     control. */

/* $ Examples */

/*     Suppose you wanted to ask a user to input an answer to */
/*     a question such as "Do you want to try again? (Y/N) " */
/*     and leave the cursor at the end of the question as shown here: */

/*        Do you want to try again? (Y/N) _ */

/*     (The underscore indicates the cursor position). */

/*     The following line of code will do what you want. */

/*        CALL PROMPT ( 'Do you want to try again? (Y/N) ', ANSWER ) */

/* $ Restrictions */

/*     This routine is environment specific.  Standard FORTRAN does not */
/*     provide for user control of cursor position after write */
/*     statements. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     K.R. Gehringer (JPL) */
/*     W.L. Taber     (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.20.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL. */

/* -    SPICELIB Version 3.19.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-CC_C. */

/* -    SPICELIB Version 3.18.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C. */

/* -    SPICELIB Version 3.17.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-NATIVE_C. */

/* -    SPICELIB Version 3.16.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-WINDOWS-64BIT-IFORT. */

/* -    SPICELIB Version 3.15.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-LINUX-64BIT-GFORTRAN. */

/* -    SPICELIB Version 3.14.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-64BIT-MS_C. */

/* -    SPICELIB Version 3.13.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-INTEL_C. */

/* -    SPICELIB Version 3.12.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-IFORT. */

/* -    SPICELIB Version 3.11.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-GFORTRAN. */

/* -    SPICELIB Version 3.10.0, 18-MAR-2009 (BVS) */

/*        Updated for PC-LINUX-GFORTRAN. */

/* -    SPICELIB Version 3.9.0, 18-MAR-2009 (BVS) */

/*        Updated for MAC-OSX-GFORTRAN. */

/* -    SPICELIB Version 3.8.0, 19-FEB-2008 (BVS) */

/*        Updated for PC-LINUX-IFORT. */

/* -    SPICELIB Version 3.7.0, 14-NOV-2006 (BVS) */

/*        Updated for PC-LINUX-64BIT-GCC_C. */

/* -    SPICELIB Version 3.6.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-INTEL_C. */

/* -    SPICELIB Version 3.5.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-IFORT. */

/* -    SPICELIB Version 3.4.0, 14-NOV-2006 (BVS) */

/*        Updated for PC-WINDOWS-IFORT. */

/* -    SPICELIB Version 3.3.0, 26-OCT-2005 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-GCC_C. */

/* -    SPICELIB Version 3.2.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN_C. */

/* -    SPICELIB Version 3.1.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN. */

/* -    SPICELIB Version 3.0.5, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 3.0.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitely given.  New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 3.0.3, 24-SEP-1999 (NJB) */

/*        CSPICE environments were added.  Some typos were corrected. */

/* -    SPICELIB Version 3.0.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitely given.  New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    SPICELIB Version 3.0.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitely given.  Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    SPICELIB Version 3.0.0, 08-APR-1998 (NJB) */

/*        Module was updated for the PC-LINUX platform. */

/* -    SPICELIB Version 2.0.0, 20-JUL-1995 (WLT) (KRG) */

/*        This routine now participates in error handling.  It */
/*        checks to make sure no I/O errors have occurred while */
/*        attempting to write to standard output or read from standard */
/*        input. It uses discovery checkin if an error is detected. */

/*        Restructured the subroutine a little bit; the writing of the */
/*        prompt is the only bit that is environment specific, so the */
/*        code was rearranged to reflect this. There is now only a single */
/*        READ statement. */

/* -    SPICELIB Version 1.0.0, 15-OCT-1992 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Prompt for keyboard input */
/*     Prompt for input with a user supplied message */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 3.0.0, 08-APR-1998 (NJB) */

/*        Module was updated for the PC-LINUX platform. */

/* -    SPICELIB Version 2.0.0, 20-JUL-1995 (WLT) (KRG) */

/*        This routine now participates in error handling.  It */
/*        checks to make sure no I/O errors have occurred while */
/*        attempting to write to standard output or read from standard */
/*        input. It uses discovery checkin if an error is detected. */

/*        Restructured the subroutine a little bit; the writing of the */
/*        prompt is the only bit that is environment specific, so the */
/*        code was rearranged to reflect this. There is now only a single */
/*        READ statement. */

/* -& */

/*     Local variables */




/*     The code below should be used in the following environments: */

/*     SUN/Fortran, */
/*     HP/HP-Fortran, */
/*     Silicon Graphics/Silicon Graphics Fortran, */
/*     DEC Alpha-OSF/1--DEC Fortran, */
/*     NeXT/Absoft Fortran */
/*     PC Linux/Fort77 */

    ci__1.cierr = 1;
    ci__1.ciunit = 6;
    ci__1.cifmt = "(A,$)";
    iostat = s_wsfe(&ci__1);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = do_fio(&c__1, prmpt, prmpt_len);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = e_wsfe();
L100001:

/*     If none of the write statements above works on a particular */
/*     unsupported platform, read on... */

/*     Although, this isn't really what you want, if you need to port */
/*     this quickly to an environment that does not support the format */
/*     statement in any of the cases above, you can comment out the */
/*     write statement above and un-comment the write statement below. */
/*     In this way you can get a program working quickly in the new */
/*     environment while you figure out how to control cursor */
/*     positioning. */

/*      WRITE (*,*, IOSTAT=IOSTAT ) PRMPT */

/*     Check for a write error. It's not likely, but the standard output */
/*     can be redirected. Better safe than confused later. */

    if (iostat != 0) {
	chkin_("PROMPT", (ftnlen)6);
	setmsg_("An error occurred while attempting to write a prompt to the"
		" standard output device, possibly because standard output ha"
		"s been redirected to a file. There is not much that can be d"
		"one about this if it happens. We do not try to determine whe"
		"ther standard output has been redirected, so be sure that th"
		"ere are sufficient resources available for the operation bei"
		"ng performed.", (ftnlen)372);
	sigerr_("SPICE(WRITEFAILED)", (ftnlen)18);
	chkout_("PROMPT", (ftnlen)6);
	return 0;
    }

/*     Now that we've written out the prompt and there was no error, we */
/*     can read in the response. */

    ci__1.cierr = 1;
    ci__1.ciend = 1;
    ci__1.ciunit = 5;
    ci__1.cifmt = "(A)";
    iostat = s_rsfe(&ci__1);
    if (iostat != 0) {
	goto L100002;
    }
    iostat = do_fio(&c__1, string, string_len);
    if (iostat != 0) {
	goto L100002;
    }
    iostat = e_rsfe();
L100002:
    if (iostat != 0) {
	chkin_("PROMPT", (ftnlen)6);
	setmsg_("An error occurred while attempting to retrieve a reply to t"
		"he prompt \"#\".  A possible cause is that you have exhauste"
		"d the input buffer while attempting to type your response.  "
		"It may help if you limit your response to # or fewer charact"
		"ers. ", (ftnlen)242);
	errch_("#", prmpt, (ftnlen)1, prmpt_len);
/* Computing MIN */
	i__2 = i_len(string, string_len);
	i__1 = min(i__2,131);
	errint_("#", &i__1, (ftnlen)1);
	sigerr_("SPICE(READFAILED)", (ftnlen)17);
	chkout_("PROMPT", (ftnlen)6);
	return 0;
    }
    return 0;
} /* prompt_ */

