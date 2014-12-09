/* spacit.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__0 = 0;
static integer c__6 = 6;

/* $Program SPACIT ( SPACIT Utility Program ) */
/* Main program */ MAIN__(void)
{
    /* Initialized data */

    static char maintl[20] = "SPACIT Options      ";
    static char mainvl[20*6] = "QUIT                " "START_LOG_FILE      " 
	    "TRANSFER_TO_BINARY  " "BINARY_TO_TRANSFER  " "SUMMARIZE        "
	    "   " "READ_COMMENTS       ";
    static char maintx[40*6] = "Quit.                                   " 
	    "Log SPACIT output to a file.            " "Convert transfer fil"
	    "e to binary file.   " "Convert binary file to transfer file.   " 
	    "Summarize binary file.                  " "Read comment area of"
	    " binary file.       ";
    static char mainnm[1*6] = "Q" "L" "T" "B" "S" "R";

    /* System generated locals */
    address a__1[3];
    integer i__1[3], i__2;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen), s_cat(char *,
	     char **, integer *, integer *, ftnlen), s_stop(char *, ftnlen);
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);

    /* Local variables */
    static logical done;
    static char line[255];
    static integer iopt, i__, r__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static char cmrcl[80*7];
    extern /* Subroutine */ int repmc_(char *, char *, char *, char *, ftnlen,
	     ftnlen, ftnlen, ftnlen);
    static logical found;
    extern /* Subroutine */ int reset_(void);
    extern integer rtrim_(char *, ftnlen);
    extern /* Subroutine */ int bodc2n_(integer *, char *, logical *, ftnlen),
	     spat2b_(logical *, integer *), spab2t_(logical *, integer *);
    extern logical failed_(void);
    static logical logfil;
    extern /* Subroutine */ int spardc_(logical *, integer *), erract_(char *,
	     char *, ftnlen, ftnlen), spalog_(char *, logical *, integer *, 
	    ftnlen), byebye_(char *, ftnlen), chkout_(char *, ftnlen);
    static integer loglun;
    extern /* Subroutine */ int getopt_(char *, integer *, char *, char *, 
	    integer *, ftnlen, ftnlen, ftnlen);
    static logical contnu;
    static char option[20];
    extern /* Subroutine */ int spasum_(logical *, integer *), tostdo_(char *,
	     ftnlen), errprt_(char *, char *, ftnlen, ftnlen), tkvrsn_(char *,
	     char *, ftnlen, ftnlen);
    static char tkv[12];

/* $ Abstract */

/*     This is a utility program that permits easy access to several */
/*     operations on SPICE data files. */

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

/*     FILES */
/*     UTILITY */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     1) All of the tasks performed require at least one filename. */

/*        File conversions require two filenames: A transfer filename and */
/*        a binary filename. */

/*        Reading the comment area of a file requires a single binary */
/*        filename. */

/*        Summarizing a file requires a binary filename, a leapseconds */
/*        filename and an SCLK filename for summarizing CK files. */

/*     2) Two text kernel files are potentially used: */

/*        a) A leapseconds file if any summary is to be performed. */
/*           This file provides information necessary to perform time */
/*           conversion between UTC and ET (TDB) */

/*        b) An SCLK, spacecraft clock, file if a C-kernel summary is to */
/*           be performed. This file provides informationnecessary to */
/*           perform time conversions between the encoded spacecraft time */
/*           system and UTC or ET (TDB). */

/*        The program will prompt for the names of these files if needed. */

/* $ Particulars */

/*     A text transfer file may include text data preceding the line */
/*     identifying the file as a SPICE transfer file.  An example of such */
/*     an ID info line: */

/*        DAFETF NAIF DAF ENCODED TRANSFER FILE */

/*     A transfer file with some sort of non-SPICE text header has the */
/*     structure: */

/*        ___________ */
/*       | */
/*       | */
/*       | */
/*       |   ...some text... */
/*       | */
/*       | */
/*       | */
/*       |___________ */
/*       DAFETF NAIF DAF ENCODED TRANSFER FILE */
/*             ...unmodified transfer file text.... */

/*     The text preceding the info line is ignored:  WARNING, if */
/*     the text header contains a valid SPICE ID string (DAFETF */
/*     for example), the program assumes all data after that line */
/*     is the transfer file data.  Something will break. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */
/*     K.R. Gehringer (JPL) */
/*     J.E. McLean    (JPL) */
/*     R.E. Thurman   (JPL) */

/* $ Version */

/* -    SPICE Version 7.2.1, 03-NOV-2000 (EDW) */

/*        Added a BYEBYE( 'SUCCESS' ) call at program's end. */

/* -    SPICE Version 7.2.0 24-MAR-1999 (EDW) */

/*        All write to standard output call replaced with TOSTDO. */

/*        Operation of the binary to transfer function enhanced to */
/*        accomodate SFDUs.  See the routines:  SPAT2B 2.0.0; */
/*        ZZGETFAT 1.0.0; ZZCONVTB 1.0.0 for details. */

/* -    Beta Version 7.1.1, 14-MAR-1997 (WLT) */

/*        The summary portion of the program was slightly enhanced */
/*        See the routines  SPASUM Version 2.0.0, CKWSS 2.1.0, and */
/*        SPKWSS 2.1.0 for details on the enhancements. */

/*        In addition the greeting was updated to print the version */
/*        of the SPICE toolkit used to link SPACIT. */

/* -    Beta Version 7.1.0, 24-JAN-1996 (KRG) */

/*        This is a cleanup of loose ends from Version 7.0.0. */

/* -    Beta Version 7.0.0, 26-JUL-1995 (KRG) */

/*        Modified the main program. Each menu function now calls a */
/*        subroutine to perform its function. */

/*        In the summaries, ET times are now displayed in a calendar */
/*        format similar to the UTC times. */

/*        The program can now detect when an incorrect file has been */
/*        specified in most cases. */

/* -    Beta Version 6.0.0, 25-APR-1994 (KRG) */

/*        Added support the the dynamic identification of file types. */
/*        This had the effect of reducing the number of options on the */
/*        main menu to just the types of operations that the SPACIT */
/*        program can perform. This allows a much more concise main menu */
/*        and fewer inputs or less a priori knowledge on the part of a */
/*        user of the program. */

/*        Changed the terminology used for text files. These are now */
/*        called transfer files to differentiate them from text kernel */
/*        files. */

/*        Added support for the new binary PCK file format. */

/*        The $ Files section has been modified. It is now more relevant */
/*        to this version of the SPACIT program. */

/*        The SPACIT program now cleans up after itself in the event of a */
/*        file conversion failure. The file being created is deleted if */
/*        the conversion fails. This is a new feature of the program. */

/*        The file summary formats have been changed. The information */
/*        contained in the summaries is that same as before, but it has */
/*        been rearranged a bit for easire readability. */

/* -    Beta Version 5.0.0, 16-FEB-1993 (KRG) */

/*        Added support for the E-Kernel. */

/*        This is an incremental improvement in the program, and others */
/*        are planned. */

/* -    Beta Version 4.0.0, 04-SEP-1992 (KRG) */

/*        The user interface for this routine was completely rewritten. */
/*        The summary subroutines were not changed. A centralized */
/*        menuing capability was added, and several utility subroutines */
/*        have also been added. */

/*        The main program now coordinates the collection of filenames, */
/*        loading the leapseconds kernel or the SCLK kernel, etc. */

/*        This is in incremental improvement in the program, and others */
/*        are planned. */

/* -    Beta Version 3.0.0, 11-JUN-1991 (JEM) */

/*        This version has several major changes from the last. */

/*        1.  Changes were made so that output can go to a file */
/*            as well as the terminal screen. */

/*        2.  This version has the functionality to display */
/*            comments from the comment area of a binary SPK or */
/*            CK file. */

/*        3.  The prompting for and loading of leapseconds and */
/*            sclk kernel files was moved to the subroutine */
/*            GETTSK. */

/*        4.  If the user hits return at a prompt, the current */
/*            task is cancelled and the menu is displayed again. */

/*        5.  The error action was set to 'RETURN' at the beginning */
/*            of the program.  FAILED is checked where necessary */
/*            and appropriate action is taken.  In the previous */
/*            version the error action was 'DEFAULT' except in */
/*            a few places. */

/*        6.  Various cosmetic changes were made such as alphabetizing */
/*            variable declarations. */

/* -    Beta Version 2.0.0, 16-JAN-1991 (JEM) */

/*        The calls to DAFB2A and DAFA2B were replaced with calls */
/*        to SPCB2A and SPCA2B to include the comment area when */
/*        converting an SPK or CK file. */

/* -    Beta Version 1.2.0, 3-SEP-1990 (JEM) */

/*        A few blank line write statements were added for display */
/*        readability. */

/* -    Beta Version 1.1.0, 4-APR-1990 (HAN) */

/*        The declarations for the text and binary file names have */
/*        been changed from CHARACTER*(40) to CHARACTER*(80). */

/* -    Beta Version 1.0.0, 31-MAY-1989 (RET) */

/* -& */
/* $ Revisions */

/* -    SPICE Version 7.2.0 24-MAR-1999 (EDW) */

/*        All write to standard output call replaced with TOSTDO. */

/*        Operation of the binary to transfer function enhanced to */
/*        accomodate SFDUs.  See the routines:  SPAT2B 2.0.0; */
/*        ZZGETFAT 1.0.0; ZZCONVTB 1.0.0 for details. */

/* -    Beta Version 7.1.1, 14-MAR-1997 (WLT) */

/*        The summary portion of the program was slightly enhanced */
/*        See the routines  SPASUM Version 2.0.0, CKWSS 2.1.0, and */
/*        SPKWSS 2.1.0 for details on the enhancements. */

/* -    Beta Version 7.1.0, 26-JUL-1995 (KRG) */

/*        Continuing the general code cleanup and modularization begun */
/*        with version 7.0.0. */

/* -    Beta Version 7.0.0, 26-JUL-1995 (KRG) */

/*        Modified the main program. Each menu function now calls a */
/*        subroutine to perform its function. */

/*        In the summaries, ET times are now displayed in a calendar */
/*        format similar to the UTC times. */

/*        The program can now detect when an incorrect file has been */
/*        specified in most cases. */

/* -    Beta Version 6.0.0, 25-APR-1994 (KRG) */

/*        Added support the the dynamic identification of file types. */
/*        This had the effect of reducing the number of options on the */
/*        main menu to just the types of operations that the SPACIT */
/*        program can perform. This allows a much more concise main menu */
/*        and fewer inputs or less a priori knowledge on the part of a */
/*        user of the program. */

/*        Changed the terminology used for text files. These are now */
/*        called transfer files to differentiate them from text kernel */
/*        files. */

/*        Added support for the new binary PCK file format. */

/*        The $ Files section has been modified. It is now more relevant */
/*        to this version of the SPACIT program. */

/*        The SPACIT program now cleans up after itself in the event of a */
/*        file conversion failure. The file being created is deleted if */
/*        the conversion fails. This is a new feature of the program. */

/*        The file summary formats have been changed. The information */
/*        contained in the summaries is that same as before, but it has */
/*        been rearranged a bit for easire readability. */

/*        This version of SPACIT is significantly different than the */
/*        previous version in the way it handles filename inputs and */
/*        related issues  necessary to call the correct subroutines. The */
/*        code for collecting binary filenames, and text filename when */
/*        necessary, is now separated form the code which collects teh */
/*        leapseconds and SCLK filenames when they are needed. Thi swas */
/*        necessary because we do not know whether we need an SCLK file */
/*        until we know the type of the binary file we will be */
/*        summarizing. This was the major change to the program. */

/* -    Beta Version 5.0.0, 16-FEB-1993 (KRG) */

/*        Added support for the E-Kernel. */

/*        This is an incremental improvement in the program, and others */
/*        are planned. */

/* -    Beta Version 4.0.0, 04-SEP-1992 (KRG) */

/*        The user interface for this routine was completely rewritten. */
/*        The summary subroutines were not changed. A centralized */
/*        menuing capability was added, and several utility subroutines */
/*        have also been added. */

/*        The main program now coordinates the collection of filenames, */
/*        loading the leapseconds kernel or the SCLK kernel, etc. */

/*        This is in incremental improvement in the program, and others */
/*        are planned. */

/* -    Beta Version 3.0.0, 11-JUN-1991 (JEM) */

/*        1.  A section of code was added just after the banner */
/*            is written.  The user is asked whether or not he/she */
/*            wants the output to go to a file.  If so, the user */
/*            is prompted for the name of the output file.  Then */
/*            that file is opened as a text file and the variable */
/*            TOFILE is set to .TRUE.  At the end, the file is */
/*            closed. */

/*            Before calling SUMSPK or SUMCK, the name of the */
/*            file to be summarized is written to the output file */
/*            if TOFILE is TRUE. */

/*            Two new arguments were added to the calling sequences */
/*            of SUMSPK and SUMCK:  TOFILE and UNIT.  The same two */
/*            arguments were also added to the calling sequences */
/*            of the routines DISPSP and DISPC which get called */
/*            by SUMSPK and SUMCK respectively.  If TOFILE is */
/*            true, the summary is written to the file connected */
/*            to UNIT. */

/*        2.  The option to display comments was listed in the header */
/*            and code was added to handle the case when NEXT = 'COM'. */
/*            A new routine called WRCOM writes the comments */
/*            to the screen and to the output file if one was */
/*            requested. */

/*        3.  The subroutine GETTSK now loads leapseconds and */
/*            sclk kernel files when necessary so that part of */
/*            the code was removed. */

/*        4.  A possible task returned from GETTSK is */
/*            'NONE'.  That means a task was selected, then */
/*            cancelled.  GETTSK is called repeatedly until the */
/*            task returned is something other that NONE.  In */
/*            this way the user is able to select another task. */

/* -& */

/*     SPICELIB functions */


/*     Parameters */


/*     Set the version. This number should agree with the version number */
/*     listed in the $ Version section of the header. */


/*     Set a value for a replacement marker. */


/*     Set a value for the length of an input text line. */


/*     Set a length for the commercial lines. */


/*     Set the number of lines in the commercial. */


/*     Set a length for the option values. */


/*     Set a length for the title of a menu. */


/*     Set the length of the text description of an option on a menu. */


/*     Set the number of options available on the main menu. */


/*     Variables */


/*     Define the main menu title ... */


/*     Define the main menu option values ... */


/*     Define the main menu descriptive text for each option ... */


/*     Define the main menu option names ... */


/*     Register the SPACIT main program with the SPICELIB error handler. */

    chkin_("SPACIT", (ftnlen)6);
    tkvrsn_("TOOLKIT", tkv, (ftnlen)7, (ftnlen)12);
    r__ = rtrim_(tkv, (ftnlen)12);

/*     Set the error action to 'RETURN'. We don't want the program */
/*     to abort if an error is signalled. We check FAILED where */
/*     necessary. If an error is signalled, we'll just handle the */
/*     error, display an appropriate message, then call RESET at the */
/*     end of the loop to continue. */

    erract_("SET", "RETURN", (ftnlen)3, (ftnlen)6);

/*     Set the error messages that we want to have displayed. We will */
/*     display the SPICELIB short and long error messages. This is done */
/*     to ensure that some sort of an error message is displayed if an */
/*     error occurs. In several places, long error messages are not set, */
/*     so if only the long error messages were displayed, it would be */
/*     possible to have an error signalled and not see any error */
/*     information. This is not a very useful thing. */

    errprt_("SET", "NONE, SHORT, LONG, TRACEBACK", (ftnlen)3, (ftnlen)28);

/*     Set up the initial ``commercial'' for when the program is */
/*     executed. */

    s_copy(cmrcl, " ", (ftnlen)80, (ftnlen)1);
    s_copy(cmrcl + 80, " ", (ftnlen)80, (ftnlen)1);
    s_copy(cmrcl + 160, " ", (ftnlen)80, (ftnlen)1);
    s_copy(cmrcl + 240, "    Welcome to SPACIT Version: 7.1.1", (ftnlen)80, (
	    ftnlen)36);
/* Writing concatenation */
    i__1[0] = 25, a__1[0] = "          (Spice Toolkit ";
    i__1[1] = r__, a__1[1] = tkv;
    i__1[2] = 1, a__1[2] = ")";
    s_cat(cmrcl + 320, a__1, i__1, &c__3, (ftnlen)80);
    s_copy(cmrcl + 400, " ", (ftnlen)80, (ftnlen)1);
    s_copy(cmrcl + 480, " ", (ftnlen)80, (ftnlen)1);

/*     Initialize the body ID to body name mapping by calling BODC2N. */
/*     This initialization may be a time consuming process, so we */
/*     do it at the start of the program. By doing this, the slowness of */
/*     the initialization is hidden as part of the apparent loadin gof */
/*     the program. */

    bodc2n_(&c__0, line, &found, (ftnlen)255);
    if (failed_()) {

/*        If anything failed when initializing the body ID code and name */
/*        tables, then we cannot proceed, so exit. */

	s_copy(line, "Error building the NAIF body code and name tables. Can"
		"not continue.", (ftnlen)255, (ftnlen)67);
	tostdo_(" ", (ftnlen)1);
	tostdo_(line, (ftnlen)255);
	tostdo_(" ", (ftnlen)1);
	s_stop("", (ftnlen)0);
    }

/*     Display the commercial. */

    for (i__ = 1; i__ <= 7; ++i__) {
	tostdo_(cmrcl + ((i__2 = i__ - 1) < 7 && 0 <= i__2 ? i__2 : s_rnge(
		"cmrcl", i__2, "spacit_", (ftnlen)562)) * 80, (ftnlen)80);
    }

/*     Initialize the flag which indicates whether a logfile has */
/*     been created. */

    logfil = FALSE_;

/*     We have only initialized things and displayed a commercial, so we */
/*     are not done. */

    done = FALSE_;

/*     While there is still more to do ... */

    while(! done) {

/*        We initialize a few things here so that they get reset for */
/*        every trip through the loop. */

/*        Initialize the logical flags that we use and */

	contnu = TRUE_;

/*        the option. */

	s_copy(option, " ", (ftnlen)20, (ftnlen)1);

/*        Get the option to be performed from the main menu. */

	tostdo_(" ", (ftnlen)1);
	getopt_(maintl, &c__6, mainnm, maintx, &iopt, (ftnlen)20, (ftnlen)1, (
		ftnlen)40);
	if (failed_()) {
	    contnu = FALSE_;
	}

/*        Process the option that was selected. */

	if (contnu) {
	    s_copy(option, mainvl + ((i__2 = iopt - 1) < 6 && 0 <= i__2 ? 
		    i__2 : s_rnge("mainvl", i__2, "spacit_", (ftnlen)604)) * 
		    20, (ftnlen)20, (ftnlen)20);
	    if (s_cmp(option, "QUIT", (ftnlen)20, (ftnlen)4) == 0) {
		s_copy(line, "   Quitting SPACIT.", (ftnlen)255, (ftnlen)19);
		tostdo_(" ", (ftnlen)1);
		tostdo_(line, (ftnlen)255);
		tostdo_(" ", (ftnlen)1);
		done = TRUE_;
	    } else if (s_cmp(option, "TRANSFER_TO_BINARY", (ftnlen)20, (
		    ftnlen)18) == 0) {
		tostdo_(" ", (ftnlen)1);
		spat2b_(&logfil, &loglun);
	    } else if (s_cmp(option, "BINARY_TO_TRANSFER", (ftnlen)20, (
		    ftnlen)18) == 0) {
		tostdo_(" ", (ftnlen)1);
		spab2t_(&logfil, &loglun);
	    } else if (s_cmp(option, "SUMMARIZE", (ftnlen)20, (ftnlen)9) == 0)
		     {
		tostdo_(" ", (ftnlen)1);
		spasum_(&logfil, &loglun);
	    } else if (s_cmp(option, "READ_COMMENTS", (ftnlen)20, (ftnlen)13) 
		    == 0) {
		tostdo_(" ", (ftnlen)1);
		spardc_(&logfil, &loglun);
	    } else if (s_cmp(option, "START_LOG_FILE", (ftnlen)20, (ftnlen)14)
		     == 0) {
		tostdo_(" ", (ftnlen)1);
		spalog_("7.1.1", &logfil, &loglun, (ftnlen)5);
	    } else {

/*              The program should never be able to get here, but */
/*              because more options may be added in the future, I */
/*              figured that it would be a good idea to include */
/*              this case. */

		contnu = FALSE_;
		s_copy(line, "'#' is not a recognized option.", (ftnlen)255, (
			ftnlen)31);
		repmc_(line, "#", mainvl + ((i__2 = iopt - 1) < 6 && 0 <= 
			i__2 ? i__2 : s_rnge("mainvl", i__2, "spacit_", (
			ftnlen)650)) * 20, line, (ftnlen)255, (ftnlen)1, (
			ftnlen)20, (ftnlen)255);
		tostdo_(" ", (ftnlen)1);
		tostdo_(line, rtrim_(line, (ftnlen)255));
		tostdo_(" ", (ftnlen)1);
	    }
	}

/*        Just in case something is still wrong, call reset again and */
/*        try to continue. */

	if (failed_()) {
	    reset_();
	}
    }
    chkout_("SPACIT", (ftnlen)6);
    byebye_("SUCCESS", (ftnlen)7);
    return 0;
} /* MAIN__ */

/* Main program alias */ int spacit_ () { MAIN__ (); return 0; }
