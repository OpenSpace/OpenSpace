/* sumck.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;
static integer c__9 = 9;
static integer c__1 = 1;
static integer c__3 = 3;
static integer c__0 = 0;

/* $Procedure      SUMCK ( Summarize a CK file ) */
/* Subroutine */ int sumck_(integer *handle, char *binfnm, char *lpsfnm, char 
	*sclfnm, logical *logfil, integer *loglun, ftnlen binfnm_len, ftnlen 
	lpsfnm_len, ftnlen sclfnm_len)
{
    /* Initialized data */

    static char menutl[20] = "CK Summary Options  ";
    static char menuvl[20*6] = "QUIT                " "Skip                " 
	    "ENTIRE_FILE         " "BY_INSTRUMENT_ID    " "BY_UTC_INTERVAL  "
	    "   " "BY_SCLK_INTERVAL    ";
    static char menutx[40*6] = "Quit, returning to main menu.           " 
	    "Skip                                    " "Summarize entire fil"
	    "e.                  " "Summarize by NAIF instrument ID code.   " 
	    "Summarize by UTC time interval.         " "Summarize by SCLK ti"
	    "me interval.        ";
    static char menunm[1*6] = "Q" "." "F" "I" "U" "S";

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen), s_wsle(cilist *), e_wsle(void), do_lio(integer *,
	     integer *, char *, ftnlen);

    /* Local variables */
    static logical done;
    static char line[255];
    extern /* Subroutine */ int sct2e_(integer *, doublereal *, doublereal *);
    extern integer cardd_(doublereal *);
    static doublereal beget;
    static char segid[40];
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static char bsclk[32];
    static doublereal endet;
    static char esclk[32];
    extern /* Subroutine */ int repmc_(char *, char *, char *, char *, ftnlen,
	     ftnlen, ftnlen, ftnlen);
    static char separ[80];
    static logical found;
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen), ckgss_(char *, integer *, integer *, 
	    integer *, integer *, doublereal *, doublereal *, integer *, 
	    integer *, ftnlen), reset_(void);
    static logical error;
    extern /* Subroutine */ int ckwss_(integer *, char *, integer *, integer *
	    , integer *, integer *, doublereal *, doublereal *, ftnlen);
    extern integer rtrim_(char *, ftnlen);
    extern /* Subroutine */ int utc2et_(char *, doublereal *, ftnlen), 
	    et2utc_(doublereal *, char *, integer *, char *, ftnlen, ftnlen), 
	    daffna_(logical *);
    extern logical failed_(void);
    static integer segbad;
    extern /* Subroutine */ int scdecd_(integer *, doublereal *, char *, 
	    ftnlen), dafbfs_(integer *);
    static integer segead;
    static doublereal begscl;
    extern /* Subroutine */ int scardd_(integer *, doublereal *), scencd_(
	    integer *, char *, doublereal *, ftnlen);
    static logical segfnd;
    static doublereal endscl;
    static char begutc[32];
    extern /* Subroutine */ int cnfirm_(char *, logical *, ftnlen), getchr_(
	    char *, char *, logical *, logical *, char *, ftnlen, ftnlen, 
	    ftnlen);
    static logical haveit;
    static char endutc[32];
    static integer segfrm;
    static doublereal segbtm, segetm;
    static integer instid, segins;
    static doublereal segint[8];
    static logical anyseg;
    extern /* Subroutine */ int getint_(char *, integer *, logical *, logical 
	    *, char *, ftnlen, ftnlen);
    static char errmsg[320], option[20], sumsep[80];
    extern logical return_(void);
    static char fnmout[255], sclout[255];
    static integer missin;
    static char lpsout[255];
    static integer menuop, segrts;
    static char tmpstr[80];
    static integer segtyp;
    static doublereal intrvl[8], intsct[8];
    static logical contnu, tryagn;
    extern /* Subroutine */ int ssized_(integer *, doublereal *), writln_(
	    char *, integer *, ftnlen), getopt_(char *, integer *, char *, 
	    char *, integer *, ftnlen, ftnlen, ftnlen), wninsd_(doublereal *, 
	    doublereal *, doublereal *), wnintd_(doublereal *, doublereal *, 
	    doublereal *);
    static char typout[255];
    extern /* Subroutine */ int chkout_(char *, ftnlen);

    /* Fortran I/O blocks */
    static cilist io___23 = { 0, 6, 0, 0, 0 };
    static cilist io___24 = { 0, 6, 0, 0, 0 };
    static cilist io___25 = { 0, 6, 0, 0, 0 };
    static cilist io___26 = { 0, 6, 0, 0, 0 };
    static cilist io___27 = { 0, 6, 0, 0, 0 };
    static cilist io___28 = { 0, 6, 0, 0, 0 };
    static cilist io___29 = { 0, 6, 0, 0, 0 };
    static cilist io___30 = { 0, 6, 0, 0, 0 };
    static cilist io___32 = { 0, 6, 0, 0, 0 };
    static cilist io___33 = { 0, 6, 0, 0, 0 };
    static cilist io___34 = { 0, 6, 0, 0, 0 };
    static cilist io___36 = { 0, 6, 0, 0, 0 };
    static cilist io___37 = { 0, 6, 0, 0, 0 };
    static cilist io___38 = { 0, 6, 0, 0, 0 };
    static cilist io___39 = { 0, 6, 0, 0, 0 };
    static cilist io___41 = { 0, 6, 0, 0, 0 };
    static cilist io___42 = { 0, 6, 0, 0, 0 };
    static cilist io___43 = { 0, 6, 0, 0, 0 };
    static cilist io___44 = { 0, 6, 0, 0, 0 };
    static cilist io___46 = { 0, 6, 0, 0, 0 };
    static cilist io___47 = { 0, 6, 0, 0, 0 };
    static cilist io___48 = { 0, 6, 0, 0, 0 };
    static cilist io___49 = { 0, 6, 0, 0, 0 };
    static cilist io___51 = { 0, 6, 0, 0, 0 };
    static cilist io___52 = { 0, 6, 0, 0, 0 };
    static cilist io___53 = { 0, 6, 0, 0, 0 };
    static cilist io___54 = { 0, 6, 0, 0, 0 };
    static cilist io___56 = { 0, 6, 0, 0, 0 };
    static cilist io___57 = { 0, 6, 0, 0, 0 };
    static cilist io___58 = { 0, 6, 0, 0, 0 };
    static cilist io___59 = { 0, 6, 0, 0, 0 };
    static cilist io___60 = { 0, 6, 0, 0, 0 };
    static cilist io___61 = { 0, 6, 0, 0, 0 };
    static cilist io___62 = { 0, 6, 0, 0, 0 };
    static cilist io___63 = { 0, 6, 0, 0, 0 };
    static cilist io___65 = { 0, 6, 0, 0, 0 };
    static cilist io___66 = { 0, 6, 0, 0, 0 };
    static cilist io___67 = { 0, 6, 0, 0, 0 };
    static cilist io___68 = { 0, 6, 0, 0, 0 };
    static cilist io___70 = { 0, 6, 0, 0, 0 };
    static cilist io___71 = { 0, 6, 0, 0, 0 };
    static cilist io___72 = { 0, 6, 0, 0, 0 };
    static cilist io___73 = { 0, 6, 0, 0, 0 };
    static cilist io___75 = { 0, 6, 0, 0, 0 };
    static cilist io___76 = { 0, 6, 0, 0, 0 };
    static cilist io___77 = { 0, 6, 0, 0, 0 };
    static cilist io___78 = { 0, 6, 0, 0, 0 };
    static cilist io___80 = { 0, 6, 0, 0, 0 };


/* $ Abstract */

/*     Summarize a CK file. */

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

/* $ Declarations */

/*     Set the number of double precision components in an unpacked CK */
/*     descriptor. */


/*     Set the number of integer components in an unpacked CK descriptor. */


/*     Set the size of a packed CK descriptor. */


/*     Set the length of a CK segment identifier. */


/*     Set the value for the lower bound of the CELL data type. */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of the SPK file to be summarized. */
/*     LOGFIL     I   Write the summary to a log file and to screen? */
/*     LOGLUN     I   Logical unit connected to the log file. */
/*     NDC        P   Number of d.p. components in SPK descriptor. */
/*     NIC        P   Number of integer components in SPK descriptor. */
/*     NC         P   Size of packed SPK descriptor. */
/*     IDSIZ      P   Length of SPK segment identifier. */
/*     LBCELL     P   Lower bound for the SPICELIB CELL data structure. */

/* $ Detailed_Input */

/*     HANDLE     is the integer handle of the CK file to be summarized. */

/*     LOGFIL     if TRUE means that the summary will be written to */
/*                a log file as well as displayed on the terminal */
/*                screen.  Otherwise, the summary will not be written */
/*                to a log file. */

/*     LOGLUN     is the logical unit connected to a log file to which */
/*                the summary is to be written if LOGFIL is TRUE. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     NDC        is the number of double precision components in an */
/*                unpacked SPK descriptor. */

/*     NIC        is the number of integer components in an unpacked */
/*                SPK descriptor. */

/*     NC         is the size of a packed SPK descriptor. */

/*     IDSIZ      is the length of an SPK segment identifier. */

/*     LBCELL     is the lower bound for the SPICELIB CELL data */
/*                structure. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     The CK file to be summarized is referred throughout this routine */
/*     by its handle. The file should already be opened for read. */

/* $ Particulars */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     K.R. Gehringer  (JPL) */
/*     M.J. Spencer    (JPL) */
/*     J.E. McLean     (JPL) */
/*     R.E. Thurman    (JPL) */

/* $ Version */

/* -    Beta Version 5.0.0  21-JUL-1995 (KRG) */

/*        Added several arguments to the call of this subroutine and */
/*        made other modifications to allow it to perform its own */
/*        formatting of the summary, including filenames and separators. */

/* -    Beta Version 4.0.0  11-APR-1994 (KRG) */

/*        Modified this routine to make use of new routines to get and */
/*        format and write CK segment summaries. */

/*        Added a missing $ Index_Entries header section. */

/*        Fixed a few typos in the header. */

/*        The routine DISPC is now obsolete. It is no longer used. */

/* -    Beta Version 3.0.0  22-MAR-1993 (KRG) */

/*        1) Changed the names of the variables TOFILE and UNIT to LOGFIL */
/*           and LOGLUN, respectively. */

/*        2) Updated the program to use the menuing subroutine GETOPT */
/*           which removes the need for the routine QSUMC. Redesigned */
/*           the case sructure of the code to facilitate the use of the */
/*           menuing routine. */

/*        3) Rearranged some of thee initializations that were performed, */
/*           moved several calls to SCARDD outside the main loop, etc. */

/*        5) Performed some general cleanup as deemed necessary. */

/* -    Beta Version 2.1.0  20-NOV-1991 (MJS) */

/*        Checked FAILED function in main loop. */

/* -    Beta Version 2.0.0  17-JUN-1991 (JEM) */

/*        1.  Added the arguments TOFILE and UNIT.  Previously the */
/*            summary was only displayed on the terminal screen. */
/*            Now, if requested by TOFILE, the summary is also */
/*            written to the file connected to UNIT. */

/*        2.  A user may cancel a task selected in QSUMC and */
/*            select another. */

/* -    SPICELIB Version 1.1.0  31-AUG-1990 (JEM) */

/*        This routine was updated due to changes in the CK and */
/*        SCLK design.  Also, several implementation-specific */
/*        parameters were moved from the header to the local */
/*        parameters section. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (RET) */

/* -& */
/* $ Index_Entries */

/*      summarize the segments in a binary ck file */

/* -& */
/* $ Revisions */

/* -    Beta Version 5.0.0  21-JUL-1995 (KRG) */

/*        Added several arguments to the call of this subroutine and */
/*        made other modifications to allow it to perform its own */
/*        formatting of the summary, including filenames and separators. */

/* -    Beta Version 4.0.0  11-APR-1994 (KRG) */

/*        Modified this routine to make use of new routines to get and */
/*        format and write CK segment summaries. */

/*        Added a missing $ Index_Entries header section. */

/*        Fixed a few typos in the header. */

/*        The routine DISPC is now obsolete. It is no longer used. */

/* -    Beta Version 3.0.0  22-MAR-1993 (KRG) */

/*        1) Changed the names of the variables TOFILE and UNIT to LOGFIL */
/*           and LOGLUN, respectively. */

/*        2) Updated the program to use the menuing subroutine GETOPT */
/*           which removes the need for the routine QSUMC. Redesigned */
/*           the case sructure of the code to facilitate the use of the */
/*           menuing routine. */

/*        3) Rearranged some of thee initializations that were performed, */
/*           moved several calls to SCARDD outside the main loop, etc. */

/*        5) Performed some general cleanup as deemed necessary. */

/* -    Beta Version 2.1.0  20-NOV-1991 (MJS) */

/*        Checked FAILED function in main loop. In the previous version, */
/*        if any time conversion produced an error, the summary would go */
/*        in an endless loop. */

/* -    Beta Version 2.0.0  22-MAY-1991 (JEM) */

/*        1.  In addition to adding the arguments TOFILE and UNIT to */
/*            the calling sequence, the following code changes were */
/*            made. The two new arguments were added to the calling */
/*            sequence of DISPC as well.  If TOFILE is true, a */
/*            description of the type of summary is written to the */
/*            output file before calling DISPC to write the summary. */
/*            If no segments are found, the message is written to the */
/*            output file as well as the terminal screen when */
/*            TOFILE is true. */

/*        2.  QSUMC was changed.  'NONE' is now a possible task */
/*            returned from QSUMC and means a task was selected, */
/*            then cancelled.  QSUMC is called repeatedly until the */
/*            task returned is something other than NONE.  In */
/*            this way the user is able to select another task. */

/* -    SPICELIB Version 1.1.0  31-AUG-1990 (JEM) */

/*        This routine was updated to handle these changes to the */
/*        C-kernel design: */

/*           1.  Ephemeris time is no longer included in CK files. */
/*               All data is associated with spacecraft clock time only. */
/*               The segment descriptor no longer contains the */
/*               start and stop ET.  Thus, the number of double */
/*               precision components (NDC) is now two instead of four. */

/*           2.  Segments may now contain rate information, along with */
/*               pointing data.  The segment descriptor contains a flag */
/*               that indicates whether or not the segment includes */
/*               rate information.  Thus, the number of integer */
/*               components (NIC) is now six instead of five. */

/*        This version of SUMCK converts encoded SCLK times to ET for */
/*        comparison with input times which are converted from UTC to ET. */

/*        This routine was also updated to handle these changes to the */
/*        SCLK design: */

/*           1.  The name of the routine that encodes spacecraft */
/*               clock time was changed from ENSCLK to SCENCD, and */
/*               the order of arguments in the calling sequence */
/*               was changed. */

/*           2.  Instrument ID codes are now negative integers to */
/*               avoid conflict with other body id codes. */

/*        The parameters that pertain to the CK file architecture, */
/*        like the number of double precision components in the */
/*        segment descriptor (NDC), were moved from the header */
/*        to the local parameter section.  These parameters are */
/*        implementation specific.  Further, the user is not invited */
/*        to change them, nor are they needed in any argument */
/*        declaration.  Thus they do not belong in the header. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*     Set value for a separator */


/*     Set up the instrument ID code prompt. */


/*     Set up the spacecraft ID code prompt. */


/*     Set up the SCLK time string prompt. */


/*     Set up labels for various output things. */


/*     Set up the UTC time string prompt. */


/*     Set the length for a line of text. */


/*     Set the length for an output line. */


/*     Set the length for an error message. */


/*     Set the length for a UTC time string. */


/*     Set the precision for the fractional part of UTC times. */


/*     Set a length for the option values. */


/*     Set a length for the title of a menu. */



/*     Set the length of the text description of an option on a menu. */


/*     The number of options available on the main menu. */


/*     Parameter for the standard output unit. */


/*     Local variables */


/*     Save everything to keep control happy. */


/*     Initial Values */

/*     Define the menu title ... */


/*     Define the menu option values ... */


/*     Define the menu descriptive text for each option ... */


/*     Define the menu option names ... */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SUMCK", (ftnlen)5);
    }

/*     Initialize the separator. */

    s_copy(separ, "*********************************************************"
	    "***********************", (ftnlen)80, (ftnlen)80);

/*     Initialize the segment separator. */

    s_copy(sumsep, "--------------------------------------------------------"
	    "------------------------", (ftnlen)80, (ftnlen)80);

/*     Set the sizes of the window cells that we will use if the file */
/*     is to be summarized by time. */

    ssized_(&c__2, intrvl);
    ssized_(&c__2, segint);
    ssized_(&c__2, intsct);

/*     Initialize a few things before we start. */

    instid = 0;
    done = FALSE_;
    while(! done) {

/*        Initialize those things we reuse on every iteration. */

	contnu = TRUE_;
	writln_(" ", &c__6, (ftnlen)1);
	getopt_(menutl, &c__6, menunm, menutx, &menuop, (ftnlen)20, (ftnlen)1,
		 (ftnlen)40);
	if (failed_()) {
	    contnu = FALSE_;
	}
	if (contnu) {

/*           Perform all of the setup necessary to perform the summary. */
/*           This include prompting for input values required, etc. */

	    repmc_("Summary for CK file: #", "#", binfnm, fnmout, (ftnlen)22, 
		    (ftnlen)1, binfnm_len, (ftnlen)255);
	    repmc_("Leapseconds File   : #", "#", lpsfnm, lpsout, (ftnlen)22, 
		    (ftnlen)1, lpsfnm_len, (ftnlen)255);
	    repmc_("SCLK File          : #", "#", sclfnm, sclout, (ftnlen)22, 
		    (ftnlen)1, sclfnm_len, (ftnlen)255);
	    s_copy(option, menuvl + ((i__1 = menuop - 1) < 6 && 0 <= i__1 ? 
		    i__1 : s_rnge("menuvl", i__1, "sumck_", (ftnlen)553)) * 
		    20, (ftnlen)20, (ftnlen)20);
	    if (s_cmp(option, "QUIT", (ftnlen)20, (ftnlen)4) == 0) {
		contnu = FALSE_;
		done = TRUE_;
	    } else if (s_cmp(option, "ENTIRE_FILE", (ftnlen)20, (ftnlen)11) ==
		     0) {

/*              Summarize the entire file. */

		repmc_("Summary Type       : #", "#", "Entire File", typout, (
			ftnlen)22, (ftnlen)1, (ftnlen)11, (ftnlen)255);
	    } else if (s_cmp(option, "BY_INSTRUMENT_ID", (ftnlen)20, (ftnlen)
		    16) == 0) {

/*              Summarize for a specified body. */

/*              First, we need to get the instrument ID code. */

		s_copy(errmsg, " ", (ftnlen)320, (ftnlen)1);
		haveit = FALSE_;
		tryagn = TRUE_;
		while(tryagn) {
		    error = FALSE_;
		    s_wsle(&io___23);
		    e_wsle();
		    s_wsle(&io___24);
		    do_lio(&c__9, &c__1, "Enter the desired NAIF instrument "
			    "code.", (ftnlen)39);
		    e_wsle();
		    s_wsle(&io___25);
		    e_wsle();
		    getint_("Instrument ID code? ", &instid, &haveit, &error, 
			    errmsg, (ftnlen)20, (ftnlen)320);
		    if (! haveit || error) {
			if (error) {
			    s_wsle(&io___26);
			    e_wsle();
			    s_wsle(&io___27);
			    do_lio(&c__9, &c__1, errmsg, rtrim_(errmsg, (
				    ftnlen)320));
			    e_wsle();
			}
			if (! haveit) {
			    s_wsle(&io___28);
			    e_wsle();
			    s_wsle(&io___29);
			    do_lio(&c__9, &c__1, "A NAIF instrument ID code "
				    "must be entered for this option.", (
				    ftnlen)58);
			    e_wsle();
			}
			if (! haveit || error) {
			    s_wsle(&io___30);
			    e_wsle();
			    cnfirm_("Try Again? (Yes/No) ", &tryagn, (ftnlen)
				    20);
			    if (! tryagn) {
				contnu = FALSE_;
			    }
			}
		    } else {
			tryagn = FALSE_;
		    }
		}

/*              Write the type of summary to the log file if we need to. */

		if (contnu) {
		    s_copy(tmpstr, "By Instrument ID #", (ftnlen)80, (ftnlen)
			    18);
		    repmc_("Summary Type       : #", "#", tmpstr, typout, (
			    ftnlen)22, (ftnlen)1, (ftnlen)80, (ftnlen)255);
		    repmi_(typout, "#", &instid, typout, (ftnlen)255, (ftnlen)
			    1, (ftnlen)255);
		}
	    } else if (s_cmp(option, "BY_UTC_INTERVAL", (ftnlen)20, (ftnlen)
		    15) == 0) {

/*              Summarize for given UTC time interval. */

/*              First, we need to get the UTC time string for the */
/*              begin time. */

		s_copy(errmsg, " ", (ftnlen)320, (ftnlen)1);
		haveit = FALSE_;
		tryagn = TRUE_;
		while(tryagn) {
		    error = FALSE_;
		    s_wsle(&io___32);
		    e_wsle();
		    s_wsle(&io___33);
		    do_lio(&c__9, &c__1, "Enter the desired beginning UTC ti"
			    "me.", (ftnlen)37);
		    e_wsle();
		    s_wsle(&io___34);
		    e_wsle();
		    getchr_("UTC time? ", begutc, &haveit, &error, errmsg, (
			    ftnlen)10, (ftnlen)32, (ftnlen)320);
		    if (! haveit || error) {
			if (error) {
			    s_wsle(&io___36);
			    e_wsle();
			    s_wsle(&io___37);
			    do_lio(&c__9, &c__1, errmsg, rtrim_(errmsg, (
				    ftnlen)320));
			    e_wsle();
			}
			if (! haveit) {
			    s_wsle(&io___38);
			    e_wsle();
			    s_wsle(&io___39);
			    do_lio(&c__9, &c__1, "A beginning UTC time strin"
				    "g must be entered for this option.", (
				    ftnlen)60);
			    e_wsle();
			}
		    } else {
			tryagn = FALSE_;
		    }

/*                 We now have the beginning time in UTC, so attempt */
/*                 to convert it to ET. If the conversion fails, we */
/*                 need to immediately reset the error handling so that */
/*                 we can continue processing. Remember, we are in a */
/*                 menuing subroutine, and we are not allowed to exit */
/*                 on an error: we must go back to the menu. thus the */
/*                 need for a resetting of the error handler here. If */
/*                 we got to here, there were no errors, so as long as */
/*                 we maintain that status, everything will be hunky */
/*                 dory. We also convert the ET back into UTC to get */
/*                 a consistent format for display. */

		    if (haveit) {
			utc2et_(begutc, &beget, (ftnlen)32);
			et2utc_(&beget, "C", &c__3, begutc, (ftnlen)1, (
				ftnlen)32);
			if (failed_()) {
			    reset_();
			    error = TRUE_;
			}
		    }

/*                 Check to see if they want to try and enter the */
/*                 beginning UTC time string again. */

		    if (! haveit || error) {
			s_wsle(&io___41);
			e_wsle();
			cnfirm_("Try Again? (Yes/No) ", &tryagn, (ftnlen)20);
			if (! tryagn) {
			    contnu = FALSE_;
			}
		    }
		}
/*              Now, if we can, we need to get the UTC time string for */
/*              the end time. */

		if (contnu) {
		    s_copy(errmsg, " ", (ftnlen)320, (ftnlen)1);
		    haveit = FALSE_;
		    tryagn = TRUE_;
		    while(tryagn) {
			error = FALSE_;
			s_wsle(&io___42);
			e_wsle();
			s_wsle(&io___43);
			do_lio(&c__9, &c__1, "Enter the desired ending UTC t"
				"ime.", (ftnlen)34);
			e_wsle();
			s_wsle(&io___44);
			e_wsle();
			getchr_("UTC time? ", endutc, &haveit, &error, errmsg,
				 (ftnlen)10, (ftnlen)32, (ftnlen)320);
			if (! haveit || error) {
			    if (error) {
				s_wsle(&io___46);
				e_wsle();
				s_wsle(&io___47);
				do_lio(&c__9, &c__1, errmsg, rtrim_(errmsg, (
					ftnlen)320));
				e_wsle();
			    }
			    if (! haveit) {
				s_wsle(&io___48);
				e_wsle();
				s_wsle(&io___49);
				do_lio(&c__9, &c__1, "An ending UTC time str"
					"ing must be entered for this option.",
					 (ftnlen)58);
				e_wsle();
			    }
			} else {
			    tryagn = FALSE_;
			}

/*                    We now have the ending time in UTC, so attempt */
/*                    to convert it to ET. If the conversion fails, we */
/*                    need to immediately reset the error handling so */
/*                    that we can continue processing. Remember, we are */
/*                    in a menuing subroutine, and we are not allowed */
/*                    to exit on an error: we must go back to the menu. */
/*                    thus the need for a resetting of the error handler */
/*                    here. If we got to here, there were no errors, so */
/*                    as long as we maintain that status, everything */
/*                    will be hunky dory. We also convert the ET back */
/*                    into UTC to get a consistent format for display. */

			if (haveit) {
			    utc2et_(endutc, &endet, (ftnlen)32);
			    et2utc_(&endet, "C", &c__3, endutc, (ftnlen)1, (
				    ftnlen)32);
			    if (failed_()) {
				reset_();
				error = TRUE_;
			    }
			}

/*                    Check to see if they want to try and enter the */
/*                    beginning UTC time string again. */

			if (! haveit || error) {
			    s_wsle(&io___51);
			    e_wsle();
			    cnfirm_("Try Again? (Yes/No) ", &tryagn, (ftnlen)
				    20);
			    if (! tryagn) {
				contnu = FALSE_;
			    }
			} else {
			    tryagn = FALSE_;
			}
		    }
		}

/*              Create an interval out of the begin and end ET times, */
/*              if we can. */

		if (contnu) {
		    scardd_(&c__0, intrvl);
		    wninsd_(&beget, &endet, intrvl);
		    if (failed_()) {
			contnu = FALSE_;
		    }
		}

/*              Write the type of summary to the output file, if we can. */

		if (contnu) {
		    s_copy(tmpstr, "By # Time Interval #", (ftnlen)80, (
			    ftnlen)20);
		    repmc_("Summary Type       : #", "#", tmpstr, typout, (
			    ftnlen)22, (ftnlen)1, (ftnlen)80, (ftnlen)255);
		    repmc_(typout, "#", "UTC", typout, (ftnlen)255, (ftnlen)1,
			     (ftnlen)3, (ftnlen)255);
		    repmc_(typout, "#", "(#, #)", typout, (ftnlen)255, (
			    ftnlen)1, (ftnlen)6, (ftnlen)255);
		    repmc_(typout, "#", begutc, typout, (ftnlen)255, (ftnlen)
			    1, (ftnlen)32, (ftnlen)255);
		    repmc_(typout, "#", endutc, typout, (ftnlen)255, (ftnlen)
			    1, (ftnlen)32, (ftnlen)255);
		}
	    } else if (s_cmp(option, "BY_SCLK_INTERVAL", (ftnlen)20, (ftnlen)
		    16) == 0) {

/*              Summarize for given SCLK time interval. */

/*              First, we need to get spacecraft ID code. */

		s_copy(errmsg, " ", (ftnlen)320, (ftnlen)1);
		haveit = FALSE_;
		tryagn = TRUE_;
		while(tryagn) {
		    error = FALSE_;
		    s_wsle(&io___52);
		    e_wsle();
		    s_wsle(&io___53);
		    do_lio(&c__9, &c__1, "Enter the desired NAIF spacecraft "
			    "ID code.", (ftnlen)42);
		    e_wsle();
		    s_wsle(&io___54);
		    e_wsle();
		    getint_("Spacecraft ID code? ", &missin, &haveit, &error, 
			    errmsg, (ftnlen)20, (ftnlen)320);
		    if (! haveit || error) {
			if (error) {
			    s_wsle(&io___56);
			    e_wsle();
			    s_wsle(&io___57);
			    do_lio(&c__9, &c__1, errmsg, rtrim_(errmsg, (
				    ftnlen)320));
			    e_wsle();
			}
			if (! haveit) {
			    s_wsle(&io___58);
			    e_wsle();
			    s_wsle(&io___59);
			    do_lio(&c__9, &c__1, "A NAIF spacecraft ID code "
				    "must be entered for this option.", (
				    ftnlen)58);
			    e_wsle();
			}
			if (! haveit || error) {
			    s_wsle(&io___60);
			    e_wsle();
			    cnfirm_("Try Again? (Yes/No) ", &tryagn, (ftnlen)
				    20);
			    if (! tryagn) {
				contnu = FALSE_;
			    }
			}
		    } else {
			tryagn = FALSE_;
		    }
		}

/*              Now, we need to get the SCLK time string for the */
/*              begin time. */

		if (contnu) {
		    s_copy(errmsg, " ", (ftnlen)320, (ftnlen)1);
		    haveit = FALSE_;
		    tryagn = TRUE_;
		    while(tryagn) {
			error = FALSE_;
			s_wsle(&io___61);
			e_wsle();
			s_wsle(&io___62);
			do_lio(&c__9, &c__1, "Enter the desired beginning SC"
				"LK time.", (ftnlen)38);
			e_wsle();
			s_wsle(&io___63);
			e_wsle();
			getchr_("SCLK time? ", bsclk, &haveit, &error, errmsg,
				 (ftnlen)11, (ftnlen)32, (ftnlen)320);
			if (! haveit || error) {
			    if (error) {
				s_wsle(&io___65);
				e_wsle();
				s_wsle(&io___66);
				do_lio(&c__9, &c__1, errmsg, rtrim_(errmsg, (
					ftnlen)320));
				e_wsle();
			    }
			    if (! haveit) {
				s_wsle(&io___67);
				e_wsle();
				s_wsle(&io___68);
				do_lio(&c__9, &c__1, "A beginning SCLK time "
					"string must be entered for this opti"
					"on.", (ftnlen)61);
				e_wsle();
			    }
			} else {
			    tryagn = FALSE_;
			}

/*                    We now have the beginning time in SCLK, so attempt */
/*                    to convert it to ET. If the conversion fails, we */
/*                    need to immediately reset the error handling so */
/*                    that we can continue processing. Remember, we are */
/*                    in a menuing subroutine, and we are not allowed to */
/*                    exit on an error: we must go back to the menu. thus */
/*                    the need for a resetting of the error handler here. */
/*                    If we got to here, there were no errors, so as long */
/*                    as we maintain that status, everything will be */
/*                    hunky dory. We also convert the ET back into SCLK, */
/*                    and UTC to get a consistent format for display. */

			if (haveit) {
			    scencd_(&missin, bsclk, &begscl, (ftnlen)32);
			    sct2e_(&missin, &begscl, &beget);
			    et2utc_(&beget, "C", &c__3, begutc, (ftnlen)1, (
				    ftnlen)32);
			    scdecd_(&missin, &begscl, bsclk, (ftnlen)32);
			    if (failed_()) {
				reset_();
				error = TRUE_;
			    }
			}

/*                    Check to see if they want to try and enter the */
/*                    beginning UTC time string again. */

			if (! haveit || error) {
			    s_wsle(&io___70);
			    e_wsle();
			    cnfirm_("Try Again? (Yes/No) ", &tryagn, (ftnlen)
				    20);
			    if (! tryagn) {
				contnu = FALSE_;
			    }
			}
		    }
		}
/*              Now, if we can, we need to get the UTC time string for */
/*              the end time. */

		if (contnu) {
		    s_copy(errmsg, " ", (ftnlen)320, (ftnlen)1);
		    haveit = FALSE_;
		    tryagn = TRUE_;
		    while(tryagn) {
			error = FALSE_;
			s_wsle(&io___71);
			e_wsle();
			s_wsle(&io___72);
			do_lio(&c__9, &c__1, "Enter the desired ending SCLK "
				"time.", (ftnlen)35);
			e_wsle();
			s_wsle(&io___73);
			e_wsle();
			getchr_("SCLK time? ", esclk, &haveit, &error, errmsg,
				 (ftnlen)11, (ftnlen)32, (ftnlen)320);
			if (! haveit || error) {
			    if (error) {
				s_wsle(&io___75);
				e_wsle();
				s_wsle(&io___76);
				do_lio(&c__9, &c__1, errmsg, rtrim_(errmsg, (
					ftnlen)320));
				e_wsle();
			    }
			    if (! haveit) {
				s_wsle(&io___77);
				e_wsle();
				s_wsle(&io___78);
				do_lio(&c__9, &c__1, "An ending SCLK time st"
					"ring must be entered for this option."
					, (ftnlen)59);
				e_wsle();
			    }
			} else {
			    tryagn = FALSE_;
			}

/*                    We now have the ending time in UTC, so attempt */
/*                    to convert it to ET. If the conversion fails, we */
/*                    need to immediately reset the error handling so */
/*                    that we can continue processing. Remember, we are */
/*                    in a menuing subroutine, and we are not allowed */
/*                    to exit on an error: we must go back to the menu. */
/*                    thus the need for a resetting of the error handler */
/*                    here. If we got to here, there were no errors, so */
/*                    as long as we maintain that status, everything */
/*                    will be hunky dory. We also convert the ET back */
/*                    into UTC to get a consistent format for display. */

			if (haveit) {
			    scencd_(&missin, esclk, &endscl, (ftnlen)32);
			    sct2e_(&missin, &endscl, &endet);
			    et2utc_(&endet, "C", &c__3, endutc, (ftnlen)1, (
				    ftnlen)32);
			    scdecd_(&missin, &endscl, esclk, (ftnlen)32);
			    if (failed_()) {
				reset_();
				error = TRUE_;
			    }
			}

/*                    Check to see if they want to try and enter the */
/*                    ending SCLK time string again. */

			if (! haveit || error) {
			    s_wsle(&io___80);
			    e_wsle();
			    cnfirm_("Try Again? (Yes/No) ", &tryagn, (ftnlen)
				    20);
			    if (! tryagn) {
				contnu = FALSE_;
			    }
			} else {
			    tryagn = FALSE_;
			}
		    }
		}

/*              Create an interval out of the begin and end ET times, */
/*              if we can. */

		if (contnu) {
		    scardd_(&c__0, intrvl);
		    wninsd_(&beget, &endet, intrvl);
		    if (failed_()) {
			contnu = FALSE_;
		    }
		}

/*              Write the type of summary to the output file, if we can. */

		if (contnu) {
		    s_copy(tmpstr, "By # Time Interval #", (ftnlen)80, (
			    ftnlen)20);
		    repmc_("Summary Type       : #", "#", tmpstr, typout, (
			    ftnlen)22, (ftnlen)1, (ftnlen)80, (ftnlen)255);
		    repmc_(typout, "#", "SCLK", typout, (ftnlen)255, (ftnlen)
			    1, (ftnlen)4, (ftnlen)255);
		    repmc_(typout, "#", "(#, #)", typout, (ftnlen)255, (
			    ftnlen)1, (ftnlen)6, (ftnlen)255);
		    repmc_(typout, "#", bsclk, typout, (ftnlen)255, (ftnlen)1,
			     (ftnlen)32, (ftnlen)255);
		    repmc_(typout, "#", esclk, typout, (ftnlen)255, (ftnlen)1,
			     (ftnlen)32, (ftnlen)255);
		}
	    }

/*           Now, if we can, search through the file from the beginning. */
/*           Keep track of whether or not any segments satisfy the search */
/*           criteria. */

	    if (contnu) {
		writln_(" ", &c__6, (ftnlen)1);
		writln_(separ, &c__6, (ftnlen)80);
		writln_(" ", &c__6, (ftnlen)1);
		writln_(fnmout, &c__6, (ftnlen)255);
		writln_(lpsout, &c__6, (ftnlen)255);
		writln_(sclout, &c__6, (ftnlen)255);
		writln_(typout, &c__6, (ftnlen)255);
		writln_(" ", &c__6, (ftnlen)1);
		if (*logfil) {
		    writln_(" ", loglun, (ftnlen)1);
		    writln_(separ, loglun, (ftnlen)80);
		    writln_(" ", loglun, (ftnlen)1);
		    writln_(fnmout, loglun, (ftnlen)255);
		    writln_(lpsout, loglun, (ftnlen)255);
		    writln_(sclout, loglun, (ftnlen)255);
		    writln_(typout, loglun, (ftnlen)255);
		    writln_(" ", loglun, (ftnlen)1);
		}
		anyseg = FALSE_;
		dafbfs_(handle);
		daffna_(&found);
		while(found && contnu) {

/*                 On each iteration of the loop, we have not found */
/*                 anything initially. */

		    segfnd = FALSE_;
		    scardd_(&c__0, intsct);
		    scardd_(&c__0, segint);

/*                 Get the descriptor of the segment. */

		    ckgss_(segid, &segins, &segfrm, &segtyp, &segrts, &segbtm,
			     &segetm, &segbad, &segead, (ftnlen)40);

/*                 Check to see if the current segment satisfies the */
/*                 current search criteria. */

		    if (s_cmp(option, "ENTIRE_FILE", (ftnlen)20, (ftnlen)11) 
			    == 0) {
			segfnd = TRUE_;
		    } else if (s_cmp(option, "BY_INSTRUMENT_ID", (ftnlen)20, (
			    ftnlen)16) == 0) {
			segfnd = instid == segins;
		    } else if (s_cmp(option, "BY_UTC_INTERVAL", (ftnlen)20, (
			    ftnlen)15) == 0) {

/*                    Create an interval out of the epochs in the */
/*                    segment. */

			missin = segins / 1000;
			sct2e_(&missin, &segbtm, &beget);
			sct2e_(&missin, &segetm, &endet);
			wninsd_(&beget, &endet, segint);

/*                    Intersect it with the input interval. */

			wnintd_(segint, intrvl, intsct);
			if (failed_()) {
			    reset_();
			    contnu = FALSE_;
			} else {
			    segfnd = cardd_(intsct) > 0;
			}
		    } else if (s_cmp(option, "BY_SCLK_INTERVAL", (ftnlen)20, (
			    ftnlen)16) == 0) {

/*                    Create an interval out of the epochs in the */
/*                    segment. */

			if (missin == segins / 1000) {
			    sct2e_(&missin, &segbtm, &beget);
			    sct2e_(&missin, &segetm, &endet);
			    wninsd_(&beget, &endet, segint);

/*                       Intersect it with the input interval. */

			    wnintd_(segint, intrvl, intsct);
			    if (failed_()) {
				reset_();
				contnu = FALSE_;
			    } else {
				segfnd = cardd_(intsct) > 0;
			    }
			} else {
			    segfnd = FALSE_;
			}
		    }
		    if (contnu && segfnd) {
			anyseg = TRUE_;

/*                    Display the segment summary. */

			writln_(sumsep, &c__6, (ftnlen)80);
			if (*logfil) {
			    writln_(sumsep, loglun, (ftnlen)80);
			}
			ckwss_(&c__6, segid, &segins, &segfrm, &segtyp, &
				segrts, &segbtm, &segetm, (ftnlen)40);
			if (*logfil) {
			    ckwss_(loglun, segid, &segins, &segfrm, &segtyp, &
				    segrts, &segbtm, &segetm, (ftnlen)40);
			}
			writln_(sumsep, &c__6, (ftnlen)80);
			if (*logfil) {
			    writln_(sumsep, loglun, (ftnlen)80);
			}
		    }

/*                 Find that next segment. */

		    daffna_(&found);
		    if (failed_()) {
			contnu = FALSE_;
		    }
		}
	    }

/*           Better say something if no segments were matching the */
/*           search criteria were found. */

	    if (contnu && ! anyseg) {
		s_copy(line, "No matching segments were found.", (ftnlen)255, 
			(ftnlen)32);
		writln_(line, &c__6, (ftnlen)255);
		if (*logfil) {
		    writln_(line, loglun, (ftnlen)255);
		}
	    }
	    if (contnu) {
		writln_(" ", &c__6, (ftnlen)1);
		writln_(separ, &c__6, (ftnlen)80);
		writln_(" ", &c__6, (ftnlen)1);
		if (*logfil) {
		    writln_(" ", loglun, (ftnlen)1);
		    writln_(separ, loglun, (ftnlen)80);
		    writln_(" ", loglun, (ftnlen)1);
		}
	    }
	}

/*        If anything failed, rset the error handling so that we can */
/*        redisplay the menu and keep doing things. */

	if (failed_()) {
	    reset_();
	}
    }
    chkout_("SUMCK", (ftnlen)5);
    return 0;
} /* sumck_ */

