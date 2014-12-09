/* commnt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__1 = 1;
static integer c__5 = 5;
static integer c__6 = 6;
static integer c__0 = 0;

/* $Procedure   COMMNT ( Comment utility program ) */
/* Main program */ MAIN__(void)
{
    /* Initialized data */

    static logical insbln = TRUE_;
    static char maintl[20] = "COMMNT Options      ";
    static char mainvl[20*5] = "QUIT                " "ADD_COMMENTS        " 
	    "READ_COMMENTS       " "EXTRACT_COMMENTS    " "DELETE_COMMENTS  "
	    "   ";
    static char maintx[40*5] = "Quit.                                   " 
	    "Add comments to a binary file.          " "Read the comments in"
	    " a binary file.     " "Extract comments from a binary file.    " 
	    "Delete the comments in a binary file.   ";
    static char mainnm[1*5] = "Q" "A" "R" "E" "D";

    /* System generated locals */
    address a__1[3];
    integer i__1[3], i__2, i__3, i__4, i__5;
    cllist cl__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen), s_cat(char *,
	     char **, integer *, integer *, ftnlen);
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen), f_clos(cllist *);

    /* Local variables */
    static char arch[3];
    static logical done;
    static char line[1000];
    static logical more;
    static integer iopt;
    static char type__[4];
    static integer i__;
    extern /* Subroutine */ int dasdc_(integer *);
    extern integer cardi_(integer *);
    static integer r__;
    extern /* Subroutine */ int spcac_(integer *, integer *, char *, char *, 
	    ftnlen, ftnlen), chkin_(char *, ftnlen), spcec_(integer *, 
	    integer *), spcdc_(integer *), errch_(char *, char *, ftnlen, 
	    ftnlen), repmc_(char *, char *, char *, char *, ftnlen, ftnlen, 
	    ftnlen, ftnlen), reset_(void);
    extern integer rtrim_(char *, ftnlen);
    extern logical failed_(void);
    extern /* Subroutine */ int dafhof_(integer *);
    static integer handle;
    extern /* Subroutine */ int dafcls_(integer *), dasacu_(integer *, char *,
	     char *, logical *, integer *, ftnlen, ftnlen), cleari_(integer *,
	     integer *), delfil_(char *, ftnlen), dasecu_(integer *, integer *
	    , logical *), scardi_(integer *, integer *), dashof_(integer *);
    static logical fileok;
    extern /* Subroutine */ int clcomm_(void), getfat_(char *, char *, char *,
	     ftnlen, ftnlen, ftnlen), cnfirm_(char *, logical *, ftnlen);
    static char fnmtbl[128*2], messag[1000], errmsg[320], messgs[1000*7], 
	    option[20], prmtbl[80*2], statbl[3*2];
    extern logical exists_(char *, ftnlen);
    static integer comlun;
    static char status[1000*2];
    static integer numfnm;
    static char prmpts[80*2];
    static integer numopn, opnset[7], tblidx[2];
    static logical comnts, contnu, ndfnms, tryagn;
    extern /* Subroutine */ int tkvrsn_(char *, char *, ftnlen, ftnlen), 
	    erract_(char *, char *, ftnlen, ftnlen), errprt_(char *, char *, 
	    ftnlen, ftnlen), tostdo_(char *, ftnlen), ssizei_(integer *, 
	    integer *), getopt_(char *, integer *, char *, char *, integer *, 
	    ftnlen, ftnlen, ftnlen), getfnm_(char *, char *, char *, logical *
	    , char *, ftnlen, ftnlen, ftnlen, ftnlen), setmsg_(char *, ftnlen)
	    , sigerr_(char *, ftnlen), txtopr_(char *, integer *, ftnlen), 
	    dafopw_(char *, integer *, ftnlen), dasopw_(char *, integer *, 
	    ftnlen), dascls_(integer *), dafopr_(char *, integer *, ftnlen), 
	    spcrfl_(integer *, char *, logical *, ftnlen), spcrnl_(char *, 
	    logical *, ftnlen), dasopr_(char *, integer *, ftnlen), txtopn_(
	    char *, integer *, ftnlen), chkout_(char *, ftnlen);
    static logical eoc;
    static char tkv[12];

/* $ Abstract */

/*     NAIF Toolkit utility program for adding, reading, extracting, */
/*     and deleting comments from a binary file. */

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

/*     SPC */
/*     DAS */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */
/*     K.R. Gehringer (JPL) */
/*     J.E. McLean    (JPL) */
/*     M.J. Spencer   (JPL) */

/* $ Version */

/* -    Version 6.0.1, 08-MAY-2001 (BVS) */

/*       Increased LINLEN from 255 to 1000 to make it consistent */
/*       with SPICELIB's SPC* and SUPPORT's DAF* internal line sizes. */

/* -    Version 5.0.1, 21-JUL-1997 (WLT) */

/*       Modified the banner at start up so that the version of the */
/*       toolkit used to link COMMNT will be displayed. */

/*       In addition all WRITE statements were replaced by calls to */
/*       TOSTDO. */

/* -    Version 5.0.0, 05-MAY-1994 (KRG) */

/*       Modified the program to use the new file type identification */
/*       capability that was added to spicelib. No file type menu is */
/*       necessary now, as the file type is determined during the */
/*       execution of the program. */

/*       The prompts for the begin and end markers used to extract a */
/*       subset of text lines from an input comment file which were then */
/*       placed into the comment area of a SPICE binary kernel file have */
/*       been removed. The entire input comment file is now placed into */
/*       the comment area of the binary kernel file. This change */
/*       simplifies the user interaction with the program. */

/*       Added support for the new PCK binary kernel files. */

/*       If an error occurs during the extraction of comments to a file, */
/*       the file that was being created is deleted. We cannot know */
/*       whether the file had been successfully created before the error */
/*       occurred. */

/* -    Version 4.0.0, 11-DEC-1992 (KRG) */

/*        Added code to support the E-Kernel, and redesigned the */
/*        user interface. */

/* -    Version 3.1.0, 19-NOV-1991 (MJS) */

/*        Variable QUIT initialized to FALSE. */

/* -    Version 3.0.1, 10-AUG-1991 (CHA) (NJB) */

/*        Updated comments to reflect status as a Toolkit */
/*        utility program.  Message indicating that no comments */
/*        were found in the specified file was changed to include */
/*        the file name. */

/* -    Version 2.0.0, 28-JUN-1991 (JEM) */

/*        The option to read the comments from the comment */
/*        area of a binary SPK or CK was added to the menu. */

/* -    Version 1.0.0, 05-APR-1991 (JEM) */

/* -& */

/*     SPICELIB functions */


/*     Parameters */

/*     Set the version of the comment program. This should be updated */
/*     every time a change is made, and it should agree with the */
/*     version number in the header. */


/*     Set a value for the logical unit which represents the standard */
/*     output device, commonly a terminal. A value of 6 is widely used, */
/*     but the Fortran standard does not specify a value, so it may be */
/*     different for different Fortran implementations. */


/*     Lower bound for a SPICELIB CELL data structure. */


/*     Maximum number of open binary files allowed. */


/*     Set a value for a replacement marker. */


/*     Set a value for a filename prompt. */


/*     File types */


/*     Set a value for the length of a text line. */


/*     Set a value for the length of an error message. */


/*     Set a value for the length of a filename. */


/*     Set a length for the prompts in the prompt table. */


/*     Set a length for the status of a file: 'OLD' or 'NEW'. */


/*     Set the length for the architecture of a file. */


/*     Set the length for the type of a file. */


/*     Set a length for the option values. */


/*     Set a length for the title of a menu. */


/*     Set a length for an option name (what is typed to select it) */
/*     for a menu. */


/*     Set the length of the text description of an option on a menu. */


/*     The number of options available on the main menu. */


/*     Set up some mnemonics for indexing the prompts in the prompt */
/*     table. */


/*     Set the maximum size of the filename table: this must be the */
/*     number of distinct ``types'' of files that the program may */
/*     require. */


/*     Set up some mnemonics for indexing the messages in the message */
/*     table. */


/*     Set the maximum size of the message table: There should be a */
/*     message for each ``type'' of action that the program can take. */


/*     Set up some mnemonics for the OK and not OK status messages. */


/*     Set the maximum number of status messages that are available. */


/*     We need to have TKVLEN characters to hold the current version */
/*     of the toolkit. */


/*     Variables */


/*     We want to insert a blank line between additions if there are */
/*     already comments in the binary file. We indicate this by giving */
/*     the variable INSBLN the value .TRUE.. */


/*     Define the main menu title ... */


/*     Define the main menu option values ... */


/*     Define the main menu descriptive text for each option ... */


/*     Define the main menu option names ... */


/*     Register the COMMNT main program with the SPICELIB error handler. */

    chkin_("COMMNT", (ftnlen)6);
    clcomm_();
    tkvrsn_("TOOLKIT", tkv, (ftnlen)7, (ftnlen)12);
    r__ = rtrim_(tkv, (ftnlen)12);

/*     Set the error action to 'RETURN'. We don't want the program */
/*     to abort if an error is signalled. We check FAILED where */
/*     necessary. If an error is signalled, we'll just handle the */
/*     error, display an appropriate message, then call RESET at the */
/*     end of the loop to continue. */

    erract_("SET", "RETURN", (ftnlen)3, (ftnlen)6);

/*     Set the error messages that we want to have displayed. We will */
/*     diaplay the SPICELIB short and long error messages. This is done */
/*     to ensure that some sort of an error message is displayed if an */
/*     error occurs. In several places, long error messages are not set, */
/*     so if only the long error messages were displayed, it would be */
/*     possible to have an error signalled and not see any error */
/*     information. This is not a very useful thing. */

    errprt_("SET", "NONE, SHORT, LONG, TRACEBACK", (ftnlen)3, (ftnlen)28);

/*     Set up the prompt table for the different types of files. */

    s_copy(prmtbl + 80, "Enter the name of the comment file to be #.", (
	    ftnlen)80, (ftnlen)43);
    s_copy(prmtbl, "Enter the name of the binary file.", (ftnlen)80, (ftnlen)
	    34);

/*     Set up the message table for the different ``types'' of */
/*     operations. The message table contains generic messages which will */
/*     have their missing parts filled in after the option and file type */
/*     havve been selected. */

    s_copy(messgs, "Reading the comment area of the # file.", (ftnlen)1000, (
	    ftnlen)39);
    s_copy(messgs + 1000, "Adding comments to the # file.", (ftnlen)1000, (
	    ftnlen)30);
    s_copy(messgs + 2000, "Extracting comments from the # file.", (ftnlen)
	    1000, (ftnlen)36);
    s_copy(messgs + 3000, "Deleting the comment area of # file.", (ftnlen)
	    1000, (ftnlen)36);
    s_copy(messgs + 4000, "Quitting the program.", (ftnlen)1000, (ftnlen)21);
    s_copy(messgs + 5000, "The comments were successfully #.", (ftnlen)1000, (
	    ftnlen)33);
    s_copy(messgs + 6000, "The comments were NOT successfully #.", (ftnlen)
	    1000, (ftnlen)37);

/*     Display a brief commercial with the name of the program and the */
/*     version. */

    s_copy(line, "   Welcome to COMMNT Version: #", (ftnlen)1000, (ftnlen)31);
    repmc_(line, "#", "6.0.1", line, (ftnlen)1000, (ftnlen)1, (ftnlen)5, (
	    ftnlen)1000);
    tostdo_(" ", (ftnlen)1);
    tostdo_(line, (ftnlen)1000);
/* Writing concatenation */
    i__1[0] = 23, a__1[0] = "        (Spice Toolkit ";
    i__1[1] = r__, a__1[1] = tkv;
    i__1[2] = 1, a__1[2] = ")";
    s_cat(line, a__1, i__1, &c__3, (ftnlen)1000);
    tostdo_(line, (ftnlen)1000);
    tostdo_(" ", (ftnlen)1);

/*     Initialize the CELL oriented set for collecting open DAF or DAS */
/*     files in the event of an error. */

    ssizei_(&c__1, opnset);

/*     While there is still more to do ... */

    done = FALSE_;
    while(! done) {

/*        We initialize a few things here, so that they get reset for */
/*        every trip through the loop. */

/*        Initialize the logical flags that we use. */

	comnts = FALSE_;
	contnu = TRUE_;
	eoc = FALSE_;
	ndfnms = FALSE_;

/*        Initialize the filename table, ... */

	s_copy(fnmtbl, " ", (ftnlen)128, (ftnlen)1);
	s_copy(fnmtbl + 128, " ", (ftnlen)128, (ftnlen)1);

/*        the file status table, ... */

	s_copy(statbl, " ", (ftnlen)3, (ftnlen)1);
	s_copy(statbl + 3, " ", (ftnlen)3, (ftnlen)1);

/*        the table indices, ... */

	tblidx[0] = 0;
	tblidx[1] = 0;

/*        set the number of file names to zero, ... */

	numfnm = 0;

/*        the prompts in the prompt table, ... */

	s_copy(prmpts, " ", (ftnlen)80, (ftnlen)1);
	s_copy(prmpts + 80, " ", (ftnlen)80, (ftnlen)1);

/*        the message, and the option. */

	s_copy(messag, " ", (ftnlen)1000, (ftnlen)1);
	s_copy(option, " ", (ftnlen)20, (ftnlen)1);

/*        Set the status messages. */

	s_copy(status, messgs + 5000, (ftnlen)1000, (ftnlen)1000);
	s_copy(status + 1000, messgs + 6000, (ftnlen)1000, (ftnlen)1000);

/*        Get the option to be performed from the main menu. */

	getopt_(maintl, &c__5, mainnm, maintx, &iopt, (ftnlen)20, (ftnlen)1, (
		ftnlen)40);
	s_copy(option, mainvl + ((i__2 = iopt - 1) < 5 && 0 <= i__2 ? i__2 : 
		s_rnge("mainvl", i__2, "commnt_", (ftnlen)502)) * 20, (ftnlen)
		20, (ftnlen)20);

/*        Set up the messages and other information for the option */
/*        selected. */

	if (contnu) {
	    if (s_cmp(option, "ADD_COMMENTS", (ftnlen)20, (ftnlen)12) == 0) {
		ndfnms = TRUE_;
		numfnm = 2;
		s_copy(messag, messgs + 1000, (ftnlen)1000, (ftnlen)1000);
		tblidx[0] = 2;
		s_copy(prmpts, prmtbl + 80, (ftnlen)80, (ftnlen)80);
		repmc_(prmpts, "#", "added", prmpts, (ftnlen)80, (ftnlen)1, (
			ftnlen)5, (ftnlen)80);
		s_copy(statbl + 3, "OLD", (ftnlen)3, (ftnlen)3);
		tblidx[1] = 1;
		s_copy(prmpts + 80, prmtbl, (ftnlen)80, (ftnlen)80);
		s_copy(statbl, "OLD", (ftnlen)3, (ftnlen)3);

/*              Set the operation status messages. */

		s_copy(status, messgs + 5000, (ftnlen)1000, (ftnlen)1000);
		repmc_(status, "#", "added", status, (ftnlen)1000, (ftnlen)1, 
			(ftnlen)5, (ftnlen)1000);
		s_copy(status + 1000, messgs + 6000, (ftnlen)1000, (ftnlen)
			1000);
		repmc_(status + 1000, "#", "added", status + 1000, (ftnlen)
			1000, (ftnlen)1, (ftnlen)5, (ftnlen)1000);
	    } else if (s_cmp(option, "READ_COMMENTS", (ftnlen)20, (ftnlen)13) 
		    == 0) {
		ndfnms = TRUE_;
		numfnm = 1;
		s_copy(messag, messgs, (ftnlen)1000, (ftnlen)1000);
		tblidx[0] = 1;
		s_copy(prmpts, prmtbl, (ftnlen)80, (ftnlen)80);
		s_copy(statbl, "OLD", (ftnlen)3, (ftnlen)3);

/*              Set the operation status messages. */

		s_copy(status, messgs + 5000, (ftnlen)1000, (ftnlen)1000);
		repmc_(status, "#", "read", status, (ftnlen)1000, (ftnlen)1, (
			ftnlen)4, (ftnlen)1000);
		s_copy(status + 1000, messgs + 6000, (ftnlen)1000, (ftnlen)
			1000);
		repmc_(status + 1000, "#", "read", status + 1000, (ftnlen)
			1000, (ftnlen)1, (ftnlen)4, (ftnlen)1000);
	    } else if (s_cmp(option, "EXTRACT_COMMENTS", (ftnlen)20, (ftnlen)
		    16) == 0) {
		ndfnms = TRUE_;
		numfnm = 2;
		s_copy(messag, messgs + 2000, (ftnlen)1000, (ftnlen)1000);
		tblidx[0] = 1;
		s_copy(prmpts, prmtbl, (ftnlen)80, (ftnlen)80);
		s_copy(statbl, "OLD", (ftnlen)3, (ftnlen)3);
		tblidx[1] = 2;
		s_copy(prmpts + 80, prmtbl + 80, (ftnlen)80, (ftnlen)80);
		repmc_(prmpts + 80, "#", "created", prmpts + 80, (ftnlen)80, (
			ftnlen)1, (ftnlen)7, (ftnlen)80);
		s_copy(statbl + 3, "NEW", (ftnlen)3, (ftnlen)3);

/*              Set the operation status messages. */

		s_copy(status, messgs + 5000, (ftnlen)1000, (ftnlen)1000);
		repmc_(status, "#", "extracted", status, (ftnlen)1000, (
			ftnlen)1, (ftnlen)9, (ftnlen)1000);
		s_copy(status + 1000, messgs + 6000, (ftnlen)1000, (ftnlen)
			1000);
		repmc_(status + 1000, "#", "extracted", status + 1000, (
			ftnlen)1000, (ftnlen)1, (ftnlen)9, (ftnlen)1000);
	    } else if (s_cmp(option, "DELETE_COMMENTS", (ftnlen)20, (ftnlen)
		    15) == 0) {
		ndfnms = TRUE_;
		numfnm = 1;
		s_copy(messag, messgs + 3000, (ftnlen)1000, (ftnlen)1000);
		tblidx[0] = 1;
		s_copy(prmpts, prmtbl, (ftnlen)80, (ftnlen)80);
		s_copy(statbl, "OLD", (ftnlen)3, (ftnlen)3);

/*              Set the operation status messages. */

		s_copy(status, messgs + 5000, (ftnlen)1000, (ftnlen)1000);
		repmc_(status, "#", "deleted", status, (ftnlen)1000, (ftnlen)
			1, (ftnlen)7, (ftnlen)1000);
		s_copy(status + 1000, messgs + 6000, (ftnlen)1000, (ftnlen)
			1000);
		repmc_(status + 1000, "#", "deleted", status + 1000, (ftnlen)
			1000, (ftnlen)1, (ftnlen)7, (ftnlen)1000);
	    } else if (s_cmp(option, "QUIT", (ftnlen)20, (ftnlen)4) == 0) {
		s_copy(messag, messgs + 4000, (ftnlen)1000, (ftnlen)1000);
	    }
	}

/*        Collect any filenames that we may need. */

	if (contnu && ndfnms) {

/*           we always need at least one filename if we get to here. */

	    i__ = 1;
	    more = TRUE_;
	    while(more) {
		fileok = FALSE_;
		tryagn = TRUE_;
		while(tryagn) {
		    tostdo_(" ", (ftnlen)1);
		    tostdo_(prmpts + ((i__2 = i__ - 1) < 2 && 0 <= i__2 ? 
			    i__2 : s_rnge("prmpts", i__2, "commnt_", (ftnlen)
			    614)) * 80, (ftnlen)80);
		    tostdo_(" ", (ftnlen)1);
		    getfnm_("Filename? ", statbl + ((i__3 = tblidx[(i__2 = 
			    i__ - 1) < 2 && 0 <= i__2 ? i__2 : s_rnge("tblidx"
			    , i__2, "commnt_", (ftnlen)617)] - 1) < 2 && 0 <= 
			    i__3 ? i__3 : s_rnge("statbl", i__3, "commnt_", (
			    ftnlen)617)) * 3, fnmtbl + (((i__5 = tblidx[(i__4 
			    = i__ - 1) < 2 && 0 <= i__4 ? i__4 : s_rnge("tbl"
			    "idx", i__4, "commnt_", (ftnlen)617)] - 1) < 2 && 
			    0 <= i__5 ? i__5 : s_rnge("fnmtbl", i__5, "commn"
			    "t_", (ftnlen)617)) << 7), &fileok, errmsg, (
			    ftnlen)10, (ftnlen)3, (ftnlen)128, (ftnlen)320);

/*                 If the filename is OK, increment the filename index */
/*                 and leave the try again loop. Otherwise, write out the */
/*                 error message, and give the opportunity to go around */
/*                 again. */

		    if (fileok) {
			++i__;
			tryagn = FALSE_;
		    } else {
			tostdo_(" ", (ftnlen)1);
			tostdo_(errmsg, (ftnlen)320);
			tostdo_(" ", (ftnlen)1);
			cnfirm_("Try again? (Yes/No) ", &tryagn, (ftnlen)20);
			if (! tryagn) {
			    contnu = FALSE_;
			    more = FALSE_;
			}
		    }
		}
		if (i__ > numfnm) {
		    more = FALSE_;
		}
	    }
	}

/*        Get the file architecture and type. */

	if (contnu && ndfnms) {
	    getfat_(fnmtbl, arch, type__, (ftnlen)128, (ftnlen)3, (ftnlen)4);
	    if (failed_()) {
		contnu = FALSE_;
	    }
	}

/*        Check to see that we got back a valid architecture and type. */

	if (contnu && ndfnms) {
	    if (s_cmp(arch, "?", (ftnlen)3, (ftnlen)1) == 0 || s_cmp(type__, 
		    "?", (ftnlen)4, (ftnlen)1) == 0) {
		contnu = FALSE_;
		setmsg_("The architecture and type of the binary file '#' co"
			"uld not be determined. A common error is to give the"
			" name of a text file instead of the name of a binary"
			" file.", (ftnlen)161);
		errch_("#", fnmtbl, (ftnlen)1, (ftnlen)128);
		sigerr_("SPICE(BADFILEFORMAT)", (ftnlen)20);
	    }
	}

/*        Customize the message. We know we can do this, because we */
/*        need files, and so we don't have the QUIT message. */

	if (contnu && ndfnms) {
	    repmc_(messag, "#", type__, messag, (ftnlen)1000, (ftnlen)1, (
		    ftnlen)4, (ftnlen)1000);
	}

/*        Process the option that was selected so long ago. */

	if (contnu) {
	    if (s_cmp(option, "QUIT", (ftnlen)20, (ftnlen)4) == 0) {
		tostdo_(" ", (ftnlen)1);
		tostdo_(messag, (ftnlen)1000);
		tostdo_(" ", (ftnlen)1);
		done = TRUE_;
	    } else if (s_cmp(option, "ADD_COMMENTS", (ftnlen)20, (ftnlen)12) 
		    == 0) {
		tostdo_(" ", (ftnlen)1);
		tostdo_(messag, (ftnlen)1000);
		s_copy(line, "From File: #", (ftnlen)1000, (ftnlen)12);
		repmc_(line, "#", fnmtbl + 128, line, (ftnlen)1000, (ftnlen)1,
			 (ftnlen)128, (ftnlen)1000);
		tostdo_(" ", (ftnlen)1);
		tostdo_(line, (ftnlen)1000);
		s_copy(line, "To File  : #", (ftnlen)1000, (ftnlen)12);
		repmc_(line, "#", fnmtbl, line, (ftnlen)1000, (ftnlen)1, (
			ftnlen)128, (ftnlen)1000);
		tostdo_(line, (ftnlen)1000);

/*              Open the text file which contains the comments to be */
/*              added to the binary file. */

		txtopr_(fnmtbl + 128, &comlun, (ftnlen)128);
		if (! failed_()) {
		    if (s_cmp(type__, "CK", (ftnlen)4, (ftnlen)2) == 0) {

/*                    Open the binary file, add the comments, and close */
/*                    the binary file. */

			dafopw_(fnmtbl, &handle, (ftnlen)128);
			spcac_(&handle, &comlun, " ", " ", (ftnlen)1, (ftnlen)
				1);
			dafcls_(&handle);
		    } else if (s_cmp(type__, "PCK", (ftnlen)4, (ftnlen)3) == 
			    0) {

/*                    Open the binary file, add the comments, and close */
/*                    the binary file. */

			dafopw_(fnmtbl, &handle, (ftnlen)128);
			spcac_(&handle, &comlun, " ", " ", (ftnlen)1, (ftnlen)
				1);
			dafcls_(&handle);
		    } else if (s_cmp(type__, "SPK", (ftnlen)4, (ftnlen)3) == 
			    0) {

/*                    Open the binary file, add the comments, and close */
/*                    the binary file. */

			dafopw_(fnmtbl, &handle, (ftnlen)128);
			spcac_(&handle, &comlun, " ", " ", (ftnlen)1, (ftnlen)
				1);
			dafcls_(&handle);
		    } else if (s_cmp(type__, "EK", (ftnlen)4, (ftnlen)2) == 0)
			     {

/*                    Open the binary file, add the comments, and close */
/*                    the binary file. */

			dasopw_(fnmtbl, &handle, (ftnlen)128);
			dasacu_(&comlun, " ", " ", &insbln, &handle, (ftnlen)
				1, (ftnlen)1);
			dascls_(&handle);
		    }

/*                 Close the comment file. */

		    cl__1.cerr = 0;
		    cl__1.cunit = comlun;
		    cl__1.csta = 0;
		    f_clos(&cl__1);
		}

/*              Display the status of the operation that was selected. */

		tostdo_(" ", (ftnlen)1);
		if (failed_()) {
		    tostdo_(status + 1000, (ftnlen)1000);
		} else {
		    tostdo_(status, (ftnlen)1000);
		}
	    } else if (s_cmp(option, "READ_COMMENTS", (ftnlen)20, (ftnlen)13) 
		    == 0) {
		tostdo_(" ", (ftnlen)1);
		tostdo_(messag, (ftnlen)1000);
		s_copy(line, "File: #", (ftnlen)1000, (ftnlen)7);
		repmc_(line, "#", fnmtbl, line, (ftnlen)1000, (ftnlen)1, (
			ftnlen)128, (ftnlen)1000);
		tostdo_(" ", (ftnlen)1);
		tostdo_(line, (ftnlen)1000);
		tostdo_(" ", (ftnlen)1);
		if (s_cmp(type__, "CK", (ftnlen)4, (ftnlen)2) == 0) {

/*                 Open the binary file, read the comments, and close */
/*                 the binary file. */

		    dafopr_(fnmtbl, &handle, (ftnlen)128);

/*                 The comments are read a line at a time and displayed */
/*                 on the screen. */

		    spcrfl_(&handle, line, &eoc, (ftnlen)1000);
		    if (! failed_()) {
			if (eoc) {
			    tostdo_("There were no comments found in the fil"
				    "e.", (ftnlen)41);
			}
			while(! eoc && ! failed_()) {
			    tostdo_(line, (ftnlen)1000);
			    spcrnl_(line, &eoc, (ftnlen)1000);
			}
		    }
		    dafcls_(&handle);
		} else if (s_cmp(type__, "PCK", (ftnlen)4, (ftnlen)3) == 0) {

/*                 Open the binary file, read the comments, and close */
/*                 the binary file. */

		    dafopr_(fnmtbl, &handle, (ftnlen)128);

/*                 The comments are read a line at a time and displayed */
/*                 on the screen. */

		    spcrfl_(&handle, line, &eoc, (ftnlen)1000);
		    if (! failed_()) {
			if (eoc) {
			    tostdo_("There were no commentfound in the file.",
				     (ftnlen)39);
			}
			while(! eoc && ! failed_()) {
			    tostdo_(line, (ftnlen)1000);
			    spcrnl_(line, &eoc, (ftnlen)1000);
			}
		    }
		    dafcls_(&handle);
		} else if (s_cmp(type__, "SPK", (ftnlen)4, (ftnlen)3) == 0) {

/*                 Open the binary file, read the comments, and close */
/*                 the binary file. */

		    dafopr_(fnmtbl, &handle, (ftnlen)128);

/*                 The comments are read a line at a time and displayed */
/*                 on the screen. */

		    spcrfl_(&handle, line, &eoc, (ftnlen)1000);
		    if (! failed_()) {
			if (eoc) {
			    tostdo_("There were no comments found in the fil"
				    "e.", (ftnlen)41);
			}
			while(! eoc && ! failed_()) {
			    tostdo_(line, (ftnlen)1000);
			    spcrnl_(line, &eoc, (ftnlen)1000);
			}
		    }
		    dafcls_(&handle);
		} else if (s_cmp(type__, "EK", (ftnlen)4, (ftnlen)2) == 0) {

/*                 Open the binary file, read the comments, and close */
/*                 the binary file. */

		    dasopr_(fnmtbl, &handle, (ftnlen)128);
		    dasecu_(&handle, &c__6, &comnts);
		    dascls_(&handle);
		    if (! comnts) {
			s_copy(line, "There were no comments found in the fi"
				"le.", (ftnlen)1000, (ftnlen)41);
			tostdo_(line, (ftnlen)1000);
		    }
		}

/*              Display the status of the operation that was selected. */

		tostdo_(" ", (ftnlen)1);
		if (failed_()) {
		    tostdo_(status + 1000, (ftnlen)1000);
		} else {
		    tostdo_(status, (ftnlen)1000);
		}
	    } else if (s_cmp(option, "EXTRACT_COMMENTS", (ftnlen)20, (ftnlen)
		    16) == 0) {
		tostdo_(" ", (ftnlen)1);
		tostdo_(messag, (ftnlen)1000);
		s_copy(line, "From File: #", (ftnlen)1000, (ftnlen)12);
		repmc_(line, "#", fnmtbl, line, (ftnlen)1000, (ftnlen)1, (
			ftnlen)128, (ftnlen)1000);
		tostdo_(" ", (ftnlen)1);
		tostdo_(line, (ftnlen)1000);
		s_copy(line, "To File  : #", (ftnlen)1000, (ftnlen)12);
		repmc_(line, "#", fnmtbl + 128, line, (ftnlen)1000, (ftnlen)1,
			 (ftnlen)128, (ftnlen)1000);
		tostdo_(line, (ftnlen)1000);

/*              Open the text file. */

		txtopn_(fnmtbl + 128, &comlun, (ftnlen)128);
		if (! failed_()) {
		    if (s_cmp(type__, "CK", (ftnlen)4, (ftnlen)2) == 0) {

/*                    Open the binary file, extract the comments, and */
/*                    close the binary file. */

			dafopr_(fnmtbl, &handle, (ftnlen)128);
			spcec_(&handle, &comlun);
			dafcls_(&handle);
		    } else if (s_cmp(type__, "PCK", (ftnlen)4, (ftnlen)3) == 
			    0) {

/*                    Open the binary file, extract the comments, and */
/*                    close the binary file. */

			dafopr_(fnmtbl, &handle, (ftnlen)128);
			spcec_(&handle, &comlun);
			dafcls_(&handle);
		    } else if (s_cmp(type__, "SPK", (ftnlen)4, (ftnlen)3) == 
			    0) {

/*                    Open the binary file, extract the comments, and */
/*                    close the binary file. */

			dafopr_(fnmtbl, &handle, (ftnlen)128);
			spcec_(&handle, &comlun);
			dafcls_(&handle);
		    } else if (s_cmp(type__, "EK", (ftnlen)4, (ftnlen)2) == 0)
			     {

/*                    Open the binary file, extract the comments, and */
/*                    close the binary file. */

			dasopr_(fnmtbl, &handle, (ftnlen)128);
			dasecu_(&handle, &comlun, &comnts);
			dascls_(&handle);
			if (! comnts) {
			    s_copy(line, "There were no comments found in th"
				    "e file.", (ftnlen)1000, (ftnlen)41);
			    tostdo_(line, (ftnlen)1000);
			}
		    }

/*                 Close the text file that we opened. */

		    cl__1.cerr = 0;
		    cl__1.cunit = comlun;
		    cl__1.csta = 0;
		    f_clos(&cl__1);
		}

/*              Display the status of the operation that was selected. */

		tostdo_(" ", (ftnlen)1);
		if (failed_()) {
		    tostdo_(status + 1000, (ftnlen)1000);
		} else {
		    tostdo_(status, (ftnlen)1000);
		}
	    } else if (s_cmp(option, "DELETE_COMMENTS", (ftnlen)20, (ftnlen)
		    15) == 0) {
		tostdo_(" ", (ftnlen)1);
		tostdo_(messag, (ftnlen)1000);
		s_copy(line, "File: #", (ftnlen)1000, (ftnlen)7);
		repmc_(line, "#", fnmtbl, line, (ftnlen)1000, (ftnlen)1, (
			ftnlen)128, (ftnlen)1000);
		tostdo_(" ", (ftnlen)1);
		tostdo_(line, (ftnlen)1000);
		if (s_cmp(type__, "CK", (ftnlen)4, (ftnlen)2) == 0) {

/*                 Open the binary file, delete the comments, and close */
/*                 the binary file. */

		    dafopw_(fnmtbl, &handle, (ftnlen)128);
		    spcdc_(&handle);
		    dafcls_(&handle);
		} else if (s_cmp(type__, "PCK", (ftnlen)4, (ftnlen)3) == 0) {

/*                 Open the binary file, delete the comments, and close */
/*                 the binary file. */

		    dafopw_(fnmtbl, &handle, (ftnlen)128);
		    spcdc_(&handle);
		    dafcls_(&handle);
		} else if (s_cmp(type__, "SPK", (ftnlen)4, (ftnlen)3) == 0) {

/*                 Open the binary file, delete the comments, and close */
/*                 the binary file. */

		    dafopw_(fnmtbl, &handle, (ftnlen)128);
		    spcdc_(&handle);
		    dafcls_(&handle);
		} else if (s_cmp(type__, "EK", (ftnlen)4, (ftnlen)2) == 0) {

/*                 Open the binary file, delete the comments, and close */
/*                 the binary file. */

		    dasopw_(fnmtbl, &handle, (ftnlen)128);
		    dasdc_(&handle);
		    dascls_(&handle);
		}

/*              Display the status of the operation that was selected. */

		tostdo_(" ", (ftnlen)1);
		if (failed_()) {
		    tostdo_(status + 1000, (ftnlen)1000);
		} else {
		    tostdo_(status, (ftnlen)1000);
		}
	    }
	}

/*        If anything failed, close any binary files that might still be */
/*        open and reset the error handling before getting the next */
/*        option. */

	if (failed_()) {

/*           Before we can attempt to perform any clean up actions if an */
/*           error occurred, we need to reset the SPICELIB error handling */
/*           mechanism so that we can call the SPICELIB routines that we */
/*           need to. */

	    reset_();

/*           Clear out any binary file handles in the open set, OPNSET. */

	    scardi_(&c__0, opnset);
	    cleari_(&c__1, &opnset[6]);

/*           Get the handles for any DAF files which may still be open. */

	    dafhof_(opnset);
	    numopn = cardi_(opnset);
	    if (numopn > 0) {
		i__2 = numopn;
		for (i__ = 1; i__ <= i__2; ++i__) {
		    dafcls_(&opnset[(i__3 = i__ + 5) < 7 && 0 <= i__3 ? i__3 :
			     s_rnge("opnset", i__3, "commnt_", (ftnlen)1100)])
			    ;
		}
	    }

/*           Clear out any binary file handles in the open set, OPNSET. */

	    scardi_(&c__0, opnset);
	    cleari_(&c__1, &opnset[6]);

/*           Get the handles for any DAS files which may still be open. */

	    dashof_(opnset);
	    numopn = cardi_(opnset);
	    if (numopn > 0) {
		i__2 = numopn;
		for (i__ = 1; i__ <= i__2; ++i__) {
		    dascls_(&opnset[(i__3 = i__ + 5) < 7 && 0 <= i__3 ? i__3 :
			     s_rnge("opnset", i__3, "commnt_", (ftnlen)1121)])
			    ;
		}
	    }

/*           If there was an error and we were extracting comments to a */
/*           file, then we should delete the file that was created, */
/*           because we do not know whether the extraction was completed */
/*           successfully. */

	    if (s_cmp(option, "EXTRACT_COMMENTS", (ftnlen)20, (ftnlen)16) == 
		    0) {
		if (exists_(fnmtbl + 128, (ftnlen)128)) {
		    delfil_(fnmtbl + 128, (ftnlen)128);
		}
	    }

/*           Finally, reset the error handling, and go get the next */
/*           option. This is just to be sure. */

	    reset_();
	}
    }
    chkout_("COMMNT", (ftnlen)6);
    return 0;
} /* MAIN__ */

/* Main program alias */ int commnt_ () { MAIN__ (); return 0; }
