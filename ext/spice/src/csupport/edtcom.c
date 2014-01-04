/* edtcom.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__80 = 80;
static integer c__2 = 2;
static integer c__0 = 0;
static integer c__1 = 1;

/* $Procedure      EDTCOM (Edit a command) */
/* Subroutine */ int edtcom_0_(int n__, char *delim, char *prompt, char *
	commnd, integer *source, ftnlen delim_len, ftnlen prompt_len, ftnlen 
	commnd_len)
{
    /* Initialized data */

    static char editor[132] = "emacs                                        "
	    "                                                                "
	    "                       ";
    static logical first = TRUE_;

    /* System generated locals */
    address a__1[2];
    integer i__1[2], i__2, i__3;
    cllist cl__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer f_clos(cllist *), i_indx(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static char name__[132];
    extern logical have_(char *, ftnlen);
    static char line[132], rest[1760];
    static integer unit, i__;
    extern integer cardc_(char *, ftnlen);
    static integer r__;
    static char space[1];
    extern logical match_(char *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static integer depth;
    extern /* Subroutine */ int repmc_(char *, char *, char *, char *, ftnlen,
	     ftnlen, ftnlen, ftnlen), reset_(void);
    static integer b1, b2, e1, e2;
    static char error[132*2];
    static logical lstat[3];
    extern integer rtrim_(char *, ftnlen);
    static logical sstat[3];
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    extern logical m2wmch_(char *, integer *, integer *, char *, ftnlen, 
	    ftnlen), failed_(void);
    extern /* Subroutine */ int edtcmd_(char *, char *, ftnlen, ftnlen);
    static char buffer[132*86];
    extern /* Subroutine */ int dmpbuf_(void), getbuf_(integer *, char *, 
	    ftnlen);
    static char scndwd[32];
    extern /* Subroutine */ int prread_(char *, char *, ftnlen, ftnlen), 
	    newfil_(char *, char *, char *, ftnlen, ftnlen, ftnlen), nspioa_(
	    char *, ftnlen);
    static logical gotone;
    extern /* Subroutine */ int nparsi_(char *, integer *, char *, integer *, 
	    ftnlen, ftnlen), sigerr_(char *, ftnlen);
    static integer comnum;
    extern /* Subroutine */ int getbsz_(integer *);
    static char dstrng[3];
    extern /* Subroutine */ int nspioh_(char *, ftnlen);
    static integer iostat;
    extern /* Subroutine */ int ssizec_(integer *, char *, ftnlen), suffix_(
	    char *, integer *, char *, ftnlen, ftnlen);
    static char pattrn[132], frstwd[32];
    extern /* Subroutine */ int nextwd_(char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen), setmsg_(char *, ftnlen), chkout_(char *, ftnlen),
	     nspioc_(char *, ftnlen), nspgst_(char *, logical *, ftnlen), 
	    prexit_(void), putcom_(char *, integer *, ftnlen);
    static char errstr[132];
    static logical status[3], svstat[3];
    extern /* Subroutine */ int intstr_(integer *, char *, ftnlen), nspwln_(
	    char *, ftnlen), nsppst_(char *, logical *, ftnlen), rstbuf_(void)
	    , putbuf_(char *, ftnlen), prstrt_(char *, char *, ftnlen, ftnlen)
	    , txtopr_(char *, integer *, ftnlen);
    static char tab[1];
    static integer ptr;

/* $ Abstract */

/*     This entry point allows the user of a program to fetch */
/*     previously entered commands, review them, re-execute the commands */
/*     or edit and re-execute the command. */

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

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     DELIM      I   is the character used to delimit the command ends */
/*     COMMND    I/O  is a command to process */
/*     SOURCE    I/O  indicates the source of the command */

/* $ Detailed_Input */

/*     DELIM        is the character used to delimit input commands. */
/*                  A command begins at the first non-blank character */
/*                  of COMMND and continues until the last non-blank */
/*                  character or the first occurrence of DELIM which */
/*                  ever is first. */


/*     COMMND       is a string that indicates some action the program */
/*                  should take.  The only commands that have meaning */
/*                  to this routine are those of the form: */

/*                    RECALL @int(1:20) */

/*                    RECALL ALL */

/*                    DO     @int(1:20) */

/*                    EDIT   @int(1:20) */

/*                  all other commands are ignored by this routine. */
/*                  (See the META/2 language specification language */
/*                  for a more detailed description of the meaning */
/*                  of the syntax specifications given above.) */

/*     SOURCE       is an integer indicating where the input command */
/*                  came from.  Unless SOURCE has a value of 2 meaning */
/*                  the command was typed interactively, no action */
/*                  is taken by this routine. */

/* $ Detailed_Output */

/*     COMMND       if the input command is recognized by this routine */
/*                  COMMND will be set to all blank characters. */
/*                  Otherwise, COMMND will remain unchanged. */

/*     SOURCE       if the input command is recognized by this routine */
/*                  SOURCE will be set to zero indicating that there */
/*                  is no longer a potential command in the string */
/*                  COMMND.  SOURCE will remain unchanged. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     In the case of a command of the form EDIT @int(1:20) this routine */
/*     must be able to create a file that the editor can edit. */

/*     If this cannot be done one of the following errors will be */
/*     signalled. */

/*     1)   If the program cannot create a new file name that */
/*          could hold the command to be edited, the error */
/*          COMLOOP(NOFILECREATION) will be signalled. */

/*     2)   If a new file name could be created but the file could */
/*          not be opened, the error COMLOOP(COMMANDEDITFAILED) */
/*          will be signalled. */


/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is provided so that command line programs */
/*     may obtain a history of commands that have been entered */
/*     into the program and possible re-execute or edit and execute */
/*     the previous commands.  This is meant to be integrated */
/*     with the basic command loop software available for */
/*     constructing command driven programs.  See the routine */
/*     CMLOOP to see how this fits into the general sequence of */
/*     command processing. */

/* $ Examples */

/*     See CMLOOP for the intended use of this routine. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     H.A. Neilan     (JPL) */
/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.24.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL. */

/* -    SPICELIB Version 1.23.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-CC_C. */

/* -    SPICELIB Version 1.22.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-INTEL-64BIT-CC_C. */

/* -    SPICELIB Version 1.21.0, 13-MAY-2010 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-NATIVE_C. */

/* -    SPICELIB Version 1.20.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-WINDOWS-64BIT-IFORT. */

/* -    SPICELIB Version 1.19.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-LINUX-64BIT-GFORTRAN. */

/* -    SPICELIB Version 1.18.0, 13-MAY-2010 (BVS) */

/*        Updated for PC-64BIT-MS_C. */

/* -    SPICELIB Version 1.17.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-INTEL_C. */

/* -    SPICELIB Version 1.16.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-IFORT. */

/* -    SPICELIB Version 1.15.0, 13-MAY-2010 (BVS) */

/*        Updated for MAC-OSX-64BIT-GFORTRAN. */

/* -    SPICELIB Version 1.14.0, 18-MAR-2009 (BVS) */

/*        Updated for PC-LINUX-GFORTRAN. */

/* -    SPICELIB Version 1.13.0, 18-MAR-2009 (BVS) */

/*        Updated for MAC-OSX-GFORTRAN. */

/* -    SPICELIB Version 1.12.0, 19-FEB-2008 (BVS) */

/*        Updated for PC-LINUX-IFORT. */

/* -    SPICELIB Version 1.11.0, 14-NOV-2006 (BVS) */

/*        Updated for PC-LINUX-64BIT-GCC_C. */

/* -    SPICELIB Version 1.10.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-INTEL_C. */

/* -    SPICELIB Version 1.9.0, 14-NOV-2006 (BVS) */

/*        Updated for MAC-OSX-IFORT. */

/* -    SPICELIB Version 1.8.0, 14-NOV-2006 (BVS) */

/*        Updated for PC-WINDOWS-IFORT. */

/* -    SPICELIB Version 1.7.0, 26-OCT-2005 (BVS) */

/*        Updated for SUN-SOLARIS-64BIT-GCC_C. */

/* -    SPICELIB Version 1.6.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN_C. */

/* -    SPICELIB Version 1.5.0, 03-JAN-2005 (BVS) */

/*        Updated for PC-CYGWIN. */

/* -    SPICELIB Version 1.4.5, 17-JUL-2002 (BVS) */

/*        Added MAC-OSX environments. */

/* -    SPICELIB Version 1.4.4, 08-OCT-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitely given.  New */
/*        environments are WIN-NT */

/* -    SPICELIB Version 1.4.3, 20-SEP-1999 (NJB) */

/*        CSPICE and PC-LINUX environment lines were added.  Some */
/*        typos were corrected. */

/* -    SPICELIB Version 1.4.2, 28-JUL-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  New */
/*        environments are PC-DIGITAL, SGI-O32 and SGI-N32. */

/* -    SPICELIB Version 1.4.1, 18-MAR-1999 (WLT) */

/*        The environment lines were expanded so that the supported */
/*        environments are now explicitly given.  Previously, */
/*        environments such as SUN-SUNOS and SUN-SOLARIS were implied */
/*        by the environment label SUN. */

/* -    SPICELIB Version 1.4.0, 9-JAN-1997 (WLT) */

/*        Added minimal support for the MAC version. When the user */
/*        requests EDIT, the routine displays the matching item */
/*        that should be editted.  This is the only option available */
/*        at the moment on the MAC.  When something better comes along */
/*        we'll do something more creative. */

/* -    SPICELIB Version 1.3.0, 5-Dec-1995 (WLT) */

/*        Fixed the bug that occured if you typed RECALL x and */
/*        there was no matching command (probably should have done */
/*        thins in version 1.2.0) EDTCOM now pronounces that this */
/*        is an error. */

/* -    SPICELIB Version 1.2.0, 11-SEP-1995 ( WLT ) */

/*        Fixed the bug that occurred if you type EDIT x or */
/*        DO x and there was no matching command in the history */
/*        list.  EDTCOM no pronounces that this is an error. */

/* -    SPICELIB Version 1.1.0, 1-JUN-1995 (HAN) */

/*        Created the master source file for VAX/VMS, Alpha/OpenVMS, */
/*        Sun (Sun OS 4.1.x and Solaris), PC(Microsoft Fortran), HP, */
/*        and NeXT. */

/* -    Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*        This is the configured version of the Command Loop */
/*        software as of May 4, 1994 */

/* -    SPICELIB Version 1.0.0, 18-APR-1994 (WLT) */


/* -& */
/* $ Index_Entries */

/*     Recall Re-execute or edit and re-execute a command */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.1.0, 1-JUN-1995 (HAN) */

/*        Created the master source file for VAX/VMS, Alpha/OpenVMS, */
/*        Sun (Sun OS 4.1.x and Solaris), PC(Microsoft Fortran), HP, */
/*        and NeXT. */

/* -& */

/*     Spicelib Functions */


/*     Meta/2 functions */


/*     Below are the various sources from which */
/*     commands might come. */

/*     NONE */
/*     COMBUF */
/*     KEYBRD */
/*     INPFIL */


/*     Local parameters used for allocating space and controlling loop */
/*     execution. */


/*     Local Variables. */

    switch(n__) {
	case 1: goto L_setedt;
	case 2: goto L_getedt;
	}


/*     The only time an EDIT/RECALL/DO command can have any meaning */
/*     is when it comes from the keyboard. */

    if (*source != 2) {
	return 0;
    }

/*     Initialize the syntax for the preprocessing commands */

    if (first) {
	first = FALSE_;
	*(unsigned char *)tab = '\t';
	*(unsigned char *)space = ' ';
    }

/*     Next we take apart the command and see if it is one of the */
/*     preprocessing commands. */

    nextwd_(commnd, frstwd, rest, commnd_len, (ftnlen)32, (ftnlen)1760);
    nextwd_(rest, scndwd, rest, (ftnlen)1760, (ftnlen)32, (ftnlen)1760);

/*     We probably don't have any of the pathologies below, but they */
/*     are easy to check so we handle them here. */

    if (s_cmp(rest, " ", (ftnlen)1760, (ftnlen)1) != 0) {
	return 0;
    }
    if (s_cmp(frstwd, " ", (ftnlen)32, (ftnlen)1) == 0) {
	return 0;
    }
    b1 = 1;
    b2 = 1;
    e1 = rtrim_(frstwd, (ftnlen)32);
    e2 = rtrim_(scndwd, (ftnlen)32);
    if (s_cmp(scndwd, " ", (ftnlen)32, (ftnlen)1) == 0 && ! m2wmch_(frstwd, &
	    b1, &e1, "RECALL", (ftnlen)32, (ftnlen)6)) {
	return 0;
    }

/*     We need the beginning and endings of the words we've extracted. */

    b1 = 1;
    b2 = 1;
    e1 = rtrim_(frstwd, (ftnlen)32);
    e2 = rtrim_(scndwd, (ftnlen)32);
    if (m2wmch_(frstwd, &b1, &e1, "RECALL", (ftnlen)32, (ftnlen)6) && s_cmp(
	    scndwd, " ", (ftnlen)32, (ftnlen)1) == 0) {

/*        We don't want the RECALL command to show up in the */
/*        output. */

	dmpbuf_();

/*        We don't write the output of a RECALL command to the */
/*        log file. */

	nspgst_("LOG", status, (ftnlen)3);
	nspioh_("LOG", (ftnlen)3);

/*        Determine the depth of the command line buffer. */

	getbsz_(&depth);

/*        Fetch each paragraph and display it. */

	while(depth > 0) {
	    ssizec_(&c__80, buffer, (ftnlen)132);
	    intstr_(&depth, dstrng, (ftnlen)3);
	    ljust_(dstrng, dstrng, (ftnlen)3, (ftnlen)3);
	    getbuf_(&depth, buffer, (ftnlen)132);
/* Writing concatenation */
	    i__1[0] = 3, a__1[0] = dstrng;
	    i__1[1] = 132, a__1[1] = buffer + 792;
	    s_cat(line, a__1, i__1, &c__2, (ftnlen)132);
	    nspwln_(line, (ftnlen)132);
	    i__2 = cardc_(buffer, (ftnlen)132);
	    for (i__ = 2; i__ <= i__2; ++i__) {
/* Writing concatenation */
		i__1[0] = 3, a__1[0] = "   ";
		i__1[1] = 132, a__1[1] = buffer + ((i__3 = i__ + 5) < 86 && 0 
			<= i__3 ? i__3 : s_rnge("buffer", i__3, "edtcom_", (
			ftnlen)508)) * 132;
		s_cat(line, a__1, i__1, &c__2, (ftnlen)132);
		nspwln_(line, (ftnlen)132);
	    }
	    --depth;
	}

/*        Reset the status of the LOG file back to whatever it */
/*        was before we started dumping old commands.  Finally */
/*        set the command to a blank. */

	nsppst_("LOG", status, (ftnlen)3);
	s_copy(commnd, " ", commnd_len, (ftnlen)1);
	*source = 0;
	return 0;
    } else if (m2wmch_(frstwd, &b1, &e1, "RECALL", (ftnlen)32, (ftnlen)6) && 
	    m2wmch_(scndwd, &b2, &e2, "@int(1:20)", (ftnlen)32, (ftnlen)10)) {

/*        We don't write the output of a RECALL command to the */
/*        log file. */

	nspgst_("LOG", status, (ftnlen)3);
	nspioh_("LOG", (ftnlen)3);

/*        Find out the depth of the command to fetch. */

	nparsi_(scndwd, &depth, errstr, &ptr, (ftnlen)32, (ftnlen)132);

/*        Get rid of the top command (it's the RECALL command). */

	dmpbuf_();
	ssizec_(&c__80, buffer, (ftnlen)132);
	getbuf_(&depth, buffer, (ftnlen)132);
	i__2 = cardc_(buffer, (ftnlen)132);
	for (i__ = 1; i__ <= i__2; ++i__) {
/* Writing concatenation */
	    i__1[0] = 3, a__1[0] = "   ";
	    i__1[1] = 132, a__1[1] = buffer + ((i__3 = i__ + 5) < 86 && 0 <= 
		    i__3 ? i__3 : s_rnge("buffer", i__3, "edtcom_", (ftnlen)
		    547)) * 132;
	    s_cat(line, a__1, i__1, &c__2, (ftnlen)132);
	    nspwln_(line, (ftnlen)132);
	}

/*        Reset the status of the LOG file back to whatever it */
/*        was before we started dumping old commands.  Finally */
/*        set the command to a blank. */

	nsppst_("LOG", status, (ftnlen)3);
	s_copy(commnd, " ", commnd_len, (ftnlen)1);
	*source = 0;
	return 0;
    } else if (m2wmch_(frstwd, &b1, &e1, "RECALL", (ftnlen)32, (ftnlen)6)) {

/*        Find out the depth of the command to fetch. */

	getbsz_(&depth);
	suffix_("*", &c__0, scndwd, (ftnlen)1, (ftnlen)32);
	comnum = 2;
	gotone = FALSE_;
	while(comnum <= depth) {
	    ssizec_(&c__80, buffer, (ftnlen)132);
	    getbuf_(&comnum, buffer, (ftnlen)132);
	    if (cardc_(buffer, (ftnlen)132) > 0 && match_(buffer + 792, 
		    scndwd, (ftnlen)132, (ftnlen)32)) {

/*              We don't write the output of a RECALL command to the */
/*              log file. */

		nspgst_("LOG", status, (ftnlen)3);
		nspioh_("LOG", (ftnlen)3);

/*              Dump the top command as it is just the recall command. */

		dmpbuf_();
		i__2 = cardc_(buffer, (ftnlen)132);
		for (i__ = 1; i__ <= i__2; ++i__) {
/* Writing concatenation */
		    i__1[0] = 3, a__1[0] = "   ";
		    i__1[1] = 132, a__1[1] = buffer + ((i__3 = i__ + 5) < 86 
			    && 0 <= i__3 ? i__3 : s_rnge("buffer", i__3, 
			    "edtcom_", (ftnlen)591)) * 132;
		    s_cat(line, a__1, i__1, &c__2, (ftnlen)132);
		    nspwln_(line, (ftnlen)132);
		}
		comnum = depth;
		gotone = TRUE_;
		s_copy(commnd, " ", commnd_len, (ftnlen)1);
		*source = 0;
		nsppst_("LOG", status, (ftnlen)3);
	    }
	    ++comnum;
	}

/*        Reset the status of the LOG file back to whatever it */
/*        was before we started dumping old commands. */

	if (! gotone) {
	    s_copy(error, "There is no command in the command history list t"
		    "hat matches '#'. ", (ftnlen)132, (ftnlen)66);
	    repmc_(error, "#", scndwd, error, (ftnlen)132, (ftnlen)1, (ftnlen)
		    32, (ftnlen)132);
	    s_copy(commnd, " ", commnd_len, (ftnlen)1);
	    *source = 0;
	    setmsg_(error, (ftnlen)132);
	    sigerr_("EDTCOM(NOMATCH)", (ftnlen)15);
	    return 0;
	}
	return 0;
    } else if (m2wmch_(frstwd, &b1, &e1, "EDIT", (ftnlen)32, (ftnlen)4) && 
	    m2wmch_(scndwd, &b2, &e2, "@int(1:20)", (ftnlen)32, (ftnlen)10)) {
	nparsi_(scndwd, &depth, errstr, &ptr, (ftnlen)32, (ftnlen)132);
	dmpbuf_();
	ssizec_(&c__80, buffer, (ftnlen)132);
	getbuf_(&depth, buffer, (ftnlen)132);

/*        Open the utility port to receive the contents of BUFFER. */

	s_copy(pattrn, "edt{0-z}{0-z}{0-z}{0-z}{0-z}.tmp", (ftnlen)132, (
		ftnlen)32);
	newfil_(pattrn, "UTILITY", name__, (ftnlen)132, (ftnlen)7, (ftnlen)
		132);
	if (failed_()) {
	    reset_();
	    chkin_("EDTCOM", (ftnlen)6);
	    setmsg_("The program was unable to open a file that could be use"
		    "d with the editor. Command editing cannot be performed a"
		    "t this time. ", (ftnlen)124);
	    sigerr_("CMLOOP(COMMANDEDITFAILED)", (ftnlen)25);
	    chkout_("EDTCOM", (ftnlen)6);
	    return 0;
	}

/*        We have at this point succeeded in opening a file */
/*        into which we can write the last command.  But we */
/*        don't want to write to the screen, log file or save */
/*        file if there is one. Inhibit writing to any port */
/*        but the utility port. */

	nspgst_("LOG", lstat, (ftnlen)3);
	nspgst_("SCREEN", sstat, (ftnlen)6);
	nspgst_("SAVE", svstat, (ftnlen)4);
	nspioh_("LOG", (ftnlen)3);
	nspioh_("SCREEN", (ftnlen)6);
	nspioh_("SAVE", (ftnlen)4);
	i__2 = cardc_(buffer, (ftnlen)132);
	for (i__ = 1; i__ <= i__2; ++i__) {
	    nspwln_(buffer + ((i__3 = i__ + 5) < 86 && 0 <= i__3 ? i__3 : 
		    s_rnge("buffer", i__3, "edtcom_", (ftnlen)674)) * 132, (
		    ftnlen)132);
	}
	nspioc_("UTILITY", (ftnlen)7);

/*        Activate the editor */

	edtcmd_(editor, name__, rtrim_(editor, (ftnlen)132), rtrim_(name__, (
		ftnlen)132));
	s_copy(error, " ", (ftnlen)132, (ftnlen)1);
	s_copy(error + 132, " ", (ftnlen)132, (ftnlen)1);
	if (have_(error, (ftnlen)132)) {
	    nsppst_("LOG", lstat, (ftnlen)3);
	    nsppst_("SCREEN", sstat, (ftnlen)6);
	    nsppst_("SAVE", svstat, (ftnlen)4);
	    s_copy(commnd, " ", commnd_len, (ftnlen)1);
	    *source = 0;
	    setmsg_(error, (ftnlen)132);
	    sigerr_("SPICE(FILEREADERROR)", (ftnlen)20);
	    return 0;
	}

/*        Read the first command from the edited file. */

	prstrt_(name__, error, (ftnlen)132, (ftnlen)132);
	if (have_(error, (ftnlen)132)) {
	    nsppst_("LOG", lstat, (ftnlen)3);
	    nsppst_("SCREEN", sstat, (ftnlen)6);
	    nsppst_("SAVE", svstat, (ftnlen)4);
	    s_copy(commnd, " ", commnd_len, (ftnlen)1);
	    *source = 0;
	    prexit_();
	    setmsg_(error, (ftnlen)132);
	    sigerr_("SPICE(FILEREADERROR)", (ftnlen)20);
	    return 0;
	}
	prread_(delim, commnd, (ftnlen)1, commnd_len);
	putcom_(commnd, &c__2, commnd_len);
	prexit_();

/*        Finally, delete the file we used with the editor. */

	txtopr_(name__, &unit, (ftnlen)132);
	cl__1.cerr = 1;
	cl__1.cunit = unit;
	cl__1.csta = "DELETE";
	iostat = f_clos(&cl__1);
	ssizec_(&c__80, buffer, (ftnlen)132);
	getbuf_(&c__1, buffer, (ftnlen)132);
	nspioa_("SCREEN", (ftnlen)6);
	r__ = rtrim_(prompt, prompt_len) + 2;
	i__2 = cardc_(buffer, (ftnlen)132);
	for (i__ = 1; i__ <= i__2; ++i__) {
	    if (i__ == 1) {
		s_copy(line, prompt, (ftnlen)132, prompt_len);
		suffix_(buffer + ((i__3 = i__ + 5) < 86 && 0 <= i__3 ? i__3 : 
			s_rnge("buffer", i__3, "edtcom_", (ftnlen)738)) * 132,
			 &c__1, line, (ftnlen)132, (ftnlen)132);
	    } else {
		s_copy(line, " ", (ftnlen)132, (ftnlen)1);
		s_copy(line + (r__ - 1), buffer + ((i__3 = i__ + 5) < 86 && 0 
			<= i__3 ? i__3 : s_rnge("buffer", i__3, "edtcom_", (
			ftnlen)741)) * 132, 132 - (r__ - 1), (ftnlen)132);
	    }
	    nspwln_(line, rtrim_(line, (ftnlen)132));
	}

/*        Reset the writing to all other ports. */

	nsppst_("LOG", lstat, (ftnlen)3);
	nsppst_("SCREEN", sstat, (ftnlen)6);
	nsppst_("SAVE", svstat, (ftnlen)4);
	s_copy(commnd, " ", commnd_len, (ftnlen)1);
	*source = 0;
    } else if (m2wmch_(frstwd, &b1, &e1, "EDIT", (ftnlen)32, (ftnlen)4) && 
	    s_cmp(scndwd, " ", (ftnlen)32, (ftnlen)1) != 0) {
	gotone = FALSE_;
	comnum = 2;
	getbsz_(&depth);
	suffix_("*", &c__0, scndwd, (ftnlen)1, (ftnlen)32);
	while(comnum <= depth) {
	    ssizec_(&c__80, buffer, (ftnlen)132);
	    getbuf_(&comnum, buffer, (ftnlen)132);
	    if (cardc_(buffer, (ftnlen)132) > 0 && match_(buffer + 792, 
		    scndwd, (ftnlen)132, (ftnlen)32)) {
		gotone = TRUE_;
		dmpbuf_();

/*              Open the utility port to receive the contents of BUFFER. */

		s_copy(pattrn, "edt{0-z}{0-z}{0-z}{0-z}{0-z}.tmp", (ftnlen)
			132, (ftnlen)32);
		newfil_(pattrn, "UTILITY", name__, (ftnlen)132, (ftnlen)7, (
			ftnlen)132);
		if (failed_()) {
		    reset_();
		    chkin_("EDTCOM", (ftnlen)6);
		    setmsg_("The program was unable to open a file that coul"
			    "d be used with the editor. Command editing canno"
			    "t be performed at this time. ", (ftnlen)124);
		    sigerr_("CMLOOP(COMMANDEDITFAILED)", (ftnlen)25);
		    chkout_("EDTCOM", (ftnlen)6);
		    return 0;
		}

/*              We have at this point succeeded in opening a file */
/*              into which we can write the last command.  But we */
/*              don't want to write to the screen, log file or save */
/*              file if there is one. Inhibit writing to any port */
/*              but the utility port. */

		nspgst_("LOG", lstat, (ftnlen)3);
		nspgst_("SCREEN", sstat, (ftnlen)6);
		nspgst_("SAVE", svstat, (ftnlen)4);
		nspioh_("LOG", (ftnlen)3);
		nspioh_("SCREEN", (ftnlen)6);
		nspioh_("SAVE", (ftnlen)4);
		i__2 = cardc_(buffer, (ftnlen)132);
		for (i__ = 1; i__ <= i__2; ++i__) {
		    nspwln_(buffer + ((i__3 = i__ + 5) < 86 && 0 <= i__3 ? 
			    i__3 : s_rnge("buffer", i__3, "edtcom_", (ftnlen)
			    814)) * 132, (ftnlen)132);
		}
		nspioc_("UTILITY", (ftnlen)7);

/*              Activate the editor */

		edtcmd_(editor, name__, rtrim_(editor, (ftnlen)132), rtrim_(
			name__, (ftnlen)132));
		s_copy(error, " ", (ftnlen)132, (ftnlen)1);
		s_copy(error + 132, " ", (ftnlen)132, (ftnlen)1);
		if (have_(error, (ftnlen)132)) {
		    nsppst_("LOG", lstat, (ftnlen)3);
		    nsppst_("SCREEN", sstat, (ftnlen)6);
		    nsppst_("SAVE", svstat, (ftnlen)4);
		    s_copy(commnd, " ", commnd_len, (ftnlen)1);
		    *source = 0;
		    setmsg_(error, (ftnlen)132);
		    sigerr_("SPICE(FILEREADERROR)", (ftnlen)20);
		    return 0;
		}

/*              Read the first command from the edited file. */

		prstrt_(name__, error, (ftnlen)132, (ftnlen)132);
		if (have_(error, (ftnlen)132)) {
		    nsppst_("LOG", lstat, (ftnlen)3);
		    nsppst_("SCREEN", sstat, (ftnlen)6);
		    nsppst_("SAVE", svstat, (ftnlen)4);
		    s_copy(commnd, " ", commnd_len, (ftnlen)1);
		    *source = 0;
		    prexit_();
		    setmsg_(error, (ftnlen)132);
		    sigerr_("SPICE(FILEREADERROR)", (ftnlen)20);
		    return 0;
		}
		prread_(delim, commnd, (ftnlen)1, commnd_len);
		putcom_(commnd, &c__2, commnd_len);
		prexit_();

/*              Finally, delete the file we used with the editor. */

		txtopr_(name__, &unit, (ftnlen)132);
		cl__1.cerr = 1;
		cl__1.cunit = unit;
		cl__1.csta = "DELETE";
		iostat = f_clos(&cl__1);
		ssizec_(&c__80, buffer, (ftnlen)132);
		getbuf_(&c__1, buffer, (ftnlen)132);
		nspioa_("SCREEN", (ftnlen)6);
		r__ = rtrim_(prompt, prompt_len) + 2;
		i__2 = cardc_(buffer, (ftnlen)132);
		for (i__ = 1; i__ <= i__2; ++i__) {
		    if (i__ == 1) {
			s_copy(line, prompt, (ftnlen)132, prompt_len);
			suffix_(buffer + ((i__3 = i__ + 5) < 86 && 0 <= i__3 ?
				 i__3 : s_rnge("buffer", i__3, "edtcom_", (
				ftnlen)880)) * 132, &c__1, line, (ftnlen)132, 
				(ftnlen)132);
		    } else {
			s_copy(line, " ", (ftnlen)132, (ftnlen)1);
			s_copy(line + (r__ - 1), buffer + ((i__3 = i__ + 5) < 
				86 && 0 <= i__3 ? i__3 : s_rnge("buffer", 
				i__3, "edtcom_", (ftnlen)883)) * 132, 132 - (
				r__ - 1), (ftnlen)132);
		    }
		    nspwln_(line, rtrim_(line, (ftnlen)132));
		}

/*              Reset the writing to all other ports. */

		nsppst_("LOG", lstat, (ftnlen)3);
		nsppst_("SCREEN", sstat, (ftnlen)6);
		nsppst_("SAVE", svstat, (ftnlen)4);
		s_copy(commnd, " ", commnd_len, (ftnlen)1);
		*source = 0;
		comnum = depth;
	    }
	    ++comnum;
	}
	if (! gotone) {
	    s_copy(error, "There is no command in the command history list t"
		    "hat matches '#'. ", (ftnlen)132, (ftnlen)66);
	    repmc_(error, "#", scndwd, error, (ftnlen)132, (ftnlen)1, (ftnlen)
		    32, (ftnlen)132);
	    s_copy(commnd, " ", commnd_len, (ftnlen)1);
	    *source = 0;
	    setmsg_(error, (ftnlen)132);
	    sigerr_("EDTCOM(NOMATCH)", (ftnlen)15);
	    return 0;
	}
    } else if (m2wmch_(frstwd, &b1, &e1, "DO", (ftnlen)32, (ftnlen)2) && 
	    m2wmch_(scndwd, &b2, &e2, "@int(1:20)", (ftnlen)32, (ftnlen)10)) {
	nparsi_(scndwd, &depth, errstr, &ptr, (ftnlen)32, (ftnlen)132);
	dmpbuf_();
	ssizec_(&c__80, buffer, (ftnlen)132);
	getbuf_(&depth, buffer, (ftnlen)132);
	nspgst_("SCREEN", sstat, (ftnlen)6);
	nspioa_("SCREEN", (ftnlen)6);
	r__ = rtrim_(prompt, prompt_len) + 2;

/*        Reset the paragraph buffer so it can receive another */
/*        paragraph. (This is where we buffer commands and we */
/*        need to buffer this one.) */

	rstbuf_();
	i__2 = cardc_(buffer, (ftnlen)132);
	for (i__ = 1; i__ <= i__2; ++i__) {
	    putbuf_(buffer + ((i__3 = i__ + 5) < 86 && 0 <= i__3 ? i__3 : 
		    s_rnge("buffer", i__3, "edtcom_", (ftnlen)943)) * 132, (
		    ftnlen)132);
	    if (i__ == 1) {
		s_copy(line, prompt, (ftnlen)132, prompt_len);
		suffix_(buffer + ((i__3 = i__ + 5) < 86 && 0 <= i__3 ? i__3 : 
			s_rnge("buffer", i__3, "edtcom_", (ftnlen)947)) * 132,
			 &c__1, line, (ftnlen)132, (ftnlen)132);
	    } else {
		s_copy(line, " ", (ftnlen)132, (ftnlen)1);
		s_copy(line + (r__ - 1), buffer + ((i__3 = i__ + 5) < 86 && 0 
			<= i__3 ? i__3 : s_rnge("buffer", i__3, "edtcom_", (
			ftnlen)950)) * 132, 132 - (r__ - 1), (ftnlen)132);
	    }
	    nspwln_(line, rtrim_(line, (ftnlen)132));
	}
	nsppst_("SCREEN", sstat, (ftnlen)6);
	s_copy(commnd, " ", commnd_len, (ftnlen)1);
	s_copy(commnd, buffer + 792, commnd_len, (ftnlen)132);
	i__2 = cardc_(buffer, (ftnlen)132);
	for (i__ = 2; i__ <= i__2; ++i__) {
	    suffix_(buffer + ((i__3 = i__ + 5) < 86 && 0 <= i__3 ? i__3 : 
		    s_rnge("buffer", i__3, "edtcom_", (ftnlen)963)) * 132, &
		    c__1, commnd, (ftnlen)132, commnd_len);
	}
	i__ = i_indx(commnd, delim, commnd_len, (ftnlen)1);
	if (i__ > 0) {
	    putcom_(commnd, &c__2, i__ - 1);
	} else {
	    putcom_(commnd, &c__1, commnd_len);
	}
	s_copy(commnd, " ", commnd_len, (ftnlen)1);
	*source = 0;
    } else if (m2wmch_(frstwd, &b1, &e1, "DO", (ftnlen)32, (ftnlen)2) && 
	    s_cmp(scndwd, " ", (ftnlen)32, (ftnlen)1) != 0) {

/*        This is basically the same as the last case, but */
/*        we look for a pattern match before doing anything. */

	gotone = FALSE_;
	getbsz_(&depth);
	suffix_("*", &c__0, scndwd, (ftnlen)1, (ftnlen)32);
	comnum = 2;
	while(comnum <= depth) {
	    ssizec_(&c__80, buffer, (ftnlen)132);
	    getbuf_(&comnum, buffer, (ftnlen)132);
	    if (cardc_(buffer, (ftnlen)132) > 0 && match_(buffer + 792, 
		    scndwd, (ftnlen)132, (ftnlen)32)) {
		gotone = TRUE_;
		dmpbuf_();
		nspgst_("SCREEN", sstat, (ftnlen)6);
		nspioa_("SCREEN", (ftnlen)6);
		r__ = rtrim_(prompt, prompt_len) + 2;

/*              Reset the paragraph buffer so it can receive another */
/*              paragraph. (This is where we buffer commands and we */
/*              need to buffer this one.) */

		rstbuf_();
		i__2 = cardc_(buffer, (ftnlen)132);
		for (i__ = 1; i__ <= i__2; ++i__) {
		    putbuf_(buffer + ((i__3 = i__ + 5) < 86 && 0 <= i__3 ? 
			    i__3 : s_rnge("buffer", i__3, "edtcom_", (ftnlen)
			    1012)) * 132, (ftnlen)132);
		    if (i__ == 1) {
			s_copy(line, prompt, (ftnlen)132, prompt_len);
			suffix_(buffer + ((i__3 = i__ + 5) < 86 && 0 <= i__3 ?
				 i__3 : s_rnge("buffer", i__3, "edtcom_", (
				ftnlen)1015)) * 132, &c__1, line, (ftnlen)132,
				 (ftnlen)132);
		    } else {
			s_copy(line, " ", (ftnlen)132, (ftnlen)1);
			s_copy(line + (r__ - 1), buffer + ((i__3 = i__ + 5) < 
				86 && 0 <= i__3 ? i__3 : s_rnge("buffer", 
				i__3, "edtcom_", (ftnlen)1018)) * 132, 132 - (
				r__ - 1), (ftnlen)132);
		    }
		    nspwln_(line, rtrim_(line, (ftnlen)132));
		}
		nsppst_("SCREEN", sstat, (ftnlen)6);
		s_copy(commnd, " ", commnd_len, (ftnlen)1);
		s_copy(commnd, buffer + 792, commnd_len, (ftnlen)132);
		i__2 = cardc_(buffer, (ftnlen)132);
		for (i__ = 2; i__ <= i__2; ++i__) {
		    suffix_(buffer + ((i__3 = i__ + 5) < 86 && 0 <= i__3 ? 
			    i__3 : s_rnge("buffer", i__3, "edtcom_", (ftnlen)
			    1031)) * 132, &c__1, commnd, (ftnlen)132, 
			    commnd_len);
		}
		i__ = i_indx(commnd, delim, commnd_len, (ftnlen)1);
		if (i__ > 0) {
		    putcom_(commnd, &c__2, i__ - 1);
		} else {
		    putcom_(commnd, &c__1, commnd_len);
		}
		s_copy(commnd, " ", commnd_len, (ftnlen)1);
		*source = 0;
		comnum = depth;
	    }
	    ++comnum;
	}
	if (! gotone) {
	    s_copy(error, "There is no command in the command history list t"
		    "hat matches '#'. ", (ftnlen)132, (ftnlen)66);
	    repmc_(error, "#", scndwd, error, (ftnlen)132, (ftnlen)1, (ftnlen)
		    32, (ftnlen)132);
	    s_copy(commnd, " ", commnd_len, (ftnlen)1);
	    *source = 0;
	    setmsg_(error, (ftnlen)132);
	    sigerr_("EDTCOM(NOMATCH)", (ftnlen)15);
	    return 0;
	}
    }
    return 0;

L_setedt:
    s_copy(editor, commnd, (ftnlen)132, commnd_len);
    return 0;

L_getedt:
    s_copy(commnd, editor, commnd_len, (ftnlen)132);
    return 0;
} /* edtcom_ */

/* Subroutine */ int edtcom_(char *delim, char *prompt, char *commnd, integer 
	*source, ftnlen delim_len, ftnlen prompt_len, ftnlen commnd_len)
{
    return edtcom_0_(0, delim, prompt, commnd, source, delim_len, prompt_len, 
	    commnd_len);
    }

/* Subroutine */ int setedt_(char *commnd, ftnlen commnd_len)
{
    return edtcom_0_(1, (char *)0, (char *)0, commnd, (integer *)0, (ftnint)0,
	     (ftnint)0, commnd_len);
    }

/* Subroutine */ int getedt_(char *commnd, ftnlen commnd_len)
{
    return edtcom_0_(2, (char *)0, (char *)0, commnd, (integer *)0, (ftnint)0,
	     (ftnint)0, commnd_len);
    }

