/* cmloop.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__7 = 7;
static integer c__3 = 3;
static integer c__1 = 1;
static integer c__78 = 78;
static logical c_false = FALSE_;

/* $Proceedure CMLOOP ( Command line loop ) */

/* Subroutine */ int cmloop_(char *delim, char *prompt, char *lognam, char *
	versn, S_fp greet, S_fp preprc, S_fp action, ftnlen delim_len, ftnlen 
	prompt_len, ftnlen lognam_len, ftnlen versn_len)
{
    /* Initialized data */

    static char spcial[8*2] = "        " "?       ";

    /* System generated locals */
    address a__1[2], a__2[7], a__3[3];
    integer i__1[2], i__2[7], i__3[3], i__4;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen), s_cat(char *,
	     char **, integer *, integer *, ftnlen);
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    extern /* Subroutine */ int echo_(char *, char *, ftnlen, ftnlen);
    extern logical have_(char *, ftnlen);
    static integer from;
    static logical trap;
    static integer rest, l;
    static logical dolog;
    extern integer ltrim_(char *, ftnlen);
    static char error[1760*2], com2do[1024];
    extern logical no_(char *, ftnlen);
    extern /* Subroutine */ int logchk_(char *, char *, logical *, ftnlen, 
	    ftnlen), cmredo_(char *, integer *, logical *, ftnlen);
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    static char commnd[1024], errflg[32];
    extern logical cmmore_(char *, ftnlen);
    extern /* Subroutine */ int setdel_(char *, ftnlen), erract_(char *, char 
	    *, ftnlen, ftnlen);
    static char usenam[255];
    extern /* Subroutine */ int errdev_(char *, char *, ftnlen, ftnlen);
    static logical problm;
    extern /* Subroutine */ int setdap_(char *, char *, ftnlen, ftnlen), 
	    getcom_(char *, integer *, ftnlen), edtcom_(char *, char *, char *
	    , integer *, ftnlen, ftnlen, ftnlen), builtn_(char *, logical *, 
	    char *, ftnlen, ftnlen), nsplog_(char *, logical *, ftnlen), 
	    nspend_(void), trnlat_(char *, char *, ftnlen, ftnlen), nsplgs_(
	    char *, char *, char *, ftnlen, ftnlen, ftnlen);
    static char hstyle[120];
    extern /* Subroutine */ int nsperr_(char *, char *, ftnlen, ftnlen), 
	    nspopl_(char *, char *, ftnlen, ftnlen);
    static char lstyle[120];
    extern /* Subroutine */ int cmstup_(void);
    extern integer qrtrim_(char *, ftnlen);
    extern /* Subroutine */ int nspslr_(integer *, integer *);
    static char sstyle[120];
    extern /* Subroutine */ int ressym_(char *, char *, ftnlen, ftnlen);
    static char vstyle[120];
    extern /* Subroutine */ int nspsty_(char *, char *, ftnlen, ftnlen);
    static logical log__[4], hit;


/* $ Abstract */

/*     This routine handles the main processing loop of a */
/*     command driven program. */

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

/*     INTERFACE */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     DELIM      I   Non-blank character used to delimit commands */
/*     PROMPT     I   Prompt to let the user know input is expected */
/*     LOGNAM     I   Name pattern of file where commands will be logged */
/*     VERSN      I   Program name and version */
/*     INTIZE     S   Subroutine that initializes I/O facilities */
/*     GREET      S   Displays a banner at program startup */
/*     ACTION     S   The command parser and processor. */

/* $ Detailed_Input */

/*     DELIM     is a character that will be used to tell the */
/*               program that a command has ended. Commands may */
/*               extend over as many lines as allowed by the */
/*               configuration include file.  They end on the */
/*               first line on which the delimiter character is */
/*               encountered. THIS CHARACTER MUST NOT BE "?" */

/*     PROMPT    is a string used to prompt the user for commands. */
/*               Typically, this is the name of the program that */
/*               calles CMLOOP. */

/*     LOGNAM    is a pattern to use when creating the name of */
/*               a file to which all commands will be written. */
/*               This can be hard coded in the calling */
/*               program, or may be determined by a file naming */
/*               convention such as is provided by Christen */
/*               and NOMEN. */

/*     VERSN     is a string that may contain anything you would */
/*               like to appear as descriptive text in the first */
/*               line of the log file (and possibly in the greeting */
/*               presented by the program)  Something like */
/*               '<program name> --- Version X.Y' would be appropriate. */
/*               For example if your programs name is KINDLE and you */
/*               are at version 4.2.3 of your program a good value for */
/*               VERSN would be */

/*               'KINDLE --- Version 4.2.3' */

/*               Your greeting routine can make use of this when */
/*               displaying your program's greeting.  In this way */
/*               you can centralize the name and version number of */
/*               your program at a high level or in a subroutine and */
/*               simply make the information available to CMLOOP so */
/*               that the automatic aspects of presenting this */
/*               information can be handled for you. */


/*     GREET     is a routine that displays a message at program */
/*               startup.  This should contain the version number */
/*               of the program, any general instructions such */
/*               as how to get help and who the author or organization */
/*               is that is responsible for this program. GREET */
/*               takes a single argument VERSN which you supply in */
/*               your call to CMLOOP.  It may also have */
/*               initializations that override various items set */
/*               up prior to the call to GREET such as the style */
/*               used for displaying error messages.  GREET */
/*               is the action taken by CMLOOP  before commencing the */
/*               loop of fetching and processing commands. */

/*     PREPRC    is a command preprocessor.  It might remove */
/*               non-printing characters such as TABS, resolve */
/*               symbols and convert units to expected ranges. */

/*     ACTION    is a routine responsible for action upon the commands */
/*               entered by a user at the keyboard. ACTION has two */
/*               arguments COMMAND a string input and ERROR a two */
/*               dimensional array for error and diagnostic output. */
/*               The first message should point to the the problem */
/*               assuming the user is aware of the context in which */
/*               the problem occurred.  The second message will */
/*               have more detailed information including trace */
/*               and other technical information. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     The parameters COMSIZ and ERRSIZ are given in the include */
/*     file commdpar.inc. */

/*     COMSIZ  is maximum number of characters that can be present */
/*             in a command. */

/*     ERRSIZ  is the maximum number of characters that can be used */
/*             when creating a diagnostic message. */

/* $ Exceptions */

/*     None.  This routine cannot detect any errors in its inputs */
/*     and all commands are regarded as legal input at this level. */
/*     Some can be acted on while others cannot.  Commands that */
/*     can not be exercised are expected to return diagnostics */
/*     in the array ERROR.  These are then reported by the */
/*     program to the user via his/her terminal. */

/* $ Files */

/*     The file specified by LOGFIL will be opened if possible */
/*     and all user commands and messages will be written to this */
/*     file. */

/*     Other files may be used a run time by "STARTing" a command */
/*     sequence file. Or by some result of the activity of the */
/*     user supplied routines ACTION, GREET, PREPRC. */

/* $ Particulars */

/*     This routine organizes the main loop of a command line */
/*     program so that the calling program can automatically */
/*     log files that a user enters, report errors in a uniform */
/*     manner and make use of sequences of commands stored in */
/*     files. The calling program supplies routines that handle */
/*     the chores of greeting the user and performing special */
/*     program initializations and performing actions based upon */
/*     the commands supplied by the user.  By making use of this */
/*     routine and its subordinates, the user inherits a flexible */
/*     I/O system and command interface freeing him/her to concentrate */
/*     on the actions of the program. */

/*     However, there is a minor price incurred by making use of */
/*     this routine.  Several commands have specific meanings that */
/*     the user cannot override.  They are commands that start with: */

/*        start */
/*        exit */
/*        stop */
/*        quit */
/*        echo */
/*        no echo */
/*        demo on */
/*        demo off */
/*        wait on */
/*        wait off */
/*        pause */
/*        ? */
/*     These commands are case insensitive with respect to the */
/*     words presented above. */


/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber    (JPL) */

/* $ Version */

/* -     Command Loop Configured Version 6.0.0, 20-JUN-2000 (WLT) */

/*         Added the ability to run the loop without logging */
/*         of inputs. */

/* -     Command Loop Configured Version 5.0.0, 23-MAR-2000 (WLT) */

/*         Modified the routine to call NSPEND instead of FINISH */
/*         now that NSPIO has been redone. */

/* -     Command Loop Configured Version 4.0.0, 20-NOV-1995 (WLT) */

/*         Added ability to run programs in batch mode and to */
/*         start procedures at program startup. */

/* -     Command Loop Configured Version 3.0.0, 1-AUG-1995 (WLT) */

/*         The routine was modified to better support command */
/*         pre-processing.  In particular symbol definition */
/*         and resolution is now supported. */

/* -     Command Loop Configured Version 2.0.0, 19-JUL-1995 (WLT) */

/*         A slight change was made so that the command delimiter */
/*         is now stored in the routine GETDEL.  Also errors */
/*         are now checked after command pre-processing has */
/*         been performed. */

/* -     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 4, 1994 */


/* -    Beta Version 1.0.0, 8-OCT-1993 (WLT) */

/* -& */

/*     Language Sensitive Strings */

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

/* $ Version */

/* -     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 4, 1994 */


/*     The following parameters are the system parameters required */
/*     by PERCY.  Be sure to read any notes before adjusting these */


/*     The maximum number of commands that can be buffered is */
/*     determined by the value of MAXCOM.  This parameter is */
/*     used primarily by NXTCOM. */


/*     The parameter FILEN is the maximum length of a file name */
/*     on a particular system. */


/*     The parameter COMSIZ is the maximum length allowed for a */
/*     command. */


/*     The parameter ERRSIZ is the maximum length allowed for */
/*     error messages. */


/*     The parameter STYSIZ is the maximum length expected for */
/*     a NICEPR style string. */


/*     The following are for special commands that will not be */
/*     processed by ACTION. */


/*     Store the delimiter used by the program incase someone */
/*     else needs to know later on. */

    setdel_(delim, delim_len);

/*     First, set up the SPICELIB error handling. */

    s_copy(error, " ", (ftnlen)1760, (ftnlen)1);
    s_copy(error + 1760, " ", (ftnlen)1760, (ftnlen)1);
    s_copy(commnd, " ", (ftnlen)1024, (ftnlen)1);
    log__[0] = FALSE_;
    log__[1] = FALSE_;
    log__[2] = TRUE_;
    log__[3] = TRUE_;
    erract_("SET", "RETURN", (ftnlen)3, (ftnlen)6);
    errdev_("SET", "NULL", (ftnlen)3, (ftnlen)4);

/*     Set the prompt for the program. */

    setdap_(delim, prompt, delim_len, prompt_len);

/*     The following styles are for reporting errors to the */
/*     screen and log file respectively. */

    trnlat_("ERRFLAG", errflg, (ftnlen)7, (ftnlen)32);
/* Writing concatenation */
    i__1[0] = 38, a__1[0] = "HARDSPACE ^ NEWLINE /cr VTAB /vt FLAG ";
    i__1[1] = 32, a__1[1] = errflg;
    s_cat(sstyle, a__1, i__1, &c__2, (ftnlen)120);
/* Writing concatenation */
    i__2[0] = 38, a__2[0] = "HARDSPACE ^ NEWLINE /cr VTAB /vt FLAG ";
    i__2[1] = 1, a__2[1] = delim;
    i__2[2] = qrtrim_(errflg, (ftnlen)32), a__2[2] = errflg;
    i__2[3] = 8, a__2[3] = " LEADER ";
    i__2[4] = 1, a__2[4] = delim;
    i__2[5] = 3, a__2[5] = "-- ";
    i__2[6] = 16, a__2[6] = "LEFT 1 RIGHT 72 ";
    s_cat(lstyle, a__2, i__2, &c__7, (ftnlen)120);

/*     The following styles will be used for logging of */
/*     commands and for commenting them out. */

    s_copy(vstyle, "LEFT 1 RIGHT 78 ", (ftnlen)120, (ftnlen)16);
/* Writing concatenation */
    i__3[0] = 23, a__3[0] = "LEFT 1 RIGHT 78 LEADER ";
    i__3[1] = 1, a__3[1] = delim;
    i__3[2] = 3, a__3[2] = "-- ";
    s_cat(hstyle, a__3, i__3, &c__3, (ftnlen)120);
    nspsty_(sstyle, lstyle, (ftnlen)120, (ftnlen)120);
    nsplgs_(vstyle, hstyle, delim, (ftnlen)120, (ftnlen)120, delim_len);
    nspslr_(&c__1, &c__78);

/*     See whether or not a log file should be used and if so */
/*     what it's name should be. */

    logchk_(lognam, usenam, &dolog, lognam_len, (ftnlen)255);

/*     Open a log file. */

    if (dolog) {
	nspopl_(usenam, versn, (ftnlen)255, versn_len);
    }
    if (have_(error, (ftnlen)1760)) {
	nsperr_(commnd, error, (ftnlen)1024, (ftnlen)1760);
    }

/*     Present a greeting to the user and perform any override */
/*     or special initializations that need to be local to this */
/*     routine. */

    (*greet)(versn, versn_len);

/*     Get the input command line.  This may have */
/*     several useful bits of information to tell us how */
/*     to run the program. */

/*     -b      means run the program in batch mode.  In this case */
/*             we should never prompt the user for information. */

/*     -start  means we have a startup file to use and we want to */
/*             use the name of that file to determine how to */
/*             proceed. */

    cmstup_();

/*     Fetch and log the first command. */

    trap = TRUE_;

/*     Get the next command and resolve any symbols or */
/*     queries that might show up in it, */

    while(trap) {
	getcom_(com2do, &from, (ftnlen)1024);
	edtcom_(delim, prompt, com2do, &from, delim_len, prompt_len, (ftnlen)
		1024);
	if (no_(error, (ftnlen)1760) && log__[(i__4 = from) < 4 && 0 <= i__4 ?
		 i__4 : s_rnge("log", i__4, "cmloop_", (ftnlen)430)]) {
	    nsplog_(com2do, &c_false, (ftnlen)1024);
	}
	if (no_(error, (ftnlen)1760)) {
	    ressym_(com2do, commnd, (ftnlen)1024, (ftnlen)1024);
	    echo_(com2do, commnd, (ftnlen)1024, (ftnlen)1024);
	}
	if (no_(error, (ftnlen)1760)) {
	    cmredo_(commnd, &from, &trap, (ftnlen)1024);
	}
	if (have_(error, (ftnlen)1760)) {
	    trap = FALSE_;
	}
    }

/*     Now apply the user's preprocessing software */
/*     to the comman. */

    s_copy(com2do, commnd, (ftnlen)1024, (ftnlen)1024);
    (*preprc)(com2do, commnd, (ftnlen)1024, (ftnlen)1024);

/*     Now process commands until we get an EXIT command. */

    while(cmmore_(commnd, (ftnlen)1024)) {

/*        Perform any preprocessing that can be performed easily */
/*        on this command. */

	if (no_(error, (ftnlen)1760)) {
	    builtn_(commnd, &hit, error, (ftnlen)1024, (ftnlen)1760);
	}
	if (no_(error, (ftnlen)1760) && ! hit) {
	    l = ltrim_(commnd, (ftnlen)1024);
	    rest = qrtrim_(commnd, (ftnlen)1024) + 1;
	    if (isrchc_(commnd + (l - 1), &c__2, spcial, rest - (l - 1), (
		    ftnlen)8) == 0) {
		(*action)(commnd, error, (ftnlen)1024, (ftnlen)1760);
	    }
	}
	problm = have_(error, (ftnlen)1760);

/*        Process any errors that were diagnosed. */

	nsperr_(commnd, error, (ftnlen)1024, (ftnlen)1760);

/*        Fetch and log the next command. */

	trap = TRUE_;
	while(trap) {
	    getcom_(com2do, &from, (ftnlen)1024);
	    edtcom_(delim, prompt, com2do, &from, delim_len, prompt_len, (
		    ftnlen)1024);
	    if (no_(error, (ftnlen)1760) && log__[(i__4 = from) < 4 && 0 <= 
		    i__4 ? i__4 : s_rnge("log", i__4, "cmloop_", (ftnlen)496)]
		    ) {
		nsplog_(com2do, &c_false, (ftnlen)1024);
	    }
	    if (no_(error, (ftnlen)1760)) {
		ressym_(com2do, commnd, (ftnlen)1024, (ftnlen)1024);
		echo_(com2do, commnd, (ftnlen)1024, (ftnlen)1024);
	    }
	    if (no_(error, (ftnlen)1760)) {
		cmredo_(commnd, &from, &trap, (ftnlen)1024);
	    }
	    if (have_(error, (ftnlen)1760)) {
		trap = FALSE_;
	    }
	}

/*        Now apply the user's preprocessing software */
/*        to the comman. */

	s_copy(com2do, commnd, (ftnlen)1024, (ftnlen)1024);
	(*preprc)(com2do, commnd, (ftnlen)1024, (ftnlen)1024);
    }

/*     Take care of closing files and so on. */

    if (log__[(i__4 = from) < 4 && 0 <= i__4 ? i__4 : s_rnge("log", i__4, 
	    "cmloop_", (ftnlen)526)]) {
	nspend_();
    }
    return 0;
} /* cmloop_ */

