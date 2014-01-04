/* nxtcom.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static logical c_true = TRUE_;
static integer c__0 = 0;
static integer c__3 = 3;

/* $Procedure      NXTCOM ( Next command ) */
/* Subroutine */ int nxtcom_0_(int n__, char *prompt, char *delim, char *
	commnd, integer *source, ftnlen prompt_len, ftnlen delim_len, ftnlen 
	commnd_len)
{
    /* Initialized data */

    static integer buffed = 0;
    static logical first = TRUE_;
    static logical readng = FALSE_;
    static char savdlm[1] = ";";
    static char savpmt[80] = "                                              "
	    "                                  ";

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static char file[128], exit[32], word[80], rest[128], stop[32];
    extern logical batch_(void);
    extern /* Subroutine */ int chkin_(char *, ftnlen), ucase_(char *, char *,
	     ftnlen, ftnlen);
    static logical nocom;
    extern /* Subroutine */ int prclr_(void);
    static char error[300], start[32], buffer[1024*20];
    extern integer brckti_(integer *, integer *, integer *);
    extern /* Subroutine */ int prread_(char *, char *, ftnlen, ftnlen);
    static integer bufsrc[20];
    static char lngmsg[300];
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), nsplog_(char *, logical *, 
	    ftnlen);
    static char shtmsg[32];
    extern /* Subroutine */ int trnlat_(char *, char *, ftnlen, ftnlen), 
	    nextwd_(char *, char *, char *, ftnlen, ftnlen, ftnlen), prexit_(
	    void), rdstmt_(char *, char *, char *, ftnlen, ftnlen, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int prstrt_(char *, char *, ftnlen, ftnlen);

/* $ Abstract */

/*     Get the next command from the keyboard or a file. */

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
/*     PARSING */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Entry */
/*     --------  ---  -------------------------------------------------- */
/*     PROMPT     I   SETDAP */
/*     DELIM      I   SETDAP */
/*     COMMND     O   GETCOM */
/*     SOURCE     O   GETCOM */

/* $ Detailed_Input */

/*     See the ENTRY points for a discussion of their arguments. */

/* $ Detailed_Output */

/*     See the ENTRY points for a discussion of their arguments. */

/* $ Files */

/*     If the commands are contained in a file, they will be read from */
/*     that file. (The 'START' keyword indicates that commands are to */
/*     be read from a specified file.) If they are not contained in a */
/*     file, they are read from the keyboard. */

/* $ Exceptions */

/*     1) If NXTCOM is called directly, the error SPICE(BOGUSENTRY) is */
/*        signalled. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     The delimeter has a fixed length of one character. The prompt has */
/*     a fixed length of eighty characters. */

/*     The file name length has been parameterized internally to the */
/*     maximum file name length length on the VAX, 128 characters. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     H.A. Neilan    (JPL) */

/* $ Version */

/* -     Commad Loop Version 2.0.0              19-NOV-1995 (WLT) */

/*         Added the batch mode capability.  If the BATCH function */
/*         returns TRUE then all keyboard routines return EXIT. */

/* -     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 4, 1994 */


/* -    Beta Version 1.0.0, 02-DEC-1988 (HAN) */

/* -& */

/*     SPICELIB functions */

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


/*     Below are the various sources from which */
/*     commands might come. */

/*     NONE */
/*     COMBUF */
/*     KEYBRD */
/*     INPFIL */


/*     Local variables */


/*     Initial values */

    switch(n__) {
	case 1: goto L_getcom;
	case 2: goto L_setdap;
	case 3: goto L_putcom;
	}


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("NXTCOM", (ftnlen)6);
    }

/*     This routine should never be called. If this routine is called, */
/*     an error is signalled. */

    setmsg_("NXTCOM: You have called an entry which performs no run-time fun"
	    "ction. This may indicate a bug. Please check the documentation f"
	    "or the subroutine NXTCOM.", (ftnlen)152);
    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("NXTCOM", (ftnlen)6);
    return 0;
/* $Procedure GETCOM ( Get a command ) */

L_getcom:
/* $ Abstract */

/*     Get a command from a file or the keyboard. */

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
/*     PARSING */

/* $ Declarations */

/*     CHARACTER*(*)         COMMND */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     COMMND     O   A command read from a file or from the screen. */
/*     SOURCE     O   The source of the command, file, terminal etc. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     COMMND     is the command which is parsed from a line read from */
/*                either the screen or a file. */

/*     SOURCE     Is an integer that indicates the source of the */
/*                command.  The table below shows the various */
/*                values and their meanings. */

/*                   0  ---   None, an error occurred */
/*                   1  ---   Command buffer */
/*                   2  ---   From standard input */
/*                   3  ---   From a STARTED File. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     If an error occurs in PRSTRT, the error SPICE(FILEREADFAILED) */
/*     is signalled. ( PRSTRT has not been modified to participate in the */
/*     new error handling. ) */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     H.A. Neilan    (JPL) */

/* $ Version */

/* -     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 4, 1994 */


/* -    Beta Version 1.0.0, 29-NOV-1988 (HAN) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("GETCOM", (ftnlen)6);
    }
    if (first) {
	trnlat_("STOP", stop, (ftnlen)4, (ftnlen)32);
	trnlat_("EXIT", exit, (ftnlen)4, (ftnlen)32);
	trnlat_("START", start, (ftnlen)5, (ftnlen)32);
	trnlat_("DEFPROMPT", savpmt, (ftnlen)9, (ftnlen)80);
	first = FALSE_;
    }

/*     While we don't have a command, try to get one.  We look */
/*     in the command buffer first. */


/*     We don't have a command yet. */

    nocom = TRUE_;
    while(nocom) {
	if (buffed > 0) {
	    s_copy(commnd, buffer + (((i__1 = buffed - 1) < 20 && 0 <= i__1 ? 
		    i__1 : s_rnge("buffer", i__1, "nxtcom_", (ftnlen)366)) << 
		    10), commnd_len, (ftnlen)1024);
	    *source = bufsrc[(i__1 = buffed - 1) < 20 && 0 <= i__1 ? i__1 : 
		    s_rnge("bufsrc", i__1, "nxtcom_", (ftnlen)367)];
	    --buffed;
	} else {

/*           If we're already reading from a file then just let PRREAD */
/*           take care of obtaining the command. If PRREAD reaches the */
/*           end of the current file, the previous file is popped off */
/*           the stack, and the next command from this file is read */
/*           instead. (If no files remain to be read, DELIM is returned.) */
/*           In that case we are no longer reading from files. */

	    if (readng) {
		prread_(savdlm, commnd, (ftnlen)1, commnd_len);
		*source = 3;
		if (s_cmp(commnd, savdlm, commnd_len, (ftnlen)1) == 0) {
		    readng = FALSE_;
		}
	    }

/*           If we're not reading from a file, get the command from the */
/*           keyboard. ( If the command was terminated by a blank line, */
/*           the command is returned as a blank. ) */

	    if (! readng) {
		if (batch_()) {
		    s_copy(commnd, exit, commnd_len, (ftnlen)32);
		} else {
		    rdstmt_(savpmt, savdlm, commnd, (ftnlen)80, (ftnlen)1, 
			    commnd_len);
		}
		*source = 2;
	    }
	}

/*        We must have a command at this point. */

	nocom = FALSE_;

/*        We need to check to see if what we have is a control word. */

	nextwd_(commnd, word, rest, commnd_len, (ftnlen)80, (ftnlen)128);
	ucase_(word, word, (ftnlen)80, (ftnlen)80);

/*        If the control word is 'START', we know that we will be */
/*        reading from a file. Let PRSTRT take care of keeping track of */
/*        the files being read from. If there's a problem in PRSTRT we */
/*        need to signal an error here due to PRSTRT's error handling. */
/*        Bail out if there's a problem. If all goes well in PRSTR, */
/*        we will read the first command in the file the next pass */
/*        through the DO LOOP. */

	if (s_cmp(word, start, (ftnlen)80, (ftnlen)32) == 0) {

/*           We need to log this command commented out so that anyone */
/*           using the resulting log file, will not have to worry */
/*           about starting a file twice. */

	    nsplog_(commnd, &c_true, commnd_len);
	    s_copy(file, " ", (ftnlen)128, (ftnlen)1);
	    nextwd_(rest, file, rest, (ftnlen)128, (ftnlen)128, (ftnlen)128);
	    if (s_cmp(file, " ", (ftnlen)128, (ftnlen)1) == 0) {
		*source = 0;
		trnlat_("MISSINGFILELONG", lngmsg, (ftnlen)15, (ftnlen)300);
		trnlat_("MISSINGFILESHORT", shtmsg, (ftnlen)16, (ftnlen)32);
		setmsg_(lngmsg, (ftnlen)300);
		sigerr_(shtmsg, (ftnlen)32);
		chkout_("GETCOM", (ftnlen)6);
		return 0;
	    }
	    prstrt_(file, error, (ftnlen)128, (ftnlen)300);

/*           If an error occurs in PRSTRT we're in trouble. Signal an */
/*           error and bail. If there's no problem, we're now reading */
/*           from a file. */

	    if (s_cmp(error, " ", (ftnlen)300, (ftnlen)1) != 0) {
		*source = 0;
		trnlat_("MISSINGFILESHORT", shtmsg, (ftnlen)16, (ftnlen)32);
		setmsg_(error, (ftnlen)300);
		sigerr_(shtmsg, (ftnlen)32);
		chkout_("GETCOM", (ftnlen)6);
		return 0;
	    } else {
		readng = TRUE_;
		nocom = TRUE_;
	    }

/*        If the control word is 'STOP', clear the stack of files. */
/*        If we were reading commands from files, we won't be anymore. */
/*        If we were reading commands from the keyboard, the command to */
/*        return is 'STOP'. */

	} else if (s_cmp(word, stop, (ftnlen)80, (ftnlen)32) == 0) {
	    if (readng) {
		prclr_();
		nsplog_(commnd, &c_true, commnd_len);
		readng = FALSE_;
		nocom = TRUE_;
	    } else {
		s_copy(commnd, word, commnd_len, (ftnlen)80);
	    }

/*        If the control word is 'EXIT', and we're reading from a file, */
/*        we need to remove that file from the stack. If we're reading */
/*        commands from the keyboard, we'll return the command 'EXIT'. */

	} else if (s_cmp(word, exit, (ftnlen)80, (ftnlen)32) == 0) {
	    if (readng) {
		prexit_();
		nsplog_(commnd, &c_true, commnd_len);
		nocom = TRUE_;
	    } else {
		s_copy(commnd, word, commnd_len, (ftnlen)80);
	    }
	}
    }
    chkout_("GETCOM", (ftnlen)6);
    return 0;
/* $Procedure SETDAP ( Set the delimeter and prompt values ) */

L_setdap:
/* $ Abstract */

/*     Set the delimeter and prompt values that are used for parsing */
/*     commands. */

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

/*     CHARACTER */
/*     PARSING */

/* $ Declarations */

/*     CHARACTER*1           DELIM */
/*     CHARACTER*80          PROMPT */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     DELIM     I/O  Character delimiting the end of a command. */
/*     PROMPT    I/O  Character string indicating the beginning of a */
/*                    command. */

/* $ Detailed_Input */

/*     DELIM      is a single character delimiting the end of a command. */
/*                The default value of DELIM is ';'. */

/*     PROMPT     is a character string indicating the beginning of a */
/*                command. PROMPT has a maximum length of eighty */
/*                characters. The default value of PROMPT is 'Next? >'. */

/* $ Detailed_Output */

/*     DELIM      is the new character delimiting the end of a command. */

/*     PROMPT     is the new character string indicating the beginning */
/*                of a command. PROMPT has a maximum length of eighty */
/*                characters. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Particulars */

/*     DELIM and PROMPT have the default values of ';' and 'Next? >' */
/*     respectively. This module is called in order to change their */
/*     values. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     H.A. Neilan    (JPL) */

/* $ Version */

/* -     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 4, 1994 */


/* -    Beta Version 1.0.0, 02-DEC-1988 (HAN) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SETDAP", (ftnlen)6);
    }

/*     Set the values of the delimeter and prompt. */

    s_copy(savdlm, delim, (ftnlen)1, delim_len);
    s_copy(savpmt, prompt, (ftnlen)80, prompt_len);
    trnlat_("STOP", stop, (ftnlen)4, (ftnlen)32);
    trnlat_("EXIT", exit, (ftnlen)4, (ftnlen)32);
    trnlat_("START", start, (ftnlen)5, (ftnlen)32);
    if (s_cmp(savpmt, " ", (ftnlen)80, (ftnlen)1) == 0) {
	trnlat_("DEFPROMPT", savpmt, (ftnlen)9, (ftnlen)80);
    }
    first = FALSE_;
    chkout_("SETDAP", (ftnlen)6);
    return 0;

/* $ Procedure */


L_putcom:

/* $ Version */

/* -     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 4, 1994 */

    if (buffed < 20) {
	++buffed;
	s_copy(buffer + (((i__1 = buffed - 1) < 20 && 0 <= i__1 ? i__1 : 
		s_rnge("buffer", i__1, "nxtcom_", (ftnlen)685)) << 10), 
		commnd, (ftnlen)1024, commnd_len);
	bufsrc[(i__1 = buffed - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("bufsrc",
		 i__1, "nxtcom_", (ftnlen)686)] = brckti_(source, &c__0, &
		c__3);
	return 0;
    }

/*     If you get to this point there's a problem.  No room */
/*     is left in the command buffer. */

    chkin_("PUTCOM", (ftnlen)6);
    trnlat_("COMBUFFULLLNG", lngmsg, (ftnlen)13, (ftnlen)300);
    trnlat_("COMBUFFULLSHT", shtmsg, (ftnlen)13, (ftnlen)32);
    setmsg_(lngmsg, (ftnlen)300);
    sigerr_(shtmsg, (ftnlen)32);
    chkout_("PUTCOM", (ftnlen)6);
    return 0;
} /* nxtcom_ */

/* Subroutine */ int nxtcom_(char *prompt, char *delim, char *commnd, integer 
	*source, ftnlen prompt_len, ftnlen delim_len, ftnlen commnd_len)
{
    return nxtcom_0_(0, prompt, delim, commnd, source, prompt_len, delim_len, 
	    commnd_len);
    }

/* Subroutine */ int getcom_(char *commnd, integer *source, ftnlen commnd_len)
{
    return nxtcom_0_(1, (char *)0, (char *)0, commnd, source, (ftnint)0, (
	    ftnint)0, commnd_len);
    }

/* Subroutine */ int setdap_(char *delim, char *prompt, ftnlen delim_len, 
	ftnlen prompt_len)
{
    return nxtcom_0_(2, prompt, delim, (char *)0, (integer *)0, prompt_len, 
	    delim_len, (ftnint)0);
    }

/* Subroutine */ int putcom_(char *commnd, integer *source, ftnlen commnd_len)
{
    return nxtcom_0_(3, (char *)0, (char *)0, commnd, source, (ftnint)0, (
	    ftnint)0, commnd_len);
    }

