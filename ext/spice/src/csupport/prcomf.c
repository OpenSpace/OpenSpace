/* prcomf.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $ Procedure */

/* Subroutine */ int prcomf_0_(int n__, char *file, char *delim, char *
	command, char *error, char *level, ftnlen file_len, ftnlen delim_len, 
	ftnlen command_len, ftnlen error_len, ftnlen level_len)
{
    /* Initialized data */

    static integer nest = 0;

    /* System generated locals */
    integer i__1;
    cilist ci__1;
    cllist cl__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer), f_clos(cllist *);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rsfe(cilist *), do_fio(integer *, char *, ftnlen), e_rsfe(void),
	     i_indx(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern logical have_(char *, ftnlen);
    static integer i__, j;
    static char files[80*8];
    static integer units[8];
    extern /* Subroutine */ int lbuild_(char *, integer *, char *, char *, 
	    ftnlen, ftnlen, ftnlen);
    extern integer lastnb_(char *, ftnlen);
    static integer iostat;
    extern /* Subroutine */ int rstbuf_(void), putbuf_(char *, ftnlen), 
	    txtopr_(char *, integer *, ftnlen);


/* $ Abstract */

/*     Keep track of nested command files. */

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

/* $ Keywords */

/*     PARSE */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  --------------------------------------------------- */
/*     FILE       I   Command file. */
/*     DELIM      I   Symbol delimiting the end of a command. */
/*     COMMAND    O   Command read from FILE. */
/*     ERROR      O   Error flag. */
/*     LEVEL      O   A list of all files currently open. */

/* $ Detailed_Input */

/*     FILE       is the name of a file from which a sequence of commands */
/*                is to be read. These commands may include commands to */
/*                read from other files. */

/*     DELIM      is the character which delimits the end of each */
/*                instruction in FILE. */

/* $ Detailed_Output */

/*     COMMAND    is a command read from the current file. */
/*                If no files are currently open, COMMAND = DELIM. */

/*     ERROR      is a descriptive error message, which is blank when */
/*                no error occurs. */

/*     LEVEL      is a list of the files currently open, in the order */
/*                in which they were opened. It is provided for trace- */
/*                back purposes. */

/* $ Detailed_Description */

/*     PRCOMF opens, reads, and closes sets of (possibly nested) */
/*     command files. For example, consider the following command */
/*     files. */

/*        FILE_A : A1             FILE_B : B1               FILE_C : C1 */
/*                 A2                      START FILE_C              C2 */
/*                 A3                      B2                        C3 */
/*                 START FILE_B            B3 */
/*                 A4                      B4 */
/*                 A5 */

/*     If the command 'START FILE_A' were issued, we would expect the */
/*     following sequence of commands to ensue: */

/*        A1, A2, A3, B1, C1, C2, C3, B2, B3, B4, A4, A5. */

/*     The first file immediately becomes, ipso facto, the current file. */
/*     Subsequently, instructions are read from the current file until */
/*     either a START or the end of the file is encountered. Each time */
/*     a new START is encountered, the current file (that is, the */
/*     location of the next command in the file) is placed on a stack, */
/*     and the first command is read from the new file (which then */
/*     becomes the current file). Each time the end of the current file */
/*     is encountered, the previous file is popped off the top of the */
/*     stack to become the current file. This continues until there are */
/*     no files remaining on the stack. */

/*     On occasion, the user may wish to exit from a file without */
/*     reading the rest of the file. In this case, the previous file */
/*     is popped off the stack without further ado. */

/*     Also, the user may wish to abruptly stop an entire nested */
/*     set of files. In this case, all of the files are popped off */
/*     the stack, and no further commands are returned. */

/*     PRCOMF and its entry points may be used to process any such */
/*     set of files. These entry points are: */

/*        - PRCLR ( ERROR ) */

/*          This clears the stack. It may thus be used to implement */
/*          a STOP command. In any case, it must be called before */
/*          any of the other entry points are called. */

/*        - PRSTRT ( FILE, ERROR ) */

/*          This introduces a new file, causing the current file (if */
/*          any) to be placed on the stack, and replacing it with FILE. */
/*          It may thus be used to implement a START command. */

/*          If the file cannot be opened, or the stack is already */
/*          full (it can hold up to seven files), ERROR will contain */
/*          a descriptive error message upon return. Otherwise, it */
/*          will be blank. */

/*        - PRREAD ( COMMAND ) */

/*          This causes the next command to be read from the current */
/*          file. If the end of the current file is reached, the */
/*          previous file is popped off the stack, and the next command */
/*          from this file is read instead. (If no files remain to be */
/*          read, DELIM is returned.) */

/*        - PREXIT */

/*          This causes the previous file to be popped off the top of */
/*          the stack to replace the current file. It may thus be used */
/*          to implement an EXIT command. */

/*        - PRTRCE ( LEVEL ) */

/*          Should an error occur during the execution of a nested */
/*          file, it may be helpful to know the sequence in which */
/*          the nested files were invoked. PRTRCE returns a list of */
/*          the files currently open, in the order in which they were */
/*          invoked. */

/* $ Input_Files */

/*     All files read by PRCOMF are opened with logical units */
/*     determined at run time. */

/* $ Output_Files */

/*     None. */

/* $ Input_Common */

/*     None. */

/* $ Output_Common */

/*     None. */

/* $ Examples */

/*     See Detailed_Description. */

/* $ Restrictions */

/*     The declared length of ERROR should be at least 80, to avoid */
/*     truncationof error messages. */

/* $ Author_and_Institution */

/*     W. L. Taber     (JPL) */
/*     I. M. Underwood (JPL) */

/* $ Version */

/* -     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 4, 1994 */


/*     Version 1, 6-SEP-1986 */

/* -& */

/*   OPTLIB functions */


/*     Local variables */


/*     NFILES is the maximum number of files that may be open at */
/*     any given time. THus, nesting of procedures is limited to */
/*     a depth of NFILES. */


/*     NEST is the number of files currently open. */


/*     FILES are the names of the files on the stack. UNITS are */
/*     the logical units to which they are connected. */

    switch(n__) {
	case 1: goto L_prclr;
	case 2: goto L_prstrt;
	case 3: goto L_prread;
	case 4: goto L_prexit;
	case 5: goto L_prtrce;
	}

    return 0;

/* $ Procedure PRCLR */


L_prclr:

/* $ Abstract */

/*     Clear the file stack. */

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

/* $ Brief_I/O */

/*     None. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     None. */

/* $ Input_Files */

/*     None. */

/* $ Output_Files */

/*     None. */

/* $ Input_Output_Common */

/*     None. */

/* $ Detailed_Description */

/*     Pop all the files off the stack. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */
/* $ Version */

/* -     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 4, 1994 */

/* - */
    while(nest > 0) {
	cl__1.cerr = 0;
	cl__1.cunit = units[(i__1 = nest - 1) < 8 && 0 <= i__1 ? i__1 : 
		s_rnge("units", i__1, "prcomf_", (ftnlen)326)];
	cl__1.csta = 0;
	f_clos(&cl__1);
	--nest;
    }
    return 0;

/* $ Procedure PRSTRT */


L_prstrt:

/* $ Abstract */

/*     Put the current file on the stack, and replace it with FILE. */

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

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  --------------------------------------------------- */
/*     FILE       I   New command file. */
/*     ERROR      O   Error flag. */

/* $ Detailed_Input */

/*     FILE       is the new current file from which commands are */
/*                to be read. */

/* $ Detailed_Output */

/*     ERROR      is blank when no error occurs, and otherwise contains */
/*                a descriptive message. Possible errors are: */

/*                     - The stack is full. */

/*                     - FILE could not be opened. */

/* $ Input_Files */

/*     FILE is opened with a logical unit determined at run time. */

/* $ Output_Files */

/*     None. */

/* $ Input_Output_Common */

/*     None. */

/* $ Detailed_Description */

/*     If the stack is full, return an error. Otherwise, try to open */
/*     FILE. If an error occurs, return immediately. Otherwise, put */
/*     the current file on the stack, and increase the nesting level. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Version */

/* -     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 4, 1994 */

/* - */

/*     No error yet. */

    s_copy(error, " ", error_len, (ftnlen)1);

/*     Proceed only if the stack is not full. */

    if (nest == 8) {
	s_copy(error, "PRSTRT: Command files are nested too deeply.", 
		error_len, (ftnlen)44);
	return 0;
    } else {
	++nest;
    }

/*     Get a new logical unit. If none are available, abort. */

    txtopr_(file, &units[(i__1 = nest - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge(
	    "units", i__1, "prcomf_", (ftnlen)445)], file_len);
    if (have_(error, error_len)) {
	--nest;
    } else {
	s_copy(files + ((i__1 = nest - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge(
		"files", i__1, "prcomf_", (ftnlen)450)) * 80, file, (ftnlen)
		80, file_len);
    }
    return 0;

/* $ Procedure PRREAD */


L_prread:

/* $ Abstract */

/*     Read the next command from the current file. */

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

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  --------------------------------------------------- */
/*     DELIM      I   Character delimiting the end of a command. */
/*     COMMAND    O   Next command from the current file. */

/* $ Detailed_Input */

/*     DELIM      is the character used to delimit the end of a */
/*                command within a command file. */

/* $ Detailed_Output */

/*     COMMAND    is the next command read from the current file. */
/*                If there is no current file, COMMND = DELIM. */

/* $ Input_Files */

/*     All files read by PRCOMF are opened with logical units */
/*     determined at run time. */

/* $ Output_Files */

/*     None. */

/* $ Input_Output_Common */

/*     None. */

/* $ Detailed_Description */

/*     Attempt to read the next statement from the current file. */
/*     If the end of the file is encountered, pop the previous file */
/*     off the top of the stack, and try to read from it. Keep this */
/*     up until a command is read, or until no files remain open. */


/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Version */

/* -     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 4, 1994 */

/* - */

/*     Don't even bother unless at least one file is open. */

    if (nest == 0) {
	s_copy(command, delim, command_len, (ftnlen)1);
	return 0;
    }

/*     Keep trying to read until we run out of files. */

    ci__1.cierr = 1;
    ci__1.ciend = 1;
    ci__1.ciunit = units[(i__1 = nest - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge(
	    "units", i__1, "prcomf_", (ftnlen)558)];
    ci__1.cifmt = "(A)";
    iostat = s_rsfe(&ci__1);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = do_fio(&c__1, command, command_len);
    if (iostat != 0) {
	goto L100001;
    }
    iostat = e_rsfe();
L100001:
    while(iostat != 0 && nest > 0) {
	cl__1.cerr = 0;
	cl__1.cunit = units[(i__1 = nest - 1) < 8 && 0 <= i__1 ? i__1 : 
		s_rnge("units", i__1, "prcomf_", (ftnlen)562)];
	cl__1.csta = 0;
	f_clos(&cl__1);
	--nest;
	if (nest >= 1) {
	    ci__1.cierr = 1;
	    ci__1.ciend = 1;
	    ci__1.ciunit = units[(i__1 = nest - 1) < 8 && 0 <= i__1 ? i__1 : 
		    s_rnge("units", i__1, "prcomf_", (ftnlen)566)];
	    ci__1.cifmt = "(A)";
	    iostat = s_rsfe(&ci__1);
	    if (iostat != 0) {
		goto L100002;
	    }
	    iostat = do_fio(&c__1, command, command_len);
	    if (iostat != 0) {
		goto L100002;
	    }
	    iostat = e_rsfe();
L100002:
	    ;
	}
    }
    rstbuf_();
    if (nest == 0) {
	s_copy(command, delim, command_len, (ftnlen)1);
	putbuf_(command, command_len);
	return 0;
    }
    putbuf_(command, command_len);

/*     Okay, we have something. Keep reading until DELIM is found. */
/*     (Or until the file ends.) Add each successive line read to */
/*     the end of COMMAND. Do not return the delimiter itself. */

    j = 1;
    i__ = i_indx(command, delim, command_len, (ftnlen)1);
    while(i__ == 0 && iostat == 0) {
	j = lastnb_(command, command_len) + 1;
	*(unsigned char *)&command[j - 1] = ' ';
	++j;
	ci__1.cierr = 1;
	ci__1.ciend = 1;
	ci__1.ciunit = units[(i__1 = nest - 1) < 8 && 0 <= i__1 ? i__1 : 
		s_rnge("units", i__1, "prcomf_", (ftnlen)597)];
	ci__1.cifmt = "(A)";
	iostat = s_rsfe(&ci__1);
	if (iostat != 0) {
	    goto L100003;
	}
	iostat = do_fio(&c__1, command + (j - 1), command_len - (j - 1));
	if (iostat != 0) {
	    goto L100003;
	}
	iostat = e_rsfe();
L100003:
	putbuf_(command + (j - 1), command_len - (j - 1));
	i__ = i_indx(command, delim, command_len, (ftnlen)1);
    }
    if (i__ > 0) {
	s_copy(command + (i__ - 1), " ", command_len - (i__ - 1), (ftnlen)1);
    }
    return 0;

/* $ Procedure PREXIT */


L_prexit:

/* $ Abstract */

/*     Replace the current file with the one at the top of the stack. */

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

/* $ Brief_I/O */

/*     None. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     None. */

/* $ Input_Files */

/*     None. */

/* $ Output_Files */

/*     None. */

/* $ Input_Output_Common */

/*     None. */

/* $ Detailed_Description */

/*     Close the current file. Pop the previous file off the top of */
/*     the stack. If there is no current file, of if there are no */
/*     files on the stack, that's cool too. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */
/* $ Version */

/* -     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 4, 1994 */

/* - */
    if (nest > 0) {
	cl__1.cerr = 0;
	cl__1.cunit = units[(i__1 = nest - 1) < 8 && 0 <= i__1 ? i__1 : 
		s_rnge("units", i__1, "prcomf_", (ftnlen)695)];
	cl__1.csta = 0;
	f_clos(&cl__1);
	--nest;
    }
    return 0;

/* $ Procedure PRTRCE */


L_prtrce:

/* $ Abstract */

/*     Provide a list of the files currently open, in the order in */
/*     which they were opened. */

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

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  --------------------------------------------------- */
/*     LEVEL      O   List of all files currently open. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     LEVEL      A list of all files that are currently open, in */
/*                the order in which they were opened. For example, */
/*                if FILE_A starts FILE_B, and FILE_B starts FILE_C, */
/*                LEVEL would be 'FILE_A:FILE_B:_FILE_C'. */

/* $ Input_Files */

/*     None. */

/* $ Output_Files */

/*     None. */

/* $ Input_Output_Common */

/*     None. */

/* $ Detailed_Description */

/*     Just step through the stack, Jack. */

/* $ Examples */

/*     See Detailed_Description. */

/* $ Restrictions */

/*     LEVEL should be declared to be at least CHARACTER*640 by the */
/*     calling program to ensure that enough space is available to */
/*     list all open files. */
/* $ Version */

/* -     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 4, 1994 */

/* - */

/*     Not much to explain. Use LBUILD to build a list, delimited */
/*     by colons. */

    s_copy(level, " ", level_len, (ftnlen)1);
    if (nest > 0) {
	lbuild_(files, &nest, ":", level, (ftnlen)80, (ftnlen)1, level_len);
    }
    return 0;
} /* prcomf_ */

/* Subroutine */ int prcomf_(char *file, char *delim, char *command, char *
	error, char *level, ftnlen file_len, ftnlen delim_len, ftnlen 
	command_len, ftnlen error_len, ftnlen level_len)
{
    return prcomf_0_(0, file, delim, command, error, level, file_len, 
	    delim_len, command_len, error_len, level_len);
    }

/* Subroutine */ int prclr_(void)
{
    return prcomf_0_(1, (char *)0, (char *)0, (char *)0, (char *)0, (char *)0,
	     (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int prstrt_(char *file, char *error, ftnlen file_len, ftnlen 
	error_len)
{
    return prcomf_0_(2, file, (char *)0, (char *)0, error, (char *)0, 
	    file_len, (ftnint)0, (ftnint)0, error_len, (ftnint)0);
    }

/* Subroutine */ int prread_(char *delim, char *command, ftnlen delim_len, 
	ftnlen command_len)
{
    return prcomf_0_(3, (char *)0, delim, command, (char *)0, (char *)0, (
	    ftnint)0, delim_len, command_len, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int prexit_(void)
{
    return prcomf_0_(4, (char *)0, (char *)0, (char *)0, (char *)0, (char *)0,
	     (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int prtrce_(char *level, ftnlen level_len)
{
    return prcomf_0_(5, (char *)0, (char *)0, (char *)0, (char *)0, level, (
	    ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, level_len);
    }

