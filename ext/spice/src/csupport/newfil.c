/* newfil.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      NEWFIL ( Open a new file on the specified port ) */
/* Subroutine */ int newfil_(char *pattrn, char *port, char *file, ftnlen 
	pattrn_len, ftnlen port_len, ftnlen file_len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char name__[128];
    logical more;
    char this__[128], fname[128];
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen), reset_(void);
    extern logical failed_(void);
    integer badopn;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), nspopn_(char *, char *, ftnlen, 
	    ftnlen);
    extern logical exists_(char *, ftnlen);
    extern /* Subroutine */ int fststr_(char *, char *, ftnlen, ftnlen), 
	    nxtstr_(char *, char *, char *, ftnlen, ftnlen, ftnlen);

/* $ Abstract */

/*     This routine opens a port with a file that is created from */
/*     the input PATTRN and returns the name of the FILE attached */
/*     to the port. */

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

/*      None. */

/* $ Keywords */

/*       FILES */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      PATTRN     I   is a name pattern following the rules of MAKSTR */
/*      PORT       I   the port to which the FILE should be attached. */
/*      FILE       O   the name of the file attached to the port. */

/* $ Detailed_Input */

/*     PATTRN      The description below is lifted without change */
/*                 from the routine MAKSTR. */

/*                 PATTRN is a string that specifies a pattern that */
/*                 all strings in a sequence must match. There are */
/*                 several special substrings in PATTRN that must */
/*                 be recognized. */

/*                 1) A substring of the form '<*>' (where * is used */
/*                    as a variable length wildcard character) is called */
/*                    an expansion. The substring that occurs between */
/*                    the angle brackets < > is called the invisible */
/*                    portion of the expansion.  When the tokens of */
/*                    PATTRN are counted the invisible portion of the */
/*                    expansion is not counted.  Thus an expansion has */
/*                    exactly two tokens '<' and '>'  The invisible */
/*                    portion of the expansion must not contain */
/*                    any of the characters '<', '>', '{', or '}'. */

/*                 2) A substring of the form '{#-$}' where # and $ */
/*                    stand for any chacter from the set */
/*                    '0', ... , '9', 'a', ... , 'z' is called a */
/*                    restriction. */

/*                 A pattern may consist of any collection of */
/*                 characters.  However, the characters '<' and */
/*                 '>' must always occur in balanced pairs with '<' */
/*                 on the left and '>' on the right. Moreover, they */
/*                 cannot be nested even if they are balanced. Similary */
/*                 '{' and '}' must always appear as a balanced pair */
/*                 and have exactly 3 characters between them.  The */
/*                 first is a lower case letter or a digit.  The second */
/*                 letter may be anything (usually a hyphen, colon or */
/*                 comma).  The third character must */
/*                 also be a letter between 0, ... ,9, a, b, ... , z */
/*                 and must occur later in the collating sequence than */
/*                 the first letter in the triple that occurs between */
/*                 '{' and '}'. */

/*                 For example the following are valid patterns */

/*                 PAT_<Value: >_{0-9}{a-z}{a-d} */
/*                 COUNTER{0-9}{0-9}{0-9}{0-9} */
/*                 COUNTER{0:9}{0,9}{a;b} */

/*                 but the following are not */

/*                 PAT_<<>>_{0-9}{a-z}{a-d}    --- Nested < > */
/*                 COUNTER{9-0}                --- 9 before 0 */
/*                 PAT_{0to0}                  --- 4 characters between{} */
/*                 PAT_{A-Z}                   --- uppercase letters in{} */
/*                 PAT_{+-$}                   --- bad characters in {} */

/*                 Pattern should be viewed as consisting of a sequence */
/*                 of tokens.  The tokens consist of characters that */
/*                 are not part of an expansion or restriction */
/*                 restrictions and the '<' and '>' characters of */
/*                 any expansion. */

/* $ Detailed_Output */

/*     PORT        is the name of an NSPIO port that will be opened */
/*                 with the file name generated from PATTRN. */

/*     FILE        is a string that is the name of the file that is */
/*                 open and attached to the specified PORT.  The */
/*                 name of the file will match the input PATTRN */
/*                 and will be the first name generated from PATTRN */
/*                 that can be opened.  See the routine MAKSTR for */
/*                 a more detailed explanation of the names */
/*                 that are generated using FSTSTR and NXTSTR. */

/* $ Parameters */

/*      None. */

/* $ Files */

/*      None. */

/* $ Exceptions */

/*     1) If the file cannot be opened, the error */
/*        CMLOOP(CANNOTMAKEFILE) will be signalled. */

/* $ Particulars */

/*     This is a utility routine for creating a file name that */
/*     can be opened without fear of name collisions and attached */
/*     to one of the file ports supported by NSPIO.  In this way */
/*     you have a high likelyhood of success in opening a log file */
/*     or utility file for use by your program (this assumes that */
/*     you have adequate privelege to open a file in the directory */
/*     implied or specified by PATTRN). */

/* $ Examples */

/*     Suppose that you need a utility file for holding some */
/*     temporary data structure in a program that makes use */
/*     of NSPIO for its IO.  Then you could make the following */
/*     call */

/*        PATTRN = 'util{0-9}{0-9}{0-9}{0-9}.tmp' */

/*        CALL NEWFIL ( PATTRN, 'UTILITY', FILE ) */

/*     If successful, FILE will hold the name of the file that */
/*     was opened and is attached to the UTILITY port of NSPIO. */
/*     Otherwise FILE will be returned as a blank and the */
/*     FAILED flag will have been set by the call to SIGERR */
/*     made in this routine. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*      W.L. Taber      (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 4, 1994 */


/* -    SPICELIB Version 1.0.0, 21-APR-1994 (WLT) */


/* -& */
/* $ Index_Entries */

/*     Create a file name and attach it to an I/O port */

/* -& */

/*     Spicelib routines. */

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


/*     Local Parameters */

    chkin_("NEWFIL", (ftnlen)6);
    s_copy(fname, " ", (ftnlen)128, (ftnlen)1);
    s_copy(name__, " ", (ftnlen)128, (ftnlen)1);
    s_copy(this__, " ", (ftnlen)128, (ftnlen)1);
    fststr_(pattrn, fname, pattrn_len, (ftnlen)128);
    s_copy(name__, fname, (ftnlen)128, (ftnlen)128);
    more = TRUE_;
    badopn = 0;
    while(badopn < 20) {

/*        Look for a file name that does not already exist. */

	while(exists_(name__, (ftnlen)128) && more) {
	    s_copy(this__, name__, (ftnlen)128, (ftnlen)128);
	    s_copy(name__, " ", (ftnlen)128, (ftnlen)1);
	    nxtstr_(pattrn, this__, name__, pattrn_len, (ftnlen)128, (ftnlen)
		    128);
	    more = s_cmp(name__, fname, (ftnlen)128, (ftnlen)128) != 0;
	}
	if (! more) {
	    s_copy(file, " ", file_len, (ftnlen)1);
	    setmsg_("It was not possible to create a # file as specified. Al"
		    "l appropriately named files already exist.", (ftnlen)97);
	    errch_("#", port, (ftnlen)1, port_len);
	    sigerr_("CMLOOP(CANNOTMAKEFILE)", (ftnlen)22);
	    chkout_("NEWFIL", (ftnlen)6);
	    return 0;
	} else {
	    s_copy(file, name__, file_len, (ftnlen)128);
	}

/*        Ok.  We've got a good candidate, try to attach it to the */
/*        specified port. */

	nspopn_(port, file, port_len, file_len);
	if (failed_()) {
	    ++badopn;

/*           We will try a few more times on the off chance that */
/*           some other program used the same name first.  This */
/*           is not likely, file protection problems or PATTRN */
/*           specifications are a more probable cause of the trouble, */
/*           but we try anyway. */

	    if (badopn < 20) {
		reset_();
	    }
	} else {

/*           We were successful in opening the port with the */
/*           specified name.  We can quit now. */

	    chkout_("NEWFIL", (ftnlen)6);
	    return 0;
	}
    }

/*     If you get to this point, a file was not succesfully */
/*     attached to PORT.  But NSPIO has already diagnosed */
/*     the problem as much as we're going to.  Just set FILE */
/*     to a blank and return. */

    s_copy(file, " ", file_len, (ftnlen)1);
    chkout_("NEWFIL", (ftnlen)6);
    return 0;
} /* newfil_ */

