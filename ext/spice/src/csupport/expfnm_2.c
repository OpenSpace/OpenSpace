/* expfnm_2.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__2 = 2;
static integer c__32 = 32;

/* $Procedure      EXPFNM_2 ( Expand a filename ) */
/* Subroutine */ int expfnm_2__(char *instr, char *outfil, ftnlen instr_len, 
	ftnlen outfil_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer i_len(char *, ftnlen);

    /* Local variables */
    integer need;
    extern /* Subroutine */ int zzgetenv_(char *, char *, ftnlen, ftnlen);
    integer blank;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    integer inlen, slash;
    char myfil[255], myval[255];
    extern integer rtrim_(char *, ftnlen);
    char myenv[32];
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    integer dollar, vallen, varlen;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), repsub_(char *, integer *, integer *, char *, char *, 
	    ftnlen, ftnlen, ftnlen), setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    integer outlen;
    extern logical return_(void);
    extern integer pos_(char *, char *, integer *, ftnlen, ftnlen);

/* $ Abstract */

/*     Given a character string that represents a filename, expand it */
/*     using a predefined environment variable or DCL symbol to a */
/*     complete path or to prepend path components to a partial filename. */

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

/* $ Declarations */
/*     Length of an environment variable or DCL symbol name. */
/*     Length of a filename. */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     INSTR      I   The character string to expand into a filename. */
/*     OUTFIL     O   The expanded filename. */
/*     ENVLEN     P   Maximum length of an environemt variable or symbol. */
/*     FNMLEN     P   Maximum length of a filename. */

/* $ Detailed_Input */

/*     INSTR      is the character string to be expanded. */

/*                The input character string must be either */

/*                   1) A defined environment variable having a value */
/*                      that is a complete path to a file. */

/*                   2) A defined environment variable, representing the */
/*                      leading directories in a complete path to a file, */
/*                      followed by a slash, '/', followed by the */
/*                      remainder of the complete path to a file, e.g., */

/*                         <environment variable>/mydir1/mydir2/file.dat */

/*                      where the environment variable must begin with a */
/*                      dollar sign ($). */

/*                   3) A complete filename, which will not be modified. */

/* $ Detailed_Output */

/*     OUTFIL     is the expanded filename. If no expansion could be */
/*                done, OUTFIL will be blank. OUTFIL may not overwrite */
/*                INSTR. */

/* $ Parameters */

/*     ENVLEN   The maximum allowed length of an environment variable */
/*              or DCL symbol name. */

/*     FNMLEN   The maximum length for a filename. */

/* $ Exceptions */

/*     1) If the input string is blank, or has embedded blanks in it, */
/*        the error SPICE(BADFILENAME) is signalled. */

/*     2) If the expanded filename is too long to fit into the */
/*        output string, the error SPICE(STRINGTOOSMALL) is signalled. */

/*     3) The output string may not overwrite the input string. */

/*     4) If no expansion of the input string can be done, the */
/*        output filename is will be blank. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This subroutine takes as input a character string, possibly */
/*     containing an environment variable or DCL symbol name, that */
/*     represents a filename. If the character string contains an */
/*     environment variable or DCL symbol name, indicated by a dollar */
/*     sign ($) immediately preceeding the environment variable or DCL */
/*     symbol name, an attempt is made to obtain a value for the */
/*     specified environment variable or DCL symbol from the operating */
/*     system. If there is no dollar sign in the input character string, */
/*     the output filename will be assigned the value of the input */
/*     character string. */

/*     If successful, the original environment variable or DCL symbol */
/*     name, including the dollar sign, will be replaced with the value */
/*     that was obtained, and the resulting character string will be */
/*     returned as the output filename. If unsuccesful, the the output */
/*     filename will be blank. */

/*     Environment variable and DCL symbol names may only be used to */
/*     represent either a complete path to a file or the leading path */
/*     elements of a complete path to a file. Thus, they must appear */
/*     first in the input character string. See the examples. */

/* $ Examples */

/*     We provide examples using a UNIX style filename and path. For */
/*     other environments, the appropriate syntax for filenames and */
/*     paths must be used. */

/*     Example 1: Passing in a complete path to a filename. */

/*        INSTR  = 'datafile.dat' */
/*        OUTFIL = 'datafile.dat' */

/*     Example 2: Using an environment variable to specify the complete */
/*                path to a filename. */

/*        Assume that we have already defined the environment variable */
/*        or DCL symbol 'DATAFILE' to be 'datafile.dat'. Then we would */
/*        get the following: */

/*           INSTR  = '$DATAFILE' */
/*           OUTFIL = 'datafile.dat' */

/*     Example 3: Using an environment variable to specify the leading */
/*                path elements of a complete path to a filename. */

/*        Assume that we have already defined the environment variable */
/*        or DCL symbol 'DATAPATH' to be '/home/project/data'. Then we */
/*        would get the following: */

/*           INSTR  = '$DATAFILE/datafila.dat' */
/*           OUTFIL = '/home/project/data/datafile.dat' */

/*     Example 4: An incorrect usage of an environment variable. */

/*        Using '/home/$DATAPATH/datafile.dat' as the input string */
/*        would produce an error because the dollar sign is not the */
/*        first nonblank character in the input string. in this case, */
/*        OUTFIL would be blank. */

/* $ Restrictions */

/*     1) This subroutine expects environment variable and DCL symbol */
/*        names to begin with a dollar sign ($). Failure to do this */
/*        could lead to unexpected results. */

/*     2) The environment variable or DCL sumbol name must be the first */
/*        part of the input character string. */

/*     3) Environment variable and DCL symbol names may be at most 32 */
/*        characters in length. Your Mileage may vary depending on the */
/*        particular environment. See the private subroutine ZZGETENV */
/*        for details. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     K.R. Gehringer (JPL) */
/*     H.A. Neilan    (JPL) */

/* $ Version */

/* -    Beta Version 2.0.0, 20-JAN-1999 (NJB) */

/*        No longer converts environment variables to upper case. */

/* -    Beta Version 1.0.0, 30-MAY-1996 (HAN) */

/*        This version fixes some inconsistencies in the original */
/*        EXPFNM_1 subroutine. */

/* -& */

/* $ Index_Entry */

/*     expand a filename */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("EXPFNM_2", (ftnlen)8);
    }

/*     If the input filename is blank, that's an error. */

    if (s_cmp(instr, " ", instr_len, (ftnlen)1) == 0) {
	s_copy(outfil, " ", outfil_len, (ftnlen)1);
	setmsg_("The input filename '#' was blank.", (ftnlen)33);
	errch_("#", instr, (ftnlen)1, instr_len);
	sigerr_("SPICE(BADFILENAME)", (ftnlen)18);
	chkout_("EXPFNM_2", (ftnlen)8);
	return 0;
    }

/*     We know the input was not blank, so left justify it and */
/*     check for embedded blanks. */

    ljust_(instr, myfil, instr_len, (ftnlen)255);
    blank = pos_(myfil, " ", &c__1, rtrim_(myfil, (ftnlen)255), (ftnlen)1);
    if (blank != 0) {
	s_copy(outfil, " ", outfil_len, (ftnlen)1);
	setmsg_("The input filename '#' contained embedded blanks.", (ftnlen)
		49);
	errch_("#", myfil, (ftnlen)1, (ftnlen)255);
	sigerr_("SPICE(BADFILENAME)", (ftnlen)18);
	chkout_("EXPFNM_2", (ftnlen)8);
	return 0;
    }

/*     We have two cases that we need to consider: */

/*        1) The input file does not contain a dollar sign. This */
/*           indicates that it is a complete filename; */

/*        2) The input file has a dollar sign as the first character. */
/*           This indicates that the input filename has its full name, */
/*           or leading path components, specified by the value of an */
/*           environment variable. In this case, we get the environment */
/*           variable's value and replace the environment variable in */
/*           the input filename. */

/*     We deal with each of these cases, in order, below. */

    dollar = pos_(myfil, "$", &c__1, (ftnlen)255, (ftnlen)1);
    if (dollar == 0) {

/*        The input is assumed to be an actual filename, so set the */
/*        output to be the input. */

	s_copy(outfil, instr, outfil_len, instr_len);
    } else if (dollar == 1) {

/*        The input is assumed to contain the name of an environment */
/*        variable whose value contains a complete path name to a */
/*        file or the leading path elements that will create a complete */
/*        path name to a file. To find out which, we look for a forward */
/*        slash. If there is one, everything between the dollar sign and */
/*        the first forward slash, noninclusive, is the name of the */
/*        environment variable. If there are no slashes, the entire */
/*        input name is the name of the environment variable. */

	slash = pos_(myfil, "/", &c__2, (ftnlen)255, (ftnlen)1);
	if (slash == 0) {
	    varlen = rtrim_(myfil, (ftnlen)255);
	} else {
	    varlen = slash - 1;
	}
	if (varlen > 32) {
	    s_copy(outfil, " ", outfil_len, (ftnlen)1);
	    setmsg_("The environment variable name '#' is too long. The maxi"
		    "mum length for an environment variable name is #.", (
		    ftnlen)104);
	    errch_("#", myfil + 1, (ftnlen)1, slash - 2);
	    errint_("#", &c__32, (ftnlen)1);
	    sigerr_("SPICE(STRINGTOOSMALL)", (ftnlen)21);
	    chkout_("EXPFNM_2", (ftnlen)8);
	    return 0;
	}

/*        Remember to skip the dollar sign. */

	s_copy(myenv, myfil + 1, (ftnlen)32, varlen - 1);

/*        Try to get the value of the environment variable. If the */
/*        environment variable does not exist, a blank string is */
/*        returned. */

	zzgetenv_(myenv, myval, (ftnlen)32, (ftnlen)255);

/*        If we got something, use it. We don't make any value */
/*        judgements. */

	if (s_cmp(myval, " ", (ftnlen)255, (ftnlen)1) == 0) {
	    s_copy(outfil, " ", outfil_len, (ftnlen)1);
	    setmsg_("The environment variable '#' was not defined.", (ftnlen)
		    45);
	    errch_("#", myenv, (ftnlen)1, (ftnlen)32);
	    sigerr_("SPICE(NOENVVARIABLE)", (ftnlen)20);
	    chkout_("EXPFNM_2", (ftnlen)8);
	    return 0;
	}
	inlen = rtrim_(myfil + 1, (ftnlen)254);
	vallen = rtrim_(myval, (ftnlen)255);
	outlen = i_len(outfil, outfil_len);
	need = inlen - varlen + vallen;

/*        If the output filename length is not long enough for */
/*        the substitution, signal an error. Otherwise, substitute */
/*        in the new value. */

	if (need > outlen) {
	    s_copy(outfil, " ", outfil_len, (ftnlen)1);
	    setmsg_("The expanded filename for the input filename '#' exceed"
		    "ed the length of the output filename. The expanded name "
		    "was # characters too long.", (ftnlen)137);
	    errch_("#", myfil, (ftnlen)1, (ftnlen)255);
	    i__1 = need - outlen;
	    errint_("#", &i__1, (ftnlen)1);
	    sigerr_("SPICE(STRINGTOOSMALL)", (ftnlen)21);
	    chkout_("EXPFNM_2", (ftnlen)8);
	    return 0;
	}
	repsub_(myfil, &c__1, &varlen, myval, outfil, (ftnlen)255, vallen, 
		outfil_len);
    } else {

/*        There was a dollar sign in a position other than the first */
/*        nonblank position of the input filename. We do not allow */
/*        this. If an input filename contains a dollar sign, it must */
/*        be in the first nonblank position. */

	s_copy(outfil, " ", outfil_len, (ftnlen)1);
	setmsg_("The input filename '#' contained a dollar sign ($) that was"
		" not in the first nonblank position; this is not allowed. Se"
		"e the subroutine EXPFNM_2 for details.", (ftnlen)157);
	errch_("#", myfil, (ftnlen)1, (ftnlen)255);
	sigerr_("SPICE(BADFILENAME)", (ftnlen)18);
	chkout_("EXPFNM_2", (ftnlen)8);
	return 0;
    }
    chkout_("EXPFNM_2", (ftnlen)8);
    return 0;
} /* expfnm_2__ */

