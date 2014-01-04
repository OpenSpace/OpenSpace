/* rdstmn.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

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

/* Subroutine */ int rdstmn_(char *prmpt, char *delim, char *stmt, ftnlen 
	prmpt_len, ftnlen delim_len, ftnlen stmt_len)
{
    /* Initialized data */

    static char blank[132] = "                                              "
	    "                                                                "
	    "                      ";

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), i_indx(char *, char *, 
	    ftnlen, ftnlen);

    /* Local variables */
    char line[132];
    extern logical batch_(void);
    char space[1];
    integer prlen;
    extern integer rtrim_(char *, ftnlen);
    char myprm[132];
    extern /* Subroutine */ int replch_(char *, char *, char *, char *, 
	    ftnlen, ftnlen, ftnlen, ftnlen), suffix_(char *, integer *, char *
	    , ftnlen, ftnlen), prompt_(char *, char *, ftnlen, ftnlen);
    char tab[1];
    integer end;


/*  Read a statement entered on one or more lines. */

/*  VARIABLE      I/O            DESCRIPTION */
/*   PRMPT        I      Prompt for input. If PRMPT is not blank, */
/*                          the cursor is positioned one space after the */
/*                          last non-blank character. Successive lines */
/*                          are indented by the length of PRMPT. */
/*   DELIM         I      Statement delimiter. RDSTMN will continue */
/*                          to read until the either the delimiter or */
/*                          a blank line is entered. */
/*   STMT          O      The statement entered, up to but not */
/*                          including the delimiter. If RDSTMN is */
/*                          terminated by the entry of a blank line, */
/*                          STMT is blank. */

/* $ Version */

/* -     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 4, 1994 */


/*  7 February 1986, I.M. Underwood */

/* - */

/*     SPICELIB functions */


/*     Local variables */


/*     Read the first statement. Use the prompt. Return immediately */
/*     if a blank line or an error is encountered. */

    if (batch_()) {
	s_copy(stmt, " ", stmt_len, (ftnlen)1);
	return 0;
    }
    prlen = rtrim_(prmpt, prmpt_len) + 1;
    s_copy(myprm, prmpt, (ftnlen)132, prmpt_len);
    s_copy(line, " ", (ftnlen)132, (ftnlen)1);
    *(unsigned char *)space = ' ';
    *(unsigned char *)tab = '\t';
    prompt_(myprm, line, prlen, (ftnlen)132);
    if (s_cmp(line, " ", (ftnlen)132, (ftnlen)1) == 0) {
	s_copy(stmt, " ", stmt_len, (ftnlen)1);
	return 0;
    } else {
	s_copy(stmt, line, stmt_len, (ftnlen)132);
    }

/*     Get rid of any of those nasty old tabs. */

    replch_(line, tab, space, line, (ftnlen)132, (ftnlen)1, (ftnlen)1, (
	    ftnlen)132);

/*     Read succeeding lines. Indent to the length of the original */
/*     prompt. Add the input line to the current statement. Quit when: */

/*            - A delimiter is encountered. (Return the statement */
/*              up to the delimiter.) */

/*            - A blank line or an error is encountered. (Return */
/*              a blank statement.) */

    while(i_indx(stmt, delim, stmt_len, (ftnlen)1) == 0) {
	prompt_(blank, line, prlen, (ftnlen)132);
	replch_(line, tab, space, line, (ftnlen)132, (ftnlen)1, (ftnlen)1, (
		ftnlen)132);
	if (s_cmp(line, " ", (ftnlen)132, (ftnlen)1) == 0) {
	    s_copy(stmt, " ", stmt_len, (ftnlen)1);
	    return 0;
	} else {
	    suffix_(line, &c__1, stmt, (ftnlen)132, stmt_len);
	}
    }

/*     If we made it to here, we encountered a delimiter. Take the */
/*     entire statement up to the character before the delimiter. */

    end = i_indx(stmt, delim, stmt_len, (ftnlen)1);
    s_copy(stmt + (end - 1), " ", stmt_len - (end - 1), (ftnlen)1);
    return 0;
} /* rdstmn_ */

