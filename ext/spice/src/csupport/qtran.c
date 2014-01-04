/* qtran.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;

/* $Procedure     QTRAN */
/* Subroutine */ int qtran_(char *input, char *output, logical *tran, ftnlen 
	input_len, ftnlen output_len)
{
    /* System generated locals */
    address a__1[3];
    integer i__1[3], i__2, i__3;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen), i_indx(char *, char *, 
	    ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen), s_cat(char *,
	     char **, integer *, integer *, ftnlen);

    /* Local variables */
    integer qlen, rlen, i__;
    char delim[1];
    extern /* Subroutine */ int geteq_(char *, ftnlen);
    char reply[128], query[33];
    extern /* Subroutine */ int getdel_(char *, ftnlen);
    extern integer lastnb_(char *, ftnlen);
    extern /* Subroutine */ int repsub_(char *, integer *, integer *, char *, 
	    char *, ftnlen, ftnlen, ftnlen);
    char equote[1];
    extern /* Subroutine */ int rdstmn_(char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen);
    char prompt[55];
    extern /* Subroutine */ int nthuqw_(char *, integer *, char *, char *, 
	    integer *, ftnlen, ftnlen, ftnlen);
    integer loc;

/* $ Abstract */

/*     Prompt the user to supply values for the first query in a string. */

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

/*     PARSE, QUERY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     INPUT      I   Input string, possibly containing queries. */
/*     OUTPUT     O   Equivalent string, with first query replaced. */
/*     TRAN       O   True when a query was replaced. */

/* $ Detailed_Input */

/*     INPUT      is the input string. This may contain any number */
/*                of queries, for which the user will be expected to */
/*                supply values. A query is any string of up to 32 */
/*                consecutive non-blank characters ending with '?'. */

/* $ Detailed_Output */

/*     OUTPUT     is the equivalent of INPUT after the first of the */
/*                queries in INPUT has been supplied with a value. */

/*                OUTPUT may overwrite INPUT. */

/*      TRAN      is true whenever a query was found and replaced, and is */
/*                false otherwise. */

/* $ Input_Files */

/*     None. */

/* $ Output_Files */

/*     None. */

/* $ Input_Output_Common */

/*     None. */

/* $ Exceptions */

/*     It is possible that query resolution will result in an overflow */
/*     of the output string.  This situation is dianosed by a routine */
/*     called by QTRAN. */

/* $ Detailed_Description */

/*     Look for a query in INPUT. (It will end with '?'.) Ask the user */
/*     to supply a value for the query. Insert the value into OUTPUT in */
/*     place of the query itself. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     I. M. Underwood (JPL) */

/* $ Version_and_Date */

/*     Version 1.1, 14-SEP-1995 */

/*        Assignment to otherwise unused variable SUPRES deleted. */

/*     Version 1,   17-SEP-1986 */

/* -& */

/*     OPTLIB functions */


/*     Local variables */


/*     Look up the special marker used for suppressing query */
/*     evaluation. */

    geteq_(equote, (ftnlen)1);
    getdel_(delim, (ftnlen)1);

/*     Look at each word. If a word ends with '?', it's a query. */
/*     (QUERY is a character longer than a valid query. So any */
/*     valid query will have at least one blank at the end.) */

    *tran = FALSE_;
    i__ = 1;
    nthuqw_(input, &i__, equote, query, &loc, input_len, (ftnlen)1, (ftnlen)
	    33);
    while(! (*tran) && s_cmp(query, " ", (ftnlen)33, (ftnlen)1) != 0) {

/*        First we have to look for the translation supression */
/*        character. */

	*tran = i_indx(query, "? ", (ftnlen)33, (ftnlen)2) > 0 && s_cmp(query,
		 "?", (ftnlen)33, (ftnlen)1) != 0;
	if (! (*tran)) {
	    ++i__;
	    nthuqw_(input, &i__, equote, query, &loc, input_len, (ftnlen)1, (
		    ftnlen)33);
	}
    }
    s_copy(output, input, output_len, input_len);

/*     If we found a query, get the user's response, and insert it */
/*     in place of the query. */

    if (*tran) {
	qlen = lastnb_(query, (ftnlen)33);
/* Writing concatenation */
	i__1[0] = 16, a__1[0] = "Enter value for ";
	i__1[1] = qlen - 1, a__1[1] = query;
	i__1[2] = 3, a__1[2] = " > ";
	s_cat(prompt, a__1, i__1, &c__3, (ftnlen)55);
	rdstmn_(prompt, delim, reply, (ftnlen)55, (ftnlen)1, (ftnlen)128);
/* Computing MAX */
	i__2 = 1, i__3 = lastnb_(reply, (ftnlen)128);
	rlen = max(i__2,i__3);
	i__2 = loc + qlen - 1;
	repsub_(output, &loc, &i__2, reply, output, output_len, rlen, 
		output_len);
    }
    return 0;
} /* qtran_ */

