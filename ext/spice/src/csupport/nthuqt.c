/* nthuqt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      NTHUQT ( N'th unquoted token ) */
/* Subroutine */ int nthuqt_(char *string, integer *n, char *equote, char *
	word, integer *loc, ftnlen string_len, ftnlen equote_len, ftnlen 
	word_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer last, b, e, i__, l;
    logical odddq, oddsp, oddsq;
    extern integer rtrim_(char *, ftnlen);
    integer spcial, dquote;
    logical inword;
    integer wcount, squote;

/* $ Abstract */

/*    This routine finds the N'th non-quoted token in a string. */
/*    Quoted substrings are ignored and not treated as */
/*    blanks. */

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

/*     STRING */
/*     WORD */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*      STRING     I   Input character string. */
/*      N          I   Index of the token to be returned. */
/*      EQUOTE     I   An extra quote character. */
/*      WORD       O   The N'TH unquoted token in STRING. */
/*      LOC        O   Location of WORD in STRING. */

/* $ Detailed_Input */

/*      STRING     is the input string to be parsed. It contains */
/*                 some number of token, where a token is any string */
/*                 of consecutive non-blank, non-comma characters */
/*                 not between balanced quotes. */

/*      N          is the index of the token to be returned. (One for */
/*                 the first token, two for the second, and so on.) */

/*      EQUOTE     is a special character that users may supply so */
/*                 that specially marked strings will be skipped */
/*                 in the selection of tokens.  If you do not want */
/*                 any specially marked strings use a blank for EQUOTE */

/* $ Detailed_Output */

/*      WORD       is the N'th token in STRING. If STRING is blank, */
/*                 or NTH is nonpositive or too large, WORD is blank. */

/*      LOC        is the location of WORD in STRING. (That is, WORD */
/*                 begins at STRING(LOC:LOC). If STRING is blank, or */
/*                 NTH is nonpositive or too large, LOC is zero. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1) */

/* $ Particulars */

/*      NTHUQT, like NTHWD, is useful primarily for parsing input */
/*      commands consisting of one or more tokens, where a token is */
/*      defined to be any sequence of consecutive non-blank characters. */

/*      The chief difference between the two routines is that */
/*      NTHUQT treats all character starting at and through */
/*      a balanced quote as blanks.  Both " and ' are treated as */
/*      quote characters. */

/* $ Examples */


/*      Let STRING be ' He said, "Now is the time" and left. ' */
/*                     1234567890123456789012345678901234567 */

/*      If N = -1   WORD = ' '          LOC =  0 */
/*              0          ' '                 0 */
/*              1,         'He'                2 */
/*              2,         'said'              5 */
/*              3,         'and'              29 */
/*              4,         'left.'            33 */
/*              5,         ' '                 0 */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*       W.L. Taber      (JPL) */

/* $ Literature_References */

/*       None. */

/* $ Version */

/* -    Inspekt Version 1.0.0, 14-JUL-1995 (WLT) */


/* -& */
/* $ Index_Entries */

/*     Find the n'th unquoted token in a string */

/* -& */

/*     Spice Functions */


/*     Local Variables */


/*     An integer */


/*     Take care of the dumb cases first. */

    if (*n <= 0) {
	s_copy(word, " ", word_len, (ftnlen)1);
	*loc = 0;
	return 0;
    }
    squote = '\'';
    dquote = '"';
    spcial = *(unsigned char *)equote;
    if (spcial == ' ') {
	spcial = squote;
    }
    last = rtrim_(string, string_len);
    wcount = 0;
    odddq = FALSE_;
    oddsq = FALSE_;
    oddsp = FALSE_;
    inword = FALSE_;
    i__1 = last;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Get the integer value of the I'th character of string. */

	l = *(unsigned char *)&string[i__ - 1];

/*        If this is a quote character, then flip the ODDQ logical */

	if (l == spcial) {
	    oddsp = ! oddsp;
	}
	if (l == squote) {
	    oddsq = ! oddsq;
	}
	if (l == dquote) {
	    odddq = ! odddq;
	}

/*        If this is a blank ... */

	if (l == ' ' || l == ',' || odddq || oddsq || oddsp || (l == squote ||
		 l == dquote || l == spcial)) {

/*           if we are in the middle of a word, we are about to */
/*           end it.  If the word counter WCOUNT has the same */
/*           value of N then we've found the N'th unquoted word. */
/*           Set the various outputs and return. */

	    if (inword && wcount == *n) {
		s_copy(word, string + (b - 1), word_len, e - (b - 1));
		*loc = b;
		return 0;
	    }

/*           If we get to here, we just point out that we are */
/*           not in a word. */

	    inword = FALSE_;
	} else {

/*           If this is not a "blank"  then ODDDQ, ODDSQ and ODDSP are */
/*           false so we are not inside a quoted string.  We are either */
/*           already in a word, or we are just starting one. */

	    if (inword) {

/*              We are in a word, just bump the end of this one. */

		e = i__;
	    } else {

/*              We are beginning a word. Up the word counter, */
/*              set the end and beginning of the word. */

		inword = TRUE_;
		++wcount;
		b = i__;
		e = i__;
	    }
	}

/*        Examine the next character. */

    }
    if (inword && wcount == *n) {
	*loc = b;
	s_copy(word, string + (b - 1), word_len, string_len - (b - 1));
    } else {
	*loc = 0;
	s_copy(word, " ", word_len, (ftnlen)1);
    }
    return 0;
} /* nthuqt_ */

