/* m2begr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      M2BEGR ( See if a word begins with a range template ) */
/* Subroutine */ int m2begr_(char *string, integer *beg, integer *end, 
	integer *a, integer *b, ftnlen string_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    static integer i__;
    static logical digit[256];
    static integer colon;
    static char error[80];
    static integer start, lparen, rparen;
    extern /* Subroutine */ int nparsi_(char *, integer *, char *, integer *, 
	    ftnlen, ftnlen);
    extern integer intmax_(void);
    static integer pointr;

/* $ Abstract */

/*     Determine whether or not the substring STRING(BEG:END) begins */
/*     with a substring of the form (A:B) where A and B are integers. */
/*     If it does, et BEG is set to the index of the first character */
/*     following this substring and the integer values of A and B are */
/*     returned.  Otherwise no action is taken. */

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

/*     META/2 a language specification language. */

/* $ Keywords */

/*     PARSING */
/*     UTILITY */
/*     WORD */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   A META/2 language statement specification. */
/*     BEG       I/0  The beginning of the substring on input and output */
/*     END       I/0  The end of the substring on input and output */
/*     A          O   Lower value of the range template */
/*     B          O   Upper value of the range template */

/* $ Detailed_Input */

/*     STRING(BEG:END)  is a word in the META/2 language.  It potentially */
/*                      begins with a substring of the form (A:B) where */
/*                      A and B are both chracter strings representing */
/*                      integers. */


/* $ Detailed_Output */

/*     BEG        On ouput BEG points to the beginning of the portion of */
/*                the input word that follows the range template (if */
/*                one was present)  Otherwise it remains unchanged. */

/*     END        points to the end of the input META/2 word. */

/*     A          is the value represented by the first numeric string */
/*                of the range template.  If a range template is not */
/*                present, A is not assigned a value. */

/*     B          is the value represented by the second numeric string */
/*                of the range template (if there is a second numeric */
/*                string)  If a range template is present, but no numeric */
/*                string is present B is assigned the value INTMAX(). */


/* $ Error_Handling */

/*     None.  A range template is present or it isn't. */


/* $ Input_Files */

/*     None. */

/* $ Output_Files */

/*     None. */

/* $ Particulars */

/*      The range template is part of the META/2 language and is */
/*      described in the required reading section.  Briefly it is */
/*      a string at the beginning of a word that has the form */

/*      (A:B) */

/*      where A is a string representing a positive integer, and */
/*      B the null string or a string representing a positive integer */
/*      greater than A. */

/*      This routine determines if a range template is present and if so */
/*      what the values of A and B are.  If B is the null string it */
/*      is assumed to represent the largest positive integer. */

/* $ Examples */

/*      Consider the following */

/*      inputs                              outputs */

/*      STRING(BEG:END) BEG    END          BEG   END   A      B */
/*      --------------- ---    --- ---      ---   ---  ---    --- */
/*      (1:2)@number     5     16            10    16   1      2 */
/*      1:2@number       7     16             7    16   x      x */
/*      (-1:23)@word     3     14             3    14   x      x */
/*      @frank           6     11             6    11   x      x */
/*      (4:)@spam(1:2)  54     67            58    67   4     INTMAX() */
/*      @spud(1:12)     10     20            10    20   x      x */

/* $ Restrictions */

/*      None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */

/* $ Version */

/* -     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 9, 1994 */


/* -     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of META/2 */
/*         software as of May 3, 1994 */


/*     Version B1.0.0, 23-MAR-1988 (WLT) (IMU) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */

    if (first) {
	first = FALSE_;
	for (i__ = 0; i__ <= 255; ++i__) {
	    digit[(i__1 = i__) < 256 && 0 <= i__1 ? i__1 : s_rnge("digit", 
		    i__1, "m2begr_", (ftnlen)202)] = FALSE_;
	}
	digit[(i__1 = '0') < 256 && 0 <= i__1 ? i__1 : s_rnge("digit", i__1, 
		"m2begr_", (ftnlen)205)] = TRUE_;
	digit[(i__1 = '1') < 256 && 0 <= i__1 ? i__1 : s_rnge("digit", i__1, 
		"m2begr_", (ftnlen)206)] = TRUE_;
	digit[(i__1 = '2') < 256 && 0 <= i__1 ? i__1 : s_rnge("digit", i__1, 
		"m2begr_", (ftnlen)207)] = TRUE_;
	digit[(i__1 = '3') < 256 && 0 <= i__1 ? i__1 : s_rnge("digit", i__1, 
		"m2begr_", (ftnlen)208)] = TRUE_;
	digit[(i__1 = '4') < 256 && 0 <= i__1 ? i__1 : s_rnge("digit", i__1, 
		"m2begr_", (ftnlen)209)] = TRUE_;
	digit[(i__1 = '5') < 256 && 0 <= i__1 ? i__1 : s_rnge("digit", i__1, 
		"m2begr_", (ftnlen)210)] = TRUE_;
	digit[(i__1 = '6') < 256 && 0 <= i__1 ? i__1 : s_rnge("digit", i__1, 
		"m2begr_", (ftnlen)211)] = TRUE_;
	digit[(i__1 = '7') < 256 && 0 <= i__1 ? i__1 : s_rnge("digit", i__1, 
		"m2begr_", (ftnlen)212)] = TRUE_;
	digit[(i__1 = '8') < 256 && 0 <= i__1 ? i__1 : s_rnge("digit", i__1, 
		"m2begr_", (ftnlen)213)] = TRUE_;
	digit[(i__1 = '9') < 256 && 0 <= i__1 ? i__1 : s_rnge("digit", i__1, 
		"m2begr_", (ftnlen)214)] = TRUE_;
	lparen = '(';
	rparen = ')';
	colon = ':';
    }

/*     We need at least (x:) in order to have a range template,  that */
/*     means at least 4 characters. */

    if (*end - *beg < 3) {
	return 0;
    }
    i__ = *beg;

/*     Range templates must begin with '(' */

    if (*(unsigned char *)&string[i__ - 1] != lparen) {
	return 0;
    }
    ++i__;

/*     We must have at least 1 digit */

    if (! digit[(i__1 = *(unsigned char *)&string[i__ - 1]) < 256 && 0 <= 
	    i__1 ? i__1 : s_rnge("digit", i__1, "m2begr_", (ftnlen)247)]) {
	return 0;
    } else {
	++i__;
    }

/*     Now examin characters until we reach a non-digit */
/*     or run out of characters in the string. */

    while(i__ <= *end && digit[(i__1 = *(unsigned char *)&string[i__ - 1]) < 
	    256 && 0 <= i__1 ? i__1 : s_rnge("digit", i__1, "m2begr_", (
	    ftnlen)262)]) {
	++i__;
    }

/*     If the last character encountered was a number or if it was */
/*     not a colon, we don't have a range template. */

    if (digit[(i__1 = *(unsigned char *)&string[i__ - 1]) < 256 && 0 <= i__1 ?
	     i__1 : s_rnge("digit", i__1, "m2begr_", (ftnlen)273)]) {
	return 0;
    } else if (*(unsigned char *)&string[i__ - 1] != colon) {
	return 0;
    }

/*     Ok. we've got an integer. Parse it and put the result */
/*     into A. */

    i__1 = *beg;
    nparsi_(string + i__1, a, error, &pointr, i__ - 1 - i__1, (ftnlen)80);

/*     Just in case, make sure the number didn't cause an NPARSI error */
/*     (the only thing can go wrong is the number is too big) */

    if (pointr != 0) {
	return 0;
    }

/*     Look at the next letter ( if there is one ) and see if it */
/*     is a digit. */

    ++i__;
    start = i__;
    if (i__ > *end) {
	return 0;
    }

/*     Examine letters until we reach a non-digit or run out of */
/*     characters to examine. */

    while(i__ < *end && digit[(i__1 = *(unsigned char *)&string[i__ - 1]) < 
	    256 && 0 <= i__1 ? i__1 : s_rnge("digit", i__1, "m2begr_", (
	    ftnlen)314)]) {
	++i__;
    }

/*     If the last character is a digit (we ran out of letters) */
/*     or was not */

    if (digit[(i__1 = *(unsigned char *)&string[i__ - 1]) < 256 && 0 <= i__1 ?
	     i__1 : s_rnge("digit", i__1, "m2begr_", (ftnlen)322)]) {
	return 0;
    } else if (*(unsigned char *)&string[i__ - 1] != rparen) {
	return 0;
    }

/*     If the last character read is beyond the first character */
/*     after the ':', then we've got an integer. */

    if (i__ > start) {
	nparsi_(string + (start - 1), b, error, &pointr, i__ - 1 - (start - 1)
		, (ftnlen)80);

/*        Make sure everythin parsed ok. */

	if (pointr != 0) {
	    return 0;
	} else if (*b < *a) {
	    return 0;
	} else {
	    *beg = i__ + 1;
	    return 0;
	}

/*     If the first character after the colon was the right parenthesis */
/*     put INTMAX into B */

    } else {
	*b = intmax_();
	*beg = i__ + 1;
	return 0;
    }
    return 0;
} /* m2begr_ */

