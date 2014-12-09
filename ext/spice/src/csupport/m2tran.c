/* m2tran.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__19 = 19;

/* $Procedure      M2TRAN ( See if a word has a restriction template ) */
/* Subroutine */ int m2tran_(char *string, integer *beg, integer *end, char *
	base, logical *key, logical *temp, ftnlen string_len, ftnlen base_len)
{
    /* Initialized data */

    static char quick[4*19] = "@alp" "@bod" "@cal" "@day" "@end" "@eng" "@epo"
	     "@int" "@mon" "@nam" "@num" "@the" "@tim" "@uni" "@wor" "@yea" 
	    "{   " "|   " "}   ";
    static integer temps[19] = { 6,5,0,0,0,8,0,4,6,5,7,5,0,5,5,0,0,0,0 };
    static integer checks[19] = { 2,2,1,1,1,2,1,2,2,2,2,2,1,2,2,1,0,0,0 };
    static integer pntrs[19] = { 1,3,5,6,7,8,10,11,13,15,17,19,21,22,24,26,26,
	    26,26 };
    static char full[16*26] = "@alpha          " "@alpha(%*)      " "@body  "
	    "         " "@body(%*)       " "@calendar       " "@day          "
	    "  " "@end            " "@english        " "@english(%*)    " 
	    "@epoch          " "@int            " "@int(*:*)       " "@month"
	    "          " "@month(%*)      " "@name           " "@name(%*)    "
	    "   " "@number         " "@number(*:*)    " "@then           " 
	    "@then(%*)       " "@time           " "@unit           " "@unit("
	    "%*)       " "@word           " "@word(%*)       " "@year        "
	    "   ";

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);

    /* Local variables */
    static integer i__, j, k;
    static logical match;
    static char cword[4];
    extern integer bsrchc_(char *, integer *, char *, ftnlen, ftnlen);
    extern logical matchw_(char *, char *, char *, char *, ftnlen, ftnlen, 
	    ftnlen, ftnlen);

/* $ Abstract */

/*     Determine a META-WORD class and whether or not a word ends */
/*     with a substring of the (%*).  If it ends with such a substring */
/*     return pointers to the left and right parentheses. */

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
/*     BEG       I/O  The beginning of a word in STRING */
/*     END       I/O  The end of a word in STRING */
/*     BASE       O   Portion of the word preceeding the template. */
/*     KEY        O   .TRUE. if the the substring is a keyword. */
/*     TEMP       O   .TRUE. if a restriction template is present. */

/* $ Detailed_Input */

/*     STRING(BEG:END)  is a META/2 word that potentially ends with a */
/*                      substring of the form (%*) where '%' and '*' */
/*                      stand for the wildstring and wildcharacter */
/*                      symbols. */

/* $ Detailed_Output */

/*     BEG        is the index of the first character of the restriction */
/*                template ( the left parenthesis ) */
/*                first parenthesis '(' if a restriction template */
/*                is present.  If no restriction template is present */
/*                it is returned as END + 1. */

/*     END        is the index of the last character in the string. */

/*     BASE       is the portion of the string that precedes the */
/*                restriction template. If no template is present */
/*                BASE is assigned the value of word (with truncation */
/*                if BASE has shorter than END - BEG + 1 . */

/*     KEY        is returned as true if STRING(BEG:END) is a keyword */
/*                in the language that is being specified.  Otherwise */
/*                it is false. */

/*     TEMP       is returned as true if STRING(BEG:END) is a META-KEY */
/*                and ends with a restriction template. Otherwise it is */
/*                false. */

/* $ Error_Handling */

/*     None.  A restriction template is present or it isn't. */

/* $ Input_Files */

/*     None. */

/* $ Output_Files */

/*     None. */

/* $ Particulars */

/*      The list of META/2 keywords is given below.  A word of a */
/*      statement template is viewed as a language keyword if it is */
/*      not on this list. */

/*        '@alpha',   '@alpha(%*)',  '@body',        '@day', */
/*        '@end'      '@english',    '@english(%*)', '@epoch', */
/*        '@int',     '@int(*:*)'    '@month',       '@month(%*)', */
/*        '@name',    '@name(%*)',   '@number'       '@number(*:*)', */
/*        '@then'     '@then(%*)',   '@time',        '@unit', */
/*        '@year',    '}' */

/*      If the word is not a keyword, then it is examined and any */
/*      restriction templates are returned. */

/*      The restriction template is part of the META/2 language and is */
/*      described in the required reading section.  Briefly it is */
/*      a string at the end of a word that has the form */

/*      (x) */

/*      where x is any string of length at least 1.  The interpretation */
/*      of this string is handled in META2. */

/*      This is purely a utility for META2 and is not a general purpose */
/*      routine. */

/* $ Examples */

/*      None. */

/* $ Restrictions */

/*      None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */

/* $ Version */

/* -     META/2 Version 4.0.0, 23-MAR-2000 (WLT) */

/*         Extended the routine to add the keyword @unit to the */
/*         list of Meta/2 keywords. */

/* -     META/2 Configured Version 3.0.0, 14-AUG-1995 (WLT) */

/*         The keyword @body was out of order in the quick */
/*         check list below.  Who knows what other terrible */
/*         bugs this was causing. */

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


/*     The array QUICK contains abbreviations of all of the know META-KEY */
/*     words in alphabetical order. */


/*     The array TEMPS gives the character position within a word where */
/*     a template will be attached to a META-KEY word. */
/*     If the first portion of a word equals QUICK(I), TEMP(I) will be */
/*     the character immediately before the template (if one is present). */

/*     If a template is not allowed for a META-KEY word, TEMP will be 0. */


/*     The array CHECKS tells how many different ways a META-KEY word */
/*     can be represented.  For example @alpha or @alpha(template). */
/*     If a word matches up in the beginning with QUICK(I) then there */
/*     are at most CHECKS(I) checks that we must perform to see if it */
/*     is in fact a legitimate META-KEY word. */


/*     PNTRS(I) points to the first position in the array FULL where */
/*     one should look to find the actual patterns that should be */
/*     checked to see if a word that matches the initial portion */
/*     in QUICK(I) is in fact a META-KEY */


/*     First do a binary search on the abreviations of the META-KEYS */
/*     to see if this is a key word. */

    s_copy(cword, string + (*beg - 1), (ftnlen)4, *end - (*beg - 1));
    i__ = bsrchc_(cword, &c__19, quick, (ftnlen)4, (ftnlen)4);
    if (i__ == 0) {

/*        We didn't even match up with one of the abbreviations,  this */
/*        can't be a META-KEY and so must be a language specification */
/*        keyword. */

	*key = TRUE_;
	*temp = FALSE_;
	s_copy(base, string + (*beg - 1), base_len, *end - (*beg - 1));
	*beg = *end + 1;
	return 0;
    } else {

/*        We at least match an abbreviation.  See if we match the */
/*        full expansion of the abbreviation. */

	*key = FALSE_;
	k = pntrs[(i__1 = i__ - 1) < 19 && 0 <= i__1 ? i__1 : s_rnge("pntrs", 
		i__1, "m2tran_", (ftnlen)324)];
	j = 1;
	match = FALSE_;
	while(j <= checks[(i__1 = i__ - 1) < 19 && 0 <= i__1 ? i__1 : s_rnge(
		"checks", i__1, "m2tran_", (ftnlen)328)] && ! match) {
	    match = matchw_(string + (*beg - 1), full + (((i__1 = k - 1) < 26 
		    && 0 <= i__1 ? i__1 : s_rnge("full", i__1, "m2tran_", (
		    ftnlen)332)) << 4), "*", "%", *end - (*beg - 1), (ftnlen)
		    16, (ftnlen)1, (ftnlen)1);
	    *key = ! match;
	    ++k;
	    ++j;
	}
	if (*key) {
	    *temp = FALSE_;
	    s_copy(base, string + (*beg - 1), base_len, *end - (*beg - 1));
	    *beg = *end + 1;
	    return 0;
	}

/*        If we get this far we must have a META-KEY.  See if there */
/*        is a restriction template. */

	if (s_cmp(string + (*beg - 1), full + (((i__2 = pntrs[(i__1 = i__ - 1)
		 < 19 && 0 <= i__1 ? i__1 : s_rnge("pntrs", i__1, "m2tran_", (
		ftnlen)355)] - 1) < 26 && 0 <= i__2 ? i__2 : s_rnge("full", 
		i__2, "m2tran_", (ftnlen)355)) << 4), *end - (*beg - 1), (
		ftnlen)16) == 0) {

/*           There is no restriction template. */

	    s_copy(base, string + (*beg - 1), base_len, *end - (*beg - 1));
	    *beg = *end + 1;
	    *temp = FALSE_;
	} else {

/*           We have a restriction template. */

	    s_copy(base, full + (((i__2 = pntrs[(i__1 = i__ - 1) < 19 && 0 <= 
		    i__1 ? i__1 : s_rnge("pntrs", i__1, "m2tran_", (ftnlen)
		    370)] - 1) < 26 && 0 <= i__2 ? i__2 : s_rnge("full", i__2,
		     "m2tran_", (ftnlen)370)) << 4), base_len, (ftnlen)16);
	    *beg += temps[(i__1 = i__ - 1) < 19 && 0 <= i__1 ? i__1 : s_rnge(
		    "temps", i__1, "m2tran_", (ftnlen)371)];
	    *temp = TRUE_;
	}
    }
    return 0;
} /* m2tran_ */

