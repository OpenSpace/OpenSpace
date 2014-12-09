/* m2keyw.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__20 = 20;

/* $Procedure      M2KEYW ( Determine whether or not a word is a keyword ) */
logical m2keyw_(char *word, ftnlen word_len)
{
    /* Initialized data */

    static char quick[4*20] = ")   " "@alp" "@bod" "@cal" "@day" "@end" "@eng"
	     "@epo" "@int" "@mon" "@nam" "@num" "@the" "@tim" "@uni" "@wor" 
	    "@yea" "{   " "|   " "}   ";
    static integer checks[20] = { 0,2,1,1,1,1,2,1,2,2,2,2,2,1,2,2,1,0,0,0 };
    static integer pntrs[20] = { 0,1,3,4,5,6,7,9,10,12,14,16,18,20,21,23,25,
	    25,25,25 };
    static char slow[16*25] = "@alpha          " "@alpha(%*)      " "@body  "
	    "         " "@calendar       " "@day            " "@end          "
	    "  " "@english        " "@english(%*)    " "@epoch          " 
	    "@int            " "@int(*:*)       " "@month          " "@month"
	    "(%*)      " "@name           " "@name(%*)       " "@number      "
	    "   " "@number(*:*)    " "@then           " "@then(%*)       " 
	    "@time           " "@unit           " "@unit(%*)       " "@word "
	    "          " "@word(%*)       " "@year           ";

    /* System generated locals */
    integer i__1;
    logical ret_val;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer i_len(char *, ftnlen), s_rnge(char *, integer, char *, integer);

    /* Local variables */
    static integer i__, j, k, l, blank;
    static logical match;
    static char cword[4];
    static integer lbrace, rbrace;
    extern integer bsrchc_(char *, integer *, char *, ftnlen, ftnlen);
    extern logical matchw_(char *, char *, char *, char *, ftnlen, ftnlen, 
	    ftnlen, ftnlen);
    static integer end;

/* $ Abstract */

/*     This function is true if the input string is a keyword in the */
/*     sense of META/2. */

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

/*     ALPHANUMERIC */
/*     ASCII */
/*     PARSING */
/*     UTILITY */
/*     WORD */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     WORD       I   A character string word */

/*     The function is returned as .TRUE. if word is an META/2 keyword. */

/* $ Detailed_Input */

/*     WORD      is a character string that is assumed to have no */
/*               spaces between the first and last non-blank characters. */

/* $ Detailed_Output */

/*     M2KEYW    returns as .TRUE. if WORD is not one of the following: */

/*             '@alpha',   '@alpha(%*)',  '@calendar',    '@body', */
/*             '@day', */
/*             '@end'      '@english',    '@english(%*)', '@epoch', */
/*             '@int',     '@int(*:*)'    '@month',       '@month(%*)', */
/*             '@name',    '@name(%*)',   '@number'       '@number(*:*)', */
/*             '@then'     '@then(%*)',   '@time',        '@year', */
/*             '{',        '|',           '}'             '@unit' */

/* $ Error_Handling */

/*     None. */
/* C */
/* $ Input_Files */

/*     None. */

/* $ Output_Files */

/*     None. */

/* $ Particulars */

/*     This is a utility routine for the subroutine META2.  It */
/*     determines whether or not a word is a keyword in the sense */
/*     of the language META/2. */

/* $ Examples */

/*     WORD                                  M2KEYW */
/*     -------                               ------ */
/*     @english(A*)                          .FALSE. */
/*     SPAM                                  .TRUE. */
/*     |                                     .FALSE. */
/*     19                                    .TRUE. */
/*     @bug                                  .TRUE. */
/*     @number                               .FALSE. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */

/* $ Version */

/* -     META/2 Version 3.0.0, 23-MAR-2000 (WLT) */

/*         Extended the routine to handle the new meta-keyword @unit */

/* -     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 9, 1994 */


/* -     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of META/2 */
/*         software as of May 3, 1994 */


/*     Version B1.0.0, 22-MAR-1988 (WLT) (IMU) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     We are going to look at the first four characters of the input */
/*     word.  If it doesn't match one of the following, then it isn't */
/*     a meta-2 specification word, it's a keyword.  The data in */
/*     this array should always be in increasing order. */


/*     If after checking against the previous list we have a match, */
/*     then we need to do further checks to see if we have a */
/*     legitimate meta-2 specification word.  If we have a bracket or */
/*     vertical bar, we are done ( zero more checks are required ). */
/*     In other cases 1 or two more checks may be required.  The */
/*     data below tells how many further checks may be required. */


/*     The PNTRS array points to the slot in the SLOW check array */
/*     where our matching pattern templates reside for checking */
/*     the current input word. */

    s_copy(cword, word, (ftnlen)4, word_len);
    i__ = bsrchc_(cword, &c__20, quick, (ftnlen)4, (ftnlen)4);
    if (i__ == 0) {
	ret_val = TRUE_;
	return ret_val;
    }

/*     We only want to examine the portion of the word that preceeds */
/*     a parsing qualifier.  First locate the last non-blank character */
/*     of the word. */

    lbrace = '[';
    rbrace = ']';
    blank = ' ';
    end = i_len(word, word_len);
    while(end > 1 && *(unsigned char *)&word[end - 1] == blank) {
	--end;
    }

/*     If the length is not at least 4 or the last character is not */
/*     a right brace, there is no name associated with this word. */

    if (*(unsigned char *)&word[end - 1] == rbrace && end >= 4) {

/*        Ok. We have a chance at getting a name.  Look for */
/*        a left brace and if found set the name and class end. */

	l = 2;
	while(l < end - 1) {
	    if (*(unsigned char *)&word[l - 1] == lbrace) {

/*              We've found the beginning of the name portion */
/*              of the word.  Record the end of the meta-2 */
/*              word and then reset L so that we exit this loop. */

		end = l - 1;
		l = end;
	    }
	    ++l;
	}
    }
    ret_val = FALSE_;
    k = pntrs[(i__1 = i__ - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge("pntrs", 
	    i__1, "m2keyw_", (ftnlen)295)];
    j = 1;
    match = FALSE_;
    while(j <= checks[(i__1 = i__ - 1) < 20 && 0 <= i__1 ? i__1 : s_rnge(
	    "checks", i__1, "m2keyw_", (ftnlen)299)] && ! match) {
	match = matchw_(word, slow + (((i__1 = k - 1) < 25 && 0 <= i__1 ? 
		i__1 : s_rnge("slow", i__1, "m2keyw_", (ftnlen)302)) << 4), 
		"*", "%", end, (ftnlen)16, (ftnlen)1, (ftnlen)1);
	ret_val = ! match;
	++k;
	++j;
    }
    return ret_val;
} /* m2keyw_ */

