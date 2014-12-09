/* matchc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure MATCHC ( Match the characters in two words ) */
integer matchc_(char *word, char *guess, ftnlen word_len, ftnlen guess_len)
{
    /* Initialized data */

    static logical first = TRUE_;
    static integer uvalue[256] = { 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
	    17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,
	    39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,
	    61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,
	    83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,
	    103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,
	    119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,
	    135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,
	    151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,
	    167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,
	    183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,
	    199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,
	    215,216,217,218,219,220,221,222,223,224,225,226,227,228,229,230,
	    231,232,233,234,235,236,237,238,239,240,241,242,243,244,245,246,
	    247,248,249,250,251,252,253,254,255 };
    static integer gcount[94] = { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0 };
    static integer wcount[94] = { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	    0,0,0,0,0,0 };

    /* System generated locals */
    integer ret_val, i__1, i__2, i__3;
    doublereal d__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer), i_len(char *, ftnlen);

    /* Local variables */
    static integer nsig, c__, i__, j;
    static doublereal scard, ucard;
    static integer total, mn, mx, scardi, ucardi, hit[94];

/* $ Abstract */

/*      Assign a score to a pair of words which reflects the closeness */
/*      of the words in terms of the characters they contain. Disregard */
/*      the case of letters */

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

/*      SEARCH */
/*      UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*      Variable  I/O  Description */
/*      --------  ---  -------------------------------------------------- */
/*      WORD       I   Word to be matched against initial guess. */
/*      GUESS      I   Initial guess. */

/* $ Detailed_Input */

/*      WORD       is a character string to be checked for a match */
/*                 against an initial guess. Non-printing characters */
/*                 (including blanks) are ignored. Typically, WORD will */
/*                 contain a single word. In any case, the significant */
/*                 part of WORD may not exceed 64 characters. */

/*      GUESS      is an initial guess at the value of the input */
/*                 word. Non-printing characters (including blanks) */
/*                 are ignored. Like WORD, this will typically be a */
/*                 single word. In any case, the significant part of */
/*                 GUESS may not exceed 64 characters. */

/* $ Detailed_Output */

/*      The function returns a score between 0 (indicating that WORD */
/*      and GUESS have no characters in common) and 100 (indicating */
/*      that WORD and GUESS have all their characters in common). */

/* $ Exceptions */

/*      1) If neither WORD nor GUESS contains any printing characters, */
/*         the function returns 0. */

/* $ Particulars */

/*      In order to determine whether a word (usually typed by a user) */
/*      matches any of a series of known words (keywords, for example), */
/*      it is necessary to be able to judge the "closeness" of an */
/*      arbitrary pair of words. Several algorithms exist which make */
/*      such a comparison, the best-known of which is probably the */
/*      Soundex algorithm. */

/*      The score assigned by MATCHC differs from most other algorithms */
/*      in that multiple occurrences of letters are counted as distinct */
/*      characters. This allows the lengths of characters to enter into */
/*      the computation. */

/*      Another difference is that MATCHC does not assign higher */
/*      weights to more "exotic" characters, like Q and Z, since these */
/*      are as likely to appear in mistyped words as are any other */
/*      characters. (Both Q and Z, for instance, are adjacent to A */
/*      on a standard keyboard.) */

/*      The score assigned by MATCHC is computed in this way. */

/*         1) The characters in each word are sorted, assigned */
/*            ordinal numbers, and placed in a set. Thus, the word */
/*            'APPEAL' gives rise to the set */

/*               'A1', 'A2', 'E1', 'L1', 'P1', 'P2' */

/*         2) The union and the symmetric difference of the sets */
/*            formed from WORD and GUESS are computed. */

/*         3) Letting #(U) and #(S) be the cardinalities of the */
/*            union and symmetric differences respectively, the */
/*            score assigned to the pair (WORD, GUESS) is */

/*                           #(S) */
/*              100 * ( 1 -  ---- ) */
/*                           #(U) */

/*      When WORD and GUESS have no characters in common, the symmetric */
/*      difference and the union are equivalent, and the score is zero. */
/*      When they share the same characters (including multiply occurring */
/*      characters), the symmetric difference is empty, and the score */
/*      is 100. */

/* $ Examples */


/* $ Restrictions */

/*      1) MATCHC is case-sensitive. Lowercase characters do not match */
/*         uppercase characters, and vice versa. */

/* $ Common_Variables */

/*      None. */

/* $ Author_and_Institution */

/*      W.L. Taber     (JPL) */
/*      I.M. Underwood (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 9, 1994 */


/* -     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of META/2 */
/*         software as of May 3, 1994 */


/*      Version B 1.0.0, 5-APR-1988 */

/* -& */

/*     SPICELIB functions */


/*     The printable character set is bounded below by ASCII character */
/*     32 (SP) and above by ASCII character 127 (DEL). */


/*     Only the first 64 characters of WORD and GUESS are significant. */


/*     Local variables */


/*     Initialize the character mapping "function" (array). */

    if (first) {
	first = FALSE_;
	uvalue[(i__1 = 'a') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matchc_", (ftnlen)261)] = 'A';
	uvalue[(i__1 = 'b') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matchc_", (ftnlen)262)] = 'B';
	uvalue[(i__1 = 'c') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matchc_", (ftnlen)263)] = 'C';
	uvalue[(i__1 = 'd') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matchc_", (ftnlen)264)] = 'D';
	uvalue[(i__1 = 'e') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matchc_", (ftnlen)265)] = 'E';
	uvalue[(i__1 = 'f') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matchc_", (ftnlen)266)] = 'F';
	uvalue[(i__1 = 'g') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matchc_", (ftnlen)267)] = 'G';
	uvalue[(i__1 = 'h') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matchc_", (ftnlen)268)] = 'H';
	uvalue[(i__1 = 'i') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matchc_", (ftnlen)269)] = 'I';
	uvalue[(i__1 = 'j') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matchc_", (ftnlen)270)] = 'J';
	uvalue[(i__1 = 'k') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matchc_", (ftnlen)271)] = 'K';
	uvalue[(i__1 = 'l') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matchc_", (ftnlen)272)] = 'L';
	uvalue[(i__1 = 'm') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matchc_", (ftnlen)273)] = 'M';
	uvalue[(i__1 = 'n') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matchc_", (ftnlen)274)] = 'N';
	uvalue[(i__1 = 'o') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matchc_", (ftnlen)275)] = 'O';
	uvalue[(i__1 = 'p') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matchc_", (ftnlen)276)] = 'P';
	uvalue[(i__1 = 'q') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matchc_", (ftnlen)277)] = 'Q';
	uvalue[(i__1 = 'r') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matchc_", (ftnlen)278)] = 'R';
	uvalue[(i__1 = 's') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matchc_", (ftnlen)279)] = 'S';
	uvalue[(i__1 = 't') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matchc_", (ftnlen)280)] = 'T';
	uvalue[(i__1 = 'u') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matchc_", (ftnlen)281)] = 'U';
	uvalue[(i__1 = 'v') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matchc_", (ftnlen)282)] = 'V';
	uvalue[(i__1 = 'w') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matchc_", (ftnlen)283)] = 'W';
	uvalue[(i__1 = 'x') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matchc_", (ftnlen)284)] = 'X';
	uvalue[(i__1 = 'y') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matchc_", (ftnlen)285)] = 'Y';
	uvalue[(i__1 = 'z') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matchc_", (ftnlen)286)] = 'Z';
    }

/*     Tally up the characters in WORD.  Also, everytime a new */
/*     character is encountered, increment the number of characters */
/*     that have been observed and record which new character has */
/*     just been observed. */

    nsig = 0;
    total = 0;
    i__1 = i_len(word, word_len);
    for (i__ = 1; i__ <= i__1; ++i__) {
	c__ = uvalue[(i__2 = *(unsigned char *)&word[i__ - 1]) < 256 && 0 <= 
		i__2 ? i__2 : s_rnge("uvalue", i__2, "matchc_", (ftnlen)301)];
	if (c__ >= 33 && c__ <= 126) {
	    ++nsig;
	    if (nsig <= 64) {
		if (wcount[(i__2 = c__ - 33) < 94 && 0 <= i__2 ? i__2 : 
			s_rnge("wcount", i__2, "matchc_", (ftnlen)309)] == 0) 
			{
		    ++total;
		    hit[(i__2 = total - 1) < 94 && 0 <= i__2 ? i__2 : s_rnge(
			    "hit", i__2, "matchc_", (ftnlen)311)] = c__;
		}
		wcount[(i__2 = c__ - 33) < 94 && 0 <= i__2 ? i__2 : s_rnge(
			"wcount", i__2, "matchc_", (ftnlen)314)] = wcount[(
			i__3 = c__ - 33) < 94 && 0 <= i__3 ? i__3 : s_rnge(
			"wcount", i__3, "matchc_", (ftnlen)314)] + 1;
	    }
	}
    }

/*     Tally up the characters in GUESS.  Also, everytime a new */
/*     character is encountered, increment the number of characters */
/*     that have been observed and record which new character has */
/*     just been observed. */

    nsig = 0;
    i__1 = i_len(guess, guess_len);
    for (i__ = 1; i__ <= i__1; ++i__) {
	c__ = uvalue[(i__2 = *(unsigned char *)&guess[i__ - 1]) < 256 && 0 <= 
		i__2 ? i__2 : s_rnge("uvalue", i__2, "matchc_", (ftnlen)332)];
	if (c__ >= 33 && c__ <= 126) {
	    ++nsig;
	    if (nsig <= 64) {
		if (wcount[(i__2 = c__ - 33) < 94 && 0 <= i__2 ? i__2 : 
			s_rnge("wcount", i__2, "matchc_", (ftnlen)340)] == 0) 
			{
		    if (gcount[(i__2 = c__ - 33) < 94 && 0 <= i__2 ? i__2 : 
			    s_rnge("gcount", i__2, "matchc_", (ftnlen)341)] ==
			     0) {
			++total;
			hit[(i__2 = total - 1) < 94 && 0 <= i__2 ? i__2 : 
				s_rnge("hit", i__2, "matchc_", (ftnlen)343)] =
				 c__;
		    }
		}
		gcount[(i__2 = c__ - 33) < 94 && 0 <= i__2 ? i__2 : s_rnge(
			"gcount", i__2, "matchc_", (ftnlen)347)] = gcount[(
			i__3 = c__ - 33) < 94 && 0 <= i__3 ? i__3 : s_rnge(
			"gcount", i__3, "matchc_", (ftnlen)347)] + 1;
	    }
	}
    }

/*     Now look through the list of characters that were hit */
/*     and compute their contributions to the cardinality */
/*     of the symmetric difference and unions of the letter sets. */

    scardi = 0;
    ucardi = 0;
    i__1 = total;
    for (i__ = 1; i__ <= i__1; ++i__) {
	j = hit[(i__2 = i__ - 1) < 94 && 0 <= i__2 ? i__2 : s_rnge("hit", 
		i__2, "matchc_", (ftnlen)369)];
	if (wcount[(i__2 = j - 33) < 94 && 0 <= i__2 ? i__2 : s_rnge("wcount",
		 i__2, "matchc_", (ftnlen)371)] > gcount[(i__3 = j - 33) < 94 
		&& 0 <= i__3 ? i__3 : s_rnge("gcount", i__3, "matchc_", (
		ftnlen)371)]) {
	    mx = wcount[(i__2 = j - 33) < 94 && 0 <= i__2 ? i__2 : s_rnge(
		    "wcount", i__2, "matchc_", (ftnlen)372)];
	    mn = gcount[(i__2 = j - 33) < 94 && 0 <= i__2 ? i__2 : s_rnge(
		    "gcount", i__2, "matchc_", (ftnlen)373)];
	} else {
	    mx = gcount[(i__2 = j - 33) < 94 && 0 <= i__2 ? i__2 : s_rnge(
		    "gcount", i__2, "matchc_", (ftnlen)375)];
	    mn = wcount[(i__2 = j - 33) < 94 && 0 <= i__2 ? i__2 : s_rnge(
		    "wcount", i__2, "matchc_", (ftnlen)376)];
	}
	scardi = scardi + mx - mn;
	ucardi += mx;

/*        While we're here, set the counts back to zero in preparation */
/*        for the next time this routine gets called. */

	wcount[(i__2 = j - 33) < 94 && 0 <= i__2 ? i__2 : s_rnge("wcount", 
		i__2, "matchc_", (ftnlen)386)] = 0;
	gcount[(i__2 = j - 33) < 94 && 0 <= i__2 ? i__2 : s_rnge("gcount", 
		i__2, "matchc_", (ftnlen)387)] = 0;
    }
    scard = (doublereal) scardi;
    ucard = (doublereal) ucardi;



/*     And assign the score. */

    if (ucard == 0.) {
	ret_val = 0;
    } else if (scard <= 2.) {
/* Computing 2nd power */
	d__1 = scard / ucard;
	ret_val = (integer) ((1. - d__1 * d__1) * 100.);
    } else {
	ret_val = (integer) ((1. - scard / ucard) * 100.);
    }
    return ret_val;
} /* matchc_ */

