/* matcho.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure MATCHO ( Match the characters in two words ) */
integer matcho_(char *word, char *guess, ftnlen word_len, ftnlen guess_len)
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

    /* System generated locals */
    integer ret_val, i__1, i__2, i__3;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    static integer glen, wlen, g2seq[32], w2seq[32], i__, j, value, c1, c2;
    extern integer ltrim_(char *, ftnlen);
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    static integer gf, gl, gp, wf, wl, wp;
    extern /* Subroutine */ int shelli_(integer *, integer *);
    static integer gscore, gpairs[512], gmscor, g2c, gtally, gcount, wscore, 
	    wpairs[512], wmscor, w2c, wtally;
    extern integer qrtrim_(char *, ftnlen);
    static integer wcount;

/* $ Abstract */

/*      Assign a score to a pair of words which reflects the closeness */
/*      of the words in terms of the characters they contain and the */
/*      order in which the characters appear. */

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
/*                 against an initial guess. The (non-printing) ASCII */
/*                 characters 1 and 2 are ignored. Typically, WORD will */
/*                 contain a single word. */

/*      GUESS      is an initial guess at the value of the input */
/*                 word. The (non-printing) ASCII characters 1 and 2 */
/*                 are ignored. Like WORD, this will typically be a */
/*                 single word. */

/* $ Detailed_Output */

/*      The function returns a score between 0 (indicating that WORD */
/*      and GUESS have no common character patterns) and 100 (indicating */
/*      that WORD and GUESS match very closely). */

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

/*      The score assigned by MATCHO indicates not only how many of the */
/*      letters two words have in common, but also the relative */
/*      difference between the order in which these letters appear. */

/*      MATCHO does not assign higher weights to more exotic characters, */
/*      like Q and Z, since these are as likely to appear in mistyped */
/*      words as are any other characters. (Both Q and Z, for instance, */
/*      are adjacent to A on a standard keyboard.) */

/*      The score assigned by MATCHO is computed in this way. */

/*          Suppose WORD is the string */

/*             w_1 w_2  ... w_n */

/*          and GUESS is the string */

/*             g_1 g_2 ... g_m */

/*          Each of the MATCHW templates */

/*          * w_i * w_j *  (where i < j) */

/*          is matched against GUESS and the total number of */
/*          matches tallied.  (There are n(n-1)/2 such templates) */

/*          Additionally 1 extra point is awarded for each match of GUESS */
/*          with a template of the form */

/*          * w_i w_i+1 * . */

/*          The total tally is multiplied by 200/n(n-1) and truncated to */
/*          100 if necessary to yield a GUESS to WORD tally. */

/*          Then the roles of WORD and GUESS are reversed and an */
/*          identical proceedure is followed to obtain a WORD to GUESS */
/*          tally.  The average of the two tallies is returned in */
/*          MATCHO. */

/*      Empirically it has been found that WORD and GUESS are in */
/*      close agreement if MATCHO is returned with a value of 75 */
/*      or more.  Users may wish to use higher or lower score when */
/*      determining when a match between two words is close. */

/* $ Examples */


/* $ Restrictions */

/*      1) MATCHO is case-insensitive. Lowercase characters match */
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


/*      Version B 1.0.0, 7-APR-1988 (WLT) (IMU) */

/* -& */

/*     SPICELIB functions */


/*     Local Parameters */


/*     Local variables */


/*     Set up the case insensitive mapping. */

    if (first) {
	first = FALSE_;
	uvalue[(i__1 = 'a') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matcho_", (ftnlen)274)] = 'A';
	uvalue[(i__1 = 'b') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matcho_", (ftnlen)275)] = 'B';
	uvalue[(i__1 = 'c') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matcho_", (ftnlen)276)] = 'C';
	uvalue[(i__1 = 'd') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matcho_", (ftnlen)277)] = 'D';
	uvalue[(i__1 = 'e') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matcho_", (ftnlen)278)] = 'E';
	uvalue[(i__1 = 'f') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matcho_", (ftnlen)279)] = 'F';
	uvalue[(i__1 = 'g') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matcho_", (ftnlen)280)] = 'G';
	uvalue[(i__1 = 'h') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matcho_", (ftnlen)281)] = 'H';
	uvalue[(i__1 = 'i') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matcho_", (ftnlen)282)] = 'I';
	uvalue[(i__1 = 'j') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matcho_", (ftnlen)283)] = 'J';
	uvalue[(i__1 = 'k') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matcho_", (ftnlen)284)] = 'K';
	uvalue[(i__1 = 'l') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matcho_", (ftnlen)285)] = 'L';
	uvalue[(i__1 = 'm') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matcho_", (ftnlen)286)] = 'M';
	uvalue[(i__1 = 'n') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matcho_", (ftnlen)287)] = 'N';
	uvalue[(i__1 = 'o') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matcho_", (ftnlen)288)] = 'O';
	uvalue[(i__1 = 'p') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matcho_", (ftnlen)289)] = 'P';
	uvalue[(i__1 = 'q') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matcho_", (ftnlen)290)] = 'Q';
	uvalue[(i__1 = 'r') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matcho_", (ftnlen)291)] = 'R';
	uvalue[(i__1 = 's') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matcho_", (ftnlen)292)] = 'S';
	uvalue[(i__1 = 't') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matcho_", (ftnlen)293)] = 'T';
	uvalue[(i__1 = 'u') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matcho_", (ftnlen)294)] = 'U';
	uvalue[(i__1 = 'v') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matcho_", (ftnlen)295)] = 'V';
	uvalue[(i__1 = 'w') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matcho_", (ftnlen)296)] = 'W';
	uvalue[(i__1 = 'x') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matcho_", (ftnlen)297)] = 'X';
	uvalue[(i__1 = 'y') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matcho_", (ftnlen)298)] = 'Y';
	uvalue[(i__1 = 'z') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "matcho_", (ftnlen)299)] = 'Z';
    }

/*     First get the ``dimensions'' of our two words (first non-blank, */
/*     last non-blank, and non-blank length). */

    gf = ltrim_(guess, guess_len);
    gl = qrtrim_(guess, guess_len);
    wf = ltrim_(word, word_len);
    wl = qrtrim_(word, word_len);
    glen = gl - gf + 1;
    wlen = wl - wf + 1;

/*     Perform some of the obvious checks first. */

    if (eqstr_(word + (wf - 1), guess + (gf - 1), wl - (wf - 1), gl - (gf - 1)
	    )) {
	ret_val = 100;
	return ret_val;
    } else if (wlen <= 1 || glen <= 1) {
	ret_val = 0;
	return ret_val;
    }

/*     Initialize the score keeper and compute the length of GUESS. */

    wmscor = (wlen - 1) * wlen / 2;
    gmscor = (glen - 1) * glen / 2;

/*     We will encode ordered letter pairs as */

/*        BASE * ICHAR(first)   +   ICHAR(second) */

/*     Where BASE is chosen large enough so that we will never have */
/*     different pairs mapping to the same integer. */

/*     Compute the encoded collection of ordered pairs for */
/*     the GUESS (GCOUNT is the number of general pairs */
/*     G2C is the number of 2 character substrings) ... */

    gcount = 0;
    g2c = 0;
    i__1 = gl - 1;
    for (i__ = gf; i__ <= i__1; ++i__) {
	c1 = uvalue[(i__2 = *(unsigned char *)&guess[i__ - 1]) < 256 && 0 <= 
		i__2 ? i__2 : s_rnge("uvalue", i__2, "matcho_", (ftnlen)355)];
	i__2 = i__;
	c2 = uvalue[(i__3 = *(unsigned char *)&guess[i__2]) < 256 && 0 <= 
		i__3 ? i__3 : s_rnge("uvalue", i__3, "matcho_", (ftnlen)356)];
	++g2c;
	g2seq[(i__2 = g2c - 1) < 32 && 0 <= i__2 ? i__2 : s_rnge("g2seq", 
		i__2, "matcho_", (ftnlen)359)] = (c1 << 10) + c2;
	i__2 = gl;
	for (j = i__ + 1; j <= i__2; ++j) {
	    c1 = uvalue[(i__3 = *(unsigned char *)&guess[i__ - 1]) < 256 && 0 
		    <= i__3 ? i__3 : s_rnge("uvalue", i__3, "matcho_", (
		    ftnlen)363)];
	    c2 = uvalue[(i__3 = *(unsigned char *)&guess[j - 1]) < 256 && 0 <=
		     i__3 ? i__3 : s_rnge("uvalue", i__3, "matcho_", (ftnlen)
		    364)];
	    ++gcount;
	    gpairs[(i__3 = gcount - 1) < 512 && 0 <= i__3 ? i__3 : s_rnge(
		    "gpairs", i__3, "matcho_", (ftnlen)367)] = (c1 << 10) + 
		    c2;
	}
    }

/*     ... then construct the encoded ordered letter pairs for WORD. */

    wcount = 0;
    w2c = 0;
    i__1 = wl - 1;
    for (i__ = wf; i__ <= i__1; ++i__) {
	c1 = uvalue[(i__2 = *(unsigned char *)&word[i__ - 1]) < 256 && 0 <= 
		i__2 ? i__2 : s_rnge("uvalue", i__2, "matcho_", (ftnlen)381)];
	i__2 = i__;
	c2 = uvalue[(i__3 = *(unsigned char *)&word[i__2]) < 256 && 0 <= i__3 
		? i__3 : s_rnge("uvalue", i__3, "matcho_", (ftnlen)382)];
	++w2c;
	w2seq[(i__2 = w2c - 1) < 32 && 0 <= i__2 ? i__2 : s_rnge("w2seq", 
		i__2, "matcho_", (ftnlen)385)] = (c1 << 10) + c2;
	i__2 = wl;
	for (j = i__ + 1; j <= i__2; ++j) {
	    c1 = uvalue[(i__3 = *(unsigned char *)&word[i__ - 1]) < 256 && 0 
		    <= i__3 ? i__3 : s_rnge("uvalue", i__3, "matcho_", (
		    ftnlen)389)];
	    c2 = uvalue[(i__3 = *(unsigned char *)&word[j - 1]) < 256 && 0 <= 
		    i__3 ? i__3 : s_rnge("uvalue", i__3, "matcho_", (ftnlen)
		    390)];
	    ++wcount;
	    wpairs[(i__3 = wcount - 1) < 512 && 0 <= i__3 ? i__3 : s_rnge(
		    "wpairs", i__3, "matcho_", (ftnlen)393)] = (c1 << 10) + 
		    c2;
	}
    }

/*     Now sort the various arrays of encoded letter pairs */

    shelli_(&g2c, g2seq);
    shelli_(&gcount, gpairs);
    shelli_(&w2c, w2seq);
    shelli_(&wcount, wpairs);
    g2seq[(i__1 = g2c) < 32 && 0 <= i__1 ? i__1 : s_rnge("g2seq", i__1, "mat"
	    "cho_", (ftnlen)407)] = 0;
    gpairs[(i__1 = gcount) < 512 && 0 <= i__1 ? i__1 : s_rnge("gpairs", i__1, 
	    "matcho_", (ftnlen)408)] = 0;
    w2seq[(i__1 = w2c) < 32 && 0 <= i__1 ? i__1 : s_rnge("w2seq", i__1, "mat"
	    "cho_", (ftnlen)409)] = 0;
    wpairs[(i__1 = wcount) < 512 && 0 <= i__1 ? i__1 : s_rnge("wpairs", i__1, 
	    "matcho_", (ftnlen)410)] = 0;

/*     First tally up the matches of the form *L1*L2*.  This is */
/*     virtually the same algorithm used for computing set */
/*     intersections. */

    wp = 1;
    gp = 1;
    wtally = 0;
    gtally = 0;
    while(wp <= wcount && gp <= gcount) {
	if (wpairs[(i__1 = wp - 1) < 512 && 0 <= i__1 ? i__1 : s_rnge("wpairs"
		, i__1, "matcho_", (ftnlen)426)] < gpairs[(i__2 = gp - 1) < 
		512 && 0 <= i__2 ? i__2 : s_rnge("gpairs", i__2, "matcho_", (
		ftnlen)426)]) {
	    ++wp;
	} else if (wpairs[(i__1 = wp - 1) < 512 && 0 <= i__1 ? i__1 : s_rnge(
		"wpairs", i__1, "matcho_", (ftnlen)430)] > gpairs[(i__2 = gp 
		- 1) < 512 && 0 <= i__2 ? i__2 : s_rnge("gpairs", i__2, "mat"
		"cho_", (ftnlen)430)]) {
	    ++gp;
	} else {
	    value = wpairs[(i__1 = wp - 1) < 512 && 0 <= i__1 ? i__1 : s_rnge(
		    "wpairs", i__1, "matcho_", (ftnlen)436)];
	    while(wpairs[(i__1 = wp - 1) < 512 && 0 <= i__1 ? i__1 : s_rnge(
		    "wpairs", i__1, "matcho_", (ftnlen)438)] == value && wp <=
		     wcount) {
		++wtally;
		++wp;
	    }
	    while(gpairs[(i__1 = gp - 1) < 512 && 0 <= i__1 ? i__1 : s_rnge(
		    "gpairs", i__1, "matcho_", (ftnlen)446)] == value && gp <=
		     gcount) {
		++gtally;
		++gp;
	    }
	}
    }

/*     Next tally up the various matches of the form *L1L2* */

    wp = 1;
    gp = 1;
    while(wp <= w2c && gp <= g2c) {
	if (w2seq[(i__1 = wp - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("w2seq", 
		i__1, "matcho_", (ftnlen)468)] < g2seq[(i__2 = gp - 1) < 32 &&
		 0 <= i__2 ? i__2 : s_rnge("g2seq", i__2, "matcho_", (ftnlen)
		468)]) {
	    ++wp;
	} else if (w2seq[(i__1 = wp - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge(
		"w2seq", i__1, "matcho_", (ftnlen)472)] > g2seq[(i__2 = gp - 
		1) < 32 && 0 <= i__2 ? i__2 : s_rnge("g2seq", i__2, "matcho_",
		 (ftnlen)472)]) {
	    ++gp;
	} else {
	    value = w2seq[(i__1 = wp - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge(
		    "w2seq", i__1, "matcho_", (ftnlen)478)];
	    while(w2seq[(i__1 = wp - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge(
		    "w2seq", i__1, "matcho_", (ftnlen)480)] == value && wp <= 
		    w2c) {
		++wtally;
		++wp;
	    }
	    while(g2seq[(i__1 = gp - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge(
		    "g2seq", i__1, "matcho_", (ftnlen)488)] == value && gp <= 
		    g2c) {
		++gtally;
		++gp;
	    }
	}
    }
    gtally = min(gtally,gmscor);
    wtally = min(wtally,wmscor);
    wscore = wtally * 100 / wmscor;
    gscore = gtally * 100 / gmscor;
/* Computing MIN */
    i__1 = (wscore + gscore) / 2;
    ret_val = min(i__1,100);
    return ret_val;
} /* matcho_ */

