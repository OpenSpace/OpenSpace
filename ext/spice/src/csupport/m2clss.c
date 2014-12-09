/* m2clss.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__1 = 1;
static integer c__0 = 0;

/* $Procedure      M2CLSS (Meta 2 --- meta 2 word classification ) */
/* Subroutine */ int m2clss_(char *word, integer *num, char *phrase, ftnlen 
	word_len, ftnlen phrase_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    address a__1[2];
    integer i__1[2], i__2;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen), s_cat(char *,
	     char **, integer *, integer *, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_rnge(char *, integer, 
	    char *, integer);

    /* Local variables */
    static char base[32], name__[80*2], body[80*2], time[80*2], year[80*2];
    static integer b, c__, e;
    static char alpha[80*2], epoch[80*2];
    extern /* Subroutine */ int repmc_(char *, char *, char *, char *, ftnlen,
	     ftnlen, ftnlen, ftnlen);
    static char other[80*2], gword[80*2], month[80*2];
    static logical rtemp;
    extern integer rtrim_(char *, ftnlen);
    static char units[80*2];
    extern /* Subroutine */ int m2tran_(char *, integer *, integer *, char *, 
	    logical *, logical *, ftnlen, ftnlen);
    static char dp[80*2], englsh[80*2];
    static integer number;
    extern /* Subroutine */ int suffix_(char *, integer *, char *, ftnlen, 
	    ftnlen);
    static char day[80*2];
    static logical key;
    static char int__[80*2];
    extern integer pos_(char *, char *, integer *, ftnlen, ftnlen);

/* $ Abstract */

/*     This routine creates a phrase of the appropiate number */
/*     that describes the meta2 syntax word WORD. */

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

/*       META2 */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      WORD       I   A meta-2 keyword. */
/*      NUM        I   The number of meta-2 keywords */
/*      PHRASE     O   A description of NUM WORDs. */

/* $ Detailed_Input */

/*     WORD        is a meta-2 keyword such as @int or @number(1:20) */

/*     NUM         is used to indicate if how many of the WORDS we */
/*                 want to talk about.  For example when describing */
/*                 @int(1:10) do you want to say */

/*                    integer between 1 and 10 */
/*                 or */
/*                    integers between 1 and 10 */

/*                 If NUM is 1 you get the first phrase.  Otherwise */
/*                 you get the second one. */

/* $ Detailed_Output */

/*     PHRASE      is a character string that describes WORD and NUM. */


/* $ Parameters */

/*      None. */

/* $ Files */

/*      None. */

/* $ Exceptions */

/*     1) Although it has not changed in a long time.  META/2 might */
/*     have some other word classifiers added.  In that case this */
/*     routine will have to be updated.  But it will make a reasonable */
/*     phrase even if the new META/2 keyword isn't recognized yet. */
/*     Something like */

/*        word(s) of the class WORD */

/*     will be used. */

/* $ Particulars */

/*     This */

/* $ Examples */

/*     Suppose that a message needs to be created that says */
/*     a word in a string does not match an expected @int(1:10) */

/*     You could use this routine together with the utility function */
/*     ANA to construct a reasonable message. */

/*     CALL  M2CLSS ( '@int(1:10), 1, PHRASE ) */
/*     ARTCLE = ANA (  PHRASE,    'C'        ) */

/*     MESSGE = '# # was expected in the string.' */

/*     CALL REPMC ( MESSGE, '#', ARTCLE, MESSGE ) */
/*     CALL REPMC ( MESSGE, '#', PHRASE, MESSGE ) */

/*     The resulting string in MESSGE would be */

/*     'An integer between 1 and 10 was expected.' */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*      W.L. Taber      (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -    META/2 Version 2.0.0, 23-MAY-2000 (WLT) */

/*        Updated the routine to support the additional Meta/2 keyword */
/*        @unit. */

/* -    META/2 Version 1.0.0, 12-AUG-1995 (WLT) */


/* -& */
/* $ Index_Entries */

/*     META/2 diagnostic message creation tool */

/* -& */

/*     SPICELIB functions */

    if (first) {
	first = FALSE_;
	s_copy(alpha, "word beginning with a letter", (ftnlen)80, (ftnlen)28);
	s_copy(alpha + 80, "words beginning with a letter", (ftnlen)80, (
		ftnlen)29);
	s_copy(body, "body name or id-code", (ftnlen)80, (ftnlen)20);
	s_copy(body + 80, "body names or id-codes", (ftnlen)80, (ftnlen)22);
	s_copy(day, "day of the year", (ftnlen)80, (ftnlen)15);
	s_copy(day + 80, "days of the year", (ftnlen)80, (ftnlen)16);
	s_copy(englsh, "word containing only letters", (ftnlen)80, (ftnlen)28)
		;
	s_copy(englsh + 80, "words containing only letters", (ftnlen)80, (
		ftnlen)29);
	s_copy(epoch, "epoch", (ftnlen)80, (ftnlen)5);
	s_copy(epoch + 80, "epochs", (ftnlen)80, (ftnlen)6);
	s_copy(month, "month of the year", (ftnlen)80, (ftnlen)17);
	s_copy(month + 80, "months of the year", (ftnlen)80, (ftnlen)18);
	s_copy(name__, "word of letters and digits starting with a letter", (
		ftnlen)80, (ftnlen)49);
	s_copy(name__ + 80, "words of letters and digits each starting with "
		"a letter ", (ftnlen)80, (ftnlen)56);
	s_copy(time, "time of day", (ftnlen)80, (ftnlen)11);
	s_copy(time + 80, "times of the day", (ftnlen)80, (ftnlen)16);
	s_copy(year, "calendar year (1000 to 3000) ", (ftnlen)80, (ftnlen)29);
	s_copy(year + 80, "calendar years (1000 to 3000) ", (ftnlen)80, (
		ftnlen)30);
	s_copy(gword, "generic word", (ftnlen)80, (ftnlen)12);
	s_copy(gword + 80, "generic words", (ftnlen)80, (ftnlen)13);
/* Writing concatenation */
	i__1[0] = 14, a__1[0] = "word of class ";
	i__1[1] = word_len, a__1[1] = word;
	s_cat(other, a__1, i__1, &c__2, (ftnlen)80);
/* Writing concatenation */
	i__1[0] = 15, a__1[0] = "words of class ";
	i__1[1] = word_len, a__1[1] = word;
	s_cat(other + 80, a__1, i__1, &c__2, (ftnlen)80);
	s_copy(int__, "integer", (ftnlen)80, (ftnlen)7);
	s_copy(int__ + 80, "integers", (ftnlen)80, (ftnlen)8);
	s_copy(dp, "number", (ftnlen)80, (ftnlen)6);
	s_copy(dp + 80, "numbers", (ftnlen)80, (ftnlen)7);
	s_copy(units, "unit specification", (ftnlen)80, (ftnlen)18);
	s_copy(units + 80, "unit specifications", (ftnlen)80, (ftnlen)19);
    }
    if (*num == 1) {
	number = 1;
    } else {
	number = 2;
    }
    b = 1;
    e = rtrim_(word, word_len);
    m2tran_(word, &b, &e, base, &key, &rtemp, word_len, (ftnlen)32);
    if (s_cmp(base, "@int", (ftnlen)32, (ftnlen)4) == 0) {
	s_copy(phrase, int__ + ((i__2 = number - 1) < 2 && 0 <= i__2 ? i__2 : 
		s_rnge("int", i__2, "m2clss_", (ftnlen)257)) * 80, phrase_len,
		 (ftnlen)80);
	if (rtemp) {
	    c__ = pos_(word, ":", &b, word_len, (ftnlen)1);
	    if (c__ == b + 1) {
		suffix_("less than or equal to #", &c__1, phrase, (ftnlen)23, 
			phrase_len);
		i__2 = b + 1;
		repmc_(phrase, "#", word + i__2, phrase, phrase_len, (ftnlen)
			1, e - 1 - i__2, phrase_len);
	    } else if (c__ == e - 1) {
		suffix_("greater than or equal to #", &c__1, phrase, (ftnlen)
			26, phrase_len);
		i__2 = b;
		repmc_(phrase, "#", word + i__2, phrase, phrase_len, (ftnlen)
			1, e - 2 - i__2, phrase_len);
	    } else {
		suffix_("between # and # (inclusive)", &c__1, phrase, (ftnlen)
			27, phrase_len);
		i__2 = b;
		repmc_(phrase, "#", word + i__2, phrase, phrase_len, (ftnlen)
			1, c__ - 1 - i__2, phrase_len);
		i__2 = c__;
		repmc_(phrase, "#", word + i__2, phrase, phrase_len, (ftnlen)
			1, e - 1 - i__2, phrase_len);
	    }
	}
	return 0;
    }
    if (s_cmp(base, "@number", (ftnlen)32, (ftnlen)7) == 0) {
	s_copy(phrase, dp + ((i__2 = number - 1) < 2 && 0 <= i__2 ? i__2 : 
		s_rnge("dp", i__2, "m2clss_", (ftnlen)283)) * 80, phrase_len, 
		(ftnlen)80);
	if (rtemp) {
	    i__2 = b + 1;
	    c__ = pos_(word, ":", &i__2, word_len, (ftnlen)1);
	    if (c__ == b + 1) {
		suffix_("less than or equal to #", &c__1, phrase, (ftnlen)23, 
			phrase_len);
		i__2 = b + 1;
		repmc_(phrase, "#", word + i__2, phrase, phrase_len, (ftnlen)
			1, e - 1 - i__2, phrase_len);
	    } else if (c__ == e - 1) {
		suffix_("greater than or equal to #", &c__1, phrase, (ftnlen)
			26, phrase_len);
		i__2 = b;
		repmc_(phrase, "#", word + i__2, phrase, phrase_len, (ftnlen)
			1, e - 2 - i__2, phrase_len);
	    } else {
		suffix_("between # and # (inclusive)", &c__1, phrase, (ftnlen)
			27, phrase_len);
		i__2 = b;
		repmc_(phrase, "#", word + i__2, phrase, phrase_len, (ftnlen)
			1, c__ - 1 - i__2, phrase_len);
		i__2 = c__;
		repmc_(phrase, "#", word + i__2, phrase, phrase_len, (ftnlen)
			1, e - 1 - i__2, phrase_len);
	    }
	}
	return 0;
    }
    if (s_cmp(base, "@unit", (ftnlen)32, (ftnlen)5) == 0) {
	s_copy(phrase, units + ((i__2 = number - 1) < 2 && 0 <= i__2 ? i__2 : 
		s_rnge("units", i__2, "m2clss_", (ftnlen)309)) * 80, 
		phrase_len, (ftnlen)80);
	if (rtemp) {
	    suffix_("with dimensions compatible with #", &c__1, phrase, (
		    ftnlen)33, phrase_len);
	    i__2 = b;
	    repmc_(phrase, "#", word + i__2, phrase, phrase_len, (ftnlen)1, e 
		    - 1 - i__2, phrase_len);
	}
	return 0;
    }
    if (s_cmp(base, "@alpha", (ftnlen)32, (ftnlen)6) == 0) {
	s_copy(phrase, alpha + ((i__2 = number - 1) < 2 && 0 <= i__2 ? i__2 : 
		s_rnge("alpha", i__2, "m2clss_", (ftnlen)325)) * 80, 
		phrase_len, (ftnlen)80);
    } else if (s_cmp(base, "@body", (ftnlen)32, (ftnlen)5) == 0) {
	s_copy(phrase, body + ((i__2 = number - 1) < 2 && 0 <= i__2 ? i__2 : 
		s_rnge("body", i__2, "m2clss_", (ftnlen)329)) * 80, 
		phrase_len, (ftnlen)80);
    } else if (s_cmp(base, "@day", (ftnlen)32, (ftnlen)4) == 0) {
	s_copy(phrase, day + ((i__2 = number - 1) < 2 && 0 <= i__2 ? i__2 : 
		s_rnge("day", i__2, "m2clss_", (ftnlen)333)) * 80, phrase_len,
		 (ftnlen)80);
    } else if (s_cmp(base, "@english", (ftnlen)32, (ftnlen)8) == 0) {
	s_copy(phrase, englsh + ((i__2 = number - 1) < 2 && 0 <= i__2 ? i__2 :
		 s_rnge("englsh", i__2, "m2clss_", (ftnlen)337)) * 80, 
		phrase_len, (ftnlen)80);
    } else if (s_cmp(base, "@epoch", (ftnlen)32, (ftnlen)6) == 0) {
	s_copy(phrase, epoch + ((i__2 = number - 1) < 2 && 0 <= i__2 ? i__2 : 
		s_rnge("epoch", i__2, "m2clss_", (ftnlen)341)) * 80, 
		phrase_len, (ftnlen)80);
    } else if (s_cmp(base, "@month", (ftnlen)32, (ftnlen)6) == 0) {
	s_copy(phrase, month + ((i__2 = number - 1) < 2 && 0 <= i__2 ? i__2 : 
		s_rnge("month", i__2, "m2clss_", (ftnlen)345)) * 80, 
		phrase_len, (ftnlen)80);
    } else if (s_cmp(base, "@name", (ftnlen)32, (ftnlen)5) == 0) {
	s_copy(phrase, name__ + ((i__2 = number - 1) < 2 && 0 <= i__2 ? i__2 :
		 s_rnge("name", i__2, "m2clss_", (ftnlen)349)) * 80, 
		phrase_len, (ftnlen)80);
    } else if (s_cmp(base, "@time", (ftnlen)32, (ftnlen)5) == 0) {
	s_copy(phrase, time + ((i__2 = number - 1) < 2 && 0 <= i__2 ? i__2 : 
		s_rnge("time", i__2, "m2clss_", (ftnlen)353)) * 80, 
		phrase_len, (ftnlen)80);
    } else if (s_cmp(base, "@year", (ftnlen)32, (ftnlen)5) == 0) {
	s_copy(phrase, year + ((i__2 = number - 1) < 2 && 0 <= i__2 ? i__2 : 
		s_rnge("year", i__2, "m2clss_", (ftnlen)357)) * 80, 
		phrase_len, (ftnlen)80);
    } else if (s_cmp(base, "@word", (ftnlen)32, (ftnlen)5) == 0) {
	s_copy(phrase, gword + ((i__2 = number - 1) < 2 && 0 <= i__2 ? i__2 : 
		s_rnge("gword", i__2, "m2clss_", (ftnlen)361)) * 80, 
		phrase_len, (ftnlen)80);
    } else {
	s_copy(phrase, other + ((i__2 = number - 1) < 2 && 0 <= i__2 ? i__2 : 
		s_rnge("other", i__2, "m2clss_", (ftnlen)365)) * 80, 
		phrase_len, (ftnlen)80);
    }
    if (rtemp) {
	suffix_("that matches the pattern '", &c__1, phrase, (ftnlen)26, 
		phrase_len);
	i__2 = b;
	suffix_(word + i__2, &c__0, phrase, e - 1 - i__2, phrase_len);
	suffix_("'", &c__0, phrase, (ftnlen)1, phrase_len);
    }
    return 0;
} /* m2clss_ */

