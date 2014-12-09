/* zzhlp026.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZHLP026 ( private help text ) */
/* Subroutine */ int zzhlp026_(integer *begin, integer *finish, char *text, 
	ftnlen text_len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer i__, j;
    extern /* Subroutine */ int repmc_(char *, char *, char *, char *, ftnlen,
	     ftnlen, ftnlen, ftnlen);

/* $ Abstract */

/*     Fill out a portion of the help text needed by percy. */

/*     Private routine intended solely for the support of Inspekt */

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

/*     PRIVATE */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     BEGIN      O   Indexes of begins of text help */
/*     FINISH     O   Indexes of ends of text help */
/*     TEXT       O   A block of text help. */

/* $ Exceptions */

/*     Error free. */

/* $ Particulars */

/*     This routine simply fills begin and end markers as well */
/*     as actual text for a block of help text for percy. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    Inspekt Version 1.0.0, 1-AUG-1997 (WLT) */


/* -& */
    j = finish[0];
    i__ = begin[0];
    finish[0] = j;
    begin[0] = i__;
    repmc_(text, "*", "*", text, text_len, (ftnlen)1, (ftnlen)1, text_len);
    s_copy(text + text_len * 2402, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2403, "alpine", text_len, (ftnlen)6);
    s_copy(text + text_len * 2404, "/u/user/naif/etc/data/spam", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 2405, "^&HANNk228***(JASNSK", text_len, (ftnlen)
	    20);
    s_copy(text + text_len * 2406, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2407, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2408, "@subsection name", text_len, (ftnlen)16);
    s_copy(text + text_len * 2409, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2410, "This template matches any word that begi"
	    "ns with a letter and contains", text_len, (ftnlen)69);
    s_copy(text + text_len * 2411, "from one  to 32 letters, numbers, unders"
	    "cores, and hyphens.  Examples", text_len, (ftnlen)69);
    s_copy(text + text_len * 2412, "are", text_len, (ftnlen)3);
    s_copy(text + text_len * 2413, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2414, "Andrea", text_len, (ftnlen)6);
    s_copy(text + text_len * 2415, "BRORSEN-METCALF", text_len, (ftnlen)15);
    s_copy(text + text_len * 2416, "COMMAND_STEM", text_len, (ftnlen)12);
    s_copy(text + text_len * 2417, "X11J9", text_len, (ftnlen)5);
    s_copy(text + text_len * 2418, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2419, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2420, "@subsection calendar", text_len, (ftnlen)
	    20);
    s_copy(text + text_len * 2421, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2422, "This template matches a sequence of word"
	    "s that make up a calendar date.", text_len, (ftnlen)71);
    s_copy(text + text_len * 2423, "The rules for matching this template are"
	    " somewhat complicated:  for the", text_len, (ftnlen)71);
    s_copy(text + text_len * 2424, "most part, any unambiguous format will b"
	    "e accepted.  Examples are", text_len, (ftnlen)65);
    s_copy(text + text_len * 2425, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2426, "JAN 1, 1992 12:28:28", text_len, (ftnlen)
	    20);
    s_copy(text + text_len * 2427, "1992-3-18 18:28", text_len, (ftnlen)15);
    s_copy(text + text_len * 2428, "5 APR 1993 18:00:00.289", text_len, (
	    ftnlen)23);
    s_copy(text + text_len * 2429, "12-1-1995", text_len, (ftnlen)9);
    s_copy(text + text_len * 2430, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2431, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2432, "@subsection Template Quantifiers", 
	    text_len, (ftnlen)32);
    s_copy(text + text_len * 2433, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2434, "The construct", text_len, (ftnlen)13);
    s_copy(text + text_len * 2435, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2436, "FORMAT (1:8)@word", text_len, (ftnlen)17);
    s_copy(text + text_len * 2437, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2438, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2439, "matches the keyword FORMAT followed by b"
	    "etween one and eight words.", text_len, (ftnlen)67);
    s_copy(text + text_len * 2440, "The quantifier", text_len, (ftnlen)14);
    s_copy(text + text_len * 2441, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2442, "(n:m)", text_len, (ftnlen)5);
    s_copy(text + text_len * 2443, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2444, "when prefixed to any class template has "
	    "similar meaning.  The lower", text_len, (ftnlen)67);
    s_copy(text + text_len * 2445, "bound is always present, and is always p"
	    "ositive.  The upper bound is", text_len, (ftnlen)68);
    s_copy(text + text_len * 2446, "optional:  for example, template", 
	    text_len, (ftnlen)32);
    s_copy(text + text_len * 2447, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2448, "(2:)@int", text_len, (ftnlen)8);
    s_copy(text + text_len * 2449, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2450, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2451, "matches any sequence of two or more inte"
	    "ger words.", text_len, (ftnlen)50);
    s_copy(text + text_len * 2452, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2453, "@subsection Numeric Qualifiers", text_len,
	     (ftnlen)30);
    s_copy(text + text_len * 2454, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2455, "The construct", text_len, (ftnlen)13);
    s_copy(text + text_len * 2456, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2457, "WIDTH @int(8:80)", text_len, (ftnlen)16);
    s_copy(text + text_len * 2458, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2459, "matches the keyword WIDTH followed by an"
	    " integer between 8 and 80", text_len, (ftnlen)65);
    s_copy(text + text_len * 2460, "inclusive.  The qualifier", text_len, (
	    ftnlen)25);
    s_copy(text + text_len * 2461, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2462, "(n:m)", text_len, (ftnlen)5);
    s_copy(text + text_len * 2463, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2464, "when suffixed to any numeric class templ"
	    "ate has similar meaning.  Both", text_len, (ftnlen)70);
    s_copy(text + text_len * 2465, "bounds are optional:  for example the te"
	    "mplates", text_len, (ftnlen)47);
    s_copy(text + text_len * 2466, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2467, "@number(2:)", text_len, (ftnlen)11);
    s_copy(text + text_len * 2468, "@number(:1000)", text_len, (ftnlen)14);
    s_copy(text + text_len * 2469, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2470, "are matched by  numeric words whose valu"
	    "es are greater than or equal to", text_len, (ftnlen)71);
    s_copy(text + text_len * 2471, "two and less than or equal to 1000 respe"
	    "ctively.", text_len, (ftnlen)48);
    s_copy(text + text_len * 2472, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2473, "@subsection Character Qualifiers", 
	    text_len, (ftnlen)32);
    s_copy(text + text_len * 2474, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2475, "The construct", text_len, (ftnlen)13);
    s_copy(text + text_len * 2476, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2477, "DIRECTORY @word([*]|*%:[*]|*:)", text_len,
	     (ftnlen)30);
    s_copy(text + text_len * 2478, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2479, "matches any word that matches one of the"
	    " individual wildcard templates", text_len, (ftnlen)70);
    s_copy(text + text_len * 2480, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2481, "[*]", text_len, (ftnlen)3);
    s_copy(text + text_len * 2482, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2483, "*%:[*]", text_len, (ftnlen)6);
    s_copy(text + text_len * 2484, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2485, "*:", text_len, (ftnlen)2);
    s_copy(text + text_len * 2486, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2487, "The wildcard characters (asterisk and pe"
	    "rcent sign)  match any substring", text_len, (ftnlen)72);
    s_copy(text + text_len * 2488, "and any character respectively.  The qua"
	    "lifier", text_len, (ftnlen)46);
    s_copy(text + text_len * 2489, "@exliteral", text_len, (ftnlen)10);
    s_copy(text + text_len * 2490, "(t1|...|tn)", text_len, (ftnlen)11);
    s_copy(text + text_len * 2491, "!endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2492, "when suffixed to any character class tem"
	    "plate has a similar meaning.", text_len, (ftnlen)68);
    s_copy(text + text_len * 2493, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2494, "@section Combining Quantifiers with Qual"
	    "ifiers", text_len, (ftnlen)46);
    s_copy(text + text_len * 2495, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2496, "Quantifiers and qualifiers may be combin"
	    "ed in any combination.  The", text_len, (ftnlen)67);
    s_copy(text + text_len * 2497, "following are all valid class templates.",
	     text_len, (ftnlen)40);
    s_copy(text + text_len * 2498, "@exliteral", text_len, (ftnlen)10);
    s_copy(text + text_len * 2499, "@int(-5:5)", text_len, (ftnlen)10);
    s_copy(text + text_len * 2500, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2501, "(3:)@name(A*|B*|*X)", text_len, (ftnlen)
	    19);
    return 0;
} /* zzhlp026_ */

