/* zzhlp024.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZHLP024 ( private help text ) */
/* Subroutine */ int zzhlp024_(integer *begin, integer *finish, char *text, 
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
    s_copy(text + text_len * 2208, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2209, "First, time strings need to be input as "
	    "strings.", text_len, (ftnlen)48);
    s_copy(text + text_len * 2210, "Strings must be enclosed", text_len, (
	    ftnlen)24);
    s_copy(text + text_len * 2211, "in quotes. The condition", text_len, (
	    ftnlen)24);
    s_copy(text + text_len * 2212, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2213, "TIME LT 1 JAN 1995", text_len, (ftnlen)18)
	    ;
    s_copy(text + text_len * 2214, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2215, "will not be recognized if it is part of "
	    "a SELECT command.  However,", text_len, (ftnlen)67);
    s_copy(text + text_len * 2216, "once you place quotes around the time, t"
	    "he time string will be", text_len, (ftnlen)62);
    s_copy(text + text_len * 2217, "recognized", text_len, (ftnlen)10);
    s_copy(text + text_len * 2218, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2219, "TIME LT \"1 JAN 1995\"", text_len, (
	    ftnlen)20);
    s_copy(text + text_len * 2220, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2221, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2222, "A wide variety of time formats are allow"
	    "ed as input to Inspekt.", text_len, (ftnlen)63);
    s_copy(text + text_len * 2223, "These formats are listed below.", 
	    text_len, (ftnlen)31);
    s_copy(text + text_len * 2224, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2225, "@subsection Spacecraft Clock", text_len, (
	    ftnlen)28);
    s_copy(text + text_len * 2226, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2227, "MO   SCLK mars observer spacecraft clock"
	    " string", text_len, (ftnlen)47);
    s_copy(text + text_len * 2228, "GLL  SCLK galileo spacecraft clock string"
	    , text_len, (ftnlen)41);
    s_copy(text + text_len * 2229, "VGR1 SCLK voyager 1 spacecraft clock str"
	    "ing", text_len, (ftnlen)43);
    s_copy(text + text_len * 2230, "VGR2 SCLK voyager 2 spacecraft clock str"
	    "ing", text_len, (ftnlen)43);
    s_copy(text + text_len * 2231, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2232, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2233, "To use these formats you must have eithe"
	    "r specified SCLK to be an", text_len, (ftnlen)65);
    s_copy(text + text_len * 2234, "appropriate  kernel (see Getting Started"
	    ") or have loaded an appropriate", text_len, (ftnlen)71);
    s_copy(text + text_len * 2235, "kernel via the LOAD SCLK KERNEL command.",
	     text_len, (ftnlen)40);
    s_copy(text + text_len * 2236, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2237, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2238, "@subsection ISO Formats", text_len, (
	    ftnlen)23);
    s_copy(text + text_len * 2239, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2240, "The International Standards Organization"
	    " (ISO) time format is used  by", text_len, (ftnlen)70);
    s_copy(text + text_len * 2241, "many NASA flight projects.", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 2242, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2243, "YYYY-MM-DDTHR:MN:SC    ISO UTC Calendar "
	    "format", text_len, (ftnlen)46);
    s_copy(text + text_len * 2244, "YYYY-DDDTHR:MN:SC      ISO UTC Day of ye"
	    "ar format", text_len, (ftnlen)49);
    s_copy(text + text_len * 2245, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2246, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2247, "@subsection Generic", text_len, (ftnlen)
	    19);
    s_copy(text + text_len * 2248, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2249, "In these formats Month stands for the mo"
	    "nth spelled out to 3 or more", text_len, (ftnlen)68);
    s_copy(text + text_len * 2250, "letters, e.g. Jan, Janu, Janua, etc.  Al"
	    "so note that where spaces have", text_len, (ftnlen)70);
    s_copy(text + text_len * 2251, "been used to separate the components of "
	    "the date you may also use a", text_len, (ftnlen)67);
    s_copy(text + text_len * 2252, "comma or a slash (i.e. the solidus (/) )"
	    ".  All times are UTC.", text_len, (ftnlen)61);
    s_copy(text + text_len * 2253, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2254, "MONTH DD YYYY  HR:MN:SC.##...#", text_len,
	     (ftnlen)30);
    s_copy(text + text_len * 2255, "DD MONTH YYYY  HR:MN:SC.##...#", text_len,
	     (ftnlen)30);
    s_copy(text + text_len * 2256, "YYYY DD MONTH  HR:MN:SC.##...#", text_len,
	     (ftnlen)30);
    s_copy(text + text_len * 2257, "YYYY MONTH DD  HR:MN:SC.##...#", text_len,
	     (ftnlen)30);
    s_copy(text + text_len * 2258, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2259, "YYYY MM DD HR:MN:SC.##...#", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 2260, "MM DD YYYY HR:MN:SC.##...#", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 2261, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2262, "JD244xxxx.xx...x", text_len, (ftnlen)16);
    s_copy(text + text_len * 2263, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2264, "@@Specifying Times", text_len, (ftnlen)18)
	    ;
    s_copy(text + text_len * 2265, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 2266, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 2267, "Where Clause", text_len, (ftnlen)12);
    finish[63] = 2268;
    begin[64] = 2269;
    s_copy(text + text_len * 2268, "A symbol is a word that begins with a le"
	    "tter of the alphabet", text_len, (ftnlen)60);
    s_copy(text + text_len * 2269, "and does not end with a question mark.  "
	    "It must be 32 or", text_len, (ftnlen)56);
    s_copy(text + text_len * 2270, "fewer characters in length.  Moreover, y"
	    "ou must specifically", text_len, (ftnlen)60);
    s_copy(text + text_len * 2271, "designate this word to be a symbol via t"
	    "he DEFINE command.", text_len, (ftnlen)58);
    s_copy(text + text_len * 2272, "The define command associates a value wi"
	    "th the symbol.", text_len, (ftnlen)54);
    s_copy(text + text_len * 2273, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2274, "When you type a command in Inspekt that "
	    "contains a symbol,", text_len, (ftnlen)58);
    s_copy(text + text_len * 2275, "the symbol is replaced by its associated"
	    " value.  The command", text_len, (ftnlen)60);
    s_copy(text + text_len * 2276, "is then re-examined and any remaining sy"
	    "mbols are replaced", text_len, (ftnlen)58);
    s_copy(text + text_len * 2277, "by their associated values.", text_len, (
	    ftnlen)27);
    s_copy(text + text_len * 2278, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2279, "Symbols allow you to customize your Insp"
	    "ekt environment.  In", text_len, (ftnlen)60);
    s_copy(text + text_len * 2280, "addition they allow you to greatly reduc"
	    "e the amount of typing", text_len, (ftnlen)62);
    s_copy(text + text_len * 2281, "you need to do in order to issue frequen"
	    "tly occurring groups", text_len, (ftnlen)60);
    s_copy(text + text_len * 2282, "of words.", text_len, (ftnlen)9);
    s_copy(text + text_len * 2283, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2284, "Words that are surrounded by any of the "
	    "characters (\"'@) are not", text_len, (ftnlen)64);
    s_copy(text + text_len * 2285, "regarded as symbols and are processed as"
	    " they appear.", text_len, (ftnlen)53);
    s_copy(text + text_len * 2286, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2287, "Symbols are case insensitive in Inspekt."
	    "  If you define \"SPUD\"", text_len, (ftnlen)62);
    s_copy(text + text_len * 2288, "to be a symbol then \"spud\", \"Spud\","
	    " \"sPud\", etc. will all be", text_len, (ftnlen)60);
    s_copy(text + text_len * 2289, "interpreted as SPUD.", text_len, (ftnlen)
	    20);
    s_copy(text + text_len * 2290, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2291, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2292, "You may not define EDIT, DO, RECALL, STA"
	    "RT, STOP, DEFINE, ECHO", text_len, (ftnlen)62);
    s_copy(text + text_len * 2293, "@@Symbol", text_len, (ftnlen)8);
    s_copy(text + text_len * 2294, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 2295, "Help", text_len, (ftnlen)4);
    finish[64] = 2296;
    begin[65] = 2297;
    s_copy(text + text_len * 2296, "The language you use to communicate with"
	    " Inspekt is a word oriented", text_len, (ftnlen)67);
    s_copy(text + text_len * 2297, "language.  With one exception the smalle"
	    "st significant component of a", text_len, (ftnlen)69);
    s_copy(text + text_len * 2298, "command is a word.  The words in a comma"
	    "nd must match a pattern that is", text_len, (ftnlen)71);
    s_copy(text + text_len * 2299, "called the syntax of the command. The sy"
	    "ntax of the commands you type", text_len, (ftnlen)69);
    s_copy(text + text_len * 2300, "at the prompt Inspekt> can be expressed "
	    "in a language called Meta/2.", text_len, (ftnlen)68);
    s_copy(text + text_len * 2301, "The sections below describe the various "
	    "constructs that make up a Meta/2", text_len, (ftnlen)72);
    return 0;
} /* zzhlp024_ */

