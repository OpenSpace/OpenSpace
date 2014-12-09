/* zzhlp023.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZHLP023 ( private help text ) */
/* Subroutine */ int zzhlp023_(integer *begin, integer *finish, char *text, 
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
    finish[59] = 2120;
    begin[60] = 2121;
    s_copy(text + text_len * 2120, "To see a summary of the characteristics "
	    "of all columns for all loaded events ker", text_len, (ftnlen)80);
    s_copy(text + text_len * 2121, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2122, "SHOW SUMMARY;", text_len, (ftnlen)13);
    s_copy(text + text_len * 2123, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2124, "Inspekt will display an alphabetical lis"
	    "ting of all columns", text_len, (ftnlen)59);
    s_copy(text + text_len * 2125, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2126, "@@SHOW SUMMARY     ...", text_len, (
	    ftnlen)22);
    s_copy(text + text_len * 2127, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 2128, "Help", text_len, (ftnlen)4);
    finish[60] = 2129;
    begin[61] = 2130;
    s_copy(text + text_len * 2129, "In addition to symbols you define, there"
	    " are special symbols called \"queries\"", text_len, (ftnlen)77);
    s_copy(text + text_len * 2130, "that have no permanent value.  You suppl"
	    "y the value of a query when a command", text_len, (ftnlen)77);
    s_copy(text + text_len * 2131, "containing it is issued.  A query is any"
	    " word beginning with a letter", text_len, (ftnlen)69);
    s_copy(text + text_len * 2132, "and ending with a question mark (?).  Wh"
	    "enever, such a word is encountered in", text_len, (ftnlen)77);
    s_copy(text + text_len * 2133, "a command, Inspekt asks you to supply a "
	    "value for the query.  You should enter", text_len, (ftnlen)78);
    s_copy(text + text_len * 2134, "the value followed by a semicolon (;).", 
	    text_len, (ftnlen)38);
    s_copy(text + text_len * 2135, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2136, "Here's an example.  Suppose you routinel"
	    "y issue some select command and that the", text_len, (ftnlen)80);
    s_copy(text + text_len * 2137, "only portion that changes is the constra"
	    "int upon the column time.", text_len, (ftnlen)65);
    s_copy(text + text_len * 2138, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2139, "SELECT TIME, COMMENTS", text_len, (ftnlen)
	    21);
    s_copy(text + text_len * 2140, "FROM EVENTLIST", text_len, (ftnlen)14);
    s_copy(text + text_len * 2141, "WHERE TIME variable condition", text_len, 
	    (ftnlen)29);
    s_copy(text + text_len * 2142, "ORDER BY TIME;", text_len, (ftnlen)14);
    s_copy(text + text_len * 2143, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2144, "Define the following symbols", text_len, (
	    ftnlen)28);
    s_copy(text + text_len * 2145, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2146, "DEFINE STDSEL SELECT TIME, COMMENTS FORM"
	    " EVENTLIST WHERE TIME", text_len, (ftnlen)61);
    s_copy(text + text_len * 2147, "       After_Before_Tween? ORDER BY TIME;"
	    , text_len, (ftnlen)41);
    s_copy(text + text_len * 2148, "DEFINE AFTER  GT TIME?", text_len, (
	    ftnlen)22);
    s_copy(text + text_len * 2149, "DEFINE BEFORE LT TIME?", text_len, (
	    ftnlen)22);
    s_copy(text + text_len * 2150, "DEFINE TWEEN  BETWEEN FIRST_TIME? AND LA"
	    "ST_TIME?", text_len, (ftnlen)48);
    s_copy(text + text_len * 2151, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2152, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2153, "Here's what happens when we now type STD"
	    "SEL;", text_len, (ftnlen)44);
    s_copy(text + text_len * 2154, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2155, "Inspekt> STDSEL;", text_len, (ftnlen)16);
    s_copy(text + text_len * 2156, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2157, "Inspekt responds with the following prom"
	    "pt.", text_len, (ftnlen)43);
    s_copy(text + text_len * 2158, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2159, "Enter value for After_Before_Tween >", 
	    text_len, (ftnlen)36);
    s_copy(text + text_len * 2160, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2161, "You can type anything at this prompt. Ho"
	    "wever the prompt suggests you should", text_len, (ftnlen)76);
    s_copy(text + text_len * 2162, "pick one of the symbols you define earli"
	    "er.  Type \"before\" followed by a semi-co", text_len, (ftnlen)80)
	    ;
    s_copy(text + text_len * 2163, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2164, "Enter value for After_Before_Tween > bef"
	    "ore;", text_len, (ftnlen)44);
    s_copy(text + text_len * 2165, "Enter value for TIME >", text_len, (
	    ftnlen)22);
    s_copy(text + text_len * 2166, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2167, "Now enter some time (be sure to put it i"
	    "n quotes) and follow this with a semi-co", text_len, (ftnlen)80);
    s_copy(text + text_len * 2168, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2169, "Enter value for After_Before_Tween > bef"
	    "ore;", text_len, (ftnlen)44);
    s_copy(text + text_len * 2170, "Enter value for TIME > \"1 jan 1995\";", 
	    text_len, (ftnlen)36);
    s_copy(text + text_len * 2171, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2172, "The effect of these three entries is", 
	    text_len, (ftnlen)36);
    s_copy(text + text_len * 2173, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2174, "SELECT TIME, COMMENTS FROM EVENTLIST", 
	    text_len, (ftnlen)36);
    s_copy(text + text_len * 2175, "WHERE TIME LT \"1 jan 1995\"", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 2176, "ORDER BY TIME;", text_len, (ftnlen)14);
    s_copy(text + text_len * 2177, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2178, "By creative use of symbols and queries y"
	    "ou can greatly alter", text_len, (ftnlen)60);
    s_copy(text + text_len * 2179, "your view of Inspekt and greatly ease th"
	    "e entry of frequently", text_len, (ftnlen)61);
    s_copy(text + text_len * 2180, "typed commands.", text_len, (ftnlen)15);
    s_copy(text + text_len * 2181, "@@Special Symbols --- Queries", text_len, 
	    (ftnlen)29);
    s_copy(text + text_len * 2182, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 2183, "Help", text_len, (ftnlen)4);
    finish[61] = 2184;
    begin[62] = 2185;
    s_copy(text + text_len * 2184, "When you create SELECT commands you will"
	    " often need to", text_len, (ftnlen)54);
    s_copy(text + text_len * 2185, "compare a character column to some speci"
	    "fied string. To", text_len, (ftnlen)55);
    s_copy(text + text_len * 2186, "specify a string you must enclose it in "
	    "either single (') or", text_len, (ftnlen)60);
    s_copy(text + text_len * 2187, "double (\") quotes.  (Note that these ar"
	    "e both single characters.", text_len, (ftnlen)64);
    s_copy(text + text_len * 2188, "You cannot use '' in place of \".) For e"
	    "xample, you might want to", text_len, (ftnlen)64);
    s_copy(text + text_len * 2189, "find all of the rows for which a charact"
	    "er column begins with", text_len, (ftnlen)61);
    s_copy(text + text_len * 2190, "the letter 'A'.  To specify this conditi"
	    "on in a query you", text_len, (ftnlen)57);
    s_copy(text + text_len * 2191, "would type:", text_len, (ftnlen)11);
    s_copy(text + text_len * 2192, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2193, "column GE 'A' AND column LE 'B'", 
	    text_len, (ftnlen)31);
    s_copy(text + text_len * 2194, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2195, "If you need to put a quote into a string"
	    ", you type double the", text_len, (ftnlen)61);
    s_copy(text + text_len * 2196, "quote as you would in a FORTRAN string. "
	    " For example the word", text_len, (ftnlen)61);
    s_copy(text + text_len * 2197, "DOESN'T would be typed as", text_len, (
	    ftnlen)25);
    s_copy(text + text_len * 2198, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2199, "'DOESN''T'", text_len, (ftnlen)10);
    s_copy(text + text_len * 2200, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2201, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2202, "@@Specifying Strings", text_len, (ftnlen)
	    20);
    s_copy(text + text_len * 2203, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 2204, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 2205, "Specifying Times", text_len, (ftnlen)16);
    finish[62] = 2206;
    begin[63] = 2207;
    s_copy(text + text_len * 2206, "Although Inspekt can display times in al"
	    "most any format, the set of", text_len, (ftnlen)67);
    s_copy(text + text_len * 2207, "inputs is necessarily more restrictive.", 
	    text_len, (ftnlen)39);
    return 0;
} /* zzhlp023_ */

