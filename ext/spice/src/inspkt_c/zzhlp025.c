/* zzhlp025.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZHLP025 ( private help text ) */
/* Subroutine */ int zzhlp025_(integer *begin, integer *finish, char *text, 
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
    s_copy(text + text_len * 2302, "specification.  After reading these sect"
	    "ions you should be able to make", text_len, (ftnlen)71);
    s_copy(text + text_len * 2303, "sense of any syntax specification presen"
	    "ted later in this User's Guide.", text_len, (ftnlen)71);
    s_copy(text + text_len * 2304, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2305, "Inspekt's command language is quite simp"
	    "le;  it does not require all of", text_len, (ftnlen)71);
    s_copy(text + text_len * 2306, "the features of Meta/2.   But since Insp"
	    "ekt is expected to grow, we've", text_len, (ftnlen)70);
    s_copy(text + text_len * 2307, "included a complete description of Meta/"
	    "2 so that you'll have a handy", text_len, (ftnlen)69);
    s_copy(text + text_len * 2308, "reference as this growth occurs.", 
	    text_len, (ftnlen)32);
    s_copy(text + text_len * 2309, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2310, "@section Keywords", text_len, (ftnlen)17);
    s_copy(text + text_len * 2311, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2312, "Each command you type at the prompt Insp"
	    "ekt> must begin with a keyword.", text_len, (ftnlen)71);
    s_copy(text + text_len * 2313, "Keywords define the structure of a comma"
	    "nd: for the most part, a command", text_len, (ftnlen)72);
    s_copy(text + text_len * 2314, "is a collection of keywords, some of whi"
	    "ch are modified by non-keyword", text_len, (ftnlen)70);
    s_copy(text + text_len * 2315, "arguments.  The keywords of a Meta/2 syn"
	    "tax description are the words", text_len, (ftnlen)69);
    s_copy(text + text_len * 2316, "that do not begin with one of the specia"
	    "l characters in the list below:", text_len, (ftnlen)71);
    s_copy(text + text_len * 2317, "@exliteral", text_len, (ftnlen)10);
    s_copy(text + text_len * 2318, "\"(\"   the left parenthesis", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 2319, "\"@\"   the \"at\" sign", text_len, (
	    ftnlen)19);
    s_copy(text + text_len * 2320, "\"|\"   the vertical bar", text_len, (
	    ftnlen)22);
    s_copy(text + text_len * 2321, "\"}\"    the right  brace", text_len, (
	    ftnlen)23);
    s_copy(text + text_len * 2322, "!endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2323, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2324, "In Inspekt all of the keywords are engli"
	    "sh words.", text_len, (ftnlen)49);
    s_copy(text + text_len * 2325, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2326, "The simplest Meta/2 expressions consist "
	    "entirely of keywords.  Some of", text_len, (ftnlen)70);
    s_copy(text + text_len * 2327, "Inspekt's commands that fall into this c"
	    "ategory are shown below.", text_len, (ftnlen)64);
    s_copy(text + text_len * 2328, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2329, "SHOW KERNELS", text_len, (ftnlen)12);
    s_copy(text + text_len * 2330, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2331, "SHOW SUMMARY", text_len, (ftnlen)12);
    s_copy(text + text_len * 2332, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2333, "SHOW INDEXES", text_len, (ftnlen)12);
    s_copy(text + text_len * 2334, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2335, "SET AUTOADJUST ON", text_len, (ftnlen)17);
    s_copy(text + text_len * 2336, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2337, "SET FORMAT TABULAR", text_len, (ftnlen)18)
	    ;
    s_copy(text + text_len * 2338, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2339, "SET FORMAT FLAGGED PRESERVED", text_len, (
	    ftnlen)28);
    s_copy(text + text_len * 2340, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2341, "SET FORMAT VERBATIM", text_len, (ftnlen)
	    19);
    s_copy(text + text_len * 2342, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2343, "SET TITLE FREQUENCY ALL", text_len, (
	    ftnlen)23);
    s_copy(text + text_len * 2344, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2345, "SET HEADER FREQUENCY FIRST", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 2346, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2347, "When you type a command, you may enter t"
	    "he keywords in upper, lower or", text_len, (ftnlen)70);
    s_copy(text + text_len * 2348, "mixed case.", text_len, (ftnlen)11);
    s_copy(text + text_len * 2349, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2350, "@section Class Templates", text_len, (
	    ftnlen)24);
    s_copy(text + text_len * 2351, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2352, "Keywords can be modified by non-keyword "
	    "values.  The values associated", text_len, (ftnlen)70);
    s_copy(text + text_len * 2353, "with a keyword always immediately follow"
	    " that keyword.  A collection of", text_len, (ftnlen)71);
    s_copy(text + text_len * 2354, "values is terminated by another keyword,"
	    " or by the end of the command.", text_len, (ftnlen)70);
    s_copy(text + text_len * 2355, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2356, "Class templates are used to indicate tha"
	    "t the values associated with a", text_len, (ftnlen)70);
    s_copy(text + text_len * 2357, "particular keyword belong to a class of "
	    "values.  For example the Meta/2", text_len, (ftnlen)71);
    s_copy(text + text_len * 2358, "expression of the rule that the keywords"
	    " SHOW COLUMN are to be followed", text_len, (ftnlen)71);
    s_copy(text + text_len * 2359, "by the name of a column looks like this.",
	     text_len, (ftnlen)40);
    s_copy(text + text_len * 2360, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2361, "SHOW COLUMN @name ...", text_len, (ftnlen)
	    21);
    s_copy(text + text_len * 2362, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2363, "The word, @name, indicates that the next"
	    " word in the command following", text_len, (ftnlen)70);
    s_copy(text + text_len * 2364, "COLUMN should begin with a letter follow"
	    "ed by characters from the", text_len, (ftnlen)65);
    s_copy(text + text_len * 2365, "collection:  upper and lower case letter"
	    "s, digits, the hyphen and the", text_len, (ftnlen)69);
    s_copy(text + text_len * 2366, "underscore.  Given this rule, a command "
	    "that begins", text_len, (ftnlen)51);
    s_copy(text + text_len * 2367, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2368, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2369, "SHOW COLUMN 13 ...", text_len, (ftnlen)18)
	    ;
    s_copy(text + text_len * 2370, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2371, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2372, "is clearly in error.", text_len, (ftnlen)
	    20);
    s_copy(text + text_len * 2373, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2374, "Class templates are very much like the w"
	    "ild card templates used in", text_len, (ftnlen)66);
    s_copy(text + text_len * 2375, "operating systems like Unix and VMS, exc"
	    "ept they are a little more", text_len, (ftnlen)66);
    s_copy(text + text_len * 2376, "specialized.  The class templates recogn"
	    "ized by Meta/2 are listed below", text_len, (ftnlen)71);
    s_copy(text + text_len * 2377, "along with descriptions of the words tha"
	    "t match them.  Class templates", text_len, (ftnlen)70);
    s_copy(text + text_len * 2378, "in a Meta/2 description begin with the"
	    " \"at\" character (@).", text_len, (ftnlen)58);
    s_copy(text + text_len * 2379, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2380, "@subsection number", text_len, (ftnlen)18)
	    ;
    s_copy(text + text_len * 2381, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2382, "This template matches any number.  Examp"
	    "les are", text_len, (ftnlen)47);
    s_copy(text + text_len * 2383, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2384, "1", text_len, (ftnlen)1);
    s_copy(text + text_len * 2385, "3.14", text_len, (ftnlen)4);
    s_copy(text + text_len * 2386, "0.07281D-10", text_len, (ftnlen)11);
    s_copy(text + text_len * 2387, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2388, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2389, "@subsection int", text_len, (ftnlen)15);
    s_copy(text + text_len * 2390, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2391, "This template matches only integer value"
	    "s.  Examples are", text_len, (ftnlen)56);
    s_copy(text + text_len * 2392, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2393, "-3", text_len, (ftnlen)2);
    s_copy(text + text_len * 2394, "26172771", text_len, (ftnlen)8);
    s_copy(text + text_len * 2395, "0.24E6", text_len, (ftnlen)6);
    s_copy(text + text_len * 2396, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2397, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2398, "@subsection word", text_len, (ftnlen)16);
    s_copy(text + text_len * 2399, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2400, "This template matches any string of cont"
	    "iguous, non-blank, printing", text_len, (ftnlen)67);
    s_copy(text + text_len * 2401, "characters.  Examples are", text_len, (
	    ftnlen)25);
    return 0;
} /* zzhlp025_ */

