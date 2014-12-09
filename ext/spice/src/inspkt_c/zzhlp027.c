/* zzhlp027.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZHLP027 ( private help text ) */
/* Subroutine */ int zzhlp027_(integer *begin, integer *finish, char *text, 
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
    s_copy(text + text_len * 2502, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2503, "(1:3)@name(John|Bobby|Teddy)", text_len, (
	    ftnlen)28);
    s_copy(text + text_len * 2504, "!endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2505, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2506, "@section Switches", text_len, (ftnlen)17);
    s_copy(text + text_len * 2507, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2508, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2509, "The construct", text_len, (ftnlen)13);
    s_copy(text + text_len * 2510, "@exliteral", text_len, (ftnlen)10);
    s_copy(text + text_len * 2511, "    (1:1){ NONE", text_len, (ftnlen)15);
    s_copy(text + text_len * 2512, "         | FIRST", text_len, (ftnlen)16);
    s_copy(text + text_len * 2513, "         | 1ST", text_len, (ftnlen)14);
    s_copy(text + text_len * 2514, "         | ALL", text_len, (ftnlen)14);
    s_copy(text + text_len * 2515, "         | EVERY @int(2:) }", text_len, (
	    ftnlen)27);
    s_copy(text + text_len * 2516, "!endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2517, "is called a switch.  It is the final con"
	    "struct that you need to know.", text_len, (ftnlen)69);
    s_copy(text + text_len * 2518, "Although it looks forbidding, it is real"
	    "ly quite simple.  A switch is", text_len, (ftnlen)69);
    s_copy(text + text_len * 2519, "a list of keyword-template phrases, sepa"
	    "rated by vertical bars, and", text_len, (ftnlen)67);
    s_copy(text + text_len * 2520, "surrounded by braces.  The left brace is"
	    " prefixed with a quantifier,", text_len, (ftnlen)68);
    s_copy(text + text_len * 2521, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2522, "   (n:m){ ... }", text_len, (ftnlen)15);
    s_copy(text + text_len * 2523, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2524, "Whenever you see a switch,  it means tha"
	    "t at least n and not more than", text_len, (ftnlen)70);
    s_copy(text + text_len * 2525, "m of the phrases separated by vertical b"
	    "ars must appear;  however, they", text_len, (ftnlen)71);
    s_copy(text + text_len * 2526, "may appear in any order.  Thus, the synt"
	    "ax", text_len, (ftnlen)42);
    s_copy(text + text_len * 2527, "@exliteral", text_len, (ftnlen)10);
    s_copy(text + text_len * 2528, "SET TITLE FREQUENCY (1:1){ NONE", 
	    text_len, (ftnlen)31);
    s_copy(text + text_len * 2529, "                         | FIRST", 
	    text_len, (ftnlen)32);
    s_copy(text + text_len * 2530, "                         | 1ST", text_len,
	     (ftnlen)30);
    s_copy(text + text_len * 2531, "                         | ALL", text_len,
	     (ftnlen)30);
    s_copy(text + text_len * 2532, "                         | EVERY @int(2:"
	    ") }", text_len, (ftnlen)43);
    s_copy(text + text_len * 2533, "!endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2534, "matches all of the following commands.", 
	    text_len, (ftnlen)38);
    s_copy(text + text_len * 2535, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2536, "SET TITLE FREQUENCY FIRST", text_len, (
	    ftnlen)25);
    s_copy(text + text_len * 2537, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2538, "SET TITLE FREQUENCY EVERY 3", text_len, (
	    ftnlen)27);
    s_copy(text + text_len * 2539, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2540, "SET TITLE FREQUENCY ALL", text_len, (
	    ftnlen)23);
    s_copy(text + text_len * 2541, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2542, "but does not match", text_len, (ftnlen)18)
	    ;
    s_copy(text + text_len * 2543, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2544, "SET TITLE FREQUENCY NONE FIRST ALL EVERY"
	    " @int(2:)", text_len, (ftnlen)49);
    s_copy(text + text_len * 2545, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2546, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2547, "When you see the special word", text_len, 
	    (ftnlen)29);
    s_copy(text + text_len * 2548, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2549, "@options", text_len, (ftnlen)8);
    s_copy(text + text_len * 2550, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2551, "within a switch, it means that the phras"
	    "es following the token are", text_len, (ftnlen)66);
    s_copy(text + text_len * 2552, "optional, whereas the phrases preceding "
	    "the token are required (again,", text_len, (ftnlen)70);
    s_copy(text + text_len * 2553, "the phrases may appear in any order).  F"
	    "or example the construct", text_len, (ftnlen)64);
    s_copy(text + text_len * 2554, "@exliteral", text_len, (ftnlen)10);
    s_copy(text + text_len * 2555, "   (2:3){         WIDTH  @int(40:132)", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 2556, "        |         HEIGHT @int(22:)", 
	    text_len, (ftnlen)34);
    s_copy(text + text_len * 2557, "        | @options", text_len, (ftnlen)18)
	    ;
    s_copy(text + text_len * 2558, "        |         TITLE (1:3)@word }", 
	    text_len, (ftnlen)36);
    s_copy(text + text_len * 2559, "!endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2560, "means that the phrases beginning with th"
	    "e keywords WIDTH and HEIGHT must", text_len, (ftnlen)72);
    s_copy(text + text_len * 2561, "appear, while the phrase beginning with "
	    "TITLE is optional.", text_len, (ftnlen)58);
    s_copy(text + text_len * 2562, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2563, "@subsection Nesting Switches", text_len, (
	    ftnlen)28);
    s_copy(text + text_len * 2564, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2565, "Switches cannot be nested.  The construct"
	    , text_len, (ftnlen)41);
    s_copy(text + text_len * 2566, "@exliteral", text_len, (ftnlen)10);
    s_copy(text + text_len * 2567, "   (a:b){ ...", text_len, (ftnlen)13);
    s_copy(text + text_len * 2568, "        | (c:d){ ...  }", text_len, (
	    ftnlen)23);
    s_copy(text + text_len * 2569, "        }", text_len, (ftnlen)9);
    s_copy(text + text_len * 2570, "!endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2571, "is illegal.", text_len, (ftnlen)11);
    s_copy(text + text_len * 2572, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2573, "@section Examples", text_len, (ftnlen)17);
    s_copy(text + text_len * 2574, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2575, "Given the syntax description", text_len, (
	    ftnlen)28);
    s_copy(text + text_len * 2576, "@exliteral", text_len, (ftnlen)10);
    s_copy(text + text_len * 2577, "   SET FORMAT (0:1){ SPACED  | MARKED } "
	    "TABULAR", text_len, (ftnlen)47);
    s_copy(text + text_len * 2578, "              (0:1){ PRESERVED }", 
	    text_len, (ftnlen)32);
    s_copy(text + text_len * 2579, "!endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2580, "Convince yourself that the following are"
	    " all valid  commands.", text_len, (ftnlen)61);
    s_copy(text + text_len * 2581, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2582, "SET FORMAT TABULAR;", text_len, (ftnlen)
	    19);
    s_copy(text + text_len * 2583, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2584, "SET FORMAT SPACED TABULAR;", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 2585, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2586, "SET FORMAT MARKED TABULAR PRESERVED;", 
	    text_len, (ftnlen)36);
    s_copy(text + text_len * 2587, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2588, "SET FORMAT TABULAR PRESERVED;", text_len, 
	    (ftnlen)29);
    s_copy(text + text_len * 2589, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2590, "SET FORMAT SPACE TABULAR PRESERVED;", 
	    text_len, (ftnlen)35);
    s_copy(text + text_len * 2591, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2592, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2593, "Convince yourself that the following are"
	    " not valid commands.", text_len, (ftnlen)60);
    s_copy(text + text_len * 2594, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2595, "SET FORMAT SPACED;", text_len, (ftnlen)18)
	    ;
    s_copy(text + text_len * 2596, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2597, "SET FORMAT PRESERVED TABULAR;", text_len, 
	    (ftnlen)29);
    s_copy(text + text_len * 2598, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2599, "SET FORMAT MARKED PRESERVED;", text_len, (
	    ftnlen)28);
    s_copy(text + text_len * 2600, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2601, "@@Syntax Description Language", text_len, 
	    (ftnlen)29);
    return 0;
} /* zzhlp027_ */

