/* zzhlp032.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZHLP032 ( private help text ) */
/* Subroutine */ int zzhlp032_(integer *begin, integer *finish, char *text, 
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
    s_copy(text + text_len * 2978, "SET PAGE TITLE @MYFMT@;", text_len, (
	    ftnlen)23);
    s_copy(text + text_len * 2979, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2980, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2981, "@subsection Examining Symbols", text_len, 
	    (ftnlen)29);
    s_copy(text + text_len * 2982, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2983, "After working in Inspekt for some time, "
	    "you may want to see what", text_len, (ftnlen)64);
    s_copy(text + text_len * 2984, "symbols you've created.  Type", text_len, 
	    (ftnlen)29);
    s_copy(text + text_len * 2985, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2986, "SHOW SYMBOL pattern", text_len, (ftnlen)
	    19);
    s_copy(text + text_len * 2987, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2988, "to see the definition and full evaluatio"
	    "n of all symbols whose names", text_len, (ftnlen)68);
    s_copy(text + text_len * 2989, "match the supplied pattern.", text_len, (
	    ftnlen)27);
    s_copy(text + text_len * 2990, "@@Using Symbols", text_len, (ftnlen)15);
    s_copy(text + text_len * 2991, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 2992, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 2993, "Echoing Translated Commands", text_len, (
	    ftnlen)27);
    s_copy(text + text_len * 2994, "Collecting Commands In Files", text_len, (
	    ftnlen)28);
    s_copy(text + text_len * 2995, "Special Symbols --- Queries", text_len, (
	    ftnlen)27);
    finish[73] = 2996;
    begin[74] = 2997;
    s_copy(text + text_len * 2996, "The VERBATIM format allows you to captur"
	    "e in a report the explicit", text_len, (ftnlen)66);
    s_copy(text + text_len * 2997, "components that were supplied to each co"
	    "lumn at the the time an", text_len, (ftnlen)63);
    s_copy(text + text_len * 2998, "event was recorded.  Each component will"
	    " appear on a separate line", text_len, (ftnlen)66);
    s_copy(text + text_len * 2999, "in the report.  An example appears below:"
	    , text_len, (ftnlen)41);
    s_copy(text + text_len * 3000, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 3001, "--- TIME     ---", text_len, (ftnlen)16);
    s_copy(text + text_len * 3002, "JAN 25 18:29:59", text_len, (ftnlen)15);
    s_copy(text + text_len * 3003, "--- ACTIVITY ---", text_len, (ftnlen)16);
    s_copy(text + text_len * 3004, "OBSERVATION", text_len, (ftnlen)11);
    s_copy(text + text_len * 3005, "--- SUBCLASS ---", text_len, (ftnlen)16);
    s_copy(text + text_len * 3006, "GOOD AIR QUALITY", text_len, (ftnlen)16);
    s_copy(text + text_len * 3007, "--- COMMENTS ---", text_len, (ftnlen)16);
    s_copy(text + text_len * 3008, "The air quality is good.", text_len, (
	    ftnlen)24);
    s_copy(text + text_len * 3009, "Smog has not been a problem for several "
	    "days now.", text_len, (ftnlen)49);
    s_copy(text + text_len * 3010, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 3011, "@@Verbatim Format", text_len, (ftnlen)17);
    s_copy(text + text_len * 3012, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 3013, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 3014, "Reports", text_len, (ftnlen)7);
    finish[74] = 3015;
    begin[75] = 3016;
    s_copy(text + text_len * 3015, "The \"WHERE\" clause of a SELECT command"
	    " allows you to narrow focus on a", text_len, (ftnlen)70);
    s_copy(text + text_len * 3016, "particular set of events.  The form of t"
	    "he WHERE-clause is:", text_len, (ftnlen)59);
    s_copy(text + text_len * 3017, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 3018, "WHERE condition", text_len, (ftnlen)15);
    s_copy(text + text_len * 3019, "AND/OR (NOT) condition", text_len, (
	    ftnlen)22);
    s_copy(text + text_len * 3020, "         ...", text_len, (ftnlen)12);
    s_copy(text + text_len * 3021, "AND/OR (NOT) condition", text_len, (
	    ftnlen)22);
    s_copy(text + text_len * 3022, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 3023, "With one exception", text_len, (ftnlen)18)
	    ;
    s_copy(text + text_len * 3024, "A condition is an expression of the form:"
	    , text_len, (ftnlen)41);
    s_copy(text + text_len * 3025, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 3026, "column_name  binary_relation  value/colu"
	    "mn", text_len, (ftnlen)42);
    s_copy(text + text_len * 3027, "or", text_len, (ftnlen)2);
    s_copy(text + text_len * 3028, "column_name  [NOT] BETWEEN value1/column"
	    " AND value2/column", text_len, (ftnlen)58);
    s_copy(text + text_len * 3029, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 3030, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 3031, "Only", text_len, (ftnlen)4);
    s_copy(text + text_len * 3032, "names of columns from loaded kernels may"
	    " be specified. The columns", text_len, (ftnlen)66);
    s_copy(text + text_len * 3033, "must be scalar valued. If the column spe"
	    "cified in a relation is", text_len, (ftnlen)63);
    s_copy(text + text_len * 3034, "a character or time column, VALUE must b"
	    "e enclosed in single (')", text_len, (ftnlen)64);
    s_copy(text + text_len * 3035, "or double (\") quotes.", text_len, (
	    ftnlen)21);
    s_copy(text + text_len * 3036, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 3037, "The binary relations are: NE, LT, LE, EQ"
	    ", GE, GT, LIKE, NOT LIKE.", text_len, (ftnlen)65);
    s_copy(text + text_len * 3038, "The short hand equivalents of the first "
	    "6 of these are !=, <, <=,", text_len, (ftnlen)65);
    s_copy(text + text_len * 3039, "=, >=, >.  All but the LIKE relation can"
	    " be used with any column.", text_len, (ftnlen)65);
    s_copy(text + text_len * 3040, "The LIKE relation can be used only with "
	    "character columns; it is", text_len, (ftnlen)64);
    s_copy(text + text_len * 3041, "the pattern matching relation. The value"
	    " to the  right of a LIKE", text_len, (ftnlen)64);
    s_copy(text + text_len * 3042, "relation is a pattern that the column mu"
	    "st match.  The character", text_len, (ftnlen)64);
    s_copy(text + text_len * 3043, "'*' matches any substring  of of any str"
	    "ing.  The character '% '", text_len, (ftnlen)64);
    s_copy(text + text_len * 3044, "matches  any single character.", text_len,
	     (ftnlen)30);
    s_copy(text + text_len * 3045, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 3046, "Conditions may be grouped using parenthe"
	    "ses.  These groups may", text_len, (ftnlen)62);
    s_copy(text + text_len * 3047, "in turn be grouped using parentheses and"
	    " the connecting words", text_len, (ftnlen)61);
    s_copy(text + text_len * 3048, "AND and OR.  Any condition can be negate"
	    "d using the word NOT.", text_len, (ftnlen)61);
    s_copy(text + text_len * 3049, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 3050, "@@Where Clause", text_len, (ftnlen)14);
    s_copy(text + text_len * 3051, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 3052, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 3053, "Example Where Clause", text_len, (ftnlen)
	    20);
    s_copy(text + text_len * 3054, "Conditional Operators", text_len, (ftnlen)
	    21);
    s_copy(text + text_len * 3055, "Looking at Data    --- SELECT", text_len, 
	    (ftnlen)29);
    finish[75] = 3056;
    return 0;
} /* zzhlp032_ */

