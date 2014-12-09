/* zzhlp031.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZHLP031 ( private help text ) */
/* Subroutine */ int zzhlp031_(integer *begin, integer *finish, char *text, 
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
    s_copy(text + text_len * 2878, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2879, "DEFINE ES EDIT SELECT;", text_len, (
	    ftnlen)22);
    s_copy(text + text_len * 2880, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2881, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2882, "When you type", text_len, (ftnlen)13);
    s_copy(text + text_len * 2883, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2884, "Inspekt> ES;", text_len, (ftnlen)12);
    s_copy(text + text_len * 2885, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2886, "The command is translated to", text_len, (
	    ftnlen)28);
    s_copy(text + text_len * 2887, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2888, "EDIT SELECT", text_len, (ftnlen)11);
    s_copy(text + text_len * 2889, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2890, "If you have recently issued a select com"
	    "mand, the command will be copied", text_len, (ftnlen)72);
    s_copy(text + text_len * 2891, "into the editor so that you may perform "
	    "any desired modifications.", text_len, (ftnlen)66);
    s_copy(text + text_len * 2892, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2893, "@subsection Creating Symbols", text_len, (
	    ftnlen)28);
    s_copy(text + text_len * 2894, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2895, "You may use symbols to stand for any gro"
	    "up of words that you would", text_len, (ftnlen)66);
    s_copy(text + text_len * 2896, "like to avoid typing repeatedly. To defi"
	    "ne a symbol type:", text_len, (ftnlen)57);
    s_copy(text + text_len * 2897, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2898, "DEFINE symbol_name value;", text_len, (
	    ftnlen)25);
    s_copy(text + text_len * 2899, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2900, "where \"symbol_name\" is the name of the"
	    " symbol you want to define and", text_len, (ftnlen)68);
    s_copy(text + text_len * 2901, "\"value\" is the associated list of word"
	    "s and characters. The value of", text_len, (ftnlen)68);
    s_copy(text + text_len * 2902, "a symbol may be blank.  Symbol names mus"
	    "t begin with a letter and", text_len, (ftnlen)65);
    s_copy(text + text_len * 2903, "can not end with a question mark '?'.  A"
	    "lso the case of letters", text_len, (ftnlen)63);
    s_copy(text + text_len * 2904, "in a symbol is insignificant.  For examp"
	    "le, the commands", text_len, (ftnlen)56);
    s_copy(text + text_len * 2905, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2906, "DEFINE MYSYM a string of words;", 
	    text_len, (ftnlen)31);
    s_copy(text + text_len * 2907, "DEFINE MySym a string of words;", 
	    text_len, (ftnlen)31);
    s_copy(text + text_len * 2908, "DEFINE mysym a string of words;", 
	    text_len, (ftnlen)31);
    s_copy(text + text_len * 2909, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2910, "all define the symbol MYSYM.  Moreover, "
	    "when symbols appear in", text_len, (ftnlen)62);
    s_copy(text + text_len * 2911, "a command, the case of the letters is in"
	    "significant.  The", text_len, (ftnlen)57);
    s_copy(text + text_len * 2912, "commands", text_len, (ftnlen)8);
    s_copy(text + text_len * 2913, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2914, "SET FORMAT MYSYM;", text_len, (ftnlen)17);
    s_copy(text + text_len * 2915, "SET FORMAT MySym;", text_len, (ftnlen)17);
    s_copy(text + text_len * 2916, "SET FORMAT mysym;", text_len, (ftnlen)17);
    s_copy(text + text_len * 2917, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2918, "will all be recognized as containing the"
	    " symbol MYSYM.", text_len, (ftnlen)54);
    s_copy(text + text_len * 2919, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2920, "@subsection Removing Symbols", text_len, (
	    ftnlen)28);
    s_copy(text + text_len * 2921, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2922, "To remove a symbol definitions type:", 
	    text_len, (ftnlen)36);
    s_copy(text + text_len * 2923, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2924, "UNDEFINE symbol_name", text_len, (ftnlen)
	    20);
    s_copy(text + text_len * 2925, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2926, "Note that", text_len, (ftnlen)9);
    s_copy(text + text_len * 2927, "you can \"undefine\" a symbol that has n"
	    "ot been defined.  This is useful", text_len, (ftnlen)70);
    s_copy(text + text_len * 2928, "in Inspekt Procedure Files when you want"
	    " to make sure that some previously", text_len, (ftnlen)74);
    s_copy(text + text_len * 2929, "defined symbol will not accidentally alt"
	    "er the meaning of a procedure command.", text_len, (ftnlen)78);
    s_copy(text + text_len * 2930, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2931, "@subsection Symbols Used in Other Symbols"
	    , text_len, (ftnlen)41);
    s_copy(text + text_len * 2932, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2933, "You may use symbols in the definition of"
	    " another symbol.  For example,", text_len, (ftnlen)70);
    s_copy(text + text_len * 2934, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2935, "DEFINE P PRESERVED;", text_len, (ftnlen)
	    19);
    s_copy(text + text_len * 2936, "DEFINE M MARKED;", text_len, (ftnlen)16);
    s_copy(text + text_len * 2937, "DEFINE TAB TABULAR;", text_len, (ftnlen)
	    19);
    s_copy(text + text_len * 2938, "DEFINE SETF SET FORMAT", text_len, (
	    ftnlen)22);
    s_copy(text + text_len * 2939, "DEFINE MYFMT SETF M TAB P;", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 2940, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2941, "When you type", text_len, (ftnlen)13);
    s_copy(text + text_len * 2942, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2943, "Inspekt> MYFMT;", text_len, (ftnlen)15);
    s_copy(text + text_len * 2944, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2945, "The effect will be the same as if you ha"
	    "d typed", text_len, (ftnlen)47);
    s_copy(text + text_len * 2946, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2947, "SET FORMAT MARKED TABULAR PRESERVED;", 
	    text_len, (ftnlen)36);
    s_copy(text + text_len * 2948, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2949, "Symbols are not evaluated until they are"
	    " encountered in a command.", text_len, (ftnlen)66);
    s_copy(text + text_len * 2950, "Moreover, symbol substitution is perform"
	    "ed until no more symbols", text_len, (ftnlen)64);
    s_copy(text + text_len * 2951, "are found in the string.", text_len, (
	    ftnlen)24);
    s_copy(text + text_len * 2952, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2953, "As a result of this \"evaluate when enco"
	    "untered\" strategy you can change", text_len, (ftnlen)71);
    s_copy(text + text_len * 2954, "the value of MYFMT to mean SET FORMAT MA"
	    "RKED TABULAR by", text_len, (ftnlen)55);
    s_copy(text + text_len * 2955, "redefining P to be a blank.", text_len, (
	    ftnlen)27);
    s_copy(text + text_len * 2956, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2957, "DEFINE P  ;", text_len, (ftnlen)11);
    s_copy(text + text_len * 2958, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2959, "In addition to saving you typing, symbol"
	    "s offer a mechanism for", text_len, (ftnlen)63);
    s_copy(text + text_len * 2960, "passing information to Inspekt Procedure"
	    "s.  If a procedure needs", text_len, (ftnlen)64);
    s_copy(text + text_len * 2961, "some variable piece of information, you "
	    "can write the procedure so", text_len, (ftnlen)66);
    s_copy(text + text_len * 2962, "that this information is expected to be "
	    "present in some symbol.", text_len, (ftnlen)63);
    s_copy(text + text_len * 2963, "Then create the symbol with the appropri"
	    "ate information before", text_len, (ftnlen)62);
    s_copy(text + text_len * 2964, "starting the procedure that needs it.", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 2965, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2966, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2967, "@subsection Suppressing Symbols Substitu"
	    "tion", text_len, (ftnlen)44);
    s_copy(text + text_len * 2968, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2969, "Occasionally you may want to make sure t"
	    "hat a word does not get mistaken", text_len, (ftnlen)72);
    s_copy(text + text_len * 2970, "for a symbol.  If the word is enclosed i"
	    "n single (') or double (\") quotes", text_len, (ftnlen)73);
    s_copy(text + text_len * 2971, "it is automatically invisible to the sym"
	    "bol translation subsystem.  Thus", text_len, (ftnlen)72);
    s_copy(text + text_len * 2972, "words in quoted strings are never mistak"
	    "en for symbols.  In addition", text_len, (ftnlen)68);
    s_copy(text + text_len * 2973, "to these words you can make one or more "
	    "words invisible to the symbol", text_len, (ftnlen)69);
    s_copy(text + text_len * 2974, "resolver by placing it between consecuti"
	    "ve '@' characters.  Thus the command", text_len, (ftnlen)76);
    s_copy(text + text_len * 2975, "below sets the page title to be \"MYFM"
	    "T\" even though MYFMT is a symbol defined", text_len, (ftnlen)77);
    s_copy(text + text_len * 2976, "above", text_len, (ftnlen)5);
    s_copy(text + text_len * 2977, "@literal", text_len, (ftnlen)8);
    return 0;
} /* zzhlp031_ */

