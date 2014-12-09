/* zzhlp029.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZHLP029 ( private help text ) */
/* Subroutine */ int zzhlp029_(integer *begin, integer *finish, char *text, 
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
    s_copy(text + text_len * 2699, "SET TITLE FREQUENCY (1:1){ 0", text_len, (
	    ftnlen)28);
    s_copy(text + text_len * 2700, "                         | FIRST | 1ST", 
	    text_len, (ftnlen)38);
    s_copy(text + text_len * 2701, "                         | ALL", text_len,
	     (ftnlen)30);
    s_copy(text + text_len * 2702, "                         | EVERY @int(2:"
	    ") }", text_len, (ftnlen)43);
    s_copy(text + text_len * 2703, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2704, "SET TITLE JUSTIFICATION (1:1){ LEFT | CE"
	    "NTER | RIGHT }", text_len, (ftnlen)54);
    s_copy(text + text_len * 2705, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2706, "SHOW COLUMN @word", text_len, (ftnlen)17);
    s_copy(text + text_len * 2707, "SHOW COMMENTS", text_len, (ftnlen)13);
    s_copy(text + text_len * 2708, "SHOW COMMENTS @word", text_len, (ftnlen)
	    19);
    s_copy(text + text_len * 2709, "SHOW ENVIRONMENT", text_len, (ftnlen)16);
    s_copy(text + text_len * 2710, "SHOW FORMAT", text_len, (ftnlen)11);
    s_copy(text + text_len * 2711, "SHOW INDEXES", text_len, (ftnlen)12);
    s_copy(text + text_len * 2712, "SHOW KERNELS", text_len, (ftnlen)12);
    s_copy(text + text_len * 2713, "SHOW PAGE", text_len, (ftnlen)9);
    s_copy(text + text_len * 2714, "SHOW SYMBOL @word", text_len, (ftnlen)17);
    s_copy(text + text_len * 2715, "SHOW SUMMARY", text_len, (ftnlen)12);
    s_copy(text + text_len * 2716, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2717, "START @word", text_len, (ftnlen)11);
    s_copy(text + text_len * 2718, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2719, "UNLOAD @word", text_len, (ftnlen)12);
    s_copy(text + text_len * 2720, "!endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2721, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2722, "@@Syntax Summaries", text_len, (ftnlen)18)
	    ;
    s_copy(text + text_len * 2723, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 2724, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 2725, "Syntax Description Language", text_len, (
	    ftnlen)27);
    finish[66] = 2726;
    begin[67] = 2727;
    s_copy(text + text_len * 2726, "A \"table\" is a named grouping of event"
	    "s (or other objects) all of", text_len, (ftnlen)65);
    s_copy(text + text_len * 2727, "which have the same named attributes.", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 2728, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2729, "The designation \"table\" comes from the"
	    " appearance of listings of the", text_len, (ftnlen)68);
    s_copy(text + text_len * 2730, "attributes of events.  If we list all of"
	    " the events we have", text_len, (ftnlen)59);
    s_copy(text + text_len * 2731, "recorded on a sheet of paper (or a termi"
	    "nal) so that the attributes", text_len, (ftnlen)67);
    s_copy(text + text_len * 2732, "for each event are always listed in the "
	    "same order from left to right,", text_len, (ftnlen)70);
    s_copy(text + text_len * 2733, "the resulting page will look like a \"ta"
	    "ble\" of values similar to", text_len, (ftnlen)64);
    s_copy(text + text_len * 2734, "a table of sines and cosines or a table "
	    "of prices.", text_len, (ftnlen)50);
    s_copy(text + text_len * 2735, "@@Table", text_len, (ftnlen)7);
    s_copy(text + text_len * 2736, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 2737, "Help", text_len, (ftnlen)4);
    finish[67] = 2738;
    begin[68] = 2739;
    s_copy(text + text_len * 2738, "Inspekt provides a number of different T"
	    "abular formats.", text_len, (ftnlen)55);
    s_copy(text + text_len * 2739, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2740, "@subsection TABULAR", text_len, (ftnlen)
	    19);
    s_copy(text + text_len * 2741, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2742, "JAN 25 07:15:00  OBSERVATION   WEATHER  "
	    " Sunny and cool (about 55 F)", text_len, (ftnlen)68);
    s_copy(text + text_len * 2743, "                                        "
	    " Air Quality good.", text_len, (ftnlen)58);
    s_copy(text + text_len * 2744, "JAN 26 07:15:00  OBSERVATION   WEATHER  "
	    " Sunny and cool (about 52 F)", text_len, (ftnlen)68);
    s_copy(text + text_len * 2745, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2746, "@subsection MARKED TABULAR", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 2747, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2748, "> JAN 25 07:15:00  OBSERVATION WEATHER  "
	    " Sunny and cool (about 55 F)", text_len, (ftnlen)68);
    s_copy(text + text_len * 2749, "                                        "
	    " Air Quality good.", text_len, (ftnlen)58);
    s_copy(text + text_len * 2750, "> JAN 26 07:15:00  OBSERVATION WEATHER  "
	    " Sunny and cool (about 52 F)", text_len, (ftnlen)68);
    s_copy(text + text_len * 2751, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2752, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2753, "@subsection SPACED TABULAR", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 2754, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2755, "JAN 25 07:15:00  OBSERVATION   WEATHER  "
	    " Sunny and cool (about 55 F)", text_len, (ftnlen)68);
    s_copy(text + text_len * 2756, "                                        "
	    " Air Quality good.", text_len, (ftnlen)58);
    s_copy(text + text_len * 2757, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2758, "AN 26 07:15:00  OBSERVATION    WEATHER  "
	    " Sunny and cool (about 52 F)", text_len, (ftnlen)68);
    s_copy(text + text_len * 2759, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2760, "@@Tabular Format", text_len, (ftnlen)16);
    s_copy(text + text_len * 2761, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 2762, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 2763, "Reports", text_len, (ftnlen)7);
    s_copy(text + text_len * 2764, "Tabular Format Mark", text_len, (ftnlen)
	    19);
    finish[68] = 2765;
    begin[69] = 2766;
    s_copy(text + text_len * 2765, "The format of a marked tabular report is"
	    " similar to that of a", text_len, (ftnlen)61);
    s_copy(text + text_len * 2766, "tabular report.  However, an extra colum"
	    "n is inserted at the left", text_len, (ftnlen)65);
    s_copy(text + text_len * 2767, "side of the report.  A format mark appea"
	    "rs", text_len, (ftnlen)42);
    s_copy(text + text_len * 2768, "in this column when each new row is begu"
	    "n.", text_len, (ftnlen)42);
    s_copy(text + text_len * 2769, "You can quickly scan the left most colum"
	    "n of the", text_len, (ftnlen)48);
    s_copy(text + text_len * 2770, "report and determine where each new row "
	    "begins.   The default format", text_len, (ftnlen)68);
    s_copy(text + text_len * 2771, "mark is the \"greater than\" symbol (>)."
	    "  To change this default to", text_len, (ftnlen)65);
    s_copy(text + text_len * 2772, "some other character, use the SET FORMAT"
	    " MARK command.", text_len, (ftnlen)54);
    s_copy(text + text_len * 2773, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2774, "SET FORMAT MARK character", text_len, (
	    ftnlen)25);
    s_copy(text + text_len * 2775, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2776, "where \"character\" is the character you"
	    " would like to replace '>'.", text_len, (ftnlen)65);
    s_copy(text + text_len * 2777, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2778, "NOTE: the format character cannot be a s"
	    "emi-colon.", text_len, (ftnlen)50);
    s_copy(text + text_len * 2779, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2780, "For example suppose that you would like "
	    "to change the format mark", text_len, (ftnlen)65);
    s_copy(text + text_len * 2781, "from '>' to '='.  Type the command", 
	    text_len, (ftnlen)34);
    s_copy(text + text_len * 2782, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2783, "SET FORMAT MARK =;", text_len, (ftnlen)18)
	    ;
    s_copy(text + text_len * 2784, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2785, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2786, "@@Tabular Format Mark", text_len, (ftnlen)
	    21);
    s_copy(text + text_len * 2787, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 2788, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 2789, "Tabular Format", text_len, (ftnlen)14);
    return 0;
} /* zzhlp029_ */

