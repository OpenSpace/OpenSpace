/* zzhlp030.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZHLP030 ( private help text ) */
/* Subroutine */ int zzhlp030_(integer *begin, integer *finish, char *text, 
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
    finish[69] = 2790;
    begin[70] = 2791;
    s_copy(text + text_len * 2790, "You may request time to be displayed in "
	    "an almost limitless variety of formats.", text_len, (ftnlen)79);
    s_copy(text + text_len * 2791, "The default format is UTC calendar forma"
	    "t. Other standard formats may be  set by", text_len, (ftnlen)80);
    s_copy(text + text_len * 2792, "using either of the commands:", text_len, 
	    (ftnlen)29);
    s_copy(text + text_len * 2793, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2794, "SET COLUMN column_name FORMAT format;", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 2795, "SET DEFAULT TIME FORMAT format;", 
	    text_len, (ftnlen)31);
    s_copy(text + text_len * 2796, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2797, "A number of standard formats may be spec"
	    "ified:", text_len, (ftnlen)46);
    s_copy(text + text_len * 2798, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2799, "@setparamsize{GLLSCLK}", text_len, (
	    ftnlen)22);
    s_copy(text + text_len * 2800, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2801, "@param  UTC.", text_len, (ftnlen)12);
    s_copy(text + text_len * 2802, "Default format: YYYY-MON-DD HR:MN:SC", 
	    text_len, (ftnlen)36);
    s_copy(text + text_len * 2803, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2804, "@param ISO.", text_len, (ftnlen)11);
    s_copy(text + text_len * 2805, "International Standards format: YYYY-MM-"
	    "DDTHR:MN:SC", text_len, (ftnlen)51);
    s_copy(text + text_len * 2806, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2807, "@param ISODOY.", text_len, (ftnlen)14);
    s_copy(text + text_len * 2808, "International Standards day of year: YYY"
	    "Y-DOYTHR:MN:SC", text_len, (ftnlen)54);
    s_copy(text + text_len * 2809, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2810, "@param  JED.", text_len, (ftnlen)12);
    s_copy(text + text_len * 2811, "Julian Ephemeris date to 5 decimal places"
	    , text_len, (ftnlen)41);
    s_copy(text + text_len * 2812, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2813, "@param MOSCLK.", text_len, (ftnlen)14);
    s_copy(text + text_len * 2814, "Mars Observer Spacecraft Clock format", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 2815, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2816, "@param GLLSCLK.", text_len, (ftnlen)15);
    s_copy(text + text_len * 2817, "Galileo Spacecraft Clock format", 
	    text_len, (ftnlen)31);
    s_copy(text + text_len * 2818, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2819, "These formats will be recognized regardl"
	    "ess of the case of the letters use to", text_len, (ftnlen)77);
    s_copy(text + text_len * 2820, "specify them.  The UTC, ISO and ISODY fo"
	    "rmats are all UTC times.  You may also", text_len, (ftnlen)78);
    s_copy(text + text_len * 2821, "create a custom format (see Custom Forma"
	    "ts).", text_len, (ftnlen)44);
    s_copy(text + text_len * 2822, "@@Time Formats", text_len, (ftnlen)14);
    s_copy(text + text_len * 2823, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 2824, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 2825, "Custom Formats", text_len, (ftnlen)14);
    s_copy(text + text_len * 2826, "Default Time Format", text_len, (ftnlen)
	    19);
    finish[70] = 2827;
    begin[71] = 2828;
    s_copy(text + text_len * 2827, "Titles are single lines of text that app"
	    "ear at the beginning", text_len, (ftnlen)60);
    s_copy(text + text_len * 2828, "of Inspekt reports.  You may adjust the "
	    "text of the title,", text_len, (ftnlen)58);
    s_copy(text + text_len * 2829, "its positioning (left justified, centere"
	    "d, or right justified), and", text_len, (ftnlen)67);
    s_copy(text + text_len * 2830, "how often it is shown as a report is dis"
	    "played.  To see the current", text_len, (ftnlen)67);
    s_copy(text + text_len * 2831, "attributes of the report title, type", 
	    text_len, (ftnlen)36);
    s_copy(text + text_len * 2832, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2833, "SHOW PAGE", text_len, (ftnlen)9);
    s_copy(text + text_len * 2834, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2835, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2836, "@@Titles", text_len, (ftnlen)8);
    s_copy(text + text_len * 2837, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 2838, "Help", text_len, (ftnlen)4);
    finish[71] = 2839;
    begin[72] = 2840;
    s_copy(text + text_len * 2839, "Inspekt is a command driven program.  Yo"
	    "u type a command at the", text_len, (ftnlen)63);
    s_copy(text + text_len * 2840, "prompt", text_len, (ftnlen)6);
    s_copy(text + text_len * 2841, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2842, "Inspekt>", text_len, (ftnlen)8);
    s_copy(text + text_len * 2843, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2844, "The program performs some action based u"
	    "pon the typed command.  If", text_len, (ftnlen)66);
    s_copy(text + text_len * 2845, "a command is too long to fit on a single"
	    " line, hit the return key", text_len, (ftnlen)65);
    s_copy(text + text_len * 2846, "when you get to the end of the line and "
	    "continue typing on the next", text_len, (ftnlen)67);
    s_copy(text + text_len * 2847, "line. When you are finished typing the c"
	    "ommand, type a semi-colon", text_len, (ftnlen)65);
    s_copy(text + text_len * 2848, "(\";\").  The semi-colon is required at "
	    "the end of all commands typed", text_len, (ftnlen)67);
    s_copy(text + text_len * 2849, "in response to a prompt ending with \""
	    ">\".  It is needed even if the", text_len, (ftnlen)66);
    s_copy(text + text_len * 2850, "line will fit on a single line. Occasion"
	    "ally, Inspekt may prompt", text_len, (ftnlen)64);
    s_copy(text + text_len * 2851, "you to supply inputs other than commands"
	    " (such as in the Help", text_len, (ftnlen)61);
    s_copy(text + text_len * 2852, "system).  In such cases the prompt will "
	    "not end in a greater than", text_len, (ftnlen)65);
    s_copy(text + text_len * 2853, "sign \">\" and the semi-colon is not nee"
	    "ded.", text_len, (ftnlen)42);
    s_copy(text + text_len * 2854, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2855, "The commands you type may be in either u"
	    "pper or lower case.", text_len, (ftnlen)59);
    s_copy(text + text_len * 2856, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2857, "If you begin typing a command and realiz"
	    "e you've made an error or", text_len, (ftnlen)65);
    s_copy(text + text_len * 2858, "wish to start again, add a blank line to"
	    " the command entered so", text_len, (ftnlen)63);
    s_copy(text + text_len * 2859, "far. Inspekt will ignore what you've typ"
	    "ed and prompt you for a", text_len, (ftnlen)63);
    s_copy(text + text_len * 2860, "new command.", text_len, (ftnlen)12);
    s_copy(text + text_len * 2861, "@@Typing Commands", text_len, (ftnlen)17);
    s_copy(text + text_len * 2862, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 2863, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 2864, "Editing Commands", text_len, (ftnlen)16);
    s_copy(text + text_len * 2865, "Column and Table Abbreviations", text_len,
	     (ftnlen)30);
    s_copy(text + text_len * 2866, "Using Symbols", text_len, (ftnlen)13);
    s_copy(text + text_len * 2867, "Special Symbols --- Queries", text_len, (
	    ftnlen)27);
    s_copy(text + text_len * 2868, "Collecting Commands In Files", text_len, (
	    ftnlen)28);
    s_copy(text + text_len * 2869, " ", text_len, (ftnlen)1);
    finish[72] = 2870;
    begin[73] = 2871;
    s_copy(text + text_len * 2870, "@subsection An Example", text_len, (
	    ftnlen)22);
    s_copy(text + text_len * 2871, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2872, "Inspekt allows you to create special wor"
	    "ds that when", text_len, (ftnlen)52);
    s_copy(text + text_len * 2873, "encountered in a command are translated "
	    "into different words.", text_len, (ftnlen)61);
    s_copy(text + text_len * 2874, "These special words are called 'symbols'."
	    , text_len, (ftnlen)41);
    s_copy(text + text_len * 2875, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2876, "For example suppose that you frequently "
	    "want to edit your last", text_len, (ftnlen)62);
    s_copy(text + text_len * 2877, "\"Select\" command.  You could make up t"
	    "he symbol ES as shown below:", text_len, (ftnlen)66);
    return 0;
} /* zzhlp030_ */

