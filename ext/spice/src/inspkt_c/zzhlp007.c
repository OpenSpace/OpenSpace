/* zzhlp007.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZHLP007 ( private help text ) */
/* Subroutine */ int zzhlp007_(integer *begin, integer *finish, char *text, 
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
    s_copy(text + text_len * 673, "Time Formats", text_len, (ftnlen)12);
    s_copy(text + text_len * 674, "Titles", text_len, (ftnlen)6);
    s_copy(text + text_len * 675, "Other Settings", text_len, (ftnlen)14);
    s_copy(text + text_len * 676, "Setting up Inspekt --- SET", text_len, (
	    ftnlen)26);
    finish[8] = 677;
    begin[9] = 678;
    s_copy(text + text_len * 677, "To create a custom time format for a colu"
	    "mn, enter the command:", text_len, (ftnlen)63);
    s_copy(text + text_len * 678, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 679, "SET COLUMN <column_name> FORMAT <format>;",
	     text_len, (ftnlen)41);
    s_copy(text + text_len * 680, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 681, "where <column_name> is the name of the co"
	    "lumn and <format> is the", text_len, (ftnlen)65);
    s_copy(text + text_len * 682, "custom format you desire.", text_len, (
	    ftnlen)25);
    s_copy(text + text_len * 683, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 684, "Custom formats work as follows.  Given a "
	    "time, there are associated", text_len, (ftnlen)67);
    s_copy(text + text_len * 685, "with it the current year, month, day, day"
	    " of year, hour, minutes,", text_len, (ftnlen)65);
    s_copy(text + text_len * 686, "seconds, current julian date, current num"
	    "ber of seconds past the", text_len, (ftnlen)64);
    s_copy(text + text_len * 687, "epoch of J2000, etc.  When a time is to b"
	    "e displayed, the custom", text_len, (ftnlen)64);
    s_copy(text + text_len * 688, "format you have provided is used as a rec"
	    "ipe for constructing the", text_len, (ftnlen)65);
    s_copy(text + text_len * 689, "time string.  Reading from left to right "
	    "the string formatter looks", text_len, (ftnlen)67);
    s_copy(text + text_len * 690, "for special substrings (listed below).  U"
	    "nrecognized substrings", text_len, (ftnlen)63);
    s_copy(text + text_len * 691, "are simply copied into the output string."
	    "  (This allows you to add", text_len, (ftnlen)66);
    s_copy(text + text_len * 692, "any label you might like to the output ti"
	    "mes.)  However, when a", text_len, (ftnlen)63);
    s_copy(text + text_len * 693, "recognized substring is found, the time f"
	    "ormatter determines the", text_len, (ftnlen)64);
    s_copy(text + text_len * 694, "corresponding component of time and appen"
	    "ds this to the output time", text_len, (ftnlen)67);
    s_copy(text + text_len * 695, "string that is under construction.", 
	    text_len, (ftnlen)34);
    s_copy(text + text_len * 696, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 697, "NOTE THAT TIME FORMATS ARE CASE SENSITIVE"
	    ".  To get a particular", text_len, (ftnlen)63);
    s_copy(text + text_len * 698, "component of the time into the output str"
	    "ing you must use exactly", text_len, (ftnlen)65);
    s_copy(text + text_len * 699, "the substring given in the list below. Fo"
	    "r example, if you wish", text_len, (ftnlen)63);
    s_copy(text + text_len * 700, "have the 3 letter abbreviation for the mo"
	    "nth appear in your output", text_len, (ftnlen)66);
    s_copy(text + text_len * 701, "times, you must use \"MON\";  the string"
	    " \"mon\" will simply be copied", text_len, (ftnlen)66);
    s_copy(text + text_len * 702, "as is into any of your time strings.", 
	    text_len, (ftnlen)36);
    s_copy(text + text_len * 703, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 704, "(Substrings beginning with \"::\" do not "
	    "affect the appearance of", text_len, (ftnlen)63);
    s_copy(text + text_len * 705, "the format only the time system or roundi"
	    "ng)", text_len, (ftnlen)44);
    s_copy(text + text_len * 706, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 707, "@setparamsize{::UTC,::TDB,::TDT}", 
	    text_len, (ftnlen)32);
    s_copy(text + text_len * 708, "@param ::UTC,::TDB,::TDT.", text_len, (
	    ftnlen)25);
    s_copy(text + text_len * 709, " use the time system UTC, TDB, TDT respec"
	    "tively  (default UTC)", text_len, (ftnlen)62);
    s_copy(text + text_len * 710, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 711, "@param ::RND, ::TRUNC.", text_len, (ftnlen)
	    22);
    s_copy(text + text_len * 712, " Round or Truncate time respectively (def"
	    "ault truncate)", text_len, (ftnlen)55);
    s_copy(text + text_len * 713, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 714, "@param YYYY.", text_len, (ftnlen)12);
    s_copy(text + text_len * 715, "year", text_len, (ftnlen)4);
    s_copy(text + text_len * 716, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 717, "@param MON, MM.", text_len, (ftnlen)15);
    s_copy(text + text_len * 718, " 3 letter abbreviation, 2 digit number fo"
	    "r month resp.", text_len, (ftnlen)54);
    s_copy(text + text_len * 719, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 720, "@param DD, DOY.", text_len, (ftnlen)15);
    s_copy(text + text_len * 721, " day of month, day of year respectively", 
	    text_len, (ftnlen)39);
    s_copy(text + text_len * 722, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 723, "@param WKD.", text_len, (ftnlen)11);
    s_copy(text + text_len * 724, " 3 letter abbreviation for day of week", 
	    text_len, (ftnlen)38);
    s_copy(text + text_len * 725, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 726, "@param HR, MN, SC.", text_len, (ftnlen)18);
    s_copy(text + text_len * 727, " hour, minutes, seconds respectively", 
	    text_len, (ftnlen)36);
    s_copy(text + text_len * 728, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 729, "@param JD,SP1950, SP2000.", text_len, (
	    ftnlen)25);
    s_copy(text + text_len * 730, " Julian date, seconds past 1950 or 2000 r"
	    "espectively", text_len, (ftnlen)52);
    s_copy(text + text_len * 731, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 732, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 733, "@param ##---#.", text_len, (ftnlen)14);
    s_copy(text + text_len * 734, "when these follow  a decimal point, they "
	    "indicate the number of", text_len, (ftnlen)63);
    s_copy(text + text_len * 735, "decimal places to use in the representati"
	    "on of the", text_len, (ftnlen)50);
    s_copy(text + text_len * 736, "preceding numeric component.  For example"
	    " 'SC.###' indicates that", text_len, (ftnlen)65);
    s_copy(text + text_len * 737, "the seconds component of a time should be"
	    " presented with", text_len, (ftnlen)56);
    s_copy(text + text_len * 738, "3 decimal points.", text_len, (ftnlen)17);
    s_copy(text + text_len * 739, "@@Custom Formats", text_len, (ftnlen)16);
    s_copy(text + text_len * 740, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 741, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 742, "Example Time Formats", text_len, (ftnlen)
	    20);
    finish[9] = 743;
    begin[10] = 744;
    s_copy(text + text_len * 743, "When printing a double precision number i"
	    "n a report,", text_len, (ftnlen)52);
    s_copy(text + text_len * 744, "Inspekt first examines", text_len, (ftnlen)
	    22);
    s_copy(text + text_len * 745, "the column attributes to determine if you"
	    " have specified", text_len, (ftnlen)56);
    s_copy(text + text_len * 746, "a particular format for that column.  If "
	    "you have that format", text_len, (ftnlen)61);
    s_copy(text + text_len * 747, "is used to create the text that is presen"
	    "ted in the report.", text_len, (ftnlen)59);
    s_copy(text + text_len * 748, "If you have not specified a particular fo"
	    "rmat, Inspekt looks", text_len, (ftnlen)60);
    s_copy(text + text_len * 749, "up the \"default floating format\" and us"
	    "es this to create the text", text_len, (ftnlen)65);
    s_copy(text + text_len * 750, "to be used in the report.  You may adjust"
	    " the default floating", text_len, (ftnlen)62);
    s_copy(text + text_len * 751, "format.  To do this issue the command", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 752, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 753, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 754, "SET DEFAULT FLOATING FORMAT format;", 
	    text_len, (ftnlen)35);
    s_copy(text + text_len * 755, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 756, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 757, "where \"format\" is the format you'd like"
	    " Inspekt to use", text_len, (ftnlen)54);
    s_copy(text + text_len * 758, "when you have not specified a particular "
	    "format for a column.", text_len, (ftnlen)61);
    s_copy(text + text_len * 759, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 760, "If you've specified a format for a double"
	    " precision column, and would", text_len, (ftnlen)69);
    s_copy(text + text_len * 761, "like to return to using the default float"
	    "ing format issue the", text_len, (ftnlen)61);
    s_copy(text + text_len * 762, "command", text_len, (ftnlen)7);
    s_copy(text + text_len * 763, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 764, "SET COLUMN column_name FORMAT DEFAULT;", 
	    text_len, (ftnlen)38);
    s_copy(text + text_len * 765, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 766, "Until you change the format for the speci"
	    "fied column again,", text_len, (ftnlen)59);
    return 0;
} /* zzhlp007_ */

