/* zzhlp008.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZHLP008 ( private help text ) */
/* Subroutine */ int zzhlp008_(integer *begin, integer *finish, char *text, 
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
    s_copy(text + text_len * 767, "the format used when presenting its value"
	    "s in a report will", text_len, (ftnlen)59);
    s_copy(text + text_len * 768, "be the default floating format.", text_len,
	     (ftnlen)31);
    s_copy(text + text_len * 769, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 770, "@@Default Floating Format", text_len, (
	    ftnlen)25);
    s_copy(text + text_len * 771, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 772, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 773, "Numeric Formats", text_len, (ftnlen)15);
    s_copy(text + text_len * 774, "Default Integer Format", text_len, (ftnlen)
	    22);
    s_copy(text + text_len * 775, "Default Time Format", text_len, (ftnlen)19)
	    ;
    s_copy(text + text_len * 776, "SHOW FORMAT   ...", text_len, (ftnlen)17);
    s_copy(text + text_len * 777, " ", text_len, (ftnlen)1);
    finish[10] = 778;
    begin[11] = 779;
    s_copy(text + text_len * 778, "When printing an integer in a report, Ins"
	    "pekt first examines", text_len, (ftnlen)60);
    s_copy(text + text_len * 779, "the column attributes to determine if you"
	    " have specified", text_len, (ftnlen)56);
    s_copy(text + text_len * 780, "a particular format for that column.  If "
	    "you have that format", text_len, (ftnlen)61);
    s_copy(text + text_len * 781, "is used to create the text that is presen"
	    "ted in the report.", text_len, (ftnlen)59);
    s_copy(text + text_len * 782, "If you have not specified a particular fo"
	    "rmat, Inspekt looks", text_len, (ftnlen)60);
    s_copy(text + text_len * 783, "up the \"default integer format\" and use"
	    "s this to create text", text_len, (ftnlen)60);
    s_copy(text + text_len * 784, "to be used in the report.  You may adjust"
	    " the default integer", text_len, (ftnlen)61);
    s_copy(text + text_len * 785, "format.  To do this issue the command", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 786, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 787, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 788, "SET DEFAULT INTEGER FORMAT format;", 
	    text_len, (ftnlen)34);
    s_copy(text + text_len * 789, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 790, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 791, "where \"format\" is the format you'd like"
	    " Inspekt to use", text_len, (ftnlen)54);
    s_copy(text + text_len * 792, "when you have not specified a particular "
	    "format for an integer", text_len, (ftnlen)62);
    s_copy(text + text_len * 793, "column.", text_len, (ftnlen)7);
    s_copy(text + text_len * 794, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 795, "If you've specified a format for an integ"
	    "er column, and would", text_len, (ftnlen)61);
    s_copy(text + text_len * 796, "like to return to using the default integ"
	    "er format issue the", text_len, (ftnlen)60);
    s_copy(text + text_len * 797, "command", text_len, (ftnlen)7);
    s_copy(text + text_len * 798, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 799, "SET COLUMN column_name FORMAT DEFAULT;", 
	    text_len, (ftnlen)38);
    s_copy(text + text_len * 800, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 801, "Until you change the format for the speci"
	    "fied column again,", text_len, (ftnlen)59);
    s_copy(text + text_len * 802, "the format used when presenting its value"
	    "s in a report will", text_len, (ftnlen)59);
    s_copy(text + text_len * 803, "be the default integer format.", text_len, 
	    (ftnlen)30);
    s_copy(text + text_len * 804, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 805, "@@Default Integer Format", text_len, (
	    ftnlen)24);
    s_copy(text + text_len * 806, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 807, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 808, "Numeric Formats", text_len, (ftnlen)15);
    s_copy(text + text_len * 809, "Default Floating Format", text_len, (
	    ftnlen)23);
    s_copy(text + text_len * 810, "Default Time Format", text_len, (ftnlen)19)
	    ;
    s_copy(text + text_len * 811, "SHOW FORMAT   ...", text_len, (ftnlen)17);
    finish[11] = 812;
    begin[12] = 813;
    s_copy(text + text_len * 812, "When printing a time in a report, Inspekt"
	    " first examines", text_len, (ftnlen)56);
    s_copy(text + text_len * 813, "the column attributes to determine if you"
	    " have specified", text_len, (ftnlen)56);
    s_copy(text + text_len * 814, "a particular format for that column.  If "
	    "you have that format", text_len, (ftnlen)61);
    s_copy(text + text_len * 815, "is used to create the text that is presen"
	    "ted in the report.", text_len, (ftnlen)59);
    s_copy(text + text_len * 816, "If you have not specified a particular fo"
	    "rmat, Inspekt looks", text_len, (ftnlen)60);
    s_copy(text + text_len * 817, "up the \"default time format\" and uses t"
	    "his to create text", text_len, (ftnlen)57);
    s_copy(text + text_len * 818, "to be used in the report.  You may adjust"
	    " the default time", text_len, (ftnlen)58);
    s_copy(text + text_len * 819, "format.  To do this issue the command", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 820, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 821, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 822, "SET DEFAULT TIME FORMAT format;", text_len,
	     (ftnlen)31);
    s_copy(text + text_len * 823, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 824, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 825, "where \"format\" is the format you'd like"
	    " Inspekt to use", text_len, (ftnlen)54);
    s_copy(text + text_len * 826, "when you have not specified a particular "
	    "format for a column.", text_len, (ftnlen)61);
    s_copy(text + text_len * 827, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 828, "If you've specified a format for a time c"
	    "olumn, and would", text_len, (ftnlen)57);
    s_copy(text + text_len * 829, "like to return to using the default time "
	    "format issue the", text_len, (ftnlen)57);
    s_copy(text + text_len * 830, "command", text_len, (ftnlen)7);
    s_copy(text + text_len * 831, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 832, "SET COLUMN column_name FORMAT DEFAULT;", 
	    text_len, (ftnlen)38);
    s_copy(text + text_len * 833, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 834, "Until you change the format for the speci"
	    "fied column again,", text_len, (ftnlen)59);
    s_copy(text + text_len * 835, "the format used when presenting its value"
	    "s in a report will", text_len, (ftnlen)59);
    s_copy(text + text_len * 836, "be the default time format.", text_len, (
	    ftnlen)27);
    s_copy(text + text_len * 837, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 838, "@@Default Time Format", text_len, (ftnlen)
	    21);
    s_copy(text + text_len * 839, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 840, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 841, "Custom Formats", text_len, (ftnlen)14);
    s_copy(text + text_len * 842, "Default Floating Format", text_len, (
	    ftnlen)23);
    s_copy(text + text_len * 843, "Default Integer Format", text_len, (ftnlen)
	    22);
    s_copy(text + text_len * 844, "SHOW FORMAT   ...", text_len, (ftnlen)17);
    finish[12] = 845;
    begin[13] = 846;
    s_copy(text + text_len * 845, "The Delimited formats provide a mechanism"
	    " for producing a report that", text_len, (ftnlen)69);
    s_copy(text + text_len * 846, "is suitable for import into Microsoft Exc"
	    "el or other spreadsheet", text_len, (ftnlen)64);
    s_copy(text + text_len * 847, "programs.", text_len, (ftnlen)9);
    s_copy(text + text_len * 848, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 849, "When the delimited format is used, the re"
	    "sults of queries are written", text_len, (ftnlen)69);
    s_copy(text + text_len * 850, "out without formatting. The values for th"
	    "e columns are quoted if they", text_len, (ftnlen)69);
    s_copy(text + text_len * 851, "are not numeric and the values are separa"
	    "ted by a tab character (or any", text_len, (ftnlen)71);
    s_copy(text + text_len * 852, "other character you choose other than '@'"
	    ").", text_len, (ftnlen)43);
    s_copy(text + text_len * 853, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 854, "The default character used to quote strin"
	    "gs is '\"'. You may choose any", text_len, (ftnlen)70);
    s_copy(text + text_len * 855, "other character (except '@') as the quote"
	    " character when you set the", text_len, (ftnlen)68);
    s_copy(text + text_len * 856, "format to DELIMITED.", text_len, (ftnlen)
	    20);
    s_copy(text + text_len * 857, " ", text_len, (ftnlen)1);
    return 0;
} /* zzhlp008_ */

