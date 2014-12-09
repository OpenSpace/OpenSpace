/* zzhlp011.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZHLP011 ( private help text ) */
/* Subroutine */ int zzhlp011_(integer *begin, integer *finish, char *text, 
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
    s_copy(text + text_len * 1040, "process.  There are several causes of su"
	    "ch errors: typos,", text_len, (ftnlen)57);
    s_copy(text + text_len * 1041, "misunderstanding of commands syntax, or "
	    "inadequate preparation of", text_len, (ftnlen)65);
    s_copy(text + text_len * 1042, "Inspekt's environment.  Inspekt attempts"
	    " to diagnose errors and", text_len, (ftnlen)63);
    s_copy(text + text_len * 1043, "provides two levels of error diagnosis. "
	    "All error diagnostics are", text_len, (ftnlen)65);
    s_copy(text + text_len * 1044, "written to Inspekt's log file.", text_len,
	     (ftnlen)30);
    s_copy(text + text_len * 1045, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1046, "When an error is detected, Inspekt will "
	    "display a message and then", text_len, (ftnlen)66);
    s_copy(text + text_len * 1047, "prompt for a new command.  If the error "
	    "message does not seem to", text_len, (ftnlen)64);
    s_copy(text + text_len * 1048, "explain what went wrong, you can get the"
	    " second level of error", text_len, (ftnlen)62);
    s_copy(text + text_len * 1049, "diagnosis by entering the command \"?;\""
	    ".  If second level of error", text_len, (ftnlen)65);
    s_copy(text + text_len * 1050, "diagnosis  is not available, you will ge"
	    "t a  message stating that", text_len, (ftnlen)65);
    s_copy(text + text_len * 1051, "there is no more information available. "
	    " If a second level is", text_len, (ftnlen)61);
    s_copy(text + text_len * 1052, "available, the diagnosis will be display"
	    "ed.  This second level", text_len, (ftnlen)62);
    s_copy(text + text_len * 1053, "will usually list the chain of subroutin"
	    "e calls that led to the", text_len, (ftnlen)63);
    s_copy(text + text_len * 1054, "error diagnosis and other information.  "
	    "If you can't determine the", text_len, (ftnlen)66);
    s_copy(text + text_len * 1055, "cause of the error, send a description o"
	    "f what you were attempting", text_len, (ftnlen)66);
    s_copy(text + text_len * 1056, "to do along  with the Inspekt.log file t"
	    "o:", text_len, (ftnlen)42);
    s_copy(text + text_len * 1057, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1058, "btaber@spice.jpl.nasa.gov", text_len, (
	    ftnlen)25);
    s_copy(text + text_len * 1059, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1060, "and I will do what I can to help out.", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 1061, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1062, "-Bill Taber", text_len, (ftnlen)11);
    s_copy(text + text_len * 1063, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1064, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1065, "@@Errors", text_len, (ftnlen)8);
    s_copy(text + text_len * 1066, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1067, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 1068, "Typing Commands", text_len, (ftnlen)15);
    finish[19] = 1069;
    begin[20] = 1070;
    s_copy(text + text_len * 1069, "Below is a collection of sample time for"
	    "mats and the time strings that would be", text_len, (ftnlen)79);
    s_copy(text + text_len * 1070, "formed for noon on the fifteenth of Febr"
	    "uary 1993.", text_len, (ftnlen)50);
    s_copy(text + text_len * 1071, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1072, "Format:  YYYY-MON-DD WKD HR:MN:SC", 
	    text_len, (ftnlen)33);
    s_copy(text + text_len * 1073, "Result:  1993-FEB-15 MON 12:00:00", 
	    text_len, (ftnlen)33);
    s_copy(text + text_len * 1074, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1075, "Format:  Day of year DDD, MON-DD YYYY HR"
	    ":MN:SC.####", text_len, (ftnlen)51);
    s_copy(text + text_len * 1076, "Result:  Day of year 046, FEB-15 1993 12"
	    ":00:00.0000", text_len, (ftnlen)51);
    s_copy(text + text_len * 1077, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1078, "Format:  YYYY-DDD.### ::RND", text_len, (
	    ftnlen)27);
    s_copy(text + text_len * 1079, "Result:  1993-046.500", text_len, (ftnlen)
	    21);
    s_copy(text + text_len * 1080, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1081, "@@Example Time Formats", text_len, (
	    ftnlen)22);
    s_copy(text + text_len * 1082, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1083, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 1084, "Time Formats", text_len, (ftnlen)12);
    s_copy(text + text_len * 1085, "Custom Formats", text_len, (ftnlen)14);
    finish[20] = 1086;
    begin[21] = 1087;
    s_copy(text + text_len * 1086, "The WHERE clause in the SELECT command b"
	    "elow narrows down a selection to those", text_len, (ftnlen)78);
    s_copy(text + text_len * 1087, "events that occurred later than 1 Jan 19"
	    "93 but before 1 MAR 1993.  It further", text_len, (ftnlen)77);
    s_copy(text + text_len * 1088, "restricts the selection to those events "
	    "that have \"PLATFORM\" occurring in a", text_len, (ftnlen)75);
    s_copy(text + text_len * 1089, "substring in the SUBSYSTEM column of the"
	    " event.", text_len, (ftnlen)47);
    s_copy(text + text_len * 1090, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1091, "select time event_type subsystem notes", 
	    text_len, (ftnlen)38);
    s_copy(text + text_len * 1092, "from  events", text_len, (ftnlen)12);
    s_copy(text + text_len * 1093, "WHERE TIME GT \"1 JAN 1993\"", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 1094, "  AND TIME LT \"1 MAR 1993\"", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 1095, "  AND SUBSYSTEM LIKE \"*PLATFORM*\";", 
	    text_len, (ftnlen)34);
    s_copy(text + text_len * 1096, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1097, "Note that the times used in the TIME con"
	    "dition", text_len, (ftnlen)46);
    s_copy(text + text_len * 1098, "and the pattern used in the SUBSYSTEM co"
	    "ndition", text_len, (ftnlen)47);
    s_copy(text + text_len * 1099, "must match is enclosed in double (\") or"
	    " single (') quotes.", text_len, (ftnlen)58);
    s_copy(text + text_len * 1100, "@@Example Where Clause", text_len, (
	    ftnlen)22);
    s_copy(text + text_len * 1101, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1102, "Help", text_len, (ftnlen)4);
    finish[21] = 1103;
    begin[22] = 1104;
    s_copy(text + text_len * 1103, "Flagged formats are presented as shown h"
	    "ere", text_len, (ftnlen)43);
    s_copy(text + text_len * 1104, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1105, "First Column : Value of column for first"
	    " event", text_len, (ftnlen)46);
    s_copy(text + text_len * 1106, "Second Column: Value of the second colum"
	    "n with", text_len, (ftnlen)46);
    s_copy(text + text_len * 1107, "               the output wrapped if it "
	    "will", text_len, (ftnlen)44);
    s_copy(text + text_len * 1108, "               not fit on a single line "
	    "of text.", text_len, (ftnlen)48);
    s_copy(text + text_len * 1109, "Third Column : Value of the third column",
	     text_len, (ftnlen)40);
    s_copy(text + text_len * 1110, "               possibly wrapped in an un"
	    "even", text_len, (ftnlen)44);
    s_copy(text + text_len * 1111, "               fashion due to", text_len, 
	    (ftnlen)29);
    s_copy(text + text_len * 1112, "               the means of specifying a"
	    "n event.", text_len, (ftnlen)48);
    s_copy(text + text_len * 1113, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1114, "First Column : Value of this for second "
	    "event", text_len, (ftnlen)45);
    s_copy(text + text_len * 1115, "      ...            ...", text_len, (
	    ftnlen)24);
    s_copy(text + text_len * 1116, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1117, "There are two forms of flagged format.  "
	    "PRESERVED and un-PRESERVED", text_len, (ftnlen)66);
    s_copy(text + text_len * 1118, "(default).  If a format is a preserved, "
	    "each component of a column", text_len, (ftnlen)66);
    s_copy(text + text_len * 1119, "is begun on a new line of the report.  O"
	    "therwise it is considered", text_len, (ftnlen)65);
    s_copy(text + text_len * 1120, "to be simply FLAGGED format. Note  a bla"
	    "nk line is inserted  between", text_len, (ftnlen)68);
    s_copy(text + text_len * 1121, "consecutive events in the output.", 
	    text_len, (ftnlen)33);
    s_copy(text + text_len * 1122, "@@Flagged Format", text_len, (ftnlen)16);
    s_copy(text + text_len * 1123, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1124, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 1125, "Reports", text_len, (ftnlen)7);
    s_copy(text + text_len * 1126, " ", text_len, (ftnlen)1);
    finish[22] = 1127;
    begin[23] = 1128;
    s_copy(text + text_len * 1127, "An E-kernel may contain many different t"
	    "ables.  Moreover, different", text_len, (ftnlen)67);
    return 0;
} /* zzhlp011_ */

