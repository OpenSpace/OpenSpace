/* zzhlp000.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZHLP000 ( private help text ) */
/* Subroutine */ int zzhlp000_(integer *begin, integer *finish, char *text, 
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
    begin[0] = 1;
    s_copy(text, "You've already discovered the basics of Inspekt's Help sys"
	    "tem. When", text_len, (ftnlen)67);
    s_copy(text + text_len, "you type \"help;\" (or \"HELP;\") a menu is  di"
	    "splayed such as the one", text_len, (ftnlen)66);
    s_copy(text + (text_len << 1), "below:", text_len, (ftnlen)6);
    s_copy(text + text_len * 3, "@literal", text_len, (ftnlen)8);
    s_copy(text + (text_len << 2), "(Q) Quit Help", text_len, (ftnlen)13);
    s_copy(text + text_len * 5, "(1) About Help", text_len, (ftnlen)14);
    s_copy(text + text_len * 6, "      ...", text_len, (ftnlen)9);
    s_copy(text + text_len * 7, "Option: _", text_len, (ftnlen)9);
    s_copy(text + (text_len << 3), "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 9, "At the prompt (Option: ) type the number or"
	    " letter shown in", text_len, (ftnlen)59);
    s_copy(text + text_len * 10, "parentheses to the left of the topic of in"
	    "terest and hit RETURN.", text_len, (ftnlen)64);
    s_copy(text + text_len * 11, "You  will be presented with a screen of te"
	    "xt that describes the", text_len, (ftnlen)63);
    s_copy(text + text_len * 12, "topic.", text_len, (ftnlen)6);
    s_copy(text + text_len * 13, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 14, "After a full screen of text has been displ"
	    "ayed, a left justified", text_len, (ftnlen)64);
    s_copy(text + text_len * 15, "short vertical line will be displayed. Jus"
	    "t to the right of this", text_len, (ftnlen)64);
    s_copy(text + (text_len << 4), "line will be the cursor.  This indicates"
	    " there is more text or", text_len, (ftnlen)62);
    s_copy(text + text_len * 17, "related topics associated with the current"
	    " topic.  Hit a carriage", text_len, (ftnlen)65);
    s_copy(text + text_len * 18, "return to display the additional informati"
	    "on.   If there is more", text_len, (ftnlen)64);
    s_copy(text + text_len * 19, "text on the topic, you will see another sc"
	    "reen of text followed by", text_len, (ftnlen)66);
    s_copy(text + text_len * 20, "the same short line and prompt.  If there "
	    "is no more text for the", text_len, (ftnlen)65);
    s_copy(text + text_len * 21, "current topic, another menu of options wil"
	    "l be displayed allowing", text_len, (ftnlen)65);
    s_copy(text + text_len * 22, "you to exit the help system; return to the"
	    " main menu; or move on", text_len, (ftnlen)64);
    s_copy(text + text_len * 23, "to topics related to the current topic.", 
	    text_len, (ftnlen)39);
    s_copy(text + text_len * 24, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 25, "@@About Help", text_len, (ftnlen)12);
    s_copy(text + text_len * 26, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 27, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 28, "Short Cut to Topics", text_len, (ftnlen)19);
    s_copy(text + text_len * 29, "Making Help Wait", text_len, (ftnlen)16);
    finish[0] = 30;
    begin[1] = 31;
    s_copy(text + text_len * 30, "The \"page\" on which reports are displaye"
	    "d is often too narrow to", text_len, (ftnlen)64);
    s_copy(text + text_len * 31, "hold a tabular report.  However, by adjust"
	    "ing the width of some columns", text_len, (ftnlen)71);
    s_copy(text + (text_len << 5), "it may be possible to fit the report in "
	    "the available space.  If", text_len, (ftnlen)64);
    s_copy(text + text_len * 33, "this is true, Inspekt will ask whether or "
	    "not it should temporarily", text_len, (ftnlen)67);
    s_copy(text + text_len * 34, "(for the duration of the report) adjust co"
	    "lumn widths so that the", text_len, (ftnlen)65);
    s_copy(text + text_len * 35, "report may be printed. This feature is cal"
	    "led \"AUTOADJUST\".  By", text_len, (ftnlen)63);
    s_copy(text + text_len * 36, "default, AUTOADJUST is set up so that Insp"
	    "ekt asks you whether or", text_len, (ftnlen)65);
    s_copy(text + text_len * 37, "not it should adjust some column widths to"
	    " fit a report on the current", text_len, (ftnlen)70);
    s_copy(text + text_len * 38, "page.  This default action can be modified"
	    " by using the SET AUTOADJUST", text_len, (ftnlen)70);
    s_copy(text + text_len * 39, "command.  You can set the automatic action"
	    " to", text_len, (ftnlen)45);
    s_copy(text + text_len * 40, "@newlist", text_len, (ftnlen)8);
    s_copy(text + text_len * 41, "@numitem Prohibit display of reports that "
	    "are too wide.", text_len, (ftnlen)55);
    s_copy(text + text_len * 42, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 43, "@numitem Ask whether or not a columns shou"
	    "ld be narrowed to", text_len, (ftnlen)59);
    s_copy(text + text_len * 44, "         accommodate a wide  report (defau"
	    "lt)", text_len, (ftnlen)45);
    s_copy(text + text_len * 45, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 46, "@numitem Automatically adjust column width"
	    "s and display reports.", text_len, (ftnlen)64);
    s_copy(text + text_len * 47, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 48, "The syntax for this command is:", text_len, 
	    (ftnlen)31);
    s_copy(text + text_len * 49, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 50, "   SET AUTOADJUST (1:1){ OFF | ASK | ON }", 
	    text_len, (ftnlen)41);
    s_copy(text + text_len * 51, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 52, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 53, "To see the current status of AUTOADJUST ty"
	    "pe the command", text_len, (ftnlen)56);
    s_copy(text + text_len * 54, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 55, "SHOW FORMAT", text_len, (ftnlen)11);
    s_copy(text + text_len * 56, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 57, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 58, "Note: AUTOADJUST does not affect the width"
	    "s of numeric or time columns.", text_len, (ftnlen)71);
    s_copy(text + text_len * 59, "Only character columns are affected by AUT"
	    "OADJUST.", text_len, (ftnlen)50);
    s_copy(text + text_len * 60, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 61, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 62, "@@Autoadjust", text_len, (ftnlen)12);
    s_copy(text + text_len * 63, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + (text_len << 6), "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 65, " ", text_len, (ftnlen)1);
    finish[1] = 66;
    begin[2] = 67;
    s_copy(text + text_len * 66, "Inspekt allows you to collect frequently e"
	    "xecuted sequences", text_len, (ftnlen)59);
    s_copy(text + text_len * 67, "of command in files called Inspekt Procedu"
	    "re Files.", text_len, (ftnlen)51);
    s_copy(text + text_len * 68, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 69, "To create a procedure file,  simply type i"
	    "n a sequence of", text_len, (ftnlen)57);
    s_copy(text + text_len * 70, "commands as you would type them when enter"
	    "ing them", text_len, (ftnlen)50);
    s_copy(text + text_len * 71, "in Inspekt.", text_len, (ftnlen)11);
    s_copy(text + text_len * 72, "End each command with a semi-colon.  Start"
	    " every new command", text_len, (ftnlen)60);
    s_copy(text + text_len * 73, "on a new line.", text_len, (ftnlen)14);
    s_copy(text + text_len * 74, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 75, "You can insert a \"comment\" line in the p"
	    "rocedure file by starting", text_len, (ftnlen)65);
    s_copy(text + text_len * 76, "the line with a semi-colon (;).", text_len, 
	    (ftnlen)31);
    s_copy(text + text_len * 77, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 78, "To start a procedure file enter the comman"
	    "d:", text_len, (ftnlen)44);
    s_copy(text + text_len * 79, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 80, "START filename;", text_len, (ftnlen)15);
    s_copy(text + text_len * 81, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 82, "where \"filename\" above is replaced by th"
	    "e name of your procedure file.", text_len, (ftnlen)70);
    s_copy(text + text_len * 83, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 84, "Procedure files may start other procedure "
	    "files.  Procedure commands", text_len, (ftnlen)68);
    s_copy(text + text_len * 85, "may not use any of the following commands:",
	     text_len, (ftnlen)42);
    s_copy(text + text_len * 86, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 87, "EDIT", text_len, (ftnlen)4);
    s_copy(text + text_len * 88, "DO", text_len, (ftnlen)2);
    s_copy(text + text_len * 89, "RECALL", text_len, (ftnlen)6);
    s_copy(text + text_len * 90, "|endliteral", text_len, (ftnlen)11);
    return 0;
} /* zzhlp000_ */

