/* zzhlp014.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZHLP014 ( private help text ) */
/* Subroutine */ int zzhlp014_(integer *begin, integer *finish, char *text, 
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
    s_copy(text + text_len * 1307, "[NOT] column <relation> value", text_len, 
	    (ftnlen)29);
    s_copy(text + text_len * 1308, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1309, "If", text_len, (ftnlen)2);
    s_copy(text + text_len * 1310, "the column is a character or time column"
	    ", the value must be enclosed in either s", text_len, (ftnlen)80);
    s_copy(text + text_len * 1311, "Allowed relations are EQ NE LT LE GT GE "
	    "and LIKE (used for pattern", text_len, (ftnlen)66);
    s_copy(text + text_len * 1312, "matching).", text_len, (ftnlen)10);
    s_copy(text + text_len * 1313, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1314, "@@Looking at Data    --- SELECT", 
	    text_len, (ftnlen)31);
    s_copy(text + text_len * 1315, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1316, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 1317, "Column and Table Abbreviations", text_len,
	     (ftnlen)30);
    s_copy(text + text_len * 1318, "Select Clause", text_len, (ftnlen)13);
    s_copy(text + text_len * 1319, "From Clause", text_len, (ftnlen)11);
    s_copy(text + text_len * 1320, "Where Clause", text_len, (ftnlen)12);
    s_copy(text + text_len * 1321, "Order By", text_len, (ftnlen)8);
    s_copy(text + text_len * 1322, "Combining Tables", text_len, (ftnlen)16);
    s_copy(text + text_len * 1323, "Reports", text_len, (ftnlen)7);
    s_copy(text + text_len * 1324, "Getting Too Much Data", text_len, (ftnlen)
	    21);
    s_copy(text + text_len * 1325, " ", text_len, (ftnlen)1);
    finish[30] = 1326;
    begin[31] = 1327;
    s_copy(text + text_len * 1326, "When you select a help topic for which t"
	    "here is some text that", text_len, (ftnlen)62);
    s_copy(text + text_len * 1327, "should be displayed, the help system beg"
	    "ins sending this text", text_len, (ftnlen)61);
    s_copy(text + text_len * 1328, "to your display.  If there is a lot of t"
	    "ext, some of it may", text_len, (ftnlen)59);
    s_copy(text + text_len * 1329, "scroll by before you have a chance to re"
	    "ad it.  There", text_len, (ftnlen)53);
    s_copy(text + text_len * 1330, "are two ways to deal with this.", 
	    text_len, (ftnlen)31);
    s_copy(text + text_len * 1331, "@newlist", text_len, (ftnlen)8);
    s_copy(text + text_len * 1332, "@numitem You can hit CTRL-S on your keyp"
	    "ad to cause output", text_len, (ftnlen)58);
    s_copy(text + text_len * 1333, "to your display to be temporarily disabl"
	    "ed. Hit CTRL-Q to restart", text_len, (ftnlen)65);
    s_copy(text + text_len * 1334, "the output.  This works on most terminal"
	    "s and terminal emulators.", text_len, (ftnlen)65);
    s_copy(text + text_len * 1335, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1336, "@numitem You can tell Inspekt to wait on"
	    "ce it finishes displaying", text_len, (ftnlen)65);
    s_copy(text + text_len * 1337, "a page full of text.", text_len, (ftnlen)
	    20);
    s_copy(text + text_len * 1338, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1339, "To do this at the Inspekt prompt type:", 
	    text_len, (ftnlen)38);
    s_copy(text + text_len * 1340, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1341, "Inspekt> SET HELP WAIT;", text_len, (
	    ftnlen)23);
    s_copy(text + text_len * 1342, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1343, "The Inspekt help system will now pause a"
	    "fter each page of", text_len, (ftnlen)57);
    s_copy(text + text_len * 1344, "text it displays and wait for you to hit"
	    " a carriage return before", text_len, (ftnlen)65);
    s_copy(text + text_len * 1345, "it displays the next page or related top"
	    "ics menu.", text_len, (ftnlen)49);
    s_copy(text + text_len * 1346, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1347, "To return to the original help system be"
	    "haviour, type the command", text_len, (ftnlen)65);
    s_copy(text + text_len * 1348, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1349, "Inspekt> SET HELP NO WAIT;", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 1350, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1351, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1352, "Note that the size of a page is also und"
	    "er your control.  You may", text_len, (ftnlen)65);
    s_copy(text + text_len * 1353, "set the number of lines that will fit on"
	    " a page by using the", text_len, (ftnlen)60);
    s_copy(text + text_len * 1354, "command \"SET PAGE HEIGHT\".  This comma"
	    "nd is described in the", text_len, (ftnlen)60);
    s_copy(text + text_len * 1355, "\"SET PAGE ...\" help topic.", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 1356, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1357, "@@Making Help Wait", text_len, (ftnlen)18)
	    ;
    s_copy(text + text_len * 1358, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1359, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 1360, "SET PAGE   ...", text_len, (ftnlen)14);
    finish[31] = 1361;
    begin[32] = 1362;
    s_copy(text + text_len * 1361, "A numeric format is specified by creatin"
	    "g a picture of the", text_len, (ftnlen)58);
    s_copy(text + text_len * 1362, "format.  For example to specify that a n"
	    "umber should start", text_len, (ftnlen)58);
    s_copy(text + text_len * 1363, "with 3 digits and be displayed to 3 deci"
	    "mal places use", text_len, (ftnlen)54);
    s_copy(text + text_len * 1364, "a picture such as this:", text_len, (
	    ftnlen)23);
    s_copy(text + text_len * 1365, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1366, "###.###", text_len, (ftnlen)7);
    s_copy(text + text_len * 1367, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1368, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1369, "If the first character of the picture is"
	    " a minus sign,", text_len, (ftnlen)54);
    s_copy(text + text_len * 1370, "the first character in the output string"
	    " will be", text_len, (ftnlen)48);
    s_copy(text + text_len * 1371, "a blank if the number is non-negative, a"
	    " minus sign", text_len, (ftnlen)51);
    s_copy(text + text_len * 1372, "if the number is negative.", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 1373, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1374, "If the first character of the picture is"
	    " a plus sign,", text_len, (ftnlen)53);
    s_copy(text + text_len * 1375, "the first character of the output string"
	    " will be a", text_len, (ftnlen)50);
    s_copy(text + text_len * 1376, "plus if the number is positive, a blank "
	    "if the number", text_len, (ftnlen)53);
    s_copy(text + text_len * 1377, "is zero, and a minus sign if the number "
	    "is negative.", text_len, (ftnlen)52);
    s_copy(text + text_len * 1378, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1379, "If the first character of the string is "
	    "NOT a sign", text_len, (ftnlen)50);
    s_copy(text + text_len * 1380, "(plus or minus) the first character of t"
	    "he output", text_len, (ftnlen)49);
    s_copy(text + text_len * 1381, "string will be a minus sign if the numbe"
	    "r is negative", text_len, (ftnlen)53);
    s_copy(text + text_len * 1382, "and will be the first character of the i"
	    "nteger part", text_len, (ftnlen)51);
    s_copy(text + text_len * 1383, "of the number otherwise.", text_len, (
	    ftnlen)24);
    s_copy(text + text_len * 1384, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1385, "The integer portion of STRING will conta"
	    "in at least", text_len, (ftnlen)51);
    s_copy(text + text_len * 1386, "as many characters as appear before the "
	    "decimal point", text_len, (ftnlen)53);
    s_copy(text + text_len * 1387, "(or last character if there is no decima"
	    "l point) but", text_len, (ftnlen)52);
    s_copy(text + text_len * 1388, "after a leading + or -. There will ALWAY"
	    "S be at least", text_len, (ftnlen)53);
    s_copy(text + text_len * 1389, "one digit output in integer portion of S"
	    "TRING.", text_len, (ftnlen)46);
    s_copy(text + text_len * 1390, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1391, "If the picture begins with a any of the "
	    "following", text_len, (ftnlen)49);
    s_copy(text + text_len * 1392, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1393, "   '+0', '-0', or '0'", text_len, (ftnlen)
	    21);
    s_copy(text + text_len * 1394, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1395, "it is said to have a leading zero.  If a"
	    " picture has", text_len, (ftnlen)52);
    s_copy(text + text_len * 1396, "a leading zero and the integer portion i"
	    "s not large", text_len, (ftnlen)51);
    s_copy(text + text_len * 1397, "enough to fill up the integer space spec"
	    "ified by", text_len, (ftnlen)48);
    s_copy(text + text_len * 1398, "the picture, the output will be zero pad"
	    "ded from the sign (if", text_len, (ftnlen)61);
    s_copy(text + text_len * 1399, "one is required) up to the first charact"
	    "er of the", text_len, (ftnlen)49);
    s_copy(text + text_len * 1400, "integer part of the number.", text_len, (
	    ftnlen)27);
    return 0;
} /* zzhlp014_ */

