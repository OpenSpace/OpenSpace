/* zzhlp016.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZHLP016 ( private help text ) */
/* Subroutine */ int zzhlp016_(integer *begin, integer *finish, char *text, 
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
    s_copy(text + text_len * 1489, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1490, "Patterns are useful only when they are u"
	    "sed to select matching", text_len, (ftnlen)62);
    s_copy(text + text_len * 1491, "strings.", text_len, (ftnlen)8);
    s_copy(text + text_len * 1492, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1493, "Pattern matches in Inspekt are case inse"
	    "nsitive: 'a' matches", text_len, (ftnlen)60);
    s_copy(text + text_len * 1494, "both 'a' and 'A'.  Upper and lower case "
	    "letters are said to", text_len, (ftnlen)59);
    s_copy(text + text_len * 1495, "be equivalent.", text_len, (ftnlen)14);
    s_copy(text + text_len * 1496, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1497, "If a pattern does not begin with a wild "
	    "card, a matching string", text_len, (ftnlen)63);
    s_copy(text + text_len * 1498, "must begin with the a character that is "
	    "equivalent to the beginning", text_len, (ftnlen)67);
    s_copy(text + text_len * 1499, "character of the pattern.  If a pattern "
	    "does not end with a wild", text_len, (ftnlen)64);
    s_copy(text + text_len * 1500, "card, a matching string must end with a "
	    "character that is equivalent", text_len, (ftnlen)68);
    s_copy(text + text_len * 1501, "to the ending character of the pattern.", 
	    text_len, (ftnlen)39);
    s_copy(text + text_len * 1502, "@@Patterns", text_len, (ftnlen)10);
    s_copy(text + text_len * 1503, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1504, "Help", text_len, (ftnlen)4);
    finish[36] = 1505;
    begin[37] = 1506;
    s_copy(text + text_len * 1505, "If you have a problem that you can't fig"
	    "ure out on", text_len, (ftnlen)50);
    s_copy(text + text_len * 1506, "your own, you can send it to NAIF at the"
	    " e-mail address", text_len, (ftnlen)55);
    s_copy(text + text_len * 1507, "given below.  However, please send Inspe"
	    "kt's log file", text_len, (ftnlen)53);
    s_copy(text + text_len * 1508, "along with a description of the problem."
	    "  The log file", text_len, (ftnlen)54);
    s_copy(text + text_len * 1509, "allows us to reproduce the problem.  We "
	    "may also need", text_len, (ftnlen)53);
    s_copy(text + text_len * 1510, "to request a copy of the E-kernels you w"
	    "ere using", text_len, (ftnlen)49);
    s_copy(text + text_len * 1511, "at the time you experienced the problem.",
	     text_len, (ftnlen)40);
    s_copy(text + text_len * 1512, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1513, "If you have comments or suggestions", 
	    text_len, (ftnlen)35);
    s_copy(text + text_len * 1514, "regarding Inspekt, send them to", 
	    text_len, (ftnlen)31);
    s_copy(text + text_len * 1515, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1516, "btaber@spice.jpl.nasa.gov", text_len, (
	    ftnlen)25);
    s_copy(text + text_len * 1517, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1518, "Within the limits of time and money, we "
	    "will do our best", text_len, (ftnlen)56);
    s_copy(text + text_len * 1519, "to respond to your correspondence.", 
	    text_len, (ftnlen)34);
    s_copy(text + text_len * 1520, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1521, "      ---Bill Taber", text_len, (ftnlen)
	    19);
    s_copy(text + text_len * 1522, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1523, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1524, "@@Problems, Suggestions", text_len, (
	    ftnlen)23);
    s_copy(text + text_len * 1525, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1526, "Help", text_len, (ftnlen)4);
    finish[37] = 1527;
    begin[38] = 1528;
    s_copy(text + text_len * 1527, "The word query is used in two ways in In"
	    "spekt.", text_len, (ftnlen)46);
    s_copy(text + text_len * 1528, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1529, "@newlist", text_len, (ftnlen)8);
    s_copy(text + text_len * 1530, "@numitem A query is a SELECT command iss"
	    "ued to Inspekt to search", text_len, (ftnlen)64);
    s_copy(text + text_len * 1531, "         loaded E-kernels for data that "
	    "satisfies a user specified", text_len, (ftnlen)66);
    s_copy(text + text_len * 1532, "         selection criteria", text_len, (
	    ftnlen)27);
    s_copy(text + text_len * 1533, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1534, "@numitem A query is a word in a command,"
	    " or symbol that ends in", text_len, (ftnlen)63);
    s_copy(text + text_len * 1535, "         a question mark.  When Inspekt "
	    "encounters such a word", text_len, (ftnlen)62);
    s_copy(text + text_len * 1536, "         in a command, it prompts the us"
	    "er for a value to replace", text_len, (ftnlen)65);
    s_copy(text + text_len * 1537, "         the \"query\"", text_len, (
	    ftnlen)20);
    s_copy(text + text_len * 1538, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1539, "@paritem For example suppose you enter t"
	    "he command", text_len, (ftnlen)50);
    s_copy(text + text_len * 1540, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1541, "@literalitem", text_len, (ftnlen)12);
    s_copy(text + text_len * 1542, "Inspekt> SHOW COLUMN NAME?;", text_len, (
	    ftnlen)27);
    s_copy(text + text_len * 1543, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1544, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1545, "@paritem Inspekt will ask you to supply "
	    "a value for NAME?  and then", text_len, (ftnlen)67);
    s_copy(text + text_len * 1546, "         continue acting upon the result"
	    "ing command. Normally you will", text_len, (ftnlen)70);
    s_copy(text + text_len * 1547, "         not type queries directly when "
	    "issuing a command. Instead,", text_len, (ftnlen)67);
    s_copy(text + text_len * 1548, "         you will define symbols that co"
	    "ntain queries or place", text_len, (ftnlen)62);
    s_copy(text + text_len * 1549, "         queries in commands that are co"
	    "llected in Inspekt procedure files.", text_len, (ftnlen)75);
    s_copy(text + text_len * 1550, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1551, "@@Query", text_len, (ftnlen)7);
    s_copy(text + text_len * 1552, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1553, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 1554, " ", text_len, (ftnlen)1);
    finish[38] = 1555;
    begin[39] = 1556;
    s_copy(text + text_len * 1555, "After you issue a SELECT command to Insp"
	    "ekt, the program presents", text_len, (ftnlen)65);
    s_copy(text + text_len * 1556, "a display of the items that match the se"
	    "lection criteria. The items", text_len, (ftnlen)67);
    s_copy(text + text_len * 1557, "displayed together with the method in wh"
	    "ich they are displayed", text_len, (ftnlen)62);
    s_copy(text + text_len * 1558, "is called a \"report\".", text_len, (
	    ftnlen)21);
    s_copy(text + text_len * 1559, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1560, "A report has several attributes that are"
	    " user adjustable.", text_len, (ftnlen)57);
    s_copy(text + text_len * 1561, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1562, "Reports are presented in a region that m"
	    "odels a physical page as you", text_len, (ftnlen)68);
    s_copy(text + text_len * 1563, "find in a loose leaf binder.  You may ad"
	    "just the height and width", text_len, (ftnlen)65);
    s_copy(text + text_len * 1564, "of these pages.", text_len, (ftnlen)15);
    s_copy(text + text_len * 1565, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1566, "A report is presented in a particular fo"
	    "rmat.   There are four basic", text_len, (ftnlen)68);
    s_copy(text + text_len * 1567, "formats: tabular, flagged, verbatim and "
	    "delimited.  You may select the format", text_len, (ftnlen)77);
    s_copy(text + text_len * 1568, "you prefer for reports the program creat"
	    "es.  Tabular formats are", text_len, (ftnlen)64);
    s_copy(text + text_len * 1569, "usually the easiest to read.  However, i"
	    "f events have a large number", text_len, (ftnlen)68);
    s_copy(text + text_len * 1570, "of attributes and you want to see many o"
	    "f these attributes, you may", text_len, (ftnlen)67);
    s_copy(text + text_len * 1571, "find the flagged format better suited to"
	    " your needs.  Verbatim format", text_len, (ftnlen)69);
    s_copy(text + text_len * 1572, "allows you to see most directly the actu"
	    "al contents of an event", text_len, (ftnlen)63);
    s_copy(text + text_len * 1573, "without any of Inspekt's formatting tool"
	    "s modifying line breaks", text_len, (ftnlen)63);
    s_copy(text + text_len * 1574, "in the original data.  However, this for"
	    "mat is difficult to read", text_len, (ftnlen)64);
    s_copy(text + text_len * 1575, "and suitable only when you need to see d"
	    "irectly how a particular", text_len, (ftnlen)64);
    s_copy(text + text_len * 1576, "event was stored in the E-kernel.  Final"
	    "ly, the delimited format creates", text_len, (ftnlen)72);
    s_copy(text + text_len * 1577, "a format suitable for import into common"
	    " spreadsheet programs.", text_len, (ftnlen)62);
    s_copy(text + text_len * 1578, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1579, "Every report has a title. You can remove"
	    " the title, change it and", text_len, (ftnlen)65);
    return 0;
} /* zzhlp016_ */

