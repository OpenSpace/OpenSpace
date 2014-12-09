/* zzhlp018.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZHLP018 ( private help text ) */
/* Subroutine */ int zzhlp018_(integer *begin, integer *finish, char *text, 
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
    s_copy(text + text_len * 1671, "interest.) To request columns in a repor"
	    "t you should type the first", text_len, (ftnlen)67);
    s_copy(text + text_len * 1672, "portion of the SELECT command as shown b"
	    "elow.", text_len, (ftnlen)45);
    s_copy(text + text_len * 1673, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1674, "SELECT col_1, col_2, ... , col_n", 
	    text_len, (ftnlen)32);
    s_copy(text + text_len * 1675, "FROM table ...", text_len, (ftnlen)14);
    s_copy(text + text_len * 1676, "WHERE condition ...", text_len, (ftnlen)
	    19);
    s_copy(text + text_len * 1677, "ORDER BY ...", text_len, (ftnlen)12);
    s_copy(text + text_len * 1678, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1679, "Note that commas (,) are required betwee"
	    "n column names.  If you", text_len, (ftnlen)63);
    s_copy(text + text_len * 1680, "select rows from a single table (i.e. th"
	    "e FROM clause lists a single", text_len, (ftnlen)68);
    s_copy(text + text_len * 1681, "table) you can just use the column names"
	    " from that table.  However,", text_len, (ftnlen)67);
    s_copy(text + text_len * 1682, "if you select columns from a \"join\" of"
	    " two or more tables and two", text_len, (ftnlen)65);
    s_copy(text + text_len * 1683, "or more of the tables possess the column"
	    " you want to see you need", text_len, (ftnlen)65);
    s_copy(text + text_len * 1684, "to make sure you tell Inspekt which tabl"
	    "e the column belongs to.", text_len, (ftnlen)64);
    s_copy(text + text_len * 1685, "To do this you attach the name of the ta"
	    "ble (or an alias for that", text_len, (ftnlen)65);
    s_copy(text + text_len * 1686, "table) to the front of the column name a"
	    "s in", text_len, (ftnlen)44);
    s_copy(text + text_len * 1687, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1688, "   table.column", text_len, (ftnlen)15);
    s_copy(text + text_len * 1689, "or", text_len, (ftnlen)2);
    s_copy(text + text_len * 1690, "   alias.column", text_len, (ftnlen)15);
    s_copy(text + text_len * 1691, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1692, "The columns will be displayed in the rep"
	    "ort in the same order as you list", text_len, (ftnlen)73);
    s_copy(text + text_len * 1693, "them in the select clause.", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 1694, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1695, "@@Select Clause", text_len, (ftnlen)15);
    s_copy(text + text_len * 1696, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1697, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 1698, "Looking at Data    --- SELECT", text_len, 
	    (ftnlen)29);
    finish[42] = 1699;
    begin[43] = 1700;
    s_copy(text + text_len * 1699, "To adjust the display attributes of a co"
	    "lumn use the SET COLUMN", text_len, (ftnlen)63);
    s_copy(text + text_len * 1700, "command.  You may adjust the width of a "
	    "column, the heading that", text_len, (ftnlen)64);
    s_copy(text + text_len * 1701, "will appear in  tabular reports, whether"
	    " or not columns will be", text_len, (ftnlen)63);
    s_copy(text + text_len * 1702, "left or right justified  in reports, and"
	    " the format of the column", text_len, (ftnlen)65);
    s_copy(text + text_len * 1703, "for time and numeric columns.  The synta"
	    "x of the SET COLUMN command", text_len, (ftnlen)67);
    s_copy(text + text_len * 1704, "is:", text_len, (ftnlen)3);
    s_copy(text + text_len * 1705, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1706, "SET COLUMN @name", text_len, (ftnlen)16);
    s_copy(text + text_len * 1707, " (1:1){ WIDTH @int(8:80)", text_len, (
	    ftnlen)24);
    s_copy(text + text_len * 1708, "      | FORMAT    (1:)@word", text_len, (
	    ftnlen)27);
    s_copy(text + text_len * 1709, "      | HEADING   (1:)@word", text_len, (
	    ftnlen)27);
    s_copy(text + text_len * 1710, "      | JUSTIFICATION RIGHT", text_len, (
	    ftnlen)27);
    s_copy(text + text_len * 1711, "      | JUSTIFICATION LEFT }", text_len, (
	    ftnlen)28);
    s_copy(text + text_len * 1712, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1713, "NOTE: If you modify the format of a nume"
	    "ric or time column, the", text_len, (ftnlen)63);
    s_copy(text + text_len * 1714, "width of the column is automatically adj"
	    "usted to match the", text_len, (ftnlen)58);
    s_copy(text + text_len * 1715, "width of the new format.  You must re-ad"
	    "just the width if you", text_len, (ftnlen)61);
    s_copy(text + text_len * 1716, "want it to be something other than the w"
	    "idth of the format.", text_len, (ftnlen)59);
    s_copy(text + text_len * 1717, "@@SET COLUMN ...", text_len, (ftnlen)16);
    s_copy(text + text_len * 1718, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1719, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 1720, "Time Formats", text_len, (ftnlen)12);
    s_copy(text + text_len * 1721, "Numeric Formats", text_len, (ftnlen)15);
    s_copy(text + text_len * 1722, "SHOW COLUMN   ...", text_len, (ftnlen)17);
    finish[43] = 1723;
    begin[44] = 1724;
    s_copy(text + text_len * 1723, "Inspekt supports 4 basic report formats:"
	    " TABULAR, FLAGGED, VERBATIM and", text_len, (ftnlen)71);
    s_copy(text + text_len * 1724, "DELIMITED. These are set using the SET F"
	    "ORMAT command. The syntax for", text_len, (ftnlen)69);
    s_copy(text + text_len * 1725, "this command is:", text_len, (ftnlen)16);
    s_copy(text + text_len * 1726, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1727, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1728, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1729, "SET FORMAT (0:1){ MARKED | SPACED }", 
	    text_len, (ftnlen)35);
    s_copy(text + text_len * 1730, "   TABULAR (0:1){ PRESERVED }", text_len, 
	    (ftnlen)29);
    s_copy(text + text_len * 1731, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1732, "SET FORMAT FLAGGED (0:1){ PRESERVED }", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 1733, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1734, "SET FORMAT VERBATIM", text_len, (ftnlen)
	    19);
    s_copy(text + text_len * 1735, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1736, "SET FORMAT DELIMITED (0:1){ PRESERVED }", 
	    text_len, (ftnlen)39);
    s_copy(text + text_len * 1737, "                     (0:2){ DELIMITER @w"
	    "ord(SPACE|%)", text_len, (ftnlen)52);
    s_copy(text + text_len * 1738, "                          | QUOTE @word("
	    "%) }", text_len, (ftnlen)44);
    s_copy(text + text_len * 1739, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1740, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1741, "TABULAR reports show each event as a row"
	    " of a table with the various", text_len, (ftnlen)68);
    s_copy(text + text_len * 1742, "event attributes appearing in fixed colu"
	    "mns.", text_len, (ftnlen)44);
    s_copy(text + text_len * 1743, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1744, "FLAGGED reports show events as a vertica"
	    "l list of name-value paragraphs", text_len, (ftnlen)71);
    s_copy(text + text_len * 1745, "with each complete event separated from "
	    "the next by a blank line.", text_len, (ftnlen)65);
    s_copy(text + text_len * 1746, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1747, "VERBATIM reports list events as a vertic"
	    "al list as well. However the", text_len, (ftnlen)68);
    s_copy(text + text_len * 1748, "column name is listed on one line and it"
	    "s value on the next line(s).", text_len, (ftnlen)68);
    s_copy(text + text_len * 1749, "This  presents the components of each co"
	    "lumn without line breaks.", text_len, (ftnlen)65);
    s_copy(text + text_len * 1750, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1751, "DELIMITED reports are used to export an "
	    "Inspekt report into a tab (or", text_len, (ftnlen)69);
    s_copy(text + text_len * 1752, "otherwise delimited) format suitable for"
	    " importing into spreadsheet", text_len, (ftnlen)67);
    s_copy(text + text_len * 1753, "programs such as Microsoft Excel.", 
	    text_len, (ftnlen)33);
    s_copy(text + text_len * 1754, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1755, "@@SET FORMAT ...", text_len, (ftnlen)16);
    s_copy(text + text_len * 1756, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1757, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 1758, "Reports", text_len, (ftnlen)7);
    s_copy(text + text_len * 1759, "SET FORMAT MARK ...", text_len, (ftnlen)
	    19);
    s_copy(text + text_len * 1760, "SHOW FORMAT     ...", text_len, (ftnlen)
	    19);
    finish[44] = 1761;
    begin[45] = 1762;
    s_copy(text + text_len * 1761, "When you use  MARKED TABULAR formats for"
	    " reports, Inspekt places", text_len, (ftnlen)64);
    return 0;
} /* zzhlp018_ */

