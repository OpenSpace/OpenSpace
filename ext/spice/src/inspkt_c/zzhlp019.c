/* zzhlp019.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZHLP019 ( private help text ) */
/* Subroutine */ int zzhlp019_(integer *begin, integer *finish, char *text, 
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
    s_copy(text + text_len * 1762, "the character \">\" in the leftmost colu"
	    "mn of the report.  You can", text_len, (ftnlen)64);
    s_copy(text + text_len * 1763, "modify this FORMAT MARK by issuing a SET"
	    " FORMAT MARK command.", text_len, (ftnlen)61);
    s_copy(text + text_len * 1764, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1765, "Decide what character you want to appear"
	    " in the leftmost column", text_len, (ftnlen)63);
    s_copy(text + text_len * 1766, "(remember you cannot use the semi-colon"
	    " \";\"). For the purpose of", text_len, (ftnlen)64);
    s_copy(text + text_len * 1767, "an example lets use \"=\". Then issue th"
	    "e command:", text_len, (ftnlen)48);
    s_copy(text + text_len * 1768, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1769, "SET FORMAT MARK =;", text_len, (ftnlen)18)
	    ;
    s_copy(text + text_len * 1770, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1771, "From this point forward MARKED TABULAR r"
	    "eports will place the character", text_len, (ftnlen)71);
    s_copy(text + text_len * 1772, "\"=\" in the left most column of reports."
	    , text_len, (ftnlen)39);
    s_copy(text + text_len * 1773, "@@SET FORMAT MARK ...", text_len, (ftnlen)
	    21);
    s_copy(text + text_len * 1774, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1775, "Help", text_len, (ftnlen)4);
    finish[45] = 1776;
    begin[46] = 1777;
    s_copy(text + text_len * 1776, "When Inspekt produces a report as the re"
	    "sult of a SELECT command,", text_len, (ftnlen)65);
    s_copy(text + text_len * 1777, "and the reporting format is some form of"
	    " tabular format, Inspekt", text_len, (ftnlen)64);
    s_copy(text + text_len * 1778, "can place a header at the beginning of t"
	    "he displayed output.  This", text_len, (ftnlen)66);
    s_copy(text + text_len * 1779, "header displays the names of the various"
	    " columns that appear in the", text_len, (ftnlen)67);
    s_copy(text + text_len * 1780, "report.  (If a column has an alias, the "
	    "alias will be used instead of", text_len, (ftnlen)69);
    s_copy(text + text_len * 1781, "the column name.)", text_len, (ftnlen)17);
    s_copy(text + text_len * 1782, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1783, "You can control how often these items ar"
	    "e placed in your report  by", text_len, (ftnlen)67);
    s_copy(text + text_len * 1784, "issuing a SET HEADER FREQUENCY command. "
	    " The syntax for this is:", text_len, (ftnlen)64);
    s_copy(text + text_len * 1785, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1786, "@exliteral", text_len, (ftnlen)10);
    s_copy(text + text_len * 1787, "SET HEADER FREQUENCY (1:1){ 0", text_len, 
	    (ftnlen)29);
    s_copy(text + text_len * 1788, "                          | 1ST", 
	    text_len, (ftnlen)31);
    s_copy(text + text_len * 1789, "                          | FIRST", 
	    text_len, (ftnlen)33);
    s_copy(text + text_len * 1790, "                          | ALL", 
	    text_len, (ftnlen)31);
    s_copy(text + text_len * 1791, "                          | EVERY @int(1"
	    ":)", text_len, (ftnlen)42);
    s_copy(text + text_len * 1792, "                          }", text_len, (
	    ftnlen)27);
    s_copy(text + text_len * 1793, "!endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1794, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1795, "The values mean respectively: on none of"
	    " the pages; on the first", text_len, (ftnlen)64);
    s_copy(text + text_len * 1796, "page only, on every page, and on the fir"
	    "st page and every nth page", text_len, (ftnlen)66);
    s_copy(text + text_len * 1797, "following the first page.", text_len, (
	    ftnlen)25);
    s_copy(text + text_len * 1798, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1799, "@@SET HEADER ...", text_len, (ftnlen)16);
    s_copy(text + text_len * 1800, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1801, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 1802, "SET TITLE  ...", text_len, (ftnlen)14);
    finish[46] = 1803;
    begin[47] = 1804;
    s_copy(text + text_len * 1803, "Reports written by Inspekt are modelled "
	    "as if they were being written to a page", text_len, (ftnlen)79);
    s_copy(text + text_len * 1804, "of fixed height and width.  This is conv"
	    "enient if you plan to save  the output", text_len, (ftnlen)78);
    s_copy(text + text_len * 1805, "of a file (using the SAVE TO command) an"
	    "d then print the the  resulting file.", text_len, (ftnlen)77);
    s_copy(text + text_len * 1806, "Moreover, if you have a long report, it "
	    "allows you to have  header and title", text_len, (ftnlen)76);
    s_copy(text + text_len * 1807, "information appear on your screen at reg"
	    "ular intervals.", text_len, (ftnlen)55);
    s_copy(text + text_len * 1808, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1809, "The default height and width of this pag"
	    "e are 20 and 80 characters respectively.", text_len, (ftnlen)80);
    s_copy(text + text_len * 1810, "To adjust these use the following comman"
	    "ds.", text_len, (ftnlen)43);
    s_copy(text + text_len * 1811, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1812, "SET PAGE HEIGHT @int(20:)", text_len, (
	    ftnlen)25);
    s_copy(text + text_len * 1813, "SET PAGE WIDTH  @int(40:132)", text_len, (
	    ftnlen)28);
    s_copy(text + text_len * 1814, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1815, "You may also set the title that appears "
	    "on reports with the command", text_len, (ftnlen)67);
    s_copy(text + text_len * 1816, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1817, "SET PAGE TITLE (1:)@word", text_len, (
	    ftnlen)24);
    s_copy(text + text_len * 1818, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1819, "Use the title \"NONE\" if you want no ti"
	    "tle to appear on report pages.", text_len, (ftnlen)68);
    s_copy(text + text_len * 1820, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1821, "@@SET PAGE   ...", text_len, (ftnlen)16);
    s_copy(text + text_len * 1822, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1823, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 1824, "SHOW PAGE        ...", text_len, (ftnlen)
	    20);
    finish[47] = 1825;
    begin[48] = 1826;
    s_copy(text + text_len * 1825, "There are two commands available for adj"
	    "usting the format of time strings when", text_len, (ftnlen)78);
    s_copy(text + text_len * 1826, "they are output in reports.  They are:", 
	    text_len, (ftnlen)38);
    s_copy(text + text_len * 1827, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1828, "SET COLUMN column_name FORMAT format;", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 1829, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1830, "SET DEFAULT TIME FORMAT format;", 
	    text_len, (ftnlen)31);
    s_copy(text + text_len * 1831, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1832, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1833, "@@SET TIME ...", text_len, (ftnlen)14);
    s_copy(text + text_len * 1834, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1835, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 1836, "Time Formats", text_len, (ftnlen)12);
    s_copy(text + text_len * 1837, "Default Time Format", text_len, (ftnlen)
	    19);
    s_copy(text + text_len * 1838, "Custom Formats", text_len, (ftnlen)14);
    finish[48] = 1839;
    begin[49] = 1840;
    s_copy(text + text_len * 1839, "When Inspekt produces a report as the re"
	    "sult of a SELECT command, it can place", text_len, (ftnlen)78);
    s_copy(text + text_len * 1840, "a title and a header on the report.", 
	    text_len, (ftnlen)35);
    s_copy(text + text_len * 1841, "You set a report title that appears on p"
	    "ages of a report by", text_len, (ftnlen)59);
    s_copy(text + text_len * 1842, "issuing the command:", text_len, (ftnlen)
	    20);
    s_copy(text + text_len * 1843, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1844, "SET PAGE TITLE title", text_len, (ftnlen)
	    20);
    s_copy(text + text_len * 1845, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1846, "Note that the TITLE is an attribute of t"
	    "he display page.  Whenever, a report", text_len, (ftnlen)76);
    s_copy(text + text_len * 1847, "is displayed in which a title is allowed"
	    ", the title you've set with the", text_len, (ftnlen)71);
    s_copy(text + text_len * 1848, "SET PAGE TITLE command will be the title"
	    " used.", text_len, (ftnlen)46);
    s_copy(text + text_len * 1849, " ", text_len, (ftnlen)1);
    return 0;
} /* zzhlp019_ */

