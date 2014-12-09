/* zzhlp017.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZHLP017 ( private help text ) */
/* Subroutine */ int zzhlp017_(integer *begin, integer *finish, char *text, 
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
    s_copy(text + text_len * 1580, "alter the frequency with which it is dis"
	    "played (only at the", text_len, (ftnlen)59);
    s_copy(text + text_len * 1581, "beginning of the report, on every page, "
	    "or every n'th page). You", text_len, (ftnlen)64);
    s_copy(text + text_len * 1582, "can also control whether the title is le"
	    "ft justified, right justified", text_len, (ftnlen)69);
    s_copy(text + text_len * 1583, "or centered on the report page.", 
	    text_len, (ftnlen)31);
    s_copy(text + text_len * 1584, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1585, "Reports in tabular format, have a header"
	    " as well as a title.  The", text_len, (ftnlen)65);
    s_copy(text + text_len * 1586, "header names the various columns that ap"
	    "pear in the report.  As with", text_len, (ftnlen)68);
    s_copy(text + text_len * 1587, "the report title you can adjust the freq"
	    "uency with which the header", text_len, (ftnlen)67);
    s_copy(text + text_len * 1588, "is displayed in the report.", text_len, (
	    ftnlen)27);
    s_copy(text + text_len * 1589, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1590, "@@Reports", text_len, (ftnlen)9);
    s_copy(text + text_len * 1591, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1592, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 1593, "Tabular Format", text_len, (ftnlen)14);
    s_copy(text + text_len * 1594, "Flagged Format", text_len, (ftnlen)14);
    s_copy(text + text_len * 1595, "Verbatim Format", text_len, (ftnlen)15);
    s_copy(text + text_len * 1596, "Delimited Format", text_len, (ftnlen)16);
    s_copy(text + text_len * 1597, "SET FORMAT ...", text_len, (ftnlen)14);
    s_copy(text + text_len * 1598, "SET HEADER ...", text_len, (ftnlen)14);
    s_copy(text + text_len * 1599, "SET TITLE  ...", text_len, (ftnlen)14);
    s_copy(text + text_len * 1600, "SET PAGE   ...", text_len, (ftnlen)14);
    finish[39] = 1601;
    begin[40] = 1602;
    s_copy(text + text_len * 1601, "When you first open an E-kernel, it is o"
	    "ften not clear what range", text_len, (ftnlen)65);
    s_copy(text + text_len * 1602, "of events are present in the kernel.  In"
	    " such situations, simply", text_len, (ftnlen)64);
    s_copy(text + text_len * 1603, "selecting events may result in much more"
	    " data that you can easily", text_len, (ftnlen)65);
    s_copy(text + text_len * 1604, "view.  For this reason you may preface y"
	    "our selection command with", text_len, (ftnlen)66);
    s_copy(text + text_len * 1605, "a \"SAMPLE\" clause. By doing so, you ca"
	    "n view a reasonable subset", text_len, (ftnlen)64);
    s_copy(text + text_len * 1606, "of the events that match your selection "
	    "criteria.  There are several", text_len, (ftnlen)68);
    s_copy(text + text_len * 1607, "forms of the SAMPLE clause.  However, al"
	    "l sample commands are formed", text_len, (ftnlen)68);
    s_copy(text + text_len * 1608, "the same way: type the SAMPLE clause and"
	    " follow it by the SELECT", text_len, (ftnlen)64);
    s_copy(text + text_len * 1609, "statement you would issue if you wanted "
	    "to view all of the matching", text_len, (ftnlen)67);
    s_copy(text + text_len * 1610, "events of a selection.", text_len, (
	    ftnlen)22);
    s_copy(text + text_len * 1611, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1612, "The SAMPLE clauses that can prefixed to "
	    "a SELECT command", text_len, (ftnlen)56);
    s_copy(text + text_len * 1613, "are shown below:", text_len, (ftnlen)16);
    s_copy(text + text_len * 1614, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1615, "SAMPLE number", text_len, (ftnlen)13);
    s_copy(text + text_len * 1616, "SAMPLE FIRST number", text_len, (ftnlen)
	    19);
    s_copy(text + text_len * 1617, "SAMPLE LAST  number", text_len, (ftnlen)
	    19);
    s_copy(text + text_len * 1618, "SAMPLE number FROM  percentile TO percen"
	    "tile", text_len, (ftnlen)44);
    s_copy(text + text_len * 1619, "SAMPLE number UP TO       percentile [EV"
	    "ERY number]", text_len, (ftnlen)51);
    s_copy(text + text_len * 1620, "SAMPLE number STARTING AT percentile [EV"
	    "ERY number]", text_len, (ftnlen)51);
    s_copy(text + text_len * 1621, "SAMPLE number CENTER   AT percentile [EV"
	    "ERY number]", text_len, (ftnlen)51);
    s_copy(text + text_len * 1622, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1623, "The first \"number\" in each clause is t"
	    "he number of samples to", text_len, (ftnlen)61);
    s_copy(text + text_len * 1624, "extract from a selection. Percentiles mu"
	    "st be integers from 0 to", text_len, (ftnlen)64);
    s_copy(text + text_len * 1625, "100. The 0th percentile corresponds to t"
	    "he first event of a selection;", text_len, (ftnlen)70);
    s_copy(text + text_len * 1626, "the 100th percentile to the last event. "
	    "In those clauses with an", text_len, (ftnlen)64);
    s_copy(text + text_len * 1627, "optional \"EVERY\" subclause, the \"EVER"
	    "Y-number\" refers to how many", text_len, (ftnlen)65);
    s_copy(text + text_len * 1628, "events to skip between displayed events."
	    " If not supplied the", text_len, (ftnlen)60);
    s_copy(text + text_len * 1629, "\"EVERY-number\" value is assumed to be "
	    "one (1).", text_len, (ftnlen)46);
    s_copy(text + text_len * 1630, "@@Sampling Data", text_len, (ftnlen)15);
    s_copy(text + text_len * 1631, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1632, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 1633, "Looking at Data    --- SELECT", text_len, 
	    (ftnlen)29);
    finish[40] = 1634;
    begin[41] = 1635;
    s_copy(text + text_len * 1634, "To save output that is printed on your t"
	    "erminal (or in your terminal window)", text_len, (ftnlen)76);
    s_copy(text + text_len * 1635, "issue the command:", text_len, (ftnlen)18)
	    ;
    s_copy(text + text_len * 1636, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1637, "SAVE TO filename;", text_len, (ftnlen)17);
    s_copy(text + text_len * 1638, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1639, "All subsequent screen output will be sav"
	    "ed the the file specified", text_len, (ftnlen)65);
    s_copy(text + text_len * 1640, "in the command. To stop saving to the SA"
	    "VE file, issue the command", text_len, (ftnlen)66);
    s_copy(text + text_len * 1641, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1642, "DISCARD;", text_len, (ftnlen)8);
    s_copy(text + text_len * 1643, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1644, "You may SAVE and DISCARD output as frequ"
	    "ently as you wish.  Moreover,", text_len, (ftnlen)69);
    s_copy(text + text_len * 1645, "a different file may be used with each S"
	    "AVE command. However, to", text_len, (ftnlen)64);
    s_copy(text + text_len * 1646, "begin saving output to a new file you mu"
	    "st first issue a DISCARD", text_len, (ftnlen)64);
    s_copy(text + text_len * 1647, "command.  If you use the same filename y"
	    "ou may (depending upon your", text_len, (ftnlen)67);
    s_copy(text + text_len * 1648, "system) overwrite the previous file.", 
	    text_len, (ftnlen)36);
    s_copy(text + text_len * 1649, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1650, "Regardless of whether you save any scree"
	    "n output, Inspekt automatically", text_len, (ftnlen)71);
    s_copy(text + text_len * 1651, "saves every command you type at the \"In"
	    "spekt>\" prompt.  These commands", text_len, (ftnlen)70);
    s_copy(text + text_len * 1652, "are collected in the Inspekt Log File.  "
	    "It has a name of the form", text_len, (ftnlen)65);
    s_copy(text + text_len * 1653, "\"nsp#####.log\" where each '#' is a dec"
	    "imal digit. In addition to", text_len, (ftnlen)64);
    s_copy(text + text_len * 1654, "commands you type, the text of any error"
	    " message that Inspekt produces", text_len, (ftnlen)70);
    s_copy(text + text_len * 1655, "are recorded in the log file.", text_len, 
	    (ftnlen)29);
    s_copy(text + text_len * 1656, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1657, "The log file is a crucial aid in determi"
	    "ning why Inspekt sometimes", text_len, (ftnlen)66);
    s_copy(text + text_len * 1658, "behaves in unexpected ways. Each log fil"
	    "e is in fact an Inspekt", text_len, (ftnlen)63);
    s_copy(text + text_len * 1659, "procedure that you can execute to exactl"
	    "y the sequence of commands", text_len, (ftnlen)66);
    s_copy(text + text_len * 1660, "you entered in some previous Inspekt wor"
	    "k session.  If you have", text_len, (ftnlen)63);
    s_copy(text + text_len * 1661, "a problem with Inspekt, and need our ass"
	    "istance, be sure to save the", text_len, (ftnlen)68);
    s_copy(text + text_len * 1662, "log file so that we can duplicate your p"
	    "roblems.", text_len, (ftnlen)48);
    s_copy(text + text_len * 1663, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1664, "@@Saving Work        --- SAVE TO", 
	    text_len, (ftnlen)32);
    s_copy(text + text_len * 1665, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1666, "Help", text_len, (ftnlen)4);
    finish[41] = 1667;
    begin[42] = 1668;
    s_copy(text + text_len * 1667, "When you issue a select command, you are"
	    " telling Inspekt to find", text_len, (ftnlen)64);
    s_copy(text + text_len * 1668, "rows from a table (or join of tables) as"
	    " specified by a FROM clause.", text_len, (ftnlen)68);
    s_copy(text + text_len * 1669, "The rows match some criterion specified "
	    "in a \"WHERE\" clause. (If", text_len, (ftnlen)64);
    s_copy(text + text_len * 1670, "no \"WHERE\" clause is specified, all ev"
	    "ents are considered to be of", text_len, (ftnlen)66);
    return 0;
} /* zzhlp017_ */

