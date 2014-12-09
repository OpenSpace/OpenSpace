/* zzhlp022.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZHLP022 ( private help text ) */
/* Subroutine */ int zzhlp022_(integer *begin, integer *finish, char *text, 
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
    s_copy(text + text_len * 2032, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2033, "@numitem What leapsecond and SCLK kernel"
	    "s are loaded", text_len, (ftnlen)52);
    s_copy(text + text_len * 2034, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2035, "To see the current session environment, "
	    "type the command", text_len, (ftnlen)56);
    s_copy(text + text_len * 2036, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2037, "SHOW ENVIRONMENT", text_len, (ftnlen)16);
    s_copy(text + text_len * 2038, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2039, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2040, "@@SHOW ENVIRONMENT ...", text_len, (
	    ftnlen)22);
    s_copy(text + text_len * 2041, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 2042, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 2043, "Making Help Wait", text_len, (ftnlen)16);
    s_copy(text + text_len * 2044, "Echoing Translated Commands", text_len, (
	    ftnlen)27);
    s_copy(text + text_len * 2045, "Kernels            --- LOAD", text_len, (
	    ftnlen)27);
    finish[55] = 2046;
    begin[56] = 2047;
    s_copy(text + text_len * 2046, "You can see what report format is curren"
	    "tly active by typing the", text_len, (ftnlen)64);
    s_copy(text + text_len * 2047, "command:", text_len, (ftnlen)8);
    s_copy(text + (text_len << 11), "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2049, "SHOW FORMAT;", text_len, (ftnlen)12);
    s_copy(text + text_len * 2050, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2051, "In addition to showing you the format, i"
	    "t will show you the current", text_len, (ftnlen)67);
    s_copy(text + text_len * 2052, "format being used for presenting time an"
	    "d in the case of MARKED", text_len, (ftnlen)63);
    s_copy(text + text_len * 2053, "TABULAR format the current format mark. "
	    " You will also be given", text_len, (ftnlen)63);
    s_copy(text + text_len * 2054, "the current value for triggering a data "
	    "DELUGE WARNING.  An example", text_len, (ftnlen)67);
    s_copy(text + text_len * 2055, "result is given here.", text_len, (ftnlen)
	    21);
    s_copy(text + text_len * 2056, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2057, "Report Format          :  MARKED TABULAR",
	     text_len, (ftnlen)40);
    s_copy(text + text_len * 2058, "Report Mark            :  '>'", text_len, 
	    (ftnlen)29);
    s_copy(text + text_len * 2059, "Default Time     Format:  YYYY MON DD HR"
	    ":MN:SC::UTC::RND", text_len, (ftnlen)56);
    s_copy(text + text_len * 2060, "Default Integer  Format:  ###########", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 2061, "Default Floating Format:  #########.####",
	     text_len, (ftnlen)40);
    s_copy(text + text_len * 2062, "Deluge Warning         :  100", text_len, 
	    (ftnlen)29);
    s_copy(text + text_len * 2063, "Auto Adjust            :  ASK (applies o"
	    "nly to tabular formats)", text_len, (ftnlen)63);
    s_copy(text + text_len * 2064, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2065, "@@SHOW FORMAT   ...", text_len, (ftnlen)
	    19);
    s_copy(text + text_len * 2066, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 2067, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 2068, "Default Floating Format", text_len, (
	    ftnlen)23);
    s_copy(text + text_len * 2069, "Default Integer Format", text_len, (
	    ftnlen)22);
    s_copy(text + text_len * 2070, "Default Time Format", text_len, (ftnlen)
	    19);
    finish[56] = 2071;
    begin[57] = 2072;
    s_copy(text + text_len * 2071, "When you issue a select command to Inspe"
	    "kt, the speed", text_len, (ftnlen)53);
    s_copy(text + text_len * 2072, "with which it is executed may depend upo"
	    "n whether the", text_len, (ftnlen)53);
    s_copy(text + text_len * 2073, "columns referenced in the select command"
	    " are indexed.", text_len, (ftnlen)53);
    s_copy(text + text_len * 2074, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2075, "To get a list of all indexed columns, ty"
	    "pe the command", text_len, (ftnlen)54);
    s_copy(text + text_len * 2076, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2077, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2078, "SHOW INDEXED", text_len, (ftnlen)12);
    s_copy(text + text_len * 2079, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2080, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2081, "@@SHOW INDEXED  ...", text_len, (ftnlen)
	    19);
    s_copy(text + text_len * 2082, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 2083, "Help", text_len, (ftnlen)4);
    finish[57] = 2084;
    begin[58] = 2085;
    s_copy(text + text_len * 2084, "You can create a summary of the loaded E"
	    "-kernels by typing the", text_len, (ftnlen)62);
    s_copy(text + text_len * 2085, "command", text_len, (ftnlen)7);
    s_copy(text + text_len * 2086, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2087, "SHOW KERNELS;", text_len, (ftnlen)13);
    s_copy(text + text_len * 2088, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2089, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2090, "There are two main reasons for issuing t"
	    "his command:", text_len, (ftnlen)52);
    s_copy(text + text_len * 2091, "@newlist", text_len, (ftnlen)8);
    s_copy(text + text_len * 2092, "@numitem Obtaining a quick summary of lo"
	    "aded kernels", text_len, (ftnlen)52);
    s_copy(text + text_len * 2093, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2094, "@numitem Finding out whether the tables "
	    "and kernels you thought you", text_len, (ftnlen)67);
    s_copy(text + text_len * 2095, "         loaded were in fact loaded by I"
	    "nspekt.", text_len, (ftnlen)47);
    s_copy(text + text_len * 2096, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2097, "@@SHOW KERNELS     ...", text_len, (
	    ftnlen)22);
    s_copy(text + text_len * 2098, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 2099, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 2100, "SHOW COMMENTS ...", text_len, (ftnlen)17);
    finish[58] = 2101;
    begin[59] = 2102;
    s_copy(text + text_len * 2101, "You can see the current page settings (i"
	    "ncluding the report title", text_len, (ftnlen)65);
    s_copy(text + text_len * 2102, "and header attributes) by typing the com"
	    "mand:", text_len, (ftnlen)45);
    s_copy(text + text_len * 2103, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2104, "SHOW PAGE;", text_len, (ftnlen)10);
    s_copy(text + text_len * 2105, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2106, "A sample output is given below. (Note: t"
	    "he Page width refers to", text_len, (ftnlen)63);
    s_copy(text + text_len * 2107, "number of columns one character wide wil"
	    "l fit on the page.)", text_len, (ftnlen)59);
    s_copy(text + text_len * 2108, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2109, "Page height (rows)   :  20", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 2110, "Page width  (columns):  80", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 2111, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2112, "Page Title           :  Inspekt Report", 
	    text_len, (ftnlen)38);
    s_copy(text + text_len * 2113, "Title Justification  :  LEFT", text_len, (
	    ftnlen)28);
    s_copy(text + text_len * 2114, "Title Appears on     :  First page only", 
	    text_len, (ftnlen)39);
    s_copy(text + text_len * 2115, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2116, "Header Appears on    :  First page only", 
	    text_len, (ftnlen)39);
    s_copy(text + text_len * 2117, "@@SHOW PAGE        ...", text_len, (
	    ftnlen)22);
    s_copy(text + text_len * 2118, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 2119, "Help", text_len, (ftnlen)4);
    return 0;
} /* zzhlp022_ */

