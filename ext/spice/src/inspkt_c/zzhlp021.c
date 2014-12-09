/* zzhlp021.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZHLP021 ( private help text ) */
/* Subroutine */ int zzhlp021_(integer *begin, integer *finish, char *text, 
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
    s_copy(text + text_len * 1941, "where topic title is the title that appe"
	    "ars in one of the help menus.", text_len, (ftnlen)69);
    s_copy(text + text_len * 1942, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1943, "You don't have to enter the title exactl"
	    "y as it appears in the help system", text_len, (ftnlen)74);
    s_copy(text + text_len * 1944, "menus.  The help topic search does not d"
	    "epend upon the case of the letters", text_len, (ftnlen)74);
    s_copy(text + text_len * 1945, "you use.  Also, you may use a wild card "
	    "pattern for the topic title.", text_len, (ftnlen)68);
    s_copy(text + text_len * 1946, "This way you don't have to remember", 
	    text_len, (ftnlen)35);
    s_copy(text + text_len * 1947, "the exact topic.  However, you do run a "
	    "slight risk that some other help", text_len, (ftnlen)72);
    s_copy(text + text_len * 1948, "topic will match your pattern.  If more "
	    "than one topic matches the pattern,", text_len, (ftnlen)75);
    s_copy(text + text_len * 1949, "Inspekt will choose the one that occurs "
	    "first in an alphabetical listing", text_len, (ftnlen)72);
    s_copy(text + text_len * 1950, "of the help topics.", text_len, (ftnlen)
	    19);
    s_copy(text + text_len * 1951, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1952, "Once you are in the help system, you mus"
	    "t use the menus to navigate the", text_len, (ftnlen)71);
    s_copy(text + text_len * 1953, "various help topics.  You can not enter "
	    "the name of some other topic and", text_len, (ftnlen)72);
    s_copy(text + text_len * 1954, "display it directly.", text_len, (ftnlen)
	    20);
    s_copy(text + text_len * 1955, "@@Short Cut to Topics", text_len, (ftnlen)
	    21);
    s_copy(text + text_len * 1956, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1957, "Help", text_len, (ftnlen)4);
    finish[52] = 1958;
    begin[53] = 1959;
    s_copy(text + text_len * 1958, "You can get a snapshot of all of the att"
	    "ributes of a column (both", text_len, (ftnlen)65);
    s_copy(text + text_len * 1959, "the user adjustable attributes and fixed"
	    " attributes) by issuing", text_len, (ftnlen)63);
    s_copy(text + text_len * 1960, "the command.", text_len, (ftnlen)12);
    s_copy(text + text_len * 1961, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1962, "SHOW COLUMN column_name;", text_len, (
	    ftnlen)24);
    s_copy(text + text_len * 1963, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1964, "If more than one table possess a column "
	    "with your name", text_len, (ftnlen)54);
    s_copy(text + text_len * 1965, "you must specify which column you are ta"
	    "lking about.", text_len, (ftnlen)52);
    s_copy(text + text_len * 1966, "Do this by prefixing the table name to t"
	    "he column name as", text_len, (ftnlen)57);
    s_copy(text + text_len * 1967, "in", text_len, (ftnlen)2);
    s_copy(text + text_len * 1968, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1969, "SHOW COLUMN table.column_name", text_len, 
	    (ftnlen)29);
    s_copy(text + text_len * 1970, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1971, "where <table> is the name of the column "
	    "of interest .", text_len, (ftnlen)53);
    s_copy(text + text_len * 1972, "Below is a possible result of the comman"
	    "d SHOW COLUMN ACTIVITY;", text_len, (ftnlen)63);
    s_copy(text + text_len * 1973, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 1974, "Attributes of column:     :  ACTIVITY", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 1975, "Type                      :  CHARACTER*("
	    "32)", text_len, (ftnlen)43);
    s_copy(text + text_len * 1976, "Indexed                   :  YES", 
	    text_len, (ftnlen)32);
    s_copy(text + text_len * 1977, "Number of Components      :  1", text_len,
	     (ftnlen)30);
    s_copy(text + text_len * 1978, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1979, "User Adjustable Attributes", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 1980, "Column justification      :  LEFT", 
	    text_len, (ftnlen)33);
    s_copy(text + text_len * 1981, "Column width              :  32", 
	    text_len, (ftnlen)31);
    s_copy(text + text_len * 1982, "Column heading            :  ACTIVITY", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 1983, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 1984, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1985, "@@SHOW COLUMN   ...", text_len, (ftnlen)
	    19);
    s_copy(text + text_len * 1986, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 1987, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 1988, "SET COLUMN ...", text_len, (ftnlen)14);
    finish[53] = 1989;
    begin[54] = 1990;
    s_copy(text + text_len * 1989, "Every SPICE kernel provides a mechanism "
	    "for the creator of the", text_len, (ftnlen)62);
    s_copy(text + text_len * 1990, "product to attach documentation to the k"
	    "ernel.  This documentation is stored in", text_len, (ftnlen)79);
    s_copy(text + text_len * 1991, "of the file called the \"comments\" area."
	    , text_len, (ftnlen)39);
    s_copy(text + text_len * 1992, "All kernels should", text_len, (ftnlen)18)
	    ;
    s_copy(text + text_len * 1993, "have a non-empty comment section. The co"
	    "mments typically will", text_len, (ftnlen)61);
    s_copy(text + text_len * 1994, "provide information on one or more of th"
	    "e following items:", text_len, (ftnlen)58);
    s_copy(text + text_len * 1995, "@newlist", text_len, (ftnlen)8);
    s_copy(text + text_len * 1996, "@numitem the date the kernel was created,"
	    , text_len, (ftnlen)41);
    s_copy(text + text_len * 1997, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 1998, "@numitem who created it,", text_len, (
	    ftnlen)24);
    s_copy(text + text_len * 1999, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2000, "@numitem who to contact if you have ques"
	    "tions about the kernel,", text_len, (ftnlen)63);
    s_copy(text + text_len * 2001, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2002, "@numitem the intended set of users of th"
	    "e kernel,", text_len, (ftnlen)49);
    s_copy(text + text_len * 2003, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2004, "@numitem special notes regarding the con"
	    "tents of the kernel.", text_len, (ftnlen)60);
    s_copy(text + text_len * 2005, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2006, "To see the comments stored in a loaded E"
	    "-kernel issue the command", text_len, (ftnlen)65);
    s_copy(text + text_len * 2007, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 2008, "SHOW COMMENTS pattern", text_len, (ftnlen)
	    21);
    s_copy(text + text_len * 2009, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 2010, "If the name of a loaded kernel", text_len,
	     (ftnlen)30);
    s_copy(text + text_len * 2011, "matches this pattern, the comments for t"
	    "hat kernel will be", text_len, (ftnlen)58);
    s_copy(text + text_len * 2012, "displayed.", text_len, (ftnlen)10);
    s_copy(text + text_len * 2013, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2014, "If none of the kernel names match this p"
	    "attern, a message informing", text_len, (ftnlen)67);
    s_copy(text + text_len * 2015, "you of this condition will be displayed."
	    "  If a kernel matches the", text_len, (ftnlen)65);
    s_copy(text + text_len * 2016, "pattern, but no comments are in the kern"
	    "el a message will be displayed", text_len, (ftnlen)70);
    s_copy(text + text_len * 2017, "indicating that no comments are availabl"
	    "e.", text_len, (ftnlen)42);
    s_copy(text + text_len * 2018, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2019, "@@SHOW COMMENTS ...", text_len, (ftnlen)
	    19);
    s_copy(text + text_len * 2020, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 2021, "Help", text_len, (ftnlen)4);
    finish[54] = 2022;
    begin[55] = 2023;
    s_copy(text + text_len * 2022, "There are a number of more or less globa"
	    "l features of", text_len, (ftnlen)53);
    s_copy(text + text_len * 2023, "an Inspekt session that affect how Inspe"
	    "kt carries out", text_len, (ftnlen)54);
    s_copy(text + text_len * 2024, "the commands you issue.  These items are"
	    " grouped together", text_len, (ftnlen)57);
    s_copy(text + text_len * 2025, "under the term Environment.  They includ"
	    "e:", text_len, (ftnlen)42);
    s_copy(text + text_len * 2026, "@newlist", text_len, (ftnlen)8);
    s_copy(text + text_len * 2027, "@numitem The editor used when you EDIT a"
	    " command.", text_len, (ftnlen)49);
    s_copy(text + text_len * 2028, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 2029, "@numitem Whether or not Help waits for y"
	    "ou to", text_len, (ftnlen)45);
    s_copy(text + text_len * 2030, "         finish reading a page before it"
	    " prints", text_len, (ftnlen)47);
    s_copy(text + text_len * 2031, "         the next screen out material", 
	    text_len, (ftnlen)37);
    return 0;
} /* zzhlp021_ */

