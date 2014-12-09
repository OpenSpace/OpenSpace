/* zzhlp002.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZHLP002 ( private help text ) */
/* Subroutine */ int zzhlp002_(integer *begin, integer *finish, char *text, 
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
    s_copy(text + text_len * 182, "To see a list of indexed columns type:", 
	    text_len, (ftnlen)38);
    s_copy(text + text_len * 183, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 184, "SHOW INDEXED", text_len, (ftnlen)12);
    s_copy(text + text_len * 185, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 186, "@@Columns", text_len, (ftnlen)9);
    s_copy(text + text_len * 187, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 188, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 189, "SET COLUMN ...", text_len, (ftnlen)14);
    s_copy(text + text_len * 190, "Reports", text_len, (ftnlen)7);
    s_copy(text + text_len * 191, "Time Formats", text_len, (ftnlen)12);
    s_copy(text + text_len * 192, "Autoadjust", text_len, (ftnlen)10);
    finish[5] = 193;
    begin[6] = 194;
    s_copy(text + text_len * 193, "Data in one row of a table can be connect"
	    "ed with data in", text_len, (ftnlen)56);
    s_copy(text + text_len * 194, "a row of a second (possibly the same) tab"
	    "le by a process", text_len, (ftnlen)56);
    s_copy(text + text_len * 195, "called \"joining\" the tables.", text_len, 
	    (ftnlen)28);
    s_copy(text + text_len * 196, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 197, "The \"join\" of two tables is the cartesi"
	    "an product of the", text_len, (ftnlen)56);
    s_copy(text + text_len * 198, "two tables.  For example suppose that the"
	    " tables MUSIC and PEOPLE have", text_len, (ftnlen)70);
    s_copy(text + text_len * 199, "been loaded into Inspekt.  Table MUSIC", 
	    text_len, (ftnlen)38);
    s_copy(text + text_len * 200, "has two columns SONG and COMPOSER, Table "
	    "PEOPLE has columns", text_len, (ftnlen)59);
    s_copy(text + text_len * 201, "NAME and COUNTRY.  Below is a sample of t"
	    "he data in these", text_len, (ftnlen)57);
    s_copy(text + text_len * 202, "two tables.", text_len, (ftnlen)11);
    s_copy(text + text_len * 203, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 204, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 205, "Table MUSIC", text_len, (ftnlen)11);
    s_copy(text + text_len * 206, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 207, "SONG              COMPOSER", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 208, "--------------------------", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 209, "YESTERDAY         MCCARTNEY", text_len, (
	    ftnlen)27);
    s_copy(text + text_len * 210, "LIVE AND LET DIE  MCCARTNEY", text_len, (
	    ftnlen)27);
    s_copy(text + text_len * 211, "BLOODY SUNDAY     HEUSSEN", text_len, (
	    ftnlen)25);
    s_copy(text + text_len * 212, "ONE TREE HILL     HEUSSEN", text_len, (
	    ftnlen)25);
    s_copy(text + text_len * 213, "SATISFACTION      JAGGER", text_len, (
	    ftnlen)24);
    s_copy(text + text_len * 214, "BOYS OF SUMMER    HENLEY", text_len, (
	    ftnlen)24);
    s_copy(text + text_len * 215, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 216, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 217, "Table PEOPLE", text_len, (ftnlen)12);
    s_copy(text + text_len * 218, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 219, "NAME             COUNTRY", text_len, (
	    ftnlen)24);
    s_copy(text + text_len * 220, "--------------------------", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 221, "MCCARTNEY        ENGLAND", text_len, (
	    ftnlen)24);
    s_copy(text + text_len * 222, "HEUSSEN          IRELAND", text_len, (
	    ftnlen)24);
    s_copy(text + text_len * 223, "JAGGER           ENGLAND", text_len, (
	    ftnlen)24);
    s_copy(text + text_len * 224, "HENLEY           USA", text_len, (ftnlen)
	    20);
    s_copy(text + text_len * 225, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 226, "The two tables have sufficient informatio"
	    "n to tell us", text_len, (ftnlen)53);
    s_copy(text + text_len * 227, "the names of all the songs written by Iri"
	    "sh composers.", text_len, (ftnlen)54);
    s_copy(text + text_len * 228, "But, how do we get Inspekt to tell us thi"
	    "s information.", text_len, (ftnlen)55);
    s_copy(text + text_len * 229, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 230, "We \"join\" the two tables so that everyt"
	    "hing is in one", text_len, (ftnlen)53);
    s_copy(text + text_len * 231, "large table.  Below is the join of the ta"
	    "bles MUSIC and", text_len, (ftnlen)55);
    s_copy(text + text_len * 232, "PEOPLE. (We've included blank lines to he"
	    "lp illustrate", text_len, (ftnlen)54);
    s_copy(text + text_len * 233, "how rows are combined to make the join ta"
	    "ble. Also for", text_len, (ftnlen)54);
    s_copy(text + text_len * 234, "purpose of illustration only, we've prese"
	    "nted the data", text_len, (ftnlen)54);
    s_copy(text + text_len * 235, "from the second table in lower case.  In "
	    "the actual join", text_len, (ftnlen)56);
    s_copy(text + text_len * 236, "the case of data does not change.)", 
	    text_len, (ftnlen)34);
    s_copy(text + text_len * 237, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 238, "Join of tables MUSIC, PEOPLE", text_len, (
	    ftnlen)28);
    s_copy(text + text_len * 239, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 240, "SONG              COMPOSER   NAME       C"
	    "OUNTRY", text_len, (ftnlen)47);
    s_copy(text + text_len * 241, "-----------------------------------------"
	    "------", text_len, (ftnlen)47);
    s_copy(text + text_len * 242, "YESTERDAY         MCCARTNEY  mccartney  e"
	    "ngland", text_len, (ftnlen)47);
    s_copy(text + text_len * 243, "YESTERDAY         MCCARTNEY  heussen    i"
	    "reland", text_len, (ftnlen)47);
    s_copy(text + text_len * 244, "YESTERDAY         MCCARTNEY  jagger     e"
	    "ngland", text_len, (ftnlen)47);
    s_copy(text + text_len * 245, "YESTERDAY         MCCARTNEY  henley     u"
	    "sa", text_len, (ftnlen)43);
    s_copy(text + text_len * 246, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 247, "LIVE AND LET DIE  MCCARTNEY  mccartney  e"
	    "ngland", text_len, (ftnlen)47);
    s_copy(text + text_len * 248, "LIVE AND LET DIE  MCCARTNEY  heussen    i"
	    "reland", text_len, (ftnlen)47);
    s_copy(text + text_len * 249, "LIVE AND LET DIE  MCCARTNEY  jagger     e"
	    "ngland", text_len, (ftnlen)47);
    s_copy(text + text_len * 250, "LIVE AND LET DIE  MCCARTNEY  henley     u"
	    "sa", text_len, (ftnlen)43);
    s_copy(text + text_len * 251, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 252, "BLOODY SUNDAY     HEUSSEN    mccartney  e"
	    "ngland", text_len, (ftnlen)47);
    s_copy(text + text_len * 253, "BLOODY SUNDAY     HEUSSEN    heussen    i"
	    "reland", text_len, (ftnlen)47);
    s_copy(text + text_len * 254, "BLOODY SUNDAY     HEUSSEN    jagger     e"
	    "ngland", text_len, (ftnlen)47);
    s_copy(text + text_len * 255, "BLOODY SUNDAY     HEUSSEN    henley     u"
	    "sa", text_len, (ftnlen)43);
    s_copy(text + (text_len << 8), " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 257, "ONE TREE HILL     HEUSSEN    mccartney  e"
	    "ngland", text_len, (ftnlen)47);
    s_copy(text + text_len * 258, "ONE TREE HILL     HEUSSEN    heussen    i"
	    "reland", text_len, (ftnlen)47);
    s_copy(text + text_len * 259, "ONE TREE HILL     HEUSSEN    jagger     e"
	    "ngland", text_len, (ftnlen)47);
    s_copy(text + text_len * 260, "ONE TREE HILL     HEUSSEN    HENLEY     U"
	    "SA", text_len, (ftnlen)43);
    s_copy(text + text_len * 261, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 262, "SATISFACTION      JAGGER     mccartney  e"
	    "ngland", text_len, (ftnlen)47);
    s_copy(text + text_len * 263, "SATISFACTION      JAGGER     heussen    i"
	    "reland", text_len, (ftnlen)47);
    s_copy(text + text_len * 264, "SATISFACTION      JAGGER     jagger     e"
	    "ngland", text_len, (ftnlen)47);
    s_copy(text + text_len * 265, "SATISFACTION      JAGGER     henley     u"
	    "sa", text_len, (ftnlen)43);
    s_copy(text + text_len * 266, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 267, "BOYS OF SUMMER    HENLEY     mccartney  e"
	    "ngland", text_len, (ftnlen)47);
    s_copy(text + text_len * 268, "BOYS OF SUMMER    HENLEY     heussen    i"
	    "reland", text_len, (ftnlen)47);
    s_copy(text + text_len * 269, "BOYS OF SUMMER    HENLEY     jagger     e"
	    "ngland", text_len, (ftnlen)47);
    s_copy(text + text_len * 270, "BOYS OF SUMMER    HENLEY     henley     u"
	    "sa", text_len, (ftnlen)43);
    s_copy(text + text_len * 271, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 272, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 273, "To select something from this join of two"
	    " tables you", text_len, (ftnlen)52);
    s_copy(text + text_len * 274, "construct your SELECT command as shown he"
	    "re:", text_len, (ftnlen)44);
    s_copy(text + text_len * 275, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 276, "select item, ... , item", text_len, (
	    ftnlen)23);
    s_copy(text + text_len * 277, "FROM   MUSIC, PEOPLE", text_len, (ftnlen)
	    20);
    s_copy(text + text_len * 278, "where ...", text_len, (ftnlen)9);
    return 0;
} /* zzhlp002_ */

