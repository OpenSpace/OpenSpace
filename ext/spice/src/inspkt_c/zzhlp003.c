/* zzhlp003.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZHLP003 ( private help text ) */
/* Subroutine */ int zzhlp003_(integer *begin, integer *finish, char *text, 
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
    s_copy(text + text_len * 279, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 280, "The tables we want to join are specified "
	    "in the FROM clause", text_len, (ftnlen)59);
    s_copy(text + text_len * 281, "of the select command.  They must be sepa"
	    "rated by commas.", text_len, (ftnlen)57);
    s_copy(text + text_len * 282, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 283, "Notice that every row of the first table,"
	    " MUSIC, has every", text_len, (ftnlen)58);
    s_copy(text + text_len * 284, "row of the second table, PEOPLE appended "
	    "to it to produce", text_len, (ftnlen)57);
    s_copy(text + text_len * 285, "a new row.   This table becomes useful wh"
	    "en we apply constraints", text_len, (ftnlen)64);
    s_copy(text + text_len * 286, "to it.  In our case we want to find out w"
	    "hich songs were written", text_len, (ftnlen)64);
    s_copy(text + text_len * 287, "by Irish song writers.", text_len, (ftnlen)
	    22);
    s_copy(text + text_len * 288, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 289, "The country of origin of the composer", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 290, "is present in a row of the join table", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 291, "if the NAME and COMPOSER columns have the",
	     text_len, (ftnlen)41);
    s_copy(text + text_len * 292, "same value.  The SELECT command", text_len,
	     (ftnlen)31);
    s_copy(text + text_len * 293, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 294, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 295, "select song, composer, name, country", 
	    text_len, (ftnlen)36);
    s_copy(text + text_len * 296, "from music, people", text_len, (ftnlen)18);
    s_copy(text + text_len * 297, "WHERE COMPOSER = NAME", text_len, (ftnlen)
	    21);
    s_copy(text + text_len * 298, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 299, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 300, "gives us the following rows.", text_len, (
	    ftnlen)28);
    s_copy(text + text_len * 301, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 302, "SONG              COMPOSER   NAME       C"
	    "OUNTRY", text_len, (ftnlen)47);
    s_copy(text + text_len * 303, "-----------------------------------------"
	    "------", text_len, (ftnlen)47);
    s_copy(text + text_len * 304, "YESTERDAY         MCCARTNEY  MCCARTNEY  E"
	    "NGLAND", text_len, (ftnlen)47);
    s_copy(text + text_len * 305, "LIVE AND LET DIE  MCCARTNEY  MCCARTNEY  E"
	    "NGLAND", text_len, (ftnlen)47);
    s_copy(text + text_len * 306, "BLOODY SUNDAY     HEUSSEN    HEUSSEN    I"
	    "RELAND", text_len, (ftnlen)47);
    s_copy(text + text_len * 307, "ONE TREE HILL     HEUSSEN    HEUSSEN    I"
	    "RELAND", text_len, (ftnlen)47);
    s_copy(text + text_len * 308, "SATISFACTION      JAGGER     JAGGER     E"
	    "NGLAND", text_len, (ftnlen)47);
    s_copy(text + text_len * 309, "BOYS OF SUMMER    HENLEY     HENLEY     U"
	    "SA", text_len, (ftnlen)43);
    s_copy(text + text_len * 310, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 311, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 312, "This report is a lot closer to what we wa"
	    "nt.", text_len, (ftnlen)44);
    s_copy(text + text_len * 313, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 314, "To see just the titles of songs written b"
	    "y Irish", text_len, (ftnlen)48);
    s_copy(text + text_len * 315, "song writers we add modify the SELECT com"
	    "mand as shown", text_len, (ftnlen)54);
    s_copy(text + text_len * 316, "below.", text_len, (ftnlen)6);
    s_copy(text + text_len * 317, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 318, "select SONG", text_len, (ftnlen)11);
    s_copy(text + text_len * 319, "from music, people", text_len, (ftnlen)18);
    s_copy(text + text_len * 320, "where composer = name", text_len, (ftnlen)
	    21);
    s_copy(text + text_len * 321, "AND   COUNTRY  = 'IRELAND'", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 322, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 323, "This results in the following report.", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 324, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 325, "SONG", text_len, (ftnlen)4);
    s_copy(text + text_len * 326, "--------------", text_len, (ftnlen)14);
    s_copy(text + text_len * 327, "BLOODY SUNDAY", text_len, (ftnlen)13);
    s_copy(text + text_len * 328, "ONE TREE HILL", text_len, (ftnlen)13);
    s_copy(text + text_len * 329, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 330, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 331, "@subsection Equi-joins", text_len, (ftnlen)
	    22);
    s_copy(text + text_len * 332, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 333, "In the previous example we had the condit"
	    "ion", text_len, (ftnlen)44);
    s_copy(text + text_len * 334, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 335, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 336, "WHERE COMPOSER = NAME", text_len, (ftnlen)
	    21);
    s_copy(text + text_len * 337, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 338, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 339, "This is a common condition when selecting"
	    " data from a join", text_len, (ftnlen)58);
    s_copy(text + text_len * 340, "of two tables.  The general situation loo"
	    "ks something like", text_len, (ftnlen)58);
    s_copy(text + text_len * 341, "this.", text_len, (ftnlen)5);
    s_copy(text + text_len * 342, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 343, "SELECT ...", text_len, (ftnlen)10);
    s_copy(text + text_len * 344, "FROM   table_1, table_2", text_len, (
	    ftnlen)23);
    s_copy(text + text_len * 345, "WHERE  column_from_table_1 = column_from_"
	    "table_2", text_len, (ftnlen)48);
    s_copy(text + text_len * 346, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 347, "When you join two or more tables and add "
	    "an equality a condition", text_len, (ftnlen)64);
    s_copy(text + text_len * 348, "to the rows from different tables, the jo"
	    "in is", text_len, (ftnlen)46);
    s_copy(text + text_len * 349, "called an \"equi-join\".  You will", 
	    text_len, (ftnlen)32);
    s_copy(text + text_len * 350, "almost always create equi-joins when join"
	    "ing tables.", text_len, (ftnlen)52);
    s_copy(text + text_len * 351, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 352, "@subsection Ambiguous Columns", text_len, (
	    ftnlen)29);
    s_copy(text + text_len * 353, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 354, "Suppose in the following example the tabl"
	    "e PEOPLE had the columns", text_len, (ftnlen)65);
    s_copy(text + text_len * 355, "COMPOSER and COUNTRY instead of NAME and "
	    "COUNTRY. The simple view", text_len, (ftnlen)65);
    s_copy(text + text_len * 356, "of the join now becomes:", text_len, (
	    ftnlen)24);
    s_copy(text + text_len * 357, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 358, "SONG              COMPOSER   COMPOSER   C"
	    "OUNTRY", text_len, (ftnlen)47);
    s_copy(text + text_len * 359, "-----------------------------------------"
	    "------", text_len, (ftnlen)47);
    s_copy(text + text_len * 360, "YESTERDAY         MCCARTNEY  MCCARTNEY  E"
	    "NGLAND", text_len, (ftnlen)47);
    s_copy(text + text_len * 361, "YESTERDAY         MCCARTNEY  HEUSSEN    I"
	    "RELAND", text_len, (ftnlen)47);
    s_copy(text + text_len * 362, "YESTERDAY         MCCARTNEY  JAGGER     E"
	    "NGLAND", text_len, (ftnlen)47);
    s_copy(text + text_len * 363, "YESTERDAY         MCCARTNEY  HENLEY     U"
	    "SA", text_len, (ftnlen)43);
    s_copy(text + text_len * 364, "LIVE AND LET DIE  MCCARTNEY  MCCARTNEY  E"
	    "NGLAND", text_len, (ftnlen)47);
    s_copy(text + text_len * 365, "        .            .           .       "
	    " .", text_len, (ftnlen)43);
    s_copy(text + text_len * 366, "        .            .           .       "
	    " .", text_len, (ftnlen)43);
    s_copy(text + text_len * 367, "        .            .           .       "
	    " .", text_len, (ftnlen)43);
    s_copy(text + text_len * 368, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 369, "If I want to talk about the column first "
	    "column COMPOSER", text_len, (ftnlen)56);
    s_copy(text + text_len * 370, "how do I distinguish it from the second c"
	    "olumn COMPOSER?", text_len, (ftnlen)56);
    s_copy(text + text_len * 371, "The first column comes from the table MUS"
	    "IC  the second", text_len, (ftnlen)55);
    s_copy(text + text_len * 372, "comes from the table PEOPLE.  To unambigu"
	    "ously specify", text_len, (ftnlen)54);
    s_copy(text + text_len * 373, "either prefix the column name by the name"
	    " of its parent", text_len, (ftnlen)55);
    s_copy(text + text_len * 374, "table as in  MUSIC.COMPOSER  or PEOPLE.CO"
	    "MPOSER.  The", text_len, (ftnlen)53);
    s_copy(text + text_len * 375, "select command would then be issued as:", 
	    text_len, (ftnlen)39);
    s_copy(text + text_len * 376, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 377, "select song", text_len, (ftnlen)11);
    s_copy(text + text_len * 378, "from music, people", text_len, (ftnlen)18);
    return 0;
} /* zzhlp003_ */

