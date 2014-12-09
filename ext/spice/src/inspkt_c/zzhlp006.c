/* zzhlp006.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZHLP006 ( private help text ) */
/* Subroutine */ int zzhlp006_(integer *begin, integer *finish, char *text, 
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
    s_copy(text + text_len * 576, "EQ             column EQ   expression", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 577, "GE             column GE   expression", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 578, "GT             column GT   expression", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 579, "NE             column NE   expression", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 580, "<              column <    expression", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 581, "<=             column <=   expression", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 582, "=              column =    expression", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 583, ">=             column >=   expression", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 584, ">              column >    expression", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 585, "!=             column <>   expression", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 586, "<>             column !=   expression", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 587, "LIKE           column LIKE expression", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 588, "NOT LIKE       column NOT LIKE expression",
	     text_len, (ftnlen)41);
    s_copy(text + text_len * 589, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 590, "BETWEEN        column BETWEEN expression", 
	    text_len, (ftnlen)40);
    s_copy(text + text_len * 591, "                      AND     expression", 
	    text_len, (ftnlen)40);
    s_copy(text + text_len * 592, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 593, "NOT BETWEEN    column NOT BETWEEN express"
	    "ion", text_len, (ftnlen)44);
    s_copy(text + text_len * 594, "                      AND         express"
	    "ion", text_len, (ftnlen)44);
    s_copy(text + text_len * 595, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 596, "where \"column\" is the name of some colu"
	    "mn and \"expression\"", text_len, (ftnlen)58);
    s_copy(text + text_len * 597, "is the name of a column or a literal valu"
	    "e such as 1 or", text_len, (ftnlen)55);
    s_copy(text + text_len * 598, "\"A Literal String\".", text_len, (ftnlen)
	    19);
    s_copy(text + text_len * 599, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 600, "Conditions listed above are true if:", 
	    text_len, (ftnlen)36);
    s_copy(text + text_len * 601, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 602, "@setparamsize{NOT BETWEEN}", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 603, "@param LT or <.", text_len, (ftnlen)15);
    s_copy(text + text_len * 604, "the value column is less than the value o"
	    "f expression.", text_len, (ftnlen)54);
    s_copy(text + text_len * 605, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 606, "@param LE or <=.", text_len, (ftnlen)16);
    s_copy(text + text_len * 607, "the value of column is less than or equal"
	    " to the value", text_len, (ftnlen)54);
    s_copy(text + text_len * 608, "of expression", text_len, (ftnlen)13);
    s_copy(text + text_len * 609, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 610, "@param EQ or =.", text_len, (ftnlen)15);
    s_copy(text + text_len * 611, "the value of column is equal to the value"
	    " of expression.", text_len, (ftnlen)56);
    s_copy(text + text_len * 612, "Note that for strings, the case of charac"
	    "ters is significant.", text_len, (ftnlen)61);
    s_copy(text + text_len * 613, "The strings 'A' and 'a' are not equal.", 
	    text_len, (ftnlen)38);
    s_copy(text + text_len * 614, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 615, "@param GE or >=.", text_len, (ftnlen)16);
    s_copy(text + text_len * 616, "the value of column is greater than or eq"
	    "ual to the value", text_len, (ftnlen)57);
    s_copy(text + text_len * 617, "of expression.", text_len, (ftnlen)14);
    s_copy(text + text_len * 618, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 619, "@param GT or >.", text_len, (ftnlen)15);
    s_copy(text + text_len * 620, "the value of column is greater than the v"
	    "alue of expression.", text_len, (ftnlen)60);
    s_copy(text + text_len * 621, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 622, "@param NE or != or <>.", text_len, (ftnlen)
	    22);
    s_copy(text + text_len * 623, "the value of column is not equal to the v"
	    "alue of expression.", text_len, (ftnlen)60);
    s_copy(text + text_len * 624, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 625, "@param LIKE.", text_len, (ftnlen)12);
    s_copy(text + text_len * 626, "the value of column matches the value of "
	    "expression when", text_len, (ftnlen)56);
    s_copy(text + text_len * 627, "expression is interpreted as a pattern.", 
	    text_len, (ftnlen)39);
    s_copy(text + text_len * 628, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 629, "@param NOT LIKE.", text_len, (ftnlen)16);
    s_copy(text + text_len * 630, "The value of column does not match the va"
	    "lue of expression", text_len, (ftnlen)58);
    s_copy(text + text_len * 631, "when expression is interpreted as a patte"
	    "rn.", text_len, (ftnlen)44);
    s_copy(text + text_len * 632, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 633, "@param BETWEEN.", text_len, (ftnlen)15);
    s_copy(text + text_len * 634, "The value of column is greater than or eq"
	    "ual to the smaller of", text_len, (ftnlen)62);
    s_copy(text + text_len * 635, "the two expressions AND less than or equa"
	    "l to the larger of the", text_len, (ftnlen)63);
    s_copy(text + text_len * 636, "two expressions.", text_len, (ftnlen)16);
    s_copy(text + text_len * 637, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 638, "@param NOT BETWEEN", text_len, (ftnlen)18);
    s_copy(text + text_len * 639, "The value of column is less than the smal"
	    "ler of the two expression", text_len, (ftnlen)66);
    s_copy(text + text_len * 640, "OR greater than the larger of the two exp"
	    "ression.", text_len, (ftnlen)49);
    s_copy(text + text_len * 641, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 642, "A WHERE clause is composed of the word \""
	    "WHERE\" followed by", text_len, (ftnlen)58);
    s_copy(text + text_len * 643, "a logical expression made up of condition"
	    "s connected by", text_len, (ftnlen)55);
    s_copy(text + text_len * 644, "AND's, OR's and NOT's and grouped using p"
	    "arentheses.", text_len, (ftnlen)52);
    s_copy(text + text_len * 645, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 646, "@@Conditional Operators", text_len, (
	    ftnlen)23);
    s_copy(text + text_len * 647, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 648, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 649, "Specifying Strings", text_len, (ftnlen)18);
    s_copy(text + text_len * 650, "Specifying Times", text_len, (ftnlen)16);
    s_copy(text + text_len * 651, "Where Clause", text_len, (ftnlen)12);
    finish[7] = 652;
    begin[8] = 653;
    s_copy(text + text_len * 652, "Inspekt can display any of it's current s"
	    "etting.  However, these", text_len, (ftnlen)64);
    s_copy(text + text_len * 653, "settings are grouped together in various "
	    "groupings.  To see one", text_len, (ftnlen)63);
    s_copy(text + text_len * 654, "of these grouping you type", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 655, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 656, "SHOW item", text_len, (ftnlen)9);
    s_copy(text + text_len * 657, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 658, "See the help topics below for more specif"
	    "ic descriptions.", text_len, (ftnlen)57);
    s_copy(text + text_len * 659, "@@Current Settings   --- SHOW", text_len, (
	    ftnlen)29);
    s_copy(text + text_len * 660, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 661, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 662, "SHOW COLUMN      ...", text_len, (ftnlen)
	    20);
    s_copy(text + text_len * 663, "SHOW COMMENTS    ...", text_len, (ftnlen)
	    20);
    s_copy(text + text_len * 664, "SHOW ENVIRONMENT ...", text_len, (ftnlen)
	    20);
    s_copy(text + text_len * 665, "SHOW FORMAT      ...", text_len, (ftnlen)
	    20);
    s_copy(text + text_len * 666, "SHOW INDEXED     ...", text_len, (ftnlen)
	    20);
    s_copy(text + text_len * 667, "SHOW KERNELS     ...", text_len, (ftnlen)
	    20);
    s_copy(text + text_len * 668, "SHOW PAGE        ...", text_len, (ftnlen)
	    20);
    s_copy(text + text_len * 669, "SHOW SUMMARY     ...", text_len, (ftnlen)
	    20);
    s_copy(text + text_len * 670, "Columns", text_len, (ftnlen)7);
    s_copy(text + text_len * 671, "Deluge Warning", text_len, (ftnlen)14);
    s_copy(text + text_len * 672, "Headers", text_len, (ftnlen)7);
    return 0;
} /* zzhlp006_ */

