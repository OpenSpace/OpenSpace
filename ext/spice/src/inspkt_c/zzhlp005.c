/* zzhlp005.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZHLP005 ( private help text ) */
/* Subroutine */ int zzhlp005_(integer *begin, integer *finish, char *text, 
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
    s_copy(text + text_len * 479, "To select columns you simply use the alia"
	    "sed names as", text_len, (ftnlen)53);
    s_copy(text + text_len * 480, "presented in the table above.  Thus to fi"
	    "nd all of the grandfathers", text_len, (ftnlen)67);
    s_copy(text + text_len * 481, "in our original table RELATIONS we would "
	    "issue the following", text_len, (ftnlen)60);
    s_copy(text + text_len * 482, "command", text_len, (ftnlen)7);
    s_copy(text + text_len * 483, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 484, "SELECT  Y.FATHER", text_len, (ftnlen)16);
    s_copy(text + text_len * 485, "FROM   RELATIONS X, RELATIONS Y", text_len,
	     (ftnlen)31);
    s_copy(text + text_len * 486, "WHERE   X.FATHER = Y.CHILD", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 487, "OR      X.MOTHER = Y.CHILD", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 488, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 489, "Here is how the conditions narrow down th"
	    "e join.", text_len, (ftnlen)48);
    s_copy(text + text_len * 490, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 491, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 492, "Join of RELATIONS X, RELATIONS Y", 
	    text_len, (ftnlen)32);
    s_copy(text + text_len * 493, "WHERE   X.FATHER = Y.CHILD", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 494, "OR      X.MOTHER = Y.CHILD", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 495, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 496, "X.CHILD  X.FATHER X.MOTHER   Y.CHILD Y.FA"
	    "THER  Y.MOTHER", text_len, (ftnlen)55);
    s_copy(text + text_len * 497, "-----------------------------------------"
	    "----------------", text_len, (ftnlen)57);
    s_copy(text + text_len * 498, "CINDY    GEORGE   ALICE      GEORGE  WILL"
	    "IAM   MARGARET", text_len, (ftnlen)55);
    s_copy(text + text_len * 499, "CINDY    GEORGE   ALICE      ALICE   JOHN"
	    "      KATHERINE", text_len, (ftnlen)56);
    s_copy(text + text_len * 500, "SALLY    GEORGE   MARTHA     GEORGE  WILL"
	    "IAM   MARGARET", text_len, (ftnlen)55);
    s_copy(text + text_len * 501, "SALLY    GEORGE   MARTHA     MARTHA  ROBE"
	    "RT    CONSTANCE", text_len, (ftnlen)56);
    s_copy(text + text_len * 502, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 503, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 504, "We get all the names in the Y.FATHER colu"
	    "mn", text_len, (ftnlen)43);
    s_copy(text + text_len * 505, "where the CHILD appearing in the Y copy o"
	    "f RELATIONS appears", text_len, (ftnlen)60);
    s_copy(text + text_len * 506, "as a parent in the X copy of RELATIONS.", 
	    text_len, (ftnlen)39);
    s_copy(text + text_len * 507, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 508, "To find every child that is a grandchild,",
	     text_len, (ftnlen)41);
    s_copy(text + text_len * 509, "we can simply pick out the children whose"
	    " mothers", text_len, (ftnlen)49);
    s_copy(text + text_len * 510, "are listed as children in the second copy"
	    " of RELATIONS.", text_len, (ftnlen)55);
    s_copy(text + text_len * 511, " ", text_len, (ftnlen)1);
    s_copy(text + (text_len << 9), "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 513, "SELECT X.CHILD", text_len, (ftnlen)14);
    s_copy(text + text_len * 514, "FROM RELATIONS X, RELATION Y", text_len, (
	    ftnlen)28);
    s_copy(text + text_len * 515, "WHERE X.MOTHER = Y.CHILD", text_len, (
	    ftnlen)24);
    s_copy(text + text_len * 516, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 517, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 518, "@subsection Using Aliases", text_len, (
	    ftnlen)25);
    s_copy(text + text_len * 519, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 520, "Even if you do not join a table to itself"
	    ", you may wish to", text_len, (ftnlen)58);
    s_copy(text + text_len * 521, "use an alias for a table name.  For insta"
	    "nce, in the second", text_len, (ftnlen)59);
    s_copy(text + text_len * 522, "example where the tables MUSIC and PEOPLE"
	    " had a column name", text_len, (ftnlen)59);
    s_copy(text + text_len * 523, "in column, you can use an alias as shown "
	    "below in order to", text_len, (ftnlen)58);
    s_copy(text + text_len * 524, "simplify the typing.", text_len, (ftnlen)
	    20);
    s_copy(text + text_len * 525, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 526, "Original Select Command Without Aliases", 
	    text_len, (ftnlen)39);
    s_copy(text + text_len * 527, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 528, "select song", text_len, (ftnlen)11);
    s_copy(text + text_len * 529, "from music, people", text_len, (ftnlen)18);
    s_copy(text + text_len * 530, "where MUSIC.COMPOSER = PEOPLE.COMPOSER", 
	    text_len, (ftnlen)38);
    s_copy(text + text_len * 531, "and   country  = 'IRELAND'", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 532, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 533, "Same Select Command but with Aliases for "
	    "the Tables", text_len, (ftnlen)51);
    s_copy(text + text_len * 534, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 535, "select song", text_len, (ftnlen)11);
    s_copy(text + text_len * 536, "from music M, people P", text_len, (ftnlen)
	    22);
    s_copy(text + text_len * 537, "where M.COMPOSER = P.COMPOSER", text_len, (
	    ftnlen)29);
    s_copy(text + text_len * 538, "and   country  = 'IRELAND'", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 539, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 540, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 541, "@subsection Combining More than Two Tables"
	    , text_len, (ftnlen)42);
    s_copy(text + text_len * 542, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 543, "You may join more than two tables if the "
	    "need arises.  The", text_len, (ftnlen)58);
    s_copy(text + text_len * 544, "result is the cartesian product of the co"
	    "ntents of the contributing", text_len, (ftnlen)67);
    s_copy(text + text_len * 545, "tables.", text_len, (ftnlen)7);
    s_copy(text + text_len * 546, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 547, "The syntax for specifying such a join is:",
	     text_len, (ftnlen)41);
    s_copy(text + text_len * 548, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 549, "FROM  TABLE_1 [alias_1],", text_len, (
	    ftnlen)24);
    s_copy(text + text_len * 550, "      TABLE_2 [alias_2],", text_len, (
	    ftnlen)24);
    s_copy(text + text_len * 551, "      ...,", text_len, (ftnlen)10);
    s_copy(text + text_len * 552, "      TABLE_N [alias_N]", text_len, (
	    ftnlen)23);
    s_copy(text + text_len * 553, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 554, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 555, "Inspekt and the E-kernel system require t"
	    "hat N be no more than 10.", text_len, (ftnlen)66);
    s_copy(text + text_len * 556, "As you might imagine, the join of many ta"
	    "bles has the potential", text_len, (ftnlen)63);
    s_copy(text + text_len * 557, "for creating an enormous table from which"
	    " data will be selected.", text_len, (ftnlen)64);
    s_copy(text + text_len * 558, "As a result Inspekt's speed may degrade s"
	    "ignificantly if you", text_len, (ftnlen)60);
    s_copy(text + text_len * 559, "join many tables.", text_len, (ftnlen)17);
    s_copy(text + text_len * 560, "@@Combining Tables", text_len, (ftnlen)18);
    s_copy(text + text_len * 561, "Quit Help", text_len, (ftnlen)9);
    s_copy(text + text_len * 562, "Help", text_len, (ftnlen)4);
    s_copy(text + text_len * 563, "From Clause", text_len, (ftnlen)11);
    s_copy(text + text_len * 564, "Looking at Data    --- SELECT", text_len, (
	    ftnlen)29);
    finish[6] = 565;
    begin[7] = 566;
    s_copy(text + text_len * 565, "Normally, when you select data from an ev"
	    "ents kernels, you place", text_len, (ftnlen)64);
    s_copy(text + text_len * 566, "conditions upon the various columns that "
	    "will be displayed.  This", text_len, (ftnlen)65);
    s_copy(text + text_len * 567, "is done via conditional operators (also c"
	    "alled relational operators).", text_len, (ftnlen)69);
    s_copy(text + text_len * 568, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 569, "The following conditionals are supported "
	    "by the E-kernel system.", text_len, (ftnlen)64);
    s_copy(text + text_len * 570, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 571, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 572, "Operator       Usage in a condition", 
	    text_len, (ftnlen)35);
    s_copy(text + text_len * 573, "--------       ---------------------------"
	    , text_len, (ftnlen)42);
    s_copy(text + text_len * 574, "LT             column LT   expression", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 575, "LE             column LE   expression", 
	    text_len, (ftnlen)37);
    return 0;
} /* zzhlp005_ */

