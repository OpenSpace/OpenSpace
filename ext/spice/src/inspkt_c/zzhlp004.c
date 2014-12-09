/* zzhlp004.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ZZHLP004 ( private help text ) */
/* Subroutine */ int zzhlp004_(integer *begin, integer *finish, char *text, 
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
    s_copy(text + text_len * 379, "where MUSIC.COMPOSER = PEOPLE.COMPOSER", 
	    text_len, (ftnlen)38);
    s_copy(text + text_len * 380, "and   country  = 'IRELAND'", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 381, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 382, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 383, "@subsection Joining a Table to Itself", 
	    text_len, (ftnlen)37);
    s_copy(text + text_len * 384, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 385, "Sometimes it is useful to be able to join"
	    " a table to itself.", text_len, (ftnlen)60);
    s_copy(text + text_len * 386, "This is particularly true if the table co"
	    "ntains some kind", text_len, (ftnlen)57);
    s_copy(text + text_len * 387, "of hierarchical information such as in ch"
	    "ild-parent pairs.", text_len, (ftnlen)58);
    s_copy(text + text_len * 388, "For example here is a list of family memb"
	    "ers and their", text_len, (ftnlen)54);
    s_copy(text + text_len * 389, "parents", text_len, (ftnlen)7);
    s_copy(text + text_len * 390, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 391, "Table RELATIONS", text_len, (ftnlen)15);
    s_copy(text + text_len * 392, "CHILD    FATHER   MOTHER", text_len, (
	    ftnlen)24);
    s_copy(text + text_len * 393, "------------------------", text_len, (
	    ftnlen)24);
    s_copy(text + text_len * 394, "CINDY    GEORGE   ALICE", text_len, (
	    ftnlen)23);
    s_copy(text + text_len * 395, "SALLY    GEORGE   MARTHA", text_len, (
	    ftnlen)24);
    s_copy(text + text_len * 396, "GEORGE   WILLIAM  MARGARET", text_len, (
	    ftnlen)26);
    s_copy(text + text_len * 397, "ALICE    JOHN     KATHERINE", text_len, (
	    ftnlen)27);
    s_copy(text + text_len * 398, "MARTHA   ROBERT   CONSTANCE", text_len, (
	    ftnlen)27);
    s_copy(text + text_len * 399, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 400, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 401, "The grandparents in this table are parent"
	    "s of parents.  We can", text_len, (ftnlen)62);
    s_copy(text + text_len * 402, "join the table to itself to get a list of"
	    " all the grandfathers.", text_len, (ftnlen)63);
    s_copy(text + text_len * 403, "The join is simple enough.  Here it is.", 
	    text_len, (ftnlen)39);
    s_copy(text + text_len * 404, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 405, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 406, "Join of RELATIONS, RELATIONS", text_len, (
	    ftnlen)28);
    s_copy(text + text_len * 407, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 408, "CHILD    FATHER   MOTHER     CHILD   FATH"
	    "ER    MOTHER", text_len, (ftnlen)53);
    s_copy(text + text_len * 409, "-----------------------------------------"
	    "----------------", text_len, (ftnlen)57);
    s_copy(text + text_len * 410, "CINDY    GEORGE   ALICE      CINDY   GEOR"
	    "GE    ALICE", text_len, (ftnlen)52);
    s_copy(text + text_len * 411, "CINDY    GEORGE   ALICE      SALLY   GEOR"
	    "GE    MARTHA", text_len, (ftnlen)53);
    s_copy(text + text_len * 412, "CINDY    GEORGE   ALICE      GEORGE  WILL"
	    "IAM   MARGARET", text_len, (ftnlen)55);
    s_copy(text + text_len * 413, "CINDY    GEORGE   ALICE      ALICE   JOHN"
	    "      KATHERINE", text_len, (ftnlen)56);
    s_copy(text + text_len * 414, "CINDY    GEORGE   ALICE      MARTHA  ROBE"
	    "RT    CONSTANCE", text_len, (ftnlen)56);
    s_copy(text + text_len * 415, "SALLY    GEORGE   MARTHA     CINDY   GEOR"
	    "GE    ALICE", text_len, (ftnlen)52);
    s_copy(text + text_len * 416, "SALLY    GEORGE   MARTHA     SALLY   GEOR"
	    "GE    MARTHA", text_len, (ftnlen)53);
    s_copy(text + text_len * 417, "SALLY    GEORGE   MARTHA     GEORGE  WILL"
	    "IAM   MARGARET", text_len, (ftnlen)55);
    s_copy(text + text_len * 418, "SALLY    GEORGE   MARTHA     ALICE   JOHN"
	    "      KATHERINE", text_len, (ftnlen)56);
    s_copy(text + text_len * 419, "SALLY    GEORGE   MARTHA     MARTHA  ROBE"
	    "RT    CONSTANCE", text_len, (ftnlen)56);
    s_copy(text + text_len * 420, "GEORGE   WILLIAM  MARGARET   CINDY   GEOR"
	    "GE    ALICE", text_len, (ftnlen)52);
    s_copy(text + text_len * 421, "GEORGE   WILLIAM  MARGARET   SALLY   GEOR"
	    "GE    MARTHA", text_len, (ftnlen)53);
    s_copy(text + text_len * 422, "GEORGE   WILLIAM  MARGARET   GEORGE  WILL"
	    "IAM   MARGARET", text_len, (ftnlen)55);
    s_copy(text + text_len * 423, "GEORGE   WILLIAM  MARGARET   ALICE   JOHN"
	    "      KATHERINE", text_len, (ftnlen)56);
    s_copy(text + text_len * 424, "GEORGE   WILLIAM  MARGARET   MARTHA  ROBE"
	    "RT    CONSTANCE", text_len, (ftnlen)56);
    s_copy(text + text_len * 425, "ALICE    JOHN     KATHERINE  CINDY   GEOR"
	    "GE    ALICE", text_len, (ftnlen)52);
    s_copy(text + text_len * 426, "ALICE    JOHN     KATHERINE  SALLY   GEOR"
	    "GE    MARTHA", text_len, (ftnlen)53);
    s_copy(text + text_len * 427, "ALICE    JOHN     KATHERINE  GEORGE  WILL"
	    "IAM   MARGARET", text_len, (ftnlen)55);
    s_copy(text + text_len * 428, "ALICE    JOHN     KATHERINE  ALICE   JOHN"
	    "      KATHERINE", text_len, (ftnlen)56);
    s_copy(text + text_len * 429, "ALICE    JOHN     KATHERINE  MARTHA  ROBE"
	    "RT    CONSTANCE", text_len, (ftnlen)56);
    s_copy(text + text_len * 430, "MARTHA   ROBERT   CONSTANCE  CINDY   GEOR"
	    "GE    ALICE", text_len, (ftnlen)52);
    s_copy(text + text_len * 431, "MARTHA   ROBERT   CONSTANCE  SALLY   GEOR"
	    "GE    MARTHA", text_len, (ftnlen)53);
    s_copy(text + text_len * 432, "MARTHA   ROBERT   CONSTANCE  GEORGE  WILL"
	    "IAM   MARGARET", text_len, (ftnlen)55);
    s_copy(text + text_len * 433, "MARTHA   ROBERT   CONSTANCE  ALICE   JOHN"
	    "      KATHERINE", text_len, (ftnlen)56);
    s_copy(text + text_len * 434, "MARTHA   ROBERT   CONSTANCE  MARTHA  ROBE"
	    "RT    CONSTANCE", text_len, (ftnlen)56);
    s_copy(text + text_len * 435, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 436, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 437, "But this table has a clear problem.  If I"
	    " want to select a", text_len, (ftnlen)58);
    s_copy(text + text_len * 438, "CHILD  from this table how do I specify w"
	    "hich", text_len, (ftnlen)45);
    s_copy(text + text_len * 439, "CHILD column I'm talking about? It clearl"
	    "y is not enough", text_len, (ftnlen)56);
    s_copy(text + text_len * 440, "to simply type RELATIONS.CHILD  because b"
	    "oth CHILD columns", text_len, (ftnlen)58);
    s_copy(text + text_len * 441, "come from the same table.", text_len, (
	    ftnlen)25);
    s_copy(text + text_len * 442, "We handle the problem of ambiguous column"
	    "s names by creating", text_len, (ftnlen)60);
    s_copy(text + text_len * 443, "a unique alias for the table when you spe"
	    "cify the join.  Thus", text_len, (ftnlen)61);
    s_copy(text + text_len * 444, "if I want to be able to select a column f"
	    "rom this table I specify", text_len, (ftnlen)65);
    s_copy(text + text_len * 445, "the join in the from clause as shown belo"
	    "w.", text_len, (ftnlen)43);
    s_copy(text + text_len * 446, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 447, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 448, "select ...", text_len, (ftnlen)10);
    s_copy(text + text_len * 449, "FROM   RELATIONS X, RELATIONS Y", text_len,
	     (ftnlen)31);
    s_copy(text + text_len * 450, "where  ...", text_len, (ftnlen)10);
    s_copy(text + text_len * 451, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 452, "This join creates the following table.", 
	    text_len, (ftnlen)38);
    s_copy(text + text_len * 453, "@literal", text_len, (ftnlen)8);
    s_copy(text + text_len * 454, "Join of RELATIONS X, RELATIONS Y", 
	    text_len, (ftnlen)32);
    s_copy(text + text_len * 455, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 456, "X.CHILD  X.FATHER X.MOTHER   Y.CHILD Y.FA"
	    "THER  Y.MOTHER", text_len, (ftnlen)55);
    s_copy(text + text_len * 457, "-----------------------------------------"
	    "----------------", text_len, (ftnlen)57);
    s_copy(text + text_len * 458, "CINDY    GEORGE   ALICE      CINDY   GEOR"
	    "GE    ALICE", text_len, (ftnlen)52);
    s_copy(text + text_len * 459, "CINDY    GEORGE   ALICE      SALLY   GEOR"
	    "GE    MARTHA", text_len, (ftnlen)53);
    s_copy(text + text_len * 460, "CINDY    GEORGE   ALICE      GEORGE  WILL"
	    "IAM   MARGARET", text_len, (ftnlen)55);
    s_copy(text + text_len * 461, "CINDY    GEORGE   ALICE      ALICE   JOHN"
	    "      KATHERINE", text_len, (ftnlen)56);
    s_copy(text + text_len * 462, "CINDY    GEORGE   ALICE      MARTHA  ROBE"
	    "RT    CONSTANCE", text_len, (ftnlen)56);
    s_copy(text + text_len * 463, "SALLY    GEORGE   MARTHA     CINDY   GEOR"
	    "GE    ALICE", text_len, (ftnlen)52);
    s_copy(text + text_len * 464, "  .         .       .          .        ."
	    "        .", text_len, (ftnlen)50);
    s_copy(text + text_len * 465, "  .         .       .          .        ."
	    "        .", text_len, (ftnlen)50);
    s_copy(text + text_len * 466, "  .         .       .          .        ."
	    "        .", text_len, (ftnlen)50);
    s_copy(text + text_len * 467, "|endliteral", text_len, (ftnlen)11);
    s_copy(text + text_len * 468, " ", text_len, (ftnlen)1);
    s_copy(text + text_len * 469, "As you can see in the example FROM clause"
	    " above, an alias is", text_len, (ftnlen)60);
    s_copy(text + text_len * 470, "specified by following the name of a tabl"
	    "e by a second word", text_len, (ftnlen)59);
    s_copy(text + text_len * 471, "and a comma.  The second word is the alia"
	    "s for the table.", text_len, (ftnlen)57);
    s_copy(text + text_len * 472, "Thus the alias for the first copy of RELA"
	    "TIONS is X,  the", text_len, (ftnlen)57);
    s_copy(text + text_len * 473, "alias for the second copy of RELATIONS is"
	    " Y.  There is nothing", text_len, (ftnlen)62);
    s_copy(text + text_len * 474, "special about the aliases X and Y.  We co"
	    "uld just as easily", text_len, (ftnlen)59);
    s_copy(text + text_len * 475, "have used EGG and SPAM respectively.  The"
	    " letters X and Y are", text_len, (ftnlen)61);
    s_copy(text + text_len * 476, "just easier to type. Aliases are case ins"
	    "ensitive.  The names", text_len, (ftnlen)61);
    s_copy(text + text_len * 477, " X.FATHER and x.FATHER refer to the same "
	    "column.", text_len, (ftnlen)48);
    s_copy(text + text_len * 478, " ", text_len, (ftnlen)1);
    return 0;
} /* zzhlp004_ */

