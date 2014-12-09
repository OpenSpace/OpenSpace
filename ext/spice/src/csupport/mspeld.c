/* mspeld.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__0 = 0;

/* $Procedure      MSPELD ( Misspelling diagnosis ) */
/* Subroutine */ int mspeld_(char *word, char *guess, char *cause, ftnlen 
	word_len, ftnlen guess_len, ftnlen cause_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char last[16];
    extern /* Subroutine */ int lcase_(char *, char *, ftnlen, ftnlen);
    char first[16];
    extern /* Subroutine */ int matche_(char *, char *, char *, integer *, 
	    ftnlen, ftnlen, ftnlen);
    char diagns[12];
    extern /* Subroutine */ int intord_(integer *, char *, ftnlen), suffix_(
	    char *, integer *, char *, ftnlen, ftnlen);
    integer loc;

/* $ Abstract */

/*     Diagnose possible spelling errors that might cause a word */
/*     to differ from another (known) word. */

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

/*     COMPARE */
/*     ERROR */
/*     PARSING */
/*     UTILITY */
/*     WORD */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     WORD       I   A word that is thought to be misspelled. */
/*     GUESS      I   A word that is thought to be "close" to WORD. */
/*     CAUSE      O   A message indicating the difference between them. */

/* $ Detailed_Input */

/*     WORD       A word that is thought to be misspelled. */

/*     GUESS      A word that is thought to be "close" to WORD. */

/* $ Detailed_Output */

/*     CAUSE      A message that indicates the difference between WORD */
/*                and GUESS. */

/* $ Exceptions */

/*     1) CAUSE is blank whenever WORD and GUESS are the same. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     A number of spelling errors are due to the lack of cooperation */
/*     between the hands that do the typing and the brain that knows */
/*     how something should be spelled.  Four common errors are: */

/*        1) Leaving out a necessary character. */
/*        2) Adding an extra character. */
/*        3) Mistyping a single character. */
/*        4) Transposing two characters. */

/*     This routine creates "friendly" diagnostic messages indicating */
/*     whether or not the difference between WORD and GUESS could have */
/*     been caused by one of these simple errors. */

/*     This routine will typically be used only after the list of */
/*     guesses has been narrowed down to words that are "close" to */
/*     the unrecognized word. */

/* $ Examples */
/* $ */

/*      WORD  :   LENGHT */
/*      GUESS :   LENGTH */
/*      CAUSE :  'It appears that you have transposed the fifth and */
/*                sixth letters of LENGTH (the letters T and H).' */


/*      WORD  :   EPHEMRIS */
/*      GUESS :   EPHEMERIS */
/*      CAUSE :  'It appears that you have left out the sixth letter of */
/*                EPHEMERIS. (The sixth letter should be E.)' */

/*      WORD  :   INTWGRATE */
/*      WORD  :   INTEGRATE */
/*      CAUSE :   'It appears that you have mistyped the fourth letter */
/*                 of INTEGRATE.  (The fourth letter should be E. You */
/*                 have W instead.)' */

/*      WORD :    INTERGER */
/*      GUESS:    INTEGER */
/*      CAUSE    'It appears that you have an extra letter at the fifth */
/*                letter of INTERGER. (The fifth letter R should be */
/*                removed.)' */

/*      WORD :    URUNAS */
/*      GUESS:    URANUS */
/*      CAUSE:   'I believe you meant URANUS. However, the actual */
/*                spelling error is not a simple one.' */

/*      WORD :    INTERDENOMINATIONAL */
/*      GUESS:    INTERDENOMINATIONAL */
/*      CAUSE:   ' ' */

/* $ Restrictions */

/*      Any restrictions that apply to the words compared by MATCHE */
/*      apply as well to WORD and GUESS. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */

/* $ Version */

/* -     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 9, 1994 */


/* -     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of META/2 */
/*         software as of May 3, 1994 */


/*     Version B1.0.0, 13-APR-1988 (WLT) (IMU) */

/* -& */

/*     Local variables */

    matche_(word, guess, diagns, &loc, word_len, guess_len, (ftnlen)12);
    if (s_cmp(diagns, "IDENTITY", (ftnlen)12, (ftnlen)8) == 0) {
	s_copy(cause, " ", cause_len, (ftnlen)1);
    } else if (s_cmp(diagns, "TRANSPOSE", (ftnlen)12, (ftnlen)9) == 0) {
	intord_(&loc, first, (ftnlen)16);
	i__1 = loc + 1;
	intord_(&i__1, last, (ftnlen)16);
	lcase_(first, first, (ftnlen)16, (ftnlen)16);
	lcase_(last, last, (ftnlen)16, (ftnlen)16);
	s_copy(cause, "It appears that you have transposed the ", cause_len, (
		ftnlen)40);
	suffix_(first, &c__1, cause, (ftnlen)16, cause_len);
	suffix_("and", &c__1, cause, (ftnlen)3, cause_len);
	suffix_(last, &c__1, cause, (ftnlen)16, cause_len);
	suffix_("letters of", &c__1, cause, (ftnlen)10, cause_len);
	suffix_(guess, &c__1, cause, guess_len, cause_len);
	suffix_("(the letters", &c__1, cause, (ftnlen)12, cause_len);
	suffix_(guess + (loc - 1), &c__1, cause, (ftnlen)1, cause_len);
	suffix_("and", &c__1, cause, (ftnlen)3, cause_len);
	i__1 = loc;
	suffix_(guess + i__1, &c__1, cause, loc + 1 - i__1, cause_len);
	suffix_(").", &c__0, cause, (ftnlen)2, cause_len);
    } else if (s_cmp(diagns, "INSERT", (ftnlen)12, (ftnlen)6) == 0) {
	intord_(&loc, first, (ftnlen)16);
	lcase_(first, first, (ftnlen)16, (ftnlen)16);
	s_copy(cause, "It appears that you have left out the ", cause_len, (
		ftnlen)38);
	suffix_(first, &c__1, cause, (ftnlen)16, cause_len);
	suffix_("letter of ", &c__1, cause, (ftnlen)10, cause_len);
	suffix_(guess, &c__1, cause, guess_len, cause_len);
	suffix_(". (The ", &c__0, cause, (ftnlen)7, cause_len);
	suffix_(first, &c__1, cause, (ftnlen)16, cause_len);
	suffix_("letter should be ", &c__1, cause, (ftnlen)17, cause_len);
	suffix_(guess + (loc - 1), &c__1, cause, (ftnlen)1, cause_len);
	suffix_(".)", &c__0, cause, (ftnlen)2, cause_len);
    } else if (s_cmp(diagns, "REPLACE", (ftnlen)12, (ftnlen)7) == 0) {
	intord_(&loc, first, (ftnlen)16);
	lcase_(first, first, (ftnlen)16, (ftnlen)16);
	s_copy(cause, "It appears that you have mistyped the ", cause_len, (
		ftnlen)38);
	suffix_(first, &c__1, cause, (ftnlen)16, cause_len);
	suffix_("letter of ", &c__1, cause, (ftnlen)10, cause_len);
	suffix_(guess, &c__1, cause, guess_len, cause_len);
	suffix_(". (The ", &c__0, cause, (ftnlen)7, cause_len);
	suffix_(first, &c__1, cause, (ftnlen)16, cause_len);
	suffix_("letter should be ", &c__1, cause, (ftnlen)17, cause_len);
	suffix_(guess + (loc - 1), &c__1, cause, (ftnlen)1, cause_len);
	suffix_(". You have ", &c__0, cause, (ftnlen)11, cause_len);
	suffix_(word + (loc - 1), &c__1, cause, (ftnlen)1, cause_len);
	suffix_("instead.)", &c__1, cause, (ftnlen)9, cause_len);
    } else if (s_cmp(diagns, "REMOVE", (ftnlen)12, (ftnlen)6) == 0) {
	intord_(&loc, first, (ftnlen)16);
	lcase_(first, first, (ftnlen)16, (ftnlen)16);
	s_copy(cause, "It appears that you have an extra letter at the ", 
		cause_len, (ftnlen)48);
	suffix_(first, &c__1, cause, (ftnlen)16, cause_len);
	suffix_("letter of ", &c__1, cause, (ftnlen)10, cause_len);
	suffix_(word, &c__1, cause, word_len, cause_len);
	suffix_(". (The ", &c__0, cause, (ftnlen)7, cause_len);
	suffix_(first, &c__1, cause, (ftnlen)16, cause_len);
	suffix_("letter ", &c__1, cause, (ftnlen)7, cause_len);
	suffix_(word + (loc - 1), &c__1, cause, (ftnlen)1, cause_len);
	suffix_("should be removed.)", &c__1, cause, (ftnlen)19, cause_len);
    } else {
	s_copy(cause, "I believe you meant ", cause_len, (ftnlen)20);
	suffix_(guess, &c__1, cause, guess_len, cause_len);
	suffix_(".  However, the actual spelling ", &c__1, cause, (ftnlen)32, 
		cause_len);
	suffix_("error is not a simple one.      ", &c__1, cause, (ftnlen)32, 
		cause_len);
    }
    return 0;
} /* mspeld_ */

