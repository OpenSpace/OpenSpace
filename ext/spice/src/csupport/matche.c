/* matche.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure MATCHE ( Match two words, allowing for common errors ) */
/* Subroutine */ int matche_(char *word, char *guess, char *transf, integer *
	loc, ftnlen word_len, ftnlen guess_len, ftnlen transf_len)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer clen;
    char copy[65];
    integer i__;
    char templ[65];
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen), 
	    remsub_(char *, integer *, integer *, char *, ftnlen, ftnlen);
    char mygues[65];
    extern integer qrtrim_(char *, ftnlen);

/* $ Abstract */

/*      Determines whether or not two words may be the same, */
/*      allowing for common typing errors. */

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

/* $ Keywords */

/*      SEARCH */
/*      UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*      Variable  I/O  Description */
/*      --------  ---  -------------------------------------------------- */
/*      WORD       I   Word to be matched against initial guess. */
/*      GUESS      I   Initial guess. */
/*      TRANSF     O   Transformation that makes WORD match GUESS. */
/*      LOC        O   Location at which to apply transformation. */

/* $ Detailed_Input */

/*      WORD       is a character string to be checked for a match */
/*                 against an initial guess. Leading and trailing */
/*                 blanks are ignored. Typically, WORD will contain */
/*                 a single word. In any case, the significant part */
/*                 of WORD may not exceed 64 characters. */

/*      GUESS      is an initial guess at the value of the input word. */
/*                 Leading and trailing blanks are ignored. Like WORD, */
/*                 this will typically be a single word. */

/* $ Detailed_Output */

/*      TRANSF     is the name of a transformation which, when applied */
/*                 to WORD, makes WORD match with GUESS. The possible */
/*                 transformations are: */

/*                    'TRANSPOSE'   Transpose two characters. */

/*                    'REPLACE'     Replace a single character. */

/*                    'INSERT'      Insert an extra character. */

/*                    'REMOVE'      Remove a character. */

/*                    'IDENTITY'    Do nothing. */

/*                 These reflect some of the most common typing mistakes. */
/*                 If none if these transformations will do the trick, */
/*                 TRANSF is 'NONE'. */

/*      LOC        is the location at which the indicated transformation */
/*                 should be applied. */

/*                    When TRANSF is   LOC is */
/*                    --------------   ------ */
/*                    'TRANSPOSE'      Location of the first character */
/*                                     to be transposed. */

/*                    'REPLACE'        Location of the character to be */
/*                                     replaced. */

/*                    'INSERT'         Location at which the character */
/*                                     should be inserted. */

/*                    'REMOVE'         Location of the character to be */
/*                                     removed. */

/*                    'IDENTITY'       Zero. */

/*                    'NONE'           Zero. */

/* $ Exceptions */

/*      None. */

/* $ Particulars */

/*      Some typing mistakes should be relatively easy to catch, since */
/*      the difference between the intended word and the typed word may */
/*      involve a single transformation. MATCHE applies the most common */
/*      transformations to an input word, and attempt to match the */
/*      resulting word to a an initial guess. */

/* $ Examples */

/*      Let */

/*         GUESS = 'APPLE' */

/*      Then */

/*         If WORD is        TRANSF is         LOC is */
/*         -----------       -------------     ------ */
/*         'APPEL'           'TRANSPOSE'        4 */
/*         'APPLY'           'REPLACE'          5 */
/*         'DAPPLE'          'REMOVE'           1 */
/*         'APPLES'          'REMOVE'           5 */
/*         'PPLE'            'INSERT'           1 */
/*         'APPE'            'INSERT'           4 */
/*         'APPL'            'INSERT'           5 */
/*         'APPLE'           'IDENTITY'         0 */
/*         'APPEAL'          'NONE'             0 */

/* $ Restrictions */

/*      1) MATCHE is case-sensitive. Lowercase characters do not match */
/*         uppercase characters, and vice versa. */

/*      2) ASCII characters 1 and 2 are used internally as wildcard */
/*         characters, and should not appear in either WORD or GUESS. */

/* $ Common_Variables */

/*      None. */

/* $ Author_and_Institution */

/*      W.L. Taber     (JPL) */
/*      I.M. Underwood (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 9, 1994 */


/* -     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of META/2 */
/*         software as of May 3, 1994 */


/*      Version B 1.0.0, 5-APR-1988 */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Save a copy of the first 64 significant characters in a buffer, */
/*     from which we may construct templates. */

    s_copy(copy, " ", (ftnlen)65, (ftnlen)1);
    ljust_(word, copy, word_len, (ftnlen)64);
    ljust_(guess, mygues, guess_len, (ftnlen)65);
    clen = qrtrim_(copy, (ftnlen)65);

/*     Apply the transformations one at a time, in the order most */
/*     likely to succeed: */

/*        Removal */
/*        Transposition */
/*        Replacement */
/*        Insertion */

/*     Quit as soon as a possible match is found. */

/*     Actually, we need to check for identity first. Otherwise, */
/*     we're likely to find a transposition that yields the same */
/*     word: for example, transposing the second and third letters */
/*     of APPLE yields APPLE. */

    if (eqstr_(word, mygues, word_len, (ftnlen)65)) {
	s_copy(transf, "IDENTITY", transf_len, (ftnlen)8);
	*loc = 0;
	return 0;
    }

/*     Removal */
/*     ------- */

/*     Remove the character at each location, and check against MYGUES. */

    i__1 = clen;
    for (i__ = 1; i__ <= i__1; ++i__) {
	remsub_(copy, &i__, &i__, templ, (ftnlen)65, (ftnlen)65);
	if (eqstr_(templ, mygues, (ftnlen)65, (ftnlen)65)) {
	    s_copy(transf, "REMOVE", transf_len, (ftnlen)6);
	    *loc = i__;
	    return 0;
	}
    }

/*     Transposition */
/*     ------------- */

/*     Transpose each pair of characters, and check against MYGUES. */

    i__1 = clen - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s_copy(templ, copy, (ftnlen)65, (ftnlen)65);
	i__2 = i__;
	s_copy(templ + (i__ - 1), copy + i__2, (ftnlen)1, i__ + 1 - i__2);
	i__2 = i__;
	s_copy(templ + i__2, copy + (i__ - 1), i__ + 1 - i__2, (ftnlen)1);
	if (eqstr_(templ, mygues, (ftnlen)65, (ftnlen)65)) {
	    s_copy(transf, "TRANSPOSE", transf_len, (ftnlen)9);
	    *loc = i__;
	    return 0;
	}
    }

/*     Replacement */
/*     ----------- */

/*     Replace each character with a wild character, and check */
/*     against MYGUES. */

    i__1 = clen;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s_copy(templ, copy, (ftnlen)65, (ftnlen)65);
	*(unsigned char *)&templ[i__ - 1] = *(unsigned char *)&mygues[i__ - 1]
		;
	if (eqstr_(templ, mygues, (ftnlen)65, (ftnlen)65)) {
	    s_copy(transf, "REPLACE", transf_len, (ftnlen)7);
	    *loc = i__;
	    return 0;
	}
    }

/*     Insertion */
/*     --------- */

/*     Insert a wild character at each location, and check against */
/*     MYGUES. */

    i__1 = clen + 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (i__ == 1) {
	    *(unsigned char *)templ = *(unsigned char *)mygues;
	    s_copy(templ + 1, copy, (ftnlen)64, (ftnlen)65);
	} else if (i__ == clen + 1) {
	    s_copy(templ, copy, (ftnlen)65, (ftnlen)65);
	    *(unsigned char *)&templ[i__ - 1] = *(unsigned char *)&mygues[i__ 
		    - 1];
	} else {
	    s_copy(templ, copy, i__ - 1, i__ - 1);
	    *(unsigned char *)&templ[i__ - 1] = *(unsigned char *)&mygues[i__ 
		    - 1];
	    i__2 = i__;
	    s_copy(templ + i__2, copy + (i__ - 1), 65 - i__2, 65 - (i__ - 1));
	}
	if (eqstr_(templ, mygues, (ftnlen)65, (ftnlen)65)) {
	    s_copy(transf, "INSERT", transf_len, (ftnlen)6);
	    *loc = i__;
	    return 0;
	}
    }

/*     None of these transformations work. */

    s_copy(transf, "NONE", transf_len, (ftnlen)4);
    *loc = 0;
    return 0;
} /* matche_ */

