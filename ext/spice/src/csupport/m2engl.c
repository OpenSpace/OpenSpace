/* m2engl.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      M2ENGL ( Determine if a word contains all letters) */
logical m2engl_(char *word, ftnlen word_len)
{
    /* System generated locals */
    logical ret_val;

    /* Local variables */
    static integer i__;
    extern integer ltrim_(char *, ftnlen);
    static integer start, length;
    extern integer qrtrim_(char *, ftnlen);
    static integer end;

/* $ Abstract */

/*     This function is true if the input string is an english word in */
/*     the sense of META/2. */

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

/*     META/2 a language specification language. */

/* $ Keywords */

/*     ALPHANUMERIC */
/*     ASCII */
/*     PARSING */
/*     UTILITY */
/*     WORD */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     WORD       I   A character string word */

/*     The function is returned as .TRUE. if word is an META/2 english */
/*     word */

/* $ Detailed_Input */

/*     WORD      is a character string that is assumed to have no */
/*               spaces between the first and last non-blank characters. */

/* $ Detailed_Output */

/*     M2ENGL    returns as .TRUE. if WORD is less than 32 characters */
/*               in length, and contains only letters. Otherwise it is */
/*               returned .FALSE. */

/* $ Error_Handling */

/*     None. */
/* C */
/* $ Input_Files */

/*     None. */

/* $ Output_Files */

/*     None. */

/* $ Particulars */

/*     This is a utility routine for the subroutine META2.  It */
/*     determines whether or not a word is an english word  name */
/*     in the sense of the language META/2. */

/* $ Examples */

/*     WORD                                  M2ENGL */
/*     -------                               ------ */
/*     SPAM                                  .TRUE. */
/*     _SPUD                                 .FALSE. */
/*     THE_QUICK_BROWN_FOX                   .FALSE. */
/*     THE_FIRST_TIME_EVERY_I_SAW_YOUR_FACE  .FALSE. */
/*     WHO?_ME?                              .FALSE. */
/*     D!#@!@#!                              .FALSE. */

/* $ Restrictions */

/*     None. */

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


/*     Version B1.0.0, 22-MAR-1988 (WLT) (IMU) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     WRDLEN is the parameter that gives the maximum allowed length */
/*     of a name. */


/*     Make sure the string has the right length. */

    start = ltrim_(word, word_len);
    end = qrtrim_(word, word_len);
    length = end - start + 1;
    ret_val = length <= 32 && length >= 1;
    i__ = start;
    while(ret_val && i__ <= end) {
	ret_val = 'A' <= *(unsigned char *)&word[i__ - 1] && 'Z' >= *(
		unsigned char *)&word[i__ - 1] || 'a' <= *(unsigned char *)&
		word[i__ - 1] && 'z' >= *(unsigned char *)&word[i__ - 1];
	++i__;
    }
    return ret_val;
} /* m2engl_ */

