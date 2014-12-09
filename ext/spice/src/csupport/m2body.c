/* m2body.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      M2BODY ( Determine whether or not a word is a body ) */
logical m2body_(char *word, ftnlen word_len)
{
    /* System generated locals */
    logical ret_val;

    /* Local variables */
    char copy[32];
    extern logical m2int_(char *, ftnlen);
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen);
    logical found;
    integer idcode;
    extern /* Subroutine */ int m2bodn2c_(char *, integer *, logical *, 
	    ftnlen);

/* $ Abstract */

/*     This function is true if the input string is a known body in the */
/*     sense of META/2. */

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

/*     The function is returned as .TRUE. if word is a META/2 body. */

/* $ Detailed_Input */

/*     WORD      is a character string that is assumed to have no */
/*               spaces between the first and last non-blank characters. */

/* $ Detailed_Output */

/*     M2INT     returns as .TRUE. if WORD is a META/2 body. */
/*               Otherwise it is returned .FALSE. */

/* $ Error_Handling */

/*     None. */
/* C */
/* $ Input_Files */

/*     None. */

/* $ Output_Files */

/*     None. */

/* $ Particulars */

/*     This is a utility routine for the subroutine META2.  It */
/*     determines whether or not a word is a body in the sense */
/*     of the language META/2. */

/* $ Examples */

/*     WORD                                  M2BODY */
/*     -------                               ------ */
/*     JUPITER                               .TRUE. */
/*     1                                     .TRUE. */
/*     0.289E19                              .FALSE. */
/*     0.2728D12                             .FALSE. */
/*     -12.1892e-5                           .FALSE. */
/*     12.E29                                .FALSE. */
/*     12.E291                               .FALSE. */
/*     1.2E10                                .TRUE. */
/*     .E12                                  .FALSE. */
/*     1.2E.12                               .FALSE. */

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

/*     Library functions */


/*     Local Parameters */


/*     Local Variables */

    if (m2int_(word, word_len)) {
	ret_val = TRUE_;
	return ret_val;
    }
    ucase_(word, copy, word_len, (ftnlen)32);
    m2bodn2c_(copy, &idcode, &found, (ftnlen)32);
    ret_val = found;
    return ret_val;
} /* m2body_ */

