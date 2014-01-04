/* m2mon.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__12 = 12;

/* $Procedure      M2MON ( Determine whether or not a word is a month ) */
logical m2mon_(char *word, ftnlen word_len)
{
    /* Initialized data */

    static char short__[3*12] = "APR" "AUG" "DEC" "FEB" "JAN" "JUL" "JUN" 
	    "MAR" "MAY" "NOV" "OCT" "SEP";
    static char months[9*12] = "APRIL    " "AUGUST   " "DECEMBER " "FEBRUARY "
	     "JANUARY  " "JULY     " "JUNE     " "MARCH    " "MAY      " 
	    "NOVEMBER " "OCTOBER  " "SEPTEMBER";

    /* System generated locals */
    integer i__1;
    logical ret_val;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    static char copy[9];
    static integer i__;
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen);
    static integer month;
    extern integer ltrim_(char *, ftnlen);
    static integer start;
    extern integer bsrchc_(char *, integer *, char *, ftnlen, ftnlen);
    static integer length;
    extern integer qrtrim_(char *, ftnlen);
    static integer end;

/* $ Abstract */

/*     This function is true if the input string is a month in the */
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

/*     The function is returned as .TRUE. if word is an META/2 month. */

/* $ Detailed_Input */

/*     WORD      is a character string that is assumed to have no */
/*               spaces between the first and last non-blank characters. */

/* $ Detailed_Output */

/*     M2MON    returns as .TRUE. if WORD is less than 32 characters */
/*               in length, starts with an alphabetic character and */
/*               contains only letters, digits, underscores and hyphens. */
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
/*     determines whether or not a word is a month in the sense */
/*     of the language META/2. */

/* $ Examples */

/*     WORD                                  M2MON */
/*     -------                               ------ */
/*     SPAM                                  .FALSE. */
/*     JAN                                   .TRUE. */
/*     FEBR                                  .TRUE. */
/*     OCTA                                  .FALSE. */
/*     AUGU                                  .TRUE. */
/*     JU                                    .FALSE. */

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


/*     Make sure the string has the right length. */

    start = ltrim_(word, word_len);
    end = qrtrim_(word, word_len);
    length = end - start + 1;
    if (length < 3) {
	ret_val = FALSE_;
	return ret_val;
    }
    if (length > 9) {
	ret_val = FALSE_;
	return ret_val;
    }
    ucase_(word, copy, word_len, (ftnlen)9);

/*     See if the first three letters match anything we've got so far. */

    month = bsrchc_(copy + (start - 1), &c__12, short__, (ftnlen)3, (ftnlen)3)
	    ;
    if (month == 0) {
	ret_val = FALSE_;
	return ret_val;
    }

/*     Now make sure that any remaining letters match up exactly. */

    i__ = start + 3;
    ret_val = TRUE_;
    while(i__ <= end && ret_val) {
	ret_val = *(unsigned char *)&copy[i__ - 1] == *(unsigned char *)&
		months[((i__1 = month - 1) < 12 && 0 <= i__1 ? i__1 : s_rnge(
		"months", i__1, "m2mon_", (ftnlen)207)) * 9 + (i__ - 1)];
	++i__;
    }
    return ret_val;
} /* m2mon_ */

