/* m2numb.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      M2NUMB ( Determine whether or not a word is a number ) */
logical m2numb_(char *word, ftnlen word_len)
{
    /* System generated locals */
    logical ret_val;

    /* Builtin functions */
    logical l_le(char *, char *, ftnlen, ftnlen), l_ge(char *, char *, ftnlen,
	     ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static doublereal x;
    extern integer ltrim_(char *, ftnlen);
    static char error[80];
    static integer start, length;
    extern /* Subroutine */ int nparsd_(char *, doublereal *, char *, integer 
	    *, ftnlen, ftnlen);
    static integer pointr;
    extern integer qrtrim_(char *, ftnlen);
    static integer end;

/* $ Abstract */

/*     This function is true if the input string is a number in the */
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

/*     The function is returned as .TRUE. if word is an META/2 number. */

/* $ Detailed_Input */

/*     WORD      is a character string that is assumed to have no */
/*               spaces between the first and last non-blank characters. */

/* $ Detailed_Output */

/*     M2NUMB    returns as .TRUE. if WORD is a parsable number. */
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
/*     determines whether or not a word is a number in the sense */
/*     of the language META/2. */

/* $ Examples */

/*     WORD                                  M2NUMB */
/*     -------                               ------ */
/*     SPAM                                  .FALSE. */
/*     1                                     .TRUE. */
/*     0.289E19                              .TRUE. */
/*     0.2728D12                             .TRUE. */
/*     -12.1892e-5                           .TRUE. */
/*     12.E29                                .TRUE. */
/*     12.E291                               .FALSE. */
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

/*     SPICELIB functions */


/*     Local variables */


/*     Make sure the string has the right length. */

    start = ltrim_(word, word_len);
    end = qrtrim_(word, word_len);
    length = end - start + 1;

/*     Rule out the goofy cases that NPARSD will allow. */

    if (length == 1) {
	ret_val = l_le("0", word, (ftnlen)1, word_len) && l_ge("9", word, (
		ftnlen)1, word_len);
	return ret_val;
    }
    if (length >= 2) {
	if (*(unsigned char *)&word[start - 1] == 'E' || *(unsigned char *)&
		word[start - 1] == 'e' || *(unsigned char *)&word[start - 1] 
		== 'D' || *(unsigned char *)&word[start - 1] == 'd') {
	    ret_val = FALSE_;
	    return ret_val;
	}
	if (s_cmp(word + (start - 1), "+E", (ftnlen)2, (ftnlen)2) == 0 || 
		s_cmp(word + (start - 1), "-E", (ftnlen)2, (ftnlen)2) == 0 || 
		s_cmp(word + (start - 1), "+D", (ftnlen)2, (ftnlen)2) == 0 || 
		s_cmp(word + (start - 1), "-D", (ftnlen)2, (ftnlen)2) == 0 || 
		s_cmp(word + (start - 1), "-e", (ftnlen)2, (ftnlen)2) == 0 || 
		s_cmp(word + (start - 1), "+e", (ftnlen)2, (ftnlen)2) == 0 || 
		s_cmp(word + (start - 1), "-d", (ftnlen)2, (ftnlen)2) == 0 || 
		s_cmp(word + (start - 1), "+d", (ftnlen)2, (ftnlen)2) == 0 || 
		s_cmp(word + (start - 1), ".E", (ftnlen)2, (ftnlen)2) == 0 || 
		s_cmp(word + (start - 1), ".D", (ftnlen)2, (ftnlen)2) == 0 || 
		s_cmp(word + (start - 1), ".e", (ftnlen)2, (ftnlen)2) == 0 || 
		s_cmp(word + (start - 1), ".d", (ftnlen)2, (ftnlen)2) == 0) {
	    ret_val = FALSE_;
	    return ret_val;
	}
    }
    if (length >= 3) {
	if (s_cmp(word + (start - 1), "+.E", (ftnlen)3, (ftnlen)3) == 0 || 
		s_cmp(word + (start - 1), "-.E", (ftnlen)3, (ftnlen)3) == 0 ||
		 s_cmp(word + (start - 1), "+.D", (ftnlen)3, (ftnlen)3) == 0 
		|| s_cmp(word + (start - 1), "-.D", (ftnlen)3, (ftnlen)3) == 
		0) {
	    ret_val = FALSE_;
	    return ret_val;
	}
    }

/*     Ok.  Now just hit the word with NPARSD. */

    s_copy(error, " ", (ftnlen)80, (ftnlen)1);
    nparsd_(word, &x, error, &pointr, word_len, (ftnlen)80);

/*     Any errors indicate we don't have a number. */

    if (s_cmp(error, " ", (ftnlen)80, (ftnlen)1) != 0) {
	ret_val = FALSE_;
    } else {
	ret_val = TRUE_;
    }
    return ret_val;
} /* m2numb_ */

