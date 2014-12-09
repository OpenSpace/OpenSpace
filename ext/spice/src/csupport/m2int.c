/* m2int.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      M2INT ( Determine whether or not a word is an integer ) */
logical m2int_(char *word, ftnlen word_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    integer i__1, i__2;
    logical ret_val;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    static integer zero, plus, i__, value;
    extern integer ltrim_(char *, ftnlen);
    static integer minus, start, factor, length;
    extern integer intmin_(void), intmax_(void);
    static logical usemin;
    static integer subseq;
    extern integer qrtrim_(char *, ftnlen);
    static logical bad[256];
    static integer end;

/* $ Abstract */

/*     This function is true if the input string is an integer in the */
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

/*     The function is returned as .TRUE. if word is a META/2 integer. */

/* $ Detailed_Input */

/*     WORD      is a character string that is assumed to have no */
/*               spaces between the first and last non-blank characters. */

/* $ Detailed_Output */

/*     M2INT     returns as .TRUE. if WORD is a META/2 integer. */
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
/*     determines whether or not a word is an integer in the sense */
/*     of the language META/2. */

/* $ Examples */

/*     WORD                                  M2INT */
/*     -------                               ------ */
/*     SPAM                                  .FALSE. */
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

/*     SPICELIB functions */


/*     Local variables */

    if (first) {
	first = FALSE_;
	for (i__ = 0; i__ <= 255; ++i__) {
	    bad[(i__1 = i__) < 256 && 0 <= i__1 ? i__1 : s_rnge("bad", i__1, 
		    "m2int_", (ftnlen)168)] = TRUE_;
	}
	minus = '-';
	plus = '+';
	zero = '0';
	bad[(i__1 = '0') < 256 && 0 <= i__1 ? i__1 : s_rnge("bad", i__1, 
		"m2int_", (ftnlen)175)] = FALSE_;
	bad[(i__1 = '1') < 256 && 0 <= i__1 ? i__1 : s_rnge("bad", i__1, 
		"m2int_", (ftnlen)176)] = FALSE_;
	bad[(i__1 = '2') < 256 && 0 <= i__1 ? i__1 : s_rnge("bad", i__1, 
		"m2int_", (ftnlen)177)] = FALSE_;
	bad[(i__1 = '3') < 256 && 0 <= i__1 ? i__1 : s_rnge("bad", i__1, 
		"m2int_", (ftnlen)178)] = FALSE_;
	bad[(i__1 = '4') < 256 && 0 <= i__1 ? i__1 : s_rnge("bad", i__1, 
		"m2int_", (ftnlen)179)] = FALSE_;
	bad[(i__1 = '5') < 256 && 0 <= i__1 ? i__1 : s_rnge("bad", i__1, 
		"m2int_", (ftnlen)180)] = FALSE_;
	bad[(i__1 = '6') < 256 && 0 <= i__1 ? i__1 : s_rnge("bad", i__1, 
		"m2int_", (ftnlen)181)] = FALSE_;
	bad[(i__1 = '7') < 256 && 0 <= i__1 ? i__1 : s_rnge("bad", i__1, 
		"m2int_", (ftnlen)182)] = FALSE_;
	bad[(i__1 = '8') < 256 && 0 <= i__1 ? i__1 : s_rnge("bad", i__1, 
		"m2int_", (ftnlen)183)] = FALSE_;
	bad[(i__1 = '9') < 256 && 0 <= i__1 ? i__1 : s_rnge("bad", i__1, 
		"m2int_", (ftnlen)184)] = FALSE_;
    }
    start = ltrim_(word, word_len);
    end = qrtrim_(word, word_len);
    length = end - start + 1;
    subseq = start + 1;
    if (length == 1) {
	bad[(i__1 = minus) < 256 && 0 <= i__1 ? i__1 : s_rnge("bad", i__1, 
		"m2int_", (ftnlen)196)] = TRUE_;
	bad[(i__1 = plus) < 256 && 0 <= i__1 ? i__1 : s_rnge("bad", i__1, 
		"m2int_", (ftnlen)197)] = TRUE_;
	ret_val = ! bad[(i__1 = *(unsigned char *)&word[start - 1]) < 256 && 
		0 <= i__1 ? i__1 : s_rnge("bad", i__1, "m2int_", (ftnlen)199)]
		;
	return ret_val;
    } else if (length > 10) {
	ret_val = FALSE_;
    } else {
	bad[(i__1 = minus) < 256 && 0 <= i__1 ? i__1 : s_rnge("bad", i__1, 
		"m2int_", (ftnlen)208)] = FALSE_;
	bad[(i__1 = plus) < 256 && 0 <= i__1 ? i__1 : s_rnge("bad", i__1, 
		"m2int_", (ftnlen)209)] = FALSE_;
    }
    if (bad[(i__1 = *(unsigned char *)&word[start - 1]) < 256 && 0 <= i__1 ? 
	    i__1 : s_rnge("bad", i__1, "m2int_", (ftnlen)213)]) {
	ret_val = FALSE_;
	return ret_val;
    }
    bad[(i__1 = minus) < 256 && 0 <= i__1 ? i__1 : s_rnge("bad", i__1, "m2in"
	    "t_", (ftnlen)218)] = TRUE_;
    bad[(i__1 = plus) < 256 && 0 <= i__1 ? i__1 : s_rnge("bad", i__1, "m2int_"
	    , (ftnlen)219)] = TRUE_;
    i__1 = end;
    for (i__ = subseq; i__ <= i__1; ++i__) {
	if (bad[(i__2 = *(unsigned char *)&word[i__ - 1]) < 256 && 0 <= i__2 ?
		 i__2 : s_rnge("bad", i__2, "m2int_", (ftnlen)222)]) {
	    ret_val = FALSE_;
	    return ret_val;
	}
    }

/*     We allow 10 digit numbers only if the first character */
/*     is a '+' or '-'  So if we have 10 digits the first must */
/*     now be a "bad" character. */

    usemin = *(unsigned char *)&word[start - 1] == minus;
    if (bad[(i__1 = *(unsigned char *)&word[start - 1]) < 256 && 0 <= i__1 ? 
	    i__1 : s_rnge("bad", i__1, "m2int_", (ftnlen)234)]) {
	if (length < 11) {
	    ret_val = TRUE_;
	    return ret_val;
	}
	start = subseq;
    } else if (length == 11) {
	ret_val = FALSE_;
	return ret_val;
    } else if (length < 10) {
	ret_val = TRUE_;
	return ret_val;
    }
    if (usemin) {
	value = intmin_();
	factor = 1;
	i__1 = start + 1;
	for (i__ = end; i__ >= i__1; --i__) {
	    value += (*(unsigned char *)&word[i__ - 1] - zero) * factor;
	    factor *= 10;
	}
	if (*(unsigned char *)&word[start - 1] > '2') {
	    ret_val = FALSE_;
	} else {
	    i__ = start;
	    value += (*(unsigned char *)&word[i__ - 1] - zero) * factor;
	    ret_val = value <= 0;
	}
    } else {
	value = intmax_();
	factor = 1;
	i__1 = start + 1;
	for (i__ = end; i__ >= i__1; --i__) {
	    value -= (*(unsigned char *)&word[i__ - 1] - zero) * factor;
	    factor *= 10;
	}
	if (*(unsigned char *)&word[start - 1] > '2') {
	    ret_val = FALSE_;
	} else {
	    i__ = start;
	    value -= (*(unsigned char *)&word[i__ - 1] - zero) * factor;
	    ret_val = value >= 0;
	}
    }
    return ret_val;
} /* m2int_ */

