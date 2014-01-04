/* m2ntem.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      M2NTEM ( Parse the numeric template of a @number ) */
/* Subroutine */ int m2ntem_(char *string, char *base, integer *beg, integer *
	end, doublereal *a, doublereal *b, ftnlen string_len, ftnlen base_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen), i_indx(char *, char *, 
	    ftnlen, ftnlen);

    /* Local variables */
    static integer last, j, k, begin;
    extern doublereal dpmin_(void), dpmax_(void);
    static char error[80];
    static doublereal minval;
    extern /* Subroutine */ int nparsd_(char *, doublereal *, char *, integer 
	    *, ftnlen, ftnlen);
    static doublereal maxval;
    extern integer intmin_(void), intmax_(void);

/* $ Abstract */

/*     Parse the numeric template of a META/2 @numeric META-KEY. */

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

/*     PARSING */
/*     UTILITY */
/*     WORD */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   A META/2 language statement specification. */
/*     BASE       I   Type of META-KEY associated with this template. */
/*     BEG       I/0  The beginning of the substring on input and output */
/*     END       I/0  The end of the substring on input and output */
/*     A          O   Lower value of the numeric restriction template */
/*     B          O   Upper value of the numeric restriction template */

/* $ Detailed_Input */

/*     STRING(BEG:END)  is a word in the META/2 language.  Is a META/2 */
/*                      range restriction template.  It has the form */
/*                      (A:B) where A and B are both chracter strings */
/*                      representing numbers. */

/*     BASE             is a character string and should be '@int' or */
/*                      '@number'. */

/* $ Detailed_Output */

/*     BEG        On ouput BEG points to the first character following */
/*                the input value of END. */

/*     END        is returned unchanged. */

/*     A          is the value represented by the first numeric string */
/*                of the restriction template.  If a numeric string */
/*                is not present, A is not assigned the minimum possible */
/*                value associated with the data type given in BASE. */

/*     B          is the value represented by the second numeric string */
/*                of the restriction template (if there is a second */
/*                numeric string)  If no numeric string is present B is */
/*                assigned the maximum possible value associated with */
/*                the data type given in BASE. */


/* $ Error_Handling */

/*     None. */

/* $ Input_Files */

/*     None. */

/* $ Output_Files */

/*     None. */

/* $ Particulars */

/*      The range restriction template is part of the META/2 language */
/*      and is  described in the required reading section.  Briefly it */
/*      is a string at the beginning of a word that has the form */

/*      (A:B) */

/*      where A is a string representing a positive integer, and */
/*      B the null string or a string representing a positive integer */
/*      greater than A. */

/*      This routine determines if a range template is present and if so */
/*      what the values of A and B are.  If A (or B )is the null string */
/*      it is assumed to represent the smallest possible (largest */
/*      possible ) number of the type indicated by BASE. */

/* $ Examples */

/*      None. */

/* $ Restrictions */

/*      None. */

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


/*     Version B1.0.0, 23-MAR-1988 (WLT) (IMU) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */

    last = *end - 1;
    begin = *beg + 1;

/*     First get the min and max's for this kind of word. */

    if (s_cmp(base, "@int", base_len, (ftnlen)4) == 0) {
	minval = (doublereal) intmin_();
	maxval = (doublereal) intmax_();
    } else {
	minval = dpmin_();
	maxval = dpmax_();
    }

/*      parse the restriction template */

    if (*(unsigned char *)&string[begin - 1] == ':') {
	*a = minval;
	i__1 = begin;
	nparsd_(string + i__1, b, error, &j, last - i__1, (ftnlen)80);
	if (s_cmp(error, " ", (ftnlen)80, (ftnlen)1) != 0) {
	    *b = maxval;
	}
    } else if (*(unsigned char *)&string[last - 1] == ':') {
	nparsd_(string + (begin - 1), a, error, &j, last - 1 - (begin - 1), (
		ftnlen)80);
	if (s_cmp(error, " ", (ftnlen)80, (ftnlen)1) != 0) {
	    *a = minval;
	}
	*b = maxval;
    } else {
	j = i_indx(string + (begin - 1), ":", last - (begin - 1), (ftnlen)1) 
		+ *beg;
	nparsd_(string + (begin - 1), a, error, &k, j - 1 - (begin - 1), (
		ftnlen)80);
	if (s_cmp(error, " ", (ftnlen)80, (ftnlen)1) != 0) {
	    *a = minval;
	}
	i__1 = j;
	nparsd_(string + i__1, b, error, &k, last - i__1, (ftnlen)80);
	if (s_cmp(error, " ", (ftnlen)80, (ftnlen)1) != 0) {
	    *b = maxval;
	}
    }
    *beg = *end + 1;
    return 0;
} /* m2ntem_ */

