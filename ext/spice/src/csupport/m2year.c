/* m2year.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      M2YEAR ( Determine whether or not a word is a year ) */
logical m2year_(char *word, ftnlen word_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    logical ret_val;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    static integer i__, value;
    extern integer ltrim_(char *, ftnlen);
    static integer i1, i2, i3, i4, length, values[256];
    extern integer qrtrim_(char *, ftnlen);

/* $ Abstract */

/*     This function is true if the input string is a year in the */
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

/*     The function is returned as .TRUE. if word is a META/2 year. */

/* $ Detailed_Input */

/*     WORD      is a character string that is assumed to have no */
/*               spaces between the first and last non-blank characters. */

/* $ Detailed_Output */

/*     M2YEAR     returns as .TRUE. if WORD is a META/2 year. */
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
/*     determines whether or not a word is a year in the sense */
/*     of the language META/2. */

/* $ Examples */

/*     WORD                                  M2YEAR */
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

/* -     META/2 Configured Version 2.1.0, 29-DEC-1994 (WLT) */

/*         The computation of the length of the input string */
/*         was incorrect.  It has been fixed.  It used to be */

/*            LENGTH = I3 - I1 + 1 */

/*         Now it is */

/*            LENGTH = I4 - I1 + 1 */



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

/*        We will construct a value for the string by taking */
/*        the non-blank portion and computing the value assuming */
/*        that the first non-blank is a digit with the appropriate */
/*        power of 10 attached.  Since all non-digit characters */
/*        will have values of 1000, we will get a value greater */
/*        than 1000 if any non-digit characters are present. */

	for (i__ = 0; i__ <= 255; ++i__) {
	    values[(i__1 = i__) < 256 && 0 <= i__1 ? i__1 : s_rnge("values", 
		    i__1, "m2year_", (ftnlen)182)] = 10000;
	}
	values[(i__1 = '0') < 256 && 0 <= i__1 ? i__1 : s_rnge("values", i__1,
		 "m2year_", (ftnlen)185)] = 0;
	values[(i__1 = '1') < 256 && 0 <= i__1 ? i__1 : s_rnge("values", i__1,
		 "m2year_", (ftnlen)186)] = 1;
	values[(i__1 = '2') < 256 && 0 <= i__1 ? i__1 : s_rnge("values", i__1,
		 "m2year_", (ftnlen)187)] = 2;
	values[(i__1 = '3') < 256 && 0 <= i__1 ? i__1 : s_rnge("values", i__1,
		 "m2year_", (ftnlen)188)] = 3;
	values[(i__1 = '4') < 256 && 0 <= i__1 ? i__1 : s_rnge("values", i__1,
		 "m2year_", (ftnlen)189)] = 4;
	values[(i__1 = '5') < 256 && 0 <= i__1 ? i__1 : s_rnge("values", i__1,
		 "m2year_", (ftnlen)190)] = 5;
	values[(i__1 = '6') < 256 && 0 <= i__1 ? i__1 : s_rnge("values", i__1,
		 "m2year_", (ftnlen)191)] = 6;
	values[(i__1 = '7') < 256 && 0 <= i__1 ? i__1 : s_rnge("values", i__1,
		 "m2year_", (ftnlen)192)] = 7;
	values[(i__1 = '8') < 256 && 0 <= i__1 ? i__1 : s_rnge("values", i__1,
		 "m2year_", (ftnlen)193)] = 8;
	values[(i__1 = '9') < 256 && 0 <= i__1 ? i__1 : s_rnge("values", i__1,
		 "m2year_", (ftnlen)194)] = 9;
    }

/*     Make sure the string has the right length. */

    i1 = ltrim_(word, word_len);
    i4 = qrtrim_(word, word_len);
    length = i4 - i1 + 1;

/*     Rule out the goofy cases that NPARSD will allow. */

    if (length != 4) {
	value = 10000;
    } else {
	i2 = i1 + 1;
	i3 = i2 + 1;
	value = values[(i__1 = *(unsigned char *)&word[i1 - 1]) < 256 && 0 <= 
		i__1 ? i__1 : s_rnge("values", i__1, "m2year_", (ftnlen)218)] 
		* 1000 + values[(i__2 = *(unsigned char *)&word[i2 - 1]) < 
		256 && 0 <= i__2 ? i__2 : s_rnge("values", i__2, "m2year_", (
		ftnlen)218)] * 100 + values[(i__3 = *(unsigned char *)&word[
		i3 - 1]) < 256 && 0 <= i__3 ? i__3 : s_rnge("values", i__3, 
		"m2year_", (ftnlen)218)] * 10 + values[(i__4 = *(unsigned 
		char *)&word[i4 - 1]) < 256 && 0 <= i__4 ? i__4 : s_rnge(
		"values", i__4, "m2year_", (ftnlen)218)];
    }

/*     That's all just make sure that the value is within the */
/*     bound required of a year. */

    ret_val = value >= 1000 && value <= 3000;
    return ret_val;
} /* m2year_ */

