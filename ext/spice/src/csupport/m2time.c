/* m2time.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      M2TIME ( Determine whether or not a word is a time ) */
logical m2time_(char *word, ftnlen word_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    integer i__1, i__2;
    logical ret_val;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    static integer ubnd[4], comp, zero, i__, n, class__[256];
    static logical colok[4];
    static integer limit[4];
    extern integer ltrim_(char *, ftnlen);
    static integer count;
    static logical pntok[4];
    static integer start, factor[4];
    extern integer qrtrim_(char *, ftnlen);
    static integer end;

/* $ Abstract */

/*     This function is true if the input string is a time in the */
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

/*     The function is returned as .TRUE. if word is an META/2 time. */

/* $ Detailed_Input */

/*     WORD      is a character string that is assumed to have no */
/*               spaces between the first and last non-blank characters. */

/* $ Detailed_Output */

/*     M2TIME    returns as .TRUE. if WORD has the form */

/*                    hh:mm:ss.ssssss */

/*               where */

/*                    hh     stands for one or two digits and the number */
/*                           they represent is less than 24. */

/*                    mm     stands for one or two digits and the number */
/*                           they represent is less than 60 */

/*                    ss.ss  stands for a decimal number less than 61. */

/*               Otherwise M2TIME is returned .FALSE. */

/* $ Error_Handling */

/*     None. */
/* C */
/* $ Input_Files */

/*     None. */

/* $ Output_Files */

/*     None. */

/* $ Particulars */

/*     This is a utility routine for the subroutine META2.  It */
/*     determines whether or not a word is a time in the sense */
/*     of the language META/2. */

/* $ Examples */

/*     WORD                                  M2TIME */
/*     -------                               ------ */
/*     SPAM                                  .FALSE. */
/*     _SPUD                                 .FALSE. */
/*     1:23:27                               .TRUE. */
/*     21.23.28                              .FALSE. */
/*     24:13:48.28                           .FALSE. */
/*     23:59:60.281                          .TRUE. */
/*     19:3:1                                .TRUE. */

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

    if (first) {
	first = FALSE_;
	for (i__ = 0; i__ <= 255; ++i__) {
	    class__[(i__1 = i__) < 256 && 0 <= i__1 ? i__1 : s_rnge("class", 
		    i__1, "m2time_", (ftnlen)190)] = 4;
	}
	class__[(i__1 = '0') < 256 && 0 <= i__1 ? i__1 : s_rnge("class", i__1,
		 "m2time_", (ftnlen)193)] = 1;
	class__[(i__1 = '1') < 256 && 0 <= i__1 ? i__1 : s_rnge("class", i__1,
		 "m2time_", (ftnlen)194)] = 1;
	class__[(i__1 = '2') < 256 && 0 <= i__1 ? i__1 : s_rnge("class", i__1,
		 "m2time_", (ftnlen)195)] = 1;
	class__[(i__1 = '3') < 256 && 0 <= i__1 ? i__1 : s_rnge("class", i__1,
		 "m2time_", (ftnlen)196)] = 1;
	class__[(i__1 = '4') < 256 && 0 <= i__1 ? i__1 : s_rnge("class", i__1,
		 "m2time_", (ftnlen)197)] = 1;
	class__[(i__1 = '5') < 256 && 0 <= i__1 ? i__1 : s_rnge("class", i__1,
		 "m2time_", (ftnlen)198)] = 1;
	class__[(i__1 = '6') < 256 && 0 <= i__1 ? i__1 : s_rnge("class", i__1,
		 "m2time_", (ftnlen)199)] = 1;
	class__[(i__1 = '7') < 256 && 0 <= i__1 ? i__1 : s_rnge("class", i__1,
		 "m2time_", (ftnlen)200)] = 1;
	class__[(i__1 = '8') < 256 && 0 <= i__1 ? i__1 : s_rnge("class", i__1,
		 "m2time_", (ftnlen)201)] = 1;
	class__[(i__1 = '9') < 256 && 0 <= i__1 ? i__1 : s_rnge("class", i__1,
		 "m2time_", (ftnlen)202)] = 1;
	class__[(i__1 = ':') < 256 && 0 <= i__1 ? i__1 : s_rnge("class", i__1,
		 "m2time_", (ftnlen)203)] = 2;
	class__[(i__1 = '.') < 256 && 0 <= i__1 ? i__1 : s_rnge("class", i__1,
		 "m2time_", (ftnlen)204)] = 3;

/*        The following are the maximum values that are allowed */
/*        for each of the various components of the time string */

	ubnd[0] = 23;
	ubnd[1] = 59;
	ubnd[2] = 60;
	ubnd[3] = 10;

/*        The following are the maximum number of digits that */
/*        are allowed for each of the components of the time */

	limit[0] = 2;
	limit[1] = 2;
	limit[2] = 2;
	limit[3] = 100;

/*        The following logicals indicate whether or not it is */
/*        ok to end the N'th component of time with a colon. */

	colok[0] = TRUE_;
	colok[1] = TRUE_;
	colok[2] = FALSE_;
	colok[3] = FALSE_;

/*        The following logicals indicate whether or not it is */
/*        ok to end the N'th component of time with a decimal point. */

	pntok[0] = FALSE_;
	pntok[1] = FALSE_;
	pntok[2] = TRUE_;
	pntok[3] = FALSE_;

/*        The following are the factors used to construct the */
/*        integer value of a component COMP = FACTOR*COMP + Next digit. */
/*        Note that for the decimal portion of seconds we don't */
/*        really compute the value of the decimal part.  The */
/*        factor term just ensures that the loop below doesn't */
/*        have any special cases. */

	factor[0] = 10;
	factor[1] = 10;
	factor[2] = 10;
	factor[3] = 0;
	zero = '0';
    }
    start = ltrim_(word, word_len);
    end = qrtrim_(word, word_len);
    comp = 0;
    n = 1;
    count = 0;
    i__ = start;
    ret_val = TRUE_;
    if (end - start < 4) {
	ret_val = FALSE_;
	return ret_val;
    }
    while(i__ <= end && ret_val) {

/*        If the next character is a digit, compute the accumulated */
/*        value of this component of the time.  Then check to */
/*        make sure that we don't have too many digits so far */
/*        in this component and that the value of this component */
/*        does not exceed the limits for this component. */

	if (class__[(i__1 = *(unsigned char *)&word[i__ - 1]) < 256 && 0 <= 
		i__1 ? i__1 : s_rnge("class", i__1, "m2time_", (ftnlen)277)] 
		== 1) {
	    ++count;
	    comp = factor[(i__1 = n - 1) < 4 && 0 <= i__1 ? i__1 : s_rnge(
		    "factor", i__1, "m2time_", (ftnlen)280)] * comp + *(
		    unsigned char *)&word[i__ - 1] - zero;
	    ret_val = count <= limit[(i__1 = n - 1) < 4 && 0 <= i__1 ? i__1 : 
		    s_rnge("limit", i__1, "m2time_", (ftnlen)282)] && comp <= 
		    ubnd[(i__2 = n - 1) < 4 && 0 <= i__2 ? i__2 : s_rnge(
		    "ubnd", i__2, "m2time_", (ftnlen)282)];

/*        If the next character is a colon ':' then we are starting */
/*        a new component.  Make sure this is ok and that we actually */
/*        had a digit or two for the last component.  Increment the */
/*        component counter, set the number of characters found in */
/*        the next component to 0 and set the value of the next */
/*        component to zero. */

	} else if (class__[(i__1 = *(unsigned char *)&word[i__ - 1]) < 256 && 
		0 <= i__1 ? i__1 : s_rnge("class", i__1, "m2time_", (ftnlen)
		293)] == 2) {
	    ret_val = colok[(i__1 = n - 1) < 4 && 0 <= i__1 ? i__1 : s_rnge(
		    "colok", i__1, "m2time_", (ftnlen)295)] && count > 0;
	    count = 0;
	    comp = 0;
	    ++n;

/*        If the next character is decimal point, we are ending a */
/*        component and starting it's decimal portion.  Make sure */
/*        that a decimal point is allowed for this component and */
/*        that we had at least one digit in the component we were */
/*        examining up to this point. */

	} else if (class__[(i__1 = *(unsigned char *)&word[i__ - 1]) < 256 && 
		0 <= i__1 ? i__1 : s_rnge("class", i__1, "m2time_", (ftnlen)
		306)] == 3) {
	    ret_val = pntok[(i__1 = n - 1) < 4 && 0 <= i__1 ? i__1 : s_rnge(
		    "pntok", i__1, "m2time_", (ftnlen)308)] && count > 0;
	    count = 0;
	    comp = 0;
	    ++n;

/*        If we hit some other character we don't have a time */
/*        word. */

	} else {
	    ret_val = FALSE_;
	}
	++i__;
    }
    ret_val = ret_val && n >= 3;
    return ret_val;
} /* m2time_ */

