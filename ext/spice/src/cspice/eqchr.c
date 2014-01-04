/* eqchr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      EQCHR (Equivalent characters) */
logical eqchr_0_(int n__, char *a, char *b, ftnlen a_len, ftnlen b_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    integer i__1, i__2;
    logical ret_val;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    static integer i__, j, uvalue[256];

/* $ Abstract */

/*     This function determines whether two characters are */
/*     equivalent when the case of the characters is ignored. */

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

/*      None. */

/* $ Keywords */

/*       CHARACTER */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     A          I   one of the characters to check */
/*     B          I   the other character to check */

/*     The function returns .TRUE. if the characters are equivalent */

/* $ Detailed_Input */

/*     A           are two characters that are to be compared to see */
/*     B           if they are the same letter (although possibly */
/*                 having different case such as 'a' and 'A') */

/* $ Detailed_Output */

/*     The function returns the value .TRUE. if the two input characters */
/*     are the same or can be made the same by converting both to */
/*     upper or lower case. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Particulars */

/*     This is a utility routine for comparing two characters to */
/*     see if they are the same when converted to upper case.  It */
/*     is particularly useful when writing string analysis routines */
/*     that should be case insensitive.  Instead of writing the */
/*     expression */

/*        A .EQ. B */

/*     use the expression */

/*        EQCHR ( A, B ) */

/*     in all tests of equivalence for characters. */

/* $ Examples */

/*     Suppose you want to determine whether or not two strings */
/*     are the same if differences in the case of letters are ignored. */
/*     The following code fragment shows how you can use this routine */
/*     to check for the equivalence of character strings. */

/*        MORE  = .TRUE. */
/*        SAME  = .TRUE. */
/*        L1    =  LEN(STR1) */
/*        L2    =  LEN(STR2) */
/*        CHECK = MIN ( L1, L2 ) */

/*        DO WHILE ( SAME .AND. MORE ) */

/*           SAME = EQCHR( STR1(I:I), STR2(I:I) ) */
/*           I    = I + 1 */
/*           MORE = I .LT. CHECK */

/*        END DO */

/*        IF ( .NOT. SAME ) THEN */

/*           There's nothing to do, we already know the strings */
/*           are not the same. */

/*        ELSE IF ( L1 .LT. L2 ) THEN */

/*           The only way the strings can be regarded as being equal */
/*           is if the extra unchecked characters in STR2 are all blank. */

/*           SAME = STR2(I:) .EQ. ' ' */

/*        ELSE */

/*           Same test as previous one but with STR1 this time. */

/*           SAME = STR1(I:) .EQ. ' ' */

/*        END IF */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */
/*     E.D. Wright     (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -     Spicelib Version 2.0.0, 17-SEP-1998 (EDW) */

/*        Replace the UVALUE data statement with a loop to fill */
/*        UVALUE.  The Absoft Mac compiler failed to compile the */
/*        data statement correctly, and so this function failed */
/*        to work properly in all situations on the Mac.  The */
/*        corrects the problem and functions on all platforms. */

/* -    SPICELIB Version 1.0.0, 16-MAY-1995 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Check two characters for case insensitive equality */

/* -& */

/*     Entry points. */


/*     Range of characters */


/*     Local Variables */

/*     The array UVALUE contains the ICHAR values for the upper case */
/*     version of each character. */

    switch(n__) {
	case 1: goto L_nechr;
	}


/*     The first time through the loop we set the upper case values */
/*     for each of the lower case letters. */

    if (first) {
	for (i__ = 0; i__ <= 255; ++i__) {
	    uvalue[(i__1 = i__) < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		    i__1, "eqchr_", (ftnlen)206)] = i__;
	}
	first = FALSE_;
	uvalue[(i__1 = 'a') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)211)] = 'A';
	uvalue[(i__1 = 'b') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)212)] = 'B';
	uvalue[(i__1 = 'c') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)213)] = 'C';
	uvalue[(i__1 = 'd') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)214)] = 'D';
	uvalue[(i__1 = 'e') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)215)] = 'E';
	uvalue[(i__1 = 'f') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)216)] = 'F';
	uvalue[(i__1 = 'g') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)217)] = 'G';
	uvalue[(i__1 = 'h') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)218)] = 'H';
	uvalue[(i__1 = 'i') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)219)] = 'I';
	uvalue[(i__1 = 'j') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)220)] = 'J';
	uvalue[(i__1 = 'k') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)221)] = 'K';
	uvalue[(i__1 = 'l') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)222)] = 'L';
	uvalue[(i__1 = 'm') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)223)] = 'M';
	uvalue[(i__1 = 'n') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)224)] = 'N';
	uvalue[(i__1 = 'o') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)225)] = 'O';
	uvalue[(i__1 = 'p') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)226)] = 'P';
	uvalue[(i__1 = 'q') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)227)] = 'Q';
	uvalue[(i__1 = 'r') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)228)] = 'R';
	uvalue[(i__1 = 's') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)229)] = 'S';
	uvalue[(i__1 = 't') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)230)] = 'T';
	uvalue[(i__1 = 'u') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)231)] = 'U';
	uvalue[(i__1 = 'v') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)232)] = 'V';
	uvalue[(i__1 = 'w') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)233)] = 'W';
	uvalue[(i__1 = 'x') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)234)] = 'X';
	uvalue[(i__1 = 'y') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)235)] = 'Y';
	uvalue[(i__1 = 'z') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)236)] = 'Z';
    }
    i__ = *(unsigned char *)a;
    j = *(unsigned char *)b;
    if (i__ > 255 || j > 255) {
	ret_val = i__ == j;
    } else {
	ret_val = uvalue[(i__1 = i__) < 256 && 0 <= i__1 ? i__1 : s_rnge(
		"uvalue", i__1, "eqchr_", (ftnlen)246)] == uvalue[(i__2 = j) <
		 256 && 0 <= i__2 ? i__2 : s_rnge("uvalue", i__2, "eqchr_", (
		ftnlen)246)];
    }
    return ret_val;
/* $Procedure      NECHR (Not Equivalent characters) */

L_nechr:
/* $ Abstract */

/*     This function determines whether two characters are */
/*     not equivalent if the case of the characters is ignored. */

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

/*      None. */

/* $ Keywords */

/*       CHARACTER */

/* $ Declarations */

/*     CHARACTER*(1)         A */
/*     CHARACTER*(1)         B */

/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      A          I   one of the characters to check */
/*      B          I   the other character to check */

/*      The function returns .TRUE. if the characters are not equivalent */

/* $ Detailed_Input */

/*     A           are two characters that are to be compared to see */
/*     B           if they are different letters. Letters that have */
/*                 the same value when converted to uppercase are */
/*                 considered to be equivalent. */

/* $ Detailed_Output */

/*     The function returns the value .FALSE. if the two input characters */
/*     are the same or can be made the same by converting both to */
/*     upper or lower case.  Otherwise it returns .TRUE. */

/* $ Parameters */

/*      None. */

/* $ Files */

/*      None. */

/* $ Exceptions */

/*     Error free. */

/* $ Particulars */

/*     This routine simply determines the truth value of .NOT. EQCHR. */
/*     See the entry point EQCHR for a discussion of that function. */

/* $ Examples */

/*     Suppose you want to determine whether or not two strings */
/*     are the same up to differences in case.  The following */
/*     code fragment shows how you can use this routine to check */
/*     for the equivalence of character strings. */

/*        MORE  = .TRUE. */
/*        SAME  = .TRUE. */
/*        L1    =  LEN(STR1) */
/*        L2    =  LEN(STR2) */
/*        CHECK = MIN ( L1, L2 ) */

/*        DO WHILE ( SAME .AND. MORE ) */

/*           IF ( NECHR(STR1(I:I),STR2(I:I) ) THEN */
/*              SAME = .FALSE. */
/*           END IF */

/*           I    = I + 1 */
/*           MORE = I .LT. CHECK */

/*        END DO */

/*        IF ( .NOT. SAME ) THEN */

/*           There's nothing to do, we already know the strings */
/*           are not the same. */

/*        ELSE IF ( L1 .LT. L2 ) THEN */

/*           The only way the strings can be regarded as being equal */
/*           is if the extra unchecked characters in STR2 are all blank. */

/*           SAME = STR2(I:) .EQ. ' ' */

/*        ELSE */

/*           Same test as previous one but with STR1 this time. */

/*           SAME = STR1(I:) .EQ. ' ' */

/*        END IF */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*      W.L. Taber      (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -     Spicelib Version 2.0.0, 17-SEP-1998 (EDW) */

/*        Replace the UVALUE data statement with a loop to fill */
/*        UVALUE.  The Absoft Mac compiler failed to compile the */
/*        data statement correctly, and so this function failed */
/*        to work properly in all situations on the Mac.  The */
/*        corrects the problem and functions on all platforms. */

/* -     Spicelib Version 1.0.0, 16-MAY-1995 */

/* -& */
/* $ Index_Entries */

/*     Check two characters for case insensitive not equal */

/* -& */
    if (first) {
	first = FALSE_;
	for (i__ = 0; i__ <= 255; ++i__) {
	    uvalue[(i__1 = i__) < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		    i__1, "eqchr_", (ftnlen)421)] = i__;
	}
	uvalue[(i__1 = 'a') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)424)] = 'A';
	uvalue[(i__1 = 'b') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)425)] = 'B';
	uvalue[(i__1 = 'c') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)426)] = 'C';
	uvalue[(i__1 = 'd') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)427)] = 'D';
	uvalue[(i__1 = 'e') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)428)] = 'E';
	uvalue[(i__1 = 'f') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)429)] = 'F';
	uvalue[(i__1 = 'g') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)430)] = 'G';
	uvalue[(i__1 = 'h') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)431)] = 'H';
	uvalue[(i__1 = 'i') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)432)] = 'I';
	uvalue[(i__1 = 'j') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)433)] = 'J';
	uvalue[(i__1 = 'k') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)434)] = 'K';
	uvalue[(i__1 = 'l') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)435)] = 'L';
	uvalue[(i__1 = 'm') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)436)] = 'M';
	uvalue[(i__1 = 'n') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)437)] = 'N';
	uvalue[(i__1 = 'o') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)438)] = 'O';
	uvalue[(i__1 = 'p') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)439)] = 'P';
	uvalue[(i__1 = 'q') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)440)] = 'Q';
	uvalue[(i__1 = 'r') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)441)] = 'R';
	uvalue[(i__1 = 's') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)442)] = 'S';
	uvalue[(i__1 = 't') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)443)] = 'T';
	uvalue[(i__1 = 'u') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)444)] = 'U';
	uvalue[(i__1 = 'v') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)445)] = 'V';
	uvalue[(i__1 = 'w') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)446)] = 'W';
	uvalue[(i__1 = 'x') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)447)] = 'X';
	uvalue[(i__1 = 'y') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)448)] = 'Y';
	uvalue[(i__1 = 'z') < 256 && 0 <= i__1 ? i__1 : s_rnge("uvalue", i__1,
		 "eqchr_", (ftnlen)449)] = 'Z';
    }
    i__ = *(unsigned char *)a;
    j = *(unsigned char *)b;
    if (i__ > 255 || j > 255) {
	ret_val = i__ != j;
    } else {
	ret_val = uvalue[(i__1 = i__) < 256 && 0 <= i__1 ? i__1 : s_rnge(
		"uvalue", i__1, "eqchr_", (ftnlen)459)] != uvalue[(i__2 = j) <
		 256 && 0 <= i__2 ? i__2 : s_rnge("uvalue", i__2, "eqchr_", (
		ftnlen)459)];
    }
    return ret_val;
} /* eqchr_ */

logical eqchr_(char *a, char *b, ftnlen a_len, ftnlen b_len)
{
    return eqchr_0_(0, a, b, a_len, b_len);
    }

logical nechr_(char *a, char *b, ftnlen a_len, ftnlen b_len)
{
    return eqchr_0_(1, a, b, a_len, b_len);
    }

