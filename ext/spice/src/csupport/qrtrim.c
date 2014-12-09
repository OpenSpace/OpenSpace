/* qrtrim.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      QRTRIM (Quick right trim ) */
integer qrtrim_(char *string, ftnlen string_len)
{
    /* System generated locals */
    integer ret_val, i__1;

    /* Builtin functions */
    integer i_len(char *, ftnlen), s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer b, i__, l, m, blank, nl;

/* $ Abstract */

/*     This is a "faster" version of the SPICELIB routine RTRIM. */

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

/*      ASCII,  CHARACTER,  SEARCH */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */

/*     STRING     I   String to be trimmed. */

/*     The function returns the maximum of 1 and the location of the */
/*     last non-blank character in STRING. */

/* $ Detailed_Input */

/*     STRING         is a string to be trimmed:  the location of the */
/*                    last non-blank character is desired. */

/* $ Detailed_Output */

/*     The function returns the maximum of 1 and the location of the */
/*     last non-blank character in STRING. */

/*     In particular, when STRING is blank, the function returns the */
/*     value 1. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Particulars */

/*     When writing a character string to a file, we usually are content */
/*     to omit the trailing blanks.  We'd like to use LASTNB as an upper */
/*     substring bound, but we have to handle the case where LASTNB */
/*     returns 0, so we write: */


/*        WRITE ( UNIT, '(A)' ),  STRING ( : MAX (1, LASTNB (STRING)) ) */


/*     This can be simplified using QRTRIM: */


/*        WRITE ( UNIT, '(A)' ),  STRING ( : QRTRIM (STRING) )  ) */

/*      This routine has the same function as the SPICE routine */
/*      RTRIM however, it turns out to be substantially faster */
/*      when applied to long strings.  This is somewhat surprising */
/*      but happens due to a combination of machine instructions */
/*      available for comparing strings and the ineffective optimizations */
/*      performed by all compilers we've examined.  See the code */
/*      for more details regarding how this routine takes advantage */
/*      of native instructions and ineffective optimizations. */

/* $ Examples */

/*     1)  Write the non-blank portion of each element of a character */
/*         cell to file SPUD.DAT: */

/*            DO I = 1,  CARDC (CELL) */

/*               CALL WRLINE ('SPUD.DAT', */
/*           .                 CELL(I) ( LTRIM (CELL) : QRTRIM (CELL) ) ) */

/*            END DO */

/*         When CELL(I) is blank, the string ' ' will be written. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*      W.L. Taber      (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 9, 1994 */


/* -     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of META/2 */
/*         software as of May 3, 1994 */


/* -    SPICELIB Version 1.0.0, 22-APR-1994 (WLT) */


/* -& */
/* $ Index_Entries */

/*     Get the index of the last non-blank character of a string. */
/*     Right trim a string */

/* -& */
    blank = ' ';
    l = i_len(string, string_len);

/*     If this is a short string there is no particular advantage */
/*     to be gained by making use of the binary search idea. */
/*     The speed up just doesn't buy much when compared with */
/*     the loop overhead. */

    if (l <= 32) {
	for (i__ = l; i__ >= 1; --i__) {
	    if (*(unsigned char *)&string[i__ - 1] != blank) {
		ret_val = i__;
		return ret_val;
	    }
	}
	ret_val = 1;
	return ret_val;
    }
    b = 1;
    nl = l - 1;

/*     We want M to be ( B + NL ) / 2   but right now that's L/2 */

    m = l / 2;
    while(l - b > 16) {

/*         What is true right now?  The string from L+1 on out */
/*         is blank.  L > B; L-1 = NL >= B;  M = (B + NL) / 2; */
/*         and M >= B,  B is at least one and if greater than 1 */
/*         there must be a non-blank character between B and the */
/*         end of the string. */

	if (*(unsigned char *)&string[l - 1] != blank) {
	    ret_val = l;
	    return ret_val;
	} else if (*(unsigned char *)&string[m - 1] != blank) {
	    l = nl;
	    b = m;
	} else /* if(complicated condition) */ {
	    i__1 = m;
	    if (s_cmp(string + i__1, " ", nl - i__1, (ftnlen)1) == 0) {

/*            If you got here, the STRING(L:L) is a blank. */
/*            The string from L+1 on out is blank. */
/*            The string from M to NL (=L-1) is blank.  Thus the */
/*            string from M out is blank. */

/*            M is greater than or equal to B */
/*            If M  is less than B + 2, then L will become */
/*            B or less and there will not be a */
/*            next pass through the loop.  That means that */
/*            we will never get to this point again and don't */
/*            have to worry about the reference STRING(M:NL) */
/*            giving us an access violation. */

		l = m - 1;

/*            With the new value of L, we now know that STRING(L+1:) */
/*            is blank. */

	    } else {

/*            If you get to this point all of the string from */
/*            L out is blank and L is greater than M. */
/*            There is a non-blank character between M+1 and NL. */
/*            If L should become equal to B below, then the loop */
/*            will not be executed again.  That means again that */
/*            we don't have to worry about STRING(M:NL) being */
/*            an ill formed string. */

		l = nl;
		b = m + 1;

/*            With the new value of L, we now know that STRING(L+1:) */
/*            is blank. */

	    }
	}
	nl = l - 1;
	m = (b + nl) / 2;

/*         What's true now?  The string from L+1 on out is blank. */
/*         Somewhere between B and L is a non-blank character. */

    }

/*      Either B never changed from 1 or B was set to a value such that */
/*      there was a non-blank character between B and the end of */
/*      the string,  And the string from L+1 out to the end is */
/*      blank.  Since we want this to mimick RTRIM, we are done. */

    for (i__ = l; i__ >= 1; --i__) {
	if (*(unsigned char *)&string[i__ - 1] != blank) {
	    ret_val = i__;
	    return ret_val;
	}
    }
    ret_val = 1;
    return ret_val;
} /* qrtrim_ */

