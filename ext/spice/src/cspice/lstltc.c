/* lstltc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure   LSTLTC ( Last character element less than ) */
integer lstltc_(char *string, integer *n, char *array, ftnlen string_len, 
	ftnlen array_len)
{
    /* System generated locals */
    integer ret_val;

    /* Builtin functions */
    logical l_le(char *, char *, ftnlen, ftnlen), l_lt(char *, char *, ftnlen,
	     ftnlen);

    /* Local variables */
    integer j, begin, items, middle, end;

/* $ Abstract */

/*      Given a character string and an ordered array of character */
/*      strings, find the index of the largest array element less */
/*      the given string. */

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

/*     None. */

/* $ Keywords */

/*      SEARCH,  ARRAY */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      STRING     I   Value to search against. */
/*      ARRAY      I   Array of possible lower bounds. */
/*      N          I   Number elements in ARRAY. */
/*      LSTLTC     O   the index of the last element of ARRAY < STRING. */

/* $ Detailed_Input */

/*      STRING  Character string for which one desires to find */
/*              the last ARRAY element less than STRING. */

/*      N       Total number of elements in ARRAY. */

/*      ARRAY   Ordered array of character strings. */
/*              We will find the last element */
/*              of the sequence that is less than STRING. */

/* $ Detailed_Output */

/*      LSTLTC  Index of the last element of the ordered array: */
/*              {ARRAY(I) : 0 < I < N + 1} that is less than STRING. */
/*              (Note that LSTLTC = I for some I in the range 1 to */
/*              N  unless STRING is less than or equal to all of these */
/*              elements, in which case LSTLTC = 0.) */

/*              In the case that N is input with value less than or equal */
/*              to zero, LSTLTC is returned as zero. */

/* $ Parameters */

/*      None. */

/* $ Particulars */


/*      An ordered array of character strings is given. */
/*      Given a real number STRING, there will be a last one of */
/*      these that is less than STRING.  This routine */
/*      finds the index LSTLTC such that ARRAY(LSTLTC) is that string. */

/*      If STRING is not greater than ARRAY(1), LSTLTC will be set to */
/*      zero. */

/*      This routine uses a binary search algorithm and so requires */
/*      at most LOG_2(N) steps to find the value of LSTLTI. */

/*      Note:  If you need to find the first element of the array that */
/*             is greater than or equal to STRING, simply add 1 to the */
/*             result returned by this function and check to see if the */
/*             result is within the array bounds given by N. */

/* $ Examples */

/*      Suppose that you have a long list of words, sorted alphabetically */
/*      and entirely in upper case.  Furthermore suppose you wished to */
/*      find all words that begin the sequence of letters PLA,  then */
/*      you could execute the following code. */

/*            START = 0 */
/*            I     = 1 */

/*            DO I = 1, NWORDS */

/*               IF ( WORD(I)(1:3) .EQ. 'PLA' ) THEN */

/*                  IF ( START .EQ. 0 ) THEN */
/*                     START = I */
/*                  END IF */

/*                  END = I */
/*               END IF */

/*            END DO */

/*      This can of course be improved by stopping the loop once START */
/*      is non-zero and END remains unchanged after a pass through the */
/*      loop.  However, this is a linear search  and on average can be */
/*      expected to take NWORDS/2 comparisons.  The above algorithm */
/*      fails to take advantage of the structure of the list of words */
/*      (they are sorted). */

/*      The code below is much simpler to code, simpler to check, and */
/*      much faster than the code above. */

/*            START = LSTLTC( 'PLA', NWORDS, WORDS ) + 1 */
/*            END   = LSTLTC( 'PLB', NWORDS, WORDS ) */

/*            do something in case there are no such words. */

/*            IF ( START .GT. END ) THEN */
/*               START = 0 */
/*               END   = 0 */
/*            END IF */

/*      This code will never exceed 2 * LOG_2 ( NWORDS ) comparisons. */
/*      For a large list of words (say 4096) the second method will */
/*      take 24 comparisons  the first method requires on average */
/*      2048 comparisons.  About 200 times as much time.  Its clear */
/*      that if searches such as this must be performed often, that */
/*      the second approach could make the difference between being */
/*      able to perform the task in a few minutes as opposed to */
/*      several hours. */

/*      For more ideas regarding the use of this routine see LSTLEI */
/*      and LSTLTI. */

/* $ Restrictions */

/*      If the array is not ordered, the program will run */
/*      to completion but the index found will not mean anything. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*      None. */

/* $ Author_and_Institution */

/*      H.A. Neilan     (JPL) */
/*      W.L. Taber      (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT) */

/*        Comment section for permuted index source lines was added */
/*        following the header. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) */

/* -& */
/* $ Index_Entries */

/*     last character element less_than */

/* -& */
/* $ Revisions */

/* -    Beta Version 1.1.0, 9-MAR-1989 (HAN) */

/*        Declaration of the variable I was removed from the code. The */
/*        variable was declared but not used. */

/* -     Beta Version 1.0.1, 1-FEB-1989 (WLT) */

/*      Example section of header upgraded. */

/* -& */

/*     Local variables */

    items = *n;
    begin = 1;
    end = *n;
    if (*n <= 0) {

/*        There's nobody home---that is there is nothing in the array */
/*        to compare against.  Zero is the only sensible thing to return */

	ret_val = 0;
    } else if (l_le(string, array + (begin - 1) * array_len, string_len, 
	    array_len)) {

/*        None of the array elements are less than STRING */

	ret_val = 0;
    } else if (l_lt(array + (end - 1) * array_len, string, array_len, 
	    string_len)) {

/*        STRING is greater than all elements of the array.  Thus the las */
/*        element of the array is the last item less than STRING. */

	ret_val = end;
    } else {

/*        STRING lies between some pair of elements of the array */

	while(items > 2) {
	    j = items / 2;
	    middle = begin + j;
	    if (l_lt(array + (middle - 1) * array_len, string, array_len, 
		    string_len)) {
		begin = middle;
	    } else {
		end = middle;
	    }
	    items = end - begin + 1;
	}
	ret_val = begin;
    }
    return ret_val;
} /* lstltc_ */

