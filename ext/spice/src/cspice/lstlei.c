/* lstlei.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure   LSTLEI ( Last integer element less than or equal to ) */
integer lstlei_(integer *x, integer *n, integer *array)
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    integer j, begin, items, middle, end;

/* $ Abstract */

/*      Given a number X and an array of non-decreasing numbers, */
/*      find the index of the largest array element less than or equal */
/*      to X. */

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
/*      X          I   Value to search against. */
/*      ARRAY      I   Array of possible lower bounds. */
/*      N          I   Number elements in ARRAY. */
/*      LSTLEI     O   the index of the last element of ARRAY <= X. */

/* $ Detailed_Input */

/*      X       Integer for which one desires to find */
/*              the last ARRAY element less than or equal to X. */

/*      ARRAY   Array of integers that forms a */
/*              non-decreasing sequence.  We will find the last element */
/*              of the sequence that is less than or equal to X. */

/*      N       Total number of elements in ARRAY */

/* $ Detailed_Output */

/*      LSTLEI  Index of the last element of the non-decreasing sequence: */
/*              {ARRAY(I) : 0 < I < N + 1} that is less than or equal */
/*              to X. (Note that LSTLEI = I for some I in the range 1 to */
/*              N  unless X is less than all of these elements in which */
/*              case LSTLEI = 0.) */

/*              In the case that N is input with value less than or equal */
/*              to zero, LSTLEI is returned as zero. */

/* $ Parameters */

/*      None. */

/* $ Particulars */


/*      An array of integers is given.  The array */
/*      ARRAY(I) (0 < I < N ) forms a non-decreasing sequence of */
/*      numbers.  Given a real number X, there will be a last one of */
/*      these numbers that is less than or equal to X.  This routine */
/*      finds the index LSTLEI such that ARRAY(LSTLEI) is that number. */

/*      If X is not greater than ARRAY(1), INDEX will be set to zero. */

/*      This routine uses a binary search algorithm and so requires */
/*      at most LOG_2(N) steps to find the value of LSTLEI. */

/*      Note:  If you need to find the first element of the array that */
/*             is greater than X, simply add 1 to the result returned */
/*             by this function and check to see if the result is */
/*             within the array bounds given by N. */

/* $ Examples */

/*     Suppose that you have an reasonably large ordered array of */
/*     integers, into which you want to insert a few more without */
/*     destroying the ordering. */

/*     Depending upon your application, it may be desirable to */
/*     not insert duplicates, to insert duplicates before */
/*     existing entries or to insert them after existing entries. */

/*     The code fragment below, illustrates an insertion scheme */
/*     that will insert duplicate items after existing items */
/*     and simultaneously update a second parallel array of */
/*     double precision numbers. */

/*           get the pair to insert */

/*           READ (*,*) KEY, VALUE */

/*           locate the place to insert the new KEY into the sorted */
/*           array of keys. */

/*           LOC = LSTLEI ( KEY, NKEYS, KEYS ) + 1 */

/*           insert the key and its associated value into the */
/*           KEYS and  VALUES arrays at location LOC */

/*           CALL INSLAI ( KEY,   1, LOC, NKEYS, KEYS   ) */
/*           CALL INSLAD ( VALUE, 1, LOC, NVALS, VALUES ) */

/*     If at the READ statement the arrays KEYS and VALUES looked like: */

/*           KEYS     VALUES     NKEYS = 6, NVALS = 6 */
/*           ----     ------- */
/*             2       3.00D0 */
/*             5       1.00D0 */
/*             7       3.14D0 */
/*            16       7.11D0 */
/*            18       2.14D0 */
/*            23      12.12D0 */

/*     and 9 and 33.33D3 were read into KEY and VALUE respectively */
/*     then LSTLEI (KEY, NKEYS, KEYS ) would be 3 and LOC would be 4. */
/*     After the calls to the routines INSLAI and INSLAD we would have */

/*           KEYS     VALUES     NKEYS = 7, NVALS = 7 */
/*           ----     ------- */
/*             2       3.00D0 */
/*             5       1.00D0 */
/*             7       3.14D0 */
/*             9      33.33D3     <===== inserted items. */
/*            16       7.11D0 */
/*            18       2.14D0 */
/*            23      12.12D0 */

/*     If 7 and 33.33D3 were read into KEY and VALUE respectively */
/*     then again LSTLEI (KEY, NKEYS, KEYS ) would be 3 and LOC would */
/*     be 4. After the calls to the routines INSLAI and INSLAD we */
/*     would have: */

/*           KEYS     VALUES     NKEYS = 7, NVALS = 7 */
/*           ----     ------- */
/*             2       3.00D0 */
/*             5       1.00D0 */
/*             7       3.14D0 */
/*             7      33.33D3     <===== inserted items. */
/*            16       7.11D0 */
/*            18       2.14D0 */
/*            23      12.12D0 */

/*     If we replaced the line of code */

/*           LOC = LSTLEI ( KEY, NKEYS, KEYS ) + 1 */
/*     by */

/*           LOC = LSTLTI ( KEY, NKEYS, KEYS ) + 1 */

/*     we would obtain a routine that inserted duplicates before */
/*     existing entries. (LSTLTI is similar to LSTLEI except it finds */
/*     the last occurrance of an integer strictly less than a value.) */
/*     Using 7 and 33.33D3 for KEY and VALUE again, the modified code */
/*     fragment would yield the results shown below. */

/*           KEYS     VALUES     NKEYS = 7, NVALS = 7 */
/*           ----     ------- */
/*             2       3.00D0 */
/*             5       1.00D0 */
/*             7      33.33D3     <===== inserted items. */
/*             7       3.14D0 */
/*            16       7.11D0 */
/*            18       2.14D0 */
/*            23      12.12D0 */


/*     (Note: you should NOT use the */
/*     code outlined above as the basis of a sorting algorithm. */
/*     The NAIF routines SHELLI, SHELLD, SHELLC, ORDERI, ORDERD, ORDERC, */
/*     REORDI, REORDD and REORDC are much more efficient routines for */
/*     sorting arrays or sorting a set of parallel arrays using */
/*     one of the set as a key. The fragment presented here is useful */
/*     for performing update insertions into previously ordered arrays.) */

/*     For more ideas regarding the use of this routine, see LSTLEC */
/*     and LSTLTC. */

/* $ Restrictions */

/*      If the sequence does not non-decreasing, the program will run */
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

/*     last integer element less_than_or_equal_to */

/* -& */
/* $ Revisions */

/* -    Beta Version 1.1.0, 9-MAR-1989 (HAN) */

/*        Declaration of the variable I was removed from the code. The */
/*        variable was declared but not used. */

/* -     Beta Version 1.0.1, 1-Feb-1989 (WLT) */

/*      Example section of header upgraded. */

/* -& */

/*     Local variables */

    items = *n;
    begin = 1;
    end = *n;
    if (*n <= 0) {

/*        There's nobody home---that is there is nothing in the array */
/*        to compare against.  Zero is the only sensible thing to return. */

	ret_val = 0;
    } else if (*x < array[begin - 1]) {

/*        None of the array elements are less than or equal to X */

	ret_val = 0;
    } else if (*x >= array[end - 1]) {

/*        X is greater than or equal to all elements of the array.  Thus */
/*        the last element of the array is the last item less than or */
/*        equal to X. */

	ret_val = end;
    } else {

/*        X lies between some pair of elements of the array */

	while(items > 2) {
	    j = items / 2;
	    middle = begin + j;
	    if (array[middle - 1] <= *x) {
		begin = middle;
	    } else {
		end = middle;
	    }
	    items = end - begin + 1;
	}
	ret_val = begin;
    }
    return ret_val;
} /* lstlei_ */

