/* bschoc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure BSCHOC ( Binary search with order vector, character ) */
integer bschoc_(char *value, integer *ndim, char *array, integer *order, 
	ftnlen value_len, ftnlen array_len)
{
    /* System generated locals */
    integer ret_val;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    logical l_lt(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer left, i__, right;

/* $ Abstract */

/*      Do a binary search for a given value within a character array, */
/*      accompanied by an order vector. Return the index of the */
/*      matching array entry, or zero if the key value is not found. */

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

/*      ARRAY,  SEARCH */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      VALUE      I   Value to find in ARRAY. */
/*      NDIM       I   Dimension of ARRAY. */
/*      ARRAY      I   Array to be searched. */
/*      ORDER      I   Order vector. */
/*      BSCHOC     O   Index of VALUE in ARRAY. (Zero if not found.) */

/* $ Detailed_Input */

/*      VALUE       is the value to be found in the input array. */

/*      NDIM        is the number of elements in the input array. */

/*      ARRAY       is the array to be searched. */


/*      ORDER       is an order array that can be used to access */
/*                  the elements of ARRAY in order (according to the */
/*                  ASCII collating sequence). */

/* $ Detailed_Output */

/*      BSCHOC      is the index of the input value in the input array. */
/*                  If ARRAY does not contain VALUE, BSCHOC is zero. */

/*                  If ARRAY contains more than one occurrence of VALUE, */
/*                  BSCHOC may point to any of the occurrences. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*      If NDIM < 1 the value of the function is zero. */

/* $ Particulars */

/*      A binary search is implemented on the input array, whose order */
/*      is given by an associated order vector. If an element of the */
/*      array is found to match the input value, the index of that */
/*      element is returned. If no matching element is found, zero is */
/*      returned. */

/* $ Examples */

/*      Let ARRAY and ORDER contain the following elements: */

/*            ARRAY         ORDER */
/*            -----------   ----- */
/*            'FEYNMAN'         2 */
/*            'BOHR'            3 */
/*            'EINSTEIN'        1 */
/*            'NEWTON'          5 */
/*            'GALILEO'         4 */

/*      Then */

/*            BSCHOC  ( 'NEWTON',   5, ARRAY, ORDER )  = 4 */
/*            BSCHOC  ( 'EINSTEIN', 5, ARRAY, ORDER )  = 3 */
/*            BSCHOC  ( 'GALILEO',  5, ARRAY, ORDER )  = 5 */
/*            BSCHOC  ( 'Galileo',  5, ARRAY, ORDER )  = 0 */
/*            BSCHOC  ( 'BETHE',    5, ARRAY, ORDER )  = 0 */

/*       That is */

/*            ARRAY(4) = 'NEWTON' */
/*            ARRAY(3) = 'EINSTEIN' */
/*            ARRAY(5) = 'GALILEO' */

/*      (Compare with BSCHOC_2.) */

/* $ Restrictions */

/*      ORDER is assumed to give the order of the elements of ARRAY */
/*      in increasing order according to the ASCII collating sequence. */
/*      If this condition is not met, the results of BSCHOC are */
/*      unpredictable. */

/* $ Author_and_Institution */

/*      I. M. Underwood */
/*      W. L. Taber */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 18-SEP-1995 (IMU) (WLT) */

/* -& */
/* $ Index_Entries */

/*     binary search for a string using an order vector */

/* -& */

/*     Local variables */


/*     Set the initial bounds for the search area. */

    left = 1;
    right = *ndim;
    while(left <= right) {

/*        Check the middle element. */

	i__ = (left + right) / 2;

/*        If the middle element matches, return its location. */

	if (s_cmp(value, array + (order[i__ - 1] - 1) * array_len, value_len, 
		array_len) == 0) {
	    ret_val = order[i__ - 1];
	    return ret_val;

/*        Otherwise narrow the search area. */

	} else if (l_lt(value, array + (order[i__ - 1] - 1) * array_len, 
		value_len, array_len)) {
	    right = i__ - 1;
	} else {
	    left = i__ + 1;
	}
    }

/*     If the search area is empty, return zero. */

    ret_val = 0;
    return ret_val;
} /* bschoc_ */

