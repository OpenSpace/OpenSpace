/* bschoi.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure BSCHOI ( Binary search with order vector, integer ) */
integer bschoi_(integer *value, integer *ndim, integer *array, integer *order)
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    integer left, i__, right;

/* $ Abstract */

/*      Do a binary search for a given value within an integer array, */
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
/*      BSCHOI     O   Index of VALUE in ARRAY. (Zero if not found.) */

/* $ Detailed_Input */

/*      VALUE       is the value to be found in the input array. */

/*      NDIM        is the number of elements in the input array. */

/*      ARRAY       is the array to be searched. */


/*      ORDER       is an order array that can be used to access */
/*                  the elements of ARRAY in order. */

/* $ Detailed_Output */

/*      BSCHOI      is the index of the input value in the input array. */
/*                  If ARRAY does not contain VALUE, BSCHOI is zero. */

/*                  If ARRAY contains more than one occurrence of VALUE, */
/*                  BSCHOI may point to any of the occurrences. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*      Error free. */

/*      1) If NDIM < 1 the value of the function is zero. */

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
/*              100             2 */
/*                1             3 */
/*               10             1 */
/*            10000             5 */
/*             1000             4 */

/*      Then */

/*            BSCHOI (  1000, 5, ARRAY, ORDER )  = 5 */
/*            BSCHOI (     1, 5, ARRAY, ORDER )  = 2 */
/*            BSCHOI ( 10000, 5, ARRAY, ORDER )  = 4 */
/*            BSCHOI (    -1, 5, ARRAY, ORDER )  = 0 */
/*            BSCHOI (    17, 5, ARRAY, ORDER )  = 0 */

/*      That is, */

/*            ARRAY(5) =  1000 */
/*            ARRAY(2) =     1 */
/*            ARRAY(4) = 10000 */

/*      (Compare with BSCHOI_2.) */

/* $ Restrictions */

/*      ORDER is assumed to give the order of the elements of ARRAY */
/*      in increasing order. If this condition is not met, the results */
/*      of BSCHOI are unpredictable. */

/* $ Author_and_Institution */

/*       I.M. Underwood  (JPL) */
/*       W.L. Taber      (JPL) */

/* $ Literature_References */

/*       None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 18-SEP-1995 (IMU) (WLT) */

/* -& */
/* $ Index_Entries */

/*     binary search for an integer using an order vector */

/* -& */

/*     Local variables */


/*     Set the initial bounds for the search area. */

    left = 1;
    right = *ndim;
    while(left <= right) {

/*        Check the middle element. */

	i__ = (left + right) / 2;

/*        If the middle element matches, return its location. */

	if (*value == array[order[i__ - 1] - 1]) {
	    ret_val = order[i__ - 1];
	    return ret_val;

/*        Otherwise narrow the search area. */

	} else if (*value < array[order[i__ - 1] - 1]) {
	    right = i__ - 1;
	} else {
	    left = i__ + 1;
	}
    }

/*     If the search area is empty, return zero. */

    ret_val = 0;
    return ret_val;
} /* bschoi_ */

