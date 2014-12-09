/* reordl.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      REORDL ( Reorder a logical array ) */
/* Subroutine */ int reordl_(integer *iorder, integer *ndim, logical *array)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    integer hold;
    logical temp;
    integer index, start;

/* $ Abstract */

/*      Re-order the elements of a logical array according to */
/*      a given order vector. */

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

/*      ARRAY,  SORT */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      IORDER     I   Order vector to be used to re-order ARRAY. */
/*      NDIM       I   Dimension of ARRAY. */
/*      ARRAY     I/O  Array to be re-ordered. */

/* $ Detailed_Input */

/*      IORDER      is the order vector to be used to re-order the input */
/*                  array. The first element of IORDER is the index of */
/*                  the first item of the re-ordered array, and so on. */

/*                  Note that the order imposed by REORDL is not the */
/*                  same order that would be imposed by a sorting */
/*                  routine. In general, the order vector will have */
/*                  been created (by one of the ORDER routines) for */
/*                  a related array, as illustrated in the example below. */

/*      NDIM        is the number of elements in the input array. */

/*      ARRAY       on input, is an array containing some number of */
/*                  logicals in unspecified order. */

/* $ Detailed_Output */

/*      ARRAY       on output, is the same array, with the logicals */
/*                  re-ordered as specified by IORDER. */

/* $ Parameters */

/*     None. */

/* $ Particulars */

/*      REORDL uses a cyclical algorithm to re-order the elements of */
/*      the array in place. After re-ordering, element IORDER(1) of */
/*      the input array is the first element of the output array, */
/*      element IORDER(2) is the input array is the second element of */
/*      the output array, and so on. */

/*      The order vector used by REORDL is typically created for */
/*      a related array by one of the ORDER routines, as shown in */
/*      the example below. */

/* $ Examples */

/*      In the following example, the ORDER and REORD routines are */
/*      used to sort four related arrays (containing the names, */
/*      masses, asteroid flag, and visual magnitudes for a group */
/*      of satellites). This is representative of the typical use of */
/*      these routines. */

/*            C */
/*            C     Sort the object arrays by name. */
/*            C */
/*                  CALL ORDERC ( NAMES, N, IORDER ) */

/*                  CALL REORDC ( IORDER, N, NAMES ) */
/*                  CALL REORDD ( IORDER, N, MASSES ) */
/*                  CALL REORDL ( IORDER, N, AFLAGS ) */
/*                  CALL REORDR ( IORDER, N, VMAGS ) */

/* $ Restrictions */

/*      None. */

/* $ Exceptions */

/*      Error free. */

/* $ Files */

/*      None. */

/* $ Author_and_Institution */

/*      W.L. Taber      (JPL) */
/*      I.M. Underwood  (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -     SPICELIB Version 1.0.0, 6-MAR-1996 (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     reorder a logical array */

/* -& */

/*     Local variables */


/*     If the array doesn't have at least two elements, don't bother. */

    if (*ndim < 2) {
	return 0;
    }

/*     START is the position in the order vector that begins the */
/*     current cycle. When all the switches have been made, START */
/*     will point to the end of the order vector. */

    start = 1;
    while(start < *ndim) {

/*        Begin with the element of input vector specified by */
/*        IORDER(START). Move it to the correct position in the */
/*        array, after saving the element it replaces to TEMP. */
/*        HOLD indicates the position of the array element to */
/*        be moved to its new position. After the element has */
/*        been moved, HOLD indicates the position of an available */
/*        space within the array. */

	index = start;
	temp = array[index - 1];
	hold = iorder[index - 1];

/*        As each slot in the output array is filled in, the sign */
/*        of the corresponding element in the order vector is changed */
/*        from positive to negative. This way, we know which elements */
/*        have already been ordered when looking for the beginning of */
/*        the next cycle. */

/*        Keep going until HOLD points to the first array element */
/*        moved during the current cycle. This ends the cycle. */

	while(hold != start) {
	    array[index - 1] = array[hold - 1];
	    index = hold;
	    hold = iorder[hold - 1];
	    iorder[index - 1] = -iorder[index - 1];
	}

/*        The last element in the cycle is restored from TEMP. */

	array[index - 1] = temp;
	iorder[hold - 1] = -iorder[hold - 1];

/*        Begin the next cycle at the next element in the order */
/*        vector with a positive sign. (That is, the next one */
/*        that hasn't been moved.) */

	while(iorder[start - 1] < 0 && start < *ndim) {
	    ++start;
	}
    }

/*     Restore the original signs of the elements of the order vector, */
/*     in case the vector is to be used again with another array. */

    i__1 = *ndim;
    for (index = 1; index <= i__1; ++index) {
	iorder[index - 1] = (i__2 = iorder[index - 1], abs(i__2));
    }
    return 0;
} /* reordl_ */

