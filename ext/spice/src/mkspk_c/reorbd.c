/* reorbd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      REORBD ( Reorder a blocks of double precisions ) */
/* Subroutine */ int reorbd_(integer *ordvec, integer *n, integer *bsize, 
	doublereal *data)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    integer hold;
    doublereal temp;
    integer i__, index, start;

/* $ Abstract */

/*     Reorder the fixed size contiguous blocks of data in an */
/*     array of double precision numbers as specified by an */
/*     order vector */

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

/*       UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      ORDVEC     I   is an order vector */
/*      N          I   is the number of order vector elements */
/*      BSIZE      I   is the size of the blocks in DATA */
/*      DATA      I/O  an array of contiguous fixed size blocks of d.p.s */

/* $ Detailed_Input */

/*     ORDVEC      is an order vector that tells how the blocks */
/*                 of data should be re-arranged. ORDVEC might */
/*                 be produced by calling ORDERI with inputs of */
/*                 integer keys. */

/*     N           is the number of values in the order vector and the */
/*                 number of contiguous blocks in the array DATA. */

/*     BSIZE       is the size of each block of data in DATA */

/*     DATA        is an array containing N*BSIZE double precision */
/*                 numbers.  The data is regarded as being arranged */
/*                 in blocks of data as shown below: */

/*                 Block 1       d.p. (1,     1 ) */
/*                               d.p. (2,     1 ) */
/*                               d.p. (3,     1 ) */
/*                                 . */
/*                                 . */
/*                                 . */
/*                               d.p. (BSIZE, 1 ) */

/*                 Block 2       d.p. (1,     2 ) */
/*                               d.p. (2,     2 ) */
/*                               d.p. (3,     2 ) */
/*                                 . */
/*                                 . */
/*                                 . */
/*                               d.p. (BSIZE, 2 ) */

/*                 Block 3       d.p. (1,     3 ) */

/*                        etc. */

/*                 The goal in calling this routine is to move the */
/*                 blocks around in the array while keeping the */
/*                 data items in each so that the blocks are in */
/*                 the order prescribed by the order vector. */


/* $ Detailed_Output */

/*     DATA        is the input array after the blocks have been move */
/*                 into the order prescribed by the order vector. */

/* $ Parameters */

/*      None. */

/* $ Exceptions */

/*     Error free. */

/*     1) */

/* $ Files */

/*      None. */

/* $ Particulars */

/*     This routine rearranges blocks of data in a double precision */
/*     array in the same way that REORDD rearranges individual */
/*     elements of a double precision array. */

/* $ Examples */

/*     Suppose that you have a collection of state vectors and an */
/*     array of corresponding epochs as shown below. */

/*        EPOCH(1)         STATES(  1 ) */
/*                         STATES(  2 ) */
/*                         STATES(  3 ) */
/*                         STATES(  4 ) */
/*                         STATES(  5 ) */
/*                         STATES(  6 ) */

/*        EPOCH(2)         STATES(  7 ) */
/*                         STATES(  8 ) */
/*                         STATES(  9 ) */
/*                         STATES( 10 ) */
/*                         STATES( 11 ) */
/*                         STATES( 12 ) */

/*        EPOCH(3)         STATES( 13 ) */
/*                         STATES( 14 ) */
/*                         STATES( 15 ) */
/*                         STATES( 16 ) */
/*                         STATES( 17 ) */
/*                         STATES( 18 ) */

/*        EPOCH(4)         STATES( 19 ) */
/*                         STATES( 20 ) */
/*                         STATES( 21 ) */
/*                         STATES( 22 ) */

/*                            . */
/*                            . */
/*                            . */

/*     But that the times are not in increasing order. To arrange the */
/*     times and states so that the times are in-order and the */
/*     relationship between epoch index and state indices are maintained */
/*     you could perform the following sequence of subroutine calls. */

/*        CALL ORDERD ( EPOCH,  N,    ORDVEC ) */
/*        CALL REORDD ( ORDVEC, N,    EPOCH  ) */
/*        CALL REORBD ( ORDVEC, N, 6, STATES ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*      None. */

/* $ Author_and_Institution */

/*      W.L. Taber      (JPL) */
/*      B.V. Semenov    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 15-MAR-2012 (BVS) */

/*        Reordered header sections. */

/* -    SPICELIB Version 1.0.0, 28-JUN-1994 (WLT) */


/* -& */
/* $ Index_Entries */

/*     Reorder a d.p. array by blocks. */
/*     Reorder the blocks of a d.p. array. */

/* -& */

/*     Local variables */


/*     If the array doesn't have at least two elements, don't bother. */

    if (*n < 2) {
	return 0;
    }

/*     START is the position in the order vector that begins the */
/*     current cycle. When all the switches have been made, START */
/*     will point to the end of the order vector. */

    start = 1;
    while(start < *n) {

/*        Begin with the element of input vector specified by */
/*        ORDVEC(START). Move it to the correct position in the */
/*        array, after saving the element it replaces to TEMP. */
/*        HOLD indicates the position of the array element to */
/*        be moved to its new position. After the element has */
/*        been moved, HOLD indicates the position of an available */
/*        space within the array.  Note we need to do this for */
/*        each element of the block.  We do the first BSIZE-1 items */
/*        first and do the last item of the block separately so */
/*        that we can "tag" the elements of the order vector to */
/*        indicate we've touched them. */

	i__1 = *bsize - 1;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    index = start;
	    temp = data[*bsize * (index - 1) + i__ - 1];
	    hold = ordvec[index - 1];

/*           Keep going until HOLD points to the first array element */
/*           moved during the current cycle. This ends the cycle. */

	    while(hold != start) {
		data[*bsize * (index - 1) + i__ - 1] = data[*bsize * (hold - 
			1) + i__ - 1];
		index = hold;
		hold = ordvec[hold - 1];
	    }

/*           The last element in the cycle is restored from TEMP. */

	    data[*bsize * (index - 1) + i__ - 1] = temp;
	}

/*        Now for the last item of the block: */

/*        As each slot in the output array is filled in, the sign */
/*        of the corresponding element in the order vector is changed */
/*        from positive to negative. This way, we know which elements */
/*        have already been ordered when looking for the beginning of */
/*        the next cycle. */

	index = start;
	temp = data[*bsize * index - 1];
	hold = ordvec[index - 1];
	while(hold != start) {
	    data[index * *bsize - 1] = data[hold * *bsize - 1];
	    index = hold;
	    hold = ordvec[hold - 1];
	    ordvec[index - 1] = -ordvec[index - 1];
	}

/*        The last element in the cycle is restored from TEMP. */

	data[index * *bsize - 1] = temp;
	ordvec[hold - 1] = -ordvec[hold - 1];

/*        Begin the next cycle at the next element in the order */
/*        vector with a positive sign. (That is, the next one */
/*        that hasn't been moved.) */

	while(ordvec[start - 1] < 0 && start < *n) {
	    ++start;
	}
    }

/*     Restore the original signs of the elements of the order vector, */
/*     in case the vector is to be used again with another array. */

    i__1 = *n;
    for (index = 1; index <= i__1; ++index) {
	ordvec[index - 1] = (i__2 = ordvec[index - 1], abs(i__2));
    }
    return 0;
} /* reorbd_ */

