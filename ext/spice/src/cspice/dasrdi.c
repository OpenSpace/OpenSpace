/* dasrdi.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__1 = 1;

/* $Procedure      DASRDI ( DAS, read data, integer ) */
/* Subroutine */ int dasrdi_(integer *handle, integer *first, integer *last, 
	integer *data)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    integer n, nread, recno;
    extern /* Subroutine */ int dasa2l_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *);
    extern logical failed_(void);
    integer clbase;
    extern /* Subroutine */ int dasrri_(integer *, integer *, integer *, 
	    integer *, integer *);
    integer clsize, wordno, numint;

/* $ Abstract */

/*     Read integer data from a range of DAS logical addresses. */

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

/*     DAS */

/* $ Keywords */

/*     ARRAY */
/*     ASSIGNMENT */
/*     DAS */
/*     FILES */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   DAS file handle. */
/*     FIRST, */
/*     LAST       I   Range of DAS integer logical addresses. */
/*     DATA       O   Data having addresses FIRST through LAST. */

/* $ Detailed_Input */

/*     HANDLE         is a file handle for an open DAS file. */

/*     FIRST, */
/*     LAST           are a range of DAS integer logical addresses. */
/*                    FIRST and LAST must be greater than or equal to */
/*                    1 and less than or equal to the highest integer */
/*                    logical address in the DAS file designated by */
/*                    HANDLE. */

/* $ Detailed_Output */

/*     DATA           is an array of integers.  DATA should have length */
/*                    at least LAST - FIRST + 1. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input file handle is invalid, the error will be */
/*         diagnosed by routines called by this routine.  DATA will */
/*         not be modified. */

/*     2)  If FIRST or LAST are out of range, the error will be diagnosed */
/*         by routines called by this routine. */

/*     3)  If FIRST is greater than LAST, DATA is left unchanged. */

/*     4)  If DATA is declared with length less than FIRST - LAST + 1, */
/*         the error cannot be diagnosed by this routine. */

/* $ Files */

/*     See the description of the argument HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     This routine provides random read access to the integer data in */
/*     a DAS file.  This data is logically structured as a */
/*     one-dimensional array of integers. */

/* $ Examples */


/*     1)  Create the new DAS file TEST.DAS and add 200 integers to it. */
/*         Close the file, then re-open it and read the data back out. */


/*                  PROGRAM TEST_READ */

/*                  CHARACTER*(4)         TYPE */

/*                  INTEGER               DATA   ( 200 ) */

/*                  INTEGER               FIRST */
/*                  INTEGER               HANDLE */
/*                  INTEGER               I */
/*                  INTEGER               LAST */
/*            C */
/*            C     Open a new DAS file.  Use the file name as */
/*            C     the internal file name. */
/*            C */
/*                  TYPE = 'TEST' */
/*                  CALL DASONW ( 'TEST.DAS', TYPE, 'TEST.DAS', HANDLE ) */

/*            C */
/*            C     Fill the array DATA with the integers 1 through */
/*            C     100, and add this array to the file. */
/*            C */
/*                  DO I = 1, 100 */
/*                     DATA(I) = I */
/*                  END DO */

/*                  CALL DASADI ( HANDLE, 100, DATA, FIRST, LAST ) */

/*            C */
/*            C     Now append the array DATA to the file again. */
/*            C */
/*                  CALL DASADI ( HANDLE, 100, DATA, FIRST, LAST ) */

/*            C */
/*            C     Close the file. */
/*            C */
/*                  CALL DASCLS ( HANDLE ) */

/*            C */
/*            C     Now verify the addition of data by opening the */
/*            C     file for read access and retrieving the data. */
/*            C */
/*                  CALL DASRDI ( HANDLE, 1, 200, DATA ) */

/*            C */
/*            C     Dump the data to the screen.  We should see the */
/*            C     sequence  1, 2, ..., 100, 1, 2, ... , 100. */
/*            C */
/*                  WRITE (*,*) ' ' */
/*                  WRITE (*,*) 'Data from TEST.DAS: ' */
/*                  WRITE (*,*) ' ' */
/*                  WRITE (*,*) DATA */

/*                  END */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     K.R. Gehringer (JPL) */
/*     N.J. Bachman   (JPL) */
/*     W.L. Taber     (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.2.1 19-DEC-1995 (NJB) */

/*        Corrected title of permuted index entry section. */

/* -    SPICELIB Version 1.2.0, 30-OCT-1995 (NJB) */

/*        Routine now uses discovery check-in.  FAILED test moved inside */
/*        loop. */

/* -    SPICELIB Version 1.1.0, 12-MAY-1994 (KRG) (NJB) */

/*        Test of FAILED() added to loop termination condition. */

/*        Removed references to specific DAS file open routines in the */
/*        $ Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if the DAS open routines ever */
/*        change. */

/*        Modified the $ Examples section to demonstrate the new ID word */
/*        format which includes a file type and to include a call to the */
/*        new routine DASONW, open new for write, which makes use of the */
/*        file type. Also,  a variable for the type of the file to be */
/*        created was added. */

/* -    SPICELIB Version 1.0.0, 13-JUN-1992 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     read integer data from a DAS file */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.2.0, 30-OCT-1995 (NJB) */

/*        Routine now uses discovery check-in.  FAILED test moved inside */
/*        loop. */

/* -    SPICELIB Version 1.1.0, 12-MAY-1994 (KRG) (NJB) */

/*        Test of FAILED() added to loop termination condition.  Without */
/*        this test, an infinite loop could result if DASA2L or DASRRI */
/*        signaled an error inside the loop. */

/*        Removed references to specific DAS file open routines in the */
/*        $ Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if the DAS open routines ever */
/*        change. */

/*        Modified the $ Examples section to demonstrate the new ID word */
/*        format which includes a file type and to include a call to the */
/*        new routine DASONW, open new for write, which makes use of the */
/*        file type. Also,  a variable for the type of the file to be */
/*        created was added. */

/* -    SPICELIB Version 1.0.0, 13-JUN-1992 (NJB) (WLT) */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Find out the physical location of the first integer.  If FIRST */
/*     is invalid, DASA2L will take care of the problem. */

    dasa2l_(handle, &c__3, first, &clbase, &clsize, &recno, &wordno);

/*     Decide how many integers to read. */

    numint = *last - *first + 1;
    nread = 0;

/*     Read as much data from record RECNO as necessary. */

/* Computing MIN */
    i__1 = numint, i__2 = 256 - wordno + 1;
    n = min(i__1,i__2);
    i__1 = wordno + n - 1;
    dasrri_(handle, &recno, &wordno, &i__1, data);
    nread = n;
    ++recno;

/*     Read from as many additional records as necessary. */

    while(nread < numint) {
	if (failed_()) {
	    return 0;
	}

/*        At this point, RECNO is the correct number of the */
/*        record to read from next.  CLBASE is the number */
/*        of the first record of the cluster we're about */
/*        to read from. */

	if (recno < clbase + clsize) {

/*           We can continue reading from the current */
/*           cluster. */

/* Computing MIN */
	    i__1 = numint - nread;
	    n = min(i__1,256);
	    dasrri_(handle, &recno, &c__1, &n, &data[nread]);
	    nread += n;
	    ++recno;
	} else {

/*           We must find the next integer cluster to */
/*           read from.  The first integer in this */
/*           cluster has address FIRST + NREAD. */

	    i__1 = *first + nread;
	    dasa2l_(handle, &c__3, &i__1, &clbase, &clsize, &recno, &wordno);
	}
    }
    return 0;
} /* dasrdi_ */

