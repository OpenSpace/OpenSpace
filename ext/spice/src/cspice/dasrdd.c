/* dasrdd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__1 = 1;

/* $Procedure      DASRDD ( DAS, read data, double precision ) */
/* Subroutine */ int dasrdd_(integer *handle, integer *first, integer *last, 
	doublereal *data)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    integer n, nread, recno, numdp;
    extern /* Subroutine */ int dasa2l_(integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *);
    extern logical failed_(void);
    integer clbase;
    extern /* Subroutine */ int dasrrd_(integer *, integer *, integer *, 
	    integer *, doublereal *);
    integer clsize, wordno;

/* $ Abstract */

/*     Read double precision data from a range of DAS logical addresses. */

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
/*     LAST       I   Range of DAS double precision logical addresses. */
/*     DATA       O   Data having addresses FIRST through LAST. */

/* $ Detailed_Input */

/*     HANDLE         is a file handle for an open DAS file. */

/*     FIRST, */
/*     LAST           are a range of DAS double precision logical */
/*                    addresses.  FIRST and LAST must be greater than or */
/*                    equal to 1 and less than or equal to the highest */
/*                    double precision logical address in the DAS file */
/*                    designated by HANDLE. */

/* $ Detailed_Output */

/*     DATA           is an array of double precision numbers.  DATA */
/*                    should have length at least LAST - FIRST + 1. */

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

/*     This routine provides random read access to the double precision */
/*     data in a DAS file.  This data is logically structured as a */
/*     one-dimensional array of double precision numbers. */

/* $ Examples */

/*     1)  Create the new DAS file TEST.DAS and add 200 double */
/*         precision numbers to it.  Close the file, then re-open */
/*         it and read the data back out. */

/*                  PROGRAM TEST_READ */

/*                  CHARACTER*(4)         TYPE */

/*                  DOUBLE PRECISION      DATA   ( 200 ) */

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
/*            C     Fill the array DATA with the double precision */
/*            C     numbers 1.D0 through 100.D0, and add this array */
/*            C     to the file. */
/*            C */
/*                  DO I = 1, 100 */
/*                     DATA(I) = DBLE(I) */
/*                  END DO */

/*                  CALL DASADD ( HANDLE, 100, DATA, FIRST, LAST ) */

/*            C */
/*            C     Now append the array DATA to the file again. */
/*            C */
/*                  CALL DASADD ( HANDLE, 100, DATA, FIRST, LAST ) */

/*            C */
/*            C     Close the file. */
/*            C */
/*                  CALL DASCLS ( HANDLE ) */

/*            C */
/*            C     Now verify the addition of data by opening the */
/*            C     file for read access and retrieving the data. */
/*            C */
/*                  CALL DASRDD ( HANDLE, 1, 200, DATA ) */

/*            C */
/*            C     Dump the data to the screen.  We should see the */
/*            C     sequence  1, 2, ..., 100, 1, 2, ... , 100.  The */
/*            C     numbers will be represented as double precision */
/*            C     numbers in the output. */
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

/* -    SPICELIB Version 1.1.1 19-DEC-1995 (NJB) */

/*        Corrected title of permuted index entry section. */

/* -    SPICELIB Version 1.2.0, 01-NOV-1995 (NJB) */

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

/*     read double precision data from a DAS file */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.2.0, 30-OCT-1995 (NJB) */

/*        Routine now uses discovery check-in.  FAILED test moved inside */
/*        loop. */

/* -    SPICELIB Version 1.1.0, 12-MAY-1994 (KRG) (NJB) */

/*        Test of FAILED() added to loop termination condition.  Without */
/*        this test, an infinite loop could result if DASA2L or DASRRD */
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


/*     Find out the physical location of the first double precision */
/*     number.  If FIRST is invalid, DASA2L will take care of the */
/*     problem. */

    dasa2l_(handle, &c__2, first, &clbase, &clsize, &recno, &wordno);

/*     Decide how many double precision numbers to read. */

    numdp = *last - *first + 1;
    nread = 0;

/*     Read as much data from record RECNO as necessary. */

/* Computing MIN */
    i__1 = numdp, i__2 = 128 - wordno + 1;
    n = min(i__1,i__2);
    i__1 = wordno + n - 1;
    dasrrd_(handle, &recno, &wordno, &i__1, data);
    nread = n;
    ++recno;

/*     Read from as many additional records as necessary. */

    while(nread < numdp) {
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
	    i__1 = numdp - nread;
	    n = min(i__1,128);
	    dasrrd_(handle, &recno, &c__1, &n, &data[nread]);
	    nread += n;
	    ++recno;
	} else {

/*           We must find the next double precision cluster to */
/*           read from.  The first double precision number in this */
/*           cluster has address FIRST + NREAD. */

	    i__1 = *first + nread;
	    dasa2l_(handle, &c__2, &i__1, &clbase, &clsize, &recno, &wordno);
	}
    }
    return 0;
} /* dasrdd_ */

