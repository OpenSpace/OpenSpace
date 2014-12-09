/* dasadi.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;

/* $Procedure      DASADI ( DAS, add data, integer ) */
/* Subroutine */ int dasadi_(integer *handle, integer *n, integer *data)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    integer free;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer ncomc, recno, lasti, ncomr;
    extern /* Subroutine */ int movei_(integer *, integer *, integer *), 
	    dasa2l_(integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *);
    extern logical failed_(void);
    integer clbase;
    extern /* Subroutine */ int dascud_(integer *, integer *, integer *), 
	    dashfs_(integer *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, integer *);
    integer record[256], lastla[3];
    extern /* Subroutine */ int dasuri_(integer *, integer *, integer *, 
	    integer *, integer *);
    integer lastrc[3], clsize;
    extern /* Subroutine */ int daswri_(integer *, integer *, integer *), 
	    chkout_(char *, ftnlen);
    integer lastwd[3], nresvc, wordno, numint;
    extern logical return_(void);
    integer nresvr, nwritn;

/* $ Abstract */

/*     Add an array of integers to a DAS file. */

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
/*     N          I   Number of integers to add to DAS file. */
/*     DATA       I   Array of integers to add. */

/* $ Detailed_Input */

/*     HANDLE         is a file handle of a DAS file opened for writing. */

/*     N              is a the number of integer `words' to */
/*                    add to the DAS file specified by HANDLE. */

/*     DATA           is an array of integers to be added to the */
/*                    specified DAS file.  Elements 1 through N are */
/*                    appended to the integer data in the file. */

/* $ Detailed_Output */

/*     None.  See $Particulars for a description of the effect of this */
/*     routine. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input file handle is invalid, the error will be */
/*         diagnosed by routines called by this routine. */

/*     2)  If an I/O error occurs during the data addition attempted */
/*         by this routine, the error will be diagnosed by routines */
/*         called by this routine. */

/*     3)  If the input count N is less than 1, no data will be */
/*         added to the specified DAS file. */

/* $ Files */

/*     See the description of the argument HANDLE in $Detailed_Input. */

/* $ Particulars */

/*     This routine adds integer data to a DAS file by `appending' it */
/*     after any integer data already in the file.  The sense in which */
/*     the data is `appended' is that the data will occupy a range of */
/*     logical addresses for integer data that immediately follow the */
/*     last logical address of a integer that is occupied at the time */
/*     this routine is called.  The diagram below illustrates this */
/*     addition: */

/*        +-------------------------+ */
/*        |    (already in use)     |  Integer logical address 1 */
/*        +-------------------------+ */
/*                    . */
/*                    . */
/*                    . */
/*        +-------------------------+ */
/*        |    (already in use)     |  Last integer logical address */
/*        +-------------------------+  in use before call to DASADI */
/*        |        DATA(1)          | */
/*        +-------------------------+ */
/*                    . */
/*                    . */
/*                    . */
/*        +-------------------------+ */
/*        |        DATA(N)          | */
/*        +-------------------------+ */


/*     The logical organization of the integers in the DAS file is */
/*     independent of the order of addition to the file or physical */
/*     location of any data of double precision or character type. */

/*     The actual physical write operations that add the input array */
/*     DATA to the indicated DAS file may not take place before this */
/*     routine returns, since the DAS system buffers data that is */
/*     written as well as data that is read.  In any case, the data */
/*     will be flushed to the file at the time the file is closed, if */
/*     not earlier.  A physical write of all buffered records can be */
/*     forced by calling the SPICELIB routine DASWBR (DAS, write */
/*     buffered records). */

/*     In order to update integer logical addresses that already contain */
/*     data, the SPICELIB routine DASUDI ( DAS update data, integer ) */
/*     should be used. */

/* $ Examples */

/*     1)  Create the new DAS file TEST.DAS and add 200 integers to it. */
/*         Close the file, then re-open it and read the data back out. */


/*                  PROGRAM TEST_ADD */

/*                  CHARACTER*(4)         TYPE */

/*                  INTEGER               DATA   ( 200 ) */

/*                  INTEGER               HANDLE */
/*                  INTEGER               I */
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

/*                  CALL DASADI ( HANDLE, 100, DATA ) */

/*            C */
/*            C     Now append the array DATA to the file again. */
/*            C */
/*                  CALL DASADI ( HANDLE, 100, DATA ) */

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

/* -    SPICELIB Version 1.2.0 10-APR-2014 (NJB) */

/*        Deleted declarations of unused parameters. */

/*        Corrected header comments: routine that flushes */
/*        written, buffered records is DASWBR, not DASWUR. */

/* -    SPICELIB Version 1.1.1 19-DEC-1995 (NJB) */

/*        Corrected title of permuted index entry section. */

/* -    SPICELIB Version 1.1.0, 12-MAY-1994 (KRG) (NJB) */

/*        Test of FAILED() added to loop termination conditions. */

/*        Removed references to specific DAS file open routines in the */
/*        $ Detailed_Input section of the header. This was done in order */
/*        to minimize documentation changes if the DAS open routines ever */
/*        change. */

/*        Modified the $ Examples section to demonstrate the new ID word */
/*        format which includes a file type and to include a call to the */
/*        new routine DASONW, open new, which makes use of the file */
/*        type. Also, a variable for the type of the file to be created */
/*        was added. */

/* -    SPICELIB Version 1.0.0, 11-NOV-1992 (NJB) (WLT) */

/* -& */
/* $ Index_Entries */

/*     add integer data to a DAS file */
/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.1.0, 12-MAY-1994 (KRG) (NJB) */

/*        Test of FAILED() added to loop termination condition.  Without */
/*        this test, an infinite loop could result if DASA2L, DASURI or */
/*        DASWRI signaled an error inside the loop. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DASADI", (ftnlen)6);
    }

/*     Get the file summary for this DAS. */

    dashfs_(handle, &nresvr, &nresvc, &ncomr, &ncomc, &free, lastla, lastrc, 
	    lastwd);
    lasti = lastla[2];

/*     We will keep track of the location that we wish to write to */
/*     with the variables RECNO and WORDNO.  RECNO will be the record */
/*     number of the record we'll write to; WORDNO will be the number */
/*     preceding the word index, within record number RECNO, that we'll */
/*     write to.  For example, if we're about to write to the first */
/*     integer in record 10, RECNO will be 10 and WORDNO will be 0.  Of */
/*     course, when WORDNO reaches NWI, we'll have to find a free record */
/*     before writing anything. */

/*     Prepare the variables RECNO and WORDNO:  use the physical */
/*     location of the last integer address, if there are any integer */
/*     data in the file.  Otherwise, RECNO becomes the first record */
/*     available for integer data. */

    if (lasti >= 1) {
	dasa2l_(handle, &c__3, &lasti, &clbase, &clsize, &recno, &wordno);
    } else {
	recno = free;
	wordno = 0;
    }

/*     Set the number of integer words already written.  Keep */
/*     writing to the file until this number equals the number of */
/*     elements in DATA. */

/*     Note that if N is non-positive, the loop doesn't get exercised. */


    nwritn = 0;
    while(nwritn < *n && ! failed_()) {

/*        Write as much data as we can (or need to) into the current */
/*        record.  We assume that RECNO, WORDNO, and NWRITN have been */
/*        set correctly at this point. */

/*        Find out how many words to write into the current record. */
/*        There may be no space left in the current record. */

/* Computing MIN */
	i__1 = *n - nwritn, i__2 = 256 - wordno;
	numint = min(i__1,i__2);
	if (numint > 0) {

/*           Write NUMINT words into the current record.  If the record */
/*           is new, write the entire record.  Otherwise, just update */
/*           the part we're interested in. */

	    if (wordno == 0) {
		movei_(&data[nwritn], &numint, record);
		daswri_(handle, &recno, record);
	    } else {
		i__1 = wordno + 1;
		i__2 = wordno + numint;
		dasuri_(handle, &recno, &i__1, &i__2, &data[nwritn]);
	    }
	    nwritn += numint;
	    wordno += numint;
	} else {

/*           It's time to start on a new record.  If the record we */
/*           just finished writing to (or just attempted writing to, */
/*           if it was full) was FREE or a higher-numbered record, */
/*           then we are writing to a contiguous set of data records: */
/*           the next record to write to is the immediate successor */
/*           of the last one.  Otherwise, FREE is the next record */
/*           to write to. */

/*           We intentionally leave FREE at the value it had before */
/*           we starting adding data to the file. */

	    if (recno >= free) {
		++recno;
	    } else {
		recno = free;
	    }
	    wordno = 0;
	}
    }

/*     Update the DAS file directories to reflect the addition of N */
/*     integer words.  DASCUD will also update the file summary */
/*     accordingly. */

    dascud_(handle, &c__3, n);
    chkout_("DASADI", (ftnlen)6);
    return 0;
} /* dasadi_ */

