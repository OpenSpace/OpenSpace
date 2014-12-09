/* pckr02.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__5 = 5;

/* $Procedure PCKR02 ( PCK, read record from type 2 segment ) */
/* Subroutine */ int pckr02_(integer *handle, doublereal *descr, doublereal *
	et, doublereal *record)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer nrec;
    doublereal init;
    integer begin;
    extern /* Subroutine */ int chkin_(char *, ftnlen), dafus_(doublereal *, 
	    integer *, integer *, doublereal *, integer *);
    integer recno;
    extern /* Subroutine */ int dafgda_(integer *, integer *, integer *, 
	    doublereal *);
    doublereal dc[2];
    integer ic[5], recadr;
    doublereal intlen;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    integer recsiz;
    extern logical return_(void);
    integer end;

/* $ Abstract */

/*     Read a single PCK data record from a segment of type 2 */
/*     (Chebyshev, 3-vector only). */

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

/*     PCK */

/* $ Keywords */

/*     EPHEMERIS */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   File handle. */
/*     DESCR      I   Segment descriptor. */
/*     ET         I   Target epoch. */
/*     RECORD     O   Data record. */

/* $ Detailed_Input */

/*     HANDLE, */
/*     DESCR       are the file handle and segment descriptor for */
/*                 a PCK segment of type 2. */

/*     ET          is a target epoch, for which a data record from */
/*                 a specific segment is required. */

/* $ Detailed_Output */

/*     RECORD      is the record from the specified segment which, */
/*                 when evaluated at epoch ET, will give the Euler */
/*                 angles (orientation) of some body. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     See argument HANDLE. */

/* $ Particulars */

/*     See the PCK Required Reading file for a description of the */
/*     structure of a data type 2 (Chebyshev polynomials, Euler */
/*     angles only) segment. */

/* $ Examples */

/*     The data returned  is in its rawest form, taken directly from */
/*     the segment.  As such, it will be meaningless to a user unless */
/*     he/she understands the structure of the data type completely. */
/*     Given that understanding, however, the PCKRxx routines might be */
/*     used to "dump" and check segment data for a particular epoch. */


/*     C */
/*     C     Get a segment applicable to a specified body and epoch. */
/*     C */
/*           CALL PCKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND ) */

/*     C */
/*     C     Look at parts of the descriptor. */
/*     C */
/*           CALL DAFUS ( DESCR, ND, NI, DCD, ICD ) */
/*           REF    = ICD( NR ) */
/*           TYPE   = ICD( NT ) */

/*           IF ( TYPE .EQ. 2 ) THEN */
/*              CALL PCKR02 ( HANDLE, DESCR, ET, RECORD ) */
/*                  . */
/*                  .  Look at the RECORD data. */
/*                  . */
/*           END IF */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     K.S. Zukor  (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.1, 03-JAN-2014 (EDW) */

/*        Minor edits to Procedure; clean trailing whitespace. */

/* -    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW) */

/*        Replaced DAFRDA call with DAFGDA. */
/*        Added IMPLICIT NONE. */

/* -    SPICELIB Version 1.0.0, 11-MAR-1993 (KSZ) */

/* -& */
/* $ Index_Entries */

/*     read record from type_2 pck segment */

/* -& */

/*     SPICELIB functions */


/*     Parameters */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("PCKR02", (ftnlen)6);
    }

/*     Unpack the segment descriptor. */

    dafus_(descr, &c__2, &c__5, dc, ic);
    begin = ic[3];
    end = ic[4];

/*     The segment is made up of a number of logical records, each */
/*     having the same size, and covering the same length of time. */

/*     We can determine which record to return by comparing the input */
/*     epoch with the initial time of the segment and the length of the */
/*     interval covered by each record.  These final two constants are */
/*     located at the end of the segment, along with the size of each */
/*     logical record and the total number of records. */

    i__1 = end - 3;
    dafgda_(handle, &i__1, &end, record);
    init = record[0];
    intlen = record[1];
    recsiz = (integer) record[2];
    nrec = (integer) record[3];
    recno = (integer) ((*et - init) / intlen) + 1;
    recno = min(recno,nrec);

/*     Compute the address of the desired record. */

    recadr = (recno - 1) * recsiz + begin;

/*     Along with the record, return the size of the record. */

    record[0] = record[2];
    i__1 = recadr + recsiz - 1;
    dafgda_(handle, &recadr, &i__1, &record[1]);
    chkout_("PCKR02", (ftnlen)6);
    return 0;
} /* pckr02_ */

