/* ckgr02.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;
static integer c__7 = 7;

/* $Procedure      CKGR02 ( C-kernel, get record, type 02 ) */
/* Subroutine */ int ckgr02_(integer *handle, doublereal *descr, integer *
	recno, doublereal *record)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer addr__, nrec;
    doublereal prec[8];
    extern /* Subroutine */ int chkin_(char *, ftnlen), cknr02_(integer *, 
	    doublereal *, integer *), dafus_(doublereal *, integer *, integer 
	    *, doublereal *, integer *), moved_(doublereal *, integer *, 
	    doublereal *), dafgda_(integer *, integer *, integer *, 
	    doublereal *), sigerr_(char *, ftnlen), chkout_(char *, ftnlen), 
	    setmsg_(char *, ftnlen), errint_(char *, integer *, ftnlen);
    extern logical return_(void);
    doublereal dcd[2];
    integer beg, icd[6];

/* $ Abstract */

/*     Given the handle and descriptor of a type 2 segment in a CK file, */
/*     return a specified pointing record from that segment. */

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

/*     CK */
/*     DAF */

/* $ Keywords */

/*     POINTING */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   The handle of the file containing the segment. */
/*     DESCR      I   The segment descriptor. */
/*     RECNO      I   The number of the pointing record to be returned. */
/*     RECORD     O   The pointing record. */

/* $ Detailed_Input */

/*     HANDLE     is the handle of the binary CK file containing the */
/*                desired segment. The file should have been opened */
/*                for read or write access, either by CKLPF, DAFOPR, */
/*                or DAFOPW. */

/*     DESCR      is the packed descriptor of the data type 2 segment. */

/*     RECNO      is the number of the individual pointing record to be */
/*                returned from the data type 2 segment. */

/* $ Detailed_Output */

/*     RECORD     is the pointing record indexed by RECNO in the segment. */
/*                The contents are as follows: */

/*                   RECORD( 1  ) = start SCLK time of interval */
/*                   RECORD( 2  ) = end SCLK time of interval */
/*                   RECORD( 3  ) = seconds per tick rate */

/*                   RECORD( 4  ) = q0 */
/*                   RECORD( 5  ) = q1 */
/*                   RECORD( 6  ) = q2 */
/*                   RECORD( 7  ) = q3 */

/*                   RECORD( 8  ) = av1 */
/*                   RECORD( 9  ) = av2 */
/*                   RECORD( 10 ) = av3 */


/*                See the section on data type 2 in the CK Required */
/*                Reading for a complete description on how pointing */
/*                is obtained from a type 2 record. */

/*                Note that the RECORD returned by this routine is */
/*                slightly different from that returned by CKR02. */
/*                The second element of the record returned by CKR02 */
/*                contains the SCLK time at which pointing was */
/*                requested, whereas this routine returns the SCLK */
/*                time of the right endpoint of the interval for which */
/*                the constant angular velocity model is valid. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the segment is not of data type 2, the error */
/*         SPICE(CKWRONGDATATYPE) is signalled. */

/*     2)  If RECNO is less than one or greater than the number of */
/*         records in the specified segment, the error */
/*         SPICE(CKNONEXISTREC) is signalled. */

/*     3)  If the specified handle does not belong to any file that is */
/*         currently known to be open, an error is diagnosed by a */
/*         routine that this routine calls. */

/*     4)  If DESCR is not a valid descriptor of a segment in the CK */
/*         file specified by HANDLE, the results of this routine are */
/*         unpredictable. */

/* $ Files */

/*     The file specified by HANDLE should be open for read or write */
/*     access. */

/* $ Particulars */

/*     For a detailed description of the structure of a type 2 segment, */
/*     see the CK Required Reading. */

/*     This is a utility routine that may be used to read the individual */
/*     pointing records that make up a data type 2 segment. It is */
/*     normally used in combination with CKNR02, which gives the number */
/*     of pointing instances stored in a segment. */

/* $ Examples */

/*     Suppose GLL_PLT.BC is a CK file that contains segments of data */
/*     type 2. Then the following code fragment uses CKNR02 and CKGR02 */
/*     to extract each pointing record in the first segment in the file. */


/*           INTEGER               ICD     ( 6 ) */
/*           INTEGER               HANDLE */
/*           INTEGER               NREC */
/*           INTEGER               I */

/*           DOUBLE PRECISION      DCD     ( 2  ) */
/*           DOUBLE PRECISION      DESCR   ( 5  ) */
/*           DOUBLE PRECISION      RECORD  ( 10 ) */

/*           LOGICAL               FOUND */

/*     C */
/*     C     First load the file. (The file may also be opened by using */
/*     C     CKLPF.) */
/*     C */
/*           CALL DAFOPR ( 'GLL_PLT.BC', HANDLE ) */

/*     C */
/*     C     Begin forward search.  Find the first array. */
/*     C */
/*           CALL DAFBFS ( HANDLE ) */
/*           CALL DAFFNA ( FOUND  ) */

/*     C */
/*     C     Get segment descriptor. */
/*     C */
/*           CALL DAFGS ( DESCR ) */

/*     C */
/*     C     Unpack the segment descriptor into its double precision */
/*     C     and integer components. */
/*     C */
/*           CALL DAFUS ( DESCR, 2, 6, DCD, ICD ) */

/*     C */
/*     C     The data type for a segment is located in the third integer */
/*     C     component of the descriptor. */
/*     C */
/*           IF ( ICD( 3 ) .EQ. 2 ) THEN */

/*     C */
/*     C        How many records does this segment contain? */
/*     C */
/*              CALL CKNR02 ( HANDLE, DESCR, NREC ) */

/*              DO I = 1, NREC */

/*     C */
/*     C           Get the Ith record in the segment. */
/*     C */
/*                 CALL CKGR02 ( HANDLE, DESCR, I, RECORD ) */
/*     C */
/*     C           Process the pointing data. */
/*     C */
/*                 . */
/*                 . */
/*                 . */

/*              END DO */

/*           END IF */

/* $ Restrictions */

/*     1) The binary CK file containing the segment whose descriptor was */
/*        passed to this routine must be opened for read or write access */
/*        by either CKLPF, DAFOPR, or DAFOPW. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J.M. Lynch (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 07-SEP-2001 (EDW) */

/*        Replaced DAFRDA call with DAFGDA. */
/*        Added IMPLICIT NONE. */

/* -    SPICELIB Version 1.0.0, 25-NOV-1992 (JML) */

/* -& */
/* $ Index_Entries */

/*     get ck type_2 record */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*        NDC        is the number of double precision components in an */
/*                   unpacked C-kernel segment descriptor. */

/*        NIC        is the number of integer components in an unpacked */
/*                   C-kernel segment descriptor. */

/*        PSIZ       is the number of double precision numbers making up */
/*                   the quaternion, angular velocity, and seconds per */
/*                   tick rate portion of a pointing record. */

/*        DTYPE      is the data type. */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("CKGR02", (ftnlen)6);
    }


/*     The unpacked descriptor contains the following information */
/*     about the segment: */

/*        DCD(1)  Initial encoded SCLK */
/*        DCD(2)  Final encoded SCLK */
/*        ICD(1)  Instrument */
/*        ICD(2)  Inertial reference frame */
/*        ICD(3)  Data type */
/*        ICD(4)  Angular velocity flag */
/*        ICD(5)  Initial address of segment data */
/*        ICD(6)  Final address of segment data */

    dafus_(descr, &c__2, &c__6, dcd, icd);
    if (icd[2] != 2) {
	setmsg_("Data type of the segment should be 2: Passed descriptor sho"
		"ws type = #.", (ftnlen)71);
	errint_("#", &icd[2], (ftnlen)1);
	sigerr_("SPICE(CKWRONGDATATYPE)", (ftnlen)22);
	chkout_("CKGR02", (ftnlen)6);
	return 0;
    }

/*     Find out how many pointing instances there are in the segment. */

    cknr02_(handle, descr, &nrec);

/*     If a request was made for a record which doesn't exist, then */
/*     signal an error and leave. */

    if (*recno < 1 || *recno > nrec) {
	setmsg_("Requested record number (#) does not exist. There are # rec"
		"ords in the segment.", (ftnlen)79);
	errint_("#", recno, (ftnlen)1);
	errint_("#", &nrec, (ftnlen)1);
	sigerr_("SPICE(CKNONEXISTREC)", (ftnlen)20);
	chkout_("CKGR02", (ftnlen)6);
	return 0;
    }

/*     The address of the first double precision number in the array */
/*     is stored in the fifth integer component of the descriptor. */

    beg = icd[4];

/*     Get the pointing record indexed by RECNO. */

    addr__ = beg + (*recno - 1 << 3);
    i__1 = addr__ + 7;
    dafgda_(handle, &addr__, &i__1, prec);
    record[2] = prec[7];
    moved_(prec, &c__7, &record[3]);

/*     Next get the interval start time.  Need to go past all of the */
/*     NREC pointing records (PSIZ * NREC numbers), and then to the */
/*     RECNOth SCLK start time. */

    addr__ = beg + (nrec << 3) + *recno - 1;
    dafgda_(handle, &addr__, &addr__, record);

/*     Next get the interval stop time.  Need to go past all of the */
/*     NREC pointing records and start times ( (PSIZ+1)*NREC numbers ), */
/*     and then to the RECNOth SCLK stop time. */

    addr__ = beg + nrec * 9 + *recno - 1;
    dafgda_(handle, &addr__, &addr__, &record[1]);
    chkout_("CKGR02", (ftnlen)6);
    return 0;
} /* ckgr02_ */

