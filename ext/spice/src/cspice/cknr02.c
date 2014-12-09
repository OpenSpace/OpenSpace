/* cknr02.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;

/* $Procedure      CKNR02 ( C-kernel, number of records, type 02 ) */
/* Subroutine */ int cknr02_(integer *handle, doublereal *descr, integer *
	nrec)
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    integer i_dnnt(doublereal *);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), dafus_(doublereal *, 
	    integer *, integer *, doublereal *, integer *), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen), 
	    errint_(char *, integer *, ftnlen);
    integer arrsiz;
    extern logical return_(void);
    doublereal dcd[2];
    integer beg, icd[6], end;

/* $ Abstract */

/*     Given the handle of a CK file and the descriptor of a type 2 */
/*     segment in that file, return the number of pointing records */
/*     in that segment. */

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
/*     DESCR      I   The descriptor of the type 2 segment. */
/*     NREC       O   The number of records in the segment. */

/* $ Detailed_Input */

/*     HANDLE     is the handle of the binary CK file containing the */
/*                segment. The file should have been opened for read */
/*                or write access, either by CKLPF, DAFOPR, or DAFOPW. */

/*     DESCR      The packed descriptor of a data type 2 segment. */

/* $ Detailed_Output */

/*     NREC       The number of pointing records in the type 2 segment */
/*                associated with HANDLE and DESCR. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the segment indicated by DESCR is not a type 2 segment, */
/*         the error 'SPICE(CKWRONGDATATYPE)' is signalled. */

/*     2)  If the specified handle does not belong to any file that is */
/*         currently known to be open, an error is diagnosed by a */
/*         routine that this routine calls. */

/*     3)  If DESCR is not a valid descriptor of a segment in the CK */
/*         file specified by HANDLE, the results of this routine are */
/*         unpredictable. */

/* $ Files */

/*     The file specified by HANDLE should be open for read or write */
/*     access. */

/* $ Particulars */

/*     For a complete description of the internal structure of a type 2 */
/*     segment, see the CK required reading. */

/*     This routine returns the number of pointing records contained */
/*     in the specified segment. It is normally used in conjunction */
/*     with CKGR02, which returns the Ith record in the segment. */

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
/*     C     First load the file. ( The file may also be opened by using */
/*     C     CKLPF. ) */
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
/*        by either CKLPF, DAFOPR, DAFOPW. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     J.M. Lynch (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 25-NOV-1992 (JML) */

/* -& */
/* $ Index_Entries */

/*     number of ck type_2 records */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*        NDC        is the number of double precision components in an */
/*                   unpacked C-kernel descriptor. */

/*        NIC        is the number of integer components in an unpacked */
/*                   C-kernel descriptor. */

/*        DTYPE      is the data type. */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("CKNR02", (ftnlen)6);
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

/*     If this segment is not of data type 2, then signal an error. */

    if (icd[2] != 2) {
	setmsg_("Data type of the segment should be 2: Passed descriptor sho"
		"ws type = #.", (ftnlen)71);
	errint_("#", &icd[2], (ftnlen)1);
	sigerr_("SPICE(CKWRONGDATATYPE)", (ftnlen)22);
	chkout_("CKNR02", (ftnlen)6);
	return 0;
    }

/*     The beginning and ending addresses of the segment are in the */
/*     descriptor. */

    beg = icd[4];
    end = icd[5];

/*     Calculate the number of pointing records in the segment from */
/*     the physical size of the segment and knowledge of its structure. */

/*        Based on the structure of a type 2 segment, the size of a */
/*        segment with N pointing intervals is given as follows: */

/*           ARRSIZ  =  PSIZ * N  +  2 * N  +  ( N-1 ) / 100       (1) */

/*        In the above equation PSIZ is eight and integer arithmetic is */
/*        used.  This equation is equivalent to: */


/*           100 * ARRSIZ  =  1000 * N  + ( N-1 ) * 100            (2) */
/*                                        ------- */
/*                                          100 */

/*        If we can eliminate the integer division then, since all of */
/*        the other values represent whole numbers, we can solve the */
/*        equation for N in terms of ARRSIZ by using double precision */
/*        arithmetic and then rounding the result to the nearest integer. */

/*        This next equation uses double precision arithmetic and is */
/*        equivalent to (2): */

/*           100 * ARRSIZ  = 1000 * N + ( N-1 ) - ( N-1 ) MOD 100  (3) */

/*        Which means: */

/*           100 * ARRSIZ + 1     ( N-1 ) MOD 100 */
/*           ----------------  +  ---------------   =   N          (4) */
/*                1001                 1001 */

/*         Since the second term on the left side of (4) is always less */
/*         than 0.1, the first term will always round to the correct */
/*         value of N. */

    arrsiz = end - beg + 1;
    d__1 = ((doublereal) arrsiz * 100. + 1.) / 1001.;
    *nrec = i_dnnt(&d__1);
    chkout_("CKNR02", (ftnlen)6);
    return 0;
} /* cknr02_ */

