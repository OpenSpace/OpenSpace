/* spkw03.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__40 = 40;
static integer c__2 = 2;
static integer c__6 = 6;
static integer c__1 = 1;

/* $Procedure SPKW03 ( Write SPK segment, type 3 ) */
/* Subroutine */ int spkw03_(integer *handle, integer *body, integer *center, 
	char *frame, doublereal *first, doublereal *last, char *segid, 
	doublereal *intlen, integer *n, integer *polydg, doublereal *cdata, 
	doublereal *btime, ftnlen frame_len, ftnlen segid_len)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i__, k;
    extern /* Subroutine */ int etcal_(doublereal *, char *, ftnlen), chkin_(
	    char *, ftnlen), dafps_(integer *, integer *, doublereal *, 
	    integer *, doublereal *);
    doublereal descr[5];
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen);
    doublereal ltime;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    doublereal rsize;
    char etstr[40];
    extern /* Subroutine */ int dafada_(doublereal *, integer *), dafbna_(
	    integer *, doublereal *, char *, ftnlen), dafena_(void);
    extern logical failed_(void);
    extern /* Subroutine */ int chckid_(char *, integer *, char *, ftnlen, 
	    ftnlen);
    integer refcod, ninrec;
    extern /* Subroutine */ int namfrm_(char *, integer *, ftnlen);
    doublereal radius, numrec;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);
    char netstr[40];
    doublereal dcd[2];
    integer icd[6];
    doublereal mid;

/* $ Abstract */

/*    Write a type 3 segment to an SPK file. */

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

/*     NAIF_IDS */
/*     SPC */
/*     SPK */

/* $ Keywords */

/*     EPHEMERIS */

/* $ Declarations */
/* $ Brief_I/O */

/*   Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of SPK file open for writing. */
/*     BODY       I   NAIF code for ephemeris object. */
/*     CENTER     I   NAIF code for the center of motion of the body. */
/*     FRAME      I   Reference frame name. */
/*     FIRST      I   Start time of interval covered by segment. */
/*     LAST       I   End time of interval covered by segment. */
/*     SEGID      I   Segment identifier. */
/*     INTLEN     I   Length of time covered by record. */
/*     N          I   Number of records in segment. */
/*     POLYDG     I   Chebyshev polynomial degree. */
/*     CDATA      I   Array of Chebyshev coefficients. */
/*     BTIME      I   Begin time of first record. */

/* $ Detailed_Input */

/*     HANDLE         DAF handle of an SPK file to which a type 3 segment */
/*                    is to be added.  The SPK file must be open for */
/*                    writing. */

/*     BODY           NAIF integer code for an ephemeris object whose */
/*                    state relative to another body is described by the */
/*                    segment to be created. */

/*     CENTER         NAIF integer code for the center of motion of the */
/*                    object identified by BODY. */

/*     FRAME          NAIF name for a reference frame relative to which */
/*                    the state information for BODY is specified. */

/*     FIRST, */
/*     LAST           Start and stop times of the time interval over */
/*                    which the segment defines the state of body. */

/*     SEGID          Segment identifier.  An SPK segment identifier may */
/*                    contain up to 40 characters. */

/*     INTLEN         Length of time, in seconds, covered by each set of */
/*                    Chebyshev polynomial coefficients (each logical */
/*                    record).  Each set of Chebyshev coefficents must */
/*                    cover this fixed time interval, INTLEN. */

/*     N              Number of sets of Chebyshev polynomial coefficients */
/*                    for coordinates and their derivatives (number of */
/*                    logical records) to be stored in the segment. */
/*                    There is one set of Chebyshev coefficients for each */
/*                    time period. */

/*     POLYDG         Degree of each set of Chebyshev polynomials. */

/*     CDATA          Array containing all the sets of Chebyshev */
/*                    polynomial coefficients to be placed in the */
/*                    segment of the SPK file.  The coefficients are */
/*                    stored in CDATA in order as follows: */

/*                       the (degree + 1) coefficients for the first */
/*                       coordinate of the first logical record */

/*                       the coefficients for the second coordinate */

/*                       the coefficients for the third coordinate */

/*                       the coefficients for the derivative of the first */
/*                       coordinate */

/*                       the coefficients for the derivative of the */
/*                       second coordinate */

/*                       the coefficients for the derivative of the third */
/*                       coordinate */

/*                       the coefficients for the first coordinate for */
/*                       the second logical record, ... */

/*                       and so on. */


/*     BTIME          Begin time (seconds past J2000 TDB) of first set */
/*                    of Chebyshev polynomial coefficients (first */
/*                    logical record). */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the number of sets of coefficients is not positive */
/*        'SPICE(NUMCOEFFSNOTPOS)' is signalled. */

/*     2) If the interval length is not positive, 'SPICE(INTLENNOTPOS)' */
/*        is signalled. */

/*     3) If the name of the reference frame is not recognized, */
/*        'SPICE(INVALIDREFFRAME)' is signalled. */

/*     4) If segment stop time is not greater then the begin time, */
/*        'SPICE(BADDESCRTIMES)' is signalled. */

/*     5) If the start time of the first record is not less than */
/*        or equal to the descriptor begin time, 'SPICE(BADDESCRTIMES)' */
/*        is signalled. */

/*     6) If the end time of the last record is not greater than */
/*        or equal to the descriptor end time, 'SPICE(BADDESCRTIMES)' is */
/*        signalled. */

/* $ Files */

/*     A new type 3 SPK segment is written to the SPK file attached */
/*     to HANDLE. */

/* $ Particulars */

/*     This routine writes an SPK type 3 data segment to the designated */
/*     SPK file, according to the format described in the SPK Required */
/*     Reading. */

/*     Each segment can contain data for only one target, central body, */
/*     and reference frame.  The Chebyshev polynomial degree and length */
/*     of time covered by each logical record are also fixed.  However, */
/*     an arbitrary number of logical records of Chebyshev polynomial */
/*     coefficients can be written in each segment.  Minimizing the */
/*     number of segments in an SPK file will help optimize how the SPICE */
/*     system accesses the file. */

/* $ Examples */

/*     Suppose that you have sets of Chebyshev polynomial coefficients */
/*     in an array CDATA pertaining to the position of the moon (NAIF ID */
/*     = 301), relative to the Earth-moon barycenter (NAIF ID = 3), in */
/*     the J2000 reference frame, and want to put these into a type 2 */
/*     segment in an existing SPK file.  The following code could be used */
/*     to add one new type 2 segment.  To add multiple segments, put the */
/*     call to SPKW02 in a loop. */

/*     C */
/*     C      First open the SPK file and get a handle for it. */
/*     C */
/*            CALL DAFOPW ( SPKNAM, HANDLE ) */

/*     C */
/*     C      Create a segment identifier. */
/*     C */
/*            SEGID = 'MY_SAMPLE_SPK_TYPE_3_SEGMENT' */

/*     C */
/*     C      Write the segment. */

/*            SUBROUTINE SPKW03 ( HANDLE, 301,    3,      'J2000', */
/*          .                     FIRST,  LAST,   SEGID,  INTLEN, */
/*          .                     N,      POLYDG, CDATA,  BTIME ) */

/*     C */
/*     C      Close the file. */
/*     C */
/*            CALL DAFCLS ( HANDLE ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     K.S. Zukor (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 30-OCT-2006 (BVS) */

/*        Removed restriction that the input reference frame should be */
/*        inertial by changing the routine that determins the frame ID */
/*        from the name from IRFNUM to NAMFRM. */

/* -    SPICELIB Version 1.0.1, 19-SEP-2006 (EDW) */

/*        Corrected typo in the section name ("Example" to "Examples"). */

/* -    SPICELIB Version 1.0.0, 01-AUG-1995 (KSZ) */

/* -& */
/* $ Index_Entries */

/*     write spk type_3 data segment */

/* -& */

/*     SPICELIB functions */


/*     Local Parameters */

/*     DTYPE is the SPK data type. */


/*     ND is the number of double precision components in an SPK */
/*     segment descriptor. SPK uses ND = 2. */


/*     NI is the number of integer components in an SPK segment */
/*     descriptor. SPK uses NI = 6. */


/*     NS is the size of a packed SPK segment descriptor. */


/*     SIDLEN is the maximum number of characters allowed in an */
/*     SPK segment identifier. */


/*     Local variables */



/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SPKW03", (ftnlen)6);
    }

/*     The number of sets of coefficients must be positive. */

    if (*n <= 0) {
	setmsg_("The number of sets of coordinatecoeffcients is not positive"
		". N = #", (ftnlen)66);
	errint_("#", n, (ftnlen)1);
	sigerr_("SPICE(NUMCOEFFSNOTPOS)", (ftnlen)22);
	chkout_("SPKW03", (ftnlen)6);
	return 0;
    }

/*    The interval length must be positive. */

    if (*intlen <= 0.) {
	setmsg_("The interval length is not positive.N = #", (ftnlen)41);
	errdp_("#", intlen, (ftnlen)1);
	sigerr_("SPICE(INTLENNOTPOS)", (ftnlen)19);
	chkout_("SPKW03", (ftnlen)6);
	return 0;
    }

/*     Get the NAIF integer code for the reference frame. */

    namfrm_(frame, &refcod, frame_len);
    if (refcod == 0) {
	setmsg_("The reference frame # is not supported.", (ftnlen)39);
	errch_("#", frame, (ftnlen)1, frame_len);
	sigerr_("SPICE(INVALIDREFFRAME)", (ftnlen)22);
	chkout_("SPKW03", (ftnlen)6);
	return 0;
    }

/*     The segment stop time must be greater than the begin time. */

    if (*first > *last) {
	setmsg_("The segment descriptor start time: # is greater than the se"
		"gment end time: #", (ftnlen)76);
	etcal_(first, etstr, (ftnlen)40);
	errch_("#", etstr, (ftnlen)1, (ftnlen)40);
	etcal_(last, netstr, (ftnlen)40);
	errch_("#", netstr, (ftnlen)1, (ftnlen)40);
	sigerr_("SPICE(BADDESCRTIMES)", (ftnlen)20);
	chkout_("SPKW03", (ftnlen)6);
	return 0;
    }

/*     The begin time of the first record must be less than or equal */
/*     to the begin time of the segment. */

    if (*first < *btime) {
	setmsg_("The segment descriptor start time: # is less than the begin"
		"ning time of the segment data: #", (ftnlen)91);
	etcal_(first, etstr, (ftnlen)40);
	errch_("#", etstr, (ftnlen)1, (ftnlen)40);
	etcal_(btime, etstr, (ftnlen)40);
	errch_("#", etstr, (ftnlen)1, (ftnlen)40);
	sigerr_("SPICE(BADDESCRTIMES)", (ftnlen)20);
	chkout_("SPKW03", (ftnlen)6);
	return 0;
    }

/*     The end time of the final record must be greater than or */
/*     equal to the end time of the segment. */

    ltime = *btime + *n * *intlen;
    if (*last > ltime) {
	setmsg_("The segment descriptor end time: # is greater than the end "
		"time of the segmentdata: #", (ftnlen)85);
	etcal_(last, etstr, (ftnlen)40);
	errch_("#", etstr, (ftnlen)1, (ftnlen)40);
	etcal_(&ltime, etstr, (ftnlen)40);
	errch_("#", etstr, (ftnlen)1, (ftnlen)40);
	sigerr_("SPICE(BADDESCRTIMES)", (ftnlen)20);
	chkout_("SPKW03", (ftnlen)6);
	return 0;
    }

/*     Now check the validity of the segment identifier. */

    chckid_("SPK segment identifier", &c__40, segid, (ftnlen)22, segid_len);
    if (failed_()) {
	chkout_("SPKW03", (ftnlen)6);
	return 0;
    }

/*     Store the start and end times to be associated */
/*     with this segment. */

    dcd[0] = *first;
    dcd[1] = *last;

/*     Create the integer portion of the descriptor. */

    icd[0] = *body;
    icd[1] = *center;
    icd[2] = refcod;
    icd[3] = 3;

/*     Pack the segment descriptor. */

    dafps_(&c__2, &c__6, dcd, icd, descr);

/*     Begin a new segment of SPK type 3 form: */

/*        Record 1 */
/*        Record 2 */
/*        ... */
/*        Record N */
/*        INIT       ( initial epoch of first record ) */
/*        INTLEN     ( length of interval covered by each record ) */
/*        RSIZE      ( number of data elements in each record ) */
/*        N          ( number of records in segment ) */

/*     Each record will have the form: */

/*        MID        ( midpoint of time interval ) */
/*        RADIUS     ( radius of time interval ) */
/*        X coefficients, Y coefficients, Z coefficients */
/*        X' coefficients, Y' coefficents, Z' coefficients */

    dafbna_(handle, descr, segid, segid_len);

/*     Calculate the number of Chebyshev coefficients in a record. */

    ninrec = (*polydg + 1) * 6;

/*     Fill segment with N records of data. */

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Calculate the midpoint and radius of the time of each */
/*        record, and put that at the beginning of each record. */

	radius = *intlen / 2;
	mid = *btime + radius + (i__ - 1) * *intlen;
	dafada_(&mid, &c__1);
	dafada_(&radius, &c__1);

/*        Put one set of coefficients into the segment. */

	k = (i__ - 1) * ninrec + 1;
	dafada_(&cdata[k - 1], &ninrec);
    }

/*     Store the initial epoch of the first record. */

    dafada_(btime, &c__1);

/*     Store the length of interval covered by each record. */

    dafada_(intlen, &c__1);

/*     Store the size of each record (total number of array elements). */

    rsize = (doublereal) (ninrec + 2);
    dafada_(&rsize, &c__1);

/*     Store the number of records contained in the segment. */

    numrec = (doublereal) (*n);
    dafada_(&numrec, &c__1);

/*     End this segment. */

    dafena_();
    chkout_("SPKW03", (ftnlen)6);
    return 0;
} /* spkw03_ */

