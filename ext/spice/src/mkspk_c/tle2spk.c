/* tle2spk.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__50000 = 50000;
static integer c__0 = 0;
static integer c__399 = 399;
static integer c__1950 = 1950;
static integer c__10 = 10;

/* $Procedure TLE2SPK ( Read two-line element set and create type 10 SPK ) */
/* Subroutine */ int tle2spk_(char *inpfn, integer *obidvl, integer *cnidvl, 
	char *frnmvl, char *sgidvl, integer *handle, doublereal *covval, char 
	*covtyp, ftnlen inpfn_len, ftnlen frnmvl_len, ftnlen sgidvl_len, 
	ftnlen covtyp_len)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_rnge(char *, integer, 
	    char *, integer);

    /* Local variables */
    static char code[32];
    static logical done;
    static char type__[1];
    extern /* Subroutine */ int zzgetelm_(integer *, char *, doublereal *, 
	    doublereal *, logical *, char *, ftnlen, ftnlen);
    static integer i__, j, n;
    extern logical elemd_(doublereal *, doublereal *);
    extern /* Subroutine */ int etcal_(doublereal *, char *, ftnlen), chkin_(
	    char *, ftnlen), errch_(char *, char *, ftnlen, ftnlen);
    static doublereal elems[500010];
    extern doublereal dpmin_(void), dpmax_(void);
    static char lines[512*2];
    static logical found;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen), moved_(
	    doublereal *, integer *, doublereal *);
    static char hword[32];
    static integer total;
    extern /* Subroutine */ int spkw10_(integer *, integer *, integer *, char 
	    *, doublereal *, doublereal *, char *, doublereal *, integer *, 
	    doublereal *, doublereal *, ftnlen, ftnlen);
    static char error[512*2], outfn[255];
    extern integer rtrim_(char *, ftnlen);
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    static char chose1[32], chose2[32];
    static doublereal begpad;
    extern /* Subroutine */ int dafhfn_(integer *, char *, ftnlen);
    static logical ok;
    static doublereal endpad;
    static logical begbol;
    extern /* Subroutine */ int scardd_(integer *, doublereal *);
    extern logical return_(void);
    static integer frcode, framid;
    static doublereal dupchk[50006], epochs[50001], geophs[8], begint, endint,
	     svepch, bsavep, bsavel[10], esavep, esavel[10];
    static logical eof, fmodel, priors, endbol;
    static integer iorder[50000];
    static char geolst[255];
    static logical bsavfl, esavfl;
    extern /* Subroutine */ int ssized_(integer *, doublereal *), setmsg_(
	    char *, ftnlen), sigerr_(char *, ftnlen), dafcls_(integer *), 
	    delfil_(char *, ftnlen), errint_(char *, integer *, ftnlen), 
	    namfrm_(char *, integer *, ftnlen), intstr_(integer *, char *, 
	    ftnlen), replch_(char *, char *, char *, char *, ftnlen, ftnlen, 
	    ftnlen, ftnlen), rdtext_(char *, char *, logical *, ftnlen, 
	    ftnlen), dtpool_(char *, logical *, integer *, char *, ftnlen, 
	    ftnlen), suffix_(char *, integer *, char *, ftnlen, ftnlen), 
	    bodvar_(integer *, char *, integer *, doublereal *, ftnlen), 
	    insrtd_(doublereal *, doublereal *), removd_(doublereal *, 
	    doublereal *), orderd_(doublereal *, integer *, integer *), 
	    reordd_(integer *, integer *, doublereal *), reorbd_(integer *, 
	    integer *, integer *, doublereal *), tostdo_(char *, ftnlen), 
	    cltext_(char *, ftnlen), chkout_(char *, ftnlen);

/* $ Abstract */

/*     This routine is a module of the MKSPK program. It creates an SPK */
/*     file from a file containing the NORAD "two-line element sets". */

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

/*     MKSPK User's Guide */

/* $ Keywords */

/*     None. */

/* $ Declarations */
/* $ Abstract */

/*     MKSPK Include File. */

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

/* $ Author_and_Institution */

/*     N.G. Khavenson (IKI RAS, Russia) */
/*     B.V. Semenov   (NAIF, JPL) */

/* $ Version */

/* -    Version 1.3.0, 08-FEB-2012 (BVS). */

/*        Added TLE coverage and ID keywords. Added default TLE pad */
/*        parameter. */

/* -    Version 1.2.0, 16-JAN-2008 (BVS). */

/*        Added ETTMWR parameter */

/* -    Version 1.1.0, 05-JUN-2001 (BVS). */

/*        Added MAXDEG parameter. */

/* -    Version 1.0.4, 21-MAR-2001 (BVS). */

/*        Added parameter for command line flag '-append' indicating */
/*        that appending to an existing output file was requested. */
/*        Added corresponding setup file keyword ('APPEND_TO_OUTPUT'.) */
/*        Added parameters for yes and no values of this keyword. */

/* -    Version 1.0.3, 28-JAN-2000 (BVS). */

/*        Added parameter specifying number of supported input data */
/*        types and parameter specifying number of supported output SPK */
/*        types */

/* -    Version 1.0.2, 22-NOV-1999 (NGK). */

/*        Added parameters for two-line elements processing. */

/* -    Version 1.0.1, 18-MAR-1999 (BVS). */

/*        Added usage, help and template displays. Corrected comments. */

/* -    Version 1.0.0,  8-SEP-1998 (NGK). */

/* -& */

/*     Begin Include Section:  MKSPK generic parameters. */


/*     Maximum number of states allowed per one segment. */


/*     String size allocation parameters */


/*     Length of buffer for input text processing */


/*     Length of a input text line */


/*     Length of file name and comment line */


/*     Length of string for keyword value processing */


/*     Length of string for word processing */


/*     Length of data order parameters string */


/*     Length of string reserved as delimiter */


/*     Numbers of different parameters */



/*     Maximum number of allowed comment lines. */


/*     Reserved number of input parameters */


/*     Full number of delimiters */


/*     Number of delimiters that may appear in time string */


/*     Command line flags */


/*     Setup file keywords reserved values */


/*     Standard YES and NO values for setup file keywords. */


/*     Number of supported input data types and input DATA TYPE */
/*     reserved values. */


/*     Number of supported output SPK data types -- this version */
/*     supports SPK types 5, 8, 9, 10, 12, 13, 15 and 17. */


/*     End of input record marker */


/*     Maximum allowed polynomial degree. The value of this parameter */
/*     is consistent with the ones in SPKW* routines. */


/*     Special time wrapper tag for input times given as ET seconds past */
/*     J2000 */


/*     Default TLE pad, 1/2 day in seconds. */


/*     End Include Section:  MKSPK generic parameters. */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     INPFN      I   Input file name */
/*     OBIDVL     I   Input TLE object ID and output SPK object ID */
/*     CNIDVL     I   Center ID NAIF code */
/*     FRNMVL     I   Reference frame name of output SPK */
/*     SGIDVL     I   Segment identifier */
/*     HANDLE     I   Handle of an SPK file open for writing. */
/*     COVVAL     I   Coverage start and stop times or pads. */
/*     COVTYP     I   Coverage value types ('TIME' or 'PAD'). */

/* $ Detailed_Input */

/*     INPFN       is the name of input file containing the NORAD */
/*                 "two-line element sets" */

/*     FRNMVL      is the reference frame that output states are */
/*                 referenced to. It must be 'J2000'. */

/*     OBIDVL      is a two-element array containing TLE object ID and */
/*                 SPK object ID. The first element is the TLE object ID */
/*                 -- object ID to look for in the input TLE file. The */
/*                 second element is the SPK object ID -- the NAIF ID to */
/*                 use in the output SPK file. */

/*     CNIDVL      is the NAIF ID for the center body. It must be 399 */
/*                 corresponding to Earth. */

/*     SGIDVL      is identifier of segment stored in output file. */

/*     HANDLE      is the file handle of an SPK file that has been */
/*                 opened for writing. */

/*     COVVAL      is a two-element array specifying coverage start and */
/*                 stop times or pads. The first element is either the */
/*                 start time or start pad. The second element is either */
/*                 the stop time or stop pad. */

/*                 The start and stop times are given as TDB seconds */
/*                 past J2000. If specified, they are used as the start */
/*                 and stop times of the output SPK. */

/*                 The start and stop pad are given durations in TDB */
/*                 seconds. If specified, they are applied to the */
/*                 earliest and latest input TLE times to extend or */
/*                 contract the output SPK coverage. Positive pad values */
/*                 extend coverage outwards; negative pad values */
/*                 contract coverage inwards. */

/*                 Combinations of the start time and stop pad and */
/*                 the start pad and stop time are permitted. */

/*     COVTYP      is a two-element array specifying whether the */
/*                 corresponding elements of COVVAL are times of pads. */

/*                 The first element specifies whether COVVAL(1) is the */
/*                 start time (COVTYP(1)='TIME') or start pad */
/*                 (COVTYP(1)='PAD'). */

/*                 The second element specifies whether COVVAL(2) is */
/*                 the stop time (COVTYP(2)='TIME') or stop pad */
/*                 (COVTYP(2)='PAD') */

/* $ Detailed_Output */

/*     None.       The data input is stored in an SPK segment in the */
/*                 DAF connected to the input HANDLE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the center body of the motion is not the Earth, then */
/*        the error SPICE(INCONSISTCENTERID) will signal. */

/*     2) If the reference frame is not 'J2000', then the error */
/*        SPICE(INCONSISTFRAME) will be signaled. */

/*     3) If code of requested space object is not found in the */
/*        input file, then the error SPICE(NOTLEDATAFOROBJECT) */
/*        will signal. */

/*     4) If second line of two-line element records does not exist, */
/*        then the error SPICE(NOSECONDLINE) will signal. */

/*     5) If second line of two-line element records does not exist at */
/*        the end of the file, then the error SPICE(NOSECONDLINE2) */
/*        will signal. */

/*     6) If any one of the required geophysical constants was not */
/*        found in the POOL, then the error SPICE(MISSINGGEOCONSTS) */
/*        will signal. */

/*     7) The error SPICE(INVALIDTLEORDER) signals if the epoch used */
/*        to provide coverage across a segment boundary has a date-time */
/*        before (earlier than) the epochs of the previous segment. */

/*     8) The error SPICE(INVALIDVALUE) signals if a time pad value is */
/*        not positive definite. */

/*     9) SPICE(BADTLE) if TLE set has incorrect format. */

/*     10) SPICE(BADTLEPADS) if both pads are zero for single TLE. */

/*     11) SPICE(BADSTOPTIME) if specified stop time before computed */
/*         start time. */

/*     12) SPICE(BADSTARTTIME) if specified start time after computed */
/*         stop time. */

/*     13) SPICE(INVALIDVALUE1) if specified start pad is negative. */

/*     14) SPICE(INVALIDINPUT1) if first coverage type is not 'TIME' */
/*         or 'PAD'. */

/*     15) SPICE(INVALIDVALUE2) if specified stop pad is negative. */

/*     16) SPICE(INVALIDINPUT2) if second coverage type is not 'TIME' */
/*         or 'PAD'. */

/* $ Files */

/*     This routine read text data from the input data file INPFN */
/*     containing two-line element set file in standard text format. */

/*     Leapsecond Kernel (LSK) file must be loaded before running */
/*     this routine. */

/*     A geophysical constants file for the Earth must be loaded */
/*     before running this routine. */

/*        The geophysical constants kernel should contain */
/*        the following variables: */

/*        BODY399_J2 --- J2 gravitational harmonic for earth. */
/*        BODY399_J3 --- J3 gravitational harmonic for earth. */
/*        BODY399_J4 --- J4 gravitational harmonic for earth. */
/*        BODY399_KE --- Square root of the GM for earth where GM */
/*                       is expressed in earth radii cubed */
/*                       per minutes squared. */
/*        BODY399_ER --- Equatorial radius of the earth in km. */
/*        BODY399_S0 --- Low altitude bound for atmospheric model in km. */
/*        BODY399_Q0 --- High altitude bound for atmospheric model in km. */
/*        BODY399_AE --- Distance units/earth radius (normally 1). */


/*     The program creates SPK file connected to HANDLE. */
/*     This file must be opened for writing before calling this */
/*     routine. */

/* $ Particulars */

/*     1) S_i > S_i+1 check: */

/*     The TLE2SPK logic can process a data file of arbitrary size */
/*     (within limits of memory) in a read-sort-write loop procedure */
/*     with each read-sort-write iteration acting on MAXEPC records */
/*     or less. */

/*     Given a TLE data file S, comprised of N sets of MAXEPC records, */
/*     S_1, S_2, .., S_N, (S_N may have less than MAXEPC records) */
/*     TLE2SPK reads S_i, sorts the records in ascending order by epoch, */
/*     then writes a type 10 segment to the SPK. Therefor, all sorting */
/*     applies locally to S_i not globally to S. The logic implicitly */
/*     assumes all epochs in S_i < all epochs in S_i+1. To ensure no */
/*     gaps between SPK segments written from S_i and S_i+1, the routine */
/*     adds the final record of S_i to the S_i+1 record set. If */
/*     S_i < S_i+1, that record is written as the first record in the */
/*     sorted S_i+1, if S_i > S_i+1, that record is written as the final */
/*     record in the sorted S_i+1. This second condition is considered */
/*     an error (SPICE(INVALIDTLEORDER)). */


/*     2) SPK coverage pads: */

/*     The pad values have only a positive sense. Nominal use of pads */
/*     expands the SPK coverage time interval based on the epoch of the */
/*     first TLE, EPOCH(1), and the epoch of the final TLE, EPOCH(N): */

/*                  EPOCH(1)                          EPOCH(N) */
/*            ------|---------------------------------|---------------- */

/*            --|(--)---------------------------------(--)|------------ */
/*              start pad                                 end pad */

/*     The effect of the pads, start and end, are independent of the */
/*     other. Use of only a start pad on the SPK coverage interval: */


/*                  EPOCH(1)                          EPOCH(N) */
/*            ------|---------------------------------|---------------- */

/*            --|(--)---------------------------------|---------------- */
/*              start pad */

/*     Use of only an end pad on the SPK coverage interval: */

/*                  EPOCH(1)                          EPOCH(N) */
/*            ------|---------------------------------|---------------- */

/*            ------|---------------------------------(--)|------------ */
/*                                                        end pad */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.G. Khavenson (IKI RAS, Russia) */
/*     B.V. Semenov   (JPL) */
/*     W.L. Taber     (JPL) */
/*     E.D. Wright    (JPL) */

/* $ Version */

/* -    Version 3.0.0, 01-MAY-2014 (EDW)(BVS) */

/*        Changed calling sequence to pass in both TLE and SPK IDs and */
/*        start and stop times or pads. Updated to produce output SPK */
/*        with coverage consistent with the start and stop times or pads */
/*        provided as inputs. */

/*        Increase to TLE buffer length defined by parameter MAXEPC */
/*        (maximum number of records in a type 10 segment) from 5000 to */
/*        50000. Addition of error checks that signal if the epochs of */
/*        a segment buffer represent dates-times before those in the */
/*        previous segment. */

/*        Implemented a set container for record epochs to eliminate */
/*        duplicate records, as defined by duplicate epochs, prior */
/*        to processing records. */

/*        Header and comment edits to describe new functionality */
/*        and to correspond to NAIF header standards. */

/*        Eliminated use of COVPAD parameter and removed dead code. */

/*        BUG FIX: fixed to process TLE lines with spacecraft code */
/*        left-padded with zeros to five digits. */

/* -    Version 2.2.0, 06-MAR-2009 (BVS). */

/*        Encapsulated forward/backward propagation period in COVPAD */
/*        variable. */

/* -    Version 2.1.0, 18-MAR-2005 (EDW) */

/*        Corrected a logic error that prevent processing of TLEs */
/*        for vehicles with ID codes four characters or shorter. */

/* -    Version 2.0.0, 06-APR-2004 (EDW) */

/*        Modified algorithm to call ZZGETELM, a modified version of */
/*        GETELM. ZZGETELM returns a flag and explanation string */
/*        for any TLE processing error. */

/*        Correct a typo: */

/*           ENDINT   = EPOCHS(I) */

/*        to */

/*           ENDINT   = EPOCHS(PUT) */

/*        This typo could cause a TLE segment summary to report the */
/*        wrong end time for the segment data. */

/* -    Version 1.0.1, 27-JAN-2000 (BVS). */

/*        Added a little better error message for the case when */
/*        geophysical constants weren't loaded. */

/* -    Version 1.0.0, 22-NOV-1999 (NGK). */

/*        Initial release based on the  MKSPK10 utility program */
/*        Version 1.0.0, 18-JUL-1997 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Creates an SPK file from a file containing the NORAD */
/*     "two-line element sets. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Size WDSIZE, LINLEN, FILSIZE are defined in include file. */



/*     The following integers are used to mark the various */
/*     slots in the array for geophysical constants. */

/*        J2 */
/*        J3 */
/*        J4 */
/*        KE */
/*        QO */
/*        SO */
/*        ER */
/*        AE */


/*     An enumeration of the various components of the */
/*     elements array---ELEMS */

/*        KNDT20 */
/*        KNDD60 */
/*        KBSTAR */
/*        KINCL */
/*        KNODE0 */
/*        KECC */
/*        KOMEGA */
/*        KMO */
/*        KNO */


/*     The next set of parameters govern how many items will */
/*     go into a segment. */


/*     Save all. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("TLE2SPK", (ftnlen)7);
    }

/*     Assign the size for the set used to check for duplicate records. */

    ssized_(&c__50000, dupchk);
    scardd_(&c__0, dupchk);

/*     Initialize the Boolean set to true when processing a record set */
/*     to multiple segments. If true, the code logic performs the S_i > */
/*     S_i+1 check. Also initialize the saved epoch to DPMIN to avoid */
/*     compiler errors. */

    priors = FALSE_;
    svepch = dpmin_();

/*     Set flags and initial values for saved padding records. */

    bsavfl = FALSE_;
    bsavep = dpmin_();
    esavfl = FALSE_;
    esavep = dpmax_();

/*     Set times and pads based on COVTYP values. If 'TIME', zero for */
/*     pads. */

    begbol = FALSE_;
    endbol = FALSE_;
    if (eqstr_(covtyp, "TIME", covtyp_len, (ftnlen)4)) {
	begbol = TRUE_;
	begpad = 0.;
    } else if (eqstr_(covtyp, "PAD", covtyp_len, (ftnlen)3)) {
	if (covval[0] < 0.) {
	    setmsg_("Time pads must have positive definite value. Begin time"
		    " pad has value #.", (ftnlen)72);
	    errdp_("#", covval, (ftnlen)1);
	    sigerr_("SPICE(INVALIDVALUE1)", (ftnlen)20);
	}
	begpad = covval[0];
    } else {
	setmsg_("COVTYP(1) must be set to either 'TIME' or 'PAD'. It was set"
		" to '#'.", (ftnlen)67);
	errch_("#", covtyp, (ftnlen)1, covtyp_len);
	sigerr_("SPICE(INVALIDINPUT1)", (ftnlen)20);
    }
    if (eqstr_(covtyp + covtyp_len, "TIME", covtyp_len, (ftnlen)4)) {
	endbol = TRUE_;
	endpad = 0.;
    } else if (eqstr_(covtyp + covtyp_len, "PAD", covtyp_len, (ftnlen)3)) {
	if (covval[1] < 0.) {
	    setmsg_("Time pads must have positive definite value. End time p"
		    "ad has value #.", (ftnlen)70);
	    errdp_("#", &covval[1], (ftnlen)1);
	    sigerr_("SPICE(INVALIDVALUE2)", (ftnlen)20);
	}
	endpad = covval[1];
    } else {
	setmsg_("COVTYP(2) must be set to either 'TIME' or 'PAD'. It was set"
		" to '#'.", (ftnlen)67);
	errch_("#", covtyp + covtyp_len, (ftnlen)1, covtyp_len);
	sigerr_("SPICE(INVALIDINPUT2)", (ftnlen)20);
    }

/*     Get file name of output file. */

    dafhfn_(handle, outfn, (ftnlen)255);

/*     Check center ID. */

    if (*cnidvl != 399) {

/*        The center body is not the Earth; TLE data applies only to */
/*        earth orbiters. Signal an error then return. KCENID is defined */
/*        in include file. */

	dafcls_(handle);
	delfil_(outfn, (ftnlen)255);
	setmsg_("Processing of two-line element data requires the setup file"
		" keyword '#' to be set to #.", (ftnlen)87);
	errch_("#", "CENTER_ID", (ftnlen)1, (ftnlen)9);
	errint_("#", &c__399, (ftnlen)1);
	sigerr_("SPICE(INCONSISTCENTERID)", (ftnlen)24);
    }

/*     Check reference frame. */

    namfrm_(frnmvl, &frcode, frnmvl_len);
    namfrm_("J2000", &framid, (ftnlen)5);
    if (frcode != framid) {

/*        The frame is not J2000. Complain. KRFRNM is defined in include */
/*        file. */

	dafcls_(handle);
	delfil_(outfn, (ftnlen)255);
	setmsg_("Processing of two-line element data requires the setup file"
		" keyword '#' to be set to '#'.", (ftnlen)89);
	errch_("#", "REF_FRAME_NAME", (ftnlen)1, (ftnlen)14);
	errch_("#", "J2000", (ftnlen)1, (ftnlen)5);
	sigerr_("SPICE(INCONSISTFRAME)", (ftnlen)21);
    }

/*     Convert object ID to string code that allows to chose the object */
/*     data from input file. */

    intstr_(obidvl, code, (ftnlen)32);

/*     Initialize CHOSE1 to seven characters, with the TLE line ID as */
/*     the first character. */

    s_copy(chose1, "1", (ftnlen)32, (ftnlen)1);

/*     Write the ID string CODE to CHOSE1 so that the last RTRIM(CODE) */
/*     characters of the CHOSE1(1:7) contain the code. */

    i__1 = 7 - rtrim_(code, (ftnlen)32);
    s_copy(chose1 + i__1, code, 7 - i__1, rtrim_(code, (ftnlen)32));

/*     Make another version of this lookup string with the code zero */
/*     padded on left to five digits because according the TLE spec: */

/*        The catalog number assigned to the object by the US Air Force. */
/*        The numbers are assigned sequentially as objects are */
/*        cataloged. Object numbers less then 10000 are always aligned */
/*        to the right, and padded with zeros or spaces to the left. */

/*     (Note that Ed says that it's not always the case.) */

    s_copy(chose2, chose1, (ftnlen)32, (ftnlen)32);
    replch_(chose2 + 2, " ", "0", chose2 + 2, (ftnlen)5, (ftnlen)1, (ftnlen)1,
	     (ftnlen)5);

/*     Read first line from TLE data file */

    rdtext_(inpfn, lines, &eof, inpfn_len, (ftnlen)512);

/*     Read the file until we find the first TLE line for this ID. */

    while(s_cmp(lines, chose1, (ftnlen)7, (ftnlen)7) != 0 && s_cmp(lines, 
	    chose2, (ftnlen)7, (ftnlen)7) != 0 && ! eof) {
	rdtext_(inpfn, lines, &eof, inpfn_len, (ftnlen)512);
    }
    if (eof) {

/*        Requested data was not found. Complain. */

	dafcls_(handle);
	delfil_(outfn, (ftnlen)255);
	setmsg_("No data for the object with NORAD ID # were found in the in"
		"put two-line element file.", (ftnlen)85);
	errint_("#", obidvl, (ftnlen)1);
	sigerr_("SPICE(NOTLEDATAFOROBJECT)", (ftnlen)25);
    }

/*     Check whether all geophysical constants needed for the SGP4 */
/*     propagation model are present in the kernel pool. */

    s_copy(geolst, " ", (ftnlen)255, (ftnlen)1);
    dtpool_("BODY399_J2", &found, &n, type__, (ftnlen)10, (ftnlen)1);
    if (! found || n != 1 || *(unsigned char *)type__ != 'N') {
	suffix_(" BODY399_J2,", &c__0, geolst, (ftnlen)12, (ftnlen)255);
    }
    dtpool_("BODY399_J3", &found, &n, type__, (ftnlen)10, (ftnlen)1);
    if (! found || n != 1 || *(unsigned char *)type__ != 'N') {
	suffix_(" BODY399_J3,", &c__0, geolst, (ftnlen)12, (ftnlen)255);
    }
    dtpool_("BODY399_J4", &found, &n, type__, (ftnlen)10, (ftnlen)1);
    if (! found || n != 1 || *(unsigned char *)type__ != 'N') {
	suffix_(" BODY399_J4,", &c__0, geolst, (ftnlen)12, (ftnlen)255);
    }
    dtpool_("BODY399_KE", &found, &n, type__, (ftnlen)10, (ftnlen)1);
    if (! found || n != 1 || *(unsigned char *)type__ != 'N') {
	suffix_(" BODY399_KE,", &c__0, geolst, (ftnlen)12, (ftnlen)255);
    }
    dtpool_("BODY399_QO", &found, &n, type__, (ftnlen)10, (ftnlen)1);
    if (! found || n != 1 || *(unsigned char *)type__ != 'N') {
	suffix_(" BODY399_QO,", &c__0, geolst, (ftnlen)12, (ftnlen)255);
    }
    dtpool_("BODY399_SO", &found, &n, type__, (ftnlen)10, (ftnlen)1);
    if (! found || n != 1 || *(unsigned char *)type__ != 'N') {
	suffix_(" BODY399_SO,", &c__0, geolst, (ftnlen)12, (ftnlen)255);
    }
    dtpool_("BODY399_ER", &found, &n, type__, (ftnlen)10, (ftnlen)1);
    if (! found || n != 1 || *(unsigned char *)type__ != 'N') {
	suffix_(" BODY399_ER,", &c__0, geolst, (ftnlen)12, (ftnlen)255);
    }
    dtpool_("BODY399_AE", &found, &n, type__, (ftnlen)10, (ftnlen)1);
    if (! found || n != 1 || *(unsigned char *)type__ != 'N') {
	suffix_(" BODY399_AE,", &c__0, geolst, (ftnlen)12, (ftnlen)255);
    }
    if (s_cmp(geolst, " ", (ftnlen)255, (ftnlen)1) != 0) {

/*        One of the geophysical constants was not found or wasn't of */
/*        the right type. Complain. */

	dafcls_(handle);
	delfil_(outfn, (ftnlen)255);
	setmsg_("The following geophysical constants were not provided to th"
		"e program or their values were not scalar DP numbers: #. Che"
		"ck whether the name of a geophysical constants PCK file was "
		"provided in the setup file keyword '#', and if so, whether t"
		"he file contains appropriate values for the keywords listed "
		"above. ", (ftnlen)306);
	errch_("#", geolst, (ftnlen)1, rtrim_(geolst, (ftnlen)255) - 1);
	errch_("#", "PCK_FILE", (ftnlen)1, (ftnlen)8);
	sigerr_("SPICE(MISSINGGEOCONSTS)", (ftnlen)23);
    }

/*     Fetch the geophysical constants needed for the SGP4 propagation */
/*     model from the kernel pool. */

    bodvar_(&c__399, "J2", &n, geophs, (ftnlen)2);
    bodvar_(&c__399, "J3", &n, &geophs[1], (ftnlen)2);
    bodvar_(&c__399, "J4", &n, &geophs[2], (ftnlen)2);
    bodvar_(&c__399, "KE", &n, &geophs[3], (ftnlen)2);
    bodvar_(&c__399, "QO", &n, &geophs[4], (ftnlen)2);
    bodvar_(&c__399, "SO", &n, &geophs[5], (ftnlen)2);
    bodvar_(&c__399, "ER", &n, &geophs[6], (ftnlen)2);
    bodvar_(&c__399, "AE", &n, &geophs[7], (ftnlen)2);

/*     Read the next line of the file. It must the second line of */
/*     the found TLE record. */

    rdtext_(inpfn, lines + 512, &eof, inpfn_len, (ftnlen)512);
    if (eof) {

/*        Next line does not exist. Complain. */

	dafcls_(handle);
	delfil_(outfn, (ftnlen)255);
	setmsg_("Second line of the first two-line element set for object # "
		"does not exist", (ftnlen)73);
	errint_("#", obidvl, (ftnlen)1);
	sigerr_("SPICE(NOSECONDLINE)", (ftnlen)19);
    }

/*     Start the loop the read the TLE data. Track the number of TLE's */
/*     read, I. Set the flag indicating that no segment were written yet */
/*     (FMODEL) and the flag indicating that there is not need to */
/*     continue reading the input file because enough data covering the */
/*     specified time window was collected and written to the file */
/*     (DONE). */

    i__ = 0;
    j = -9;
    fmodel = TRUE_;
    done = FALSE_;
    while(! eof && ! done) {

/*        Increment the record read index. */

	++i__;
	j += 10;

/*        Try to process this TLE record. If an error occurs during */
/*        processing, OK will have value .FALSE. and ERROR will contain */
/*        a description of the error. */

	zzgetelm_(&c__1950, lines, &epochs[(i__1 = i__ - 1) < 50001 && 0 <= 
		i__1 ? i__1 : s_rnge("epochs", i__1, "tle2spk_", (ftnlen)876)]
		, &elems[(i__2 = j - 1) < 500010 && 0 <= i__2 ? i__2 : s_rnge(
		"elems", i__2, "tle2spk_", (ftnlen)876)], &ok, error + 512, (
		ftnlen)512, (ftnlen)512);

/*        If we find an error, signal a standard SPICE error. */

	if (! ok) {
	    dafcls_(handle);
	    delfil_(outfn, (ftnlen)255);
	    setmsg_("Error in TLE set #1. The error reads: #2", (ftnlen)40);
	    errint_("#1", &i__, (ftnlen)2);
	    errch_("#2", error + 512, (ftnlen)2, (ftnlen)512);
	    sigerr_("SPICE(BADTLE)", (ftnlen)13);
	}

/*        Check if this is a duplicate record. */

	if (elemd_(&epochs[(i__1 = i__ - 1) < 50001 && 0 <= i__1 ? i__1 : 
		s_rnge("epochs", i__1, "tle2spk_", (ftnlen)896)], dupchk)) {

/*           The EPOCH value exists in the set. Ignore the record and */
/*           prevent its use for this loop iteration. */

	    --i__;
	    j += -10;
	    ok = FALSE_;
	}

/*        If this record is not duplicate, buffer it or toss it based on */
/*        time constraints. */

	if (ok) {
	    if (begbol && endbol) {

/*              We have both time constraints. Check this record's time */
/*              against start and stop times. */

		if (epochs[(i__1 = i__ - 1) < 50001 && 0 <= i__1 ? i__1 : 
			s_rnge("epochs", i__1, "tle2spk_", (ftnlen)919)] > 
			covval[0] && epochs[(i__2 = i__ - 1) < 50001 && 0 <= 
			i__2 ? i__2 : s_rnge("epochs", i__2, "tle2spk_", (
			ftnlen)919)] < covval[1]) {

/*                 This time is between start ands stop times. Leave */
/*                 this record in the buffer and add its time to the */
/*                 duplicate check set. */

		    insrtd_(&epochs[(i__1 = i__ - 1) < 50001 && 0 <= i__1 ? 
			    i__1 : s_rnge("epochs", i__1, "tle2spk_", (ftnlen)
			    927)], dupchk);
		} else if (epochs[(i__1 = i__ - 1) < 50001 && 0 <= i__1 ? 
			i__1 : s_rnge("epochs", i__1, "tle2spk_", (ftnlen)929)
			] <= covval[0]) {

/*                 This time is less than or equal to the start time. */
/*                 For now, set the flag to ignore this record. We will */
/*                 reset it back if we will find that this is record is */
/*                 the first record to be used as pad. */

		    ok = FALSE_;

/*                 Check if we need to save this record for padding. */

		    if (covval[0] - bsavep > covval[0] - epochs[(i__1 = i__ - 
			    1) < 50001 && 0 <= i__1 ? i__1 : s_rnge("epochs", 
			    i__1, "tle2spk_", (ftnlen)942)]) {

/*                    This record is closer to the start time than the */
/*                    currently saved record. Swap the currently saved */
/*                    time with this time in the duplicate check set and */
/*                    reset saved time and elements. */

			removd_(&bsavep, dupchk);
			insrtd_(&epochs[(i__1 = i__ - 1) < 50001 && 0 <= i__1 
				? i__1 : s_rnge("epochs", i__1, "tle2spk_", (
				ftnlen)952)], dupchk);
			if (! bsavfl) {
			    ok = TRUE_;
			}
			bsavfl = TRUE_;
			bsavep = epochs[(i__1 = i__ - 1) < 50001 && 0 <= i__1 
				? i__1 : s_rnge("epochs", i__1, "tle2spk_", (
				ftnlen)959)];
			moved_(&elems[(i__1 = j - 1) < 500010 && 0 <= i__1 ? 
				i__1 : s_rnge("elems", i__1, "tle2spk_", (
				ftnlen)960)], &c__10, bsavel);
		    }

/*                 Drop this record from the buffer because we only */
/*                 needed it for padding, which is saved separately. */

		    --i__;
		    j += -10;
		} else if (epochs[(i__1 = i__ - 1) < 50001 && 0 <= i__1 ? 
			i__1 : s_rnge("epochs", i__1, "tle2spk_", (ftnlen)971)
			] >= covval[1]) {

/*                 This time is greater than or equal to the stop time. */
/*                 For now, set the flag to ignore this record. We will */
/*                 reset it back if we will find that this is record is */
/*                 the first record to be used as pad. */

		    ok = FALSE_;

/*                 Check if we need to save this record for padding. */

		    if (esavep - covval[1] > epochs[(i__1 = i__ - 1) < 50001 
			    && 0 <= i__1 ? i__1 : s_rnge("epochs", i__1, 
			    "tle2spk_", (ftnlen)984)] - covval[1]) {

/*                    This record is closer to the stop time than the */
/*                    currently saved record. Swap the currently saved */
/*                    time with this time in the duplicate check set and */
/*                    swap saved time and elements. */

			removd_(&esavep, dupchk);
			insrtd_(&epochs[(i__1 = i__ - 1) < 50001 && 0 <= i__1 
				? i__1 : s_rnge("epochs", i__1, "tle2spk_", (
				ftnlen)994)], dupchk);
			if (! esavfl) {
			    ok = TRUE_;
			}
			esavfl = TRUE_;
			esavep = epochs[(i__1 = i__ - 1) < 50001 && 0 <= i__1 
				? i__1 : s_rnge("epochs", i__1, "tle2spk_", (
				ftnlen)1001)];
			moved_(&elems[(i__1 = j - 1) < 500010 && 0 <= i__1 ? 
				i__1 : s_rnge("elems", i__1, "tle2spk_", (
				ftnlen)1002)], &c__10, esavel);
		    }

/*                 Drop this record from the buffer because we only */
/*                 needed it for padding which is saved separately. */

		    --i__;
		    j += -10;
		}
	    } else if (begbol) {

/*              We have only start time constraint. Check this record's */
/*              time against the start time. */

		if (epochs[(i__1 = i__ - 1) < 50001 && 0 <= i__1 ? i__1 : 
			s_rnge("epochs", i__1, "tle2spk_", (ftnlen)1022)] > 
			covval[0]) {

/*                 This time is greater than the start time. Leave this */
/*                 record in the buffer and add its time to the */
/*                 duplicate check set. */

		    insrtd_(&epochs[(i__1 = i__ - 1) < 50001 && 0 <= i__1 ? 
			    i__1 : s_rnge("epochs", i__1, "tle2spk_", (ftnlen)
			    1029)], dupchk);
		} else {

/*                 This time is less than or equal to the start time. */
/*                 For now, set the flag to ignore this record. We will */
/*                 reset it back if we will find that this is record is */
/*                 the first record to be used as pad. */

		    ok = FALSE_;

/*                 Check if we need to save this record for padding. */

		    if (covval[0] - bsavep > covval[0] - epochs[(i__1 = i__ - 
			    1) < 50001 && 0 <= i__1 ? i__1 : s_rnge("epochs", 
			    i__1, "tle2spk_", (ftnlen)1044)]) {

/*                    This record is closer to the start time than the */
/*                    currently saved record. Swap the currently saved */
/*                    time with this time in the duplicate check set and */
/*                    reset saved time and elements. */

			removd_(&bsavep, dupchk);
			insrtd_(&epochs[(i__1 = i__ - 1) < 50001 && 0 <= i__1 
				? i__1 : s_rnge("epochs", i__1, "tle2spk_", (
				ftnlen)1054)], dupchk);
			if (! bsavfl) {
			    ok = TRUE_;
			}
			bsavfl = TRUE_;
			bsavep = epochs[(i__1 = i__ - 1) < 50001 && 0 <= i__1 
				? i__1 : s_rnge("epochs", i__1, "tle2spk_", (
				ftnlen)1061)];
			moved_(&elems[(i__1 = j - 1) < 500010 && 0 <= i__1 ? 
				i__1 : s_rnge("elems", i__1, "tle2spk_", (
				ftnlen)1062)], &c__10, bsavel);
		    }

/*                 Drop this record from the buffer because we only */
/*                 needed it for padding which is saved separately. */

		    --i__;
		    j += -10;
		}
	    } else if (endbol) {

/*              We have only stop time constraint. Check this record's */
/*              time against stop time. */

		if (epochs[(i__1 = i__ - 1) < 50001 && 0 <= i__1 ? i__1 : 
			s_rnge("epochs", i__1, "tle2spk_", (ftnlen)1081)] < 
			covval[1]) {

/*                 This time is less than the stop time. Leave this */
/*                 record in the buffer and add its time to the */
/*                 duplicate check set. */

		    insrtd_(&epochs[(i__1 = i__ - 1) < 50001 && 0 <= i__1 ? 
			    i__1 : s_rnge("epochs", i__1, "tle2spk_", (ftnlen)
			    1088)], dupchk);
		} else {

/*                 This time is greater than or equal to the stop time. */
/*                 For now, set the flag to ignore this record. We will */
/*                 reset it back if we will find that this is record is */
/*                 the first record to be used as pad. */

		    ok = FALSE_;

/*                 Check if we need to save this record for padding. */

		    if (esavep - covval[1] > epochs[(i__1 = i__ - 1) < 50001 
			    && 0 <= i__1 ? i__1 : s_rnge("epochs", i__1, 
			    "tle2spk_", (ftnlen)1103)] - covval[1]) {

/*                    This record is closer to the stop time than the */
/*                    currently saved record. Swap the currently saved */
/*                    time with this time in the duplicate check set and */
/*                    reset saved time and elements. */

			removd_(&esavep, dupchk);
			insrtd_(&epochs[(i__1 = i__ - 1) < 50001 && 0 <= i__1 
				? i__1 : s_rnge("epochs", i__1, "tle2spk_", (
				ftnlen)1113)], dupchk);
			if (! esavfl) {
			    ok = TRUE_;
			}
			esavfl = TRUE_;
			esavep = epochs[(i__1 = i__ - 1) < 50001 && 0 <= i__1 
				? i__1 : s_rnge("epochs", i__1, "tle2spk_", (
				ftnlen)1120)];
			moved_(&elems[(i__1 = j - 1) < 500010 && 0 <= i__1 ? 
				i__1 : s_rnge("elems", i__1, "tle2spk_", (
				ftnlen)1121)], &c__10, esavel);
		    }

/*                 Drop this record from the buffer because we only */
/*                 needed it for padding which is saved separately. */

		    --i__;
		    j += -10;
		}
	    } else {

/*              We have no time constraints. Leave this record in the */
/*              buffer and add its time to the duplicate check set. */

		insrtd_(&epochs[(i__1 = i__ - 1) < 50001 && 0 <= i__1 ? i__1 :
			 s_rnge("epochs", i__1, "tle2spk_", (ftnlen)1140)], 
			dupchk);
	    }
	}

/*        Set total number of currently buffered records including saved */
/*        pad. */

	total = i__;
	if (bsavfl) {
	    ++total;
	}
	if (esavfl) {
	    ++total;
	}

/*        If we filled up the EPOCHS buffer, then we need to complete */
/*        this segment. If this TLE set had duplicate time or was */
/*        outside of specified time boundaries, there is no reason to */
/*        evaluate this block. Wait for the next iteration of the loop. */

	if (total == 50000 && ok) {

/*           If we have padding records, add them to the end of the */
/*           buffer. */

	    if (bsavfl) {
		++i__;
		j += 10;
		epochs[(i__1 = i__ - 1) < 50001 && 0 <= i__1 ? i__1 : s_rnge(
			"epochs", i__1, "tle2spk_", (ftnlen)1175)] = bsavep;
		moved_(bsavel, &c__10, &elems[(i__1 = j - 1) < 500010 && 0 <= 
			i__1 ? i__1 : s_rnge("elems", i__1, "tle2spk_", (
			ftnlen)1176)]);
	    }
	    if (esavfl) {
		++i__;
		j += 10;
		epochs[(i__1 = i__ - 1) < 50001 && 0 <= i__1 ? i__1 : s_rnge(
			"epochs", i__1, "tle2spk_", (ftnlen)1182)] = esavep;
		moved_(esavel, &c__10, &elems[(i__1 = j - 1) < 500010 && 0 <= 
			i__1 ? i__1 : s_rnge("elems", i__1, "tle2spk_", (
			ftnlen)1183)]);
	    }

/*           The epochs may are out of order. Get an order vector. Use */
/*           it to sort the EPOCHS vector to ascending order. Apply the */
/*           same ordering operation to the ELEMS vector. */

	    orderd_(epochs, &c__50000, iorder);
	    reordd_(iorder, &c__50000, epochs);
	    reorbd_(iorder, &c__50000, &c__10, elems);

/*           Sorting complete, check for S_i > S_i+1 issue. */

	    if (priors) {
		if (svepch > epochs[0]) {

/*                 Signal an error then stop the application run. */

		    dafcls_(handle);
		    delfil_(outfn, (ftnlen)255);
		    setmsg_("Current segment ordering indicates a TLE orderi"
			    "ng with the property the final epoch of the prev"
			    "ious segment's data represents a date-time after"
			    " the initial date-time of the current segment's "
			    "data. This may occur if the TLE data set has an "
			    "ordering in descending order of epochs and the n"
			    "umber of TLE sets exceeds #.", (ftnlen)315);
		    errint_("#", &c__50000, (ftnlen)1);
		    sigerr_("SPICE(INVALIDTLEORDER)", (ftnlen)22);
		}
	    }

/*           Set time coverage for this segment. */

	    if (begbol) {

/*              We have start time. If it is the first segment, set */
/*              coverage start to that time and reset flag indicating */
/*              that this is the first segment. If not, set it to the */
/*              first TLE time. */

		if (fmodel) {
		    begint = covval[0];
		    fmodel = FALSE_;
		} else {
		    begint = epochs[0];
		}
	    } else {

/*              We have start pad. If it is the only segment, set */
/*              coverage start to the first TLE time minus pad. If not, */
/*              set it to the first TLE time. */

		if (fmodel) {
		    begint = epochs[0] - begpad;
		    fmodel = FALSE_;
		} else {
		    begint = epochs[0];
		}
	    }

/*           If the time of the last buffered point is greater than or */
/*           equal to the specified stop time, we set the coverage end */
/*           time to the specified stop time and consider ourselves */
/*           done. Otherwise, we set coverage stop to the time of the */
/*           last point and assume that we will add more segments. */

	    if (endbol && epochs[49999] >= covval[1]) {
		endint = covval[1];
		done = TRUE_;
	    } else {
		endint = epochs[49999];
	    }

/*           Report that we write next (or final) SPK segment. */

	    tostdo_(" ", (ftnlen)1);
	    if (done) {
		tostdo_("Writing final SPK segment ...", (ftnlen)29);
	    } else {
		tostdo_("Writing next SPK segment ...", (ftnlen)28);
	    }
	    spkw10_(handle, &obidvl[1], cnidvl, frnmvl, &begint, &endint, 
		    sgidvl, geophs, &c__50000, elems, epochs, frnmvl_len, 
		    sgidvl_len);

/*           We will continue processing of the input data and therefore */
/*           we need to achieve continuity of the data between segments. */
/*           To do that, we move one record from the end of the buffer */
/*           to the beginning of the buffer and reset all indexes */
/*           correspondingly. */

	    epochs[0] = epochs[49999];
	    priors = TRUE_;
	    svepch = epochs[0];
	    for (i__ = 1; i__ <= 10; ++i__) {
		elems[(i__1 = i__ - 1) < 500010 && 0 <= i__1 ? i__1 : s_rnge(
			"elems", i__1, "tle2spk_", (ftnlen)1304)] = elems[(
			i__2 = i__ + 499989) < 500010 && 0 <= i__2 ? i__2 : 
			s_rnge("elems", i__2, "tle2spk_", (ftnlen)1304)];
	    }

/*           Reset duplicate check set and pad records. */

	    scardd_(&c__0, dupchk);
	    insrtd_(epochs, dupchk);

/*           Reset padding flags and values. */

	    bsavfl = FALSE_;
	    bsavep = dpmin_();
	    esavfl = FALSE_;
	    esavep = dpmax_();

/*           Reset epoch and element buffer indexes. */

	    i__ = 1;
	    j = 1;
	}

/*        Unless we are done, keep reading lines from TLE data file */
/*        until we find next the TLE set for the requested ID or reach */
/*        the end of the file. */

	if (! done) {
	    rdtext_(inpfn, lines, &eof, inpfn_len, (ftnlen)512);
	    while(s_cmp(lines, chose1, (ftnlen)7, (ftnlen)7) != 0 && s_cmp(
		    lines, chose2, (ftnlen)7, (ftnlen)7) != 0 && ! eof) {
		rdtext_(inpfn, lines, &eof, inpfn_len, (ftnlen)512);
	    }

/*           If not the end of the file, read the second TLE line. */

	    if (! eof) {
		rdtext_(inpfn, lines + 512, &eof, inpfn_len, (ftnlen)512);
		if (eof) {

/*                 Next line does not exist. Complain. */

		    dafcls_(handle);
		    delfil_(outfn, (ftnlen)255);
		    setmsg_("Second line of the last two-line element set fo"
			    "r object # does not exist", (ftnlen)72);
		    errint_("#", obidvl, (ftnlen)1);
		    sigerr_("SPICE(NOSECONDLINE2)", (ftnlen)20);
		}
	    }
	}
    }

/*     We can be done at this point if the previous segment had all the */
/*     data needed to cover through the specified stop time. If we are */
/*     not done, we will write one more segment. */

    if (! done) {

/*        If we have padding records, add them to the buffer. */

	if (bsavfl) {
	    ++i__;
	    j += 10;
	    epochs[(i__1 = i__ - 1) < 50001 && 0 <= i__1 ? i__1 : s_rnge(
		    "epochs", i__1, "tle2spk_", (ftnlen)1387)] = bsavep;
	    moved_(bsavel, &c__10, &elems[(i__1 = j - 1) < 500010 && 0 <= 
		    i__1 ? i__1 : s_rnge("elems", i__1, "tle2spk_", (ftnlen)
		    1388)]);
	}
	if (esavfl) {
	    ++i__;
	    j += 10;
	    epochs[(i__1 = i__ - 1) < 50001 && 0 <= i__1 ? i__1 : s_rnge(
		    "epochs", i__1, "tle2spk_", (ftnlen)1394)] = esavep;
	    moved_(esavel, &c__10, &elems[(i__1 = j - 1) < 500010 && 0 <= 
		    i__1 ? i__1 : s_rnge("elems", i__1, "tle2spk_", (ftnlen)
		    1395)]);
	}

/*        We either ran out of file, or we ran out of elements. In */
/*        either case if there are any remaining element sets to be */
/*        written, now is the time to do it. If we have more than one */
/*        point we need to take care of an out-of-order data. */

	if (i__ > 1) {

/*           The epochs may are out of order. Get an order vector. Use */
/*           it to sort the EPOCHS vector to ascending order. Apply the */
/*           same ordering operation to the ELEMS vector. */

	    orderd_(epochs, &i__, iorder);
	    reordd_(iorder, &i__, epochs);
	    reorbd_(iorder, &i__, &c__10, elems);

/*           Sorting complete, check for  S_i > S_i+1 issue. */

	    if (priors) {
		if (svepch > epochs[0]) {

/*                 Signal an error then stop the application run. */

		    dafcls_(handle);
		    delfil_(outfn, (ftnlen)255);
		    setmsg_("Final segment ordering indicates a TLE ordering"
			    " with the property the final epoch of the previo"
			    "us segment's data represents a date-time after t"
			    "he initial date-time of the final segment's data"
			    ". This may occur if the TLE data set has an orde"
			    "ring in descending order of epochs and the numbe"
			    "r of TLE sets exceeds #.", (ftnlen)311);
		    errint_("#", &c__50000, (ftnlen)1);
		    sigerr_("SPICE(INVALIDTLEORDER)", (ftnlen)22);
		}
	    }
	}

/*        Set the SPK coverage time interval. */

	if (begbol) {

/*           We have start time. If it is the only segment, set coverage */
/*           start to that time. If not, set it to the first TLE time. */

	    if (fmodel) {
		begint = covval[0];
	    } else {
		begint = epochs[0];
	    }
	} else {

/*           We have start pad. If it is the only segment, set coverage */
/*           start to the first TLE time minus pad. If not, set it to */
/*           the first TLE time. */

	    if (fmodel) {
		begint = epochs[0] - begpad;
	    } else {
		begint = epochs[0];
	    }
	}
	if (endbol) {

/*           We have stop time. Set coverage stop to that time. */

	    endint = covval[1];
	} else {

/*           We have stop pad. Set coverage stop to the last TLE time */
/*           minus pad. */

	    endint = epochs[(i__1 = i__ - 1) < 50001 && 0 <= i__1 ? i__1 : 
		    s_rnge("epochs", i__1, "tle2spk_", (ftnlen)1491)] + 
		    endpad;
	}

/*        With coverage start and stop in hand we can check if they make */
/*        sense (i.e. start is earlier than stop). If they don't, we */
/*        will generate an error that will be more meaningful than the */
/*        error generated by a similar check in SPKW10. */

	if (begint >= endint) {

/*           There is one case when this will not be an error. That is */
/*           when we already wrote at least one segment and ended up */
/*           here with a single point and zero pads in hand or a single */
/*           point exactly matching the stop time. In this case we don't */
/*           need to write another segment and can consider ourselves */
/*           done. */

	    if (! fmodel) {
		if (begint == endint) {
		    done = TRUE_;
		} else {
		    setmsg_("There is a bug in the program. Please contact N"
			    "AIF.", (ftnlen)51);
		    sigerr_("SPICE(MKSPKTLE2SPKBUG0)", (ftnlen)23);
		}
	    } else {

/*              Check what happened case by case based on what times */
/*              and pads were given in the setup file. No matter what */
/*              happened, there was an error so we need close and delete */
/*              the output SPK. */

		dafcls_(handle);
		delfil_(outfn, (ftnlen)255);
		if (! begbol && ! endbol) {

/*                 No start or stop time was specified. In such cases */
/*                 the only real way to get here is if we had one input */
/*                 point and both pads were zero. Check that this is the */
/*                 case and, if so, report an error. Otherwise report a */
/*                 bug. */

		    if (i__ == 1 && begpad == 0. && endpad == 0.) {
			setmsg_("Both TLE pads cannot be zero when input TLE"
				" file contains only one distinct record for "
				"the specified object.", (ftnlen)108);
			sigerr_("SPICE(BADTLEPADS)", (ftnlen)17);
		    } else {
			setmsg_("There is a bug in the program. Please conta"
				"ct NAIF.", (ftnlen)51);
			sigerr_("SPICE(MKSPKTLE2SPKBUG1)", (ftnlen)23);
		    }
		} else if (! begbol) {

/*                 Only stop time was specified. In such cases the only */
/*                 real way to get here is if this stop time is less */
/*                 than or equal to start time determined from the first */
/*                 TLE record and default or user-specified pad. In such */
/*                 cases we must have buffered at most 2 records. Check */
/*                 that this is the case and, if so, report an error. */
/*                 Otherwise report a bug. */

		    if (i__ <= 2) {
			setmsg_("Stop time, # (#) TDB, specified in the setu"
				"p file is earlier than or equal to the the s"
				"tart time, # (#) TDB, computed from the time"
				", # (#) TDB, of the first applicable TLE rec"
				"ord for the specified object and default or "
				"specified pad, # seconds.", (ftnlen)244);
			etcal_(&endint, hword, (ftnlen)32);
			errdp_("#", &endint, (ftnlen)1);
			errch_("#", hword, (ftnlen)1, (ftnlen)32);
			etcal_(&begint, hword, (ftnlen)32);
			errdp_("#", &begint, (ftnlen)1);
			errch_("#", hword, (ftnlen)1, (ftnlen)32);
			etcal_(epochs, hword, (ftnlen)32);
			errdp_("#", epochs, (ftnlen)1);
			errch_("#", hword, (ftnlen)1, (ftnlen)32);
			errdp_("#", &begpad, (ftnlen)1);
			sigerr_("SPICE(BADSTOPTIME)", (ftnlen)18);
		    } else {
			setmsg_("There is a bug in the program. Please conta"
				"ct NAIF.", (ftnlen)51);
			sigerr_("SPICE(MKSPKTLE2SPKBUG2)", (ftnlen)23);
		    }
		} else if (! endbol) {

/*                 Only start time was specified. In such cases the only */
/*                 real way to get here is if this start time is greater */
/*                 than or equal to stop time determined from the last */
/*                 TLE record and default or user-specified pad. In such */
/*                 cases we must have buffered at most 2 records. Check */
/*                 that this is the case and, if so, report an error. */
/*                 Otherwise report a bug. */

		    if (i__ <= 2) {
			setmsg_("Start time, # (#) TDB, specified in the set"
				"up file is later than or equal to the the st"
				"op time, # (#) TDB, computed from the time, "
				"# (#) TDB, of the last applicable TLE record"
				" for the specified object and default or spe"
				"cified pad, # seconds.", (ftnlen)241);
			etcal_(&begint, hword, (ftnlen)32);
			errdp_("#", &begint, (ftnlen)1);
			errch_("#", hword, (ftnlen)1, (ftnlen)32);
			etcal_(&endint, hword, (ftnlen)32);
			errdp_("#", &endint, (ftnlen)1);
			errch_("#", hword, (ftnlen)1, (ftnlen)32);
			etcal_(epochs, hword, (ftnlen)32);
			errdp_("#", epochs, (ftnlen)1);
			errch_("#", hword, (ftnlen)1, (ftnlen)32);
			errdp_("#", &endpad, (ftnlen)1);
			sigerr_("SPICE(BADSTARTTIME)", (ftnlen)19);
		    } else {
			setmsg_("There is a bug in the program. Please conta"
				"ct NAIF.", (ftnlen)51);
			sigerr_("SPICE(MKSPKTLE2SPKBUG3)", (ftnlen)23);
		    }
		} else {

/*                 We can get here only if both start and stop times */
/*                 were specified. But in this cases, they should have */
/*                 been checked them to be OK before calling this */
/*                 routine. So if we got here, it is a bug. */

		    setmsg_("There is a bug in the program. Please contact N"
			    "AIF.", (ftnlen)51);
		    sigerr_("SPICE(MKSPKTLE2SPKBUG4)", (ftnlen)23);
		}
	    }
	}

/*        Write the last segment, but only if we need to. */

	if (! done) {

/*           Write final SPK segment. */

	    tostdo_(" ", (ftnlen)1);
	    tostdo_("Writing final SPK segment ...", (ftnlen)29);
	    spkw10_(handle, &obidvl[1], cnidvl, frnmvl, &begint, &endint, 
		    sgidvl, geophs, &i__, elems, epochs, frnmvl_len, 
		    sgidvl_len);
	}
    }

/*     Close input TLE file. */

    cltext_(inpfn, inpfn_len);

/*     All done. */

    chkout_("TLE2SPK", (ftnlen)7);
    return 0;
} /* tle2spk_ */

