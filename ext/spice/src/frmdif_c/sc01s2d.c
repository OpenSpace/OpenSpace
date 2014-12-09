/* sc01s2d.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__0 = 0;
static integer c__1 = 1;
static integer c__10 = 10;

/* $Procedure      SC01S2D ( Type 1 SCLK String to Decimal Form ) */
/* Subroutine */ int sc01s2d_(integer *sc, char *sclkch, doublereal *sclkdp, 
	ftnlen sclkch_len)
{
    /* Initialized data */

    static char namlst[32*3] = "SCLK01_N_FIELDS                 " "SCLK01_OF"
	    "FSETS                  " "SCLK01_MODULI                   ";

    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_rnge(char *, integer, 
	    char *, integer);

    /* Local variables */
    static integer noff, nmod, last;
    extern integer cpos_(char *, char *, integer *, ftnlen, ftnlen);
    static integer next, i__, n;
    extern /* Subroutine */ int scld01_(char *, integer *, integer *, integer 
	    *, doublereal *, ftnlen), scli01_(char *, integer *, integer *, 
	    integer *, integer *, ftnlen), chkin_(char *, ftnlen), errch_(
	    char *, char *, ftnlen, ftnlen);
    static integer oldsc;
    extern /* Subroutine */ int movec_(char *, integer *, char *, ftnlen, 
	    ftnlen), repmi_(char *, char *, integer *, char *, ftnlen, ftnlen,
	     ftnlen);
    static logical first;
    static char error[320];
    extern integer rtrim_(char *, ftnlen);
    static integer start;
    extern logical failed_(void);
    static integer nfield;
    static doublereal fractn;
    static char kvname[32*3];
    static doublereal fvalue[10], offset[10], moduli[10];
    static integer findex;
    static logical update;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), nparsi_(char *, integer *, char *, integer *, ftnlen, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen), suffix_(char *, integer *, char *, ftnlen, ftnlen), 
	    cvpool_(char *, logical *, ftnlen);
    extern integer sctype_(integer *);
    extern logical return_(void);
    extern /* Subroutine */ int swpool_(char *, integer *, char *, ftnlen, 
	    ftnlen);
    static integer ptr;

/* $ Abstract */

/*     Convert a type 1 SCLK string to decimal form. */

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

/*     CHARACTER */
/*     CONVERSION */
/*     PARSING */

/* $ Declarations */
/* $ Abstract */

/*     Include file sclk.inc */

/*     SPICE private file intended solely for the support of SPICE */
/*     routines.  Users should not include this file directly due */
/*     to the volatile nature of this file */

/*     The parameters below define sizes and limits used by */
/*     the SCLK system. */

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

/* $ Parameters */

/*     See the declaration section below. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 24-MAY-2010 (NJB) */

/*        Increased value of maximum coefficient record count */
/*        parameter MXCOEF from 10K to 50K. */

/* -    SPICELIB Version 1.0.0, 11-FEB-2008 (NJB) */

/* -& */

/*     Number of supported SCLK field delimiters: */


/*     Supported SCLK string field delimiters: */


/*     Maximum number of partitions: */


/*     Partition string length. */

/*     Since the maximum number of partitions is given by MXPART is */
/*     9999, PRTSTR needs at most 4 characters for the partition number */
/*     and one character for the slash. */


/*     Maximum number of coefficient records: */


/*     Maximum number of fields in an SCLK string: */


/*     Length of strings used to represent D.P. */
/*     numbers: */


/*     Maximum number of supported parallel time systems: */


/*     End of include file sclk.inc */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     SC         I   NAIF integer code for a spacecraft. */
/*     SCLKCH     I   An SCLK string. */
/*     SCLKDP     O   A decimal form of SCLK string. */

/* $ Detailed_Input */

/*     SC             is NAIF integer code for a spacecraft. */

/*     SCLKCH         is an input Type 1 SCLK string, with or without */
/*                    partition, unabridged (all fields) or abridged */
/*                    (any number of fields on the right may be */
/*                    missing), with fields separated by any of the */
/*                    allowed Type 1 delimiters (see sclk.inc). */

/*                    Any Type 1 SCLK string generated by SPICELIB */
/*                    SCLK routines will be successfully processed by */
/*                    routine. */

/*                    Type 1 SCLK string produced by other means */
/*                    will be processed as well if they contain */
/*                    at least one SCLK field, do not start with a */
/*                    delimiter, and do not have more than one */
/*                    delimiter in a row (this restriction extends */
/*                    to multiple spaces). */


/* $ Detailed_Output */

/*     SCLKDP         is a DP number providing a decimal form the input */
/*                    SCLK with integer part equal to the left-most */
/*                    field of the clock and the fractional decimal part */
/*                    computed from the rest of the clock fields as */
/*                    described in Particulars. */

/* $ Parameters */

/*     See include file. */

/* $ Exceptions */

/*     1) If clock type associated with SC is not 1, */
/*        'SPICE(NOTSUPPORTED)' is signaled. */

/*     2) If input SCLK string is blank, 'SPICE(BLANKSCLKSTRING)' is */
/*        signaled. */

/*     3) If number of SCLK fields provided in SCLK01_N_FIELDS is */
/*        not equal to count of values provided in SCLK01_OFFSETS, */
/*        'SPICE(BADSCLKDATA1)' is signaled. */

/*     4) If number of SCLK fields provided in SCLK01_N_FIELDS is */
/*        not equal to count of values provided in SCLK01_MODULI, */
/*        'SPICE(BADSCLKDATA2)' is signaled. */

/*     5) If any of the MODULI is zero, 'SPICE(BADSCLKDATA3)' is */
/*        signaled. */

/*     6) If partition separator in the last character, */
/*        'SPICE(INVALIDSCLKSTRING1)' is signaled. */

/*     7) If delimiter appears at the beginning of the string, of */
/*        immediately following partition separator or another */
/*        delimiter, 'SPICE(OUTOFPLACEDELIMITER)' is signaled. */

/*     8) If a field does not represent an integer, */
/*        'SPICE(NONINTEGERFIELD)' is signaled. */

/*     9) If SCLK string contains more fields that specified in */
/*        SCLK01_N_FIELDS, 'SPICE(TOOMANYFIELDS)' is signaled. */

/*     10) If no fields were extracted from the string, */
/*        'SPICE(INVALIDSCLKSTRING2)' is signaled. */

/*     11) Routines called by this routine signal errors if they don't */
/*        fetch required SCLK parameters. */

/* $ Files */

/*     SCLK kernel for the clock of interest must be loaded before */
/*     calling this routine. This routine fetches SCLK01_N_FIELDS, */
/*     SCLK01_OFFSETS, and SCLK01_MODULI for this clock from the POOL. */

/* $ Particulars */

/*     A Type 1 SCLK string is converted decimal form as follows: */

/*        -  partition number, if present, is dropped */

/*        -  the string is split into fields */

/*        -  left-most field is used as the integer part of the output */
/*           DP number */

/*        -  the fractional decimal part is computed using all clock */
/*           fields to the right of the left-most field by starting from */
/*           the right-most field and 1) applying offset to the field */
/*           value, 2) dividing adjusted value by moduli, 3) adding the */
/*           resultant fractional part to unadjusted value of the next */
/*           field to the left until all fields up to the left-most */
/*           field are processed. */

/* $ Examples */

/*     For an SCLK with the following parameters: */

/*        SCLK01_N_FIELDS_84            = (                 2 ) */
/*        SCLK01_MODULI_84              = ( 10000000000   256 ) */
/*        SCLK01_OFFSETS_84             = (           0     0 ) */

/*     this table illustrates the conversions this routine will perform: */

/*            SCLKCH               SCLKDP */
/*        ----------------     ---------------- */
/*        '0.0'                        0.000000 */
/*        '0.1'                        0.003906 */
/*        '0.128'                      0.500000 */
/*        '0.256'                      1.000000 */
/*        '883612839'          883612839.000000 */
/*        '883612839.0'        883612839.000000 */
/*        '883612839 0'        883612839.000000 */
/*        '883612839-128'      883612839.500000 */
/*        '1/883612839-128'    883612839.500000 */
/*        '1/883612839-255'    883612839.996094 */

/*        '1/883612839--255'   error: double delimiter */
/*        '1/.255'             error: leading delimiter */
/*        '.255'               error: leading delimiter */
/*        '0.255.0'            error: too many fields */

/* $ Restrictions */

/*     This routine works only for Type 1 SCLKs. */

/*     SCLK kernel for the clock of interest must be loaded before */
/*     calling this routine. */

/*     Input SCLK field must not start with a delimiter and must not */
/*     have more than one delimiter of the same kind (including spaces) */
/*     or of different kinds (e.g. ':' followed by space) in a row. */

/*     Number of items in OFFSETS and MODULI fetched from the POOL */
/*     must be equal to the value of N_FIELDS. */

/*     No value of MODULI can be zero. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Version */

/* -    FRMDIFF Version 1.0.0, 12-SEP-2008 (BVS) */

/*        Initial version. All parts related to managing SCLK parameters */
/*        were shamelessly stolen from SC01. */

/* -& */
/* $ Index_Entries */

/*     Type 1 string SCLK to decimal form */

/* -& */

/*     Local parameters. */


/*     SPICELIB functions */


/*     Variables */


/*     Save everything since we will be using many saved values. */


/*     Initial values. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SC01S2D", (ftnlen)7);
    }

/*     Report an error if this is not a type 1 SCLK. */

    if (sctype_(sc) != 1) {
	setmsg_("The clock type associated with the ID # was #. The SCLK str"
		"ing '#' cannot be converted to decimal form because such con"
		"version is supported only for type 1 clocks.", (ftnlen)163);
	errint_("#", sc, (ftnlen)1);
	i__1 = sctype_(sc);
	errint_("#", &i__1, (ftnlen)1);
	errch_("#", sclkch, (ftnlen)1, sclkch_len);
	sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	chkout_("SC01S2D", (ftnlen)7);
	return 0;
    }

/*     Report an error if input string is blank. */

    if (s_cmp(sclkch, " ", sclkch_len, (ftnlen)1) == 0) {
	setmsg_("Input SCLK string is blank.", (ftnlen)27);
	sigerr_("SPICE(BLANKSCLKSTRING)", (ftnlen)22);
	chkout_("SC01S2D", (ftnlen)7);
	return 0;
    }

/*     On the first pass through the subroutine, or if the spacecraft */
/*     clock ID changes, we will set watches on the SCLK kernel */
/*     variables for the current clock. */

    if (first || *sc != oldsc) {
	first = FALSE_;

/*        Make up a list of names of kernel variables that we'll use. */

	movec_(namlst, &c__3, kvname, (ftnlen)32, (ftnlen)32);
	for (i__ = 1; i__ <= 3; ++i__) {
	    suffix_("_#", &c__0, kvname + (((i__1 = i__ - 1) < 3 && 0 <= i__1 
		    ? i__1 : s_rnge("kvname", i__1, "sc01s2d_", (ftnlen)348)) 
		    << 5), (ftnlen)2, (ftnlen)32);
	    i__3 = -(*sc);
	    repmi_(kvname + (((i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : 
		    s_rnge("kvname", i__1, "sc01s2d_", (ftnlen)349)) << 5), 
		    "#", &i__3, kvname + (((i__2 = i__ - 1) < 3 && 0 <= i__2 ?
		     i__2 : s_rnge("kvname", i__2, "sc01s2d_", (ftnlen)349)) 
		    << 5), (ftnlen)32, (ftnlen)1, (ftnlen)32);
	}

/*        Set a watch on all of the kernel variables we use. */

	swpool_("SC01S2D", &c__3, kvname, (ftnlen)7, (ftnlen)32);

/*        Keep track of the last spacecraft clock ID encountered. */

	oldsc = *sc;
    }

/*     Find out whether we need to look up new format descriptors from */
/*     the kernel pool. If any relevant kernel variables were updated, */
/*     we have to do a look-up. Note that changing the s/c clock ID */
/*     causes a new watch to be set, so a look-up is required. */

    cvpool_("SC01S2D", &update, (ftnlen)7);
    if (update) {

/*        Our first piece of business is to look up all of the data we */
/*        require from the kernel pool. We must form the names of the */
/*        items we want using the input S/C ID code.  The items we need */
/*        are: */

/*           -  The number of fields in an (unabridged) SCLK string */
/*           -  The moduli of the fields of an SCLK string */
/*           -  The offsets for each clock field. */

	scli01_(namlst, sc, &c__1, &n, &nfield, (ftnlen)32);
	scld01_(namlst + 32, sc, &c__10, &noff, offset, (ftnlen)32);
	scld01_(namlst + 64, sc, &c__10, &nmod, moduli, (ftnlen)32);

/*        Don't try to continue if we had a lookup error. */

	if (failed_()) {
	    chkout_("SC01S2D", (ftnlen)7);
	    return 0;
	}

/*        Verify that SCLK items that we got are consistent and that */
/*        none of the moduli is zero. */

	if (nfield != noff) {
	    setmsg_("Invalid SCLK parameters for ID #. The number of fields "
		    "(#) provided by keyword '#' does not match the count of "
		    "field offsets (#) provided in the keyword '#'.", (ftnlen)
		    157);
	    errint_("#", sc, (ftnlen)1);
	    errint_("#", &nfield, (ftnlen)1);
	    errch_("#", kvname, (ftnlen)1, (ftnlen)32);
	    errint_("#", &noff, (ftnlen)1);
	    errch_("#", kvname + 32, (ftnlen)1, (ftnlen)32);
	    sigerr_("SPICE(BADSCLKDATA1)", (ftnlen)19);
	    chkout_("SC01S2D", (ftnlen)7);
	    return 0;
	}
	if (nfield != nmod) {
	    setmsg_("Invalid SCLK parameters for ID #. The number of fields "
		    "(#) provided by keyword '#' does not match the count of "
		    "field moduli (#) provided in the keyword '#'.", (ftnlen)
		    156);
	    errint_("#", sc, (ftnlen)1);
	    errint_("#", &nfield, (ftnlen)1);
	    errch_("#", kvname, (ftnlen)1, (ftnlen)32);
	    errint_("#", &nmod, (ftnlen)1);
	    errch_("#", kvname + 64, (ftnlen)1, (ftnlen)32);
	    sigerr_("SPICE(BADSCLKDATA2)", (ftnlen)19);
	    chkout_("SC01S2D", (ftnlen)7);
	    return 0;
	}
	i__1 = nmod;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (moduli[(i__2 = i__ - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge(
		    "moduli", i__2, "sc01s2d_", (ftnlen)433)] == 0.) {
		setmsg_("Invalid SCLK parameters for ID #. Modulus for the f"
			"ield number # provided in the keyword '#' is zero.", (
			ftnlen)101);
		errint_("#", sc, (ftnlen)1);
		errint_("#", &i__, (ftnlen)1);
		errch_("#", kvname + 64, (ftnlen)1, (ftnlen)32);
		sigerr_("SPICE(BADSCLKDATA3)", (ftnlen)19);
		chkout_("SC01S2D", (ftnlen)7);
		return 0;
	    }
	}
    }

/*     Parse input SCLK string. Start either at the beginning or */
/*     immediately after partition separator (if there is one). */

    findex = 0;
    last = rtrim_(sclkch, sclkch_len);
/* Computing MAX */
    i__1 = 1, i__2 = cpos_(sclkch, "/", &c__1, sclkch_len, (ftnlen)1) + 1;
    start = max(i__1,i__2);
    if (start > last) {
	setmsg_("SCLK string '#' is invalid. It has partition separator '/' "
		"as the last character.", (ftnlen)81);
	errch_("#", sclkch, (ftnlen)1, sclkch_len);
	sigerr_("SPICE(INVALIDSCLKSTRING1)", (ftnlen)25);
	chkout_("SC01S2D", (ftnlen)7);
	return 0;
    }
    while(start <= last) {

/*        If the starting character is a delimiter, signal an error */
/*        as we will not accept delimiter following '/', delimiters */
/*        at the beginning of the SCLK without partition, and */
/*        multiple delimiters in a row any place in the string. */

	if (cpos_(".:-, ", sclkch + (start - 1), &c__1, (ftnlen)5, (ftnlen)1) 
		!= 0) {
	    setmsg_("Delimiter '#' at position # in SCLK string '#' is out o"
		    "f place.", (ftnlen)63);
	    errch_("#", sclkch + (start - 1), (ftnlen)1, (ftnlen)1);
	    errint_("#", &start, (ftnlen)1);
	    errch_("#", sclkch, (ftnlen)1, sclkch_len);
	    sigerr_("SPICE(OUTOFPLACEDELIMITER)", (ftnlen)26);
	    chkout_("SC01S2D", (ftnlen)7);
	    return 0;
	}

/*        Locate next delimiter. */

	next = cpos_(sclkch, ".:-, ", &start, sclkch_len, (ftnlen)5);
	if (next == 0 || next > last) {
	    next = last;
	} else {
	    --next;
	}

/*        Parse field as integer. */

	nparsi_(sclkch + (start - 1), &n, error, &ptr, next - (start - 1), (
		ftnlen)320);
	if (ptr != 0) {
	    setmsg_("Field '#' in SCLK string '#' is not an integer number.", 
		    (ftnlen)54);
	    errch_("#", sclkch + (start - 1), (ftnlen)1, next - (start - 1));
	    errch_("#", sclkch, (ftnlen)1, sclkch_len);
	    sigerr_("SPICE(NONINTEGERFIELD)", (ftnlen)22);
	    chkout_("SC01S2D", (ftnlen)7);
	    return 0;
	}

/*        Buffer the field value if we aren't over the maximum number */
/*        fields. */

	if (findex + 1 <= nfield) {
	    ++findex;
	    fvalue[(i__1 = findex - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge(
		    "fvalue", i__1, "sc01s2d_", (ftnlen)520)] = (doublereal) 
		    n;
	} else {
	    setmsg_("SCLK string '#' has more fields than the number of fiel"
		    "ds (#) defined by loaded SCLK parameters.", (ftnlen)96);
	    errch_("#", sclkch, (ftnlen)1, sclkch_len);
	    errint_("#", &nfield, (ftnlen)1);
	    sigerr_("SPICE(TOOMANYFIELDS)", (ftnlen)20);
	    chkout_("SC01S2D", (ftnlen)7);
	    return 0;
	}

/*        Reset start to move to the next field. */

	start = next + 2;
    }

/*     Report an error if we have not buffered any field values. */

    if (findex == 0) {
	setmsg_("SCLK string '#' does not contain any fields.", (ftnlen)44);
	errch_("#", sclkch, (ftnlen)1, sclkch_len);
	sigerr_("SPICE(INVALIDSCLKSTRING2)", (ftnlen)25);
	chkout_("SC01S2D", (ftnlen)7);
	return 0;
    }

/*     Convert fields to the right of the first field to decimal */
/*     fraction. */

    fractn = 0.;
    for (i__ = findex; i__ >= 2; --i__) {
	fractn = (fvalue[(i__1 = i__ - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge(
		"fvalue", i__1, "sc01s2d_", (ftnlen)559)] - offset[(i__2 = 
		i__ - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge("offset", i__2, 
		"sc01s2d_", (ftnlen)559)] + fractn) / moduli[(i__3 = i__ - 1) 
		< 10 && 0 <= i__3 ? i__3 : s_rnge("moduli", i__3, "sc01s2d_", 
		(ftnlen)559)];
    }

/*     Add fraction to the first field value to get output decimal SCLK. */

    *sclkdp = fvalue[0] + fractn;

/*     That's all folks. */

    chkout_("SC01S2D", (ftnlen)7);
    return 0;
} /* sc01s2d_ */

