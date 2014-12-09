/* setup.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__5 = 5;
static doublereal c_b259 = 1.;

/* $Procedure      SETUP ( Get setup file keyword values ) */
/* Subroutine */ int setup_0_(int n__, char *flag__, char *chval, integer *
	intval, doublereal *dpval, logical *found, ftnlen flag_len, ftnlen 
	chval_len)
{
    /* Initialized data */

    static char alldlm[3*5] = "TAB" "EOL" ",  " "   " ";  ";
    static logical floadd = FALSE_;

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char arch[80], type__[80];
    integer i__, n;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    char chrdp[80];
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen), 
	    errch_(char *, char *, ftnlen, ftnlen);
    char frkfn[255], padvl[80], value[12], lskfn[255];
    extern integer wdcnt_(char *, ftnlen);
    doublereal wrkdp;
    logical exist;
    extern integer rtrim_(char *, ftnlen);
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    char units[80];
    extern /* Subroutine */ int bodn2c_(char *, integer *, logical *, ftnlen),
	     str2et_(char *, doublereal *, ftnlen);
    integer handle;
    extern integer lastnb_(char *, ftnlen), isrchc_(char *, integer *, char *,
	     ftnlen, ftnlen);
    extern logical exists_(char *, ftnlen), return_(void);
    integer fnmlen;
    char wrkchr[512], objnvl[80], cennvl[80], angunt[80], dstunt[80], sttmvl[
	    80], sptmvl[80], prctvl[80], keywrd[80];
    integer refcod;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), gcpool_(char *, integer *, 
	    integer *, integer *, char *, logical *, ftnlen, ftnlen), namfrm_(
	    char *, integer *, ftnlen), furnsh_(char *, ftnlen), gipool_(char 
	    *, integer *, integer *, integer *, integer *, logical *, ftnlen),
	     errint_(char *, integer *, ftnlen), astrip_(char *, char *, char 
	    *, char *, ftnlen, ftnlen, ftnlen, ftnlen), convrt_(doublereal *, 
	    char *, char *, doublereal *, ftnlen, ftnlen), gdpool_(char *, 
	    integer *, integer *, integer *, doublereal *, logical *, ftnlen),
	     nextwd_(char *, char *, char *, ftnlen, ftnlen, ftnlen), nparsd_(
	    char *, doublereal *, char *, integer *, ftnlen, ftnlen), dtpool_(
	    char *, logical *, integer *, char *, ftnlen, ftnlen), getfat_(
	    char *, char *, char *, ftnlen, ftnlen, ftnlen), pcklof_(char *, 
	    integer *, ftnlen);
    extern integer pos_(char *, char *, integer *, ftnlen, ftnlen);

/* $ Abstract */

/*     This routine is a module of the MKSPK program. It returns */
/*     values of the setup file keywords loaded into the POOL. */

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

/*     Variable  I/O  Entry */
/*     --------  ---  -------------------------------------------------- */
/*     FLAG       I   SETUPC, SETUPI, SETUPD, SETUPA */
/*     CHVAL      O   SETUPC */
/*     INTVAL     O   SETUPI */
/*     DPVAL      O   SETUPD */
/*     FOUND      O   SETUPC, SETUPI, SETUPD, SETUPA */
/*     ALLDLM     P   Complete list of input data delimiters. */

/* $ Detailed_Input */

/*     See the ENTRY points for a discussion of their arguments. */

/* $ Detailed_Output */

/*     See the ENTRY points for a discussion of their arguments. */

/* $ Parameters */

/*     ALLDLM    is a complete list of supported input data delimiters. */

/* $ Exceptions */

/*     1) If input file name was not provided neither on the */
/*        command line nor as a value of the setup file keyword, */
/*        then the error 'SPICE(NOINPUTFILENAME)' will be signaled. */

/*     2) If input file name is blank, then the error */
/*        'SPICE(BLANKINPUTFILENAME)' will be signaled. */

/*     3) If input file does not exist, then the error */
/*        'SPICE(INPUTFILENOTEXIST)' will be signaled. */

/*     4) If output SPK file name was not provided neither on */
/*        the command line nor as a value of the setup file keyword, */
/*        then the error 'SPICE(NOOUTPUTFILENAME)' will be signaled. */

/*     5) If output file name is blank, then the error */
/*        'SPICE(BLANKOUTPTFILENAME)' will be signaled. */

/*     6) If append flag specified in the setup file keyword */
/*        is not recognized the error 'SPICE(UNRECOGNAPPFLAG)' */
/*        will be signaled. */

/*     7) If type of the input data was not specified in the */
/*        setup file keyword, then the error 'SPICE(NOINPUTDATATYPE)' */
/*        will be signaled. */

/*     8) If input data type specified in the setup file keyword */
/*        is not recognized, then the error 'SPICE(UNRECOGNDATATYPE)' */
/*        will be signaled. */

/*     9) If the name of the reference frame with respect to */
/*        which the input trajectory data are provided was not */
/*        specified in the setup file keyword, then the error */
/*        'SPICE(NOFRAMENAME)' will be signaled. */

/*     10) If the reference frame is not recognized and the */
/*        name of a frames definition kernel file which could */
/*        contain a definition of this frame was not provided via */
/*        the setup file keyword, then the error */
/*        'SPICE(NOFRAMESKERNELNAME)' will be signaled. */

/*     11) If the reference frame is not recognized, then the */
/*        error 'SPICE(FRAMENOTRECOGNIZED)' will be signaled. */

/*     12) If string containing producer name was not specified */
/*        in the setup file keyword, then the error */
/*        'SPICE(NOPRODUCERID)' will be signaled. */

/*     13) If time wrapper string specified in the setup file */
/*        keyword didn't contain special character indicating */
/*        location at which time string from the input records */
/*        should be inserted, then the error 'SPICE(NO#INTIMEWRAPPER)' */
/*        will be signaled. */

/*     14) If the file containing comments to be inserted into */
/*        the comment of the output SPK file, specified in the */
/*        setup file keyword doesn't exist, then the error */
/*        'SPICE(COMMFILENOTEXIST)' will be signaled. */

/*     15) If the order parameters in input data records was */
/*        not specified in the setup file keyword, then the error */
/*        'SPICE(NODATAORDER)' will be signaled. */

/*     16) If character separating parameters in the input data */
/*        records was not specified in the setup file keyword, */
/*        then the error 'SPICE(NODELIMCHARACTER)' will be signaled. */

/*     17) If delimiter specified in the setup file keyword is */
/*        not recognized, then the error 'SPICE(UNRECOGNDELIMITER)' */
/*        will be signaled. */

/*     18) If neither object ID nor object name was specified */
/*        in the setup file using corresponding keywords, then */
/*        the error 'SPICE(NOOBJECTIDORNAME)' will be signaled. */

/*     19) If object name specified in the setup file keyword */
/*        cannot be translated to NAIF ID, then the error */
/*        'SPICE(BADOBJECTNAME)' will be signaled. */

/*     20) If neither center ID nor center name was specified */
/*        in the setup file using corresponding keywords, then */
/*        the error 'SPICE(NOCENTERIDORNAME)' will be signaled. */

/*     21) If center name specified in the setup file keyword */
/*        cannot be translated to NAIF ID, then the error */
/*        'SPICE(BADCENTERNAME)' will be signaled. */

/*     22) If output SPK file type was not specified in the */
/*        setup file keyword, then the error 'SPICE(NOOUTPUTSPKTYPE)' */
/*        will be signaled. */

/*     23) If output SPK type specified in the setup file */
/*        keyword is not supported in this version of the program, */
/*        then the error 'SPICE(SPKTYPENOTSUPPORTD)' will be */
/*        signaled. */

/*     24) If number of lines which a single input file record */
/*        occupies was not specified in the setup file keyword */
/*        as an integer value, then the error */
/*        'SPICE(NOLINESPERRECCOUNT)' will be signaled. */

/*     25) If number of lines, which a single input file record */
/*        occupies, specified in the setup file keyword is not a */
/*        positive integer number or word, then the error */
/*        'SPICE(BADLINEPERRECCOUNT)' will be signaled. */

/*     26) If polynomial degree was not specified in the setup */
/*        file keyword, then the error 'SPICE(NOPOLYNOMIALDEGREE)' */
/*        will be signaled. */

/*     27) If orbit precession type was not specified in the */
/*        setup file keyword, then the error 'SPICE(NOPRECESSIONTYPE)' */
/*        will be signaled. */

/*     28) If precession type specified in the setup file */
/*        keyword is not recognized, then the error */
/*        'SPICE(UNRECOGNPRECTYPE)' will be signaled. */

/*     29) If leapsecond file name was not specified in the */
/*        setup file keyword, then the error 'SPICE(NOLSKFILENAME)' */
/*        will be signaled. */

/*     30) If PCK file specified in the setup file keyword is */
/*        not a text or binary PCK file, then the error */
/*        'SPICE(NOTAPCKFILE)' will be signaled. */

/*     31) In case of a programming error by NAIF, a fail-safe error */
/*        in the form 'SPICE(MKSPKBUGSETUP#)' will be signaled. */

/*     32) If the first item in a TLE coverage pad keyword value is not */
/*        a number, then the error 'SPICE(BADTLECOVERAGEPAD2)' will be */
/*        signaled. */

/*     33) If a TLE coverage pad value is not exactly two items, a */
/*        number and a word, then the error 'SPICE(BADTLECOVERAGEPAD)' */
/*        will be signaled. */

/*     34) If the duration in a TLE coverage pad keyword value is */
/*        negative, then the error 'SPICE(BADTLECOVERAGEPAD3)' will be */
/*        signaled. */

/* $ Files */

/*     See the ENTRY points for a details. */

/* $ Particulars */

/*     SETUP should never be called directly, but should instead be */
/*     accessed only through its entry points. If SETUP is called */
/*     directly a 'SPICE(BOGUSENTRY)' error will be signaled. */

/*     The purpose of this routine is to get the values of the setup */
/*     file keywords that were loaded into the POOL. The following */
/*     entry points should be used to get, process and return */
/*     specified value to the main program: */

/*           SETUPC   Return a string values */

/*           SETUPI   Return an integer values */

/*           SETUPD   Return a d.p. value */

/*           SETUPA   Get LSK and PCK file names and load its */
/*                    contents. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.G. Khavenson (IKI RAS, Russia) */
/*     B.V. Semenov   (NAIF, JPL) */
/*     E.D. Wright    (NAIF) */

/* $ Version */

/* -    Version 1.2.0, 24-FEB-2012 (BVS) */

/*        Added processing of TLE coverage and ID keywords. */

/* -    Version 1.1.0, 08-APR-2004 (EDW) */

/*        Replaced all LDPOOL calls with FURNSH. */

/* -    Version 1.0.8, 21-MAR-2001 (BVS) */

/*        Added loading of the frames kernel for the object and */
/*        center cases. FK may contain id-name mapping that is */
/*        needed for resolving the object/center names. Added */
/*        processing of 'APPEND FLAG' case to SETUPC entry point. */
/*        Also, in SETUPC, eliminated check for existence of */
/*        output SPK. It's now done in the calling program. */

/* -    Version 1.0.7, 19-JAN-2000 (BVS) */

/*        Added code to recognize SPK types 12 and 13. */

/* -    Version 1.0.6, 25-NOV-1999 (NGK) */

/*        Added input/output types processing for case of two-line */
/*        elements. */

/* -    Version 1.0.5, 29-MAR-1999 (NGK) */

/*        Corrected comments. */

/* -    Version 1.0.4, 21-MAR-1999 (BVS) */

/*        Removed all warning messages. */

/* -    Version 1.0.3, 19-MAR-1999 (BVS) */

/*        Changed type of the LINES_PER_RECORD value in entry point */
/*        SETUPI to integer. */

/* -    Version 1.0.2, 15-MAR-1999 (BVS) */

/*        Corrected comments. Added "bogus entry" error message if */
/*        SETUP is called directly. */

/* -    Version 1.0.1, 13-JAN-1999 (BVS) */

/*        Modified error and warning messages. */

/* -    Version 1.0.0, 08-SEP-1998 (NGK) */

/* -& */
/* $ Index_Entries */

/*     Return a value of a MKSPK setup file keyword */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */



/*     Local variables */


/*     Input and output file names length. */


/*     Generic line variables. Sizes LINLEN, FILSIZ, SHRTLN, VALUEL */
/*     declared in the include file. */


/*     Array of all reserved delimiter that may appear in input */
/*     data. Size and dimension declared in the include file. */


/*     Binary PCK  handling */


/*     The remaining local variables */


/*     Assign  all value chose as delimiter */

    switch(n__) {
	case 1: goto L_setupc;
	case 2: goto L_setupi;
	case 3: goto L_setupd;
	case 4: goto L_setupa;
	}


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SETUP", (ftnlen)5);
    }

/*     This routine should never be called. If this routine is called, */
/*     an error is signaled. */

    setmsg_("SETUP: You have called an entry which performs performs no run-"
	    "time function. This may indicate a bug", (ftnlen)101);
    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("SETUP", (ftnlen)5);
    return 0;
/* $Procedure   SETUPC ( Return string values to main program ) */

L_setupc:
/* $ Abstract */

/*     Return character string value of a particular setup file */
/*     keyword. */

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

/*     KERNEL */

/* $ Keywords */

/*     KERNEL */
/*     POOL */

/* $ Declarations */

/*     CHARACTER*(*)         FLAG */
/*     CHARACTER*(*)         CHVAL */
/*     LOGICAL               FOUND */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     FLAG       I   Flag of keyword */
/*     CHVAL      O   Returned string value */
/*     FOUND      O   True if variable is in pool. */

/* $ Detailed_Input */

/*     FLAG        is the string containing text identifier for the */
/*                 keyword of interest. */

/* $ Detailed_Output */

/*     CHVAL       is string value of the keyword */

/*     FOUND       is TRUE if the value was obtained from the loaded */
/*                 setup file keyword(s). */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     See main routine header for complete list of exceptions. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     None */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.G. Khavenson (IKI RAS, Russia) */
/*     B.V. Semenov   (NAIF, JPL) */

/* $ Version */

/* -    Version 1.0.7, 21-MAR-2001 (BVS) */

/*        Added processing of 'APPEND FLAG' case. Changed FK */
/*        loading logic in 'REFERENCE FRAME NAME' case. Eliminated */
/*        check for existence of output SPK. It's now done in the */
/*        calling program. */

/* -    Version 1.0.6, 25-NOV-1999 (NGK) */

/*        Added input type processing for case of two-line elements. */

/* -    Version 1.0.5, 29-MAR-1999 (NGK) */

/*        Corrected comments. */

/* -    Version 1.0.4, 21-MAR-1999 (BVS) */

/*        Removed all warning messages. */

/* -    Version 1.0.2, 15-MAR-1999 (BVS) */

/*        Corrected comments. */

/* -    Version 1.0.1, 13-JAN-1999 (BVS) */

/*        Modified error and warning messages. */

/* -    Version 1.0.0, 8-SEP-1998 (NGK) */

/* -& */
/* $ Index_Entries */

/*     Return a string value of a MKSPK setup file keyword */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SETUPC", (ftnlen)6);
    }
    if (s_cmp(flag__, "INPUT FILE NAME", flag_len, (ftnlen)15) == 0) {

/*        Retrieve INPUT_DATA_FILE value (if it wasn't provided */
/*        on command line and therefore wasn't assigned to CHVAL). */
/*        Parameter KINDFN declared in include file. */

	if (s_cmp(chval, " ", chval_len, (ftnlen)1) == 0) {
	    gcpool_("INPUT_DATA_FILE", &c__1, &c__1, &n, chval, found, (
		    ftnlen)15, chval_len);
	    if (! (*found)) {
		setmsg_("Input file name was not provided neither on the com"
			"mand line nor as a value of the setup file keyword '"
			"#'.", (ftnlen)106);
		errch_("#", "INPUT_DATA_FILE", (ftnlen)1, (ftnlen)15);
		sigerr_("SPICE(NOINPUTFILENAME)", (ftnlen)22);
	    }
	}
	ljust_(chval, chval, chval_len, chval_len);
	fnmlen = lastnb_(chval, chval_len);
	if (fnmlen == 0) {
	    setmsg_("Input file name is blank.", (ftnlen)25);
	    sigerr_("SPICE(BLANKINPUTFILENAME)", (ftnlen)25);
	}
	if (! exists_(chval, fnmlen)) {
	    setmsg_("Input file '#' does not exist. ", (ftnlen)31);
	    errch_("#", chval, (ftnlen)1, chval_len);
	    sigerr_("SPICE(INPUTFILENOTEXIST)", (ftnlen)24);
	}
    } else if (s_cmp(flag__, "OUTPUT FILE NAME", flag_len, (ftnlen)16) == 0) {

/*        Retrieve OUTPUT_SPK_FILE value (if it wasn't provided on */
/*        the command line and therefore wasn't assigned to CHVAL) */
/*        Parameter KOSPFN declared in include file. */

	if (s_cmp(chval, " ", chval_len, (ftnlen)1) == 0) {
	    gcpool_("OUTPUT_SPK_FILE", &c__1, &c__1, &n, chval, found, (
		    ftnlen)15, chval_len);
	    if (! (*found)) {
		setmsg_("Output SPK file name was not provided neither on th"
			"e command line nor as a value of the setup file keyw"
			"ord '#'.", (ftnlen)111);
		errch_("#", "OUTPUT_SPK_FILE", (ftnlen)1, (ftnlen)15);
		sigerr_("SPICE(NOOUTPUTFILENAME)", (ftnlen)23);
	    }
	}
	ljust_(chval, chval, chval_len, chval_len);
	fnmlen = lastnb_(chval, chval_len);
	if (fnmlen == 0) {
	    setmsg_("Output file name is blank.", (ftnlen)26);
	    sigerr_("SPICE(BLANKOUTPTFILENAME)", (ftnlen)25);
	}
    } else if (s_cmp(flag__, "INPUT DATA TYPE", flag_len, (ftnlen)15) == 0) {

/*        Retrieve INPUT_DATA_TYPE value and check it. */
/*        Parameter KINDTP declared in include file. */

	gcpool_("INPUT_DATA_TYPE", &c__1, &c__1, &n, chval, found, (ftnlen)15,
		 chval_len);
	if (! (*found)) {
	    setmsg_("Type of the input data was not specified in the setup f"
		    "ile keyword '#'.", (ftnlen)71);
	    errch_("#", "INPUT_DATA_TYPE", (ftnlen)1, (ftnlen)15);
	    sigerr_("SPICE(NOINPUTDATATYPE)", (ftnlen)22);
	}
	ucase_(chval, chval, chval_len, chval_len);
	ljust_(chval, chval, chval_len, chval_len);
	if (s_cmp(chval, "STATES", rtrim_(chval, chval_len), (ftnlen)6) != 0 
		&& s_cmp(chval, "ELEMENTS", rtrim_(chval, chval_len), (ftnlen)
		8) != 0 && s_cmp(chval, "EQ_ELEMENTS", rtrim_(chval, 
		chval_len), (ftnlen)11) != 0 && s_cmp(chval, "TL_ELEMENTS", 
		rtrim_(chval, chval_len), (ftnlen)11) != 0) {
	    setmsg_("Input data type '#' specified in the setup file keyword"
		    " '#' is not recognized. Refer to the User's Guide for th"
		    "e program for the list of supported input data types.", (
		    ftnlen)164);
	    errch_("#", chval, (ftnlen)1, chval_len);
	    errch_("#", "INPUT_DATA_TYPE", (ftnlen)1, (ftnlen)15);
	    sigerr_("SPICE(UNRECOGNDATATYPE)", (ftnlen)23);
	}
    } else if (s_cmp(flag__, "REFERENCE FRAME NAME", flag_len, (ftnlen)20) == 
	    0) {

/*        Retrieve REF_FRAME_NAME value. */
/*        Parameter KRFRNM declared in include file. */

	gcpool_("REF_FRAME_NAME", &c__1, &c__1, &n, chval, found, (ftnlen)14, 
		chval_len);
	if (! (*found)) {
	    setmsg_("The name of the reference frame with respect to which t"
		    "he input trajectory data are provided was not specified "
		    "in the setup file keyword '#'.", (ftnlen)141);
	    errch_("#", "REF_FRAME_NAME", (ftnlen)1, (ftnlen)14);
	    sigerr_("SPICE(NOFRAMENAME)", (ftnlen)18);
	}
	ljust_(chval, chval, chval_len, chval_len);

/*        Get the NAIF integer code for the reference frame. */

	namfrm_(chval, &refcod, chval_len);
	if (refcod == 0) {

/*           Frame name was not recognized. See if frames file has */
/*           already been loaded. */

	    if (! floadd) {

/*              It hasn't. Retrieve FRAME_DEFINITION file name and */
/*              load it. Keyword FRKFIL declared in include file. */

		gcpool_("FRAME_DEF_FILE", &c__1, &c__1, &n, frkfn, found, (
			ftnlen)14, (ftnlen)255);
		if (! (*found)) {
		    setmsg_("The reference frame '#' is not recognized and t"
			    "he name of a frames definition kernel file which"
			    " could contain a definition for this frame was n"
			    "ot provided via the setup file keyword '#'.", (
			    ftnlen)186);
		    errch_("#", chval, (ftnlen)1, chval_len);
		    errch_("#", "FRAME_DEF_FILE", (ftnlen)1, (ftnlen)14);
		    sigerr_("SPICE(NOFRAMESKERNELNAME)", (ftnlen)25);
		}
		ljust_(frkfn, frkfn, (ftnlen)255, (ftnlen)255);
		furnsh_(frkfn, (ftnlen)255);
		floadd = TRUE_;

/*              Try to resolve frame ID again. */

		namfrm_(chval, &refcod, chval_len);
	    }

/*           Has loading FK file helped? If not, signal an error. */

	    if (refcod == 0) {
		setmsg_("The reference frame '#' is not recognized. Check wh"
			"ether the frames definition kernel file '#' specifie"
			"d in the setup file keyword '#' contains definition "
			"for this frame.", (ftnlen)170);
		errch_("#", chval, (ftnlen)1, chval_len);
		errch_("#", frkfn, (ftnlen)1, (ftnlen)255);
		errch_("#", "FRAME_DEF_FILE", (ftnlen)1, (ftnlen)14);
		sigerr_("SPICE(FRAMENOTRECOGNIZED)", (ftnlen)25);
	    }
	}
    } else if (s_cmp(flag__, "PRODUCER ID", flag_len, (ftnlen)11) == 0) {

/*        Retrieve PRODUCER_ID value. Parameter KPROID declared in */
/*        include file. */

	gcpool_("PRODUCER_ID", &c__1, &c__1, &n, chval, found, (ftnlen)11, 
		chval_len);
	if (! (*found)) {
	    setmsg_("String containing producer name was not specified in th"
		    "e setup file keyword '#'.", (ftnlen)80);
	    errch_("#", "PRODUCER_ID", (ftnlen)1, (ftnlen)11);
	    sigerr_("SPICE(NOPRODUCERID)", (ftnlen)19);
	}
	ljust_(chval, chval, chval_len, chval_len);
    } else if (s_cmp(flag__, "TIME WRAPPER", flag_len, (ftnlen)12) == 0) {

/*        Retrieve TIME_WRAPPER value. Parameter KTIMWR declared in */
/*        include file. */

	gcpool_("TIME_WRAPPER", &c__1, &c__1, &n, chval, found, (ftnlen)12, 
		chval_len);
	if (*found) {
	    if (pos_(chval, "#", &c__1, chval_len, (ftnlen)1) == 0) {
		setmsg_("Time wrapper string specified in the setup file key"
			"word '#' didn't contain special character '#' indica"
			"ting location at which time string from the input re"
			"cords should be inserted.", (ftnlen)180);
		errch_("#", "TIME_WRAPPER", (ftnlen)1, (ftnlen)12);
		sigerr_("SPICE(NO#INTIMEWRAPPER)", (ftnlen)23);
	    }
	} else {
	    s_copy(chval, "#", chval_len, (ftnlen)1);
	}
	ljust_(chval, chval, chval_len, chval_len);
    } else if (s_cmp(flag__, "SEGMENT ID", flag_len, (ftnlen)10) == 0) {

/*        Retrieve SEGMENT_ID value. Parameter KSEGID declared in */
/*        include file. */

	gcpool_("SEGMENT_ID", &c__1, &c__1, &n, chval, found, (ftnlen)10, 
		chval_len);
	if (*found) {
	    ljust_(chval, chval, chval_len, chval_len);
	}
    } else if (s_cmp(flag__, "COMMENT FILE NAME", flag_len, (ftnlen)17) == 0) 
	    {

/*        Retrieve COMMENT_FILE name value. Parameter CMTFIL declared */
/*        in include file. */

	gcpool_("COMMENT_FILE", &c__1, &c__1, &n, chval, found, (ftnlen)12, 
		chval_len);
	if (*found) {
	    ljust_(chval, chval, chval_len, chval_len);
	    fnmlen = lastnb_(chval, chval_len);

/*           Check if the comment file exists. */

	    if (! exists_(chval, fnmlen)) {
		setmsg_("The file '#' containing comments to be inserted int"
			"o the comment of the output SPK file, specified in t"
			"he setup file keyword '#' doesn't exist.", (ftnlen)
			143);
		errch_("#", chval, (ftnlen)1, chval_len);
		errch_("#", "COMMENT_FILE", (ftnlen)1, (ftnlen)12);
		sigerr_("SPICE(COMMFILENOTEXIST)", (ftnlen)23);
	    }
	}
    } else if (s_cmp(flag__, "DATA ORDER", flag_len, (ftnlen)10) == 0) {

/*        Retrieve DATA_ORDER value. Parameter KDATOR declared */
/*        in include file. */

	gcpool_("DATA_ORDER", &c__1, &c__1, &n, chval, found, (ftnlen)10, 
		chval_len);
	if (! (*found)) {
	    setmsg_("The order of parameters in input data file records was "
		    "not specified in the setup file keyword '#'.", (ftnlen)99)
		    ;
	    errch_("#", "DATA_ORDER", (ftnlen)1, (ftnlen)10);
	    sigerr_("SPICE(NODATAORDER)", (ftnlen)18);
	}
	ljust_(chval, chval, chval_len, chval_len);
	ucase_(chval, chval, chval_len, chval_len);
    } else if (s_cmp(flag__, "DATA DELIMITER", flag_len, (ftnlen)14) == 0) {

/*        Retrieve DATA_DELIMITER value and check it. Parameter KDATDL */
/*        declared in include file. */

	gcpool_("DATA_DELIMITER", &c__1, &c__1, &n, value, found, (ftnlen)14, 
		(ftnlen)12);
	if (! (*found)) {
	    setmsg_("Character separating parameters in the input data recor"
		    "ds was not specified in the setup file keyword '#'.", (
		    ftnlen)106);
	    errch_("#", "DATA_DELIMITER", (ftnlen)1, (ftnlen)14);
	    sigerr_("SPICE(NODELIMCHARACTER)", (ftnlen)23);
	}
	ljust_(value, value, (ftnlen)12, (ftnlen)12);
	ucase_(value, value, (ftnlen)12, (ftnlen)12);
	if (isrchc_(value, &c__5, alldlm, (ftnlen)12, (ftnlen)3) == 0) {

/*           Delimiter is not allowed. Error will be signaled. */

	    setmsg_("Delimiter '#' specified in the setup file keyword '#' i"
		    "s not recognized. Refer to the User's Guide for the prog"
		    "ram for the list of supported input data delimiters.", (
		    ftnlen)163);
	    errch_("#", value, (ftnlen)1, (ftnlen)12);
	    errch_("#", "DATA_DELIMITER", (ftnlen)1, (ftnlen)14);
	    sigerr_("SPICE(UNRECOGNDELIMITER)", (ftnlen)24);
	}
	s_copy(chval, value, chval_len, (ftnlen)12);

/*        Delimiter is 'EOL'. It will be replaced with EOLMRK */
/*        marker on buffer lines. */

	if (s_cmp(value, alldlm + 3, (ftnlen)12, (ftnlen)3) == 0) {
	    s_copy(chval, "#", chval_len, (ftnlen)1);
	}

/*        Delimiter is 'TAB'. It will be replaced with symbol CHAR (9) */
/*        on buffer lines. */

	if (s_cmp(value, alldlm, (ftnlen)12, (ftnlen)3) == 0) {
	    s_copy(chval, "\t", chval_len, (ftnlen)1);
	}
    } else if (s_cmp(flag__, "APPEND FLAG", flag_len, (ftnlen)11) == 0) {

/*        Retrieve APPEND_TO_OUTPUT value and check it. */
/*        Parameter KAPPND declared in the include file. */

	gcpool_("APPEND_TO_OUTPUT", &c__1, &c__1, &n, chval, found, (ftnlen)
		16, chval_len);
	if (*found) {
	    ucase_(chval, chval, chval_len, chval_len);
	    ljust_(chval, chval, chval_len, chval_len);
	    if (s_cmp(chval, "YES", rtrim_(chval, chval_len), (ftnlen)3) != 0 
		    && s_cmp(chval, "NO", rtrim_(chval, chval_len), (ftnlen)2)
		     != 0) {
		setmsg_("Append flag '#' specified in the setup file keyword"
			" '#' is not recognized. Refer to the User's Guide fo"
			"r the program for the list of supported values.", (
			ftnlen)150);
		errch_("#", chval, (ftnlen)1, chval_len);
		errch_("#", "APPEND_TO_OUTPUT", (ftnlen)1, (ftnlen)16);
		sigerr_("SPICE(UNRECOGNAPPFLAG)", (ftnlen)22);
	    }
	} else {

/*           Well, if it wasn't provided, then it's 'NO' :-) */

	    s_copy(chval, "NO", chval_len, (ftnlen)2);
	    *found = TRUE_;
	}
    } else {
	sigerr_("SPICE(MKSPKBUGSETUP1)", (ftnlen)21);
    }
    chkout_("SETUPC", (ftnlen)6);
    return 0;
/* $Procedure   SETUPI ( Return integer values to main program ) */

L_setupi:
/* $ Abstract */

/*     Return integer value of a particular setup file keyword. */

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

/*     KERNEL */

/* $ Keywords */

/*     KERNEL */
/*     POOL */

/* $ Declarations */

/*     CHARACTER*(*)         FLAG */
/*     CHARACTER*(*)         INTVAL */
/*     LOGICAL               FOUND */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     FLAG       I   Flag of keyword */
/*     INTVAL     O   Returned integer value */
/*     FOUND      O   True if variable is in pool. */

/* $ Detailed_Input */

/*     FLAG        is the string containing text identifier for the */
/*                 keyword of interest. */

/* $ Detailed_Output */

/*     INTVAL      is integer value of the keyword */

/*     FOUND       is TRUE if the value was obtained from the loaded */
/*                 setup file keyword(s). */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     See main routine header for complete list of exceptions. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     None */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.G. Khavenson (IKI RAS, Russia) */
/*     B.V. Semenov   (NAIF, JPL) */

/* $ Version */

/* -    Version 1.0.9, 12-FEB-2012 (BVS) */

/*        Added fetching input and output IDs for TLEs. */

/* -    Version 1.0.8, 21-MAR-2001 (BVS) */

/*        Added FK loading to object and center ID cases. */

/* -    Version 1.0.7, 19-JAN-2000 (BVS) */

/*        Added code to recognize SPK types 12 and 13. */

/* -    Version 1.0.6, 25-NOV-1999 (NGK) */

/*        Added output type processing for case of two-line element. */

/* -    Version 1.0.5, 29-MAR-1999 (NGK) */

/*        Corrected comments. */

/* -    Version 1.0.4, 21-MAR-1999 (BVS) */

/*        Removed all warning messages. */

/* -    Version 1.0.3, 19-MAR-1999 (BVS) */

/*        Changed type of the LINES_PER_RECORD value to integer. */

/* -    Version 1.0.2, 15-MAR-1999 (BVS) */

/*        Corrected comments. */

/* -    Version 1.0.1, 13-JAN-1999 (BVS) */

/*        Modified error and warning messages. */

/* -    Version 1.0.0, 8-SEP-1998 (NGK) */

/* -& */
/* $ Index_Entries */

/*      Return an integer value of a MKSPK setup file keyword */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SETUPI", (ftnlen)6);
    }
    if (s_cmp(flag__, "OBJECT ID CHECK", flag_len, (ftnlen)15) == 0) {

/*        Check if object ID is provided. */

	gipool_("OBJECT_ID", &c__1, &c__1, &n, intval, found, (ftnlen)9);
    } else if (s_cmp(flag__, "OBJECT NAME CHECK", flag_len, (ftnlen)17) == 0) 
	    {

/*        Check if object name is provided and can be resolved to ID. */

	gcpool_("OBJECT_NAME", &c__1, &c__1, &n, objnvl, found, (ftnlen)11, (
		ftnlen)80);
	if (*found) {
	    bodn2c_(objnvl, intval, found, (ftnlen)80);
	    if (! (*found)) {

/*              If FK was not loaded, load it and try to resolve the */
/*              name again. */

		if (! floadd) {
		    gcpool_("FRAME_DEF_FILE", &c__1, &c__1, &n, frkfn, found, 
			    (ftnlen)14, (ftnlen)255);
		    if (*found) {
			ljust_(frkfn, frkfn, (ftnlen)255, (ftnlen)255);
			furnsh_(frkfn, (ftnlen)255);
			floadd = TRUE_;
		    }
		    bodn2c_(objnvl, intval, found, (ftnlen)80);
		}
	    }
	}
    } else if (s_cmp(flag__, "OBJECT ID", flag_len, (ftnlen)9) == 0) {

/*        Retrieve OBJECT_ID value or retrieve OBJECT_NAME value and */
/*        convert them to object ID code. Parameters KOBJID and KOBJNM */
/*        declared in include file. */

	gipool_("OBJECT_ID", &c__1, &c__1, &n, intval, found, (ftnlen)9);
	if (! (*found)) {
	    gcpool_("OBJECT_NAME", &c__1, &c__1, &n, objnvl, found, (ftnlen)
		    11, (ftnlen)80);
	    if (! (*found)) {
		setmsg_("Neither object ID nor object name was specified in "
			"the setup file using '#' or '#' keyword.", (ftnlen)91)
			;
		errch_("#", "OBJECT_ID", (ftnlen)1, (ftnlen)9);
		errch_("#", "OBJECT_NAME", (ftnlen)1, (ftnlen)11);
		sigerr_("SPICE(NOOBJECTIDORNAME)", (ftnlen)23);
	    }
	    ljust_(objnvl, objnvl, (ftnlen)80, (ftnlen)80);
	    bodn2c_(objnvl, intval, found, (ftnlen)80);
	    if (! (*found)) {

/*              We couldn't resolve the name. What if name-id mapping */
/*              is provided in the frames kernel that we haven't loaded */
/*              yet? */

		if (! floadd) {
		    gcpool_("FRAME_DEF_FILE", &c__1, &c__1, &n, frkfn, found, 
			    (ftnlen)14, (ftnlen)255);
		    if (*found) {
			ljust_(frkfn, frkfn, (ftnlen)255, (ftnlen)255);
			furnsh_(frkfn, (ftnlen)255);
			floadd = TRUE_;
		    }

/*                 If FK was provided, we loaded it; can the object name */
/*                 be resolved now? */

		    bodn2c_(objnvl, intval, found, (ftnlen)80);
		}

/*              Check found flag again to see if loading FK helped. */

		if (! (*found)) {
		    setmsg_("Object name '#' specified in the setup file key"
			    "word '#' cannot be translated to NAIF ID.", (
			    ftnlen)88);
		    errch_("#", objnvl, (ftnlen)1, (ftnlen)80);
		    errch_("#", "OBJECT_NAME", (ftnlen)1, (ftnlen)11);
		    sigerr_("SPICE(BADOBJECTNAME)", (ftnlen)20);
		}
	    }
	}
    } else if (s_cmp(flag__, "CENTER ID", flag_len, (ftnlen)9) == 0) {

/*        Retrieve CENTER_ID value or retrieve CENTER_NAME value and */
/*        convert them to center ID code. Parameters KCENID and KCENNM */
/*        declared in include file. */

	gipool_("CENTER_ID", &c__1, &c__1, &n, intval, found, (ftnlen)9);
	if (! (*found)) {
	    gcpool_("CENTER_NAME", &c__1, &c__1, &n, cennvl, found, (ftnlen)
		    11, (ftnlen)80);
	    if (! (*found)) {
		setmsg_("Neither center ID nor center name was specified in "
			"the setup file using '#' or '#' keyword.", (ftnlen)91)
			;
		errch_("#", "CENTER_ID", (ftnlen)1, (ftnlen)9);
		errch_("#", "CENTER_NAME", (ftnlen)1, (ftnlen)11);
		sigerr_("SPICE(NOCENTERIDORNAME)", (ftnlen)23);
	    }
	    ljust_(cennvl, cennvl, (ftnlen)80, (ftnlen)80);
	    bodn2c_(cennvl, intval, found, (ftnlen)80);
	    if (! (*found)) {

/*              We couldn't resolve the name. What if name-id mapping */
/*              is provided in the frames kernel that we haven't loaded */
/*              yet? */

		if (! floadd) {
		    gcpool_("FRAME_DEF_FILE", &c__1, &c__1, &n, frkfn, found, 
			    (ftnlen)14, (ftnlen)255);
		    if (*found) {
			ljust_(frkfn, frkfn, (ftnlen)255, (ftnlen)255);
			furnsh_(frkfn, (ftnlen)255);
			floadd = TRUE_;
		    }

/*                 If FK was provided, we loaded it; can the center name */
/*                 be resolved now? */

		    bodn2c_(cennvl, intval, found, (ftnlen)80);
		}

/*              Check found flag again to see if loading FK helped. */

		if (! (*found)) {
		    setmsg_("Center name '#' specified in the setup file key"
			    "word '#' cannot be translated to NAIF ID.", (
			    ftnlen)88);
		    errch_("#", cennvl, (ftnlen)1, (ftnlen)80);
		    errch_("#", "CENTER_NAME", (ftnlen)1, (ftnlen)11);
		    sigerr_("SPICE(BADCENTERNAME)", (ftnlen)20);
		}
	    }
	}
    } else if (s_cmp(flag__, "OUTPUT SPK TYPE", flag_len, (ftnlen)15) == 0) {

/*        Retrieve OUTPUT_SPK_TYPE  value and check it. Parameter KOSPTP */
/*        declared in include file. */

	gipool_("OUTPUT_SPK_TYPE", &c__1, &c__1, &n, intval, found, (ftnlen)
		15);
	if (! (*found)) {
	    setmsg_("Output SPK file type was not specified in the setup fil"
		    "e keyword '#'.", (ftnlen)69);
	    errch_("#", "OUTPUT_SPK_TYPE", (ftnlen)1, (ftnlen)15);
	    sigerr_("SPICE(NOOUTPUTSPKTYPE)", (ftnlen)22);
	}
	if (*intval != 5 && *intval != 8 && *intval != 9 && *intval != 10 && *
		intval != 12 && *intval != 13 && *intval != 15 && *intval != 
		17) {
	    setmsg_("Output SPK type '#' specified in the setup file keyword"
		    " '#' is not supported in this version of the program. ", (
		    ftnlen)109);
	    errint_("#", intval, (ftnlen)1);
	    errch_("#", "OUTPUT_SPK_TYPE", (ftnlen)1, (ftnlen)15);
	    sigerr_("SPICE(SPKTYPENOTSUPPORTD)", (ftnlen)25);
	}
    } else if (s_cmp(flag__, "IGNORE FIRST LINES", flag_len, (ftnlen)18) == 0)
	     {

/*        Retrieve IGNORE_FIRST_LINES value.Parameter KIGNFL declared */
/*        in include file. */

	gipool_("IGNORE_FIRST_LINE", &c__1, &c__1, &n, intval, found, (ftnlen)
		17);
    } else if (s_cmp(flag__, "LINES PER RECORD", flag_len, (ftnlen)16) == 0) {

/*        Retrieve LINES_PER_RECORD value. Parameter KLNPRC declared */
/*        in include file. */

	gipool_("LINES_PER_RECORD", &c__1, &c__1, &n, intval, found, (ftnlen)
		16);
	if (! (*found)) {
	    setmsg_("Number of lines which a single input data file record o"
		    "ccupies was not specified in the setup file keyword '#' "
		    "as an integer value.", (ftnlen)131);
	    errch_("#", "LINES_PER_RECORD", (ftnlen)1, (ftnlen)16);
	    sigerr_("SPICE(NOLINESPERRECCOUNT)", (ftnlen)25);
	} else {
	    if (*intval <= 0) {

/*              Provided number of lines per record is less than */
/*              or equal to zero: how are we supposed to deal with */
/*              this??? */

		setmsg_("Number of lines, which a single input file record o"
			"ccupies '#', specified in the setup file keyword '#'"
			" is not a positive integer number . ", (ftnlen)139);
		errch_("#", wrkchr, (ftnlen)1, (ftnlen)512);
		errch_("#", "LINES_PER_RECORD", (ftnlen)1, (ftnlen)16);
		sigerr_("SPICE(BADLINEPERRECCOUNT)", (ftnlen)25);
	    }
	}
    } else if (s_cmp(flag__, "TIME STRING LENGTH", flag_len, (ftnlen)18) == 0)
	     {

/*        Retrieve EPOCH_STR_LENGTH value. Parameter KELENG declared */
/*        in include file. */

	gipool_("EPOCH_STR_LENGTH", &c__1, &c__1, &n, intval, found, (ftnlen)
		16);
    } else if (s_cmp(flag__, "POLYNOMIAL DEGREE", flag_len, (ftnlen)17) == 0) 
	    {

/*        Retrieve POLYNOM_DEGREE value. Parameter KPLDEG declared */
/*        in include file. */

	gipool_("POLYNOM_DEGREE", &c__1, &c__1, &n, intval, found, (ftnlen)14)
		;
	if (! (*found)) {
	    setmsg_("Polynomial degree was not specified in the setup file k"
		    "eyword '#'.", (ftnlen)66);
	    errch_("#", "POLYNOM_DEGREE", (ftnlen)1, (ftnlen)14);
	    sigerr_("SPICE(NOPOLYNOMIALDEGREE)", (ftnlen)25);
	}
    } else if (s_cmp(flag__, "TLE INPUT OBJECT ID", flag_len, (ftnlen)19) == 
	    0) {

/*        Retrieve TLE_INPUT_OBJECT_ID value. Parameter KTLTID declared */
/*        in the include file. */

	gipool_("TLE_INPUT_OBJ_ID", &c__1, &c__1, &n, intval, found, (ftnlen)
		16);
    } else if (s_cmp(flag__, "TLE SPK OBJECT ID", flag_len, (ftnlen)17) == 0) 
	    {

/*        Retrieve TLE_SPK_OBJECT_ID value. Parameter KTLSID declared */
/*        in the include file. */

	gipool_("TLE_SPK_OBJ_ID", &c__1, &c__1, &n, intval, found, (ftnlen)14)
		;
    } else {
	sigerr_("SPICE(MKSPKBUGSETUP2)", (ftnlen)21);
    }
    chkout_("SETUPI", (ftnlen)6);
    return 0;
/* $Procedure   SETUPD ( Return d.p. values to main program ) */

L_setupd:
/* $ Abstract */

/*     Return double precision value of a particular setup file */
/*     keyword. */

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

/*     KERNEL */

/* $ Keywords */

/*     KERNEL */
/*     POOL */

/* $ Declarations */

/*     CHARACTER*(*)         FLAG */
/*     CHARACTER*(*)         DPVAL */
/*     LOGICAL               FOUND */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     FLAG       I   Flag of keyword */
/*     DPVAL      O   Returned d.p. value */
/*     FOUND      O   True if variable is in pool. */

/* $ Detailed_Input */

/*     FLAG        is the string containing text identifier for the */
/*                 keyword of interest. */

/* $ Detailed_Output */

/*     DPVAL       is d.p. value of the keyword */

/*     FOUND       is TRUE if the value was obtained from the loaded */
/*                 setup file keyword(s). */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     See main routine header for complete list of exceptions. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     None */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.G. Khavenson (IKI RAS, Russia) */
/*     B.V. Semenov   (NAIF, JPL) */

/* $ Version */

/* -    Version 1.1.0, 08-FEB-2012 (BVS) */

/*        Added fetching TLE start and stop pads. */

/* -    Version 1.0.5, 29-MAR-1999 (NGK) */

/*        Corrected comments. */

/* -    Version 1.0.4, 21-MAR-1999 (BVS) */

/*        Removed all warning messages. */

/* -    Version 1.0.2, 15-MAR-1999 (BVS) */

/*        Corrected comments. */

/* -    Version 1.0.1, 13-JAN-1999 (BVS) */

/*        Modified error and warning messages. */

/* -    Version 1.0.0, 8-SEP-1998 (NGK) */

/* -& */
/* $ Index_Entries */

/*      Return a d.p. value of a MKSPK setup file keyword */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SETUPD", (ftnlen)6);
    }
    if (s_cmp(flag__, "DISTANCE COEFFICIENT", flag_len, (ftnlen)20) == 0) {

/*        Retrieve DISTANCE unit value. If this exists we put */
/*        its coefficient instead of default assigned unit (km). */
/*        Parameter KINUNI declared in include file. */

	exist = FALSE_;
	i__ = 1;
	gcpool_("INPUT_DATA_UNITS", &i__, &c__1, &n, wrkchr, found, (ftnlen)
		16, (ftnlen)512);
	if (*found) {
	    while(n != 0) {
		ucase_(wrkchr, wrkchr, (ftnlen)512, (ftnlen)512);
		astrip_(wrkchr, " ", " ", wrkchr, (ftnlen)512, (ftnlen)1, (
			ftnlen)1, (ftnlen)512);
		if (pos_(wrkchr, "DISTANCES=", &c__1, (ftnlen)512, (ftnlen)10)
			 != 0) {

/*                 Return distance units */

		    i__1 = pos_(wrkchr, "=", &c__1, (ftnlen)512, (ftnlen)1);
		    s_copy(dstunt, wrkchr + i__1, (ftnlen)80, lastnb_(wrkchr, 
			    (ftnlen)512) - i__1);
		    exist = TRUE_;
		}
		++i__;
		gcpool_("INPUT_DATA_UNITS", &i__, &c__1, &n, wrkchr, found, (
			ftnlen)16, (ftnlen)512);
	    }
	}
	if (exist) {

/*           Coefficient that converts distance unit to KM. */

	    convrt_(&c_b259, dstunt, "KM", dpval, (ftnlen)80, (ftnlen)2);
	} else {
	    *found = FALSE_;
	}
    } else if (s_cmp(flag__, "ANGLE COEFFICIENT", flag_len, (ftnlen)17) == 0) 
	    {

/*        Retrieve ANGLE unit value. If this exists we put */
/*        its coefficient instead of default coefficient. */
/*        Parameter KINUNI declared in include file. */

	exist = FALSE_;
	i__ = 1;
	gcpool_("INPUT_DATA_UNITS", &i__, &c__1, &n, wrkchr, found, (ftnlen)
		16, (ftnlen)512);
	if (*found) {
	    while(n != 0) {
		ucase_(wrkchr, wrkchr, (ftnlen)512, (ftnlen)512);
		astrip_(wrkchr, " ", " ", wrkchr, (ftnlen)512, (ftnlen)1, (
			ftnlen)1, (ftnlen)512);
		if (pos_(wrkchr, "ANGLES=", &c__1, (ftnlen)512, (ftnlen)7) != 
			0) {

/*                 Return angle units */

		    i__1 = pos_(wrkchr, "=", &c__1, (ftnlen)512, (ftnlen)1);
		    s_copy(angunt, wrkchr + i__1, (ftnlen)80, lastnb_(wrkchr, 
			    (ftnlen)512) - i__1);
		    exist = TRUE_;
		}
		++i__;
		gcpool_("INPUT_DATA_UNITS", &i__, &c__1, &n, wrkchr, found, (
			ftnlen)16, (ftnlen)512);
	    }
	}
	if (exist) {

/*           Coefficient that converts angle unit to radian. */

	    convrt_(&c_b259, angunt, "RADIANS", dpval, (ftnlen)80, (ftnlen)7);
	} else {
	    *found = FALSE_;
	}
    } else if (s_cmp(flag__, "START TIME", flag_len, (ftnlen)10) == 0) {

/*        Retrieve START_TIME value. Parameter KSTATM declared */
/*        in include file. */

	gcpool_("START_TIME", &c__1, &c__1, &n, sttmvl, found, (ftnlen)10, (
		ftnlen)80);
	if (*found) {
	    str2et_(sttmvl, dpval, (ftnlen)80);
	}
    } else if (s_cmp(flag__, "STOP TIME", flag_len, (ftnlen)9) == 0) {

/*        Retrieve STOP_TIME value. Parameter KSTOTM declared */
/*        in include file. */

	gcpool_("STOP_TIME", &c__1, &c__1, &n, sptmvl, found, (ftnlen)9, (
		ftnlen)80);
	if (*found) {
	    str2et_(sptmvl, dpval, (ftnlen)80);
	}
    } else if (s_cmp(flag__, "CENTER BODY GM", flag_len, (ftnlen)14) == 0) {

/*        Retrieve CENTER_GM value. Parameter KCENGM declared */
/*        in include file. */

	gdpool_("CENTER_GM", &c__1, &c__1, &n, dpval, found, (ftnlen)9);
    } else if (s_cmp(flag__, "J2", flag_len, (ftnlen)2) == 0) {

/*        Retrieve CENTER_J2 value. Parameter KCENJ2 declared */
/*        in include file. */

	gdpool_("CENTER_J2", &c__1, &c__1, &n, dpval, found, (ftnlen)9);
    } else if (s_cmp(flag__, "EQUATORIAL RADIUS", flag_len, (ftnlen)17) == 0) 
	    {

/*        Retrieve CENTER_EQ_RADIUS value. Parameter KCNEQR declared */
/*        in include file. */

	gdpool_("CENTER_EQ_RADIUS", &c__1, &c__1, &n, dpval, found, (ftnlen)
		16);
    } else if (s_cmp(flag__, "PRECESSION TYPE", flag_len, (ftnlen)15) == 0) {

/*        Retrieve PRECESSION_TYPE value and set J2 flags */
/*        according to it. Parameter KPRTYP declared in include file. */

	gcpool_("PRECESSION_TYPE", &c__1, &c__1, &n, prctvl, found, (ftnlen)
		15, (ftnlen)80);
	if (! (*found)) {
	    setmsg_("Orbit precession type was not specified in the setup fi"
		    "le keyword '#'.", (ftnlen)70);
	    errch_("#", "PRECESSION_TYPE", (ftnlen)1, (ftnlen)15);
	    sigerr_("SPICE(NOPRECESSIONTYPE)", (ftnlen)23);
	}
	ljust_(prctvl, prctvl, (ftnlen)80, (ftnlen)80);
	ucase_(prctvl, prctvl, (ftnlen)80, (ftnlen)80);
	if (s_cmp(prctvl, "NO PRECESSION", rtrim_(prctvl, (ftnlen)80), (
		ftnlen)13) == 0) {
	    *dpval = 3.;
	} else if (s_cmp(prctvl, "APSIDE PRECESSION ONLY", rtrim_(prctvl, (
		ftnlen)80), (ftnlen)22) == 0) {
	    *dpval = 2.;
	} else if (s_cmp(prctvl, "NODE PRECESSION ONLY", rtrim_(prctvl, (
		ftnlen)80), (ftnlen)20) == 0) {
	    *dpval = 1.;
	} else if (s_cmp(prctvl, "APSIDE AND NODE PRECESSION", rtrim_(prctvl, 
		(ftnlen)80), (ftnlen)26) == 0) {
	    *dpval = 0.;
	} else {
	    setmsg_("Precession type '#' specified in the setup file keyword"
		    " '#' is not recognized. Refer to the User's Guide for th"
		    "e program for the list of supported precession types.", (
		    ftnlen)164);
	    errch_("#", prctvl, (ftnlen)1, (ftnlen)80);
	    errch_("#", "PRECESSION_TYPE", (ftnlen)1, (ftnlen)15);
	    sigerr_("SPICE(UNRECOGNPRECTYPE)", (ftnlen)23);
	}
    } else if (s_cmp(flag__, "POLE RA", flag_len, (ftnlen)7) == 0) {

/*        Retrieve CENTER_POLE_RA value. Parameter KCEPRA declared */
/*        in include file. */

	gdpool_("CENTER_POLE_RA", &c__1, &c__1, &n, dpval, found, (ftnlen)14);
    } else if (s_cmp(flag__, "POLE DEC", flag_len, (ftnlen)8) == 0) {

/*        Retrieve CENTER_POLE_DEC value. Parameter KCPDEC declared */
/*        in include file. */

	gdpool_("CENTER_POLE_DEC", &c__1, &c__1, &n, dpval, found, (ftnlen)15)
		;
    } else if (s_cmp(flag__, "TLE COVERAGE START PAD", flag_len, (ftnlen)22) 
	    == 0 || s_cmp(flag__, "TLE COVERAGE STOP PAD", flag_len, (ftnlen)
	    21) == 0) {

/*        Retrieve TLE coverage value. Set keyword name based on whether */
/*        the start pad or the stop pad is requested. */

	if (s_cmp(flag__, "TLE COVERAGE START PAD", flag_len, (ftnlen)22) == 
		0) {
	    s_copy(keywrd, "TLE_START_PAD", (ftnlen)80, (ftnlen)13);
	} else if (s_cmp(flag__, "TLE COVERAGE STOP PAD", flag_len, (ftnlen)
		21) == 0) {
	    s_copy(keywrd, "TLE_STOP_PAD", (ftnlen)80, (ftnlen)12);
	} else {
	    sigerr_("SPICE(MKSPKBUGSETUP5)", (ftnlen)21);
	}
	gcpool_(keywrd, &c__1, &c__1, &n, padvl, found, (ftnlen)80, (ftnlen)
		80);
	if (*found) {

/*           Check that the pad value consists of two items, number and */
/*           units. */

	    if (wdcnt_(padvl, (ftnlen)80) == 2) {

/*              Split the value string and convert the first item to a */
/*              DP number. */

		nextwd_(padvl, chrdp, units, (ftnlen)80, (ftnlen)80, (ftnlen)
			80);
		nparsd_(chrdp, &wrkdp, wrkchr, &i__, (ftnlen)80, (ftnlen)512);
		if (i__ == 0) {

/*                 Check that the pad is not negative. */

		    if (wrkdp < 0.) {
			setmsg_("The value '#' specified as the first item i"
				"n the value of the setup file keyword '#' is"
				" negative. Negative pads are not allowed.", (
				ftnlen)128);
			errch_("#", chrdp, (ftnlen)1, (ftnlen)80);
			errch_("#", keywrd, (ftnlen)1, (ftnlen)80);
			sigerr_("SPICE(BADTLECOVERAGEPAD3)", (ftnlen)25);
		    }

/*                 Convert pad to seconds. */

		    convrt_(&wrkdp, units, "SECONDS", dpval, (ftnlen)80, (
			    ftnlen)7);
		} else {
		    setmsg_("The value '#' specified as the first item in th"
			    "e value of the setup file keyword '#' is not a n"
			    "umber.", (ftnlen)101);
		    errch_("#", chrdp, (ftnlen)1, (ftnlen)80);
		    errch_("#", keywrd, (ftnlen)1, (ftnlen)80);
		    sigerr_("SPICE(BADTLECOVERAGEPAD2)", (ftnlen)25);
		}
	    } else {
		setmsg_("The TLE coverage pad '#' specified in the setup fil"
			"e keyword '#' does not consists of exactly two items"
			", a number and a word representing time units.", (
			ftnlen)149);
		errch_("#", padvl, (ftnlen)1, (ftnlen)80);
		errch_("#", keywrd, (ftnlen)1, (ftnlen)80);
		sigerr_("SPICE(BADTLECOVERAGEPAD)", (ftnlen)24);
	    }
	}
    } else {
	sigerr_("SPICE(MKSPKBUGSETUP3)", (ftnlen)21);
    }
    chkout_("SETUPD", (ftnlen)6);
    return 0;
/* $Procedure   SETUPA ( Get LSK and PCK file names and load its content ) */

L_setupa:
/* $ Abstract */

/*     Load LSK and PCK files provided in a setup file. */

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

/*     KERNEL */
/*     PCK */

/* $ Keywords */

/*     KERNEL */
/*     POOL */

/* $ Declarations */

/*     CHARACTER*(*)         FLAG */
/*     LOGICAL               FOUND */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     FLAG       I   Flag of keyword */
/*     FOUND      O   True if variable is in pool. */

/* $ Detailed_Input */

/*     FLAG        is the string containing text identifier for the */
/*                 keyword of interest. */

/* $ Detailed_Output */

/*     FOUND       is TRUE if the files were found and loaded. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     See main routine header for complete list of exceptions. */

/* $ Files */

/*     This routine requires LSK and PCK files specified in the */
/*     corresponding setup file keywords to exist and be legitimate */
/*     SPICE kernel file. */

/* $ Particulars */

/*     This routine does not return the value of LSK and PCK */
/*     file names but loads corresponding files using the pool or */
/*     binary loaders. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.G. Khavenson (IKI RAS, Russia) */
/*     B.V. Semenov   (NAIF, JPL) */

/* $ Version */

/* -    Version 1.0.5, 29-MAR-1999 (NGK) */

/*        Corrected comments. */

/* -    Version 1.0.4, 21-MAR-1999 (BVS) */

/*        Removed all warning messages. */

/* -    Version 1.0.2, 15-MAR-1999 (BVS) */

/*        Corrected comments. */

/* -    Version 1.0.1, 13-JAN-1999 (BVS) */

/*        Modified error and warning messages. */

/* -    Version 1.0.0, 8-SEP-1998 (NGK) */

/* -& */
/* $ Index_Entries */

/*      Load LSK and PCK files provided in a MKSPK setup file */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SETUPA", (ftnlen)6);
    }
    if (s_cmp(flag__, "LOAD LSK FILE", flag_len, (ftnlen)13) == 0) {

/*        Retrieve LEAPSECONDS file name and load it. Parameter LSKFIL */
/*        declared in include file. */

	gcpool_("LEAPSECONDS_FILE", &c__1, &c__1, &n, lskfn, found, (ftnlen)
		16, (ftnlen)255);
	if (! (*found)) {
	    setmsg_("Leapsecond file name was not specified in the setup fil"
		    "e keyword '#'.", (ftnlen)69);
	    errch_("#", "LEAPSECONDS_FILE", (ftnlen)1, (ftnlen)16);
	    sigerr_("SPICE(NOLSKFILENAME)", (ftnlen)20);
	}
	ljust_(lskfn, lskfn, (ftnlen)255, (ftnlen)255);
	furnsh_(lskfn, (ftnlen)255);
    } else if (s_cmp(flag__, "LOAD PCK FILE", flag_len, (ftnlen)13) == 0) {

/*        Retrieve PCK_FILE names value. Parameter PCKFIL declared in */
/*        include file. */

/*        Check if keyword exists and get number of its values. */

	dtpool_("PCK_FILE", found, &n, type__, (ftnlen)8, (ftnlen)80);
	if (*found && s_cmp(type__, "C", (ftnlen)80, (ftnlen)1) == 0) {
	    i__1 = n;
	    for (i__ = 1; i__ <= i__1; ++i__) {

/*              Get next file name, left-justify it and get file's */
/*              type and architecture */

		gcpool_("PCK_FILE", &i__, &c__1, &n, wrkchr, found, (ftnlen)8,
			 (ftnlen)512);
		ljust_(wrkchr, wrkchr, (ftnlen)512, (ftnlen)512);
		getfat_(wrkchr, arch, type__, (ftnlen)512, (ftnlen)80, (
			ftnlen)80);
		if (s_cmp(type__, "PCK", (ftnlen)80, (ftnlen)3) == 0 && s_cmp(
			arch, "DAF", (ftnlen)80, (ftnlen)3) == 0) {

/*                 It's a binary PCK file. Use binary loader PCKLOF. */

		    pcklof_(wrkchr, &handle, (ftnlen)512);
		} else if (s_cmp(type__, "PCK", (ftnlen)80, (ftnlen)3) == 0 &&
			 s_cmp(arch, "KPL", (ftnlen)80, (ftnlen)3) == 0) {

/*                 Text PCK. Call FURNSH. */

		    furnsh_(wrkchr, (ftnlen)512);
		} else {

/*                 Unknown combination of TYPE/ARCHITECTURE. Complain. */

		    setmsg_("The file '#' specified in the setup file keywor"
			    "d '#' is not a text or binary PCK file.", (ftnlen)
			    86);
		    errch_("#", wrkchr, (ftnlen)1, (ftnlen)512);
		    errch_("#", "PCK_FILE", (ftnlen)1, (ftnlen)8);
		    sigerr_("SPICE(NOTAPCKFILE)", (ftnlen)18);
		}
	    }
	}
    } else {
	sigerr_("SPICE(MKSPKBUGSETUP4)", (ftnlen)21);
    }
    chkout_("SETUPA", (ftnlen)6);
    return 0;
} /* setup_ */

/* Subroutine */ int setup_(char *flag__, char *chval, integer *intval, 
	doublereal *dpval, logical *found, ftnlen flag_len, ftnlen chval_len)
{
    return setup_0_(0, flag__, chval, intval, dpval, found, flag_len, 
	    chval_len);
    }

/* Subroutine */ int setupc_(char *flag__, char *chval, logical *found, 
	ftnlen flag_len, ftnlen chval_len)
{
    return setup_0_(1, flag__, chval, (integer *)0, (doublereal *)0, found, 
	    flag_len, chval_len);
    }

/* Subroutine */ int setupi_(char *flag__, integer *intval, logical *found, 
	ftnlen flag_len)
{
    return setup_0_(2, flag__, (char *)0, intval, (doublereal *)0, found, 
	    flag_len, (ftnint)0);
    }

/* Subroutine */ int setupd_(char *flag__, doublereal *dpval, logical *found, 
	ftnlen flag_len)
{
    return setup_0_(3, flag__, (char *)0, (integer *)0, dpval, found, 
	    flag_len, (ftnint)0);
    }

/* Subroutine */ int setupa_(char *flag__, logical *found, ftnlen flag_len)
{
    return setup_0_(4, flag__, (char *)0, (integer *)0, (doublereal *)0, 
	    found, flag_len, (ftnint)0);
    }

