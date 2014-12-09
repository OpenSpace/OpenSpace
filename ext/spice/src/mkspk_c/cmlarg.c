/* cmlarg.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__1 = 1;
static integer c__2 = 2;

/* $Procedure      CMLARG ( Get command line arguments ) */
/* Subroutine */ int cmlarg_(char *cmdfil, char *inpfn, char *outfn, logical *
	appflg, ftnlen cmdfil_len, ftnlen inpfn_len, ftnlen outfn_len)
{
    /* System generated locals */
    address a__1[3], a__2[2];
    integer i__1[3], i__2, i__3[2];

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen), s_cat(char *,
	     char **, integer *, integer *, ftnlen);
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_stop(char *, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char line[512], rest[512];
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer nblen_(char *, ftnlen);
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen), 
	    errch_(char *, char *, ftnlen, ftnlen);
    char error[512];
    extern integer rtrim_(char *, ftnlen);
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    char begdat[11];
    extern logical return_(void);
    char messge[80*50], begtxt[11], inptyp[11], outtyp[11], cmltmg[11];
    integer ioutyp;
    extern /* Subroutine */ int getcml_(char *, ftnlen), tostdo_(char *, 
	    ftnlen), nextwd_(char *, char *, char *, ftnlen, ftnlen, ftnlen), 
	    setmsg_(char *, ftnlen), sigerr_(char *, ftnlen), nparsi_(char *, 
	    integer *, char *, integer *, ftnlen, ftnlen), errint_(char *, 
	    integer *, ftnlen), prompt_(char *, char *, ftnlen, ftnlen), 
	    chkout_(char *, ftnlen);
    extern integer pos_(char *, char *, integer *, ftnlen, ftnlen);
    integer ptr;

/* $ Abstract */

/*     This routine is a module of the MKSPK program. It parses */
/*     command line and returns setup, input, and output files names */
/*     and a flag indicating that appending to an existing output */
/*     file was requested, or displays help, usage or template */
/*     information if either of them was requested using corresponding */
/*     command line switch. */

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

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     CMDFIL     O   Setup filename */
/*     INPFN      O   Input filename */
/*     OUTFN      O   Output filename */
/*     APPFLG     O   Append flag */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     CMDFIL      is the setup file name provided on command line after */
/*                 corresponding switch or interactively in response */
/*                 to the prompt. */

/*     INPFN       is the input file name provided on command line after */
/*                 corresponding switch. If it wasn't provided, it's set */
/*                 to blank. */

/*     OUTFN       is the output file name provided on command line */
/*                 after corresponding switch. If it wasn't provided, */
/*                 it's set to blank. */

/*     APPFLG      is the flag indicating that appending to an */
/*                 existing output file was requested. If appropriate */
/*                 command line switch was present, this flag is set */
/*                 to .TRUE. Otherwise, it's set to .FALSE. */

/* $ Parameters */

/*     Command line keys are declared as parameters in the include */
/*     file. */

/* $ Exceptions */

/*     1) If input data type specified on command line after */
/*        -t/-template command line option is not recognized, then */
/*        the error 'SPICE(UNRECOGNDATATYPE)' will be signaled. */

/*     2) If only input data type specified on the command line after */
/*        -t/-template command line option, then the error */
/*        'SPICE(MISSINGARGUMENTS)' will be signaled. */

/*     3) If output SPK type provided on the command line after */
/*        -t/-template command line option is not an integer number */
/*        is not one of the supported types, then the error */
/*        'SPICE(BADOUTPUTSPKTYPE)' will be signaled. */

/*     4) If output SPK type and input data types provided after */
/*        -t/-template command line option mismatch, then the error */
/*        'SPICE(TYPESMISMATCH)' will be signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     If setup filename was not provided on the command line after */
/*     corresponding switch, this routine will prompt for it. It will */
/*     not prompt for input and output file names if they weren't */
/*     provided. */

/*     If usage, help or template display was requested, the routine */
/*     displays it and stops. */

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

/* -    Version 2.1.0, 13-FEB-2012 (BVS) */

/*        Updated complete and TLE-specific template displays for new */
/*        TLE keywords. */

/* -    Version 2.0.0, 20-MAR-2001 (BVS) */

/*        Changed calling sequence to also return a flag indicating */
/*        that appending to an existing output file was requested. */
/*        Added processing of the command line parameter '-append'. */
/*        Changed usage and help displays to show the command */
/*        line option '-append' and corresponding setup file */
/*        keyword APPEND_TO_OUTPUT. */

/* -    Version 1.0.8, 28-JAN-2000 (BVS) */

/*        Fixed TLE/10 template to contain producer ID. */

/* -    Version 1.0.7, 20-JAN-2000 (BVS) */

/*        Added code to recognize output SPK types 12 and 13. */

/* -    Version 1.0.6, 18-JAN-2000 (BVS) */

/*        Added a few more redundant error checks (for more polite */
/*        and clear complaints :-). Replaced straight string */
/*        comparisons with calls to EQSTR. Corrected help display to */
/*        mention two-line elements input. */

/* -    Version 1.0.5, 25-NOV-1999 (NGK) */

/*        Added template display for two-line element processing. */

/* -    Version 1.0.4, 29-SEP-1999 (NGK) */

/*        Added template displays depending of input data type and */
/*        output SPK type. */

/* -    Version 1.0.3, 09-SEP-1999 (NGK) */

/*        Added parameters and special processing for \begintext and */
/*        \begindata tokens to ensure that they are printed correctly */
/*        on all platforms. */

/* -    Version 1.0.2, 29-MAR-1999 (NGK) */

/*        Added comments. */

/* -    Version 1.0.1, 18-MAR-1999 (BVS) */

/*        Added usage, help and template displays. Corrected comments. */

/* -    Version 1.0.0, 8-SEP-1998 (NGK) */

/* -& */
/* $ Index_Entries */

/*     Get command line arguments */

/* -& */

/*     SPICELIB functions */


/*     Local parameters. Size SHRTLN declared in the include file. */


/*     Local variables */

/*     Generic line variable. Size LINLEN declared in the include file. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("CMLARG", (ftnlen)6);
    }

/*     Form the terms that must enclose setup assignments */

    s_copy(begdat, "\\begindata                                             "
	    "                          ", (ftnlen)11, (ftnlen)81);
    s_copy(begtxt, "\\begintext                                             "
	    "                          ", (ftnlen)11, (ftnlen)81);

/*     Read command line. Add one space at the beginning and one the */
/*     end of it to be able to search for command line keys surrounded */
/*     by spaces. */

    getcml_(line, (ftnlen)512);
/* Writing concatenation */
    i__1[0] = 1, a__1[0] = " ";
    i__1[1] = rtrim_(line, (ftnlen)512), a__1[1] = line;
    i__1[2] = 1, a__1[2] = " ";
    s_cat(rest, a__1, i__1, &c__3, (ftnlen)512);
    s_copy(line, rest, (ftnlen)512, (ftnlen)512);

/*     Display usage and stop. Flags CMLUSS and CMLUSL are declared */
/*     in the include file. */

    if (pos_(line, " -u ", &c__1, (ftnlen)512, (ftnlen)4) != 0 || pos_(line, 
	    " -usage ", &c__1, (ftnlen)512, (ftnlen)8) != 0) {
	s_copy(messge, "     Program usage:", (ftnlen)80, (ftnlen)19);
	s_copy(messge + 80, " ", (ftnlen)80, (ftnlen)1);
	s_copy(messge + 160, "           > mkspk [-setup <setup file name>]", 
		(ftnlen)80, (ftnlen)45);
	s_copy(messge + 240, "                   [-input <input data file na"
		"me>]", (ftnlen)80, (ftnlen)50);
	s_copy(messge + 320, "                   [-output <output SPK file n"
		"ame>]", (ftnlen)80, (ftnlen)51);
	s_copy(messge + 400, "                   [-append]", (ftnlen)80, (
		ftnlen)28);
	s_copy(messge + 480, "                   [-u|-usage]", (ftnlen)80, (
		ftnlen)30);
	s_copy(messge + 560, "                   [-h|-help]", (ftnlen)80, (
		ftnlen)29);
	s_copy(messge + 640, "                   [-t|-template] [<input data"
		" type> <output spk type>]", (ftnlen)80, (ftnlen)71);
	s_copy(messge + 720, " ", (ftnlen)80, (ftnlen)1);
	s_copy(messge + 800, "     If a setup file name isn't provided on th"
		"e command line, the program", (ftnlen)80, (ftnlen)73);
	s_copy(messge + 880, "     will prompt for it. It will not prompt fo"
		"r the input or output file", (ftnlen)80, (ftnlen)72);
	s_copy(messge + 960, "     names; these file names must be provided "
		"on the command line or in the", (ftnlen)80, (ftnlen)75);
	s_copy(messge + 1040, "     setup file. If input and output file nam"
		"es are provided on the command", (ftnlen)80, (ftnlen)75);
	s_copy(messge + 1120, "     line, any file names assigned using setu"
		"p keywords are ignored. The", (ftnlen)80, (ftnlen)72);
	s_copy(messge + 1200, "     input file must already exist and, if -a"
		"ppend key or corresponding ", (ftnlen)80, (ftnlen)72);
	s_copy(messge + 1280, "     setup file keyword is not specified, the"
		" output file must be a new file.", (ftnlen)80, (ftnlen)77);
	s_copy(messge + 1360, " ", (ftnlen)80, (ftnlen)1);
	for (i__ = 1; i__ <= 18; ++i__) {
	    tostdo_(messge + ((i__2 = i__ - 1) < 50 && 0 <= i__2 ? i__2 : 
		    s_rnge("messge", i__2, "cmlarg_", (ftnlen)325)) * 80, (
		    ftnlen)80);
	}
	s_stop("", (ftnlen)0);
    }

/*     Display help and stop. Flags CMLHLS and CMLHLL are declared */
/*     in the include file. */

    if (pos_(line, " -h ", &c__1, (ftnlen)512, (ftnlen)4) != 0 || pos_(line, 
	    " -help ", &c__1, (ftnlen)512, (ftnlen)7) != 0) {
	s_copy(messge, "     MKSPK is a NAIF Toolkit utility program that ge"
		"nerates a spacecraft or", (ftnlen)80, (ftnlen)75);
	s_copy(messge + 80, "     target body's ephemeris file in SPICE SPK "
		"format using one of the", (ftnlen)80, (ftnlen)70);
	s_copy(messge + 160, "     following SPK types: 5, 8, 9, 10, 12, 13,"
		" 15, 17. This SPK file is", (ftnlen)80, (ftnlen)71);
	s_copy(messge + 240, "     a binary file constructed according to th"
		"e DAF (Double precision Array", (ftnlen)80, (ftnlen)75);
	s_copy(messge + 320, "     File) architecture, containing one or mor"
		"e SPK data segments.", (ftnlen)80, (ftnlen)66);
	s_copy(messge + 400, " ", (ftnlen)80, (ftnlen)1);
	s_copy(messge + 480, "     The MKSPK program accepts one ASCII text "
		"file containing various", (ftnlen)80, (ftnlen)69);
	s_copy(messge + 560, "     descriptions of input data (setup file) a"
		"nd a second ASCII text file", (ftnlen)80, (ftnlen)73);
	s_copy(messge + 640, "     (input file) containing the ephemeris dat"
		"a to be processed.", (ftnlen)80, (ftnlen)64);
	s_copy(messge + 720, " ", (ftnlen)80, (ftnlen)1);
	s_copy(messge + 800, "     Input data could be time ordered lists of"
		" states (positions and", (ftnlen)80, (ftnlen)68);
	s_copy(messge + 880, "     velocities), or sets of conic elements, o"
		"r a set of equinoctial", (ftnlen)80, (ftnlen)68);
	s_copy(messge + 960, "     elements, or sets of two-line elements. A"
		"ll input data must", (ftnlen)80, (ftnlen)64);
	s_copy(messge + 1040, "      be defined in a reference frame and rel"
		"ative to a body center, ", (ftnlen)80, (ftnlen)69);
	s_copy(messge + 1120, "     both of which are specified in the setup",
		 (ftnlen)80, (ftnlen)45);
	s_copy(messge + 1200, "     file.", (ftnlen)80, (ftnlen)10);
	s_copy(messge + 1280, " ", (ftnlen)80, (ftnlen)1);
	s_copy(messge + 1360, "     The program also allows the user to opti"
		"onally specify some", (ftnlen)80, (ftnlen)64);
	s_copy(messge + 1440, "     descriptive text in a separate file (com"
		"ment file) to be placed into", (ftnlen)80, (ftnlen)73);
	s_copy(messge + 1520, "     the ``comment area'' of the NAIF SPK eph"
		"emeris file.", (ftnlen)80, (ftnlen)57);
	s_copy(messge + 1600, " ", (ftnlen)80, (ftnlen)1);
	s_copy(messge + 1680, "     For documentation purposes content of th"
		"e MKSPK setup file is", (ftnlen)80, (ftnlen)66);
	s_copy(messge + 1760, "     automatically placed at the end of the `"
		"`comment area'' of the SPK", (ftnlen)80, (ftnlen)71);
	s_copy(messge + 1840, "     file.", (ftnlen)80, (ftnlen)10);
	s_copy(messge + 1920, " ", (ftnlen)80, (ftnlen)1);
	s_copy(messge + 2000, "     Run the program with -usage command line"
		" key to see usage information.", (ftnlen)80, (ftnlen)75);
	s_copy(messge + 2080, " ", (ftnlen)80, (ftnlen)1);
	for (i__ = 1; i__ <= 27; ++i__) {
	    tostdo_(messge + ((i__2 = i__ - 1) < 50 && 0 <= i__2 ? i__2 : 
		    s_rnge("messge", i__2, "cmlarg_", (ftnlen)392)) * 80, (
		    ftnlen)80);
	}
	s_stop("", (ftnlen)0);
    }

/*     Display template depending of input/output type and stop. */
/*     Flags CMLTMS and CMLTML and template keywords are declared */
/*     in the include file. */

    if (pos_(line, " -t ", &c__1, (ftnlen)512, (ftnlen)4) != 0 || pos_(line, 
	    " -template ", &c__1, (ftnlen)512, (ftnlen)11) != 0) {

/*        Two top lines in the message are left blank because they will */
/*        be set depending on whether complete or input/output type */
/*        specific template will be displayed. */

	s_copy(messge, " ", (ftnlen)80, (ftnlen)1);
	s_copy(messge + 80, " ", (ftnlen)80, (ftnlen)1);
	s_copy(messge + 160, " ", (ftnlen)80, (ftnlen)1);
	s_copy(messge + 240, begdat, (ftnlen)80, (ftnlen)11);
	s_copy(messge + 320, "   INPUT_DATA_TYPE   = 'STATES' or 'ELEMENTS' "
		"or 'EQ_ELEMENTS' or 'TL_ELEMENTS'", (ftnlen)80, (ftnlen)79);
	s_copy(messge + 400, "   OUTPUT_SPK_TYPE   = 5 or 8 or 9 or 10 or 12"
		" or 13 or 15 or 17", (ftnlen)80, (ftnlen)64);
	s_copy(messge + 480, "   OBJECT_ID         = NAIF numeric code of th"
		"e object or TLE s/c code", (ftnlen)80, (ftnlen)70);
	s_copy(messge + 560, " or", (ftnlen)80, (ftnlen)3);
	s_copy(messge + 640, "   OBJECT_NAME       = 'NAIF supported object "
		"name'", (ftnlen)80, (ftnlen)51);
	s_copy(messge + 720, "   CENTER_ID         = NAIF numeric code of th"
		"e central body", (ftnlen)80, (ftnlen)60);
	s_copy(messge + 800, " or", (ftnlen)80, (ftnlen)3);
	s_copy(messge + 880, "   CENTER_NAME       = 'NAIF supported body na"
		"me'", (ftnlen)80, (ftnlen)49);
	s_copy(messge + 960, "   REF_FRAME_NAME    = 'reference frame name'", 
		(ftnlen)80, (ftnlen)45);
	s_copy(messge + 1040, "   PRODUCER_ID       = 'producer identifier'", 
		(ftnlen)80, (ftnlen)44);
	s_copy(messge + 1120, "   DATA_ORDER        = 'ordered list of input"
		" parameter names'", (ftnlen)80, (ftnlen)62);
	s_copy(messge + 1200, "   DATA_DELIMITER    = 'delimiter separating "
		"input data items'", (ftnlen)80, (ftnlen)62);
	s_copy(messge + 1280, "   LEAPSECONDS_FILE  = 'leapseconds file name'"
		, (ftnlen)80, (ftnlen)46);
	s_copy(messge + 1360, "   INPUT_DATA_FILE   = 'input data file name'",
		 (ftnlen)80, (ftnlen)45);
	s_copy(messge + 1440, "   OUTPUT_SPK_FILE   = 'output SPK file name'",
		 (ftnlen)80, (ftnlen)45);
	s_copy(messge + 1520, "   PCK_FILE          = ( 'PCK_1 file name' 'P"
		"CK_2 file name' ... )", (ftnlen)80, (ftnlen)66);
	s_copy(messge + 1600, "   FRAME_DEF_FILE    = 'frame definition file"
		" name'", (ftnlen)80, (ftnlen)51);
	s_copy(messge + 1680, "   COMMENT_FILE      = 'comment file name'", (
		ftnlen)80, (ftnlen)42);
	s_copy(messge + 1760, "   INPUT_DATA_UNITS  = ( 'ANGLES = ang. unit'"
		" 'DISTANCES= dist. unit' )", (ftnlen)80, (ftnlen)71);
	s_copy(messge + 1840, "   EPOCH_STR_LENGTH  = length of epoch string",
		 (ftnlen)80, (ftnlen)45);
	s_copy(messge + 1920, "   IGNORE_FIRST_LINE = number of initial file"
		" lines to be ignored", (ftnlen)80, (ftnlen)65);
	s_copy(messge + 2000, "   LINES_PER_RECORD  = number of lines in one"
		" input record", (ftnlen)80, (ftnlen)58);
	s_copy(messge + 2080, "   TIME_WRAPPER      = '# time wrapper'", (
		ftnlen)80, (ftnlen)39);
	s_copy(messge + 2160, "   START_TIME        = 'start time'", (ftnlen)
		80, (ftnlen)35);
	s_copy(messge + 2240, "   STOP_TIME         = 'stop time'", (ftnlen)
		80, (ftnlen)34);
	s_copy(messge + 2320, "   PRECESSION_TYPE   = 'NO PRECESSION' or", (
		ftnlen)80, (ftnlen)41);
	s_copy(messge + 2400, "                       'APSIDE PRECESSION ONL"
		"Y' or", (ftnlen)80, (ftnlen)50);
	s_copy(messge + 2480, "                       'NODE PRECESSION ONLY'"
		" or", (ftnlen)80, (ftnlen)48);
	s_copy(messge + 2560, "                       'APSIDE AND NODE PRECE"
		"SSION'", (ftnlen)80, (ftnlen)51);
	s_copy(messge + 2640, "   POLYNOM_DEGREE    = polynomial degree of L"
		"agrange or Hermite interpolation", (ftnlen)80, (ftnlen)77);
	s_copy(messge + 2720, "   CENTER_GM         = center GM value", (
		ftnlen)80, (ftnlen)38);
	s_copy(messge + 2800, "   CENTER_POLE_RA    = the right ascension of"
		" the center's north pole", (ftnlen)80, (ftnlen)69);
	s_copy(messge + 2880, "   CENTER_POLE_DEC   = the declination of the"
		" center's north pole", (ftnlen)80, (ftnlen)65);
	s_copy(messge + 2960, "   CENTER_J2         = center's J2 value", (
		ftnlen)80, (ftnlen)40);
	s_copy(messge + 3040, "   CENTER_EQ_RADIUS  = center's equatorial ra"
		"dius", (ftnlen)80, (ftnlen)49);
	s_copy(messge + 3120, "   SEGMENT_ID        = 'segment identifier'", (
		ftnlen)80, (ftnlen)43);
	s_copy(messge + 3200, "   APPEND_TO_OUTPUT  = 'YES' or 'NO'", (ftnlen)
		80, (ftnlen)36);
	s_copy(messge + 3280, begtxt, (ftnlen)80, (ftnlen)11);
	s_copy(messge + 3360, " ", (ftnlen)80, (ftnlen)1);
	s_copy(messge + 3440, "   TLE_INPUT_OBJ_ID  = code of the object to "
		"look for in the input TLE file", (ftnlen)80, (ftnlen)75);
	s_copy(messge + 3520, "   TLE_SPK_OBJ_ID    = NAIF ID to use in the "
		"output TLE-based SPK file", (ftnlen)80, (ftnlen)70);
	s_copy(messge + 3600, "   TLE_START_PAD     = 'duration units'", (
		ftnlen)80, (ftnlen)39);
	s_copy(messge + 3680, "   TLE_STOP_PAD      = 'duration units'", (
		ftnlen)80, (ftnlen)39);

/*        Check whether type of input data and output SPK file were */
/*        specified after ``display-template'' command line option. */

	if (pos_(line, " -t ", &c__1, (ftnlen)512, (ftnlen)4) != 0) {
	    s_copy(cmltmg, "-t", (ftnlen)11, (ftnlen)2);
	} else if (pos_(line, " -template ", &c__1, (ftnlen)512, (ftnlen)11) 
		!= 0) {
	    s_copy(cmltmg, "-template", (ftnlen)11, (ftnlen)9);
	}

/*        First, extract input data type. */

	i__2 = pos_(line, cmltmg, &c__1, (ftnlen)512, rtrim_(cmltmg, (ftnlen)
		11)) + nblen_(cmltmg, (ftnlen)11) - 1;
	nextwd_(line + i__2, inptyp, rest, 512 - i__2, (ftnlen)11, (ftnlen)
		512);
	if (s_cmp(inptyp, " ", (ftnlen)11, (ftnlen)1) == 0) {

/*           No input/output type specified on command line. Full */
/*           template display. */

	    s_copy(messge, "Complete MKSPK Setup File Template:  ", (ftnlen)
		    80, (ftnlen)37);
	    for (i__ = 1; i__ <= 9; ++i__) {
		tostdo_(messge + ((i__2 = i__ - 1) < 50 && 0 <= i__2 ? i__2 : 
			s_rnge("messge", i__2, "cmlarg_", (ftnlen)528)) * 80, 
			(ftnlen)80);
	    }
	    tostdo_(messge + 560, (ftnlen)80);
	    tostdo_(messge + 3440, (ftnlen)80);
	    tostdo_(messge + 3520, (ftnlen)80);
	    for (i__ = 10; i__ <= 28; ++i__) {
		tostdo_(messge + ((i__2 = i__ - 1) < 50 && 0 <= i__2 ? i__2 : 
			s_rnge("messge", i__2, "cmlarg_", (ftnlen)536)) * 80, 
			(ftnlen)80);
	    }
	    tostdo_(messge + 560, (ftnlen)80);
	    tostdo_(messge + 3600, (ftnlen)80);
	    tostdo_(messge + 2240, (ftnlen)80);
	    tostdo_(messge + 560, (ftnlen)80);
	    tostdo_(messge + 3680, (ftnlen)80);
	    for (i__ = 30; i__ <= 43; ++i__) {
		tostdo_(messge + ((i__2 = i__ - 1) < 50 && 0 <= i__2 ? i__2 : 
			s_rnge("messge", i__2, "cmlarg_", (ftnlen)546)) * 80, 
			(ftnlen)80);
	    }
	    s_stop("", (ftnlen)0);
	} else {

/*           Check whether we can recognize input data type. */

	    if (! eqstr_(inptyp, "STATES", (ftnlen)11, (ftnlen)6) && ! eqstr_(
		    inptyp, "ELEMENTS", (ftnlen)11, (ftnlen)8) && ! eqstr_(
		    inptyp, "EQ_ELEMENTS", (ftnlen)11, (ftnlen)11) && ! 
		    eqstr_(inptyp, "TL_ELEMENTS", (ftnlen)11, (ftnlen)11)) {
		setmsg_("Input data type '#' specified on command line after"
			" '#'/'#' option is not recognized. Supported input d"
			"ata types are #, #, # and #.", (ftnlen)131);
		errch_("#", inptyp, (ftnlen)1, (ftnlen)11);
		errch_("#", "-t", (ftnlen)1, (ftnlen)2);
		errch_("#", "-template", (ftnlen)1, (ftnlen)9);
		errch_("#", "STATES", (ftnlen)1, (ftnlen)6);
		errch_("#", "ELEMENTS", (ftnlen)1, (ftnlen)8);
		errch_("#", "EQ_ELEMENTS", (ftnlen)1, (ftnlen)11);
		errch_("#", "TL_ELEMENTS", (ftnlen)1, (ftnlen)11);
		sigerr_("SPICE(UNRECOGNDATATYPE)", (ftnlen)23);
	    }
	}

/*        Now extract output SPK type. */

	i__2 = pos_(line, inptyp, &c__1, (ftnlen)512, rtrim_(inptyp, (ftnlen)
		11)) + nblen_(inptyp, (ftnlen)11) - 1;
	nextwd_(line + i__2, outtyp, rest, 512 - i__2, (ftnlen)11, (ftnlen)
		512);
	if (s_cmp(outtyp, " ", (ftnlen)11, (ftnlen)1) == 0) {
	    setmsg_("Both input/output types must be specified on command li"
		    "ne for '#' option; only input type was provided.", (
		    ftnlen)103);
	    errch_("#", cmltmg, (ftnlen)1, (ftnlen)11);
	    sigerr_("SPICE(MISSINGARGUMENTS)", (ftnlen)23);
	}

/*        Convert it to integer and check whether it's a supported */
/*        type. */

	nparsi_(outtyp, &ioutyp, error, &ptr, (ftnlen)11, (ftnlen)512);
	if (ptr != 0) {
	    setmsg_("Output SPK type '#' provided on the command line is not"
		    " an integer number.", (ftnlen)74);
	    errch_("#", outtyp, (ftnlen)1, (ftnlen)11);
	    sigerr_("SPICE(BADOUTPUTSPKTYPE)", (ftnlen)23);
	}
	if (ioutyp != 5 && ioutyp != 8 && ioutyp != 9 && ioutyp != 10 && 
		ioutyp != 12 && ioutyp != 13 && ioutyp != 15 && ioutyp != 17) 
		{
	    setmsg_("Output SPK type '#' provided on the command line is not"
		    " supported. Supported SPK types are: 5, 8, 9, 10, 12, 13"
		    ", 15 and 17. ", (ftnlen)124);
	    errch_("#", outtyp, (ftnlen)1, (ftnlen)11);
	    sigerr_("SPICE(BADOUTPUTSPKTYPE)", (ftnlen)23);
	}

/*        Input/output type were specified on the command line. Display */
/*        template depending of input/output type. */

	ucase_(inptyp, inptyp, (ftnlen)11, (ftnlen)11);
/* Writing concatenation */
	i__1[0] = 46, a__1[0] = "MKSPK Setup File Template for input data ty"
		"pe ";
	i__1[1] = rtrim_(inptyp, (ftnlen)11), a__1[1] = inptyp;
	i__1[2] = 12, a__1[2] = " and output ";
	s_cat(messge, a__1, i__1, &c__3, (ftnlen)80);
/* Writing concatenation */
	i__3[0] = 9, a__2[0] = "SPK type ";
	i__3[1] = 11, a__2[1] = outtyp;
	s_cat(messge + 80, a__2, i__3, &c__2, (ftnlen)80);
/* Writing concatenation */
	i__1[0] = 24, a__1[0] = "   INPUT_DATA_TYPE   = '";
	i__1[1] = rtrim_(inptyp, (ftnlen)11), a__1[1] = inptyp;
	i__1[2] = 1, a__1[2] = "'";
	s_cat(messge + 320, a__1, i__1, &c__3, (ftnlen)80);
/* Writing concatenation */
	i__3[0] = 23, a__2[0] = "   OUTPUT_SPK_TYPE   = ";
	i__3[1] = 11, a__2[1] = outtyp;
	s_cat(messge + 400, a__2, i__3, &c__2, (ftnlen)80);
	s_copy(messge + 480, "   OBJECT_ID         = NAIF numeric code of th"
		"e object", (ftnlen)80, (ftnlen)54);
	if (eqstr_(inptyp, "STATES", (ftnlen)11, (ftnlen)6) && ioutyp == 5) {

/*           Input type is STATES, output type is 5. */

	    for (i__ = 1; i__ <= 27; ++i__) {
		tostdo_(messge + ((i__2 = i__ - 1) < 50 && 0 <= i__2 ? i__2 : 
			s_rnge("messge", i__2, "cmlarg_", (ftnlen)653)) * 80, 
			(ftnlen)80);
	    }
	    tostdo_(messge + 2720, (ftnlen)80);
	} else if (eqstr_(inptyp, "STATES", (ftnlen)11, (ftnlen)6) && ioutyp 
		== 8) {

/*           Input type is STATES, output type is 8. */

	    s_copy(messge + 2640, "   POLYNOM_DEGREE    = polynomial degree "
		    "of Lagrange interpolation", (ftnlen)80, (ftnlen)66);
	    for (i__ = 1; i__ <= 27; ++i__) {
		tostdo_(messge + ((i__2 = i__ - 1) < 50 && 0 <= i__2 ? i__2 : 
			s_rnge("messge", i__2, "cmlarg_", (ftnlen)666)) * 80, 
			(ftnlen)80);
	    }
	    tostdo_(messge + 2640, (ftnlen)80);
	} else if (eqstr_(inptyp, "STATES", (ftnlen)11, (ftnlen)6) && ioutyp 
		== 9) {

/*           Input type is STATES, output type is 9. */

	    s_copy(messge + 2640, "   POLYNOM_DEGREE    = polynomial degree "
		    "of Lagrange interpolation", (ftnlen)80, (ftnlen)66);
	    for (i__ = 1; i__ <= 27; ++i__) {
		tostdo_(messge + ((i__2 = i__ - 1) < 50 && 0 <= i__2 ? i__2 : 
			s_rnge("messge", i__2, "cmlarg_", (ftnlen)679)) * 80, 
			(ftnlen)80);
	    }
	    tostdo_(messge + 2640, (ftnlen)80);
	} else if (eqstr_(inptyp, "STATES", (ftnlen)11, (ftnlen)6) && ioutyp 
		== 12) {

/*           Input type is STATES, output type is 12. */

	    s_copy(messge + 2640, "   POLYNOM_DEGREE    = polynomial degree "
		    "of Hermite interpolation ", (ftnlen)80, (ftnlen)66);
	    for (i__ = 1; i__ <= 27; ++i__) {
		tostdo_(messge + ((i__2 = i__ - 1) < 50 && 0 <= i__2 ? i__2 : 
			s_rnge("messge", i__2, "cmlarg_", (ftnlen)692)) * 80, 
			(ftnlen)80);
	    }
	    tostdo_(messge + 2640, (ftnlen)80);
	} else if (eqstr_(inptyp, "STATES", (ftnlen)11, (ftnlen)6) && ioutyp 
		== 13) {

/*           Input type is STATES, output type is 13. */

	    s_copy(messge + 2640, "   POLYNOM_DEGREE    = polynomial degree "
		    "of Hermite interpolation ", (ftnlen)80, (ftnlen)66);
	    for (i__ = 1; i__ <= 27; ++i__) {
		tostdo_(messge + ((i__2 = i__ - 1) < 50 && 0 <= i__2 ? i__2 : 
			s_rnge("messge", i__2, "cmlarg_", (ftnlen)705)) * 80, 
			(ftnlen)80);
	    }
	    tostdo_(messge + 2640, (ftnlen)80);
	} else if (eqstr_(inptyp, "STATES", (ftnlen)11, (ftnlen)6) && ioutyp 
		== 15) {

/*           Input type is STATES, output type is 15. */

	    for (i__ = 1; i__ <= 33; ++i__) {
		tostdo_(messge + ((i__2 = i__ - 1) < 50 && 0 <= i__2 ? i__2 : 
			s_rnge("messge", i__2, "cmlarg_", (ftnlen)716)) * 80, 
			(ftnlen)80);
	    }
	    for (i__ = 35; i__ <= 39; ++i__) {
		tostdo_(messge + ((i__2 = i__ - 1) < 50 && 0 <= i__2 ? i__2 : 
			s_rnge("messge", i__2, "cmlarg_", (ftnlen)720)) * 80, 
			(ftnlen)80);
	    }
	} else if (eqstr_(inptyp, "STATES", (ftnlen)11, (ftnlen)6) && ioutyp 
		== 17) {

/*           Input type is STATES, output type is 17. */

	    for (i__ = 1; i__ <= 29; ++i__) {
		tostdo_(messge + ((i__2 = i__ - 1) < 50 && 0 <= i__2 ? i__2 : 
			s_rnge("messge", i__2, "cmlarg_", (ftnlen)729)) * 80, 
			(ftnlen)80);
	    }
	    for (i__ = 35; i__ <= 37; ++i__) {
		tostdo_(messge + ((i__2 = i__ - 1) < 50 && 0 <= i__2 ? i__2 : 
			s_rnge("messge", i__2, "cmlarg_", (ftnlen)733)) * 80, 
			(ftnlen)80);
	    }
	} else if (eqstr_(inptyp, "ELEMENTS", (ftnlen)11, (ftnlen)8) && 
		ioutyp == 5) {

/*           Input type is ELEMENTS, output type is 5. */

	    for (i__ = 1; i__ <= 27; ++i__) {
		tostdo_(messge + ((i__2 = i__ - 1) < 50 && 0 <= i__2 ? i__2 : 
			s_rnge("messge", i__2, "cmlarg_", (ftnlen)742)) * 80, 
			(ftnlen)80);
	    }
	    tostdo_(messge + 2720, (ftnlen)80);
	} else if (eqstr_(inptyp, "ELEMENTS", (ftnlen)11, (ftnlen)8) && 
		ioutyp == 8) {

/*           Input type is ELEMENTS, output type is 8. */

	    s_copy(messge + 2640, "   POLYNOM_DEGREE    = polynomial degree "
		    "of Lagrange interpolation", (ftnlen)80, (ftnlen)66);
	    for (i__ = 1; i__ <= 27; ++i__) {
		tostdo_(messge + ((i__2 = i__ - 1) < 50 && 0 <= i__2 ? i__2 : 
			s_rnge("messge", i__2, "cmlarg_", (ftnlen)755)) * 80, 
			(ftnlen)80);
	    }
	    tostdo_(messge + 2640, (ftnlen)80);
	    tostdo_(messge + 2720, (ftnlen)80);
	} else if (eqstr_(inptyp, "ELEMENTS", (ftnlen)11, (ftnlen)8) && 
		ioutyp == 9) {

/*           Input type is ELEMENTS, output type is 9. */

	    s_copy(messge + 2640, "   POLYNOM_DEGREE    = polynomial degree "
		    "of Lagrange interpolation", (ftnlen)80, (ftnlen)66);
	    for (i__ = 1; i__ <= 27; ++i__) {
		tostdo_(messge + ((i__2 = i__ - 1) < 50 && 0 <= i__2 ? i__2 : 
			s_rnge("messge", i__2, "cmlarg_", (ftnlen)769)) * 80, 
			(ftnlen)80);
	    }
	    tostdo_(messge + 2640, (ftnlen)80);
	    tostdo_(messge + 2720, (ftnlen)80);
	} else if (eqstr_(inptyp, "ELEMENTS", (ftnlen)11, (ftnlen)8) && 
		ioutyp == 12) {

/*           Input type is ELEMENTS, output type is 12. */

	    s_copy(messge + 2640, "   POLYNOM_DEGREE    = polynomial degree "
		    "of Hermite interpolation", (ftnlen)80, (ftnlen)65);
	    for (i__ = 1; i__ <= 27; ++i__) {
		tostdo_(messge + ((i__2 = i__ - 1) < 50 && 0 <= i__2 ? i__2 : 
			s_rnge("messge", i__2, "cmlarg_", (ftnlen)783)) * 80, 
			(ftnlen)80);
	    }
	    tostdo_(messge + 2640, (ftnlen)80);
	    tostdo_(messge + 2720, (ftnlen)80);
	} else if (eqstr_(inptyp, "ELEMENTS", (ftnlen)11, (ftnlen)8) && 
		ioutyp == 13) {

/*           Input type is ELEMENTS, output type is 13. */

	    s_copy(messge + 2640, "   POLYNOM_DEGREE    = polynomial degree "
		    "of Hermite interpolation", (ftnlen)80, (ftnlen)65);
	    for (i__ = 1; i__ <= 27; ++i__) {
		tostdo_(messge + ((i__2 = i__ - 1) < 50 && 0 <= i__2 ? i__2 : 
			s_rnge("messge", i__2, "cmlarg_", (ftnlen)797)) * 80, 
			(ftnlen)80);
	    }
	    tostdo_(messge + 2640, (ftnlen)80);
	    tostdo_(messge + 2720, (ftnlen)80);
	} else if (eqstr_(inptyp, "ELEMENTS", (ftnlen)11, (ftnlen)8) && 
		ioutyp == 15) {

/*           Input type is ELEMENTS, output type is 15. */

	    for (i__ = 1; i__ <= 33; ++i__) {
		tostdo_(messge + ((i__2 = i__ - 1) < 50 && 0 <= i__2 ? i__2 : 
			s_rnge("messge", i__2, "cmlarg_", (ftnlen)809)) * 80, 
			(ftnlen)80);
	    }
	    for (i__ = 35; i__ <= 39; ++i__) {
		tostdo_(messge + ((i__2 = i__ - 1) < 50 && 0 <= i__2 ? i__2 : 
			s_rnge("messge", i__2, "cmlarg_", (ftnlen)813)) * 80, 
			(ftnlen)80);
	    }
	} else if (eqstr_(inptyp, "ELEMENTS", (ftnlen)11, (ftnlen)8) && 
		ioutyp == 17) {

/*           Input type is ELEMENTS, output type is 17. */

	    for (i__ = 1; i__ <= 29; ++i__) {
		tostdo_(messge + ((i__2 = i__ - 1) < 50 && 0 <= i__2 ? i__2 : 
			s_rnge("messge", i__2, "cmlarg_", (ftnlen)822)) * 80, 
			(ftnlen)80);
	    }
	    for (i__ = 35; i__ <= 37; ++i__) {
		tostdo_(messge + ((i__2 = i__ - 1) < 50 && 0 <= i__2 ? i__2 : 
			s_rnge("messge", i__2, "cmlarg_", (ftnlen)826)) * 80, 
			(ftnlen)80);
	    }
	} else if (eqstr_(inptyp, "EQ_ELEMENTS", (ftnlen)11, (ftnlen)11) && 
		ioutyp == 17) {

/*           Input type is EQ_ELEMENTS, output type is 17. */

	    for (i__ = 1; i__ <= 29; ++i__) {
		tostdo_(messge + ((i__2 = i__ - 1) < 50 && 0 <= i__2 ? i__2 : 
			s_rnge("messge", i__2, "cmlarg_", (ftnlen)835)) * 80, 
			(ftnlen)80);
	    }
	    for (i__ = 36; i__ <= 37; ++i__) {
		tostdo_(messge + ((i__2 = i__ - 1) < 50 && 0 <= i__2 ? i__2 : 
			s_rnge("messge", i__2, "cmlarg_", (ftnlen)839)) * 80, 
			(ftnlen)80);
	    }
	} else if (eqstr_(inptyp, "TL_ELEMENTS", (ftnlen)11, (ftnlen)11) && 
		ioutyp == 10) {

/*           Input type is TL_ELEMENTS, output type is 10. */

	    s_copy(messge + 480, "   OBJECT_ID         = TLE satellite code ",
		     (ftnlen)80, (ftnlen)42);
/* Writing concatenation */
	    i__3[0] = rtrim_(messge + 720, (ftnlen)80), a__2[0] = messge + 
		    720;
	    i__3[1] = 6, a__2[1] = " (399)";
	    s_cat(messge + 720, a__2, i__3, &c__2, (ftnlen)80);
/* Writing concatenation */
	    i__3[0] = rtrim_(messge + 880, (ftnlen)80), a__2[0] = messge + 
		    880;
	    i__3[1] = 10, a__2[1] = " ('EARTH')";
	    s_cat(messge + 880, a__2, i__3, &c__2, (ftnlen)80);
/* Writing concatenation */
	    i__3[0] = rtrim_(messge + 960, (ftnlen)80), a__2[0] = messge + 
		    960;
	    i__3[1] = 10, a__2[1] = " ('J2000')";
	    s_cat(messge + 960, a__2, i__3, &c__2, (ftnlen)80);
	    s_copy(messge + 1520, "   PCK_FILE          = 'Geophysical const"
		    "ants file name'", (ftnlen)80, (ftnlen)56);
	    for (i__ = 1; i__ <= 9; ++i__) {
		tostdo_(messge + ((i__2 = i__ - 1) < 50 && 0 <= i__2 ? i__2 : 
			s_rnge("messge", i__2, "cmlarg_", (ftnlen)860)) * 80, 
			(ftnlen)80);
	    }
	    tostdo_(messge + 560, (ftnlen)80);
	    tostdo_(messge + 3440, (ftnlen)80);
	    tostdo_(messge + 3520, (ftnlen)80);
	    for (i__ = 10; i__ <= 14; ++i__) {
		tostdo_(messge + ((i__2 = i__ - 1) < 50 && 0 <= i__2 ? i__2 : 
			s_rnge("messge", i__2, "cmlarg_", (ftnlen)868)) * 80, 
			(ftnlen)80);
	    }
	    for (i__ = 17; i__ <= 20; ++i__) {
		tostdo_(messge + ((i__2 = i__ - 1) < 50 && 0 <= i__2 ? i__2 : 
			s_rnge("messge", i__2, "cmlarg_", (ftnlen)872)) * 80, 
			(ftnlen)80);
	    }
	    tostdo_(messge + 1680, (ftnlen)80);
	    tostdo_(messge + 2160, (ftnlen)80);
	    tostdo_(messge + 560, (ftnlen)80);
	    tostdo_(messge + 3600, (ftnlen)80);
	    tostdo_(messge + 2240, (ftnlen)80);
	    tostdo_(messge + 560, (ftnlen)80);
	    tostdo_(messge + 3680, (ftnlen)80);
	} else {

/*           Well, input/output types mismatch - notify the user. */

	    setmsg_("You cannot generate type # SPK from '#' input. See MKSP"
		    "K User's Guide for valid input/output type combinations.",
		     (ftnlen)111);
	    errint_("#", &ioutyp, (ftnlen)1);
	    errch_("#", inptyp, (ftnlen)1, (ftnlen)11);
	    sigerr_("SPICE(TYPESMISMATCH)", (ftnlen)20);
	}

/*        Whatever template we just displayed, we need to display last */
/*        three lines of it and stop the program. */

	for (i__ = 40; i__ <= 43; ++i__) {
	    tostdo_(messge + ((i__2 = i__ - 1) < 50 && 0 <= i__2 ? i__2 : 
		    s_rnge("messge", i__2, "cmlarg_", (ftnlen)902)) * 80, (
		    ftnlen)80);
	}
	s_stop("", (ftnlen)0);
    }

/*     Get setup name from command line if it is present. Flag */
/*     CMLSET is declared in the include file. */

    if (pos_(line, " -setup ", &c__1, (ftnlen)512, (ftnlen)8) == 0) {
	tostdo_(" ", (ftnlen)1);
	prompt_("SETUP FILE NAME> ", cmdfil, (ftnlen)17, cmdfil_len);
    } else {
	i__2 = pos_(line, "-setup", &c__1, (ftnlen)512, (ftnlen)6) + nblen_(
		"-setup", (ftnlen)6) - 1;
	nextwd_(line + i__2, cmdfil, rest, 512 - i__2, cmdfil_len, (ftnlen)
		512);
    }

/*     Get input file name from command line if it is present. */
/*     Flag CMLINP is declared in the include file. */

    if (pos_(line, " -input ", &c__1, (ftnlen)512, (ftnlen)8) == 0) {
	s_copy(inpfn, " ", inpfn_len, (ftnlen)1);
    } else {
	i__2 = pos_(line, "-input", &c__1, (ftnlen)512, (ftnlen)6) + nblen_(
		"-input", (ftnlen)6) - 1;
	nextwd_(line + i__2, inpfn, rest, 512 - i__2, inpfn_len, (ftnlen)512);
    }

/*     Get output file name from command line if it is present. Flag */
/*     CMLOUT declared in the include file. */

    if (pos_(line, " -output ", &c__1, (ftnlen)512, (ftnlen)9) == 0) {
	s_copy(outfn, " ", outfn_len, (ftnlen)1);
    } else {
	i__2 = pos_(line, "-output", &c__1, (ftnlen)512, (ftnlen)7) + nblen_(
		"-output", (ftnlen)7) - 1;
	nextwd_(line + i__2, outfn, rest, 512 - i__2, outfn_len, (ftnlen)512);
    }

/*     Check for presence of the flag indicating that appending to */
/*     an existing output file was requested. CMLAPP is declared in */
/*     the include file. */

    if (pos_(line, " -append ", &c__1, (ftnlen)512, (ftnlen)9) != 0) {
	*appflg = TRUE_;
    } else {
	*appflg = FALSE_;
    }
    chkout_("CMLARG", (ftnlen)6);
    return 0;
} /* cmlarg_ */

