/* chckdo.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__9 = 9;

/* $Procedure      CHCKDO ( Check presence of required input parameters ) */
/* Subroutine */ int chckdo_(char *indtvl, integer *outtvl, integer *param, 
	integer *nparam, char *doval, ftnlen indtvl_len, ftnlen doval_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer l;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen), repmc_(char *, char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen, ftnlen);
    logical found;
    extern integer rtrim_(char *, ftnlen), isrchi_(integer *, integer *, 
	    integer *);
    extern logical return_(void);
    char errlin[512];
    extern /* Subroutine */ int setmsg_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), inssub_(char *, char *, integer *, char *, ftnlen, 
	    ftnlen, ftnlen), chkout_(char *, ftnlen);

/* $ Abstract */

/*     This routine is a module of the MKSPK program. It checks whether */
/*     set of input parameters specified in the DATA_ORDER value */
/*     contains all parameters required for a given input data type and */
/*     output SPK type. */

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

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     INDTVL     I   Input data type. */
/*     OUTTVL     I   Output spk type. */
/*     PARAM      I   Array of DATA_ORDER parameter IDs */
/*     NPARAM     I   Number of not zero parameter IDs in PARAM */
/*     DOVAL      I   Array of parameter values acceptable in DATA_ORDER */

/* $ Detailed_Input */

/*     INDTVL      is the input data type. See MKSPK.INC for the */
/*                 current list of supported input data types. */

/*     OUTTVL      is the output SPK type. Currently supported output */
/*                 SPK types are 5, 8, 9, 12, 13, 15 and 17. */

/*     PARAM       is an integer array containing indexes of the */
/*                 recognizable input parameters present in the */
/*                 DATA_ORDER keyword value in the order in which they */
/*                 were provided in that value. */

/*     NPARAM      is the number of elements in PARAM. */

/*     DOVAL       is an array containing complete set recognizable */
/*                 input parameters. (see main module for the current */
/*                 list) */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     If the set of input parameters does not contain some of the */
/*     required tokens, then the error 'SPICE(MISSINGDATAORDERTK)' */
/*     will be signalled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

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

/* -    Version 1.0.3, 29-MAR-1999 (NGK). */

/*        Added comments. */

/* -    Version 1.0.2, 18-MAR-1999 (BVS). */

/*        Corrected comments. */

/* -    Version 1.0.1, 13-JAN-1999 (BVS). */

/*        Modified error messages. */

/* -    Version 1.0.0, 08-SEP-1998 (NGK). */

/* -& */
/* $ Index_Entries */

/*     Check adequacy of the DATA_ORDER defined in MKSPK setup */

/* -& */

/*     SPICELIB functions */


/*     Parameters INELTP, INSTTP, INEQTP containing supported */
/*     input data type names and keyword parameter KDATOR are declared */
/*     in the include file. */


/*     Local variables */


/*     Error line variable. Size LINLEN declared in the include file. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("CHCKDO", (ftnlen)6);
    }

/*     Check if EPOCH is present among specified input parameters. */

    if (isrchi_(&c__1, nparam, param) == 0) {
	setmsg_("Set of input data parameters specified in the setup file ke"
		"yword '#' must contain token '#' designating epoch position "
		"in the input data records.", (ftnlen)145);
	errch_("#", "DATA_ORDER", (ftnlen)1, (ftnlen)10);
	errch_("#", doval, (ftnlen)1, doval_len);
	sigerr_("SPICE(MISSINGEPOCHTOKEN)", (ftnlen)24);
    }

/*     Check whether all necessary input parameters are present */
/*     according to the input data type. */

    found = TRUE_;
    s_copy(errlin, "The following token(s) designating input parameter(s) re"
	    "quired when input data type is '#' is(are) missing in the value "
	    "of the setup file keyword '#':", (ftnlen)512, (ftnlen)150);
    if (s_cmp(indtvl, "ELEMENTS", rtrim_(indtvl, indtvl_len), (ftnlen)8) == 0)
	     {

/*        Input type is ELEMENTS. Check whether eccentricity, */
/*        inclination, argument of periapsis and longitude of ascending */
/*        node are present in the input data. */

	repmc_(errlin, "#", "ELEMENTS", errlin, (ftnlen)512, (ftnlen)1, (
		ftnlen)8, (ftnlen)512);
	repmc_(errlin, "#", "DATA_ORDER", errlin, (ftnlen)512, (ftnlen)1, (
		ftnlen)10, (ftnlen)512);
	if (isrchi_(&c__9, nparam, param) == 0) {
	    i__1 = rtrim_(errlin, (ftnlen)512) + 1;
	    inssub_(errlin, " '#',", &i__1, errlin, (ftnlen)512, (ftnlen)5, (
		    ftnlen)512);
	    repmc_(errlin, "#", doval + (doval_len << 3), errlin, (ftnlen)512,
		     (ftnlen)1, doval_len, (ftnlen)512);
	    found = FALSE_;
	}
	for (l = 13; l <= 15; ++l) {
	    if (isrchi_(&l, nparam, param) == 0) {
		i__1 = rtrim_(errlin, (ftnlen)512) + 1;
		inssub_(errlin, " '#',", &i__1, errlin, (ftnlen)512, (ftnlen)
			5, (ftnlen)512);
		repmc_(errlin, "#", doval + (l - 1) * doval_len, errlin, (
			ftnlen)512, (ftnlen)1, doval_len, (ftnlen)512);
		found = FALSE_;
	    }
	}
    } else if (s_cmp(indtvl, "STATES", rtrim_(indtvl, indtvl_len), (ftnlen)6) 
	    == 0) {

/*        Input type is STATES. Check whether all state vector */
/*        components are present in the input data. */

	repmc_(errlin, "#", "STATES", errlin, (ftnlen)512, (ftnlen)1, (ftnlen)
		6, (ftnlen)512);
	repmc_(errlin, "#", "DATA_ORDER", errlin, (ftnlen)512, (ftnlen)1, (
		ftnlen)10, (ftnlen)512);
	for (l = 2; l <= 7; ++l) {
	    if (isrchi_(&l, nparam, param) == 0) {
		i__1 = rtrim_(errlin, (ftnlen)512) + 1;
		inssub_(errlin, " '#',", &i__1, errlin, (ftnlen)512, (ftnlen)
			5, (ftnlen)512);
		repmc_(errlin, "#", doval + (l - 1) * doval_len, errlin, (
			ftnlen)512, (ftnlen)1, doval_len, (ftnlen)512);
		found = FALSE_;
	    }
	}
    } else if (s_cmp(indtvl, "EQ_ELEMENTS", rtrim_(indtvl, indtvl_len), (
	    ftnlen)11) == 0) {

/*        Input type is EQ_ELEMENTS. Check whether all equinoctial */
/*        elements are present in the input data. */

	repmc_(errlin, "#", "EQ_ELEMENTS", errlin, (ftnlen)512, (ftnlen)1, (
		ftnlen)11, (ftnlen)512);
	repmc_(errlin, "#", "DATA_ORDER", errlin, (ftnlen)512, (ftnlen)1, (
		ftnlen)10, (ftnlen)512);
	for (l = 21; l <= 29; ++l) {
	    if (isrchi_(&l, nparam, param) == 0) {
		i__1 = rtrim_(errlin, (ftnlen)512) + 1;
		inssub_(errlin, " '#',", &i__1, errlin, (ftnlen)512, (ftnlen)
			5, (ftnlen)512);
		repmc_(errlin, "#", doval + (l - 1) * doval_len, errlin, (
			ftnlen)512, (ftnlen)1, doval_len, (ftnlen)512);
		found = FALSE_;
	    }
	}
    }

/*     Signal the error if any of the required parameters wasn't found. */

    if (! found) {
	i__1 = rtrim_(errlin, (ftnlen)512) - 1;
	s_copy(errlin + i__1, ".", rtrim_(errlin, (ftnlen)512) - i__1, (
		ftnlen)1);
	setmsg_(errlin, (ftnlen)512);
	sigerr_("SPICE(MISSINGDATAORDERTK)", (ftnlen)25);
    }

/*     Check whether all necessary input parameters are present */
/*     according to the output SPK type. */

    found = TRUE_;
    if (*outtvl == 17) {

/*        Output type is 17. Verify if dM/dt, dNOD/dt, dPER/dt */
/*        exist in input data. */

	s_copy(errlin, "The following token(s) designating input parameter(s"
		") required when output SPK type is 17 is(are) missing in the"
		" value of the setup file keyword '#':", (ftnlen)512, (ftnlen)
		149);
	for (l = 27; l <= 29; ++l) {
	    if (isrchi_(&l, nparam, param) == 0) {
		i__1 = rtrim_(errlin, (ftnlen)512) + 1;
		inssub_(errlin, " '#',", &i__1, errlin, (ftnlen)512, (ftnlen)
			5, (ftnlen)512);
		repmc_(errlin, "#", doval + (l - 1) * doval_len, errlin, (
			ftnlen)512, (ftnlen)1, doval_len, (ftnlen)512);
		found = FALSE_;
	    }
	}
    }

/*     Signal the error if any of the required parameters wasn't found. */

    if (! found) {
	i__1 = rtrim_(errlin, (ftnlen)512) - 1;
	s_copy(errlin + i__1, ".", rtrim_(errlin, (ftnlen)512) - i__1, (
		ftnlen)1);
	setmsg_(errlin, (ftnlen)512);
	sigerr_("SPICE(MISSINGDATAORDERTK)", (ftnlen)25);
    }
    chkout_("CHCKDO", (ftnlen)6);
    return 0;
} /* chckdo_ */

