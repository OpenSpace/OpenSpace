/* parsdo.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      PARSDO ( Parsing of DATA_ORDER string ) */
/* Subroutine */ int parsdo_(char *line, char *doval, integer *nval, integer *
	param, integer *nparam, ftnlen line_len, ftnlen doval_len)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i__, l;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    char value[12];
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen), 
	    lastnb_(char *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int nextwd_(char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen), setmsg_(char *, ftnlen), sigerr_(char *, ftnlen),
	     chkout_(char *, ftnlen);

/* $ Abstract */

/*     This routine is a module of the MKSPK program. It parses the */
/*     DATA_ORDER value provided in a setup file and forms an array */
/*     of indexes of recognizable input parameters contaned in it. */

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

/*     PARSING */

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
/*     --------  ---  ---------------------------------------------- */
/*     LINE       I   DATA_ORDER string */
/*     DOVAL      I   Array of recognizable input parameter names */
/*     NVAL       I   Number of recognizable input parameters */
/*     PARAM      O   Array of parameter IDs present in DATA_ORDER */
/*     NPARAM     O   Number of elements in PARAM */

/* $ Detailed_Input */

/*     LINE        is the DATA_ORDER value that will be parsed. */

/*     DOVAL       is an array containing complete set recognizable */
/*                 input parameters (see main module for the current */
/*                 list). */

/*     NVAL        is the total number of recognizable input parameters */
/*                 (number of elements in DOVAL). */

/* $ Detailed_Output */

/*     PARAM       is an integer array containing indexes of the */
/*                 recognizable input parameters present in the input */
/*                 DATA_ORDER value in the order in which they are */
/*                 provided in that value. */

/*     NPARAM      is the number of elements in PARAM. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If token in the data order is not recognized, then the */
/*        error 'SPICE(BADDATAORDERTOKEN)' will be signalled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This subroutine parses DATA_ORDER string containing names of */
/*     input data record parameters in the order in which they appear */
/*     in the input records and returns an integer array of the indexes */
/*     of the parameters that were found in the string. */

/* $ Examples */

/*     Let DATA_ORDER has following value: */

/*        LINE      = 'EPOCH X Y Z SKIP VX VY VZ' */

/*     and DOVAL array contains the following values: */

/*        DOVAL(1)  =  'EPOCH' */
/*        DOVAL(2)  =  'X' */
/*        DOVAL(3)  =  'Y' */
/*        DOVAL(4)  =  'Z' */
/*        DOVAL(5)  =  'VX' */
/*        DOVAL(6)  =  'VY' */
/*        DOVAL(7)  =  'VZ' */
/*        ... */
/*        DOVAL(30) =  'SKIP' */

/*     Then after parsing we will have on the output: */

/*        NPARAM    = 8 */

/*        PARAM     = 1, 2, 3, 4, 30, 5, 6, 7 */

/* $ Restrictions */

/*     Because search for a parameter in the DATA_ORDER value is case */
/*     sensitive, the DATA_ORDER value and parameter names must be */
/*     in the same case (nominally uppercase). */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.G. Khavenson (IKI RAS, Russia) */
/*     B.V. Semenov   (NAIF, JPL) */

/* $ Version */

/* -    Version 1.0.3, 29-MAR-1999 (NGK). */

/*        Corrected examples section. */

/* -    Version 1.0.2, 18-MAR-1999 (BVS). */

/*        Corrected comments. */

/* -    Version 1.0.1, 13-JAN-1999 (BVS). */

/*        Modified error messages. */

/* -    Version 1.0.0, 08-SEP-1998 (NGK). */

/* -& */
/* $ Index_Entries */

/*     Parse MKSPK setup DATA_ORDER string. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Size VALUEL declared in the include file. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("PARSDO", (ftnlen)6);
    }

/*     Assign zero to PARAM array. */

    i__1 = *nval;
    for (l = 1; l <= i__1; ++l) {
	param[l - 1] = 0;
    }

/*     Reset counter of words on line. */

    *nparam = 0;
    while(lastnb_(line, line_len) != 0) {

/*        Get next word from the line. Value is already uppercase. */

	nextwd_(line, value, line, line_len, (ftnlen)12, line_len);
	i__ = isrchc_(value, nval, doval, (ftnlen)12, doval_len);

/*        Look whether this value is one of the reserved values. */

	if (i__ != 0) {

/*           This value is OK. Memorize it. */

	    ++(*nparam);
	    param[*nparam - 1] = i__;
	} else {

/*           We can not recognize this value. */

	    setmsg_("Can not recognize token '#' in the value of the setup f"
		    "ile keyword '#'. Refer to the User's Guide for the progr"
		    "am for complete list of allowed tokens.", (ftnlen)150);
	    errch_("#", value, (ftnlen)1, (ftnlen)12);
	    errch_("#", "DATA_ORDER", (ftnlen)1, (ftnlen)10);
	    sigerr_("SPICE(BADDATAORDERTOKEN)", (ftnlen)24);
	}
    }
    chkout_("PARSDO", (ftnlen)6);
    return 0;
} /* parsdo_ */

