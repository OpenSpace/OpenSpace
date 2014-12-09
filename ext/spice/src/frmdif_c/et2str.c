/* et2str.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      ET2STR ( Convert ET to String for FRMDIFF Output ) */
/* Subroutine */ int et2str_(doublereal *et, char *timfmt, integer *sclkid, 
	integer *sigdig, char *string, ftnlen timfmt_len, ftnlen string_len)
{
    extern /* Subroutine */ int sce2c_(integer *, doublereal *, doublereal *),
	     sce2s_(integer *, doublereal *, char *, ftnlen), chkin_(char *, 
	    ftnlen);
    doublereal sclkd, ticks;
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int sc01s2d_(integer *, char *, doublereal *, 
	    ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int dpstrp_(doublereal *, integer *, char *, 
	    ftnlen), dpstre_(doublereal *, integer *, char *, ftnlen), 
	    timout_(doublereal *, char *, char *, ftnlen, ftnlen), chkout_(
	    char *, ftnlen);

/* $ Abstract */

/*     Converts ET to a string for FRMDIFF output. */

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

/*     Include Section:  FRMDIFF Global Parameters */

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

/*     B.V. Semenov   (JPL) */

/* $ Version */

/* -    Version 2.1.0, 25-MAR-2014 (BVS). */

/*        Updated version. */

/* -    Version 2.0.0, 27-FEB-2012 (BVS). */

/*        Updated version. */

/* -    Version 1.0.0, 09-DEC-2008 (BVS). */

/* -& */

/*     Program name and version. */


/*     Command line keys. */


/*     Command line key values. */


/*     Max and min number orientations that the program can handle. */


/*     Default number orientations. */


/*     Maximum number of IDs in a CK or a binary PCK file */


/*     Line size parameters. */


/*     Version, help, usage and header display parameters. */


/*     DAF descriptor size and component counts. */


/*     Cell lower boundary. */


/*     Maximum allowed number of coverage windows. */


/*     Smallest allowed step. */


/*     Fraction of step to be used as pad at the end of intervals. */


/*     Default, minimum, and maximum numbers of significant digits */
/*     allowed for numbers in dump reports. */


/*     End of FRMDIFF parameters. */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     ET         I   ET time */
/*     TIMFMT     I   Output format (per FRMDIFF spec) */
/*     SCLKIF     I   SCLK ID */
/*     SIGDIG     I   Number of significant digits */
/*     STRING     O   Output time string */

/* $ Detailed_Input */

/*     TBD. */

/* $ Detailed_Output */

/*     TBD. */

/* $ Parameters */

/*     None */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     TBD. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov   (JPL) */

/* $ Version */

/* -    Version 2.0.0, 27-FEB-2012 (BVS) */

/*        Changed the calling sequence to include additional input */
/*        SIGDIG. */

/*        Updated to use SIGDIG instead of 14 to specify the number of */
/*        significant digits in numeric times. */

/*        Replaced calls to SPICELIB's DPSTR with calls to SUPPORT's */
/*        DPSTRE or local DPSTRP, both of which do not limit the number */
/*        of significant digits to 14. */

/* -    Version 1.0.0, 30-AUG-2008 (BVS) */

/*        Initial version. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ET2STR", (ftnlen)6);
    }

/*     Convert ET to string depending on specified type. */

    if (eqstr_(timfmt, "et", timfmt_len, (ftnlen)2)) {

/*        Output string should contain ET seconds. Use DPSTRP. */

	dpstrp_(et, sigdig, string, string_len);
    } else if (eqstr_(timfmt, "ticks", timfmt_len, (ftnlen)5)) {

/*        Output string should contain SCLK ticks. Use SCE2C and */
/*        DPSTRE. */

	sce2c_(sclkid, et, &ticks);
	dpstre_(&ticks, sigdig, string, string_len);
    } else if (eqstr_(timfmt, "sclk", timfmt_len, (ftnlen)4)) {

/*        Output string should contain SCLK string. Use SCE2S. */

	sce2s_(sclkid, et, string, string_len);
    } else if (eqstr_(timfmt, "sclkd", timfmt_len, (ftnlen)5)) {

/*        Output string should contain decimal SCLK. Convert ET to SCLK */
/*        string, then string to decimal form, and package decimal form */
/*        back into a string :). */

	sce2s_(sclkid, et, string, string_len);
	sc01s2d_(sclkid, string, &sclkd, string_len);
	dpstre_(&sclkd, sigdig, string, string_len);
    } else {

/*        Output string should be set by TIMOUT using provided TIMFMT. */

	timout_(et, timfmt, string, timfmt_len, string_len);
    }

/*     All done. */

    chkout_("ET2STR", (ftnlen)6);
    return 0;
} /* et2str_ */

