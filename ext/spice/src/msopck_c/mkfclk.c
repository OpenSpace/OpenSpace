/* mkfclk.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__33 = 33;

/* $Procedure      MKFCLK ( MaKe a Fake sCLK kernel ) */
/* Subroutine */ int mkfclk_(char *sclk, integer *scid, doublereal *refet, 
	char *madeby, ftnlen sclk_len, ftnlen madeby_len)
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    cllist cl__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer), f_clos(cllist *);

    /* Local variables */
    static doublereal tvec[6];
    static integer unit;
    static char text[80*33];
    static integer i__;
    extern /* Subroutine */ int etcal_(doublereal *, char *, ftnlen), chkin_(
	    char *, ftnlen), repmc_(char *, char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen, ftnlen), dpfmt_(doublereal *, char *, char *, 
	    ftnlen, ftnlen), repmi_(char *, char *, integer *, char *, ftnlen,
	     ftnlen, ftnlen);
    extern integer rtrim_(char *, ftnlen);
    static char start[80];
    extern /* Subroutine */ int replch_(char *, char *, char *, char *, 
	    ftnlen, ftnlen, ftnlen, ftnlen), chkout_(char *, ftnlen), cputim_(
	    doublereal *), writla_(integer *, char *, integer *, ftnlen);
    static char curtim[80];
    extern /* Subroutine */ int cmprss_(char *, integer *, char *, char *, 
	    ftnlen, ftnlen, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int txtopn_(char *, integer *, ftnlen);

/* $ Abstract */

/*     This routine creates a simple SCLK kernel for those times when a */
/*     real SCLK is not available but is needed for producing CKs. */

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

/*      None. */

/* $ Keywords */

/*      UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     SCLK       I   Name of the SCLK file to make. */
/*     SCID       I   Spacecraft ID associated with the clock. */
/*     REFET      I   Reference ET (seconds past J2000). */
/*     MADEBY     I   String identifying the file producer. */

/* $ Detailed_Input */

/*     SCLK     Name of the SCLK file to make. */

/*     SCID     Spacecraft ID associated with the clock. This is the ID */
/*              that will be used as the spacecraft ID argument in the */
/*              calls to SCLK routines to provide conversion for this */
/*              clock. */

/*     REFET    Reference ET (seconds past J2000). The time at which the */
/*              clock will start. */

/*     MADEBY   String identifying the file producer. This string will */
/*              be included 'as is' in the sentence "This file was */
/*              created by [MADEBY] on [DATE]." in the file comments. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     This routine creates an SCLK kernel with the specified name for */
/*     the specified spacecraft ID that simply models TDB seconds past */
/*     the input epoch REFET.  The granularity of this clock is 1 */
/*     millisecond. */

/* $ Exceptions */

/*     1) If an error occurs while attempting to write to the SCLK */
/*        file, a routine called by this routine will detect and signal */
/*        the error. */

/* $ Particulars */

/*     This routine allows anyone creating C-kernels to have a readily */
/*     available SCLK kernel that simply follows ET. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */
/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 24-AUG-2006 (BVS) */

/*        Initial version heavily based on Bill's mkclck from prediCkt. */

/* -& */
/* $ Index_Entries */

/*     Create an SCLK kernel that follows ET */

/* -& */

/*     SPICELIB Functions */


/*     Local parameters. */


/*     Local variables. */


/*     Save everything to prevent potential memory problems in f2c'ed */
/*     version. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("MKFCLK", (ftnlen)6);
    }

/*     SCLK template. */

    s_copy(text, "KPL/SCLK", (ftnlen)80, (ftnlen)8);
    s_copy(text + 80, " ", (ftnlen)80, (ftnlen)1);
    s_copy(text + 160, "Fake SCLK Kernel for Spacecraft with ID %", (ftnlen)
	    80, (ftnlen)41);
    s_copy(text + 240, "----------------------------------------------------"
	    "----", (ftnlen)80, (ftnlen)56);
    s_copy(text + 320, " ", (ftnlen)80, (ftnlen)1);
    s_copy(text + 400, "   This SCLK kernel contains the data necessary for "
	    "converting from", (ftnlen)80, (ftnlen)67);
    s_copy(text + 480, "   ephemeris time (ET) to ticks for a fake clock ass"
	    "ociated with the", (ftnlen)80, (ftnlen)68);
    s_copy(text + 560, "   spacecraft with ID code %. This fake clock runs a"
	    "t the same rate", (ftnlen)80, (ftnlen)67);
    s_copy(text + 640, "   as ET, starting at ET ! and going for 100", (
	    ftnlen)80, (ftnlen)44);
    s_copy(text + 720, "   years. It has two fields -- seconds and milliseco"
	    "nds. The clock's ", (ftnlen)80, (ftnlen)69);
    s_copy(text + 800, "   granularity is 1 millisecond.", (ftnlen)80, (
	    ftnlen)32);
    s_copy(text + 880, " ", (ftnlen)80, (ftnlen)1);
    s_copy(text + 960, "   This file was created by ^", (ftnlen)80, (ftnlen)
	    29);
    s_copy(text + 1040, "   on $.", (ftnlen)80, (ftnlen)8);
    s_copy(text + 1120, " ", (ftnlen)80, (ftnlen)1);
    s_copy(text + 1200, "Kernel data", (ftnlen)80, (ftnlen)11);
    s_copy(text + 1280, "---------------------------------------------------"
	    "-----", (ftnlen)80, (ftnlen)56);
    s_copy(text + 1360, " ", (ftnlen)80, (ftnlen)1);
    s_copy(text + 1440, "   \\begindata", (ftnlen)80, (ftnlen)13);
    s_copy(text + 1520, " ", (ftnlen)80, (ftnlen)1);
    s_copy(text + 1600, "      SCLK_KERNEL_ID = ( @$ )", (ftnlen)80, (ftnlen)
	    29);
    s_copy(text + 1680, "      SCLK_DATA_TYPE_#       = ( 1 )", (ftnlen)80, (
	    ftnlen)36);
    s_copy(text + 1760, "      SCLK01_TIME_SYSTEM_#   = ( 1 )", (ftnlen)80, (
	    ftnlen)36);
    s_copy(text + 1840, "      SCLK01_N_FIELDS_#      = ( 2 )", (ftnlen)80, (
	    ftnlen)36);
    s_copy(text + 1920, "      SCLK01_MODULI_#        = ( 3155760000 1000 )", 
	    (ftnlen)80, (ftnlen)50);
    s_copy(text + 2000, "      SCLK01_OFFSETS_#       = ( 0 0 )", (ftnlen)80, 
	    (ftnlen)38);
    s_copy(text + 2080, "      SCLK01_OUTPUT_DELIM_#  = ( 1 )", (ftnlen)80, (
	    ftnlen)36);
    s_copy(text + 2160, "      SCLK_PARTITION_START_# = ( 0.00000E+00 )", (
	    ftnlen)80, (ftnlen)46);
    s_copy(text + 2240, "      SCLK_PARTITION_END_#   = ( 3.15576E+12 )", (
	    ftnlen)80, (ftnlen)46);
    s_copy(text + 2320, "      SCLK01_COEFFICIENTS_#  = ( 0.0 @! 1.0 )", (
	    ftnlen)80, (ftnlen)45);
    s_copy(text + 2400, " ", (ftnlen)80, (ftnlen)1);
    s_copy(text + 2480, "   \\begintext", (ftnlen)80, (ftnlen)13);
    s_copy(text + 2560, " ", (ftnlen)80, (ftnlen)1);

/*     Convert reference ET to calendar format. */

    etcal_(refet, start, (ftnlen)80);
    cmprss_(" ", &c__1, start, start, (ftnlen)1, (ftnlen)80, (ftnlen)80);
    i__ = rtrim_(start, (ftnlen)80);
    replch_(start, " ", "-", start, i__, (ftnlen)1, (ftnlen)1, i__);

/*     Get CPU time and package it into a string. */

    s_copy(curtim, "YYYY-MM-DD/HR:MN:SC", (ftnlen)80, (ftnlen)19);
    cputim_(tvec);
    dpfmt_(tvec, "0YYY", curtim, (ftnlen)4, (ftnlen)4);
    dpfmt_(&tvec[1], "0M", curtim + 5, (ftnlen)2, (ftnlen)2);
    dpfmt_(&tvec[2], "0D", curtim + 8, (ftnlen)2, (ftnlen)2);
    dpfmt_(&tvec[3], "0h", curtim + 11, (ftnlen)2, (ftnlen)2);
    dpfmt_(&tvec[4], "0m", curtim + 14, (ftnlen)2, (ftnlen)2);
    dpfmt_(&tvec[5], "0s", curtim + 17, (ftnlen)2, (ftnlen)2);

/*     Replace markers in template with IDs, times, etc. */

    for (i__ = 1; i__ <= 33; ++i__) {
	i__3 = -(*scid);
	repmi_(text + ((i__1 = i__ - 1) < 33 && 0 <= i__1 ? i__1 : s_rnge(
		"text", i__1, "mkfclk_", (ftnlen)246)) * 80, "#", &i__3, text 
		+ ((i__2 = i__ - 1) < 33 && 0 <= i__2 ? i__2 : s_rnge("text", 
		i__2, "mkfclk_", (ftnlen)246)) * 80, (ftnlen)80, (ftnlen)1, (
		ftnlen)80);
	repmi_(text + ((i__1 = i__ - 1) < 33 && 0 <= i__1 ? i__1 : s_rnge(
		"text", i__1, "mkfclk_", (ftnlen)247)) * 80, "%", scid, text 
		+ ((i__2 = i__ - 1) < 33 && 0 <= i__2 ? i__2 : s_rnge("text", 
		i__2, "mkfclk_", (ftnlen)247)) * 80, (ftnlen)80, (ftnlen)1, (
		ftnlen)80);
	repmc_(text + ((i__1 = i__ - 1) < 33 && 0 <= i__1 ? i__1 : s_rnge(
		"text", i__1, "mkfclk_", (ftnlen)248)) * 80, "!", start, text 
		+ ((i__2 = i__ - 1) < 33 && 0 <= i__2 ? i__2 : s_rnge("text", 
		i__2, "mkfclk_", (ftnlen)248)) * 80, (ftnlen)80, (ftnlen)1, (
		ftnlen)80, (ftnlen)80);
	repmc_(text + ((i__1 = i__ - 1) < 33 && 0 <= i__1 ? i__1 : s_rnge(
		"text", i__1, "mkfclk_", (ftnlen)249)) * 80, "$", curtim, 
		text + ((i__2 = i__ - 1) < 33 && 0 <= i__2 ? i__2 : s_rnge(
		"text", i__2, "mkfclk_", (ftnlen)249)) * 80, (ftnlen)80, (
		ftnlen)1, (ftnlen)80, (ftnlen)80);
	repmc_(text + ((i__1 = i__ - 1) < 33 && 0 <= i__1 ? i__1 : s_rnge(
		"text", i__1, "mkfclk_", (ftnlen)250)) * 80, "^", madeby, 
		text + ((i__2 = i__ - 1) < 33 && 0 <= i__2 ? i__2 : s_rnge(
		"text", i__2, "mkfclk_", (ftnlen)250)) * 80, (ftnlen)80, (
		ftnlen)1, madeby_len, (ftnlen)80);
    }

/*     Open output SCLK file, write SCLK contents and close the file. */

    txtopn_(sclk, &unit, sclk_len);
    writla_(&c__33, text, &unit, (ftnlen)80);
    cl__1.cerr = 0;
    cl__1.cunit = unit;
    cl__1.csta = 0;
    f_clos(&cl__1);

/*     All done. */

    chkout_("MKFCLK", (ftnlen)6);
    return 0;
} /* mkfclk_ */

