/* ckwss.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__11 = 11;

/* $Procedure      CKWSS ( CK write segment summary ) */
/* Subroutine */ int ckwss_(integer *unit, char *segid, integer *segins, 
	integer *segfrm, integer *segtyp, integer *segrts, doublereal *segbtm,
	 doublereal *segetm, ftnlen segid_len)
{
    /* Initialized data */

    static char cktyp[80*6] = "Discrete Pointing                            "
	    "                                   " "Continuous Pointing: Const"
	    "ant Angular Velocity                                  " "Continu"
	    "ous Pointing: Linear Interpolation                              "
	    "         " "Continuous Pointing: Chebyshev, Variable Interval Le"
	    "ngth                        " "Continuous Pointing: MEX/Rosetta "
	    "Polynomial Interpolation                       " "Continuous Poi"
	    "nting: ESOC/DDID Piecewise Interpolation                        "
	    "  ";
    static char pvstat[40*2] = "Pointing Only                           " 
	    "Pointing and Angular Velocity           ";

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_rnge(char *, integer, 
	    char *, integer);

    /* Local variables */
    static integer sclk;
    extern /* Subroutine */ int sct2e_(integer *, doublereal *, doublereal *);
    static doublereal beget;
    static char frame[32];
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static doublereal endet;
    extern /* Subroutine */ int repmc_(char *, char *, char *, char *, ftnlen,
	     ftnlen, ftnlen, ftnlen);
    static char lines[80*11];
    static logical found;
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen), bodc2n_(integer *, char *, logical *, 
	    ftnlen), et2utc_(doublereal *, char *, integer *, char *, ftnlen, 
	    ftnlen);
    extern logical failed_(void);
    extern /* Subroutine */ int scdecd_(integer *, doublereal *, char *, 
	    ftnlen), ckmeta_(integer *, char *, integer *, ftnlen);
    static char begtim[32], endtim[32], spname[32];
    extern /* Subroutine */ int frmnam_(integer *, char *, ftnlen), chkout_(
	    char *, ftnlen);
    static integer spcrft;
    extern /* Subroutine */ int writla_(integer *, char *, integer *, ftnlen);
    static char typdsc[80];
    extern logical return_(void);

/* $ Abstract */

/*     Write a segment summary for a CK segment to a Fortran logical */
/*     unit. */

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

/*     None. */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*      UNIT      I   The logical unit to use for writing the summary. */
/*      SEGID     I   Segment ID for a segment in a CK file. */
/*      SEGINS    I   ID for the instrument having data in a CK segment. */
/*      SEGFRM    I   Reference frame for a segment in a CK file. */
/*      SEGTYP    I   Data type for a segment in a CK file. */
/*      SEGRTS    I   Flag for velocity info in a CK segment. */
/*      SEGBTM    I   Begin time (SCLK) for a segment in a CK file. */
/*      SEGETM    I   End time (SCLK) for a segment in a CK file. */

/* $ Detailed_Input */

/*      UNIT     The Fortran logical unit on which the segment summary */
/*               is to be written. */

/*      SEGID    Segment ID for the current segment in a CK file. */

/*      SEGINS   NAIF integer ID code for the instrument having data */
/*               in the current segment in a CK file. */

/*      SEGFRM   Inertial reference frame for the current segment in a */
/*               CK file. This is the NAIF integer code for the inertial */
/*               reference frame. */

/*      SEGTYP   Data type for the current segment in a CK file. This */
/*               is an integer code which specifies the type of the data */
/*               in the current segment. */

/*      SEGRTS   Integer flag which indicates whether the segment */
/*               contains angular velocity data in addition to pointing */
/*               data, SEGRTS .EQ. 1, or just pointing data, SEGRTS .EQ. */
/*               0. */

/*      SEGBTM   The beginning encoded SCLK time for the data in the */
/*               current segment in a CK file. */

/*      SEGETM   The ending encoded SCLK time for the data in the */
/*               current segment in a CK file. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If an error occurs while writing to the logical unit, the error */
/*        SPICE(FILEWRITEFAILED) will be signalled. */

/*     2) If an error occurs in a routine called by this routine, this */
/*        routine will check out and return. Presumably an appropriate */
/*        error message will already have been set. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine will format and display a CK segment summary in a */
/*     human compatible fashion. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     1) This routine performs time conversions using SCDECD, and */
/*        therefore requires that a SPICE SCLK kernel file be */
/*        loaded into the SPICELIB kernel pool before it is called. */

/*     2) This routine performs time conversions using ET2UTC, and */
/*        therefore requires that a SPICE leapseconds kernel file be */
/*        loaded into the SPICELIB kernel pool before it is called. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     K.R. Gehringer (JPL) */

/* $ Version */

/* -    SPACIT Version 4.0.0,  08-MAR-2014 (NJB) */

/*        The routine was updated to handle CK type 6. */

/* -    SPACIT Version 3.0.0,  28-AUG-2002 (NJB) */

/*        The routine was updated to handle CK types 4 and 5. */

/* -    Beta Version 2.1.0,  7-FEB-1997 (WLT) */

/*        The routine was modified to use CKMETA to obtain the */
/*        spacecraft and spacecraft clock associated with a */
/*        a segment.  This replaces the old method of just dividing */
/*        by 1000. */

/* -    Beta Version 2.0.0, 24-JAN-1996 (KRG) */

/*        There have been several undocumented revisions of this */
/*        subroutine to improve its display formats and fix display bugs. */
/*        We are starting a new trend here, with the documentation of the */
/*        changes to this version. Hopefully we will continue to do so. */

/*        The changes to this version are: */

/*           Calling a new subroutien to get reference frame names, to */
/*           support the non-inertial frames software. */

/*           Fixing some display inconsistencies when body, or frame */
/*           names are not found. */

/* -    Beta Version 1.0.0, 25-FEB-1993 (KRG) */

/* -& */
/* $ Index_Entries */

/*      format and write a ck segment summary */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*     Set the value for the maximum output display width. */


/*     Set the maximum length for the inertial reference frame name. */


/*     Set the maximum length for a body name. */


/*     Set the precision for fractions of seconds used for UTC times */
/*     when converted from ET times. */


/*     Set the length of a time string, UTC or SCLK. */


/*     Set the maximum length of a CK data type description. */


/*     Set a value for the length of the pointing only/pointing and */
/*     angular velocity messages. */


/*     Set the maximum number of CK data types. */


/*     Set up some mnemonics for accessing the correct labels. */


/*     Set the number of output lines. */


/*     Local variables */


/*     Initial Values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("CKWSS", (ftnlen)5);
    }

/*     Set up the line labels. */

    s_copy(lines, "   Segment ID     : #", (ftnlen)80, (ftnlen)21);
    s_copy(lines + 160, "   Spacecraft     : Body #", (ftnlen)80, (ftnlen)26);
    s_copy(lines + 80, "   Instrument Code: #", (ftnlen)80, (ftnlen)21);
    s_copy(lines + 560, "   UTC Start Time : #", (ftnlen)80, (ftnlen)21);
    s_copy(lines + 640, "   UTC Stop Time  : #", (ftnlen)80, (ftnlen)21);
    s_copy(lines + 720, "   SCLK Start Time: #", (ftnlen)80, (ftnlen)21);
    s_copy(lines + 800, "   SCLK Stop Time : #", (ftnlen)80, (ftnlen)21);
    s_copy(lines + 240, "   Reference Frame: Frame #", (ftnlen)80, (ftnlen)27)
	    ;
    s_copy(lines + 320, "   CK Data Type   : Type #", (ftnlen)80, (ftnlen)26);
    s_copy(lines + 400, "      Description : #", (ftnlen)80, (ftnlen)21);
    s_copy(lines + 480, "   Available Data : #", (ftnlen)80, (ftnlen)21);

/*     Format the segment ID. */

    repmc_(lines, "#", segid, lines, (ftnlen)80, (ftnlen)1, segid_len, (
	    ftnlen)80);

/*     Get the spacecraft ID code from the instrument ID code by dividing */
/*     by 1000. */

    ckmeta_(segins, "SPK", &spcrft, (ftnlen)3);
    ckmeta_(segins, "SCLK", &sclk, (ftnlen)4);

/*     Format the spacecraft name and its name if we found it. */

    bodc2n_(&spcrft, spname, &found, (ftnlen)32);
    if (found) {
	repmc_(lines + 160, "#", "#, #", lines + 160, (ftnlen)80, (ftnlen)1, (
		ftnlen)4, (ftnlen)80);
	repmi_(lines + 160, "#", &spcrft, lines + 160, (ftnlen)80, (ftnlen)1, 
		(ftnlen)80);
	repmc_(lines + 160, "#", spname, lines + 160, (ftnlen)80, (ftnlen)1, (
		ftnlen)32, (ftnlen)80);
    } else {
	repmi_(lines + 160, "#", &spcrft, lines + 160, (ftnlen)80, (ftnlen)1, 
		(ftnlen)80);
    }

/*     Format the instrument name if we found it. */

    repmi_(lines + 80, "#", segins, lines + 80, (ftnlen)80, (ftnlen)1, (
	    ftnlen)80);

/*     Convert the segment start and stop times from encoded SCLK */
/*     to SCLK time strings that are human readable. */

    scdecd_(&sclk, segbtm, begtim, (ftnlen)32);
    scdecd_(&sclk, segetm, endtim, (ftnlen)32);
    if (failed_()) {
	chkout_("CKWSS", (ftnlen)5);
	return 0;
    }

/*     Format the UTC AND SCLK times. */

    repmc_(lines + 720, "#", begtim, lines + 720, (ftnlen)80, (ftnlen)1, (
	    ftnlen)32, (ftnlen)80);
    repmc_(lines + 800, "#", endtim, lines + 800, (ftnlen)80, (ftnlen)1, (
	    ftnlen)32, (ftnlen)80);

/*     Convert the segment start and stop times from encoded SCLK to ET */
/*     so that we can convert them to UTC. */

    sct2e_(&sclk, segbtm, &beget);
    sct2e_(&sclk, segetm, &endet);
    if (failed_()) {
	chkout_("CKWSS", (ftnlen)5);
	return 0;
    }

/*     Convert the segment start and stop times from ET to UTC for */
/*     human readability. */

    et2utc_(&beget, "C", &c__3, begtim, (ftnlen)1, (ftnlen)32);
    et2utc_(&endet, "C", &c__3, endtim, (ftnlen)1, (ftnlen)32);
    if (failed_()) {
	chkout_("CKWSS", (ftnlen)5);
	return 0;
    }

/*     Format the UTC times. */

    repmc_(lines + 560, "#", begtim, lines + 560, (ftnlen)80, (ftnlen)1, (
	    ftnlen)32, (ftnlen)80);
    repmc_(lines + 640, "#", endtim, lines + 640, (ftnlen)80, (ftnlen)1, (
	    ftnlen)32, (ftnlen)80);

/*     Format the inertial reference frame and its name if we found it. */

    frmnam_(segfrm, frame, (ftnlen)32);
    if (s_cmp(frame, " ", (ftnlen)32, (ftnlen)1) != 0) {
	repmc_(lines + 240, "#", "#, #", lines + 240, (ftnlen)80, (ftnlen)1, (
		ftnlen)4, (ftnlen)80);
	repmi_(lines + 240, "#", segfrm, lines + 240, (ftnlen)80, (ftnlen)1, (
		ftnlen)80);
	repmc_(lines + 240, "#", frame, lines + 240, (ftnlen)80, (ftnlen)1, (
		ftnlen)32, (ftnlen)80);
    } else {
	repmi_(lines + 240, "#", segfrm, lines + 240, (ftnlen)80, (ftnlen)1, (
		ftnlen)80);
    }

/*     Format the CK segment type and a description if we have one. */

    if (*segtyp > 6 || *segtyp < 1) {
	s_copy(typdsc, "No description for this type. Do you need a new tool"
		"kit?", (ftnlen)80, (ftnlen)56);
    } else {
	s_copy(typdsc, cktyp + ((i__1 = *segtyp - 1) < 6 && 0 <= i__1 ? i__1 :
		 s_rnge("cktyp", i__1, "ckwss_", (ftnlen)424)) * 80, (ftnlen)
		80, (ftnlen)80);
    }
    repmi_(lines + 320, "#", segtyp, lines + 320, (ftnlen)80, (ftnlen)1, (
	    ftnlen)80);
    repmc_(lines + 400, "#", typdsc, lines + 400, (ftnlen)80, (ftnlen)1, (
	    ftnlen)80, (ftnlen)80);

/*     Format the pointing / pointing and angular velocity status */

    repmc_(lines + 480, "#", pvstat + ((i__1 = *segrts) < 2 && 0 <= i__1 ? 
	    i__1 : s_rnge("pvstat", i__1, "ckwss_", (ftnlen)432)) * 40, lines 
	    + 480, (ftnlen)80, (ftnlen)1, (ftnlen)40, (ftnlen)80);

/*     Display the summary. */

    writla_(&c__11, lines, unit, (ftnlen)80);

/*     We were either successful or not on the previous write. In either */
/*     event, we want to check out and return to the caller, so there is */
/*     no need to check FAILED() here. */

    chkout_("CKWSS", (ftnlen)5);
    return 0;
} /* ckwss_ */

