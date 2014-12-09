/* spkwss.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__10 = 10;

/* $Procedure      SPKWSS ( SPK write segment summary ) */
/* Subroutine */ int spkwss_(integer *unit, char *segid, integer *segtgt, 
	integer *segcen, integer *segfrm, integer *segtyp, doublereal *segbtm,
	 doublereal *segetm, ftnlen segid_len)
{
    /* Initialized data */

    static char spktyp[80*21] = "Modified Difference Array                  "
	    "                                     " "Fixed Width, Fixed Order"
	    " Chebyshev Polynomials: Pos                             " "Fixed"
	    " Width, Fixed Order Chebyshev Polynomials: Pos, Vel             "
	    "           " "TRW Elements (Space Telescope, TDRS)              "
	    "                              " "Two Body Propagation Using Disc"
	    "rete States                                      " "Type 6      "
	    "                                                                "
	    "    " "Precession Conic Elements                                "
	    "                       " "Discrete States, Evenly Spaced, Lagran"
	    "ge Interpolation                          " "Discrete States, Un"
	    "evenly Spaced, Lagrange Interpolation                        " 
	    "Two-Line Elements (Short Period)                               "
	    "                 " "Two-Line Elements (Long Period)             "
	    "                                    " "Discrete States, Evenly S"
	    "paced, Hermite Interpolation                           " "Discre"
	    "te States, Unevenly Spaced, Hermite Interpolation               "
	    "          " "Variable Width, Fixed order Chebyshev Polynomials: "
	    "Pos, Vel                     " "Two-Body with J2 precession     "
	    "                                                " "ISO elements "
	    "                                                                "
	    "   " "Precessing Equinoctial Elements                           "
	    "                      " "Mex/Rosetta Hermite/Lagrange Interpolat"
	    "ion                                      " "ESOC/DDID Piecewise "
	    "Interpolation                                               " 
	    "Fixed Width, Fixed Order Chebyshev Polynomials: Vel            "
	    "                 " "Extended Modified Difference Array          "
	    "                                    ";

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_rnge(char *, integer, 
	    char *, integer);

    /* Local variables */
    char body[32];
    extern /* Subroutine */ int etcal_(doublereal *, char *, ftnlen);
    char frame[32];
    extern /* Subroutine */ int chkin_(char *, ftnlen), repmc_(char *, char *,
	     char *, char *, ftnlen, ftnlen, ftnlen, ftnlen);
    char lines[80*10];
    logical found;
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen), bodc2n_(integer *, char *, logical *, 
	    ftnlen), et2utc_(doublereal *, char *, integer *, char *, ftnlen, 
	    ftnlen);
    extern logical failed_(void);
    char begtim[32], endtim[32];
    extern /* Subroutine */ int frmnam_(integer *, char *, ftnlen), chkout_(
	    char *, ftnlen), writla_(integer *, char *, integer *, ftnlen);
    char typdsc[80];
    extern logical return_(void);

/* $ Abstract */

/*     Write the segment summary for an SPK segment to a Fortran logical */
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
/*      SEGIDS    I   Segment ID for the segment in an SPK file. */
/*      SEGTGT    I   Target body for the segment in an SPK file. */
/*      SEGCEN    I   Center body for the segment in an SPK file. */
/*      SEGFRM    I   Reference frame for the segment in an SPK file. */
/*      SEGTYP    I   Ephemeris type for the segment in an SPK file. */
/*      SEGBTM    I   Begin time (ET) for the segment in an SPK file. */
/*      SEGETM    I   End time (ET) for the segment in an SPK file. */

/* $ Detailed_Input */

/*      UNIT     The Fortran logical unit to which the segment summary */
/*               is written. */

/*      SEGID    Segment ID for a segment in an SPK file. */

/*      SEGTGT   Target body for a segment in an SPK file. This is the */
/*               NAIF integer code for the target body. */

/*      SEGCEN   Center body for a segment in an SPK file. This is the */
/*               NAIF integer code for the center body. */

/*      SEGFRM   Inertial reference frame for a segment in an SPK file. */
/*               this is the NAIF integer code for the inertial reference */
/*               frame. */

/*      SEGTYP   Ephemeris type for a segment in an SPK file. This is an */
/*               integer code which represents the SPK segment data type. */

/*      SEGBTM   Begin time (ET) for a segment in an SPK file. */

/*      SEGETM   End time (ET) for a segment in an SPK file. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If an error occurs while writing to the logical unit, the error */
/*        will be signaled by a routine called by this routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine will format and display an SPK segment summary in a */
/*     human compatible fashion. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     1) This routine performs time conversions using ET2UTC, and */
/*        therefore requires that a SPICE leapseconds kernel file be */
/*        loaded into the SPICELIB kernel pool before being called. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     K.R. Gehringer (JPL) */
/*     W.L. Taber     (JPL) */

/* $ Version */

/* -    SPACIT Version 4.0.0, 18-OCT-2012 (NJB) */

/*        Updated to support SPK types 19, 20, and 21. */

/* -    SPACIT Version 3.0.0, 28-AUG-2002 (NJB) */

/*        Updated to support SPK type 18.  Fixed typo in type 13 */
/*        description. */

/* -    Beta Version 2.1.0, 28-FEB-1997 (WLT) */

/*        Added descriptions for types 4, 7, 10, 11, 12, 13, 15, 16 */
/*        and 17. */

/* -    Beta Version 2.0.0, 24-JAN-1996 (KRG) */

/*        There have been several undocumented revisions of this */
/*        subroutine to improve its display formats and fix display bugs. */
/*        We are starting a new trend here, with the documentation of the */
/*        changes to this version. Hopefully we will continue to do so. */

/*        The changes to this version are: */

/*           Calling a new subroutine to get reference frame names, to */
/*           support the non-inertial frames software. */

/*           Fixing some display inconsistencies when body, or frame */
/*           names are not found. */

/* -    Beta Version 1.0.0, 25-FEB-1993 (KRG) */

/* -& */
/* $ Index_Entries */

/*      format and write an spk segment summary */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*     Set the value for the maximum output display width. */


/*     Set the maximum length for the inertial reference frame name. */


/*     Set the maximum length for a body name. */


/*     Set the precision for fractions of seconds used for UTC times */
/*     when converted from ET times. */


/*     Set the length of a UTC time string. */


/*     Set the maximum length of an SPK data type description. */


/*     Set the maximum number of SPK data types. */


/*     Set up some mnemonics for accessing the correct labels. */


/*     Set the number of output lines. */


/*     Local variables */


/*     Initial Values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SPKWSS", (ftnlen)6);
    }

/*     Set up the line labels. */

    s_copy(lines, "   Segment ID     : #", (ftnlen)80, (ftnlen)21);
    s_copy(lines + 480, "   UTC Start Time : #", (ftnlen)80, (ftnlen)21);
    s_copy(lines + 560, "   UTC Stop Time  : #", (ftnlen)80, (ftnlen)21);
    s_copy(lines + 640, "   ET Start Time  : #", (ftnlen)80, (ftnlen)21);
    s_copy(lines + 720, "   ET Stop time   : #", (ftnlen)80, (ftnlen)21);
    s_copy(lines + 80, "   Target Body    : Body #", (ftnlen)80, (ftnlen)26);
    s_copy(lines + 160, "   Center Body    : Body #", (ftnlen)80, (ftnlen)26);
    s_copy(lines + 240, "   Reference frame: Frame #", (ftnlen)80, (ftnlen)27)
	    ;
    s_copy(lines + 320, "   SPK Data Type  : Type #", (ftnlen)80, (ftnlen)26);
    s_copy(lines + 400, "      Description : #", (ftnlen)80, (ftnlen)21);

/*     Format segment ID. */

    repmc_(lines, "#", segid, lines, (ftnlen)80, (ftnlen)1, segid_len, (
	    ftnlen)80);

/*     Convert the segment start and stop times from ET to UTC for */
/*     human readability. */

    et2utc_(segbtm, "C", &c__3, begtim, (ftnlen)1, (ftnlen)32);
    et2utc_(segetm, "C", &c__3, endtim, (ftnlen)1, (ftnlen)32);
    if (failed_()) {
	chkout_("SPKWSS", (ftnlen)6);
	return 0;
    }

/*     Format the UTC times. */

    repmc_(lines + 480, "#", begtim, lines + 480, (ftnlen)80, (ftnlen)1, (
	    ftnlen)32, (ftnlen)80);
    repmc_(lines + 560, "#", endtim, lines + 560, (ftnlen)80, (ftnlen)1, (
	    ftnlen)32, (ftnlen)80);

/*     Convert the ET times into Calendar format. */

    etcal_(segbtm, begtim, (ftnlen)32);
    etcal_(segetm, endtim, (ftnlen)32);
    if (failed_()) {
	chkout_("SPKWSS", (ftnlen)6);
	return 0;
    }

/*     Format the ET times. */

    repmc_(lines + 640, "#", begtim, lines + 640, (ftnlen)80, (ftnlen)1, (
	    ftnlen)32, (ftnlen)80);
    repmc_(lines + 720, "#", endtim, lines + 720, (ftnlen)80, (ftnlen)1, (
	    ftnlen)32, (ftnlen)80);

/*     Format the target body and its name if we found it. */

    bodc2n_(segtgt, body, &found, (ftnlen)32);
    if (found) {
	repmc_(lines + 80, "#", "#, #", lines + 80, (ftnlen)80, (ftnlen)1, (
		ftnlen)4, (ftnlen)80);
	repmi_(lines + 80, "#", segtgt, lines + 80, (ftnlen)80, (ftnlen)1, (
		ftnlen)80);
	repmc_(lines + 80, "#", body, lines + 80, (ftnlen)80, (ftnlen)1, (
		ftnlen)32, (ftnlen)80);
    } else {
	repmi_(lines + 80, "#", segtgt, lines + 80, (ftnlen)80, (ftnlen)1, (
		ftnlen)80);
    }

/*     Format the central body and its name if we found it. */

    bodc2n_(segcen, body, &found, (ftnlen)32);
    if (found) {
	repmc_(lines + 160, "#", "#, #", lines + 160, (ftnlen)80, (ftnlen)1, (
		ftnlen)4, (ftnlen)80);
	repmi_(lines + 160, "#", segcen, lines + 160, (ftnlen)80, (ftnlen)1, (
		ftnlen)80);
	repmc_(lines + 160, "#", body, lines + 160, (ftnlen)80, (ftnlen)1, (
		ftnlen)32, (ftnlen)80);
    } else {
	repmi_(lines + 160, "#", segcen, lines + 160, (ftnlen)80, (ftnlen)1, (
		ftnlen)80);
    }

/*     Format the reference frame and its name if we found it. */

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

/*     Format the SPK segment type and a description if we have one. */

    if (*segtyp > 21 || *segtyp < 1) {
	s_copy(typdsc, "No description for this type. Do you need a new tool"
		"kit?", (ftnlen)80, (ftnlen)56);
    } else {
	s_copy(typdsc, spktyp + ((i__1 = *segtyp - 1) < 21 && 0 <= i__1 ? 
		i__1 : s_rnge("spktyp", i__1, "spkwss_", (ftnlen)400)) * 80, (
		ftnlen)80, (ftnlen)80);
    }
    repmi_(lines + 320, "#", segtyp, lines + 320, (ftnlen)80, (ftnlen)1, (
	    ftnlen)80);
    repmc_(lines + 400, "#", typdsc, lines + 400, (ftnlen)80, (ftnlen)1, (
	    ftnlen)80, (ftnlen)80);

/*     Display the summary. */

    writla_(&c__10, lines, unit, (ftnlen)80);

/*     We were either successful or not on the previous write. In either */
/*     event, we want to check out and return to the caller, so there is */
/*     no need to check FAILED() here. */

    chkout_("SPKWSS", (ftnlen)6);
    return 0;
} /* spkwss_ */

