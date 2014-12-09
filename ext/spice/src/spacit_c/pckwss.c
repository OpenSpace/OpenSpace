/* pckwss.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__9 = 9;

/* $Procedure      PCKWSS ( PCK write segment summary ) */
/* Subroutine */ int pckwss_(integer *unit, char *segid, integer *segbod, 
	integer *segfrm, integer *segtyp, doublereal *segbtm, doublereal *
	segetm, ftnlen segid_len)
{
    /* Initialized data */

    static char pcktyp[80*3] = "***Not Used***                              "
	    "                                    " "Fixed Width, Fixed Order "
	    "Chebyshev Polynomials: Angles                          " "Variab"
	    "le Width Chebyshev Polynomials Angles (in degrees!!!)           "
	    "          ";

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_rnge(char *, integer, 
	    char *, integer);

    /* Local variables */
    static char body[32];
    extern /* Subroutine */ int etcal_(doublereal *, char *, ftnlen);
    static char frame[32];
    extern /* Subroutine */ int chkin_(char *, ftnlen), repmc_(char *, char *,
	     char *, char *, ftnlen, ftnlen, ftnlen, ftnlen);
    static char lines[80*9];
    static logical found;
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen), bodc2n_(integer *, char *, logical *, 
	    ftnlen), et2utc_(doublereal *, char *, integer *, char *, ftnlen, 
	    ftnlen);
    extern logical failed_(void);
    static char begtim[32], endtim[32];
    extern /* Subroutine */ int frmnam_(integer *, char *, ftnlen), chkout_(
	    char *, ftnlen), writla_(integer *, char *, integer *, ftnlen);
    static char typdsc[80];
    extern logical return_(void);

/* $ Abstract */

/*     Write the segment summary for a PCK segment to a Fortran logical */
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
/*      SEGIDS    I   Segment ID for the segment in a PCK file. */
/*      SEGBOD    I   Body for the segment in a PCK file. */
/*      SEGFRM    I   Reference frame for the segment in a PCK file. */
/*      SEGTYP    I   Ephemeris type for the segment in a PCK file. */
/*      SEGBTM    I   Begin time (ET) for the segment in a PCK file. */
/*      SEGETM    I   End time (ET) for the segment in a PCK file. */

/* $ Detailed_Input */

/*      UNIT     The Fortran logical unit to which the segment summary */
/*               is written. */

/*      SEGID    Segment ID for a segment in a PCK file. */

/*      SEGBOD   Body for a segment in a PCK file. This is the */
/*               NAIF integer code for the body. */

/*      SEGFRM   Inertial reference frame for a segment in a PCK file. */
/*               this is the NAIF integer code for the inertial reference */
/*               frame. */

/*      SEGTYP   Ephemeris type for a segment in a PCK file. This is an */
/*               integer code which represents the PCK segment data type. */

/*      SEGBTM   Begin time (ET) for a segment in a PCK file. */

/*      SEGETM   End time (ET) for a segment in a PCK file. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If an error occurs while writing to the logical unit, the error */
/*        will be signalled by a routine called by this routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine will format and display a PCK segment summary in a */
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

/*     W.L. Taber (JPL) */
/*     K.R. Gehringer (JPL) */

/* $ Version */

/* -    Beta Version 2.1.0, 17-May-2001 (WLT) (20 years in CA today!) */

/*        Added a description for type 03 PCK segments. */

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

/*      format and write a pck segment summary */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*     Set the value for the maximum output display width. */


/*     Set the maximum length for the inertial reference frame name. */


/*     Set the maximum length for a body name. */


/*     Set the precision for fractions of seconds used for UTC times */
/*     when converted from ET times. */


/*     Set the length of a UTC time string. */


/*     Set the maximum length of an PCK data type description. */


/*     Set the maximum number of PCK data types. */


/*     Set up some mnemonics for accessing the correct labels. */


/*     Set the number of output lines. */


/*     Local variables */


/*     Save everything to keep configuration control happy. */


/*     Initial Values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("PCKWSS", (ftnlen)6);
    }

/*     Set up the line labels. */

    s_copy(lines, "   Segment ID     : #", (ftnlen)80, (ftnlen)21);
    s_copy(lines + 400, "   UTC Start time : #", (ftnlen)80, (ftnlen)21);
    s_copy(lines + 480, "   UTC Stop time  : #", (ftnlen)80, (ftnlen)21);
    s_copy(lines + 560, "   ET Start time  : #", (ftnlen)80, (ftnlen)21);
    s_copy(lines + 640, "   ET Stop time   : #", (ftnlen)80, (ftnlen)21);
    s_copy(lines + 80, "   Body           : Body #", (ftnlen)80, (ftnlen)26);
    s_copy(lines + 160, "   Reference frame: Frame #", (ftnlen)80, (ftnlen)27)
	    ;
    s_copy(lines + 240, "   PCK Data Type  : #", (ftnlen)80, (ftnlen)21);
    s_copy(lines + 320, "      Description : #", (ftnlen)80, (ftnlen)21);

/*     Format the segment ID. */

    repmc_(lines, "#", segid, lines, (ftnlen)80, (ftnlen)1, segid_len, (
	    ftnlen)80);

/*     Convert the segment start and stop times from ET to UTC for */
/*     human readability. */

    et2utc_(segbtm, "C", &c__3, begtim, (ftnlen)1, (ftnlen)32);
    et2utc_(segetm, "C", &c__3, endtim, (ftnlen)1, (ftnlen)32);
    if (failed_()) {
	chkout_("PCKWSS", (ftnlen)6);
	return 0;
    }

/*     Format the UTC times. */

    repmc_(lines + 400, "#", begtim, lines + 400, (ftnlen)80, (ftnlen)1, (
	    ftnlen)32, (ftnlen)80);
    repmc_(lines + 480, "#", endtim, lines + 480, (ftnlen)80, (ftnlen)1, (
	    ftnlen)32, (ftnlen)80);

/*     Convert the ET times into Calendar format. */

    etcal_(segbtm, begtim, (ftnlen)32);
    etcal_(segetm, endtim, (ftnlen)32);
    if (failed_()) {
	chkout_("PCKWSS", (ftnlen)6);
	return 0;
    }

/*     Format the ET times. */

    repmc_(lines + 560, "#", begtim, lines + 560, (ftnlen)80, (ftnlen)1, (
	    ftnlen)32, (ftnlen)80);
    repmc_(lines + 640, "#", endtim, lines + 640, (ftnlen)80, (ftnlen)1, (
	    ftnlen)32, (ftnlen)80);

/*     Format the body and its name if we found it. */

    bodc2n_(segbod, body, &found, (ftnlen)32);
    if (found) {
	repmc_(lines + 80, "#", "#, #", lines + 80, (ftnlen)80, (ftnlen)1, (
		ftnlen)4, (ftnlen)80);
	repmi_(lines + 80, "#", segbod, lines + 80, (ftnlen)80, (ftnlen)1, (
		ftnlen)80);
	repmc_(lines + 80, "#", body, lines + 80, (ftnlen)80, (ftnlen)1, (
		ftnlen)32, (ftnlen)80);
    } else {
	repmi_(lines + 80, "#", segbod, lines + 80, (ftnlen)80, (ftnlen)1, (
		ftnlen)80);
    }

/*     Format the inertial reference frame and its name if we found it. */

    frmnam_(segfrm, frame, (ftnlen)32);
    if (s_cmp(frame, " ", (ftnlen)32, (ftnlen)1) != 0) {
	repmc_(lines + 160, "#", "#, #", lines + 160, (ftnlen)80, (ftnlen)1, (
		ftnlen)4, (ftnlen)80);
	repmi_(lines + 160, "#", segfrm, lines + 160, (ftnlen)80, (ftnlen)1, (
		ftnlen)80);
	repmc_(lines + 160, "#", frame, lines + 160, (ftnlen)80, (ftnlen)1, (
		ftnlen)32, (ftnlen)80);
    } else {
	repmi_(lines + 160, "#", segfrm, lines + 160, (ftnlen)80, (ftnlen)1, (
		ftnlen)80);
    }

/*     Format the PCK segment type and a description if we have one. */
/*     The reason SEGTYP >= 2 is that this routine works on binary */
/*     PCK files, and their segment types begin with type 2. Type 1 is */
/*     considered to be the text PCK files. */

    if (*segtyp > 3 || *segtyp < 2) {
	s_copy(typdsc, "No description for this type. Do you need a new tool"
		"kit?", (ftnlen)80, (ftnlen)56);
    } else {
	s_copy(typdsc, pcktyp + ((i__1 = *segtyp - 1) < 3 && 0 <= i__1 ? i__1 
		: s_rnge("pcktyp", i__1, "pckwss_", (ftnlen)352)) * 80, (
		ftnlen)80, (ftnlen)80);
    }
    repmi_(lines + 240, "#", segtyp, lines + 240, (ftnlen)80, (ftnlen)1, (
	    ftnlen)80);
    repmc_(lines + 320, "#", typdsc, lines + 320, (ftnlen)80, (ftnlen)1, (
	    ftnlen)80, (ftnlen)80);

/*     Display the summary. */

    writla_(&c__9, lines, unit, (ftnlen)80);

/*     We were either successful or not on the previous write. In either */
/*     event, we want to check out and return to the caller, so there is */
/*     no need to check FAILED() here. */

    chkout_("PCKWSS", (ftnlen)6);
    return 0;
} /* pckwss_ */

