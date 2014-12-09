/* pckgss.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__5 = 5;

/* $Procedure      PCKGSS ( PCK get segment summary ) */
/* Subroutine */ int pckgss_(char *segid, integer *segbod, integer *segfrm, 
	integer *segtyp, doublereal *segbtm, doublereal *segetm, integer *
	segbad, integer *segead, ftnlen segid_len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    doublereal dsum[2];
    integer isum[5];
    extern /* Subroutine */ int dafgn_(char *, ftnlen), dafgs_(doublereal *), 
	    chkin_(char *, ftnlen), dafus_(doublereal *, integer *, integer *,
	     doublereal *, integer *);
    char tmpid[40];
    doublereal sumry[5];
    extern logical failed_(void);
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Get the summary for the current segment in a PCK file. */

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

/*      PCK */

/* $ Keywords */

/*     None. */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*      SEGID     O   Segment ID for the segment in a PCK file. */
/*      SEGBOD    O   Body for the segment in a PCK file. */
/*      SEGFRM    O   Reference frame for the segment in a PCK file. */
/*      SEGTYP    O   Ephemeris type for the segment in a PCK file. */
/*      SEGBTM    O   Begin time (ET) for the segment in a PCK file. */
/*      SEGETM    O   End time (ET) for the segment in a PCK file. */
/*      SEGBAD    O   Begin address in the PCK file of a PCK segment. */
/*      SEGEAD    O   End address in the PCK file of a PCK segment. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*      SEGID    Segment ID for the current segment in a PCK file. */

/*      SEGBOD   Body for the current segment in a PCK file. This is the */
/*               NAIF integer code for the body. */

/*      SEGFRM   Inertial reference frame for the current segment in */
/*               a PCK file. this is the NAIF integer code for the */
/*               inertial reference frame. */

/*      SEGTYP   Data type for the current segment in a PCK file. */
/*               This is an integer code which represents the PCK segment */
/*               data type. */

/*      SEGBTM   Begin time (ET) for the current segment in a PCK file. */

/*      SEGETM   End time (ET) for the current segment in a PCK file. */

/*      SEGBAD   Beginning DAF address for the data of the current */
/*               segment in a PCK file. */

/*      SEGEAD   Ending DAF address for the data of the current segment */
/*               in a PCK file. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If any errors occur, they will be signalled by routines called */
/*        by this routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine provides the summary of the current segment in the */
/*     current PCK file. A PCK file must have been opened, and a */
/*     search must have been initiated before this routine may called. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     K.R. Gehringer (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 22-JAN-1993 (KRG) */

/* -& */
/* $ Index_Entries */

/*      get the summary of the current pck segment */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*     Length of the double precision portion of an PCK segment summary. */


/*     Length of the integer portion of an PCK segment summary. */


/*     Length of a segment summary in an PCK file. */


/*     Length of an PCK segment ID. */


/*     Set up mnemonic names for the segment begin and end time indices. */


/*     Set up mnemonic names for the target body, the center body, the */
/*     inertial reference frame, the ephemeris type, the begin address */
/*     of the segment, and the end address of the segment. */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("PCKGSS", (ftnlen)6);
    }

/*     Get the segment ID, the segment summary, and unpack the segment */
/*     summary. If an error occurred during any of these operations, */
/*     checkout and return to the caller, as an appropriate error message */
/*     will already be set. We use a temporary variable to read the */
/*     segment ID so that we do not modify the input value unless we get */
/*     one. */

    dafgn_(tmpid, (ftnlen)40);
    dafgs_(sumry);
    dafus_(sumry, &c__2, &c__5, dsum, isum);
    if (failed_()) {
	chkout_("PCKGSS", (ftnlen)6);
	return 0;
    }

/*     Set the output values for the begin and end ET times for the */
/*     segment. */

    *segbtm = dsum[0];
    *segetm = dsum[1];

/*     Set the output values for: the segment ID,  the center body, the */
/*     inertial reference frame code, the segment data type code, the */
/*     begin address of the segment data, and the end address of the */
/*     segment data. */

    s_copy(segid, tmpid, segid_len, (ftnlen)40);
    *segbod = isum[0];
    *segfrm = isum[1];
    *segtyp = isum[2];
    *segbad = isum[3];
    *segead = isum[4];
    chkout_("PCKGSS", (ftnlen)6);
    return 0;
} /* pckgss_ */

