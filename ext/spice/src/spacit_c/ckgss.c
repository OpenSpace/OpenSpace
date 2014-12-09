/* ckgss.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;

/* $Procedure      CKGSS ( CK get segment summary ) */
/* Subroutine */ int ckgss_(char *segid, integer *segins, integer *segfrm, 
	integer *segtyp, integer *segrts, doublereal *segbtm, doublereal *
	segetm, integer *segbad, integer *segead, ftnlen segid_len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    doublereal dsum[2];
    integer isum[6];
    extern /* Subroutine */ int dafgn_(char *, ftnlen), dafgs_(doublereal *), 
	    chkin_(char *, ftnlen), dafus_(doublereal *, integer *, integer *,
	     doublereal *, integer *);
    char tmpid[40];
    doublereal sumry[5];
    extern logical failed_(void);
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Get the summary for the current segment in a CK file. */

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
/*      SEGID     O   Segment ID for the segment in the CK file. */
/*      SEGINS    O   ID for the instrument having data in the segment. */
/*      SEGFRM    O   Reference frame for a segment in the cK file. */
/*      SEGTYP    O   Data type for a segment in the CK file. */
/*      SEGRTS    O   Flag for velocity info in the CK segment. */
/*      SEGBTM    O   Begin time (SCLK) for a segment in the CK file. */
/*      SEGETM    O   End time (SCLK) for a segment in the CK file. */
/*      SEGBAD    O   Begin address in the CK file of a segment. */
/*      SEGEAD    O   End address in the CK file of a segment. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

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

/*      SEGBAD   Begining DAF address for the data in the current */
/*               segment of a CK file. */

/*      SEGEAD   Ending DAF address for the data in the current segment */
/*               of a CK file. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If any errors occur, they will be signalled by routines called */
/*        by this routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine will provide the summary of the current segment */
/*     in the current CK file. A search must have been initiated */
/*     before this routine is called. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     K.R. Gehringer (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 22-APR-1994 (KRG) */

/* -& */
/* $ Index_Entries */

/*      summarize the current segment in a ck file */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*     Length of the double precision portion of a CK segment summary. */


/*     Length of the integer portion of a CK segment summary. */


/*     Length of a segment summary in an CK file. */


/*     Length of an CK segment ID. */


/*     Set up mnemonic names for the segment begin and end time indices. */


/*     Set up mnemonic names for the instrument, the inertial reference */
/*     frame, the data type, the rates flag, the begin address of the */
/*     segment, and the end address for the segment. */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("CKGSS", (ftnlen)5);
    }

/*     Get the segment ID, the segment summary, and unpack the segment */
/*     summary. If an error occurrs during any of these operations */
/*     checkout and return to the caller, as an appropriate error message */
/*     will already be set. We use a temporary variable to read the */
/*     segment ID so that we do not modify the input value unless we get */
/*     an ID. */

    dafgn_(tmpid, (ftnlen)40);
    dafgs_(sumry);
    dafus_(sumry, &c__2, &c__6, dsum, isum);
    if (failed_()) {
	chkout_("CKGSS", (ftnlen)5);
	return 0;
    }

/*     Set the output values for the begin and end encoded SCLK times */
/*     for the segment. */

    *segbtm = dsum[0];
    *segetm = dsum[1];

/*     Set the output values for: the segment ID, the instrument, the */
/*     inertial reference frame code, the segment data type code, the */
/*     rates flag, the begin address of the segment data, and the end */
/*     address of the segment data. */

    s_copy(segid, tmpid, segid_len, (ftnlen)40);
    *segins = isum[0];
    *segfrm = isum[1];
    *segtyp = isum[2];
    *segrts = isum[3];
    *segbad = isum[4];
    *segead = isum[5];
    chkout_("CKGSS", (ftnlen)5);
    return 0;
} /* ckgss_ */

