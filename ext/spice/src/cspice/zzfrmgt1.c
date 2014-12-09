/* zzfrmgt1.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure      ZZFRMGT1 (Frame get transformation) */
/* Subroutine */ int zzfrmgt1_(integer *infrm, doublereal *et, doublereal *
	xform, integer *outfrm, logical *found)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer cent, type__, i__, j;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    doublereal tsipm[36]	/* was [6][6] */;
    char versn[6];
    extern logical failed_(void);
    extern /* Subroutine */ int ckfxfm_(integer *, doublereal *, doublereal *,
	     integer *, logical *), namfrm_(char *, integer *, ftnlen), 
	    frinfo_(integer *, integer *, integer *, integer *, logical *), 
	    tisbod_(char *, integer *, doublereal *, doublereal *, ftnlen), 
	    tkfram_(integer *, doublereal *, integer *, logical *), sigerr_(
	    char *, ftnlen);
    integer typeid;
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen), errint_(char *, integer *, ftnlen), irfrot_(integer *, 
	    integer *, doublereal *);
    extern logical return_(void);
    extern /* Subroutine */ int invstm_(doublereal *, doublereal *);
    doublereal rot[9]	/* was [3][3] */;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Find the transformation from a user specified frame to */
/*     another frame at a user specified epoch. */

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

/*     FRAMES */

/* $ Declarations */
/* $ Abstract */

/*     The parameters below form an enumerated list of the recognized */
/*     frame types.  They are: INERTL, PCK, CK, TK, DYN.  The meanings */
/*     are outlined below. */

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

/* $ Parameters */

/*     INERTL      an inertial frame that is listed in the routine */
/*                 CHGIRF and that requires no external file to */
/*                 compute the transformation from or to any other */
/*                 inertial frame. */

/*     PCK         is a frame that is specified relative to some */
/*                 INERTL frame and that has an IAU model that */
/*                 may be retrieved from the PCK system via a call */
/*                 to the routine TISBOD. */

/*     CK          is a frame defined by a C-kernel. */

/*     TK          is a "text kernel" frame.  These frames are offset */
/*                 from their associated "relative" frames by a */
/*                 constant rotation. */

/*     DYN         is a "dynamic" frame.  These currently are */
/*                 parameterized, built-in frames where the full frame */
/*                 definition depends on parameters supplied via a */
/*                 frame kernel. */

/*     ALL         indicates any of the above classes. This parameter */
/*                 is used in APIs that fetch information about frames */
/*                 of a specified class. */


/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 4.0.0, 08-MAY-2012 (NJB) */

/*       The parameter ALL was added to support frame fetch APIs. */

/* -    SPICELIB Version 3.0.0, 28-MAY-2004 (NJB) */

/*       The parameter DYN was added to support the dynamic frame class. */

/* -    SPICELIB Version 2.0.0, 12-DEC-1996 (WLT) */

/*        Various unused frames types were removed and the */
/*        frame time TK was added. */

/* -    SPICELIB Version 1.0.0, 10-DEC-1995 (WLT) */

/* -& */

/*     End of INCLUDE file frmtyp.inc */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     INFRM      I   The integer code for a SPICE reference frame. */
/*     ET         I   An epoch in seconds past J2000. */
/*     XFORM      O   A state transformation matrix. */
/*     OUTFRM     O   The frame that XFORM transforms INFRM to. */
/*     FOUND      O   TRUE if a frame transformation can be found. */

/* $ Detailed_Input */

/*     INFRM       is the SPICE id-code for some reference frame. */

/*     ET          is an epoch in ephemeris seconds past J2000 at */
/*                 which the user wishes to retrieve a state */
/*                 transformation matrix. */

/* $ Detailed_Output */

/*     XFORM       is a 6x6 matrix that transforms states relative to */
/*                 INFRM to states relative to OUTFRM.  (Assuming such */
/*                 a transformation can be found.) */

/*     OUTFRM      is a reference frame.  The 6x6 matrix XFORM transforms */
/*                 states relative to INFRM to states relative to OUTFRM. */
/*                 The state transformation is achieved by multiplying */
/*                 XFORM on the right by a state relative to INFRM.  This */
/*                 is easily accomplished via the subroutine call */
/*                 shown below. */

/*                    CALL MXVG ( XFORM, STATE, 6, 6, OSTATE ) */

/*     FOUND       is a logical flag indicating whether or not a */
/*                 transformation matrix could be found from INFRM */
/*                 to some other frame.  If a transformation matrix */
/*                 cannot be found OUTFRM will be set to zero, FOUND */
/*                 will be set to FALSE and XFORM will be returned */
/*                 as the zero matrix. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) If a transformation matrix cannot be located, then */
/*        FOUND will be set to FALSE, OUTFRM will be set to zero */
/*        and XFORM will be set to the zero 6x6 matrix. */

/*     2) If the class of the requested frame is not recognized the */
/*        exception 'SPICE(UNKNOWNFRAMETYPE)' will be signalled. */

/*        of this routine. */

/*     3) If the reference frame REF is dynamic, the error */
/*        SPICE(RECURSIONTOODEEP) will be signaled. */


/* $ Particulars */

/*     This is a low level routine used for determining a chain */
/*     of state transformation matrices from one frame to another. */

/* $ Examples */

/*     See FRMCHG. */

/* $ Restrictions */

/*     1) SPICE Private routine. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 12-DEC-2004 (NJB) */

/*        Based on SPICELIB Version 3.0.0, 21-JUN-2004 (NJB) */

/* -& */
/* $ Index_Entries */

/*     Find a frame transformation matrix from a specified frame */

/* -& */

/*     Spicelib Functions */


/*     Local Variables */

    s_copy(versn, "2.0.0", (ftnlen)6, (ftnlen)5);
    *found = FALSE_;

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZFRMGT1", (ftnlen)8);

/*     Get all the needed information about this frame. */

    frinfo_(infrm, &cent, &type__, &typeid, found);
    if (! (*found)) {
	chkout_("ZZFRMGT1", (ftnlen)8);
	return 0;
    }
    if (type__ == 2) {
	tisbod_("J2000", &typeid, et, tsipm, (ftnlen)5);
	invstm_(tsipm, xform);
	namfrm_("J2000", outfrm, (ftnlen)5);
    } else if (type__ == 1) {
	irfrot_(infrm, &c__1, rot);
	for (i__ = 1; i__ <= 3; ++i__) {
	    for (j = 1; j <= 3; ++j) {
		xform[(i__1 = i__ + j * 6 - 7) < 36 && 0 <= i__1 ? i__1 : 
			s_rnge("xform", i__1, "zzfrmgt1_", (ftnlen)218)] = 
			rot[(i__2 = i__ + j * 3 - 4) < 9 && 0 <= i__2 ? i__2 :
			 s_rnge("rot", i__2, "zzfrmgt1_", (ftnlen)218)];
		xform[(i__1 = i__ + 3 + (j + 3) * 6 - 7) < 36 && 0 <= i__1 ? 
			i__1 : s_rnge("xform", i__1, "zzfrmgt1_", (ftnlen)219)
			] = rot[(i__2 = i__ + j * 3 - 4) < 9 && 0 <= i__2 ? 
			i__2 : s_rnge("rot", i__2, "zzfrmgt1_", (ftnlen)219)];
		xform[(i__1 = i__ + 3 + j * 6 - 7) < 36 && 0 <= i__1 ? i__1 : 
			s_rnge("xform", i__1, "zzfrmgt1_", (ftnlen)220)] = 0.;
		xform[(i__1 = i__ + (j + 3) * 6 - 7) < 36 && 0 <= i__1 ? i__1 
			: s_rnge("xform", i__1, "zzfrmgt1_", (ftnlen)221)] = 
			0.;
	    }
	}
	*outfrm = 1;
    } else if (type__ == 3) {
	ckfxfm_(&typeid, et, xform, outfrm, found);
    } else if (type__ == 4) {
	tkfram_(&typeid, rot, outfrm, found);
	for (i__ = 1; i__ <= 3; ++i__) {
	    for (j = 1; j <= 3; ++j) {
		xform[(i__1 = i__ + j * 6 - 7) < 36 && 0 <= i__1 ? i__1 : 
			s_rnge("xform", i__1, "zzfrmgt1_", (ftnlen)238)] = 
			rot[(i__2 = i__ + j * 3 - 4) < 9 && 0 <= i__2 ? i__2 :
			 s_rnge("rot", i__2, "zzfrmgt1_", (ftnlen)238)];
		xform[(i__1 = i__ + 3 + (j + 3) * 6 - 7) < 36 && 0 <= i__1 ? 
			i__1 : s_rnge("xform", i__1, "zzfrmgt1_", (ftnlen)239)
			] = rot[(i__2 = i__ + j * 3 - 4) < 9 && 0 <= i__2 ? 
			i__2 : s_rnge("rot", i__2, "zzfrmgt1_", (ftnlen)239)];
		xform[(i__1 = i__ + 3 + j * 6 - 7) < 36 && 0 <= i__1 ? i__1 : 
			s_rnge("xform", i__1, "zzfrmgt1_", (ftnlen)240)] = 0.;
		xform[(i__1 = i__ + (j + 3) * 6 - 7) < 36 && 0 <= i__1 ? i__1 
			: s_rnge("xform", i__1, "zzfrmgt1_", (ftnlen)241)] = 
			0.;
	    }
	}
    } else if (type__ == 5) {
	setmsg_("The reference frame # is a dynamic frame. Dynamic frames ma"
		"y not be used at recursion level 1.", (ftnlen)94);
	errint_("#", infrm, (ftnlen)1);
	sigerr_("SPICE(RECURSIONTOODEEP)", (ftnlen)23);
	chkout_("ZZFRMGT1", (ftnlen)8);
	return 0;
    } else {
	setmsg_("The reference frame # has class id-code #. This form of ref"
		"erence frame is not supported in version # of ZZFRMGT1. You "
		"need to update your version of SPICELIB to the latest versio"
		"n in order to support this frame. ", (ftnlen)213);
	errint_("#", infrm, (ftnlen)1);
	errint_("#", &type__, (ftnlen)1);
	errch_("#", versn, (ftnlen)1, (ftnlen)6);
	sigerr_("SPICE(UNKNOWNFRAMETYPE)", (ftnlen)23);
	chkout_("ZZFRMGT1", (ftnlen)8);
	return 0;
    }
    if (failed_()) {
	*found = FALSE_;
    }
    chkout_("ZZFRMGT1", (ftnlen)8);
    return 0;
} /* zzfrmgt1_ */

