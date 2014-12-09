/* bltfrm.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static integer c__126 = 126;
static integer c__127 = 127;

/* $Procedure BLTFRM ( Built-in frame IDs ) */
/* Subroutine */ int bltfrm_(integer *frmcls, integer *idset)
{
    /* Initialized data */

    static logical pass1 = TRUE_;

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    static integer i__, j, fcode[126];
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer sizei_(integer *);
    extern logical failed_(void);
    static integer bidids[127], to, fclsid[126], bididx[127];
    extern /* Subroutine */ int scardi_(integer *, integer *);
    static char frname[32*126];
    static integer bidpol[133], fclass[126], corder[126], center[126], bnmidx[
	    127], bidlst[127];
    extern /* Subroutine */ int orderi_(integer *, integer *, integer *);
    static integer bnmpol[133];
    static char bnmnms[32*127];
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    static integer ctrord[126], bnmlst[127];
    extern /* Subroutine */ int chkout_(char *, ftnlen), zzfdat_(integer *, 
	    integer *, char *, integer *, integer *, integer *, integer *, 
	    integer *, integer *, integer *, char *, integer *, integer *, 
	    integer *, integer *, integer *, ftnlen, ftnlen), setmsg_(char *, 
	    ftnlen), errint_(char *, integer *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Return a set containing the frame IDs of all built-in frames of a */
/*     specified class. */

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

/*     CELLS */
/*     FRAMES */
/*     NAIF_IDS */
/*     SETS */

/* $ Keywords */

/*     FRAME */
/*     SET */
/*     UTILITY */

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

/* $ Abstract */

/*     This file contains the number of inertial reference */
/*     frames that are currently known by the SPICE toolkit */
/*     software. */

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
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     NINERT     P   Number of known inertial reference frames. */

/* $ Parameters */

/*     NINERT     is the number of recognized inertial reference */
/*                frames.  This value is needed by both CHGIRF */
/*                ZZFDAT, and FRAMEX. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 10-OCT-1996 (WLT) */

/* -& */
/* $ Abstract */

/*     This file contains the number of non-inertial reference */
/*     frames that are currently built into the SPICE toolkit */
/*     software. */


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
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     NINERT     P   Number of built-in non-inertial reference frames. */

/* $ Parameters */

/*     NINERT     is the number of built-in non-inertial reference */
/*                frames.  This value is needed by both  ZZFDAT, and */
/*                FRAMEX. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */
/*     W.L. Taber      (JPL) */
/*     F.S. Turner     (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.5.0, 11-OCT-2011 (BVS) */

/*        Increased the number of non-inertial frames from 100 to 105 */
/*        in order to accomodate the following PCK based frames: */

/*           IAU_CERES */
/*           IAU_PALLAS */
/*           IAU_LUTETIA */
/*           IAU_DAVIDA */
/*           IAU_STEINS */

/* -    SPICELIB Version 1.4.0, 11-MAY-2010 (BVS) */

/*        Increased the number of non-inertial frames from 96 to 100 */
/*        in order to accomodate the following PCK based frames: */

/*           IAU_BORRELLY */
/*           IAU_TEMPEL_1 */
/*           IAU_VESTA */
/*           IAU_ITOKAWA */

/* -    SPICELIB Version 1.3.0, 12-DEC-2002 (BVS) */

/*        Increased the number of non-inertial frames from 85 to 96 */
/*        in order to accomodate the following PCK based frames: */

/*           IAU_CALLIRRHOE */
/*           IAU_THEMISTO */
/*           IAU_MAGACLITE */
/*           IAU_TAYGETE */
/*           IAU_CHALDENE */
/*           IAU_HARPALYKE */
/*           IAU_KALYKE */
/*           IAU_IOCASTE */
/*           IAU_ERINOME */
/*           IAU_ISONOE */
/*           IAU_PRAXIDIKE */

/* -    SPICELIB Version 1.2.0, 02-AUG-2002 (FST) */

/*        Increased the number of non-inertial frames from 81 to 85 */
/*        in order to accomodate the following PCK based frames: */

/*           IAU_PAN */
/*           IAU_GASPRA */
/*           IAU_IDA */
/*           IAU_EROS */

/* -    SPICELIB Version 1.1.0, 20-FEB-1997 (WLT) */

/*        Increased the number of non-inertial frames from 79 to 81 */
/*        in order to accomodate the following earth rotation */
/*        models: */

/*           ITRF93 */
/*           EARTH_FIXED */

/* -    SPICELIB Version 1.0.0, 10-OCT-1996 (WLT) */

/* -& */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     FRMCLS     I   Frame class. */
/*     IDSET      O   Set of ID codes of frames of the specified class. */

/* $ Detailed_Input */

/*     FRMCLS         is an integer code specifying the frame class or */
/*                    classes for which built-in frame ID codes are */
/*                    requested. FRMCLS may designate a single class or */
/*                    "all classes." */

/*                    The include file frmtyp.inc declares parameters */
/*                    identifying frame classes. The supported values */
/*                    and corresponding meanings of FRMCLS are */

/*                       Parameter      Value    Meaning */
/*                       =========      =====    ================= */
/*                       ALL              -1     All frame classes */
/*                       INERTL            1     Built-in inertial */
/*                       PCK               2     PCK-based frame */
/*                       CK                3     CK-based frame */
/*                       TK                4     Fixed offset ("text */
/*                                               kernel") frame */
/*                       DYN               5     Dynamic frame */

/* $ Detailed_Output */

/*     IDSET          is a SPICE set containing the ID codes of all */
/*                    built-in reference frames of the specified class */
/*                    or classes. */

/* $ Parameters */

/*     See the INCLUDE file frmtyp.inc. */

/* $ Exceptions */

/*     1) If the input frame class argument is not defined in */
/*        frmtyp.inc, the error SPICE(BADFRAMECLASS) is signaled. */

/*     2) If the size of IDSET is too small to hold the requested */
/*        frame ID set, the error SPICE(SETTOOSMALL) is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine has a counterpart */

/*        KPLFRM */

/*     which fetches the frame IDs of all frames specified in the kernel */
/*     pool. */

/* $ Examples */

/*     1) Display the IDs and names of all SPICE built-in frames. */
/*        Group the outputs by frame class. Also fetch and display */
/*        the entire set of IDs and names using the parameter ALL. */


/*        Program source code: */


/*              PROGRAM EX1 */

/*              IMPLICIT NONE */

/*              INCLUDE 'ninert.inc' */
/*              INCLUDE 'nninrt.inc' */
/*              INCLUDE 'frmtyp.inc' */
/*        C */
/*        C     SPICELIB functions */
/*        C */
/*              INTEGER               CARDI */
/*        C */
/*        C     Local parameters */
/*        C */
/*              INTEGER               NFRAME */
/*              PARAMETER           ( NFRAME = NINERT + NNINRT ) */

/*              INTEGER               LBCELL */
/*              PARAMETER           ( LBCELL = -5 ) */

/*              INTEGER               LNSIZE */
/*              PARAMETER           ( LNSIZE = 80 ) */

/*              INTEGER               FRNMLN */
/*              PARAMETER           ( FRNMLN = 32 ) */

/*        C */
/*        C     Local variables */
/*        C */
/*              CHARACTER*(FRNMLN)    FRNAME */
/*              CHARACTER*(LNSIZE)    OUTLIN */

/*              INTEGER               I */
/*              INTEGER               IDSET ( LBCELL : NFRAME ) */
/*              INTEGER               J */

/*        C */
/*        C     Initialize the frame set. */
/*        C */
/*              CALL SSIZEI ( NFRAME, IDSET ) */

/*        C */
/*        C     Fetch and display the frames of each class. */
/*        C */
/*              DO I = 1, 6 */

/*                 IF ( I .LT. 6 ) THEN */
/*        C */
/*        C           Fetch the frames of class I. */
/*        C */
/*                    CALL BLTFRM ( I, IDSET ) */

/*                    OUTLIN = 'Number of frames of class #: #' */
/*                    CALL REPMI ( OUTLIN, '#', I,            OUTLIN ) */
/*                    CALL REPMI ( OUTLIN, '#', CARDI(IDSET), OUTLIN ) */

/*                 ELSE */
/*        C */
/*        C           Fetch IDs of all built-in frames. */
/*        C */
/*                    CALL BLTFRM ( ALL, IDSET ) */

/*                    OUTLIN = 'Number of built-in frames: #' */
/*                    CALL REPMI ( OUTLIN, '#', CARDI(IDSET), OUTLIN ) */

/*                 END IF */

/*                 CALL TOSTDO ( ' '    ) */
/*                 CALL TOSTDO ( OUTLIN ) */
/*                 CALL TOSTDO ( '   Frame IDs and names' ) */

/*                 DO J = 1, CARDI(IDSET) */
/*                    CALL FRMNAM ( IDSET(J), FRNAME ) */
/*                    WRITE (*,*) IDSET(J), '  ', FRNAME */
/*                 END DO */

/*              END DO */

/*              END */


/*     The output from the program, when the program was linked */
/*     against the N0064 SPICE Toolkit, is shown below. Note that */
/*     the set of built-in frames, particularly the non-inertial */
/*     ones, will grow over time, so the output shown here may */
/*     be out of sync with that produced by a current SPICE Toolkit. */

/*     The output shown here has been abbreviated. */


/*        Number of frames of class 1: 21 */
/*           Frame IDs and names */
/*                   1   J2000 */
/*                   2   B1950 */
/*                   3   FK4 */
/*                   4   DE-118 */
/*                   5   DE-96 */
/*                   6   DE-102 */
/*                   7   DE-108 */
/*                   8   DE-111 */
/*                   9   DE-114 */
/*                  10   DE-122 */
/*                  11   DE-125 */
/*                  12   DE-130 */
/*                  13   GALACTIC */
/*                  14   DE-200 */
/*                  15   DE-202 */
/*                  16   MARSIAU */
/*                  17   ECLIPJ2000 */
/*                  18   ECLIPB1950 */
/*                  19   DE-140 */
/*                  20   DE-142 */
/*                  21   DE-143 */

/*        Number of frames of class 2: 99 */
/*           Frame IDs and names */
/*               10001   IAU_MERCURY_BARYCENTER */
/*               10002   IAU_VENUS_BARYCENTER */

/*                   ... */

/*               10100   IAU_ITOKAWA */
/*               13000   ITRF93 */

/*        Number of frames of class 3: 0 */
/*           Frame IDs and names */

/*        Number of frames of class 4: 1 */
/*           Frame IDs and names */
/*               10081   EARTH_FIXED */

/*        Number of frames of class 5: 0 */
/*           Frame IDs and names */

/*        Number of built-in frames: 121 */
/*           Frame IDs and names */
/*                   1   J2000 */
/*                   2   B1950 */

/*                   ... */

/*               10100   IAU_ITOKAWA */
/*               13000   ITRF93 */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     B.V. Semenov    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 09-AUG-2013 (BVS) */

/*        Updated for changed ZZFDAT calling sequence. */

/* -    SPICELIB Version 1.0.0, 21-MAY-2012 (NJB) */

/* -& */
/* $ Index_Entries */

/*     fetch names of built-in reference frames */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Built-in frame hashes returned by ZZFDAT. */


/*     Saved variables */


/*     Save all variables in order to avoid problems */
/*     in code translated by f2c. */


/*     Initial values */

    if (return_()) {
	return 0;
    }
    chkin_("BLTFRM", (ftnlen)6);

/*     The output set starts out empty. */

    scardi_(&c__0, idset);

/*     On the first pass, fetch all data for the */
/*     built-in frames. */

    if (pass1) {
	zzfdat_(&c__126, &c__127, frname, fcode, center, fclass, fclsid, 
		ctrord, bnmlst, bnmpol, bnmnms, bnmidx, bidlst, bidpol, 
		bidids, bididx, (ftnlen)32, (ftnlen)32);
	if (failed_()) {
	    chkout_("BLTFRM", (ftnlen)6);
	    return 0;
	}
	pass1 = FALSE_;
    }

/*     Check the input frame class. */

/*     This block of code must be kept in sync with frmtyp.inc. */

    if (*frmcls > 5 || *frmcls == 0 || *frmcls < -1) {
	setmsg_("Frame class specifier FRMCLS was #; this value is not suppo"
		"rted.", (ftnlen)64);
	errint_("#", frmcls, (ftnlen)1);
	sigerr_("SPICE(BADFRAMECLASS)", (ftnlen)20);
	chkout_("BLTFRM", (ftnlen)6);
	return 0;
    }

/*     Make sure the set is large enough to hold all of */
/*     the IDs of the built-in frames. */

    if (sizei_(idset) < 126) {
	setmsg_("Frame ID set argument IDSET has size #; required size is at"
		" least #.", (ftnlen)68);
	i__1 = sizei_(idset);
	errint_("#", &i__1, (ftnlen)1);
	errint_("#", &c__126, (ftnlen)1);
	sigerr_("SPICE(SETTOOSMALL)", (ftnlen)18);
	chkout_("BLTFRM", (ftnlen)6);
	return 0;
    }

/*     Transfer ID codes of all frames of the specified class */
/*     to the output set. First, generate an order vector for */
/*     the ID codes. */

    orderi_(fcode, &c__126, corder);
    to = 0;
    for (i__ = 1; i__ <= 126; ++i__) {

/*        Get the index J in the parallel data arrays of */
/*        the Ith frame, ordered by ID code. */

	j = corder[(i__1 = i__ - 1) < 126 && 0 <= i__1 ? i__1 : s_rnge("cord"
		"er", i__1, "bltfrm_", (ftnlen)451)];
	if (fclass[(i__1 = j - 1) < 126 && 0 <= i__1 ? i__1 : s_rnge("fclass",
		 i__1, "bltfrm_", (ftnlen)453)] == *frmcls || *frmcls == -1) {

/*           The frame at index J belongs to the */
/*           requested class. Append the frame's ID */
/*           code to the set. */

	    ++to;
	    idset[to + 5] = fcode[(i__1 = j - 1) < 126 && 0 <= i__1 ? i__1 : 
		    s_rnge("fcode", i__1, "bltfrm_", (ftnlen)461)];
	}
    }

/*     Set the cardinality of the output set. */

/*     Note that we populated the set using an order vector, so sorting */
/*     the elements is not necessary. We rely on ZZFDAT to not give us */
/*     duplicate frame specifications. */

    scardi_(&to, idset);
    chkout_("BLTFRM", (ftnlen)6);
    return 0;
} /* bltfrm_ */

