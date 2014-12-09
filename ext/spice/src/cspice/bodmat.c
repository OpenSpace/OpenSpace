/* bodmat.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__1 = 1;
static integer c__3 = 3;
static integer c__100 = 100;
static integer c__9 = 9;

/* $Procedure      BODMAT ( Return transformation matrix for a body ) */
/* Subroutine */ int bodmat_(integer *body, doublereal *et, doublereal *tipm)
{
    /* Initialized data */

    static logical first = TRUE_;
    static logical found = FALSE_;

    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer i_dnnt(doublereal *);
    double sin(doublereal), cos(doublereal), d_mod(doublereal *, doublereal *)
	    ;

    /* Local variables */
    integer cent;
    char item[32];
    doublereal j2ref[9]	/* was [3][3] */;
    extern integer zzbodbry_(integer *);
    extern /* Subroutine */ int eul2m_(doublereal *, doublereal *, doublereal 
	    *, integer *, integer *, integer *, doublereal *);
    doublereal d__;
    integer i__, j;
    doublereal dcoef[3], t, w;
    extern /* Subroutine */ int etcal_(doublereal *, char *, ftnlen);
    integer refid;
    doublereal delta;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal epoch, rcoef[3], tcoef[200]	/* was [2][100] */, wcoef[3];
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen);
    doublereal theta;
    extern /* Subroutine */ int moved_(doublereal *, integer *, doublereal *),
	     repmi_(char *, char *, integer *, char *, ftnlen, ftnlen, ftnlen)
	    , errdp_(char *, doublereal *, ftnlen);
    doublereal costh[100];
    extern doublereal vdotg_(doublereal *, doublereal *, integer *);
    char dtype[1];
    doublereal sinth[100], tsipm[36]	/* was [6][6] */;
    extern doublereal twopi_(void);
    static integer j2code;
    doublereal ac[100], dc[100];
    integer na, nd;
    doublereal ra, wc[100];
    extern /* Subroutine */ int cleard_(integer *, doublereal *);
    extern logical bodfnd_(integer *, char *, ftnlen);
    extern /* Subroutine */ int bodvcd_(integer *, char *, integer *, integer 
	    *, doublereal *, ftnlen);
    integer frcode;
    extern doublereal halfpi_(void);
    extern /* Subroutine */ int ccifrm_(integer *, integer *, integer *, char 
	    *, integer *, logical *, ftnlen);
    integer nw;
    doublereal conepc, conref;
    extern /* Subroutine */ int pckmat_(integer *, doublereal *, integer *, 
	    doublereal *, logical *);
    integer ntheta;
    extern /* Subroutine */ int gdpool_(char *, integer *, integer *, integer 
	    *, doublereal *, logical *, ftnlen);
    char fixfrm[32], errmsg[1840];
    extern /* Subroutine */ int irfnum_(char *, integer *, ftnlen), dtpool_(
	    char *, logical *, integer *, char *, ftnlen, ftnlen);
    doublereal tmpmat[9]	/* was [3][3] */;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), suffix_(char *, 
	    integer *, char *, ftnlen, ftnlen), errint_(char *, integer *, 
	    ftnlen), sigerr_(char *, ftnlen), chkout_(char *, ftnlen), 
	    irfrot_(integer *, integer *, doublereal *);
    extern logical return_(void);
    char timstr[35];
    extern doublereal j2000_(void);
    doublereal dec;
    integer dim, ref;
    doublereal phi;
    extern doublereal rpd_(void), spd_(void);
    extern /* Subroutine */ int mxm_(doublereal *, doublereal *, doublereal *)
	    ;

/* $ Abstract */

/*     Return the J2000 to body Equator and Prime Meridian coordinate */
/*     transformation matrix for a specified body. */

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

/*     PCK */
/*     NAIF_IDS */
/*     TIME */

/* $ Keywords */

/*     CONSTANTS */

/* $ Declarations */
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


/*     Include File:  SPICELIB Error Handling Parameters */

/*        errhnd.inc  Version 2    18-JUN-1997 (WLT) */

/*           The size of the long error message was */
/*           reduced from 25*80 to 23*80 so that it */
/*           will be accepted by the Microsoft Power Station */
/*           FORTRAN compiler which has an upper bound */
/*           of 1900 for the length of a character string. */

/*        errhnd.inc  Version 1    29-JUL-1997 (NJB) */



/*     Maximum length of the long error message: */


/*     Maximum length of the short error message: */


/*     End Include File:  SPICELIB Error Handling Parameters */

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
/*     BODY       I   ID code of body. */
/*     ET         I   Epoch of transformation. */
/*     TIPM       O   Transformation from Inertial to PM for BODY at ET. */

/* $ Detailed_Input */

/*     BODY        is the integer ID code of the body for which the */
/*                 transformation is requested. Bodies are numbered */
/*                 according to the standard NAIF numbering scheme. */

/*     ET          is the epoch at which the transformation is */
/*                 requested. (This is typically the epoch of */
/*                 observation minus the one-way light time from */
/*                 the observer to the body at the epoch of */
/*                 observation.) */

/* $ Detailed_Output */

/*     TIPM        is the transformation matrix from Inertial to body */
/*                 Equator and Prime Meridian.  The X axis of the PM */
/*                 system is directed to the intersection of the */
/*                 equator and prime meridian. The Z axis points north. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If data required to define the body-fixed frame associated */
/*        with BODY are not found in the binary PCK system or the kernel */
/*        pool, the error SPICE(FRAMEDATANOTFOUND) is signaled. In */
/*        the case of IAU style body-fixed frames, the absence of */
/*        prime meridian polynomial data (which are required) is used */
/*        as an indicator of missing data. */

/*     2) If the test for exception (1) passes, but in fact requested */
/*        data are not available in the kernel pool, the error will be */
/*        signaled by routines in the call tree of this routine. */

/*     3) If the kernel pool does not contain all of the data required */
/*        to define the number of nutation precession angles */
/*        corresponding to the available nutation precession */
/*        coefficients, the error SPICE(INSUFFICIENTANGLES) is */
/*        signaled. */

/*     4) If the reference frame REF is not recognized, a routine */
/*        called by BODMAT will diagnose the condition and invoke the */
/*        SPICE error handling system. */

/*     5) If the specified body code BODY is not recognized, the */
/*        error is diagnosed by a routine called by BODMAT. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine is related to the more general routine TIPBOD */
/*     which returns a matrix that transforms vectors from a */
/*     specified inertial reference frame to body equator and */
/*     prime meridian coordinates.  TIPBOD accepts an input argument */
/*     REF that allows the caller to specify an inertial reference */
/*     frame. */

/*     The transformation represented by BODMAT's output argument TIPM */
/*     is defined as follows: */

/*        TIPM = [W] [DELTA] [PHI] */
/*                 3        1     3 */

/*     If there exists high-precision binary PCK kernel information */
/*     for the body at the requested time, these angles, W, DELTA */
/*     and PHI are computed directly from that file.  The most */
/*     recently loaded binary PCK file has first priority followed */
/*     by previously loaded binary PCK files in backward time order. */
/*     If no binary PCK file has been loaded, the text P_constants */
/*     kernel file is used. */

/*     If there is only text PCK kernel information, it is */
/*     expressed in terms of RA, DEC and W (same W as above), where */

/*        RA    = PHI - HALFPI() */
/*        DEC   = HALFPI() - DELTA */

/*     RA, DEC, and W are defined as follows in the text PCK file: */

/*           RA  = RA0  + RA1*T  + RA2*T*T   + a  sin theta */
/*                                              i          i */

/*           DEC = DEC0 + DEC1*T + DEC2*T*T  + d  cos theta */
/*                                              i          i */

/*           W   = W0   + W1*d   + W2*d*d    + w  sin theta */
/*                                              i          i */

/*     where: */

/*           d = days past J2000. */

/*           T = Julian centuries past J2000. */

/*           a , d , and w  arrays apply to satellites only. */
/*            i   i       i */

/*           theta  = THETA0 * THETA1*T are specific to each planet. */
/*                i */

/*     These angles -- typically nodal rates -- vary in number and */
/*     definition from one planetary system to the next. */

/* $ Examples */

/*     In the following code fragment, BODMAT is used to rotate */
/*     the position vector (POS) from a target body (BODY) to a */
/*     spacecraft from inertial coordinates to body-fixed coordinates */
/*     at a specific epoch (ET), in order to compute the planetocentric */
/*     longitude (PCLONG) of the spacecraft. */

/*        CALL BODMAT ( BODY, ET, TIPM ) */
/*        CALL MXV    ( TIPM, POS, POS ) */
/*        CALL RECLAT ( POS, RADIUS, PCLONG, LAT ) */

/*     To compute the equivalent planetographic longitude (PGLONG), */
/*     it is necessary to know the direction of rotation of the target */
/*     body, as shown below. */

/*        CALL BODVCD ( BODY, 'PM', 3, DIM, VALUES ) */

/*        IF ( VALUES(2) .GT. 0.D0 ) THEN */
/*           PGLONG = PCLONG */
/*        ELSE */
/*           PGLONG = TWOPI() - PCLONG */
/*        END IF */

/*     Note that the items necessary to compute the transformation */
/*     TIPM must have been loaded into the kernel pool (by one or more */
/*     previous calls to FURNSH). */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     1)  Refer to the NAIF_IDS required reading file for a complete */
/*         list of the NAIF integer ID codes for bodies. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     W.L. Taber      (JPL) */
/*     I.M. Underwood  (JPL) */
/*     K.S. Zukor      (JPL) */

/* $ Version */

/* -    SPICELIB Version 4.1.1, 01-FEB-2008 (NJB) */

/*        The routine was updated to improve the error messages created */
/*        when required PCK data are not found. Now in most cases the */
/*        messages are created locally rather than by the kernel pool */
/*        access routines. In particular missing binary PCK data will */
/*        be indicated with a reasonable error message. */

/* -    SPICELIB Version 4.1.0, 25-AUG-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in MXM call. */

/*         Calls to ZZBODVCD have been replaced with calls to */
/*         BODVCD. */

/* -     SPICELIB Version 4.0.0, 12-FEB-2004 (NJB) */

/*         Code has been updated to support satellite ID codes in the */
/*         range 10000 to 99999 and to allow nutation precession angles */
/*         to be associated with any object. */

/*         Implementation changes were made to improve robustness */
/*         of the code. */

/* -     SPICELIB Version 3.2.0, 22-MAR-1995 (KSZ) */

/*        Gets TSIPM matrix from PCKMAT (instead of Euler angles */
/*        from PCKEUL.) */

/* -     SPICELIB Version 3.0.0, 10-MAR-1994 (KSZ) */

/*        Ability to get Euler angles from binary PCK file added. */
/*        This uses the new routine PCKEUL. */

/* -     SPICELIB Version 2.0.1, 10-MAR-1992 (WLT) */

/*         Comment section for permuted index source lines was added */
/*         following the header. */

/* -     SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) */

/*         Updated to handle P_constants referenced to different epochs */
/*         and inertial reference frames. */

/*         The header was updated to specify that the inertial reference */
/*         frame used by BODMAT is restricted to be J2000. */

/* -    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     fetch transformation matrix for a body */
/*     transformation from j2000 position to bodyfixed */
/*     transformation from j2000 to bodyfixed coordinates */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 4.1.0, 25-AUG-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in MXM call. */

/*         Calls to ZZBODVCD have been replaced with calls to */
/*         BODVCD. */

/* -     SPICELIB Version 4.0.0, 12-FEB-2004 (NJB) */

/*         Code has been updated to support satellite ID codes in the */
/*         range 10000 to 99999 and to allow nutation precession angles */
/*         to be associated with any object. */

/*         Calls to deprecated kernel pool access routine RTPOOL */
/*         were replaced by calls to GDPOOL. */

/*         Calls to BODVAR have been replaced with calls to */
/*         ZZBODVCD. */

/* -     SPICELIB Version 3.2.0, 22-MAR-1995 (KSZ) */

/*        BODMAT now get the TSIPM matrix from PCKMAT, and */
/*        unpacks TIPM from it.  Also the calculated but unused */
/*        variable LAMBDA was removed. */

/* -     SPICELIB Version 3.0.0, 10-MAR-1994 (KSZ) */

/*        BODMAT now uses new software to check for the */
/*        existence of binary PCK files, search the for */
/*        data corresponding to the requested body and time, */
/*        and return the appropriate Euler angles, using the */
/*        new routine PCKEUL.  Otherwise the code calculates */
/*        the Euler angles from the P_constants kernel file. */

/* -     SPICELIB Version 2.0.0, 04-SEP-1991 (NJB) */

/*         Updated to handle P_constants referenced to different epochs */
/*         and inertial reference frames. */

/*         The header was updated to specify that the inertial reference */
/*         frame used by BODMAT is restricted to be J2000. */

/*         BODMAT now checks the kernel pool for presence of the */
/*         variables */

/*            BODY#_CONSTANTS_REF_FRAME */

/*         and */

/*            BODY#_CONSTANTS_JED_EPOCH */

/*         where # is the NAIF integer code of the barycenter of a */
/*         planetary system or of a body other than a planet or */
/*         satellite.  If either or both of these variables are */
/*         present, the P_constants for BODY are presumed to be */
/*         referenced to the specified inertial frame or epoch. */
/*         If the epoch of the constants is not J2000, the input */
/*         time ET is converted to seconds past the reference epoch. */
/*         If the frame of the constants is not J2000, the rotation from */
/*         the P_constants' frame to body-fixed coordinates is */
/*         transformed to the rotation from J2000 coordinates to */
/*         body-fixed coordinates. */

/*         For efficiency reasons, this routine now duplicates much */
/*         of the code of BODEUL so that it doesn't have to call BODEUL. */
/*         In some cases, BODEUL must covert Euler angles to a matrix, */
/*         rotate the matrix, and convert the result back to Euler */
/*         angles.  If this routine called BODEUL, then in such cases */
/*         this routine would convert the transformed angles back to */
/*         a matrix.  That would be a bit much.... */


/* -    Beta Version 1.1.0, 16-FEB-1989 (IMU) (NJB) */

/*        Examples section completed.  Declaration of unused variable */
/*        FOUND removed. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Standard SPICE Error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("BODMAT", (ftnlen)6);
    }

/*     Get the code for the J2000 frame, if we don't have it yet. */

    if (first) {
	irfnum_("J2000", &j2code, (ftnlen)5);
	first = FALSE_;
    }

/*     Get Euler angles from high precision data file. */

    pckmat_(body, et, &ref, tsipm, &found);
    if (found) {
	for (i__ = 1; i__ <= 3; ++i__) {
	    for (j = 1; j <= 3; ++j) {
		tipm[(i__1 = i__ + j * 3 - 4) < 9 && 0 <= i__1 ? i__1 : 
			s_rnge("tipm", i__1, "bodmat_", (ftnlen)485)] = tsipm[
			(i__2 = i__ + j * 6 - 7) < 36 && 0 <= i__2 ? i__2 : 
			s_rnge("tsipm", i__2, "bodmat_", (ftnlen)485)];
	    }
	}
    } else {

/*        The data for the frame of interest are not available in a */
/*        loaded binary PCK file. This is not an error: the data may be */
/*        present in the kernel pool. */

/*        Conduct a non-error-signaling check for the presence of a */
/*        kernel variable that is required to implement an IAU style */
/*        body-fixed reference frame. If the data aren't available, we */
/*        don't want BODVCD to signal a SPICE(KERNELVARNOTFOUND) error; */
/*        we want to issue the error signal locally, with a better error */
/*        message. */

	s_copy(item, "BODY#_PM", (ftnlen)32, (ftnlen)8);
	repmi_(item, "#", body, item, (ftnlen)32, (ftnlen)1, (ftnlen)32);
	dtpool_(item, &found, &nw, dtype, (ftnlen)32, (ftnlen)1);
	if (! found) {

/*           Now we do have an error. */

/*           We don't have the data we'll need to produced the requested */
/*           state transformation matrix. In order to create an error */
/*           message understandable to the user, find, if possible, the */
/*           name of the reference frame associated with the input body. */
/*           Note that the body is really identified by a PCK frame class */
/*           ID code, though most of the documentation just calls it a */
/*           body ID code. */

	    ccifrm_(&c__2, body, &frcode, fixfrm, &cent, &found, (ftnlen)32);
	    etcal_(et, timstr, (ftnlen)35);
	    s_copy(errmsg, "PCK data required to compute the orientation of "
		    "the # # for epoch # TDB were not found. If these data we"
		    "re to be provided by a binary PCK file, then it is possi"
		    "ble that the PCK file does not have coverage for the spe"
		    "cified body-fixed frame at the time of interest. If the "
		    "data were to be provided by a text PCK file, then possib"
		    "ly the file does not contain data for the specified body"
		    "-fixed frame. In either case it is possible that a requi"
		    "red PCK file was not loaded at all.", (ftnlen)1840, (
		    ftnlen)475);

/*           Fill in the variable data in the error message. */

	    if (found) {

/*              The frame system knows the name of the body-fixed frame. */

		setmsg_(errmsg, (ftnlen)1840);
		errch_("#", "body-fixed frame", (ftnlen)1, (ftnlen)16);
		errch_("#", fixfrm, (ftnlen)1, (ftnlen)32);
		errch_("#", timstr, (ftnlen)1, (ftnlen)35);
	    } else {

/*              The frame system doesn't know the name of the */
/*              body-fixed frame, most likely due to a missing */
/*              frame kernel. */

		suffix_("#", &c__1, errmsg, (ftnlen)1, (ftnlen)1840);
		setmsg_(errmsg, (ftnlen)1840);
		errch_("#", "body-fixed frame associated with the ID code", (
			ftnlen)1, (ftnlen)44);
		errint_("#", body, (ftnlen)1);
		errch_("#", timstr, (ftnlen)1, (ftnlen)35);
		errch_("#", "Also, a frame kernel defining the body-fixed fr"
			"ame associated with body # may need to be loaded.", (
			ftnlen)1, (ftnlen)96);
		errint_("#", body, (ftnlen)1);
	    }
	    sigerr_("SPICE(FRAMEDATANOTFOUND)", (ftnlen)24);
	    chkout_("BODMAT", (ftnlen)6);
	    return 0;
	}

/*        Find the body code used to label the reference frame and epoch */
/*        specifiers for the orientation constants for BODY. */

/*        For planetary systems, the reference frame and epoch for the */
/*        orientation constants is associated with the system */
/*        barycenter, not with individual bodies in the system.  For any */
/*        other bodies, (the Sun or asteroids, for example) the body's */
/*        own code is used as the label. */

	refid = zzbodbry_(body);

/*        Look up the epoch of the constants.  The epoch is specified */
/*        as a Julian ephemeris date.  The epoch defaults to J2000. */

	s_copy(item, "BODY#_CONSTANTS_JED_EPOCH", (ftnlen)32, (ftnlen)25);
	repmi_(item, "#", &refid, item, (ftnlen)32, (ftnlen)1, (ftnlen)32);
	gdpool_(item, &c__1, &c__1, &dim, &conepc, &found, (ftnlen)32);
	if (found) {

/*           The reference epoch is returned as a JED.  Convert to */
/*           ephemeris seconds past J2000.  Then convert the input ET to */
/*           seconds past the reference epoch. */

	    conepc = spd_() * (conepc - j2000_());
	    epoch = *et - conepc;
	} else {
	    epoch = *et;
	}

/*        Look up the reference frame of the constants.  The reference */
/*        frame is specified by a code recognized by CHGIRF.  The */
/*        default frame is J2000, symbolized by the code J2CODE. */

	s_copy(item, "BODY#_CONSTANTS_REF_FRAME", (ftnlen)32, (ftnlen)25);
	repmi_(item, "#", &refid, item, (ftnlen)32, (ftnlen)1, (ftnlen)32);
	gdpool_(item, &c__1, &c__1, &dim, &conref, &found, (ftnlen)32);
	if (found) {
	    ref = i_dnnt(&conref);
	} else {
	    ref = j2code;
	}

/*        Whatever the body, it has quadratic time polynomials for */
/*        the RA and Dec of the pole, and for the rotation of the */
/*        Prime Meridian. */

	s_copy(item, "POLE_RA", (ftnlen)32, (ftnlen)7);
	cleard_(&c__3, rcoef);
	bodvcd_(body, item, &c__3, &na, rcoef, (ftnlen)32);
	s_copy(item, "POLE_DEC", (ftnlen)32, (ftnlen)8);
	cleard_(&c__3, dcoef);
	bodvcd_(body, item, &c__3, &nd, dcoef, (ftnlen)32);
	s_copy(item, "PM", (ftnlen)32, (ftnlen)2);
	cleard_(&c__3, wcoef);
	bodvcd_(body, item, &c__3, &nw, wcoef, (ftnlen)32);

/*        There may be additional nutation and libration (THETA) terms. */

	ntheta = 0;
	na = 0;
	nd = 0;
	nw = 0;
	s_copy(item, "NUT_PREC_ANGLES", (ftnlen)32, (ftnlen)15);
	if (bodfnd_(&refid, item, (ftnlen)32)) {
	    bodvcd_(&refid, item, &c__100, &ntheta, tcoef, (ftnlen)32);
	    ntheta /= 2;
	}
	s_copy(item, "NUT_PREC_RA", (ftnlen)32, (ftnlen)11);
	if (bodfnd_(body, item, (ftnlen)32)) {
	    bodvcd_(body, item, &c__100, &na, ac, (ftnlen)32);
	}
	s_copy(item, "NUT_PREC_DEC", (ftnlen)32, (ftnlen)12);
	if (bodfnd_(body, item, (ftnlen)32)) {
	    bodvcd_(body, item, &c__100, &nd, dc, (ftnlen)32);
	}
	s_copy(item, "NUT_PREC_PM", (ftnlen)32, (ftnlen)11);
	if (bodfnd_(body, item, (ftnlen)32)) {
	    bodvcd_(body, item, &c__100, &nw, wc, (ftnlen)32);
	}
/* Computing MAX */
	i__1 = max(na,nd);
	if (max(i__1,nw) > ntheta) {
	    setmsg_("Insufficient number of nutation/precession angles for b"
		    "ody * at time #.", (ftnlen)71);
	    errint_("*", body, (ftnlen)1);
	    errdp_("#", et, (ftnlen)1);
	    sigerr_("SPICE(KERNELVARNOTFOUND)", (ftnlen)24);
	    chkout_("BODMAT", (ftnlen)6);
	    return 0;
	}

/*        Evaluate the time polynomials at EPOCH. */

	d__ = epoch / spd_();
	t = d__ / 36525.;
	ra = rcoef[0] + t * (rcoef[1] + t * rcoef[2]);
	dec = dcoef[0] + t * (dcoef[1] + t * dcoef[2]);
	w = wcoef[0] + d__ * (wcoef[1] + d__ * wcoef[2]);

/*        Add nutation and libration as appropriate. */

	i__1 = ntheta;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    theta = (tcoef[(i__2 = (i__ << 1) - 2) < 200 && 0 <= i__2 ? i__2 :
		     s_rnge("tcoef", i__2, "bodmat_", (ftnlen)700)] + t * 
		    tcoef[(i__3 = (i__ << 1) - 1) < 200 && 0 <= i__3 ? i__3 : 
		    s_rnge("tcoef", i__3, "bodmat_", (ftnlen)700)]) * rpd_();
	    sinth[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge("sinth",
		     i__2, "bodmat_", (ftnlen)702)] = sin(theta);
	    costh[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : s_rnge("costh",
		     i__2, "bodmat_", (ftnlen)703)] = cos(theta);
	}
	ra += vdotg_(ac, sinth, &na);
	dec += vdotg_(dc, costh, &nd);
	w += vdotg_(wc, sinth, &nw);

/*        Convert from degrees to radians and mod by two pi. */

	ra *= rpd_();
	dec *= rpd_();
	w *= rpd_();
	d__1 = twopi_();
	ra = d_mod(&ra, &d__1);
	d__1 = twopi_();
	dec = d_mod(&dec, &d__1);
	d__1 = twopi_();
	w = d_mod(&w, &d__1);

/*        Convert to Euler angles. */

	phi = ra + halfpi_();
	delta = halfpi_() - dec;

/*        Produce the rotation matrix defined by the Euler angles. */

	eul2m_(&w, &delta, &phi, &c__3, &c__1, &c__3, tipm);
    }

/*     Convert TIPM to the J2000-to-bodyfixed rotation, if is is not */
/*     already referenced to J2000. */

    if (ref != j2code) {

/*        Find the transformation from the J2000 frame to the frame */
/*        designated by REF.  Form the transformation from `REF' */
/*        coordinates to body-fixed coordinates.  Compose the */
/*        transformations to obtain the J2000-to-body-fixed */
/*        transformation. */

	irfrot_(&j2code, &ref, j2ref);
	mxm_(tipm, j2ref, tmpmat);
	moved_(tmpmat, &c__9, tipm);
    }

/*     TIPM now gives the transformation from J2000 to */
/*     body-fixed coordinates at epoch ET seconds past J2000, */
/*     regardless of the epoch and frame of the orientation constants */
/*     for the specified body. */

    chkout_("BODMAT", (ftnlen)6);
    return 0;
} /* bodmat_ */

