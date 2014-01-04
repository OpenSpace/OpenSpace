/* zzfdat.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__121 = 121;

/* $Procedure      ZZFDAT ( Initialize frame names and idcodes ) */
/* Subroutine */ int zzfdat_(integer *ncount, char *name__, integer *idcode, 
	integer *center, integer *type__, integer *typid, integer *norder, 
	integer *corder, integer *centrd, ftnlen name_len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), irfnam_(integer *, 
	    char *, ftnlen), orderc_(char *, integer *, integer *, ftnlen), 
	    orderi_(integer *, integer *, integer *), sigerr_(char *, ftnlen),
	     chkout_(char *, ftnlen), setmsg_(char *, ftnlen);

/* $ Abstract */


/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     This routine initializes the table of frame names and their */
/*     ID codes. */

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

/*     PRIVATE */

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

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 3.0.0, 28-MAY-2004 (NJB) */

/*       The parameter DYN was added to support the dynamic frame class. */

/* -    SPICELIB Version 2.0.0, 12-DEC-1996 (WLT) */

/*        Various unused frames types were removed and the */
/*        frame time TK was added. */

/* -    SPICELIB Version 1.0.0, 10-DEC-1995 (WLT) */

/* -& */
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
/*     NCOUNT     I   Input checking variable. */
/*     NAME       O   array containing the names of all known frames */
/*     IDCODE     O   array containing the ID codes of all known frames */
/*     CENTER     O   array containing the centers of the known frames */
/*     TYPE       O   array containing the types of the known frames */
/*     TYPID      O   array containing the subtype id */
/*     NORDER     O   an order vector for NAME */
/*     CORDER     O   an order vector for IDCODE */

/* $ Detailed_Input */

/*     NCOUNT      is the number of names that the calling routine */
/*                 expects to receive.  It should have the value of */
/*                 NNAMES which is given below for NNAMES.  If this */
/*                 is not the case then the error 'SPICE(BUG)' is */
/*                 signaled. */

/*                 If everything has been properly called, compiled */
/*                 and linked this error should never be signaled. */
/*                 If it is signaled, it indicates that either a calling */
/*                 sequence, or version mismatch has occurred. */

/* $ Detailed_Output */

/*     All of the arrays described below should be declared with the */
/*     same dimensions---NCOUNT. */

/*     NAME        is an array of the official SPICE names for the */
/*                 recognized frames (both inertial and non-inertial) */

/*     IDCODE      is an array parallel to NAME of SPICE ID codes for */
/*                 the various frames. */

/*     CENTER      is an array parallel to NAME of body ID codes for */
/*                 the centers of frames. */

/*     TYPE        is an array parallel to NAME of inertial frame types */
/*                 for the various frames.  These include INERTL, PCK, */
/*                 CK, etc. */

/*     TYPID       is an array parallel to NAME of the ID code for the */
/*                 frame within the TYPE of the frame.  Once the class */
/*                 of the frame has been identified by TYPE, TYPID is */
/*                 used to access the information specific about this */
/*                 frame. */

/*     NORDER      is an order vector for the array NAME. */
/*                 NAME(NORDER(I)) is the I'th name in the array NAME */
/*                 when ordered by the FORTRAN collating sequence. */

/*     CORDER      is an order vector for the array IDCODE.  The */
/*                 value IDCODE(CORDER(I)) is the I'th IDCODE when */
/*                 ordered from smallest to largest. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine establishes the default SPICE */
/*     reference frames and their id-codes.  In addition */
/*     it returns order vectors for both the names and the ID codes. */

/*     This is a private routine intended solely as a support routine */
/*     for the SPICE routine FRCODE. */

/* $ Examples */

/*     This routine should typically be called as part of an */
/*     initialization portion of FRCODE */

/*        LOGICAL               FIRST */
/*        SAVE                  FIRST */

/*        DATA                  FIRST / .TRUE. / */


/*        IF ( FIRST ) THEN */

/*           FIRST = .FALSE. */
/*           CALL ZZFDAT ( NCOUNT, NAME, IDCODE, NORDER, CORDER ) */

/*        END IF */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */
/*     W.L. Taber      (JPL) */
/*     F.S. Turner     (JPL) */
/*     E.D. Wright     (JPL) */

/* $ Version */

/* -    SPICELIB Version 4.2.0, 11-MAY-2010 (BVS) */

/*        Added the following PCK frames: */

/*           IAU_BORRELLY */
/*           IAU_TEMPEL_1 */
/*           IAU_VESTA */
/*           IAU_ITOKAWA */

/* -    SPICELIB Version 4.1.0, 12-DEC-2002 (BVS) */

/*        Added PCK frames for new Jovian satellites: */

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

/* -    SPICELIB Version 4.0.1, 18-OCT-2002 (EDW) */

/*        Corrected the erroneous frame values for IAU_PAN. */
/*        Minor edits to the header. */

/* -    SPICELIB Version 4.0.0, 02-AUG-2002 (FST) */

/*        The frames IAU_PAN, IAU_GASPRA, IAU_IDA, and IAU_EROS */
/*        were added to the list of recognized frames. */

/* -    SPICELIB Version 3.1.1, 20-APR-1999 (WLT) */

/*        Changed the variable name TYPEID to TYPID in the calling */
/*        sequence to avoid having to take special measures in the f2c */
/*        conversion process. */

/* -    SPICELIB Version 3.1.0, 11-SEP-1997 (WLT) */

/*        The error condition check early in the routine */
/*        did not use the exception handling subsystem correctly. */
/*        This has been fixed. */

/* -    SPICELIB Version 3.0.0, 02-JUN-1997 (WLT) */

/*        The calling sequence changed.  ZZFDAT now also returns */
/*        an order vector for the CENTERs of the frames. */

/* -    SPICELIB Version 2.0.0, 03-APR-1997 (WLT) */

/*        The frames ITRF93 and EARTH_FIXED were added to the */
/*        list of recognized frames. */

/* -    SPICELIB Version 1.1.0, 14-OCT-1996 (WLT) */

/*        Changed declarations so that the variables NINERT and */
/*        NNINRT are included instead of being declared locally. */

/* -    SPICELIB Version 1.0.0, 19-SEP-1995 (WLT) */


/* -& */

/*     To add to the list of recognized frames, */

/*     1. Determine whether or not the frame is inertial. */

/*       Inertial Case. */

/*        A. Be sure that the routine CHGIRF has been modified to */
/*           reflect the new frame and set NINERT (above) equal to */
/*           the number of recognized inertial frames give by CHGIRF. */

/*        Non Inertial Case. */

/*        A. Locate the last non-inertial frame in the lengthy list */
/*           below. */

/*        B. Add the frame name to the array NAME.  Add the IDCODE */
/*           to the array IDCODE.  (Unless there is a compelling reason */
/*           to do otherwise this should just be the next integer in */
/*           the sequence of ID codes.  The mixture of old and new code */
/*           should look something like this: */

/*              Last bit of old assignments */

/*                 NAME   ( NINERT + NON ) = last name in the old routine */
/*                 IDCODE ( NINERT + NON ) = 10000 + NON */

/*              Your new assignment */

/*                 NAME   ( NINERT + NEXT ) = your name */
/*                 IDCODE ( NINERT + NEXT ) = 10000 + NEXT */

/*           where */

/*              NON  = the value of the parameter above */
/*              NEXT = NON + 1 */

/*        C. Modify the value of the parameter NON above to reflect the */
/*           new number of non-inertial frames. */

/*     2. Update the version and date routine. */

/*     3. Update the routines that call this routine so that they */
/*        will be expecting the correct number of names and ID codes */
/*        to be returned. */


/*     Perform the consistency check first. */

    if (*ncount != 121) {
	chkin_("ZZFDAT", (ftnlen)6);
	setmsg_("There is an inconsistency between the version of the routin"
		"e calling ZZFDAT and the current version of ZZFDAT. Check to"
		" make sure that you have the most current versions of ZZFDAT"
		" and the routines that make use of it.", (ftnlen)217);
	sigerr_("SPICE(VERSIONMISMATCH)", (ftnlen)22);
	chkout_("ZZFDAT", (ftnlen)6);
	return 0;
    }

/*     Inertial Frames Section */

/*     Fetch the names of the inertial frames from CHGIRF */

    for (i__ = 1; i__ <= 21; ++i__) {
	idcode[i__ - 1] = i__;
	center[i__ - 1] = 0;
	type__[i__ - 1] = 1;
	typid[i__ - 1] = i__;
	irfnam_(&i__, name__ + (i__ - 1) * name_len, name_len);
    }

/*     Non-Inertial Frames Section. */

/*     Note that the loop below is appropriate only for the */
/*     first 79 non-inertial frames because by construction they */
/*     are all PCK based.  As new frames are added you should */
/*     use the template near the end of this routine to add */
/*     the new information. */

    for (i__ = 22; i__ <= 100; ++i__) {
	type__[i__ - 1] = 2;
    }
    s_copy(name__ + name_len * 21, "IAU_MERCURY_BARYCENTER", name_len, (
	    ftnlen)22);
    idcode[21] = 10001;
    center[21] = 1;
    typid[21] = 1;
    s_copy(name__ + name_len * 22, "IAU_VENUS_BARYCENTER", name_len, (ftnlen)
	    20);
    idcode[22] = 10002;
    center[22] = 2;
    typid[22] = 2;
    s_copy(name__ + name_len * 23, "IAU_EARTH_BARYCENTER", name_len, (ftnlen)
	    20);
    idcode[23] = 10003;
    center[23] = 3;
    typid[23] = 3;
    s_copy(name__ + name_len * 24, "IAU_MARS_BARYCENTER", name_len, (ftnlen)
	    19);
    idcode[24] = 10004;
    center[24] = 4;
    typid[24] = 4;
    s_copy(name__ + name_len * 25, "IAU_JUPITER_BARYCENTER", name_len, (
	    ftnlen)22);
    idcode[25] = 10005;
    center[25] = 5;
    typid[25] = 5;
    s_copy(name__ + name_len * 26, "IAU_SATURN_BARYCENTER", name_len, (ftnlen)
	    21);
    idcode[26] = 10006;
    center[26] = 6;
    typid[26] = 6;
    s_copy(name__ + name_len * 27, "IAU_URANUS_BARYCENTER", name_len, (ftnlen)
	    21);
    idcode[27] = 10007;
    center[27] = 7;
    typid[27] = 7;
    s_copy(name__ + name_len * 28, "IAU_NEPTUNE_BARYCENTER", name_len, (
	    ftnlen)22);
    idcode[28] = 10008;
    center[28] = 8;
    typid[28] = 8;
    s_copy(name__ + name_len * 29, "IAU_PLUTO_BARYCENTER", name_len, (ftnlen)
	    20);
    idcode[29] = 10009;
    center[29] = 9;
    typid[29] = 9;
    s_copy(name__ + name_len * 30, "IAU_SUN", name_len, (ftnlen)7);
    idcode[30] = 10010;
    center[30] = 10;
    typid[30] = 10;
    s_copy(name__ + name_len * 31, "IAU_MERCURY", name_len, (ftnlen)11);
    idcode[31] = 10011;
    center[31] = 199;
    typid[31] = 199;
    s_copy(name__ + (name_len << 5), "IAU_VENUS", name_len, (ftnlen)9);
    idcode[32] = 10012;
    center[32] = 299;
    typid[32] = 299;
    s_copy(name__ + name_len * 33, "IAU_EARTH", name_len, (ftnlen)9);
    idcode[33] = 10013;
    center[33] = 399;
    typid[33] = 399;
    s_copy(name__ + name_len * 34, "IAU_MARS", name_len, (ftnlen)8);
    idcode[34] = 10014;
    center[34] = 499;
    typid[34] = 499;
    s_copy(name__ + name_len * 35, "IAU_JUPITER", name_len, (ftnlen)11);
    idcode[35] = 10015;
    center[35] = 599;
    typid[35] = 599;
    s_copy(name__ + name_len * 36, "IAU_SATURN", name_len, (ftnlen)10);
    idcode[36] = 10016;
    center[36] = 699;
    typid[36] = 699;
    s_copy(name__ + name_len * 37, "IAU_URANUS", name_len, (ftnlen)10);
    idcode[37] = 10017;
    center[37] = 799;
    typid[37] = 799;
    s_copy(name__ + name_len * 38, "IAU_NEPTUNE", name_len, (ftnlen)11);
    idcode[38] = 10018;
    center[38] = 899;
    typid[38] = 899;
    s_copy(name__ + name_len * 39, "IAU_PLUTO", name_len, (ftnlen)9);
    idcode[39] = 10019;
    center[39] = 999;
    typid[39] = 999;
    s_copy(name__ + name_len * 40, "IAU_MOON", name_len, (ftnlen)8);
    idcode[40] = 10020;
    center[40] = 301;
    typid[40] = 301;
    s_copy(name__ + name_len * 41, "IAU_PHOBOS", name_len, (ftnlen)10);
    idcode[41] = 10021;
    center[41] = 401;
    typid[41] = 401;
    s_copy(name__ + name_len * 42, "IAU_DEIMOS", name_len, (ftnlen)10);
    idcode[42] = 10022;
    center[42] = 402;
    typid[42] = 402;
    s_copy(name__ + name_len * 43, "IAU_IO", name_len, (ftnlen)6);
    idcode[43] = 10023;
    center[43] = 501;
    typid[43] = 501;
    s_copy(name__ + name_len * 44, "IAU_EUROPA", name_len, (ftnlen)10);
    idcode[44] = 10024;
    center[44] = 502;
    typid[44] = 502;
    s_copy(name__ + name_len * 45, "IAU_GANYMEDE", name_len, (ftnlen)12);
    idcode[45] = 10025;
    center[45] = 503;
    typid[45] = 503;
    s_copy(name__ + name_len * 46, "IAU_CALLISTO", name_len, (ftnlen)12);
    idcode[46] = 10026;
    center[46] = 504;
    typid[46] = 504;
    s_copy(name__ + name_len * 47, "IAU_AMALTHEA", name_len, (ftnlen)12);
    idcode[47] = 10027;
    center[47] = 505;
    typid[47] = 505;
    s_copy(name__ + name_len * 48, "IAU_HIMALIA", name_len, (ftnlen)11);
    idcode[48] = 10028;
    center[48] = 506;
    typid[48] = 506;
    s_copy(name__ + name_len * 49, "IAU_ELARA", name_len, (ftnlen)9);
    idcode[49] = 10029;
    center[49] = 507;
    typid[49] = 507;
    s_copy(name__ + name_len * 50, "IAU_PASIPHAE", name_len, (ftnlen)12);
    idcode[50] = 10030;
    center[50] = 508;
    typid[50] = 508;
    s_copy(name__ + name_len * 51, "IAU_SINOPE", name_len, (ftnlen)10);
    idcode[51] = 10031;
    center[51] = 509;
    typid[51] = 509;
    s_copy(name__ + name_len * 52, "IAU_LYSITHEA", name_len, (ftnlen)12);
    idcode[52] = 10032;
    center[52] = 510;
    typid[52] = 510;
    s_copy(name__ + name_len * 53, "IAU_CARME", name_len, (ftnlen)9);
    idcode[53] = 10033;
    center[53] = 511;
    typid[53] = 511;
    s_copy(name__ + name_len * 54, "IAU_ANANKE", name_len, (ftnlen)10);
    idcode[54] = 10034;
    center[54] = 512;
    typid[54] = 512;
    s_copy(name__ + name_len * 55, "IAU_LEDA", name_len, (ftnlen)8);
    idcode[55] = 10035;
    center[55] = 513;
    typid[55] = 513;
    s_copy(name__ + name_len * 56, "IAU_THEBE", name_len, (ftnlen)9);
    idcode[56] = 10036;
    center[56] = 514;
    typid[56] = 514;
    s_copy(name__ + name_len * 57, "IAU_ADRASTEA", name_len, (ftnlen)12);
    idcode[57] = 10037;
    center[57] = 515;
    typid[57] = 515;
    s_copy(name__ + name_len * 58, "IAU_METIS", name_len, (ftnlen)9);
    idcode[58] = 10038;
    center[58] = 516;
    typid[58] = 516;
    s_copy(name__ + name_len * 59, "IAU_MIMAS", name_len, (ftnlen)9);
    idcode[59] = 10039;
    center[59] = 601;
    typid[59] = 601;
    s_copy(name__ + name_len * 60, "IAU_ENCELADUS", name_len, (ftnlen)13);
    idcode[60] = 10040;
    center[60] = 602;
    typid[60] = 602;
    s_copy(name__ + name_len * 61, "IAU_TETHYS", name_len, (ftnlen)10);
    idcode[61] = 10041;
    center[61] = 603;
    typid[61] = 603;
    s_copy(name__ + name_len * 62, "IAU_DIONE", name_len, (ftnlen)9);
    idcode[62] = 10042;
    center[62] = 604;
    typid[62] = 604;
    s_copy(name__ + name_len * 63, "IAU_RHEA", name_len, (ftnlen)8);
    idcode[63] = 10043;
    center[63] = 605;
    typid[63] = 605;
    s_copy(name__ + (name_len << 6), "IAU_TITAN", name_len, (ftnlen)9);
    idcode[64] = 10044;
    center[64] = 606;
    typid[64] = 606;
    s_copy(name__ + name_len * 65, "IAU_HYPERION", name_len, (ftnlen)12);
    idcode[65] = 10045;
    center[65] = 607;
    typid[65] = 607;
    s_copy(name__ + name_len * 66, "IAU_IAPETUS", name_len, (ftnlen)11);
    idcode[66] = 10046;
    center[66] = 608;
    typid[66] = 608;
    s_copy(name__ + name_len * 67, "IAU_PHOEBE", name_len, (ftnlen)10);
    idcode[67] = 10047;
    center[67] = 609;
    typid[67] = 609;
    s_copy(name__ + name_len * 68, "IAU_JANUS", name_len, (ftnlen)9);
    idcode[68] = 10048;
    center[68] = 610;
    typid[68] = 610;
    s_copy(name__ + name_len * 69, "IAU_EPIMETHEUS", name_len, (ftnlen)14);
    idcode[69] = 10049;
    center[69] = 611;
    typid[69] = 611;
    s_copy(name__ + name_len * 70, "IAU_HELENE", name_len, (ftnlen)10);
    idcode[70] = 10050;
    center[70] = 612;
    typid[70] = 612;
    s_copy(name__ + name_len * 71, "IAU_TELESTO", name_len, (ftnlen)11);
    idcode[71] = 10051;
    center[71] = 613;
    typid[71] = 613;
    s_copy(name__ + name_len * 72, "IAU_CALYPSO", name_len, (ftnlen)11);
    idcode[72] = 10052;
    center[72] = 614;
    typid[72] = 614;
    s_copy(name__ + name_len * 73, "IAU_ATLAS", name_len, (ftnlen)9);
    idcode[73] = 10053;
    center[73] = 615;
    typid[73] = 615;
    s_copy(name__ + name_len * 74, "IAU_PROMETHEUS", name_len, (ftnlen)14);
    idcode[74] = 10054;
    center[74] = 616;
    typid[74] = 616;
    s_copy(name__ + name_len * 75, "IAU_PANDORA", name_len, (ftnlen)11);
    idcode[75] = 10055;
    center[75] = 617;
    typid[75] = 617;
    s_copy(name__ + name_len * 76, "IAU_ARIEL", name_len, (ftnlen)9);
    idcode[76] = 10056;
    center[76] = 701;
    typid[76] = 701;
    s_copy(name__ + name_len * 77, "IAU_UMBRIEL", name_len, (ftnlen)11);
    idcode[77] = 10057;
    center[77] = 702;
    typid[77] = 702;
    s_copy(name__ + name_len * 78, "IAU_TITANIA", name_len, (ftnlen)11);
    idcode[78] = 10058;
    center[78] = 703;
    typid[78] = 703;
    s_copy(name__ + name_len * 79, "IAU_OBERON", name_len, (ftnlen)10);
    idcode[79] = 10059;
    center[79] = 704;
    typid[79] = 704;
    s_copy(name__ + name_len * 80, "IAU_MIRANDA", name_len, (ftnlen)11);
    idcode[80] = 10060;
    center[80] = 705;
    typid[80] = 705;
    s_copy(name__ + name_len * 81, "IAU_CORDELIA", name_len, (ftnlen)12);
    idcode[81] = 10061;
    center[81] = 706;
    typid[81] = 706;
    s_copy(name__ + name_len * 82, "IAU_OPHELIA", name_len, (ftnlen)11);
    idcode[82] = 10062;
    center[82] = 707;
    typid[82] = 707;
    s_copy(name__ + name_len * 83, "IAU_BIANCA", name_len, (ftnlen)10);
    idcode[83] = 10063;
    center[83] = 708;
    typid[83] = 708;
    s_copy(name__ + name_len * 84, "IAU_CRESSIDA", name_len, (ftnlen)12);
    idcode[84] = 10064;
    center[84] = 709;
    typid[84] = 709;
    s_copy(name__ + name_len * 85, "IAU_DESDEMONA", name_len, (ftnlen)13);
    idcode[85] = 10065;
    center[85] = 710;
    typid[85] = 710;
    s_copy(name__ + name_len * 86, "IAU_JULIET", name_len, (ftnlen)10);
    idcode[86] = 10066;
    center[86] = 711;
    typid[86] = 711;
    s_copy(name__ + name_len * 87, "IAU_PORTIA", name_len, (ftnlen)10);
    idcode[87] = 10067;
    center[87] = 712;
    typid[87] = 712;
    s_copy(name__ + name_len * 88, "IAU_ROSALIND", name_len, (ftnlen)12);
    idcode[88] = 10068;
    center[88] = 713;
    typid[88] = 713;
    s_copy(name__ + name_len * 89, "IAU_BELINDA", name_len, (ftnlen)11);
    idcode[89] = 10069;
    center[89] = 714;
    typid[89] = 714;
    s_copy(name__ + name_len * 90, "IAU_PUCK", name_len, (ftnlen)8);
    idcode[90] = 10070;
    center[90] = 715;
    typid[90] = 715;
    s_copy(name__ + name_len * 91, "IAU_TRITON", name_len, (ftnlen)10);
    idcode[91] = 10071;
    center[91] = 801;
    typid[91] = 801;
    s_copy(name__ + name_len * 92, "IAU_NEREID", name_len, (ftnlen)10);
    idcode[92] = 10072;
    center[92] = 802;
    typid[92] = 802;
    s_copy(name__ + name_len * 93, "IAU_NAIAD", name_len, (ftnlen)9);
    idcode[93] = 10073;
    center[93] = 803;
    typid[93] = 803;
    s_copy(name__ + name_len * 94, "IAU_THALASSA", name_len, (ftnlen)12);
    idcode[94] = 10074;
    center[94] = 804;
    typid[94] = 804;
    s_copy(name__ + name_len * 95, "IAU_DESPINA", name_len, (ftnlen)11);
    idcode[95] = 10075;
    center[95] = 805;
    typid[95] = 805;
    s_copy(name__ + name_len * 96, "IAU_GALATEA", name_len, (ftnlen)11);
    idcode[96] = 10076;
    center[96] = 806;
    typid[96] = 806;
    s_copy(name__ + name_len * 97, "IAU_LARISSA", name_len, (ftnlen)11);
    idcode[97] = 10077;
    center[97] = 807;
    typid[97] = 807;
    s_copy(name__ + name_len * 98, "IAU_PROTEUS", name_len, (ftnlen)11);
    idcode[98] = 10078;
    center[98] = 808;
    typid[98] = 808;
    s_copy(name__ + name_len * 99, "IAU_CHARON", name_len, (ftnlen)10);
    idcode[99] = 10079;
    center[99] = 901;
    typid[99] = 901;

/*     This is for the first new PCK frame---the high precision earth */
/*     frame ITRF93. */

    s_copy(name__ + name_len * 100, "ITRF93", name_len, (ftnlen)6);
    idcode[100] = 13000;
    center[100] = 399;
    typid[100] = 3000;
    type__[100] = 2;

/*     This if for the alias frame EARTH BODYFIXED.  This is a TK */
/*     class frame.  To use it a FRAME kernel must be loaded via */
/*     FURNSH. */

    s_copy(name__ + name_len * 101, "EARTH_FIXED", name_len, (ftnlen)11);
    idcode[101] = 10081;
    center[101] = 399;
    typid[101] = 10081;
    type__[101] = 4;

/*     Frames introduced into the generic NAIF PCK */
/*     system as referenced from the 1997 IAU report. */

    s_copy(name__ + name_len * 102, "IAU_PAN", name_len, (ftnlen)7);
    idcode[102] = 10082;
    center[102] = 618;
    typid[102] = 618;
    type__[102] = 2;
    s_copy(name__ + name_len * 103, "IAU_GASPRA", name_len, (ftnlen)10);
    idcode[103] = 10083;
    center[103] = 9511010;
    typid[103] = 9511010;
    type__[103] = 2;
    s_copy(name__ + name_len * 104, "IAU_IDA", name_len, (ftnlen)7);
    idcode[104] = 10084;
    center[104] = 2431010;
    typid[104] = 2431010;
    type__[104] = 2;

/*     Frame referenced from the Eros orientation */
/*     model in the 2000 IAU report. */

    s_copy(name__ + name_len * 105, "IAU_EROS", name_len, (ftnlen)8);
    idcode[105] = 10085;
    center[105] = 2000433;
    typid[105] = 2000433;
    type__[105] = 2;

/*     Frames for Jovian satellites approved by IAU in late 2002. */

    s_copy(name__ + name_len * 106, "IAU_CALLIRRHOE", name_len, (ftnlen)14);
    idcode[106] = 10086;
    center[106] = 517;
    typid[106] = 517;
    type__[106] = 2;
    s_copy(name__ + name_len * 107, "IAU_THEMISTO", name_len, (ftnlen)12);
    idcode[107] = 10087;
    center[107] = 518;
    typid[107] = 518;
    type__[107] = 2;
    s_copy(name__ + name_len * 108, "IAU_MAGACLITE", name_len, (ftnlen)13);
    idcode[108] = 10088;
    center[108] = 519;
    typid[108] = 519;
    type__[108] = 2;
    s_copy(name__ + name_len * 109, "IAU_TAYGETE", name_len, (ftnlen)11);
    idcode[109] = 10089;
    center[109] = 520;
    typid[109] = 520;
    type__[109] = 2;
    s_copy(name__ + name_len * 110, "IAU_CHALDENE", name_len, (ftnlen)12);
    idcode[110] = 10090;
    center[110] = 521;
    typid[110] = 521;
    type__[110] = 2;
    s_copy(name__ + name_len * 111, "IAU_HARPALYKE", name_len, (ftnlen)13);
    idcode[111] = 10091;
    center[111] = 522;
    typid[111] = 522;
    type__[111] = 2;
    s_copy(name__ + name_len * 112, "IAU_KALYKE", name_len, (ftnlen)10);
    idcode[112] = 10092;
    center[112] = 523;
    typid[112] = 523;
    type__[112] = 2;
    s_copy(name__ + name_len * 113, "IAU_IOCASTE", name_len, (ftnlen)11);
    idcode[113] = 10093;
    center[113] = 524;
    typid[113] = 524;
    type__[113] = 2;
    s_copy(name__ + name_len * 114, "IAU_ERINOME", name_len, (ftnlen)11);
    idcode[114] = 10094;
    center[114] = 525;
    typid[114] = 525;
    type__[114] = 2;
    s_copy(name__ + name_len * 115, "IAU_ISONOE", name_len, (ftnlen)10);
    idcode[115] = 10095;
    center[115] = 526;
    typid[115] = 526;
    type__[115] = 2;
    s_copy(name__ + name_len * 116, "IAU_PRAXIDIKE", name_len, (ftnlen)13);
    idcode[116] = 10096;
    center[116] = 527;
    typid[116] = 527;
    type__[116] = 2;

/*     Frames for comets and asteroids, for which rotation constants */
/*     were added in 2006 IAU Report. */

    s_copy(name__ + name_len * 117, "IAU_BORRELLY", name_len, (ftnlen)12);
    idcode[117] = 10097;
    center[117] = 1000005;
    typid[117] = 1000005;
    type__[117] = 2;
    s_copy(name__ + name_len * 118, "IAU_TEMPEL_1", name_len, (ftnlen)12);
    idcode[118] = 10098;
    center[118] = 1000093;
    typid[118] = 1000093;
    type__[118] = 2;
    s_copy(name__ + name_len * 119, "IAU_VESTA", name_len, (ftnlen)9);
    idcode[119] = 10099;
    center[119] = 2000004;
    typid[119] = 2000004;
    type__[119] = 2;
    s_copy(name__ + name_len * 120, "IAU_ITOKAWA", name_len, (ftnlen)11);
    idcode[120] = 10100;
    center[120] = 2025143;
    typid[120] = 2025143;
    type__[120] = 2;

/*     Below is a template to use for adding another non-inertial */
/*     frame.  Copy it, fill in the new values and then leave */
/*     a new template for the next person who needs to modify this */
/*     routine. */

/*     NAME   ( NINERT + 101 ) =  name */
/*     IDCODE ( NINERT + 101 ) =  10101 */
/*     CENTER ( NINERT + 101 ) =  center */
/*     TYPID  ( NINERT + 101 ) =  type ID code */
/*     TYPE   ( NINERT + 101 ) =  type (INERTL, PCK, etc. ) */

    orderc_(name__, &c__121, norder, name_len);
    orderi_(idcode, &c__121, corder);
    orderi_(center, &c__121, centrd);
    return 0;
} /* zzfdat_ */

