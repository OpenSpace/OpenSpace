/* framex.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__121 = 121;
static integer c__0 = 0;
static integer c__1 = 1;
static integer c__8 = 8;
static integer c__100 = 100;

/* $Procedure FRAMEX (Frame Expert) */
/* Subroutine */ int framex_0_(int n__, char *cname, char *frname, integer *
	frcode, integer *cent, integer *class__, integer *clssid, logical *
	found, ftnlen cname_len, ftnlen frname_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static char name__[32*121], line[80*8];
    static integer item, type__[121];
    extern /* Subroutine */ int zzdynbid_(char *, integer *, char *, integer *
	    , ftnlen, ftnlen);
    static char look2[32];
    extern /* Subroutine */ int zzdynvai_(char *, integer *, char *, integer *
	    , integer *, integer *, ftnlen, ftnlen);
    static integer i__, n;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static char pname[32];
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen), 
	    errch_(char *, char *, ftnlen, ftnlen), repmc_(char *, char *, 
	    char *, char *, ftnlen, ftnlen, ftnlen, ftnlen), repmi_(char *, 
	    char *, integer *, char *, ftnlen, ftnlen, ftnlen);
    static logical gotit;
    static integer start;
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen), 
	    bodc2n_(integer *, char *, logical *, ftnlen), bodn2c_(char *, 
	    integer *, logical *, ftnlen);
    static integer id;
    extern logical failed_(void);
    static integer idcode[121];
    static char lcname[36];
    extern integer bschoc_(char *, integer *, char *, integer *, ftnlen, 
	    ftnlen);
    static char lcfram[32];
    extern integer bschoi_(integer *, integer *, integer *, integer *);
    static integer kvclid, corder[121], centrd[121], center[121];
    static char kvbuff[32*100];
    extern /* Subroutine */ int gcpool_(char *, integer *, integer *, integer 
	    *, char *, logical *, ftnlen, ftnlen);
    static integer norder[121];
    extern /* Subroutine */ int gipool_(char *, integer *, integer *, integer 
	    *, integer *, logical *, ftnlen), sigerr_(char *, ftnlen);
    static integer typeid[121], values[8];
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen), zzfdat_(integer *, char *, integer *, integer *, integer 
	    *, integer *, integer *, integer *, integer *, ftnlen);
    static char dattyp[1], lookup[32];
    static integer kvclss;
    extern /* Subroutine */ int prefix_(char *, integer *, char *, ftnlen, 
	    ftnlen), dtpool_(char *, logical *, integer *, char *, ftnlen, 
	    ftnlen), gnpool_(char *, integer *, integer *, integer *, char *, 
	    logical *, ftnlen, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int suffix_(char *, integer *, char *, ftnlen, 
	    ftnlen);
    static logical fnd;

/* $ Abstract */

/*     This is an umbrella routine for the entry points available */
/*     for manipulating different reference frames. It should not */
/*     be called directly. */

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
/* $ Brief_I/O */

/*     VARIABLE  I/O  ENTRY POINT */
/*     --------  ---  -------------------------------------------------- */
/*     CNAME      I   CNMFRM */
/*     FRNAME    I/O  NAMFRM, FRMNAM, CCIFRM */
/*     FRCODE    I/O  NAMFRM, FRMNAM, FRINFO, CIDFRM, CCIFRM */
/*     CENT      I/O  FRINFO, CIDFRM, CCIFRM */
/*     CLASS     I/O  FRINFO, CCIFRM */
/*     CLSSID    I/O  FRINFO, CCIFRM */
/*     FOUND      O   FRINFO */


/* $ Detailed_Input */

/*     See individual entry points for details concerning inputs. */

/* $ Detailed_Output */

/*     See individual entry points for details concerning inputs. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If this routine is called directly the error */
/*       'SPICE(BOGUSENTRY)' will be signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This is an umbrella routine that comprises the SPICE */
/*     interface to the reference frame transformation software. */

/*     There are 5 entry points. */

/*     NAMFRM  converts string to the id-codes used by low level */
/*             SPICE software */

/*     FRMNAM  converts frame id-codes to the more familiar names */
/*             used to describe various reference frames. */

/*     FRINFO  returns the center associated with a reference frame. */

/*     CIDFRM  given the id-code of an object, returns the bodyfixed */
/*             frame associated with it. */

/*     CNMFRM  given the name of an object, returns the bodyfixed */
/*             frame associated with it. */

/*     CCIFRM  given a frame's class and class ID, returns */
/*             the frame's ID code, name, and center. */

/* $ Examples */

/*     Suppose that you needed to transform between two reference */
/*     frames on the basis of their names and that you wanted to */
/*     correct for light time to the center of the second frame */
/*     as seen from an observer with idcode OBS. */

/*     The code fragment below illustrates how you could use the */
/*     entry points gathered in this routine to retrieve the */
/*     state transformation matrix. */


/*        First convert names to frame id codes. */

/*        CHARACTER*(32)        NAME1 */
/*        CHARACTER*(32)        NAME2 */

/*        INTEGER               FRAME1 */
/*        INTEGER               FRAME2 */
/*        INTEGER               CENT */
/*        INTEGER               OBS */

/*        DOUBLE PRECISION      ET */
/*        DOUBLE PRECISION      LT */

/*        DOUBLE PRECISION      STATE ( 6 ) */
/*        DOUBLE PRECISION      XFORM ( 6, 6 ) */


/*        First we use the entry points NAMFRM to convert the frame */
/*        names to id codes. */

/*        CALL NAMFRM ( NAME1, FRAME1 ) */
/*        CALL NAMFRM ( NAME2, FRAME2 ) */

/*        Next we determine the center of the second frame */

/*        CALL FRINFO ( FRAME2, CENT, CLASS, CLSSID, FOUND ) */

/*        Determine the light time to the center of the second frame. */

/*        CALL SPKGEO ( CENT,  ET, 'J2000',  OBS, STATE, LT ) */

/*        Finally get the state transformation from FRAME1 to FRAME2 */
/*        at time ET - LT */

/*        CALL FRMCHG ( FRAME1, FRAME2, ET-LT, XFORM ) */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     B.V. Semenov    (JPL) */
/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 5.0.1, 17-MAR-2009 (EDW) */

/*        Entry point NAMFRM: Typo correction in Required_Reading, */
/*        changed FRAME to FRAMES. */

/* -    SPICELIB Version 5.0.0, 05-NOV-2007 (NJB) */

/*        Entry point CCIFRM (map frame class and class ID */
/*        to frame ID code, name, and center) has been added. */

/* -    SPICELIB Version 4.0.0, 13-SEP-2005 (NJB) */

/*        Entry point FRINFO is no longer error-free. Various frame */
/*        definition errors that were previously ignored are now */
/*        diagnosed. */

/*        Entry point FRINFO has been updated to support specification */
/*        of frame center by name or ID code. Previously only ID codes */
/*        could be used to identify frame centers. */

/* -    SPICELIB Version 3.2.0, 20-DEC-2004 (BVS) */

/*        Added parameter incorporating maximum body name length and set */
/*        it to the same value as MAXL from zzbodtrn.inc. Used this */
/*        parameter to declare local variable that holds frame center */
/*        name (LCNAME). */

/*        In FRINFO entry: removed special handling of the frame IDs */
/*        less than -999. If they cannot be ``resolved'' using kernel */
/*        pool keywords, the frame is NOT declared CK-based with center */
/*        ID derived by dividing frame ID by a 1000 and class ID */
/*        assigned the frame ID anymore. In the current practice with */
/*        multitude of TK frames with IDs set instrument IDs this */
/*        default behavior is simply not valid. */

/* -    SPICELIB Version 3.1.0, 28-NOV-2002 (NJB) */

/*        Bug fix: updated CNMFRM so a TK frame specified by name and */
/*        designated as an object's preferred frame via kernel pool */
/*        assignments is found, and so that the correct name of this */
/*        frame is returned. */

/* -    SPICELIB Version 3.0.1, 25-JUN-1999 (WLT) */

/*        Extended documentation of entry point CNMFRM and */
/*        corrected example for that entry point. */

/* -    SPICELIB Version 3.0.0, 03-JUN-1997 (WLT) */

/*        The entry points CIDFRM and CNMFRM were added so that */
/*        user's may determine the frame-id and name to associated */
/*        with a planetary object. */

/* -    SPICELIB Version 2.0.0, 04-APR-1997 (WLT) */

/*        The routine was upgraded to reflect that a block of */
/*        frame idcodes have been reserved for use by the DSN. */
/*        Id-codes 13001 to 13999 have been set aside for DSN */
/*        models for the orientation of the earth.  These frames */
/*        are all PCK frames.  Moreover, the PCK id-code to */
/*        use with these frames is simply the Frame-Code minus 10000. */
/*        All of these frames are centered at the earth (body 399). */

/* -    SPICELIB Version 1.1.0, 14-OCT-1996 (WLT) */

/*       The values NINERT and NNINRT are included instead of */
/*       being declared locally. */

/* -    SPICELIB Version 1.0.0, 18-SEP-1995 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Frame Transformation */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 4.0.0, 12-SEP-2005 (NJB) */

/*        Entry point FRINFO is no longer error-free. Various frame */
/*        definition errors that were previously ignored are now */
/*        diagnosed. */

/*        Entry point FRINFO has been updated to support specification */
/*        of frame center by name or ID code. Previously only ID codes */
/*        could be used to identify frame centers. */

/* -    SPICELIB Version 3.1.0, 28-NOV-2002 (NJB) */

/*        Bug fix: updated CNMFRM so a TK frame specified by name and */
/*        designated as an object's preferred frame via kernel pool */
/*        assignments is found, and so that the correct name of this */
/*        frame is returned. */

/* -    SPICELIB Version 2.0.0, 04-APR-1997 (WLT) */

/*        The routine was upgraded to reflect that a block of */
/*        frame idcodes have been reserved for use by the DSN. */
/*        Id-codes 13001 to 13999 have been set aside for DSN */
/*        models for the orientation of the earth.  These frames */
/*        are all PCK frames.  Moreover, the PCK id-code to */
/*        use with these frames is simply the Frame-Code minus 10000. */
/*        All of these frames are centered at the earth (body 399). */

/*        Accompanying documentation is provided in the appendix */
/*        ``DSN Frames'' of the SPICE document FRAMES.REQ. */

/* -& */

/*     SPICELIB Functions */


/*     Local parameters */


/*     Body name length. The value BDNMLN used here must be the */
/*     same as the value of MAXL defined in the INCLUDE file */

/*        zzbodtrn.inc */

/*     Current value of MAXL = 36. */


/*     Frame name length. */


/*     Kernel variable name length. */


/*     Kernel variable buffer size. */


/*     Local Variables */


/*     Saved variables */

/*     Because we need to save almost everything we save everything */
/*     rather than taking a chance and accidentally leaving something */
/*     off the list. */


/*     Initial values */

    switch(n__) {
	case 1: goto L_namfrm;
	case 2: goto L_frmnam;
	case 3: goto L_frinfo;
	case 4: goto L_cidfrm;
	case 5: goto L_cnmfrm;
	case 6: goto L_ccifrm;
	}

    chkin_("FRAMEX", (ftnlen)6);
    setmsg_("A call has been made to the umbrella routine FRAMEX.  This rout"
	    "ine doesn't do anything.  It acts only as an umbrella routine fo"
	    "r its entry points.  This call probably indicates a misunderstan"
	    "ding in programming. ", (ftnlen)212);
    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("FRAMEX", (ftnlen)6);
    return 0;
/* $Procedure NAMFRM (Name to frame) */

L_namfrm:
/* $ Abstract */

/*     Look up the frame id code associated with a string. */

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

/*     CHARACTER*(*)         FRNAME */
/*     INTEGER               FRCODE */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     FRNAME     I   The name of some reference frame */
/*     FRCODE     O   The SPICE id code of the frame. */

/* $ Detailed_Input */

/*     FRNAME      is a character string that stands for some */
/*                 reference frame (either inertial or non-inertial). */

/*                 Leading blanks in FRNAME are ignored. And the */
/*                 case of the letters in FRNAME are insignificant. */

/*                 Note that all legitimate frame names contain */
/*                 32 or fewer characters. */

/* $ Detailed_Output */

/*     FRCODE      is the SPICE integer code used for internal */
/*                 representation of the named reference frame. */

/*                 If the name input through FRNAME is not recognized */
/*                 FRCODE will be returned with a value of zero. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the input name is not recognized, FRCODE will be */
/*        returned with a value of 0. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This is a low level interface routine intended primarily for */
/*     use within the SPK and CK systems to assist in the transformation */
/*     to user specified reference frames. */

/*     The routine first consults a stored list of reference frame */
/*     names in an attempt to determine the appropriate reference */
/*     frame code. */

/*     If this search is unsuccessful, the routine then examines the */
/*     kernel pool to determine whether or not a variable of the */
/*     form */

/*        'FRAME_' // FRNAME */

/*        (where leading blanks of FRNAME are ignored) */

/*     is present.  If it is and the number of values associated with the */
/*     name is 1, this value is taken to be the frame id code. */

/*     Note:  It is NOT possible to override the default names and */
/*     id codes stored locally in this routine by placing an */
/*     appropriately  variable in the kernel pool with a different */
/*     id code.  The predefined values always take precedence. */

/*     Consult the FRAMES required reading document for more details */
/*     about constructing your own frame definitions. */

/* $ Examples */

/*     Suppose that you needed to find the SPICE id code for the */
/*     bodyfixed reference frame for Mars as modeled by the */
/*     IAU cartographic working group.  Use the following code */
/*     to perform this task. */

/*        CALL NAMFRM ( 'IAU_MARS', FRCODE ) */

/*        WRITE (*,*) 'The SPICE code for the Mars bodyfixed frame is: ', */
/*       .             FRCODE. */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.0.2, 17-MAR-2009 (EDW) */

/*        Typo correction in Required_Reading, changed FRAME to FRAMES. */

/* -    SPICELIB Version 3.0.1, 25-JUN-1999 (WLT) */

/*        Extended documentation of entry point CNMFRM and */
/*        corrected example for that entry point. */

/* -    SPICELIB Version 3.0.0, 03-JUN-1997 (WLT) */

/*        The entry points CIDFRM and CNMFRM were added so that */
/*        user's may determine the frame-id and name to associated */
/*        with a planetary object. */

/* -    SPICELIB Version 2.0.0, 04-APR-1997 (WLT) */

/*        The routine was upgraded to reflect that a block of */
/*        frame idcodes have been reserved for use by the DSN. */
/*        Id-codes 13001 to 13999 have been set aside for DSN */
/*        models for the orientation of the earth.  These frames */
/*        are all PCK frames.  Moreover, the PCK id-code to */
/*        use with these frames is simply the Frame-Code minus 10000. */
/*        All of these frames are centered at the earth (body 399). */

/* -    SPICELIB Version 1.1.0, 14-OCT-1996 (WLT) */

/*       The values NINERT and NNINRT are included instead of */
/*       being declared locally. */

/* -    SPICELIB Version 1.0.0, 18-SEP-1995 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Frame name to frame idcode translation */

/* -& */
    *frcode = 0;

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("NAMFRM", (ftnlen)6);

/*     Perform any needed first pass initializations. */

    if (first) {
	first = FALSE_;
	zzfdat_(&c__121, name__, idcode, center, type__, typeid, norder, 
		corder, centrd, (ftnlen)32);
    }

/*     Determine the location of the requested item in the array */
/*     of names. */

    ljust_(frname, pname, frname_len, (ftnlen)32);
    ucase_(pname, pname, (ftnlen)32, (ftnlen)32);
    item = bschoc_(pname, &c__121, name__, norder, (ftnlen)32, (ftnlen)32);

/*     If the name is in our list, we can just look up its idcode in */
/*     the parallel array. */

    if (item > 0) {
	*frcode = idcode[(i__1 = item - 1) < 121 && 0 <= i__1 ? i__1 : s_rnge(
		"idcode", i__1, "framex_", (ftnlen)627)];
    } else {

/*        The items wasn't in the list, see if it's in the kernel pool. */

	prefix_("FRAME_", &c__0, pname, (ftnlen)6, (ftnlen)32);
	gipool_(pname, &c__1, &c__8, &n, values, &gotit, (ftnlen)32);
	if (n == 1 && gotit) {
	    *frcode = values[0];
	} else {
	    *frcode = 0;
	}
    }
    chkout_("NAMFRM", (ftnlen)6);
    return 0;
/* $Procedure      FRMNAM (Frame to Name) */

L_frmnam:
/* $ Abstract */

/*     Retrieve the name of a reference frame associated with */
/*     a SPICE id code. */

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

/*     INTEGER               FRCODE */
/*     CHARACTER*(*)         FRNAME */


/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     FRCODE     I   an integer code for a reference frame */
/*     FRNAME     O   the name associated with the reference frame. */

/* $ Detailed_Input */

/*     FRCODE      is an integer code for a reference frame. */

/* $ Detailed_Output */

/*     FRNAME      is the name associated with the reference frame. */
/*                 It will be returned left justified. */

/*                 If FRCODE is not recognized as the name of a */
/*                 known reference frame FRNAME will be returned */
/*                 as a blank. */

/*                 If FRNAME is not sufficiently long to hold the */
/*                 name, it will be truncated on the right. */

/*                 All reference frame names are 32 or fewer characters */
/*                 in length.  Thus declaring FRNAME to be CHARACTER*(32) */
/*                 will ensure that the returned name will not be */
/*                 truncated. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If FRCODE is not recognized as the name of a */
/*        known reference frame FRNAME will be returned */
/*        as a blank. */

/*     2) If FRNAME is not sufficiently long to hold the */
/*        name, it will be truncated on the right. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine retrieves the name of a reference frame associated */
/*     with a SPICE frame id code. */

/*     The id codes stored locally are scanned for a match with FRCODE. */
/*     If a match is found, the name stored locally will be returned */
/*     as the name for the frame. */

/*     If FRCODE is not a member of the list of internally stored */
/*     id codes, the kernel pool will be examined to see if the */
/*     variable */

/*        FRAME_idcode_NAME */

/*     is present (where idcode is the decimal character equivalent */
/*     of FRCODE).  If the variable is located and it has both */
/*     character type and dimension 1, the string value of the */
/*     kernel pool variable is returned as the name of the reference */
/*     frame. */

/*     Note that because the local information is always examined */
/*     first and searches of the kernel pool are performed only */
/*     after exhausting local information, it is not possible to */
/*     override the local name for any reference frame that is */
/*     known by this routine. */

/* $ Examples */

/*     Suppose you needed to create a message concerning a reference */
/*     frame and wish to use the name of the frame in the message. */
/*     Suppose further that you have only the frame id code at your */
/*     disposal.  You can capture the frame name using this routine */
/*     as shown here. */

/*        CHARACTER*(32)        FRNAME */

/*        CALL FRMNAM ( FRCODE, FRNAME ) */

/*        IF ( FRNAME .EQ. ' ' ) THEN */
/*           CALL INTSTR ( FRCODE, FRNAME ) */
/*        END IF */

/*        WRITE (*,*) 'Concerning reference frame:', FRNAME */

/*        print the rest of your message. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.0.1, 25-JUN-1999 (WLT) */

/*        Extended documentation of entry point CNMFRM and */
/*        corrected example for that entry point. */

/* -    SPICELIB Version 3.0.0, 03-JUN-1997 (WLT) */

/*        The entry points CIDFRM and CNMFRM were added so that */
/*        user's may determine the frame-id and name to associated */
/*        with a planetary object. */

/* -    SPICELIB Version 2.0.0, 04-APR-1997 (WLT) */

/*        The routine was upgraded to reflect that a block of */
/*        frame idcodes have been reserved for use by the DSN. */
/*        Id-codes 13001 to 13999 have been set aside for DSN */
/*        models for the orientation of the earth.  These frames */
/*        are all PCK frames.  Moreover, the PCK id-code to */
/*        use with these frames is simply the Frame-Code minus 10000. */
/*        All of these frames are centered at the earth (body 399). */

/* -    SPICELIB Version 1.1.0, 14-OCT-1996 (WLT) */

/*       The values NINERT and NNINRT are included instead of */
/*       being declared locally. */

/* -    SPICELIB Version 1.0.0, 18-SEP-1995 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Frame idcode to frame name translation */

/* -& */

/*     Standard SPICE error handling. */

    s_copy(frname, " ", frname_len, (ftnlen)1);
    if (return_()) {
	return 0;
    }
    chkin_("FRMNAM", (ftnlen)6);

/*     Perform any needed first pass initializations. */

    if (first) {
	first = FALSE_;
	zzfdat_(&c__121, name__, idcode, center, type__, typeid, norder, 
		corder, centrd, (ftnlen)32);
    }
    item = bschoi_(frcode, &c__121, idcode, corder);
    if (item != 0) {
	s_copy(frname, name__ + (((i__1 = item - 1) < 121 && 0 <= i__1 ? i__1 
		: s_rnge("name", i__1, "framex_", (ftnlen)868)) << 5), 
		frname_len, (ftnlen)32);
    } else {
	s_copy(pname, "FRAME_#_NAME", (ftnlen)32, (ftnlen)12);
	repmi_(pname, "#", frcode, pname, (ftnlen)32, (ftnlen)1, (ftnlen)32);
	gcpool_(pname, &c__1, &c__8, &n, line, &gotit, (ftnlen)32, (ftnlen)80)
		;
	if (n == 1 && gotit) {
	    ljust_(line, frname, (ftnlen)80, frname_len);
	} else {
	    s_copy(frname, " ", frname_len, (ftnlen)1);
	}
    }
    chkout_("FRMNAM", (ftnlen)6);
    return 0;
/* $Procedure      FRINFO ( Frame Information ) */

L_frinfo:
/* $ Abstract */

/*     Retrieve the minimal attributes associated with a frame */
/*     needed for converting transformations to and from it. */

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

/*     IMPLICIT NONE */
/*     INTEGER               FRCODE */
/*     INTEGER               CENT */
/*     LOGICAL               FOUND */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     FRCODE     I   the idcode for some frame */
/*     CENT       O   the center of the frame */
/*     CLASS      O   the class (type) of the frame */
/*     CLSSID     O   the idcode for the frame within its class. */
/*     FOUND      O   TRUE if the requested information is available. */

/* $ Detailed_Input */

/*     FRCODE      is the id code for some reference frame. */

/* $ Detailed_Output */

/*     CENT        is the body id code for the center of the reference */
/*                 frame (if such an id code is appropriate). */

/*     CLASS       is the class or type of the frame.  This identifies */
/*                 which subsystem will be used to perform frame */
/*                 transformations. */

/*     CLSSID      is the id-code used for the frame within its class. */
/*                 This may be different from the frame id-code. */

/*     FOUND       is TRUE if CENT, CLASS and CCODE are available. */
/*                 Otherwise, FOUND is returned with the value FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If a frame definition is encountered that does not define */
/*        a central body for the frame, the error will be diagnosed */
/*        by routines in the call tree of this routine. */

/*     2) If a frame definition is encountered that does not define */
/*        a class for the frame, the error will be diagnosed by routines */
/*        in the call tree of this routine. */

/*     3) If a frame definition is encountered that does not define */
/*        a class ID for the frame, the error will be diagnosed by */
/*        routines in the call tree of this routine. */

/*     4) If a kernel variable defining a frame name is found, but */
/*        that variable has dimension greater than 1, the error */
/*        SPICE(INVALIDDIMENSION) will be signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This is a low level routine needed by state transformation */
/*     software to transform states and attitudes between different */
/*     reference frames. */

/*     The routine first examines local "hard-coded" information about */
/*     reference frames to see if the requested frame belongs to this */
/*     set.  If it does that information is returned. */

/*     If the requested information is not stored locally, the routine */
/*     then examines the kernel pool to see if the requested information */
/*     is stored there.  If it is and has the expected format, the data */
/*     is retrieved and returned. */

/* $ Examples */

/*     Suppose that you needed to determine the center of some */
/*     reference frame.  The following code fragment illustrates */
/*     how to use this routine to determine this information. */

/*        CALL FRINFO ( FRCODE, CENT, CLASS, CLSSID, FOUND ) */

/*        IF ( FOUND ) THEN */

/*           WRITE (*,*) 'The center of reference frame ', FRCODE */
/*           WRITE (*,*) 'has body id code : ', CENT */

/*        ELSE */

/*           WRITE (*,*) 'There is insufficient data for frame ', FRCODE */

/*        END IF */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*      None. */

/* $ Author_and_Institution */

/*      N.J. Bachman    (JPL) */
/*      B.V. Semenov    (JPL) */
/*      W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 4.0.0, 12-SEP-2005 (NJB) */

/*        Entry point FRINFO is no longer error-free. Various frame */
/*        definition errors that were previously ignored are now */
/*        diagnosed. */

/*        Specification of frame center by name or ID is now supported. */
/*        Previously only ID codes could be used to identify frame */
/*        centers.  Various frame definition errors that were previously */
/*        ignored are now diagnosed. */

/* -    SPICELIB Version 3.1.0, 20-DEC-2004 (BVS) */

/*        Removed special handling of the frame IDs less than -999. If */
/*        they cannot be ``resolved'' using kernel pool keywords, the */
/*        frame is NOT declared CK-based with center ID derived by */
/*        dividing frame ID by a 1000 and class ID assigned the frame ID */
/*        anymore. In the current practice with multitude of TK frames */
/*        with IDs set instrument IDs this default behavior is simply */
/*        not valid. */

/* -    SPICELIB Version 3.0.1, 25-JUN-1999 (WLT) */

/*        Extended documentation of entry point CNMFRM and */
/*        corrected example for that entry point. */

/* -    SPICELIB Version 3.0.0, 03-JUN-1997 (WLT) */

/*        The entry points CIDFRM and CNMFRM were added so that */
/*        user's may determine the frame-id and name to associated */
/*        with a planetary object. */

/* -    SPICELIB Version 2.0.0, 04-APR-1997 (WLT) */

/*        The routine was upgraded to reflect that a block of */
/*        frame idcodes have been reserved for use by the DSN. */
/*        Id-codes 13001 to 13999 have been set aside for DSN */
/*        models for the orientation of the earth.  These frames */
/*        are all PCK frames.  Moreover, the PCK id-code to */
/*        use with these frames is simply the Frame-Code minus 10000. */
/*        All of these frames are centered at the earth (body 399). */

/* -    SPICELIB Version 1.1.0, 14-OCT-1996 (WLT) */

/*       The values NINERT and NNINRT are included instead of */
/*       being declared locally. */

/* -    SPICELIB Version 1.0.0, 18-SEP-1995 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Fetch reference frame attributes */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 4.0.0, 12-SEP-2005 (NJB) */

/*        Entry point FRINFO is no longer error-free. */
/*        The following errors are now diagnosed: */

/*           - Invalid dimension of frame name variable */

/*           - If a valid frame name assignment is present: */

/*              + Missing frame ID code assignment */
/*              + Missing class assignment */
/*              + Missing class ID assignment */

/*        Specification of frame center by name or ID is now supported. */
/*        Previously only ID codes could be used to identify frame */
/*        centers. */


/* -    SPICELIB Version 3.1.0, 20-DEC-2004 (BVS) */

/*        Removed special handling of the frame IDs less than -999. If */
/*        they cannot be ``resolved'' using kernel pool keywords, the */
/*        frame is NOT declared CK-based with center ID derived by */
/*        dividing frame ID by a 1000 and class ID assigned the frame ID */
/*        anymore. In the current practice with multitude of TK frames */
/*        with IDs set instrument IDs this default behavior is simply */
/*        not valid. */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("FRINFO", (ftnlen)6);

/*     No frame information has been found yet. */

    *found = FALSE_;

/*     Perform any needed first pass initializations. */

    if (first) {
	first = FALSE_;
	zzfdat_(&c__121, name__, idcode, center, type__, typeid, norder, 
		corder, centrd, (ftnlen)32);
    }

/*     Determine the location of the requested item in the array */
/*     of ID codes. */

    item = bschoi_(frcode, &c__121, idcode, corder);

/*     If the name is in our list, we can just look up its ID code in */
/*     the parallel array. */

    if (item > 0) {
	*cent = center[(i__1 = item - 1) < 121 && 0 <= i__1 ? i__1 : s_rnge(
		"center", i__1, "framex_", (ftnlen)1173)];
	*class__ = type__[(i__1 = item - 1) < 121 && 0 <= i__1 ? i__1 : 
		s_rnge("type", i__1, "framex_", (ftnlen)1174)];
	*clssid = typeid[(i__1 = item - 1) < 121 && 0 <= i__1 ? i__1 : s_rnge(
		"typeid", i__1, "framex_", (ftnlen)1175)];
	*found = TRUE_;
    } else {

/*        The frame ID wasn't in the list; see if the frame name */
/*        is in the kernel pool. */

	s_copy(pname, "FRAME_#_NAME", (ftnlen)32, (ftnlen)12);
	repmi_(pname, "#", frcode, pname, (ftnlen)32, (ftnlen)1, (ftnlen)32);
	gcpool_(pname, &c__1, &c__8, &n, line, &gotit, (ftnlen)32, (ftnlen)80)
		;
	if (gotit) {
	    if (n > 1) {

/*              We have an array-valued variable that looks like */
/*              a frame name.  We consider this an error. */

		setmsg_("Kernel variable # is array-valued; Frame name varia"
			"bles must be scalar-valued.", (ftnlen)78);
		errch_("#", pname, (ftnlen)1, (ftnlen)32);
		sigerr_("SPICE(INVALIDDIMENSION)", (ftnlen)23);
		chkout_("FRINFO", (ftnlen)6);
		return 0;
	    }
	    ljust_(line, lcfram, (ftnlen)80, (ftnlen)32);

/*           Start by looking up the central body of the frame. The name */
/*           of the kernel variable for the body could refer to the */
/*           frame by name or frame ID; the body itself could be */
/*           specified by name or body ID. */

	    zzdynbid_(lcfram, frcode, "CENTER", cent, (ftnlen)32, (ftnlen)6);
	    if (failed_()) {
		chkout_("FRINFO", (ftnlen)6);
		return 0;
	    }
	    *found = TRUE_;

/*           FOUND has been set to indicate whether we found the frame's */
/*           center.  If we did, CENT has been assigned. */

/*           Next look up the frame class and class ID. */

	    zzdynvai_(lcfram, frcode, "CLASS", &c__1, &n, values, (ftnlen)32, 
		    (ftnlen)5);
	    *class__ = values[0];
	    zzdynvai_(lcfram, frcode, "CLASS_ID", &c__1, &n, values, (ftnlen)
		    32, (ftnlen)8);
	    *clssid = values[0];
	    if (failed_()) {
		chkout_("FRINFO", (ftnlen)6);
		return 0;
	    }
	}

/*        In support of the DSN, NAIF has reserved a block of */
/*        ID-codes for DSN specific frames  from 13000 to 13999. */
/*        These are always PCK based frames for the earth. */
/*        The PCK ID code is just FRCODE - 10000. */

	if (! (*found) && *frcode >= 13000 && *frcode < 14000) {
	    *cent = 399;
	    *class__ = 2;
	    *clssid = *frcode - 10000;
	    *found = TRUE_;
	}
    }
    chkout_("FRINFO", (ftnlen)6);
    return 0;
/* $Procedure CIDFRM ( center SPK id  frame ) */

L_cidfrm:
/* $ Abstract */

/*     Retrieve frame ID code and name to associate with a frame center. */

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

/*     IMPLICIT NONE */
/*     INTEGER               CENT */
/*     INTEGER               FRCODE */
/*     CHARACTER*(*)         FRNAME */
/*     LOGICAL               FOUND */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     CENT       I   an object to associate a frame with. */
/*     FRCODE     O   the ID code of the frame associated with CENT */
/*     FRNAME     O   the name of the frame with id FRCODE */
/*     FOUND      O   TRUE if the requested information is available. */

/* $ Detailed_Input */

/*     CENT        is the id code for object for which there is a */
/*                 preferred reference frame. */

/* $ Detailed_Output */

/*     FRCODE      is the frame id-code to associate with a the object */
/*                 specified by CENT. */

/*     FRNAME      is the name of the frame that should be associated */
/*                 with the object specified by CNAME. FRNAME should be */
/*                 declared as CHARACTER*(32) to ensure that it can */
/*                 contain the full name of the frame.  If FRNAME does */
/*                 not have enough room to hold the full name of the */
/*                 frame, the name will be truncated on the right. */

/*     FOUND       is TRUE if the appropriate frame id-code and frame */
/*                 name can be determined.  Otherwise FOUND is returned */
/*                 with the value FALSE. */

/* $ Parameters */

/*      None. */

/* $ Exceptions */

/*     1) If FRNAME is not have room to contain the frame name, the name */
/*        will be truncated on the right. ( Declaring FRNAME to be */
/*        CHARACTER*(32) will ensure that the name will not be */
/*        truncated. */

/* $ Files */

/*      None. */

/* $ Particulars */

/*     This routine allows the user to determine the frame that should */
/*     be associated with a particular object.   For example, if you */
/*     need the frame to associate with the Io, you can call CIDFRM */
/*     to determine the frame name and id-code for the bodyfixed frame */
/*     of Io. */

/*     The preferred frame to use with an object is specified via one */
/*     of the kernel pool variables: */

/*         OBJECT_<cent>_FRAME */

/*     where <cent> is the decimal representation of the integer CENT. */

/*     For those PCK objects that have "built-in" frame names this */
/*     routine returns the corresponding "IAU" frame and frame ID code. */

/* $ Examples */

/*     Suppose that you want to determine the state of a target */
/*     in the preferred reference frame of some observer.  This */
/*     routine can be used in conjunction with SPKEZ to compute */
/*     the state. */

/*        CALL CIDFRM ( OBS, FRCODE, FRNAME, FOUND ) */

/*        IF ( .NOT. FOUND ) THEN */

/*           WRITE (*,*) 'The bodyfixed frame for object ', OBS */
/*           WRITE (*,*) 'could not be identified.' */
/*           STOP */

/*        END IF */

/*        CALL SPKEZ ( TARG, ET, FRNAME, ABCORR, OBS, STATE, LT ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*      None. */

/* $ Author_and_Institution */

/*      W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.0.1, 25-JUN-1999 (WLT) */

/*        Extended documentation of entry point CNMFRM and */
/*        corrected example for that entry point. */

/* -    SPICELIB Version 3.0.0, 03-JUN-1997 (WLT) */

/*        The entry points CIDFRM and CNMFRM were added so that */
/*        user's may determine the frame-id and name to associated */
/*        with a planetary object. */

/* -    SPICELIB Version 2.0.0, 04-APR-1997 (WLT) */

/*        The routine was upgraded to reflect that a block of */
/*        frame ID codes have been reserved for use by the DSN. */
/*        Id-codes 13001 to 13999 have been set aside for DSN */
/*        models for the orientation of the earth.  These frames */
/*        are all PCK frames.  Moreover, the PCK id-code to */
/*        use with these frames is simply the Frame-Code minus 10000. */
/*        All of these frames are centered at the earth (body 399). */

/* -    SPICELIB Version 1.1.0, 14-OCT-1996 (WLT) */

/*       The values NINERT and NNINRT are included instead of */
/*       being declared locally. */

/* -    SPICELIB Version 1.0.0, 18-SEP-1995 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Find the bodyfixed frame associated with an object */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("CIDFRM", (ftnlen)6);

/*     Perform any needed first pass initializations. */

    if (first) {
	first = FALSE_;
	zzfdat_(&c__121, name__, idcode, center, type__, typeid, norder, 
		corder, centrd, (ftnlen)32);
    }

/*     First look up in the kernel pool the frame associated with this */
/*     center. */

    s_copy(lookup, "OBJECT_#_FRAME", (ftnlen)32, (ftnlen)14);
    repmi_(lookup, "#", cent, lookup, (ftnlen)32, (ftnlen)1, (ftnlen)32);
    dtpool_(lookup, &gotit, &n, dattyp, (ftnlen)32, (ftnlen)1);

/*     If we didn't find this object in the form OBJECT_<number>_FRAME */
/*     maybe it is present in the form OBJECT_<name>_FRAME.  It's */
/*     worth a try. */

    if (! gotit) {

/*        See if we can get the name for this center's id-code. */

	bodc2n_(cent, lcname, &gotit, (ftnlen)36);
	if (gotit) {

/*           Construct and look up the alternative name in the */
/*           kernel pool. */

	    s_copy(lookup, "OBJECT_#_FRAME", (ftnlen)32, (ftnlen)14);
	    repmc_(lookup, "#", lcname, lookup, (ftnlen)32, (ftnlen)1, (
		    ftnlen)36, (ftnlen)32);
	    ucase_(lookup, lookup, (ftnlen)32, (ftnlen)32);
	    dtpool_(lookup, &gotit, &n, dattyp, (ftnlen)32, (ftnlen)1);
	}
    }

/*     There are two cases.  The user may specify either a name */
/*     or id-code for the frame to use to model the orientation of */
/*     an object.  We assume they'll opt for the character string */
/*     form so we test that case first. */

    if (gotit) {
	if (*(unsigned char *)dattyp == 'C') {
	    gcpool_(lookup, &c__1, &c__1, &n, pname, &gotit, (ftnlen)32, (
		    ftnlen)32);

/*           We've got the name:  See if we have this in our handy list */
/*           of built-in names. */

	    item = bschoc_(pname, &c__121, name__, norder, (ftnlen)32, (
		    ftnlen)32);
	    if (item > 0) {
		s_copy(frname, pname, frname_len, (ftnlen)32);
		*frcode = idcode[(i__1 = item - 1) < 121 && 0 <= i__1 ? i__1 :
			 s_rnge("idcode", i__1, "framex_", (ftnlen)1517)];
		*found = TRUE_;
	    } else {

/*              Nope.  look in the kernel pool for the data associated */
/*              with this frame. */

		s_copy(frname, pname, frname_len, (ftnlen)32);
		prefix_("FRAME_", &c__0, pname, (ftnlen)6, (ftnlen)32);
		gipool_(pname, &c__1, &c__8, &n, values, &gotit, (ftnlen)32);
		if (n == 1 && gotit) {
		    *frcode = values[0];
		    *found = TRUE_;
		} else {
		    *frcode = 0;
		    s_copy(frname, " ", frname_len, (ftnlen)1);
		    *found = FALSE_;
		}
	    }
	} else if (*(unsigned char *)dattyp == 'N') {

/*           Ok.  They decided to use the numeric form to specify */
/*           the frame id.  We need to figure out the name of the frame. */
/*           First we retrieve the frame id they've loaded into the */
/*           kernel pool. */

	    gipool_(lookup, &c__1, &c__1, &n, values, &gotit, (ftnlen)32);

/*           We've got the frame ID, see if we already know about this */
/*           id-code. */

	    item = bschoi_(values, &c__121, center, centrd);
	    if (item != 0) {

/*              Just look up the name and set the frame code. */

		s_copy(frname, name__ + (((i__1 = item - 1) < 121 && 0 <= 
			i__1 ? i__1 : s_rnge("name", i__1, "framex_", (ftnlen)
			1565)) << 5), frname_len, (ftnlen)32);
		*frcode = values[0];
		*found = TRUE_;
	    } else {

/*              Hmmm. Not on the built-in list.  See if it's in the */
/*              kernel pool somewhere. */

		s_copy(pname, "FRAME_#_NAME", (ftnlen)32, (ftnlen)12);
		repmi_(pname, "#", values, pname, (ftnlen)32, (ftnlen)1, (
			ftnlen)32);
		gcpool_(pname, &c__1, &c__8, &n, line, &gotit, (ftnlen)32, (
			ftnlen)80);
		if (n == 1 && gotit) {
		    ljust_(line, frname, (ftnlen)80, frname_len);
		    *frcode = values[0];
		    *found = TRUE_;
		} else {
		    *frcode = values[0];
		    s_copy(frname, " ", frname_len, (ftnlen)1);
		    *found = FALSE_;
		}
	    }
	}

/*        One way or the other we've filled in the values at this */
/*        point.  Nothing left to do but check out and return. */

	chkout_("CIDFRM", (ftnlen)6);
	return 0;
    }

/*     The only way to reach this point is if the user did not */
/*     specify via the kernel pool a frame to use for this center. */

/*     We have a special case for EARTH. */

    if (*cent == 399) {
	*frcode = 10013;
	s_copy(frname, "IAU_EARTH", frname_len, (ftnlen)9);
	*found = TRUE_;
	chkout_("CIDFRM", (ftnlen)6);
	return 0;
    }

/*     Determine the location of the requested item in the array */
/*     of centers. */

    item = bschoi_(cent, &c__121, center, centrd);

/*     If the name is in our list, we can just look up its ID code and */
/*     name in the parallel array. */

    if (item > 0) {
	*frcode = idcode[(i__1 = item - 1) < 121 && 0 <= i__1 ? i__1 : s_rnge(
		"idcode", i__1, "framex_", (ftnlen)1632)];
	s_copy(frname, name__ + (((i__1 = item - 1) < 121 && 0 <= i__1 ? i__1 
		: s_rnge("name", i__1, "framex_", (ftnlen)1633)) << 5), 
		frname_len, (ftnlen)32);
	*found = TRUE_;
    } else {

/*        There's nothing we can do now.  We don't know what frame */
/*        might be associated with this object. */

	s_copy(frname, " ", frname_len, (ftnlen)1);
	*frcode = 0;
	*found = FALSE_;
    }
    chkout_("CIDFRM", (ftnlen)6);
    return 0;
/* $Procedure CNMFRM ( Center name to associated frame ) */

L_cnmfrm:
/* $ Abstract */

/*     Retrieve frame ID code and name to associate with an object. */

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

/*     IMPLICIT NONE */
/*     CHARACTER*(*)         CNAME */
/*     INTEGER               FRCODE */
/*     CHARACTER*(*)         FRNAME */
/*     LOGICAL               FOUND */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     CNAME      I   name of the object to find a frame for */
/*     FRCODE     O   the ID code of the frame associated with CNAME */
/*     FRNAME     O   the name of the frame with id FRCODE */
/*     FOUND      O   TRUE if the requested information is available. */

/* $ Detailed_Input */

/*     CNAME       is the name for object for which there is a */
/*                 preferred reference frame */

/* $ Detailed_Output */

/*     FRCODE      is the frame id-code to associate with a the object */
/*                 specified by CNAME. */

/*     FRNAME      is the name of the frame that should be associated */
/*                 with the object specified by CNAME. FRNAME should be */
/*                 declared as CHARACTER*(32) to ensure that it can */
/*                 contain the full name of the frame.  If FRNAME does */
/*                 not have enough room to hold the full name of the */
/*                 frame, the name will be truncated on the right. */

/*     FOUND       is TRUE if the appropriate frame id-code and frame */
/*                 name can be determined.  Otherwise FOUND is returned */
/*                 with the value FALSE. */

/* $ Parameters */

/*      None. */

/* $ Exceptions */

/*     1) If FRNAME is not have room to contain the frame name, the name */
/*        will be truncated on the right. ( Declaring FRNAME to be */
/*        CHARACTER*(32) will ensure that the name will not be */
/*        truncated. */

/* $ Files */

/*      None. */

/* $ Particulars */

/*     This routine allows the user to determine the frame that should */
/*     be associated with a particular object.   For example, if you */
/*     need the frame to associate with the Io, you can call CNMFRM */
/*     to determine the frame name and id-code for the bodyfixed frame */
/*     of Io. */

/*     The preferred frame to use with an object is specified via one */
/*     of the kernel pool variables: */

/*         OBJECT_<cname>_FRAME */

/*     where <cname> is the non-blank portion of the string CNAME. */

/*     For those PCK objects that have "built-in" frame names this */
/*     routine returns the corresponding "IAU" frame and frame ID code. */

/* $ Examples */

/*     Suppose that you want to determine the state of a target */
/*     in the preferred reference frame of some observer.  This */
/*     routine can be used in conjunction with SPKEZR to compute */
/*     the state. */

/*        CALL CNMFRM ( OBSNAM, FRCODE, FRNAME, FOUND ) */

/*        IF ( .NOT. FOUND ) THEN */

/*           WRITE (*,*) 'The bodyfixed frame for object ', OBSNAM */
/*           WRITE (*,*) 'could not be identified.' */
/*           STOP */

/*        END IF */

/*        CALL SPKEZR ( TARGET, ET, FRNAME, ABCORR, OBSNAM, STATE, LT ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*      None. */

/* $ Author_and_Institution */

/*      W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.1.0, 28-NOV-2002 (NJB) */

/*        Bug fix: updated this routine so a TK frame specified by name */
/*        and designated as an object's preferred frame via kernel pool */
/*        assignments is found, and so that the correct name of this */
/*        frame is returned. */

/* -    SPICELIB Version 3.0.1, 25-JUN-1999 (WLT) */

/*        Extended documentation of entry point CNMFRM and */
/*        corrected example for that entry point. */

/* -    SPICELIB Version 3.0.0, 03-JUN-1997 (WLT) */

/*        The entry points CIDFRM and CNMFRM were added so that */
/*        user's may determine the frame-id and name to associated */
/*        with a planetary object. */

/* -    SPICELIB Version 2.0.0, 04-APR-1997 (WLT) */

/*        The routine was upgraded to reflect that a block of */
/*        frame ID codes have been reserved for use by the DSN. */
/*        Id-codes 13001 to 13999 have been set aside for DSN */
/*        models for the orientation of the earth.  These frames */
/*        are all PCK frames.  Moreover, the PCK id-code to */
/*        use with these frames is simply the Frame-Code minus 10000. */
/*        All of these frames are centered at the earth (body 399). */


/* -    SPICELIB Version 1.1.0, 14-OCT-1996 (WLT) */

/*       The values NINERT and NNINRT are included instead of */
/*       being declared locally. */

/* -    SPICELIB Version 1.0.0, 18-SEP-1995 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Find the bodyfixed frame associated with an object */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 3.1.0, 28-NOV-2002 (NJB) */

/*        Bug fix: updated this routine so a TK frame specified by name */
/*        and designated as an object's preferred frame via kernel pool */
/*        assignments is found, and so that the correct name of this */
/*        frame is returned. */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("CNMFRM", (ftnlen)6);

/*     Perform any needed first pass initializations. */

    if (first) {
	first = FALSE_;
	zzfdat_(&c__121, name__, idcode, center, type__, typeid, norder, 
		corder, centrd, (ftnlen)32);
    }

/*     First look up in the kernel pool the frame associated with this */
/*     center. */

    s_copy(lookup, "OBJECT_#_FRAME", (ftnlen)32, (ftnlen)14);
    repmc_(lookup, "#", cname, lookup, (ftnlen)32, (ftnlen)1, cname_len, (
	    ftnlen)32);
    ucase_(lookup, lookup, (ftnlen)32, (ftnlen)32);
    dtpool_(lookup, &gotit, &n, dattyp, (ftnlen)32, (ftnlen)1);

/*     If we didn't find this object in the form OBJECT_<name>_FRAME */
/*     maybe it is present in the form OBJECT_<number>_FRAME.  It's */
/*     worth a try. */

    if (! gotit) {

/*        See if we can get the name for this center's id-code. */

	bodn2c_(cname, &id, &gotit, cname_len);
	if (gotit) {

/*           Construct and look up the alternative name in the */
/*           kernel pool. */

	    s_copy(lookup, "OBJECT_#_FRAME", (ftnlen)32, (ftnlen)14);
	    repmi_(lookup, "#", &id, lookup, (ftnlen)32, (ftnlen)1, (ftnlen)
		    32);
	    dtpool_(lookup, &gotit, &n, dattyp, (ftnlen)32, (ftnlen)1);
	}
    }

/*     There are two cases.  The user may specify either a name */
/*     or id-code for the frame to use to model the orientation of */
/*     an object.  We assume they'll opt for the character string */
/*     form so we test that case first. */

    if (gotit) {
	if (*(unsigned char *)dattyp == 'C') {
	    gcpool_(lookup, &c__1, &c__1, &n, pname, &gotit, (ftnlen)32, (
		    ftnlen)32);

/*           We've got the name:  See if we have this in our handy list */
/*           of built-in names. */

	    item = bschoc_(pname, &c__121, name__, norder, (ftnlen)32, (
		    ftnlen)32);
	    if (item > 0) {
		s_copy(frname, pname, frname_len, (ftnlen)32);
		*frcode = idcode[(i__1 = item - 1) < 121 && 0 <= i__1 ? i__1 :
			 s_rnge("idcode", i__1, "framex_", (ftnlen)1926)];
		*found = TRUE_;
	    } else {

/*              Nope.  look in the kernel pool for the data associated */
/*              with this frame. */

/*              Capture the frame name now, since we're going to modify */
/*              PNAME. */

		s_copy(frname, pname, frname_len, (ftnlen)32);
		prefix_("FRAME_", &c__0, pname, (ftnlen)6, (ftnlen)32);
		gipool_(pname, &c__1, &c__8, &n, values, &gotit, (ftnlen)32);
		if (n == 1 && gotit) {
		    *frcode = values[0];
		    *found = TRUE_;
		} else {
		    *frcode = 0;
		    s_copy(frname, " ", frname_len, (ftnlen)1);
		    *found = FALSE_;
		}
	    }
	} else if (*(unsigned char *)dattyp == 'N') {

/*           Ok.  They decided to use the numeric form to specify */
/*           the frame id.  We need to figure out the name of the frame. */
/*           First we retrieve the frame id they've loaded into the */
/*           kernel pool. */

	    gipool_(lookup, &c__1, &c__1, &n, values, &gotit, (ftnlen)32);

/*           We've got the frame ID, see if we already know about this */
/*           id-code. */

	    item = bschoi_(values, &c__121, idcode, corder);
	    if (item != 0) {

/*              Just look up the name and set the frame code. */

		s_copy(frname, name__ + (((i__1 = item - 1) < 121 && 0 <= 
			i__1 ? i__1 : s_rnge("name", i__1, "framex_", (ftnlen)
			1977)) << 5), frname_len, (ftnlen)32);
		*frcode = values[0];
		*found = TRUE_;
	    } else {

/*              Hmmm. Not on the built-in list.  See if it's in the */
/*              kernel pool somewhere. */

		s_copy(pname, "FRAME_#_NAME", (ftnlen)32, (ftnlen)12);
		repmi_(pname, "#", values, pname, (ftnlen)32, (ftnlen)1, (
			ftnlen)32);
		gcpool_(pname, &c__1, &c__8, &n, line, &gotit, (ftnlen)32, (
			ftnlen)80);
		if (n == 1 && gotit) {
		    ljust_(line, frname, (ftnlen)80, frname_len);
		    *frcode = values[0];
		    *found = TRUE_;
		} else {
		    *frcode = values[0];
		    s_copy(frname, " ", frname_len, (ftnlen)1);
		    *found = FALSE_;
		}
	    }
	}

/*        One way or the other we've filled in the values at this */
/*        point.  Nothing left to do but check out and return. */

	chkout_("CNMFRM", (ftnlen)6);
	return 0;
    }

/*     The only way to reach this point is if the user did not */
/*     specify via the kernel pool a frame to use for this center. */


    s_copy(frname, "IAU_#", frname_len, (ftnlen)5);
    repmc_(frname, "#", cname, frname, frname_len, (ftnlen)1, cname_len, 
	    frname_len);
    ucase_(frname, frname, frname_len, frname_len);

/*     Determine the location of the requested item in the array */
/*     of centers. */

    item = bschoc_(frname, &c__121, name__, norder, frname_len, (ftnlen)32);

/*     If the name is in our list, we can just look up its ID code and */
/*     name in the parallel array. */

    if (item > 0) {
	*frcode = idcode[(i__1 = item - 1) < 121 && 0 <= i__1 ? i__1 : s_rnge(
		"idcode", i__1, "framex_", (ftnlen)2038)];
	*found = TRUE_;
    } else {

/*        There's nothing we can do now.  We don't know what frame */
/*        might be associated with this object. */

	*frcode = 0;
	*found = FALSE_;
    }
    chkout_("CNMFRM", (ftnlen)6);
    return 0;
/* $Procedure CCIFRM ( Class and class ID to associated frame ) */

L_ccifrm:
/* $ Abstract */

/*     Return the frame name, frame ID, and center associated with */
/*     a given frame class and class ID. */

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

/*     FRAMES */

/* $ Keywords */

/*     FRAMES */

/* $ Declarations */

/*     INTEGER               CLASS */
/*     INTEGER               CLSSID */
/*     INTEGER               FRCODE */
/*     CHARACTER*(*)         FRNAME */
/*     INTEGER               CENT */
/*     LOGICAL               FOUND */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     CLASS      I   Class of frame. */
/*     CLSSID     I   Class ID of frame. */
/*     FRCODE     O   ID code of the frame identified by CLASS, CLSSID. */
/*     FRNAME     O   Name of the frame identified by CLASS, CLSSID. */
/*     CENT       O   Center of the frame identified by CLASS, CLSSID. */
/*     FOUND      O   TRUE if the requested information is available. */

/* $ Detailed_Input */

/*     CLASS       is the class or type of the frame.  This identifies */
/*                 which subsystem will be used to perform frame */
/*                 transformations. */

/*     CLSSID      is the ID code used for the frame within its class. */
/*                 This may be different from the frame ID code. */

/* $ Detailed_Output */

/*     FRCODE      is the frame ID code for the reference frame */
/*                 identified by CLASS and CLSSID. */

/*     FRNAME      is the name of the frame identified by CLASS and */
/*                 CLSSID. FRNAME should be declared as CHARACTER*(32) */
/*                 to ensure that it can contain the full name of the */
/*                 frame.  If FRNAME does not have enough room to hold */
/*                 the full name of the frame, the name will be */
/*                 truncated on the right. */

/*     CENT        is the body ID code for the center of the reference */
/*                 frame identified by CLASS and CLSSID. */

/*     FOUND       is .TRUE. if FRCODE, FRNAME, and CENT are available. */
/*                 Otherwise, FOUND is returned with the value .FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) This routine assumes that the first frame found with matching */
/*        class and class ID is the correct one. SPICE's frame system */
/*        does not diagnose the situation where there are multiple, */
/*        distinct frames with matching classes and class ID codes, but */
/*        this situation could occur if such conflicting frame */
/*        specifications are loaded via one or more frame kernels. The */
/*        user is responsible for avoiding such frame specification */
/*        conflicts. */

/*     2) If FRNAME does not have room to contain the frame name, the */
/*        name will be truncated on the right. ( Declaring FRNAME to be */
/*        CHARACTER*(32) will ensure that the name will not be */
/*        truncated. */

/*     3) If a frame class assignment is found that associates a */
/*        string (as opposed to numeric) value with a frame class */
/*        keyword, the error SPICE(INVALIDFRAMEDEF) will be signaled. */

/*     4) If a frame class assignment is found that matches the input */
/*        class, but a corresponding class ID assignment is not */
/*        found in the kernel pool, the error SPICE(INVALIDFRAMEDEF) */
/*        will be signaled. */

/*     5) If a frame specification is found in the kernel pool with */
/*        matching frame class and class ID, but either the frame name */
/*        or frame ID code are not found, the error */
/*        SPICE(INVALIDFRAMEDEF) will be signaled. */

/*     6) If a frame specification is found in the kernel pool with */
/*        matching frame class and class ID, but the frame center */
/*        is not found, the error will be diagnosed by routines */
/*        in the call tree of this routine. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine allows the user to determine the frame associated */
/*     with a given frame class and class ID code. The kernel pool is */
/*     searched first for a matching frame; if no match is found, then */
/*     the set of built-in frames is searched. */

/*     Since the neither the frame class nor the class ID are primary */
/*     keys, searching for matching frames is a linear (and therefore */
/*     typically slow) process. */

/* $ Examples */

/*     Suppose that you want to find the name of a frame associated */
/*     with a PCK class ID, such as that found in a binary PCK. */
/*     One could use the following code fragment: */

/*        CALL CCIFRM ( 2, CLSSID, FRCODE, FRNAME, CENT, FOUND ) */

/*        IF ( .NOT. FOUND ) THEN */

/*           WRITE (*,*) 'The PCK frame for class ID ', CLSSID */
/*           WRITE (*,*) 'could not be identified.' */
/*           STOP */

/*        END IF */

/*        WRITE (*,*) 'The PCK frame having class ID ', CLSSID, ' is ' */
/*        WRITE (*,*) FRNAME */

/* $ Restrictions */

/*     See item (1) in the Exceptions section above. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 05-NOV-2007 (NJB) */

/* -& */
/* $ Index_Entries */

/*     Find info associated with a frame class and class id */
/*     Map frame class and class id to frame info */
/*     Map frame class and class id to frame name, id, and center */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("CCIFRM", (ftnlen)6);

/*     No frame found so far. */

    *found = FALSE_;

/*     Perform any needed first pass initializations. */

    if (first) {
	first = FALSE_;
	zzfdat_(&c__121, name__, idcode, center, type__, typeid, norder, 
		corder, centrd, (ftnlen)32);
    }

/*     First try to look up from the kernel pool the frame associated */
/*     with the input class and class ID. Since neither of these input */
/*     values appears in a kernel variable name, we may have to look at */
/*     all of the frame specifications in the kernel pool. Start out by */
/*     looking the frame class assignments from any loaded frame */
/*     specifications. */

    s_copy(lookup, "FRAME_*_CLASS", (ftnlen)32, (ftnlen)13);
    start = 1;
    gnpool_(lookup, &start, &c__100, &n, kvbuff, &fnd, (ftnlen)32, (ftnlen)32)
	    ;
    while(fnd && n > 0) {

/*        For each kernel variable name found in the buffer, look up the */
/*        associated class. If the class matches the input class, look */
/*        up the class ID as well. Set the output arguments and return */
/*        if we get a complete match. */

	i__1 = n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    gipool_(kvbuff + (((i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : 
		    s_rnge("kvbuff", i__2, "framex_", (ftnlen)2293)) << 5), &
		    c__1, &c__1, &n, &kvclss, &fnd, (ftnlen)32);
	    if (! fnd) {
		setmsg_("Invalid frame specification found in kernel pool: f"
			"rame class keyword is # but integer class was not as"
			"sociated with this keyword.", (ftnlen)130);
		errch_("#", kvbuff + (((i__2 = i__ - 1) < 100 && 0 <= i__2 ? 
			i__2 : s_rnge("kvbuff", i__2, "framex_", (ftnlen)2301)
			) << 5), (ftnlen)1, (ftnlen)32);
		sigerr_("SPICE(INVALIDFRAMEDEF)", (ftnlen)22);
		chkout_("CCIFRM", (ftnlen)6);
		return 0;
	    }
	    if (kvclss == *class__) {

/*              Get the class ID for the current frame. */

		s_copy(look2, kvbuff + (((i__2 = i__ - 1) < 100 && 0 <= i__2 ?
			 i__2 : s_rnge("kvbuff", i__2, "framex_", (ftnlen)
			2312)) << 5), (ftnlen)32, (ftnlen)32);
		suffix_("_ID", &c__0, look2, (ftnlen)3, (ftnlen)32);
		gipool_(look2, &c__1, &c__1, &n, &kvclid, &fnd, (ftnlen)32);
		if (! fnd) {
		    setmsg_("Invalid frame specification found in kernel poo"
			    "l: frame class keyword is # but associated integ"
			    "er class ID assignment was not found.", (ftnlen)
			    132);
		    errch_("#", kvbuff + (((i__2 = i__ - 1) < 100 && 0 <= 
			    i__2 ? i__2 : s_rnge("kvbuff", i__2, "framex_", (
			    ftnlen)2325)) << 5), (ftnlen)1, (ftnlen)32);
		    sigerr_("SPICE(INVALIDFRAMEDEF)", (ftnlen)22);
		    chkout_("CCIFRM", (ftnlen)6);
		    return 0;
		}

/*              Check the class ID for the current kernel variable */
/*              against the input value. */

		if (kvclid == *clssid) {

/*                 We have a match. We need to return the frame */
/*                 ID, frame name, and center. As long as we're */
/*                 looking at a valid frame specification, this is */
/*                 no problem. */

/*                 Look up the frame name first. Create the frame */
/*                 name keyword. */

		    repmc_(kvbuff + (((i__2 = i__ - 1) < 100 && 0 <= i__2 ? 
			    i__2 : s_rnge("kvbuff", i__2, "framex_", (ftnlen)
			    2346)) << 5), "_CLASS", "_NAME", look2, (ftnlen)
			    32, (ftnlen)6, (ftnlen)5, (ftnlen)32);
		    gcpool_(look2, &c__1, &c__1, &n, frname, &fnd, (ftnlen)32,
			     frname_len);
		    if (! fnd) {
			setmsg_("Invalid frame specification found in kernel"
				" pool: frame class keyword is # but associat"
				"ed frame name assignment was not found.", (
				ftnlen)126);
			errch_("#", kvbuff + (((i__2 = i__ - 1) < 100 && 0 <= 
				i__2 ? i__2 : s_rnge("kvbuff", i__2, "framex_"
				, (ftnlen)2356)) << 5), (ftnlen)1, (ftnlen)32)
				;
			sigerr_("SPICE(INVALIDFRAMEDEF)", (ftnlen)22);
			chkout_("CCIFRM", (ftnlen)6);
			return 0;
		    }

/*                 We could extract the frame ID code from KVBUFF(I), but */
/*                 instead we'll make sure that the ID is defined in the */
/*                 kernel pool. */

		    s_copy(look2, frname, (ftnlen)32, frname_len);
		    prefix_("FRAME_", &c__0, look2, (ftnlen)6, (ftnlen)32);
		    gipool_(look2, &c__1, &c__1, &n, frcode, &fnd, (ftnlen)32)
			    ;
		    if (! fnd) {
			setmsg_("Invalid frame specification found in kernel"
				" pool: frame name is is # but associated fra"
				"me ID assignment was not found.", (ftnlen)118)
				;
			errch_("#", frname, (ftnlen)1, frname_len);
			sigerr_("SPICE(INVALIDFRAMEDEF)", (ftnlen)22);
			chkout_("CCIFRM", (ftnlen)6);
			return 0;
		    }

/*                 Look up the frame center. Whether the frame center */
/*                 has been specified by name or ID code, the ID code */
/*                 will be returned by ZZDYNBID. */

		    zzdynbid_(frname, frcode, "CENTER", cent, frname_len, (
			    ftnlen)6);

/*                 As long as we looked up the center successfully, */
/*                 we're done. */

		    if (! failed_()) {
			*found = TRUE_;
		    }

/*                 Exit here, whether or not we looked up the frame's */
/*                 center succesfully. */

		    chkout_("CCIFRM", (ftnlen)6);
		    return 0;
		}
	    }

/*           Getting to this point means we didn't have a match; */
/*           examine the next buffer entry. */

	}

/*        Get the next buffer full of frame class keywords. */

	start += n;
	gnpool_(lookup, &start, &c__100, &n, kvbuff, &fnd, (ftnlen)32, (
		ftnlen)32);
    }

/*     Getting to this point means we didn't find a matching */
/*     frame specification in the kernel pool. Check the built-in */
/*     frame list. Unfortunately, this is a linear search. */

    for (i__ = 1; i__ <= 121; ++i__) {
	if (type__[(i__1 = i__ - 1) < 121 && 0 <= i__1 ? i__1 : s_rnge("type",
		 i__1, "framex_", (ftnlen)2434)] == *class__ && typeid[(i__2 =
		 i__ - 1) < 121 && 0 <= i__2 ? i__2 : s_rnge("typeid", i__2, 
		"framex_", (ftnlen)2434)] == *clssid) {

/*           We have a match. Assign the output arguments and return. */

	    s_copy(frname, name__ + (((i__1 = i__ - 1) < 121 && 0 <= i__1 ? 
		    i__1 : s_rnge("name", i__1, "framex_", (ftnlen)2439)) << 
		    5), frname_len, (ftnlen)32);
	    *frcode = idcode[(i__1 = i__ - 1) < 121 && 0 <= i__1 ? i__1 : 
		    s_rnge("idcode", i__1, "framex_", (ftnlen)2440)];
	    *cent = center[(i__1 = i__ - 1) < 121 && 0 <= i__1 ? i__1 : 
		    s_rnge("center", i__1, "framex_", (ftnlen)2441)];
	    *found = TRUE_;
	    chkout_("CCIFRM", (ftnlen)6);
	    return 0;
	}
    }

/*     We drop down to this point only if no matching frame was found. */
/*     The FOUND flag has already been set to .FALSE. */

    chkout_("CCIFRM", (ftnlen)6);
    return 0;
} /* framex_ */

/* Subroutine */ int framex_(char *cname, char *frname, integer *frcode, 
	integer *cent, integer *class__, integer *clssid, logical *found, 
	ftnlen cname_len, ftnlen frname_len)
{
    return framex_0_(0, cname, frname, frcode, cent, class__, clssid, found, 
	    cname_len, frname_len);
    }

/* Subroutine */ int namfrm_(char *frname, integer *frcode, ftnlen frname_len)
{
    return framex_0_(1, (char *)0, frname, frcode, (integer *)0, (integer *)0,
	     (integer *)0, (logical *)0, (ftnint)0, frname_len);
    }

/* Subroutine */ int frmnam_(integer *frcode, char *frname, ftnlen frname_len)
{
    return framex_0_(2, (char *)0, frname, frcode, (integer *)0, (integer *)0,
	     (integer *)0, (logical *)0, (ftnint)0, frname_len);
    }

/* Subroutine */ int frinfo_(integer *frcode, integer *cent, integer *class__,
	 integer *clssid, logical *found)
{
    return framex_0_(3, (char *)0, (char *)0, frcode, cent, class__, clssid, 
	    found, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int cidfrm_(integer *cent, integer *frcode, char *frname, 
	logical *found, ftnlen frname_len)
{
    return framex_0_(4, (char *)0, frname, frcode, cent, (integer *)0, (
	    integer *)0, found, (ftnint)0, frname_len);
    }

/* Subroutine */ int cnmfrm_(char *cname, integer *frcode, char *frname, 
	logical *found, ftnlen cname_len, ftnlen frname_len)
{
    return framex_0_(5, cname, frname, frcode, (integer *)0, (integer *)0, (
	    integer *)0, found, cname_len, frname_len);
    }

/* Subroutine */ int ccifrm_(integer *class__, integer *clssid, integer *
	frcode, char *frname, integer *cent, logical *found, ftnlen 
	frname_len)
{
    return framex_0_(6, (char *)0, frname, frcode, cent, class__, clssid, 
	    found, (ftnint)0, frname_len);
    }

