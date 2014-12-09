/* subslr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static integer c__2 = 2;
static integer c__3 = 3;
static integer c__10 = 10;

/* $Procedure SUBSLR ( Sub-solar point ) */
/* Subroutine */ int subslr_(char *method, char *target, doublereal *et, char 
	*fixref, char *abcorr, char *obsrvr, doublereal *spoint, doublereal *
	trgepc, doublereal *srfvec, ftnlen method_len, ftnlen target_len, 
	ftnlen fixref_len, ftnlen abcorr_len, ftnlen obsrvr_len)
{
    /* Initialized data */

    static logical elipsd = TRUE_;
    static logical first = TRUE_;
    static logical near__ = TRUE_;
    static char prvcor[5] = "     ";
    static char prvmth[80] = "Near point: Ellipsoid                         "
	    "                                  ";

    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int vadd_(doublereal *, doublereal *, doublereal *
	    ), zzbods2c_(integer *, char *, integer *, logical *, char *, 
	    integer *, logical *, ftnlen, ftnlen);
    doublereal sdir[3];
    integer nitr;
    extern /* Subroutine */ int vsub_(doublereal *, doublereal *, doublereal *
	    ), vequ_(doublereal *, doublereal *);
    integer type__;
    static logical xmit;
    doublereal spos[3], tpos[3];
    extern /* Subroutine */ int zznamfrm_(integer *, char *, integer *, char *
	    , integer *, ftnlen, ftnlen), zzvalcor_(char *, logical *, ftnlen)
	    ;
    doublereal j2pos[3];
    extern /* Subroutine */ int zzctruin_(integer *);
    integer i__;
    doublereal s, radii[3];
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    static logical usecn;
    extern doublereal vdist_(doublereal *, doublereal *);
    doublereal xform[9]	/* was [3][3] */;
    static logical uselt;
    char words[32*2];
    doublereal sunst[6];
    static logical svfnd1, svfnd2;
    static integer svctr1[2], svctr2[2];
    extern logical failed_(void);
    static integer svctr3[2];
    integer refcde;
    doublereal lt, etdiff;
    integer obscde;
    extern /* Subroutine */ int bodvcd_(integer *, char *, integer *, integer 
	    *, doublereal *, ftnlen);
    integer nw, nradii;
    doublereal ltdiff;
    extern doublereal clight_(void);
    integer trgcde, center;
    extern doublereal touchd_(doublereal *);
    char locmth[80];
    doublereal obsrng;
    integer typeid;
    logical attblk[15];
    static integer svtcde;
    doublereal altsun, obspos[3];
    extern logical return_(void);
    doublereal prevet, prevlt, ssbost[6], ssbtst[6], sslrlt;
    static char svtarg[36], svobsr[36];
    static integer svobsc;
    doublereal sslrst[6];
    static char svfref[32];
    static integer svrefc;
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen), sigerr_(char *, ftnlen), frinfo_(integer *, integer *, 
	    integer *, integer *, logical *), errint_(char *, integer *, 
	    ftnlen), ljucrs_(integer *, char *, char *, ftnlen, ftnlen), 
	    lparse_(char *, char *, integer *, integer *, char *, ftnlen, 
	    ftnlen, ftnlen), spkezp_(integer *, doublereal *, char *, char *, 
	    integer *, doublereal *, doublereal *, ftnlen, ftnlen), vminus_(
	    doublereal *, doublereal *), nearpt_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *), surfpt_(
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, logical *), spkssb_(integer *, 
	    doublereal *, char *, doublereal *, ftnlen), pxform_(char *, char 
	    *, doublereal *, doublereal *, ftnlen, ftnlen), spkcpo_(char *, 
	    doublereal *, char *, char *, char *, doublereal *, char *, char *
	    , doublereal *, doublereal *, ftnlen, ftnlen, ftnlen, ftnlen, 
	    ftnlen, ftnlen), spkcpt_(doublereal *, char *, char *, doublereal 
	    *, char *, char *, char *, char *, doublereal *, doublereal *, 
	    ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen);
    logical fnd;
    doublereal slt;
    extern /* Subroutine */ int mxv_(doublereal *, doublereal *, doublereal *)
	    ;

/* $ Abstract */

/*     Compute the rectangular coordinates of the sub-solar point on */
/*     a target body at a specified epoch, optionally corrected for */
/*     light time and stellar aberration. */

/*     This routine supersedes SUBSOL. */

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
/*     NAIF_IDS */
/*     PCK */
/*     SPK */
/*     TIME */

/* $ Keywords */

/*     GEOMETRY */

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

/*     Include file zzabcorr.inc */

/*     SPICE private file intended solely for the support of SPICE */
/*     routines.  Users should not include this file directly due */
/*     to the volatile nature of this file */

/*     The parameters below define the structure of an aberration */
/*     correction attribute block. */

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

/*     An aberration correction attribute block is an array of logical */
/*     flags indicating the attributes of the aberration correction */
/*     specified by an aberration correction string.  The attributes */
/*     are: */

/*        - Is the correction "geometric"? */

/*        - Is light time correction indicated? */

/*        - Is stellar aberration correction indicated? */

/*        - Is the light time correction of the "converged */
/*          Newtonian" variety? */

/*        - Is the correction for the transmission case? */

/*        - Is the correction relativistic? */

/*    The parameters defining the structure of the block are as */
/*    follows: */

/*       NABCOR    Number of aberration correction choices. */

/*       ABATSZ    Number of elements in the aberration correction */
/*                 block. */

/*       GEOIDX    Index in block of geometric correction flag. */

/*       LTIDX     Index of light time flag. */

/*       STLIDX    Index of stellar aberration flag. */

/*       CNVIDX    Index of converged Newtonian flag. */

/*       XMTIDX    Index of transmission flag. */

/*       RELIDX    Index of relativistic flag. */

/*    The following parameter is not required to define the block */
/*    structure, but it is convenient to include it here: */

/*       CORLEN    The maximum string length required by any aberration */
/*                 correction string */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 18-DEC-2004 (NJB) */

/* -& */
/*     Number of aberration correction choices: */


/*     Aberration correction attribute block size */
/*     (number of aberration correction attributes): */


/*     Indices of attributes within an aberration correction */
/*     attribute block: */


/*     Maximum length of an aberration correction string: */


/*     End of include file zzabcorr.inc */

/* $ Abstract */

/*     This include file defines the dimension of the counter */
/*     array used by various SPICE subsystems to uniquely identify */
/*     changes in their states. */

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

/*     CTRSIZ      is the dimension of the counter array used by */
/*                 various SPICE subsystems to uniquely identify */
/*                 changes in their states. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS) */

/* -& */

/*     End of include file. */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     METHOD     I   Computation method. */
/*     TARGET     I   Name of target body. */
/*     ET         I   Epoch in ephemeris seconds past J2000 TDB. */
/*     FIXREF     I   Body-fixed, body-centered target body frame. */
/*     ABCORR     I   Aberration correction. */
/*     OBSRVR     I   Name of observing body. */
/*     SPOINT     O   Sub-solar point on the target body. */
/*     TRGEPC     O   Sub-solar point epoch. */
/*     SRFVEC     O   Vector from observer to sub-solar point. */

/* $ Detailed_Input */

/*     METHOD      is a short string providing parameters defining */
/*                 the computation method to be used. */

/*                 The supported values of METHOD are listed below. */
/*                 Please note that the colon is a required delimiter; */
/*                 using a blank will not work. */

/*                    'Near point: ellipsoid'   The sub-solar point */
/*                                              computation uses a */
/*                                              triaxial ellipsoid to */
/*                                              model the surface of the */
/*                                              target body. The */
/*                                              sub-solar point is */
/*                                              defined as the nearest */
/*                                              point on the target */
/*                                              relative to the Sun. */

/*                    'Intercept: ellipsoid'    The sub-solar point */
/*                                              computation uses a */
/*                                              triaxial ellipsoid to */
/*                                              model the surface of the */
/*                                              target body. The */
/*                                              sub-solar point is */
/*                                              defined as the target */
/*                                              surface intercept of the */
/*                                              line containing the Sun */
/*                                              and the target's center. */

/*                 Neither case nor white space are significant in */
/*                 METHOD. For example, the string */

/*                   ' nearpoint:ELLIPSOID ' */

/*                 is valid. */


/*     TARGET      is the name of the target body. The target body is */
/*                 an ephemeris object (its trajectory is given by */
/*                 SPK data), and is an extended object. */

/*                 The string TARGET is case-insensitive, and leading */
/*                 and trailing blanks in TARGET are not significant. */
/*                 Optionally, you may supply a string containing the */
/*                 integer ID code for the object. For example both */
/*                 'MOON' and '301' are legitimate strings that indicate */
/*                 the Moon is the target body. */

/*                 When the target body's surface is represented by a */
/*                 tri-axial ellipsoid, this routine assumes that a */
/*                 kernel variable representing the ellipsoid's radii is */
/*                 present in the kernel pool. Normally the kernel */
/*                 variable would be defined by loading a PCK file. */


/*     ET          is the epoch of participation of the observer, */
/*                 expressed as ephemeris seconds past J2000 TDB: ET is */
/*                 the epoch at which the observer's state is computed. */

/*                 When aberration corrections are not used, ET is also */
/*                 the epoch at which the position and orientation of */
/*                 the target body and the position of the Sun are */
/*                 computed. */

/*                 When aberration corrections are used, ET is the epoch */
/*                 at which the observer's state relative to the solar */
/*                 system barycenter is computed; in this case the */
/*                 position and orientation of the target body are */
/*                 computed at ET-LT, where LT is the one-way light time */
/*                 between the sub-solar point and the observer. See the */
/*                 description of ABCORR below for details. */


/*     FIXREF      is the name of the body-fixed, body-centered */
/*                 reference frame associated with the target body. The */
/*                 output sub-solar point SPOINT will be expressed */
/*                 relative to this reference frame. The string FIXREF */
/*                 is case-insensitive, and leading and trailing blanks */
/*                 in FIXREF are not significant. */


/*     ABCORR      indicates the aberration correction to be applied */
/*                 when computing the target position and orientation */
/*                 and the position of the Sun. */

/*                 For remote sensing applications, where the apparent */
/*                 sub-solar point seen by the observer is desired, */
/*                 normally either of the corrections */

/*                    'LT+S' */
/*                    'CN+S' */

/*                 should be used. These and the other supported options */
/*                 are described below. ABCORR may be any of the */
/*                 following: */

/*                    'NONE'     Apply no correction. Return the */
/*                               geometric sub-solar point on the target */
/*                               body. */

/*                 Let LT represent the one-way light time between the */
/*                 observer and the sub-solar point (note: NOT between */
/*                 the observer and the target body's center). The */
/*                 following values of ABCORR apply to the "reception" */
/*                 case in which photons depart from the sub-solar */
/*                 point's location at the light-time corrected epoch */
/*                 ET-LT and *arrive* at the observer's location at ET: */

/*                    'LT'       Correct for one-way light time (also */
/*                               called "planetary aberration") using a */
/*                               Newtonian formulation. This correction */
/*                               yields the location of sub-solar */
/*                               point at the moment it emitted photons */
/*                               arriving at the observer at ET. */

/*                               The light time correction uses an */
/*                               iterative solution of the light time */
/*                               equation. The solution invoked by the */
/*                               'LT' option uses one iteration. */

/*                               The target position and orientation as */
/*                               seen by the observer are corrected for */
/*                               light time. The position of the Sun */
/*                               relative to the target is corrected for */
/*                               one-way light time between the Sun and */
/*                               target. */

/*                    'LT+S'     Correct for one-way light time and */
/*                               stellar aberration using a Newtonian */
/*                               formulation. This option modifies the */
/*                               sub-solar point obtained with the 'LT' */
/*                               option to account for the observer's */
/*                               velocity relative to the solar system */
/*                               barycenter. These corrections yield */
/*                               the apparent sub-solar point. */

/*                    'CN'       Converged Newtonian light time */
/*                               correction. In solving the light time */
/*                               equation, the 'CN' correction iterates */
/*                               until the solution converges. Both the */
/*                               position and rotation of the target */
/*                               body, and the position of the Sun, are */
/*                               corrected for light time. */

/*                    'CN+S'     Converged Newtonian light time and */
/*                               stellar aberration corrections. This */
/*                               option produces a solution that is at */
/*                               least as accurate at that obtainable */
/*                               with the 'LT+S' option. Whether the */
/*                               'CN+S' solution is substantially more */
/*                               accurate depends on the geometry of the */
/*                               participating objects and on the */
/*                               accuracy of the input data. In all */
/*                               cases this routine will execute more */
/*                               slowly when a converged solution is */
/*                               computed. */

/*                 Neither case nor white space are significant in */
/*                 ABCORR. For example, the string */

/*                   'Lt + s' */

/*                 is valid. */


/*     OBSRVR      is the name of the observing body. The observing body */
/*                 is an ephemeris object: it typically is a spacecraft, */
/*                 the earth, or a surface point on the earth. OBSRVR is */
/*                 case-insensitive, and leading and trailing blanks in */
/*                 OBSRVR are not significant. Optionally, you may */
/*                 supply a string containing the integer ID code for */
/*                 the object. For example both 'MOON' and '301' are */
/*                 legitimate strings that indicate the Moon is the */
/*                 observer. */

/*                 The observer may coincide with the target. */

/* $ Detailed_Output */


/*     SPOINT      is the sub-solar point on the target body. */

/*                 The sub-solar point is defined either as the point */
/*                 on the target body that is closest to the Sun, */
/*                 or the target surface intercept of the line from the */
/*                 Sun to the target's center; the input argument */
/*                 METHOD selects the definition to be used. */

/*                 SPOINT is expressed in Cartesian coordinates, */
/*                 relative to the body-fixed target frame designated by */
/*                 FIXREF. The body-fixed target frame is evaluated at */
/*                 the sub-solar point epoch TRGEPC (see description */
/*                 below). */

/*                 When aberration corrections are used, SPOINT is */
/*                 computed using target body position and orientation */
/*                 that have been adjusted for the corrections */
/*                 applicable to SPOINT itself rather than to the target */
/*                 body's center. In particular, if the stellar */
/*                 aberration correction applicable to SPOINT is */
/*                 represented by a shift vector S, then the light-time */
/*                 corrected position of the target is shifted by S */
/*                 before the sub-solar point is computed. */

/*                 The components of SPOINT have units of km. */


/*     TRGEPC      is the "sub-solar point epoch." TRGEPC is defined as */
/*                 follows: letting LT be the one-way light time between */
/*                 the observer and the sub-solar point, TRGEPC is */
/*                 either the epoch ET-LT or ET depending on whether the */
/*                 requested aberration correction is, respectively, for */
/*                 received radiation or omitted. LT is computed using */
/*                 the method indicated by ABCORR. */

/*                 TRGEPC is expressed as seconds past J2000 TDB. */


/*     SRFVEC      is the vector from the observer's position at ET to */
/*                 the aberration-corrected (or optionally, geometric) */
/*                 position of SPOINT, where the aberration corrections */
/*                 are specified by ABCORR. SRFVEC is expressed in the */
/*                 target body-fixed reference frame designated by */
/*                 FIXREF, evaluated at TRGEPC. */

/*                 The components of SRFVEC are given in units of km. */

/*                 One can use the SPICELIB function VNORM to obtain the */
/*                 distance between the observer and SPOINT: */

/*                    DIST = VNORM ( SRFVEC ) */

/*                 The observer's position OBSPOS, relative to the */
/*                 target body's center, where the center's position is */
/*                 corrected for aberration effects as indicated by */
/*                 ABCORR, can be computed via the call: */

/*                    CALL VSUB ( SPOINT, SRFVEC, OBSPOS ) */

/*                 To transform the vector SRFVEC from a reference frame */
/*                 FIXREF at time TRGEPC to a time-dependent reference */
/*                 frame REF at time ET, the routine PXFRM2 should be */
/*                 called. Let XFORM be the 3x3 matrix representing the */
/*                 rotation from the reference frame FIXREF at time */
/*                 TRGEPC to the reference frame REF at time ET. Then */
/*                 SRFVEC can be transformed to the result REFVEC as */
/*                 follows: */

/*                     CALL PXFRM2 ( FIXREF, REF,    TRGEPC, ET, XFORM ) */
/*                     CALL MXV    ( XFORM,  SRFVEC, REFVEC ) */


/* $ Parameters */

/*     None. */

/* $ Exceptions */


/*     1)  If the specified aberration correction is unrecognized, the */
/*         error will be diagnosed and signaled by a routine in the call */
/*         tree of this routine. */

/*     2)  If either the target or observer input strings cannot be */
/*         converted to an integer ID code, the error */
/*         SPICE(IDCODENOTFOUND) is signaled. */

/*     3)  If the input target body-fixed frame FIXREF is not */
/*         recognized, the error SPICE(NOFRAME) is signaled. A frame */
/*         name may fail to be recognized because a required frame */
/*         specification kernel has not been loaded; another cause is a */
/*         misspelling of the frame name. */

/*     4)  If the input frame FIXREF is not centered at the target body, */
/*         the error SPICE(INVALIDFRAME) is signaled. */

/*     5)  If the input argument METHOD is not recognized, the error */
/*         SPICE(INVALIDMETHOD) is signaled. */

/*     6)  If insufficient ephemeris data have been loaded prior to */
/*         calling SUBSLR, the error will be diagnosed and signaled by a */
/*         routine in the call tree of this routine. Note that when */
/*         light time correction is used, sufficient ephemeris data must */
/*         be available to propagate the states of observer, target, and */
/*         the Sun to the solar system barycenter. */

/*     7)  If the computation method specifies an ellipsoidal target */
/*         shape and triaxial radii of the target body have not been */
/*         loaded into the kernel pool prior to calling SUBSLR, the */
/*         error will be diagnosed and signaled by a routine in the call */
/*         tree of this routine. */

/*     8)  The target must be an extended body: if any of the radii of */
/*         the target body are non-positive, the error will be */
/*         diagnosed and signaled by routines in the call tree of this */
/*         routine. */

/*     9)  If PCK data specifying the target body-fixed frame */
/*         orientation have not been loaded prior to calling SUBSLR, */
/*         the error will be diagnosed and signaled by a routine in the */
/*         call tree of this routine. */

/* $ Files */

/*     Appropriate kernels must be loaded by the calling program before */
/*     this routine is called. */

/*     The following data are required: */

/*        - SPK data: ephemeris data for target, observer, and */
/*          Sun must be loaded. If aberration corrections are used, the */
/*          states of target, observer, and the Sun relative to the */
/*          solar system barycenter must be calculable from the */
/*          available ephemeris data. Typically ephemeris data are made */
/*          available by loading one or more SPK files via FURNSH. */

/*        - PCK data: if the target body shape is modeled as an */
/*          ellipsoid, triaxial radii for the target body must be loaded */
/*          into the kernel pool. Typically this is done by loading a */
/*          text PCK file via FURNSH. */

/*        - Further PCK data: rotation data for the target body must be */
/*          loaded. These may be provided in a text or binary PCK file. */

/*        - Frame data: if a frame definition is required to convert the */
/*          observer and target states to the body-fixed frame of the */
/*          target, that definition must be available in the kernel */
/*          pool. Typically the definition is supplied by loading a */
/*          frame kernel via FURNSH. */

/*     In all cases, kernel data are normally loaded once per program */
/*     run, NOT every time this routine is called. */

/* $ Particulars */

/*     There are two different popular ways to define the sub-solar */
/*     point: "nearest point on target to the Sun" or "target surface */
/*     intercept of the line containing the Sun and target." These */
/*     coincide when the target is spherical and generally are distinct */
/*     otherwise. */

/*     This routine computes light time corrections using light time */
/*     between the observer and the sub-solar point, as opposed to the */
/*     center of the target. Similarly, stellar aberration corrections */
/*     done by this routine are based on the direction of the vector */
/*     from the observer to the light-time corrected sub-solar point, */
/*     not to the target center. This technique avoids errors due to the */
/*     differential between aberration corrections across the target */
/*     body. Therefore it's valid to use aberration corrections with */
/*     this routine even when the observer is very close to the */
/*     sub-solar point, in particular when the observer to sub-solar */
/*     point distance is much less than the observer to target center */
/*     distance. */

/*     The definition of the aberration-corrected sub-solar point is */
/*     implicit: SPOINT is defined by an equation of the general form */

/*        SPOINT = F ( SPOINT ) */

/*     Because of the contraction properties of both light time and */
/*     stellar aberration corrections---that is, the difference in the */
/*     corrections for two vectors is much smaller than the difference */
/*     between the vectors themselves---it's easy to solve this equation */
/*     accurately and fairly quickly. */

/*     When comparing sub-solar point computations with results from */
/*     sources other than SPICE, it's essential to make sure the same */
/*     geometric definitions are used. */

/* $ Examples */


/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as input, */
/*     the compiler and supporting libraries, and the machine specific */
/*     arithmetic implementation. */


/*     1) Find the sub-solar point on Mars as seen from the Earth for a */
/*        specified time. Perform the computation twice, using both the */
/*        "intercept" and "near point" options. Display the locations of */
/*        the Sun and the sub-solar point using both planetocentric */
/*        and planetographic coordinates. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */

/*           KPL/MK */

/*           This meta-kernel is intended to support operation of SPICE */
/*           example programs. The kernels shown here should not be */
/*           assumed to contain adequate or correct versions of data */
/*           required by SPICE-based user applications. */

/*           In order for an application to use this meta-kernel, the */
/*           kernels referenced here must be present in the user's */
/*           current working directory. */


/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'de421.bsp', */
/*                                  'pck00010.tpc', */
/*                                  'naif0010.tls'  ) */

/*           \begintext */


/*       Example code begins here. */


/*           PROGRAM EX1 */
/*           IMPLICIT NONE */
/*     C */
/*     C     SPICELIB functions */
/*     C */
/*           DOUBLE PRECISION      DPR */
/*     C */
/*     C     Local parameters */
/*     C */
/*           CHARACTER*(*)         META */
/*           PARAMETER           ( META   = 'subslr.tm' ) */

/*           CHARACTER*(*)         FM */
/*           PARAMETER           ( FM     =  '(A,F21.9)' ) */

/*           INTEGER               MTHLEN */
/*           PARAMETER           ( MTHLEN = 50 ) */
/*     C */
/*     C     Local variables */
/*     C */
/*           CHARACTER*(MTHLEN)    METHOD ( 2 ) */

/*           DOUBLE PRECISION      ET */
/*           DOUBLE PRECISION      F */
/*           DOUBLE PRECISION      RADII  ( 3 ) */
/*           DOUBLE PRECISION      RE */
/*           DOUBLE PRECISION      RP */
/*           DOUBLE PRECISION      SPCLAT */
/*           DOUBLE PRECISION      SPCLON */
/*           DOUBLE PRECISION      SPCRAD */
/*           DOUBLE PRECISION      SPGALT */
/*           DOUBLE PRECISION      SPGLAT */
/*           DOUBLE PRECISION      SPGLON */
/*           DOUBLE PRECISION      SPOINT ( 3 ) */
/*           DOUBLE PRECISION      SRFVEC ( 3 ) */
/*           DOUBLE PRECISION      SUNLT */
/*           DOUBLE PRECISION      SUNPOS ( 3 ) */
/*           DOUBLE PRECISION      SUNST  ( 6 ) */
/*           DOUBLE PRECISION      SUPCLN */
/*           DOUBLE PRECISION      SUPCLT */
/*           DOUBLE PRECISION      SUPCRD */
/*           DOUBLE PRECISION      SUPGAL */
/*           DOUBLE PRECISION      SUPGLN */
/*           DOUBLE PRECISION      SUPGLT */
/*           DOUBLE PRECISION      TRGEPC */

/*           INTEGER               I */
/*           INTEGER               N */
/*     C */
/*     C     Saved variables */
/*     C */
/*           SAVE                  METHOD */
/*     C */
/*     C     Initial values */
/*     C */
/*           DATA                  METHOD / 'Intercept:  ellipsoid', */
/*          .                               'Near point: ellipsoid' / */
/*     C */
/*     C     Load kernel files via the meta-kernel. */
/*     C */
/*           CALL FURNSH ( META ) */

/*     C */
/*     C     Convert the UTC request time to ET (seconds past */
/*     C     J2000, TDB). */
/*     C */
/*           CALL STR2ET ( '2008 AUG 11 00:00:00', ET ) */

/*     C */
/*     C     Look up the target body's radii. We'll use these to */
/*     C     convert Cartesian to planetographic coordinates. Use */
/*     C     the radii to compute the flattening coefficient of */
/*     C     the reference ellipsoid. */
/*     C */
/*           CALL BODVRD ( 'MARS', 'RADII', 3, N, RADII ) */

/*     C */
/*     C     Let RE and RP be, respectively, the equatorial and */
/*     C     polar radii of the target. */
/*     C */
/*           RE = RADII( 1 ) */
/*           RP = RADII( 3 ) */

/*           F  = ( RE - RP ) / RE */

/*     C */
/*     C     Compute the sub-solar point using light time and stellar */
/*     C     aberration corrections. Use the "target surface intercept" */
/*     C     definition of sub-solar point on the first loop */
/*     C     iteration, and use the "near point" definition on the */
/*     C     second. */
/*     C */
/*           DO I = 1, 2 */

/*              CALL SUBSLR ( METHOD(I), */
/*          .                'MARS',  ET,     'IAU_MARS', 'LT+S', */
/*          .                'EARTH', SPOINT, TRGEPC,     SRFVEC ) */
/*     C */
/*     C        Convert the sub-solar point's rectangular coordinates */
/*     C        to planetographic longitude, latitude and altitude. */
/*     C        Convert radians to degrees. */
/*     C */
/*              CALL RECPGR ( 'MARS', SPOINT, RE,    F, */
/*          .                 SPGLON, SPGLAT, SPGALT   ) */

/*              SPGLON = SPGLON * DPR () */
/*              SPGLAT = SPGLAT * DPR () */

/*     C */
/*     C        Convert sub-solar point's rectangular coordinates to */
/*     C        planetocentric radius, longitude, and latitude. Convert */
/*     C        radians to degrees. */
/*     C */
/*              CALL RECLAT ( SPOINT, SPCRAD, SPCLON, SPCLAT ) */

/*              SPCLON = SPCLON * DPR () */
/*              SPCLAT = SPCLAT * DPR () */

/*     C */
/*     C        Compute the Sun's apparent position relative to the */
/*     C        sub-solar point at TRGEPC. Add the position of */
/*     C        the sub-solar point relative to the target's center */
/*     C        to obtain the position of the sun relative to the */
/*     C        target's center. Express the latter position in */
/*     C        planetographic coordinates. */
/*     C */
/*              CALL SPKCPO ( 'SUN',  TRGEPC, 'IAU_MARS', 'OBSERVER', */
/*          .                 'LT+S', SPOINT, 'MARS',     'IAU_MARS', */
/*          .                 SUNST,  SUNLT                            ) */

/*              CALL VADD ( SUNST, SPOINT, SUNPOS ) */

/*              CALL RECPGR ( 'MARS', SUNPOS, RE,    F, */
/*          .                 SUPGLN, SUPGLT, SUPGAL   ) */

/*              SUPGLN = SUPGLN * DPR () */
/*              SUPGLT = SUPGLT * DPR () */

/*     C */
/*     C        Convert the Sun's rectangular coordinates to */
/*     C        planetocentric radius, longitude, and latitude. */
/*     C        Convert radians to degrees. */
/*     C */
/*              CALL RECLAT ( SUNPOS, SUPCRD, SUPCLN, SUPCLT ) */

/*              SUPCLN = SUPCLN * DPR () */
/*              SUPCLT = SUPCLT * DPR () */

/*     C */
/*     C        Write the results. */
/*     C */
/*              WRITE(*,FM) ' ' */
/*              WRITE(*,* ) 'Computation method = ', METHOD(I) */
/*              WRITE(*,FM) ' ' */
/*              WRITE(*,FM) */
/*          .   '  Sub-solar point altitude            (km) = ', SPGALT */
/*              WRITE(*,FM) */
/*          .   '  Sub-solar planetographic longitude (deg) = ', SPGLON */
/*              WRITE(*,FM) */
/*          .   '  Sun''s planetographic longitude     (deg) = ', SUPGLN */
/*              WRITE(*,FM) */
/*          .   '  Sub-solar planetographic latitude  (deg) = ', SPGLAT */
/*              WRITE(*,FM) */
/*          .   '  Sun''s planetographic latitude      (deg) = ', SUPGLT */
/*              WRITE(*,FM) */
/*          .   '  Sub-solar planetocentric longitude (deg) = ', SPCLON */
/*              WRITE(*,FM) */
/*          .   '  Sun''s planetocentric longitude     (deg) = ', SUPCLN */
/*              WRITE(*,FM) */
/*          .   '  Sub-solar planetocentric latitude  (deg) = ', SPCLAT */
/*              WRITE(*,FM) */
/*          .   '  Sun''s planetocentric latitude      (deg) = ', SUPCLT */
/*              WRITE(*,FM) ' ' */

/*           END DO */

/*           END */


/*     When this program was executed on a PC/Linux/gfortran platform, */
/*     the output was: */


/*      Computation method = Intercept:  ellipsoid */

/*       Sub-solar point altitude            (km) =           0.000000000 */
/*       Sub-solar planetographic longitude (deg) =         175.810675510 */
/*       Sun's planetographic longitude     (deg) =         175.810675508 */
/*       Sub-solar planetographic latitude  (deg) =          23.668550281 */
/*       Sun's planetographic latitude      (deg) =          23.420823362 */
/*       Sub-solar planetocentric longitude (deg) =        -175.810675510 */
/*       Sun's planetocentric longitude     (deg) =        -175.810675508 */
/*       Sub-solar planetocentric latitude  (deg) =          23.420819936 */
/*       Sun's planetocentric latitude      (deg) =          23.420819936 */


/*      Computation method = Near point: ellipsoid */

/*       Sub-solar point altitude            (km) =           0.000000000 */
/*       Sub-solar planetographic longitude (deg) =         175.810675410 */
/*       Sun's planetographic longitude     (deg) =         175.810675408 */
/*       Sub-solar planetographic latitude  (deg) =          23.420823362 */
/*       Sun's planetographic latitude      (deg) =          23.420823362 */
/*       Sub-solar planetocentric longitude (deg) =        -175.810675410 */
/*       Sun's planetocentric longitude     (deg) =        -175.810675408 */
/*       Sub-solar planetocentric latitude  (deg) =          23.175085578 */
/*       Sun's planetocentric latitude      (deg) =          23.420819936 */


/* $ Restrictions */

/*    None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     S.C. Krening   (JPL) */
/*     B.V. Semenov   (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 31-MAR-2014 (NJB) (SCK) (BVS) */

/*        Bug fix: stellar aberration is no longer applied to the */
/*        observer-to-estimated sub-solar point vector while solving for */
/*        the sub-solar point. This correction involved unnecessary code */
/*        but did not affect this routine's outputs. */

/*        Bug fix: FIRST is now set to .FALSE. at the completion of a */
/*        successful initialization pass. This does not affect the */
/*        routine's outputs but improves efficiency. */

/*        Exceptions removed: the observer and target are now */
/*        permitted to coincide. */

/*        Upgrade: the algorithm for finding the apparent state of the */
/*        sun as seen from the estimated sub-solar point has been */
/*        improved. */

/*        Upgrade: this routine now uses ZZVALCOR rather than ZZPRSCOR, */
/*        simplifying the implementation. */

/*        The header example program was updated to reflect the new */
/*        method of computing the apparent sun location, and the set */
/*        of kernels referenced by the example meta-kernel were updated. */
/*        The display of the program's output was updated accordingly. */

/*        References to the new PXFRM2 routine were added, which changed */
/*        the Detailed Output section. */

/*        Updated to save the input body names and ZZBODTRN state */
/*        counters and to do name-ID conversions only if the counters */
/*        have changed. */

/*        Updated to save the input frame name and POOL state counter */
/*        and to do frame name-ID conversion only if the counter has */
/*        changed. */

/*        Updated to call LJUCRS instead of CMPRSS/UCASE. */

/* -    SPICELIB Version 1.1.0, 18-MAY-2010 (NJB) */

/*        Bug fix: calls to FAILED() have been added after */
/*        SPK calls, target radius lookup, near point */
/*        and surface intercept computations. */

/* -    SPICELIB Version 1.0.1, 17-MAR-2009 (NJB) */

/*        Typo correction: changed FIXFRM to FIXREF in header */
/*        documentation. Meta-kernel name suffix was changed to */
/*        ".tm" in header code example. */

/*        Typo correction in Required_Reading, changed */
/*        FRAME to FRAMES. */

/* -    SPICELIB Version 1.0.0, 02-MAR-2008 (NJB) */

/* -& */
/* $ Index_Entries */

/*     find sub-solar point on target body */
/*     find nearest point to sun on target body */

/* -& */
/* $ Revisions */

/*     None. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     This value will become system-dependent when systems */
/*     using 128-bit d.p. numbers are supported by SPICELIB. */
/*     CNVLIM, when added to 1.0D0, should yield 1.0D0. */


/*     Saved body name length. */


/*     Saved frame name length. */


/*     Local variables */


/*     Saved name/ID item declarations. */


/*     Saved frame name/ID item declarations. */


/*     Saved variables */


/*     Saved name/ID items. */


/*     Saved frame name/ID items. */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SUBSLR", (ftnlen)6);

/*     Counter initialization is done separately. */

    if (first) {

/*        Initialize counters. */

	zzctruin_(svctr1);
	zzctruin_(svctr2);
	zzctruin_(svctr3);
    }
    if (first || s_cmp(abcorr, prvcor, abcorr_len, (ftnlen)5) != 0) {

/*        The aberration correction flag differs from the value it */
/*        had on the previous call, if any. Analyze the new flag. */

	zzvalcor_(abcorr, attblk, abcorr_len);
	if (failed_()) {
	    chkout_("SUBSLR", (ftnlen)6);
	    return 0;
	}

/*        The aberration correction flag is recognized; save it. */

	s_copy(prvcor, abcorr, (ftnlen)5, abcorr_len);

/*        Set logical flags indicating the attributes of the requested */
/*        correction: */

/*           XMIT is .TRUE. when the correction is for transmitted */
/*           radiation. */

/*           USELT is .TRUE. when any type of light time correction */
/*           (normal or converged Newtonian) is specified. */

/*           USECN indicates converged Newtonian light time correction. */

/*        The above definitions are consistent with those used by */
/*        ZZPRSCOR. */

	xmit = attblk[4];
	uselt = attblk[1];
	usecn = attblk[3];

/*        Reject an aberration correction flag calling for transmission */
/*        corrections. */

	if (xmit) {
	    setmsg_("Aberration correction flag # calls for transmission-sty"
		    "le corrections.", (ftnlen)70);
	    errch_("#", abcorr, (ftnlen)1, abcorr_len);
	    sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	    chkout_("SUBSLR", (ftnlen)6);
	    return 0;
	}
	first = FALSE_;
    }

/*     Obtain integer codes for the target and observer. */

    zzbods2c_(svctr1, svtarg, &svtcde, &svfnd1, target, &trgcde, &fnd, (
	    ftnlen)36, target_len);
    if (! fnd) {
	setmsg_("The target, '#', is not a recognized name for an ephemeris "
		"object. The cause of this problem may be that you need an up"
		"dated version of the SPICE Toolkit. ", (ftnlen)155);
	errch_("#", target, (ftnlen)1, target_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("SUBSLR", (ftnlen)6);
	return 0;
    }
    zzbods2c_(svctr2, svobsr, &svobsc, &svfnd2, obsrvr, &obscde, &fnd, (
	    ftnlen)36, obsrvr_len);
    if (! fnd) {
	setmsg_("The observer, '#', is not a recognized name for an ephemeri"
		"s object. The cause of this problem may be that you need an "
		"updated version of the SPICE Toolkit. ", (ftnlen)157);
	errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("SUBSLR", (ftnlen)6);
	return 0;
    }

/*     Determine the attributes of the frame designated by FIXREF. */

    zznamfrm_(svctr3, svfref, &svrefc, fixref, &refcde, (ftnlen)32, 
	    fixref_len);
    frinfo_(&refcde, &center, &type__, &typeid, &fnd);
    if (failed_()) {
	chkout_("SUBSLR", (ftnlen)6);
	return 0;
    }
    if (! fnd) {
	setmsg_("Reference frame # is not recognized by the SPICE frame subs"
		"ystem. Possibly a required frame definition kernel has not b"
		"een loaded.", (ftnlen)130);
	errch_("#", fixref, (ftnlen)1, fixref_len);
	sigerr_("SPICE(NOFRAME)", (ftnlen)14);
	chkout_("SUBSLR", (ftnlen)6);
	return 0;
    }

/*     Make sure that FIXREF is centered at the target body's center. */

    if (center != trgcde) {
	setmsg_("Reference frame # is not centered at the target body #. The"
		" ID code of the frame center is #.", (ftnlen)93);
	errch_("#", fixref, (ftnlen)1, fixref_len);
	errch_("#", target, (ftnlen)1, target_len);
	errint_("#", &center, (ftnlen)1);
	sigerr_("SPICE(INVALIDFRAME)", (ftnlen)19);
	chkout_("SUBSLR", (ftnlen)6);
	return 0;
    }

/*     If necessary, parse the method specification. PRVMTH */
/*     and the derived flags NEAR and ELIPSD start out with */
/*     valid values. PRVMTH records the last valid value of */
/*     METHOD; NEAR and ELIPSD are the corresponding flags. */

    if (s_cmp(method, prvmth, method_len, (ftnlen)80) != 0) {

/*        Parse the computation method specification. Work with a local */
/*        copy of the method specification that contains no leading or */
/*        embedded blanks. */

	ljucrs_(&c__0, method, locmth, method_len, (ftnlen)80);
	lparse_(locmth, ":", &c__2, &nw, words, (ftnlen)80, (ftnlen)1, (
		ftnlen)32);
	if (nw != 2) {
	    setmsg_("Computation method argument was <#>; this string must s"
		    "pecify a supported shape model and computation type. See"
		    " the header of SUBSLR for details.", (ftnlen)145);
	    errch_("#", method, (ftnlen)1, method_len);
	    sigerr_("SPICE(INVALIDMETHOD)", (ftnlen)20);
	    chkout_("SUBSLR", (ftnlen)6);
	    return 0;
	}

/*        The text preceding the first delimiter indicates the */
/*        sub-observer point definition: "nearpoint" or "intercept." The */
/*        second word designates the target shape model. Recall that */
/*        we've removed all blanks from the input string, so we won't */
/*        see the string "near point." */

/*        Check the sub-observer point definition. */

	if (s_cmp(words, "NEARPOINT", (ftnlen)32, (ftnlen)9) != 0 && s_cmp(
		words, "INTERCEPT", (ftnlen)32, (ftnlen)9) != 0) {
	    setmsg_("Computation method argument was <#>; this string must s"
		    "pecify a supported shape model and computation type. See"
		    " the header of SUBSLR for details.", (ftnlen)145);
	    errch_("#", method, (ftnlen)1, method_len);
	    sigerr_("SPICE(INVALIDMETHOD)", (ftnlen)20);
	    chkout_("SUBSLR", (ftnlen)6);
	    return 0;
	}

/*        Check the shape specification. */

	if (s_cmp(words + 32, "ELLIPSOID", (ftnlen)32, (ftnlen)9) != 0) {
	    setmsg_("Computation method argument was <#>; this string must s"
		    "pecify a supported shape model and computation type. See"
		    " the header of SUBSLR for details.", (ftnlen)145);
	    errch_("#", method, (ftnlen)1, method_len);
	    sigerr_("SPICE(INVALIDMETHOD)", (ftnlen)20);
	    chkout_("SUBSLR", (ftnlen)6);
	    return 0;
	}

/*        At this point the method specification has passed our tests. */
/*        Use the flag NEAR to indicate whether the computation type is */
/*        "near point." Use the flag ELIPSD to indicate that the shape */
/*        is modeled as an ellipsoid (which is true, for now). */

	near__ = s_cmp(words, "NEARPOINT", (ftnlen)32, (ftnlen)9) == 0;
	elipsd = TRUE_;

/*        Save the current value of METHOD. */

	s_copy(prvmth, method, (ftnlen)80, method_len);
    }

/*     Get the sign S prefixing LT in the expression for TRGEPC. */
/*     When light time correction is not used, setting S = 0 */
/*     allows us to seamlessly set TRGEPC equal to ET. */

    if (uselt) {
	s = -1.;
    } else {
	s = 0.;
    }

/*     Determine the position of the observer in target body-fixed */
/*     coordinates. This is a first estimate. */

/*         -  Call SPKEZP to compute the position of the target body as */
/*            seen from the observing body and the light time (LT) */
/*            between them. We request that the coordinates of POS be */
/*            returned relative to the body fixed reference frame */
/*            associated with the target body, using aberration */
/*            corrections specified by the input argument ABCORR. */

/*         -  Call VMINUS to negate the direction of the vector (OBSPOS) */
/*            so it will be the position of the observer as seen from */
/*            the target body in target body fixed coordinates. */

/*            Note that this result is not the same as the result of */
/*            calling SPKEZP with the target and observer switched. We */
/*            computed the vector FROM the observer TO the target in */
/*            order to get the proper light time and stellar aberration */
/*            corrections (if requested). Now we need the inverse of */
/*            that corrected vector in order to compute the sub-solar */
/*            point. */

    spkezp_(&trgcde, et, fixref, abcorr, &obscde, tpos, &lt, fixref_len, 
	    abcorr_len);
    if (failed_()) {
	chkout_("SUBSLR", (ftnlen)6);
	return 0;
    }

/*     Negate the target's position to obtain the position of the */
/*     observer relative to the target. */

    vminus_(tpos, obspos);

/*     Make a first estimate of the target epoch. */

    *trgepc = *et + s * lt;

/*     Find the sub-solar point and distance from observer to */
/*     sub-solar point using the specified geometric definition. */

    if (elipsd) {

/*        Find the sub-solar point given the target epoch, */
/*        observer-target position, and target body orientation */
/*        we've already computed. If we're not using light */
/*        time correction, this is all we need do. Otherwise, */
/*        our result will give us an initial estimate of the */
/*        target epoch, which we'll then improve. */

/*        Get the radii of the target body from the kernel pool. */

	bodvcd_(&trgcde, "RADII", &c__3, &nradii, radii, (ftnlen)5);
	if (failed_()) {
	    chkout_("SUBSLR", (ftnlen)6);
	    return 0;
	}

/*        Get the position of the Sun SPOS as seen from the target */
/*        in the target body-fixed frame at TRGEPC. */

	spkezp_(&c__10, trgepc, fixref, abcorr, &trgcde, spos, &slt, 
		fixref_len, abcorr_len);
	if (failed_()) {
	    chkout_("SUBSLR", (ftnlen)6);
	    return 0;
	}

/*        Make a first estimate of the sub-solar point. The algorithm */
/*        we use depends on the sub-solar point definition. */

	if (near__) {

/*           Locate the nearest point to the Sun on the target. */

	    nearpt_(spos, radii, &radii[1], &radii[2], spoint, &altsun);
	} else {

/*           Locate the surface intercept of the ray from the */
/*           Sun to the target center. */

	    vminus_(spos, sdir);
	    surfpt_(spos, sdir, radii, &radii[1], &radii[2], spoint, &fnd);
	    if (! fnd) {

/*              If there's no intercept, we have a numerical problem. */

		setmsg_("No intercept of observer-target ray was found.", (
			ftnlen)46);
		sigerr_("SPICE(DEGENERATECASE)", (ftnlen)21);
		chkout_("SUBSLR", (ftnlen)6);
		return 0;
	    }
	}
	if (failed_()) {
	    chkout_("SUBSLR", (ftnlen)6);
	    return 0;
	}
	obsrng = vdist_(obspos, spoint);

/*        Compute the one-way light time and target epoch based on our */
/*        first computation of SPOINT. The coefficient S has been */
/*        set to give us the correct answer for each aberration */
/*        correction case. */

	lt = obsrng / clight_();
	*trgepc = *et + s * lt;

/*        If we're not using light time and stellar aberration */
/*        corrections, we're almost done now. Note that we need only */
/*        check for use of light time corrections, because use of */
/*        stellar aberration corrections alone has been prevented by an */
/*        earlier check. */

	if (! uselt) {

/*           The TRGEPC value we'll return comes from our value of */
/*           OBSRNG computed above. The previous call to SPKEZP call */
/*           yielded the vector OBSPOS. SPOINT was set immediately */
/*           above. The only output left to compute is SRFVEC. */

	    vsub_(spoint, obspos, srfvec);
	    chkout_("SUBSLR", (ftnlen)6);
	    return 0;
	}

/*        We'll now make an improved sub-solar point estimate using the */
/*        previous estimate of the sub-solar point. The number of */
/*        iterations depends on the light time correction type. */

	if (usecn) {
	    nitr = 10;
	} else {
	    nitr = 1;
	}

/*        Get the J2000-relative state of the observer relative to */
/*        the solar system barycenter at ET. */

	spkssb_(&obscde, et, "J2000", ssbost, (ftnlen)5);
	if (failed_()) {
	    chkout_("SUBSLR", (ftnlen)6);
	    return 0;
	}

/*        Initialize the variables required to evaluate the */
/*        loop termination condition. */

	i__ = 0;
	ltdiff = 1.;
	etdiff = 1.;
	prevlt = lt;
	prevet = *trgepc;
	while(i__ < nitr && ltdiff > abs(lt) * 1e-17 && etdiff > 0.) {

/*           Get the J2000-relative state of the target relative to */
/*           the solar system barycenter at the target epoch. */

	    spkssb_(&trgcde, trgepc, "J2000", ssbtst, (ftnlen)5);
	    if (failed_()) {
		chkout_("SUBSLR", (ftnlen)6);
		return 0;
	    }

/*           Find the position of the observer relative to the target. */
/*           Convert this vector from the J2000 frame to the target */
/*           frame at TRGEPC. */

	    vsub_(ssbost, ssbtst, j2pos);
	    pxform_("J2000", fixref, trgepc, xform, (ftnlen)5, fixref_len);
	    if (failed_()) {
		chkout_("SUBSLR", (ftnlen)6);
		return 0;
	    }
	    mxv_(xform, j2pos, obspos);

/*           Note: if we're using stellar aberration correction, we do */
/*           not apply the stellar aberration correction of the */
/*           estimated sub-solar point as seen by the observer to the */
/*           next estimate of the sun-solar point. The location of this */
/*           point depends on the illumination of the target, which is a */
/*           function of the observer-surface point light time. This is */
/*           the only way in which the observer plays a role in */
/*           determining the sub-solar point. */

/*           Stellar aberration of the sun's position relative to the */
/*           sub-solar point *is* used. */

/*           First find the apparent position of the sun */
/*           as seem from the estimated sub-solar point. */

	    spkcpo_("SUN", trgepc, fixref, "OBSERVER", abcorr, spoint, target,
		     fixref, sunst, &slt, (ftnlen)3, fixref_len, (ftnlen)8, 
		    abcorr_len, target_len, fixref_len);

/*           Create the target-center to sun vector. */

	    vadd_(sunst, spoint, spos);
	    if (failed_()) {
		chkout_("SUBSLR", (ftnlen)6);
		return 0;
	    }

/*           Find the sub-solar point using the current estimated */
/*           geometry. */

	    if (near__) {

/*              Locate the nearest point to the observer on the target. */

		nearpt_(spos, radii, &radii[1], &radii[2], spoint, &altsun);
	    } else {

/*              Locate the surface intercept of the ray from the */
/*              Sun to the target center. */

		vminus_(spos, sdir);
		surfpt_(spos, sdir, radii, &radii[1], &radii[2], spoint, &fnd)
			;
		if (! fnd) {

/*                 If there's no intercept, we have a numerical problem. */

		    setmsg_("No intercept of observer-target ray was found.", 
			    (ftnlen)46);
		    sigerr_("SPICE(DEGENERATECASE)", (ftnlen)21);
		    chkout_("SUBSLR", (ftnlen)6);
		    return 0;
		}
	    }
	    if (failed_()) {
		chkout_("SUBSLR", (ftnlen)6);
		return 0;
	    }
	    obsrng = vdist_(obspos, spoint);

/*           Compute a new light time estimate and new target epoch. */

	    lt = obsrng / clight_();
	    *trgepc = *et + s * lt;

/*           At this point, we have new estimates of the sub-solar point */
/*           SPOINT, the observer altitude OBSRNG, the target epoch */
/*           TRGEPC, and the position of the observer relative to the */
/*           target OBSPOS. */

/*           We use the d.p. identity function TOUCHD to force the */
/*           compiler to create double precision arguments from the */
/*           differences LT-PREVLT and TRGEPC-PREVET. Some compilers */
/*           will perform extended-precision register arithmetic, which */
/*           can prevent a difference from rounding to zero. Simply */
/*           storing the result of the subtraction in a double precision */
/*           variable doesn't solve the problem, because that variable */
/*           can be optimized out of existence. */

	    d__2 = lt - prevlt;
	    ltdiff = (d__1 = touchd_(&d__2), abs(d__1));
	    d__2 = *trgepc - prevet;
	    etdiff = (d__1 = touchd_(&d__2), abs(d__1));
	    prevlt = lt;
	    prevet = *trgepc;
	    ++i__;
	}
    } else {

/*        We've already checked the computation method input argument, */
/*        so we don't expect to arrive here. This code is present for */
/*        safety. */

	setmsg_("The computation method # was not recognized. ", (ftnlen)45);
	errch_("#", method, (ftnlen)1, method_len);
	sigerr_("SPICE(INVALIDMETHOD)", (ftnlen)20);
	chkout_("SUBSLR", (ftnlen)6);
	return 0;
    }

/*     SPOINT and TRGEPC have been set. Compute SRFVEC. */

    spkcpt_(spoint, target, fixref, et, fixref, "TARGET", abcorr, obsrvr, 
	    sslrst, &sslrlt, target_len, fixref_len, fixref_len, (ftnlen)6, 
	    abcorr_len, obsrvr_len);
    vequ_(sslrst, srfvec);
    chkout_("SUBSLR", (ftnlen)6);
    return 0;
} /* subslr_ */

