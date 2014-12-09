/* sincpt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static integer c__3 = 3;
static doublereal c_b57 = 1e-14;

/* $Procedure SINCPT ( Surface intercept ) */
/* Subroutine */ int sincpt_(char *method, char *target, doublereal *et, char 
	*fixref, char *abcorr, char *obsrvr, char *dref, doublereal *dvec, 
	doublereal *spoint, doublereal *trgepc, doublereal *srfvec, logical *
	found, ftnlen method_len, ftnlen target_len, ftnlen fixref_len, 
	ftnlen abcorr_len, ftnlen obsrvr_len, ftnlen dref_len)
{
    /* Initialized data */

    static logical first = TRUE_;
    static char loccor[5] = "     ";
    static char prvcor[5] = "     ";
    static logical usecn = FALSE_;
    static logical uselt = FALSE_;
    static logical usestl = FALSE_;
    static logical xmit = FALSE_;

    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int vadd_(doublereal *, doublereal *, doublereal *
	    ), zzbods2c_(integer *, char *, integer *, logical *, char *, 
	    integer *, logical *, ftnlen, ftnlen);
    doublereal dist, udir[3];
    extern /* Subroutine */ int vscl_(doublereal *, doublereal *, doublereal *
	    );
    integer nitr;
    extern doublereal vsep_(doublereal *, doublereal *);
    extern /* Subroutine */ int vsub_(doublereal *, doublereal *, doublereal *
	    ), vequ_(doublereal *, doublereal *);
    doublereal rpos[3], tpos[3], j2dir[3];
    extern /* Subroutine */ int zznamfrm_(integer *, char *, integer *, char *
	    , integer *, ftnlen, ftnlen);
    doublereal j2est[3];
    extern /* Subroutine */ int zzvalcor_(char *, logical *, ftnlen);
    doublereal j2pos[3];
    extern /* Subroutine */ int zzctruin_(integer *);
    integer i__;
    doublereal s, radii[3], range;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    doublereal pnear[3];
    extern doublereal vdist_(doublereal *, doublereal *);
    doublereal xform[9]	/* was [3][3] */;
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    extern doublereal vnorm_(doublereal *);
    extern logical vzero_(doublereal *);
    doublereal j2geom[3], r2jmat[9]	/* was [3][3] */, j2tmat[9]	/* 
	    was [3][3] */;
    static logical svfnd1, svfnd2;
    static integer svctr1[2], svctr2[2];
    extern logical failed_(void);
    static integer svctr3[2], svctr4[2];
    integer dfrcde;
    doublereal lt, etdiff;
    integer fxfcde;
    extern doublereal dasine_(doublereal *, doublereal *);
    doublereal refepc;
    integer nradii, obscde;
    doublereal ltdiff;
    extern doublereal clight_(void);
    integer dclass;
    doublereal maxrad, reject;
    extern doublereal touchd_(doublereal *);
    doublereal ltcent, negpos[3], rayalt, relerr, obspos[3], prevet, srflen, 
	    stldir[3], trgdir[3];
    extern logical return_(void);
    doublereal prevlt, ssbost[6], ssbtst[6], stlerr[3], stltmp[3];
    integer dcentr, dtypid, fxcent, fxclss, fxtyid, trgcde;
    logical attblk[15];
    static char svtarg[36];
    static integer svtcde;
    static char svobsr[36];
    static integer svobsc;
    static char svfref[32];
    static integer svfxfc;
    static char svdref[32];
    static integer svdfrc;
    extern /* Subroutine */ int chkout_(char *, ftnlen), suffix_(char *, 
	    integer *, char *, ftnlen, ftnlen), setmsg_(char *, ftnlen), 
	    sigerr_(char *, ftnlen), frinfo_(integer *, integer *, integer *, 
	    integer *, logical *), errint_(char *, integer *, ftnlen), 
	    spkezp_(integer *, doublereal *, char *, char *, integer *, 
	    doublereal *, doublereal *, ftnlen, ftnlen), vminus_(doublereal *,
	     doublereal *), pxform_(char *, char *, doublereal *, doublereal *
	    , ftnlen, ftnlen), spkssb_(integer *, doublereal *, char *, 
	    doublereal *, ftnlen), stelab_(doublereal *, doublereal *, 
	    doublereal *), stlabx_(doublereal *, doublereal *, doublereal *), 
	    bodvcd_(integer *, char *, integer *, integer *, doublereal *, 
	    ftnlen), surfpt_(doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, logical *), npedln_(
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *), vhatip_(doublereal *);
    logical fnd;
    extern /* Subroutine */ int mxv_(doublereal *, doublereal *, doublereal *)
	    ;

/* $ Abstract */

/*     Given an observer and a direction vector defining a ray, compute */
/*     the surface intercept of the ray on a target body at a specified */
/*     epoch, optionally corrected for light time and stellar */
/*     aberration. */

/*     This routine supersedes SRFXPT. */

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
/*     DREF       I   Reference frame of ray's direction vector. */
/*     DVEC       I   Ray's direction vector. */
/*     SPOINT     O   Surface intercept point on the target body. */
/*     TRGEPC     O   Intercept epoch. */
/*     SRFVEC     O   Vector from observer to intercept point. */
/*     FOUND      O   Flag indicating whether intercept was found. */

/* $ Detailed_Input */

/*     METHOD      is a short string providing parameters defining */
/*                 the computation method to be used. */

/*                 The only choice currently supported is */

/*                    'Ellipsoid'        The intercept computation uses */
/*                                       a triaxial ellipsoid to model */
/*                                       the surface of the target body. */
/*                                       The ellipsoid's radii must be */
/*                                       available in the kernel pool. */

/*                 Neither case nor white space are significant in */
/*                 METHOD. For example, the string ' eLLipsoid ' is */
/*                 valid. */

/*     TARGET      is the name of the target body. TARGET is */
/*                 case-insensitive, and leading and trailing blanks in */
/*                 TARGET are not significant. Optionally, you may */
/*                 supply a string containing the integer ID code */
/*                 for the object. For example both 'MOON' and '301' */
/*                 are legitimate strings that indicate the Moon is the */
/*                 target body. */

/*                 When the target body's surface is represented by a */
/*                 tri-axial ellipsoid, this routine assumes that a */
/*                 kernel variable representing the ellipsoid's radii is */
/*                 present in the kernel pool. Normally the kernel */
/*                 variable would be defined by loading a PCK file. */


/*     ET          is the epoch of participation of the observer, */
/*                 expressed as ephemeris seconds past J2000 TDB: ET is */
/*                 the epoch at which the observer's state is computed. */

/*                 When aberration corrections are not used, ET is also */
/*                 the epoch at which the state and orientation of the */
/*                 target body are computed. */

/*                 When aberration corrections are used, the position */
/*                 and orientation of the target body are computed at */
/*                 ET-LT or ET+LT, where LT is the one-way light time */
/*                 between the intercept point and the observer, and the */
/*                 sign applied to LT depends on the selected */
/*                 correction. See the description of ABCORR below for */
/*                 details. */


/*     FIXREF      is the name of the body-fixed, body-centered */
/*                 reference frame associated with the target body. The */
/*                 output intercept point SPOINT and the observer to */
/*                 intercept vector SRFVEC will be expressed relative to */
/*                 this reference frame. The string FIXREF is */
/*                 case-insensitive, and leading and trailing blanks in */
/*                 FIXREF are not significant. */


/*     ABCORR      indicates the aberration corrections to be applied */
/*                 when computing the target's position and orientation. */

/*                 For remote sensing applications, where the apparent */
/*                 surface intercept point seen by the observer is */
/*                 desired, normally the correction */

/*                    'CN+S' */

/*                 should be used. This and the other supported options */
/*                 are described below. ABCORR may be any of the */
/*                 following: */

/*                    'NONE'     Apply no correction. Return the */
/*                               geometric surface intercept point on the */
/*                               target body. */

/*                 Let LT represent the one-way light time between the */
/*                 observer and the surface intercept point (note: NOT */
/*                 between the observer and the target body's center). */
/*                 The following values of ABCORR apply to the */
/*                 "reception" case in which photons depart from the */
/*                 intercept point's location at the light-time */
/*                 corrected epoch ET-LT and *arrive* at the observer's */
/*                 location at ET: */

/*                    'LT'       Correct for one-way light time (also */
/*                               called "planetary aberration") using a */
/*                               Newtonian formulation. This correction */
/*                               yields the location of the surface */
/*                               intercept point at the moment it */
/*                               emitted photons arriving at the */
/*                               observer at ET. */

/*                               The light time correction uses an */
/*                               iterative solution of the light time */
/*                               equation. The solution invoked by the */
/*                               'LT' option uses one iteration. */

/*                               Both the target position as seen by the */
/*                               observer, and rotation of the target */
/*                               body, are corrected for light time. */

/*                    'LT+S'     Correct for one-way light time and */
/*                               stellar aberration using a Newtonian */
/*                               formulation. This option modifies the */
/*                               surface intercept obtained with the */
/*                               'LT' option to account for the */
/*                               observer's velocity relative to the */
/*                               solar system barycenter. These */
/*                               computations yield the apparent surface */
/*                               intercept point. */

/*                    'CN'       Converged Newtonian light time */
/*                               correction. In solving the light time */
/*                               equation, the 'CN' correction iterates */
/*                               until the solution converges. Both the */
/*                               position and rotation of the target */
/*                               body are corrected for light time. */

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

/*                               For reception-case applications */
/*                               involving intercepts near the target */
/*                               body limb, this option should be used. */

/*                 The following values of ABCORR apply to the */
/*                 "transmission" case in which photons *depart* from */
/*                 the observer's location at ET and arrive at the */
/*                 intercept point at the light-time corrected epoch */
/*                 ET+LT: */

/*                    'XLT'      "Transmission" case: correct for */
/*                               one-way light time using a Newtonian */
/*                               formulation. This correction yields the */
/*                               intercept location at the moment it */
/*                               receives photons emitted from the */
/*                               observer's location at ET. */

/*                               The light time correction uses an */
/*                               iterative solution of the light time */
/*                               equation. The solution invoked by the */
/*                               'XLT' option uses one iteration. */

/*                               Both the target position as seen by the */
/*                               observer, and rotation of the target */
/*                               body, are corrected for light time. */

/*                    'XLT+S'    "Transmission" case: correct for */
/*                               one-way light time and stellar */
/*                               aberration using a Newtonian */
/*                               formulation  This option modifies the */
/*                               intercept obtained with the 'XLT' */
/*                               option to account for the observer's */
/*                               velocity relative to the solar system */
/*                               barycenter. */

/*                    'XCN'      Converged Newtonian light time */
/*                               correction. This is the same as XLT */
/*                               correction but with further iterations */
/*                               to a converged Newtonian light time */
/*                               solution. */

/*                    'XCN+S'    "Transmission" case: converged */
/*                               Newtonian light time and stellar */
/*                               aberration corrections. This option */
/*                               produces a solution that is at least as */
/*                               accurate at that obtainable with the */
/*                               'XLT+S' option. Whether the 'XCN+S' */
/*                               solution is substantially more accurate */
/*                               depends on the geometry of the */
/*                               participating objects and on the */
/*                               accuracy of the input data. In all */
/*                               cases this routine will execute more */
/*                               slowly when a converged solution is */
/*                               computed. */

/*                               For transmission-case applications */
/*                               involving intercepts near the target */
/*                               body limb, this option should be used. */

/*                Case and embedded blanks are not significant in */
/*                ABCORR. For example, the string */

/*                   'Cn + s' */

/*                 is valid. */


/*     OBSRVR      is the name of the observing body. This is typically */
/*                 a spacecraft, the earth, or a surface point on the */
/*                 earth. OBSRVR is case-insensitive, and leading and */
/*                 trailing blanks in OBSRVR are not significant. */
/*                 Optionally, you may supply a string containing the */
/*                 integer ID code for the object. For example both */
/*                 'MOON' and '301' are legitimate strings that indicate */
/*                 the Moon is the observer. */


/*     DREF        is the name of the reference frame relative to which */
/*                 the ray's direction vector is expressed. This may be */
/*                 any frame supported by the SPICE system, including */
/*                 built-in frames (documented in the Frames Required */
/*                 Reading) and frames defined by a loaded frame kernel */
/*                 (FK). The string DREF is case-insensitive, and */
/*                 leading and trailing blanks in DREF are not */
/*                 significant. */

/*                 When DREF designates a non-inertial frame, the */
/*                 orientation of the frame is evaluated at an epoch */
/*                 dependent on the frame's center and, if the center is */
/*                 not the observer, on the selected aberration */
/*                 correction. See the description of the direction */
/*                 vector DVEC for details. */


/*     DVEC        Ray direction vector emanating from the observer. The */
/*                 intercept with the target body's surface of the ray */
/*                 defined by the observer and DVEC is sought. */

/*                 DVEC is specified relative to the reference frame */
/*                 designated by DREF. */

/*                 Non-inertial reference frames are treated as follows: */
/*                 if the center of the frame is at the observer's */
/*                 location, the frame is evaluated at ET. If the */
/*                 frame's center is located elsewhere, then letting */
/*                 LTCENT be the one-way light time between the observer */
/*                 and the central body associated with the frame, the */
/*                 orientation of the frame is evaluated at ET-LTCENT, */
/*                 ET+LTCENT, or ET depending on whether the requested */
/*                 aberration correction is, respectively, for received */
/*                 radiation, transmitted radiation, or is omitted. */
/*                 LTCENT is computed using the method indicated by */
/*                 ABCORR. */


/* $ Detailed_Output */


/*     SPOINT      is the surface intercept point on the target body of */
/*                 the ray defined by the observer and the direction */
/*                 vector. If the ray intersects the target body in */
/*                 multiple points, the selected intersection point is */
/*                 the one closest to the observer. The output argument */
/*                 FOUND (see below) indicates whether an intercept was */
/*                 found. */

/*                 SPOINT is expressed in Cartesian coordinates, */
/*                 relative to the target body-fixed frame designated by */
/*                 FIXREF. The body-fixed target frame is evaluated at */
/*                 the intercept epoch TRGEPC (see description below). */

/*                 When light time correction is used, the duration of */
/*                 light travel between SPOINT to the observer is */
/*                 considered to be the one way light time. When both */
/*                 light time and stellar aberration corrections are */
/*                 used, SPOINT is selected such that, when SPOINT is */
/*                 corrected for light time and stellar aberration, the */
/*                 resulting vector is parallel to SPOINT lies on the */
/*                 ray defined by the observer's location and DVEC. */

/*                 The components of SPOINT are given in units of km. */


/*     TRGEPC      is the "intercept epoch." TRGEPC is defined as */
/*                 follows: letting LT be the one-way light time between */
/*                 the observer and the intercept point, TRGEPC is the */
/*                 epoch ET-LT, ET+LT, or ET depending on whether the */
/*                 requested aberration correction is, respectively, for */
/*                 received radiation, transmitted radiation, or */
/*                 omitted. LT is computed using the method indicated by */
/*                 ABCORR. */

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

/*                 The second example in the Examples header section */
/*                 below presents a complete program that demonstrates */
/*                 this procedure. */


/*     FOUND       A logical flag indicating whether or not the ray */
/*                 intersects the target. If an intersection exists */
/*                 FOUND will be returned as .TRUE. If the ray misses */
/*                 the target, FOUND will be returned as .FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */


/*     1)  If the specified aberration correction is unrecognized, the */
/*         error will be diagnosed and signaled by a routine in the call */
/*         tree of this routine. */

/*     2)  If either the target or observer input strings cannot be */
/*         converted to an integer ID code, the error */
/*         SPICE(IDCODENOTFOUND) is signaled. */

/*     3)  If OBSRVR and TARGET map to the same NAIF integer ID code, */
/*         the error SPICE(BODIESNOTDISTINCT) is signaled. */

/*     4)  If the input target body-fixed frame FIXREF is not */
/*         recognized, the error SPICE(NOFRAME) is signaled. A frame */
/*         name may fail to be recognized because a required frame */
/*         specification kernel has not been loaded; another cause is a */
/*         misspelling of the frame name. */

/*     5)  If the input frame FIXREF is not centered at the target body, */
/*         the error SPICE(INVALIDFRAME) is signaled. */

/*     6)  If the input argument METHOD is not recognized, the error */
/*         SPICE(INVALIDMETHOD) is signaled. */

/*     7)  If the target and observer have distinct identities but are */
/*         at the same location (for example, the target is Mars and the */
/*         observer is the Mars barycenter), the error */
/*         SPICE(NOSEPARATION) is signaled. */

/*     8)  If insufficient ephemeris data have been loaded prior to */
/*         calling SINCPT, the error will be diagnosed and signaled by a */
/*         routine in the call tree of this routine. Note that when */
/*         light time correction is used, sufficient ephemeris data must */
/*         be available to propagate the states of both observer and */
/*         target to the solar system barycenter. */

/*     9)  If the computation method specifies an ellipsoidal target */
/*         shape and triaxial radii of the target body have not been */
/*         loaded into the kernel pool prior to calling SINCPT, the */
/*         error will be diagnosed and signaled by a routine in the call */
/*         tree of this routine. */

/*     10) The target must be an extended body: if any of the radii of */
/*         the target body are non-positive, the error will be */
/*         diagnosed and signaled by routines in the call tree of this */
/*         routine. */

/*     11) If PCK data specifying the target body-fixed frame */
/*         orientation have not been loaded prior to calling SINCPT, */
/*         the error will be diagnosed and signaled by a routine in the */
/*         call tree of this routine. */

/*     12) If the reference frame designated by DREF is not recognized */
/*         by the SPICE frame subsystem, the error SPICE(NOFRAME) */
/*         will be signaled. */

/*     13) If the direction vector DVEC is the zero vector, the error */
/*         SPICE(ZEROVECTOR) will be signaled. */


/* $ Files */

/*     Appropriate kernels must be loaded by the calling program before */
/*     this routine is called. */

/*     The following data are required: */

/*        - SPK data: ephemeris data for target and observer must be */
/*          loaded. If aberration corrections are used, the states of */
/*          target and observer relative to the solar system barycenter */
/*          must be calculable from the available ephemeris data. */
/*          Typically ephemeris data are made available by loading one */
/*          or more SPK files via FURNSH. */

/*        - PCK data: if the computation method is specified as */
/*          "Ellipsoid," triaxial radii for the target body must be */
/*          loaded into the kernel pool. Typically this is done by */
/*          loading a text PCK file via FURNSH. */

/*        - Further PCK data: rotation data for the target body must */
/*          be loaded. These may be provided in a text or binary PCK */
/*          file. */

/*     The following data may be required: */

/*        - Frame data: if a frame definition is required to convert */
/*          the observer and target states to the body-fixed frame of */
/*          the target, that definition must be available in the kernel */
/*          pool. Similarly, the frame definition required to map */
/*          between the frame designated by DREF and the target */
/*          body-fixed frame must be available. Typically the */
/*          definitions of frames not already built-in to SPICE are */
/*          supplied by loading a frame kernel. */

/*        - CK data: if the frame to which DREF refers is fixed to a */
/*          spacecraft instrument or structure, at least one CK file */
/*          will be needed to permit transformation of vectors between */
/*          that frame and both the J2000 and the target body-fixed */
/*          frames. */

/*        - SCLK data: if a CK file is needed, an associated SCLK */
/*          kernel is required to enable conversion between encoded SCLK */
/*          (used to time-tag CK data) and barycentric dynamical time */
/*          (TDB). */

/*     In all cases, kernel data are normally loaded once per program */
/*     run, NOT every time this routine is called. */

/* $ Particulars */

/*     Given a ray defined by a direction vector and the location of an */
/*     observer, SINCPT computes the surface intercept point of the ray */
/*     on a specified target body. SINCPT also determines the vector */
/*     from the observer to the surface intercept point. */

/*     When aberration corrections are used, this routine finds the */
/*     value of SPOINT such that, if SPOINT is regarded as an ephemeris */
/*     object, after the selected aberration corrections are applied to */
/*     the vector from the observer to SPOINT, the resulting vector is */
/*     parallel to the direction vector DVEC. */

/*     This routine computes light time corrections using light time */
/*     between the observer and the surface intercept point, as opposed */
/*     to the center of the target. Similarly, stellar aberration */
/*     corrections done by this routine are based on the direction of */
/*     the vector from the observer to the light-time corrected */
/*     intercept point, not to the target center. This technique avoids */
/*     errors due to the differential between aberration corrections */
/*     across the target body. Therefore it's valid to use aberration */
/*     corrections with this routine even when the observer is very */
/*     close to the intercept point, in particular when the */
/*     observer-intercept point distance is much less than the */
/*     observer-target center distance. It's also valid to use stellar */
/*     aberration corrections even when the intercept point is near or */
/*     on the limb (as may occur in occultation computations using a */
/*     point target). */

/*     When comparing surface intercept point computations with results */
/*     from sources other than SPICE, it's essential to make sure the */
/*     same geometric definitions are used. */

/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */


/*     1) The following program computes surface intercept points on Mars */
/*        for the boresight and FOV boundary vectors of the MGS MOC */
/*        narrow angle camera. The intercepts are computed for a single */
/*        observation epoch. Light time and stellar aberration */
/*        corrections are used. For simplicity, camera distortion is */
/*        ignored. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File: mgs_example2.tm */

/*           This meta-kernel is intended to support operation of SPICE */
/*           example programs. The kernels shown here should not be */
/*           assumed to contain adequate or correct versions of data */
/*           required by SPICE-based user applications. */

/*           In order for an application to use this meta-kernel, the */
/*           kernels referenced here must be present in the user's */
/*           current working directory. */

/*           The names and contents of the kernels referenced */
/*           by this meta-kernel are as follows: */

/*              File name                     Contents */
/*              ---------                     -------- */
/*              de418.bsp                     Planetary ephemeris */
/*              pck00008.tpc                  Planet orientation and */
/*                                            radii */
/*              naif0008.tls                  Leapseconds */
/*              mgs_moc_v20.ti                MGS MOC instrument */
/*                                            parameters */
/*              mgs_sclkscet_00061.tsc        MGS SCLK coefficients */
/*              mgs_sc_ext12.bc               MGS s/c bus attitude */
/*              mgs_ext12_ipng_mgs95j.bsp     MGS ephemeris */

/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'de418.bsp', */
/*                                  'pck00008.tpc', */
/*                                  'naif0008.tls', */
/*                                  'mgs_moc_v20.ti', */
/*                                  'mgs_sclkscet_00061.tsc', */
/*                                  'mgs_sc_ext12.bc', */
/*                                  'mgs_ext12_ipng_mgs95j.bsp' ) */
/*           \begintext */


/*        Example code begins here. */

/*          PROGRAM EX1 */
/*          IMPLICIT NONE */
/*    C */
/*    C     SPICELIB functions */
/*    C */
/*          DOUBLE PRECISION      VNORM */

/*    C */
/*    C     Local parameters */
/*    C */
/*          CHARACTER*(*)         META */
/*          PARAMETER           ( META   = 'mgs_example2.tm' ) */

/*          INTEGER               ABCLEN */
/*          PARAMETER           ( ABCLEN = 20 ) */

/*          INTEGER               LNSIZE */
/*          PARAMETER           ( LNSIZE = 78 ) */

/*          INTEGER               METLEN */
/*          PARAMETER           ( METLEN = 40 ) */

/*          INTEGER               NAMLEN */
/*          PARAMETER           ( NAMLEN = 32 ) */

/*          INTEGER               TIMLEN */
/*          PARAMETER           ( TIMLEN = 50 ) */

/*          INTEGER               SHPLEN */
/*          PARAMETER           ( SHPLEN = 80 ) */

/*          INTEGER               NCORNR */
/*          PARAMETER           ( NCORNR = 4 ) */

/*    C */
/*    C     Local variables */
/*    C */
/*          CHARACTER*(ABCLEN)    ABCORR */
/*          CHARACTER*(NAMLEN)    CAMERA */
/*          CHARACTER*(NAMLEN)    DREF */
/*          CHARACTER*(METLEN)    METHOD */
/*          CHARACTER*(NAMLEN)    OBSRVR */
/*          CHARACTER*(SHPLEN)    SHAPE */
/*          CHARACTER*(NAMLEN)    TARGET */
/*          CHARACTER*(LNSIZE)    TITLE */
/*          CHARACTER*(TIMLEN)    UTC */

/*          DOUBLE PRECISION      BOUNDS ( 3, NCORNR ) */
/*          DOUBLE PRECISION      BSIGHT ( 3 ) */
/*          DOUBLE PRECISION      DIST */
/*          DOUBLE PRECISION      DPR */
/*          DOUBLE PRECISION      DVEC   ( 3 ) */
/*          DOUBLE PRECISION      ET */
/*          DOUBLE PRECISION      LAT */
/*          DOUBLE PRECISION      LON */
/*          DOUBLE PRECISION      RADIUS */
/*          DOUBLE PRECISION      SPOINT ( 3 ) */
/*          DOUBLE PRECISION      SRFVEC ( 3 ) */
/*          DOUBLE PRECISION      TRGEPC */

/*          INTEGER               CAMID */
/*          INTEGER               I */
/*          INTEGER               J */
/*          INTEGER               N */

/*          LOGICAL               FOUND */

/*          DATA                  ABCORR / 'CN+S'      / */
/*          DATA                  CAMERA / 'MGS_MOC_NA'/ */
/*          DATA                  METHOD / 'Ellipsoid' / */
/*          DATA                  OBSRVR / 'MGS'       / */
/*          DATA                  TARGET / 'Mars'      / */
/*          DATA                  UTC    / '2003 OCT 13 06:00:00 UTC' / */

/*    C */
/*    C     Load kernel files: */
/*    C */
/*          CALL FURNSH ( META ) */

/*    C */
/*    C     Convert the UTC request time to ET (seconds past */
/*    C     J2000, TDB). */
/*    C */
/*          CALL STR2ET ( UTC, ET ) */

/*    C */
/*    C     Get the MGS MOC Narrow angle camera (MGS_MOC_NA) */
/*    C     ID code. Then look up the field of view (FOV) */
/*    C     parameters by calling GETFOV. */
/*    C */
/*          CALL BODN2C ( CAMERA, CAMID, FOUND ) */

/*          IF ( .NOT. FOUND ) THEN */
/*             CALL SETMSG ( 'Could not find ID code for ' // */
/*         .                 'instrument #.'               ) */
/*             CALL ERRCH  ( '#', CAMERA                   ) */
/*             CALL SIGERR ( 'SPICE(NOTRANSLATION)'        ) */
/*          END IF */

/*    C */
/*    C     GETFOV will return the name of the camera-fixed frame */
/*    C     in the string DREF, the camera boresight vector in */
/*    C     the array BSIGHT, and the FOV corner vectors in the */
/*    C     array BOUNDS. */
/*    C */
/*          CALL GETFOV ( CAMID,  NCORNR, SHAPE,  DREF, */
/*         .              BSIGHT, N,      BOUNDS       ) */


/*          WRITE (*,*) ' ' */
/*          WRITE (*,*) 'Surface Intercept Locations for Camera' */
/*          WRITE (*,*) 'FOV Boundary and Boresight Vectors' */
/*          WRITE (*,*) ' ' */
/*          WRITE (*,*) '   Instrument:            ', CAMERA */
/*          WRITE (*,*) '   Epoch:                 ', UTC */
/*          WRITE (*,*) '   Aberration correction: ', ABCORR */
/*          WRITE (*,*) ' ' */

/*    C */
/*    C     Now compute and display the surface intercepts for the */
/*    C     boresight and all of the FOV boundary vectors. */
/*    C */
/*          DO I = 1, NCORNR+1 */

/*             IF ( I .LE. NCORNR ) THEN */

/*                TITLE = 'Corner vector #' */
/*                CALL REPMI ( TITLE, '#', I, TITLE ) */

/*                CALL VEQU ( BOUNDS(1,I), DVEC ) */

/*             ELSE */

/*                TITLE = 'Boresight vector' */
/*                CALL VEQU ( BSIGHT, DVEC ) */

/*             END IF */

/*    C */
/*    C        Compute the surface intercept point using */
/*    C        the specified aberration corrections. */
/*    C */
/*             CALL SINCPT ( METHOD, TARGET, ET,     'IAU_MARS', */
/*         .                 ABCORR, OBSRVR, DREF,   DVEC, */
/*         .                 SPOINT, TRGEPC, SRFVEC, FOUND      ) */

/*             IF ( FOUND ) THEN */
/*    C */
/*    C           Compute range from observer to apparent intercept. */
/*    C */
/*                DIST = VNORM ( SRFVEC ) */
/*    C */
/*    C           Convert rectangular coordinates to planetocentric */
/*    C           latitude and longitude. Convert radians to degrees. */
/*    C */
/*                CALL RECLAT ( SPOINT, RADIUS, LON, LAT ) */

/*                LON = LON * DPR () */
/*                LAT = LAT * DPR () */
/*    C */
/*    C           Display the results. */
/*    C */
/*                WRITE (*,*) ' ' */
/*                WRITE (*,*) TITLE */

/*                TITLE = '  Vector in # frame = ' */
/*                CALL REPMC ( TITLE, '#', DREF, TITLE ) */

/*                WRITE (*,*) ' ' */
/*                WRITE (*,*) TITLE */

/*                IF ( I .LE. NCORNR ) THEN */
/*                   WRITE (*,*) '  ', ( BOUNDS(J,I), J = 1, 3 ) */
/*                ELSE */
/*                   WRITE (*,*) '  ', BSIGHT */
/*                END IF */

/*                WRITE (*,*) ' ' */
/*                WRITE (*,*) '  Intercept:' */
/*                WRITE (*,*) */
/*         .      '     Radius                   (km)  = ', RADIUS */
/*                WRITE (*,*) */
/*         .      '     Planetocentric Latitude  (deg) = ', LAT */
/*                WRITE (*,*) */
/*         .      '     Planetocentric Longitude (deg) = ', LON */
/*                WRITE (*,*) */
/*         .      '     Range                    (km)  = ', DIST */
/*                WRITE (*,*) ' ' */

/*             ELSE */

/*                WRITE (*,*) ' ' */
/*                WRITE (*,*) 'Intercept not found.' */
/*                WRITE (*,*) ' ' */

/*             END IF */

/*          END DO */

/*          END */


/*     When this program was executed on a PC/Linux/g77 platform, the */
/*     output was: */

/*        Surface Intercept Locations for Camera */
/*        FOV Boundary and Boresight Vectors */

/*           Instrument:            MGS_MOC_NA */
/*           Epoch:                 2003 OCT 13 06:00:00 UTC */
/*           Aberration correction: CN+S */


/*        Corner vector 1 */

/*          Vector in MGS_MOC_NA frame = */
/*            1.85713838E-06 -0.00380156227  0.999992774 */

/*          Intercept: */
/*             Radius                   (km)  =   3384.94114 */
/*             Planetocentric Latitude  (deg) =  -48.4774819 */
/*             Planetocentric Longitude (deg) =  -123.474079 */
/*             Range                    (km)  =   388.983104 */


/*        Corner vector 2 */

/*          Vector in MGS_MOC_NA frame = */
/*            1.85713838E-06  0.00380156227  0.999992774 */

/*          Intercept: */
/*             Radius                   (km)  =   3384.9397 */
/*             Planetocentric Latitude  (deg) =  -48.4816363 */
/*             Planetocentric Longitude (deg) =  -123.398823 */
/*             Range                    (km)  =   388.975121 */


/*        Corner vector 3 */

/*          Vector in MGS_MOC_NA frame = */
/*           -1.85713838E-06  0.00380156227  0.999992774 */

/*          Intercept: */
/*             Radius                   (km)  =   3384.93969 */
/*             Planetocentric Latitude  (deg) =  -48.4816619 */
/*             Planetocentric Longitude (deg) =  -123.398826 */
/*             Range                    (km)  =   388.974662 */


/*        Corner vector 4 */

/*          Vector in MGS_MOC_NA frame = */
/*           -1.85713838E-06 -0.00380156227  0.999992774 */

/*          Intercept: */
/*             Radius                   (km)  =   3384.94113 */
/*             Planetocentric Latitude  (deg) =  -48.4775075 */
/*             Planetocentric Longitude (deg) =  -123.474082 */
/*             Range                    (km)  =   388.982645 */


/*        Boresight vector */

/*          Vector in MGS_MOC_NA frame = */
/*            0.  0.  1. */

/*          Intercept: */
/*             Radius                   (km)  =   3384.94041 */
/*             Planetocentric Latitude  (deg) =  -48.4795798 */
/*             Planetocentric Longitude (deg) =  -123.436454 */
/*             Range                    (km)  =   388.975736 */



/*     2) Use SUBPNT to find the sub-spacecraft point on Mars for the */
/*        Mars Reconnaissance Orbiter spacecraft (MRO) at a specified */
/*        time, using the "near point: ellipsoid" computation method. */
/*        Use both LT+S and CN+S aberration corrections to illustrate */
/*        the differences. */

/*        Convert the spacecraft to sub-observer point vector obtained */
/*        from SUBPNT into the MRO_HIRISE_LOOK_DIRECTION reference frame */
/*        at the observation time. Perform a consistency check with this */
/*        vector: compare the Mars surface intercept of the ray */
/*        emanating from the spacecraft and pointed along this vector */
/*        with the sub-observer point. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File: mro_example.tm */

/*           This meta-kernel is intended to support operation of SPICE */
/*           example programs. The kernels shown here should not be */
/*           assumed to contain adequate or correct versions of data */
/*           required by SPICE-based user applications. */

/*           In order for an application to use this meta-kernel, the */
/*           kernels referenced here must be present in the user's */
/*           current working directory. */

/*           The names and contents of the kernels referenced */
/*           by this meta-kernel are as follows: */

/*              File name                     Contents */
/*              ---------                     -------- */
/*              de418.bsp                     Planetary ephemeris */
/*              pck00008.tpc                  Planet orientation and */
/*                                                 radii */
/*              naif0008.tls                  Leapseconds */
/*              mro_psp4_ssd_mro95a.bsp       MRO ephemeris */
/*              mro_v11.tf                    MRO frame specifications */
/*              mro_sclkscet_00022_65536.tsc  MRO SCLK coefficients and */
/*                                                 parameters */
/*              mro_sc_psp_070925_071001.bc   MRO attitude */


/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'de418.bsp', */
/*                                  'pck00008.tpc', */
/*                                  'naif0008.tls', */
/*                                  'mro_psp4_ssd_mro95a.bsp', */
/*                                  'mro_v11.tf', */
/*                                  'mro_sclkscet_00022_65536.tsc', */
/*                                  'mro_sc_psp_070925_071001.bc'  ) */
/*           \begintext */


/*       Example code begins here. */


/*          PROGRAM EX2 */
/*          IMPLICIT NONE */
/*    C */
/*    C     SPICELIB functions */
/*    C */
/*          DOUBLE PRECISION      DPR */
/*          DOUBLE PRECISION      VDIST */
/*          DOUBLE PRECISION      VNORM */

/*    C */
/*    C     Local parameters */
/*    C */
/*          CHARACTER*(*)         META */
/*          PARAMETER           ( META   = 'mro_example.tm' ) */

/*          CHARACTER*(*)         F1 */
/*          PARAMETER           ( F1     = '(A,F21.9)' ) */

/*          CHARACTER*(*)         F2 */
/*          PARAMETER           ( F2     = '(A)' ) */

/*          INTEGER               FRNMLN */
/*          PARAMETER           ( FRNMLN = 32 ) */

/*          INTEGER               MTHLEN */
/*          PARAMETER           ( MTHLEN = 50 ) */

/*          INTEGER               CORLEN */
/*          PARAMETER           ( CORLEN = 5 ) */

/*          INTEGER               NCORR */
/*          PARAMETER           ( NCORR  = 2 ) */

/*    C */
/*    C     Local variables */
/*    C */
/*          CHARACTER*(CORLEN)    ABCORR ( NCORR ) */
/*          CHARACTER*(FRNMLN)    HIREF */
/*          CHARACTER*(MTHLEN)    METHOD */

/*          DOUBLE PRECISION      ALT */
/*          DOUBLE PRECISION      ET */
/*          DOUBLE PRECISION      LAT */
/*          DOUBLE PRECISION      LON */
/*          DOUBLE PRECISION      MROVEC ( 3 ) */
/*          DOUBLE PRECISION      R1     ( 3, 3 ) */
/*          DOUBLE PRECISION      R2     ( 3, 3 ) */
/*          DOUBLE PRECISION      RADIUS */
/*          DOUBLE PRECISION      SPOINT ( 3 ) */
/*          DOUBLE PRECISION      SRFVEC ( 3 ) */
/*          DOUBLE PRECISION      TRGEPC */
/*          DOUBLE PRECISION      XFORM  ( 3, 3 ) */
/*          DOUBLE PRECISION      XEPOCH */
/*          DOUBLE PRECISION      XPOINT ( 3 ) */
/*          DOUBLE PRECISION      XVEC   ( 3 ) */

/*          INTEGER               I */

/*          LOGICAL               FOUND */

/*    C */
/*    C     Initial values */
/*    C */
/*          DATA                  ABCORR / 'LT+S', 'CN+S' / */
/*    C */
/*    C     Load kernel files via the meta-kernel. */
/*    C */
/*          CALL FURNSH ( META ) */

/*    C */
/*    C     Convert the TDB request time string to seconds past */
/*    C     J2000, TDB. */
/*    C */
/*          CALL STR2ET ( '2007 SEP 30 00:00:00 TDB', ET ) */

/*    C */
/*    C     Compute the sub-spacecraft point using the */
/*    C     "NEAR POINT: ELLIPSOID" definition. */
/*    C     Compute the results using both LT+S and CN+S */
/*    C     aberration corrections. */
/*    C */
/*          METHOD = 'Near point: ellipsoid' */

/*          WRITE(*,F2) ' ' */
/*          WRITE(*,F2) 'Computation method = '//METHOD */

/*          DO I = 1, NCORR */

/*             CALL SUBPNT ( METHOD, */
/*         .                 'Mars', ET,     'IAU_MARS', ABCORR(I), */
/*         .                 'MRO',  SPOINT, TRGEPC,     SRFVEC    ) */
/*    C */
/*    C        Compute the observer's altitude above SPOINT. */
/*    C */
/*             ALT = VNORM ( SRFVEC ) */
/*    C */
/*    C        Express SRFVEC in the MRO_HIRISE_LOOK_DIRECTION */
/*    C        reference frame at epoch ET. Since SRFVEC is expressed */
/*    C        relative to the IAU_MARS frame at TRGEPC, we must */
/*    C        call PXFRM2 to compute the position transformation matrix */
/*    C        from IAU_MARS at TRGEPC to the MRO_HIRISE_LOOK_DIRECTION */
/*    C        frame at time ET. */
/*    C */
/*    C        To make code formatting a little easier, we'll store */
/*    C        the long MRO reference frame name in a variable: */
/*    C */
/*             HIREF = 'MRO_HIRISE_LOOK_DIRECTION' */

/*             CALL PXFRM2 ( 'IAU_MARS', HIREF,  TRGEPC, ET, XFORM ) */
/*             CALL MXV    (  XFORM,     SRFVEC, MROVEC ) */

/*    C */
/*    C        Convert rectangular coordinates to planetocentric */
/*    C        latitude and longitude. Convert radians to degrees. */
/*    C */
/*             CALL RECLAT ( SPOINT, RADIUS, LON, LAT  ) */

/*             LON = LON * DPR () */
/*             LAT = LAT * DPR () */
/*    C */
/*    C        Write the results. */
/*    C */
/*             WRITE(*,F2) ' ' */
/*             WRITE(*,F2) 'Aberration correction = '//ABCORR(I) */
/*             WRITE(*,F1) ' ' */
/*             WRITE(*,F2) '  MRO-to-sub-observer vector in' */
/*             WRITE(*,F2) '  MRO HIRISE look direction frame' */
/*             WRITE(*,F1) '     X-component             (km) = ', */
/*         .               MROVEC(1) */
/*             WRITE(*,F1) '     Y-component             (km) = ', */
/*         .               MROVEC(2) */
/*             WRITE(*,F1) '     Z-component             (km) = ', */
/*         .               MROVEC(3) */
/*             WRITE(*,F1) '  Sub-observer point radius  (km) = ', RADIUS */
/*             WRITE(*,F1) '  Planetocentric latitude   (deg) = ', LAT */
/*             WRITE(*,F1) '  Planetocentric longitude  (deg) = ', LON */
/*             WRITE(*,F1) '  Observer altitude          (km) = ', ALT */

/*    C */
/*    C        Consistency check: find the surface intercept on */
/*    C        Mars of the ray emanating from the spacecraft and having */
/*    C        direction vector MROVEC in the MRO HIRISE look direction */
/*    C        reference frame at ET. Call the intercept point */
/*    C        XPOINT. XPOINT should coincide with SPOINT, up to a */
/*    C        small round-off error. */
/*    C */
/*             CALL SINCPT ( 'Ellipsoid', 'Mars', ET,    'IAU_MARS', */
/*         .                 ABCORR(I),   'MRO',  HIREF, MROVEC, */
/*         .                 XPOINT,      XEPOCH, XVEC,  FOUND  ) */

/*             IF ( .NOT. FOUND ) THEN */
/*                WRITE (*,F1) 'Bug: no intercept' */
/*             ELSE */
/*    C */
/*    C           Report the distance between XPOINT and SPOINT. */
/*    C */
/*                WRITE (*,F1) '  Intercept comparison error (km) = ', */
/*         .                   VDIST( XPOINT, SPOINT ) */
/*             END IF */

/*             WRITE(*,F1) ' ' */

/*          END DO */

/*          END */


/*     When this program was executed on a PC/Linux/gfortran platform, */
/*     the output was: */


/*        Computation method = Near point: ellipsoid */

/*        Aberration correction = LT+S */

/*          MRO-to-sub-observer vector in */
/*          MRO HIRISE look direction frame */
/*             X-component             (km) =           0.286931987 */
/*             Y-component             (km) =          -0.260417167 */
/*             Z-component             (km) =         253.816284981 */
/*          Sub-observer point radius  (km) =        3388.299078207 */
/*          Planetocentric latitude   (deg) =         -38.799836879 */
/*          Planetocentric longitude  (deg) =        -114.995294746 */
/*          Observer altitude          (km) =         253.816580760 */
/*          Intercept comparison error (km) =           0.000002144 */


/*        Aberration correction = CN+S */

/*          MRO-to-sub-observer vector in */
/*          MRO HIRISE look direction frame */
/*             X-component             (km) =           0.286931866 */
/*             Y-component             (km) =          -0.260417914 */
/*             Z-component             (km) =         253.816274506 */
/*          Sub-observer point radius  (km) =        3388.299078205 */
/*          Planetocentric latitude   (deg) =         -38.799836883 */
/*          Planetocentric longitude  (deg) =        -114.995294968 */
/*          Observer altitude          (km) =         253.816570285 */
/*          Intercept comparison error (km) =           0.000000001 */


/* $ Restrictions */

/*     A cautionary note: if aberration corrections are used, and */
/*     if DREF is the target body-fixed frame, the epoch at which that */
/*     frame is evaluated is offset from ET by the light time between */
/*     the observer and the *center* of the target body. This light time */
/*     normally will differ from the light time between the observer and */
/*     intercept point. Consequently the orientation of the target */
/*     body-fixed frame at TRGEPC will not match that of the target */
/*     body-fixed frame at the epoch associated with DREF. As a result, */
/*     various derived quantities may not be as expected: for example, */
/*     SRFVEC would not be parallel to DVEC. */

/*     In many applications the errors arising from this frame */
/*     discrepancy may be insignificant; however a safe approach is to */
/*     always use as DREF a frame other than the target body-fixed */
/*     frame. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     S.C. Krening   (JPL) */
/*     B.V. Semenov   (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 31-MAR-2014 (NJB) (SCK) (BVS) */

/*        Bug fix: FIRST is now set to .FALSE. at the completion */
/*        of a successful initialization pass. This does not affect */
/*        the routine's outputs but improves efficiency. */

/*        Bug fix: redundant call to SPKSSB was removed. This does not */
/*        affect the routine's outputs but improves efficiency. */

/*        References to the new PXFRM2 routine were added, which changed */
/*        the Detailed Output section and the second example. Some header */
/*        comment corrections were made. */

/*        Upgrade: this routine now uses ZZVALCOR rather than */
/*        ZZPRSCOR, simplifying the implementation. */

/*        Upgrade: this routine now saves the input body names and */
/*        ZZBODTRN state counters and does name-ID conversions only if */
/*        the counters have changed. */

/*        Upgrade: this routine now saves the input frame names and POOL */
/*        state counters and does frame name-ID conversions only if the */
/*        counters have changed. */

/* -    SPICELIB Version 1.2.0, 07-APR-2010 (NJB) */

/*        Code style improvement: re-use of variables in */
/*        FRINFO calls has been eliminated. There is no impact */
/*        of the behavior of the routine. */

/* -    SPICELIB Version 1.1.0, 17-MAR-2009 (NJB)(EDW) */

/*        Bug fix: quick test for non-intersection is */
/*        no longer performed when observer-target distance */
/*        is less than target's maximum radius. */

/*        Typos in the Detailed Input section's description of DREF */
/*        were corrected. */

/*        In the header examples, meta-kernel names were updated to use */
/*        the suffix */

/*           ".tm" */

/*        Incorrect frame name FIXFRM was changed to FIXREF in */
/*        documentation. */

/*        Typo correction in Required_Reading, changed FRAME */
/*        to FRAMES. */

/* -    SPICELIB Version 1.0.0, 02-MAR-2008 (NJB) */

/* -& */
/* $ Index_Entries */

/*     find surface intercept point */
/*     find intersection of ray and target body surface */
/*     find intercept of ray on target body surface */

/* -& */
/* $ Revisions */

/*     None. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     This value will become system-dependent when systems */
/*     using 128-bit d.p. numbers are supported by SPICELIB. */
/*     CNVLIM, when added to 1.0D0, should yield 1.0D0. */


/*     Round-off error limit for arc sine input: */


/*     Fraction of target body angular radius used to define */
/*     region outside of which rays are immediately rejected */
/*     as non-intersecting. */


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
    chkin_("SINCPT", (ftnlen)6);

/*     Nothing has been found yet. */

    *found = FALSE_;

/*     Counter initialization is done separately. */

    if (first) {

/*        Initialize counters. */

	zzctruin_(svctr1);
	zzctruin_(svctr2);
	zzctruin_(svctr3);
	zzctruin_(svctr4);
    }
    if (first || s_cmp(abcorr, prvcor, abcorr_len, (ftnlen)5) != 0) {

/*        The aberration correction flag differs from the value it */
/*        had on the previous call, if any. Analyze the new flag. */

	zzvalcor_(abcorr, attblk, abcorr_len);
	if (failed_()) {
	    chkout_("SINCPT", (ftnlen)6);
	    return 0;
	}

/*        The aberration correction flag is valid; save it. */

	s_copy(prvcor, abcorr, (ftnlen)5, abcorr_len);

/*        Set logical flags indicating the attributes of the requested */
/*        correction: */

/*           XMIT is .TRUE. when the correction is for transmitted */
/*           radiation. */

/*           USELT is .TRUE. when any type of light time correction */
/*           (normal or converged Newtonian) is specified. */

/*           USECN indicates converged Newtonian light time correction. */

/*           USESTL indicates stellar aberration corrections. */


/*        The above definitions are consistent with those used by */
/*        ZZPRSCOR. */

	xmit = attblk[4];
	uselt = attblk[1];
	usecn = attblk[3];
	usestl = attblk[2];

/*        The variable LOCCOR will contain a representation of */
/*        the aberration correction specification with stellar */
/*        aberration omitted. */

	if (attblk[0]) {
	    s_copy(loccor, "NONE", (ftnlen)5, (ftnlen)4);
	} else {
	    if (xmit) {
		s_copy(loccor, "X", (ftnlen)5, (ftnlen)1);
	    } else {
		s_copy(loccor, " ", (ftnlen)5, (ftnlen)1);
	    }
	    if (usecn) {
		suffix_("CN", &c__0, loccor, (ftnlen)2, (ftnlen)5);
	    } else if (uselt) {
		suffix_("LT", &c__0, loccor, (ftnlen)2, (ftnlen)5);
	    }
	}

/*        At this point, the first pass actions were successful. */

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
	chkout_("SINCPT", (ftnlen)6);
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
	chkout_("SINCPT", (ftnlen)6);
	return 0;
    }

/*     Check the input body codes. If they are equal, signal */
/*     an error. */

    if (obscde == trgcde) {
	setmsg_("In computing the surface intercept point, the observing bod"
		"y and target body are the same. Both are #.", (ftnlen)102);
	errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	sigerr_("SPICE(BODIESNOTDISTINCT)", (ftnlen)24);
	chkout_("SINCPT", (ftnlen)6);
	return 0;
    }

/*     Determine the attributes of the frame designated by FIXREF. */

    zznamfrm_(svctr3, svfref, &svfxfc, fixref, &fxfcde, (ftnlen)32, 
	    fixref_len);
    frinfo_(&fxfcde, &fxcent, &fxclss, &fxtyid, &fnd);
    if (failed_()) {
	chkout_("SINCPT", (ftnlen)6);
	return 0;
    }
    if (! fnd) {
	setmsg_("Reference frame # is not recognized by the SPICE frame subs"
		"ystem. Possibly a required frame definition kernel has not b"
		"een loaded.", (ftnlen)130);
	errch_("#", fixref, (ftnlen)1, fixref_len);
	sigerr_("SPICE(NOFRAME)", (ftnlen)14);
	chkout_("SINCPT", (ftnlen)6);
	return 0;
    }

/*     Make sure that FIXREF is centered at the target body's center. */

    if (fxcent != trgcde) {
	setmsg_("Reference frame # is not centered at the target body #. The"
		" ID code of the frame center is #.", (ftnlen)93);
	errch_("#", fixref, (ftnlen)1, fixref_len);
	errch_("#", target, (ftnlen)1, target_len);
	errint_("#", &fxcent, (ftnlen)1);
	sigerr_("SPICE(INVALIDFRAME)", (ftnlen)19);
	chkout_("SINCPT", (ftnlen)6);
	return 0;
    }

/*     Check for a zero ray direction vector. */

    if (vzero_(dvec)) {
	setmsg_("Input ray direction was the zero vector; this vector must b"
		"e non-zero.", (ftnlen)70);
	sigerr_("SPICE(ZEROVECTOR)", (ftnlen)17);
	chkout_("SINCPT", (ftnlen)6);
	return 0;
    }

/*     Get the sign S prefixing LT in the expression for TRGEPC. */
/*     When light time correction is not used, setting S = 0 */
/*     allows us to seamlessly set TRGEPC equal to ET. */

    if (uselt) {
	if (xmit) {
	    s = 1.;
	} else {
	    s = -1.;
	}
    } else {
	s = 0.;
    }

/*     Determine the position of the observer in target */
/*     body-fixed coordinates. */

/*         -  Call SPKEZP to compute the position of the target body as */
/*            seen from the observing body and the light time (LT) */
/*            between them. We request that the coordinates of POS be */
/*            returned relative to the body fixed reference frame */
/*            associated with the target body, using aberration */
/*            corrections specified by LOCCOR; these are the corrections */
/*            the input argument ABCORR, minus the stellar aberration */
/*            correction if it was called for. */

/*         -  Call VMINUS to negate the direction of the vector (OBSPOS) */
/*            so it will be the position of the observer as seen from */
/*            the target body in target body fixed coordinates. */

/*            Note that this result is not the same as the result of */
/*            calling SPKEZP with the target and observer switched. We */
/*            computed the vector FROM the observer TO the target in */
/*            order to get the proper light time and stellar aberration */
/*            corrections (if requested). Now we need the inverse of */
/*            that corrected vector in order to compute the intercept */
/*            point. */

    spkezp_(&trgcde, et, fixref, loccor, &obscde, tpos, &lt, fixref_len, (
	    ftnlen)5);

/*     Negate the target's position to obtain the position of the */
/*     observer relative to the target. */

    vminus_(tpos, obspos);

/*     We now need to convert the direction vector into the */
/*     body fixed frame associated with the target. The target */
/*     epoch is dependent on the aberration correction. The */
/*     coefficient S has been set to give us the correct answer */
/*     for each case. */

    *trgepc = *et + s * lt;

/*     Determine the attributes of the frame designated by DREF. */

    zznamfrm_(svctr4, svdref, &svdfrc, dref, &dfrcde, (ftnlen)32, dref_len);
    frinfo_(&dfrcde, &dcentr, &dclass, &dtypid, &fnd);
    if (failed_()) {
	chkout_("SINCPT", (ftnlen)6);
	return 0;
    }
    if (! fnd) {
	setmsg_("Reference frame # is not recognized by the SPICE frame subs"
		"ystem. Possibly a required frame definition kernel has not b"
		"een loaded.", (ftnlen)130);
	errch_("#", dref, (ftnlen)1, dref_len);
	sigerr_("SPICE(NOFRAME)", (ftnlen)14);
	chkout_("SINCPT", (ftnlen)6);
	return 0;
    }

/*     Transform the direction vector from frame DREF to the body-fixed */
/*     frame associated with the target. The epoch TRGEPC associated */
/*     with the body-fixed frame has been set already. */

/*     We'll compute the transformation in two parts: first */
/*     from frame DREF to J2000, then from J2000 to the target */
/*     frame. */

    if (dclass == 1) {

/*        Inertial frames can be evaluated at any epoch. */

	refepc = *et;
    } else if (! uselt) {

/*        We're not using light time corrections (converged or */
/*        otherwise), so there's no time offset. */

	refepc = *et;
    } else if (dcentr == obscde) {

/*        If the center of frame DREF is the observer (which is */
/*        usually the case if the observer is a spacecraft), then */
/*        the epoch of frame DREF is simply ET. */

/*        There's no offset between the center for frame DREF */
/*        and the observer. */

	refepc = *et;
    } else {

/*        Find the light time from the observer to the center of */
/*        frame DREF. */

	spkezp_(&dcentr, et, "J2000", abcorr, &obscde, rpos, &ltcent, (ftnlen)
		5, abcorr_len);
	if (failed_()) {
	    chkout_("SINCPT", (ftnlen)6);
	    return 0;
	}
	refepc = *et + s * ltcent;
    }

/*     The epoch REFEPC associated with frame DREF has been set. */

/*     Compute the transformation from frame DREF to J2000 and the */
/*     transformation from J2000 to the target body-fixed frame. */

/*     Map DVEC to both the J2000 and target body-fixed frames. We'll */
/*     store DVEC, expressed relative to the J2000 frame, in the */
/*     variable J2DIR. DVEC in the target body-fixed frame will be */
/*     stored in TRGDIR. */

/*     We may need both versions of DVEC: if we use light time */
/*     correction, we'll update "intercept epoch", and hence the */
/*     transformation between J2000 and the target body-fixed frame. */
/*     The transformation between DREF and J2000 doesn't change, on the */
/*     other hand, so we don't have to recompute J2DIR. We need TRGDIR */
/*     in all cases. */

    pxform_(dref, "J2000", &refepc, r2jmat, dref_len, (ftnlen)5);
    if (failed_()) {
	chkout_("SINCPT", (ftnlen)6);
	return 0;
    }
    mxv_(r2jmat, dvec, j2dir);

/*     Save this version of J2DIR as J2GEOM. Later we'll */
/*     modify J2DIR, if necessary, to account for stellar */
/*     aberration. */

    vequ_(j2dir, j2geom);

/*     Map J2DIR (in the J2000 frame) to the target body-fixed */
/*     frame. */

    pxform_("J2000", fixref, trgepc, j2tmat, (ftnlen)5, fixref_len);
    if (failed_()) {
	chkout_("SINCPT", (ftnlen)6);
	return 0;
    }
    mxv_(j2tmat, j2dir, trgdir);

/*     At this point, */

/*        TRGEPC is set. */
/*        TRGDIR is set. */
/*        J2DIR is set. */


/*     Get the J2000-relative state of the observer relative to */
/*     the solar system barycenter at ET. We'll use this in */
/*     several places later. */

    spkssb_(&obscde, et, "J2000", ssbost, (ftnlen)5);

/*     If we're using stellar aberration correction, at this point we'll */
/*     account for it. We're going to find a surface point such that */
/*     the radiation path from that point to the observer, after */
/*     correction for stellar aberration, is parallel to the ray. So */
/*     by applying the inverse of the correction to the ray, we obtain */
/*     the ray with which we must perform our intercept computation. */

    if (usestl) {

/*        We approximate the inverse stellar aberration correction by */
/*        using the correction for the reverse transmission direction. */
/*        If we're in the reception case, we apply the transmission */
/*        stellar aberration correction to J2DIR and vice versa. */

/*        We iterate our estimates until we have the desired level */
/*        of convergence or reach the iteration limit. */

	nitr = 5;
	if (xmit) {

/*           Use reception stellar aberration correction */
/*           routine STELAB to generate a first estimate of */
/*           the direction vector after stellar aberration */
/*           has been "removed"---that is, apply the inverse */
/*           of the transmission stellar aberration correction */
/*           mapping to J2DIR. */

	    stelab_(j2dir, &ssbost[3], stldir);

/*           Now improve our estimate. */

	    relerr = 1.;
	    i__ = 1;
	    while(i__ <= nitr && relerr > 1e-17) {

/*              Estimate the error in our previous approximation */
/*              by applying the reception stellar aberration */
/*              to STLDIR and finding the difference with J2DIR. */

		stlabx_(stldir, &ssbost[3], j2est);
		vsub_(j2dir, j2est, stlerr);

/*              Adding the error in the reception mapping to STLDIR */
/*              will give us an improved estimate of the inverse. */

		vadd_(stlerr, stldir, stltmp);
		vequ_(stltmp, stldir);
		relerr = vnorm_(stlerr) / vnorm_(stldir);
		++i__;
	    }

/*           At this point we've found a good estimate of the */
/*           direction vector under the inverse of the transmission */
/*           stellar aberration correction mapping. */

	} else {

/*           Use transmission stellar aberration correction */
/*           routine STLABX to generate a first estimate of */
/*           the direction vector after stellar aberration */
/*           has been "removed." */

	    stlabx_(j2dir, &ssbost[3], stldir);

/*           Now improve our estimate. */

	    relerr = 1.;
	    i__ = 1;
	    while(i__ <= nitr && relerr > 1e-17) {

/*              Estimate the error in our previous approximation */
/*              by applying the reception stellar aberration */
/*              to STLDIR and finding the difference with J2DIR. */

		stelab_(stldir, &ssbost[3], j2est);
		vsub_(j2dir, j2est, stlerr);

/*              Adding the error in the reception mapping to STLDIR */
/*              will give us an improved estimate of the inverse. */

		vadd_(stlerr, stldir, stltmp);
		vequ_(stltmp, stldir);
		relerr = vnorm_(stlerr) / vnorm_(stldir);
		++i__;
	    }

/*           At this point we've found a good estimate of the */
/*           direction vector under the inverse of the reception */
/*           stellar aberration correction mapping. */

	}

/*        Replace the J2000-relative ray direction with the corrected */
/*        direction. */

	vequ_(stldir, j2dir);
	mxv_(j2tmat, j2dir, trgdir);
    }

/*     Find the surface intercept point and distance from observer to */
/*     intercept point using the specified geometric definition. */

    if (eqstr_(method, "Ellipsoid", method_len, (ftnlen)9)) {

/*        Find the surface intercept given the target epoch, */
/*        observer-target position, and target body orientation */
/*        we've already computed. If we're not using light */
/*        time correction, this is all we must do. Otherwise, */
/*        our result will give us an initial estimate of the */
/*        target epoch, which we'll then improve. */

/*        Get the radii of the target body from the kernel pool. */

	bodvcd_(&trgcde, "RADII", &c__3, &nradii, radii, (ftnlen)5);

/*        Make an easy test to see whether we can quit now because */
/*        an intercept cannot exist. If the ray is separated from */
/*        the observer-target center vector by more than (MARGIN * */
/*        the maximum triaxial radius), we're done. Let REJECT be */
/*        the angular separation limit. */

/* Computing MAX */
	d__1 = max(radii[0],radii[1]);
	maxrad = max(d__1,radii[2]);
	range = vnorm_(obspos);
	if (range == 0.) {

/*           We've already ensured that observer and target are */
/*           distinct, so this should be a very unusual occurrence. */

	    setmsg_("Observer-target distance is zero. Observer is #; target"
		    " is #.", (ftnlen)61);
	    errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	    errch_("#", target, (ftnlen)1, target_len);
	    sigerr_("SPICE(NOSEPARATION)", (ftnlen)19);
	    chkout_("SINCPT", (ftnlen)6);
	    return 0;
	}
	if (range > maxrad * 1.01) {

/*           Compute the arc sine with SPICE error checking. */

	    d__1 = maxrad * 1.01 / range;
	    reject = dasine_(&d__1, &c_b57);
	    vminus_(obspos, negpos);
	    if (vsep_(negpos, trgdir) > reject) {

/*              The angular separation of ray and target is too great */
/*              for a solution to exist, even with a better light time */
/*              estimate. */

		chkout_("SINCPT", (ftnlen)6);
		return 0;
	    }
	}

/*        Locate the intercept of the ray with the target; if there's no */
/*        intercept, find the closest point on the target to the ray. */

	surfpt_(obspos, trgdir, radii, &radii[1], &radii[2], spoint, found);
	if (failed_()) {
	    chkout_("SINCPT", (ftnlen)6);
	    return 0;
	}

/*        If we found an intercept, and if we're not using light time */
/*        corrections, we're almost done now. We still need SRFVEC. */
/*        SPOINT, TRGEPC, and FOUND have already been set. */

	if (*found && ! uselt) {
	    vsub_(spoint, obspos, srfvec);
	    chkout_("SINCPT", (ftnlen)6);
	    return 0;
	}

/*        From this point onward, we're dealing with a case calling for */
/*        light time and possibly stellar aberration corrections. */

	if (! (*found)) {

/*           If there's no intercept, we're probably done. However, */
/*           we need to guard against the possibility that the ray does */
/*           intersect the ellipsoid but we haven't discovered it */
/*           because our first light time estimate was too poor. */

/*           We'll make an improved light time estimate as follows: */
/*           Find the nearest point on the ellipsoid to the ray. Find */
/*           the light time between the observer and this point. */

/*           If we're using converged Newtonian corrections, we */
/*           iterate this procedure up to three times. */

	    if (usecn) {
		nitr = 3;
	    } else {
		nitr = 1;
	    }
	    i__ = 1;
	    while(i__ <= nitr && ! (*found)) {
		npedln_(radii, &radii[1], &radii[2], obspos, trgdir, pnear, &
			rayalt);
		lt = vdist_(obspos, pnear) / clight_();

/*              Use the new light time estimate to repeat the intercept */
/*              computation. */

		*trgepc = *et + s * lt;

/*              Get the J2000-relative state of the target relative to */
/*              the solar system barycenter at the target epoch. */

		spkssb_(&trgcde, trgepc, "J2000", ssbtst, (ftnlen)5);
		if (failed_()) {
		    chkout_("SINCPT", (ftnlen)6);
		    return 0;
		}

/*              Find the position of the observer relative to the target. */
/*              Convert this vector from the J2000 frame to the target */
/*              frame at TRGEPC. */

		vsub_(ssbost, ssbtst, j2pos);
		pxform_("J2000", fixref, trgepc, xform, (ftnlen)5, fixref_len)
			;
		if (failed_()) {
		    chkout_("SINCPT", (ftnlen)6);
		    return 0;
		}

/*              Convert the observer's position relative to the target */
/*              from the J2000 frame to the target frame at the target */
/*              epoch. */

		mxv_(xform, j2pos, obspos);

/*              Convert the ray's direction vector from the J2000 frame */
/*              to the target frame at the target epoch. */

		mxv_(xform, j2dir, trgdir);

/*              Repeat the intercept computation. */

		surfpt_(obspos, trgdir, radii, &radii[1], &radii[2], spoint, 
			found);
		++i__;
	    }

/*           If there's still no intercept, we're done. */

	    if (! (*found)) {
		chkout_("SINCPT", (ftnlen)6);
		return 0;
	    }
	}

/*        Making it to this point means we've got an intersection. */

/*        Since we're using light time corrections, we're going to make */
/*        an estimate of light time to the intercept point, then re-do */
/*        our computation of the target position and orientation using */
/*        the new light time value. */

	if (usecn) {
	    nitr = 10;
	} else {
	    nitr = 1;
	}

/*        Compute new light time estimate and new target epoch. */

	dist = vdist_(obspos, spoint);
	lt = dist / clight_();
	*trgepc = *et + s * lt;
	prevlt = 0.;
	prevet = *trgepc;
	i__ = 0;
	ltdiff = 1.;
	etdiff = 1.;
	while(i__ < nitr && ltdiff > abs(lt) * 1e-17 && etdiff > 0.) {

/*           Get the J2000-relative state of the target relative to */
/*           the solar system barycenter at the target epoch. */

	    spkssb_(&trgcde, trgepc, "J2000", ssbtst, (ftnlen)5);
	    if (failed_()) {
		chkout_("SINCPT", (ftnlen)6);
		return 0;
	    }

/*           Find the position of the observer relative to the target. */
/*           Convert this vector from the J2000 frame to the target */
/*           frame at TRGEPC. */

/*           Note that SSBOST contains the J2000-relative state of the */
/*           observer relative to the solar system barycenter at ET. */

	    vsub_(ssbost, ssbtst, j2pos);
	    pxform_("J2000", fixref, trgepc, xform, (ftnlen)5, fixref_len);
	    if (failed_()) {
		chkout_("SINCPT", (ftnlen)6);
		return 0;
	    }

/*           Convert the observer's position relative to the target from */
/*           the J2000 frame to the target frame at the target epoch. */

	    mxv_(xform, j2pos, obspos);
	    vminus_(obspos, negpos);

/*           Convert the ray's direction vector from the J2000 frame */
/*           to the target frame at the target epoch. */

	    mxv_(xform, j2dir, trgdir);

/*           Repeat the intercept computation. */

	    surfpt_(obspos, trgdir, radii, &radii[1], &radii[2], spoint, 
		    found);

/*           If there's no intercept, we're done. */

	    if (! (*found)) {
		chkout_("SINCPT", (ftnlen)6);
		return 0;
	    }

/*           Compute the distance between intercept and observer. */

	    dist = vdist_(obspos, spoint);

/*           Compute new light time estimate and new target epoch. */

	    lt = dist / clight_();
	    *trgepc = *et + s * lt;

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
	setmsg_("The computation method # was not recognized. ", (ftnlen)45);
	errch_("#", method, (ftnlen)1, method_len);
	sigerr_("SPICE(INVALIDMETHOD)", (ftnlen)20);
	chkout_("SINCPT", (ftnlen)6);
	return 0;
    }

/*     FOUND, SPOINT, TRGEPC, and OBSPOS have been set at this point. */
/*     We need SRFVEC. Since OBSPOS doesn't take into account stellar */
/*     aberration, we can' derive SRFVEC from OBSPOS as is done in */
/*     the related routines SUBPNT and SUBSLR. Here, we derive */
/*     SRFVEC from J2GEOM, which is the input ray direction expressed in */
/*     the J2000 frame. We use XFORM, which is computed in the loop */
/*     above, to convert J2GEOM to FIXREF, evaluated at TRGEPC. */

    mxv_(xform, j2geom, udir);
    vhatip_(udir);

/*     Let SRFLEN be the length of SRFVEC; we CAN get this */
/*     length from OBSPOS and SPOINT, since stellar */
/*     aberration correction (as implemented in SPICE) */
/*     doesn't change the length of the vector SPOINT-OBSPOS. */

    srflen = vdist_(spoint, obspos);

/*     Scale UDIR to obtain the desired value of SRFVEC. */

    vscl_(&srflen, udir, srfvec);
    chkout_("SINCPT", (ftnlen)6);
    return 0;
} /* sincpt_ */

