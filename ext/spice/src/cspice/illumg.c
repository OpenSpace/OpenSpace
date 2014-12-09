/* illumg.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static integer c__3 = 3;

/* $Procedure ILLUMG ( Illumination angles, general source ) */
/* Subroutine */ int illumg_(char *method, char *target, char *illum, 
	doublereal *et, char *fixref, char *abcorr, char *obsrvr, doublereal *
	spoint, doublereal *trgepc, doublereal *srfvec, doublereal *phase, 
	doublereal *incdnc, doublereal *emissn, ftnlen method_len, ftnlen 
	target_len, ftnlen illum_len, ftnlen fixref_len, ftnlen abcorr_len, 
	ftnlen obsrvr_len)
{
    /* Initialized data */

    static logical elipsd = TRUE_;
    static logical first = TRUE_;
    static char prvcor[5] = "     ";
    static char prvmth[80] = "Ellipsoid                                     "
	    "                                  ";

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int zzbods2c_(integer *, char *, integer *, 
	    logical *, char *, integer *, logical *, ftnlen, ftnlen);
    extern doublereal vsep_(doublereal *, doublereal *);
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *);
    integer type__;
    logical xmit;
    extern /* Subroutine */ int zznamfrm_(integer *, char *, integer *, char *
	    , integer *, ftnlen, ftnlen), zzvalcor_(char *, logical *, ftnlen)
	    , zzctruin_(integer *);
    integer n;
    doublereal s, radii[3];
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    static logical uselt, svfnd1, svfnd2;
    static integer svctr1[2], svctr2[2];
    extern logical failed_(void);
    static integer svctr3[2];
    integer refcde, obscde;
    doublereal lt;
    extern /* Subroutine */ int bodvcd_(integer *, char *, integer *, integer 
	    *, doublereal *, ftnlen);
    static integer trgcde, center;
    char locmth[80];
    doublereal normal[3];
    integer typeid;
    logical attblk[15];
    static integer svtcde, svobsc;
    doublereal obspos[3];
    static char svtarg[36];
    doublereal tistat[6];
    static char svfref[32];
    doublereal opstat[6];
    extern logical return_(void);
    static char svobsr[36];
    static integer svrefc;
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen), sigerr_(char *, ftnlen), ljucrs_(integer *, char *, char 
	    *, ftnlen, ftnlen), frinfo_(integer *, integer *, integer *, 
	    integer *, logical *), errint_(char *, integer *, ftnlen), 
	    spkcpt_(doublereal *, char *, char *, doublereal *, char *, char *
	    , char *, char *, doublereal *, doublereal *, ftnlen, ftnlen, 
	    ftnlen, ftnlen, ftnlen, ftnlen), spkcpo_(char *, doublereal *, 
	    char *, char *, char *, doublereal *, char *, char *, doublereal *
	    , doublereal *, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen), 
	    surfnm_(doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *), vminus_(doublereal *, doublereal *);
    logical fnd;
    doublereal lti;

/* $ Abstract */

/*     Find the illumination angles (phase, incidence, and */
/*     emission) at a specified surface point of a target body. */
/*     The illumination source is a specified ephemeris object. */

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
/*     MOSPICE */

/* $ Declarations */
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
/*     ILLUM      I   Name of illumination source. */
/*     ET         I   Epoch in ephemeris seconds past J2000 TDB. */
/*     FIXREF     I   Body-fixed, body-centered target body frame. */
/*     ABCORR     I   Desired aberration correction. */
/*     OBSRVR     I   Name of observing body. */
/*     SPOINT     I   Body-fixed coordinates of a target surface point. */
/*     TRGEPC     O   Target surface point epoch. */
/*     SRFVEC     O   Vector from observer to target surface point. */
/*     PHASE      O   Phase angle at the surface point. */
/*     INCDNC     O   Source incidence angle at the surface point. */
/*     EMISSN     O   Emission angle at the surface point. */

/* $ Detailed_Input */


/*     METHOD      is a short string providing parameters defining */
/*                 the computation method to be used. Parameters */
/*                 include, but are not limited to, the shape model */
/*                 used to represent the surface of the target body. */

/*                 The only choice currently supported is */

/*                    'Ellipsoid'        The illumination angle */
/*                                       computation uses a triaxial */
/*                                       ellipsoid to model the surface */
/*                                       of the target body. The */
/*                                       ellipsoid's radii must be */
/*                                       available in the kernel pool. */

/*                 Neither case nor white space are significant in */
/*                 METHOD. For example, the string ' eLLipsoid ' is */
/*                 valid. */


/*     TARGET      is the name of the target body. TARGET is */
/*                 case-insensitive, and leading and trailing blanks in */
/*                 TARGET are not significant. Optionally, you may */
/*                 supply a string containing the integer ID code for */
/*                 the object. For example both 'MOON' and '301' are */
/*                 legitimate strings that indicate the Moon is the */
/*                 target body. */


/*     ILLUM       is the name of the illumination source. This source */
/*                 may be any ephemeris object. Case, blanks, and */
/*                 numeric values are treated in the same way as for the */
/*                 input TARGET. */


/*     ET          is the epoch, expressed as seconds past J2000 TDB, */
/*                 for which the apparent illumination angles at the */
/*                 specified surface point on the target body, as seen */
/*                 from the observing body, are to be computed. */


/*     FIXREF      is the name of the body-fixed, body-centered */
/*                 reference frame associated with the target body. The */
/*                 input surface point SPOINT and the output vector */
/*                 SRFVEC are expressed relative to this reference */
/*                 frame. The string FIXREF is case-insensitive, and */
/*                 leading and trailing blanks in FIXREF are not */
/*                 significant. */


/*     ABCORR      is the aberration correction to be used in computing */
/*                 the position and orientation of the target body and */
/*                 the location of the illumination source. */

/*                 For remote sensing applications, where the apparent */
/*                 illumination angles seen by the observer are desired, */
/*                 normally either of the corrections */

/*                    'LT+S' */
/*                    'CN+S' */

/*                 should be used. These and the other supported options */
/*                 are described below. ABCORR may be any of the */
/*                 following: */

/*                    'NONE'     No aberration correction. */

/*                 Let LT represent the one-way light time between the */
/*                 observer and SPOINT (note: NOT between the observer */
/*                 and the target body's center). The following values */
/*                 of ABCORR apply to the "reception" case in which */
/*                 photons depart from SPOINT at the light-time */
/*                 corrected epoch ET-LT and *arrive* at the observer's */
/*                 location at ET: */

/*                    'LT'       Correct both the position of SPOINT as */
/*                               seen by the observer, and the position */
/*                               of the illumination source as seen by */
/*                               the target, for light time. */

/*                    'LT+S'     Correct both the position of SPOINT as */
/*                               seen by the observer, and the position */
/*                               of the illumination source as seen by */
/*                               the target, for light time and stellar */
/*                               aberration. */

/*                    'CN'       Converged Newtonian light time */
/*                               correction. In solving the light time */
/*                               equations for target and the */
/*                               illumination source, the "CN" */
/*                               correction iterates until the solution */
/*                               converges. */

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

/*                 OBSRVR may be not be identical to TARGET. */


/*     SPOINT      is a surface point on the target body, expressed in */
/*                 Cartesian coordinates, relative to the body-fixed */
/*                 target frame designated by FIXREF. */

/*                 SPOINT need not be visible from the observer's */
/*                 location at the epoch ET. */

/*                 The components of SPOINT have units of km. */


/* $ Detailed_Output */


/*     TRGEPC      is the "surface point epoch." TRGEPC is defined as */
/*                 follows: letting LT be the one-way light time between */
/*                 the observer and the input surface point SPOINT, */
/*                 TRGEPC is either the epoch ET-LT or ET depending on */
/*                 whether the requested aberration correction is, */
/*                 respectively, for received radiation or omitted. LT */
/*                 is computed using the method indicated by ABCORR. */

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

/*                 To transform the vector SRFVEC to a time-dependent */
/*                 reference frame REF at ET, a sequence of calls is */
/*                 required. For example, let XFORM be 3x3 matrix */
/*                 describing the transformation between the target */
/*                 body-fixed frame at TRGEPC to the time-dependent */
/*                 frame REF at ET. Then SRFVEC can be transformed to */
/*                 the result REFVEC as follows: */

/*                     CALL PXFRM2 ( FIXREF, REF,    TRGEPC, ET, XFORM ) */
/*                     CALL MXV    ( XFORM,  SRFVEC, REFVEC ) */


/*     PHASE       is the phase angle at SPOINT, as seen from OBSRVR at */
/*                 time ET. This is the angle between the negative of */
/*                 the vector SRFVEC and the SPOINT-source vector at */
/*                 TRGEPC. Units are radians. The range of PHASE is */
/*                 [0, pi]. */

/*     INCDNC      is the illumination source incidence angle at SPOINT, */
/*                 as seen from OBSRVR at time ET. This is the angle */
/*                 between the surface normal vector at SPOINT and the */
/*                 SPOINT-source vector at TRGEPC. Units are radians. */
/*                 The range of INCDNC is [0, pi]. */

/*     EMISSN      is the emission angle at SPOINT, as seen from OBSRVR */
/*                 at time ET. This is the angle between the surface */
/*                 normal vector at SPOINT and the negative of the */
/*                 vector SRFVEC. Units are radians. The range of EMISSN */
/*                 is [0, pi]. */


/* $ Parameters */

/*     None. */

/* $ Exceptions */


/*     1)  If the specified aberration correction is relativistic or */
/*         calls for stellar aberration but not light time correction, */
/*         the error SPICE(NOTSUPPORTED) is signaled. If the specified */
/*         aberration correction is any other unrecognized value, the */
/*         error will be diagnosed and signaled by a routine in the call */
/*         tree of this routine. */

/*     2)  If any of the target, observer, or illumination source */
/*         input strings cannot be converted to an integer ID code, the */
/*         error SPICE(IDCODENOTFOUND) is signaled. */

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
/*         calling ILLUMG, the error will be diagnosed and signaled by a */
/*         routine in the call tree of this routine. Note that when */
/*         light time correction is used, sufficient ephemeris data must */
/*         be available to propagate the states of observer, target, and */
/*         the illumination source to the solar system barycenter. */

/*     9)  If the computation method specifies an ellipsoidal target */
/*         shape and triaxial radii of the target body have not been */
/*         loaded into the kernel pool prior to calling ILLUMG, the */
/*         error will be diagnosed and signaled by a routine in the call */
/*         tree of this routine. */

/*     10) The target must be an extended body: if any of the radii of */
/*         the target body are non-positive, the error will be */
/*         diagnosed and signaled by routines in the call tree of this */
/*         routine. */

/*     11) If PCK data specifying the target body-fixed frame */
/*         orientation have not been loaded prior to calling ILLUMG, */
/*         the error will be diagnosed and signaled by a routine in the */
/*         call tree of this routine. */


/* $ Files */

/*     Appropriate kernels must be loaded by the calling program before */
/*     this routine is called. */

/*     The following data are required: */

/*        - SPK data: ephemeris data for target, observer, and the */
/*          illumination source must be loaded. If aberration */
/*          corrections are used, the states of target, observer, and */
/*          the illumination source relative to the solar system */
/*          barycenter must be calculable from the available ephemeris */
/*          data. Typically ephemeris data are made available by loading */
/*          one or more SPK files via FURNSH. */

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


/*     The term "illumination angles" refers to following set of */
/*     angles: */


/*        phase angle              Angle between the vectors from the */
/*                                 surface point to the observer and */
/*                                 from the surface point to the */
/*                                 illumination source. */

/*        incidence angle          Angle between the surface normal at */
/*                                 the specified surface point and the */
/*                                 vector from the surface point to the */
/*                                 illumination source. */

/*        emission angle           Angle between the surface normal at */
/*                                 the specified surface point and the */
/*                                 vector from the surface point to the */
/*                                 observer. */

/*     The diagram below illustrates the geometric relationships */
/*     defining these angles. The labels for the incidence, emission, */
/*     and phase angles are "inc.", "e.", and "phase". */


/*                                                      * */
/*                                              illumination source */

/*                    surface normal vector */
/*                              ._                 _. */
/*                              |\                 /|  illumination */
/*                                \    phase      /    source vector */
/*                                 \   .    .    / */
/*                                 .            . */
/*                                   \   ___   / */
/*                              .     \/     \/ */
/*                                    _\ inc./ */
/*                             .    /   \   / */
/*                             .   |  e. \ / */
/*         *             <--------------- *  surface point on */
/*      viewing            vector            target body */
/*      location           to viewing */
/*      (observer)         location */


/*     Note that if the target-observer vector, the target normal vector */
/*     at the surface point, and the target-illumination source vector */
/*     are coplanar, then phase is the sum of the incidence and emission */
/*     angles. This rarely occurs; usually */

/*        phase angle  <  incidence angle + emission angle */

/*     All of the above angles can be computed using light time */
/*     corrections, light time and stellar aberration corrections, or no */
/*     aberration corrections. In order to describe apparent geometry as */
/*     observed by a remote sensing instrument, both light time and */
/*     stellar aberration corrections should be used. */

/*     The way aberration corrections are applied by this routine */
/*     is described below. */

/*        Light time corrections */
/*        ====================== */

/*           Observer-target surface point vector */
/*           ------------------------------------ */

/*           Let ET be the epoch at which an observation or remote */
/*           sensing measurement is made, and let ET - LT ("LT" stands */
/*           for "light time") be the epoch at which the photons */
/*           received at ET were emitted from the surface point SPOINT. */
/*           Note that the light time between the surface point and */
/*           observer will generally differ from the light time between */
/*           the target body's center and the observer. */


/*           Target body's orientation */
/*           ------------------------- */

/*           Using the definitions of ET and LT above, the target body's */
/*           orientation at ET - LT is used. The surface normal is */
/*           dependent on the target body's orientation, so the body's */
/*           orientation model must be evaluated for the correct epoch. */


/*           Target body -- illumination source vector */
/*           ----------------------------------------- */

/*           The surface features on the target body near SPOINT will */
/*           appear in a measurement made at ET as they were at ET-LT. */
/*           In particular, lighting on the target body is dependent on */
/*           the apparent location of the illumination source as seen */
/*           from the target body at ET-LT. So, a second light time */
/*           correction is used to compute the position of the */
/*           illumination source relative to the surface point. */


/*        Stellar aberration corrections */
/*        ============================== */

/*        Stellar aberration corrections are applied only if */
/*        light time corrections are applied as well. */

/*           Observer-target surface point body vector */
/*           ----------------------------------------- */

/*           When stellar aberration correction is performed, the */
/*           direction vector SRFVEC is adjusted so as to point to the */
/*           apparent position of SPOINT: considering SPOINT to be an */
/*           ephemeris object, SRFVEC points from the observer's */
/*           position at ET to the light time and stellar aberration */
/*           corrected position of SPOINT. */

/*           Target body-illumination source vector */
/*           -------------------------------------- */

/*           The target body-illumination source vector is the apparent */
/*           position of the illumination source, corrected for light */
/*           time and stellar aberration, as seen from the target body */
/*           at time ET-LT. */


/* $ Examples */

/*     The numerical results shown for this example may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Find the phase, solar incidence, and emission angles at the */
/*        sub-solar and sub-spacecraft points on Mars as seen from the */
/*        Mars Global Surveyor spacecraft at a specified UTC time. Use */
/*        light time and stellar aberration corrections. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*        KPL/MK */

/*           File: illumg.tm */

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
/*              de421.bsp                     Planetary ephemeris */
/*              pck00010.tpc                  Planet orientation and */
/*                        radii */
/*              naif0010.tls                  Leapseconds */
/*              mgs_ext13_ipng_mgs95j.bsp     MGS ephemeris */

/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'de421.bsp', */
/*                                  'pck00010.tpc', */
/*                                  'naif0010.tls', */
/*                                  'mgs_ext13_ipng_mgs95j.bsp'  ) */
/*           \begintext */


/*        Example code begins here. */


/*           PROGRAM ANGLES */
/*           IMPLICIT NONE */
/*     C */
/*     C     SPICELIB functions */
/*     C */
/*           DOUBLE PRECISION      DPR */
/*     C */
/*     C     Local parameters */
/*     C */
/*           CHARACTER*(*)         META */
/*           PARAMETER           ( META   = 'illumg.tm' ) */

/*           INTEGER               NAMLEN */
/*           PARAMETER           ( NAMLEN = 32 ) */

/*           INTEGER               TIMLEN */
/*           PARAMETER           ( TIMLEN = 25 ) */

/*           INTEGER               CORLEN */
/*           PARAMETER           ( CORLEN = 5 ) */
/*     C */
/*     C     Local variables */
/*     C */
/*           CHARACTER*(CORLEN)    ABCORR */
/*           CHARACTER*(NAMLEN)    OBSRVR */
/*           CHARACTER*(NAMLEN)    TARGET */
/*           CHARACTER*(TIMLEN)    UTC */

/*           DOUBLE PRECISION      ET */
/*           DOUBLE PRECISION      SRFVEC ( 3 ) */
/*           DOUBLE PRECISION      SSCEMI */
/*           DOUBLE PRECISION      SSCPHS */
/*           DOUBLE PRECISION      SSCPT  ( 3 ) */
/*           DOUBLE PRECISION      SSCSOL */
/*           DOUBLE PRECISION      SSLEMI */
/*           DOUBLE PRECISION      SSLPHS */
/*           DOUBLE PRECISION      SSLSOL */
/*           DOUBLE PRECISION      SSOLPT ( 3 ) */
/*           DOUBLE PRECISION      TRGEPC */

/*     C */
/*     C     Load kernel files. */
/*     C */
/*           CALL FURNSH ( META ) */
/*     C */
/*     C     Convert the UTC request time string to seconds past */
/*     C     J2000 TDB. */
/*     C */
/*           UTC = '2004 JAN 1 12:00:00' */

/*           CALL UTC2ET ( UTC, ET ) */

/*     C */
/*     C     Assign observer and target names. The acronym MGS */
/*     C     indicates Mars Global Surveyor. See NAIF_IDS for a */
/*     C     list of names recognized by SPICE. Also set the */
/*     C     aberration correction flag. */
/*     C */
/*           TARGET = 'Mars' */
/*           OBSRVR = 'MGS' */
/*           ABCORR = 'CN+S' */
/*     C */
/*     C     Find the sub-solar point on the Earth as seen from */
/*     C     the MGS spacecraft at ET. Use the "near point: ellipsoid" */
/*     C     style of sub-point definition. This makes it easy */
/*     C     to verify the solar incidence angle. */
/*     C */
/*           CALL SUBSLR ( 'Near point: ellipsoid', */
/*          .              TARGET,  ET,      'IAU_MARS', */
/*          .              ABCORR,  OBSRVR,  SSOLPT, TRGEPC, SRFVEC ) */
/*     C */
/*     C     Now find the sub-spacecraft point. */
/*     C */
/*           CALL SUBPNT ( 'Near point: ellipsoid', */
/*          .              TARGET,  ET,     'IAU_MARS', */
/*          .              ABCORR,  OBSRVR, SSCPT,   TRGEPC, SRFVEC ) */
/*     C */
/*     C     Find the phase, solar incidence, and emission */
/*     C     angles at the sub-solar point on the Earth as seen */
/*     C     from MGS at time ET. */
/*     C */
/*           CALL ILLUMG ( 'Ellipsoid', TARGET, 'SUN',  ET, */
/*          .              'IAU_MARS',  ABCORR, OBSRVR, SSOLPT, */
/*          .              TRGEPC,      SRFVEC, SSLPHS, SSLSOL, SSLEMI ) */
/*     C */
/*     C     Do the same for the sub-spacecraft point. */
/*     C */
/*           CALL ILLUMG ( 'Ellipsoid', TARGET, 'SUN',  ET, */
/*          .              'IAU_MARS',  ABCORR, OBSRVR, SSCPT, */
/*          .               TRGEPC,     SRFVEC, SSCPHS, SSCSOL, SSCEMI ) */
/*     C */
/*     C     Convert the angles to degrees and write them out. */
/*     C */
/*           SSLPHS = DPR() * SSLPHS */
/*           SSLSOL = DPR() * SSLSOL */
/*           SSLEMI = DPR() * SSLEMI */

/*           SSCPHS = DPR() * SSCPHS */
/*           SSCSOL = DPR() * SSCSOL */
/*           SSCEMI = DPR() * SSCEMI */

/*           WRITE (*,*) ' ' */
/*           WRITE (*,*) 'UTC epoch is ', UTC */
/*           WRITE (*,*) ' ' */
/*           WRITE (*,*) 'Illumination angles at the sub-solar point:' */
/*           WRITE (*,*) ' ' */
/*           WRITE (*,*) 'Phase angle           (deg.): ', SSLPHS */
/*           WRITE (*,*) 'Solar incidence angle (deg.): ', SSLSOL */
/*           WRITE (*,*) 'Emission angle        (deg.): ', SSLEMI */
/*           WRITE (*,*) ' ' */
/*           WRITE (*,*) 'The solar incidence angle should be 0.' */
/*           WRITE (*,*) 'The emission and phase angles should be equal.' */

/*           WRITE (*,*) ' ' */
/*           WRITE (*,*) 'Illumination angles at the sub-s/c point:' */
/*           WRITE (*,*) ' ' */
/*           WRITE (*,*) 'Phase angle           (deg.): ', SSCPHS */
/*           WRITE (*,*) 'Solar incidence angle (deg.): ', SSCSOL */
/*           WRITE (*,*) 'Emission angle        (deg.): ', SSCEMI */
/*           WRITE (*,*) ' ' */
/*           WRITE (*,*) 'The emission angle should be 0.' */
/*           WRITE (*,*) 'The solar incidence and phase angles should ' */
/*          .//          'be equal.' */
/*           END */


/*     When this program was executed on a PC/Linux/gfortran platform, */
/*     the output was: */


/*        UTC epoch is 2004 JAN 1 12:00:00 */

/*        Illumination angles at the sub-solar point: */

/*        Phase angle           (deg.):    115.54199464940093 */
/*        Solar incidence angle (deg.):   8.27288196025359598E-015 */
/*        Emission angle        (deg.):    115.54199464940093 */

/*        The solar incidence angle should be 0. */
/*        The emission and phase angles should be equal. */

/*        Illumination angles at the sub-s/c point: */

/*        Phase angle           (deg.):    62.083997890874976 */
/*        Solar incidence angle (deg.):    62.083997892615827 */
/*        Emission angle        (deg.):   2.13680201386761237E-009 */

/*        The emission angle should be 0. */
/*        The solar incidence and phase angles should be equal. */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     B.V. Semenov   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 31-MAR-2014 (NJB)(BVS) */

/* -& */
/* $ Index_Entries */

/*     illumination angles general source */
/*     lighting angles general source */
/*     phase angle general source */
/*     incidence angle general source */
/*     emission angle general source */

/* -& */
/* $ Revisions */

/*     None. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Saved body name length. */


/*     Saved frame name length. */


/*     Local variables */


/*     Saved name/ID item declarations. */


/*     Saved frame name/ID item declarations. */


/*     Saved variables */


/*     Saved name/ID items. */


/*     Saved frame name/ID items. */


/*     Note: XMIT need not be saved, since it's used only */
/*     for error checking when an aberration correction flag */
/*     is parsed. */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ILLUMG", (ftnlen)6);

/*     Counter initialization is done separately. */

    if (first) {

/*        Initialize counters. */

	zzctruin_(svctr1);
	zzctruin_(svctr2);
	zzctruin_(svctr3);
    }

/*     If necessary, parse the aberration correction flag. */

    if (first || s_cmp(abcorr, prvcor, abcorr_len, (ftnlen)5) != 0) {

/*        The aberration correction flag differs from the value it */
/*        had on the previous call, if any. Analyze the new flag. */

	zzvalcor_(abcorr, attblk, abcorr_len);
	if (failed_()) {
	    chkout_("ILLUMG", (ftnlen)6);
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

/*           USESTL indicates stellar aberration corrections. */


/*        The above definitions are consistent with those used by */
/*        ZZVALCOR. */

	xmit = attblk[4];
	uselt = attblk[1];

/*        Reject an aberration correction flag calling for transmission */
/*        corrections. */

	if (xmit) {
	    setmsg_("Aberration correction flag # calls for transmission-sty"
		    "le corrections.", (ftnlen)70);
	    errch_("#", abcorr, (ftnlen)1, abcorr_len);
	    sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	    chkout_("ILLUMG", (ftnlen)6);
	    return 0;
	}

/*       We do NOT set FIRST to .FALSE. here, since we're not */
/*       yet done with it. */

    }

/*     If necessary, parse the method specification. PRVMTH */
/*     and the derived flags NEAR and ELIPSD start out with */
/*     valid values. PRVMTH records the last valid value of */
/*     METHOD; ELIPSD is the corresponding shape flag. */

    if (first || s_cmp(method, prvmth, method_len, (ftnlen)80) != 0) {

/*        Parse the computation method specification. Work with a local */
/*        copy of the method specification that contains no leading or */
/*        embedded blanks. */

	ljucrs_(&c__0, method, locmth, method_len, (ftnlen)80);

/*        Check the shape specification. */

	if (s_cmp(locmth, "ELLIPSOID", (ftnlen)80, (ftnlen)9) != 0) {
	    setmsg_("Computation method argument was <#>; this string must s"
		    "pecify a supported shape model and computation type. See"
		    " the header of SUBSLR for details.", (ftnlen)145);
	    errch_("#", method, (ftnlen)1, method_len);
	    sigerr_("SPICE(INVALIDMETHOD)", (ftnlen)20);
	    chkout_("ILLUMG", (ftnlen)6);
	    return 0;
	}

/*        At this point the method specification has passed our tests. */
/*        Use the flag ELIPSD to indicate that the shape is modeled as */
/*        an ellipsoid (which is true, for now). */

	elipsd = TRUE_;

/*        Save the current value of METHOD. */

	s_copy(prvmth, method, (ftnlen)80, method_len);
    }

/*     We're done with all tasks that must be executed on the first */
/*     pass. */

    first = FALSE_;

/*     Obtain integer codes for the target, observer, and */
/*     illumination source. */

    zzbods2c_(svctr1, svtarg, &svtcde, &svfnd1, target, &trgcde, &fnd, (
	    ftnlen)36, target_len);
    if (! fnd) {
	setmsg_("The target, '#', is not a recognized name for an ephemeris "
		"object. The cause of this problem may be that you need an up"
		"dated version of the SPICE Toolkit. ", (ftnlen)155);
	errch_("#", target, (ftnlen)1, target_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("ILLUMG", (ftnlen)6);
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
	chkout_("ILLUMG", (ftnlen)6);
	return 0;
    }

/*     Check the observer and target body codes. If they are equal, */
/*     signal an error. */

    if (obscde == trgcde) {
	setmsg_("In computing illumination angles, the observing body and ta"
		"rget body are the same. Both are #.", (ftnlen)94);
	errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	sigerr_("SPICE(BODIESNOTDISTINCT)", (ftnlen)24);
	chkout_("ILLUMG", (ftnlen)6);
	return 0;
    }

/*     Determine the attributes of the frame designated by FIXREF. */

    zznamfrm_(svctr3, svfref, &svrefc, fixref, &refcde, (ftnlen)32, 
	    fixref_len);
    frinfo_(&refcde, &center, &type__, &typeid, &fnd);
    if (failed_()) {
	chkout_("ILLUMG", (ftnlen)6);
	return 0;
    }
    if (! fnd) {
	setmsg_("Reference frame # is not recognized by the SPICE frame subs"
		"ystem. Possibly a required frame definition kernel has not b"
		"een loaded.", (ftnlen)130);
	errch_("#", fixref, (ftnlen)1, fixref_len);
	sigerr_("SPICE(NOFRAME)", (ftnlen)14);
	chkout_("ILLUMG", (ftnlen)6);
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
	chkout_("ILLUMG", (ftnlen)6);
	return 0;
    }

/*     Get the sign S prefixing LT in the expression for TRGEPC. */
/*     When light time correction is not used, setting S = 0 */
/*     allows us to seamlessly set TRGEPC equal to ET. */

/*     We don't support transmission corrections, so S is never */
/*     set to 1. */

    if (uselt) {
	s = -1.;
    } else {
	s = 0.;
    }

/*     Look up the state of the surface point relative to the observer. */
/*     The body-fixed frame of the surface point is to be evaluated */
/*     at the epoch of the surface point, not at the epoch of the */
/*     center of the frame; we indicate this by setting the input */
/*     argument EVLREF to 'TARGET'. */

    spkcpt_(spoint, target, fixref, et, fixref, "TARGET", abcorr, obsrvr, 
	    opstat, &lt, target_len, fixref_len, fixref_len, (ftnlen)6, 
	    abcorr_len, obsrvr_len);
    if (failed_()) {
	chkout_("ILLUMG", (ftnlen)6);
	return 0;
    }

/*     TRGEPC is the epoch associated with the surface point. Below, */
/*     since S is set to 0.D0 if no aberration corrections are used, we */
/*     require no logical branch. */

    *trgepc = *et + s * lt;

/*     Now find the state of the illumination source as seen by */
/*     the surface point at TRGEPC. We want to evaluate the orientation */
/*     of the body-fixed frame of the surface point at the epoch */
/*     associated with the surface point, not at the epoch associated */
/*     with the frame's center;  we indicate this by setting the input */
/*     argument EVLREF to 'OBSERVER'. */

    spkcpo_(illum, trgepc, fixref, "OBSERVER", abcorr, spoint, target, fixref,
	     tistat, &lti, illum_len, fixref_len, (ftnlen)8, abcorr_len, 
	    target_len, fixref_len);
    if (failed_()) {
	chkout_("ILLUMG", (ftnlen)6);
	return 0;
    }

/*     Find the surface normal at SPOINT. This computation depends */
/*     on target body shape model. */

    if (elipsd) {

/*        We'll need the radii of the target body. */

	bodvcd_(&trgcde, "RADII", &c__3, &n, radii, (ftnlen)5);
	if (failed_()) {
	    chkout_("ILLUMG", (ftnlen)6);
	    return 0;
	}
	surfnm_(radii, &radii[1], &radii[2], spoint, normal);
    } else {

/*        We've already checked the computation method input argument, */
/*        so we don't expect to arrive here. This code is present for */
/*        safety. */

	setmsg_("The computation method # was not recognized. ", (ftnlen)45);
	errch_("#", method, (ftnlen)1, method_len);
	sigerr_("SPICE(INVALIDMETHOD)", (ftnlen)20);
	chkout_("ILLUMG", (ftnlen)6);
	return 0;
    }

/*     We'll need the negative of the observer-surface point position */
/*     for the following angle computation. Set the output SRFVEC while */
/*     we're at it. */

    vequ_(opstat, srfvec);
    vminus_(srfvec, obspos);

/*     Find the illumination angles. VSEP will give us angular */
/*     separation in radians. */

    *phase = vsep_(obspos, tistat);
    *incdnc = vsep_(normal, tistat);
    *emissn = vsep_(normal, obspos);

/*     TRGEPC has already been set. */

    chkout_("ILLUMG", (ftnlen)6);
    return 0;
} /* illumg_ */

