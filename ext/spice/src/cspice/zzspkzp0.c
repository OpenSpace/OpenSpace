/* zzspkzp0.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZSPKZP0 ( S/P Kernel, easy position ) */
/* Subroutine */ int zzspkzp0_(integer *targ, doublereal *et, char *ref, char 
	*abcorr, integer *obs, doublereal *ptarg, doublereal *lt, ftnlen 
	ref_len, ftnlen abcorr_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    static integer fj2000;
    extern /* Subroutine */ int zzrefch0_(integer *, integer *, doublereal *, 
	    doublereal *), zzspkpa0_(integer *, doublereal *, char *, 
	    doublereal *, char *, doublereal *, doublereal *, ftnlen, ftnlen);
    static doublereal temp[3], sobs[6];
    extern /* Subroutine */ int zzspkgp0_(integer *, doublereal *, char *, 
	    integer *, doublereal *, doublereal *, ftnlen), zzspksb0_(integer 
	    *, doublereal *, char *, doublereal *, ftnlen);
    static integer type__;
    static logical xmit;
    static integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern logical eqchr_(char *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen);
    static logical found;
    extern integer ltrim_(char *, ftnlen);
    static doublereal xform[9]	/* was [3][3] */;
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    static doublereal postn[3];
    extern logical failed_(void);
    static integer center;
    extern /* Subroutine */ int namfrm_(char *, integer *, ftnlen), frinfo_(
	    integer *, integer *, integer *, integer *, logical *);
    static doublereal ltcent;
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    static integer reqfrm, typeid;
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int mxv_(doublereal *, doublereal *, doublereal *)
	    ;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Return the position of a target body relative to an observing */
/*     body, optionally corrected for light time (planetary aberration) */
/*     and stellar aberration. */

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

/*     SPK */
/*     NAIF_IDS */
/*     FRAMES */
/*     TIME */

/* $ Keywords */

/*     EPHEMERIS */

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
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     TARG       I   Target body NAIF ID code. */
/*     ET         I   Observer epoch. */
/*     REF        I   Reference frame of output position vector. */
/*     ABCORR     I   Aberration correction flag. */
/*     OBS        I   Observing body NAIF ID code. */
/*     PTARG      O   Position of target. */
/*     LT         O   One way light time between observer and target. */

/* $ Detailed_Input */

/*     TARG        is the NAIF ID code for a target body.  The target */
/*                 and observer define a position vector which points */
/*                 from the observer to the target. */

/*     ET          is the ephemeris time, expressed as seconds past */
/*                 J2000 TDB, at which the position of the target body */
/*                 relative to the observer is to be computed.  ET */
/*                 refers to time at the observer's location. */

/*     REF         is the name of the reference frame relative to which */
/*                 the output position vector should be expressed. This */
/*                 may be any frame supported by the SPICE system, */
/*                 including built-in frames (documented in the Frames */
/*                 Required Reading) and frames defined by a loaded */
/*                 frame kernel (FK). */

/*                 When REF designates a non-inertial frame, the */
/*                 orientation of the frame is evaluated at an epoch */
/*                 dependent on the selected aberration correction. See */
/*                 the description of the output position vector PTARG */
/*                 for details. */

/*     ABCORR      indicates the aberration corrections to be applied to */
/*                 the position of the target body to account for */
/*                 one-way light time and stellar aberration.  See the */
/*                 discussion in the Particulars section for */
/*                 recommendations on how to choose aberration */
/*                 corrections. */

/*                 ABCORR may be any of the following: */

/*                    'NONE'     Apply no correction. Return the */
/*                               geometric position of the target body */
/*                               relative to the observer. */

/*                 The following values of ABCORR apply to the */
/*                 "reception" case in which photons depart from the */
/*                 target's location at the light-time corrected epoch */
/*                 ET-LT and *arrive* at the observer's location at ET: */

/*                    'LT'       Correct for one-way light time (also */
/*                               called "planetary aberration") using a */
/*                               Newtonian formulation. This correction */
/*                               yields the position of the target at */
/*                               the moment it emitted photons arriving */
/*                               at the observer at ET. */

/*                               The light time correction uses an */
/*                               iterative solution of the light time */
/*                               equation (see Particulars for details). */
/*                               The solution invoked by the 'LT' option */
/*                               uses one iteration. */

/*                    'LT+S'     Correct for one-way light time and */
/*                               stellar aberration using a Newtonian */
/*                               formulation. This option modifies the */
/*                               position obtained with the 'LT' option */
/*                               to account for the observer's velocity */
/*                               relative to the solar system */
/*                               barycenter. The result is the apparent */
/*                               position of the target---the position */
/*                               as seen by the observer. */

/*                    'CN'       Converged Newtonian light time */
/*                               correction.  In solving the light time */
/*                               equation, the 'CN' correction iterates */
/*                               until the solution converges (three */
/*                               iterations on all supported platforms). */

/*                               The 'CN' correction typically does not */
/*                               substantially improve accuracy because */
/*                               the errors made by ignoring */
/*                               relativistic effects may be larger than */
/*                               the improvement afforded by obtaining */
/*                               convergence of the light time solution. */
/*                               The 'CN' correction computation also */
/*                               requires a significantly greater number */
/*                               of CPU cycles than does the */
/*                               one-iteration light time correction. */

/*                    'CN+S'     Converged Newtonian light time */
/*                               and stellar aberration corrections. */


/*                 The following values of ABCORR apply to the */
/*                 "transmission" case in which photons *depart* from */
/*                 the observer's location at ET and arrive at the */
/*                 target's location at the light-time corrected epoch */
/*                 ET+LT: */

/*                    'XLT'      "Transmission" case:  correct for */
/*                               one-way light time using a Newtonian */
/*                               formulation. This correction yields the */
/*                               position of the target at the moment it */
/*                               receives photons emitted from the */
/*                               observer's location at ET. */

/*                    'XLT+S'    "Transmission" case:  correct for */
/*                               one-way light time and stellar */
/*                               aberration using a Newtonian */
/*                               formulation  This option modifies the */
/*                               position obtained with the 'XLT' option */
/*                               to account for the observer's velocity */
/*                               relative to the solar system */
/*                               barycenter. The position component of */
/*                               the computed target position indicates */
/*                               the direction that photons emitted from */
/*                               the observer's location must be "aimed" */
/*                               to hit the target. */

/*                    'XCN'      "Transmission" case:  converged */
/*                               Newtonian light time correction. */

/*                    'XCN+S'    "Transmission" case:  converged */
/*                               Newtonian light time and stellar */
/*                               aberration corrections. */


/*                 Neither special nor general relativistic effects are */
/*                 accounted for in the aberration corrections applied */
/*                 by this routine. */

/*                 Case and blanks are not significant in the string */
/*                 ABCORR. */

/*     OBS         is the NAIF ID code for the observing body. */

/* $ Detailed_Output */

/*     PTARG       is a Cartesian 3-vector representing the position of */
/*                 the target body relative to the specified observer. */
/*                 PTARG is corrected for the specified aberrations, and */
/*                 is expressed with respect to the reference frame */
/*                 specified by REF.  The three components of PTARG */
/*                 represent the x-, y- and z-components of the target's */
/*                 position. */

/*                 PTARG points from the observer's location at ET to */
/*                 the aberration-corrected location of the target. */
/*                 Note that the sense of this position vector is */
/*                 independent of the direction of radiation travel */
/*                 implied by the aberration correction. */

/*                 Units are always km. */

/*                 Non-inertial frames are treated as follows: letting */
/*                 LTCENT be the one-way light time between the observer */
/*                 and the central body associated with the frame, the */
/*                 orientation of the frame is evaluated at ET-LTCENT, */
/*                 ET+LTCENT, or ET depending on whether the requested */
/*                 aberration correction is, respectively, for received */
/*                 radiation, transmitted radiation, or is omitted. */
/*                 LTCENT is computed using the method indicated by */
/*                 ABCORR. */

/*     LT          is the one-way light time between the observer and */
/*                 target in seconds.  If the target position is */
/*                 corrected for aberrations, then LT is the one-way */
/*                 light time between the observer and the light time */
/*                 corrected target location. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If name of target or observer cannot be translated to its */
/*        NAIF ID code, the error SPICE(IDCODENOTFOUND) is signaled. */

/*     2) If the reference frame REF is not a recognized reference */
/*        frame the error 'SPICE(UNKNOWNFRAME)' is signaled. */

/*     3) If the loaded kernels provide insufficient data to */
/*        compute the requested position vector, the deficiency will */
/*        be diagnosed by a routine in the call tree of this routine. */

/*     4) If an error occurs while reading an SPK or other kernel file, */
/*        the error  will be diagnosed by a routine in the call tree */
/*        of this routine. */

/* $ Files */

/*     This routine computes positions using SPK files that have been */
/*     loaded into the SPICE system, normally via the kernel loading */
/*     interface routine FURNSH. See the routine FURNSH and the SPK */
/*     and KERNEL Required Reading for further information on loading */
/*     (and unloading) kernels. */

/*     If the output position PTARG is to be expressed relative to a */
/*     non-inertial frame, or if any of the ephemeris data used to */
/*     compute PTARG are expressed relative to a non-inertial frame in */
/*     the SPK files providing those data, additional kernels may be */
/*     needed to enable the reference frame transformations required to */
/*     compute the position.  Normally these additional kernels are PCK */
/*     files or frame kernels.  Any such kernels must already be loaded */
/*     at the time this routine is called. */

/* $ Particulars */

/*     This routine is part of the user interface to the SPICE ephemeris */
/*     system.  It allows you to retrieve position information for any */
/*     ephemeris object relative to any other in a reference frame that */
/*     is convenient for further computations. */


/*     Aberration corrections */
/*     ====================== */

/*     In space science or engineering applications one frequently */
/*     wishes to know where to point a remote sensing instrument, such */
/*     as an optical camera or radio antenna, in order to observe or */
/*     otherwise receive radiation from a target.  This pointing problem */
/*     is complicated by the finite speed of light:  one needs to point */
/*     to where the target appears to be as opposed to where it actually */
/*     is at the epoch of observation.  We use the adjectives */
/*     "geometric," "uncorrected," or "true" to refer to an actual */
/*     position or state of a target at a specified epoch.  When a */
/*     geometric position or state vector is modified to reflect how it */
/*     appears to an observer, we describe that vector by any of the */
/*     terms "apparent," "corrected," "aberration corrected," or "light */
/*     time and stellar aberration corrected." The SPICE Toolkit can */
/*     correct for two phenomena affecting the apparent location of an */
/*     object:  one-way light time (also called "planetary aberration") */
/*     and stellar aberration. */

/*     One-way light time */
/*     ------------------ */

/*     Correcting for one-way light time is done by computing, given an */
/*     observer and observation epoch, where a target was when the */
/*     observed photons departed the target's location.  The vector from */
/*     the observer to this computed target location is called a "light */
/*     time corrected" vector.  The light time correction depends on the */
/*     motion of the target relative to the solar system barycenter, but */
/*     it is independent of the velocity of the observer relative to the */
/*     solar system barycenter. Relativistic effects such as light */
/*     bending and gravitational delay are not accounted for in the */
/*     light time correction performed by this routine. */

/*     Stellar aberration */
/*     ------------------ */

/*     The velocity of the observer also affects the apparent location */
/*     of a target:  photons arriving at the observer are subject to a */
/*     "raindrop effect" whereby their velocity relative to the observer */
/*     is, using a Newtonian approximation, the photons' velocity */
/*     relative to the solar system barycenter minus the velocity of the */
/*     observer relative to the solar system barycenter.  This effect is */
/*     called "stellar aberration."  Stellar aberration is independent */
/*     of the velocity of the target.  The stellar aberration formula */
/*     used by this routine does not include (the much smaller) */
/*     relativistic effects. */

/*     Stellar aberration corrections are applied after light time */
/*     corrections:  the light time corrected target position vector is */
/*     used as an input to the stellar aberration correction. */

/*     When light time and stellar aberration corrections are both */
/*     applied to a geometric position vector, the resulting position */
/*     vector indicates where the target "appears to be" from the */
/*     observer's location. */

/*     As opposed to computing the apparent position of a target, one */
/*     may wish to compute the pointing direction required for */
/*     transmission of photons to the target.  This also requires */
/*     correction of the geometric target position for the effects of */
/*     light time and stellar aberration, but in this case the */
/*     corrections are computed for radiation traveling *from* the */
/*     observer to the target. */

/*     The "transmission" light time correction yields the target's */
/*     location as it will be when photons emitted from the observer's */
/*     location at ET arrive at the target.  The transmission stellar */
/*     aberration correction is the inverse of the traditional stellar */
/*     aberration correction:  it indicates the direction in which */
/*     radiation should be emitted so that, using a Newtonian */
/*     approximation, the sum of the velocity of the radiation relative */
/*     to the observer and of the observer's velocity, relative to the */
/*     solar system barycenter, yields a velocity vector that points in */
/*     the direction of the light time corrected position of the target. */

/*     One may object to using the term "observer" in the transmission */
/*     case, in which radiation is emitted from the observer's location. */
/*     The terminology was retained for consistency with earlier */
/*     documentation. */

/*     Below, we indicate the aberration corrections to use for some */
/*     common applications: */

/*        1) Find the apparent direction of a target for a remote-sensing */
/*           observation. */

/*              Use 'LT+S':  apply both light time and stellar */
/*              aberration corrections. */

/*           Note that using light time corrections alone ('LT') is */
/*           generally not a good way to obtain an approximation to an */
/*           apparent target vector:  since light time and stellar */
/*           aberration corrections often partially cancel each other, */
/*           it may be more accurate to use no correction at all than to */
/*           use light time alone. */


/*        2) Find the corrected pointing direction to radiate a signal */
/*           to a target.  This computation is often applicable for */
/*           implementing communications sessions. */

/*              Use 'XLT+S':  apply both light time and stellar */
/*              aberration corrections for transmission. */


/*        3) Compute the apparent position of a target body relative */
/*           to a star or other distant object. */

/*              Use 'LT' or 'LT+S' as needed to match the correction */
/*              applied to the position of the distant object.  For */
/*              example, if a star position is obtained from a catalog, */
/*              the position vector may not be corrected for stellar */
/*              aberration.  In this case, to find the angular */
/*              separation of the star and the limb of a planet, the */
/*              vector from the observer to the planet should be */
/*              corrected for light time but not stellar aberration. */


/*        4) Obtain an uncorrected position vector derived directly from */
/*           data in an SPK file. */

/*              Use 'NONE'. */


/*        5) Use a geometric position vector as a low-accuracy estimate */
/*           of the apparent position for an application where execution */
/*           speed is critical. */

/*              Use 'NONE'. */


/*        6) While this routine cannot perform the relativistic */
/*           aberration corrections required to compute positions */
/*           with the highest possible accuracy, it can supply the */
/*           geometric positions required as inputs to these */
/*           computations. */

/*              Use 'NONE', then apply high-accuracy aberration */
/*              corrections (not available in the SPICE Toolkit). */


/*     Below, we discuss in more detail how the aberration corrections */
/*     applied by this routine are computed. */

/*        Geometric case */
/*        ============== */

/*        ZZSPKZP0 begins by computing the geometric position T(ET) of */
/*        the target body relative to the solar system barycenter (SSB). */
/*        Subtracting the geometric position of the observer O(ET) gives */
/*        the geometric position of the target body relative to the */
/*        observer. The one-way light time, LT, is given by */

/*                  | T(ET) - O(ET) | */
/*           LT = ------------------- */
/*                          c */

/*        The geometric relationship between the observer, target, and */
/*        solar system barycenter is as shown: */


/*           SSB ---> O(ET) */
/*            |      / */
/*            |     / */
/*            |    / */
/*            |   /  T(ET) - O(ET) */
/*            V  V */
/*           T(ET) */


/*        The returned position vector is */

/*           T(ET) - O(ET) */



/*        Reception case */
/*        ============== */

/*        When any of the options 'LT', 'CN', 'LT+S', 'CN+S' is selected */
/*        for ABCORR, ZZSPKZP0 computes the position of the target body */
/*        at epoch ET-LT, where LT is the one-way light time.  Let T(t) */
/*        and O(t) represent the positions of the target and observer */
/*        relative to the solar system barycenter at time t; then LT is */
/*        the solution of the light-time equation */

/*                  | T(ET-LT) - O(ET) | */
/*           LT = ------------------------                            (1) */
/*                           c */

/*        The ratio */

/*            | T(ET) - O(ET) | */
/*          ---------------------                                     (2) */
/*                    c */

/*        is used as a first approximation to LT; inserting (2) into the */
/*        right hand side of the light-time equation (1) yields the */
/*        "one-iteration" estimate of the one-way light time ("LT"). */
/*        Repeating the process until the estimates of LT converge */
/*        yields the "converged Newtonian" light time estimate ("CN"). */

/*        Subtracting the geometric position of the observer O(ET) gives */
/*        the position of the target body relative to the observer: */
/*        T(ET-LT) - O(ET). */

/*           SSB ---> O(ET) */
/*            | \     | */
/*            |  \    | */
/*            |   \   | T(ET-LT) - O(ET) */
/*            |    \  | */
/*            V     V V */
/*           T(ET)  T(ET-LT) */

/*        The light time corrected position vector is */

/*           T(ET-LT) - O(ET) */

/*        If correction for stellar aberration is requested, the target */
/*        position is rotated toward the solar system barycenter- */
/*        relative velocity vector of the observer.  The rotation is */
/*        computed as follows: */

/*           Let r be the light time corrected vector from the observer */
/*           to the object, and v be the velocity of the observer with */
/*           respect to the solar system barycenter. Let w be the angle */
/*           between them. The aberration angle phi is given by */

/*              sin(phi) = v sin(w) / c */

/*           Let h be the vector given by the cross product */

/*              h = r X v */

/*           Rotate r by phi radians about h to obtain the apparent */
/*           position of the object. */


/*        Transmission case */
/*        ================== */

/*        When any of the options 'XLT', 'XCN', 'XLT+S', 'XCN+S' is */
/*        selected, ZZSPKZP0 computes the position of the target body T */
/*        at epoch ET+LT, where LT is the one-way light time.  LT is the */
/*        solution of the light-time equation */

/*                  | T(ET+LT) - O(ET) | */
/*           LT = ------------------------                            (3) */
/*                            c */

/*        Subtracting the geometric position of the observer, O(ET), */
/*        gives the position of the target body relative to the */
/*        observer: T(ET-LT) - O(ET). */

/*                   SSB --> O(ET) */
/*                  / |    * */
/*                 /  |  *  T(ET+LT) - O(ET) */
/*                /   |* */
/*               /   *| */
/*              V  V  V */
/*          T(ET+LT)  T(ET) */

/*        The light-time corrected position vector is */

/*           T(ET+LT) - O(ET) */

/*        If correction for stellar aberration is requested, the target */
/*        position is rotated away from the solar system barycenter- */
/*        relative velocity vector of the observer. The rotation is */
/*        computed as in the reception case, but the sign of the */
/*        rotation angle is negated. */


/*     Precision of light time corrections */
/*     =================================== */

/*        Corrections using one iteration of the light time solution */
/*        ---------------------------------------------------------- */

/*        When the requested aberration correction is 'LT', 'LT+S', */
/*        'XLT', or 'XLT+S', only one iteration is performed in the */
/*        algorithm used to compute LT. */

/*        The relative error in this computation */

/*           | LT_ACTUAL - LT_COMPUTED |  /  LT_ACTUAL */

/*        is at most */

/*            (V/C)**2 */
/*           ---------- */
/*            1 - (V/C) */

/*        which is well approximated by (V/C)**2, where V is the */
/*        velocity of the target relative to an inertial frame and C is */
/*        the speed of light. */

/*        For nearly all objects in the solar system V is less than 60 */
/*        km/sec.  The value of C is 300000 km/sec.  Thus the one */
/*        iteration solution for LT has a potential relative error of */
/*        not more than 4*10**-8.  This is a potential light time error */
/*        of approximately 2*10**-5 seconds per astronomical unit of */
/*        distance separating the observer and target.  Given the bound */
/*        on V cited above: */

/*           As long as the observer and target are */
/*           separated by less than 50 astronomical units, */
/*           the error in the light time returned using */
/*           the one-iteration light time corrections */
/*           is less than 1 millisecond. */


/*        Converged corrections */
/*        --------------------- */

/*        When the requested aberration correction is 'CN', 'CN+S', */
/*        'XCN', or 'XCN+S', three iterations are performed in the */
/*        computation of LT.  The relative error present in this */
/*        solution is at most */

/*            (V/C)**4 */
/*           ---------- */
/*            1 - (V/C) */

/*        which is well approximated by (V/C)**4.  Mathematically the */
/*        precision of this computation is better than a nanosecond for */
/*        any pair of objects in the solar system. */

/*        However, to model the actual light time between target and */
/*        observer one must take into account effects due to general */
/*        relativity.  These may be as high as a few hundredths of a */
/*        millisecond for some objects. */

/*        When one considers the extra time required to compute the */
/*        converged Newtonian light time (the state of the target */
/*        relative to the solar system barycenter is looked up three */
/*        times instead of once) together with the real gain in */
/*        accuracy, it seems unlikely that you will want to request */
/*        either the "CN" or "CN+S" light time corrections.  However, */
/*        these corrections can be useful for testing situations where */
/*        high precision (as opposed to accuracy) is required. */


/*     Relativistic Corrections */
/*     ========================= */

/*     This routine does not attempt to perform either general or */
/*     special relativistic corrections in computing the various */
/*     aberration corrections.  For many applications relativistic */
/*     corrections are not worth the expense of added computation */
/*     cycles.  If however, your application requires these additional */
/*     corrections we suggest you consult the astronomical almanac (page */
/*     B36) for a discussion of how to carry out these corrections. */


/* $ Examples */

/*     1)  Load a planetary ephemeris SPK, then look up a series of */
/*         geometric positions of the moon relative to the earth, */
/*         referenced to the J2000 frame. */


/*               IMPLICIT NONE */
/*         C */
/*         C     Local constants */
/*         C */
/*               CHARACTER*(*)         FRAME */
/*               PARAMETER           ( FRAME  = 'J2000' ) */

/*               CHARACTER*(*)         ABCORR */
/*               PARAMETER           ( ABCORR = 'NONE' ) */

/*         C */
/*         C     The name of the SPK file shown here is fictitious; */
/*         C     you must supply the name of an SPK file available */
/*         C     on your own computer system. */
/*         C */
/*               CHARACTER*(*)         SPK */
/*               PARAMETER           ( SPK    = 'planet.bsp' ) */

/*         C */
/*         C     ET0 represents the date 2000 Jan 1 12:00:00 TDB. */
/*         C */
/*               DOUBLE PRECISION      ET0 */
/*               PARAMETER           ( ET0    = 0.0D0 ) */

/*         C */
/*         C     Use a time step of 1 hour; look up 100 positions. */
/*         C */
/*               DOUBLE PRECISION      STEP */
/*               PARAMETER           ( STEP   = 3600.0D0 ) */

/*               INTEGER               MAXITR */
/*               PARAMETER           ( MAXITR = 100 ) */

/*         C */
/*         C     The NAIF IDs of the earth and moon are 399 and 301 */
/*         C     respectively. */
/*         C */
/*               INTEGER               OBSRVR */
/*               PARAMETER           ( OBSRVR = 399 ) */

/*               INTEGER               TARGET */
/*               PARAMETER           ( TARGET = 301 ) */

/*         C */
/*         C     Local variables */
/*         C */
/*               DOUBLE PRECISION      ET */
/*               DOUBLE PRECISION      LT */
/*               DOUBLE PRECISION      POS ( 3 ) */

/*               INTEGER               I */

/*         C */
/*         C     Load the SPK file. */
/*         C */
/*               CALL FURNSH ( SPK ) */

/*         C */
/*         C     Step through a series of epochs, looking up a */
/*         C     position vector at each one. */
/*         C */
/*               DO I = 1, MAXITR */

/*                  ET = ET0 + (I-1)*STEP */

/*                  CALL ZZSPKZP0 ( TARGET, ET, FRAME, ABCORR, OBSRVR, */
/*              .                 POS,    LT                        ) */

/*                  WRITE (*,*) 'ET = ', ET */
/*                  WRITE (*,*) 'J2000 x-position (km):   ', POS(1) */
/*                  WRITE (*,*) 'J2000 y-position (km):   ', POS(2) */
/*                  WRITE (*,*) 'J2000 z-position (km):   ', POS(3) */
/*                  WRITE (*,*) ' ' */

/*               END DO */

/*               END */


/* $ Restrictions */

/*     1) SPICE Private routine. */

/* $ Literature_References */

/*     SPK Required Reading. */

/* $ Author_and_Institution */

/*     C.H. Acton      (JPL) */
/*     B.V. Semenov    (JPL) */
/*     N.J. Bachman    (JPL) */
/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 05-JAN-2005 (NJB) */

/*        Based on SPICELIB Version 3.1.0, 05-JAN-2005 (NJB) */

/* -& */
/* $ Index_Entries */

/*     using body names get position relative to an observer */
/*     get position relative observer corrected for aberrations */
/*     read ephemeris data */
/*     read trajectory data */

/* -& */
/* $ Revisions */

/* -& */


/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZSPKZP0", (ftnlen)8);
    }

/*     Get the frame id for J2000 on the first call to this routine. */

    if (first) {
	first = FALSE_;
	namfrm_("J2000", &fj2000, (ftnlen)5);
    }

/*     Decide whether the aberration correction is for received or */
/*     transmitted radiation. */

    i__ = ltrim_(abcorr, abcorr_len);
    xmit = eqchr_(abcorr + (i__ - 1), "X", (ftnlen)1, (ftnlen)1);

/*     If we only want geometric positions, then compute just that. */

/*     Otherwise, compute the state of the observer relative to */
/*     the SSB.  Then feed that position into ZZSPKPA0 to compute the */
/*     apparent position of the target body relative to the observer */
/*     with the requested aberration corrections. */

    if (eqstr_(abcorr, "NONE", abcorr_len, (ftnlen)4)) {
	zzspkgp0_(targ, et, ref, obs, ptarg, lt, ref_len);
    } else {

/*        Get the auxiliary information about the requested output */
/*        frame. */

	namfrm_(ref, &reqfrm, ref_len);
	if (reqfrm == 0) {
	    setmsg_("The requested output frame '#' is not recognized by the"
		    " reference frame subsystem.  Please check that the appro"
		    "priate kernels have been loaded and that you have correc"
		    "tly entered the name of the output frame. ", (ftnlen)209);
	    errch_("#", ref, (ftnlen)1, ref_len);
	    sigerr_("SPICE(UNKNOWNFRAME)", (ftnlen)19);
	    chkout_("ZZSPKZP0", (ftnlen)8);
	    return 0;
	}
	frinfo_(&reqfrm, &center, &type__, &typeid, &found);

/*        If we are dealing with an inertial frame, we can simply */
/*        call ZZSPKSB0, ZZSPKPA0 and return. */

	if (type__ == 1) {
	    zzspksb0_(obs, et, ref, sobs, ref_len);
	    zzspkpa0_(targ, et, ref, sobs, abcorr, ptarg, lt, ref_len, 
		    abcorr_len);
	    chkout_("ZZSPKZP0", (ftnlen)8);
	    return 0;
	}

/*        Still here? */

/*        We are dealing with a non-inertial frame.  But we need to */
/*        do light time and stellar aberration in an inertial frame. */
/*        Get the "apparent" position of TARG in the intermediary */
/*        inertial reference frame J2000. */

/*        We also need the light time to the center of the frame. */

	zzspksb0_(obs, et, "J2000", sobs, (ftnlen)5);
	zzspkpa0_(targ, et, "J2000", sobs, abcorr, postn, lt, (ftnlen)5, 
		abcorr_len);
	if (failed_()) {
	    chkout_("ZZSPKZP0", (ftnlen)8);
	    return 0;
	}
	if (center == *obs) {
	    ltcent = 0.;
	} else if (center == *targ) {
	    ltcent = *lt;
	} else {
	    zzspkpa0_(&center, et, "J2000", sobs, abcorr, temp, &ltcent, (
		    ftnlen)5, abcorr_len);
	}

/*        If something went wrong (like we couldn't get the position of */
/*        the center relative to the observer) now it is time to quit. */

	if (failed_()) {
	    chkout_("ZZSPKZP0", (ftnlen)8);
	    return 0;
	}

/*        If the aberration corrections are for transmission, negate */
/*        the light time, since we wish to compute the orientation */
/*        of the non-inertial frame at an epoch later than ET by */
/*        the one-way light time. */

	if (xmit) {
	    ltcent = -ltcent;
	}

/*        Get the rotation from J2000 to the requested frame */
/*        and convert the position. */

	d__1 = *et - ltcent;
	zzrefch0_(&fj2000, &reqfrm, &d__1, xform);
	if (failed_()) {
	    chkout_("ZZSPKZP0", (ftnlen)8);
	    return 0;
	}
	mxv_(xform, postn, ptarg);
    }
    chkout_("ZZSPKZP0", (ftnlen)8);
    return 0;
} /* zzspkzp0_ */

