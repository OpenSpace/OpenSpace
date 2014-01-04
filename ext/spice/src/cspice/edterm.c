/* edterm.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;

/* $Procedure EDTERM ( Ellipsoid terminator ) */
/* Subroutine */ int edterm_(char *trmtyp, char *source, char *target, 
	doublereal *et, char *fixfrm, char *abcorr, char *obsrvr, integer *
	npts, doublereal *trgepc, doublereal *obspos, doublereal *trmpts, 
	ftnlen trmtyp_len, ftnlen source_len, ftnlen target_len, ftnlen 
	fixfrm_len, ftnlen abcorr_len, ftnlen obsrvr_len)
{
    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    extern /* Subroutine */ int zzcorepc_(char *, doublereal *, doublereal *, 
	    doublereal *, ftnlen), zzedterm_(char *, doublereal *, doublereal 
	    *, doublereal *, doublereal *, doublereal *, integer *, 
	    doublereal *, ftnlen);
    integer n;
    doublereal r__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    integer trgid;
    logical found;
    doublereal ltsrc;
    extern /* Subroutine */ int bods2c_(char *, integer *, logical *, ftnlen);
    integer frcode, frclas;
    doublereal srcrad[3];
    integer center, clssid;
    doublereal trgrad[3];
    extern /* Subroutine */ int namfrm_(char *, integer *, ftnlen), frinfo_(
	    integer *, integer *, integer *, integer *, logical *), bodvrd_(
	    char *, char *, integer *, integer *, doublereal *, ftnlen, 
	    ftnlen), sigerr_(char *, ftnlen);
    doublereal lttarg;
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen), errint_(char *, integer *, ftnlen);
    doublereal srcpos[3], trgpos[3];
    extern logical return_(void);
    extern /* Subroutine */ int spkpos_(char *, doublereal *, char *, char *, 
	    char *, doublereal *, doublereal *, ftnlen, ftnlen, ftnlen, 
	    ftnlen), vminus_(doublereal *, doublereal *);

/* $ Abstract */

/*     Compute a set of points on the umbral or penumbral terminator of */
/*     a specified target body, where the target shape is modeled as an */
/*     ellipsoid. */

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

/*     ELLIPSES */

/* $ Keywords */

/*     BODY */
/*     GEOMETRY */
/*     MATH */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     TRMTYP     I   Terminator type. */
/*     SOURCE     I   Light source. */
/*     TARGET     I   Target body. */
/*     ET         I   Observation epoch. */
/*     FIXFRM     I   Body-fixed frame associated with target. */
/*     ABCORR     I   Aberration correction. */
/*     OBSRVR     I   Observer. */
/*     NPTS       I   Number of points in terminator set. */
/*     TRGEPC     O   Epoch associated with target center. */
/*     OBSPOS     O   Position of observer in body-fixed frame. */
/*     TRMPTS     O   Terminator point set. */

/* $ Detailed_Input */

/*     TRMTYP      is a string indicating the type of terminator to */
/*                 compute:  umbral or penumbral.  The umbral terminator */
/*                 is the boundary of the portion of the ellipsoid */
/*                 surface in total shadow. The penumbral terminator is */
/*                 the boundary of the portion of the surface that is */
/*                 completely illuminated.  Note that in astronomy */
/*                 references, the unqualified word "terminator" refers */
/*                 to the umbral terminator.  Here, the unqualified */
/*                 word refers to either type of terminator. */

/*                 Possible values of TRMTYP are */

/*                    'UMBRAL' */
/*                    'PENUMBRAL' */

/*                 Case and leading or trailing blanks in TRMTYP are */
/*                 not significant. */


/*     SOURCE      is the name of the body acting as a light source. */
/*                 SOURCE is case-insensitive, and leading and trailing */
/*                 blanks in TARGET are not significant. Optionally, you */
/*                 may supply a string containing the integer ID code */
/*                 for the object.  For example both 'SUN' and '10' are */
/*                 legitimate strings that indicate the Sun is the light */
/*                 source. */

/*                 This routine assumes that a kernel variable */
/*                 representing the light source's radii is present in */
/*                 the kernel pool.  Normally the kernel variable would */
/*                 be defined by loading a PCK file. */

/*                 The shape of the light source is always modeled as a */
/*                 sphere, regardless of whether radii defining a */
/*                 triaxial ellipsoidal shape model are available in the */
/*                 kernel pool.  The maximum radius of the body is used */
/*                 as the radius of the sphere. */


/*     TARGET      is the name of the target body.  TARGET is */
/*                 case-insensitive, and leading and trailing blanks in */
/*                 TARGET are not significant. Optionally, you may */
/*                 supply a string containing the integer ID code for */
/*                 the object.  For example both 'MOON' and '301' are */
/*                 legitimate strings that indicate the moon is the */
/*                 target body. */

/*                 This routine assumes that a kernel variable */
/*                 representing the target's radii is present in the */
/*                 kernel pool.  Normally the kernel variable would be */
/*                 defined by loading a PCK file. */


/*     ET          is the epoch of participation of the observer, */
/*                 expressed as ephemeris seconds past J2000 TDB: ET is */
/*                 the epoch at which the observer's position is */
/*                 computed. */

/*                 When aberration corrections are not used, ET is also */
/*                 the epoch at which the position and orientation of the */
/*                 target body and position of the light source are */
/*                 computed. */

/*                 When aberration corrections are used, ET is the epoch */
/*                 at which the observer's position relative to the solar */
/*                 system barycenter is computed; in this case the */
/*                 position and orientation of the target body are */
/*                 computed at ET-LT or ET+LT, where LT is the one-way */
/*                 light time between the target body's center and the */
/*                 observer, and the sign applied to LT depends on the */
/*                 selected correction. See the description of ABCORR */
/*                 below for details. */


/*     FIXFRM      is the name of the reference frame relative to which */
/*                 the output terminator points are expressed. This must */
/*                 a body-centered, body-fixed frame associated with the */
/*                 target.  The frame's axes must be compatible with the */
/*                 triaxial ellipsoidal shape model associated with the */
/*                 target body (normally provide via a PCK): this */
/*                 routine assumes that the first, second, and third */
/*                 axis lengths correspond, respectively, to the x, y, */
/*                 and z-axes of the frame designated by FIXFRM. */

/*                 FIXFRM may refer to a built-in frame (documented in */
/*                 the Frames Required Reading) or a frame defined by a */
/*                 loaded frame kernel (FK). */

/*                 The orientation of the frame designated by FIXFRM is */
/*                 evaluated at epoch of participation of the target */
/*                 body.  See the descriptions of ET and ABCORR for */
/*                 details. */


/*     ABCORR      indicates the aberration correction to be applied */
/*                 when computing the observer-target position, the */
/*                 orientation of the target body, and the target- */
/*                 source position vector.  ABCORR may be any of */
/*                 the following. */

/*                    'NONE'     Apply no correction.  Compute the */
/*                               terminator points using the position */
/*                               of the light source and target, and */
/*                               the orientation of the target, at ET. */

/*                 Let LT represent the one-way light time between the */
/*                 observer and the target body's center. The following */
/*                 values of ABCORR apply to the "reception" case in */
/*                 which photons depart from the target body's center at */
/*                 the light-time corrected epoch ET-LT and *arrive* at */
/*                 the observer's location at ET: */


/*                    'LT'       Correct for one-way light time (also */
/*                               called "planetary aberration") using a */
/*                               Newtonian formulation. This correction */
/*                               yields the location of the terminator */
/*                               points at the approximate time they */
/*                               emitted photons arriving at the */
/*                               observer at ET (the difference between */
/*                               light time to the target center and */
/*                               light time to the terminator points */
/*                               is ignored). */

/*                               The light time correction uses an */
/*                               iterative solution of the light time */
/*                               equation. The solution invoked by the */
/*                               'LT' option uses one iteration. */

/*                               The target position as seen by the */
/*                               observer, the position of the light */
/*                               source as seen from the target at */
/*                               ET-LT, and the rotation of the target */
/*                               body, are corrected for light time. */

/*                    'LT+S'     Correct for one-way light time and */
/*                               stellar aberration using a Newtonian */
/*                               formulation. This option modifies the */
/*                               positions obtained with the 'LT' option */
/*                               to account for the observer's velocity */
/*                               relative to the solar system */
/*                               barycenter.  This correction also */
/*                               applies to the position of the light */
/*                               source relative to the target.  The */
/*                               result is the apparent terminator as */
/*                               seen by the observer. */

/*                    'CN'       Converged Newtonian light time */
/*                               correction.  In solving the light time */
/*                               equation, the 'CN' correction iterates */
/*                               until the solution converges.  The */
/*                               position and rotation of the target */
/*                               body and the position of the light */
/*                               source relative to the target are */
/*                               corrected for light time. */

/*                    'CN+S'     Converged Newtonian light time */
/*                               and stellar aberration corrections. */


/*     OBSRVR      is the name of the observing body.  This is typically */
/*                 a spacecraft, the Earth, or a surface point on the */
/*                 Earth.  OBSRVR is case-insensitive, and leading and */
/*                 trailing blanks in OBSRVR are not significant. */
/*                 Optionally, you may supply a string containing the */
/*                 integer ID code for the object.  For example both */
/*                 'EARTH' and '399' are legitimate strings that indicate */
/*                 the Earth is the observer. */


/*     NPTS        is the number of terminator points to compute. */


/* $ Detailed_Output */

/*     TRGEPC      is the "target epoch."  TRGEPC is defined as follows: */
/*                 letting LT be the one-way light time between the */
/*                 target center and observer, TRGEPC is either the */
/*                 epoch ET-LT or ET depending on whether the requested */
/*                 aberration correction is, respectively, for received */
/*                 radiation or omitted.  LT is computed using the */
/*                 method indicated by ABCORR. */

/*                 TRGEPC is expressed as seconds past J2000 TDB. */


/*     OBSPOS      is the vector from the center of the target body at */
/*                 epoch TRGEPC to the observer at epoch ET.  OBSPOS is */
/*                 expressed in the target body-fixed reference frame */
/*                 FIXFRM, which is evaluated at TRGEPC. */

/*                 OBSPOS is returned to simplify various related */
/*                 computations that would otherwise be cumbersome.  For */
/*                 example, the vector XVEC from the observer to the */
/*                 Ith terminator point can be calculated via the call */

/*                    CALL VMINUS ( TRMPTS(1,I), OBSPOS, XVEC ) */

/*                 The components of OBSPOS are given in units of km. */


/*     TRMPTS      is an array of points on the umbral or penumbral */
/*                 terminator of the ellipsoid, as specified by the */
/*                 input argument TRMTYP.  The Ith point is contained in */
/*                 the array elements */

/*                     TRMPTS(J,I),  J = 1, 2, 3 */

/*                 Each terminator point is the point of tangency of a */
/*                 plane that is also tangent to the light source. These */
/*                 associated points of tangency on the light source */
/*                 have uniform distribution in longitude when expressed */
/*                 in a cylindrical coordinate system whose Z-axis is */
/*                 OBSPOS.  The magnitude of the separation in longitude */
/*                 between the tangency points on the light source is */

/*                    2*Pi / NPTS */

/*                 If the target is spherical, the terminator points */
/*                 also are uniformly distributed in longitude in the */
/*                 cylindrical system described above.  If the target is */
/*                 non-spherical, the longitude distribution of the */
/*                 points generally is not uniform. */

/*                 The terminator points are expressed in the body-fixed */
/*                 reference frame designated by FIXFRM.  Units are km. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the input frame name FIXFRM cannot be mapped */
/*         to a frame ID code, the error SPICE(NOTRANSLATION) is */
/*         signaled. */

/*     2)  If the target name TARGET cannot be mapped */
/*         to a body ID code, the error SPICE(NOTRANSLATION) is */
/*         signaled. */

/*     3)  If the frame designated by FIXFRM is not centered */
/*         on the target, the error SPICE(INVALIDFIXFRM) is */
/*         signaled. */

/*     4)  If the terminator type is not recognized, the error */
/*         will be diagnosed by a routine in the call tree of */
/*         this routine. */

/*     5)  If the set size NPTS is not at least 1, the error */
/*         will be diagnosed by a routine in the call tree of */
/*         this routine. */

/*     6)  If any of the ellipsoid's semi-axis lengths is non-positive, */
/*         the error will be diagnosed by a routine in the call tree of */
/*         this routine. */

/*     7)  If the light source has non-positive radius, the error */
/*         will be diagnosed by a routine in the call tree of */
/*         this routine. */

/*     8)  If the light source intersects the smallest sphere */
/*         centered at the origin and containing the ellipsoid, the */
/*         error will be diagnosed by a routine in the call tree of */
/*         this routine. */

/*     9)  If radii for the target body or light source are not */
/*         available in the kernel pool, the error will be diagnosed by */
/*         a routine in the call tree of this routine.  If radii are */
/*         available but either body does not have three radii, the */
/*         error SPICE(INVALIDCOUNT) will be signaled. */

/*     10) If any SPK look-up fails, the error will be diagnosed by */
/*         a routine in the call tree of this routine. */

/* $ Files */

/*     Appropriate SPK, PCK, and frame kernels must be loaded by the */
/*     calling program before this routine is called. */

/*     The following data are required: */

/*        - SPK data: ephemeris data for target, observer, and light */
/*          source must be loaded. If aberration corrections are used, */
/*          the states of all three objects relative to the solar system */
/*          barycenter must be calculable from the available ephemeris */
/*          data. Typically ephemeris data are made available by loading */
/*          one or more SPK files via FURNSH. */

/*        - PCK data: triaxial radii for the target body and */
/*          the light source must be loaded into the kernel pool. */
/*          Typically this is done by loading a text PCK file via */
/*          FURNSH. */

/*        - Further PCK data:  rotation data for the target body must */
/*          be loaded.  These may be provided in a text or binary PCK */
/*          file. */

/*        - Frame data:  if a frame definition is required to convert */
/*          the observer and target states to the target body-fixed */
/*          frame designated by FIXFRM, that definition must be */
/*          available in the kernel pool.  Typically the definitions of */
/*          frames not already built-in to SPICE are supplied by loading */
/*          a frame kernel. */

/*     In all cases, kernel data are normally loaded once per program */
/*     run, NOT every time this routine is called. */

/* $ Particulars */

/*     This routine models the boundaries of shadow regions on an */
/*     ellipsoidal target body "illuminated" by a spherical light */
/*     source.  Light rays are assumed to travel along straight lines; */
/*     refraction is not modeled. */

/*     Points on the target body's surface at which the entire cap of */
/*     the light source is visible are considered to be completely */
/*     illuminated. Points on the target's surface at which some portion */
/*     (or all) of the cap of the light source are blocked are */
/*     considered to be in partial (or total) shadow. */

/*     In this routine, we use the term "umbral terminator" to denote */
/*     the curve ususally called the "terminator":  this curve is the */
/*     boundary of the portion of the target body's surface that lies in */
/*     total shadow. We use the term "penumbral terminator" to denote */
/*     the boundary of the completely illuminated portion of the */
/*     surface. */

/*     In general, the terminator on an ellipsoid is a more complicated */
/*     curve than the limb (which is always an ellipse).  Aside from */
/*     various special cases, the terminator does not lie in a plane. */

/*     However, the condition for a point X on the ellipsoid to lie on */
/*     the terminator is simple:  a plane tangent to the ellipsoid at X */
/*     must also be tangent to the light source.  If this tangent plane */
/*     does not intersect the vector from the center of the ellipsoid to */
/*     the center of the light source, then X lies on the umbral */
/*     terminator; otherwise X lies on the penumbral terminator. */

/* $ Examples */

/*     1)  Compute a set of umbral terminator points on the Moon. */
/*         Perform a consistency check using the solar incidence angle */
/*         at each point.  We expect to see a solar incidence angle of */
/*         approximately 90 degrees.  Since the solar incidence angle is */
/*         measured between the local outward normal and the direction */
/*         to the Sun, the solar incidence angle at an umbral terminator */
/*         point should exceed 90 degrees by approximately the angular */
/*         radius of the Sun. */

/*         This program loads SPICE kernels via a meta-kernel.  The ' */
/*         contents of the meta-kernel used to produce the results shown */
/*         below */

/*               \begindata */

/*                  KERNELS_TO_LOAD = ( 'naif0008.tls' */
/*                                      'pck00008.tpc' */
/*                                      'de405s.bsp'  ) */
/*               \begintext */


/*         Program source code: */

/*            PROGRAM EX1 */
/*            IMPLICIT NONE */

/*            DOUBLE PRECISION      DPR */
/*            DOUBLE PRECISION      VDIST */

/*            CHARACTER*(*)         META */
/*            PARAMETER           ( META   = 'ex1_meta.ker' ) */

/*            INTEGER               NPTS */
/*            PARAMETER           ( NPTS   = 3 ) */

/*            INTEGER               CORLEN */
/*            PARAMETER           ( CORLEN = 5 ) */

/*            INTEGER               BDNMLN */
/*            PARAMETER           ( BDNMLN = 36 ) */

/*            INTEGER               FRNMLN */
/*            PARAMETER           ( FRNMLN = 32 ) */

/*            INTEGER               TIMLEN */
/*            PARAMETER           ( TIMLEN = 50 ) */

/*            CHARACTER*(CORLEN)    ABCORR */
/*            CHARACTER*(FRNMLN)    FRAME */
/*            CHARACTER*(BDNMLN)    SOURCE */
/*            CHARACTER*(BDNMLN)    TARGET */
/*            CHARACTER*(BDNMLN)    OBSRVR */
/*            CHARACTER*(TIMLEN)    UTC */

/*            DOUBLE PRECISION      ANGRAD */
/*            DOUBLE PRECISION      EMISSN */
/*            DOUBLE PRECISION      ET */
/*            DOUBLE PRECISION      LAT */
/*            DOUBLE PRECISION      LON */
/*            DOUBLE PRECISION      LT */
/*            DOUBLE PRECISION      OBSPOS ( 3 ) */
/*            DOUBLE PRECISION      PHASE */
/*            DOUBLE PRECISION      RADIUS */
/*            DOUBLE PRECISION      SOLAR */
/*            DOUBLE PRECISION      SRCPOS ( 3 ) */
/*            DOUBLE PRECISION      SRCRAD ( 3 ) */
/*            DOUBLE PRECISION      TRGEPC */
/*            DOUBLE PRECISION      TRMPTS ( 3, NPTS ) */

/*            INTEGER               I */
/*            INTEGER               N */


/*            CALL FURNSH ( META ) */

/*            UTC    = '2007 FEB 3 00:00:00.000' */

/*            CALL STR2ET ( UTC, ET ) */

/*            OBSRVR = 'EARTH' */
/*            TARGET = 'MOON' */
/*            SOURCE = 'SUN' */
/*            FRAME  = 'IAU_MOON' */
/*            ABCORR = 'LT+S' */

/*            CALL EDTERM ( 'UMBRAL', SOURCE, TARGET, */
/*           .              ET,       FRAME,  ABCORR, */
/*           .              OBSRVR,   NPTS,   TRGEPC, */
/*           .              OBSPOS,   TRMPTS         ) */

/*      C */
/*      C     Find the angular radius of the Sun as */
/*      C     seen from the target.  First, look up */
/*      C     the target-sun vector. */
/*      C */
/*            CALL SPKPOS ( SOURCE, TRGEPC, FRAME, */
/*           .              ABCORR, TARGET, SRCPOS, LT ) */

/*      C */
/*      C     Look up the radii of the Sun. */
/*      C */
/*            CALL BODVRD ( SOURCE, 'RADII', 3, N, SRCRAD ) */

/*            DO I = 1, NPTS */

/*               WRITE (*,*) ' ' */

/*               CALL RECLAT ( TRMPTS(1,I), RADIUS, LON, LAT ) */

/*               WRITE (*,*) 'Terminator point ', I, ':' */
/*               WRITE (*,*) '   Radius                     (km):  ', */
/*           .                   RADIUS */
/*               WRITE (*,*) '   Planetocentric longitude   (deg): ', */
/*           .                   LON*DPR() */
/*               WRITE (*,*) '   Planetocentric latitude    (deg): ', */
/*           .                   LAT*DPR() */

/*      C */
/*      C        Find the illumination angles at the */
/*      C        Ith terminator point. */
/*      C */
/*               CALL ILLUM  ( TARGET, ET,         ABCORR, */
/*           .                 OBSRVR, TRMPTS(1,I), PHASE, */
/*           .                 SOLAR,  EMISSN              ) */

/*               WRITE (*,*) */
/*           .               '   Solar incidence angle      (deg): ', */
/*           .                   SOLAR*DPR() */

/*      C */
/*      C        Find the angular radius of the Sun as seen from */
/*      C        the terminator point. */
/*      C */
/*               ANGRAD = ASIN (   SRCRAD(1) */
/*           .                   / VDIST ( SRCPOS,TRMPTS(1,I) )  ) */

/*      C */
/*      C        Display the solar incidence angle after */
/*      C        subtracting the angular radius of the Sun */
/*      C        as seen from the terminator point.  The */
/*      C        result should be approximately 90 degrees. */
/*      C */
/*               WRITE (*, '(1X,A,2PE22.14)') */
/*           .               '   Minus Sun''s ' // */
/*           .               'angular radius (deg): ', */
/*           .               (SOLAR-ANGRAD) * DPR() */

/*            END DO */

/*            END */


/*         When executed, this program produces the output shown */
/*         below.  Note that the results may vary slightly from one */
/*         computing platform to another.  Results are dependent on */
/*         the kernels used as well as the hardware and system software */
/*         running on the host system. */


/*            Terminator point  1: */
/*               Radius                     (km):    1737.4 */
/*               Planetocentric longitude   (deg):  -95.0845526 */
/*               Planetocentric latitude    (deg):   0.00405276211 */
/*               Solar incidence angle      (deg):   90.2697657 */
/*               Minus Sun's angular radius (deg):   90.0000000000000E+00 */

/*            Terminator point  2: */
/*               Radius                     (km):    1737.4 */
/*               Planetocentric longitude   (deg):   84.2280921 */
/*               Planetocentric latitude    (deg):   59.9957555 */
/*               Solar incidence angle      (deg):   90.2697657 */
/*               Minus Sun's angular radius (deg):   90.0000000000000E+00 */

/*            Terminator point  3: */
/*               Radius                     (km):    1737.4 */
/*               Planetocentric longitude   (deg):   87.2164179 */
/*               Planetocentric latitude    (deg):  -59.9795505 */
/*               Solar incidence angle      (deg):   90.2697657 */
/*               Minus Sun's angular radius (deg):   90.0000000000000E+00 */


/* $ Restrictions */

/*     1) This routine models light paths as straight lines. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 03-FEB-2007 (NJB) */

/* -& */
/* $ Index_Entries */

/*     find terminator on ellipsoid */
/*     find umbral terminator on ellipsoid */
/*     find penumbral terminator on ellipsoid */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Standard SPICELIB error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("EDTERM", (ftnlen)6);

/*     Get the input frame code and frame info. */

    namfrm_(fixfrm, &frcode, fixfrm_len);
    if (frcode == 0) {
	setmsg_("Input frame # has no associated frame ID code.", (ftnlen)46);
	errch_("#", fixfrm, (ftnlen)1, fixfrm_len);
	sigerr_("SPICE(NOTRANSLATION)", (ftnlen)20);
	chkout_("EDTERM", (ftnlen)6);
	return 0;
    }
    frinfo_(&frcode, &center, &frclas, &clssid, &found);
    if (! found) {
	setmsg_("Input frame # has associated frame ID code #, but no info w"
		"as found by FRINFO for this frame.", (ftnlen)93);
	errch_("#", fixfrm, (ftnlen)1, fixfrm_len);
	errint_("#", &frcode, (ftnlen)1);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("EDTERM", (ftnlen)6);
	return 0;
    }

/*     Get the ID code of the target. */

    bods2c_(target, &trgid, &found, target_len);
    if (! found) {
	setmsg_("Input target # has no associated body ID code.", (ftnlen)46);
	errch_("#", target, (ftnlen)1, target_len);
	sigerr_("SPICE(NOTRANSLATION)", (ftnlen)20);
	chkout_("EDTERM", (ftnlen)6);
	return 0;
    }

/*     If the frame is not centered on the target, reject it. */

    if (center != trgid) {
	setmsg_("Input frame # is not centered on target body #. This frame "
		"must be a body-fixed frame associated with the target.", (
		ftnlen)113);
	errch_("#", fixfrm, (ftnlen)1, fixfrm_len);
	errch_("#", target, (ftnlen)1, target_len);
	sigerr_("SPICE(INVALIDFIXFRM)", (ftnlen)20);
	chkout_("EDTERM", (ftnlen)6);
	return 0;
    }

/*     Look up the radii associated with the target body. */

    bodvrd_(target, "RADII", &c__3, &n, trgrad, target_len, (ftnlen)5);
    if (n != 3) {
	setmsg_("Three radii are required for the target body's (#) shape mo"
		"del, but # were found.", (ftnlen)81);
	errch_("#", target, (ftnlen)1, target_len);
	errint_("#", &n, (ftnlen)1);
	sigerr_("SPICE(INVALIDCOUNT)", (ftnlen)19);
	chkout_("EDTERM", (ftnlen)6);
	return 0;
    }

/*     Look up the radii associated with the light source. */

    bodvrd_(source, "RADII", &c__3, &n, srcrad, source_len, (ftnlen)5);
    if (n != 3) {
	setmsg_("Three radii are required for the light source's (#) shape m"
		"odel, but # were found.", (ftnlen)82);
	errch_("#", source, (ftnlen)1, source_len);
	errint_("#", &n, (ftnlen)1);
	sigerr_("SPICE(INVALIDCOUNT)", (ftnlen)19);
	chkout_("EDTERM", (ftnlen)6);
	return 0;
    }
/* Computing MAX */
    d__1 = max(srcrad[0],srcrad[1]);
    r__ = max(d__1,srcrad[2]);

/*     Look up the observer-target vector and the target-source vector. */
/*     Also set the output OBSPOS. */

    spkpos_(target, et, fixfrm, abcorr, obsrvr, trgpos, &lttarg, target_len, 
	    fixfrm_len, abcorr_len, obsrvr_len);
    zzcorepc_(abcorr, et, &lttarg, trgepc, abcorr_len);
    vminus_(trgpos, obspos);
    spkpos_(source, trgepc, fixfrm, abcorr, target, srcpos, &ltsrc, 
	    source_len, fixfrm_len, abcorr_len, target_len);

/*     We're ready to compute the terminator. */

    zzedterm_(trmtyp, trgrad, &trgrad[1], &trgrad[2], &r__, srcpos, npts, 
	    trmpts, trmtyp_len);
    chkout_("EDTERM", (ftnlen)6);
    return 0;
} /* edterm_ */

