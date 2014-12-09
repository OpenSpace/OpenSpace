/* gfudb.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c_n1 = -1;
static integer c__3 = 3;
static integer c__0 = 0;
static logical c_false = FALSE_;

/* $Procedure GFUDB ( GF, user defined boolean ) */
/* Subroutine */ int gfudb_(U_fp udfuns, U_fp udfunb, doublereal *step, 
	doublereal *cnfine, doublereal *result)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer sized_(doublereal *);
    extern logical gfbail_();
    logical ok;
    extern /* Subroutine */ int scardd_(integer *, doublereal *);
    extern /* Subroutine */ int gfrefn_(), gfrepf_(), gfrepi_(), gfrepu_(), 
	    gfstep_();
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), gfsstp_(doublereal *);
    extern logical odd_(integer *);
    doublereal tol;
    extern /* Subroutine */ int zzgfudb_(U_fp, U_fp, doublereal *, U_fp, U_fp,
	     logical *, U_fp, U_fp, U_fp, logical *, L_fp, doublereal *, 
	    doublereal *), zzholdd_(integer *, integer *, logical *, 
	    doublereal *);

/* $ Abstract */

/*     Perform a GF search on a user defined boolean quantity. */

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

/*     GF */
/*     TIME */
/*     WINDOWS */

/* $ Keywords */

/*     EVENT */
/*     EPHEMERIS */
/*     SEARCH */
/*     WINDOW */

/* $ Declarations */
/* $ Abstract */

/*     This file contains public, global parameter declarations */
/*     for the SPICELIB Geometry Finder (GF) subsystem. */

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

/*     GF */

/* $ Keywords */

/*     GEOMETRY */
/*     ROOT */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman      (JPL) */
/*     L.E. Elson        (JPL) */
/*     E.D. Wright       (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.3.0, 01-OCT-2011 (NJB) */

/*       Added NWILUM parameter. */

/* -    SPICELIB Version 1.2.0, 14-SEP-2010 (EDW) */

/*       Added NWPA parameter. */

/* -    SPICELIB Version 1.1.0, 08-SEP-2009 (EDW) */

/*       Added NWRR parameter. */
/*       Added NWUDS parameter. */

/* -    SPICELIB Version 1.0.0, 21-FEB-2009 (NJB) (LSE) (EDW) */

/* -& */

/*     Root finding parameters: */

/*     CNVTOL is the default convergence tolerance used by the */
/*     high-level GF search API routines. This tolerance is */
/*     used to terminate searches for binary state transitions: */
/*     when the time at which a transition occurs is bracketed */
/*     by two times that differ by no more than CNVTOL, the */
/*     transition time is considered to have been found. */

/*     Units are TDB seconds. */


/*     NWMAX is the maximum number of windows allowed for user-defined */
/*     workspace array. */

/*        DOUBLE PRECISION      WORK   ( LBCELL : MW, NWMAX ) */

/*     Currently no more than twelve windows are required; the three */
/*     extra windows are spares. */

/*     Callers of GFEVNT can include this file and use the parameter */
/*     NWMAX to declare the second dimension of the workspace array */
/*     if necessary. */


/*     Callers of GFIDST should declare their workspace window */
/*     count using NWDIST. */


/*     Callers of GFSEP should declare their workspace window */
/*     count using NWSEP. */


/*     Callers of GFRR should declare their workspace window */
/*     count using NWRR. */


/*     Callers of GFUDS should declare their workspace window */
/*     count using NWUDS. */


/*     Callers of GFPA should declare their workspace window */
/*     count using NWPA. */


/*     Callers of GFILUM should declare their workspace window */
/*     count using NWILUM. */


/*     ADDWIN is a parameter used to expand each interval of the search */
/*     (confinement) window by a small amount at both ends in order to */
/*     accommodate searches using equality constraints. The loaded */
/*     kernel files must accommodate these expanded time intervals. */


/*     FRMNLN is a string length for frame names. */


/*     NVRMAX is the maximum number of vertices if FOV type is "POLYGON" */


/*     FOVTLN -- maximum length for FOV string. */


/*     Specify the character strings that are allowed in the */
/*     specification of field of view shapes. */


/*     Character strings that are allowed in the */
/*     specification of occultation types: */


/*     Occultation target shape specifications: */


/*     Specify the number of supported occultation types and occultation */
/*     type string length: */


/*     Instrument field-of-view (FOV) parameters */

/*     Maximum number of FOV boundary vectors: */


/*     FOV shape parameters: */

/*        circle */
/*        ellipse */
/*        polygon */
/*        rectangle */


/*     End of file gf.inc. */

/* $ Abstract */

/*     SPICE private include file intended solely for the support of */
/*     SPICE routines. Users should not include this routine in their */
/*     source code due to the volatile nature of this file. */

/*     This file contains private, global parameter declarations */
/*     for the SPICELIB Geometry Finder (GF) subsystem. */

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

/*     GF */

/* $ Keywords */

/*     GEOMETRY */
/*     ROOT */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman      (JPL) */
/*     E.D. Wright       (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 17-FEB-2009 (NJB) (EDW) */

/* -& */

/*     The set of supported coordinate systems */

/*        System          Coordinates */
/*        ----------      ----------- */
/*        Rectangular     X, Y, Z */
/*        Latitudinal     Radius, Longitude, Latitude */
/*        Spherical       Radius, Colatitude, Longitude */
/*        RA/Dec          Range, Right Ascension, Declination */
/*        Cylindrical     Radius, Longitude, Z */
/*        Geodetic        Longitude, Latitude, Altitude */
/*        Planetographic  Longitude, Latitude, Altitude */

/*     Below we declare parameters for naming coordinate systems. */
/*     User inputs naming coordinate systems must match these */
/*     when compared using EQSTR. That is, user inputs must */
/*     match after being left justified, converted to upper case, */
/*     and having all embedded blanks removed. */


/*     Below we declare names for coordinates. Again, user */
/*     inputs naming coordinates must match these when */
/*     compared using EQSTR. */


/*     Note that the RA parameter value below matches */

/*        'RIGHT ASCENSION' */

/*     when extra blanks are compressed out of the above value. */


/*     Parameters specifying types of vector definitions */
/*     used for GF coordinate searches: */

/*     All string parameter values are left justified, upper */
/*     case, with extra blanks compressed out. */

/*     POSDEF indicates the vector is defined by the */
/*     position of a target relative to an observer. */


/*     SOBDEF indicates the vector points from the center */
/*     of a target body to the sub-observer point on */
/*     that body, for a given observer and target. */


/*     SOBDEF indicates the vector points from the center */
/*     of a target body to the surface intercept point on */
/*     that body, for a given observer, ray, and target. */


/*     Number of workspace windows used by ZZGFREL: */


/*     Number of additional workspace windows used by ZZGFLONG: */


/*     Index of "existence window" used by ZZGFCSLV: */


/*     Progress report parameters: */

/*     MXBEGM, */
/*     MXENDM    are, respectively, the maximum lengths of the progress */
/*               report message prefix and suffix. */

/*     Note: the sum of these lengths, plus the length of the */
/*     "percent complete" substring, should not be long enough */
/*     to cause wrap-around on any platform's terminal window. */


/*     Total progress report message length upper bound: */


/*     End of file zzgf.inc. */

/* $ Abstract */

/*     SPICE private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     This file contains parameter declarations for the ZZHOLDD */
/*     routine. */

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

/*     None. */

/* $ Declarations */

/*     None. */

/* $ Brief_I/O */

/*     None. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     GEN       general value, primarily for testing. */

/*     GF_REF    user defined GF reference value. */

/*     GF_TOL    user defined GF convergence tolerance. */

/*     GF_DT     user defined GF step for numeric differentiation. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     E.D. Wright    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0  03-DEC-2013 (EDW) */

/* -& */

/*     OP codes. The values exist in the integer domain */
/*     [ -ZZNOP, -1], */


/*     Current number of OP codes. */


/*     ID codes. The values exist in the integer domain */
/*     [ 1, NID], */


/*     General use, primarily testing. */


/*     The user defined GF reference value. */


/*     The user defined GF convergence tolerance. */


/*     The user defined GF step for numeric differentiation. */


/*     Current number of ID codes, dimension of array */
/*     in ZZHOLDD. Bad things can happen if this parameter */
/*     does not have the proper value. */


/*     End of file zzholdd.inc. */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     LBCELL     P   SPICE Cell lower bound. */
/*     CNVTOL     P   Convergence tolerance. */
/*     UDFUNS     I   Name of the routine that computes a scalar */
/*                    quantity corresponding to an ET. */
/*     UDFUNB     I   Name of the routine returning the boolean value */
/*                    corresponding to an ET. */
/*     STEP       I   Constant step size in seconds for finding geometric */
/*                    events. */
/*     CNFINE     I   SPICE window to which the search is restricted. */
/*     RESULT    I-O  SPICE window containing results. */

/* $ Detailed_Input */

/*     UDFUNS     the routine that returns the value of the scalar */
/*                quantity of interest at time ET. The calling sequence */
/*                for UDFUNC is: */

/*                   CALL UDFUNS ( ET, VALUE ) */

/*                where: */

/*                   ET      a double precision value representing */
/*                           ephemeris time, expressed as seconds past */
/*                           J2000 TDB at which to evaluate UDFUNS. */

/*                   VALUE   is the value of the scalar quantity */
/*                           at ET. */

/*     UDFUNB     the user defined routine returning a boolean value */
/*                for an epoch ET. The calling sequence for UNFUNB is: */

/*                   CALL UDFUNB ( UDFUNS, ET, BOOL ) */

/*                where: */

/*                   UDFUNS   the name of the scalar function as */
/*                            defined above. */

/*                   ET       a double precision value representing */
/*                            ephemeris time, expressed as seconds past */
/*                            J2000 TDB, at which to evaluate UDFUNB. */

/*                   BOOL     the boolean value at ET. */

/*                GFUDB will correctly operate only for boolean */
/*                functions with true conditions defining non zero */
/*                measure time intervals. */

/*                Note, UDFUNB need not call UDFUNS. The use of UDFUNS */
/*                is determined by the needs of the calculation and */
/*                the user's design. */

/*     STEP       the step size to be used in the search. STEP must */
/*                be shorter than any interval, within the confinement */
/*                window, over which the user defined boolean function */
/*                is met. In other words, STEP must be shorter than the */
/*                shortest time interval for which the boolean function */
/*                is true; STEP must also be shorter than the shortest */
/*                time interval between two boolean function true events */
/*                occurring within the confinement window (see below). */
/*                However, STEP must not be *too* short, or the search */
/*                will take an unreasonable amount of time. */

/*                The choice of STEP affects the completeness but not */
/*                the precision of solutions found by this routine; the */
/*                precision is controlled by the convergence tolerance. */
/*                See the discussion of the parameter CNVTOL for */
/*                details. */

/*                STEP has units of TDB seconds. */

/*     CNFINE     is a SPICE window that confines the time period over */
/*                which the specified search is conducted. CNFINE may */
/*                consist of a single interval or a collection of */
/*                intervals. */

/*                In some cases the confinement window can be used to */
/*                greatly reduce the time period that must be searched */
/*                for the desired solution. See the Particulars section */
/*                below for further discussion. */

/*                See the Examples section below for a code example */
/*                that shows how to create a confinement window. */

/*                CNFINE must be initialized by the caller via the */
/*                SPICELIB routine SSIZED. */

/*     RESULT     a double precision SPICE window which will contain the */
/*                search results. RESULT must be declared and initialized */
/*                with sufficient size to capture the full set of time */
/*                intervals within the search region on which the */
/*                specified constraint is satisfied. */

/*                RESULT must be initialized by the caller via the */
/*                SPICELIB routine SSIZED. */

/*                If RESULT is non-empty on input, its contents */
/*                will be discarded before GFUDB conducts its search. */

/* $ Detailed_Output */

/*     RESULT     is a SPICE window containing the time intervals within */
/*                the confinement window, during which the specified */
/*                boolean quantity is true. */

/*                If no times within the confinement window satisfy the */
/*                search, RESULT will be returned with a cardinality */
/*                of zero. */

/* $ Parameters */

/*     LBCELL     the integer value defining the lower bound for */
/*                SPICE Cell arrays (a SPICE window is a kind of cell). */

/*     CNVTOL     is the convergence tolerance used for finding */
/*                endpoints of the intervals comprising the result */
/*                window. CNVTOL is used to determine when binary */
/*                searches for roots should terminate: when a root is */
/*                bracketed within an interval of length CNVTOL, the */
/*                root is considered to have been found. */

/*                The accuracy, as opposed to precision, of roots found */
/*                by this routine depends on the accuracy of the input */
/*                data. In most cases, the accuracy of solutions will be */
/*                inferior to their precision. */

/*     See INCLUDE file gf.inc for declarations and descriptions of */
/*     parameters used throughout the GF system. */

/* $ Exceptions */

/*     1)  In order for this routine to produce correct results, */
/*         the step size must be appropriate for the problem at hand. */
/*         Step sizes that are too large may cause this routine to miss */
/*         roots; step sizes that are too small may cause this routine */
/*         to run unacceptably slowly and in some cases, find spurious */
/*         roots. */

/*         This routine does not diagnose invalid step sizes, except */
/*         that if the step size is non-positive, the error */
/*         SPICE(INVALIDSTEP) is signaled. */

/*     2)  Due to numerical errors, in particular, */

/*            - truncation error in time values */
/*            - finite tolerance value */
/*            - errors in computed geometric quantities */

/*         it is *normal* for the condition of interest to not always be */
/*         satisfied near the endpoints of the intervals comprising the */
/*         RESULT window. One technique to handle such a situation, */
/*         slightly contract RESULT using the window routine WNCOND. */

/*     3)  If the size of the SPICE window RESULT is less than 2 or */
/*         not an even value, the error SPICE(INVALIDDIMENSION) will */
/*         signal. If RESULT has insufficient capacity to contain the */
/*         number of intervals on which the specified condition */
/*         is met, the error will be diagnosed by a routine in the call */
/*         tree of this routine. */

/*     4)  If required ephemerides or other kernel data are not */
/*         available, an error is signaled by a routine in the call tree */
/*         of this routine. */

/* $ Files */

/*     Appropriate kernels must be loaded by the calling program before */
/*     this routine is called. */

/*     If the boolean function requires access to ephemeris data: */

/*        - SPK data: ephemeris data for any body over the */
/*          time period defined by the confinement window must be */
/*          loaded. If aberration corrections are used, the states of */
/*          target and observer relative to the solar system barycenter */
/*          must be calculable from the available ephemeris data. */
/*          Typically ephemeris data are made available by loading one */
/*          or more SPK files via FURNSH. */

/*        - If non-inertial reference frames are used, then PCK */
/*          files, frame kernels, C-kernels, and SCLK kernels may be */
/*          needed. */

/*     In all cases, kernel data are normally loaded once per program */
/*     run, NOT every time this routine is called. */

/* $ Particulars */

/*     This routine determines a set of one or more time intervals */
/*     within the confinement window when the boolean function */
/*     evaluates to true. The resulting set of intervals is returned */
/*     as a SPICE window. */

/*     Below we discuss in greater detail aspects of this routine's */
/*     solution process that are relevant to correct and efficient */
/*     use of this routine in user applications. */

/*     UDFUNS Default Template */
/*     ======================= */

/*     The boolean function includes an argument for an input scalar */
/*     function. Use of a scalar function during the evaluation of */
/*     the boolean function is not required. SPICE provides a no-op */
/*     scalar routine, UDF, as a dummy argument for instances when */
/*     the boolean function does not need to call the scalar function. */

/*     The Search Process */
/*     ================== */

/*     The search for boolean events is treated as a search for state */
/*     transitions: times are sought when the boolean function value */
/*     changes from true to false or vice versa. */

/*     Step Size */
/*     ========= */

/*     Each interval of the confinement window is searched as follows: */
/*     first, the input step size is used to determine the time */
/*     separation at which the boolean function will be sampled. */
/*     Starting at the left endpoint of the interval, samples of the */
/*     boolean function will be taken at each step. If a state change */
/*     is detected, a root has been bracketed; at that point, the */
/*     "root"--the time at which the state change occurs---is found by a */
/*     refinement process, for example, via binary search. */

/*     Note that the optimal choice of step size depends on the lengths */
/*     of the intervals over which the boolean function is constant: */
/*     the step size should be shorter than the shortest such interval */
/*     and the shortest separation between the intervals, within */
/*     the confinement window. */

/*     Having some knowledge of the relative geometry of the targets and */
/*     observer can be a valuable aid in picking a reasonable step size. */
/*     In general, the user can compensate for lack of such knowledge by */
/*     picking a very short step size; the cost is increased computation */
/*     time. */

/*     Note that the step size is not related to the precision with which */
/*     the endpoints of the intervals of the result window are computed. */
/*     That precision level is controlled by the convergence tolerance. */


/*     Convergence Tolerance */
/*     ===================== */

/*     Once a root has been bracketed, a refinement process is used to */
/*     narrow down the time interval within which the root must lie. */
/*     This refinement process terminates when the location of the root */
/*     has been determined to within an error margin called the */
/*     "convergence tolerance." The default convergence tolerance */
/*     used by this routine is set by the parameter CNVTOL (defined */
/*     in gf.inc). */

/*     The value of CNVTOL is set to a "tight" value so that the */
/*     tolerance doesn't become the limiting factor in the accuracy of */
/*     solutions found by this routine. In general the accuracy of input */
/*     data will be the limiting factor. */

/*     The user may change the convergence tolerance from the default */
/*     CNVTOL value by calling the routine GFSTOL, e.g. */

/*        CALL GFSTOL( tolerance value ) */

/*     Call GFSTOL prior to calling this routine. All subsequent */
/*     searches will use the updated tolerance value. */

/*     Setting the tolerance tighter than CNVTOL is unlikely to be */
/*     useful, since the results are unlikely to be more accurate. */
/*     Making the tolerance looser will speed up searches somewhat, */
/*     since a few convergence steps will be omitted. However, in most */
/*     cases, the step size is likely to have a much greater effect */
/*     on processing time than would the convergence tolerance. */


/*     The Confinement Window */
/*     ====================== */

/*     The simplest use of the confinement window is to specify a time */
/*     interval within which a solution is sought. */

/*     The confinement window also can be used to restrict a search to */
/*     a time window over which required data are known to be */
/*     available. */

/*     In some cases, the confinement window can be used to make */
/*     searches more efficient. Sometimes it's possible to do an */
/*     efficient search to reduce the size of the time period over */
/*     which a relatively slow search of interest must be performed. */
/*     See the "CASCADE" example program in gf.req for a demonstration. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     Use the meta-kernel shown below to load the required SPICE */
/*     kernels. */

/*           KPL/MK */

/*           File name: standard.tm */

/*           This meta-kernel is intended to support operation of SPICE */
/*           example programs. The kernels shown here should not be */
/*           assumed to contain adequate or correct versions of data */
/*           required by SPICE-based user applications. */

/*           In order for an application to use this meta-kernel, the */
/*           kernels referenced here must be present in the user's */
/*           current working directory. */


/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'de418.bsp', */
/*                                  'pck00009.tpc', */
/*                                  'naif0009.tls'  ) */

/*           \begintext */

/*     Example(1): */

/*     Calculate the time intervals when the position of the moon */
/*     relative to the earth in the IAU_EARTH frame has a positive value */
/*     for the Z position component, also with a positive value for the */
/*     Vz velocity component. */


/*        Code: */

/*           PROGRAM GFUDB_T */

/*           EXTERNAL                  UDF */
/*           EXTERNAL                  GFB */

/*     C */
/*     C     Local parameters */
/*     C */
/*           INTEGER               LBCELL */
/*           PARAMETER           ( LBCELL = -5 ) */

/*     C */
/*     C     Use the parameter MAXWIN for both the result window size and */
/*     C     the workspace size. */
/*     C */
/*           INTEGER               MAXWIN */
/*           PARAMETER           ( MAXWIN = 100 ) */

/*           DOUBLE PRECISION      LEFT */
/*           DOUBLE PRECISION      RIGHT */
/*           DOUBLE PRECISION      ET */
/*           DOUBLE PRECISION      ETS */
/*           DOUBLE PRECISION      ETE */
/*           DOUBLE PRECISION      STEP */
/*           DOUBLE PRECISION      STATE (6) */
/*           DOUBLE PRECISION      CNFINE ( LBCELL : 2 ) */
/*           DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN ) */

/*           INTEGER               I */

/*           CHARACTER*(32)        UTC */

/*     C */
/*     C     SPICELIB functions. */
/*     C */
/*           INTEGER               WNCARD */
/*           DOUBLE PRECISION      SPD */

/*     C */
/*     C     Initialize windows. */
/*     C */
/*           CALL SSIZED ( MAXWIN, RESULT ) */
/*           CALL SSIZED ( 2,      CNFINE ) */


/*     C */
/*     C     Load needed kernels. */
/*     C */
/*           CALL FURNSH ( 'standard.tm' ) */

/*     C */
/*     C     Store the time bounds of our search interval in */
/*     C     the confinement window. One year, 2011. */
/*     C */
/*           CALL STR2ET ( 'Jan 1 2011', ETS ) */
/*           CALL STR2ET ( 'Jan 1 2012', ETE ) */
/*           CALL WNINSD ( ETS, ETE, CNFINE ) */

/*     C */
/*     C     The moon orbit about the earth-moon barycenter is */
/*     C     twenty-eight days. The event condition occurs */
/*     C     during (very) approximately a quarter of the orbit. Use */
/*     C     a step of five days. */
/*     C */
/*           STEP = 5.D0 * SPD() */

/*           CALL GFUDB ( UDF, GFB, STEP, CNFINE, RESULT ) */

/*           IF ( WNCARD(RESULT) .EQ. 0 ) THEN */

/*                 WRITE (*, '(A)') 'Result window is empty.' */

/*           ELSE */

/*              DO I = 1, WNCARD(RESULT) */

/*     C */
/*     C           Fetch and display each RESULT interval. */
/*     C */
/*                 CALL WNFETD ( RESULT, I, LEFT, RIGHT ) */
/*                 WRITE (*,*) 'Interval ', I */

/*                 CALL ET2UTC ( LEFT, 'C', 4, UTC ) */
/*                 WRITE (*, *) '   Interval start: ', UTC */

/*                 CALL SPKEZ ( 301, LEFT, 'IAU_EARTH', 'NONE', 399, */
/*          .                   STATE, LT ) */
/*                 WRITE (*, *) '                Z= ', STATE(3) */
/*                 WRITE (*, *) '               Vz= ', STATE(6) */

/*                 CALL ET2UTC ( RIGHT, 'C', 4, UTC ) */
/*                 WRITE (*, *) '   Interval end  : ', UTC */

/*                 CALL SPKEZ ( 301, RIGHT, 'IAU_EARTH', 'NONE', 399, */
/*          .                   STATE, LT ) */
/*                 WRITE (*, *) '                Z= ', STATE(3) */
/*                 WRITE (*, *) '               Vz= ', STATE(6) */
/*                 WRITE (*, *) ' ' */

/*              END DO */

/*           END IF */

/*           END */



/*     C-Procedure GFB */
/*     C */
/*     C     User defined boolean routine. */
/*     C */

/*           SUBROUTINE GFB ( UDFUNS, ET, BOOL ) */
/*           IMPLICIT NONE */

/*     C- Abstract */
/*     C */
/*     C     User defined geometric boolean function: */
/*     C */
/*     C        Z >= 0 with dZ/dt > 0. */
/*     C */

/*           EXTERNAL              UDFUNS */

/*           DOUBLE PRECISION      ET */
/*           LOGICAL               BOOL */

/*     C */
/*     C     Local variables. */
/*     C */
/*           INTEGER               TARG */
/*           INTEGER               OBS */

/*           CHARACTER*(12)        REF */
/*           CHARACTER*(12)        ABCORR */

/*           DOUBLE PRECISION      STATE ( 6 ) */
/*           DOUBLE PRECISION      LT */

/*     C */
/*     C     Initialization. Retrieve the vector from the earth to */
/*     C     the moon in the IAU_EARTH frame, without aberration */
/*     C     correction. */
/*     C */
/*           TARG   = 301 */
/*           REF    = 'IAU_EARTH' */
/*           ABCORR = 'NONE' */
/*           OBS    = 399 */

/*     C */
/*     C     Evaluate the state of TARG from OBS at ET with */
/*     C     correction ABCORR. */
/*     C */
/*           CALL SPKEZ ( TARG, ET, REF, ABCORR, OBS, STATE, LT ) */

/*     C */
/*     C     Calculate the boolean value. */
/*     C */
/*           BOOL = (STATE(3) .GE. 0.D0) .AND. (STATE(6) .GT. 0.D0 ) */

/*           RETURN */

/*           END */

/*     The program outputs: */

/*      Interval            1 */
/*         Interval start: 2011 JAN 09 15:24:23.4155 */
/*                      Z=  -3.67969050785177387E-008 */
/*                     Vz=   0.39698408492943960 */
/*         Interval end  : 2011 JAN 16 16:08:28.5634 */
/*                      Z=    156247.48820202681 */
/*                     Vz=   3.76859567857712463E-013 */

/*      Interval            2 */
/*         Interval start: 2011 FEB 05 23:17:57.3590 */
/*                      Z=  -3.98442807636456564E-008 */
/*                     Vz=   0.39678128322307005 */
/*         Interval end  : 2011 FEB 13 01:38:28.4256 */
/*                      Z=    157016.05516171581 */
/*                     Vz=   3.22388166509868235E-013 */

/*      Interval            3 */
/*         Interval start: 2011 MAR 05 06:08:17.6680 */
/*                      Z=  -1.16190221888246015E-008 */
/*                     Vz=   0.39399025399881443 */
/*         Interval end  : 2011 MAR 12 10:27:45.1887 */
/*                      Z=    157503.77393430873 */
/*                     Vz=  -3.41879302645509142E-013 */

/*                        ... */

/*      Interval           12 */
/*         Interval start: 2011 NOV 05 18:43:39.7428 */
/*                      Z=  -1.80199890564836096E-008 */
/*                     Vz=   0.37393762954280635 */
/*         Interval end  : 2011 NOV 13 03:50:17.1540 */
/*                      Z=    153172.08661820635 */
/*                     Vz=  -3.62962481251227764E-013 */

/*      Interval           13 */
/*         Interval start: 2011 DEC 03 01:16:40.8174 */
/*                      Z=   1.30391470065660542E-007 */
/*                     Vz=   0.37425784503188919 */
/*         Interval end  : 2011 DEC 10 09:51:07.7182 */
/*                      Z=    152511.72037686800 */
/*                     Vz=   2.11386680729064302E-013 */

/*      Interval           14 */
/*         Interval start: 2011 DEC 30 09:48:57.4099 */
/*                      Z=   9.79434844339266419E-009 */
/*                     Vz=   0.37733320145276139 */
/*         Interval end  : 2012 JAN 01 00:00:00.0000 */
/*                      Z=    50793.083312689421 */
/*                     Vz=   0.35454996926793847 */


/*     Example(2): */

/*     Calculate the time intervals when the Z component of the earth */
/*     to moon position vector in the IAU_EARTH frame has value */
/*     between -1000 km and 1000 km (e.g. above and below the equatorial */
/*     plane). */


/*        Code: */

/*           PROGRAM GFUDB_T2 */

/*           EXTERNAL                  GFB */
/*           EXTERNAL                  GFQ */

/*     C */
/*     C     Local parameters */
/*     C */
/*           INTEGER               LBCELL */
/*           PARAMETER           ( LBCELL = -5 ) */

/*     C */
/*     C     Use the parameter MAXWIN for both the result window size */
/*     C     and the workspace size. */
/*     C */
/*           INTEGER               MAXWIN */
/*           PARAMETER           ( MAXWIN = 100 ) */

/*           DOUBLE PRECISION      LEFT */
/*           DOUBLE PRECISION      RIGHT */
/*           DOUBLE PRECISION      ET */
/*           DOUBLE PRECISION      ETS */
/*           DOUBLE PRECISION      ETE */
/*           DOUBLE PRECISION      STEP */
/*           DOUBLE PRECISION      POS (3) */
/*           DOUBLE PRECISION      CNFINE ( LBCELL : 2 ) */
/*           DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN ) */

/*           INTEGER               I */

/*           CHARACTER*(32)        UTC */

/*     C */
/*     C     SPICELIB functions. */
/*     C */
/*           INTEGER               WNCARD */
/*           DOUBLE PRECISION      SPD */

/*     C */
/*     C     Initialize windows. */
/*     C */
/*           CALL SSIZED ( MAXWIN, RESULT ) */
/*           CALL SSIZED ( 2,      CNFINE ) */


/*     C */
/*     C     Load needed kernels. */
/*     C */
/*           CALL FURNSH ( 'standard.tm' ) */

/*     C */
/*     C     Store the time bounds of our search interval in */
/*     C     the confinement window. One year, 2011. */
/*     C */
/*           CALL STR2ET ( 'Jan 1 2011', ETS ) */
/*           CALL STR2ET ( 'Jan 1 2012', ETE ) */
/*           CALL WNINSD ( ETS, ETE, CNFINE ) */

/*     C */
/*     C     The duration of the event is approximately ninety minutes. */
/*     C     Use a step of one hour. */
/*     C */
/*           STEP = 60.D0*60.D0 */

/*           CALL GFUDB ( GFQ, GFB, STEP, CNFINE, RESULT ) */

/*           IF ( WNCARD(RESULT) .EQ. 0 ) THEN */

/*                 WRITE (*, '(A)') 'Result window is empty.' */

/*           ELSE */

/*              DO I = 1, WNCARD(RESULT) */

/*     C */
/*     C           Fetch and display each RESULT interval. */
/*     C */
/*                 CALL WNFETD ( RESULT, I, LEFT, RIGHT ) */
/*                 WRITE (*,*) 'Interval ', I */

/*                 CALL ET2UTC ( LEFT, 'C', 4, UTC ) */
/*                 WRITE (*, *) '   Interval start: ', UTC */

/*                 CALL SPKEZP ( 301, LEFT, 'IAU_EARTH', 'NONE', 399, */
/*          .                   POS, LT ) */
/*                 WRITE (*, *) '                Z= ', POS(3) */

/*                 CALL ET2UTC ( RIGHT, 'C', 4, UTC ) */
/*                 WRITE (*, *) '   Interval end  : ', UTC */

/*                 CALL SPKEZP ( 301, RIGHT, 'IAU_EARTH', 'NONE', 399, */
/*          .                   POS, LT ) */
/*                 WRITE (*, *) '                Z= ', POS(3) */
/*                 WRITE (*, *) ' ' */

/*              END DO */

/*           END IF */

/*           END */



/*     C-Procedure GFQ */
/*     C */
/*     C     User defined scalar routine. */
/*     C */

/*           SUBROUTINE GFQ ( ET, VALUE ) */
/*           IMPLICIT NONE */

/*     C- Abstract */
/*     C */
/*     C     Return the Z component of the POS vector. */
/*     C */

/*           DOUBLE PRECISION      ET */
/*           DOUBLE PRECISION      VALUE */

/*     C */
/*     C     Local variables. */
/*     C */
/*           INTEGER               TARG */
/*           INTEGER               OBS */

/*           CHARACTER*(12)        REF */
/*           CHARACTER*(12)        ABCORR */

/*           DOUBLE PRECISION      POS ( 3 ) */
/*           DOUBLE PRECISION      LT */

/*     C */
/*     C     Initialization. Retrieve the vector from the earth to */
/*     C     the moon in the IAU_EARTH frame, without aberration */
/*     C     correction. */
/*     C */
/*           TARG   = 301 */
/*           REF    = 'IAU_EARTH' */
/*           ABCORR = 'NONE' */
/*           OBS    = 399 */

/*     C */
/*     C     Evaluate the position of TARG from OBS at ET with */
/*     C     correction ABCORR. */
/*     C */
/*           CALL SPKEZP ( TARG, ET, REF, ABCORR, OBS, POS, LT ) */

/*           VALUE = POS(3) */

/*           RETURN */
/*           END */



/*     C-Procedure GFB */
/*     C */
/*     C     User defined boolean routine. */
/*     C */

/*           SUBROUTINE GFB ( UDFUNS, ET, BOOL ) */
/*           IMPLICIT NONE */

/*     C- Abstract */
/*     C */
/*     C     User defined boolean function: */
/*     C */
/*     C        VALUE >= LIM1 with VALUE <= LIM2. */
/*     C */

/*           EXTERNAL              UDFUNS */

/*           DOUBLE PRECISION      ET */
/*           LOGICAL               BOOL */
/*           DOUBLE PRECISION      VALUE */


/*           DOUBLE PRECISION      LIM1 */
/*           DOUBLE PRECISION      LIM2 */

/*           LIM1 = -1000.D0 */
/*           LIM2 =  1000.D0 */

/*           CALL UDFUNS ( ET, VALUE ) */

/*     C */
/*     C     Calculate the boolean value. */
/*     C */
/*           BOOL = (VALUE .GE. LIM1) .AND. (VALUE .LE. LIM2 ) */

/*           RETURN */
/*           END */

/*     The program outputs: */

/*      Interval            1 */
/*         Interval start: 2011 JAN 09 14:42:24.4846 */
/*                      Z=   -999.99999990308515 */
/*         Interval end  : 2011 JAN 09 16:06:22.5021 */
/*                      Z=    1000.0000000900436 */

/*      Interval            2 */
/*         Interval start: 2011 JAN 23 04:07:44.4554 */
/*                      Z=    1000.0000001154267 */
/*         Interval end  : 2011 JAN 23 05:23:06.2437 */
/*                      Z=   -1000.0000001147444 */

/*      Interval            3 */
/*         Interval start: 2011 FEB 05 22:35:57.1561 */
/*                      Z=   -999.99999997469570 */
/*         Interval end  : 2011 FEB 05 23:59:57.7487 */
/*                      Z=    999.99999989149978 */

/*                        ... */

/*      Interval           25 */
/*         Interval start: 2011 DEC 03 00:32:08.8206 */
/*                      Z=   -999.99999987966544 */
/*         Interval end  : 2011 DEC 03 02:01:12.7695 */
/*                      Z=    999.99999987608885 */

/*      Interval           26 */
/*         Interval start: 2011 DEC 17 10:17:24.0390 */
/*                      Z=    1000.0000000822058 */
/*         Interval end  : 2011 DEC 17 11:40:37.2235 */
/*                      Z=   -999.99999997521718 */

/*      Interval           27 */
/*         Interval start: 2011 DEC 30 09:04:47.2759 */
/*                      Z=   -1000.0000000487748 */
/*         Interval end  : 2011 DEC 30 10:33:07.6707 */
/*                      Z=    999.99999986779312 */

/*     Recall the default convergence tolerance for the GF system has */
/*     value 10^-6 seconds. */

/* $ Restrictions */

/*     1) Any kernel files required by this routine must be loaded */
/*        (normally via the SPICELIB routine FURNSH) before this routine */
/*        is called. */

/* $ Literature_References */

/*    None. */

/* $ Author_and_Institution */

/*    N.J. Bachman   (JPL) */
/*    E.D. Wright    (JPL) */

/* $ Version */

/* -   SPICELIB Version 1.0.0, 15-JUL-2014 (EDW) */

/* -& */
/* $ Index_Entries */

/*   GF user defined boolean function search */

/* -& */

/*     SPICELIB functions. */


/*     Local variables. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("GFUDB", (ftnlen)5);

/*     Check the result window size. */

    i__1 = sized_(result);
    if (sized_(result) < 2 || odd_(&i__1)) {
	setmsg_("Result window size was #; size must be at least 2 and an ev"
		"en value.", (ftnlen)68);
	i__1 = sized_(result);
	errint_("#", &i__1, (ftnlen)1);
	sigerr_("SPICE(INVALIDDIMENSION)", (ftnlen)23);
	chkout_("GFUDB", (ftnlen)5);
	return 0;
    }

/*     Set the step size. */

    gfsstp_(step);

/*     Retrieve the convergence tolerance, if set. */

    zzholdd_(&c_n1, &c__3, &ok, &tol);

/*     Use the default value CNVTOL if no stored tolerance value. */

    if (! ok) {
	tol = 1e-6;
    }

/*     Initialize the RESULT window to empty. */

    scardd_(&c__0, result);
    zzgfudb_((U_fp)udfuns, (U_fp)udfunb, &tol, (U_fp)gfstep_, (U_fp)gfrefn_, &
	    c_false, (U_fp)gfrepi_, (U_fp)gfrepu_, (U_fp)gfrepf_, &c_false, (
	    L_fp)gfbail_, cnfine, result);
    chkout_("GFUDB", (ftnlen)5);
    return 0;
} /* gfudb_ */

