/* gfoclt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c_n1 = -1;
static integer c__3 = 3;
static logical c_false = FALSE_;

/* $Procedure GFOCLT ( GF, find occultation ) */
/* Subroutine */ int gfoclt_(char *occtyp, char *front, char *fshape, char *
	fframe, char *back, char *bshape, char *bframe, char *abcorr, char *
	obsrvr, doublereal *step, doublereal *cnfine, doublereal *result, 
	ftnlen occtyp_len, ftnlen front_len, ftnlen fshape_len, ftnlen 
	fframe_len, ftnlen back_len, ftnlen bshape_len, ftnlen bframe_len, 
	ftnlen abcorr_len, ftnlen obsrvr_len)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer sized_(doublereal *);
    extern logical gfbail_();
    extern /* Subroutine */ int gfocce_(char *, char *, char *, char *, char *
	    , char *, char *, char *, char *, doublereal *, U_fp, U_fp, 
	    logical *, U_fp, U_fp, U_fp, logical *, L_fp, doublereal *, 
	    doublereal *, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, 
	    ftnlen, ftnlen, ftnlen);
    logical ok;
    extern /* Subroutine */ int gfrefn_(), gfrepf_(), gfrepi_(), gfrepu_(), 
	    gfstep_();
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int gfsstp_(doublereal *);
    doublereal tol;
    extern /* Subroutine */ int zzholdd_(integer *, integer *, logical *, 
	    doublereal *);

/* $ Abstract */

/*     Determine time intervals when an observer sees one target */
/*     body occulted by, or in transit across, another. */

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
/*     GF */
/*     KERNEL */
/*     NAIF_IDS */
/*     SPK */
/*     TIME */
/*     WINDOWS */

/* $ Keywords */

/*     EVENT */
/*     GEOMETRY */
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

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     LBCELL     P   SPICE Cell lower bound. */
/*     CNVTOL     P   Convergence tolerance. */
/*     ZZGET      P   ZZHOLDD retrieves a stored DP value. */
/*     GF_TOL     P   ZZHOLDD acts on the GF subsystem tolerance. */
/*     OCCTYP     I   Type of occultation. */
/*     FRONT      I   Name of body occulting the other. */
/*     FSHAPE     I   Type of shape model used for front body. */
/*     FFRAME     I   Body-fixed, body-centered frame for front body. */
/*     BACK       I   Name of body occulted by the other. */
/*     BSHAPE     I   Type of shape model used for back body. */
/*     BFRAME     I   Body-fixed, body-centered frame for back body. */
/*     ABCORR     I   Aberration correction flag. */
/*     OBSRVR     I   Name of the observing body. */
/*     STEP       I   Step size in seconds for finding occultation */
/*                    events. */
/*     CNFINE     I   SPICE window to which the search is restricted. */
/*     RESULT     O   SPICE window containing results. */

/* $ Detailed_Input */


/*     OCCTYP     indicates the type of occultation that is to be found. */
/*                Note that transits are considered to be a type of */
/*                occultation. */

/*                Supported values and corresponding definitions are: */

/*                   'FULL'               denotes the full occultation */
/*                                        of the body designated by */
/*                                        BACK by the body designated */
/*                                        by FRONT, as seen from */
/*                                        the location of the observer. */
/*                                        In other words, the occulted */
/*                                        body is completely invisible */
/*                                        as seen from the observer's */
/*                                        location. */

/*                   'ANNULAR'            denotes an annular */
/*                                        occultation: the body */
/*                                        designated by FRONT blocks */
/*                                        part of, but not the limb of, */
/*                                        the body designated by BACK, */
/*                                        as seen from the location of */
/*                                        the observer. */

/*                   'PARTIAL'            denotes a partial, */
/*                                        non-annular occultation: the */
/*                                        body designated by FRONT */
/*                                        blocks part, but not all, of */
/*                                        the limb of the body */
/*                                        designated by BACK, as seen */
/*                                        from the location of the */
/*                                        observer. */

/*                   'ANY'                denotes any of the above three */
/*                                        types of occultations: */
/*                                        'PARTIAL', 'ANNULAR', or */
/*                                        'FULL'. */

/*                                        'ANY' should be used to search */
/*                                        for times when the body */
/*                                        designated by FRONT blocks */
/*                                        any part of the body designated */
/*                                        by BACK. */

/*                                        The option 'ANY' must be used */
/*                                        if either the front or back */
/*                                        target body is modeled as */
/*                                        a point. */

/*                Case and leading or trailing blanks are not */
/*                significant in the string OCCTYP. */



/*     FRONT      is the name of the target body that occults---that is, */
/*                passes in front of---the other. Optionally, you may */
/*                supply the integer NAIF ID code for the body as a */
/*                string. For example both 'MOON' and '301' are */
/*                legitimate strings that designate the Moon. */

/*                Case and leading or trailing blanks are not */
/*                significant in the string FRONT. */


/*     FSHAPE     is a string indicating the geometric model used to */
/*                represent the shape of the front target body. The */
/*                supported options are: */

/*                   'ELLIPSOID'     Use a triaxial ellipsoid model */
/*                                   with radius values provided via the */
/*                                   kernel pool. A kernel variable */
/*                                   having a name of the form */

/*                                      'BODYnnn_RADII' */

/*                                   where nnn represents the NAIF */
/*                                   integer code associated with the */
/*                                   body, must be present in the kernel */
/*                                   pool. This variable must be */
/*                                   associated with three numeric */
/*                                   values giving the lengths of the */
/*                                   ellipsoid's X, Y, and Z semi-axes. */

/*                   'POINT'         Treat the body as a single point. */
/*                                   When a point target is specified, */
/*                                   the occultation type must be */
/*                                   set to 'ANY'. */

/*                At least one of the target bodies FRONT and BACK must */
/*                be modeled as an ellipsoid. */

/*                Case and leading or trailing blanks are not */
/*                significant in the string FSHAPE. */


/*     FFRAME     is the name of the body-fixed, body-centered reference */
/*                frame associated with the front target body. Examples */
/*                of such names are 'IAU_SATURN' (for Saturn) and */
/*                'ITRF93' (for the Earth). */

/*                If the front target body is modeled as a point, FFRAME */
/*                should be left blank. */

/*                Case and leading or trailing blanks bracketing a */
/*                non-blank frame name are not significant in the string */
/*                FFRAME. */


/*     BACK       is the name of the target body that is occulted */
/*                by---that is, passes in back of---the other. */
/*                Optionally, you may supply the integer NAIF ID code */
/*                for the body as a string. For example both 'MOON' and */
/*                '301' are legitimate strings that designate the Moon. */

/*                Case and leading or trailing blanks are not */
/*                significant in the string BACK. */


/*     BSHAPE     is the shape specification for the body designated */
/*                by BACK. The supported options are those for */
/*                FSHAPE. See the description of FSHAPE above for */
/*                details. */


/*     BFRAME     is the name of the body-fixed, body-centered reference */
/*                frame associated with the ``back'' target body. */
/*                Examples of such names are 'IAU_SATURN' (for Saturn) */
/*                and 'ITRF93' (for the Earth). */

/*                If the back target body is modeled as a point, BFRAME */
/*                should be left blank. */

/*                Case and leading or trailing blanks bracketing a */
/*                non-blank frame name are not significant in the string */
/*                BFRAME. */


/*     ABCORR     indicates the aberration corrections to be applied to */
/*                the state of each target body to account for one-way */
/*                light time.  Stellar aberration corrections are */
/*                ignored if specified, since these corrections don't */
/*                improve the accuracy of the occultation determination. */

/*                See the header of the SPICE routine SPKEZR for a */
/*                detailed description of the aberration correction */
/*                options. For convenience, the options supported by */
/*                this routine are listed below: */

/*                   'NONE'     Apply no correction. */

/*                   'LT'       "Reception" case:  correct for */
/*                              one-way light time using a Newtonian */
/*                              formulation. */

/*                   'CN'       "Reception" case:  converged */
/*                              Newtonian light time correction. */

/*                   'XLT'      "Transmission" case:  correct for */
/*                              one-way light time using a Newtonian */
/*                              formulation. */

/*                   'XCN'      "Transmission" case:  converged */
/*                              Newtonian light time correction. */

/*                Case and blanks are not significant in the string */
/*                ABCORR. */


/*     OBSRVR     is the name of the body from which the occultation is */
/*                observed. Optionally, you may supply the integer NAIF */
/*                ID code for the body as a string. */

/*                Case and leading or trailing blanks are not */
/*                significant in the string OBSRVR. */


/*     STEP       is the step size to be used in the search. STEP must */
/*                be shorter than any interval, within the confinement */
/*                window, over which the specified occultation condition */
/*                is met. In other words, STEP must be shorter than the */
/*                shortest occultation event that the user wishes to */
/*                detect; STEP must also be shorter than the shortest */
/*                time interval between two occultation events that */
/*                occur within the confinement window (see below). */
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

/*                The endpoints of the time intervals comprising CNFINE */
/*                are interpreted as seconds past J2000 TDB. */

/*                See the Examples section below for a code example */
/*                that shows how to create a confinement window. */

/*                CNFINE must be initialized by the caller via the */
/*                SPICELIB routine SSIZED. */


/* $ Detailed_Output */

/*     RESULT     is a SPICE window representing the set of time */
/*                intervals, within the confinement window, when the */
/*                specified occultation occurs. */

/*                The endpoints of the time intervals comprising RESULT */
/*                are interpreted as seconds past J2000 TDB. */

/*                If RESULT is non-empty on input, its contents */
/*                will be discarded before GFOCLT conducts its */
/*                search. */

/* $ Parameters */

/*     LBCELL     is the lower bound for SPICE cell arrays. */

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
/*         SPICE(INVALIDSTEPSIZE) will be signaled. */

/*     2)  Due to numerical errors, in particular, */

/*            - Truncation error in time values */
/*            - Finite tolerance value */
/*            - Errors in computed geometric quantities */

/*         it is *normal* for the condition of interest to not always be */
/*         satisfied near the endpoints of the intervals comprising the */
/*         result window. */

/*         The result window may need to be contracted slightly by the */
/*         caller to achieve desired results. The SPICE window routine */
/*         WNCOND can be used to contract the result window. */

/*     3)  If name of either target or the observer cannot be translated */
/*         to a NAIF ID code, the error will be diagnosed by a routine */
/*         in the call tree of this routine. */

/*     4)  If the radii of a target body modeled as an ellipsoid cannot */
/*         be determined by searching the kernel pool for a kernel */
/*         variable having a name of the form */

/*            'BODYnnn_RADII' */

/*         where nnn represents the NAIF integer code associated with */
/*         the body, the error will be diagnosed by a routine in the */
/*         call tree of this routine. */

/*     5)  If either of the target bodies FRONT or BACK coincides with */
/*         the observer body OBSRVR, the error will be diagnosed by a */
/*         routine in the call tree of this routine. */

/*     6)  If the body designated by FRONT coincides with that */
/*         designated by BACK, the error will be diagnosed by a routine */
/*         in the call tree of this routine. */

/*     7)  If either of the body model specifiers FSHAPE or BSHAPE */
/*         is not recognized, the error will be diagnosed by a routine */
/*         in the call tree of this routine. */

/*     8)  If both of the body model specifiers FSHAPE and BSHAPE */
/*         specify point targets, the error will be diagnosed by a */
/*         routine in the call tree of this routine. */

/*     9)  If a target body-fixed reference frame associated with a */
/*         non-point target is not recognized, the error will be */
/*         diagnosed by a routine in the call tree of this routine. */

/*     10) If a target body-fixed reference frame is not centered at */
/*         the corresponding target body,  the error will be */
/*         diagnosed by a routine in the call tree of this routine. */

/*     11) If the loaded kernels provide insufficient data to */
/*         compute any required state vector, the deficiency will */
/*         be diagnosed by a routine in the call tree of this routine. */

/*     12) If an error occurs while reading an SPK or other kernel file, */
/*         the error will be diagnosed by a routine in the call tree */
/*         of this routine. */

/*     13) If a point target is specified and the occultation */
/*         type is set to a valid value other than 'ANY', the */
/*         error will be diagnosed by a routine in the call tree */
/*         of this routine. */

/*     14) If the output SPICE window RESULT has insufficient capacity */
/*         to contain the number of intervals on which the specified */
/*         occultation condition is met, the error will be diagnosed */
/*         by a routine in the call tree of this routine. If the result */
/*         window has size less than 2, the error SPICE(WINDOWTOOSMALL) */
/*         will be signaled by this routine. */

/*     15) Invalid occultation types will be diagnosed by a routine in */
/*         the call tree of this routine. */

/*     16) Invalid aberration correction specifications will be */
/*         diagnosed by a routine in the call tree of this routine. */

/* $ Files */

/*     Appropriate SPICE kernels must be loaded by the calling program */
/*     before this routine is called. */

/*     The following data are required: */

/*        - SPK data: the calling application must load ephemeris data */
/*          for the target, source and observer that cover the time */
/*          period specified by the window CNFINE. If aberration */
/*          corrections are used, the states of the target bodies and of */
/*          the observer relative to the solar system barycenter must be */
/*          calculable from the available ephemeris data. Typically */
/*          ephemeris data */
/*          are made available by loading one or more SPK files via */
/*          FURNSH. */

/*        - PCK data: bodies modeled as triaxial ellipsoids must have */
/*          semi-axis lengths provided by variables in the kernel pool. */
/*          Typically these data are made available by loading a text */
/*          PCK file via FURNSH. */

/*        - FK data: if either of the reference frames designated by */
/*          BFRAME or FFRAME are not built in to the SPICE system, */
/*          one or more FKs specifying these frames must be loaded. */

/*     Kernel data are normally loaded once per program run, NOT every */
/*     time this routine is called. */

/* $ Particulars */

/*     This routine provides a simpler, but less flexible, interface */
/*     than does the SPICELIB routine GFOCCE for conducting searches for */
/*     occultation events. Applications that require support for */
/*     progress reporting, interrupt handling, non-default step or */
/*     refinement functions, or non-default convergence tolerance should */
/*     call GFOCCE rather than this routine. */

/*     This routine determines a set of one or more time intervals */
/*     within the confinement window when a specified type of */
/*     occultation occurs. The resulting set of intervals is returned as */
/*     a SPICE window. */

/*     Below we discuss in greater detail aspects of this routine's */
/*     solution process that are relevant to correct and efficient */
/*     use of this routine in user applications. */


/*     The Search Process */
/*     ================== */

/*     The search for occultations is treated as a search for state */
/*     transitions: times are sought when the state of the BACK body */
/*     changes from "not occulted" to "occulted" or vice versa. */

/*     Step Size */
/*     ========= */

/*     Each interval of the confinement window is searched as follows: */
/*     first, the input step size is used to determine the time */
/*     separation at which the occultation state will be sampled. */
/*     Starting at the left endpoint of the interval, samples of the */
/*     occultation state will be taken at each step. If a state change */
/*     is detected, a root has been bracketed; at that point, the */
/*     "root"--the time at which the state change occurs---is found by a */
/*     refinement process, for example, via binary search. */

/*     Note that the optimal choice of step size depends on the lengths */
/*     of the intervals over which the occultation state is constant: */
/*     the step size should be shorter than the shortest occultation */
/*     duration and the shortest period between occultations, within */
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
/*     a time window over which required data (typically ephemeris */
/*     data, in the case of occultation searches) are known to be */
/*     available. */

/*     In some cases, the confinement window be used to make searches */
/*     more efficient. Sometimes it's possible to do an efficient search */
/*     to reduce the size of the time period over which a relatively */
/*     slow search of interest must be performed. See the "CASCADE" */
/*     example program in gf.req for a demonstration. */

/* $ Examples */


/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Find occultations of the Sun by the Moon (that is, solar */
/*        eclipses) as seen from the center of the Earth over the month */
/*        December, 2001. */

/*        Use light time corrections to model apparent positions of Sun */
/*        and Moon. Stellar aberration corrections are not specified */
/*        because they don't affect occultation computations. */

/*        We select a step size of 3 minutes, which means we */
/*        ignore occultation events lasting less than 3 minutes, */
/*        if any exist. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */

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

/*              KERNELS_TO_LOAD = ( 'de421.bsp', */
/*                                  'pck00008.tpc', */
/*                                  'naif0009.tls'  ) */

/*           \begintext */


/*       Example code begins here. */


/*           PROGRAM EX1 */

/*           IMPLICIT NONE */

/*           INTEGER               WNCARD */

/*           CHARACTER*(*)         TIMFMT */
/*           PARAMETER           ( TIMFMT = */
/*          .   'YYYY MON DD HR:MN:SC.###### (TDB)::TDB' ) */

/*           INTEGER               MAXWIN */
/*           PARAMETER           ( MAXWIN = 2 * 100 ) */

/*           INTEGER               TIMLEN */
/*           PARAMETER           ( TIMLEN = 40 ) */

/*           INTEGER               LBCELL */
/*           PARAMETER           ( LBCELL = -5 ) */

/*           CHARACTER*(TIMLEN)    WIN0 */
/*           CHARACTER*(TIMLEN)    WIN1 */
/*           CHARACTER*(TIMLEN)    BEGSTR */
/*           CHARACTER*(TIMLEN)    ENDSTR */

/*           DOUBLE PRECISION      CNFINE ( LBCELL : MAXWIN ) */
/*           DOUBLE PRECISION      ET0 */
/*           DOUBLE PRECISION      ET1 */
/*           DOUBLE PRECISION      LEFT */
/*           DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN ) */
/*           DOUBLE PRECISION      RIGHT */
/*           DOUBLE PRECISION      STEP */

/*           INTEGER               I */

/*     C */
/*     C     Load kernels. */
/*     C */
/*           CALL FURNSH ( 'standard.tm' ) */

/*     C */
/*     C     Initialize the confinement and result windows. */
/*     C */
/*           CALL SSIZED ( MAXWIN, CNFINE ) */
/*           CALL SSIZED ( MAXWIN, RESULT ) */

/*     C */
/*     C     Obtain the TDB time bounds of the confinement */
/*     C     window, which is a single interval in this case. */
/*     C */
/*           WIN0 = '2001 DEC 01 00:00:00 TDB' */
/*           WIN1 = '2002 JAN 01 00:00:00 TDB' */

/*           CALL STR2ET ( WIN0, ET0 ) */
/*           CALL STR2ET ( WIN1, ET1 ) */

/*     C */
/*     C     Insert the time bounds into the confinement */
/*     C     window. */
/*     C */
/*           CALL WNINSD ( ET0, ET1, CNFINE ) */

/*     C */
/*     C     Select a 3-minute step. We'll ignore any occultations */
/*     C     lasting less than 3 minutes. Units are TDB seconds. */
/*     C */
/*           STEP = 180.D0 */

/*     C */
/*     C     Perform the search. */
/*     C */
/*           CALL GFOCLT ( 'ANY', */
/*          .              'MOON',  'ellipsoid', 'IAU_MOON', */
/*          .              'SUN',   'ellipsoid', 'IAU_SUN', */
/*          .              'LT',    'EARTH',     STEP, */
/*          .              CNFINE,  RESULT                  ) */


/*           IF ( WNCARD(RESULT) .EQ. 0 ) THEN */

/*              WRITE (*,*) 'No occultation was found.' */

/*           ELSE */

/*              DO I = 1, WNCARD(RESULT) */
/*     C */
/*     C           Fetch and display each occultation interval. */
/*     C */
/*                 CALL WNFETD ( RESULT, I, LEFT, RIGHT ) */

/*                 CALL TIMOUT ( LEFT,  TIMFMT, BEGSTR ) */
/*                 CALL TIMOUT ( RIGHT, TIMFMT, ENDSTR ) */

/*                 WRITE (*,*) 'Interval ', I */
/*                 WRITE (*,*) '   Start time: '//BEGSTR */
/*                 WRITE (*,*) '   Stop time:  '//ENDSTR */

/*              END DO */

/*           END IF */

/*           END */


/*     When this program was executed on a PC/Linux/g77 platform, the */
/*     output was: */

/*        Interval  1 */
/*           Start time: 2001 DEC 14 20:10:14.195952 (TDB) */
/*           Stop time:  2001 DEC 14 21:35:50.317994 (TDB) */



/*     2) Find occultations of Titan by Saturn or of Saturn by */
/*        Titan as seen from the center of the Earth over the */
/*        last four months of 2008. Model both target bodies as */
/*        ellipsoids. Search for every type of occultation. */

/*        Use light time corrections to model apparent positions of */
/*        Saturn and Titan. Stellar aberration corrections are not */
/*        specified because they don't affect occultation computations. */

/*        We select a step size of 15 minutes, which means we */
/*        ignore occultation events lasting less than 15 minutes, */
/*        if any exist. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */


/*           KPL/MK */

/*           File name: gfoclt_ex2.tm */

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
/*              sat288.bsp                    Satellite ephemeris for */
/*                                            Saturn */
/*              pck00008.tpc                  Planet orientation and */
/*                                            radii */
/*              naif0009.tls                  Leapseconds */

/*           \begindata */

/*              KERNELS_TO_LOAD = ( 'de421.bsp', */
/*                                  'sat286.bsp', */
/*                                  'pck00008.tpc', */
/*                                  'naif0009.tls'  ) */

/*           \begintext */

/*           End of meta-kernel */


/*       Example code begins here. */


/*           PROGRAM EX2 */
/*           IMPLICIT NONE */
/*     C */
/*     C     SPICELIB functions */
/*     C */
/*           INTEGER               WNCARD */
/*     C */
/*     C     Local parameters */
/*     C */
/*           CHARACTER*(*)         TIMFMT */
/*           PARAMETER           ( TIMFMT = */
/*          .   'YYYY MON DD HR:MN:SC.###### (TDB)::TDB' ) */

/*           INTEGER               MAXWIN */
/*           PARAMETER           ( MAXWIN = 2 * 100 ) */

/*           INTEGER               TIMLEN */
/*           PARAMETER           ( TIMLEN = 40 ) */

/*           INTEGER               BDNMLN */
/*           PARAMETER           ( BDNMLN = 36 ) */

/*           INTEGER               FRNMLN */
/*           PARAMETER           ( FRNMLN = 32 ) */
/*     C */
/*     C     Number of occultation types: */
/*     C */
/*           INTEGER               NTYPES */
/*           PARAMETER           ( NTYPES = 4 ) */
/*     C */
/*     C     Occultation type name length: */
/*     C */
/*           INTEGER               OCNMLN */
/*           PARAMETER           ( OCNMLN = 10 ) */
/*     C */
/*     C     Output line length: */
/*     C */
/*           INTEGER               LNSIZE */
/*           PARAMETER           ( LNSIZE = 80 ) */

/*           INTEGER               LBCELL */
/*           PARAMETER           ( LBCELL = -5 ) */

/*           CHARACTER*(BDNMLN)    BACK */
/*           CHARACTER*(FRNMLN)    BFRAME */
/*           CHARACTER*(FRNMLN)    FFRAME */
/*           CHARACTER*(BDNMLN)    FRONT */
/*           CHARACTER*(LNSIZE)    LINE */
/*           CHARACTER*(BDNMLN)    OBSRVR */
/*           CHARACTER*(OCNMLN)    OCCTYP ( NTYPES ) */
/*           CHARACTER*(LNSIZE)    TEMPLT ( NTYPES ) */
/*           CHARACTER*(TIMLEN)    TIMSTR */
/*           CHARACTER*(LNSIZE)    TITLE */
/*           CHARACTER*(TIMLEN)    WIN0 */
/*           CHARACTER*(TIMLEN)    WIN1 */

/*           DOUBLE PRECISION      CNFINE ( LBCELL : MAXWIN ) */
/*           DOUBLE PRECISION      ET0 */
/*           DOUBLE PRECISION      ET1 */
/*           DOUBLE PRECISION      FINISH */
/*           DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN ) */
/*           DOUBLE PRECISION      START */
/*           DOUBLE PRECISION      STEP */

/*           INTEGER               I */
/*           INTEGER               J */
/*           INTEGER               K */
/*     C */
/*     C     Saved variables */
/*     C */
/*     C     The confinement and result windows CNFINE */
/*     C     and RESULT are saved because this practice */
/*     C     helps to prevent stack overflow. */
/*     C */
/*     C     The variables OCCTYP and TEMPLT are */
/*     C     saved to facilitate turning this main program into */
/*     C     a subroutine. In a main program, it's not */
/*     C     necessary to save these variables. */
/*     C */
/*           SAVE                  CNFINE */
/*           SAVE                  OCCTYP */
/*           SAVE                  RESULT */
/*           SAVE                  TEMPLT */
/*     C */
/*     C     Initial values */
/*     C */
/*           DATA                  OCCTYP / 'FULL', */
/*          .                               'ANNULAR', */
/*          .                               'PARTIAL', */
/*          .                               'ANY'     / */

/*           DATA                  TEMPLT / */
/*          .      'Condition: # occultation of # by #', */
/*          .      'Condition: # occultation of # by #', */
/*          .      'Condition: # occultation of # by #', */
/*          .      'Condition: # occultation of # by #'      / */

/*     C */
/*     C     Load kernels. */
/*     C */
/*           CALL FURNSH ( 'gfoclt_ex2.tm' ) */

/*     C */
/*     C     Initialize the confinement and result windows. */
/*     C */
/*           CALL SSIZED ( MAXWIN, CNFINE ) */
/*           CALL SSIZED ( MAXWIN, RESULT ) */

/*     C */
/*     C     Obtain the TDB time bounds of the confinement */
/*     C     window, which is a single interval in this case. */
/*     C */
/*           WIN0 = '2008 SEP 01 00:00:00 TDB' */
/*           WIN1 = '2009 JAN 01 00:00:00 TDB' */

/*           CALL STR2ET ( WIN0, ET0 ) */
/*           CALL STR2ET ( WIN1, ET1 ) */
/*     C */
/*     C     Insert the time bounds into the confinement */
/*     C     window. */
/*     C */
/*           CALL WNINSD ( ET0, ET1, CNFINE ) */
/*     C */
/*     C     Select a 15-minute step. We'll ignore any occultations */
/*     C     lasting less than 15 minutes. Units are TDB seconds. */
/*     C */
/*           STEP = 900.D0 */
/*     C */
/*     C     The observation location is the Earth. */
/*     C */
/*           OBSRVR = 'EARTH' */

/*     C */
/*     C     Loop over the occultation types. */
/*     C */
/*           DO I = 1, NTYPES */
/*     C */
/*     C        For each type, do a search for both transits of */
/*     C        Titan across Saturn and occultations of Titan by */
/*     C        Saturn. */
/*     C */
/*              DO J = 1, 2 */

/*                 IF ( J .EQ. 1 ) THEN */

/*                    FRONT  = 'TITAN' */
/*                    FFRAME = 'IAU_TITAN' */
/*                    BACK   = 'SATURN' */
/*                    BFRAME = 'IAU_SATURN' */

/*                 ELSE */

/*                    FRONT  = 'SATURN' */
/*                    FFRAME = 'IAU_SATURN' */
/*                    BACK   = 'TITAN' */
/*                    BFRAME = 'IAU_TITAN' */

/*                 END IF */
/*     C */
/*     C           Perform the search. The target body shapes */
/*     C           are modeled as ellipsoids. */
/*     C */
/*                 CALL GFOCLT ( OCCTYP(I), */
/*          .                    FRONT,  'ELLIPSOID', FFRAME, */
/*          .                    BACK,   'ELLIPSOID', BFRAME, */
/*          .                    'LT',   OBSRVR,      STEP, */
/*          .                    CNFINE, RESULT              ) */
/*     C */
/*     C           Display the results. */
/*     C */
/*                 WRITE (*,*) ' ' */
/*     C */
/*     C           Substitute the occultation type and target */
/*     C           body names into the title string: */
/*     C */
/*                 CALL REPMC ( TEMPLT(I), '#', OCCTYP(I), TITLE ) */
/*                 CALL REPMC ( TITLE,     '#', BACK,      TITLE ) */
/*                 CALL REPMC ( TITLE,     '#', FRONT,     TITLE ) */

/*                 WRITE (*, '(A)' ) TITLE */

/*                 IF ( WNCARD(RESULT) .EQ. 0 ) THEN */

/*                    WRITE (*, '(A)' ) ' Result window is empty: ' */
/*          .         //                'no occultation was found.' */

/*                 ELSE */

/*                    WRITE (*, '(A)' ) ' Result window start, ' */
/*          .         //                'stop times:' */

/*                    DO K = 1, WNCARD(RESULT) */
/*     C */
/*     C                 Fetch the endpoints of the Kth interval */
/*     C                 of the result window. */
/*     C */
/*                       CALL WNFETD ( RESULT, K, START, FINISH ) */

/*                       LINE = '  #  #' */

/*                       CALL TIMOUT ( START, TIMFMT, TIMSTR ) */

/*                       CALL REPMC  ( LINE, '#', TIMSTR, LINE ) */

/*                       CALL TIMOUT ( FINISH, TIMFMT, TIMSTR ) */

/*                       CALL REPMC  ( LINE, '#', TIMSTR, LINE ) */

/*                       WRITE ( *, '(A)' ) LINE */

/*                    END DO */

/*                 END IF */
/*     C */
/*     C           We've finished displaying the results of the */
/*     C           current search. */
/*     C */
/*              END DO */
/*     C */
/*     C        We've finished displaying the results of the */
/*     C        searches using the current occultation type. */
/*     C */
/*           END DO */

/*           WRITE (*,*) ' ' */

/*           END */

/*     When this program was executed on a PC/Linux/g77 platform, the */
/*     output was: */


/* Condition: FULL occultation of SATURN by TITAN */
/* Result window is empty: no occultation was found. */

/* Condition: FULL occultation of TITAN by SATURN */
/*  Result window start, stop times: */
/*   2008 OCT 27 22:08:01.627053 (TDB)  2008 OCT 28 01:05:03.375236 (TDB) */
/*   2008 NOV 12 21:21:59.252262 (TDB)  2008 NOV 13 02:06:05.053051 (TDB) */
/*   2008 NOV 28 20:49:02.402832 (TDB)  2008 NOV 29 02:13:58.986344 (TDB) */
/*   2008 DEC 14 20:05:09.246177 (TDB)  2008 DEC 15 01:44:53.523002 (TDB) */
/*   2008 DEC 30 19:00:56.577073 (TDB)  2008 DEC 31 00:42:43.222909 (TDB) */

/* Condition: ANNULAR occultation of SATURN by TITAN */
/*  Result window start, stop times: */
/*   2008 OCT 19 21:29:20.599087 (TDB)  2008 OCT 19 22:53:34.518737 (TDB) */
/*   2008 NOV 04 20:15:38.620368 (TDB)  2008 NOV 05 00:18:59.139978 (TDB) */
/*   2008 NOV 20 19:38:59.647712 (TDB)  2008 NOV 21 00:35:26.725908 (TDB) */
/*   2008 DEC 06 18:58:34.073268 (TDB)  2008 DEC 07 00:16:17.647040 (TDB) */
/*   2008 DEC 22 18:02:46.288289 (TDB)  2008 DEC 22 23:26:52.712459 (TDB) */

/* Condition: ANNULAR occultation of TITAN by SATURN */
/*  Result window is empty: no occultation was found. */

/* Condition: PARTIAL occultation of SATURN by TITAN */
/*  Result window start, stop times: */
/*   2008 OCT 19 20:44:30.326771 (TDB)  2008 OCT 19 21:29:20.599087 (TDB) */
/*   2008 OCT 19 22:53:34.518737 (TDB)  2008 OCT 19 23:38:26.250580 (TDB) */
/*   2008 NOV 04 19:54:40.339331 (TDB)  2008 NOV 04 20:15:38.620368 (TDB) */
/*   2008 NOV 05 00:18:59.139978 (TDB)  2008 NOV 05 00:39:58.612935 (TDB) */
/*   2008 NOV 20 19:21:46.689523 (TDB)  2008 NOV 20 19:38:59.647712 (TDB) */
/*   2008 NOV 21 00:35:26.725908 (TDB)  2008 NOV 21 00:52:40.604703 (TDB) */
/*   2008 DEC 06 18:42:36.100544 (TDB)  2008 DEC 06 18:58:34.073268 (TDB) */
/*   2008 DEC 07 00:16:17.647040 (TDB)  2008 DEC 07 00:32:16.324244 (TDB) */
/*   2008 DEC 22 17:47:10.776722 (TDB)  2008 DEC 22 18:02:46.288289 (TDB) */
/*   2008 DEC 22 23:26:52.712459 (TDB)  2008 DEC 22 23:42:28.850542 (TDB) */

/* Condition: PARTIAL occultation of TITAN by SATURN */
/*  Result window start, stop times: */
/*   2008 OCT 27 21:37:16.970175 (TDB)  2008 OCT 27 22:08:01.627053 (TDB) */
/*   2008 OCT 28 01:05:03.375236 (TDB)  2008 OCT 28 01:35:49.266506 (TDB) */
/*   2008 NOV 12 21:01:47.105498 (TDB)  2008 NOV 12 21:21:59.252262 (TDB) */
/*   2008 NOV 13 02:06:05.053051 (TDB)  2008 NOV 13 02:26:18.227357 (TDB) */
/*   2008 NOV 28 20:31:28.522707 (TDB)  2008 NOV 28 20:49:02.402832 (TDB) */
/*   2008 NOV 29 02:13:58.986344 (TDB)  2008 NOV 29 02:31:33.691598 (TDB) */
/*   2008 DEC 14 19:48:27.094229 (TDB)  2008 DEC 14 20:05:09.246177 (TDB) */
/*   2008 DEC 15 01:44:53.523002 (TDB)  2008 DEC 15 02:01:36.360243 (TDB) */
/*   2008 DEC 30 18:44:23.485898 (TDB)  2008 DEC 30 19:00:56.577073 (TDB) */
/*   2008 DEC 31 00:42:43.222909 (TDB)  2008 DEC 31 00:59:17.030568 (TDB) */

/* Condition: ANY occultation of SATURN by TITAN */
/*  Result window start, stop times: */
/*   2008 OCT 19 20:44:30.326771 (TDB)  2008 OCT 19 23:38:26.250580 (TDB) */
/*   2008 NOV 04 19:54:40.339331 (TDB)  2008 NOV 05 00:39:58.612935 (TDB) */
/*   2008 NOV 20 19:21:46.689523 (TDB)  2008 NOV 21 00:52:40.604703 (TDB) */
/*   2008 DEC 06 18:42:36.100544 (TDB)  2008 DEC 07 00:32:16.324244 (TDB) */
/*   2008 DEC 22 17:47:10.776722 (TDB)  2008 DEC 22 23:42:28.850542 (TDB) */

/* Condition: ANY occultation of TITAN by SATURN */
/*  Result window start, stop times: */
/*   2008 OCT 27 21:37:16.970175 (TDB)  2008 OCT 28 01:35:49.266506 (TDB) */
/*   2008 NOV 12 21:01:47.105498 (TDB)  2008 NOV 13 02:26:18.227357 (TDB) */
/*   2008 NOV 28 20:31:28.522707 (TDB)  2008 NOV 29 02:31:33.691598 (TDB) */
/*   2008 DEC 14 19:48:27.094229 (TDB)  2008 DEC 15 02:01:36.360243 (TDB) */
/*   2008 DEC 30 18:44:23.485898 (TDB)  2008 DEC 31 00:59:17.030568 (TDB) */


/* $ Restrictions */

/*     The kernel files to be used by GFOCLT must be loaded (normally via */
/*     the SPICELIB routine FURNSH) before GFOCLT is called. */

/* $ Literature_References */

/*    None. */

/* $ Author_and_Institution */

/*    N. J. Bachman  (JPL) */
/*    L. S. Elson    (JPL) */
/*    E. D. Wright   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0  31-AUG-2010 (EDW) */

/*        Implemented use of ZZHOLDD to allow user to alter convergence */
/*        tolerance. */

/*        Removed the STEP > 0 error check. The GFSSTP call includes */
/*        the check. */

/* -    SPICELIB Version 1.0.0  07-APR-2009 (NJB) (LSE) (EDW) */

/* -& */
/* $ Index_Entries */

/*     GF occultation search */

/* -& */
/* $ Revisions */

/*     None. */

/* -& */

/*     SPICELIB functions */


/*     Local variables. */


/*     External routines */


/*     Interrupt handler: */


/*     Routines to set step size, refine transition times */
/*     and report work: */


/*     Local parameters */


/*     Geometric quantity  bail switch: */


/*     Progress report switch: */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("GFOCLT", (ftnlen)6);

/*     Note to maintenance programmer: input exception checks */
/*     are delegated to GFOCCE. If the implementation of that */
/*     routine changes, or if this routine is modified to call */
/*     a different routine in place of GFOCCE, then the error */
/*     handling performed by GFOCCE will have to be performed */
/*     here or in a routine called by this routine. */

/*     Check the result window's size. */

    if (sized_(result) < 2) {
	setmsg_("Result window size must be at least 2 but was #.", (ftnlen)
		48);
	i__1 = sized_(result);
	errint_("#", &i__1, (ftnlen)1);
	sigerr_("SPICE(WINDOWTOOSMALL)", (ftnlen)21);
	chkout_("GFOCLT", (ftnlen)6);
	return 0;
    }

/*     Check and set the step size. */

    gfsstp_(step);

/*     Retrieve the convergence tolerance, if set. */

    zzholdd_(&c_n1, &c__3, &ok, &tol);

/*     Use the default value CNVTOL if no stored tolerance value. */

    if (! ok) {
	tol = 1e-6;
    }

/*     Look for solutions. */

    gfocce_(occtyp, front, fshape, fframe, back, bshape, bframe, abcorr, 
	    obsrvr, &tol, (U_fp)gfstep_, (U_fp)gfrefn_, &c_false, (U_fp)
	    gfrepi_, (U_fp)gfrepu_, (U_fp)gfrepf_, &c_false, (L_fp)gfbail_, 
	    cnfine, result, occtyp_len, front_len, fshape_len, fframe_len, 
	    back_len, bshape_len, bframe_len, abcorr_len, obsrvr_len);
    chkout_("GFOCLT", (ftnlen)6);
    return 0;
} /* gfoclt_ */

