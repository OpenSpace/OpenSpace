/* gfocce.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static logical c_false = FALSE_;
static doublereal c_b21 = 1.;

/* $Procedure      GFOCCE ( GF, occultation event ) */
/* Subroutine */ int gfocce_(char *occtyp, char *front, char *fshape, char *
	fframe, char *back, char *bshape, char *bframe, char *abcorr, char *
	obsrvr, doublereal *tol, U_fp udstep, U_fp udrefn, logical *rpt, S_fp 
	udrepi, U_fp udrepu, S_fp udrepf, logical *bail, L_fp udbail, 
	doublereal *cnfine, doublereal *result, ftnlen occtyp_len, ftnlen 
	front_len, ftnlen fshape_len, ftnlen fframe_len, ftnlen back_len, 
	ftnlen bshape_len, ftnlen bframe_len, ftnlen abcorr_len, ftnlen 
	obsrvr_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int zzgfocin_(char *, char *, char *, char *, 
	    char *, char *, char *, char *, char *, ftnlen, ftnlen, ftnlen, 
	    ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen);
    extern /* Subroutine */ int zzgfocst_();
    extern /* Subroutine */ int zzgfsolv_(U_fp, U_fp, U_fp, logical *, L_fp, 
	    logical *, doublereal *, doublereal *, doublereal *, doublereal *,
	     logical *, U_fp, doublereal *);
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), ucase_(char *, char *,
	     ftnlen, ftnlen), errdp_(char *, doublereal *, ftnlen);
    extern integer sized_(doublereal *);
    integer count;
    doublereal start;
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    extern logical failed_(void);
    extern /* Subroutine */ int scardd_(integer *, doublereal *);
    char lbshap[9], lfshap[9];
    extern integer wncard_(doublereal *);
    doublereal finish;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), setmsg_(char *, 
	    ftnlen), errint_(char *, integer *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int chkout_(char *, ftnlen), wnfetd_(doublereal *,
	     integer *, doublereal *, doublereal *);

/* $ Abstract */

/*     Determine time intervals when an observer sees one target */
/*     occulted by another. Report progress and handle interrupts */
/*     if so commanded. */

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

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     LBCELL     P   SPICE Cell lower bound. */
/*     OCCTYP     I   Type of occultation. */
/*     FRONT      I   Name of body occulting the other. */
/*     FSHAPE     I   Type of shape model used for front body. */
/*     FFRAME     I   Body-fixed, body-centered frame for front body. */
/*     BACK       I   Name of body occulted by the other. */
/*     BSHAPE     I   Type of shape model used for back body. */
/*     BFRAME     I   Body-fixed, body-centered frame for back body. */
/*     ABCORR     I   Aberration correction flag. */
/*     OBSRVR     I   Name of the observing body. */
/*     TOL        I   Convergence tolerance in seconds. */
/*     UDSTEP     I   Name of the routine that returns a time step. */
/*     UDREFN     I   Name of the routine that computes a refined time. */
/*     RPT        I   Progress report flag. */
/*     UDREPI     I   Function that initializes progress reporting. */
/*     UDREPU     I   Function that updates the progress report. */
/*     UDREPF     I   Function that finalizes progress reporting. */
/*     BAIL       I   Logical indicating program interrupt monitoring. */
/*     UDBAIL     I   Name of a routine that signals a program interrupt. */
/*     CNFINE     I   SPICE window to which the search is restricted. */
/*     RESULT     O   SPICE window containing results. */

/* $ Detailed_Input */


/*     OCCTYP     indicates the type of occultation that is to be found. */
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

/*                   'PARTIAL'            denotes an partial, */
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


/*     FSHAPE     is a string indicating the geometric model used */
/*                to represent the shape of the front body. The */
/*                supported options are: */

/*                   'ELLIPSOID'     Use a triaxial ellipsoid model, */
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
/*                Case and leading or trailing blanks are not */
/*                significant in the string FFRAME. */


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
/*                frame associated with the ``back'' target body.  See */
/*                the description of FFRAME above for details. */
/*                Examples of such names are 'IAU_SATURN' (for Saturn) */
/*                and 'ITRF93' (for the Earth). */

/*                If the back target body is modeled as a point, BFRAME */
/*                should be left blank. */

/*                Case and leading or trailing blanks bracketing a */
/*                non-blank frame name are not significant in the string */
/*                BFRAME. */


/*     ABCORR     indicates the aberration corrections to be applied to */
/*                the state of the target body to account for one-way */
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


/*     TOL        is a tolerance value used to determine convergence of */
/*                root-finding operations. TOL is measured in TDB seconds */
/*                and must be greater than zero. */


/*     UDSTEP     is an externally specified routine that computes a */
/*                time step used to find transitions of the state being */
/*                considered. A state transition occurs where the state */
/*                changes from being "in occultation" to being "not in */
/*                occultation" or vice versa. */

/*                This routine relies on UDSTEP returning step sizes */
/*                small enough so that state transitions within the */
/*                confinement window are not overlooked. */

/*                The calling sequence for UDSTEP is: */

/*                   CALL UDSTEP ( ET, STEP ) */

/*                where: */

/*                   ET      is the input start time from which the */
/*                           algorithm is to search forward for a state */
/*                           transition. ET is expressed as seconds past */
/*                           J2000 TDB. ET is a DOUBLE PRECISION number. */

/*                   STEP    is the output step size.  STEP indicates */
/*                           how far to advance ET so that ET and */
/*                           ET+STEP may bracket a state transition and */
/*                           definitely do not bracket more than one */
/*                           state transition. STEP is a DOUBLE */
/*                           PRECISION number. Units are TDB seconds. */

/*                If a constant step size is desired, the routine GFSTEP */
/*                may be used. If GFSTEP is used, the step size must be */
/*                set by calling GFSSTP prior to calling this routine. */


/*     UDREFN     is the name of the externally specified routine that */
/*                refines the times that bracket a transition point. In */
/*                other words, once a pair of times, T1 and T2, that */
/*                bracket a state transition have been found, UDREFN */
/*                computes an intermediate time T such that either */
/*                [T1, T] or [T, T2] contains the time of the state */
/*                transition. The calling sequence for UDREFN is: */

/*                   CALL UDREFN ( T1, T2, S1, S2, T ) */

/*                where the inputs are: */

/*                   T1    is a time when the visibility state is S1. T1 */
/*                         is expressed as seconds past J2000 TDB. */

/*                   T2    is a time when the visibility state is S2. T2 */
/*                         is expressed as seconds past J2000 TDB. and */
/*                         is assumed to be larger than T1. */

/*                   S1    is the visibility state at time T1. S1 is a */
/*                         LOGICAL value. */

/*                   S2    is the visibility state at time T2. S2 is a */
/*                         LOGICAL value. */

/*                The output is: */

/*                   T     is the next time to check for a state */
/*                         transition. T is expressed as seconds past */
/*                         J2000 TDB and is between T1 and T2. */

/*                If a simple bisection method is desired, the routine */
/*                GFREFN may be used. */


/*     RPT        is a logical variable which controls whether */
/*                progress reporting is enabled. When RPT is .TRUE., */
/*                progress reporting is enabled and the routines */
/*                UDREPI, UDREPU, and UDPREF (see descriptions below) */
/*                are used to report progress. */


/*     UDREPI     is a user-defined subroutine that initializes a */
/*                progress report. When progress reporting is */
/*                enabled, UDREPI is called at the start */
/*                of a search. The calling sequence of UDREPI is */

/*                   UDREPI ( CNFINE, SRCPRE, SRCSUF ) */

/*                   DOUBLE PRECISION    CNFINE ( LBCELL : * ) */
/*                   CHARACTER*(*)       SRCPRE */
/*                   CHARACTER*(*)       SRCSUF */

/*                where */

/*                   CNFINE */

/*                is the confinement window and */

/*                   SRCPRE */
/*                   SRCSUF */

/*                are prefix and suffix strings used in the progress */
/*                report: these strings are intended to bracket a */
/*                representation of the fraction of work done.  For */
/*                example, when the CSPICE progress reporting functions */
/*                are used, if srcpre and srcsuf are, respectively, */

/*                   "Occultation search" */
/*                   "done." */

/*                the progress report display at the end of the */
/*                search will be: */

/*                   FOV search 100.00% done. */

/*                The SPICELIB routine GFREPI may be used as the */
/*                actual argument corresponding to UDREPI. If so, */
/*                the SPICELIB routines GFREPU and GFREPF must be */
/*                the actual arguments corresponding to UDREPU and */
/*                UDREPF. */


/*     UDREPU     is a user-defined subroutine that updates the */
/*                progress report for a search.  The calling sequence */
/*                of UDREPU is */

/*                   UDREPU ( IVBEG, IVEND, ET ) */

/*                   DOUBLE PRECISION      IVBEG */
/*                   DOUBLE PRECISION      IVEND */
/*                   DOUBLE PRECISION      ET */

/*                Here IVBEG, IVEND are the bounds of an interval that */
/*                is contained in some interval belonging to the */
/*                confinement window. The confinement window is */
/*                associated with some root finding activity. It is used */
/*                to determine how much total time is being searched in */
/*                order to find the events of interest. */

/*                ET is an epoch belonging to the interval */
/*                [IVBEG, IVEND]. */

/*                In order for a meaningful progress report to be */
/*                displayed, IVBEG and IVEND must satisfy the following */
/*                constraints: */

/*                 - IVBEG must be less than or equal to IVEND. */

/*                 - The interval [ IVBEG, IVEND ] must be contained in */
/*                   some interval of the confinement window. It can be */
/*                   a proper subset of the containing interval; that */
/*                   is, it can be smaller than the interval of the */
/*                   confinement window that contains it. */

/*                 - Over a search, the sum of the differences */

/*                      IVEND - IVBEG */

/*                   for all calls to this routine made during the search */
/*                   must equal the measure of the confinement window. */

/*                The SPICELIB routine GFREPU may be used as the */
/*                actual argument corresponding to UDREPU. If so, */
/*                the SPICELIB routines GFREPI and GFREPF must be */
/*                the actual arguments corresponding to UDREPI and */
/*                UDREPF. */


/*     UDREPF     is a user-defined subroutine that finalizes a */
/*                progress report. UDREPF has no arguments. */

/*                The SPICELIB routine GFREPF may be used as the */
/*                actual argument corresponding to UDREPF. If so, */
/*                the SPICELIB routines GFREPI and GFREPU must be */
/*                the actual arguments corresponding to UDREPI and */
/*                UDREPU. */


/*     BAIL       is a logical variable indicating whether or not */
/*                interrupt handling is enabled. When BAIL is */
/*                set to .TRUE., the input function UDBAIL (see */
/*                description below) is used to determine whether */
/*                an interrupt has been issued. */


/*     UDBAIL     is the name of a user defined logical function that */
/*                indicates whether an interrupt signal has been */
/*                issued (for example, from the keyboard).  UDBAIL */
/*                has no arguments and returns a LOGICAL value. */
/*                The return value is .TRUE. if an interrupt has */
/*                been issued; otherwise the value is .FALSE. */

/*                GFOCCE uses UDBAIL only when BAIL (see above) is set */
/*                to .TRUE., indicating that interrupt handling is */
/*                enabled. When interrupt handling is enabled, GFOCCE */
/*                and routines in its call tree will call UDBAIL to */
/*                determine whether to terminate processing and return */
/*                immediately. */

/*                If interrupt handing is not enabled, a logical */
/*                function must still be passed to GFOCCE as */
/*                an input argument. The SPICE function */

/*                   GFBAIL */

/*                may be used for this purpose. */


/*     CNFINE     is a SPICE window that confines the time period over */
/*                which the specified search is conducted. CNFINE may */
/*                consist of a single interval or a collection of */
/*                intervals. */

/*                The endpoints of the time intervals comprising CNFINE */
/*                are interpreted as seconds past J2000 TDB.. */

/*                See the Examples section below for a code example */
/*                that shows how to create a confinement window. */

/*                CNFINE must be initialized by the caller via the */
/*                SPICELIB routine SSIZED. */


/* $ Detailed_Output */

/*     RESULT     is a SPICE window representing the set of time */
/*                intervals, within the confinement period, when the */
/*                specified occultation occurs. */

/*                The endpoints of the time intervals comprising RESULT */
/*                are interpreted as seconds past J2000 TDB. */

/*                If RESULT is non-empty on input, its contents */
/*                will be discarded before GFOCCE conducts its */
/*                search. */

/* $ Parameters */

/*     LBCELL     is the SPICELIB cell lower bound. */

/* $ Exceptions */

/*     1)  In order for this routine to produce correct results, */
/*         the step size must be appropriate for the problem at hand. */
/*         Step sizes that are too large may cause this routine to miss */
/*         roots; step sizes that are too small may cause this routine */
/*         to run unacceptably slowly and in some cases, find spurious */
/*         roots. */

/*         This routine does not diagnose invalid step sizes, except */
/*         that if the step size is non-positive, the error */
/*         SPICE(INVALIDSTEP) will be signaled. */

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
/*         to a NAIF ID code, the error SPICE(IDCODENOTFOUND) is */
/*         signaled. */

/*     4)  If the radii of a target body modeled as an ellipsoid cannot */
/*         be determined by searching the kernel pool for a kernel */
/*         variable having a name of the form */

/*            'BODYnnn_RADII' */

/*         where nnn represents the NAIF integer code associated with */
/*         the body, the error will be diagnosed by a routine in the */
/*         call tree of this routine. */

/*     5)  If either of the target bodies FRONT or BACK coincides with */
/*         the observer body OBSRVR, the error SPICE(BODIESNOTDISTINCT) */
/*         will be signaled. */

/*     6)  If the body designated by FRONT coincides with that */
/*         designated by BACK, the error SPICE(BODIESNOTDISTINCT) will */
/*         be signaled. */

/*     7)  If either of the body model specifiers FSHAPE or BSHAPE */
/*         is not recognized, the error SPICE(INVALIDSHAPE) will be */
/*         signaled. */

/*     8)  If both of the body model specifiers FSHAPE and BSHAPE */
/*         specify point targets, the error SPICE(INVALIDSHAPECOMBO) */
/*         will be signaled. */

/*     9)  If a target body-fixed reference frame associated with a */
/*         non-point target is not recognized, the error will be */
/*         diagnosed by a routine in the call tree of this routine. */

/*     10) If a target body-fixed reference frame is not centered at */
/*         the corresponding target body,  the error will be */
/*         diagnosed by a routine in the call tree of this routine. */

/*     11) If the loaded kernels provide insufficient data to */
/*         compute the requested state vector, the deficiency will */
/*         be diagnosed by a routine in the call tree of this routine. */

/*     12) If an error occurs while reading an SPK or other kernel file, */
/*         the error will be diagnosed by a routine in the call tree */
/*         of this routine. */

/*     13) If a point target is specified and the occultation */
/*         type is set to a valid value other than 'ANY', the */
/*         error SPICE(BADTYPESHAPECOMBO) will be signaled. */

/*     14) If the output SPICE window RESULT has insufficient capacity */
/*         to contain the number of intervals on which the specified */
/*         visibility condition is met, the error will be diagnosed */
/*         by a routine in the call tree of this routine. If the result */
/*         window has size less than 2, the error SPICE(WINDOWTOOSMALL) */
/*         will be signaled by this routine. */

/*     15) Invalid occultation types will be diagnosed by a routine in */
/*         the call tree of this routine. */

/*     16) Invalid aberration correction specifications will be */
/*         diagnosed by a routine in the call tree of this routine. */

/*     17) If the convergence tolerance size is non-positive, the error */
/*         SPICE(INVALIDTOLERANCE) will be signaled. */


/* $ Files */

/*     Appropriate kernels must be loaded by the calling program before */
/*     this routine is called. */

/*     The following data are required: */

/*        - SPK data: the calling application must load ephemeris data */
/*          for the target, source and observer that cover the time */
/*          period specified by the window CNFINE. If aberration */
/*          corrections are used, the states of target and observer */
/*          relative to the solar system barycenter must be calculable */
/*          from the available ephemeris data. Typically ephemeris data */
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

/*     This routine provides the SPICE GF system's most flexible */
/*     interface for searching for occultation events. */

/*     Applications that require do not require support for progress */
/*     reporting, interrupt handling, non-default step or refinement */
/*     functions, or non-default convergence tolerance normally should */
/*     call GFOCLT rather than this routine. */

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
/*     Starting at the left endpoint of an interval, samples will be */
/*     taken at each step. If a state change is detected, a root has */
/*     been bracketed; at that point, the "root"--the time at which the */
/*     state change occurs---is found by a refinement process, for */
/*     example, via binary search. */

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
/*     "convergence tolerance." */

/*     The convergence tolerance used by high-level GF routines that */
/*     call this routine is set via the parameter CNVTOL, which is */
/*     declared in the INCLUDE file gf.inc. The value of CNVTOL is set */
/*     to a "tight" value so that the tolerance doesn't become the */
/*     limiting factor in the accuracy of solutions found by this */
/*     routine. In general the accuracy of input data will be the */
/*     limiting factor. */

/*     Setting the input tolerance TOL tighter than CNVTOL is unlikely */
/*     to be useful, since the results are unlikely to be more accurate. */
/*     Making the tolerance looser will speed up searches somewhat, */
/*     since a few convergence steps will be omitted. However, in most */
/*     cases, the step size is likely to have a much greater effect on */
/*     processing time than would the convergence tolerance. */


/*     The Confinement Window */
/*     ====================== */

/*     The simplest use of the confinement window is to specify a time */
/*     interval within which a solution is sought. However, the */
/*     confinement window can, in some cases, be used to make searches */
/*     more efficient. Sometimes it's possible to do an efficient search */
/*     to reduce the size of the time period over which a relatively */
/*     slow search of interest must be performed. For an example, see */
/*     the program CASCADE in the GF Example Programs chapter of the GF */
/*     Required Reading, gf.req. */


/* $ Examples */


/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     1) Conduct a search using the default GF progress reporting */
/*        capability. */

/*        The program will use console I/O to display a simple */
/*        ASCII-based progress report. */

/*        The program will find occultations of the Sun by the Moon as */
/*        seen from the center of the Earth over the month December, */
/*        2001. */

/*        We use light time corrections to model apparent positions of */
/*        Sun and Moon. Stellar aberration corrections are not specified */
/*        because they don't affect occultation computations. */

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


/*              PROGRAM EX1 */

/*              IMPLICIT NONE */

/*              EXTERNAL              GFSTEP */
/*              EXTERNAL              GFREFN */
/*              EXTERNAL              GFREPI */
/*              EXTERNAL              GFREPU */
/*              EXTERNAL              GFREPF */

/*              INTEGER               WNCARD */
/*              LOGICAL               GFBAIL */
/*              EXTERNAL              GFBAIL */


/*              CHARACTER*(*)         TIMFMT */
/*              PARAMETER           ( TIMFMT = */
/*             .   'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)' ) */

/*              DOUBLE PRECISION      CNVTOL */
/*              PARAMETER           ( CNVTOL = 1.D-6 ) */

/*              INTEGER               MAXWIN */
/*              PARAMETER           ( MAXWIN = 2 * 100 ) */

/*              INTEGER               TIMLEN */
/*              PARAMETER           ( TIMLEN = 40 ) */

/*              INTEGER               LBCELL */
/*              PARAMETER           ( LBCELL = -5 ) */

/*              CHARACTER*(TIMLEN)    WIN0 */
/*              CHARACTER*(TIMLEN)    WIN1 */
/*              CHARACTER*(TIMLEN)    BEGSTR */
/*              CHARACTER*(TIMLEN)    ENDSTR */

/*              DOUBLE PRECISION      CNFINE ( LBCELL : MAXWIN ) */
/*              DOUBLE PRECISION      ET0 */
/*              DOUBLE PRECISION      ET1 */
/*              DOUBLE PRECISION      LEFT */
/*              DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN ) */
/*              DOUBLE PRECISION      RIGHT */

/*              INTEGER               I */

/*              LOGICAL               BAIL */
/*              LOGICAL               RPT */

/*        C */
/*        C     Load kernels. */
/*        C */
/*              CALL FURNSH ( 'standard.tm' ) */

/*        C */
/*        C     Initialize the confinement and result windows. */
/*        C */
/*              CALL SSIZED ( MAXWIN, CNFINE ) */
/*              CALL SSIZED ( MAXWIN, RESULT ) */

/*        C */
/*        C     Obtain the TDB time bounds of the confinement */
/*        C     window, which is a single interval in this case. */
/*        C */
/*              WIN0 = '2001 DEC 01 00:00:00 TDB' */
/*              WIN1 = '2002 JAN 01 00:00:00 TDB' */

/*              CALL STR2ET ( WIN0, ET0 ) */
/*              CALL STR2ET ( WIN1, ET1 ) */

/*        C */
/*        C     Insert the time bounds into the confinement */
/*        C     window. */
/*        C */
/*              CALL WNINSD ( ET0, ET1, CNFINE ) */

/*        C */
/*        C     Select a 20 second step. We'll ignore any occultations */
/*        C     lasting less than 20 seconds. */
/*        C */
/*              CALL GFSSTP ( 20.D0 ) */

/*        C */
/*        C     Turn on progress reporting; turn off interrupt */
/*        C     handling. */
/*        C */
/*              RPT  = .TRUE. */
/*              BAIL = .FALSE. */

/*        C */
/*        C     Perform the search. */
/*        C */
/*              CALL GFOCCE ( 'ANY', */
/*             .              'MOON',   'ellipsoid',  'IAU_MOON', */
/*             .              'SUN',    'ellipsoid',  'IAU_SUN', */
/*             .              'LT',     'EARTH',      CNVTOL, */
/*             .              GFSTEP,   GFREFN,       RPT, */
/*             .              GFREPI,   GFREPU,       GFREPF, */
/*             .              BAIL,     GFBAIL,       CNFINE,  RESULT ) */


/*              IF ( WNCARD(RESULT) .EQ. 0 ) THEN */

/*                 WRITE (*,*) 'No occultation was found.' */

/*              ELSE */

/*                 DO I = 1, WNCARD(RESULT) */
/*        C */
/*        C           Fetch and display each occultation interval. */
/*        C */
/*                    CALL WNFETD ( RESULT, I, LEFT, RIGHT ) */

/*                    CALL TIMOUT ( LEFT,  TIMFMT, BEGSTR ) */
/*                    CALL TIMOUT ( RIGHT, TIMFMT, ENDSTR ) */

/*                    WRITE (*,*) 'Interval ', I */
/*                    WRITE (*,*) '   Start time: '//BEGSTR */
/*                    WRITE (*,*) '   Stop time:  '//ENDSTR */

/*                 END DO */

/*              END IF */

/*              END */


/*     When this program was executed on a PC/Linux/g77 platform, the */
/*     progress report had the format shown below: */

/*        Occultation/transit search   6.02% done. */

/*     The completion percentage was updated approximately once per */
/*     second. */

/*     When this program completed execution, the output was: */

/*        Occultation/transit search 100.00% done. */
/*         Interval  1 */
/*            Start time: 2001 DEC 14 20:10:14.195952  (TDB) */
/*            Stop time:  2001 DEC 14 21:35:50.317994  (TDB) */


/* $ Restrictions */

/*     1) If the caller passes in the default, constant step */
/*        size routine, GFSTEP, the caller must set the step */
/*        size by calling the entry point GFSSTP before */
/*        calling GFOCCE. The call syntax for GFSSTP is */

/*           CALL GFSSTP ( STEP ) */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     L.S. Elson     (JPL) */
/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */
/*     E.D. Wright    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0 15-APR-2009 (NJB) (LSE) (WLT) (IMU) (EDW) */

/* -& */
/* $ Index_Entries */

/*      GF mid-level occultation search */

/* -& */

/*     SPICELIB functions */


/*     External routines */


/*     Local parameters */


/*     STEP is a step size initializer for the unused, dummy step size */
/*     argument to ZZGFSOLV. The routine UDSTEP, which is passed to */
/*     ZZGFSOLV, will be used by that routine to obtain the step size. */


/*     CSTEP indicates whether a constant step size, provided */
/*     via the input argument STEP, is to be used by ZZGFSOLV. */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("GFOCCE", (ftnlen)6);

/*     Check the result window's size. */

    if (sized_(result) < 2) {
	setmsg_("Result window size must be at least 2 but was #.", (ftnlen)
		48);
	i__1 = sized_(result);
	errint_("#", &i__1, (ftnlen)1);
	sigerr_("SPICE(WINDOWTOOSMALL)", (ftnlen)21);
	chkout_("GFOCCE", (ftnlen)6);
	return 0;
    }

/*     Empty the RESULT window. */

    scardd_(&c__0, result);

/*     Check the convergence tolerance. */

    if (*tol <= 0.) {
	setmsg_("Tolerance must be positive but was #.", (ftnlen)37);
	errdp_("#", tol, (ftnlen)1);
	sigerr_("SPICE(INVALIDTOLERANCE)", (ftnlen)23);
	chkout_("GFOCCE", (ftnlen)6);
	return 0;
    }

/*     Check the target shape specifications. */

    ljust_(bshape, lbshap, bshape_len, (ftnlen)9);
    ucase_(lbshap, lbshap, (ftnlen)9, (ftnlen)9);
    ljust_(fshape, lfshap, fshape_len, (ftnlen)9);
    ucase_(lfshap, lfshap, (ftnlen)9, (ftnlen)9);

/*     Note for maintenance programmer: these checks will */
/*     require modification to handle DSK-based shapes. */

    if (s_cmp(lfshap, "POINT", (ftnlen)9, (ftnlen)5) == 0 && s_cmp(lbshap, 
	    "POINT", (ftnlen)9, (ftnlen)5) == 0) {
	setmsg_("The front and back target shape specifications are both PTS"
		"HAP; at least one of these targets must be an extended objec"
		"t.", (ftnlen)121);
	sigerr_("SPICE(INVALIDSHAPECOMBO)", (ftnlen)24);
	chkout_("GFOCCE", (ftnlen)6);
	return 0;
    }

/*     Initialize the occultation calculation. */

    zzgfocin_(occtyp, front, lfshap, fframe, back, lbshap, bframe, obsrvr, 
	    abcorr, occtyp_len, front_len, (ftnlen)9, fframe_len, back_len, (
	    ftnlen)9, bframe_len, obsrvr_len, abcorr_len);
    if (failed_()) {
	chkout_("GFOCCE", (ftnlen)6);
	return 0;
    }

/*     Prepare the progress reporter if appropriate. */

    if (*rpt) {
	(*udrepi)(cnfine, "Occultation/transit search ", "done.", (ftnlen)27, 
		(ftnlen)5);
    }

/*     Cycle over the intervals in the confining window. */

    count = wncard_(cnfine);
    i__1 = count;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Retrieve the bounds for the Ith interval of the confinement */
/*        window. Search this interval for occultation events. Union the */
/*        result with the contents of the RESULT window. */

	wnfetd_(cnfine, &i__, &start, &finish);
	zzgfsolv_((U_fp)zzgfocst_, (U_fp)udstep, (U_fp)udrefn, bail, (L_fp)
		udbail, &c_false, &c_b21, &start, &finish, tol, rpt, (U_fp)
		udrepu, result);
	if (failed_()) {
	    chkout_("GFOCCE", (ftnlen)6);
	    return 0;
	}
	if (*bail) {

/*           Interrupt handling is enabled. */

	    if ((*udbail)()) {

/*              An interrupt has been issued. Return now regardless of */
/*              whether the search has been completed. */

		chkout_("GFOCCE", (ftnlen)6);
		return 0;
	    }
	}
    }

/*     End the progress report. */

    if (*rpt) {
	(*udrepf)();
    }
    chkout_("GFOCCE", (ftnlen)6);
    return 0;
} /* gfocce_ */

