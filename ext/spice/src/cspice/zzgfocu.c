/* zzgfocu.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__4 = 4;
static integer c__0 = 0;
static integer c__3 = 3;
static doublereal c_b171 = 1e-12;

/* $Procedure ZZGFOCU ( GF, occultation utilities ) */
/* Subroutine */ int zzgfocu_0_(int n__, char *occtyp, char *front, char *
	fshape, char *fframe, char *back, char *bshape, char *bframe, char *
	obsrvr, char *abcorr, doublereal *time, logical *ocstat, ftnlen 
	occtyp_len, ftnlen front_len, ftnlen fshape_len, ftnlen fframe_len, 
	ftnlen back_len, ftnlen bshape_len, ftnlen bframe_len, ftnlen 
	obsrvr_len, ftnlen abcorr_len)
{
    /* Initialized data */

    static doublereal svorig[3] = { 0.,0.,0. };
    static char svtyps[7*4] = "ANNULAR" "ANY    " "PARTIAL" "FULL   ";

    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_rnge(char *, integer, 
	    char *, integer);

    /* Local variables */
    doublereal srad;
    extern /* Subroutine */ int vscl_(doublereal *, doublereal *, doublereal *
	    );
    extern doublereal vsep_(doublereal *, doublereal *);
    extern /* Subroutine */ int vsub_(doublereal *, doublereal *, doublereal *
	    ), zzcorepc_(char *, doublereal *, doublereal *, doublereal *, 
	    ftnlen), zzvalcor_(char *, logical *, ftnlen);
    doublereal t2sep;
    integer i__, n;
    doublereal radii[3];
    extern /* Subroutine */ int minad_(doublereal *, integer *, doublereal *, 
	    integer *), maxad_(doublereal *, integer *, doublereal *, integer 
	    *), chkin_(char *, ftnlen);
    char shape[9];
    integer idobs;
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen), 
	    ucase_(char *, char *, ftnlen, ftnlen);
    doublereal bdist, fdist;
    integer trgid;
    logical found;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen), moved_(
	    doublereal *, integer *, doublereal *);
    doublereal mtemp[9]	/* was [3][3] */, tdist;
    static integer svobs;
    extern doublereal vnorm_(doublereal *);
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen), 
	    bods2c_(char *, integer *, logical *, ftnlen);
    integer idback;
    extern logical failed_(void);
    extern /* Subroutine */ int cleard_(integer *, doublereal *);
    integer occode;
    doublereal ltback;
    extern doublereal dasine_(doublereal *, doublereal *), halfpi_(void);
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    doublereal bckfrt[3], bckobs[3], bckpos[3], etbcor;
    static char svbnam[36];
    extern logical return_(void);
    char fixfrm[32], posnam[10];
    static char svbfrm[32], svbshp[9], svcorr[5], svffrm[32], svfnam[36], 
	    svfshp[9], svonam[36], svtype[7];
    doublereal bsmaxs[9]	/* was [3][3] */, etfcor, frtbck[3], frtobs[3]
	    , frtpos[3], fsmaxs[9]	/* was [3][3] */, ltfrnt, maxang, 
	    minang, spoint[3], srfvec[3];
    static doublereal svbrad[3], svfrad[3], svmnbr, svmnfr, svmxbr, svmxfr;
    doublereal trgepc, trgsep;
    integer center, clssid, ffrmid, frclss, idfrnt, occnum;
    static integer svback, svfrnt;
    logical attblk[15], pntocc;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    integer loc;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), suffix_(char *, 
	    integer *, char *, ftnlen, ftnlen), bodvcd_(integer *, char *, 
	    integer *, integer *, doublereal *, ftnlen), errint_(char *, 
	    integer *, ftnlen), namfrm_(char *, integer *, ftnlen), frinfo_(
	    integer *, integer *, integer *, integer *, logical *), spkezp_(
	    integer *, doublereal *, char *, char *, integer *, doublereal *, 
	    doublereal *, ftnlen, ftnlen), pxform_(char *, char *, doublereal 
	    *, doublereal *, ftnlen, ftnlen), vminus_(doublereal *, 
	    doublereal *), sincpt_(char *, char *, doublereal *, char *, char 
	    *, char *, char *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, logical *, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, 
	    ftnlen);
    extern integer zzocced_(doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     This routine contains the entry points that produce the */
/*     computations needed for solving for occultation states */
/*     in the geometry finding routines. */

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
/*     NAIF_IDS */
/*     PCK */
/*     SPK */
/*     TIME */

/* $ Keywords */

/*     SEARCH */
/*     GEOMETRY */
/*     OCCULTATION */

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

/*     Declare ZZOCCED return code parameters, comparison strings */
/*     and other parameters. */

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

/*     ELLIPSOID */
/*     GEOMETRY */
/*     GF */
/*     OCCULTATION */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 01-SEP-2005 (NJB) */

/* -& */
/*     The function returns an integer code indicating the geometric */
/*     relationship of the three bodies. */

/*     Codes and meanings are: */

/*        -3                    Total occultation of first target by */
/*                              second. */


/*        -2                    Annular occultation of first target by */
/*                              second.  The second target does not */
/*                              block the limb of the first. */


/*        -1                    Partial occultation of first target by */
/*                              second target. */


/*         0                    No occultation or transit:  both objects */
/*                              are completely visible to the observer. */


/*         1                    Partial occultation of second target by */
/*                              first target. */


/*         2                    Annular occultation of second target by */
/*                              first. */


/*         3                    Total occultation of second target by */
/*                              first. */


/*     End include file zzocced.inc */

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

/*     VARIABLE  I/O  Entry points */
/*     --------  ---  -------------------------------------------------- */
/*     OCCTYP     I   ZZGFOCIN */
/*     FRONT      I   ZZGFOCIN */
/*     FSHAPE     I   ZZGFOCIN */
/*     FFRAME     I   ZZGFOCIN */
/*     BACK       I   ZZGFOCIN */
/*     BSHAPE     I   ZZGFOCIN */
/*     BFRAME     I   ZZGFOCIN */
/*     OBSRVR     I   ZZGFOCIN */
/*     ABCORR     I   ZZGFOCIN */
/*     TIME       I   ZZGFOCST */
/*     OCSTAT     O   ZZGFOCST */

/* $ Detailed_Input */

/*     See entry points. */

/* $ Detailed_Output */

/*     See entry points. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     See entry points. */

/* $ Files */

/*     Appropriate SPK and PCK kernels must be loaded by the calling */
/*     program before the entry points of this routine are called. */

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

/*     In all cases, kernel data are normally loaded once per program */
/*     run, NOT every time the entry points of this routine are called. */

/* $ Particulars */

/*     This routine is designed to determine whether a specified */
/*     type of occultation or transit is in progress at a specified */
/*     epoch. Two methods of modeling the shapes of the target */
/*     bodies are supported: */

/*        1)  Model both target bodies as triaxial ellipsoids. For this */
/*            case, the user may choose between occultations that are */
/*            partial, full or annular. See the entry header for */
/*            ZZGFOCIN for an explanation of these terms. */

/*        2)  Treat one target body as a point object and the other */
/*            target body is a triaxial ellipsoid. The only supported */
/*            occultation type is "ANY" for this case. */

/*     This routine contains two entry points that support searches */
/*     for occultations performed using ZZGFSOLV: */

/*        ZZGFOCIN   Saves the user-supplied inputs defining the */
/*                   occultation computation to be performed. */
/*                   Initializes the occultation search. */

/*        ZZGFOCST   Returns the occultation state for a specified */
/*                   time. */

/* $ Examples */

/*     See GFOCCE. */

/* $ Restrictions */

/*     This is a SPICELIB private routine; it should not be called by */
/*     user applications. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     L.S. Elson     (JPL) */
/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */
/*     E.D. Wright    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 18-MAY-2014 (NJB) */

/*        Bug fix: in entry point ZZGFOCIN, corrected long error message */
/*        for the case in which a body-fixed reference frame is not */
/*        centered on the correct body. */

/* -    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB) (LSE) (WLT) (IMU) (EDW) */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     ALPHA is a bound for the fraction of the speed of light */
/*     at which target body may move, relative to the solar */
/*     system barycenter. */


/*     ATOL is a tolerance value for computing arc sine. */


/*     Local variables */


/*     Saved variables */


/*     Initial values */

    switch(n__) {
	case 1: goto L_zzgfocin;
	case 2: goto L_zzgfocst;
	}


/*     Below we initialize the list of occultation types. */


/*     This routine should never be called directly. */

    chkin_("ZZGFOCU", (ftnlen)7);
    sigerr_("SPICE(BOGUSENTRY)", (ftnlen)17);
    chkout_("ZZGFOCU", (ftnlen)7);
    return 0;
/* $Procedure  ZZGFOCIN ( GF, occultation initialization ) */

L_zzgfocin:
/* $ Abstract */

/*    Perform initialization functions for occultation state */
/*    determination. */

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

/*     NAIF_IDS */
/*     FRAMES */
/*     PCK */
/*     SPK */
/*     TIME */

/* $ Keywords */

/*     SEARCH */
/*     GEOMETRY */
/*     OCCULTATION */

/* $ Declarations */

/*     CHARACTER*(*)         OCCTYP */
/*     CHARACTER*(*)         FRONT */
/*     CHARACTER*(*)         FSHAPE */
/*     CHARACTER*(*)         FFRAME */
/*     CHARACTER*(*)         BACK */
/*     CHARACTER*(*)         BSHAPE */
/*     CHARACTER*(*)         BFRAME */
/*     CHARACTER*(*)         OBSRVR */
/*     CHARACTER*(*)         ABCORR */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     OCCTYP     I   Type of occultation. */
/*     FRONT      I   Name of body occulting the other. */
/*     FSHAPE     I   Type of shape model used for front body. */
/*     FFRAME     I   Body-fixed, body-centered frame for front body. */
/*     BACK       I   Name of body occulted by the other. */
/*     BSHAPE     I   Type of shape model used for back body. */
/*     BFRAME     I   Body-fixed, body-centered frame for back body. */
/*     OBSRVR     I   Name of the observing body. */
/*     ABCORR     I   Aberration correction flag. */

/* $ Detailed_Input */


/*     OCCTYP     indicates the type of occultation that is to be found. */
/*                The full set of possible values of OCCTYP may be used */
/*                when both target bodies are modeled as ellipsoids. */
/*                When either target is modeled as a point, OCCTYP must */
/*                be set to 'ANY' (see description below). */

/*                Supported values of OCCTYP and corresponding */
/*                definitions are: */

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

/*                                        The option 'ANY' MUST be used */
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
/*                by BACK. See the description of FSHAPE above for */
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


/*     OBSRVR     is the name of the body from which the occultation is */
/*                observed. Optionally, you may supply the integer NAIF */
/*                ID code for the body as a string. */

/*                Case and leading or trailing blanks are not */
/*                significant in the string OBSRVR. */


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

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If name of either target or the observer cannot be translated */
/*         to a NAIF ID code, the error SPICE(IDCODENOTFOUND) is */
/*         signaled. */

/*     2)  If either of the target bodies FRONT or BACK coincides with */
/*         the observer body OBSRVR, or if the targets coincide, */
/*         the error SPICE(BODIESNOTDISTINCT) will be signaled. */

/*     3)  If either of the body model specifiers FSHAPE or BSHAPE */
/*         is not recognized, the error SPICE(INVALIDSHAPE) will be */
/*         signaled. */

/*     4)  If both of the body model specifiers FSHAPE and BSHAPE */
/*         specify point targets, the error SPICE(INVALIDSHAPECOMBO) */
/*         will be signaled. */

/*     5)  If an unrecognized value of OCCTYP is seen, the error */
/*         SPICE(INVALIDOCCTYPE) is signaled. */

/*     6)  If one target body is modeled as a point and OCCTYP is not */
/*         set to 'ANY', the error SPICE(BADTYPESHAPECOMBO) is signaled. */

/*     7)  If a target indicated to be an ellipsoid by its shape */
/*         specification argument does not have three associated */
/*         positive radii, the error SPICE(DEGENERATECASE) will be */
/*         signaled. */

/*     8)  If the number of radii associated with a target body is */
/*         not three, the error SPICE(BADRADIUSCOUNT) will be */
/*         signaled. */

/*     9)  If a target body-fixed reference frame associated with a */
/*         non-point target is not recognized, the error */
/*         SPICE(INVALIDFRAME) will be signaled. */

/*     10) If a target body-fixed reference frame is not centered at */
/*         the corresponding target body, the error */
/*         SPICE(INVALIDFRAME) will be signaled. */

/*     11) If the aberration correction string is invalid, the error */
/*         will be diagnosed by a routine in the call tree of this */
/*         routine. */

/* $ Files */

/*     See the header of the umbrella routine ZZGFOCU. */

/* $ Particulars */

/*     This entry point initializes the parameters needed by the */
/*     occultation state determination entry point ZZGFOCST. */

/* $ Examples */

/*     See implementation of GFOCCE. */

/* $ Restrictions */

/*     This is a SPICELIB private routine; it should not be called by */
/*     user applications. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     L.S. Elson     (JPL) */
/*     W.L. Taber     (JPL) */
/*     N.J. Bachman   (JPL) */
/*     E.D. Wright    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 18-MAY-2014 (NJB) */

/*        Bug fix: corrected long error message for the case in which a */
/*        body-fixed reference frame is not centered on the correct */
/*        body. */

/* -    SPICELIB Version 1.0.0, 15-APR-2009 (LSE) (WLT) (NJB) (EDW) */

/* -& */
/* $ Revisions */

/*     None. */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZGFOCIN", (ftnlen)8);

/*     Find NAIF IDs for FRONT, BACK, and OBSRVR. */

    bods2c_(front, &idfrnt, &found, front_len);
    if (! found) {
	setmsg_("The front target object, '#', is not a recognized name for "
		"an ephemeris object. The cause of this problem may be that y"
		"ou need an updated version of the SPICE toolkit. ", (ftnlen)
		168);
	errch_("#", front, (ftnlen)1, front_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("ZZGFOCIN", (ftnlen)8);
	return 0;
    }
    bods2c_(back, &idback, &found, back_len);
    if (! found) {
	setmsg_("The back target object, '#', is not a recognized name for a"
		"n ephemeris object. The cause of this problem may be that yo"
		"u need an updated version of the SPICE toolkit. ", (ftnlen)
		167);
	errch_("#", back, (ftnlen)1, back_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("ZZGFOCIN", (ftnlen)8);
	return 0;
    }
    bods2c_(obsrvr, &idobs, &found, obsrvr_len);
    if (! found) {
	setmsg_("The observer, '#', is not a recognized name for an ephemeri"
		"s object. The cause of this problem may be that you need an "
		"updated version of the SPICE toolkit. ", (ftnlen)157);
	errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	sigerr_("SPICE(IDCODENOTFOUND)", (ftnlen)21);
	chkout_("ZZGFOCIN", (ftnlen)8);
	return 0;
    }

/*     Make sure the observer and both targets are distinct. */

    if (idfrnt == idback || idfrnt == idobs || idback == idobs) {
	setmsg_("The observer and both targets must be distinct objects, but"
		" are not: OBSRVR = #; FRONT = #; BACK = #.", (ftnlen)101);
	errch_("#", obsrvr, (ftnlen)1, obsrvr_len);
	errch_("#", front, (ftnlen)1, front_len);
	errch_("#", back, (ftnlen)1, back_len);
	sigerr_("SPICE(BODIESNOTDISTINCT)", (ftnlen)24);
	chkout_("ZZGFOCIN", (ftnlen)8);
	return 0;
    }

/*     Save the objects' names. We'll need these if */
/*     we need to call SINCPT. */

    s_copy(svfnam, front, (ftnlen)36, front_len);
    s_copy(svbnam, back, (ftnlen)36, back_len);
    s_copy(svonam, obsrvr, (ftnlen)36, obsrvr_len);

/*     Store the ID codes, shape specifications, and body-fixed, */
/*     body-centered frame names of the objects involved in this event. */

    svfrnt = idfrnt;
    s_copy(svffrm, fframe, (ftnlen)32, fframe_len);
    ljust_(fshape, svfshp, fshape_len, (ftnlen)9);
    ucase_(svfshp, svfshp, (ftnlen)9, (ftnlen)9);
    svback = idback;
    s_copy(svbfrm, bframe, (ftnlen)32, bframe_len);
    ljust_(bshape, svbshp, bshape_len, (ftnlen)9);
    ucase_(svbshp, svbshp, (ftnlen)9, (ftnlen)9);
    svobs = idobs;

/*     Note for maintenance programmer: these checks will */
/*     require modification to handle DSK-based shapes. */

    if (s_cmp(svfshp, "POINT", (ftnlen)9, (ftnlen)5) != 0 && s_cmp(svfshp, 
	    "ELLIPSOID", (ftnlen)9, (ftnlen)9) != 0) {
	setmsg_("The front target shape specification, '#', is not a recogni"
		"zed.", (ftnlen)63);
	errch_("#", fshape, (ftnlen)1, fshape_len);
	sigerr_("SPICE(INVALIDSHAPE)", (ftnlen)19);
	chkout_("ZZGFOCIN", (ftnlen)8);
	return 0;
    }
    if (s_cmp(svbshp, "POINT", (ftnlen)9, (ftnlen)5) != 0 && s_cmp(svbshp, 
	    "ELLIPSOID", (ftnlen)9, (ftnlen)9) != 0) {
	setmsg_("The back target shape specification, '#', is not a recogniz"
		"ed.", (ftnlen)62);
	errch_("#", bshape, (ftnlen)1, bshape_len);
	sigerr_("SPICE(INVALIDSHAPE)", (ftnlen)19);
	chkout_("ZZGFOCIN", (ftnlen)8);
	return 0;
    }
    if (s_cmp(svfshp, "POINT", (ftnlen)9, (ftnlen)5) == 0 && s_cmp(svbshp, 
	    "POINT", (ftnlen)9, (ftnlen)5) == 0) {
	setmsg_("The front and back target shape specifications are both PTS"
		"HAP; at least one of these targets must be an extended objec"
		"t.", (ftnlen)121);
	sigerr_("SPICE(INVALIDSHAPECOMBO)", (ftnlen)24);
	chkout_("ZZGFOCIN", (ftnlen)8);
	return 0;
    }

/*     Save a single upper-case character representing the occultation */
/*     type string. */

    ljust_(occtyp, svtype, occtyp_len, (ftnlen)7);
    ucase_(svtype, svtype, (ftnlen)7, (ftnlen)7);

/*     Check the occultation type. */

    occnum = isrchc_(svtype, &c__4, svtyps, (ftnlen)7, (ftnlen)7);
    if (occnum == 0) {
	setmsg_("The occultation type # is not recognized.  Supported types "
		"are: #, #, #,  #.", (ftnlen)76);
	errch_("#", occtyp, (ftnlen)1, occtyp_len);
	for (i__ = 1; i__ <= 4; ++i__) {
	    errch_("#", svtyps + ((i__1 = i__ - 1) < 4 && 0 <= i__1 ? i__1 : 
		    s_rnge("svtyps", i__1, "zzgfocu_", (ftnlen)877)) * 7, (
		    ftnlen)1, (ftnlen)7);
	}
	sigerr_("SPICE(INVALIDOCCTYPE)", (ftnlen)21);
	chkout_("ZZGFOCIN", (ftnlen)8);
	return 0;
    }

/*     If we have a point target, the occultation type must */
/*     be 'ANY'. */

    if (s_cmp(svfshp, "POINT", (ftnlen)9, (ftnlen)5) == 0 || s_cmp(svbshp, 
	    "POINT", (ftnlen)9, (ftnlen)5) == 0) {
	if (s_cmp(svtype, "ANY", (ftnlen)7, (ftnlen)3) != 0) {
	    setmsg_("Occultation type # is not allowed when either target bo"
		    "dy is modeled as a point. Set OCCTYP to ANY for use with"
		    " point targets.", (ftnlen)126);
	    errch_("#", occtyp, (ftnlen)1, occtyp_len);
	    sigerr_("SPICE(BADTYPESHAPECOMBO)", (ftnlen)24);
	    chkout_("ZZGFOCIN", (ftnlen)8);
	    return 0;
	}
    }

/*     Check the aberration correction. If SPKEZR can't handle it, */
/*     neither can we. */

    zzvalcor_(abcorr, attblk, abcorr_len);
    if (failed_()) {
	chkout_("ZZGFOCIN", (ftnlen)8);
	return 0;
    }

/*     Create a local aberration correction string without */
/*     a stellar aberration correction specifier. */

    if (attblk[0]) {
	s_copy(svcorr, "NONE", (ftnlen)5, (ftnlen)4);
    } else {

/*        The correction string specified either Newtonian or converged */
/*        light time correction. */

	if (attblk[4]) {
	    s_copy(svcorr, "X", (ftnlen)5, (ftnlen)1);
	} else {
	    s_copy(svcorr, " ", (ftnlen)5, (ftnlen)1);
	}
	if (attblk[3]) {
	    suffix_("CN", &c__0, svcorr, (ftnlen)2, (ftnlen)5);
	} else {
	    suffix_("LT", &c__0, svcorr, (ftnlen)2, (ftnlen)5);
	}
    }

/*     Check the front and back targets' shapes, frames */
/*     and radii. */

    for (i__ = 1; i__ <= 2; ++i__) {
	if (i__ == 1) {
	    s_copy(posnam, "front", (ftnlen)10, (ftnlen)5);
	    s_copy(fixfrm, fframe, (ftnlen)32, fframe_len);
	    trgid = idfrnt;
	    s_copy(shape, svfshp, (ftnlen)9, (ftnlen)9);
	} else {
	    s_copy(posnam, "back", (ftnlen)10, (ftnlen)4);
	    s_copy(fixfrm, bframe, (ftnlen)32, bframe_len);
	    trgid = idback;
	    s_copy(shape, svbshp, (ftnlen)9, (ftnlen)9);
	}
	if (s_cmp(shape, "ELLIPSOID", (ftnlen)9, (ftnlen)9) == 0) {

/*           Fetch and check the radii. */

	    bodvcd_(&trgid, "RADII", &c__3, &n, radii, (ftnlen)5);

/*           Check the count of the radii. */

	    if (n != 3) {
		setmsg_("Target # should have 3 radii but actually has #. Th"
			"is may be due to an error in a PCK file used to prov"
			"ide the radii.", (ftnlen)117);
		errch_("#", posnam, (ftnlen)1, (ftnlen)10);
		errint_("#", &n, (ftnlen)1);
		sigerr_("SPICE(BADRADIUSCOUNT)", (ftnlen)21);
		chkout_("ZZGFOCIN", (ftnlen)8);
		return 0;
	    }

/*           Check to make sure the current target has 3 positive */
/*           semi-axis lengths. */

	    if (radii[0] <= 0. || radii[1] <= 0. || radii[2] <= 0.) {
		setmsg_("One or more semi-axis lengths of the # target body "
			"are non-positive: 1 = #, 2 = #, 3 = #. ", (ftnlen)90);
		errch_("#", posnam, (ftnlen)1, (ftnlen)10);
		errdp_("#", radii, (ftnlen)1);
		errdp_("#", &radii[1], (ftnlen)1);
		errdp_("#", &radii[2], (ftnlen)1);
		sigerr_("SPICE(DEGENERATECASE)", (ftnlen)21);
		chkout_("ZZGFOCIN", (ftnlen)8);
		return 0;
	    }

/*           Checks of radii have been completed. */

	    if (i__ == 1) {
		moved_(radii, &c__3, svfrad);

/*              Select smallest and largest semi-axis lengths of body */
/*              for later tests. */

		minad_(svfrad, &c__3, &svmnfr, &loc);
		maxad_(svfrad, &c__3, &svmxfr, &loc);
	    } else {
		moved_(radii, &c__3, svbrad);
		minad_(svbrad, &c__3, &svmnbr, &loc);
		maxad_(svbrad, &c__3, &svmxbr, &loc);
	    }
	    if (failed_()) {
		chkout_("ZZGFOCIN", (ftnlen)8);
		return 0;
	    }

/*           The target is ellipsoidal; there must be */
/*           a target body-fixed frame associated with this */
/*           body. */

	    if (s_cmp(fixfrm, " ", (ftnlen)32, (ftnlen)1) == 0) {
		setmsg_("The # target is modeled as an ellipsoid, but the as"
			"sociated body-fixed frame name is blank.", (ftnlen)91)
			;
		sigerr_("SPICE(INVALIDFRAME)", (ftnlen)19);
		errch_("#", posnam, (ftnlen)1, (ftnlen)10);
		chkout_("ZZGFOCIN", (ftnlen)8);
		return 0;
	    } else {

/*              Look up the target's body-fixed frame ID code. */

		namfrm_(fixfrm, &ffrmid, (ftnlen)32);
		if (ffrmid == 0) {
		    setmsg_("The # target's body-fixed frame name # is not r"
			    "ecognized.", (ftnlen)57);
		    errch_("#", posnam, (ftnlen)1, (ftnlen)10);
		    errch_("#", fixfrm, (ftnlen)1, (ftnlen)32);
		    sigerr_("SPICE(INVALIDFRAME)", (ftnlen)19);
		    chkout_("ZZGFOCIN", (ftnlen)8);
		    return 0;
		}

/*              Obtain the center of the frame and verify it's the */
/*              Ith target. */

		frinfo_(&ffrmid, &center, &frclss, &clssid, &found);
		if (! found) {

/*                 Since we mapped the frame name to an ID code, we */
/*                 expect to find the frame info. So control should */
/*                 never reach this point. */

		    setmsg_("Frame ID found for # body-fixed frame # but FRI"
			    "NFO couldn't find frame info.", (ftnlen)76);
		    errch_("#", posnam, (ftnlen)1, (ftnlen)10);
		    errch_("#", fixfrm, (ftnlen)1, (ftnlen)32);
		    sigerr_("SPICE(BUG)", (ftnlen)10);
		    chkout_("ZZGFOCIN", (ftnlen)8);
		    return 0;
		}
		if (center != trgid) {

/*                 The body-fixed frame for the current target */
/*                 isn't actually centered on the body. */

		    setmsg_("Supposed body-fixed frame # for # target is act"
			    "ually centered on body #.", (ftnlen)72);
		    errch_("#", fixfrm, (ftnlen)1, (ftnlen)32);
		    errch_("#", posnam, (ftnlen)1, (ftnlen)10);
		    errint_("#", &center, (ftnlen)1);
		    sigerr_("SPICE(INVALIDFRAME)", (ftnlen)19);
		    chkout_("ZZGFOCIN", (ftnlen)8);
		    return 0;
		}
	    }

/*           We've performed radii and frame checks for an ellipsoidal */
/*           target. */

	} else if (s_cmp(shape, "POINT", (ftnlen)9, (ftnlen)5) == 0) {

/*           Zero out radius values for this target; set the */
/*           frame to blank. */

	    if (i__ == 1) {
		cleard_(&c__3, svfrad);
		svmnfr = 0.;
		svmxfr = 0.;
		s_copy(svffrm, " ", (ftnlen)32, (ftnlen)1);
	    } else {
		cleard_(&c__3, svbrad);
		svmnbr = 0.;
		svmxbr = 0.;
		s_copy(svbfrm, " ", (ftnlen)32, (ftnlen)1);
	    }
	} else {

/*           We have an unsupported target shape. */

	    setmsg_("The # target body has shape #; the only supported shape"
		    "s are ELLIPSOID and POINT.", (ftnlen)81);
	    errch_("#", posnam, (ftnlen)1, (ftnlen)10);
	    errch_("#", shape, (ftnlen)1, (ftnlen)9);
	    sigerr_("SPICE(INVALIDSHAPE)", (ftnlen)19);
	    chkout_("ZZGFOCIN", (ftnlen)8);
	    return 0;
	}

/*        We've performed shape, and if applicable, frame and radii */
/*        checks for the Ith target. */

    }

/*     We've performed shape, and if applicable, frame and radii */
/*     checks for both targets. */

    chkout_("ZZGFOCIN", (ftnlen)8);
    return 0;
/* $Procedure ZZGFOCST ( GF, "in occultation?"  ) */

L_zzgfocst:
/* $ Abstract */

/*     See if the object is currently occulted. */

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
/*     TIME */

/* $ Keywords */

/*     SEARCH */
/*     GEOMETRY */
/*     OCCULTATION */

/* $ Declarations */

/*     DOUBLE PRECISION      TIME */
/*     LOGICAL               OCSTAT */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     TIME       I   TDB epoch (in seconds past J2000) */
/*     OCSTAT     O   .TRUE. if the object is occulted, .FALSE. */
/*                    otherwise. */

/* $ Detailed_Input */

/*     TIME       is the epoch of interest in TDB seconds past the */
/*                J2000 epoch. */

/* $ Detailed_Output */

/*     OCSTAT     is a logical flag indicating the state of */
/*                occultation. If the configuration initialized by */
/*                ZZGFOCIN is in occultation at the epoch TIME, OCSTAT is */
/*                returned with the value .TRUE. Otherwise it is */
/*                returned with the value .FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If any SPK lookup fails, the error will be diagnosed by */
/*        routines in the call tree of this routine. */

/*     2) If any frame transformation lookup fails, the error will be */
/*        diagnosed by routines in the call tree of this routine. */

/*     3) If any occultation computation is done for ellipsoidal */
/*        targets, and if either semi-axis matrix is invalid, the error */
/*        will be diagnosed by routines in the call tree of this */
/*        routine. */

/*     4) If any two of the bodies defining the occultation geometry */
/*        intersect, either error SPICE(NOTDISJOINT) will be */
/*        signaled by this routine, or the error will be diagnosed by */
/*        routines in the call tree of this routine. */

/*     5)  If the body model specifiers FSHAPE and BSHAPE don't specify */
/*         either two ellipsoidal targets or one ellipsoidal target and */
/*         one point target, the error SPICE(INVALIDSHAPECOMBO) */
/*         will be signaled. */

/* $ Files */

/*     See the Files header section of the umbrella routine ZZGFOCU. */

/* $ Particulars */

/*     This routine determines the occultation state of the */
/*     configuration specified by the last call to ZZGFOCIN and the */
/*     input time value. */

/* $ Examples */

/*     See the umbrella routine ZZGFOCU. */

/* $ Restrictions */

/*     This is a SPICELIB private routine; it should not be called by */
/*     user applications. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     L.S. Elson     (JPL) */
/*     W.L. Taber     (JPL) */
/*     E.D. Wright    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 30-DEC-2008 (NJB) (LSE) (WLT) (EDW) */

/* -& */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("ZZGFOCST", (ftnlen)8);

/*     Initialize the state output. */

    *ocstat = FALSE_;

/*     Get the apparent positions of FRONT and BACK as seen from the */
/*     observer. */

    spkezp_(&svfrnt, time, "J2000", svcorr, &svobs, frtpos, &ltfrnt, (ftnlen)
	    5, (ftnlen)5);
    spkezp_(&svback, time, "J2000", svcorr, &svobs, bckpos, &ltback, (ftnlen)
	    5, (ftnlen)5);
    if (failed_()) {
	chkout_("ZZGFOCST", (ftnlen)8);
	return 0;
    }

/*     Handle the cases of one and two extended targets */
/*     separately. */

    if (s_cmp(svbshp, "ELLIPSOID", (ftnlen)9, (ftnlen)9) == 0 && s_cmp(svfshp,
	     "ELLIPSOID", (ftnlen)9, (ftnlen)9) == 0) {

/*        The caller has selected a test for a partial, annular or full */
/*        occultation using ellipsoidal shape models. */

/*        Look up the axes of each target body in the J2000 frame at the */
/*        light time corrected epoch for that body. */

	zzcorepc_(svcorr, time, &ltback, &etbcor, (ftnlen)5);
	pxform_(svbfrm, "J2000", &etbcor, mtemp, (ftnlen)32, (ftnlen)5);
	if (failed_()) {
	    chkout_("ZZGFOCST", (ftnlen)8);
	    return 0;
	}

/*        Scale the columns of MTEMP by the axis lengths of the back */
/*        target. */

	for (i__ = 1; i__ <= 3; ++i__) {
	    vscl_(&svbrad[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
		    "svbrad", i__1, "zzgfocu_", (ftnlen)1358)], &mtemp[(i__2 =
		     i__ * 3 - 3) < 9 && 0 <= i__2 ? i__2 : s_rnge("mtemp", 
		    i__2, "zzgfocu_", (ftnlen)1358)], &bsmaxs[(i__3 = i__ * 3 
		    - 3) < 9 && 0 <= i__3 ? i__3 : s_rnge("bsmaxs", i__3, 
		    "zzgfocu_", (ftnlen)1358)]);
	}
	zzcorepc_(svcorr, time, &ltfrnt, &etfcor, (ftnlen)5);
	pxform_(svffrm, "J2000", &etfcor, mtemp, (ftnlen)32, (ftnlen)5);
	if (failed_()) {
	    chkout_("ZZGFOCST", (ftnlen)8);
	    return 0;
	}

/*        Scale the columns of MTEMP by the axis lengths of the second */
/*        target. */

	for (i__ = 1; i__ <= 3; ++i__) {
	    vscl_(&svfrad[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge(
		    "svfrad", i__1, "zzgfocu_", (ftnlen)1374)], &mtemp[(i__2 =
		     i__ * 3 - 3) < 9 && 0 <= i__2 ? i__2 : s_rnge("mtemp", 
		    i__2, "zzgfocu_", (ftnlen)1374)], &fsmaxs[(i__3 = i__ * 3 
		    - 3) < 9 && 0 <= i__3 ? i__3 : s_rnge("fsmaxs", i__3, 
		    "zzgfocu_", (ftnlen)1374)]);
	}

/*        Classify the occultation state of BACK by FRONT as seen from */
/*        the observer. */

	occode = zzocced_(svorig, bckpos, bsmaxs, frtpos, fsmaxs);
	if (failed_()) {
	    chkout_("ZZGFOCST", (ftnlen)8);
	    return 0;
	}
	if (occode == 0) {

/*           Neither body occults the other. */

	    *ocstat = FALSE_;
	} else if (s_cmp(svtype, "ANY", (ftnlen)7, (ftnlen)3) == 0 && occode <
		 0) {

/*           The "of" body (target 1) is at least partially occulted by */
/*           the BY object. */

	    *ocstat = TRUE_;
	} else if (s_cmp(svtype, "FULL", (ftnlen)7, (ftnlen)4) == 0 && occode 
		== -3) {

/*           The BACK body is in total occultation. */

	    *ocstat = TRUE_;
	} else if (s_cmp(svtype, "ANNULAR", (ftnlen)7, (ftnlen)7) == 0 && 
		occode == -2) {

/*           The  BACK body is in annular occultation. */

	    *ocstat = TRUE_;
	} else if (s_cmp(svtype, "PARTIAL", (ftnlen)7, (ftnlen)7) == 0 && 
		occode == -1) {

/*           The BACK body is partially occulted. */

	    *ocstat = TRUE_;
	} else {

/*           The occultation state doesn't match the requested state. */

	    *ocstat = FALSE_;
	}
	chkout_("ZZGFOCST", (ftnlen)8);
	return 0;
    } else if (s_cmp(svfshp, "ELLIPSOID", (ftnlen)9, (ftnlen)9) == 0 && s_cmp(
	    svbshp, "POINT", (ftnlen)9, (ftnlen)5) == 0 || s_cmp(svfshp, 
	    "POINT", (ftnlen)9, (ftnlen)5) == 0 && s_cmp(svbshp, "ELLIPSOID", 
	    (ftnlen)9, (ftnlen)9) == 0) {

/*        One of the targets is modeled as a point; the other is */
/*        modeled as an ellipsoid. */

/*        If the front target is an ellipsoid and the back target */
/*        is a point, we'll classify the geometry as a "point */
/*        occultation." Otherwise we have a "point transit" case. */
/*        We'll set the logical flag PNTOCC to .TRUE. to indicate */
/*        a point occultation. */

	pntocc = s_cmp(svbshp, "POINT", (ftnlen)9, (ftnlen)5) == 0;

/*        We're going to start out by doing some error checking. */
/*        We're looking for intersections of the participating */
/*        objects: these should never occur. */

/*        Let BDIST, FDIST be the distances from the observer */
/*        to the back and front targets, respectively. */

	bdist = vnorm_(bckpos);
	fdist = vnorm_(frtpos);

/*        Find the vector from BACK to FRONT.  We'll use this later, */
/*        but we want it now in order to make sure that BACK doesn't */
/*        intersect FRONT. */

	vsub_(frtpos, bckpos, bckfrt);
	if (pntocc) {

/*           The front target is an ellipsoid. */

	    if (fdist <= svmnfr) {

/*              The observer is INSIDE the front target. We */
/*              treat this as an error. */

		setmsg_("Observer is inside front target body.", (ftnlen)37);
		sigerr_("SPICE(NOTDISJOINT)", (ftnlen)18);
		chkout_("ZZGFOCST", (ftnlen)8);
		return 0;
	    } else if (bdist == 0.) {
		setmsg_("Back target coincides with observer.", (ftnlen)36);
		sigerr_("SPICE(NOTDISJOINT)", (ftnlen)18);
		chkout_("ZZGFOCST", (ftnlen)8);
		return 0;
	    } else if (vnorm_(bckfrt) <= svmnfr) {
		setmsg_("BACK target is inside FRONT target.", (ftnlen)35);
		sigerr_("SPICE(NOTDISJOINT)", (ftnlen)18);
		chkout_("ZZGFOCST", (ftnlen)8);
		return 0;
	    }
	} else {

/*           The back target is an ellipsoid. */

	    if (bdist <= svmnbr) {

/*              The observer is INSIDE the back target. We */
/*              treat this as an error. */

		setmsg_("Observer is inside back target body.", (ftnlen)36);
		sigerr_("SPICE(NOTDISJOINT)", (ftnlen)18);
		chkout_("ZZGFOCST", (ftnlen)8);
		return 0;
	    } else if (fdist == 0.) {
		setmsg_("Front target coincides with observer.", (ftnlen)37);
		sigerr_("SPICE(NOTDISJOINT)", (ftnlen)18);
		chkout_("ZZGFOCST", (ftnlen)8);
		return 0;
	    } else if (vnorm_(bckfrt) <= svmnbr) {
		setmsg_("FRONT target is inside BACK target.", (ftnlen)35);
		sigerr_("SPICE(NOTDISJOINT)", (ftnlen)18);
		chkout_("ZZGFOCST", (ftnlen)8);
		return 0;
	    }
	}

/*        Find angular separation of the target centers as */
/*        seen by the observer. */

	trgsep = vsep_(bckpos, frtpos);

/*        Find angular radius of the outer bounding sphere of the */
/*        ellipsoid, as seen by the observer. */

/*        In computing this angular radius, scale up the bounding */
/*        sphere to compensate for the light time error we've made */
/*        by computing light time to the target's center. The */
/*        correct value to use is light time to the limb point having */
/*        minimum angular separation from the point target. */

/*        Presuming the ellipsoidal target can move no faster than */
/*        alpha*c (where c represents the speed of light in a vacuum), */
/*        and considering the fact that the light time error cannot */
/*        exceed r/c, where r is the radius of the outer bounding sphere */
/*        of the ellipsoid, we find that the magnitude of the position */
/*        error of the ellipsoid cannot exceed alpha*r. Then the */
/*        correctly positioned ellipsoid---that is, located at */
/*        the position corresponding to the correct light time */
/*        correction---must be contained in the outer bounding */
/*        sphere we've found, if we scale the sphere up by 1+alpha. */

/*        Perform the test only if the observer is outside the */
/*        outer bounding sphere of the ellipsoidal target. */

	if (pntocc) {
	    srad = svmxfr * 1.01;
	    tdist = fdist;
	} else {
	    srad = svmxbr * 1.01;
	    tdist = bdist;
	}
	if (srad < tdist) {
	    d__1 = srad / tdist;
	    maxang = dasine_(&d__1, &c_b171);
	    if (trgsep > maxang) {

/*              No occultation is possible. */

		*ocstat = FALSE_;
		chkout_("ZZGFOCST", (ftnlen)8);
		return 0;
	    }
	}
	if (failed_()) {
	    chkout_("ZZGFOCST", (ftnlen)8);
	    return 0;
	}

/*        We'll need the negatives of the observer-target vectors in */
/*        several places later, so compute them now. */

	vminus_(frtpos, frtobs);
	vminus_(bckpos, bckobs);

/*        Now check for an occulted state assuming a spherical extended */
/*        body with radius equal to the minimum semi-axis. Again, */
/*        adjust the sphere for our light time error. */

	if (pntocc) {
	    d__1 = svmnfr * .98999999999999999 / fdist;
	    minang = dasine_(&d__1, &c_b171);
	} else {
	    d__1 = svmnbr * .98999999999999999 / bdist;
	    minang = dasine_(&d__1, &c_b171);
	}
	if (failed_()) {
	    chkout_("ZZGFOCST", (ftnlen)8);
	    return 0;
	}
	if (trgsep < minang) {

/*           The targets must overlap as seen from the observer. */

	    if (pntocc) {

/*              Examine the angle between the vector from FRONT to the */
/*              observer and the vector from FRONT to BACK.  If that */
/*              angle is greater than or equal to the complement of the */
/*              angular radius of FRONT, then FRONT occults BACK. First */
/*              find the position of FRONT and BACK relative to each */
/*              other. */

		vminus_(bckfrt, frtbck);
		t2sep = vsep_(frtobs, frtbck);
		if (t2sep > halfpi_() - minang) {

/*                 There must be an occultation. */

		    *ocstat = TRUE_;
		} else {

/*                 There can't be an occultation: the "back" object */
/*                 is actually in transit across the "front" object. */

		    *ocstat = FALSE_;
		}
	    } else {

/*              We're looking for a point transit condition. */

		t2sep = vsep_(bckobs, bckfrt);
		if (t2sep < halfpi_() - minang) {

/*                 There must be a transit. */

		    *ocstat = TRUE_;
		} else {

/*                 There can't be a transit: the "back" object */
/*                 actually occults the "front" object. */

		    *ocstat = FALSE_;
		}
	    }

/*           OCSTAT has been set. */

	    chkout_("ZZGFOCST", (ftnlen)8);
	    return 0;
	}

/*        If we've reached this point, we have a situation where we */
/*        can't classify the geometry using bounding spheres. Instead, */
/*        we'll see whether the observer-point target vector intersects */
/*        the ellipsoidal body. */

	if (pntocc) {

/*           The front body is the ellipsoid. */

	    sincpt_("Ellipsoid", svfnam, time, svffrm, svcorr, svonam, "J2000"
		    , bckpos, spoint, &trgepc, srfvec, &found, (ftnlen)9, (
		    ftnlen)36, (ftnlen)32, (ftnlen)5, (ftnlen)36, (ftnlen)5);
	    if (failed_()) {
		chkout_("ZZGFOCST", (ftnlen)8);
		return 0;
	    }
	    if (found) {

/*              There's an intercept. If the distance from the observer */
/*              to the intercept is less than the distance from the */
/*              observer to the back target, then the back target is */
/*              occulted; otherwise there's a point transit, which is */
/*              not considered an occultation in this case. */

		*ocstat = vnorm_(srfvec) < bdist;
	    } else {

/*              There's no overlap and hence no occultation. */

		*ocstat = FALSE_;
	    }
	} else {

/*           The back body is the ellipsoid. */

	    sincpt_("Ellipsoid", svbnam, time, svbfrm, svcorr, svonam, "J2000"
		    , frtpos, spoint, &trgepc, srfvec, &found, (ftnlen)9, (
		    ftnlen)36, (ftnlen)32, (ftnlen)5, (ftnlen)36, (ftnlen)5);
	    if (failed_()) {
		chkout_("ZZGFOCST", (ftnlen)8);
		return 0;
	    }
	    if (found) {

/*              There's an intercept. If the distance from the observer */
/*              to the intercept is greater than the distance from the */
/*              observer to the front target, then the front target is */
/*              in transit across the back target; otherwise there's a */
/*              point occultation, which is not considered a transit in */
/*              this case. */

		*ocstat = vnorm_(srfvec) > fdist;
	    } else {

/*              There's no overlap and hence no occultation. */

		*ocstat = FALSE_;
	    }
	}
    } else {

/*        Bad combination of shapes. We expect this situation to have */
/*        been caught at initialization time, but make this check for */
/*        safety. */

	setmsg_("The combination of shapes of front and back targets is not "
		"supported: front shape = #; back shape = #.", (ftnlen)102);
	errch_("#", svfshp, (ftnlen)1, (ftnlen)9);
	errch_("#", svbshp, (ftnlen)1, (ftnlen)9);
	sigerr_("SPICE(INVALIDSHAPECOMBO)", (ftnlen)24);
	chkout_("ZZGFOCST", (ftnlen)8);
	return 0;
    }
    chkout_("ZZGFOCST", (ftnlen)8);
    return 0;
} /* zzgfocu_ */

/* Subroutine */ int zzgfocu_(char *occtyp, char *front, char *fshape, char *
	fframe, char *back, char *bshape, char *bframe, char *obsrvr, char *
	abcorr, doublereal *time, logical *ocstat, ftnlen occtyp_len, ftnlen 
	front_len, ftnlen fshape_len, ftnlen fframe_len, ftnlen back_len, 
	ftnlen bshape_len, ftnlen bframe_len, ftnlen obsrvr_len, ftnlen 
	abcorr_len)
{
    return zzgfocu_0_(0, occtyp, front, fshape, fframe, back, bshape, bframe, 
	    obsrvr, abcorr, time, ocstat, occtyp_len, front_len, fshape_len, 
	    fframe_len, back_len, bshape_len, bframe_len, obsrvr_len, 
	    abcorr_len);
    }

/* Subroutine */ int zzgfocin_(char *occtyp, char *front, char *fshape, char *
	fframe, char *back, char *bshape, char *bframe, char *obsrvr, char *
	abcorr, ftnlen occtyp_len, ftnlen front_len, ftnlen fshape_len, 
	ftnlen fframe_len, ftnlen back_len, ftnlen bshape_len, ftnlen 
	bframe_len, ftnlen obsrvr_len, ftnlen abcorr_len)
{
    return zzgfocu_0_(1, occtyp, front, fshape, fframe, back, bshape, bframe, 
	    obsrvr, abcorr, (doublereal *)0, (logical *)0, occtyp_len, 
	    front_len, fshape_len, fframe_len, back_len, bshape_len, 
	    bframe_len, obsrvr_len, abcorr_len);
    }

/* Subroutine */ int zzgfocst_(doublereal *time, logical *ocstat)
{
    return zzgfocu_0_(2, (char *)0, (char *)0, (char *)0, (char *)0, (char *)
	    0, (char *)0, (char *)0, (char *)0, (char *)0, time, ocstat, (
	    ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (
	    ftnint)0, (ftnint)0, (ftnint)0);
    }

