/* occult.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      OCCULT ( find occultation type at time ) */
/* Subroutine */ int occult_(char *targ1, char *shape1, char *frame1, char *
	targ2, char *shape2, char *frame2, char *abcorr, char *obsrvr, 
	doublereal *et, integer *ocltid, ftnlen targ1_len, ftnlen shape1_len, 
	ftnlen frame1_len, ftnlen targ2_len, ftnlen shape2_len, ftnlen 
	frame2_len, ftnlen abcorr_len, ftnlen obsrvr_len)
{
    /* Initialized data */

    static char occtyp[9*3] = "PARTIAL  " "ANNULAR  " "FULL     ";

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    char back[36];
    extern /* Subroutine */ int zzgfocin_(char *, char *, char *, char *, 
	    char *, char *, char *, char *, char *, ftnlen, ftnlen, ftnlen, 
	    ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen);
    char shap1[9], shap2[9];
    extern /* Subroutine */ int zzgfocst_(doublereal *, logical *);
    integer i__;
    char bname[36], fname[36];
    extern /* Subroutine */ int chkin_(char *, ftnlen), ucase_(char *, char *,
	     ftnlen, ftnlen);
    integer index;
    char front[36];
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    logical ellps2;
    extern logical failed_(void);
    char bframe[32], fframe[32], bshape[9], fshape[9];
    integer mltfac;
    logical ocstat;
    extern /* Subroutine */ int chkout_(char *, ftnlen), setmsg_(char *, 
	    ftnlen), errint_(char *, integer *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int sigerr_(char *, ftnlen);

/* $ Abstract */

/*     Determines the occultation condition (not occulted, partially, */
/*     etc.) of one target relative to another target as seen by */
/*     an observer at a given time. */

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
/*     KERNEL */

/* $ Keywords */

/*     GEOMETRY */
/*     OCCULTATION */
/*     ELLIPSOID */

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

/*     This file contains public, global parameter declarations */
/*     for the SPICELIB occultation routines. */

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

/*     ELLIPSOID */
/*     GEOMETRY */
/*     OCCULTATION */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     S.C. Krening      (JPL) */
/*     N.J. Bachman      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 24-JAN-2012 (SCK) (NJB) */

/* -& */

/*     The following integer codes indicate the geometric relationship */
/*     of the three bodies. */

/*     The meaning of the sign of each code is given below. */

/*                    Code sign          Meaning */
/*                    ---------          ------------------------------ */
/*                       > 0             The second ellipsoid is */
/*                                       partially or fully occulted */
/*                                       by the first. */

/*                       < 0             The first ellipsoid is */
/*                                       partially of fully */
/*                                       occulted by the second. */

/*                       = 0             No occultation. */

/*     The meanings of the codes are given below. The variable names */
/*     indicate the type of occultation and which target is in the back. */
/*     For example, TOTAL1 represents a total occultation in which the */
/*     first target is in the back (or occulted by) the second target. */

/*                    Name      Code     Meaning */
/*                    ------    -----    ------------------------------ */
/*                    TOTAL1     -3      Total occultation of first */
/*                                       target by second. */

/*                    ANNLR1     -2      Annular occultation of first */
/*                                       target by second.  The second */
/*                                       target does not block the limb */
/*                                       of the first. */

/*                    PARTL1     -1      Partial occultation of first */
/*                                       target by second target. */

/*                    NOOCC       0      No occultation or transit:  both */
/*                                       objects are completely visible */
/*                                       to the observer. */

/*                    PARTL2      1      Partial occultation of second */
/*                                       target by first target. */

/*                    ANNLR2      2      Annular occultation of second */
/*                                       target by first. */

/*                    TOTAL2      3      Total occultation of second */
/*                                       target by first. */


/*     End include file occult.inc */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     TARG1      I   Name or ID of first target. */
/*     SHAPE1     I   Type of shape model used for first target. */
/*     FRAME1     I   Body-fixed, body-centered frame for first body. */
/*     TARG2      I   Name or ID of second target. */
/*     SHAPE2     I   Type of shape model used for second target. */
/*     FRAME2     I   Body-fixed, body-centered frame for second body. */
/*     ABCORR     I   Aberration correction flag. */
/*     OBSRVR     I   Name or ID of the observer. */
/*     ET         I   Time of the observation (seconds past J2000). */
/*     OCLTID     O   Occultation identification code. */

/* $ Detailed_Input */

/*     TARG1      is the name of the first target body. Both object */
/*                names and NAIF IDs are accepted. For example, both */
/*                'Moon' and '301' are accepted. */

/*     SHAPE1     is a string indicating the geometric model used to */
/*                represent the shape of the first target body. The */
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

/*                At least one of the target bodies TARG1 or TARG2 must */
/*                be modeled as an ellipsoid. */

/*                Case and leading or trailing blanks are not */
/*                significant in the string. */

/*     FRAME1     is the name of the body-fixed, body-centered reference */
/*                frame associated with the first target body. Examples */
/*                of such names are 'IAU_SATURN' (for Saturn) and */
/*                'ITRF93' (for the Earth). */

/*                If the first target body is modeled as a point, FRAME1 */
/*                should be left blank (Ex: ' '). */

/*                Case and leading or trailing blanks bracketing a */
/*                non-blank frame name are not significant in the string. */

/*     TARG2      is the name of the second target body. See the */
/*                description of TARG1 above for more details. */

/*     SHAPE2     is the shape specification for the body designated */
/*                by TARG2. See the description of SHAPE1 above for */
/*                details. */

/*     FRAME2     is the name of the body-fixed, body-centered reference */
/*                frame associated with the second target body. See the */
/*                description of FRAME1 above for more details. */

/*     ABCORR     indicates the aberration corrections to be applied to */
/*                the state of each target body to account for one-way */
/*                light time. Stellar aberration corrections are */
/*                ignored if specified, since these corrections don't */
/*                improve the accuracy of the occultation determination. */

/*                See the header of the SPICE routine SPKEZR for a */
/*                detailed description of the aberration correction */
/*                options. For convenience, the options supported by */
/*                this routine are listed below: */

/*                   'NONE'     Apply no correction. */

/*                   'LT'       "Reception" case: correct for */
/*                              one-way light time using a Newtonian */
/*                              formulation. */

/*                   'CN'       "Reception" case: converged */
/*                              Newtonian light time correction. */

/*                   'XLT'      "Transmission" case: correct for */
/*                              one-way light time using a Newtonian */
/*                              formulation. */

/*                   'XCN'      "Transmission" case: converged */
/*                              Newtonian light time correction. */

/*                Case and blanks are not significant in the string */
/*                ABCORR. */

/*     OBSRVR     is the name of the body from which the occultation */
/*                is observed. See the description of TARG1 above for */
/*                more details. */

/*     ET         is the observation time in seconds past the J2000 */
/*                epoch. */


/* $ Detailed_Output */

/*     OCLTID     is an integer occultation code indicating the geometric */
/*                relationship of the three bodies. */

/*                The meaning of the sign of OCLTID is given below. */

/*                    Code sign          Meaning */
/*                    ---------          ------------------------------ */
/*                       > 0             The second ellipsoid is */
/*                                       partially or fully occulted */
/*                                       by the first. */

/*                       < 0             The first ellipsoid is */
/*                                       partially of fully */
/*                                       occulted by the second. */

/*                       = 0             No occultation. */

/*                Possible OCLTID values and meanings are given below. */
/*                The variable names indicate the type of occultation */
/*                and which target is in the back. For example, TOTAL1 */
/*                represents a total occultation in which the first */
/*                target is in the back (or occulted by) the second */
/*                target. */

/*                    Name      Code     Meaning */
/*                    ------    -----    ------------------------------ */
/*                    TOTAL1     -3      Total occultation of first */
/*                                       target by second. */

/*                    ANNLR1     -2      Annular occultation of first */
/*                                       target by second. The second */
/*                                       target does not block the limb */
/*                                       of the first. */

/*                    PARTL1     -1      Partial occultation of first */
/*                                       target by second target. */

/*                    NOOCC       0      No occultation or transit: both */
/*                                       objects are completely visible */
/*                                       to the observer. */

/*                    PARTL2      1      Partial occultation of second */
/*                                       target by first target. */

/*                    ANNLR2      2      Annular occultation of second */
/*                                       target by first. */

/*                    TOTAL2      3      Total occultation of second */
/*                                       target by first. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the target or observer body names input by the user are */
/*         not recognized, the error will be diagnosed by a routine in */
/*         the call tree of this routine. */

/*     2)  If the input shapes are not accepted, the error will be */
/*         diagnosed by a routine in the call tree of this routine. */

/*     3)  If both input shapes are points, the error will be */
/*         diagnosed by a routine in the call tree of this routine. */

/*     4)  If the radii of a target body modeled as an ellipsoid cannot */
/*         be determined by searching the kernel pool for a kernel */
/*         variable having a name of the form */

/*            'BODYnnn_RADII' */

/*         where nnn represents the NAIF integer code associated with */
/*         the body, the error will be diagnosed by a routine in the */
/*         call tree of this routine. */

/*     5)  If any of the target or observer bodies (TARG1, TARG2, or */
/*         OBSRVR) are the same, the error will be diagnosed */
/*         by a routine in the call tree of this routine. */

/*     6)  If the loaded kernels provide insufficient data to */
/*         compute any required state vector, the deficiency will */
/*         be diagnosed by a routine in the call tree of this routine. */

/*     7)  If an error occurs while reading an SPK or other kernel, */
/*         the error will be diagnosed by a routine in the call tree */
/*         of this routine. */

/*     8)  Invalid aberration correction specifications will be */
/*         diagnosed by a routine in the call tree of this routine. */

/* $ Files */

/*     Appropriate SPICE kernels must be loaded by the calling program */
/*     before this routine is called. */

/*     The following data are required: */

/*        - SPK data: the calling application must load ephemeris data */
/*          for the targets and observer for the specified input time. */
/*          If aberration corrections are used, the states of the target */
/*          bodies and of the observer relative to the solar system */
/*          barycenter must be calculable from the available ephemeris */
/*          data. Typically ephemeris data are made available by loading */
/*          one or more SPK files via FURNSH. */

/*        - PCK data: bodies modeled as triaxial ellipsoids must have */
/*          semi-axis lengths provided by variables in the kernel pool. */
/*          Typically these data are made available by loading a text */
/*          PCK file via FURNSH. */

/*     Kernel data are normally loaded once per program run, NOT every */
/*     time this routine is called. */

/* $ Particulars */

/*     For many purposes, modeling extended bodies as triaxial */
/*     ellipsoids is adequate for determining whether one body is */
/*     occulted by another as seen from a specified observer. */

/* $ Examples */

/*     1) Find whether MRO is occulted by Mars as seen by */
/*        the DSS-13 ground station at a few specific */
/*        times. */

/*        Use the meta-kernel shown below to load the required SPICE */
/*        kernels. */

/*           KPL/MK */

/*           File: mro_ex_occult.tm */

/*           This is the meta-kernel file for the example problem for */
/*           the subroutine OCCULT. These kernel files can be found in */
/*           the NAIF archives. */

/*           In order for an application to use this meta-kernel, the */
/*           kernels referenced here must be present in the user's */
/*           current working directory. */

/*           The names and contents of the kernels referenced */
/*           by this meta-kernel are as follows: */

/*                 File name                       Contents */
/*                 ---------                       -------- */
/*                 de421.bsp                       Planetary ephemeris */
/*                 earthstns_itrf93_050714.bsp     DSN station ephemeris */
/*                 pck00010.tpc                    Planet orientation and */
/*                                                 radii */
/*                 earth_000101_120409_120117.bpc  High precision Earth */
/*                                                 orientation */
/*                 mro_psp_rec.bsp                 MRO ephemeris */
/*                 naif0010.tls                    Leapseconds */
/*                 earth_topo_050714.tf            Topocentric reference */
/*                                                 frames for */
/*                                                 DSN stations */

/*           \begindata */

/*           KERNELS_TO_LOAD = ( 'de421.bsp', */
/*                               'earthstns_itrf93_050714.bsp', */
/*                               'pck00010.tpc', */
/*                               'earth_000101_120409_120117.bpc', */
/*                               'mro_psp_rec.bsp', */
/*                               'naif0010.tls', */
/*                               'earth_topo_050714.tf' ) */
/*           \begintext */

/*        Example code begins here. */

/*           PROGRAM OCCULT_MRO */
/*           IMPLICIT NONE */

/*           INCLUDE              'occult.inc' */

/*     C */
/*     C     Local parameters */
/*     C */
/*           CHARACTER*(*)         META */
/*           PARAMETER           ( META  = mro_ex_occult.tm' ) */

/*           CHARACTER*(*)         FRMT */
/*           PARAMETER           ( FRMT  = '(A18, A5, A21, A5, A4, A6)' ) */

/*           INTEGER               CHSIZ */
/*           PARAMETER           ( CHSIZ = 30 ) */

/*     C */
/*     C     Local variables */
/*     C */
/*           CHARACTER*(CHSIZ)     ABCORR */
/*           CHARACTER*(CHSIZ)     FORM */
/*           CHARACTER*(CHSIZ)     OBSRVR */
/*           CHARACTER*(CHSIZ)     SHAPE1 */
/*           CHARACTER*(CHSIZ)     SHAPE2 */
/*           CHARACTER*(CHSIZ)     TARG1 */
/*           CHARACTER*(CHSIZ)     TARG2 */
/*           CHARACTER*(CHSIZ)     TIME */
/*           CHARACTER*(CHSIZ)     TSTART */
/*           CHARACTER*(CHSIZ)     TEND */
/*           CHARACTER*(CHSIZ)     OUTCH ( 4 ) */

/*           DOUBLE PRECISION      ET */
/*           DOUBLE PRECISION      ET1 */
/*           DOUBLE PRECISION      ETEND */

/*           INTEGER               DT */
/*           INTEGER               OCLTID */

/*     C */
/*     C     Saved variables */
/*     C */
/*           SAVE OUTCH */

/*           DATA OUTCH ( 1 ) / 'totally occulted by'   / */
/*           DATA OUTCH ( 2 ) / 'transited by'          / */
/*           DATA OUTCH ( 3 ) / 'partially occulted by' / */
/*           DATA OUTCH ( 4 ) / 'not occulted by'       / */

/*     C */
/*     C     Initialize the time range. Set the output time */
/*     C     format to PST. Set DT to an hour interval in */
/*     C     units of seconds. */
/*     C */
/*           TSTART = '2012-JAN-5 1:15:00 UTC' */
/*           TEND   = '2012-JAN-5 2:50:00 UTC' */
/*           FORM   = 'YYYY-MON-DD HR:MN ::UTC-8' */
/*           DT     = 1000 */

/*     C */
/*     C     Initialize the targets, observer, and aberration */
/*     C     correction. */
/*     C */
/*           TARG1  = 'MRO' */
/*           SHAPE1 = 'POINT' */
/*           TARG2  = 'MARS' */
/*           SHAPE2 = 'ELLIPSOID' */
/*           OBSRVR = 'DSS-13' */
/*           ABCORR = 'CN' */

/*     C */
/*     C     Load kernel files via the meta-kernel. */
/*     C */
/*           CALL FURNSH ( META ) */
/*     C */
/*     C     Calculate the start and stop times in ET. */
/*     C */
/*           CALL STR2ET ( TSTART, ET1   ) */
/*           CALL STR2ET ( TEND,   ETEND ) */

/*           ET = ET1 */
/*           DO WHILE ( ET .LT. ETEND ) */
/*     C */
/*     C        Calculate the type of occultation that */
/*     C        corresponds to time ET. */
/*     C */
/*              CALL OCCULT ( TARG1,  SHAPE1, ' ', */
/*          .                 TARG2,  SHAPE2, 'IAU_MARS', */
/*          .                 ABCORR, OBSRVR,  ET, OCLTID ) */
/*     C */
/*     C        Output the results. */
/*     C */
/*              CALL TIMOUT ( ET, FORM, TIME ) */

/*              IF ( OCLTID .EQ. TOTAL1 ) THEN */
/*                 WRITE (*,FRMT) TIME, TARG1, OUTCH(1), TARG2, */
/*          .                     'wrt ', OBSRVR */

/*              ELSEIF ( OCLTID .EQ. ANNLR1 ) THEN */
/*                 WRITE (*,FRMT) TIME, TARG1, OUTCH(2), TARG2, */
/*          .                     'wrt ', OBSRVR */

/*              ELSEIF ( OCLTID .EQ. PARTL1 ) THEN */
/*                 WRITE (*,FRMT) TIME, TARG1, OUTCH(3), TARG2, */
/*          .                     'wrt ', OBSRVR, */
/*          .                     'NOT POSSIBLE FOR POINT' */

/*              ELSEIF ( OCLTID .EQ. NOOCC ) THEN */
/*                 WRITE (*,FRMT) TIME, TARG1, OUTCH(4), TARG2, */
/*          .                     'wrt ', OBSRVR */

/*              ELSEIF ( OCLTID .EQ. PARTL2 ) THEN */
/*                 WRITE (*,FRMT) TIME, TARG2, OUTCH(3), TARG1, */
/*          .                     'wrt ', OBSRVR, */
/*          .                     'NOT POSSIBLE FOR POINT' */

/*              ELSEIF ( OCLTID .EQ. ANNLR2 ) THEN */
/*                 WRITE (*,FRMT) TIME, TARG2, OUTCH(2), TARG1, */
/*          .                     'wrt ', OBSRVR */

/*              ELSEIF ( OCLTID .EQ. TOTAL2 ) THEN */
/*                 WRITE (*,FRMT) TIME, TARG2, OUTCH(1), TARG1, */
/*          .                     'wrt ', OBSRVR */

/*              ELSE */
/*                 WRITE (*,*) 'Bad occultation ID:  ', OCLTID */

/*              END IF */
/*     C */
/*     C        Increment the time. */
/*     C */
/*              ET = ET + DT */

/*           END DO */

/*           END */

/*        When this program was executed using gfortran on a PC Linux */
/*        64 bit environment, the output was: */

/*           2012-JAN-04 17:15 MARS transited by         MRO  wrt DSS-13 */
/*           2012-JAN-04 17:31 MRO  not occulted by      MARS wrt DSS-13 */
/*           2012-JAN-04 17:48 MRO  totally occulted by  MARS wrt DSS-13 */
/*           2012-JAN-04 18:04 MRO  totally occulted by  MARS wrt DSS-13 */
/*           2012-JAN-04 18:21 MRO  not occulted by      MARS wrt DSS-13 */
/*           2012-JAN-04 18:38 MARS transited by         MRO  wrt DSS-13 */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     S.C. Krening   (JPL) */
/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 14-NOV-2013 (SCK) (NJB) */


/* -& */
/* $ Index_Entries */

/*     occultation type at a specified time */

/* -& */

/*     SPICELIB functions */


/*     External routines */


/*     Local parameters */


/*     Local variables */


/*     Saved variables */


/*     The variable OCCTYP associates the string of an occultation */
/*     type (from gf.inc) with its positive integer code (from */
/*     occult.inc).  The variable OCCTYP is set up so each string is */
/*     stored at the index relating to that configuration's positive */
/*     integer code.  The positive integer codes assume the first */
/*     target is occulting (in front of) the second target. */

/*                 Ex:  PARTL2 = 1               (from occult.inc) */
/*                      OCCTYP ( 1 ) = 'PARTIAL' (from gf.inc) */

/*     The table below shows the relation between each index of OCCTYP, */
/*     the occultation condition, which target is in front and back, the */
/*     multiplication factor, and the output integer occultation code. */
/*     Note that the output integer occultation code is the integer index */
/*     of OCCTYP multiplied by the multiplication factor. */

/*                 OCLTID = Index * MLTFAC */

/*     MLTFAC is 1 if TARG1 is in front, and -1 if TARG1 is in back. */
/*     The setup of OCCTYP could be changed, but it is important to keep */
/*     the output integer occultation codes consistent with the values */
/*     from occult.inc. */

/*         Index   Occult. Condition   Front   Back   MLTFAC  OCLTID */
/*         -----   -----------------   -----   -----  ------  ------ */
/*           1          Partial        TARG1   TARG2    1       1 */
/*           1          Partial        TARG2   TARG1   -1      -1 */
/*           2          Annular        TARG1   TARG2    1       2 */
/*           2          Annular        TARG2   TARG1   -1      -2 */
/*           3          Total          TARG1   TARG2    1       3 */
/*           3          Total          TARG2   TARG1   -1      -3 */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("OCCULT", (ftnlen)6);

/*     Left justify the shapes and target names and make them upper case. */

    ljust_(shape1, shap1, shape1_len, (ftnlen)9);
    ucase_(shap1, shap1, (ftnlen)9, (ftnlen)9);
    ljust_(shape2, shap2, shape2_len, (ftnlen)9);
    ucase_(shap2, shap2, (ftnlen)9, (ftnlen)9);
    ljust_(targ1, fname, targ1_len, (ftnlen)36);
    ucase_(fname, fname, (ftnlen)36, (ftnlen)36);
    ljust_(targ2, bname, targ2_len, (ftnlen)36);
    ucase_(bname, bname, (ftnlen)36, (ftnlen)36);

/*     The variable ELLPS2 is a flag that relates if both targets are */
/*     represented as ellipsoids. If not, only the 'any' occultation */
/*     check can be completed. */

    if (s_cmp(shap1, "ELLIPSOID", (ftnlen)9, (ftnlen)9) == 0 && s_cmp(shap2, 
	    "ELLIPSOID", (ftnlen)9, (ftnlen)9) == 0) {
	ellps2 = TRUE_;
    } else {
	ellps2 = FALSE_;
    }

/*     Test two main cases: */
/*     1) The first target is the front body. */
/*     2) The second target is the front body. */

/*     First, initialize the occultation code to reflect no occultation. */

    *ocltid = 0;
    for (i__ = 1; i__ <= 2; ++i__) {

/*        The first time through, make the first target the */
/*        front. On the second time, make the second target the front. */
/*        For details on the variable MLTFAC, please see the detailed */
/*        explanation of the OCCTYP variable near the start of the code. */

	if (i__ == 1) {
	    s_copy(front, fname, (ftnlen)36, (ftnlen)36);
	    s_copy(fshape, shap1, (ftnlen)9, (ftnlen)9);
	    s_copy(fframe, frame1, (ftnlen)32, frame1_len);
	    s_copy(back, bname, (ftnlen)36, (ftnlen)36);
	    s_copy(bshape, shap2, (ftnlen)9, (ftnlen)9);
	    s_copy(bframe, frame2, (ftnlen)32, frame2_len);
	    mltfac = 1;
	} else {
	    s_copy(front, bname, (ftnlen)36, (ftnlen)36);
	    s_copy(fshape, shap2, (ftnlen)9, (ftnlen)9);
	    s_copy(fframe, frame2, (ftnlen)32, frame2_len);
	    s_copy(back, fname, (ftnlen)36, (ftnlen)36);
	    s_copy(bshape, shap1, (ftnlen)9, (ftnlen)9);
	    s_copy(bframe, frame1, (ftnlen)32, frame1_len);
	    mltfac = -1;
	}

/*        Check if there is any occultation with the current front/back */
/*        configuration. ZZGFOCIN performs initializations. ZZGFOCST */
/*        returns a true/false logical indicating if there is an */
/*        occultation. */

	zzgfocin_("ANY", front, fshape, fframe, back, bshape, bframe, obsrvr, 
		abcorr, (ftnlen)3, (ftnlen)36, (ftnlen)9, (ftnlen)32, (ftnlen)
		36, (ftnlen)9, (ftnlen)32, obsrvr_len, abcorr_len);
	if (failed_()) {
	    chkout_("OCCULT", (ftnlen)6);
	    return 0;
	}
	zzgfocst_(et, &ocstat);
	if (failed_()) {
	    chkout_("OCCULT", (ftnlen)6);
	    return 0;
	}

/*        If there was an occultation, and both targets are represented */
/*        as ellipsoids, test the three types of occultations: partial, */
/*        annular, and full. Note: If the integer parameters within */
/*        occult.inc are changed, the following DO loop will need to be */
/*        altered. */

	if (ocstat && ellps2) {
	    for (index = 1; index <= 3; ++index) {
		zzgfocin_(occtyp + ((i__1 = index - 1) < 3 && 0 <= i__1 ? 
			i__1 : s_rnge("occtyp", i__1, "occult_", (ftnlen)718))
			 * 9, front, fshape, fframe, back, bshape, bframe, 
			obsrvr, abcorr, (ftnlen)9, (ftnlen)36, (ftnlen)9, (
			ftnlen)32, (ftnlen)36, (ftnlen)9, (ftnlen)32, 
			obsrvr_len, abcorr_len);
		if (failed_()) {
		    chkout_("OCCULT", (ftnlen)6);
		    return 0;
		}
		zzgfocst_(et, &ocstat);
		if (failed_()) {
		    chkout_("OCCULT", (ftnlen)6);
		    return 0;
		}

/*              If the occultation condition is true, return the integer */
/*              occultation ID code. */

		if (ocstat) {
		    *ocltid = mltfac * index;
		    chkout_("OCCULT", (ftnlen)6);
		    return 0;
		}

/*              End the DO loop that checks the occultation type. */

	    }

/*        If the search for 'any' occultation was true and the front */
/*        target is an ellipse, this is a total occultation. (Other */
/*        target is a point). */

	} else if (ocstat && s_cmp(fshape, "ELLIPSOID", (ftnlen)9, (ftnlen)9) 
		== 0) {
	    *ocltid = mltfac * 3;
	    chkout_("OCCULT", (ftnlen)6);
	    return 0;

/*        If the search for 'any' occultation was true and the back */
/*        target is an ellipse, this is an annular occultation. (Other */
/*        target is a point). */

	} else if (ocstat && s_cmp(bshape, "ELLIPSOID", (ftnlen)9, (ftnlen)9) 
		== 0) {
	    *ocltid = mltfac << 1;
	    chkout_("OCCULT", (ftnlen)6);
	    return 0;
	}

/*        End the DO loop that checks the front/back orientation of */
/*        the input targets. */

    }

/*     If the occultation searches show that there was no occultation */
/*     at the given time, return an occultation code that indicates */
/*     no occultation. If this part of the code has been reached and */
/*     the occultation code indicates an occultation was found, an error */
/*     has occurred. */

    if (*ocltid != 0) {
	setmsg_("This error should never be reached; the occultation code re"
		"sult # is invalid.", (ftnlen)77);
	errint_("#", ocltid, (ftnlen)1);
	sigerr_("SPICE(BUG)", (ftnlen)10);
	chkout_("OCCULT", (ftnlen)6);
	return 0;
    }
    chkout_("OCCULT", (ftnlen)6);
    return 0;
} /* occult_ */

