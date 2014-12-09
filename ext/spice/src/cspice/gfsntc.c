/* gfsntc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c_n1 = -1;
static integer c__3 = 3;
static integer c__0 = 0;
static integer c__10 = 10;
static logical c_false = FALSE_;

/* $Procedure GFSNTC (GF, surface intercept vector coordinate search) */
/* Subroutine */ int gfsntc_(char *target, char *fixref, char *method, char *
	abcorr, char *obsrvr, char *dref, doublereal *dvec, char *crdsys, 
	char *coord, char *relate, doublereal *refval, doublereal *adjust, 
	doublereal *step, doublereal *cnfine, integer *mw, integer *nw, 
	doublereal *work, doublereal *result, ftnlen target_len, ftnlen 
	fixref_len, ftnlen method_len, ftnlen abcorr_len, ftnlen obsrvr_len, 
	ftnlen dref_len, ftnlen crdsys_len, ftnlen coord_len, ftnlen 
	relate_len)
{
    /* System generated locals */
    integer work_dim1, work_offset, i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern logical even_(integer *);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer sized_(doublereal *);
    extern logical gfbail_();
    logical ok;
    extern /* Subroutine */ int scardd_(integer *, doublereal *);
    extern logical return_(void);
    extern /* Subroutine */ int gfrefn_(), gfrepi_(), gfrepu_(), gfrepf_(), 
	    gfstep_();
    char qcpars[80*10], qpnams[80*10];
    doublereal qdpars[10];
    integer qipars[10];
    logical qlpars[10];
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), gfsstp_(doublereal *), gfevnt_(U_fp, U_fp, char *, 
	    integer *, char *, char *, doublereal *, integer *, logical *, 
	    char *, doublereal *, doublereal *, doublereal *, doublereal *, 
	    logical *, U_fp, U_fp, U_fp, integer *, integer *, doublereal *, 
	    logical *, L_fp, doublereal *, ftnlen, ftnlen, ftnlen, ftnlen);
    doublereal tol;
    extern /* Subroutine */ int zzholdd_(integer *, integer *, logical *, 
	    doublereal *);

/* $ Abstract */

/*     Determine time intervals for which a coordinate of an */
/*     surface intercept position vector satisfies a numerical */
/*     constraint. */

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
/*     SPK */
/*     CK */
/*     TIME */
/*     WINDOWS */

/* $ Keywords */

/*     COORDINATE */
/*     GEOMETRY */
/*     SEARCH */
/*     EVENT */

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
/*     ZZGET      P   ZZHOLDD retrieves a stored DP value. */
/*     GF_TOL     P   ZZHOLDD acts on the GF subsystem tolerance. */
/*     TARGET     I   Name of the target body. */
/*     FIXREF     I   Body fixed frame associated with TARGET . */
/*     METHOD     I   Name of method type for surface intercept */
/*                    calculation. */
/*     ABCORR     I   Aberration correction flag. */
/*     OBSRVR     I   Name of the observing body. */
/*     DREF       I   Reference frame of direction vector DVEC. */
/*     DVEC       I   Pointing direction vector from OBSRVR. */
/*     CRDSYS     I   Name of the coordinate system containing COORD. */
/*     COORD      I   Name of the coordinate of interest. */
/*     RELATE     I   Relational operator. */
/*     REFVAL     I   Reference value. */
/*     ADJUST     I   Adjustment value for absolute extrema searches. */
/*     STEP       I   Step size used for locating extrema and roots. */
/*     CNFINE     I   SPICE window to which the search is confined. */
/*     MW         I   Workspace window size. */
/*     NW         I   The number of workspace windows needed for the */
/*                    search. */
/*     WORK      I-O   Array of workspace windows */
/*     RESULT    I-O   SPICE window containing results. */

/* $ Detailed_Input */

/*     TARGET   the string name of a target body.  Optionally, you may */
/*              supply the integer ID code for the object as an */
/*              integer string.  For example both 'MOON' and '301' */
/*              are legitimate strings that indicate the moon is the */
/*              target body. */

/*              On calling GFSNTC, the kernel pool must contain the radii */
/*              data corresponding to TARGET. */

/*     FIXREF   the string name of the body-fixed, body-centered */
/*              reference frame associated with the target body TARGET. */

/*              The SPICE frame subsystem must recognize the 'fixref' */
/*              name. */

/*     METHOD   the string name of the method to use for the surface */
/*              intercept calculation. The accepted values for METHOD: */

/*                 'Ellipsoid'        The intercept computation uses */
/*                                    a triaxial ellipsoid to model */
/*                                    the surface of the target body. */
/*                                    The ellipsoid's radii must be */
/*                                    available in the kernel pool. */

/*              The METHOD string lacks sensitivity to case, embedded, */
/*              leading and trailing blanks. */

/*     ABCORR   the string description of the aberration corrections to */
/*              apply to the state evaluations to account for one-way */
/*              light time and stellar aberration. */

/*              Any aberration correction accepted by the SPICE */
/*              routine SPKEZR is accepted here. See the header */
/*              of SPKEZR for a detailed description of the */
/*              aberration correction options. For convenience, */
/*              the options are listed below: */

/*                 'NONE'     Apply no correction. Returns the "true" */
/*                            geometric state. */

/*                 'LT'       "Reception" case:  correct for */
/*                            one-way light time using a Newtonian */
/*                            formulation. */

/*                 'LT+S'     "Reception" case:  correct for */
/*                            one-way light time and stellar */
/*                            aberration using a Newtonian */
/*                            formulation. */

/*                 'CN'       "Reception" case:  converged */
/*                            Newtonian light time correction. */

/*                'CN+S'     "Reception" case:  converged */
/*                            Newtonian light time and stellar */
/*                            aberration corrections. */

/*                 'XLT'      "Transmission" case:  correct for */
/*                            one-way light time using a Newtonian */
/*                            formulation. */

/*                 'XLT+S'    "Transmission" case:  correct for */
/*                            one-way light time and stellar */
/*                            aberration using a Newtonian */
/*                            formulation. */

/*                 'XCN'      "Transmission" case:  converged */
/*                            Newtonian light time correction. */

/*                 'XCN+S'    "Transmission" case:  converged */
/*                            Newtonian light time and stellar */
/*                            aberration corrections. */

/*              The ABCORR string lacks sensitivity to case, leading */
/*              and trailing blanks. */

/*              *Note* */

/*              When using a reference frame defined as a dynamic frame, */
/*              the user should realize defining an aberration correction */
/*              for the search different from that in the frames */
/*              definition will affect the search results. */

/*              In general, use the same aberration correction for */
/*              intercept point searches as used in the definition of a */
/*              dynamic frame (if applicable). */

/*     OBSRVR   the string name of an observing body.  Optionally, you */
/*              may supply the ID code of the object as an integer */
/*              string. For example, both 'EARTH' and '399' are */
/*              legitimate strings to indicate the observer as Earth. */

/*     DREF     the string name of the reference frame corresponding to */
/*              DVEC. */

/*              The DREF string lacks sensitivity to case, leading */
/*              and trailing blanks. */

/*     DVEC     the pointing or boresight vector from the observer. The */
/*              intercept of this vector and TARGET is the event of */
/*              interest. */

/*     CRDSYS   the string name of the coordinate system for which the */
/*              coordinate of interest is a member */

/*     COORD    the string name of the coordinate of interest in CRDSYS */

/*              The supported coordinate systems and coordinate names: */

/*          Coordinate System (CRDSYS)   Coordinates (COORD)  Range */

/*          'RECTANGULAR'              'X' */
/*                                     'Y' */
/*                                     'Z' */

/*          'LATITUDINAL'              'RADIUS' */
/*                                     'LONGITUDE'        (-Pi,Pi] */
/*                                     'LATITUDE'         [-Pi/2,Pi/2] */

/*          'RA/DEC'                   'RANGE' */
/*                                     'RIGHT ASCENSION'  [0,2Pi) */
/*                                     'DECLINATION'      [-Pi/2,Pi/2] */

/*          'SPHERICAL'                'RADIUS' */
/*                                     'COLATITUDE'       [0,Pi] */
/*                                     'LONGITUDE'        (-Pi,Pi] */

/*          'CYLINDRICAL'              'RADIUS' */
/*                                     'LONGITUDE'        [0,2Pi) */
/*                                     'Z' */

/*          'GEODETIC'                 'LONGITUDE'        (-Pi,Pi] */
/*                                     'LATITUDE'         [-Pi/2,Pi/2] */
/*                                     'ALTITUDE' */

/*          'PLANETOGRAPHIC'           'LONGITUDE'        [0,2Pi) */
/*                                     'LATITUDE'         [-Pi/2,Pi/2] */
/*                                     'ALTITUDE' */

/*                 The ALTITUDE coordinates have a constant value */
/*                 of zero +/- roundoff for ellipsoid targets. */

/*                 Limit searches for coordinate events in the GEODETIC */
/*                 and PLANETOGRAPHIC coordinate systems to TARGET bodies */
/*                 with axial symmetry in the equatorial plane, i.e. */
/*                 equality of the body X and Y radii (oblate or prolate */
/*                 spheroids). */

/*     RELATE   the string or character describing the relational */
/*              operator used to define a constraint on the selected */
/*              coordinate of the surface intercept vector. The result */
/*              window found by this routine indicates the time intervals */
/*              where the constraint is satisfied. Supported values of */
/*              RELATE and corresponding meanings are shown below: */

/*                 '>'       The coordinate value is greater than the */
/*                           reference value REFVAL. */

/*                 '='       The coordinate value is equal to the */
/*                           reference value REFVAL. */

/*                 '<'       The coordinate value is less than the */
/*                           reference value REFVAL. */

/*                 'ABSMAX'  The coordinate value is at an absolute */
/*                           maximum. */

/*                 'ABSMIN'  The coordinate value is at an absolute */
/*                           minimum. */

/*                 'LOCMAX'  The coordinate value is at a local */
/*                           maximum. */

/*                 'LOCMIN'  The coordinate value is at a local */
/*                           minimum. */

/*              The caller may indicate that the region of interest */
/*              is the set of time intervals where the quantity is */
/*              within a specified measure of an absolute extremum. */
/*              The argument ADJUST (described below) is used to */
/*              specify this measure. */

/*              Local extrema are considered to exist only in the */
/*              interiors of the intervals comprising the confinement */
/*              window:  a local extremum cannot exist at a boundary */
/*              point of the confinement window. */

/*              The RELATE string lacks sensitivity to case, leading */
/*              and trailing blanks. */

/*     REFVAL   the double precision reference value used together with */
/*              the argument RELATE to define an equality or inequality */
/*              to satisfy by the selected coordinate of the surface */
/*              intercept vector. See the discussion of RELATE above for */
/*              further information. */

/*               The units of REFVAL correspond to the type as defined */
/*               by COORD, radians for angular measures, kilometers for */
/*               distance measures. */

/*     ADJUST   a double precision value used to modify searches for */
/*              absolute extrema: when RELATE is set to ABSMAX or ABSMIN */
/*              and ADJUST is set to a positive value, GFSNTC finds times */
/*              when the intercept vector coordinate is within ADJUST */
/*              radians/kilometers of the specified extreme value. */

/*              For RELATE set to ABSMAX, the RESULT window contains */
/*              time intervals when the intercept vector coordinate has */
/*              values between ABSMAX - ADJUST and ABSMAX. */

/*              For RELATE set to ABSMIN, the RESULT window contains */
/*              time intervals when the intercept vector coordinate has */
/*              values between ABSMIN and ABSMIN + ADJUST. */

/*              ADJUST is not used for searches for local extrema, */
/*              equality or inequality conditions. */

/*     STEP     the double precision time step size to use in the search. */

/*              Selection of the time step for surface intercept geometry */
/*              requires consideration of the mechanics of a surface */
/*              intercept event. In most cases, two distinct searches */
/*              will be needed, one to determine the windows when the */
/*              boresight vector intercepts the surface and then the */
/*              search based on the user defined constraints within those */
/*              windows. The boresight of nadir pointing instrument may */
/*              continually intercept a body, but an instrument scanning */
/*              across a disc will have configurations when the */
/*              boresight does not intercept the body. */

/*              The step size must be smaller than the shortest interval */
/*              within the confinement window over which the intercept */
/*              exists and also smaller than the shortest interval over */
/*              which the intercept does not exist. */

/*              For coordinates other than LONGITUDE and RIGHT ASCENSION, */
/*              the step size must be shorter than the shortest interval, */
/*              within the confinement window, over which the coordinate */
/*              is monotone increasing or decreasing. */

/*              For LONGITUDE and RIGHT ASCENSION, the step size must */
/*              be shorter than the shortest interval, within the */
/*              confinement window, over which either the sin or cosine */
/*              of the coordinate is monotone increasing or decreasing. */

/*              The choice of STEP affects the completeness but not */
/*              the precision of solutions found by this routine; the */
/*              precision is controlled by the convergence tolerance. */
/*              See the discussion of the parameter CNVTOL for */
/*              details. */

/*              STEP has units of TDB seconds. */

/*     CNFINE   a double precision SPICE window that confines the time */
/*              period over which the specified search is conducted. */
/*              CNFINE may consist of a single interval or a collection */
/*              of intervals. */

/*              In some cases the confinement window can be used to */
/*              greatly reduce the time period that must be searched */
/*              for the desired solution. See the Particulars section */
/*              below for further discussion. */

/*              See the Examples section below for a code example */
/*              that shows how to create a confinement window. */

/*              CNFINE must be initialized by the caller using the */
/*              SPICELIB routine SSIZED. */

/*     MW       is a parameter specifying the length of the SPICE */
/*              windows in the workspace array WORK (see description */
/*              below) used by this routine. */

/*              MW should be set to a number at least twice as large */
/*              as the maximum number of intervals required by any */
/*              workspace window. In many cases, it's not necessary to */
/*              compute an accurate estimate of how many intervals are */
/*              needed; rather, the user can pick a size considerably */
/*              larger than what's really required. */

/*              However, since excessively large arrays can prevent */
/*              applications from compiling, linking, or running */
/*              properly, sometimes MW must be set according to */
/*              the actual workspace requirement. A rule of thumb */
/*              for the number of intervals NINTVLS needed is */

/*                  NINTVLS  =  2*N  +  ( M / STEP ) */

/*              where */

/*                  N     is the number of intervals in the confinement */
/*                        window */

/*                  M     is the measure of the confinement window, in */
/*                        units of seconds */

/*                  STEP  is the search step size in seconds */

/*              MW should then be set to */

/*                  2 * NINTVLS */

/*     NW       is a parameter specifying the number of SPICE windows */
/*              in the workspace array WORK (see description below) */
/*              used by this routine. NW should be set to the */
/*              parameter NWMAX; this parameter is declared in the */
/*              include file gf.inc. (The reason this dimension is */
/*              an input argument is that this allows run-time */
/*              error checking to be performed.) */

/*     WORK     is an array used to store workspace windows. This */
/*              array should be declared by the caller as shown: */

/*                 INCLUDE 'gf.inc' */
/*                    ... */

/*                 DOUBLE PRECISION    WORK ( LBCELL : MW, NWMAX ) */

/*              where MW is a constant declared by the caller and */
/*              NWMAX is a constant defined in the SPICELIB INCLUDE */
/*              file gf.inc. See the discussion of MW above. */

/*              WORK need not be initialized by the caller. */

/*     RESULT   a double precision SPICE window which will contain the */
/*              search results. RESULT must be initialized using */
/*              a call to SSIZED. RESULT must be declared and initialized */
/*              with sufficient size to capture the full set of time */
/*              intervals within the search region on which the specified */
/*              constraint is satisfied. */

/*              If RESULT is non-empty on input, its contents */
/*              will be discarded before GFSNTC conducts its */
/*              search. */

/* $ Detailed_Output */

/*     WORK     the input workspace array, modified by this */
/*              routine. */

/*     RESULT   the SPICE window of intervals, contained within the */
/*              confinement window CNFINE, on which the specified */
/*              constraint is satisfied. */

/*              If the search is for local extrema, or for absolute */
/*              extrema with ADJUST set to zero, then normally each */
/*              interval of RESULT will be a singleton: the left and */
/*              right endpoints of each interval will be identical. */

/*              If no times within the confinement window satisfy the */
/*              constraint, RESULT will be returned with a */
/*              cardinality of zero. */

/* $ Parameters */

/*     LBCELL   the integer value defining the lower bound for */
/*              SPICE Cell arrays (a SPICE window is a kind of cell). */

/*     CNVTOL   is the convergence tolerance used for finding */
/*              endpoints of the intervals comprising the result */
/*              window. CNVTOL is also used for finding intermediate */
/*              results; in particular, CNVTOL is used for finding the */
/*              windows on which the specified coordinate is increasing */
/*              or decreasing. CNVTOL is used to determine when binary */
/*              searches for roots should terminate: when a root is */
/*              bracketed within an interval of length CNVTOL; the */
/*              root is considered to have been found. */

/*              The accuracy, as opposed to precision, of roots found */
/*              by this routine depends on the accuracy of the input */
/*              data. In most cases, the accuracy of solutions will be */
/*              inferior to their precision. */

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
/*         that if the step size is non-positive, an error is signaled */
/*         by a routine in the call tree of this routine. */

/*     2)  Due to numerical errors, in particular, */

/*            - truncation error in time values */
/*            - finite tolerance value */
/*            - errors in computed geometric quantities */

/*         it is *normal* for the condition of interest to not always be */
/*         satisfied near the endpoints of the intervals comprising the */
/*         RESULT window. One technique to handle such a situation, */
/*         slightly contract RESULT using the window routine WNCOND. */

/*     3)  If the window size MW is less than 2 or not an even value, */
/*         the error SPICE(INVALIDDIMENSION) will signal. */

/*     4)  If the window size of RESULT is less than 2, the error */
/*         SPICE(INVALIDDIMENSION) will signal. */

/*     5)  If an error (typically cell overflow) occurs during */
/*         window arithmetic, the error will be diagnosed by a routine */
/*         in the call tree of this routine. */

/*     6)  If the relational operator RELATE is not recognized, an */
/*         error is signaled by a routine in the call tree of this */
/*         routine. */

/*     7)  If the size of the workspace is too small, an error is */
/*         signaled by a routine in the call tree of this routine. */

/*     8)  If ADJUST is negative, an error is signaled by a routine in */
/*         the call tree of this routine. */

/*     9)  If either of the input body names do not map to NAIF ID */
/*         codes, an error is signaled by a routine in the call tree of */
/*         this routine. */

/*     10) If required ephemerides or other kernel data are not */
/*         available, an error is signaled by a routine in the call tree */
/*         of this routine. */

/*     11) If a body has unequal equatorial radii, a search for */
/*         coordinate events in the GEODETIC or PLANETOGRAPHIC coordinate */
/*         systems will cause the SPICE(NOTSUPPORTED) error to signal. */

/* $ Files */

/*     Appropriate SPK and PCK kernels must be loaded by the calling */
/*     program before this routine is called. */

/*     The following data are required: */

/*        - SPK data: the calling application must load ephemeris data */
/*          for the targets, observer, and any intermediate objects in */
/*          a chain connecting the targets and observer that cover the */
/*          time period specified by the window CNFINE. If aberration */
/*          corrections are used, the states of target and observer */
/*          relative to the solar system barycenter must be calculable */
/*          from the available ephemeris data. Typically ephemeris data */
/*          are made available by loading one or more SPK files using */
/*          FURNSH. */

/*        - If non-inertial reference frames are used, then PCK */
/*          files, frame kernels, C-kernels, and SCLK kernels may be */
/*          needed. */

/*     Such kernel data are normally loaded once per program run, NOT */
/*     every time this routine is called. */

/* $ Particulars */

/*     This routine provides a simpler, but less flexible interface */
/*     than does the routine GFEVNT for conducting searches for */
/*     surface intercept vector coordinate value events. */
/*     Applications that require support for progress reporting, */
/*     interrupt handling, non-default step or refinement functions, or */
/*     non-default convergence tolerance should call GFEVNT rather than */
/*     this routine. */

/*     This routine determines a set of one or more time intervals */
/*     within the confinement window when the selected coordinate of */
/*     the surface intercept position vector satisfies a caller-specified */
/*     constraint. The resulting set of intervals is returned as a SPICE */
/*     window. */

/*     Below we discuss in greater detail aspects of this routine's */
/*     solution process that are relevant to correct and efficient */
/*     use of this routine in user applications. */


/*     The Search Process */
/*     ================== */

/*     Regardless of the type of constraint selected by the caller, this */
/*     routine starts the search for solutions by determining the time */
/*     periods, within the confinement window, over which the specified */
/*     coordinate function is monotone increasing and monotone */
/*     decreasing. Each of these time periods is represented by a SPICE */
/*     window. Having found these windows, all of the coordinate */
/*     function's local extrema within the confinement window are known. */
/*     Absolute extrema then can be found very easily. */

/*     Within any interval of these "monotone" windows, there will be at */
/*     most one solution of any equality constraint. Since the boundary */
/*     of the solution set for any inequality constraint is contained in */
/*     the union of */

/*        - the set of points where an equality constraint is met */
/*        - the boundary points of the confinement window */

/*     the solutions of both equality and inequality constraints can be */
/*     found easily once the monotone windows have been found. */


/*     Step Size */
/*     ========= */

/*     The monotone windows (described above) are found using a two-step */
/*     search process. Each interval of the confinement window is */
/*     searched as follows: first, the input step size is used to */
/*     determine the time separation at which the sign of the rate of */
/*     change of coordinate will be sampled. Starting at */
/*     the left endpoint of an interval, samples will be taken at each */
/*     step. If a change of sign is found, a root has been bracketed; at */
/*     that point, the time at which the time derivative of the */
/*     coordinate is zero can be found by a refinement process, for */
/*     example, using a binary search. */

/*     Note that the optimal choice of step size depends on the lengths */
/*     of the intervals over which the coordinate function is monotone: */
/*     the step size should be shorter than the shortest of these */
/*     intervals (within the confinement window). */

/*     The optimal step size is *not* necessarily related to the lengths */
/*     of the intervals comprising the result window. For example, if */
/*     the shortest monotone interval has length 10 days, and if the */
/*     shortest result window interval has length 5 minutes, a step size */
/*     of 9.9 days is still adequate to find all of the intervals in the */
/*     result window. In situations like this, the technique of using */
/*     monotone windows yields a dramatic efficiency improvement over a */
/*     state-based search that simply tests at each step whether the */
/*     specified constraint is satisfied. The latter type of search can */
/*     miss solution intervals if the step size is longer than the */
/*     shortest solution interval. */

/*     Having some knowledge of the relative geometry of the target and */
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
/*     interval within which a solution is sought. However, the */
/*     confinement window can, in some cases, be used to make searches */
/*     more efficient. Sometimes it's possible to do an efficient search */
/*     to reduce the size of the time period over which a relatively */
/*     slow search of interest must be performed. */

/*     Practical use of the coordinate search capability would likely */
/*     consist of searches over multiple coordinate constraints to find */
/*     time intervals that satisfies the constraints. An */
/*     effective technique to accomplish such a search is */
/*     to use the result window from one search as the confinement window */
/*     of the next. */

/*     Longitude and Right Ascension */
/*     ============================= */

/*     The cyclic nature of the longitude and right ascension coordinates */
/*     produces branch cuts at +/- 180 degrees longitude and 0-360 */
/*     longitude. Round-off error may cause solutions near these branches */
/*     to cross the branch. Use of the SPICE routine WNCOND will contract */
/*     solution windows by some epsilon, reducing the measure of the */
/*     windows and eliminating the branch crossing. A one millisecond */
/*     contraction will in most cases eliminate numerical round-off */
/*     caused branch crossings. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     The examples shown below require a "standard" set of SPICE */
/*     kernels. We list these kernels in a meta kernel named */
/*     'standard.tm'. */

/*        KPL/MK */

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
/*              de414.bsp                     Planetary ephemeris */
/*              pck00008.tpc                  Planet orientation and */
/*                                            radii */
/*              naif0008.tls                  Leapseconds */


/*           \begindata */

/*           KERNELS_TO_LOAD = ( '/kernels/gen/lsk/naif0008.tls' */
/*                               '/kernels/gen/spk/de414.bsp' */
/*                               '/kernels/gen/pck/pck00008.tpc' */
/*                             ) */


/*     The examples shown below require a frames kernel defining a */
/*     a dynamic frame, Sun-Earth Motion. The frame defined by the */
/*     sun-to-earth direction vector as the X axis. The Y axis in the */
/*     earth orbital plane, and Z completing the right hand system. */

/*     We name this frames kernel "sem.tf". */

/*     \begindata */

/*       FRAME_SEM                     =  10100000 */
/*       FRAME_10100000_NAME           = 'SEM' */
/*       FRAME_10100000_CLASS          =  5 */
/*       FRAME_10100000_CLASS_ID       =  10100000 */
/*       FRAME_10100000_CENTER         =  10 */
/*       FRAME_10100000_RELATIVE       = 'J2000' */
/*       FRAME_10100000_DEF_STYLE      = 'PARAMETERIZED' */
/*       FRAME_10100000_FAMILY         = 'TWO-VECTOR' */
/*       FRAME_10100000_PRI_AXIS       = 'X' */
/*       FRAME_10100000_PRI_VECTOR_DEF = 'OBSERVER_TARGET_POSITION' */
/*       FRAME_10100000_PRI_OBSERVER   = 'SUN' */
/*       FRAME_10100000_PRI_TARGET     = 'EARTH' */
/*       FRAME_10100000_PRI_ABCORR     = 'NONE' */
/*       FRAME_10100000_SEC_AXIS       = 'Y' */
/*       FRAME_10100000_SEC_VECTOR_DEF = 'OBSERVER_TARGET_VELOCITY' */
/*       FRAME_10100000_SEC_OBSERVER   = 'SUN' */
/*       FRAME_10100000_SEC_TARGET     = 'EARTH' */
/*       FRAME_10100000_SEC_ABCORR     = 'NONE' */
/*       FRAME_10100000_SEC_FRAME      = 'J2000' */

/*     Example(1) */

/*     Find the time during 2007 for which the latitude of the */
/*     intercept point of the vector pointing from the sun towards */
/*     the earth in the IAU_EARTH frame equals zero i.e. the intercept */
/*     point crosses the equator. */

/*           PROGRAM GFSNTC_EX1 */
/*           IMPLICIT              NONE */

/*     C */
/*     C     Include GF parameter declarations: */
/*     C */
/*           INCLUDE               'gf.inc' */

/*     C */
/*     C     SPICELIB functions */
/*     C */
/*           DOUBLE PRECISION      SPD */
/*           INTEGER               WNCARD */

/*     C */
/*     C     Local variables and initial parameters. */
/*     C */
/*           INTEGER               LBCELL */
/*           PARAMETER           ( LBCELL = -5 ) */

/*     C */
/*     C     Create 50 windows. */
/*     C */
/*           INTEGER               MAXWIN */
/*           PARAMETER           ( MAXWIN = 1000 ) */

/*     C */
/*     C     One window consists of two intervals. */
/*     C */
/*           INTEGER               NINTRVL */
/*           PARAMETER           ( NINTRVL = MAXWIN *2 ) */

/*           INTEGER               STRLEN */
/*           PARAMETER           ( STRLEN = 64 ) */

/*           CHARACTER*(STRLEN)    BEGSTR */
/*           CHARACTER*(STRLEN)    ENDSTR */
/*           CHARACTER*(STRLEN)    TARGET */
/*           CHARACTER*(STRLEN)    OBSRVR */
/*           CHARACTER*(STRLEN)    DREF */
/*           CHARACTER*(STRLEN)    ABCORR */
/*           CHARACTER*(STRLEN)    METHOD */
/*           CHARACTER*(STRLEN)    FIXREF */
/*           CHARACTER*(STRLEN)    CRDSYS */
/*           CHARACTER*(STRLEN)    COORD */
/*           CHARACTER*(STRLEN)    RELATE */

/*           DOUBLE PRECISION      STEP */
/*           DOUBLE PRECISION      DVEC   ( 3 ) */
/*           DOUBLE PRECISION      CNFINE ( LBCELL : NINTRVL ) */
/*           DOUBLE PRECISION      RESULT ( LBCELL : NINTRVL ) */
/*           DOUBLE PRECISION      WORK   ( LBCELL : NINTRVL, NWMAX ) */


/*           DOUBLE PRECISION      BEGTIM */
/*           DOUBLE PRECISION      ENDTIM */
/*           DOUBLE PRECISION      BEG */
/*           DOUBLE PRECISION      END */
/*           DOUBLE PRECISION      REFVAL */
/*           DOUBLE PRECISION      ADJUST */
/*           INTEGER               COUNT */

/*           INTEGER               I */


/*     C */
/*     C     The SEM frame defines the X axis as always earth pointing. */
/*     C */
/*     C     Define the earth pointing vector in the SEM frame. */
/*     C */
/*           DATA                  DVEC   / 1.D0, 0.D0, 0.D0 / */

/*     C */
/*     C     Load kernels. */
/*     C */
/*           CALL FURNSH ('standard.tm') */
/*           CALL FURNSH ('sem.tf') */

/*     C */
/*     C     Initialize windows RESULT and CNFINE. */
/*     C */
/*           CALL SSIZED ( NINTRVL, RESULT ) */
/*           CALL SSIZED ( 2,      CNFINE ) */

/*     C */
/*     C     Store the time bounds of our search interval in */
/*     C     the CNFINE confinement window. */
/*     C */
/*           CALL STR2ET ( '2007 JAN 01', BEGTIM ) */
/*           CALL STR2ET ( '2008 JAN 01', ENDTIM ) */

/*           CALL WNINSD ( BEGTIM, ENDTIM, CNFINE ) */

/*     C */
/*     C     Search using a step size of 1 day (in units of seconds). */
/*     C */
/*           STEP   = SPD() */

/*     C */
/*     C     Search for a condition where the latitudinal system */
/*     C     coordinate latitude in the IAU_EARTH frame has value zero. */
/*     C     In this case, the pointing vector, 'DVEC', defines the */
/*     C     vector direction pointing at the earth from the sun. */
/*     C */
/*           ADJUST = 0.D0 */
/*           REFVAL = 0.D0 */
/*           TARGET = 'EARTH' */
/*           OBSRVR = 'SUN' */
/*           DREF   = 'SEM' */
/*           METHOD = 'Ellipsoid' */
/*           FIXREF = 'IAU_EARTH' */
/*           CRDSYS = 'LATITUDINAL' */
/*           COORD  = 'LATITUDE' */
/*           RELATE = '=' */

/*     C */
/*     C     Use the same aberration correction flag as that in the SEM */
/*     C     frame definition. */
/*     C */
/*           ABCORR = 'NONE' */

/*           CALL GFSNTC (  TARGET,  FIXREF, */
/*          .               METHOD,  ABCORR, OBSRVR, */
/*          .               DREF,    DVEC, */
/*          .               CRDSYS,  COORD, */
/*          .               RELATE,  REFVAL, */
/*          .               ADJUST,  STEP, CNFINE, */
/*          .               NINTRVL, NWMAX, WORK, RESULT ) */

/*     C */
/*     C     Check the number of intervals in the result window. */
/*     C */
/*           COUNT = WNCARD(RESULT) */

/*     C */
/*     C     List the beginning and ending points in each interval */
/*     C     if RESULT contains data. */
/*     C */
/*           IF ( COUNT .EQ. 0 ) THEN */
/*              WRITE (*, '(A)') 'Result window is empty.' */
/*           ELSE */

/*              DO I = 1, COUNT */

/*     C */
/*     C           Fetch the endpoints of the Ith interval */
/*     C           of the result window. */
/*     C */
/*                 CALL WNFETD ( RESULT, I, BEG, END  ) */

/*                 CALL TIMOUT ( BEG, */
/*          .                'YYYY-MON-DD HR:MN:SC.###### ' */
/*          .      //        '(TDB) ::TDB ::RND',  BEGSTR ) */
/*                 CALL TIMOUT ( END, */
/*          .                'YYYY-MON-DD HR:MN:SC.###### ' */
/*          .      //        '(TDB) ::TDB ::RND',  ENDSTR ) */

/*                 WRITE (*,*) 'Interval ',  I */
/*                 WRITE (*,*) 'Beginning TDB ', BEGSTR */
/*                 WRITE (*,*) 'Ending TDB    ', ENDSTR */

/*              END DO */

/*           END IF */

/*           END */

/*      The program outputs: */

/*         Interval  1 */
/*         Beginning TDB 2007-MAR-21 00:01:25.495120 (TDB) */
/*         Ending TDB    2007-MAR-21 00:01:25.495120 (TDB) */

/*         Interval  2 */
/*         Beginning TDB 2007-SEP-23 09:46:39.574123 (TDB) */
/*         Ending TDB    2007-SEP-23 09:46:39.574123 (TDB) */

/*      Example(2) */

/*      Find the time during 2007 for which the intercept point on the */
/*      earth of the sun-to-earth vector as described in Example 1 in */
/*      the IAU_EARTH frame lies within a geodetic latitude-longitude */
/*      "box" defined as */

/*         16 degrees <= latitude  <= 17 degrees */
/*         85 degrees <= longitude <= 86 degrees */

/*      This problem requires four searches, each search on one of the */
/*      box restrictions. The user needs also realize the temporal */
/*      behavior of latitude greatly differs from that of the longitude. */
/*      The intercept latitude varies between approximately 23.44 degrees */
/*      and -23.44 degrees during the year. The intercept longitude */
/*      varies between -180 degrees and 180 degrees in one day. */

/*           PROGRAM GFSNTC_EX2 */
/*           IMPLICIT              NONE */

/*     C */
/*     C     Include GF parameter declarations: */
/*     C */
/*           INCLUDE               'gf.inc' */

/*     C */
/*     C     SPICELIB functions */
/*     C */
/*           DOUBLE PRECISION      SPD */
/*           DOUBLE PRECISION      RPD */
/*           INTEGER               WNCARD */

/*     C */
/*     C     Local variables and initial parameters. */
/*     C */
/*           INTEGER               LBCELL */
/*           PARAMETER           ( LBCELL = -5 ) */

/*     C */
/*     C     Create 50 windows. */
/*     C */
/*           INTEGER               MAXWIN */
/*           PARAMETER           ( MAXWIN = 1000 ) */

/*     C */
/*     C     One window consists of two intervals. */
/*     C */
/*           INTEGER               NINTRVL */
/*           PARAMETER           ( NINTRVL = MAXWIN *2 ) */

/*           INTEGER               STRLEN */
/*           PARAMETER           ( STRLEN = 64 ) */

/*           CHARACTER*(STRLEN)    BEGSTR */
/*           CHARACTER*(STRLEN)    ENDSTR */
/*           CHARACTER*(STRLEN)    TARGET */
/*           CHARACTER*(STRLEN)    OBSRVR */
/*           CHARACTER*(STRLEN)    DREF */
/*           CHARACTER*(STRLEN)    ABCORR */
/*           CHARACTER*(STRLEN)    METHOD */
/*           CHARACTER*(STRLEN)    FIXREF */
/*           CHARACTER*(STRLEN)    CRDSYS */
/*           CHARACTER*(STRLEN)    COORD */
/*           CHARACTER*(STRLEN)    RELATE */

/*           DOUBLE PRECISION      STEP */
/*           DOUBLE PRECISION      DVEC    ( 3 ) */
/*           DOUBLE PRECISION      CNFINE  ( LBCELL : NINTRVL ) */
/*           DOUBLE PRECISION      RESULT1 ( LBCELL : NINTRVL ) */
/*           DOUBLE PRECISION      RESULT2 ( LBCELL : NINTRVL ) */
/*           DOUBLE PRECISION      RESULT3 ( LBCELL : NINTRVL ) */
/*           DOUBLE PRECISION      RESULT4 ( LBCELL : NINTRVL ) */
/*           DOUBLE PRECISION      WORK    ( LBCELL : NINTRVL, NWMAX ) */


/*           DOUBLE PRECISION      BEGTIM */
/*           DOUBLE PRECISION      ENDTIM */
/*           DOUBLE PRECISION      BEG */
/*           DOUBLE PRECISION      END */
/*           DOUBLE PRECISION      REFVAL */
/*           DOUBLE PRECISION      ADJUST */

/*           INTEGER               COUNT */
/*           INTEGER               I */


/*     C */
/*     C     The SEM frame defines the X axis as always earth pointing. */
/*     C */
/*     C     Define the earth pointing vector in the SEM frame. */
/*     C */
/*           DATA                  DVEC   / 1.D0, 0.D0, 0.D0 / */

/*     C */
/*     C     Load kernels. */
/*     C */
/*           CALL FURNSH ('standard.tm') */
/*           CALL FURNSH ('sem.tf') */

/*     C */
/*     C     Initialize windows RESULT and CNFINE. */
/*     C */
/*           CALL SSIZED ( NINTRVL, RESULT1 ) */
/*           CALL SSIZED ( NINTRVL, RESULT2 ) */
/*           CALL SSIZED ( NINTRVL, RESULT3 ) */
/*           CALL SSIZED ( NINTRVL, RESULT4 ) */
/*           CALL SSIZED ( 2,      CNFINE ) */

/*     C */
/*     C     Store the time bounds of our search interval in */
/*     C     the CNFINE confinement window. */
/*     C */
/*           CALL STR2ET ( '2007 JAN 01', BEGTIM ) */
/*           CALL STR2ET ( '2008 JAN 01', ENDTIM ) */

/*           CALL WNINSD ( BEGTIM, ENDTIM, CNFINE ) */

/*     C */
/*     C     The latitude varies relatively slowly, ~46 degrees during */
/*     C     the year. The extrema occur approximately every six months. */
/*     C     Search using a step size less than half that value */
/*     C     (180 days). For this example use ninety days (in units */
/*     C     of seconds). */
/*     C */
/*           STEP   = SPD()*90.D0 */

/*     C */
/*     C     Perform four searches to determine the times when the */
/*     C     latitude-longitude box restriction conditions apply. In */
/*     C     this case, the pointing vector, 'DVEC', defines the vector */
/*     C     direction pointing at the earth from the sun. */
/*     C */
/*     C     Use geodetic coordinates. */
/*     C */
/*           ADJUST = 0.D0 */
/*           TARGET = 'EARTH' */
/*           OBSRVR = 'SUN' */
/*           DREF   = 'SEM' */
/*           METHOD = 'Ellipsoid' */
/*           FIXREF = 'IAU_EARTH' */
/*           CRDSYS = 'GEODETIC' */


/*     C */
/*     C     Use the same aberration correction flag as that in the SEM */
/*     C     frame definition. */
/*     C */
/*           ABCORR = 'NONE' */

/*     C */
/*     C     Perform the searches such that the result window of a search */
/*     C     serves as the confinement window of the subsequent search. */
/*     C */

/*     C */
/*     C     Since the latitude coordinate varies slowly and is well */
/*     C     behaved over the time of the confinement window, search */
/*     C     first for the windows satisfying the latitude requirements, */
/*     C     then use that result as confinement for the longitude */
/*     C     search. */
/*     C */
/*           COORD  = 'LATITUDE' */
/*           REFVAL = 16.D0 * RPD() */
/*           RELATE = '>' */

/*           CALL GFSNTC (  TARGET,  FIXREF, */
/*          .               METHOD,  ABCORR, OBSRVR, */
/*          .               DREF,    DVEC, */
/*          .               CRDSYS,  COORD, */
/*          .               RELATE,  REFVAL, */
/*          .               ADJUST,  CNFINE, STEP, */
/*          .               NINTRVL, NWMAX, WORK, RESULT1 ) */

/*           REFVAL = 17.D0 * RPD() */
/*           RELATE = '<' */

/*           CALL GFSNTC (  TARGET,  FIXREF, */
/*          .               METHOD,  ABCORR, OBSRVR, */
/*          .               DREF,    DVEC, */
/*          .               CRDSYS,  COORD, */
/*          .               RELATE,  REFVAL, */
/*          .               ADJUST,  RESULT1, STEP, */
/*          .               NINTRVL, NWMAX, WORK, RESULT2 ) */


/*     C */
/*     C     Now the longitude search. */
/*     C */
/*           COORD  = 'LONGITUDE' */

/*     C */
/*     C     Reset the stepsize to something appropriate for the 360 */
/*     C     degrees in 24 hours domain. The longitude shows near */
/*     C     linear behavior so use a stepsize less than half the period */
/*     C     of twelve hours. Ten hours will suffice in this case. */
/*     C */
/*           STEP   = SPD() * (10.D0/24.D0) */

/*           REFVAL = 85.D0 * RPD() */
/*           RELATE = '>' */

/*           CALL GFSNTC (  TARGET,  FIXREF, */
/*          .               METHOD,  ABCORR, OBSRVR, */
/*          .               DREF,    DVEC, */
/*          .               CRDSYS,  COORD, */
/*          .               RELATE,  REFVAL, */
/*          .               ADJUST,  RESULT2, STEP, */
/*          .               NINTRVL, NWMAX, WORK, RESULT3 ) */

/*     C */
/*     C     Contract the endpoints of each window to account */
/*     C     for possible round-off error at the -180/180 degree branch. */
/*     C */
/*     C     A contraction value of a millisecond should eliminate */
/*     C     any round-off caused branch crossing. */
/*     C */
/*           CALL WNCOND ( 1D-3, 1D-3, RESULT3 ) */

/*           REFVAL = 86.D0 * RPD() */
/*           RELATE = '<' */

/*           CALL GFSNTC (  TARGET,  FIXREF, */
/*          .               METHOD,  ABCORR, OBSRVR, */
/*          .               DREF,    DVEC, */
/*          .               CRDSYS,  COORD, */
/*          .               RELATE,  REFVAL, */
/*          .               ADJUST,  RESULT3, STEP, */
/*          .               NINTRVL, NWMAX, WORK, RESULT4 ) */

/*     C */
/*     C     Check the number of intervals in the result window. */
/*     C */
/*           COUNT = WNCARD(RESULT4) */

/*     C */
/*     C     List the beginning and ending points in each interval */
/*     C     if RESULT contains data. */
/*     C */
/*           IF ( COUNT .EQ. 0 ) THEN */
/*              WRITE(*, '(A)') 'Result window is empty.' */
/*           ELSE */

/*              DO I = 1, COUNT */

/*     C */
/*     C           Fetch the endpoints of the Ith interval */
/*     C           of the result window. */
/*     C */
/*                 CALL WNFETD ( RESULT4, I, BEG, END  ) */

/*                 CALL TIMOUT ( BEG, */
/*          .                'YYYY-MON-DD HR:MN:SC.###### ' */
/*          .      //        '(TDB) ::TDB ::RND',  BEGSTR ) */
/*                 CALL TIMOUT ( END, */
/*          .                'YYYY-MON-DD HR:MN:SC.###### ' */
/*          .      //        '(TDB) ::TDB ::RND',  ENDSTR ) */

/*                 WRITE(*,*) 'Interval ',  I */
/*                 WRITE(*,*) 'Beginning TDB ', BEGSTR */
/*                 WRITE(*,*) 'Ending TDB    ', ENDSTR */
/*                 WRITE(*,*) ' ' */

/*              END DO */

/*           END IF */

/*           END */

/*      The program outputs: */

/*        Interval  1 */
/*        Beginning TDB 2007-MAY-05 06:14:04.637735 (TDB) */
/*        Ending TDB    2007-MAY-05 06:18:03.621908 (TDB) */

/*        Interval  2 */
/*        Beginning TDB 2007-MAY-06 06:13:59.583484 (TDB) */
/*        Ending TDB    2007-MAY-06 06:17:58.569239 (TDB) */

/*        Interval  3 */
/*        Beginning TDB 2007-MAY-07 06:13:55.102940 (TDB) */
/*        Ending TDB    2007-MAY-07 06:17:54.090299 (TDB) */

/*        Interval  4 */
/*        Beginning TDB 2007-AUG-06 06:23:17.282927 (TDB) */
/*        Ending TDB    2007-AUG-06 06:27:16.264009 (TDB) */

/*        Interval  5 */
/*        Beginning TDB 2007-AUG-07 06:23:10.545441 (TDB) */
/*        Ending TDB    2007-AUG-07 06:27:09.524926 (TDB) */

/*        Interval  6 */
/*        Beginning TDB 2007-AUG-08 06:23:03.233996 (TDB) */
/*        Ending TDB    2007-AUG-08 06:27:02.211889 (TDB) */

/*        Interval  7 */
/*        Beginning TDB 2007-AUG-09 06:22:55.351256 (TDB) */
/*        Ending TDB    2007-AUG-09 06:26:54.327566 (TDB) */

/* $ Restrictions */

/*     1) The kernel files to be used by this routine must be loaded */
/*        (normally using the SPICELIB routine FURNSH) before this */
/*        routine is called. */

/*     2) This routine has the side effect of re-initializing the */
/*        coordinate quantity utility package.  Callers may */
/*        need to re-initialize the package after calling this routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */
/*     E.D. Wright    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 05-SEP-2012 (EDW) */

/*        Edit to comments to correct search description. */

/*        Implemented use of ZZHOLDD to allow user to alter convergence */
/*        tolerance. */

/*        Removed the STEP > 0 error check. The GFSSTP call includes */
/*        the check. */

/* -    SPICELIB Version 1.0.1, 16-FEB-2010 (NJB) (EDW) */

/*        Edits to and corrections of argument descriptions and */
/*        header. */

/* -    SPICELIB Version 1.0.0, 17-FEB-2009 (NJB) (EDW) */

/* -& */
/* $ Index_Entries */

/*     GF surface intercept coordinate search */

/* -& */

/*     SPICELIB functions */


/*     Routines to set step size, refine transition times */
/*     and report work. */


/*     Local parameters */


/*     Local variables */


/*     Quantity definition parameter arrays: */


/*     Standard SPICE error handling. */

    /* Parameter adjustments */
    work_dim1 = *mw + 6;
    work_offset = work_dim1 - 5;

    /* Function Body */
    if (return_()) {
	return 0;
    }

/*     Check into the error subsystem. */

    chkin_("GFSNTC", (ftnlen)6);

/*     Confirm minimum window sizes. */

    if (*mw < 2 || ! even_(mw)) {
	setmsg_("Workspace window size was #; size must be at least 2 and an"
		" even value.", (ftnlen)71);
	errint_("#", mw, (ftnlen)1);
	sigerr_("SPICE(INVALIDDIMENSION)", (ftnlen)23);
	chkout_("GFSNTC", (ftnlen)6);
	return 0;
    }
    if (sized_(result) < 2) {
	setmsg_("Result window size was #; size must be at least 2.", (ftnlen)
		50);
	i__1 = sized_(result);
	errint_("#", &i__1, (ftnlen)1);
	sigerr_("SPICE(INVALIDDIMENSION)", (ftnlen)23);
	chkout_("GFSNTC", (ftnlen)6);
	return 0;
    }

/*     Set up a call to GFEVNT specific to the surface intercept */
/*     coordinate search. */

    s_copy(qpnams, "TARGET", (ftnlen)80, (ftnlen)6);
    s_copy(qcpars, target, (ftnlen)80, target_len);
    s_copy(qpnams + 80, "OBSERVER", (ftnlen)80, (ftnlen)8);
    s_copy(qcpars + 80, obsrvr, (ftnlen)80, obsrvr_len);
    s_copy(qpnams + 160, "ABCORR", (ftnlen)80, (ftnlen)6);
    s_copy(qcpars + 160, abcorr, (ftnlen)80, abcorr_len);
    s_copy(qpnams + 240, "COORDINATE SYSTEM", (ftnlen)80, (ftnlen)17);
    s_copy(qcpars + 240, crdsys, (ftnlen)80, crdsys_len);
    s_copy(qpnams + 320, "COORDINATE", (ftnlen)80, (ftnlen)10);
    s_copy(qcpars + 320, coord, (ftnlen)80, coord_len);
    s_copy(qpnams + 400, "REFERENCE FRAME", (ftnlen)80, (ftnlen)15);
    s_copy(qcpars + 400, fixref, (ftnlen)80, fixref_len);
    s_copy(qpnams + 480, "VECTOR DEFINITION", (ftnlen)80, (ftnlen)17);
    s_copy(qcpars + 480, "SURFACE INTERCEPT POINT", (ftnlen)80, (ftnlen)23);
    s_copy(qpnams + 560, "METHOD", (ftnlen)80, (ftnlen)6);
    s_copy(qcpars + 560, method, (ftnlen)80, method_len);
    s_copy(qpnams + 640, "DREF", (ftnlen)80, (ftnlen)4);
    s_copy(qcpars + 640, dref, (ftnlen)80, dref_len);
    s_copy(qpnams + 720, "DVEC", (ftnlen)80, (ftnlen)4);
    qdpars[0] = dvec[0];
    qdpars[1] = dvec[1];
    qdpars[2] = dvec[2];

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

/*     Look for solutions. */

/*     Progress report and interrupt options are set to .FALSE. */

    gfevnt_((U_fp)gfstep_, (U_fp)gfrefn_, "COORDINATE", &c__10, qpnams, 
	    qcpars, qdpars, qipars, qlpars, relate, refval, &tol, adjust, 
	    cnfine, &c_false, (U_fp)gfrepi_, (U_fp)gfrepu_, (U_fp)gfrepf_, mw,
	     nw, work, &c_false, (L_fp)gfbail_, result, (ftnlen)10, (ftnlen)
	    80, (ftnlen)80, relate_len);
    chkout_("GFSNTC", (ftnlen)6);
    return 0;
} /* gfsntc_ */

