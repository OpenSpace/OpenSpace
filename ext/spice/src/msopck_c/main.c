/* msopck.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__1 = 1;
static doublereal c_b142 = 0.;
static integer c__3 = 3;
static doublereal c_b605 = 1.;
static integer c__4 = 4;
static doublereal c_b968 = .5;
static integer c__300 = 300;

/* $Program  MSOPCK ( make CK 1, 2 or 3 from quats, angs or mats) */
/* Main program */ MAIN__(void)
{
    /* System generated locals */
    address a__1[2];
    integer i__1[2], i__2, i__3, i__4, i__5;
    doublereal d__1, d__2, d__3;
    char ch__1[110], ch__2[286];
    cllist cl__1;
    alist al__1;

    /* Builtin functions */
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen),
	     s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_stop(char *, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), i_dnnt(doublereal *), 
	    f_clos(cllist *), f_rew(alist *);

    /* Local variables */
    extern /* Subroutine */ int vadd_(doublereal *, doublereal *, doublereal *
	    );
    static integer scid;
    extern /* Subroutine */ int ckw01_(integer *, doublereal *, doublereal *, 
	    integer *, char *, logical *, char *, integer *, doublereal *, 
	    doublereal *, doublereal *, ftnlen, ftnlen), ckw02_(integer *, 
	    doublereal *, doublereal *, integer *, char *, char *, integer *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, ftnlen, ftnlen);
    static char line[265];
    extern /* Subroutine */ int ckw03_(integer *, doublereal *, doublereal *, 
	    integer *, char *, logical *, char *, integer *, doublereal *, 
	    doublereal *, doublereal *, integer *, doublereal *, ftnlen, 
	    ftnlen);
    static doublereal hmat[9]	/* was [3][3] */;
    static char lskf[265];
    static doublereal tvec[6];
    static integer hlun;
    extern /* Subroutine */ int mequ_(doublereal *, doublereal *), vscl_(
	    doublereal *, doublereal *, doublereal *);
    static char word[40];
    static doublereal avvs[300000]	/* was [3][100000] */, savv[3];
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *), mtxv_(
	    doublereal *, doublereal *, doublereal *), sce2c_(integer *, 
	    doublereal *, doublereal *), linrot_m__(doublereal *, doublereal *
	    , doublereal *, doublereal *, doublereal *), zzmckdmp_(char *, 
	    ftnlen), sct2e_(integer *, doublereal *, doublereal *), eul2m_(
	    doublereal *, doublereal *, doublereal *, integer *, integer *, 
	    integer *, doublereal *);
    static integer i__, j, n;
    extern /* Subroutine */ int scld01_(char *, integer *, integer *, integer 
	    *, doublereal *, ftnlen), spcac_(integer *, integer *, char *, 
	    char *, ftnlen, ftnlen);
    static char segid[40];
    extern /* Subroutine */ int chkin_(char *, ftnlen), scli01_(char *, 
	    integer *, integer *, integer *, integer *, ftnlen);
    static char hline[265];
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen);
    static char sclkf[265];
    static doublereal hrate;
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen), 
	    repmc_(char *, char *, char *, char *, ftnlen, ftnlen, ftnlen, 
	    ftnlen);
    static integer index;
    static logical chkto;
    static char eaxis[1*3];
    extern doublereal dpmax_(void);
    static logical found;
    extern doublereal dpmin_(void);
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    static doublereal rates[100000];
    extern integer wdcnt_(char *, ftnlen);
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen);
    static doublereal hquat[4];
    static char hword[40];
    static doublereal tmpdp;
    static char dtype[40];
    static integer selun;
    extern /* Subroutine */ int vhatg_(doublereal *, integer *, doublereal *),
	     ckopn_(char *, char *, integer *, integer *, ftnlen, ftnlen);
    static char error[265];
    static integer silun, nints;
    static doublereal normq, quats[400000]	/* was [4][100000] */, squat[
	    4], prevt;
    extern integer rtrim_(char *, ftnlen);
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int xpose_(doublereal *, doublereal *);
    static char ttype[40];
    extern /* Subroutine */ int dpfmt_(doublereal *, char *, char *, ftnlen, 
	    ftnlen), repmf_(char *, char *, doublereal *, integer *, char *, 
	    char *, ftnlen, ftnlen, ftnlen, ftnlen);
    static doublereal stopt[100000], sstpt;
    extern /* Subroutine */ int ck3sdn_(doublereal *, logical *, integer *, 
	    doublereal *, doublereal *, doublereal *, integer *, doublereal *,
	     doublereal *, integer *);
    static logical found1, found2;
    static doublereal tmpdp1, tmpdp2;
    static logical addtab;
    extern /* Subroutine */ int str2et_(char *, doublereal *, ftnlen);
    static integer handle, badcnt;
    static logical arflag, badqua;
    static char framef[265], ifname[80];
    static doublereal offang[3], qn[4], earate, clkfrc;
    static char fsclkf[265], dashln[80];
    static doublereal eulang[3];
    static char templt[80*49], prodid[265], cmmntf[265], inputf[265], linesv[
	    265], outptf[265], setupf[265], frmnam[40], astrln[80], offaxs[1*
	    3];
    static doublereal intrvl, offmat[9]	/* was [3][3] */, qerror, rerror[3], 
	    startt[100000], hdparr[100000], sclkdp, begtim, endtim, sstrtt, 
	    timcor, sdntol, sclkmd[2], sclkof[2], clklft, clkrgh, curmat[9]	
	    /* was [3][3] */, prvmat[9]	/* was [3][3] */, nxtmat[9]	/* 
	    was [3][3] */, scldav[3], tmpvec[3], tmpmat[9]	/* was [3][3] 
	    */;
    static integer eulaxs[3], offaxi[3];
    static doublereal mat[9]	/* was [3][3] */;
    static integer iorder[100000], cktype, inplun, instid, lcount, scrtch, 
	    srtidx, stpidx, nfilds, nitems;
    static logical angrat, insarf, appndf, eof, ckopnd, badrat, seopnd, 
	    siopnd;
    static integer ptr;
    static logical cmmflg, offrot, savbtm, muarat, avgrat, wrtseg, eulbod, 
	    dnsmpl;
    extern integer sctype_(integer *);
    extern logical exists_(char *, ftnlen);
    extern integer frstnp_(char *, ftnlen), pos_(char *, char *, integer *, 
	    ftnlen, ftnlen), isrchd_(doublereal *, integer *, doublereal *);
    extern doublereal vnormg_(doublereal *, integer *), rpd_(void);
    static logical qfilter, rfilter;
    extern /* Subroutine */ int errprt_(char *, char *, ftnlen, ftnlen), 
	    tkvrsn_(char *, char *, ftnlen, ftnlen), tostdo_(char *, ftnlen), 
	    getcml_(char *, ftnlen), prefix_(char *, integer *, char *, 
	    ftnlen, ftnlen), suffix_(char *, integer *, char *, ftnlen, 
	    ftnlen), nextwd_(char *, char *, char *, ftnlen, ftnlen, ftnlen), 
	    setmsg_(char *, ftnlen), sigerr_(char *, ftnlen), getfat_(char *, 
	    char *, char *, ftnlen, ftnlen, ftnlen), furnsh_(char *, ftnlen), 
	    gcpool_(char *, integer *, integer *, integer *, char *, logical *
	    , ftnlen, ftnlen), gipool_(char *, integer *, integer *, integer *
	    , integer *, logical *, ftnlen), ckmeta_(integer *, char *, 
	    integer *, ftnlen), errint_(char *, integer *, ftnlen), mkfclk_(
	    char *, integer *, doublereal *, char *, ftnlen, ftnlen), namfrm_(
	    char *, integer *, ftnlen), gdpool_(char *, integer *, integer *, 
	    integer *, doublereal *, logical *, ftnlen), dtpool_(char *, 
	    logical *, integer *, char *, ftnlen, ftnlen), txtopr_(char *, 
	    integer *, ftnlen), readln_(integer *, char *, logical *, ftnlen),
	     scencd_(integer *, char *, doublereal *, ftnlen), nparsd_(char *,
	     doublereal *, char *, integer *, ftnlen, ftnlen), rmaind_(
	    doublereal *, doublereal *, doublereal *, doublereal *), m2q_(
	    doublereal *, doublereal *), txtops_(integer *), writln_(char *, 
	    integer *, ftnlen), q2m_(doublereal *, doublereal *), mxm_(
	    doublereal *, doublereal *, doublereal *), orderd_(doublereal *, 
	    integer *, integer *), reordd_(integer *, integer *, doublereal *)
	    , timout_(doublereal *, char *, char *, ftnlen, ftnlen), dafopw_(
	    char *, integer *, ftnlen), cputim_(doublereal *), dafcls_(
	    integer *), chkout_(char *, ftnlen);

/* $ Abstract */

/*     This program creates a type 1, 2 or 3 CK file from an input text */
/*     file containing orientation provided as quaternions, Euler */
/*     angles, or matrices and angular rates. */

/*     Program usage: */

/*       > msopck <setup_file> <input_data_file_name> <output_ck_name> */

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

/* $ Files */

/*     This program takes setup information from a setup file in */
/*     test kernel file format. Example of a setup file is below: */

/*         \begindata */

/*            LSK_FILE_NAME           = 'LSK file name' */

/*            SCLK_FILE_NAME          = 'SCLK file name' */

/*            MAKE_FAKE_SCLK          = 'fake SCLK file name' */

/*            FRAMES_FILE_NAME        = 'FK file name' */

/*            INTERNAL_FILE_NAME      = 'internal file name string' */

/*            COMMENTS_FILE_NAME      = 'file containing comments' */

/*            CK_TYPE                 = 1, 2 or 3 */

/*            CK_SEGMENT_ID           = 'segment ID string' */

/*            INSTRUMENT_ID           = NAIF instrument ID */

/*            REFERENCE_FRAME_NAME    = 'reference frame name' */

/*            ANGULAR_RATE_PRESENT    = 'YES', 'NO', 'MAKE UP' or */
/*                                      'MAKE UP/NO AVERAGING' */

/*            QUATERNION_NORM_ERROR   = maximum normalization error */

/*            ANGULAR_RATE_THRESHOLD  = ( max X rate, rad/sec */
/*                                        max Y rate, rad/sec */
/*                                        max Z rate, rad/sec ) */

/*            MAXIMUM_VALID_INTERVAL  = interval length, seconds */

/*            TIME_CORRECTION         = bias to be appiled to input */
/*                                      time tags, seconds */

/*            INPUT_DATA_TYPE         = 'MSOP QUATERNIONS', */
/*                                      'SPICE QUATERNIONS' */
/*                                      'EULER ANGLES' or */
/*                                      'MATRICES' */

/*            INPUT_TIME_TYPE         = 'SCLK', 'UTC', 'TICKS', */
/*                                      'DSCLK', or 'ET' */

/*            EULER_ROTATIONS_ORDER   = order of rotation as 3 element */
/*                                      array; ex: ( 'X' 'Y' 'Z' ) */

/*            EULER_ROTATIONS_TYPE    = 'SPACE' (default) or 'BODY' */

/*            ANGULAR_RATE_FRAME      = 'REFERENCE' or 'INSTRUMENT' */

/*            EULER_ANGLE_UNITS       = 'DEGREES' or 'RADIANS' */

/*            OFFSET_ROTATION_ANGLES  = 3 angles, ex: ( 0.5, 0.1, 0.1 ) */

/*            OFFSET_ROTATION_AXES    = 3 axes, ex: ( 'X' 'Y' 'Z' ) */

/*            OFFSET_ROTATION_UNITS   = 'DEGREES' or 'RADIANS' */

/*            DOWN_SAMPLE_TOLERANCE   = down sampling tolerance, */
/*                                      in radians */

/*            INCLUDE_INTERVAL_TABLE  = 'YES' (default) or 'NO' */

/*            CHECK_TIME_ORDER        = 'YES' or 'NO' (default) */

/*            PRODUCER_ID             = 'producer group/person name' */

/*         \begintext */

/*     where: */

/*            LSK_FILE_NAME           leapseconds file name. Required */
/*                                    input. */

/*            SCLK_FILE_NAME          SCLK file name. Required input. */
/*                                    If not provided, then */
/*                                    MAKE_FAKE_SCLK keyword must be */
/*                                    present. */

/*            MAKE_FAKE_SCLK          Name of the fake SCLK file to be */
/*                                    made by the program. Must be */
/*                                    present if SCLK_FILE_NAME keyword */
/*                                    was not provided. */

/*            FRAMES_FILE_NAME        frames file name; this setup */
/*                                    parameter is optional and must */
/*                                    be provided only if orientation */
/*                                    is given relative to a user- */
/*                                    defined frame */

/*            INTERNAL_FILE_NAME      internal file name as 80-chars */
/*                                    long string; optional, if not */
/*                                    present, set automatically to */
/*                                    default value */

/*            COMMENTS_FILE_NAME      file containing comments to */
/*                                    add to the comment area */
/*                                    of an output CK file. Optional, */
/*                                    input, but if not present, then */
/*                                    only defaults are written to CK */
/*                                    comment area. */

/*            CK_TYPE                 output CK segment type, can be */
/*                                    1, 2 or 3. Required input. */

/*            CK_SEGMENT_ID           40-char segment ID string; */
/*                                    optional, if not present set to */
/*                                    default value. */

/*            INSTRUMENT_ID           the s/c structure NAIF ID of */
/*                                    the orientation data to write */
/*                                    to the CK file. Required input. */

/*            REFERENCE_FRAME_NAME    reference frame name for */
/*                                    orientation data. Required input. */

/*            ANGULAR_RATE_PRESENT    angular rates state flag, possible */
/*                                    values: 'YES', 'NO', 'MAKE UP', */
/*                                    or 'MAKE UP/NO AVERAGING''MAKE UP' */
/*                                    and 'MAKE UP/NO AVERAGING' apply */
/*                                    only for output CK file types 2 */
/*                                    or 3 */

/*                                    'MAKE UP' and */
/*                                    'MAKE UP/NO AVERAGING', */
/*                                    have the same effect for type 2 */
/*                                    CKs, in which manufactured rates */
/*                                    will be consistent with */
/*                                    orientation but not continuous */
/*                                    in time */

/*                                    'MAKE UP' creates angular rates for */
/*                                    type 3 CKs by using three */
/*                                    consecutive quaternions, */
/*                                    calculating the angular rate */
/*                                    between the first and second, */
/*                                    then the second and third, then */
/*                                    averaging the angular rates, */
/*                                    assigning this average as the */
/*                                    angular rate for the second */
/*                                    (middle) quaternion */

/*                      x-------------x------------x */
/*                      Q1            Q2           Q3 */

/*             angular_rate   = angular rate from Q1 to Q2 */
/*                         12 */

/*             angular_rate   = angular rate from Q2 to Q3 */
/*                         23 */
/*             so */

/*             angular_rate   =  (1/2)(angular_rate   + angular_rate  ) */
/*                         Q2                      12               23 */

/*                                    'MAKE UP/NO AVERAGING' creates */
/*                                    angular rates for type 3 CKs by */
/*                                    assigning the angular rate derived */
/*                                    from the second and third */
/*                                    quaternions to the second */
/*                                    quaternion; angular rates made */
/*                                    up for type 3 CK are continuous */
/*                                    but not consistent with */
/*                                    orientation change */

/*                      x-------------x------------x */
/*                      Q1            Q2           Q3 */

/*             angular_rate   = angular rate from Q2 to Q3 */
/*                         23 */
/*             so */

/*             angular_rate   = angular_rate */
/*                         Q2               23 */

/*                                    Do not expect angular rates */
/*                                    created with the 'MAKE UP' flag */
/*                                    to accurately represent reality. */

/*            QUATERNION_NORM_ERROR   DP number specifying maximum */
/*                                    normalization error for input */
/*                                    quaternions, optional, but if not */
/*                                    present, no filtering occurs */

/*            ANGULAR_RATE_FRAME      specifies whether angular rates */
/*                                    are given in the 'REFERENCE' */
/*                                    frame or 'INSTRUMENT' frame, */
/*                                    optional, bit if not present, */
/*                                    assumed as 'REFERENCE' */

/*            ANGULAR_RATE_THRESHOLD  three DP number specifying */
/*                                    threshold for maximum X, Y and Z */
/*                                    angular rates in radians per */
/*                                    seconds, optional, but if not */
/*                                    present, no filtering occurs */

/*            MAXIMUM_VALID_INTERVAL  maximum interval between two */
/*                                    adjacent input points for which */
/*                                    interpolation still allowed, in */
/*                                    seconds, optional, but if not */
/*                                    present, interpolation allowed in */
/*                                    the whole coverage interval */

/*            TIME_CORRECTION         signed number of seconds */
/*                                    representing fixed correction to */
/*                                    be added to each input time tag, */
/*                                    optional, no correction applied */
/*                                    if not provided */

/*            INPUT_DATA_TYPE         type of input data; can be one of */
/*                                    'MSOP QUATERNIONS', 'SPICE */
/*                                    QUATERNIONS', 'EULER ANGLES' or */
/*                                    'MATRICES'. Required input. */

/*            INPUT_TIME_TYPE         input time tags type, possible */
/*                                    values: 'SCLK', 'UTC', 'TICKS', */
/*                                    'DSCLK', or 'ET' */

/*            EULER_ROTATIONS_ORDER   order of rotation for Euler angles */
/*                                    input; must be provided when */
/*                                    INPUT_DATA_TYPE is 'EULER */
/*                                    ANGLES'; must be a 3 element like */
/*                                    ( 'X' 'Y' 'Z' ) or ( 1, 2, 3 ) */

/*            EULER_ROTATIONS_TYPE    defines the rotation type for Euler */
/*                                    rotation: 'BODY' or 'SPACE', */
/*                                    function defaults to 'SPACE' */

/*            EULER_ANGLE_UNITS       specifies units for input */
/*                                    euler angles: 'DEGREES' or */
/*                                    'RADIANS'; must be present in */
/*                                    INPUT_DATA_TYPE = 'EULER ANGLES'. */

/*            OFFSET_ROTATION_ANGLES  Euler angles specifying fixed */
/*                                    rotation from CK reference frame */
/*                                    to the reference frame relative */
/*                                    to which input orientation is */
/*                                    given, optional, but if present, */
/*                                    companion keywords must */
/*                                    also exist with three elements, */
/*                                    e.g. ( 0.5, 0.2, 0.1 ) */

/*            OFFSET_ROTATION_AXES    corresponding rotation axis for */
/*                                    angles in OFFSET_ROTATION_ANGLES */
/*                                    keyword; must have three elements */
/*                                    ( 'X', 'Y', 'Z' ) or ( 1, 2, 3 ), */
/*                                    optional, but if present, companion */
/*                                    keywords must also exist */

/*            OFFSET_ROTATION_UNITS   units for OFFSET_ROTATION_ANGLES */
/*                                    angles: 'DEGREES' or 'RADIANS', */
/*                                    optional, but if present, companion */
/*                                    keywords must also exist */

/*            DOWN_SAMPLE_TOLERANCE   angle, in radians, representing */
/*                                    maximum rotation difference */
/*                                    between orientation provided by */
/*                                    the data points eliminated from */
/*                                    input data stream and orientation */
/*                                    computed by interpolating using CK */
/*                                    type 3 algorithm between data */
/*                                    points written to the output CK */
/*                                    file; if this keyword is present, */
/*                                    MSOPCK creates output type 3 CK */
/*                                    that includes only a subset of the */
/*                                    input data points, resulting in */
/*                                    smaller output CK file */
/*                                    ``matching'' the input attitude to */
/*                                    specified tolerance; optional, no */
/*                                    down sampling is performed if not */
/*                                    provided; ignored if provided for */
/*                                    output CK types other than type 3 */

/*            INCLUDE_INTERVAL_TABLE  optional flag indicating whether */
/*                                    the interpolation interval table */
/*                                    is to be included into the comment */
/*                                    area of the output CK and program's */
/*                                    screen output ('YES', default) or */
/*                                    not ('NO'). */

/*            CHECK_TIME_ORDER        optional flag indicating whether */
/*                                    the program should check if the */
/*                                    input data records are */
/*                                    time-ordered and report an error */
/*                                    if not. The check is done if this */
/*                                    keyword is set to 'YES'. If the */
/*                                    keyword is set to 'NO' or omitted */
/*                                    the check is not done. */

/*            PRODUCER_ID             name of a group or person who */
/*                                    created the file */

/* $ Author_and_Institution */

/*     B.V.Semenov, NAIF/JPL */

/* $ Version */

/* -    Version 6.2.0, 2013-07-01 (BVS) */

/*        BUG FIX: changed the algorithm that determines the clock rate */
/*        to be stored in type 2 output segments to use the average rate */
/*        computed from the input record endpoints rather than the */
/*        average rate computed by averaging all rate values fetched */
/*        from the loaded SCLK coefficient table. */

/*        BUG FIX: to prevent roundoff errors for clocks with very fine */
/*        ticks changed the delta added to the start time of the "zero" */
/*        length intervals in type 2 CK with made up angular rates from */
/*        0.01 ticks to 0.000001 seconds. */

/* -    Version 6.1.0, 2012-04-11 (BVS) */

/*        Added optional setup file keyword CHECK_TIME_ORDER. */

/*        BUG FIX (in SUPPORT's CK3SDN): changed the down-sampling */
/*        end-point selection algorithm to not consider end-point */
/*        quaternions that are close to 180 degrees apart to prevent */
/*        cases in which the quaternion (QMINI) and matrix */
/*        (LINROT_M-like) interpolation algorithms produce rotations in */
/*        the opposite directions due to numerics. */

/* -    Version 6.0.0, 2006-11-07 (BVS) */

/*        Numerous small updates including: */

/*           -  added ET (ephemeris seconds past J2000) to the list of */
/*              accepted time formats. */

/*           -  added optional setup file keyword telling the program to */
/*              make a fake SCLK kernel (MAKE_FAKE_SCLK = 'fake SCLK */
/*              name'). */

/*           -  changed the way in which the user specified angular rate */
/*              threshold is applied: the absolute value of angular rate */
/*              components is now compared to the user supplied */
/*              maximum; */

/*           -  added check for the angular rate threshold value to be */
/*              positive; */

/*           -  added check for expected number of items of each input */
/*              line to improve run time diagnostic of insufficient data */

/*           -  improved error messages; */

/*           -  added global SAVE to the main program and all routines */
/*              to prevent potential memory problems in the f2c'ed */
/*              version of the program */

/* -    Version 5.0.0, 2006-06-23 (BVS) */

/*        Added DSCLK (decimal SCLK) to the list of accepted time */
/*        formats. Added setup file keyword controlling inclusion of the */
/*        interpolation interval table into the comment area and screen */
/*        output (INCLUDE_INTERVAL_TABLE  = 'YES'(default) or 'NO'). */

/* -    Version 4.1.0, 2005-09-26 (BVS) */

/*        Changed to load specified FK file before any other setup */
/*        parameters that might depend on it are processed. */

/* -    Version 4.0.0, 2005-09-21 (BVS) */

/*        Added capability to down sample input attitude data stream */
/*        before writing a type 3 CK file. */

/* -    Version 3.1.0, 2004-04-05 (EDW) */

/*        Replaced LDPOOL calls with FURNSH. Doing so ensures */
/*        the ASCII terminator test for text kernels. */

/* -    Version 3.0.0, 2003-05-05 (EDW) */

/*        Added capability to perform a BODY or SPACE rotation */
/*        using defined Euler angles. Default behavior is SPACE */
/*        as that was the pre-existing functionality. */

/*        Header and comment edits. */

/* -    Alpha Version 2.0.0, 2001-04-25 (BVS) */

/*        Added TICKS (DP SCLK) to the list of accepted time formats. */
/*        Made 1.9.0 addition backward compatible by adding 'MAKE UP/ */
/*        NO AVERAGING'; now 'MAKE UP' work the way it used to (averages */
/*        made up rates for type 3) and 'MAKE UP/NO AVERAGING' does not */
/*        do averaging. Updated template output and fixed backslashes */
/*        before begindata and begintext. */

/* -    Alpha Version 1.9.0, 2000-10-02 (BVS) */

/*        Changed AV manufacturing algorithm for CK3 to remove */
/*        averaging. Now it's the same as for CK2. */

/* -    Alpha Version 1.8.0, 1999-DEC-01 (BVS) */

/*        Replaced CKCLS with DAFCLS (... until CKCLS will be made */
/*        less smart about changing DAF IDs :-) */

/* -    Alpha Version 1.7.0, 1999-SEP-19 (BVS) */

/*        Added capability to apply bias to input times. */

/* -    Alpha Version 1.6.0, 1999-SEP-12 (BVS) */

/*        Added quaternion normalization for both cases of */
/*        angular rate manufacturing (CK types 2 and 3) in order */
/*        to get around tight ``ISROT'' tolerances in LINROT_M. */

/* -    Alpha Version 1.5.0, 1999-JUN-25 (BVS) */

/*        Changed the logic to eliminate gaps at the boundaries of */
/*        multiple segments generated in a single program run. */

/* -    Alpha Version 1.4.0, 1999-APR-28 (BVS) */

/*        Capability to manufacture angular rates was extended to */
/*        type 3 CK files. */

/* -    Alpha Version 1.3.0, 1999-FEB-22 (BVS) */

/*        Added capability to manufacture angular rates for type 2 */
/*        CK files. Replaced SCE2T with SCE2C to enable fractional */
/*        ticks. */

/* -    Alpha Version 1.2.0, 1998-DEC-10 (BVS) */

/*        Added PRODUCER_ID in the setup file. Added PRODUCT_CREATION */
/*        _TIME to the comment area contents. */

/* -    Alpha Version 1.1.1, 1998-DEC-02 (BVS) */

/*        Incorrect order -- OFFSET * M instead of M * OFFSET -- */
/*        in which additional rotation was applied was corrected. */

/* -    Alpha Version 1.1.0, 1998-SEP-04 (BVS) */

/*        Added capabilities implemented by setup keywords */
/*        ANGULAR_RATE_FRAME, EULER_ANGLE_UNITS and */
/*        OFFSET_ROTATION_xxx. Quaternion filtering changed to */
/*        correspond to ATTREC. */

/* -    Alpha Version 1.0.0, 1998-JUL-24 (BVS) */

/*        Initial release for MSOP delivery C4.1 */

/* -& */

/*     Parameters. */


/*     Setup keywords. */


/*     Run time meta information keywords. */


/*     Pre-defined setup keyword values. */


/*     Input data types. */


/*     Time types. */


/*     Output UTC time picture -- ISO standard. */


/*     Backslash character ord. */


/*     Increment, as fraction of a second, to be added to the start time */
/*     of the "zero" length intervals in type 2 CK with made up angular */
/*     rates. */


/*     Number of lines in the template display. */


/*     Variables. */


/*     SPICELIB functions. */


/*     Save everything to prevent potential memory problems in f2c'ed */
/*     version. */


/*     Initialize everything that compilers might flag as */
/*     un-initialized. All of these are initialized within multi-branch */
/*     IF cases with one of the branches erroring out. */

    earate = rpd_();
    insarf = FALSE_;
    muarat = FALSE_;
    avgrat = FALSE_;
    wrtseg = FALSE_;
    eulbod = FALSE_;
    addtab = TRUE_;
    chkto = FALSE_;

/*     Standard SPICE error handling. */

    chkin_("MSOPCK", (ftnlen)6);

/*     Display only short and long error messages. */

    errprt_("SET", "NONE, SHORT, LONG, TRACEBACK", (ftnlen)3, (ftnlen)28);

/*     Display version. */

    tkvrsn_("TOOLKIT", word, (ftnlen)7, (ftnlen)40);
    tostdo_(" ", (ftnlen)1);
/* Writing concatenation */
    i__1[0] = 70, a__1[0] = "MSOPCK Utility Program, Version 6.2.0, 2013-07-"
	    "01; SPICE Toolkit Ver. ";
    i__1[1] = rtrim_(word, (ftnlen)40), a__1[1] = word;
    s_cat(ch__1, a__1, i__1, &c__2, (ftnlen)110);
    tostdo_(ch__1, rtrim_(word, (ftnlen)40) + 70);
    tostdo_(" ", (ftnlen)1);

/*     Get command line. */

    getcml_(line, (ftnlen)265);

/*     Did we get three parameters on the command line? */

    if (wdcnt_(line, (ftnlen)265) != 3) {

/*        Check whether someone requested help or template. If */
/*        not, display usage. */

	ucase_(line, line, (ftnlen)265, (ftnlen)265);
	prefix_(" ", &c__1, line, (ftnlen)1, (ftnlen)265);
	suffix_(" ", &c__1, line, (ftnlen)1, (ftnlen)265);
	if (pos_(line, " -H ", &c__1, (ftnlen)265, (ftnlen)4) != 0 || pos_(
		line, " -HELP ", &c__1, (ftnlen)265, (ftnlen)7) != 0) {
	    s_copy(templt, "   This program creates type 1, 2 or 3 CK file f"
		    "rom an input", (ftnlen)80, (ftnlen)60);
	    s_copy(templt + 80, "   orientation data provided in a text file"
		    " as quaternions,", (ftnlen)80, (ftnlen)59);
	    s_copy(templt + 160, "   euler angles or matrices tagged by UTC,"
		    " SCLK, or ET times.", (ftnlen)80, (ftnlen)61);
	    s_copy(templt + 240, " ", (ftnlen)80, (ftnlen)1);
	    s_copy(templt + 320, "   See MSOPCK User's Guide for more inform"
		    "ation.", (ftnlen)80, (ftnlen)48);
	    s_copy(templt + 400, " ", (ftnlen)80, (ftnlen)1);
	    for (i__ = 1; i__ <= 6; ++i__) {
		tostdo_(templt + ((i__2 = i__ - 1) < 49 && 0 <= i__2 ? i__2 : 
			s_rnge("templt", i__2, "msopck_", (ftnlen)933)) * 80, 
			(ftnlen)80);
	    }
	} else if (pos_(line, " -T ", &c__1, (ftnlen)265, (ftnlen)4) != 0 || 
		pos_(line, " -TEMPLATE ", &c__1, (ftnlen)265, (ftnlen)11) != 
		0) {
	    s_copy(templt, "   MSOPCK Setup File Template. See MSOPCK User's"
		    " Guide for more", (ftnlen)80, (ftnlen)63);
	    s_copy(templt + 80, "   information.", (ftnlen)80, (ftnlen)15);
	    s_copy(templt + 160, " ", (ftnlen)80, (ftnlen)1);
	    s_copy(templt + 240, "   \\begindata", (ftnlen)80, (ftnlen)13);
	    s_copy(templt + 320, " ", (ftnlen)80, (ftnlen)1);
	    s_copy(templt + 400, "      LSK_FILE_NAME           = 'LSK file "
		    "name'", (ftnlen)80, (ftnlen)47);
	    s_copy(templt + 480, "      SCLK_FILE_NAME          = 'SCLK file"
		    " name'", (ftnlen)80, (ftnlen)48);
	    s_copy(templt + 560, "         or", (ftnlen)80, (ftnlen)11);
	    s_copy(templt + 640, "      MAKE_FAKE_SCLK          = 'fake SCLK"
		    " file name'", (ftnlen)80, (ftnlen)53);
	    s_copy(templt + 720, "      FRAMES_FILE_NAME        = 'FK file n"
		    "ame'", (ftnlen)80, (ftnlen)46);
	    s_copy(templt + 800, " ", (ftnlen)80, (ftnlen)1);
	    s_copy(templt + 880, "      INTERNAL_FILE_NAME      = 'internal "
		    "file name string'", (ftnlen)80, (ftnlen)59);
	    s_copy(templt + 960, "      COMMENTS_FILE_NAME      = 'file cont"
		    "aining comments'", (ftnlen)80, (ftnlen)58);
	    s_copy(templt + 1040, " ", (ftnlen)80, (ftnlen)1);
	    s_copy(templt + 1120, "      CK_TYPE                 = 1, 2, or 3"
		    , (ftnlen)80, (ftnlen)42);
	    s_copy(templt + 1200, "      CK_SEGMENT_ID           = 'segment "
		    "ID string'", (ftnlen)80, (ftnlen)51);
	    s_copy(templt + 1280, "      INSTRUMENT_ID           = NAIF inst"
		    "rument ID", (ftnlen)80, (ftnlen)50);
	    s_copy(templt + 1360, "      REFERENCE_FRAME_NAME    = 'referenc"
		    "e frame name'", (ftnlen)80, (ftnlen)54);
	    s_copy(templt + 1440, "      ANGULAR_RATE_PRESENT    = 'YES', 'N"
		    "O', 'MAKE UP', or", (ftnlen)80, (ftnlen)58);
	    s_copy(templt + 1520, "                                'MAKE UP/"
		    "NO AVERAGING'", (ftnlen)80, (ftnlen)54);
	    s_copy(templt + 1600, " ", (ftnlen)80, (ftnlen)1);
	    s_copy(templt + 1680, "      QUATERNION_NORM_ERROR   = maximum n"
		    "ormalization error", (ftnlen)80, (ftnlen)59);
	    s_copy(templt + 1760, "      ANGULAR_RATE_THRESHOLD  = ( max X r"
		    "ate, rad/sec", (ftnlen)80, (ftnlen)53);
	    s_copy(templt + 1840, "                                  max Y r"
		    "ate, rad/sec", (ftnlen)80, (ftnlen)53);
	    s_copy(templt + 1920, "                                  max Z r"
		    "ate, rad/sec )", (ftnlen)80, (ftnlen)55);
	    s_copy(templt + 2000, "      MAXIMUM_VALID_INTERVAL  = interval "
		    "length, seconds", (ftnlen)80, (ftnlen)56);
	    s_copy(templt + 2080, "      TIME_CORRECTION         = bias to b"
		    "e appiled to input times, seconds", (ftnlen)80, (ftnlen)
		    74);
	    s_copy(templt + 2160, " ", (ftnlen)80, (ftnlen)1);
	    s_copy(templt + 2240, "      INPUT_TIME_TYPE         = 'SCLK', '"
		    "UTC', 'TICKS', 'DSCLK', or 'ET'", (ftnlen)80, (ftnlen)72);
	    s_copy(templt + 2320, "      INPUT_DATA_TYPE         = 'MSOP QUA"
		    "TERNIONS',", (ftnlen)80, (ftnlen)51);
	    s_copy(templt + 2400, "                                'SPICE QU"
		    "ATERNIONS',", (ftnlen)80, (ftnlen)52);
	    s_copy(templt + 2480, "                                'EULER AN"
		    "GLES', or", (ftnlen)80, (ftnlen)50);
	    s_copy(templt + 2560, "                                'MATRICES'"
		    , (ftnlen)80, (ftnlen)42);
	    s_copy(templt + 2640, "      EULER_ANGLE_UNITS       = 'DEGREES'"
		    " or 'RADIANS'", (ftnlen)80, (ftnlen)54);
	    s_copy(templt + 2720, "      EULER_ROTATIONS_ORDER   = rotation "
		    "axes; example: ( 'X' 'Y' 'Z' )", (ftnlen)80, (ftnlen)71);
	    s_copy(templt + 2800, "      EULER_ROTATIONS_TYPE    = 'BODY' or"
		    " 'SPACE' (default SPACE)", (ftnlen)80, (ftnlen)65);
	    s_copy(templt + 2880, "      ANGULAR_RATE_FRAME      = 'REFERENC"
		    "E' or 'INSTRUMENT'", (ftnlen)80, (ftnlen)59);
	    s_copy(templt + 2960, " ", (ftnlen)80, (ftnlen)1);
	    s_copy(templt + 3040, "      OFFSET_ROTATION_ANGLES  = angles; e"
		    "xample: ( 0.5, 0.1, 0.1 )", (ftnlen)80, (ftnlen)66);
	    s_copy(templt + 3120, "      OFFSET_ROTATION_AXES    = rotation "
		    "axes: example: ( 'X' 'Y' 'Z' )", (ftnlen)80, (ftnlen)71);
	    s_copy(templt + 3200, "      OFFSET_ROTATION_UNITS   = 'DEGREES'"
		    " or 'RADIANS'", (ftnlen)80, (ftnlen)54);
	    s_copy(templt + 3280, "      DOWN_SAMPLE_TOLERANCE   = down samp"
		    "ling tolerance, radians", (ftnlen)80, (ftnlen)64);
	    s_copy(templt + 3360, "      INCLUDE_INTERVAL_TABLE  = 'YES' or "
		    "'NO' (default 'YES')", (ftnlen)80, (ftnlen)61);
	    s_copy(templt + 3440, "      CHECK_TIME_ORDER        = 'YES' or "
		    "'NO' (default 'NO')", (ftnlen)80, (ftnlen)60);
	    s_copy(templt + 3520, " ", (ftnlen)80, (ftnlen)1);
	    s_copy(templt + 3600, "      PRODUCER_ID             = 'producer"
		    " group/person name'", (ftnlen)80, (ftnlen)60);
	    s_copy(templt + 3680, " ", (ftnlen)80, (ftnlen)1);
	    s_copy(templt + 3760, "   \\begintext", (ftnlen)80, (ftnlen)13);
	    s_copy(templt + 3840, " ", (ftnlen)80, (ftnlen)1);
	    for (i__ = 1; i__ <= 49; ++i__) {
		tostdo_(templt + ((i__2 = i__ - 1) < 49 && 0 <= i__2 ? i__2 : 
			s_rnge("templt", i__2, "msopck_", (ftnlen)1028)) * 80,
			 (ftnlen)80);
	    }
	} else {
	    tostdo_("Usage: ", (ftnlen)7);
	    tostdo_(" ", (ftnlen)1);
	    tostdo_("  > msopck <setup_file> <input_file> <output_ck_file>", (
		    ftnlen)53);
	    tostdo_(" ", (ftnlen)1);
	}
	s_stop("", (ftnlen)0);
    }

/*     Extract file names from command line. Setup file and input file */
/*     must exist. */

    nextwd_(line, setupf, line, (ftnlen)265, (ftnlen)265, (ftnlen)265);
    if (! exists_(setupf, (ftnlen)265)) {
	setmsg_("The setup file '#' specified as the first argument on the c"
		"ommand line doesn't exist.", (ftnlen)85);
	errch_("#", setupf, (ftnlen)1, (ftnlen)265);
	sigerr_("SPICE(SETUPDOESNOTEXIST)", (ftnlen)24);
    }
    nextwd_(line, inputf, line, (ftnlen)265, (ftnlen)265, (ftnlen)265);
    if (! exists_(inputf, (ftnlen)265)) {
	setmsg_("The input data file '#' specified as the second argument on"
		" the command line doesn't exist.", (ftnlen)91);
	errch_("#", inputf, (ftnlen)1, (ftnlen)265);
	sigerr_("SPICE(INPUTDOESNOTEXIST)", (ftnlen)24);
    }
    nextwd_(line, outptf, line, (ftnlen)265, (ftnlen)265, (ftnlen)265);
    if (exists_(outptf, (ftnlen)265)) {

/*        Well, an output file has its right to exist. And if it's */
/*        so, we set a flag that new segment will be appended to */
/*        an existing file instead of being written to a new one. */
/*        But first we check whether this existing file is a CK. */

	getfat_(outptf, word, line, (ftnlen)265, (ftnlen)40, (ftnlen)265);
	if (s_cmp(word, "DAF", (ftnlen)40, (ftnlen)3) != 0 || s_cmp(line, 
		"CK", (ftnlen)265, (ftnlen)2) != 0) {
	    setmsg_("The file '#' specified as the third argument on the com"
		    "mand line isn't a CK file. CK segments cannot be appende"
		    "dto a file that is not a CK.", (ftnlen)139);
	    errch_("#", outptf, (ftnlen)1, (ftnlen)265);
	    sigerr_("SPICE(NOTACKFILE)", (ftnlen)17);
	}
	appndf = TRUE_;
    } else {

/*        Otherwise, it's a new file. So flag is FALSE. */

	appndf = FALSE_;
    }

/*     Whatever file it is -- existing or new, it's not opened yet. :) */
/*     The same is true for both scratch files -- one for errors and */
/*     another for interpolation intervals. */

    ckopnd = FALSE_;
    seopnd = FALSE_;
    siopnd = FALSE_;

/*     Load setup file into the pool and retrieve all setup values */
/*     from POOL. */

    furnsh_(setupf, (ftnlen)265);

/*     The first thing to do is to get the name of and load FK file. */
/*     This must be done before everything else because some routines */
/*     called later to process other setup file parameters may rely on */
/*     the data that might have been provided in the FK. Note that the */
/*     FK name is an optional setup file parameter, so we don't complain */
/*     if it is not there. */

    gcpool_("FRAMES_FILE_NAME", &c__1, &c__1, &n, framef, &found, (ftnlen)16, 
	    (ftnlen)265);
    if (found) {
	furnsh_(framef, (ftnlen)265);
    }

/*     The second thing to do is to get the instrument ID. We need it to */
/*     figure out what the spacecraft ID is before we get to the point */
/*     where we may be asked to make an fake SCLK. */

    gipool_("INSTRUMENT_ID", &c__1, &c__1, &n, &instid, &found, (ftnlen)13);
    if (! found) {
	setmsg_("No NAIF Instrument ID was provided in the setup file using "
		"the keyword '#'.", (ftnlen)75);
	errch_("#", "INSTRUMENT_ID", (ftnlen)1, (ftnlen)13);
	sigerr_("SPICE(NOINSTRUMENTID)", (ftnlen)21);
    }

/*     Find out what is NAIF ID for the S/C from the instruments ID. */

    ckmeta_(&instid, "SCLK", &scid, (ftnlen)4);
    if (scid == 0) {
	setmsg_("The NAIF ID for the spacecraft cannot be determined from th"
		"e NAIF instrument ID '#'. Check whether this instrument ID i"
		"s a legitimate ID for the instrument/structure of interest.", 
		(ftnlen)178);
	errint_("#", &instid, (ftnlen)1);
	sigerr_("SPICE(BADINSTRUMENTID)", (ftnlen)22);
    }

/*     Get producer ID string. */

    gcpool_("PRODUCER_ID", &c__1, &c__1, &n, prodid, &found, (ftnlen)11, (
	    ftnlen)265);
    if (! found) {
	setmsg_("No producer ID was provided in the setup file using the key"
		"word '#'.", (ftnlen)68);
	errch_("#", "PRODUCER_ID", (ftnlen)1, (ftnlen)11);
	sigerr_("SPICE(NOPRODUCERID)", (ftnlen)19);
    }

/*     Get the name of and load LSK file. */

    gcpool_("LSK_FILE_NAME", &c__1, &c__1, &n, lskf, &found, (ftnlen)13, (
	    ftnlen)265);
    if (! found) {
	setmsg_("No leapseconds file name was provided in the setup file usi"
		"ng the keyword '#'.", (ftnlen)78);
	errch_("#", "LSK_FILE_NAME", (ftnlen)1, (ftnlen)13);
	sigerr_("SPICE(NOSLKFILENAME)", (ftnlen)20);
    } else {
	furnsh_(lskf, (ftnlen)265);
    }

/*     Try to get both setup file keywords related to SCLK files. */

    gcpool_("SCLK_FILE_NAME", &c__1, &c__1, &n, sclkf, &found1, (ftnlen)14, (
	    ftnlen)265);
    gcpool_("MAKE_FAKE_SCLK", &c__1, &c__1, &n, fsclkf, &found2, (ftnlen)14, (
	    ftnlen)265);
    if (found1 && found2) {

/*        Both SCLK keywords were provided. Complain and stop. */

	setmsg_("Only one of the SCLK keywords '#' and '#' can be present in"
		" the setup file. The setup file '#' contained both of them.", 
		(ftnlen)118);
	errch_("#", "SCLK_FILE_NAME", (ftnlen)1, (ftnlen)14);
	errch_("#", "MAKE_FAKE_SCLK", (ftnlen)1, (ftnlen)14);
	errch_("#", setupf, (ftnlen)1, (ftnlen)265);
	sigerr_("SPICE(TWOSCLKFILENAMES)", (ftnlen)23);
    } else if (found1) {

/*        The name of an existing SCLK was provided. Load it using */
/*        FURNSH. */

	furnsh_(sclkf, (ftnlen)265);
    } else if (found2) {

/*        The name of a fake SCLK to be made was provided. Make it */
/*        and load it using FURNSH. First, check if this file already */
/*        exists. */

	if (exists_(fsclkf, (ftnlen)265)) {
	    setmsg_("The SCLK file '#' specified using the setup file keywor"
		    "d '#' already exists. ", (ftnlen)77);
	    errch_("#", fsclkf, (ftnlen)1, (ftnlen)265);
	    errch_("#", "MAKE_FAKE_SCLK", (ftnlen)1, (ftnlen)14);
	    sigerr_("SPICE(FAKESCLKEXISTS)", (ftnlen)21);
	} else {

/*           Make a fake SCLK that starts at J2000. */

/* Writing concatenation */
	    i__1[0] = 265, a__1[0] = prodid;
	    i__1[1] = 21, a__1[1] = " using MSOPCK program";
	    s_cat(ch__2, a__1, i__1, &c__2, (ftnlen)286);
	    mkfclk_(fsclkf, &scid, &c_b142, ch__2, (ftnlen)265, (ftnlen)286);
	    furnsh_(fsclkf, (ftnlen)265);
	}
    } else {

/*        Neither of the two SCLK keywords was provided. Complain */
/*        and stop. */

	setmsg_("Neither the name of an existing SCLK file was provided usin"
		"g the setup file keyword '#' nor the name of a fake SCLK to "
		"be made was given using the keyword '#'.", (ftnlen)159);
	errch_("#", "SCLK_FILE_NAME", (ftnlen)1, (ftnlen)14);
	errch_("#", "MAKE_FAKE_SCLK", (ftnlen)1, (ftnlen)14);
	sigerr_("SPICE(NOSCLKFILENAMES)", (ftnlen)22);
    }

/*     Get CK type... */

    gipool_("CK_TYPE", &c__1, &c__1, &n, &cktype, &found, (ftnlen)7);
    if (! found) {
	setmsg_("No output CK segment type was provided in the setup file us"
		"ing the keyword '#'.", (ftnlen)79);
	errch_("#", "CK_TYPE", (ftnlen)1, (ftnlen)7);
	sigerr_("SPICE(NOCKSEGMENTTYPE)", (ftnlen)22);
    }

/*     ...check whether it's 1, 2 or 3. */

    if (cktype == 1 || cktype == 2 || cktype == 3) {
    } else {
	setmsg_("The output CK segment type value '#' provided in the setup "
		"file keyword '#' is not acceptable. Acceptable values are 1,"
		" 2 and 3.", (ftnlen)128);
	errint_("#", &cktype, (ftnlen)1);
	errch_("#", "CK_TYPE", (ftnlen)1, (ftnlen)7);
	sigerr_("SPICE(BADCKTYPESPEC)", (ftnlen)20);
    }

/*     Get reference frame name. */

    gcpool_("REFERENCE_FRAME_NAME", &c__1, &c__1, &n, frmnam, &found, (ftnlen)
	    20, (ftnlen)40);
    if (! found) {
	setmsg_("No reference frame name was provided in the setup file usin"
		"g keyword '#'.", (ftnlen)73);
	errch_("#", "REFERENCE_FRAME_NAME", (ftnlen)1, (ftnlen)20);
	sigerr_("SPICE(NOFRAMENAME)", (ftnlen)18);
    } else {

/*        Check whether we can recognize this name. */

	namfrm_(frmnam, &n, (ftnlen)40);
	if (n == 0) {
	    setmsg_("The reference frame '#' specified using the setup file "
		    "keyword '#' is not recognized.", (ftnlen)85);
	    errch_("#", frmnam, (ftnlen)1, (ftnlen)40);
	    errch_("#", "REFERENCE_FRAME_NAME", (ftnlen)1, (ftnlen)20);
	    sigerr_("SPICE(UNRECOGNIZEDFRAME)", (ftnlen)24);
	}
    }

/*     Look whether angular rates are present or not. */

    gcpool_("ANGULAR_RATE_PRESENT", &c__1, &c__1, &n, word, &found, (ftnlen)
	    20, (ftnlen)40);
    if (! found) {
	setmsg_("No angular rate flag was provided in the setup file using t"
		"he keyword '#'.", (ftnlen)74);
	errch_("#", "ANGULAR_RATE_PRESENT", (ftnlen)1, (ftnlen)20);
	sigerr_("SPICE(NOANGULARRATEFLAG)", (ftnlen)24);
    }

/*     Does the flag have a value that we expected? */

    if (eqstr_(word, "YES", (ftnlen)40, (ftnlen)3)) {
	angrat = TRUE_;
	muarat = FALSE_;
	avgrat = FALSE_;
    } else if (eqstr_(word, "NO", (ftnlen)40, (ftnlen)2)) {
	angrat = FALSE_;
	muarat = FALSE_;
	avgrat = FALSE_;
    } else if (eqstr_(word, "MAKE UP", (ftnlen)40, (ftnlen)7)) {
	angrat = FALSE_;
	muarat = TRUE_;
	avgrat = TRUE_;
    } else if (eqstr_(word, "MAKE UP/NO AVERAGING", (ftnlen)40, (ftnlen)20)) {
	angrat = FALSE_;
	muarat = TRUE_;
	avgrat = FALSE_;
    } else {
	setmsg_("The angular rate flag specified using the setup file keywor"
		"d '#' has the value '#' while it can be only '#', '#', '#' o"
		"r '#'.", (ftnlen)125);
	errch_("#", "ANGULAR_RATE_PRESENT", (ftnlen)1, (ftnlen)20);
	errch_("#", word, (ftnlen)1, (ftnlen)40);
	errch_("#", "YES", (ftnlen)1, (ftnlen)3);
	errch_("#", "NO", (ftnlen)1, (ftnlen)2);
	errch_("#", "MAKE UP", (ftnlen)1, (ftnlen)7);
	errch_("#", "MAKE UP/NO AVERAGING", (ftnlen)1, (ftnlen)20);
	sigerr_("SPICE(BADANGULARRATEFLAG)", (ftnlen)25);
    }

/*     Check whether making angular rates up was requested for CK types */
/*     other that 2 or 3. */

    if (muarat) {
	if (! (cktype == 2 || cktype == 3)) {
	    setmsg_("The angular rate flag '#' specified in the setup file k"
		    "eyword '#' is applicable only for the output CK types 2 "
		    "and 3.", (ftnlen)117);
	    errch_("#", word, (ftnlen)1, (ftnlen)40);
	    errch_("#", "ANGULAR_RATE_PRESENT", (ftnlen)1, (ftnlen)20);
	    sigerr_("SPICE(NARATESFLAG)", (ftnlen)18);
	}
    }

/*     Angular rates must be present for type 2 CK, if making them up */
/*     wasn't requested. */

    if (! angrat && ! muarat && cktype == 2) {
	setmsg_("The angular rate flag specified in the setup file keyword '"
		"#' must be set to '#' (in which case angular rates must be p"
		"resent in the input file) or '#' (in which case angular rate"
		"s will manufactured by the program) if creation of a type 2 "
		"CK file was requested.", (ftnlen)261);
	errch_("#", "ANGULAR_RATE_PRESENT", (ftnlen)1, (ftnlen)20);
	errch_("#", "YES", (ftnlen)1, (ftnlen)3);
	errch_("#", "MAKE UP", (ftnlen)1, (ftnlen)7);
	sigerr_("SPICE(NORATESFORTYPE2CK)", (ftnlen)24);
    }

/*     Get maximum quaternion normalization error. If not present, then */
/*     no filtering required. If present, check whether provided value */
/*     makes sense. */

    gdpool_("QUATERNION_NORM_ERROR", &c__1, &c__1, &n, &qerror, &found, (
	    ftnlen)21);
    if (! found) {
	qfilter = FALSE_;
    } else {
	if (qerror <= 0. || qerror >= 1.) {
	    setmsg_("The quaternion normalization error value '#' provided i"
		    "n the setup file using the keyword '#' doesn't make sens"
		    "e. It should be less than 1 but greater than 0.", (ftnlen)
		    158);
	    errdp_("#", &qerror, (ftnlen)1);
	    errch_("#", "QUATERNION_NORM_ERROR", (ftnlen)1, (ftnlen)21);
	    sigerr_("SPICE(BADQUATTHRESHOLD)", (ftnlen)23);
	}
	qfilter = TRUE_;
    }

/*     Get angular rate components threshold values, ...  only if */
/*     we have to consider rates at all. */

    rfilter = FALSE_;
    if (angrat) {

/*        Run DTPOOL first -- it tells us more that GDPOOL. Angular rate */
/*        thresholds are also optional. */

	dtpool_("ANGULAR_RATE_THRESHOLD", &found, &n, word, (ftnlen)22, (
		ftnlen)40);
	if (! found) {
	    rfilter = FALSE_;
	} else {

/*           What if it's present but not the right type? */

	    if (n != 3 || s_cmp(word, "N", (ftnlen)40, (ftnlen)1) != 0) {
		setmsg_("The number of values provided for the maximum angul"
			"ar rate using the setup file keyword '#' is not equa"
			"l to 3 or one of its elements is not a DP number.", (
			ftnlen)152);
		errch_("#", "ANGULAR_RATE_THRESHOLD", (ftnlen)1, (ftnlen)22);
		sigerr_("SPICE(BADANGRATEERROR)", (ftnlen)22);
	    }

/*           If we are here, the keyword looks fine. Get the values and */
/*           check them. */

	    gdpool_("ANGULAR_RATE_THRESHOLD", &c__1, &c__3, &n, rerror, &
		    found, (ftnlen)22);
	    for (i__ = 1; i__ <= 3; ++i__) {
		if (rerror[(i__2 = i__ - 1) < 3 && 0 <= i__2 ? i__2 : s_rnge(
			"rerror", i__2, "msopck_", (ftnlen)1464)] <= 0.) {
		    setmsg_("The maximum angular rate value '#' provided in "
			    "the setup file using the keyword '#' doesn't mak"
			    "e sense. It should be greater than 0.", (ftnlen)
			    132);
		    errdp_("#", &rerror[(i__2 = i__ - 1) < 3 && 0 <= i__2 ? 
			    i__2 : s_rnge("rerror", i__2, "msopck_", (ftnlen)
			    1470)], (ftnlen)1);
		    errch_("#", "ANGULAR_RATE_THRESHOLD", (ftnlen)1, (ftnlen)
			    22);
		    sigerr_("SPICE(BADRATETHRESHOLD)", (ftnlen)23);
		}
	    }
	    rfilter = TRUE_;
	}

/*        Find out relative to what frame angular rates are. */

	gcpool_("ANGULAR_RATE_FRAME", &c__1, &c__1, &n, word, &found, (ftnlen)
		18, (ftnlen)40);
	if (! found) {

/*           Keyword wasn't in a setup file. Set default to AR to be */
/*           relative to the reference frame rather than instrument */
/*           frame. */

	    insarf = FALSE_;
	} else {

/*           Keyword was there. See if we can recognize value. */

	    if (eqstr_(word, "INSTRUMENT", (ftnlen)40, (ftnlen)10)) {
		insarf = TRUE_;
	    } else if (eqstr_(word, "REFERENCE", (ftnlen)40, (ftnlen)9)) {
		insarf = FALSE_;
	    } else {
		setmsg_("The angular rate reference frame flag specified usi"
			"ng the setup file keyword '#' has value '#' while it"
			" can be only '#' or '#'.", (ftnlen)127);
		errch_("#", "ANGULAR_RATE_FRAME", (ftnlen)1, (ftnlen)18);
		errch_("#", word, (ftnlen)1, (ftnlen)40);
		errch_("#", "INSTRUMENT", (ftnlen)1, (ftnlen)10);
		errch_("#", "REFERENCE", (ftnlen)1, (ftnlen)9);
		sigerr_("SPICE(BADRATEFRAMEFLAG)", (ftnlen)23);
	    }
	}
    }

/*     Get maximum interpolation interval length. */

    gdpool_("MAXIMUM_VALID_INTERVAL", &c__1, &c__1, &n, &intrvl, &found, (
	    ftnlen)22);
    if (! found) {
	intrvl = dpmax_();
    }

/*     Get fixed time correction value (signed seconds). */

    gdpool_("TIME_CORRECTION", &c__1, &c__1, &n, &timcor, &found, (ftnlen)15);
    if (! found) {
	timcor = 0.;
    }

/*     Get segment ID string, if present. If not -- make up */
/*     default value. */

    gcpool_("CK_SEGMENT_ID", &c__1, &c__1, &n, segid, &found, (ftnlen)13, (
	    ftnlen)40);
    if (! found) {
	s_copy(segid, outptf, (ftnlen)40, (ftnlen)40);
    }

/*     If output CK file is a new file, get internal file name, if */
/*     present. If not present -- make up default value. */

    cmmflg = FALSE_;
    if (! appndf) {
	gcpool_("INTERNAL_FILE_NAME", &c__1, &c__1, &n, ifname, &found, (
		ftnlen)18, (ftnlen)80);
	if (! found) {
	    s_copy(ifname, outptf, (ftnlen)80, (ftnlen)60);
	}

/*        Also for a new CK file we get name of external file */
/*        containing comments if such was provided. */

	gcpool_("COMMENTS_FILE_NAME", &c__1, &c__1, &n, cmmntf, &found, (
		ftnlen)18, (ftnlen)265);
	if (found) {
	    if (! exists_(cmmntf, (ftnlen)265)) {
		setmsg_("The external comments file '#' specified using the "
			"setup file keyword '#' doesn't exist.", (ftnlen)88);
		errch_("#", cmmntf, (ftnlen)1, (ftnlen)265);
		errch_("#", "COMMENTS_FILE_NAME", (ftnlen)1, (ftnlen)18);
		sigerr_("SPICE(NOCOMMENTSFILE)", (ftnlen)21);
	    }
	}
	cmmflg = found;
    }

/*     What is our input data type? */

    gcpool_("INPUT_DATA_TYPE", &c__1, &c__1, &n, dtype, &found, (ftnlen)15, (
	    ftnlen)40);
    if (! found) {
	setmsg_("No input data type identifier was provided in the setup fil"
		"e using the keyword '#'.", (ftnlen)83);
	errch_("#", "INPUT_DATA_TYPE", (ftnlen)1, (ftnlen)15);
	sigerr_("SPICE(NODATATYPEFLAG)", (ftnlen)21);
    } else {

/*        Can we recognize this data type? */

	if (eqstr_(dtype, "MSOP QUATERNIONS", (ftnlen)40, (ftnlen)16) || 
		eqstr_(dtype, "SPICE QUATERNIONS", (ftnlen)40, (ftnlen)17) || 
		eqstr_(dtype, "EULER ANGLES", (ftnlen)40, (ftnlen)12) || 
		eqstr_(dtype, "MATRICES", (ftnlen)40, (ftnlen)8)) {

/*           Yes, we can. */

	} else {

/*           No, we cannot. Complain. */

	    setmsg_("The input data type '#' specified using the setup file "
		    "keyword '#' is not recognized. Acceptable values are '#'"
		    ", '#', '#' and '#'.", (ftnlen)130);
	    errch_("#", dtype, (ftnlen)1, (ftnlen)40);
	    errch_("#", "INPUT_DATA_TYPE", (ftnlen)1, (ftnlen)15);
	    errch_("#", "MSOP QUATERNIONS", (ftnlen)1, (ftnlen)16);
	    errch_("#", "SPICE QUATERNIONS", (ftnlen)1, (ftnlen)17);
	    errch_("#", "EULER ANGLES", (ftnlen)1, (ftnlen)12);
	    errch_("#", "MATRICES", (ftnlen)1, (ftnlen)8);
	    sigerr_("SPICE(BADDATATYPEFLAG)", (ftnlen)22);
	}
    }

/*     What is our input time type? */

    gcpool_("INPUT_TIME_TYPE", &c__1, &c__1, &n, ttype, &found, (ftnlen)15, (
	    ftnlen)40);
    if (! found) {
	setmsg_("No input time type identifier was provided in the setup fil"
		"e using the keyword '#'.", (ftnlen)83);
	errch_("#", "INPUT_TIME_TYPE", (ftnlen)1, (ftnlen)15);
	sigerr_("SPICE(NOTIMETYPEFLAG)", (ftnlen)21);
    } else {

/*        Can we recognize this time type? */

	if (eqstr_(ttype, "SCLK", (ftnlen)40, (ftnlen)4) || eqstr_(ttype, 
		"UTC", (ftnlen)40, (ftnlen)3) || eqstr_(ttype, "ET", (ftnlen)
		40, (ftnlen)2) || eqstr_(ttype, "TICKS", (ftnlen)40, (ftnlen)
		5)) {

/*           Yes, we can. For time types SCLK, UTC, TICKS, and ET we */
/*           don't need to do anything else at this point. */

	} else if (eqstr_(ttype, "DSCLK", (ftnlen)40, (ftnlen)5)) {

/*           But if the time type is DSCLK we need to check if the clock */
/*           is a type 1 SCLK and if it has exactly two fields. If so, */
/*           we will need get the module and offset of the minor (right) */
/*           field. */

	    if (sctype_(&scid) != 1) {
		setmsg_("The input time type '#' specified using the setup f"
			"ile keyword '#' can be used only when input time tag"
			"s are to be converted to type 1 SPICE SCLKs. The typ"
			"e of the clock associated with s/c ID '#' was not 1;"
			" it was #.", (ftnlen)217);
		errch_("#", "DSCLK", (ftnlen)1, (ftnlen)5);
		errch_("#", "INPUT_TIME_TYPE", (ftnlen)1, (ftnlen)15);
		errint_("#", &scid, (ftnlen)1);
		i__2 = sctype_(&scid);
		errint_("#", &i__2, (ftnlen)1);
		sigerr_("SPICE(NOTTYPE1SCLK)", (ftnlen)19);
	    }
	    nfilds = 0;
	    scli01_("SCLK01_N_FIELDS", &scid, &c__1, &n, &nfilds, (ftnlen)15);
	    if (nfilds != 2) {
		setmsg_("The input time type '#' specified using the setup f"
			"ile keyword '#' can be used only when input time tag"
			"s are to be converted to type 1 SPICE SCLKs that hav"
			"e exactly two fields. The number of fields in the cl"
			"ock associated with s/c ID '#' was not 2; it was #.", 
			(ftnlen)258);
		errch_("#", "DSCLK", (ftnlen)1, (ftnlen)5);
		errch_("#", "INPUT_TIME_TYPE", (ftnlen)1, (ftnlen)15);
		errint_("#", &scid, (ftnlen)1);
		errint_("#", &nfilds, (ftnlen)1);
		sigerr_("SPICE(NOTTWOFIELDSCLK)", (ftnlen)22);
	    }
	    scld01_("SCLK01_MODULI", &scid, &c__2, &n, sclkmd, (ftnlen)13);
	    if (n != 2) {
		setmsg_("The input time type '#' specified using the setup f"
			"ile keyword '#' can be used only when input time tag"
			"s are to be converted to type 1 SPICE SCLKs that hav"
			"e exactly two fields. The number of values in MODULI"
			" keyword for the clock associated with s/c ID '#' wa"
			"s not 2; it was #.", (ftnlen)277);
		errch_("#", "DSCLK", (ftnlen)1, (ftnlen)5);
		errch_("#", "INPUT_TIME_TYPE", (ftnlen)1, (ftnlen)15);
		errint_("#", &scid, (ftnlen)1);
		errint_("#", &n, (ftnlen)1);
		sigerr_("SPICE(NOTTWOMODULI)", (ftnlen)19);
	    }
	    scld01_("SCLK01_OFFSETS", &scid, &c__2, &n, sclkof, (ftnlen)14);
	    if (n != 2) {
		setmsg_("The input time type '#' specified using the setup f"
			"ile keyword '#' can be used only when input time tag"
			"s are to be converted to type 1 SPICE SCLKs that hav"
			"e exactly two fields. The number of values in OFFSET"
			"S keyword for the clock associated with s/c ID '#' w"
			"as not 2; it was #.", (ftnlen)278);
		errch_("#", "DSCLK", (ftnlen)1, (ftnlen)5);
		errch_("#", "INPUT_TIME_TYPE", (ftnlen)1, (ftnlen)15);
		errint_("#", &scid, (ftnlen)1);
		errint_("#", &n, (ftnlen)1);
		sigerr_("SPICE(NOTTWOOFFSETS)", (ftnlen)20);
	    }
	} else {

/*           No, we cannot. Complain. */

	    setmsg_("The input time type '#' specified using the setup file "
		    "keyword '#' is not recognized. Acceptable values are '#'"
		    ", '#', '#', '#', and '#'.", (ftnlen)136);
	    errch_("#", ttype, (ftnlen)1, (ftnlen)40);
	    errch_("#", "INPUT_TIME_TYPE", (ftnlen)1, (ftnlen)15);
	    errch_("#", "SCLK", (ftnlen)1, (ftnlen)4);
	    errch_("#", "UTC", (ftnlen)1, (ftnlen)3);
	    errch_("#", "TICKS", (ftnlen)1, (ftnlen)5);
	    errch_("#", "DSCLK", (ftnlen)1, (ftnlen)5);
	    errch_("#", "ET", (ftnlen)1, (ftnlen)2);
	    sigerr_("SPICE(BADTIMETYPEFLAG)", (ftnlen)22);
	}
    }

/*     If input data type is Euler angles, we need to know what the */
/*     order of rotation is. */

    if (eqstr_(dtype, "EULER ANGLES", (ftnlen)40, (ftnlen)12)) {

/*        Run DTPOOL first -- it tells us more that GDPOOL. */

	dtpool_("EULER_ROTATIONS_ORDER", &found, &n, word, (ftnlen)21, (
		ftnlen)40);
	if (! found) {
	    setmsg_("The order of rotations must be provided using the setup"
		    " file keyword '#' if the '#' setup file keyword is set t"
		    "o '#'.", (ftnlen)117);
	    errch_("#", "EULER_ROTATIONS_ORDER", (ftnlen)1, (ftnlen)21);
	    errch_("#", "INPUT_DATA_TYPE", (ftnlen)1, (ftnlen)15);
	    errch_("#", "EULER ANGLES", (ftnlen)1, (ftnlen)12);
	    sigerr_("SPICE(NOROTATIONORDER)", (ftnlen)22);
	} else {

/*           What if it's present but not 3 elements? */

	    if (n != 3) {
		setmsg_("The number of values provided using the setup file "
			"keyword '#' is not equal 3.", (ftnlen)78);
		errch_("#", "EULER_ROTATIONS_ORDER", (ftnlen)1, (ftnlen)21);
		sigerr_("SPICE(BADROTATIONSORDER)", (ftnlen)24);
	    }

/*           Ok, if it's character, we get and decode 'X', 'Y' and 'Z'. */
/*           If it's integer we just check it and save it. */

	    if (s_cmp(word, "C", (ftnlen)40, (ftnlen)1) == 0) {
		gcpool_("EULER_ROTATIONS_ORDER", &c__1, &c__3, &n, eaxis, &
			found, (ftnlen)21, (ftnlen)1);
		for (i__ = 1; i__ <= 3; ++i__) {
		    if (eqstr_(eaxis + ((i__2 = i__ - 1) < 3 && 0 <= i__2 ? 
			    i__2 : s_rnge("eaxis", i__2, "msopck_", (ftnlen)
			    1785)), "X", (ftnlen)1, (ftnlen)1)) {
			eulaxs[(i__2 = i__ - 1) < 3 && 0 <= i__2 ? i__2 : 
				s_rnge("eulaxs", i__2, "msopck_", (ftnlen)
				1786)] = 1;
		    } else if (eqstr_(eaxis + ((i__2 = i__ - 1) < 3 && 0 <= 
			    i__2 ? i__2 : s_rnge("eaxis", i__2, "msopck_", (
			    ftnlen)1787)), "Y", (ftnlen)1, (ftnlen)1)) {
			eulaxs[(i__2 = i__ - 1) < 3 && 0 <= i__2 ? i__2 : 
				s_rnge("eulaxs", i__2, "msopck_", (ftnlen)
				1788)] = 2;
		    } else if (eqstr_(eaxis + ((i__2 = i__ - 1) < 3 && 0 <= 
			    i__2 ? i__2 : s_rnge("eaxis", i__2, "msopck_", (
			    ftnlen)1789)), "Z", (ftnlen)1, (ftnlen)1)) {
			eulaxs[(i__2 = i__ - 1) < 3 && 0 <= i__2 ? i__2 : 
				s_rnge("eulaxs", i__2, "msopck_", (ftnlen)
				1790)] = 3;
		    } else {
			setmsg_("The element '#' of the Euler rotations orde"
				"r provided using the setup file keyword '#' "
				"is  not 'X', 'Y' or 'Z'.", (ftnlen)111);
			errch_("#", eaxis + ((i__2 = i__ - 1) < 3 && 0 <= 
				i__2 ? i__2 : s_rnge("eaxis", i__2, "msopck_",
				 (ftnlen)1796)), (ftnlen)1, (ftnlen)1);
			errch_("#", "EULER_ROTATIONS_ORDER", (ftnlen)1, (
				ftnlen)21);
			sigerr_("SPICE(BADROTATIONAXISXYZ)", (ftnlen)25);
		    }
		}
	    } else if (s_cmp(word, "N", (ftnlen)40, (ftnlen)1) == 0) {
		gipool_("EULER_ROTATIONS_ORDER", &c__1, &c__3, &n, eulaxs, &
			found, (ftnlen)21);
		for (i__ = 1; i__ <= 3; ++i__) {
		    if (eulaxs[(i__2 = i__ - 1) < 3 && 0 <= i__2 ? i__2 : 
			    s_rnge("eulaxs", i__2, "msopck_", (ftnlen)1804)] 
			    <= 0 || eulaxs[(i__3 = i__ - 1) < 3 && 0 <= i__3 ?
			     i__3 : s_rnge("eulaxs", i__3, "msopck_", (ftnlen)
			    1804)] >= 4) {
			setmsg_("The element # of the Euler rotations order "
				"provided using the setup file keyword '#' is"
				" not 1, 2 or 3.", (ftnlen)102);
			errint_("#", &eulaxs[(i__2 = i__ - 1) < 3 && 0 <= 
				i__2 ? i__2 : s_rnge("eulaxs", i__2, "msopck_"
				, (ftnlen)1809)], (ftnlen)1);
			errch_("#", "EULER_ROTATIONS_ORDER", (ftnlen)1, (
				ftnlen)21);
			sigerr_("SPICE(BADROTATIONAXIS123)", (ftnlen)25);
		    }
		}
	    } else {
		setmsg_("The kernel pool variable '#' type was '#'.", (ftnlen)
			42);
		errch_("#", "EULER_ROTATIONS_ORDER", (ftnlen)1, (ftnlen)21);
		errch_("#", word, (ftnlen)1, (ftnlen)40);
		sigerr_("SPICE(BADROTAXESFORMAT)", (ftnlen)23);
	    }
	}

/*        We need to know the input angle units. */

	gcpool_("EULER_ANGLE_UNITS", &c__1, &c__1, &n, word, &found, (ftnlen)
		17, (ftnlen)40);
	if (! found) {
	    setmsg_("No units for input Euler angles were provided in the se"
		    "tup file using the keyword '#'.", (ftnlen)86);
	    errch_("#", "EULER_ANGLE_UNITS", (ftnlen)1, (ftnlen)17);
	    sigerr_("SPICE(NOEULERANGLEUNITS)", (ftnlen)24);
	}

/*        Do we recognize these units? */

	if (eqstr_(word, "DEGREES", (ftnlen)40, (ftnlen)7)) {
	    earate = rpd_();
	} else if (eqstr_(word, "RADIANS", (ftnlen)40, (ftnlen)7)) {
	    earate = 1.;
	} else {
	    setmsg_("The Euler angle units specified using the setup file ke"
		    "yword '#' were '#'. Acceptable values are only '#' and '"
		    "#'.", (ftnlen)114);
	    errch_("#", "EULER_ANGLE_UNITS", (ftnlen)1, (ftnlen)17);
	    errch_("#", word, (ftnlen)1, (ftnlen)40);
	    errch_("#", "DEGREES", (ftnlen)1, (ftnlen)7);
	    errch_("#", "RADIANS", (ftnlen)1, (ftnlen)7);
	    sigerr_("SPICE(BADEULERANGLEUNITS)", (ftnlen)25);
	}

/*        Body or Space rotation? If no marker, default to space. */

	gcpool_("EULER_ROTATIONS_TYPE", &c__1, &c__1, &n, word, &found, (
		ftnlen)20, (ftnlen)40);
	if (! found) {
	    eulbod = FALSE_;
	} else {
	    if (eqstr_(word, "BODY", (ftnlen)40, (ftnlen)4)) {
		eulbod = TRUE_;
	    } else if (eqstr_(word, "SPACE", (ftnlen)40, (ftnlen)5)) {
		eulbod = FALSE_;
	    } else {
		setmsg_("The Euler rotation type specified using the setup f"
			"ile keyword '#' was '#' while it can be only '#'  or"
			" '#'.", (ftnlen)108);
		errch_("#", "EULER_ROTATIONS_TYPE", (ftnlen)1, (ftnlen)20);
		errch_("#", word, (ftnlen)1, (ftnlen)40);
		errch_("#", "BODY", (ftnlen)1, (ftnlen)4);
		errch_("#", "SPACE", (ftnlen)1, (ftnlen)5);
		sigerr_("SPICE(BADROTATIONTYPE)", (ftnlen)22);
	    }
	}
    }

/*     And at last we check whether we have an offset rotation which */
/*     must be applied to our orientation. */

    dtpool_("OFFSET_ROTATION_ANGLES", &found, &n, word, (ftnlen)22, (ftnlen)
	    40);
    if (found) {

/*        Yes, it looks there was a keyword containing Euler angles */
/*        specifying offset rotation. But did the value contain */
/*        three numbers? */

	if (n != 3 || s_cmp(word, "N", (ftnlen)40, (ftnlen)1) != 0) {
	    setmsg_("Too few or too many offset angle values were specified "
		    "using the setup file keyword '#' or these values weren't"
		    " numbers.", (ftnlen)120);
	    errch_("#", "OFFSET_ROTATION_ANGLES", (ftnlen)1, (ftnlen)22);
	    sigerr_("SPICE(BADOFFSETANGLES)", (ftnlen)22);
	} else {

/*           Get values for angles. */

	    gdpool_("OFFSET_ROTATION_ANGLES", &c__1, &c__3, &n, offang, &
		    found, (ftnlen)22);
	}

/*        Now let's see whether there are axis for these angles. */

	dtpool_("OFFSET_ROTATION_AXES", &found, &n, word, (ftnlen)20, (ftnlen)
		40);
	if (! found || n != 3) {
	    setmsg_("No offset angle axes were specified using the setup fil"
		    "e keyword '#' or this keyword didn't contain an array of"
		    " three values.", (ftnlen)125);
	    errch_("#", "OFFSET_ROTATION_AXES", (ftnlen)1, (ftnlen)20);
	    sigerr_("SPICE(NOOFFSETANGLEAXES)", (ftnlen)24);
	} else {

/*           Yes, there are. Get 'hem! If it's character, we get and */
/*           decode 'X', 'Y' and 'Z'. If it's integer we just check */
/*           it and save it. */

	    if (s_cmp(word, "C", (ftnlen)40, (ftnlen)1) == 0) {
		gcpool_("OFFSET_ROTATION_AXES", &c__1, &c__3, &n, offaxs, &
			found, (ftnlen)20, (ftnlen)1);
		for (i__ = 1; i__ <= 3; ++i__) {
		    if (eqstr_(offaxs + ((i__2 = i__ - 1) < 3 && 0 <= i__2 ? 
			    i__2 : s_rnge("offaxs", i__2, "msopck_", (ftnlen)
			    1938)), "X", (ftnlen)1, (ftnlen)1)) {
			offaxi[(i__2 = i__ - 1) < 3 && 0 <= i__2 ? i__2 : 
				s_rnge("offaxi", i__2, "msopck_", (ftnlen)
				1939)] = 1;
		    } else if (eqstr_(offaxs + ((i__2 = i__ - 1) < 3 && 0 <= 
			    i__2 ? i__2 : s_rnge("offaxs", i__2, "msopck_", (
			    ftnlen)1940)), "Y", (ftnlen)1, (ftnlen)1)) {
			offaxi[(i__2 = i__ - 1) < 3 && 0 <= i__2 ? i__2 : 
				s_rnge("offaxi", i__2, "msopck_", (ftnlen)
				1941)] = 2;
		    } else if (eqstr_(offaxs + ((i__2 = i__ - 1) < 3 && 0 <= 
			    i__2 ? i__2 : s_rnge("offaxs", i__2, "msopck_", (
			    ftnlen)1942)), "Z", (ftnlen)1, (ftnlen)1)) {
			offaxi[(i__2 = i__ - 1) < 3 && 0 <= i__2 ? i__2 : 
				s_rnge("offaxi", i__2, "msopck_", (ftnlen)
				1943)] = 3;
		    } else {
			setmsg_("The element '#' of the offset rotations ord"
				"er provided using the setup file keyword '#'"
				" is not 'X', 'Y' or 'Z'.", (ftnlen)111);
			errch_("#", offaxs + ((i__2 = i__ - 1) < 3 && 0 <= 
				i__2 ? i__2 : s_rnge("offaxs", i__2, "msopck_"
				, (ftnlen)1949)), (ftnlen)1, (ftnlen)1);
			errch_("#", "OFFSET_ROTATION_AXES", (ftnlen)1, (
				ftnlen)20);
			sigerr_("SPICE(BADOFFSETAXISXYZ)", (ftnlen)23);
		    }
		}
	    } else if (s_cmp(word, "N", (ftnlen)40, (ftnlen)1) == 0) {
		gipool_("OFFSET_ROTATION_AXES", &c__1, &c__3, &n, offaxi, &
			found, (ftnlen)20);
		for (i__ = 1; i__ <= 3; ++i__) {
		    if (offaxi[(i__2 = i__ - 1) < 3 && 0 <= i__2 ? i__2 : 
			    s_rnge("offaxi", i__2, "msopck_", (ftnlen)1957)] 
			    <= 0 || offaxi[(i__3 = i__ - 1) < 3 && 0 <= i__3 ?
			     i__3 : s_rnge("offaxi", i__3, "msopck_", (ftnlen)
			    1957)] >= 4) {
			setmsg_("The element # of the Euler rotations order "
				"provided using the setup file keyword '#' is"
				" not 1, 2 or 3.", (ftnlen)102);
			errint_("#", &offaxi[(i__2 = i__ - 1) < 3 && 0 <= 
				i__2 ? i__2 : s_rnge("offaxi", i__2, "msopck_"
				, (ftnlen)1962)], (ftnlen)1);
			errch_("#", "OFFSET_ROTATION_AXES", (ftnlen)1, (
				ftnlen)20);
			sigerr_("SPICE(BADOFFSETAXIS123)", (ftnlen)23);
		    }
		}
	    } else {
		setmsg_("The kernel pool variable '#' type was '#'.", (ftnlen)
			42);
		errch_("#", "OFFSET_ROTATION_AXES", (ftnlen)1, (ftnlen)20);
		errch_("#", word, (ftnlen)1, (ftnlen)40);
		sigerr_("SPICE(BADOFFSETAXESFORMAT)", (ftnlen)26);
	    }
	}

/*        And, what are the units? */

	dtpool_("OFFSET_ROTATION_UNITS", &found, &n, word, (ftnlen)21, (
		ftnlen)40);
	if (! found || n != 1 || s_cmp(word, "C", (ftnlen)40, (ftnlen)1) != 0)
		 {
	    setmsg_("No offset angle units were specified using the setup fi"
		    "le keyword '#' or this keyword wasn't set to a character"
		    " string.", (ftnlen)119);
	    errch_("#", "OFFSET_ROTATION_UNITS", (ftnlen)1, (ftnlen)21);
	    errch_("#", word, (ftnlen)1, (ftnlen)40);
	    sigerr_("SPICE(NOOFFSETANGLEUNITS)", (ftnlen)25);
	} else {

/*           Get units flag. Apply conversion to offset angles. */

	    gcpool_("OFFSET_ROTATION_UNITS", &c__1, &c__1, &n, word, &found, (
		    ftnlen)21, (ftnlen)40);
	    if (eqstr_(word, "DEGREES", (ftnlen)40, (ftnlen)7)) {

/*              Go from degrees to radians. */

		for (i__ = 1; i__ <= 3; ++i__) {
		    offang[(i__2 = i__ - 1) < 3 && 0 <= i__2 ? i__2 : s_rnge(
			    "offang", i__2, "msopck_", (ftnlen)2002)] = 
			    offang[(i__3 = i__ - 1) < 3 && 0 <= i__3 ? i__3 : 
			    s_rnge("offang", i__3, "msopck_", (ftnlen)2002)] *
			     rpd_();
		}
	    } else if (eqstr_(word, "RADIANS", (ftnlen)40, (ftnlen)7)) {

/*              No conversion needed. */

	    } else {
		setmsg_("The offset angle units specified in using the setup"
			" file keyword '#' were '#' while they can be only '#"
			"' or '#'.", (ftnlen)112);
		errch_("#", "OFFSET_ROTATION_UNITS", (ftnlen)1, (ftnlen)21);
		errch_("#", word, (ftnlen)1, (ftnlen)40);
		errch_("#", "DEGREES", (ftnlen)1, (ftnlen)7);
		errch_("#", "RADIANS", (ftnlen)1, (ftnlen)7);
		sigerr_("SPICE(BADOFFSETANGUNITS)", (ftnlen)24);
	    }
	}

/*        It looks like we have got all necessary pieces together. Set */
/*        flag and compute matrix from angles. */

	offrot = TRUE_;
	eul2m_(offang, &offang[1], &offang[2], offaxi, &offaxi[1], &offaxi[2],
		 offmat);
    } else {

/*        Nope, there were no offset Euler angles. Set corresponding */
/*        flag. */

	offrot = FALSE_;
    }

/*     Get down sampling tolerance. If not present, then no down */
/*     sampling is required (the FOUND returned by GDPOOL is the */
/*     flag indicating that.) */

    gdpool_("DOWN_SAMPLE_TOLERANCE", &c__1, &c__1, &n, &sdntol, &dnsmpl, (
	    ftnlen)21);

/*     Check for the flag indicating whether interpolation interval */
/*     table is to be included into the comments and screen output. */

    gcpool_("INCLUDE_INTERVAL_TABLE", &c__1, &c__1, &n, word, &found, (ftnlen)
	    22, (ftnlen)40);
    if (found) {
	if (eqstr_(word, "YES", (ftnlen)40, (ftnlen)3)) {
	    addtab = TRUE_;
	} else if (eqstr_(word, "NO", (ftnlen)40, (ftnlen)2)) {
	    addtab = FALSE_;
	} else {
	    setmsg_("The setup file keyword '#' indicating whether an interp"
		    "olation intervals table should be included into the outp"
		    "ut CK file comment area and the program's output had val"
		    "ue '#'. The only allowed values for this keyword are '#'"
		    " and '#'.", (ftnlen)232);
	    errch_("#", "INCLUDE_INTERVAL_TABLE", (ftnlen)1, (ftnlen)22);
	    errch_("#", word, (ftnlen)1, (ftnlen)40);
	    errch_("#", "YES", (ftnlen)1, (ftnlen)3);
	    errch_("#", "NO", (ftnlen)1, (ftnlen)2);
	    sigerr_("SPICE(BADTABLEFLAG)", (ftnlen)19);
	}
    } else {
	addtab = TRUE_;
    }

/*     Check for the flag indicating whether we need to check for input */
/*     records to be strictly time-ordered. */

    gcpool_("CHECK_TIME_ORDER", &c__1, &c__1, &n, word, &found, (ftnlen)16, (
	    ftnlen)40);
    if (found) {
	if (eqstr_(word, "YES", (ftnlen)40, (ftnlen)3)) {
	    chkto = TRUE_;
	} else if (eqstr_(word, "NO", (ftnlen)40, (ftnlen)2)) {
	    chkto = FALSE_;
	} else {
	    setmsg_("The setup file keyword '#' indicating whether input rec"
		    "ords should be checked to be strictly time-ordered had v"
		    "alue '#'. The only allowed values for this keyword are '"
		    "#' and '#'.", (ftnlen)178);
	    errch_("#", "CHECK_TIME_ORDER", (ftnlen)1, (ftnlen)16);
	    errch_("#", word, (ftnlen)1, (ftnlen)40);
	    errch_("#", "YES", (ftnlen)1, (ftnlen)3);
	    errch_("#", "NO", (ftnlen)1, (ftnlen)2);
	    sigerr_("SPICE(BADCHECKFLAG)", (ftnlen)19);
	}
    } else {
	chkto = FALSE_;
    }

/*     Final touch in setup processing: depending on the input data */
/*     type, output CK type and angular rate flag set the number of we */
/*     expect on each data line. Every line should have at least one */
/*     time tag, thus initially the count is 1. */

    nitems = 1;

/*     For type 2 CKs input might contain the stop time tag. */

    if (cktype == 2 && ! muarat && angrat) {
	++nitems;
    }

/*     For quats, angles, and matrices, add their corresponding size. */

    if (eqstr_(dtype, "MSOP QUATERNIONS", (ftnlen)40, (ftnlen)16) || eqstr_(
	    dtype, "SPICE QUATERNIONS", (ftnlen)40, (ftnlen)17)) {
	nitems += 4;
    } else if (eqstr_(dtype, "EULER ANGLES", (ftnlen)40, (ftnlen)12)) {
	nitems += 3;
    } else if (eqstr_(dtype, "MATRICES", (ftnlen)40, (ftnlen)8)) {
	nitems += 9;
    }

/*     Finally, count in angular rates it they should be picked from the */
/*     input. */

    if (angrat) {
	nitems += 3;
    }

/*     OK, we have got all setups. Time to do some real work. */
/*     Open input file and set record counter to 0. */

    index = 0;
    lcount = 0;
    badcnt = 0;
    savbtm = FALSE_;
    sstrtt = dpmax_();
    sstpt = dpmin_();
    prevt = dpmin_();
    txtopr_(inputf, &inplun, (ftnlen)265);
    s_copy(dashln, "--------------------------------------------------------"
	    "------------------------", (ftnlen)80, (ftnlen)80);
    s_copy(astrln, "********************************************************"
	    "************************", (ftnlen)80, (ftnlen)80);

/*     Read lines from input file until EOF */

    readln_(&inplun, line, &eof, (ftnlen)265);
    while(! eof) {

/*        This second level loop is for writing multiple segments. */
/*        We stop collecting data when EOF or we fill internal data */
/*        buffer completely. */

	while(! eof && index < 100000) {

/*           It's not EOF and buffer is not full. Increment record */
/*           index and go ahead. */

	    ++index;
	    ++lcount;
	    s_copy(linesv, line, (ftnlen)265, (ftnlen)265);

/*           Before doing any parsing let's check if this line contains */
/*           enough data. If not, complain and stop. */

	    if (wdcnt_(line, (ftnlen)265) < nitems) {
		setmsg_("The line # of the input file contains only # space-"
			"delimited items while according to the setup file pa"
			"rameters it is expected to contain # items.", (ftnlen)
			146);
		errint_("#", &lcount, (ftnlen)1);
		i__2 = wdcnt_(line, (ftnlen)265);
		errint_("#", &i__2, (ftnlen)1);
		errint_("#", &nitems, (ftnlen)1);
		sigerr_("SPICE(BADINPUTDATALINE)", (ftnlen)23);
	    }

/*           We need parse our inputs lines. We do it differently */
/*           depending on output CK type, input data type and angular */
/*           rate presence flag. */

/*           First we get the first time tag (and only time tag for */
/*           types 1 and 3). Note that internally we store time as */
/*           ET seconds, not encoded SCLKs. We will convert times to */
/*           SCLKs right before writing CK file. */

	    nextwd_(line, word, line, (ftnlen)265, (ftnlen)40, (ftnlen)265);
	    if (eqstr_(ttype, "SCLK", (ftnlen)40, (ftnlen)4)) {
		scencd_(&scid, word, &sclkdp, (ftnlen)40);
		sct2e_(&scid, &sclkdp, &startt[(i__2 = index - 1) < 100000 && 
			0 <= i__2 ? i__2 : s_rnge("startt", i__2, "msopck_", (
			ftnlen)2215)]);
	    } else if (eqstr_(ttype, "UTC", (ftnlen)40, (ftnlen)3)) {
		str2et_(word, &startt[(i__2 = index - 1) < 100000 && 0 <= 
			i__2 ? i__2 : s_rnge("startt", i__2, "msopck_", (
			ftnlen)2219)], (ftnlen)40);
	    } else if (eqstr_(ttype, "TICKS", (ftnlen)40, (ftnlen)5)) {
		nparsd_(word, &sclkdp, error, &ptr, (ftnlen)40, (ftnlen)265);
		if (ptr != 0) {
		    setmsg_("The DP SCLK (ticks) time '#' from the line # of"
			    " the input file is not a number.", (ftnlen)79);
		    errch_("#", word, (ftnlen)1, (ftnlen)40);
		    errint_("#", &lcount, (ftnlen)1);
		    sigerr_("SPICE(BADDPSCLK1)", (ftnlen)17);
		}
		sct2e_(&scid, &sclkdp, &startt[(i__2 = index - 1) < 100000 && 
			0 <= i__2 ? i__2 : s_rnge("startt", i__2, "msopck_", (
			ftnlen)2234)]);
	    } else if (eqstr_(ttype, "ET", (ftnlen)40, (ftnlen)2)) {
		nparsd_(word, &startt[(i__2 = index - 1) < 100000 && 0 <= 
			i__2 ? i__2 : s_rnge("startt", i__2, "msopck_", (
			ftnlen)2238)], error, &ptr, (ftnlen)40, (ftnlen)265);
		if (ptr != 0) {
		    setmsg_("The ET seconds past J2000 time '#' from the lin"
			    "e # of the input file is not a number.", (ftnlen)
			    85);
		    errch_("#", word, (ftnlen)1, (ftnlen)40);
		    errint_("#", &lcount, (ftnlen)1);
		    sigerr_("SPICE(BADET1)", (ftnlen)13);
		}
	    } else if (eqstr_(ttype, "DSCLK", (ftnlen)40, (ftnlen)5)) {

/*              A decimal SCLK must be a DP number. */

		nparsd_(word, &sclkdp, error, &ptr, (ftnlen)40, (ftnlen)265);
		if (ptr != 0) {
		    setmsg_("The decimal SCLK time '#' from the line # of th"
			    "e input file is not a number.", (ftnlen)76);
		    errch_("#", word, (ftnlen)1, (ftnlen)40);
		    errint_("#", &lcount, (ftnlen)1);
		    sigerr_("SPICE(BADDECIMALSCLK1)", (ftnlen)22);
		}

/*              Extract left and right SCLK fields from decimal SCLK */
/*              while saving fractional ticks. */

		rmaind_(&sclkdp, &c_b605, &clklft, &clkfrc);
		clkfrc *= sclkmd[1];
		rmaind_(&clkfrc, &c_b605, &clkrgh, &tmpdp);
		clkfrc = tmpdp;
		clkrgh += sclkof[1];

/*              Re-package left and right fields into SCLK string, */
/*              convert it to encoded SCLK, add fractional part to it, */
/*              and convert more precise encoded SCLK to ET. */

		s_copy(hword, "#:#", (ftnlen)40, (ftnlen)3);
		i__2 = i_dnnt(&clklft);
		repmi_(hword, "#", &i__2, hword, (ftnlen)40, (ftnlen)1, (
			ftnlen)40);
		i__2 = i_dnnt(&clkrgh);
		repmi_(hword, "#", &i__2, hword, (ftnlen)40, (ftnlen)1, (
			ftnlen)40);
		scencd_(&scid, hword, &sclkdp, (ftnlen)40);
		sclkdp += clkfrc;
		sct2e_(&scid, &sclkdp, &startt[(i__2 = index - 1) < 100000 && 
			0 <= i__2 ? i__2 : s_rnge("startt", i__2, "msopck_", (
			ftnlen)2286)]);
	    }

/*           If requested, check for time-ordered input. Signal an */
/*           error if it is not. */

	    if (chkto) {
		if (startt[(i__2 = index - 1) < 100000 && 0 <= i__2 ? i__2 : 
			s_rnge("startt", i__2, "msopck_", (ftnlen)2296)] <= 
			prevt) {
		    setmsg_("The time '#' from the line # of the input file "
			    "is less than or equal to the time from the previ"
			    "ous line. The input file records must be strictl"
			    "y time-ordered.", (ftnlen)158);
		    errch_("#", word, (ftnlen)1, (ftnlen)40);
		    errint_("#", &lcount, (ftnlen)1);
		    sigerr_("SPICE(TIMESOUTOFORDER)", (ftnlen)22);
		}
		prevt = startt[(i__2 = index - 1) < 100000 && 0 <= i__2 ? 
			i__2 : s_rnge("startt", i__2, "msopck_", (ftnlen)2308)
			];
	    }

/*           Add time bias (TIMCOR is 0 if no correction was requested.) */

	    startt[(i__2 = index - 1) < 100000 && 0 <= i__2 ? i__2 : s_rnge(
		    "startt", i__2, "msopck_", (ftnlen)2315)] = startt[(i__3 =
		     index - 1) < 100000 && 0 <= i__3 ? i__3 : s_rnge("startt"
		    , i__3, "msopck_", (ftnlen)2315)] + timcor;

/*           For type 2 there is a second time tag, but only if */
/*           we have real angular rates provided on the input. If */
/*           angular rates will have to be made up, we don't expect */
/*           second time tag. */

	    if (cktype == 2 && ! muarat && angrat) {
		nextwd_(line, word, line, (ftnlen)265, (ftnlen)40, (ftnlen)
			265);
		if (eqstr_(ttype, "SCLK", (ftnlen)40, (ftnlen)4)) {
		    scencd_(&scid, word, &sclkdp, (ftnlen)40);
		    sct2e_(&scid, &sclkdp, &stopt[(i__2 = index - 1) < 100000 
			    && 0 <= i__2 ? i__2 : s_rnge("stopt", i__2, "mso"
			    "pck_", (ftnlen)2330)]);
		} else if (eqstr_(ttype, "UTC", (ftnlen)40, (ftnlen)3)) {
		    str2et_(word, &stopt[(i__2 = index - 1) < 100000 && 0 <= 
			    i__2 ? i__2 : s_rnge("stopt", i__2, "msopck_", (
			    ftnlen)2334)], (ftnlen)40);
		} else if (eqstr_(ttype, "TICKS", (ftnlen)40, (ftnlen)5)) {
		    nparsd_(word, &sclkdp, error, &ptr, (ftnlen)40, (ftnlen)
			    265);
		    if (ptr != 0) {
			setmsg_("The DP SCLK (ticks) time '#' from the line "
				"# of the input file is not a number.", (
				ftnlen)79);
			errch_("#", word, (ftnlen)1, (ftnlen)40);
			errint_("#", &lcount, (ftnlen)1);
			sigerr_("SPICE(BADDPSCLK2)", (ftnlen)17);
		    }
		    sct2e_(&scid, &sclkdp, &stopt[(i__2 = index - 1) < 100000 
			    && 0 <= i__2 ? i__2 : s_rnge("stopt", i__2, "mso"
			    "pck_", (ftnlen)2349)]);
		} else if (eqstr_(ttype, "ET", (ftnlen)40, (ftnlen)2)) {
		    nparsd_(word, &stopt[(i__2 = index - 1) < 100000 && 0 <= 
			    i__2 ? i__2 : s_rnge("stopt", i__2, "msopck_", (
			    ftnlen)2353)], error, &ptr, (ftnlen)40, (ftnlen)
			    265);
		    if (ptr != 0) {
			setmsg_("The ET seconds past J2000 time '#' from the"
				" line # of the input file is not a number.", (
				ftnlen)85);
			errch_("#", word, (ftnlen)1, (ftnlen)40);
			errint_("#", &lcount, (ftnlen)1);
			sigerr_("SPICE(BADET2)", (ftnlen)13);
		    }
		} else if (eqstr_(ttype, "DSCLK", (ftnlen)40, (ftnlen)5)) {

/*                 A decimal SCLK must be a DP number. */

		    nparsd_(word, &sclkdp, error, &ptr, (ftnlen)40, (ftnlen)
			    265);
		    if (ptr != 0) {
			setmsg_("The decimal SCLK time '#' from the line # o"
				"f the input file is not a number.", (ftnlen)
				76);
			errch_("#", word, (ftnlen)1, (ftnlen)40);
			errint_("#", &lcount, (ftnlen)1);
			sigerr_("SPICE(BADDECIMALSCLK2)", (ftnlen)22);
		    }

/*                 Extract left and right SCLK fields from decimal SCLK */
/*                 while saving fractional ticks. */

		    rmaind_(&sclkdp, &c_b605, &clklft, &clkfrc);
		    clkfrc *= sclkmd[1];
		    rmaind_(&clkfrc, &c_b605, &clkrgh, &tmpdp);
		    clkfrc = tmpdp;
		    clkrgh += sclkof[1];

/*                 Re-package left and right fields into SCLK string, */
/*                 convert it to encoded SCLK, add fractional part to it, */
/*                 and convert more precise encoded SCLK to ET. */

		    s_copy(hword, "#:#", (ftnlen)40, (ftnlen)3);
		    i__2 = i_dnnt(&clklft);
		    repmi_(hword, "#", &i__2, hword, (ftnlen)40, (ftnlen)1, (
			    ftnlen)40);
		    i__2 = i_dnnt(&clkrgh);
		    repmi_(hword, "#", &i__2, hword, (ftnlen)40, (ftnlen)1, (
			    ftnlen)40);
		    scencd_(&scid, hword, &sclkdp, (ftnlen)40);
		    sclkdp += clkfrc;
		    sct2e_(&scid, &sclkdp, &stopt[(i__2 = index - 1) < 100000 
			    && 0 <= i__2 ? i__2 : s_rnge("stopt", i__2, "mso"
			    "pck_", (ftnlen)2402)]);
		}

/*              Add time bias (TIMCOR is 0 if no correction was */
/*              requested.) */

		stopt[(i__2 = index - 1) < 100000 && 0 <= i__2 ? i__2 : 
			s_rnge("stopt", i__2, "msopck_", (ftnlen)2410)] = 
			stopt[(i__3 = index - 1) < 100000 && 0 <= i__3 ? i__3 
			: s_rnge("stopt", i__3, "msopck_", (ftnlen)2410)] + 
			timcor;
	    }

/*           Next item(s) that we need to get belong to the orientation */
/*           data part of the input record and convert it to the */
/*           quaternion. */

	    if (eqstr_(dtype, "MSOP QUATERNIONS", (ftnlen)40, (ftnlen)16)) {

/*              For MSOP-type quaternions we first extract them into an */
/*              intermediate quaternion. */

		for (i__ = 1; i__ <= 4; ++i__) {
		    nextwd_(line, word, line, (ftnlen)265, (ftnlen)40, (
			    ftnlen)265);
		    nparsd_(word, &hquat[(i__2 = i__ - 1) < 4 && 0 <= i__2 ? 
			    i__2 : s_rnge("hquat", i__2, "msopck_", (ftnlen)
			    2427)], error, &ptr, (ftnlen)40, (ftnlen)265);
		    if (ptr != 0) {
			setmsg_("The quaternion component '#' from the line "
				"# of the input file is not a number.", (
				ftnlen)79);
			errch_("#", word, (ftnlen)1, (ftnlen)40);
			errint_("#", &lcount, (ftnlen)1);
			sigerr_("SPICE(BADMSOPQUATERNION)", (ftnlen)24);
		    }
		}

/*              And after that we reassign it to the main buffer and */
/*              conjugate (shift/negate) it along the way. */

		quats[(i__2 = (index << 2) - 4) < 400000 && 0 <= i__2 ? i__2 :
			 s_rnge("quats", i__2, "msopck_", (ftnlen)2441)] = 
			hquat[3];
		quats[(i__2 = (index << 2) - 3) < 400000 && 0 <= i__2 ? i__2 :
			 s_rnge("quats", i__2, "msopck_", (ftnlen)2442)] = 
			-hquat[0];
		quats[(i__2 = (index << 2) - 2) < 400000 && 0 <= i__2 ? i__2 :
			 s_rnge("quats", i__2, "msopck_", (ftnlen)2443)] = 
			-hquat[1];
		quats[(i__2 = (index << 2) - 1) < 400000 && 0 <= i__2 ? i__2 :
			 s_rnge("quats", i__2, "msopck_", (ftnlen)2444)] = 
			-hquat[2];
	    } else if (eqstr_(dtype, "SPICE QUATERNIONS", (ftnlen)40, (ftnlen)
		    17)) {

/*              For SPICE-style quaternions we simply extract them */
/*              into the main buffer . */

		for (i__ = 1; i__ <= 4; ++i__) {
		    nextwd_(line, word, line, (ftnlen)265, (ftnlen)40, (
			    ftnlen)265);
		    nparsd_(word, &quats[(i__2 = i__ + (index << 2) - 5) < 
			    400000 && 0 <= i__2 ? i__2 : s_rnge("quats", i__2,
			     "msopck_", (ftnlen)2453)], error, &ptr, (ftnlen)
			    40, (ftnlen)265);
		    if (ptr != 0) {
			setmsg_("The quaternion component '#' from the line "
				"# of the input file is not a number.", (
				ftnlen)79);
			errch_("#", word, (ftnlen)1, (ftnlen)40);
			errint_("#", &lcount, (ftnlen)1);
			sigerr_("SPICE(BADSPICEQUATERNION)", (ftnlen)25);
		    }
		}
	    } else if (eqstr_(dtype, "EULER ANGLES", (ftnlen)40, (ftnlen)12)) 
		    {

/*              For Euler angles we extract them into an intermediate */
/*              array, convert to matrix, after that to quaternion and */
/*              store quaternion in the main buffer. */

		for (i__ = 1; i__ <= 3; ++i__) {
		    nextwd_(line, word, line, (ftnlen)265, (ftnlen)40, (
			    ftnlen)265);
		    nparsd_(word, &eulang[(i__2 = i__ - 1) < 3 && 0 <= i__2 ? 
			    i__2 : s_rnge("eulang", i__2, "msopck_", (ftnlen)
			    2472)], error, &ptr, (ftnlen)40, (ftnlen)265);
		    if (ptr != 0) {
			setmsg_("The angle '#' from the line # of the input "
				"file is not a number.", (ftnlen)64);
			errch_("#", word, (ftnlen)1, (ftnlen)40);
			errint_("#", &lcount, (ftnlen)1);
			sigerr_("SPICE(BADANGLE)", (ftnlen)15);
		    }
		}

/*              If a Body based rotation. */

		if (eulbod) {
		    eulang[0] = -eulang[0];
		    eulang[1] = -eulang[1];
		    eulang[2] = -eulang[2];
		}

/*              Convert the angles definitions to a rotation matrix. */

		d__1 = eulang[0] * earate;
		d__2 = eulang[1] * earate;
		d__3 = eulang[2] * earate;
		eul2m_(&d__1, &d__2, &d__3, eulaxs, &eulaxs[1], &eulaxs[2], 
			mat);

/*              If a Body based rotation. */

		if (eulbod) {
		    xpose_(mat, tmpmat);
		    mequ_(tmpmat, mat);
		}

/*              Convert the matrix to quaternions. */

		m2q_(mat, &quats[(i__2 = (index << 2) - 4) < 400000 && 0 <= 
			i__2 ? i__2 : s_rnge("quats", i__2, "msopck_", (
			ftnlen)2512)]);
	    } else if (eqstr_(dtype, "MATRICES", (ftnlen)40, (ftnlen)8)) {

/*              For matrices we extract them into an intermediate */
/*              matrix, after that convert it to quaternion and */
/*              store quaternion in the main buffer. Note that we expect */
/*              matrices to be provided in row-major order. */

		for (i__ = 1; i__ <= 3; ++i__) {
		    for (j = 1; j <= 3; ++j) {
			nextwd_(line, word, line, (ftnlen)265, (ftnlen)40, (
				ftnlen)265);
			nparsd_(word, &mat[(i__2 = i__ + j * 3 - 4) < 9 && 0 
				<= i__2 ? i__2 : s_rnge("mat", i__2, "msopck_"
				, (ftnlen)2525)], error, &ptr, (ftnlen)40, (
				ftnlen)265);
			if (ptr != 0) {
			    setmsg_("The matrix element '#' from the line # "
				    "of the input file is not a number.", (
				    ftnlen)73);
			    errch_("#", word, (ftnlen)1, (ftnlen)40);
			    errint_("#", &lcount, (ftnlen)1);
			    sigerr_("SPICE(BADMATRIX)", (ftnlen)16);
			}
		    }
		}
		m2q_(mat, &quats[(i__2 = (index << 2) - 4) < 400000 && 0 <= 
			i__2 ? i__2 : s_rnge("quats", i__2, "msopck_", (
			ftnlen)2537)]);
	    }

/*           We are done with orientation data. Now it's time for */
/*           angular rates. We need the only if angular rate presence */
/*           flag is set to 'YES'. */

	    if (angrat) {

/*              Well, there is no freedom in angular rate */
/*              representation -- we should always have 3 elements. */

		for (i__ = 1; i__ <= 3; ++i__) {
		    nextwd_(line, word, line, (ftnlen)265, (ftnlen)40, (
			    ftnlen)265);
		    nparsd_(word, &avvs[(i__2 = i__ + index * 3 - 4) < 300000 
			    && 0 <= i__2 ? i__2 : s_rnge("avvs", i__2, "msop"
			    "ck_", (ftnlen)2553)], error, &ptr, (ftnlen)40, (
			    ftnlen)265);
		    if (ptr != 0) {
			setmsg_("The angular rate element '#' from the line "
				"# of the input file is not a number.", (
				ftnlen)79);
			errch_("#", word, (ftnlen)1, (ftnlen)40);
			errint_("#", &lcount, (ftnlen)1);
			sigerr_("SPICE(BADANGULARRATE)", (ftnlen)21);
		    }

/*                 But in case of Euler angles input, angular rates must */
/*                 be expressed in units consistent with units for */
/*                 angles. */

		    if (eqstr_(dtype, "EULER ANGLES", (ftnlen)40, (ftnlen)12))
			     {
			avvs[(i__2 = i__ + index * 3 - 4) < 300000 && 0 <= 
				i__2 ? i__2 : s_rnge("avvs", i__2, "msopck_", 
				(ftnlen)2569)] = avvs[(i__3 = i__ + index * 3 
				- 4) < 300000 && 0 <= i__3 ? i__3 : s_rnge(
				"avvs", i__3, "msopck_", (ftnlen)2569)] * 
				earate;
		    }
		}
	    }

/*           Now let's check whether orientation and angular rate data */
/*           that we have obtained from this input line are good. */
/*           We do quaternion first but only if filtering for quats was */
/*           requested. */

	    badqua = FALSE_;
	    if (qfilter) {

/*              Algorithm for quaternion normalization/check implemented */
/*              in a few steps below was borrowed from ATTREC, ver 1.1. */

/*              Step 1: Normalize quaternion */

		vhatg_(&quats[(i__2 = (index << 2) - 4) < 400000 && 0 <= i__2 
			? i__2 : s_rnge("quats", i__2, "msopck_", (ftnlen)
			2592)], &c__4, qn);

/*              Step 2: Calculate Norm of original Quaternion */

		normq = vnormg_(&quats[(i__2 = (index << 2) - 4) < 400000 && 
			0 <= i__2 ? i__2 : s_rnge("quats", i__2, "msopck_", (
			ftnlen)2597)], &c__4);

/*              Step 3: Check for goodness of quaternion components by */
/*              comparison of each element with it's normalized element. */

		for (j = 1; j <= 4; ++j) {
		    if ((d__1 = quats[(i__2 = j + (index << 2) - 5) < 400000 
			    && 0 <= i__2 ? i__2 : s_rnge("quats", i__2, "mso"
			    "pck_", (ftnlen)2604)] - qn[(i__3 = j - 1) < 4 && 
			    0 <= i__3 ? i__3 : s_rnge("qn", i__3, "msopck_", (
			    ftnlen)2604)], abs(d__1)) > qerror) {
			badqua = TRUE_;
		    }
		}

/*              Step 4: Check for quaternion = zero vector. */

		if (normq == 0.) {
		    badqua = TRUE_;
		}
	    }

/*           Now angular rate. Again, only if rate information was */
/*           present and filtering was requested. */

	    badrat = FALSE_;
	    if (angrat && rfilter) {
		if ((d__1 = avvs[(i__2 = index * 3 - 3) < 300000 && 0 <= i__2 
			? i__2 : s_rnge("avvs", i__2, "msopck_", (ftnlen)2626)
			], abs(d__1)) > rerror[0] || (d__2 = avvs[(i__3 = 
			index * 3 - 2) < 300000 && 0 <= i__3 ? i__3 : s_rnge(
			"avvs", i__3, "msopck_", (ftnlen)2626)], abs(d__2)) > 
			rerror[1] || (d__3 = avvs[(i__4 = index * 3 - 1) < 
			300000 && 0 <= i__4 ? i__4 : s_rnge("avvs", i__4, 
			"msopck_", (ftnlen)2626)], abs(d__3)) > rerror[2]) {

/*                 One of the components of this rate doesn't */
/*                 into specified threshold. */

		    badrat = TRUE_;
		}
	    }

/*           Is this record bad? Write the line into scratch buffer, */
/*           decrement INDEX and proceed to the next line. */

	    if (badqua || badrat) {
		--index;
		++badcnt;

/*              Is scratch file open? Open it if not. */

		if (! seopnd) {
		    txtops_(&selun);
		    seopnd = TRUE_;
		}

/*              Write the line number, diagnostics and line itself. */

		s_copy(hline, "# ON LINE #: #", (ftnlen)265, (ftnlen)14);
		if (badqua) {
		    repmc_(hline, "#", "BAD QAUT", hline, (ftnlen)265, (
			    ftnlen)1, (ftnlen)8, (ftnlen)265);
		} else {
		    repmc_(hline, "#", "BAD RATE", hline, (ftnlen)265, (
			    ftnlen)1, (ftnlen)8, (ftnlen)265);
		}
		repmi_(hline, "#", &lcount, hline, (ftnlen)265, (ftnlen)1, (
			ftnlen)265);
		repmc_(hline, "#", linesv, hline, (ftnlen)265, (ftnlen)1, (
			ftnlen)265, (ftnlen)265);
		writln_(hline, &selun, (ftnlen)265);
	    } else {

/*              It looks like both, quaternion and angular rate, were */
/*              OK. Let's see if the AR was given relative to the */
/*              instrument frame rather than relative to reference */
/*              frame. */

		if (insarf && angrat) {

/*                 Yes, it was. We need to compute matrix and multiply */
/*                 AR by the transpose of that matrix. */

		    q2m_(&quats[(i__2 = (index << 2) - 4) < 400000 && 0 <= 
			    i__2 ? i__2 : s_rnge("quats", i__2, "msopck_", (
			    ftnlen)2685)], mat);
		    mtxv_(mat, &avvs[(i__2 = index * 3 - 3) < 300000 && 0 <= 
			    i__2 ? i__2 : s_rnge("avvs", i__2, "msopck_", (
			    ftnlen)2686)], tmpvec);
		    vequ_(tmpvec, &avvs[(i__2 = index * 3 - 3) < 300000 && 0 
			    <= i__2 ? i__2 : s_rnge("avvs", i__2, "msopck_", (
			    ftnlen)2687)]);
		}

/*              Let's also see if there is an additional offset that */
/*              must be added to this rotation. */

		if (offrot) {

/*                 Apply it to quaternion first. */

		    q2m_(&quats[(i__2 = (index << 2) - 4) < 400000 && 0 <= 
			    i__2 ? i__2 : s_rnge("quats", i__2, "msopck_", (
			    ftnlen)2700)], mat);
		    mxm_(mat, offmat, tmpmat);
		    m2q_(tmpmat, &quats[(i__2 = (index << 2) - 4) < 400000 && 
			    0 <= i__2 ? i__2 : s_rnge("quats", i__2, "msopck_"
			    , (ftnlen)2702)]);

/*                 Apply it to angular rate, if it's present. */

		    if (angrat) {
			mtxv_(offmat, &avvs[(i__2 = index * 3 - 3) < 300000 &&
				 0 <= i__2 ? i__2 : s_rnge("avvs", i__2, 
				"msopck_", (ftnlen)2708)], tmpvec);
			vequ_(tmpvec, &avvs[(i__2 = index * 3 - 3) < 300000 &&
				 0 <= i__2 ? i__2 : s_rnge("avvs", i__2, 
				"msopck_", (ftnlen)2709)]);
		    }
		}
	    }

/*           We have collected all data from the current line; read */
/*           the next line. */

	    readln_(&inplun, line, &eof, (ftnlen)265);

/*           End of the secondary (fill buffer) loop. */

	}

/*        We either reached EOF or filled our buffers. In any case, */
/*        we need to check whether we need to write a segment, do */
/*        nothing or complain if no data was collected at all. */

	if (index == 0) {

/*           We can have INDEX equal zero at this point for only one */
/*           reason: the input file contained some data records but all */
/*           of them were "filtered out" using values from quaternion */
/*           and/or angular rate threshold parameters specified in the */
/*           setup file. Let the user know about this. */

	    setmsg_("Of the # data lines from the input file none contained "
		    "values that satisfied quaternion and/or angular rate thr"
		    "eshold constraints given in the setup file.", (ftnlen)154)
		    ;
	    errint_("#", &lcount, (ftnlen)1);
	    sigerr_("SPICE(NOACCEPTABLEDATA)", (ftnlen)23);
	} else if (index == 1) {

/*           If we have one data point in the buffer it can be a carry- */
/*           over from the previous segment. Let's check whether we have */
/*           already written something to the output CK file. */

	    if (ckopnd) {

/*              We did. So this point is a carry over. We don't need */
/*              to write one more segment containing just it. */

		wrtseg = FALSE_;
	    } else {

/*              It looks like it's the one and only data point provided */
/*              in the input file. We need to write one-point segment. */

		wrtseg = TRUE_;
	    }
	} else {

/*           We have more than one data point -- we need to write */
/*           another segment. */

	    wrtseg = TRUE_;
	}

/*        Let's see what we decided. */

	if (wrtseg) {

/*           It looks like we need to write a segment. First, lets */
/*           find order of primary record times -- every CK type that */
/*           this program can output requires times to be in strictly */
/*           increasing order. */

	    orderd_(startt, &index, iorder);

/*           Reorder all buffers by found order. */

	    reordd_(iorder, &index, startt);
	    reordd_(iorder, &index, stopt);

/*           Reorder quats column by column. */

	    for (i__ = 1; i__ <= 4; ++i__) {
		i__2 = index;
		for (j = 1; j <= i__2; ++j) {
		    hdparr[(i__3 = j - 1) < 100000 && 0 <= i__3 ? i__3 : 
			    s_rnge("hdparr", i__3, "msopck_", (ftnlen)2807)] =
			     quats[(i__4 = i__ + (j << 2) - 5) < 400000 && 0 
			    <= i__4 ? i__4 : s_rnge("quats", i__4, "msopck_", 
			    (ftnlen)2807)];
		}
		reordd_(iorder, &index, hdparr);
		i__2 = index;
		for (j = 1; j <= i__2; ++j) {
		    quats[(i__3 = i__ + (j << 2) - 5) < 400000 && 0 <= i__3 ? 
			    i__3 : s_rnge("quats", i__3, "msopck_", (ftnlen)
			    2811)] = hdparr[(i__4 = j - 1) < 100000 && 0 <= 
			    i__4 ? i__4 : s_rnge("hdparr", i__4, "msopck_", (
			    ftnlen)2811)];
		}
	    }

/*           Do the same for AVVS. */

	    for (i__ = 1; i__ <= 3; ++i__) {
		i__2 = index;
		for (j = 1; j <= i__2; ++j) {
		    hdparr[(i__3 = j - 1) < 100000 && 0 <= i__3 ? i__3 : 
			    s_rnge("hdparr", i__3, "msopck_", (ftnlen)2820)] =
			     avvs[(i__4 = i__ + j * 3 - 4) < 300000 && 0 <= 
			    i__4 ? i__4 : s_rnge("avvs", i__4, "msopck_", (
			    ftnlen)2820)];
		}
		reordd_(iorder, &index, hdparr);
		i__2 = index;
		for (j = 1; j <= i__2; ++j) {
		    avvs[(i__3 = i__ + j * 3 - 4) < 300000 && 0 <= i__3 ? 
			    i__3 : s_rnge("avvs", i__3, "msopck_", (ftnlen)
			    2824)] = hdparr[(i__4 = j - 1) < 100000 && 0 <= 
			    i__4 ? i__4 : s_rnge("hdparr", i__4, "msopck_", (
			    ftnlen)2824)];
		}
	    }

/*           We need to save the last collected data point to use at */
/*           as first point of the next CK segment (if there will be */
/*           such.) */

	    sstrtt = startt[(i__2 = index - 1) < 100000 && 0 <= i__2 ? i__2 : 
		    s_rnge("startt", i__2, "msopck_", (ftnlen)2833)];
	    sstpt = stopt[(i__2 = index - 1) < 100000 && 0 <= i__2 ? i__2 : 
		    s_rnge("stopt", i__2, "msopck_", (ftnlen)2834)];
	    squat[0] = quats[(i__2 = (index << 2) - 4) < 400000 && 0 <= i__2 ?
		     i__2 : s_rnge("quats", i__2, "msopck_", (ftnlen)2836)];
	    squat[1] = quats[(i__2 = (index << 2) - 3) < 400000 && 0 <= i__2 ?
		     i__2 : s_rnge("quats", i__2, "msopck_", (ftnlen)2837)];
	    squat[2] = quats[(i__2 = (index << 2) - 2) < 400000 && 0 <= i__2 ?
		     i__2 : s_rnge("quats", i__2, "msopck_", (ftnlen)2838)];
	    squat[3] = quats[(i__2 = (index << 2) - 1) < 400000 && 0 <= i__2 ?
		     i__2 : s_rnge("quats", i__2, "msopck_", (ftnlen)2839)];
	    savv[0] = avvs[(i__2 = index * 3 - 3) < 300000 && 0 <= i__2 ? 
		    i__2 : s_rnge("avvs", i__2, "msopck_", (ftnlen)2841)];
	    savv[1] = avvs[(i__2 = index * 3 - 2) < 300000 && 0 <= i__2 ? 
		    i__2 : s_rnge("avvs", i__2, "msopck_", (ftnlen)2842)];
	    savv[2] = avvs[(i__2 = index * 3 - 1) < 300000 && 0 <= i__2 ? 
		    i__2 : s_rnge("avvs", i__2, "msopck_", (ftnlen)2843)];

/*           For all CK segments we will create a coverage */
/*           summary table in the comment area of the output */
/*           file. The header of the table is the same for all CK */
/*           types. First we check whether scratch that will contain */
/*           comments is opened already; if not -- open it. */

	    if (! siopnd) {
		txtops_(&silun);
		siopnd = TRUE_;
	    }

/*           Write segment interval table header. */

	    s_copy(hline, "SEG.SUMMARY: ID #, COVERG: # #", (ftnlen)265, (
		    ftnlen)30);
	    repmi_(hline, "#", &instid, hline, (ftnlen)265, (ftnlen)1, (
		    ftnlen)265);
	    timout_(startt, "YYYY-MM-DDTHR:MN:SC.###", hword, (ftnlen)23, (
		    ftnlen)40);
	    repmc_(hline, "#", hword, hline, (ftnlen)265, (ftnlen)1, (ftnlen)
		    40, (ftnlen)265);
	    if (cktype == 2 && ! muarat) {
		timout_(&stopt[(i__2 = index - 1) < 100000 && 0 <= i__2 ? 
			i__2 : s_rnge("stopt", i__2, "msopck_", (ftnlen)2865)]
			, "YYYY-MM-DDTHR:MN:SC.###", hword, (ftnlen)23, (
			ftnlen)40);
	    } else {
		timout_(&startt[(i__2 = index - 1) < 100000 && 0 <= i__2 ? 
			i__2 : s_rnge("startt", i__2, "msopck_", (ftnlen)2867)
			], "YYYY-MM-DDTHR:MN:SC.###", hword, (ftnlen)23, (
			ftnlen)40);
	    }
	    repmc_(hline, "#", hword, hline, (ftnlen)265, (ftnlen)1, (ftnlen)
		    40, (ftnlen)265);
	    writln_(hline, &silun, (ftnlen)265);
	    writln_(dashln, &silun, (ftnlen)80);

/*           Now we will get all additional data that is */
/*           needed but wasn't present in the input file. This */
/*           data is different for a different CK types. */

	    if (cktype == 1) {

/*              Guess.. we have everything for Type 1. What a luck! */
/*              We just need to convert times to encoded SCLKs. */

		i__2 = index;
		for (i__ = 1; i__ <= i__2; ++i__) {
		    sce2c_(&scid, &startt[(i__3 = i__ - 1) < 100000 && 0 <= 
			    i__3 ? i__3 : s_rnge("startt", i__3, "msopck_", (
			    ftnlen)2885)], &tmpdp);
		    startt[(i__3 = i__ - 1) < 100000 && 0 <= i__3 ? i__3 : 
			    s_rnge("startt", i__3, "msopck_", (ftnlen)2886)] =
			     tmpdp;
		}

/*              One other things we need to do is to put into the */
/*              comment area coverage table a warning message saying */
/*              that segment contains discrete data. */

		s_copy(hline, "DISCRETE POINTING DATA -- COVERAGE SUMMARY IS"
			" NOT APPLICABLE", (ftnlen)265, (ftnlen)60);
		writln_(hline, &silun, (ftnlen)265);
	    } else if (cktype == 2) {

/*              Lets see whether we have got angular rates from the */
/*              input or we have to make them up. */

		if (muarat) {

/*                 Well, we have to make them up. This how we do it: */

/*                 -- if time interval between two quaternions is */
/*                    less or equal to the maximum interpolation */
/*                    interval set in the setup file, we set stop time */
/*                    for current record to the start time of the next */
/*                    record */

/*                 -- then we compute constant angular rate getting us */
/*                    from orientation expressed by the current */
/*                    quaternions to the orientation expressed by the */
/*                    next one and save it as the rate in current */
/*                    record; */

/*                 -- if time interval between current and next is too */
/*                    big, we set stop time to start + TIKTOL and */
/*                    angular rate to 0; */

/*                 We go into the loop only if we have more than one */
/*                 record. */

		    if (index > 1) {
			i__2 = index - 1;
			for (i__ = 1; i__ <= i__2; ++i__) {

/*                       Check time spacing between the current and */
/*                       the next point. */

			    if (startt[(i__3 = i__) < 100000 && 0 <= i__3 ? 
				    i__3 : s_rnge("startt", i__3, "msopck_", (
				    ftnlen)2935)] - startt[(i__4 = i__ - 1) < 
				    100000 && 0 <= i__4 ? i__4 : s_rnge("sta"
				    "rtt", i__4, "msopck_", (ftnlen)2935)] <= 
				    intrvl) {

/*                          We can interpolate between these points. */
/*                          If it's the first point we compute matrix */
/*                          from current quaternion. Otherwise we */
/*                          will use saved matrix. */

				if (i__ == 1) {
				    vhatg_(&quats[(i__3 = (i__ << 2) - 4) < 
					    400000 && 0 <= i__3 ? i__3 : 
					    s_rnge("quats", i__3, "msopck_", (
					    ftnlen)2944)], &c__4, qn);
				    q2m_(qn, curmat);
				}

/*                          Compute matrix from the next quaternion. */

				vhatg_(&quats[(i__3 = (i__ + 1 << 2) - 4) < 
					400000 && 0 <= i__3 ? i__3 : s_rnge(
					"quats", i__3, "msopck_", (ftnlen)
					2951)], &c__4, qn);
				q2m_(qn, nxtmat);

/*                          Compute scaled angular rate using LINROT_M */
/*                          from MGSSPICE and divide it by time */
/*                          duration, if it's non-zero. */

				linrot_m__(curmat, nxtmat, &c_b605, hmat, 
					scldav);
				if (startt[(i__3 = i__) < 100000 && 0 <= i__3 
					? i__3 : s_rnge("startt", i__3, "mso"
					"pck_", (ftnlen)2962)] - startt[(i__4 =
					 i__ - 1) < 100000 && 0 <= i__4 ? 
					i__4 : s_rnge("startt", i__4, "msopc"
					"k_", (ftnlen)2962)] > 0.) {
				    hrate = 1. / (startt[(i__3 = i__) < 
					    100000 && 0 <= i__3 ? i__3 : 
					    s_rnge("startt", i__3, "msopck_", 
					    (ftnlen)2964)] - startt[(i__4 = 
					    i__ - 1) < 100000 && 0 <= i__4 ? 
					    i__4 : s_rnge("startt", i__4, 
					    "msopck_", (ftnlen)2964)]);
				    vscl_(&hrate, scldav, &avvs[(i__3 = i__ * 
					    3 - 3) < 300000 && 0 <= i__3 ? 
					    i__3 : s_rnge("avvs", i__3, "mso"
					    "pck_", (ftnlen)2965)]);
				    stopt[(i__3 = i__ - 1) < 100000 && 0 <= 
					    i__3 ? i__3 : s_rnge("stopt", 
					    i__3, "msopck_", (ftnlen)2966)] = 
					    startt[(i__4 = i__) < 100000 && 0 
					    <= i__4 ? i__4 : s_rnge("startt", 
					    i__4, "msopck_", (ftnlen)2966)];
				} else {

/*                             Current and next times are the same -- */
/*                             "NO CAN DO, SORRY ..." */

				    setmsg_("Two identical times (# ET) were"
					    " provided in the input file. Ang"
					    "ular rates cannot be made up.", (
					    ftnlen)92);
				    errdp_("#", &startt[(i__3 = i__) < 100000 
					    && 0 <= i__3 ? i__3 : s_rnge(
					    "startt", i__3, "msopck_", (
					    ftnlen)2979)], (ftnlen)1);
				    sigerr_("SPICE(IDENTICALTIMES1)", (ftnlen)
					    22);
				}
			    } else {

/*                          Well, the current and the next points are */
/*                          too far apart -- we cannot interpolate */
/*                          between them and therefore we set stop time */
/*                          to start time + TIKTOL and rate to zero. */

				stopt[(i__3 = i__ - 1) < 100000 && 0 <= i__3 ?
					 i__3 : s_rnge("stopt", i__3, "msopc"
					"k_", (ftnlen)2992)] = startt[(i__4 = 
					i__ - 1) < 100000 && 0 <= i__4 ? i__4 
					: s_rnge("startt", i__4, "msopck_", (
					ftnlen)2992)] + 1e-6;
				avvs[(i__3 = i__ * 3 - 3) < 300000 && 0 <= 
					i__3 ? i__3 : s_rnge("avvs", i__3, 
					"msopck_", (ftnlen)2993)] = 0.;
				avvs[(i__3 = i__ * 3 - 2) < 300000 && 0 <= 
					i__3 ? i__3 : s_rnge("avvs", i__3, 
					"msopck_", (ftnlen)2994)] = 0.;
				avvs[(i__3 = i__ * 3 - 1) < 300000 && 0 <= 
					i__3 ? i__3 : s_rnge("avvs", i__3, 
					"msopck_", (ftnlen)2995)] = 0.;

/*                          Compute matrix from the next quaternion */
/*                          because it's going to get re-assigned and */
/*                          used as "current" matrix on the next step. */

				vhatg_(&quats[(i__3 = (i__ + 1 << 2) - 4) < 
					400000 && 0 <= i__3 ? i__3 : s_rnge(
					"quats", i__3, "msopck_", (ftnlen)
					3002)], &c__4, qn);
				q2m_(qn, nxtmat);
			    }

/*                       Copy "next" matrix to "current" matrix. */

			    mequ_(nxtmat, curmat);
			}
		    }

/*                 Now, for the last (and maybe only :) record: set */
/*                 stop time to start time + TIKTOL and rate to zero. */

		    stopt[(i__2 = index - 1) < 100000 && 0 <= i__2 ? i__2 : 
			    s_rnge("stopt", i__2, "msopck_", (ftnlen)3020)] = 
			    startt[(i__3 = index - 1) < 100000 && 0 <= i__3 ? 
			    i__3 : s_rnge("startt", i__3, "msopck_", (ftnlen)
			    3020)] + 1e-6;
		    avvs[(i__2 = index * 3 - 3) < 300000 && 0 <= i__2 ? i__2 :
			     s_rnge("avvs", i__2, "msopck_", (ftnlen)3021)] = 
			    0.;
		    avvs[(i__2 = index * 3 - 2) < 300000 && 0 <= i__2 ? i__2 :
			     s_rnge("avvs", i__2, "msopck_", (ftnlen)3022)] = 
			    0.;
		    avvs[(i__2 = index * 3 - 1) < 300000 && 0 <= i__2 ? i__2 :
			     s_rnge("avvs", i__2, "msopck_", (ftnlen)3023)] = 
			    0.;
		}

/*              Generate comment area intervals table. */

		hrate = startt[0];
		i__2 = index;
		for (i__ = 1; i__ <= i__2; ++i__) {
		    if (i__ == index) {
			s_copy(hline, "      #    #", (ftnlen)265, (ftnlen)12)
				;
			timout_(&hrate, "YYYY-MM-DDTHR:MN:SC.###", hword, (
				ftnlen)23, (ftnlen)40);
			repmc_(hline, "#", hword, hline, (ftnlen)265, (ftnlen)
				1, (ftnlen)40, (ftnlen)265);
			timout_(&stopt[(i__3 = i__ - 1) < 100000 && 0 <= i__3 
				? i__3 : s_rnge("stopt", i__3, "msopck_", (
				ftnlen)3040)], "YYYY-MM-DDTHR:MN:SC.###", 
				hword, (ftnlen)23, (ftnlen)40);
			repmc_(hline, "#", hword, hline, (ftnlen)265, (ftnlen)
				1, (ftnlen)40, (ftnlen)265);
			writln_(hline, &silun, (ftnlen)265);
		    } else {
			if (stopt[(i__3 = i__ - 1) < 100000 && 0 <= i__3 ? 
				i__3 : s_rnge("stopt", i__3, "msopck_", (
				ftnlen)3047)] < startt[(i__4 = i__) < 100000 
				&& 0 <= i__4 ? i__4 : s_rnge("startt", i__4, 
				"msopck_", (ftnlen)3047)]) {
			    s_copy(hline, "      #    #", (ftnlen)265, (
				    ftnlen)12);
			    timout_(&hrate, "YYYY-MM-DDTHR:MN:SC.###", hword, 
				    (ftnlen)23, (ftnlen)40);
			    repmc_(hline, "#", hword, hline, (ftnlen)265, (
				    ftnlen)1, (ftnlen)40, (ftnlen)265);
			    timout_(&stopt[(i__3 = i__ - 1) < 100000 && 0 <= 
				    i__3 ? i__3 : s_rnge("stopt", i__3, "mso"
				    "pck_", (ftnlen)3053)], "YYYY-MM-DDTHR:MN"
				    ":SC.###", hword, (ftnlen)23, (ftnlen)40);
			    repmc_(hline, "#", hword, hline, (ftnlen)265, (
				    ftnlen)1, (ftnlen)40, (ftnlen)265);
			    writln_(hline, &silun, (ftnlen)265);
			    hrate = startt[(i__3 = i__) < 100000 && 0 <= i__3 
				    ? i__3 : s_rnge("startt", i__3, "msopck_",
				     (ftnlen)3058)];
			}
		    }
		}

/*              And at last, we need to convert ETs to SCLKs for start */
/*              and stop times. We will check for time consistency and */
/*              recompute rates along the way. */

		i__2 = index;
		for (i__ = 1; i__ <= i__2; ++i__) {
		    if (startt[(i__3 = i__ - 1) < 100000 && 0 <= i__3 ? i__3 :
			     s_rnge("startt", i__3, "msopck_", (ftnlen)3073)] 
			    >= stopt[(i__4 = i__ - 1) < 100000 && 0 <= i__4 ? 
			    i__4 : s_rnge("stopt", i__4, "msopck_", (ftnlen)
			    3073)]) {
			setmsg_("Start time (# ET) of an input record is gre"
				"ater than or equal to stop time (# ET). This"
				" is not allowed for Type 2 CK input.", (
				ftnlen)123);
			errdp_("#", &startt[(i__3 = i__ - 1) < 100000 && 0 <= 
				i__3 ? i__3 : s_rnge("startt", i__3, "msopck_"
				, (ftnlen)3079)], (ftnlen)1);
			errdp_("#", &stopt[(i__3 = i__ - 1) < 100000 && 0 <= 
				i__3 ? i__3 : s_rnge("stopt", i__3, "msopck_",
				 (ftnlen)3080)], (ftnlen)1);
			sigerr_("SPICE(INCONSISTENTTIMES1)", (ftnlen)25);
		    }
		    sce2c_(&scid, &startt[(i__3 = i__ - 1) < 100000 && 0 <= 
			    i__3 ? i__3 : s_rnge("startt", i__3, "msopck_", (
			    ftnlen)3084)], &tmpdp1);
		    sce2c_(&scid, &stopt[(i__3 = i__ - 1) < 100000 && 0 <= 
			    i__3 ? i__3 : s_rnge("stopt", i__3, "msopck_", (
			    ftnlen)3085)], &tmpdp2);
		    if (tmpdp1 >= tmpdp2) {
			setmsg_("Encoded ticks (#) corresponding to start ti"
				"me (# ET) of an input record are greater tha"
				"n or equal to encoded ticks # corresponding "
				"to stop time (# ET). This is not allowed for"
				" Type 2 CK input.", (ftnlen)192);
			errdp_("#", &tmpdp1, (ftnlen)1);
			errdp_("#", &startt[(i__3 = i__ - 1) < 100000 && 0 <= 
				i__3 ? i__3 : s_rnge("startt", i__3, "msopck_"
				, (ftnlen)3098)], (ftnlen)1);
			errdp_("#", &tmpdp2, (ftnlen)1);
			errdp_("#", &stopt[(i__3 = i__ - 1) < 100000 && 0 <= 
				i__3 ? i__3 : s_rnge("stopt", i__3, "msopck_",
				 (ftnlen)3100)], (ftnlen)1);
			sigerr_("SPICE(INCONSISTENTTIMES2)", (ftnlen)25);
		    }
		    rates[(i__3 = i__ - 1) < 100000 && 0 <= i__3 ? i__3 : 
			    s_rnge("rates", i__3, "msopck_", (ftnlen)3104)] = 
			    (stopt[(i__4 = i__ - 1) < 100000 && 0 <= i__4 ? 
			    i__4 : s_rnge("stopt", i__4, "msopck_", (ftnlen)
			    3104)] - startt[(i__5 = i__ - 1) < 100000 && 0 <= 
			    i__5 ? i__5 : s_rnge("startt", i__5, "msopck_", (
			    ftnlen)3104)]) / (tmpdp2 - tmpdp1);
		    startt[(i__3 = i__ - 1) < 100000 && 0 <= i__3 ? i__3 : 
			    s_rnge("startt", i__3, "msopck_", (ftnlen)3107)] =
			     tmpdp1;
		    stopt[(i__3 = i__ - 1) < 100000 && 0 <= i__3 ? i__3 : 
			    s_rnge("stopt", i__3, "msopck_", (ftnlen)3108)] = 
			    tmpdp2;
		}
	    } else if (cktype == 3) {

/*              For type 3 we must come up with interpolation intervals */
/*              using maximum interval duration from the setup file; */
/*              RATES array will hold beginnings of these intervals. We */
/*              will also use STOPT array to hold ends of intervals */
/*              though they are not needed for CK data, just for */
/*              intervals table in the comment area. */

		nints = 1;
		rates[(i__2 = nints - 1) < 100000 && 0 <= i__2 ? i__2 : 
			s_rnge("rates", i__2, "msopck_", (ftnlen)3123)] = 
			startt[0];
		if (index > 1) {
		    i__2 = index;
		    for (i__ = 2; i__ <= i__2; ++i__) {
			if (startt[(i__3 = i__ - 1) < 100000 && 0 <= i__3 ? 
				i__3 : s_rnge("startt", i__3, "msopck_", (
				ftnlen)3127)] - startt[(i__4 = i__ - 2) < 
				100000 && 0 <= i__4 ? i__4 : s_rnge("startt", 
				i__4, "msopck_", (ftnlen)3127)] > intrvl) {
			    ++nints;
			    rates[(i__3 = nints - 1) < 100000 && 0 <= i__3 ? 
				    i__3 : s_rnge("rates", i__3, "msopck_", (
				    ftnlen)3129)] = startt[(i__4 = i__ - 1) < 
				    100000 && 0 <= i__4 ? i__4 : s_rnge("sta"
				    "rtt", i__4, "msopck_", (ftnlen)3129)];
			    stopt[(i__3 = nints - 2) < 100000 && 0 <= i__3 ? 
				    i__3 : s_rnge("stopt", i__3, "msopck_", (
				    ftnlen)3130)] = startt[(i__4 = i__ - 2) < 
				    100000 && 0 <= i__4 ? i__4 : s_rnge("sta"
				    "rtt", i__4, "msopck_", (ftnlen)3130)];
			}
		    }
		}
		stopt[(i__2 = nints - 1) < 100000 && 0 <= i__2 ? i__2 : 
			s_rnge("stopt", i__2, "msopck_", (ftnlen)3134)] = 
			startt[(i__3 = index - 1) < 100000 && 0 <= i__3 ? 
			i__3 : s_rnge("startt", i__3, "msopck_", (ftnlen)3134)
			];

/*              Lets see whether we have got angular rates from the */
/*              input or we have to make them up. */

		if (muarat) {

/*                 Well, we have to make them up. Lets check if have */
/*                 more than one point. */

		    if (index > 1) {

/*                    We sure do :) -- let's go over points and */
/*                    compute rates for each of them. We will take */
/*                    special care of the points that are starts */
/*                    and stops of the interpolation intervals. */

			i__2 = index;
			for (i__ = 2; i__ <= i__2; ++i__) {

/*                       If it's the second point we compute matrix */
/*                       for the first quaternion. Otherwise we */
/*                       will use saved matrix. */

			    if (i__ == 2) {
				vhatg_(&quats[(i__3 = (i__ - 1 << 2) - 4) < 
					400000 && 0 <= i__3 ? i__3 : s_rnge(
					"quats", i__3, "msopck_", (ftnlen)
					3162)], &c__4, qn);
				q2m_(qn, prvmat);
			    }

/*                       Now we compute matrix from current quaternion */
/*                       and compute constant angular rate for rotation */
/*                       between them. */

			    vhatg_(&quats[(i__3 = (i__ << 2) - 4) < 400000 && 
				    0 <= i__3 ? i__3 : s_rnge("quats", i__3, 
				    "msopck_", (ftnlen)3171)], &c__4, qn);
			    q2m_(qn, curmat);
			    linrot_m__(prvmat, curmat, &c_b605, hmat, scldav);
			    if (startt[(i__3 = i__ - 1) < 100000 && 0 <= i__3 
				    ? i__3 : s_rnge("startt", i__3, "msopck_",
				     (ftnlen)3176)] - startt[(i__4 = i__ - 2) 
				    < 100000 && 0 <= i__4 ? i__4 : s_rnge(
				    "startt", i__4, "msopck_", (ftnlen)3176)] 
				    != 0.) {
				hrate = 1. / (startt[(i__3 = i__ - 1) < 
					100000 && 0 <= i__3 ? i__3 : s_rnge(
					"startt", i__3, "msopck_", (ftnlen)
					3178)] - startt[(i__4 = i__ - 2) < 
					100000 && 0 <= i__4 ? i__4 : s_rnge(
					"startt", i__4, "msopck_", (ftnlen)
					3178)]);
				vscl_(&hrate, scldav, &avvs[(i__3 = i__ * 3 - 
					3) < 300000 && 0 <= i__3 ? i__3 : 
					s_rnge("avvs", i__3, "msopck_", (
					ftnlen)3179)]);
			    } else {

/*                          Current and next times are the same -- */
/*                          "NO CAN DO, SORRY ..." */

				setmsg_("Two identical times (# ET) were pro"
					"vided in the input file. Angular rat"
					"es cannot be made up.", (ftnlen)92);
				errdp_("#", &startt[(i__3 = i__) < 100000 && 
					0 <= i__3 ? i__3 : s_rnge("startt", 
					i__3, "msopck_", (ftnlen)3190)], (
					ftnlen)1);
				sigerr_("SPICE(IDENTICALTIMES2)", (ftnlen)22);
			    }

/*                       Now we need to check whether our previous point */
/*                       is at the start or stop of some interpolation */
/*                       interval. Search for its time in the interval */
/*                       start and stop time arrays. */

			    srtidx = isrchd_(&startt[(i__3 = i__ - 2) < 
				    100000 && 0 <= i__3 ? i__3 : s_rnge("sta"
				    "rtt", i__3, "msopck_", (ftnlen)3201)], &
				    nints, rates);
			    stpidx = isrchd_(&startt[(i__3 = i__ - 2) < 
				    100000 && 0 <= i__3 ? i__3 : s_rnge("sta"
				    "rtt", i__3, "msopck_", (ftnlen)3202)], &
				    nints, stopt);
			    if (srtidx != 0 && stpidx != 0) {

/*                          Well, previous point is a an interval by */
/*                          itself -- it's present in both start and */
/*                          stop arrays -- the only thing we can do */
/*                          for it is to set its rate to zero. */

				avvs[(i__3 = (i__ - 1) * 3 - 3) < 300000 && 0 
					<= i__3 ? i__3 : s_rnge("avvs", i__3, 
					"msopck_", (ftnlen)3212)] = 0.;
				avvs[(i__3 = (i__ - 1) * 3 - 2) < 300000 && 0 
					<= i__3 ? i__3 : s_rnge("avvs", i__3, 
					"msopck_", (ftnlen)3213)] = 0.;
				avvs[(i__3 = (i__ - 1) * 3 - 1) < 300000 && 0 
					<= i__3 ? i__3 : s_rnge("avvs", i__3, 
					"msopck_", (ftnlen)3214)] = 0.;
			    } else if (srtidx != 0) {

/*                          Previous point is the start of some */
/*                          interval. We need to set its rate to the */
/*                          value we just computed, the one that */
/*                          bring us from that point to the current */
/*                          point. */

				vequ_(&avvs[(i__3 = i__ * 3 - 3) < 300000 && 
					0 <= i__3 ? i__3 : s_rnge("avvs", 
					i__3, "msopck_", (ftnlen)3225)], &
					avvs[(i__4 = (i__ - 1) * 3 - 3) < 
					300000 && 0 <= i__4 ? i__4 : s_rnge(
					"avvs", i__4, "msopck_", (ftnlen)3225)
					]);
			    } else if (stpidx != 0) {

/*                          Previous point is the stop of some */
/*                          interval. Correct rate -- the one that */
/*                          rotates from the point before previous */
/*                          point to the previous point -- is already */
/*                          in there. We don't need to do anything */
/*                          to change it. */

			    } else {

/*                          Well, our previous point is just a point */
/*                          in a middle. Based on user request, we will */
/*                          save computed rate in the previous point */
/*                          or compute average value of this rate and */
/*                          previous rate and save it in the previous */
/*                          point. */

				if (avgrat) {

/*                             This is what the program did originally, */
/*                             prior to the version 2.0 -- rate at given */
/*                             point was computed as an average of the */
/*                             rate which bring us from the previous */
/*                             point to this point and the rate that */
/*                             bring us from this point to the next */
/*                             point. */

				    vadd_(&avvs[(i__3 = (i__ - 1) * 3 - 3) < 
					    300000 && 0 <= i__3 ? i__3 : 
					    s_rnge("avvs", i__3, "msopck_", (
					    ftnlen)3259)], &avvs[(i__4 = i__ *
					     3 - 3) < 300000 && 0 <= i__4 ? 
					    i__4 : s_rnge("avvs", i__4, "mso"
					    "pck_", (ftnlen)3259)], tmpvec);
				    vscl_(&c_b968, tmpvec, &avvs[(i__3 = (i__ 
					    - 1) * 3 - 3) < 300000 && 0 <= 
					    i__3 ? i__3 : s_rnge("avvs", i__3,
					     "msopck_", (ftnlen)3260)]);
				} else {

/*                             But it turned out that this averaging */
/*                             doesn't work for spinners with nutation. */
/*                             It produced bizarre results that just */
/*                             didn't make any sense in terms of physics. */
/*                             Then averaging approach was augmented with */
/*                             something similar to type 2 -- storing */
/*                             in previous point the rate that takes us */
/*                             from previous point to the current point. */

				    vequ_(&avvs[(i__3 = i__ * 3 - 3) < 300000 
					    && 0 <= i__3 ? i__3 : s_rnge(
					    "avvs", i__3, "msopck_", (ftnlen)
					    3274)], &avvs[(i__4 = (i__ - 1) * 
					    3 - 3) < 300000 && 0 <= i__4 ? 
					    i__4 : s_rnge("avvs", i__4, "mso"
					    "pck_", (ftnlen)3274)]);
				}
			    }

/*                       Memorize current matrix in previous. */

			    mequ_(curmat, prvmat);
			}

/*                    Check whether the very last point was an interval */
/*                    itself and set rate to zero. Otherwise, the rate */
/*                    which is there is OK already. */

			if (startt[(i__2 = index - 1) < 100000 && 0 <= i__2 ? 
				i__2 : s_rnge("startt", i__2, "msopck_", (
				ftnlen)3292)] == rates[(i__3 = nints - 1) < 
				100000 && 0 <= i__3 ? i__3 : s_rnge("rates", 
				i__3, "msopck_", (ftnlen)3292)]) {
			    avvs[(i__2 = index * 3 - 3) < 300000 && 0 <= i__2 
				    ? i__2 : s_rnge("avvs", i__2, "msopck_", (
				    ftnlen)3294)] = 0.;
			    avvs[(i__2 = index * 3 - 2) < 300000 && 0 <= i__2 
				    ? i__2 : s_rnge("avvs", i__2, "msopck_", (
				    ftnlen)3295)] = 0.;
			    avvs[(i__2 = index * 3 - 1) < 300000 && 0 <= i__2 
				    ? i__2 : s_rnge("avvs", i__2, "msopck_", (
				    ftnlen)3296)] = 0.;
			}
		    } else {

/*                    Set angular rate of our only point to zero. */

			avvs[(i__2 = index * 3 - 3) < 300000 && 0 <= i__2 ? 
				i__2 : s_rnge("avvs", i__2, "msopck_", (
				ftnlen)3305)] = 0.;
			avvs[(i__2 = index * 3 - 2) < 300000 && 0 <= i__2 ? 
				i__2 : s_rnge("avvs", i__2, "msopck_", (
				ftnlen)3306)] = 0.;
			avvs[(i__2 = index * 3 - 1) < 300000 && 0 <= i__2 ? 
				i__2 : s_rnge("avvs", i__2, "msopck_", (
				ftnlen)3307)] = 0.;
		    }
		}

/*              Convert ETs to SCLKs for start times. */

		i__2 = index;
		for (i__ = 1; i__ <= i__2; ++i__) {
		    sce2c_(&scid, &startt[(i__3 = i__ - 1) < 100000 && 0 <= 
			    i__3 ? i__3 : s_rnge("startt", i__3, "msopck_", (
			    ftnlen)3317)], &tmpdp);
		    startt[(i__3 = i__ - 1) < 100000 && 0 <= i__3 ? i__3 : 
			    s_rnge("startt", i__3, "msopck_", (ftnlen)3318)] =
			     tmpdp;
		}

/*              Generate comment area intervals table. */

		i__2 = nints;
		for (i__ = 1; i__ <= i__2; ++i__) {
		    s_copy(hline, "      #    #", (ftnlen)265, (ftnlen)12);
		    timout_(&rates[(i__3 = i__ - 1) < 100000 && 0 <= i__3 ? 
			    i__3 : s_rnge("rates", i__3, "msopck_", (ftnlen)
			    3326)], "YYYY-MM-DDTHR:MN:SC.###", hword, (ftnlen)
			    23, (ftnlen)40);
		    repmc_(hline, "#", hword, hline, (ftnlen)265, (ftnlen)1, (
			    ftnlen)40, (ftnlen)265);
		    timout_(&stopt[(i__3 = i__ - 1) < 100000 && 0 <= i__3 ? 
			    i__3 : s_rnge("stopt", i__3, "msopck_", (ftnlen)
			    3328)], "YYYY-MM-DDTHR:MN:SC.###", hword, (ftnlen)
			    23, (ftnlen)40);
		    repmc_(hline, "#", hword, hline, (ftnlen)265, (ftnlen)1, (
			    ftnlen)40, (ftnlen)265);
		    writln_(hline, &silun, (ftnlen)265);
		}

/*              Now (at last!) convert interval start times to SCLKs. */

		i__2 = nints;
		for (i__ = 1; i__ <= i__2; ++i__) {
		    sce2c_(&scid, &rates[(i__3 = i__ - 1) < 100000 && 0 <= 
			    i__3 ? i__3 : s_rnge("rates", i__3, "msopck_", (
			    ftnlen)3337)], &tmpdp);
		    rates[(i__3 = i__ - 1) < 100000 && 0 <= i__3 ? i__3 : 
			    s_rnge("rates", i__3, "msopck_", (ftnlen)3338)] = 
			    tmpdp;
		}
	    }

/*           Add one more line at the bottom of the segment coverage */
/*           summary table. */

	    writln_(" ", &silun, (ftnlen)1);

/*           All processing is done. We can write CK data now. But */
/*           did we open the file already? */

	    if (! ckopnd) {

/*              Was it a new or existing file? */

		if (appndf) {

/*                 It's an existing file. Use DAFOPR to open it for */
/*                 write access. */

		    dafopw_(outptf, &handle, (ftnlen)265);
		} else {

/*                 It's a new file. Use CKOPN. Let's reserve a */
/*                 few hundreds records for future comments. */

		    ckopn_(outptf, ifname, &c__300, &handle, (ftnlen)265, (
			    ftnlen)80);
		}

/*              Don't forget to set OPENED flag. :) */

		ckopnd = TRUE_;
	    }

/*           We opened the CK for write access. Now call the proper */
/*           CKWxx writer depending on what type of segment we */
/*           need to write. Also save current end time for future */
/*           comment area meta-information output. */

	    if (cktype == 1) {
		ckw01_(&handle, startt, &startt[(i__2 = index - 1) < 100000 &&
			 0 <= i__2 ? i__2 : s_rnge("startt", i__2, "msopck_", 
			(ftnlen)3387)], &instid, frmnam, &angrat, segid, &
			index, startt, quats, avvs, (ftnlen)40, (ftnlen)40);
		endtim = startt[(i__2 = index - 1) < 100000 && 0 <= i__2 ? 
			i__2 : s_rnge("startt", i__2, "msopck_", (ftnlen)3391)
			];
	    } else if (cktype == 2) {
		ckw02_(&handle, startt, &stopt[(i__2 = index - 1) < 100000 && 
			0 <= i__2 ? i__2 : s_rnge("stopt", i__2, "msopck_", (
			ftnlen)3395)], &instid, frmnam, segid, &index, startt,
			 stopt, quats, avvs, rates, (ftnlen)40, (ftnlen)40);
		endtim = stopt[(i__2 = index - 1) < 100000 && 0 <= i__2 ? 
			i__2 : s_rnge("stopt", i__2, "msopck_", (ftnlen)3400)]
			;
	    } else if (cktype == 3) {
		arflag = angrat || muarat;
		if (dnsmpl) {
		    ck3sdn_(&sdntol, &arflag, &index, startt, quats, avvs, &
			    nints, rates, hdparr, iorder);
		}
		ckw03_(&handle, startt, &startt[(i__2 = index - 1) < 100000 &&
			 0 <= i__2 ? i__2 : s_rnge("startt", i__2, "msopck_", 
			(ftnlen)3412)], &instid, frmnam, &arflag, segid, &
			index, startt, quats, avvs, &nints, rates, (ftnlen)40,
			 (ftnlen)40);
		endtim = startt[(i__2 = index - 1) < 100000 && 0 <= i__2 ? 
			i__2 : s_rnge("startt", i__2, "msopck_", (ftnlen)3417)
			];
	    }

/*           Save the very first start time to use in run time */
/*           meta-information output to comment area. */

	    if (! savbtm) {
		savbtm = TRUE_;
		begtim = startt[0];
	    }
	}

/*        Don't forget to reset index. We reset it to 1 copy the */
/*        last data point written to current segment to the first */
/*        data point to be written to the next segment. */

	index = 1;
	startt[0] = sstrtt;
	stopt[0] = sstpt;
	quats[0] = squat[0];
	quats[1] = squat[1];
	quats[2] = squat[2];
	quats[3] = squat[3];
	avvs[0] = savv[0];
	avvs[1] = savv[1];
	avvs[2] = savv[2];

/*        End of the primary (read input data) loop. */

    }

/*     Let see if we encountered EOF with our first read and therefore */
/*     didn't even get into the main loop. */

    if (! ckopnd) {
	setmsg_("The input data file '#' doesn't contain any data.", (ftnlen)
		49);
	errch_("#", inputf, (ftnlen)1, (ftnlen)265);
	sigerr_("SPICE(EMPTYINPUTFILE)", (ftnlen)21);
    }

/*     Collect all comments in one big scratch file. Open it first. */

    txtops_(&scrtch);

/*     Copy contents the external comments file. Clean up non-printing */
/*     characters on the way. */

    if (! appndf && cmmflg) {
	writln_(" ", &scrtch, (ftnlen)1);
	writln_(astrln, &scrtch, (ftnlen)80);
	writln_(" ", &scrtch, (ftnlen)1);
	txtopr_(cmmntf, &hlun, (ftnlen)265);
	readln_(&hlun, line, &eof, (ftnlen)265);
	while(! eof) {
	    while(frstnp_(line, (ftnlen)265) != 0) {
		n = frstnp_(line, (ftnlen)265);
		*(unsigned char *)&line[n - 1] = ' ';
	    }
	    writln_(line, &scrtch, (ftnlen)265);
	    readln_(&hlun, line, &eof, (ftnlen)265);
	}
	cl__1.cerr = 0;
	cl__1.cunit = hlun;
	cl__1.csta = 0;
	f_clos(&cl__1);
    }

/*     Copy contents of the setup file */

    writln_(" ", &scrtch, (ftnlen)1);
    writln_(astrln, &scrtch, (ftnlen)80);
    s_copy(line, "MSOPCK SETUP FILE: #", (ftnlen)265, (ftnlen)20);
    repmc_(line, "#", setupf, line, (ftnlen)265, (ftnlen)1, (ftnlen)265, (
	    ftnlen)265);
    writln_(line, &scrtch, (ftnlen)265);
    writln_(astrln, &scrtch, (ftnlen)80);
    writln_(" ", &scrtch, (ftnlen)1);
    txtopr_(setupf, &hlun, (ftnlen)265);
    readln_(&hlun, line, &eof, (ftnlen)265);
    while(! eof) {
	while(frstnp_(line, (ftnlen)265) != 0) {
	    n = frstnp_(line, (ftnlen)265);
	    *(unsigned char *)&line[n - 1] = ' ';
	}
	writln_(line, &scrtch, (ftnlen)265);
	readln_(&hlun, line, &eof, (ftnlen)265);
    }
    cl__1.cerr = 0;
    cl__1.cunit = hlun;
    cl__1.csta = 0;
    f_clos(&cl__1);

/*     Run time meta information -- start, stop and creation times. */

    writln_(" ", &scrtch, (ftnlen)1);
    writln_(astrln, &scrtch, (ftnlen)80);
    s_copy(line, "RUN-TIME OBTAINED META INFORMATION:", (ftnlen)265, (ftnlen)
	    35);
    writln_(line, &scrtch, (ftnlen)265);
    writln_(astrln, &scrtch, (ftnlen)80);
    writln_(" ", &scrtch, (ftnlen)1);
    s_copy(hline, "PRODUCT_CREATION_TIME = #", (ftnlen)265, (ftnlen)25);
    s_copy(hword, "YYYY-MM-DDTHR:MN:SC", (ftnlen)40, (ftnlen)19);
    cputim_(tvec);
    dpfmt_(tvec, "0YYY", hword, (ftnlen)4, (ftnlen)4);
    dpfmt_(&tvec[1], "0M", hword + 5, (ftnlen)2, (ftnlen)2);
    dpfmt_(&tvec[2], "0D", hword + 8, (ftnlen)2, (ftnlen)2);
    dpfmt_(&tvec[3], "0h", hword + 11, (ftnlen)2, (ftnlen)2);
    dpfmt_(&tvec[4], "0m", hword + 14, (ftnlen)2, (ftnlen)2);
    dpfmt_(&tvec[5], "0s", hword + 17, (ftnlen)2, (ftnlen)2);
    repmc_(hline, "#", hword, hline, (ftnlen)265, (ftnlen)1, (ftnlen)40, (
	    ftnlen)265);
    writln_(hline, &scrtch, (ftnlen)265);
    s_copy(hline, "START_TIME            = #", (ftnlen)265, (ftnlen)25);
    sct2e_(&scid, &begtim, &tmpdp);
    begtim = tmpdp;
    timout_(&begtim, "YYYY-MM-DDTHR:MN:SC.###", hword, (ftnlen)23, (ftnlen)40)
	    ;
    repmc_(hline, "#", hword, hline, (ftnlen)265, (ftnlen)1, (ftnlen)40, (
	    ftnlen)265);
    writln_(hline, &scrtch, (ftnlen)265);
    s_copy(hline, "STOP_TIME             = #", (ftnlen)265, (ftnlen)25);
    sct2e_(&scid, &endtim, &tmpdp);
    endtim = tmpdp;
    timout_(&endtim, "YYYY-MM-DDTHR:MN:SC.###", hword, (ftnlen)23, (ftnlen)40)
	    ;
    repmc_(hline, "#", hword, hline, (ftnlen)265, (ftnlen)1, (ftnlen)40, (
	    ftnlen)265);
    writln_(hline, &scrtch, (ftnlen)265);

/*     Copy intervals information from scratch file. */

    if (siopnd && addtab) {
	writln_(" ", &scrtch, (ftnlen)1);
	writln_(astrln, &scrtch, (ftnlen)80);
	s_copy(line, "INTERPOLATION INTERVALS IN THE FILE SEGMENTS:", (ftnlen)
		265, (ftnlen)45);
	writln_(line, &scrtch, (ftnlen)265);
	writln_(astrln, &scrtch, (ftnlen)80);
	writln_(" ", &scrtch, (ftnlen)1);
	al__1.aerr = 0;
	al__1.aunit = silun;
	f_rew(&al__1);
	readln_(&silun, line, &eof, (ftnlen)265);
	while(! eof) {
	    writln_(line, &scrtch, (ftnlen)265);
	    readln_(&silun, line, &eof, (ftnlen)265);
	}
    }

/*     Copy bad input records numbers. */

    if (seopnd) {
	writln_(" ", &scrtch, (ftnlen)1);
	writln_(astrln, &scrtch, (ftnlen)80);
	s_copy(line, "BAD DATA RECORDS FROM THE INPUT FILE: # OF # (#%)", (
		ftnlen)265, (ftnlen)49);
	repmi_(line, "#", &badcnt, line, (ftnlen)265, (ftnlen)1, (ftnlen)265);
	repmi_(line, "#", &lcount, line, (ftnlen)265, (ftnlen)1, (ftnlen)265);
	d__1 = badcnt * 100. / lcount;
	repmf_(line, "#", &d__1, &c__4, "F", line, (ftnlen)265, (ftnlen)1, (
		ftnlen)1, (ftnlen)265);
	writln_(line, &scrtch, (ftnlen)265);
	writln_(astrln, &scrtch, (ftnlen)80);
	writln_(" ", &scrtch, (ftnlen)1);
	al__1.aerr = 0;
	al__1.aunit = selun;
	f_rew(&al__1);
	readln_(&selun, line, &eof, (ftnlen)265);
	while(! eof) {
	    writln_(line, &scrtch, (ftnlen)265);
	    readln_(&selun, line, &eof, (ftnlen)265);
	}
    }
    writln_(" ", &scrtch, (ftnlen)1);
    writln_(astrln, &scrtch, (ftnlen)80);

/*     Add comments to the file. */

    al__1.aerr = 0;
    al__1.aunit = scrtch;
    f_rew(&al__1);
    if (ckopnd) {
	spcac_(&handle, &scrtch, " ", " ", (ftnlen)1, (ftnlen)1);
    }

/*     Dump comments to the screen. */

    al__1.aerr = 0;
    al__1.aunit = scrtch;
    f_rew(&al__1);
    readln_(&scrtch, line, &eof, (ftnlen)265);
    while(! eof) {
	tostdo_(line, rtrim_(line, (ftnlen)265));
	readln_(&scrtch, line, &eof, (ftnlen)265);
    }

/*     Close input file. */

    cl__1.cerr = 0;
    cl__1.cunit = inplun;
    cl__1.csta = 0;
    f_clos(&cl__1);

/*     Close output CK file. */

    if (ckopnd) {
	dafcls_(&handle);
    }

/*     Dump CK for testing. */

    gcpool_("DUMP_OUTPUT_CK", &c__1, &c__1, &n, line, &found, (ftnlen)14, (
	    ftnlen)265);
    if (found && eqstr_(line, "YES", (ftnlen)265, (ftnlen)3)) {
	zzmckdmp_(outptf, (ftnlen)265);
    }

/*     Check out :) */

    chkout_("MSOPCK", (ftnlen)6);
    return 0;
} /* MAIN__ */

/* Main program alias */ int msopck_ () { MAIN__ (); return 0; }
