/* mkspk.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__4 = 4;
static integer c__8 = 8;
static integer c__19 = 19;
static integer c__16 = 16;
static integer c__40000 = 40000;
static integer c__500 = 500;
static integer c__6 = 6;
static integer c__1 = 1;

/* $Procedure  MKSPK  ( Make SPK File of Type 5,8,9,10,12,13,15 or 17 ) */
/* Main program */ MAIN__(void)
{
    /* Initialized data */

    static char itypes[32*4] = "STATES                          " "ELEMENTS "
	    "                       " "EQ_ELEMENTS                     " "TL_"
	    "ELEMENTS                     ";
    static logical eosgmr = FALSE_;
    static logical ettags = FALSE_;
    static char timdlm[3*2] = ",  " "   ";
    static integer otypes[8] = { 5,8,9,10,12,13,15,17 };
    static logical iotype[32]	/* was [4][8] */ = { TRUE_,TRUE_,FALSE_,
	    FALSE_,TRUE_,TRUE_,FALSE_,FALSE_,TRUE_,TRUE_,FALSE_,FALSE_,FALSE_,
	    FALSE_,FALSE_,TRUE_,TRUE_,TRUE_,FALSE_,FALSE_,TRUE_,TRUE_,FALSE_,
	    FALSE_,TRUE_,TRUE_,FALSE_,FALSE_,TRUE_,TRUE_,TRUE_,FALSE_ };
    static logical cmntfl = FALSE_;
    static logical rakey = FALSE_;
    static logical dekey = FALSE_;
    static logical onerun = TRUE_;
    static logical staepo = TRUE_;
    static logical stoepo = TRUE_;

    /* System generated locals */
    address a__1[2], a__2[8];
    integer i__1[2], i__2, i__3, i__4[8];
    doublereal d__1;
    char ch__1[71], ch__2[2], ch__3[693];
    cllist cl__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen), s_cat(char *,
	     char **, integer *, integer *, ftnlen);
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);
    double cos(doublereal), sin(doublereal);
    integer f_clos(cllist *);
    double sqrt(doublereal), d_sign(doublereal *, doublereal *), tan(
	    doublereal);

    /* Local variables */
    extern /* Subroutine */ int bodmat_(integer *, doublereal *, doublereal *)
	    , recrad_(doublereal *, doublereal *, doublereal *, doublereal *),
	     remlad_(integer *, integer *, doublereal *, integer *), spkcls_(
	    integer *), chkout_(char *, ftnlen), byebye_(char *, ftnlen);
    static char arch[32];
    static doublereal eqel[9];
    static char line[512];
    static integer nval, npar;
    static doublereal tvec[6];
    static integer neor;
    extern integer cpos_(char *, char *, integer *, ftnlen, ftnlen);
    extern /* Subroutine */ int vhat_(doublereal *, doublereal *);
    static doublereal mpol[9]	/* was [3][3] */, elts[8], tipm[9]	/* 
	    was [3][3] */, xfrm[36]	/* was [6][6] */;
    extern doublereal vsep_(doublereal *, doublereal *);
    static integer npos;
    static char type__[32];
    extern integer posr_(char *, char *, integer *, ftnlen, ftnlen);
    static doublereal mwor[9]	/* was [3][3] */;
    extern /* Subroutine */ int mxmt_(doublereal *, doublereal *, doublereal *
	    );
    static doublereal j2flg, e;
    static integer i__, j, k, l, m, n;
    static doublereal p, etbeg, dnode, z__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static doublereal meanm, epoch[10000], etend;
    static integer param[50];
    static doublereal depol, dperi;
    static char doval[12*50], htime[512], cmtfn[255];
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen), 
	    repmc_(char *, char *, char *, char *, ftnlen, ftnlen, ftnlen, 
	    ftnlen);
    static char inpfn[255];
    static logical found;
    static doublereal rapol;
    extern /* Subroutine */ int dpfmt_(doublereal *, char *, char *, ftnlen, 
	    ftnlen);
    static doublereal state[60000]	/* was [6][10000] */;
    extern integer ncpos_(char *, char *, integer *, ftnlen, ftnlen);
    static doublereal wrkel, etfst;
    static integer first;
    static doublereal tmpdp;
    static char error[512];
    extern /* Subroutine */ int spkw05_(integer *, integer *, integer *, char 
	    *, doublereal *, doublereal *, char *, doublereal *, integer *, 
	    doublereal *, doublereal *, ftnlen, ftnlen);
    static char outfn[255];
    static doublereal etlst;
    extern integer rtrim_(char *, ftnlen);
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    static doublereal m0, tstep;
    extern doublereal twopi_(void);
    extern /* Subroutine */ int ucrss_(doublereal *, doublereal *, doublereal 
	    *), spkw08_(integer *, integer *, integer *, char *, doublereal *,
	     doublereal *, char *, integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, ftnlen, ftnlen), spkw09_(integer *, 
	    integer *, integer *, char *, doublereal *, doublereal *, char *, 
	    integer *, integer *, doublereal *, doublereal *, ftnlen, ftnlen),
	     spkw12_(integer *, integer *, integer *, char *, doublereal *, 
	    doublereal *, char *, integer *, integer *, doublereal *, 
	    doublereal *, doublereal *, ftnlen, ftnlen), spkw13_(integer *, 
	    integer *, integer *, char *, doublereal *, doublereal *, char *, 
	    integer *, integer *, doublereal *, doublereal *, ftnlen, ftnlen),
	     spkw15_(integer *, integer *, integer *, char *, doublereal *, 
	    doublereal *, char *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, ftnlen, ftnlen), 
	    spkw17_(integer *, integer *, integer *, char *, doublereal *, 
	    doublereal *, char *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, ftnlen, ftnlen);
    static logical found1, found2;
    static doublereal cnj2vl;
    static logical found3, found4;
    static doublereal pa[3], padbeg;
    extern /* Subroutine */ int str2et_(char *, doublereal *, ftnlen);
    static doublereal ta;
    extern doublereal pi_(void);
    static integer handle;
    static logical flflag;
    static char cmdfil[255];
    static doublereal angcof;
    static integer nl;
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen), 
	    isrchi_(integer *, integer *, integer *), frstnb_(char *, ftnlen),
	     frstnp_(char *, ftnlen);
    extern logical exists_(char *, ftnlen);
    static char buffer[1024], bufaux[1024], wrkchr[512], cmnbuf[255*506], 
	    errstr[512*2];
    static integer nparam, epocfl;
    static char pridvl[80], indtvl[80], frnmvl[80], sgidvl[80], tmwrvl[80];
    extern logical odd_(integer *);
    static char appchr[80], delmvl[1];
    static integer outtvl, nlnrec, obidvl, cnidvl, pldgvl, eplnvl, nflnvl;
    static doublereal cngmvl, cnervl[3];
    static integer inpunt, cmnunt;
    static logical eof;
    static integer jlepoc;
    static doublereal dvl[50], dstcof, tp[3], pv[3], timstp, requat, hstate[6]
	    , cosinc;
    extern integer pos_(char *, char *, integer *, ftnlen, ftnlen);
    static logical statfl, stotfl, errflg, appflg;
    static integer itpidx, otpidx, par, ptr, padsts, recidx;
    static char astrln[80], tstamp[80], verstr[12];
    static integer tlidvl[2];
    static char covtyp[32*2];
    static doublereal covval[2], padend;
    extern /* Subroutine */ int errprt_(char *, char *, ftnlen, ftnlen), 
	    tkvrsn_(char *, char *, ftnlen, ftnlen), tostdo_(char *, ftnlen), 
	    cmlarg_(char *, char *, char *, logical *, ftnlen, ftnlen, ftnlen)
	    , clpool_(void), furnsh_(char *, ftnlen), setupc_(char *, char *, 
	    logical *, ftnlen, ftnlen), setupa_(char *, logical *, ftnlen), 
	    setupi_(char *, integer *, logical *, ftnlen), setmsg_(char *, 
	    ftnlen), sigerr_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen), setupd_(char *, doublereal *, logical *, ftnlen), 
	    parsdo_(char *, char *, integer *, integer *, integer *, ftnlen, 
	    ftnlen), chckdo_(char *, integer *, integer *, integer *, char *, 
	    ftnlen, ftnlen), bodvar_(integer *, char *, integer *, doublereal 
	    *, ftnlen), getfat_(char *, char *, char *, ftnlen, ftnlen, 
	    ftnlen), spkopa_(char *, integer *, ftnlen), spkopn_(char *, char 
	    *, integer *, integer *, ftnlen, ftnlen), ssizec_(integer *, char 
	    *, ftnlen), txtopr_(char *, integer *, ftnlen), readln_(integer *,
	     char *, logical *, ftnlen), scardc_(integer *, char *, ftnlen), 
	    spcacb_(integer *, char *, ftnlen), cputim_(doublereal *), 
	    tle2spk_(char *, integer *, integer *, char *, char *, integer *, 
	    doublereal *, char *, ftnlen, ftnlen, ftnlen, ftnlen), dafcls_(
	    integer *), delfil_(char *, ftnlen), redbuf_(integer *, char *, 
	    integer *, integer *, char *, integer *, char *, logical *, 
	    ftnlen, ftnlen, ftnlen), nparsd_(char *, doublereal *, char *, 
	    integer *, ftnlen, ftnlen), setelm_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, integer *, integer *, doublereal *, 
	    logical *, char *, ftnlen), conics_(doublereal *, doublereal *, 
	    doublereal *), oscelt_(doublereal *, doublereal *, doublereal *, 
	    doublereal *), timout_(doublereal *, char *, char *, ftnlen, 
	    ftnlen), sxform_(char *, char *, doublereal *, doublereal *, 
	    ftnlen, ftnlen);

/* $ Abstract */

/*     This program creates an SPK file of type 5, 8, 9, 10, 12, 13, 15 */
/*     or 17 from a trajectory data provided in a text file in the form */
/*     of states, conic elements, equinoctial elements or NORAD two-line */
/*     element sets. */

/*     USAGE: mkspk  [-setup <setup file name>] */
/*                   [-input <input file name>] */
/*                   [-output <output file name>] */
/*                   [-append] */
/*                   [-u|-usage] */
/*                   [-h|-help] */
/*                   [-t|-template][<input data type> <output spk type>] */

/*     If MKSPK is executed without -setup flag, a user will be */
/*     prompted for the name of setup file. */

/*     If MKSPK is executed without -input and/or -output flags, then */
/*     the  names of an input and/or output files must be provided in */
/*     a setup file using corresponding keywords. */

/*     If output SPK file exists and MKSPK should be appending new */
/*     data to it, then the -append flag or corresponding setup file */
/*     keyword must be provided. */

/*     If MKSPK is executed with -u (-usage), -h (-help) or -t */
/*     (-template) flag than usage, help or setup file template will */
/*     be displayed and the program will stop. */

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

/*     MKSPK User's Guide */
/*     SPK Required Reading */
/*     TIME Required Reading */
/*     POOL Required Reading */

/* $ Keywords */

/*     EPHEMERIS */

/* $ Declarations */
/* $ Exceptions */

/*     There are numerous exceptions handled by the program. Below is */
/*     the list of exceptions handled in the main module. For the list */
/*     of exceptions handles by a particular modules, see the */
/*     "Exceptions" section in the header of that module. */

/*     1) If input record parameters delimiter character */
/*        specified in setup file keyword is one of the characters */
/*        that can be used in a valid input time string and two */
/*        time tags (epoch and epoch of periapsis) are present in */
/*        an input record, then the error 'SPICE(CANTUSEPERIAPEPOCH)' */
/*        will be signaled. */

/*     2) If start time required for the output SPK type 15 */
/*        was not provided in the setup file keyword, then the */
/*        error 'SPICE(NOSTARTTIME4SPK15)' will be signaled. */

/*     3) If stop time required for the output SPK type 15 was */
/*        not provided in the setup file keyword, then the error */
/*        'SPICE(NOSTOPTIME4SPK15)' will be signaled. */

/*     4) If start time required for the output SPK type 17 */
/*        was not provided in the setup file keyword, then the */
/*        error 'SPICE(NOSTARTTIME4SPK17)' will be signaled. */

/*     5) If stop time required for the output SPK type 17 was */
/*        not provided in the setup file keyword, then the error */
/*        'SPICE(NOSTOPTIME4SPK17)' will be signaled. */

/*     6) If total number of lines in the file is less than */
/*        then number of first lines to be ignored as specified in */
/*        setup file keyword, then the error 'SPICE(TOOFEWINPUTLINES)' */
/*        will be signaled. */

/*     7) If the string found in the input file doesn't represent */
/*        a double precision number which it should, then the */
/*        error 'SPICE(NOTADPNUMBER)' will be signaled. */

/*     8) If set of orbital elements specified in the keyword */
/*        of the setup file cannot be converted to standard SPICE */
/*        elements set, then the error 'SPICE(INCONSISTELEMENTS)' */
/*        or 'SPICE(INCOMPLETEELEMENTS)' will be signaled. */

/*     9) If for SPK 8, 9, 12, 13 the epoch of the first input data */
/*        file record is greater than the start time specified in the */
/*        setup file keyword, then the error 'SPICE(INCONSISTSTARTTIME)' */
/*        will be signaled. */

/*     10) If for SPK 8, 9, 12, 13 the epoch of the first input data */
/*        file record is greater than the stop time specified in the */
/*        setup file keyword, then the error 'SPICE(INCONSISTSTOPTIME)' */
/*        will be signaled. */

/*     11) If for SPK 8, 9, 12, 13 the epoch of the last input data */
/*        file record is smaller than the start time specified in the */
/*        setup file keyword, then the error 'SPICE(INCONSISTSTARTTIME)' */
/*        will be signaled. */

/*     12) If the epoch of the last input data file record is */
/*        smaller than the stop time specified in the setup file */
/*        keyword, then the error 'SPICE(INCONSISTSTOPTIME)' will */
/*        be signaled. */

/*     13) If the stop time specified in the setup file keyword is */
/*        smaller than the start time specified in the setup file */
/*        keyword, then the error 'SPICE(INVERSTARTSTOPTIME)' will */
/*        be signaled. */

/*     14) If the input data type is not applicable for output SPK */
/*        type or vice-versa, then the error 'SPICE(TYPESMISMATCH)' */
/*        will be signaled. */

/*     15) If appending to an existing file has not been requested */
/*        using command line key or setup file keyword and output */
/*        file already exists, then the error 'SPICE(OUTPUTFILEEXISTS)' */
/*        will be signaled. */

/*     16) If appending to an existing file has been requested */
/*        but existing output file is not an SPK, then the error */
/*        'SPICE(OUTPUTISNOTSPK)' will be signaled. */

/*     17) If the time spacing between input data points was not */
/*        constant for output types 8 and 12, then the */
/*        'SPICE(UNEQUALTIMESTEP)' error will be signaled. */

/*     18) If the degree of Lagrange polynomial specified in the setup */
/*        keyword is not a positive number or greater than MAXDEG, then */
/*        the error 'SPICE(BADLAGRANGEDEGREE)' will be signaled. */

/*     19) If the degree of Hermit polynomial specified in the setup */
/*        keyword is not a positive number or greater than MAXDEG, then */
/*        the error 'SPICE(BADHERMITDEGREE)' will be signaled. */

/*     20) If the input file contains records with duplicate time tags, */
/*        then the error 'SPICE(DUPLICATETIMES)' will be signaled. */

/*     21) If the input file contains records with times out of order, */
/*        then the error 'SPICE(TIMESOUTOFORDER)' will be signaled. */

/*     22) If only one of the TLE-specific ID keywords is specified, */
/*        then the error 'SPICE(MISSINGTLEIDKEYWORD)' will be signaled. */

/*     23) If either the generic object ID or object name keyword is */
/*         specified together with the TLE-specific ID keywords, then */
/*        the error 'SPICE(KEYWORDSMISMATCH1)' will be signaled. */

/*     24) If both the start time and TLE start pad keywords are */
/*        specified, then the error 'SPICE(KEYWORDSMISMATCH2)' will */
/*        be signaled. */

/*     25) If both the stop time and TLE stop pad keywords are */
/*        specified, then the error 'SPICE(KEYWORDSMISMATCH3)' will */
/*        be signaled. */

/*     26) If the stop time specified in the setup file keyword is */
/*        smaller than or equal to the start time specified in the setup */
/*        file keyword for a TLE run, then the error */
/*        'SPICE(INVERSTIMES2)' will be signaled. */

/*     27) If the segment ID string specified SEGMENT_ID keyword is */
/*        longer than 40 characters, then the error */
/*        'SPICE(SEGIDTOOLONG)' will be signaled. */

/* $ Files */

/*     The program requires: */

/*        -- input data to be provided in a text file; the name of */
/*           an input text file can be provided in the corresponding */
/*           setup file keyword or on the command line after */
/*           corresponding key. (For a TL_ELEMENTS input / SPK 10 */
/*           output run, the input file must be in NORAD TLE format.) */

/*        -- setup information to be provided via setup file which */
/*           corresponds to the text kernel file format; the name */
/*           of a setup file can be provided on the command line after */
/*           corresponding key or in response to the program prompt. */

/*        -- Leapsecond Kernel (LSK) file to support time conversions; */
/*           the name of an LSK file must be provided in the */
/*           corresponding setup file keyword. */

/*        -- Planetary Constant Kernel (PCK) file(s) if some planetary */
/*           constants are needed for requested output SPK type or */
/*           input data conversion but weren't provided in special */
/*           setup file keywords; the name of a PCK file(s) must be */
/*           provided in the corresponding setup file keyword. */

/*        -- A geophysical constants file for the Earth (as PCK file) */
/*           if SPK of type 10 are processed; the name of a geophysical */
/*           constants file must be provided in the corresponding setup */
/*           file keyword. */

/*        -- Frame Definitions Kernel (FRAMES) file if trajectory */
/*           data in an output SPK file will be stored with respect */
/*           to a user defined frame; the name of an FRAMES file must */
/*           be provided in the corresponding setup file keyword. */

/*     As an option the program can insert comments from a text file */
/*     into the comment area of an output SPK file. The name of a */
/*     external comment file can be provided in the corresponding */
/*     setup file keyword. */

/*     The program can create (or append data to) only one SPK file */
/*     containing one or multiple segments of a specified type during */
/*     each run. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     To demonstrate the usage of the program, consider that we have */
/*     an input file containing state vectors of the spacecraft with */
/*     the NAIF ID code -555 with relative to the Earth (NAIF ID -399) */
/*     in the 'J2000' inertial frame. */

/*     Let the structure of this file be: */

/*        Header 1 */
/*        Header 2 */
/*        X1, Y1, Z1, P1, P2, VX1, VY1, VZ1, EPOCH1 */
/*        ......................................... */
/*        ......................................... */
/*        ......................................... */
/*        XN, YN, ZN, P1, P2, VXN, VYN, VZN, EPOCHN */

/*     where */

/*        Xi....EPOCHi      data lines; */
/*        Header i          header lines; */
/*        Xi, Yi, Zi        position components in km; */
/*        VXi, VYi, VZi     velocity components in km/sec; */
/*        EPOCHi            data record ET time in calendar format; */
/*        Pi                additional parameters that are not needed */
/*                          for output SPK data and should be ignored */
/*                          by the program; */

/*     Consider that we also have in hand a generic SPICE LSK file */
/*     "lsk.tls", a generic SPICE PCK file "pck.tpc", a PCK file */
/*     "gravity.tgr" containing gravity constants and a text file */
/*     "cmn.tcm" containing comments that we want to be inserted */
/*     into the comment area of the output SPK file. */

/*     With these inputs we will create a setup file "cmd.tcm" */
/*     containing the following keywords to make MKSPK to interpret */
/*     our input data correctly: */

/*       INPUT_DATA_TYPE             = 'STATES' */
/*       OBJECT_ID                   = -555 */
/*       CENTER_ID                   = 399 */
/*       REF_FRAME_NAME              = 'J2000' */
/*       DATA_ORDER                  = 'X Y Z SKIP SKIP Vx Vy Vz EPOCH' */
/*       INPUT_DATA_UNITS            = ( 'DISTANCES=km' ) */
/*       TIME_WRAPPER                = '# (TDB)' */
/*       IGNORE_FIRST_LINE           = 2 */
/*       DATA_DELIMITER              = ',' */
/*       LINES_PER_RECORD            = 1 */
/*       LEAPSECONDS_FILE            = 'lsk.tls' */
/*       PCK_FILE                    = ( 'pck.tpc', 'gravity.tgr' ) */
/*       COMMENT_FILE                = 'cmd.tcm' */
/*       PRODUCER_ID                 = 'N.G.Khavenson, IKI RAS, Russia' */
/*       SEGMENT_ID                  = 'EXAMPLE' */

/*     To generate an SPK file of type 5 we will need to add only */
/*     OUTPUT_SPK_TYPE keyword to our earlier set: */

/*        OUTPUT_SPK_TYPE            =  5 */

/*     to generate an SPK file of type 8 we will need to add the */
/*     the OUTPUT_SPK_TYPE and POLYNOM_DEGREE keywords to our */
/*     earlier set: */

/*        OUTPUT_SPK_TYPE            =  8 */
/*        POLYNOM_DEGREE             =  4 */

/*     to generate an SPK file of type 15 we will need to add the */
/*     the OUTPUT_SPK_TYPE, PRECESSION_TYPE, START_TIME and STOP_TIME */
/*     keywords to our earlier set: */

/*        OUTPUT_SPK_TYPE            =  15 */
/*        START_TIME                 =  'start time' */
/*        STOP_TIME                  =  'stop time' */
/*        PRECESSION_TYPE            =  'used type of precession' */

/*     When a set of keywords required for a particular output SPK type */
/*     was added to a set of keywords defining input data, we can run */
/*     MKSPK for any of the above cases as follows: */

/*        > mkspk -setup cmd.tcm -input data.txt -output spk.bsp */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     1) K.A. Ehricke, "Space flight, 1. Environment and celestial */
/*        mechanics." Princeton, New Jersey - Toronto - New York - */
/*        London, 1960. */

/* $ Author_and_Institution */

/*     N.G. Khavenson  (IKI RAS, Russia) */
/*     B.V. Semenov    (NAIF, JPL) */
/*     E.D. Wright     (NAIF, JPL) */

/* $ Version */

/* -    Version 6.0.0, 06-JAN-2014 (BVS) */

/*        Updated for additional TLE-specific setup keywords to allow */
/*        user specified TLE time pad. Increased TLE segment buffer size */
/*        to 50,000 and changed TLE processing logic to sort input TLEs */
/*        and eliminate duplicate sets. (All these changes are in */
/*        TLE2SPK.) */

/*        BUG FIX: fixed to process TLE lines with spacecraft code */
/*        left-padded with zeros to five digits. (This change is in */
/*        TLE2SPK.) */

/*        BUG FIX: fixed to adjust longitude of the ascending node and */
/*        argument of periapse for the time of the nearest periapsis for */
/*        elliptic orbit cases for type 15 inputs that are not at the */
/*        time of periapsis. */

/* -    Version 5.1.1, 18-MAY-2010 (BVS) */

/*        Removed spurious "C$" marker from the header. */

/* -    Version 5.1.0, 06-MAR-2009 (BVS) */

/*        Bug fix: added adjustment of the descriptor end time to */
/*        prevent an error signaled by SPKW08 and SPKW12 due to */
/*        round off in computed time step. */

/* -    Version 5.0.0, 16-JAN-2008 (BVS) */

/*        Added capability to accept time tags given as ET seconds past */
/*        J2000; the TIME_WRAPPER setup keyword must be set to */
/*        '# ETSECONDS' to do that. Changed the format of the displayed */
/*        version line. */

/* -    Version 4.4.0, 26-SEP-2006 (BVS) */

/*        Fixed the bug causing the 8th input equinoctial element */
/*        (DMPN/DT) be assigned to the 8th (DMPN/DT) and 9th (DNOD/DT) */
/*        equinoctial elements stored in the output SPK file when the */
/*        first six equinoctial elements were derived from input states */
/*        or classical elements. */

/* -    Version 4.3.0, 20-DEC-2004 (BVS) */

/*        Added truncation of segment ID set by default to prevent SPKW* */
/*        routines from signaling an error when the input file name, to */
/*        which the segment ID is set, is longer than 40 characters. */

/* -    Version 4.2.4, 26-APR-2004 (EDW) */

/*        Updated TLE parser to confirm correct numerical ranges on */
/*        expected values. TLE2SPK signals an error if a TLE parse fails. */

/*        Replaced all LDPOOL calls with FURNSH. */

/* -    Version 4.1.0 beta, 31-MAY-2001 (BVS) */

/*        Changed time START/STOP time bracketing and segment breakdown */
/*        algorithms to "pad" (if possible) beginning and end of each */
/*        segment with appropriate number of states to insure that */
/*        interpolation over segment boundary is continuous and smooth. */
/*        Added checks for duplicate and out-of-order input times. */

/* -    Version 4.0.0 beta, 22-MAR-2001 (BVS) */

/*        Added capability to append to an existing SPK file. Added */
/*        check for equal time spacing in the input for output types 8 */
/*        and 12. */

/* -    Version 3.0.2 beta, 03-NOV-2000 (EDW) */

/*        Added a BYEBYE( 'SUCCESS' ) call at program's end. */

/* -    Version 3.0.1 beta, 27-JAN-2000 (BVS) */

/*        Added clean input/output type consistency check. Added check */
/*        for odd polynomial degree for types 12/13. */

/* -    Version 3.0.0 beta, 20-JAN-2000 (BVS) */

/*        Modified to handle SPK types 12 and 13 (Hermite types.) */

/* -    Version 2.0.0 beta, 23-NOV-1999 (NGK) */

/*        Modified to handle TLE/SPK type 10. Added check to verify */
/*        that for the input type EQ_ELEMENTS to output type is 17. */

/* -    Version 1.0.7 beta, 13-MAY-1999 (BVS) */

/*        Removed checks of start and stop time provided in a setup */
/*        file against first and last epochs of the data for SPK types */
/*        5, 15 and 17. Changed logic to assign correct start time */
/*        of the first segment and stop time of the last segment for */
/*        output files of type 5. These changes allowed generation of */
/*        type 5, 15 or 17 SPK file from an input containing a single */
/*        record and arbitrary coverage start and stop times specified */
/*        in a setup file. */

/* -    Version 1.0.6 beta, 30-MAR-1999 (NGK)(BVS) */

/*        First official release. */

/* -    Version 1.0.6 pre-release, 29-MAR-1999 (NGK) */

/*        The codes of EQEL calculations were corrected and */
/*        examples section was adjusted. */

/* -    Version 1.0.5 pre-release, 21-MAR-1999 (BVS) */

/*        The logic in the following program blocks was adjusted: */

/*           -- addition of the comments to the comment area of the */
/*              output SPK file; (changed comment buffer full up */
/*              schema, eliminated addition of the producer ID as a */
/*              separate item, added separator lines and product */
/*              creation time.) */

/*           -- parsing of the input in the case when number of */
/*              input lines per record is specified (EPOCFL=3). */

/*           -- reassigning last records of previous segment to the */
/*              first records of the next segment for output types */
/*              8 and 9; (now copies DEG+1 records instead of 1) */

/*           -- setting default SEGMENT_ID; (now is set the input file */
/*              name rather than producer ID; */

/*           -- value of the token identifying input data items to */
/*              be ignored; (changed to SKIP instead of NULL) */

/*           -- REDBUF calling sequence; (rearranged arguments to */
/*              correspond to the new calling sequence) */

/*        Modified header and some comments in the code. */

/* -    Version 1.0.4 pre-release, 15-FEB-1999 (NGK) */

/*        Modified writing of the comments, included checking of the */
/*        start/stop time against each other, added cleaning of a */
/*        non-printing characters from the comments. */

/* -    Version 1.0.3 pre-release, 13-JAN-1999 (BVS) */

/*        Modified error, warning and progress messages in all modules. */
/*        Changed welcome message. */

/* -    Version 1.0.2 pre-release, 30-DEC-1998 (NGK) */


/* -    Version 1.0.2 pre-release, 15-NOV-1998 (NGK) */


/* -    Version 1.0.1 pre-release, 11-OCT-1998 (NGK) */


/* -    Version 1.0.0 pre-release, 3-DEC-1997 (NGK) */

/* -& */

/* $ Index_Entries */

/*     Make SPK file of types 5, 8, 9, 10, 12, 13, 15 and 17. */

/* -& */

/*     SPICELIB functions. */

/* $ Abstract */

/*     MKSPK Include File. */

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

/* $ Author_and_Institution */

/*     N.G. Khavenson (IKI RAS, Russia) */
/*     B.V. Semenov   (NAIF, JPL) */

/* $ Version */

/* -    Version 1.3.0, 08-FEB-2012 (BVS). */

/*        Added TLE coverage and ID keywords. Added default TLE pad */
/*        parameter. */

/* -    Version 1.2.0, 16-JAN-2008 (BVS). */

/*        Added ETTMWR parameter */

/* -    Version 1.1.0, 05-JUN-2001 (BVS). */

/*        Added MAXDEG parameter. */

/* -    Version 1.0.4, 21-MAR-2001 (BVS). */

/*        Added parameter for command line flag '-append' indicating */
/*        that appending to an existing output file was requested. */
/*        Added corresponding setup file keyword ('APPEND_TO_OUTPUT'.) */
/*        Added parameters for yes and no values of this keyword. */

/* -    Version 1.0.3, 28-JAN-2000 (BVS). */

/*        Added parameter specifying number of supported input data */
/*        types and parameter specifying number of supported output SPK */
/*        types */

/* -    Version 1.0.2, 22-NOV-1999 (NGK). */

/*        Added parameters for two-line elements processing. */

/* -    Version 1.0.1, 18-MAR-1999 (BVS). */

/*        Added usage, help and template displays. Corrected comments. */

/* -    Version 1.0.0,  8-SEP-1998 (NGK). */

/* -& */

/*     Begin Include Section:  MKSPK generic parameters. */


/*     Maximum number of states allowed per one segment. */


/*     String size allocation parameters */


/*     Length of buffer for input text processing */


/*     Length of a input text line */


/*     Length of file name and comment line */


/*     Length of string for keyword value processing */


/*     Length of string for word processing */


/*     Length of data order parameters string */


/*     Length of string reserved as delimiter */


/*     Numbers of different parameters */



/*     Maximum number of allowed comment lines. */


/*     Reserved number of input parameters */


/*     Full number of delimiters */


/*     Number of delimiters that may appear in time string */


/*     Command line flags */


/*     Setup file keywords reserved values */


/*     Standard YES and NO values for setup file keywords. */


/*     Number of supported input data types and input DATA TYPE */
/*     reserved values. */


/*     Number of supported output SPK data types -- this version */
/*     supports SPK types 5, 8, 9, 10, 12, 13, 15 and 17. */


/*     End of input record marker */


/*     Maximum allowed polynomial degree. The value of this parameter */
/*     is consistent with the ones in SPKW* routines. */


/*     Special time wrapper tag for input times given as ET seconds past */
/*     J2000 */


/*     Default TLE pad, 1/2 day in seconds. */


/*     End Include Section:  MKSPK generic parameters. */


/*     Local parameters. */


/*     Program version. */


/*     Dimension of a single state vector. */


/*     The lower bound for the CELL 'data type'. */


/*     Time step equality tolerance for type 8 and 12 input */
/*     verification (is one millisecond a good guess?) */


/*     Local variables. */

/*     String sizes FILSIZ, BUFLEN, LINLEN, DLMSIZ, VALUEL, SHRTLN */
/*     defined in include file. */


/*     Setup, input, output, comment file names. */


/*     String processing parameters. Dimension MXCMNT is defined in */
/*     the include file. */


/*     Array of reserved delimiters that may appear in a time string. */
/*     Dimension TMDNMB is defined in the include file. */


/*     Array of acceptable in DATA_ORDER parameters and number of it. */
/*     Dimension MXNVAL is defined in the include file. */


/*     Parameter IDs array and number of actual elements in it. */


/*     Epoch string processing type flag. */


/*     Setup keyword values. */


/*     Input and comment file units. */


/*     End of input data file flag. */


/*     Binary PCK and SPK file handle. */


/*     Indexes of last buffered epoch. */


/*     Array for storing parsed input data. */


/*     Distance/angle unit conversion coefficients. */


/*     Frame transformation matrices. */


/*     SPKWxx and CONICS/OSCELT subroutine arguments. Dimension MAXSTA */
/*     is defined in the include file. */


/*     "Ignore first input line" flag. */


/*     "End of segment" flag. */


/*     "One time execution" flag. */


/*     "Comment file existence" flag. */


/*     "Processing START/STOP times" flag. */


/*     "Bracket input epoch array by START/STOP time" flag. */


/*     "Retrieve north polar vector RA and DEC from PCK file" flag. */


/*     "SETELM Error" flag. */


/*     Append to existing output file flag. */


/*     Time tags are given as ET seconds past J2000 flag. */


/*     File architecture and type. */


/*     Input/output type "match" algorithm arrays. */


/*     Miscellaneous variables. */


/*     Variables for TLE-specific setups. */


/*     Three arrays below are needed to check whether input data type */
/*     matches output SPK type and vice-versa. */

/*     Supported input data types -- must be "in sync" with parameters */
/*     specified in MKSPK.INC. */


/*     Supported output SPK data types -- must be "in sync" with */
/*     parameters specified in MKSPK.INC. */


/*     This array defines whether input data type is appropriate for */
/*     output SPK type. */


/*     Logical flag initial values. */


/*     Delimiters that may appear in a time string. WARNING! If you */
/*     need to change this list, make sure that the changes are in sync */
/*     with complete list of supported delimiter values stored in the */
/*     parameter ALLDLM in the SETUP module. */


/*     DATA_ORDER parameter values. */

    s_copy(doval, "EPOCH", (ftnlen)12, (ftnlen)5);
    s_copy(doval + 12, "X", (ftnlen)12, (ftnlen)1);
    s_copy(doval + 24, "Y", (ftnlen)12, (ftnlen)1);
    s_copy(doval + 36, "Z", (ftnlen)12, (ftnlen)1);
    s_copy(doval + 48, "VX", (ftnlen)12, (ftnlen)2);
    s_copy(doval + 60, "VY", (ftnlen)12, (ftnlen)2);
    s_copy(doval + 72, "VZ", (ftnlen)12, (ftnlen)2);
    s_copy(doval + 84, "A", (ftnlen)12, (ftnlen)1);
    s_copy(doval + 96, "E", (ftnlen)12, (ftnlen)1);
    s_copy(doval + 108, "RP", (ftnlen)12, (ftnlen)2);
    s_copy(doval + 120, "T", (ftnlen)12, (ftnlen)1);
    s_copy(doval + 132, "P", (ftnlen)12, (ftnlen)1);
    s_copy(doval + 144, "INC", (ftnlen)12, (ftnlen)3);
    s_copy(doval + 156, "PER", (ftnlen)12, (ftnlen)3);
    s_copy(doval + 168, "NOD", (ftnlen)12, (ftnlen)3);
    s_copy(doval + 180, "MEAN", (ftnlen)12, (ftnlen)4);
    s_copy(doval + 192, "EXAN", (ftnlen)12, (ftnlen)4);
    s_copy(doval + 204, "TRAN", (ftnlen)12, (ftnlen)4);
    s_copy(doval + 216, "EPOCHP", (ftnlen)12, (ftnlen)6);
    s_copy(doval + 228, "TAU", (ftnlen)12, (ftnlen)3);
    s_copy(doval + 240, "EQ_A", (ftnlen)12, (ftnlen)4);
    s_copy(doval + 252, "EQ_H", (ftnlen)12, (ftnlen)4);
    s_copy(doval + 264, "EQ_K", (ftnlen)12, (ftnlen)4);
    s_copy(doval + 276, "EQ_ML", (ftnlen)12, (ftnlen)5);
    s_copy(doval + 288, "EQ_P", (ftnlen)12, (ftnlen)4);
    s_copy(doval + 300, "EQ_Q", (ftnlen)12, (ftnlen)4);
    s_copy(doval + 312, "DPER/DT", (ftnlen)12, (ftnlen)7);
    s_copy(doval + 324, "DMPN/DT", (ftnlen)12, (ftnlen)7);
    s_copy(doval + 336, "DNOD/DT", (ftnlen)12, (ftnlen)7);
    s_copy(doval + 348, "SKIP", (ftnlen)12, (ftnlen)4);
    nval = 30;

/*     Default values of the distance/angle unit conversion */
/*     coefficients: KM and RADIANS. */

    angcof = 1.;
    dstcof = 1.;

/*     Comment area separator line. */

    s_copy(astrln, "********************************************************"
	    "************************", (ftnlen)80, (ftnlen)80);

/*     Standard SPICE error handling. */

    chkin_("MKSPK", (ftnlen)5);

/*     Display only short and long error messages. */

    errprt_("SET", "NONE, SHORT, LONG, TRACEBACK", (ftnlen)3, (ftnlen)28);

/*     Display "welcome" message. */

    tkvrsn_("TOOLKIT", verstr, (ftnlen)7, (ftnlen)12);
    tostdo_(" ", (ftnlen)1);
/* Writing concatenation */
    i__1[0] = 59, a__1[0] = "MKSPK -- Version 6.0.0, January 6, 2014 -- Tool"
	    "kit Version ";
    i__1[1] = rtrim_(verstr, (ftnlen)12), a__1[1] = verstr;
    s_cat(ch__1, a__1, i__1, &c__2, (ftnlen)71);
    tostdo_(ch__1, rtrim_(verstr, (ftnlen)12) + 59);
    tostdo_(" ", (ftnlen)1);

/*     Get command line arguments. */

    cmlarg_(cmdfil, inpfn, outfn, &appflg, (ftnlen)255, (ftnlen)255, (ftnlen)
	    255);

/*     Load setup file into the kernel pool. */

    tostdo_("Loading setup file ... ", (ftnlen)23);
    clpool_();
    furnsh_(cmdfil, (ftnlen)255);
    tostdo_(" ", (ftnlen)1);
    tostdo_("Processing setup file keyword values ... ", (ftnlen)41);

/*     Get setup file parameters required for all output SPK types. */
/*     (COMMENT FILE NAME and SEGMENT ID are optional but they are */
/*     applicable for all output SPK types.) */

    setupc_("INPUT FILE NAME", inpfn, &found, (ftnlen)15, (ftnlen)255);
    setupc_("OUTPUT FILE NAME", outfn, &found, (ftnlen)16, (ftnlen)255);
    setupa_("LOAD LSK FILE", &found, (ftnlen)13);
    setupa_("LOAD PCK FILE", &found, (ftnlen)13);
    setupi_("CENTER ID", &cnidvl, &found, (ftnlen)9);
    setupc_("REFERENCE FRAME NAME", frnmvl, &found, (ftnlen)20, (ftnlen)80);
    setupc_("INPUT DATA TYPE", indtvl, &found, (ftnlen)15, (ftnlen)80);
    setupi_("OUTPUT SPK TYPE", &outtvl, &found, (ftnlen)15);
    setupc_("PRODUCER ID", pridvl, &found, (ftnlen)11, (ftnlen)80);
    setupc_("COMMENT FILE NAME", cmtfn, &cmntfl, (ftnlen)17, (ftnlen)255);
    setupc_("SEGMENT ID", sgidvl, &found, (ftnlen)10, (ftnlen)80);
    if (found) {

/*        Check that segment ID length is no greater than 40 characters */
/*        to catch this error before it gets to SPKW* routines. */

	if (rtrim_(sgidvl, (ftnlen)80) > 40) {
	    setmsg_("The segment ID string '#' specified in the '#' keyword "
		    "is longer than 40 characters. ", (ftnlen)85);
	    errch_("#", sgidvl, (ftnlen)1, (ftnlen)80);
	    errch_("#", "SEGMENT_ID", (ftnlen)1, (ftnlen)10);
	    sigerr_("SPICE(SEGIDTOOLONG)", (ftnlen)19);
	}
    } else {

/*        If segment ID was not provided, set it to be the input file */
/*        name truncated at 40 characters. */

	s_copy(sgidvl, inpfn, (ftnlen)80, (ftnlen)40);
    }

/*     The first thing to do is to check whether input data type */
/*     matches output SPK type. First we find indexes of the types in */
/*     the "registry" arrays. */

    itpidx = isrchc_(indtvl, &c__4, itypes, rtrim_(indtvl, (ftnlen)80), (
	    ftnlen)32);
    otpidx = isrchi_(&outtvl, &c__8, otypes);
    if (itpidx != 0 && otpidx != 0) {

/*        Both types are "registered" in the corresponding arrays. */
/*        Let's see if they match. */

	if (! iotype[(i__2 = itpidx + (otpidx << 2) - 5) < 32 && 0 <= i__2 ? 
		i__2 : s_rnge("iotype", i__2, "mkspk_", (ftnlen)1021)]) {
	    setmsg_("SPK type # cannot be created from '#' input. See MKSPK "
		    "User's Guide for the list of legitimate input/output typ"
		    "e pairs.", (ftnlen)119);
	    errint_("#", &outtvl, (ftnlen)1);
	    errch_("#", indtvl, (ftnlen)1, (ftnlen)80);
	    sigerr_("SPICE(TYPESMISMATCH)", (ftnlen)20);
	}
    } else {

/*        We should have checked whether input/output types are */
/*        supported ones earlier, in SETUP. If we didn't -- it's a bug. */

	setmsg_("Please, contact NAIF.", (ftnlen)21);
	sigerr_("SPICE(MKSPKBUG3)", (ftnlen)16);
    }

/*     Continue collecting setup information. We will need to look for */
/*     different sets of additional items for TLEs vs other types. */

    if (s_cmp(indtvl, "TL_ELEMENTS", rtrim_(indtvl, (ftnlen)80), (ftnlen)11) 
	    == 0) {

/*        Output SPK type is 10. We continue to get setup file */
/*        parameters required for TLEs. */

/*        Get input TLE and output SPK object IDs. */

	setupi_("TLE INPUT OBJECT ID", tlidvl, &found1, (ftnlen)19);
	setupi_("TLE SPK OBJECT ID", &tlidvl[1], &found2, (ftnlen)17);

/*        Did we get neither of IDs, one of IDs, or both IDs? */

	if (! found1 && ! found2) {

/*           If neither the input TLE ID nor the output SPK object ID */
/*           were explicitly provided, try to get the old-style object */
/*           ID (which will be the ID to look for in the TLE file) and */
/*           derive SPK object ID from it as was done in the program */
/*           versions before 6.0.0. Since the OBJECT_ID keyword was a */
/*           required keyword, this call signals an error if neither ID */
/*           nor name were found. */

	    setupi_("OBJECT ID", tlidvl, &found, (ftnlen)9);
	    tlidvl[1] = -100000 - tlidvl[0];
	} else if (! found1 && found2 || found1 && ! found2) {

/*           If only one of TLE IDs was explicitly provided, report an */
/*           error. */

	    setmsg_("Both keywords, '#' and '#', must be provided in the set"
		    "up file to explicitly specify the IDs to look for in the"
		    " input TLE and to use in the output SPK file. Only '#' w"
		    "as provided for this run.", (ftnlen)192);
	    errch_("#", "TLE_INPUT_OBJ_ID", (ftnlen)1, (ftnlen)16);
	    errch_("#", "TLE_SPK_OBJ_ID", (ftnlen)1, (ftnlen)14);
	    if (found1) {
		errch_("#", "TLE_INPUT_OBJ_ID", (ftnlen)1, (ftnlen)16);
	    } else {
		errch_("#", "TLE_SPK_OBJ_ID", (ftnlen)1, (ftnlen)14);
	    }
	    sigerr_("SPICE(MISSINGTLEKEYWORD)", (ftnlen)24);
	} else {

/*           If both the input TLE ID and the output SPK object ID were */
/*           explicitly provided, check that neither old-style object ID */
/*           keyword nor the old style name keywords were provided. If */
/*           either of them was provided, signal an error. */

	    setupi_("OBJECT ID CHECK", &i__, &found1, (ftnlen)15);
	    setupi_("OBJECT NAME CHECK", &i__, &found2, (ftnlen)17);
	    if (found1 || found2) {
		setmsg_("The keyword '#' cannot be provided together with th"
			"e '#' and '#' keywords.", (ftnlen)74);
		if (found1) {
		    errch_("#", "OBJECT_ID", (ftnlen)1, (ftnlen)9);
		} else {
		    errch_("#", "OBJECT_NAME", (ftnlen)1, (ftnlen)11);
		}
		errch_("#", "TLE_INPUT_OBJ_ID", (ftnlen)1, (ftnlen)16);
		errch_("#", "TLE_SPK_OBJ_ID", (ftnlen)1, (ftnlen)14);
		sigerr_("SPICE(KEYWORDSMISMATCH1)", (ftnlen)24);
	    }
	}

/*        Get begin and end times and begin and end TLE pads. */

	setupd_("START TIME", &etbeg, &found1, (ftnlen)10);
	setupd_("STOP TIME", &etend, &found2, (ftnlen)9);
	setupd_("TLE COVERAGE START PAD", &padbeg, &found3, (ftnlen)22);
	setupd_("TLE COVERAGE STOP PAD", &padend, &found4, (ftnlen)21);

/*        Assign start time or pad. Note that both are optional and */
/*        cannot be provided together. */

	if (found1 && found3) {
	    setmsg_("The keywords '#' and '#' cannot be provided at the same"
		    " time.", (ftnlen)61);
	    errch_("#", "START_TIME", (ftnlen)1, (ftnlen)10);
	    errch_("#", "TLE_START_PAD", (ftnlen)1, (ftnlen)13);
	    sigerr_("SPICE(KEYWORDSMISMATCH2)", (ftnlen)24);
	} else if (found1) {
	    s_copy(covtyp, "TIME", (ftnlen)32, (ftnlen)4);
	    covval[0] = etbeg;
	} else if (found3) {
	    s_copy(covtyp, "PAD", (ftnlen)32, (ftnlen)3);
	    covval[0] = padbeg;
	} else {
	    s_copy(covtyp, "PAD", (ftnlen)32, (ftnlen)3);
	    covval[0] = 43200.;
	}

/*        Check that stop time and stop pad are not provided together. */

	if (found2 && found4) {
	    setmsg_("The keywords '#' and '#' cannot be provided at the same"
		    " time.", (ftnlen)61);
	    errch_("#", "STOP_TIME", (ftnlen)1, (ftnlen)9);
	    errch_("#", "TLE_STOP_PAD", (ftnlen)1, (ftnlen)12);
	    sigerr_("SPICE(KEYWORDSMISMATCH3)", (ftnlen)24);
	} else if (found2) {
	    s_copy(covtyp + 32, "TIME", (ftnlen)32, (ftnlen)4);
	    covval[1] = etend;
	} else if (found4) {
	    s_copy(covtyp + 32, "PAD", (ftnlen)32, (ftnlen)3);
	    covval[1] = padend;
	} else {
	    s_copy(covtyp + 32, "PAD", (ftnlen)32, (ftnlen)3);
	    covval[1] = 43200.;
	}

/*        One more sanity check. If begin and end time were given, check */
/*        that begin is less than end. */

	if (found1 && found2) {
	    if (etend <= etbeg) {
		setmsg_("The stop time specified in setup file keyword '#' i"
			"s less than or equal to the start time specified in "
			"the setup file keyword '#'.", (ftnlen)130);
		errch_("#", "STOP_TIME", (ftnlen)1, (ftnlen)9);
		errch_("#", "START_TIME", (ftnlen)1, (ftnlen)10);
		sigerr_("SPICE(INVERSTIMES2)", (ftnlen)19);
	    }
	}

/*        All done with additional TLE setups. */

    } else {

/*        Output SPK type is not 10. We continue to get setup file */
/*        parameters required for other output SPK types. */

/*        Get SPK object ID. */

	setupi_("OBJECT ID", &obidvl, &found, (ftnlen)9);

/*        Get the count of top lines to skip and distance and angle */
/*        units conversion coefficients. */

	setupi_("IGNORE FIRST LINES", &nflnvl, &flflag, (ftnlen)18);
	setupd_("DISTANCE COEFFICIENT", &dstcof, &found, (ftnlen)20);
	setupd_("ANGLE COEFFICIENT", &angcof, &found, (ftnlen)17);

/*        Get start and stop time; check whether the stop time is */
/*        greater than the start time. */

	setupd_("START TIME", &etbeg, &statfl, (ftnlen)10);
	setupd_("STOP TIME", &etend, &stotfl, (ftnlen)9);
	if (statfl && stotfl) {
	    if (etend < etbeg) {
		setmsg_("The stop time specified in setup file keyword '#' i"
			"s smaller than the start time specified in the setup"
			" file keyword '#'.", (ftnlen)121);
		errch_("#", "STOP_TIME", (ftnlen)1, (ftnlen)9);
		errch_("#", "START_TIME", (ftnlen)1, (ftnlen)10);
		sigerr_("SPICE(INVERSTARTSTOPTIME)", (ftnlen)25);
	    }
	}

/*        Get data order, parse it and check it with respect to the */
/*        input data/output SPK types. */

	setupc_("DATA ORDER", line, &found, (ftnlen)10, (ftnlen)512);
	parsdo_(line, doval, &nval, param, &nparam, (ftnlen)512, (ftnlen)12);
	chckdo_(indtvl, &outtvl, param, &nparam, doval, (ftnlen)80, (ftnlen)
		12);

/*        Get data delimiter and time wrapper values. If the time */
/*        wrapper has the special value indicating that time tags are */
/*        given as ET seconds past J2000, set corresponding logical */
/*        flag. */

	setupc_("DATA DELIMITER", delmvl, &found, (ftnlen)14, (ftnlen)1);
	setupc_("TIME WRAPPER", tmwrvl, &found, (ftnlen)12, (ftnlen)80);
	if (eqstr_(tmwrvl, "#ETSECONDS", (ftnlen)80, (ftnlen)10) || eqstr_(
		tmwrvl, "ETSECONDS#", (ftnlen)80, (ftnlen)10)) {
	    ettags = TRUE_;
	}

/*        Get length of input EPOCH string. */

	setupi_("TIME STRING LENGTH", &eplnvl, &found, (ftnlen)18);
	if (! found) {

/*           Length of EPOCH string was not defined in the setup file, */
/*           we need to check what input data delimiter value is and */
/*           whether the number of lines which each input record */
/*           occupies was specified in the setup file and set the */
/*           EPOCH flag correspondingly. */

	    if (isrchc_(delmvl, &c__2, timdlm, (ftnlen)1, (ftnlen)3) != 0) {

/*              Delimiter is one of the symbols that can appear in a */
/*              time string. Check whether there is a second EPOCH */
/*              string (epoch of periapsis) is present in the input */
/*              records. */

		if (isrchi_(&c__19, &nparam, param) != 0) {

/*                 It is not allowed to include EPOCHP in set of input */
/*                 data if input data delimiter delimiters in one of the */
/*                 symbols that can appear in a time string. */

		    setmsg_("When input record parameters delimiter characte"
			    "r ('#') specified in setup file keyword '#' is o"
			    "ne of the characters that can be used in a valid"
			    " input time string, two time tags cannot be pres"
			    "ent in an input record and therefore epoch of pe"
			    "riapsis cannot be included in the list of parame"
			    "ters in the '#' keyword. ", (ftnlen)312);
		    errch_("#", delmvl, (ftnlen)1, (ftnlen)1);
		    errch_("#", "DATA_DELIMITER", (ftnlen)1, (ftnlen)14);
		    errch_("#", "DATA_ORDER", (ftnlen)1, (ftnlen)10);
		    sigerr_("SPICE(CANTUSEPERIAPEPOCH)", (ftnlen)25);
		} else {

/*                 Get number of lines which each input data record */
/*                 occupies. If it's not present SETUPI will fail with */
/*                 an error message. */

		    setupi_("LINES PER RECORD", &nlnrec, &found, (ftnlen)16);
		    epocfl = 3;
		}
	    } else {

/*              Delimiter is not one of the symbols that can be used in a */
/*              time string. Epoch value may appear in any location in */
/*              the DATA_ORDER. */

		epocfl = 2;
	    }
	} else {

/*           We received length of EPOCH string value. */

	    epocfl = 1;
	}

/*        Get setup file parameters specific for given output SPK type. */

	if (outtvl == 5) {

/*           For output SPK type 5 we always needed GM. */

	    setupd_("CENTER BODY GM", &cngmvl, &found, (ftnlen)14);
	    if (! found) {
		bodvar_(&cnidvl, "GM", &n, &cngmvl, (ftnlen)2);
	    } else {
		cngmvl = cngmvl * dstcof * dstcof * dstcof;
	    }

/*           For Type 5 SPKs we set the number of "padding" states to 1 */
/*           assuming that if START and/or STOP times were provided */
/*           in the setup file and either of them happen to be between */
/*           two input states, we will grab one state before START or */
/*           after STOP to make sure that interpolated position and */
/*           velocity is as close to the input data as possible. */

	    padsts = 1;
	} else if (outtvl == 8) {

/*           For output SPK type 8 we need GM only if the input type is */
/*           not STATES. */

	    if (! (s_cmp(indtvl, "STATES", rtrim_(indtvl, (ftnlen)80), (
		    ftnlen)6) == 0)) {
		setupd_("CENTER BODY GM", &cngmvl, &found, (ftnlen)14);
		if (! found) {
		    bodvar_(&cnidvl, "GM", &n, &cngmvl, (ftnlen)2);
		} else {
		    cngmvl = cngmvl * dstcof * dstcof * dstcof;
		}
	    }

/*           We also need polynomial degree which must be a positive */
/*           number. */

	    setupi_("POLYNOMIAL DEGREE", &pldgvl, &found, (ftnlen)17);
	    if (pldgvl <= 0 || pldgvl > 15) {
		setmsg_("The degree of Lagrange polynomial specified in the "
			"setup keyword '#' must be a positive number less tha"
			"n #. It was #.", (ftnlen)117);
		errch_("#", "POLYNOM_DEGREE", (ftnlen)1, (ftnlen)14);
		errint_("#", &c__16, (ftnlen)1);
		errint_("#", &pldgvl, (ftnlen)1);
		sigerr_("SPICE(BADLAGRANGEDEGREE)", (ftnlen)24);
	    }

/*           For Type 8 SPKs we set the number of "padding" states to */
/*           the specified polynomial degree. If START and/or STOP times */
/*           were provided in the setup file and either of them happen */
/*           to be between two input states, we will grab PLDGVL states */
/*           before START or after STOP to make sure that interpolated */
/*           position and velocity is as close to the input data as */
/*           possible. (Well, PLDGVL is overkill, and we could have */
/*           gotten away with 1/2 of it, but it would make the algorithm */
/*           handling START/STOP boundary processing more complex.) */

	    padsts = pldgvl;
	} else if (outtvl == 9) {

/*           For output SPK type 9 we need GM only if input type is not */
/*           STATES. */

	    if (! (s_cmp(indtvl, "STATES", rtrim_(indtvl, (ftnlen)80), (
		    ftnlen)6) == 0)) {
		setupd_("CENTER BODY GM", &cngmvl, &found, (ftnlen)14);
		if (! found) {
		    bodvar_(&cnidvl, "GM", &n, &cngmvl, (ftnlen)2);
		} else {
		    cngmvl = cngmvl * dstcof * dstcof * dstcof;
		}
	    }

/*           We also need polynomial degree which must be a positive */
/*           number. */

	    setupi_("POLYNOMIAL DEGREE", &pldgvl, &found, (ftnlen)17);
	    if (pldgvl <= 0 || pldgvl > 15) {
		setmsg_("The degree of Lagrange polynomial specified in the "
			"setup keyword '#' must be a positive number less tha"
			"n #. It was #.", (ftnlen)117);
		errch_("#", "POLYNOM_DEGREE", (ftnlen)1, (ftnlen)14);
		errint_("#", &c__16, (ftnlen)1);
		errint_("#", &pldgvl, (ftnlen)1);
		sigerr_("SPICE(BADLAGRANGEDEGREE)", (ftnlen)24);
	    }

/*           Same as for type 8: for type 9 we set the number of */
/*           "padding" states to the specified polynomial degree. */

	    padsts = pldgvl;
	} else if (outtvl == 12) {

/*           For output SPK type 12 we need GM only if the input type is */
/*           not STATES. */

	    if (! (s_cmp(indtvl, "STATES", rtrim_(indtvl, (ftnlen)80), (
		    ftnlen)6) == 0)) {
		setupd_("CENTER BODY GM", &cngmvl, &found, (ftnlen)14);
		if (! found) {
		    bodvar_(&cnidvl, "GM", &n, &cngmvl, (ftnlen)2);
		} else {
		    cngmvl = cngmvl * dstcof * dstcof * dstcof;
		}
	    }

/*           We also need polynomial degree which must be an odd number. */

	    setupi_("POLYNOMIAL DEGREE", &pldgvl, &found, (ftnlen)17);
	    if (! odd_(&pldgvl)) {
		setmsg_("The degree of Hermit polynomial specified in the se"
			"tup keyword '#' must be an odd number. It was #. ", (
			ftnlen)100);
		errch_("#", "POLYNOM_DEGREE", (ftnlen)1, (ftnlen)14);
		errint_("#", &pldgvl, (ftnlen)1);
		sigerr_("SPICE(EVENHEMITDEGREE)", (ftnlen)22);
	    }
	    if (pldgvl <= 0 || pldgvl > 15) {
		setmsg_("The degree of Hermit polynomial specified in the se"
			"tup keyword '#' must be a positive number less than "
			"#. It was #.", (ftnlen)115);
		errch_("#", "POLYNOM_DEGREE", (ftnlen)1, (ftnlen)14);
		errint_("#", &c__16, (ftnlen)1);
		errint_("#", &pldgvl, (ftnlen)1);
		sigerr_("SPICE(BADHERMITDEGREE)", (ftnlen)22);
	    }


/*           For Type 12 SPKs we set the number of "padding" states to */
/*           1/2 of the specified polynomial degree. If START and/or */
/*           STOP times were provided in the setup file and either of */
/*           them happen to be between two input states, we will grab */
/*           PLDGVL/2 states before START or after STOP to make sure */
/*           that interpolated position and velocity is as close to the */
/*           input data as possible. (Well, 1/2 of PLDGVL is overkill, */
/*           and we could have gotten away with 1/4 of it, but it would */
/*           make the algorithm handling START/STOP boundary processing */
/*           more complex.) ... and it cannot be zero. */

/* Computing MAX */
	    i__2 = 1, i__3 = pldgvl / 2;
	    padsts = max(i__2,i__3);
	} else if (outtvl == 13) {

/*           For output SPK type 13 we need GM only if input type is not */
/*           STATES. */

	    if (! (s_cmp(indtvl, "STATES", rtrim_(indtvl, (ftnlen)80), (
		    ftnlen)6) == 0)) {
		setupd_("CENTER BODY GM", &cngmvl, &found, (ftnlen)14);
		if (! found) {
		    bodvar_(&cnidvl, "GM", &n, &cngmvl, (ftnlen)2);
		} else {
		    cngmvl = cngmvl * dstcof * dstcof * dstcof;
		}
	    }

/*           We also need polynomial degree which must be an odd number. */

	    setupi_("POLYNOMIAL DEGREE", &pldgvl, &found, (ftnlen)17);
	    if (! odd_(&pldgvl)) {
		setmsg_("The degree of Hermit polynomial specified in the se"
			"tup keyword '#' must be an odd number. It was #. ", (
			ftnlen)100);
		errch_("#", "POLYNOM_DEGREE", (ftnlen)1, (ftnlen)14);
		errint_("#", &pldgvl, (ftnlen)1);
		sigerr_("SPICE(EVENHEMITDEGREE)", (ftnlen)22);
	    }
	    if (pldgvl <= 0 || pldgvl > 15) {
		setmsg_("The degree of Hermit polynomial specified in the se"
			"tup keyword '#' must be a positive number less than "
			"#. It was #.", (ftnlen)115);
		errch_("#", "POLYNOM_DEGREE", (ftnlen)1, (ftnlen)14);
		errint_("#", &c__16, (ftnlen)1);
		errint_("#", &pldgvl, (ftnlen)1);
		sigerr_("SPICE(BADHERMITDEGREE)", (ftnlen)22);
	    }

/*           Same as for type 12: for Type 13 SPKs we set the number of */
/*           "padding" states to 1/2 of the specified polynomial degree. */

/* Computing MAX */
	    i__2 = 1, i__3 = pldgvl / 2;
	    padsts = max(i__2,i__3);
	} else if (outtvl == 15) {

/*           For output SPK type 15 we always need GM. */

	    setupd_("CENTER BODY GM", &cngmvl, &found, (ftnlen)14);
	    if (! found) {
		bodvar_(&cnidvl, "GM", &n, &cngmvl, (ftnlen)2);
	    } else {
		cngmvl = cngmvl * dstcof * dstcof * dstcof;
	    }

/*           We also need START and STOP times to be provided in */
/*           the setup file. */

	    if (! statfl) {
		setmsg_("Start time required for the output SPK type 15 was "
			"not provided in the setup file keyword '#'.", (ftnlen)
			94);
		errch_("#", "START_TIME", (ftnlen)1, (ftnlen)10);
		sigerr_("SPICE(NOSTARTTIME4SPK15)", (ftnlen)24);
	    }
	    if (! stotfl) {
		setmsg_("Stop time required for the output SPK type 15 was n"
			"ot provided in the setup file keyword '#'.", (ftnlen)
			93);
		errch_("#", "STOP_TIME", (ftnlen)1, (ftnlen)9);
		sigerr_("SPICE(NOSTOPTIME4SPK15)", (ftnlen)23);
	    }

/*           We need J2, R equ, precession type, polar PV vector. */

	    setupd_("J2", &cnj2vl, &found, (ftnlen)2);
	    if (! found) {
		bodvar_(&cnidvl, "J2", &n, &cnj2vl, (ftnlen)2);
	    }
	    setupd_("EQUATORIAL RADIUS", &requat, &found, (ftnlen)17);
	    if (! found) {
		bodvar_(&cnidvl, "RADII", &n, cnervl, (ftnlen)5);
		requat = cnervl[0];
	    } else {
		requat *= dstcof;
	    }
	    setupd_("PRECESSION TYPE", &j2flg, &found, (ftnlen)15);
	    setupd_("POLE RA", &rapol, &found, (ftnlen)7);
	    rakey = ! found;
	    rapol *= angcof;
	    setupd_("POLE DEC", &depol, &found, (ftnlen)8);
	    dekey = ! found;
	    depol *= angcof;
	    if (! rakey && ! dekey) {

/*              Polar vector rectangular coordinates. */

		pv[0] = cos(rapol) * cos(depol);
		pv[1] = sin(rapol) * cos(depol);
		pv[2] = sin(depol);
	    }

/*           We don't need any padding states for type 15. */

	    padsts = 0;
	} else if (outtvl == 17) {

/*           For output SPK type 17 we need GM if input data type is not */
/*           EQ_ELEMENTS. */

	    if (! (s_cmp(indtvl, "EQ_ELEMENTS", rtrim_(indtvl, (ftnlen)80), (
		    ftnlen)11) == 0)) {
		setupd_("CENTER BODY GM", &cngmvl, &found, (ftnlen)14);
		if (! found) {
		    bodvar_(&cnidvl, "GM", &n, &cngmvl, (ftnlen)2);
		} else {
		    cngmvl = cngmvl * dstcof * dstcof * dstcof;
		}
	    }

/*           We also need START and STOP times to be provided in */
/*           the setup file. */

	    if (! statfl) {
		setmsg_("Start time required for the output SPK type 17 was "
			"not provided in the setup file keyword '#'.", (ftnlen)
			94);
		errch_("#", "START_TIME", (ftnlen)1, (ftnlen)10);
		sigerr_("SPICE(NOSTARTTIME4SPK17)", (ftnlen)24);
	    }
	    if (! stotfl) {
		setmsg_("Stop time required for the output SPK type 17 was n"
			"ot provided in the setup file keyword '#'.", (ftnlen)
			93);
		errch_("#", "STOP_TIME", (ftnlen)1, (ftnlen)9);
		sigerr_("SPICE(NOSTOPTIME4SPK17)", (ftnlen)23);
	    }

/*           We need POLE RA and DEC. Get it. */

	    setupd_("POLE RA", &rapol, &found, (ftnlen)7);
	    rakey = ! found;
	    rapol *= angcof;
	    setupd_("POLE DEC", &depol, &found, (ftnlen)8);
	    dekey = ! found;
	    depol *= angcof;

/*           We don't need any padding states for type 17. */

	    padsts = 0;
	}
    }

/*     We are done with processing of setups; time to get to the real */
/*     business of making SPK :-). The first thing to do is to open SPK. */
/*     We can open it for appending or as a new file depending on what */
/*     user has asked us to do. User can request appending via command */
/*     line or setup file. See if appending was requested via command */
/*     line switch (this is the highest priority indication.) */

    if (! appflg) {

/*        No, it wasn't. We need to check for setup file flag. */

	setupc_("APPEND FLAG", appchr, &found, (ftnlen)11, (ftnlen)80);

/*        If setup file keyword was set to 'yes', re-set append flag */
/*        to indicate that append was requested. */

	if (s_cmp(appchr, "YES", (ftnlen)80, (ftnlen)3) == 0) {
	    appflg = TRUE_;
	}
    }

/*     Check if append was requested. */

    if (appflg) {

/*        Yup. But does the file already exist? */

	if (exists_(outfn, (ftnlen)255)) {

/*           Aha, it does exist. But is it actually an SPK file? */

	    getfat_(outfn, arch, type__, (ftnlen)255, (ftnlen)32, (ftnlen)32);
	    if (s_cmp(arch, "DAF", (ftnlen)32, (ftnlen)3) != 0 || s_cmp(
		    type__, "SPK", (ftnlen)32, (ftnlen)3) != 0) {

/*              If it's not an SPK, it's "no can do" :-) */

		setmsg_("Output file '#', appending to which has been reques"
			"ted, is not an SPK file. Its file architecture/type "
			"were detected to be '#/#'.", (ftnlen)129);
		errch_("#", outfn, (ftnlen)1, (ftnlen)255);
		errch_("#", arch, (ftnlen)1, (ftnlen)32);
		errch_("#", type__, (ftnlen)1, (ftnlen)32);
		sigerr_("SPICE(OUTPUTISNOTSPK)", (ftnlen)21);
	    } else {

/*              The file is an SPK. Open it for appending. */

		spkopa_(outfn, &handle, (ftnlen)255);
	    }
	} else {

/*           Nope, it doesn't exist. Hmmm ... what can we do? Maybe */
/*           open it as a new SPK file? */

	    spkopn_(outfn, "SPK", &c__40000, &handle, (ftnlen)255, (ftnlen)3);
	    appflg = FALSE_;
	}
    } else {

/*        Nope. We need to make a new file. What if the file already */
/*        exists? Then we need to complain loud and clear ... */

	if (exists_(outfn, (ftnlen)255)) {
	    setmsg_("Output SPK file '#' already exists. It must be a new fi"
		    "le or, if appending to this file is desired, appropriate"
		    " command line key or setup file keyword must be specifie"
		    "d.", (ftnlen)169);
	    errch_("#", outfn, (ftnlen)1, (ftnlen)255);
	    sigerr_("SPICE(OUTPUTFILEEXISTS)", (ftnlen)23);
	}

/*        The file doesn't exist. Open it as a new SPK file. */

	spkopn_(outfn, "SPK", &c__40000, &handle, (ftnlen)255, (ftnlen)3);
    }

/*     Before we start processing of the data, we need to add comments */
/*     the output SPK file. */

    tostdo_(" ", (ftnlen)1);
    tostdo_("Writing comments to the output SPK file  ... ", (ftnlen)45);

/*     Set the maximum size of the comment line buffer. */

    ssizec_(&c__500, cmnbuf, (ftnlen)255);
    l = 0;

/*     If the comment file was provided we copy its content to the */
/*     comment area. */

    if (cmntfl) {

/*        We open the comment file, copy text from it to the comment */
/*        buffer line by line, clean non-printing characters from the */
/*        lines on the fly and dump the buffer to the comment area */
/*        when it's full. We repeat until all comments have been copied. */

	txtopr_(cmtfn, &cmnunt, (ftnlen)255);
	eof = FALSE_;

/*        Insert top comment separator line. */

	s_copy(cmnbuf + 1530, astrln, (ftnlen)255, (ftnlen)80);
	s_copy(cmnbuf + 1785, " ", (ftnlen)255, (ftnlen)1);
	l = 2;
	while(! eof) {

/*           Get next comment line. */

	    readln_(&cmnunt, line, &eof, (ftnlen)512);
	    if (! eof) {

/*              Replace non-printing characters with spaces. */

		while(frstnp_(line, (ftnlen)512) != 0) {
		    m = frstnp_(line, (ftnlen)512);
		    *(unsigned char *)&line[m - 1] = ' ';
		}
		if (l < 500) {

/*                 Store line in the buffer. */

		    ++l;
		    s_copy(cmnbuf + ((i__2 = l + 5) < 506 && 0 <= i__2 ? i__2 
			    : s_rnge("cmnbuf", i__2, "mkspk_", (ftnlen)1871)) 
			    * 255, line, (ftnlen)255, rtrim_(line, (ftnlen)
			    512));
		} else {

/*                 Buffer is full. Set the cardinality of the comment */
/*                 buffer and write it to SPK comment area. Reset */
/*                 counter. */

		    scardc_(&l, cmnbuf, (ftnlen)255);
		    spcacb_(&handle, cmnbuf, (ftnlen)255);
		    l = 0;
		}
	    }
	}

/*        Close comments file. */

	cl__1.cerr = 0;
	cl__1.cunit = cmnunt;
	cl__1.csta = 0;
	f_clos(&cl__1);
    }

/*     Dump the rest of the buffer into the comment area. */

    if (l != 0) {
	scardc_(&l, cmnbuf, (ftnlen)255);
	spcacb_(&handle, cmnbuf, (ftnlen)255);
	l = 0;
    }

/*     Add a header preceding contents of the setup file and containing */
/*     setup file name and current CPU time. */

    cputim_(tvec);
    s_copy(tstamp, "YYYY-MM-DDTHR:MN:SC", (ftnlen)80, (ftnlen)19);
    dpfmt_(tvec, "0YYY", tstamp, (ftnlen)4, (ftnlen)4);
    dpfmt_(&tvec[1], "0M", tstamp + 5, (ftnlen)2, (ftnlen)2);
    dpfmt_(&tvec[2], "0D", tstamp + 8, (ftnlen)2, (ftnlen)2);
    dpfmt_(&tvec[3], "0h", tstamp + 11, (ftnlen)2, (ftnlen)2);
    dpfmt_(&tvec[4], "0m", tstamp + 14, (ftnlen)2, (ftnlen)2);
    dpfmt_(&tvec[5], "0s", tstamp + 17, (ftnlen)2, (ftnlen)2);
    s_copy(cmnbuf + 1530, " ", (ftnlen)255, (ftnlen)1);
    s_copy(cmnbuf + 1785, astrln, (ftnlen)255, (ftnlen)80);
/* Writing concatenation */
    i__1[0] = 21, a__1[0] = "MKSPK RUN DATE/TIME: ";
    i__1[1] = rtrim_(tstamp, (ftnlen)80), a__1[1] = tstamp;
    s_cat(cmnbuf + 2040, a__1, i__1, &c__2, (ftnlen)255);
/* Writing concatenation */
    i__1[0] = 21, a__1[0] = "MKSPK SETUP FILE:    ";
    i__1[1] = rtrim_(cmdfil, (ftnlen)255), a__1[1] = cmdfil;
    s_cat(cmnbuf + 2295, a__1, i__1, &c__2, (ftnlen)255);
/* Writing concatenation */
    i__1[0] = 21, a__1[0] = "MKSPK INPUT FILE:    ";
    i__1[1] = rtrim_(inpfn, (ftnlen)255), a__1[1] = inpfn;
    s_cat(cmnbuf + 2550, a__1, i__1, &c__2, (ftnlen)255);
/* Writing concatenation */
    i__1[0] = 21, a__1[0] = "MKSPK OUTPUT FILE:   ";
    i__1[1] = rtrim_(outfn, (ftnlen)255), a__1[1] = outfn;
    s_cat(cmnbuf + 2805, a__1, i__1, &c__2, (ftnlen)255);
    if (appflg) {
	s_copy(cmnbuf + 3060, "OUTPUT FILE STATUS:  EXISTING FILE", (ftnlen)
		255, (ftnlen)34);
    } else {
	s_copy(cmnbuf + 3060, "OUTPUT FILE STATUS:  NEW FILE", (ftnlen)255, (
		ftnlen)29);
    }
    s_copy(cmnbuf + 3315, astrln, (ftnlen)255, (ftnlen)80);
    s_copy(cmnbuf + 3570, " ", (ftnlen)255, (ftnlen)1);
    l = 9;

/*     Now we will copy contents of the setup file to the comment area */
/*     using exactly the same procedure: open the setup file, copy */
/*     text from the file into the buffer line by line, clean */
/*     non-printing characters from the lines on the fly and dump the */
/*     buffer to the comment area when it's full. We repeat until all */
/*     setup lines have been copied. */

    txtopr_(cmdfil, &cmnunt, (ftnlen)255);
    eof = FALSE_;
    while(! eof) {

/*        Read next line. */

	readln_(&cmnunt, line, &eof, (ftnlen)512);
	if (! eof) {

/*           Replace non-printing character with spaces. */

	    while(frstnp_(line, (ftnlen)512) != 0) {
		m = frstnp_(line, (ftnlen)512);
		*(unsigned char *)&line[m - 1] = ' ';
	    }
	    if (l < 500) {

/*              Store line on buffer. */

		++l;
		s_copy(cmnbuf + ((i__2 = l + 5) < 506 && 0 <= i__2 ? i__2 : 
			s_rnge("cmnbuf", i__2, "mkspk_", (ftnlen)1969)) * 255,
			 line, (ftnlen)255, rtrim_(line, (ftnlen)512));
	    } else {

/*              Buffer is full. Set the cardinality of the comment */
/*              buffer and write it to SPK comment area. Reset counter */
/*              and store the last line that we have obtained in the */
/*              first line of the buffer. */

		scardc_(&l, cmnbuf, (ftnlen)255);
		spcacb_(&handle, cmnbuf, (ftnlen)255);
		l = 0;
	    }
	}
    }
    cl__1.cerr = 0;
    cl__1.cunit = cmnunt;
    cl__1.csta = 0;
    f_clos(&cl__1);

/*     Add "bottom of the comments" separator line. */

    if (l <= 498) {
	s_copy(cmnbuf + ((i__2 = l + 6) < 506 && 0 <= i__2 ? i__2 : s_rnge(
		"cmnbuf", i__2, "mkspk_", (ftnlen)1997)) * 255, " ", (ftnlen)
		255, (ftnlen)1);
	s_copy(cmnbuf + ((i__2 = l + 7) < 506 && 0 <= i__2 ? i__2 : s_rnge(
		"cmnbuf", i__2, "mkspk_", (ftnlen)1998)) * 255, astrln, (
		ftnlen)255, (ftnlen)80);
	l += 2;
    } else {

/*        Dump current contents of the comment buffer, first. After */
/*        that stick separator at the top of the buffer. */

	scardc_(&l, cmnbuf, (ftnlen)255);
	spcacb_(&handle, cmnbuf, (ftnlen)255);
	s_copy(cmnbuf + 1530, " ", (ftnlen)255, (ftnlen)1);
	s_copy(cmnbuf + 1785, astrln, (ftnlen)255, (ftnlen)80);
	l = 2;
    }

/*     Dump the buffer one more time. */

    if (l > 0) {
	scardc_(&l, cmnbuf, (ftnlen)255);
	spcacb_(&handle, cmnbuf, (ftnlen)255);
    }

/*     Start processing input data. */

    tostdo_(" ", (ftnlen)1);
    tostdo_("Started processing input data ... ", (ftnlen)34);
    if (s_cmp(indtvl, "TL_ELEMENTS", rtrim_(indtvl, (ftnlen)80), (ftnlen)11) 
	    == 0) {

/*        Process two-line element data and make output SPK of type 10. */

	tle2spk_(inpfn, tlidvl, &cnidvl, frnmvl, sgidvl, &handle, covval, 
		covtyp, (ftnlen)255, (ftnlen)80, (ftnlen)80, (ftnlen)32);
    } else {

/*        Process other than two-line element data. */
/*        Open the input data file. */

	txtopr_(inpfn, &inpunt, (ftnlen)255);
	if (flflag) {

/*           Ignore first NFLVAL lines of input file. */

	    i__2 = nflnvl;
	    for (nl = 1; nl <= i__2; ++nl) {
		readln_(&inpunt, line, &eof, (ftnlen)512);
		if (eof) {
		    dafcls_(&handle);
		    delfil_(outfn, (ftnlen)255);
		    setmsg_("# first lines of the input file '#' cannot be i"
			    "gnored as specified in setup file keyword '#' be"
			    "cause the total number of lines in the file is l"
			    "ess than this number.", (ftnlen)164);
		    errint_("#", &nflnvl, (ftnlen)1);
		    errch_("#", inpfn, (ftnlen)1, (ftnlen)255);
		    errch_("#", "IGNORE_FIRST_LINE", (ftnlen)1, (ftnlen)17);
		    sigerr_("SPICE(TOOFEWINPUTLINES)", (ftnlen)23);
		}
	    }
	}

/*        Set key for reading text data into the lines buffer. */

	eof = FALSE_;

/*        Reset indexes of the DATA_ORDER parameters, of the last */
/*        element in STATE/EPOCH arrays and of the last line that were */
/*        read from the input data file. */

	i__ = 1;
	j = 0;
	recidx = 0;

/*        Reset other buffer parameters. */

	k = 0;
	s_copy(buffer, " ", (ftnlen)1024, (ftnlen)1);
	s_copy(bufaux, " ", (ftnlen)1024, (ftnlen)1);
	while(! eof) {

/*           Reading text data into the lines buffer until the end */
/*           of buffer. */

	    redbuf_(&inpunt, delmvl, &nlnrec, &epocfl, buffer, &k, bufaux, &
		    eof, (ftnlen)1, (ftnlen)1024, (ftnlen)1024);

/*           Start to parse buffer string. Find position of the first */
/*           non-blank character in the buffer. */

	    first = frstnb_(buffer, (ftnlen)1024);
	    while(first != 0) {

/*              Calculate the index N of data order element to which */
/*              we will assign the value with we are about to pull from */
/*              the buffer. */

		if (i__ % nparam == 0) {
		    n = nparam;
		} else {
		    n = i__ % nparam;
		}

/*              Should we skip current value? */

		if (param[(i__2 = n - 1) < 50 && 0 <= i__2 ? i__2 : s_rnge(
			"param", i__2, "mkspk_", (ftnlen)2134)] == 30) {

/*                 Yes, so skip it without parsing and reset FIRST to be */
/*                 beginning of the next value in the buffer. */

/* Writing concatenation */
		    i__1[0] = 1, a__1[0] = delmvl;
		    i__1[1] = 1, a__1[1] = "$";
		    s_cat(ch__2, a__1, i__1, &c__2, (ftnlen)2);
		    npos = cpos_(buffer, ch__2, &first, (ftnlen)1024, (ftnlen)
			    2);
		} else {

/*                 We know the begin character position of the value */
/*                 which we need to parse (FIRST). Now we need to find */
/*                 its end character position in the buffer (NPOS). */
/*                 We do differently depending on the EPOCFL value */
/*                 (which basically determines our parsing schema.) */

		    if (epocfl == 1) {

/*                    The case when the length of EPOCH string was */
/*                    provided in the setup file which simplifies */
/*                    pulling times out of the buffer a lot -- we just */
/*                    grab substring from FIRST to the next delimiter */
/*                    which we start to search for from FIRST+"string */
/*                    length" initial position. */

			if (param[(i__2 = n - 1) < 50 && 0 <= i__2 ? i__2 : 
				s_rnge("param", i__2, "mkspk_", (ftnlen)2161)]
				 == 1 || param[(i__3 = n - 1) < 50 && 0 <= 
				i__3 ? i__3 : s_rnge("param", i__3, "mkspk_", 
				(ftnlen)2161)] == 19) {

/*                       Position of the delimiter for time string value. */

			    i__2 = first + eplnvl;
			    npos = pos_(buffer, delmvl, &i__2, (ftnlen)1024, (
				    ftnlen)1);
			} else {

/*                       Position of the delimiter for other then */
/*                       time string value. */

			    npos = pos_(buffer, delmvl, &first, (ftnlen)1024, 
				    (ftnlen)1);
			}
		    }
		    if (epocfl == 2) {

/*                    This case is when delimiter is the character that */
/*                    can not be used in a time string. We just search */
/*                    for position of the next delimiter starting from */
/*                    FIRST. */

			npos = pos_(buffer, delmvl, &first, (ftnlen)1024, (
				ftnlen)1);
		    }
		    if (epocfl == 3) {

/*                    Though case -- the delimiter is one of the */
/*                    characters that can used in a time string. */
/*                    Luckily, there is only one EPOCH string can */
/*                    be present in each record in this situation. */

			if (param[(i__2 = n - 1) < 50 && 0 <= i__2 ? i__2 : 
				s_rnge("param", i__2, "mkspk_", (ftnlen)2201)]
				 == 1) {

/*                       Our current value is EPOCH -- we need to find */
/*                       next end-of-record delimiter position on the */
/*                       buffer. */

			    neor = pos_(buffer, "$", &first, (ftnlen)1024, (
				    ftnlen)1);
			    npar = nparam;
			    par = param[(i__2 = npar - 1) < 50 && 0 <= i__2 ? 
				    i__2 : s_rnge("param", i__2, "mkspk_", (
				    ftnlen)2210)];

/*                       Lets see if our epoch is the last parameter in */
/*                       the record. */

			    if (par != 1) {

/*                          No, it is not. Well, we need to search for */
/*                          delimiter position in reverse order until we */
/*                          find a delimiter that immediately follows our */
/*                          EPOCH value. */

				while(par != 1) {

/*                             Search in reverse order for delimiter */
/*                             from the last found position of the record */
/*                             marker. */

				    neor = posr_(buffer, delmvl, &neor, (
					    ftnlen)1024, (ftnlen)1) - 1;

/*                             If our delimiter is a space, we need to */
/*                             advance even further back to make sure */
/*                             that a call POSR on the next iteration */
/*                             finds the space that precedes our current */
/*                             value rather than follows it. */

				    if (*(unsigned char *)delmvl == ' ') {
					if (*(unsigned char *)&buffer[neor - 
						1] == ' ') {

/*                                   Skip white spaces. */

					    while(*(unsigned char *)&buffer[
						    neor - 1] == ' ') {
			  --neor;
					    }
					}
				    }
				    --npar;
				    par = param[(i__2 = npar - 1) < 50 && 0 <=
					     i__2 ? i__2 : s_rnge("param", 
					    i__2, "mkspk_", (ftnlen)2259)];
				}
				npos = neor + 1;
			    } else {

/*                          Yes, the EPOCH is the last parameter in the */
/*                          record. It means that NEOR is currently */
/*                          pointing at the artificially inserted */
/*                          end-or-record marker. We just reassign it to */
/*                          NPOS. */

				npos = neor;
			    }
			} else {

/*                       Position of the delimiter for other then */
/*                       time string value. */

/* Writing concatenation */
			    i__1[0] = 1, a__1[0] = delmvl;
			    i__1[1] = 1, a__1[1] = "$";
			    s_cat(ch__2, a__1, i__1, &c__2, (ftnlen)2);
			    npos = cpos_(buffer, ch__2, &first, (ftnlen)1024, 
				    (ftnlen)2);
			}
		    }

/*                 At this point we have determined where in the input */
/*                 buffer our current value is. We pull it out, parse */
/*                 it, store parsed value in the data buffer, change */
/*                 data order index counter and shift to the next value */
/*                 in the buffer. */

		    s_copy(wrkchr, buffer + (first - 1), (ftnlen)512, npos - 
			    1 - (first - 1));

/*                 Is current value a time string or a number? */

		    if (param[(i__2 = n - 1) < 50 && 0 <= i__2 ? i__2 : 
			    s_rnge("param", i__2, "mkspk_", (ftnlen)2302)] == 
			    1 || param[(i__3 = n - 1) < 50 && 0 <= i__3 ? 
			    i__3 : s_rnge("param", i__3, "mkspk_", (ftnlen)
			    2302)] == 19) {

/*                    Are times given as ET seconds past J2000? */

			if (ettags) {

/*                       Yes. Parse them as DPs. */

			    nparsd_(wrkchr, &dvl[(i__3 = param[(i__2 = n - 1) 
				    < 50 && 0 <= i__2 ? i__2 : s_rnge("param",
				     i__2, "mkspk_", (ftnlen)2314)] - 1) < 50 
				    && 0 <= i__3 ? i__3 : s_rnge("dvl", i__3, 
				    "mkspk_", (ftnlen)2314)], error, &ptr, (
				    ftnlen)512, (ftnlen)512);
			    if (ptr != 0) {

/*                          We can not parse this data value. */

				dafcls_(&handle);
				delfil_(outfn, (ftnlen)255);
				setmsg_("The string '#' found in the input f"
					"ile '#' doesn't represent a double p"
					"recision number which it should be t"
					"o be a value of input parameter '#'.",
					 (ftnlen)143);
				errch_("#", wrkchr, (ftnlen)1, (ftnlen)512);
				errch_("#", inpfn, (ftnlen)1, (ftnlen)255);
				errch_("#", doval + ((i__3 = param[(i__2 = n 
					- 1) < 50 && 0 <= i__2 ? i__2 : 
					s_rnge("param", i__2, "mkspk_", (
					ftnlen)2333)] - 1) < 50 && 0 <= i__3 ?
					 i__3 : s_rnge("doval", i__3, "mkspk_"
					, (ftnlen)2333)) * 12, (ftnlen)1, (
					ftnlen)12);
				sigerr_("SPICE(NOTADPNUMBER)", (ftnlen)19);
			    }
			} else {

/*                       No. Insert time string into the wrapper and */
/*                       pass it to the STR2ET. */

			    repmc_(tmwrvl, "#", wrkchr, htime, (ftnlen)80, (
				    ftnlen)1, (ftnlen)512, (ftnlen)512);
			    str2et_(htime, &dvl[(i__3 = param[(i__2 = n - 1) <
				     50 && 0 <= i__2 ? i__2 : s_rnge("param", 
				    i__2, "mkspk_", (ftnlen)2344)] - 1) < 50 
				    && 0 <= i__3 ? i__3 : s_rnge("dvl", i__3, 
				    "mkspk_", (ftnlen)2344)], (ftnlen)512);
			}
		    } else {

/*                    Parse an input number as DP. */

			nparsd_(wrkchr, &dvl[(i__3 = param[(i__2 = n - 1) < 
				50 && 0 <= i__2 ? i__2 : s_rnge("param", i__2,
				 "mkspk_", (ftnlen)2353)] - 1) < 50 && 0 <= 
				i__3 ? i__3 : s_rnge("dvl", i__3, "mkspk_", (
				ftnlen)2353)], error, &ptr, (ftnlen)512, (
				ftnlen)512);
			if (ptr != 0) {

/*                       We can not parse this data value. */

			    dafcls_(&handle);
			    delfil_(outfn, (ftnlen)255);
			    setmsg_("The string '#' found in the input file "
				    "'#' doesn't represent a double precision"
				    " number which it should be to be a value"
				    " of input parameter '#'.", (ftnlen)143);
			    errch_("#", wrkchr, (ftnlen)1, (ftnlen)512);
			    errch_("#", inpfn, (ftnlen)1, (ftnlen)255);
			    errch_("#", doval + ((i__3 = param[(i__2 = n - 1) 
				    < 50 && 0 <= i__2 ? i__2 : s_rnge("param",
				     i__2, "mkspk_", (ftnlen)2371)] - 1) < 50 
				    && 0 <= i__3 ? i__3 : s_rnge("doval", 
				    i__3, "mkspk_", (ftnlen)2371)) * 12, (
				    ftnlen)1, (ftnlen)12);
			    sigerr_("SPICE(NOTADPNUMBER)", (ftnlen)19);
			}
		    }
		}

/*              Check whether we have accumulated a complete set of */
/*              parameter for one data record (point). If so, start */
/*              processing of the record. */

		if (i__ % nparam == 0) {
		    ++j;
		    ++recidx;

/*                 We have accumulated full set of data to process */
/*                 input data for SPKW... We process accumulated set of */
/*                 parameters depending on the input data type. */

		    if (s_cmp(indtvl, "ELEMENTS", rtrim_(indtvl, (ftnlen)80), 
			    (ftnlen)8) == 0) {

/*                    Input data type is ELEMENTS. We need to convert */
/*                    whatever set of elements we have got on the */
/*                    input the standard SPICE representation. */

			setelm_(&cngmvl, dvl, &dstcof, &angcof, param, &
				nparam, elts, &errflg, errstr, (ftnlen)512);
			if (errflg) {

/*                       We could not calculate standard SPICE conic */
/*                       elements from a given set. Signal and exit. */

			    dafcls_(&handle);
			    delfil_(outfn, (ftnlen)255);
/* Writing concatenation */
			    i__4[0] = rtrim_(errstr, (ftnlen)512), a__2[0] = 
				    errstr;
			    i__4[1] = 31, a__2[1] = " Verify whether set of "
				    "orbital ";
			    i__4[2] = 26, a__2[2] = "elements specified in t"
				    "he ";
			    i__4[3] = 25, a__2[3] = "keyword '#' of the setu"
				    "p ";
			    i__4[4] = 24, a__2[4] = "file '#' is correct and "
				    ;
			    i__4[5] = 29, a__2[5] = "whether it's consistent"
				    " with ";
			    i__4[6] = 30, a__2[6] = "actual values provided "
				    "in the ";
			    i__4[7] = 16, a__2[7] = "input file '#'. ";
			    s_cat(ch__3, a__2, i__4, &c__8, (ftnlen)693);
			    setmsg_(ch__3, rtrim_(errstr, (ftnlen)512) + 181);
			    errch_("#", "DATA_ORDER", (ftnlen)1, (ftnlen)10);
			    errch_("#", cmdfil, (ftnlen)1, (ftnlen)255);
			    errch_("#", inpfn, (ftnlen)1, (ftnlen)255);
			    sigerr_(errstr + 512, (ftnlen)512);
			}

/*                    Convert elements to state vector and accumulate */
/*                    in the STATE buffer. */

			conics_(elts, dvl, &state[(i__2 = j * 6 - 6) < 60000 
				&& 0 <= i__2 ? i__2 : s_rnge("state", i__2, 
				"mkspk_", (ftnlen)2433)]);

/*                    Accumulate epoch in EPOCH buffer. */

			epoch[(i__2 = j - 1) < 10000 && 0 <= i__2 ? i__2 : 
				s_rnge("epoch", i__2, "mkspk_", (ftnlen)2438)]
				 = dvl[0];
		    } else if (s_cmp(indtvl, "STATES", rtrim_(indtvl, (ftnlen)
			    80), (ftnlen)6) == 0) {

/*                    Input type is STATES. We accumulate state vector */
/*                    in STATE buffer . */

			for (l = 1; l <= 6; ++l) {
			    state[(i__2 = l + j * 6 - 7) < 60000 && 0 <= i__2 
				    ? i__2 : s_rnge("state", i__2, "mkspk_", (
				    ftnlen)2448)] = dvl[(i__3 = l) < 50 && 0 
				    <= i__3 ? i__3 : s_rnge("dvl", i__3, 
				    "mkspk_", (ftnlen)2448)] * dstcof;
			}

/*                    Accumulate epoch in EPOCH buffer. */

			epoch[(i__2 = j - 1) < 10000 && 0 <= i__2 ? i__2 : 
				s_rnge("epoch", i__2, "mkspk_", (ftnlen)2455)]
				 = dvl[0];

/*                    For output SPK types 15 and 17 we need to convert */
/*                    state to SPICE conic elements that will be stored */
/*                    in the output SPK file. */

			if (outtvl == 15 || outtvl == 17) {
			    oscelt_(&state[(i__2 = j * 6 - 6) < 60000 && 0 <= 
				    i__2 ? i__2 : s_rnge("state", i__2, "mks"
				    "pk_", (ftnlen)2464)], &epoch[(i__3 = j - 
				    1) < 10000 && 0 <= i__3 ? i__3 : s_rnge(
				    "epoch", i__3, "mkspk_", (ftnlen)2464)], &
				    cngmvl, elts);
			}
		    } else {

/*                    If input type is EQ_ELEMENTS we simply accumulate */
/*                    epoch in EPOCH buffer and leave the rest */
/*                    parameters in the DVAL array. */

			epoch[(i__2 = j - 1) < 10000 && 0 <= i__2 ? i__2 : 
				s_rnge("epoch", i__2, "mkspk_", (ftnlen)2476)]
				 = dvl[0];
		    }

/*                 Now we do additional processing depending on the */
/*                 output SPK type. There are two cases here: SPK */
/*                 type 15 for which we may need to compute the elements */
/*                 for the time of periapsis amd SPK 8 and 12 for which */
/*                 we need to verify equal spacing between data points. */

		    if (outtvl == 15) {

/*                    If mean anomaly of the input elements is not zero, */
/*                    we need to re-compute the elements to be at the */
/*                    time of periapsis. */

			if (elts[5] != 0.) {
			    if (elts[1] < 1.) {

/*                          Case of elliptical orbit. */

/*                          Start with determining the time of the */
/*                          nearest periapse. */

				wrkel = (1. - elts[1]) / elts[0];
				meanm = sqrt(elts[7] * wrkel) * wrkel;
				m0 = elts[5];
				if (m0 > pi_()) {
				    m0 -= twopi_();
				}
				epoch[(i__2 = j - 1) < 10000 && 0 <= i__2 ? 
					i__2 : s_rnge("epoch", i__2, "mkspk_",
					 (ftnlen)2512)] = elts[6] - m0 / 
					meanm;

/*                          Like SPKE15, use brute-force to compute true */
/*                          anomaly of the input record. */

				conics_(elts, &epoch[(i__2 = j - 1) < 10000 &&
					 0 <= i__2 ? i__2 : s_rnge("epoch", 
					i__2, "mkspk_", (ftnlen)2518)], 
					hstate);
				ta = vsep_(hstate, &state[(i__2 = j * 6 - 6) <
					 60000 && 0 <= i__2 ? i__2 : s_rnge(
					"state", i__2, "mkspk_", (ftnlen)2519)
					]);
				ta = d_sign(&ta, &m0);

/*                          Continue mimicking SPKE15 algorithm computing */
/*                          node and apsides delta angles. */

				cosinc = cos(elts[2]);
				p = elts[0] * (elts[1] + 1.);
/* Computing 2nd power */
				d__1 = requat / p;
				z__ = ta * 1.5 * cnj2vl * (d__1 * d__1);
				dnode = -z__ * cosinc;
/* Computing 2nd power */
				d__1 = cosinc;
				dperi = z__ * (d__1 * d__1 * 2.5 - .5);

/*                          Apply node and apsides delta angles to the */
/*                          elements as directed by the precession flag. */
/*                          If the flag is 3, do no adjustments. If the */
/*                          flag is not 1, adjust the argument of */
/*                          periapse. If the flag is not 2, adjust the */
/*                          longitude of the ascending node. */

				if (j2flg != 3.) {
				    if (j2flg != 1.) {
					elts[4] -= dperi;
				    }
				    if (j2flg != 2.) {
					elts[3] -= dnode;
				    }
				}
			    } else if (elts[1] > 1.) {

/*                          Case of hyperbolic orbit. */

				wrkel = (elts[1] - 1.) / elts[0];
				meanm = sqrt(elts[7] * wrkel) * wrkel;
				epoch[(i__2 = j - 1) < 10000 && 0 <= i__2 ? 
					i__2 : s_rnge("epoch", i__2, "mkspk_",
					 (ftnlen)2562)] = elts[6] - elts[5] / 
					meanm;
			    } else {

/*                          Case of parabolic orbit. */

				meanm = sqrt(elts[7] / 2. / elts[0]) / elts[0]
					;
				epoch[(i__2 = j - 1) < 10000 && 0 <= i__2 ? 
					i__2 : s_rnge("epoch", i__2, "mkspk_",
					 (ftnlen)2572)] = elts[6] - elts[5] / 
					meanm;
			    }

/*                       Calculate state vector at epoch of periapsis. */

			    elts[5] = 0.;
			    elts[6] = epoch[(i__2 = j - 1) < 10000 && 0 <= 
				    i__2 ? i__2 : s_rnge("epoch", i__2, "mks"
				    "pk_", (ftnlen)2580)];
			    conics_(elts, &epoch[(i__2 = j - 1) < 10000 && 0 
				    <= i__2 ? i__2 : s_rnge("epoch", i__2, 
				    "mkspk_", (ftnlen)2582)], &state[(i__3 = 
				    j * 6 - 6) < 60000 && 0 <= i__3 ? i__3 : 
				    s_rnge("state", i__3, "mkspk_", (ftnlen)
				    2582)]);
			}
		    } else if (outtvl == 8 || outtvl == 12) {

/*                    For types 8 and 12 we need to verify time spacing */
/*                    between this data point and the previous one. It */
/*                    must the same (within tolerance) as the one before. */

			if (j > 2) {
			    tmpdp = epoch[(i__2 = j - 1) < 10000 && 0 <= i__2 
				    ? i__2 : s_rnge("epoch", i__2, "mkspk_", (
				    ftnlen)2596)] - epoch[(i__3 = j - 2) < 
				    10000 && 0 <= i__3 ? i__3 : s_rnge("epoch"
				    , i__3, "mkspk_", (ftnlen)2596)];
			    if ((d__1 = tmpdp - tstep, abs(d__1)) > .001) {

/*                          Time step changed. Signal an error and exit. */

				dafcls_(&handle);
				delfil_(outfn, (ftnlen)255);
				setmsg_("Time spacing between input data poi"
					"nts #, #, and # is not constant. It "
					"must be for output SPK types 8 and 1"
					"2. UTC times of the data points are:"
					" '#', '#', and '#'.", (ftnlen)162);
				i__2 = recidx - 2;
				errint_("#", &i__2, (ftnlen)1);
				i__2 = recidx - 1;
				errint_("#", &i__2, (ftnlen)1);
				errint_("#", &recidx, (ftnlen)1);
				timout_(&epoch[(i__2 = j - 3) < 10000 && 0 <= 
					i__2 ? i__2 : s_rnge("epoch", i__2, 
					"mkspk_", (ftnlen)2616)], "YYYY-MM-D"
					"DTHR:MN:SC.### ::RND", error, (ftnlen)
					29, (ftnlen)512);
				errch_("#", error, (ftnlen)1, (ftnlen)512);
				timout_(&epoch[(i__2 = j - 2) < 10000 && 0 <= 
					i__2 ? i__2 : s_rnge("epoch", i__2, 
					"mkspk_", (ftnlen)2619)], "YYYY-MM-D"
					"DTHR:MN:SC.### ::RND", error, (ftnlen)
					29, (ftnlen)512);
				errch_("#", error, (ftnlen)1, (ftnlen)512);
				timout_(&epoch[(i__2 = j - 1) < 10000 && 0 <= 
					i__2 ? i__2 : s_rnge("epoch", i__2, 
					"mkspk_", (ftnlen)2622)], "YYYY-MM-D"
					"DTHR:MN:SC.### ::RND", error, (ftnlen)
					29, (ftnlen)512);
				errch_("#", error, (ftnlen)1, (ftnlen)512);
				sigerr_("SPICE(UNEQUALTIMESTEP)", (ftnlen)22);
			    }
			}

/*                    Memorize current time step. */

			if (j > 1) {
			    tstep = epoch[(i__2 = j - 1) < 10000 && 0 <= i__2 
				    ? i__2 : s_rnge("epoch", i__2, "mkspk_", (
				    ftnlen)2636)] - epoch[(i__3 = j - 2) < 
				    10000 && 0 <= i__3 ? i__3 : s_rnge("epoch"
				    , i__3, "mkspk_", (ftnlen)2636)];
			}
		    }

/*                 The next block is an initialization and check-out */
/*                 for a few things that we couldn't get/check before */
/*                 we have got at least one input record ( .... hm, */
/*                 for some SPK types it can be THE ONLY record we need.) */

		    if (onerun) {

/*                    For output types 15 and 17 we need to compute */
/*                    center body North Pole RA and DEC if they weren't */
/*                    provided in the setup file. */

			if ((outtvl == 15 || outtvl == 17) && (rakey || dekey)
				) {

/*                       First, get matrix from J2000 to the output */
/*                       reference frame at the time of our data point. */

			    sxform_("J2000", frnmvl, &epoch[(i__2 = j - 1) < 
				    10000 && 0 <= i__2 ? i__2 : s_rnge("epoch"
				    , i__2, "mkspk_", (ftnlen)2663)], xfrm, (
				    ftnlen)5, (ftnlen)80);
			    for (l = 1; l <= 3; ++l) {
				for (m = 1; m <= 3; ++m) {
				    mwor[(i__2 = l + m * 3 - 4) < 9 && 0 <= 
					    i__2 ? i__2 : s_rnge("mwor", i__2,
					     "mkspk_", (ftnlen)2667)] = xfrm[(
					    i__3 = l + m * 6 - 7) < 36 && 0 <=
					     i__3 ? i__3 : s_rnge("xfrm", 
					    i__3, "mkspk_", (ftnlen)2667)];
				}
			    }

/*                       Now compute rotation matrix from J2000 to the */
/*                       center body body-fixed IAU frame, extract pole */
/*                       direction (Z), rotate it to the output */
/*                       reference frame and compute its RA and DEC. */

			    bodmat_(&cnidvl, &epoch[(i__2 = j - 1) < 10000 && 
				    0 <= i__2 ? i__2 : s_rnge("epoch", i__2, 
				    "mkspk_", (ftnlen)2677)], tipm);
			    mxmt_(mwor, tipm, mpol);
			    pv[0] = mpol[6];
			    pv[1] = mpol[7];
			    pv[2] = mpol[8];
			    recrad_(pv, &wrkel, &rapol, &depol);
			}

/*                    For output type 15 we need to compute P, E, TP, PA */
/*                    (refer to SPKW15 to see what they are :) */

			if (outtvl == 15) {
			    p = elts[0] * (elts[1] + 1.);
			    e = elts[1];
			    ucrss_(&state[(i__2 = j * 6 - 6) < 60000 && 0 <= 
				    i__2 ? i__2 : s_rnge("state", i__2, "mks"
				    "pk_", (ftnlen)2697)], &state[(i__3 = j * 
				    6 - 3) < 60000 && 0 <= i__3 ? i__3 : 
				    s_rnge("state", i__3, "mkspk_", (ftnlen)
				    2697)], tp);
			    vhat_(&state[(i__2 = j * 6 - 6) < 60000 && 0 <= 
				    i__2 ? i__2 : s_rnge("state", i__2, "mks"
				    "pk_", (ftnlen)2698)], pa);

/*                       We also set "end of segment data collection" */
/*                       flag because for output type 15 we need only */
/*                       one record ... and we have got it already. */

			    eosgmr = TRUE_;
			    jlepoc = 1;
			}

/*                    For output type 17 we need to compute re-assign */
/*                    the equinoctial elements from the temporary buffer */
/*                    DVAL or compute them from "plain" elements if we */
/*                    have them from the input. */

			if (outtvl == 17) {

/*                       Calculate equinoctial elements for output */
/*                       type 17. */

			    if (s_cmp(indtvl, "EQ_ELEMENTS", rtrim_(indtvl, (
				    ftnlen)80), (ftnlen)11) == 0) {

/*                          Input data type is EQ_ELEMENTS. */

				eqel[0] = dvl[20] * dstcof;
				eqel[1] = dvl[21];
				eqel[2] = dvl[22];
				eqel[3] = dvl[23] * angcof;
				eqel[4] = dvl[24];
				eqel[5] = dvl[25];
				eqel[6] = dvl[26] * angcof;
				eqel[7] = dvl[27] * angcof;
				eqel[8] = dvl[28] * angcof;
			    } else {

/*                          Input data type is not EQ_ELEMENTS. */
/*                          Convert conic elements to equinoctial. */

				eqel[0] = elts[0] / (1. - elts[1]);
				eqel[1] = elts[1] * sin(elts[3] + elts[4]);
				eqel[2] = elts[1] * cos(elts[3] + elts[4]);
				eqel[3] = elts[3] + elts[4] + elts[5];
				eqel[4] = tan(elts[2] / 2.) * sin(elts[3]);
				eqel[5] = tan(elts[2] / 2.) * cos(elts[3]);
				eqel[6] = dvl[26] * angcof;
				eqel[7] = dvl[27] * angcof;
				eqel[8] = dvl[28] * angcof;
			    }

/*                       Same as for type 15 -- we also set "end of */
/*                       segment data collection" flag because for */
/*                       output type 17 we need only one record ... and */
/*                       we have got it already. */

			    eosgmr = TRUE_;
			    jlepoc = 1;
			}

/*                    For SPK types 8, 9, 12 and 13 we need to verify */
/*                    first data point epoch against start and stop */
/*                    times specified in the setup file. The */
/*                    corresponding SPKWxx routines will do it later, of */
/*                    course, but if we do this check now, user won't */
/*                    need to wait until the program "sucks" in all */
/*                    input data just to fail :-). */

			if (outtvl == 8 || outtvl == 9 || outtvl == 12 || 
				outtvl == 13) {

/*                       Verify if START time is correctly defined */
/*                       relatively first epoch of input data. */

			    if (statfl && epoch[0] > etbeg) {

/*                          Epoch of first point of input data is greater */
/*                          than START time defined in setup. Complain. */

				dafcls_(&handle);
				delfil_(outfn, (ftnlen)255);
				setmsg_("The epoch of the first input data f"
					"ile record is greater than the start"
					" time specified in the setup file ke"
					"yword '#'.", (ftnlen)117);
				errch_("#", "START_TIME", (ftnlen)1, (ftnlen)
					10);
				sigerr_("SPICE(INCONSISTSTARTTIME)", (ftnlen)
					25);
			    }

/*                       Verify if STOP time is correctly defined */
/*                       relatively first epoch of input data. */

			    if (stotfl && epoch[0] > etend) {

/*                          Epoch of first point of input data is greater */
/*                          than STOP time defined in setup. Complain. */

				dafcls_(&handle);
				delfil_(outfn, (ftnlen)255);
				setmsg_("The epoch of the first input data f"
					"ile record is greater than the stop "
					"time specified in the setup file key"
					"word '#'.", (ftnlen)116);
				errch_("#", "STOP_TIME", (ftnlen)1, (ftnlen)9)
					;
				sigerr_("SPICE(INCONSISTSTOPTIME)", (ftnlen)
					24);
			    }
			}
			onerun = FALSE_;

/*                    End of one time running part. */

		    }

/*                 The block in the following IF case essentially does */
/*                 time bracketing by comparing epoch of the current */
/*                 data record with the start and stop times provided in */
/*                 the setup file. (Note that at this point the EOSGMR */
/*                 is .TRUE. for output types 15 or 17, for which this */
/*                 bracketing is not relevant, and .FALSE. for all other */
/*                 types.) */

		    if (! eosgmr) {

/*                    While we are at it, let's check if our times are */
/*                    increasing and non-duplicate. (Of course, SPKWxx */
/*                    will check for this too, but we can do a better */
/*                    job by telling users which times exactly aren't */
/*                    good before filling up all buffers and calling */
/*                    SPKWxx.) */

			if (j > 1) {
			    if (epoch[(i__2 = j - 1) < 10000 && 0 <= i__2 ? 
				    i__2 : s_rnge("epoch", i__2, "mkspk_", (
				    ftnlen)2858)] == epoch[(i__3 = j - 2) < 
				    10000 && 0 <= i__3 ? i__3 : s_rnge("epoch"
				    , i__3, "mkspk_", (ftnlen)2858)]) {
				dafcls_(&handle);
				delfil_(outfn, (ftnlen)255);
				timout_(&epoch[(i__2 = j - 1) < 10000 && 0 <= 
					i__2 ? i__2 : s_rnge("epoch", i__2, 
					"mkspk_", (ftnlen)2861)], "YYYY-MM-D"
					"DTHR:MN:SC.### ::RND", error, (ftnlen)
					29, (ftnlen)512);
				setmsg_("The epoch of the input data record "
					"No. # -- '#' -- is the same as of th"
					"e input data record No. #. Duplicate"
					" time tags are not allowed.", (ftnlen)
					134);
				i__2 = recidx - 1;
				errint_("#", &i__2, (ftnlen)1);
				errch_("#", error, (ftnlen)1, (ftnlen)512);
				errint_("#", &recidx, (ftnlen)1);
				sigerr_("SPICE(DUPLICATETIMES)", (ftnlen)21);
			    }
			    if (epoch[(i__2 = j - 1) < 10000 && 0 <= i__2 ? 
				    i__2 : s_rnge("epoch", i__2, "mkspk_", (
				    ftnlen)2874)] < epoch[(i__3 = j - 2) < 
				    10000 && 0 <= i__3 ? i__3 : s_rnge("epoch"
				    , i__3, "mkspk_", (ftnlen)2874)]) {
				dafcls_(&handle);
				delfil_(outfn, (ftnlen)255);
				setmsg_("The epoch of the input data record "
					"No. # -- '#' -- is greater than the "
					"epoch of the input data record No. #"
					" -- '#'. Times out of order are not "
					"allowed.", (ftnlen)151);
				i__2 = recidx - 1;
				errint_("#", &i__2, (ftnlen)1);
				timout_(&epoch[(i__2 = j - 2) < 10000 && 0 <= 
					i__2 ? i__2 : s_rnge("epoch", i__2, 
					"mkspk_", (ftnlen)2883)], "YYYY-MM-D"
					"DTHR:MN:SC.### ::RND", error, (ftnlen)
					29, (ftnlen)512);
				errch_("#", error, (ftnlen)1, (ftnlen)512);
				errint_("#", &recidx, (ftnlen)1);
				timout_(&epoch[(i__2 = j - 1) < 10000 && 0 <= 
					i__2 ? i__2 : s_rnge("epoch", i__2, 
					"mkspk_", (ftnlen)2887)], "YYYY-MM-D"
					"DTHR:MN:SC.### ::RND", error, (ftnlen)
					29, (ftnlen)512);
				errch_("#", error, (ftnlen)1, (ftnlen)512);
				sigerr_("SPICE(TIMESOUTOFORDER)", (ftnlen)22);
			    }
			}

/*                    Now, let's do time bracketing. Was START time */
/*                    provided in the setup file? */

			if (statfl) {

/*                       Yes. Did we already find an input epoch greater */
/*                       than the START time? */

			    if (staepo) {

/*                          No. Is current epoch greater than or equal */
/*                          to the START time? */

				if (epoch[(i__2 = j - 1) < 10000 && 0 <= i__2 
					? i__2 : s_rnge("epoch", i__2, "mksp"
					"k_", (ftnlen)2911)] >= etbeg) {

/*                             It is. We memorize the coverage start */
/*                             time and set the flag indicating that we */
/*                             are done processing start time bracket. */

				    etfst = etbeg;
				    staepo = FALSE_;
				} else {

/*                             Current epoch is less than START time. */
/*                             Depending on whether we have enough */
/*                             padding states or not, we do nothing or */
/*                             "drop" the first buffered state. */

				    if (j > padsts) {
					m = j * 6;
					remlad_(&c__6, &c__1, state, &m);
					remlad_(&c__1, &c__1, epoch, &j);
				    }
				}
			    }
			} else {

/*                       START time was not provided in the setup file. */
/*                       We set it to be the first value of input data. */

			    if (staepo) {
				etfst = epoch[(i__2 = j - 1) < 10000 && 0 <= 
					i__2 ? i__2 : s_rnge("epoch", i__2, 
					"mkspk_", (ftnlen)2947)];
				staepo = FALSE_;
			    }
			}

/*                    Was STOP time provided in the setup file? */

			if (stotfl) {

/*                       Yes. Did we already find an input epoch greater */
/*                       than STOP time? */

			    if (stoepo) {

/*                          No. Is current epoch greater than the STOP */
/*                          time? */

				if (epoch[(i__2 = j - 1) < 10000 && 0 <= i__2 
					? i__2 : s_rnge("epoch", i__2, "mksp"
					"k_", (ftnlen)2969)] > etend) {

/*                             It is. Memorize its index (as the */
/*                             temporary index of the last state we will */
/*                             write to the segment) and coverage end */
/*                             time and set the flag indicating that we */
/*                             are done processing end time bracket. */

				    jlepoc = j;
				    etlst = etend;
				    stoepo = FALSE_;

/*                             If we needed only one padding state or if */
/*                             we have filled up the buffer, we are */
/*                             done. */

				    if (padsts == 1 || j == 10000) {
					eosgmr = TRUE_;
				    }
				} else {

/*                             STOP time is defined in the setup but */
/*                             current epoch is less than it. */

				    jlepoc = j;
				    etlst = epoch[(i__2 = j - 1) < 10000 && 0 
					    <= i__2 ? i__2 : s_rnge("epoch", 
					    i__2, "mkspk_", (ftnlen)2999)];
				}
			    } else {

/*                          Yes. We already found epoch greater than the */
/*                          specified stop time. If we buffered enough */
/*                          states for padding (or filled our buffer up) */
/*                          we are done. */

				if (j - jlepoc == padsts - 1 || j == 10000) {
				    jlepoc = j;
				    eosgmr = TRUE_;
				}
			    }
			} else {

/*                       STOP time is not defined in setup. We set it as */
/*                       current value of epoch array. */

			    jlepoc = j;
			    etlst = epoch[(i__2 = j - 1) < 10000 && 0 <= i__2 
				    ? i__2 : s_rnge("epoch", i__2, "mkspk_", (
				    ftnlen)3026)];
			}
		    }

/*                 Let's see if it's time to write a segment. We do it */
/*                 in two cases: */

/*                    -- when we filled up a data buffer; or */

/*                    -- when for some reason we have decided that we */
/*                       have processed all input data we needed and */
/*                       its time to write the last segment and bail */
/*                       out of the program. */

/*                 The reason for the latter case would be that we have */
/*                 got one and only required record for types 15 and 17 */
/*                 or that the last record that we have got from the */
/*                 input had time greater than the stop time provided in */
/*                 setup file. (in both cases EOSGMR was set to .TRUE.) */

/*                 In other words (Nicolas's words :) when we have */
/*                 maximum set of states allowed per segment or epoch */
/*                 greater than STOP time or process SPKW15 and SPKW17. */

		    if (j == 10000 || eosgmr) {

/*                    Report that we write next SPK segment. */

			tostdo_(" ", (ftnlen)1);
			tostdo_("Writing next SPK segment ...", (ftnlen)28);

/*                    Compute time step (for fixed stepped types.) If we */
/*                    have got only one state, it's zero. */

			if (jlepoc > 1) {
			    timstp = (epoch[(i__2 = jlepoc - 1) < 10000 && 0 
				    <= i__2 ? i__2 : s_rnge("epoch", i__2, 
				    "mkspk_", (ftnlen)3066)] - epoch[0]) / (
				    doublereal) (jlepoc - 1);
			} else {
			    timstp = 0.;
			}

/*                    Also, for table types if we have filled the buffer */
/*                    but didn't hit the STOP time boundary at the same */
/*                    time, we need to make sure that there is "soft and */
/*                    smooth" transition between this segment and the */
/*                    next one. As a preamble, we need to adjust segment */
/*                    coverage stop time to be PADSTS less than the last */
/*                    buffered state. */
			if (! eosgmr) {
			    etlst = epoch[(i__2 = jlepoc - padsts - 1) < 
				    10000 && 0 <= i__2 ? i__2 : s_rnge("epoch"
				    , i__2, "mkspk_", (ftnlen)3082)];
			}

/*                    We will call corresponding SPKWxx depending on */
/*                    our output SPK type. */

			if (outtvl == 5) {
			    spkw05_(&handle, &obidvl, &cnidvl, frnmvl, &etfst,
				     &etlst, sgidvl, &cngmvl, &jlepoc, state, 
				    epoch, (ftnlen)80, (ftnlen)80);
			} else if (outtvl == 8) {

/*                       Adjust descriptor end time to make sure that */
/*                       round off doesn't trigger an error in SPKW08. */

			    if (epoch[0] + (jlepoc - 1) * timstp < etlst) {
				etlst = epoch[0] + (jlepoc - 1) * timstp;
			    }
			    spkw08_(&handle, &obidvl, &cnidvl, frnmvl, &etfst,
				     &etlst, sgidvl, &pldgvl, &jlepoc, state, 
				    epoch, &timstp, (ftnlen)80, (ftnlen)80);
			} else if (outtvl == 9) {
			    spkw09_(&handle, &obidvl, &cnidvl, frnmvl, &etfst,
				     &etlst, sgidvl, &pldgvl, &jlepoc, state, 
				    epoch, (ftnlen)80, (ftnlen)80);
			} else if (outtvl == 12) {

/*                       Adjust descriptor end time to make sure that */
/*                       round off doesn't trigger an error in SPKW12. */

			    if (epoch[0] + (jlepoc - 1) * timstp < etlst) {
				etlst = epoch[0] + (jlepoc - 1) * timstp;
			    }
			    spkw12_(&handle, &obidvl, &cnidvl, frnmvl, &etfst,
				     &etlst, sgidvl, &pldgvl, &jlepoc, state, 
				    epoch, &timstp, (ftnlen)80, (ftnlen)80);
			} else if (outtvl == 13) {
			    spkw13_(&handle, &obidvl, &cnidvl, frnmvl, &etfst,
				     &etlst, sgidvl, &pldgvl, &jlepoc, state, 
				    epoch, (ftnlen)80, (ftnlen)80);
			} else if (outtvl == 15) {
			    spkw15_(&handle, &obidvl, &cnidvl, frnmvl, &etbeg,
				     &etend, sgidvl, epoch, tp, pa, &p, &e, &
				    j2flg, pv, &cngmvl, &cnj2vl, &requat, (
				    ftnlen)80, (ftnlen)80);
			} else if (outtvl == 17) {
			    spkw17_(&handle, &obidvl, &cnidvl, frnmvl, &etbeg,
				     &etend, sgidvl, epoch, eqel, &rapol, &
				    depol, (ftnlen)80, (ftnlen)80);
			}

/*                    Postambule for table types when we have filled the */
/*                    buffer but didn't hit the STOP time boundary at */
/*                    the same time, would be moving last 2*PADSTS */
/*                    states to the top of the buffer before we will */
/*                    continue processing data for the next segment. */

			if (! eosgmr) {

/*                       Reset segment coverage start time. */

			    etfst = epoch[(i__2 = jlepoc - padsts - 1) < 
				    10000 && 0 <= i__2 ? i__2 : s_rnge("epoch"
				    , i__2, "mkspk_", (ftnlen)3167)];

/*                       Delete first (JLEPOC-2*PADSTS-1) states and */
/*                       epochs from the buffers. */

			    n = (jlepoc - (padsts << 1) - 1) * 6;
			    m = jlepoc * 6;
			    remlad_(&n, &c__1, state, &m);
			    n = jlepoc - (padsts << 1) - 1;
			    m = jlepoc;
			    remlad_(&n, &c__1, epoch, &m);

/*                       Reset index of the last buffered state and */
/*                       segment coverage stop time. */

			    jlepoc = (padsts << 1) + 1;
			    j = jlepoc;
			    etlst = epoch[(i__2 = j - 1) < 10000 && 0 <= i__2 
				    ? i__2 : s_rnge("epoch", i__2, "mkspk_", (
				    ftnlen)3187)];
			}

/*                    End of the segment writing block. */

		    }

/*                 End of single complete input data record processing */
/*                 block. */

		}

/*              Check if we should continue to process the data */
/*              for next segment. */

		if (! eosgmr) {

/*                 Continue to process input data. Reset FIRST position */
/*                 to continue the parsing. */

		    i__2 = npos + 1;
		    first = ncpos_(buffer, " ", &i__2, (ftnlen)1024, (ftnlen)
			    1);

/*                 Increase counter to parse next value. */

		    ++i__;
		} else {

/*                 We should finish data process. Set FIRST equal zero */
/*                 to leave the buffer string parsing loop. */

		    first = 0;
		}

/*           End of buffer string parsing loop. */

	    }

/*           Check whether we should continue processing of the input */
/*           data for next segment. */

	    if (! eosgmr) {

/*              Yes, we should. Then reset buffer content to continue */
/*              reading input data into the buffer. */

		s_copy(buffer, bufaux, (ftnlen)1024, k);
	    } else {

/*              We should finish input data processing. Set EOF to leave */
/*              the top level loop (input data buffering loop). */

		eof = TRUE_;
	    }

/*           End of the top loop (input data buffering loop). */

	}

/*        Check if we still have some data in the data buffer and should */
/*        write the last segment. */

	if (! eosgmr) {

/*           Yes, we should write one more segment. Let's check whether */
/*           our START boundary, if we had one, was processed. If not, */
/*           it means that that START time is greater than the last */
/*           point (and only) point that we accumulated, and the segment */
/*           coverage start time (ETFST) */
/*           did not get set. Let's set it. */

	    if (statfl && staepo) {
		etfst = etbeg;
	    }

/*           Do the same for if STOP boundary was set but not processed: */
/*           set segment coverage end time. */

	    if (stotfl && stoepo) {
		etlst = etend;
	    }

/*           Next, let's check whether the data that we have in the data */
/*           buffer falls within start and stop time boundaries, if such */
/*           were defined in the setup file. This must be true for SPK */
/*           types 8, 9, 12 and 13. */

	    if (outtvl == 8 || outtvl == 9 || outtvl == 12 || outtvl == 13) {
		if (statfl && epoch[(i__2 = j - 1) < 10000 && 0 <= i__2 ? 
			i__2 : s_rnge("epoch", i__2, "mkspk_", (ftnlen)3296)] 
			< etbeg) {

/*                 Epoch of last point of input data is less */
/*                 than START time defined in setup. Complain. */

		    dafcls_(&handle);
		    delfil_(outfn, (ftnlen)255);
		    setmsg_("The epoch of the last input data file record is"
			    " smaller than the start time specified in the se"
			    "tup file keyword '#'.", (ftnlen)116);
		    errch_("#", "START_TIME", (ftnlen)1, (ftnlen)10);
		    sigerr_("SPICE(INCONSISTSTARTTIME)", (ftnlen)25);
		}
		if (stotfl && epoch[(i__2 = j - 1) < 10000 && 0 <= i__2 ? 
			i__2 : s_rnge("epoch", i__2, "mkspk_", (ftnlen)3313)] 
			< etend) {

/*                 Epoch of last point of input data is less */
/*                 than STOP time defined in setup. Complain. */

		    dafcls_(&handle);
		    delfil_(outfn, (ftnlen)255);
		    setmsg_("The epoch of the last input data file record is"
			    " smaller than the stop time specified in the set"
			    "up file keyword '#'.", (ftnlen)115);
		    errch_("#", "STOP_TIME", (ftnlen)1, (ftnlen)9);
		    sigerr_("SPICE(INCONSISTSTOPTIME)", (ftnlen)24);
		}
	    }

/*           We may need to write last segment process only for types */
/*           5, 8, 9, 12 and 13. */

	    tostdo_(" ", (ftnlen)1);
	    tostdo_("Writing last SPK segment ...", (ftnlen)28);

/*           Compute time step (for fixed stepped types.) If we have got */
/*           only one state, it's zero. */

	    if (jlepoc > 1) {
		timstp = (epoch[(i__2 = jlepoc - 1) < 10000 && 0 <= i__2 ? 
			i__2 : s_rnge("epoch", i__2, "mkspk_", (ftnlen)3344)] 
			- epoch[0]) / (doublereal) (jlepoc - 1);
	    } else {
		timstp = 0.;
	    }

/*           We will call corresponding SPKWxx depending on our output */
/*           SPK type. */

	    if (outtvl == 5) {
		spkw05_(&handle, &obidvl, &cnidvl, frnmvl, &etfst, &etlst, 
			sgidvl, &cngmvl, &jlepoc, state, epoch, (ftnlen)80, (
			ftnlen)80);
	    } else if (outtvl == 8) {

/*              Adjust descriptor end time to make sure that round off */
/*              doesn't trigger an error in SPKW08. */

		if (epoch[0] + (jlepoc - 1) * timstp < etlst) {
		    etlst = epoch[0] + (jlepoc - 1) * timstp;
		}
		spkw08_(&handle, &obidvl, &cnidvl, frnmvl, &etfst, &etlst, 
			sgidvl, &pldgvl, &jlepoc, state, epoch, &timstp, (
			ftnlen)80, (ftnlen)80);
	    } else if (outtvl == 9) {
		spkw09_(&handle, &obidvl, &cnidvl, frnmvl, &etfst, &etlst, 
			sgidvl, &pldgvl, &jlepoc, state, epoch, (ftnlen)80, (
			ftnlen)80);
	    } else if (outtvl == 12) {

/*              Adjust descriptor end time to make sure that round off */
/*              doesn't trigger an error in SPKW12. */

		if (epoch[0] + (jlepoc - 1) * timstp < etlst) {
		    etlst = epoch[0] + (jlepoc - 1) * timstp;
		}
		spkw12_(&handle, &obidvl, &cnidvl, frnmvl, &etfst, &etlst, 
			sgidvl, &pldgvl, &jlepoc, state, epoch, &timstp, (
			ftnlen)80, (ftnlen)80);
	    } else if (outtvl == 13) {
		spkw13_(&handle, &obidvl, &cnidvl, frnmvl, &etfst, &etlst, 
			sgidvl, &pldgvl, &jlepoc, state, epoch, (ftnlen)80, (
			ftnlen)80);
	    }
	}

/*        Close the input data file. */

	cl__1.cerr = 0;
	cl__1.cunit = inpunt;
	cl__1.csta = 0;
	f_clos(&cl__1);
    }

/*     Close the output SPK file. */

    spkcls_(&handle);

/*     OFF of SPICE error HANDLER. */

    tostdo_(" ", (ftnlen)1);
    tostdo_("All done.", (ftnlen)9);
    tostdo_(" ", (ftnlen)1);
    chkout_("MKSPK", (ftnlen)5);
    byebye_("SUCCESS", (ftnlen)7);
    return 0;
} /* MAIN__ */

/* Main program alias */ int mkspk_ () { MAIN__ (); return 0; }
