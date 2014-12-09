/* ckbrief.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__2 = 2;
static integer c__6 = 6;
static integer c_b243 = 999999;
static integer c_b281 = 2000000;
static integer c__0 = 0;
static doublereal c_b313 = 0.;

/* $Program    CKBRIEF ( BRIEF CK summary ) */

/* Main program */ MAIN__(void)
{
    /* System generated locals */
    address a__1[2];
    integer i__1, i__2[2], i__3, i__4, i__5, i__6, i__7;
    char ch__1[50013], ch__2[50033], ch__3[50020];

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_rnge(char *, integer, 
	    char *, integer);
    /* Subroutine */ int s_stop(char *, ftnlen), s_cat(char *, char **, 
	    integer *, integer *, ftnlen);

    /* Local variables */
    static integer avfa[1000000];
    static char arch[3], line[256];
    static logical help;
    static integer nobj, avfo[1000000];
    static logical fdsp, gdsp;
    static integer avfs[1000000];
    extern /* Subroutine */ int zzckcvr2_(integer *, integer *, integer *, 
	    doublereal *);
    static doublereal wind[2000006];
    static logical ndsp;
    extern /* Subroutine */ int zzckcvr3_(integer *, integer *, integer *, 
	    doublereal *), zzckcvr4_(integer *, integer *, integer *, 
	    doublereal *), zzckcvr5_(integer *, integer *, integer *, 
	    doublereal *);
    static logical tdsp;
    static char type__[3], text[256*47], strn[256];
    static logical vrsn;
    static char tout[256], pass1[50000], pass2[50000], pass3[50000];
    static integer i__, k, l;
    extern integer cardd_(doublereal *);
    extern /* Subroutine */ int kdata_(integer *, char *, char *, char *, 
	    char *, integer *, logical *, ftnlen, ftnlen, ftnlen, ftnlen), 
	    dafgs_(doublereal *);
    static logical doall, obnam;
    static doublereal tenda[1000000];
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern logical beint_(char *, ftnlen);
    static logical sfile;
    static integer nobgo;
    extern /* Subroutine */ int rdnbl_(char *, char *, logical *, ftnlen, 
	    ftnlen), repmc_(char *, char *, char *, char *, ftnlen, ftnlen, 
	    ftnlen, ftnlen), errch_(char *, char *, ftnlen, ftnlen);
    static integer nobgs;
    static doublereal tendo[1000000], tempe[1000000];
    static integer value;
    static doublereal tends[1000000];
    static logical idump, found;
    extern /* Subroutine */ int dafus_(doublereal *, integer *, integer *, 
	    doublereal *, integer *);
    static integer typea[1000000];
    static char hword[256];
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen), movei_(integer *, integer *, integer *), 
	    moved_(doublereal *, integer *, doublereal *);
    static doublereal temps[1000000];
    static char error[256];
    extern integer rtrim_(char *, ftnlen);
    static integer start;
    static char utext[256*21];
    static doublereal dc[2];
    static integer ic[6], baddra[1000000];
    extern /* Subroutine */ int daffna_(logical *);
    static integer eaddra[1000000];
    extern /* Subroutine */ int dafbfs_(integer *);
    static integer handle, framea[1000000];
    extern /* Subroutine */ int dafcls_(integer *), scardd_(integer *, 
	    doublereal *);
    static logical havfil;
    static integer frameo[1000000];
    extern /* Subroutine */ int getcml_(char *, ftnlen);
    static integer frames[1000000], findex;
    extern /* Subroutine */ int inslai_(integer *, integer *, integer *, 
	    integer *, integer *);
    extern integer isrchi_(integer *, integer *, integer *);
    static char srcfil[256];
    static integer iorder[1000000], objlis[100];
    static logical fulbuf, nomerg, fndsum;
    static integer numobj;
    static logical mixdav, mixdfr;
    static integer fcount;
    static doublereal segsum[5];
    static char filtyp[256], source[256];
    extern /* Subroutine */ int erract_(char *, char *, ftnlen, ftnlen), 
	    tostdo_(char *, ftnlen), fndnwd_(char *, integer *, integer *, 
	    integer *, ftnlen), suffix_(char *, integer *, char *, ftnlen, 
	    ftnlen), nparsi_(char *, integer *, char *, integer *, ftnlen, 
	    ftnlen), getfat_(char *, char *, char *, ftnlen, ftnlen, ftnlen);
    static doublereal tstrta[1000000];
    extern /* Subroutine */ int setmsg_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), furnsh_(char *, ftnlen), ktotal_(char *, integer *, 
	    ftnlen), tkvrsn_(char *, char *, ftnlen, ftnlen), dafopr_(char *, 
	    integer *, ftnlen), errint_(char *, integer *, ftnlen), ssized_(
	    integer *, doublereal *), dispsm_(integer *, integer *, 
	    doublereal *, doublereal *, integer *, integer *, char *, logical 
	    *, logical *, logical *, logical *, ftnlen), wnvald_(integer *, 
	    integer *, doublereal *), orderd_(doublereal *, integer *, 
	    integer *), reordd_(integer *, integer *, doublereal *);
    static doublereal tstrto[1000000];
    extern /* Subroutine */ int reordi_(integer *, integer *, integer *), 
	    chkout_(char *, ftnlen), byebye_(char *, ftnlen);
    static doublereal tstrts[1000000];
    static integer beg, ida[1000000], end;
    static logical fnd, eof;
    static integer ido[1000000], loc, ids[1000000], cnt, ptr;
    extern /* Subroutine */ int zzckcv06_(integer *, integer *, integer *, 
	    integer *, doublereal *, doublereal *, char *, doublereal *, 
	    ftnlen);


/* $ Abstract */

/*     CKBRIEF is a utility program that provides brief summaries of */
/*     the contents of one or more CK files. */

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

/*     CKBRIEF User's Guide. */

/* $ Keywords */

/*     FILES */
/*     UTILITY */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     1) All summary tasks performed require name for at least one */
/*        CK file to be provided. */

/*     2) To display time tags in any format (except DP SCLK), LSK */
/*        and appropriate SCLK files must be provided. */

/* $ Particulars */

/*     For usage details see User's Guide. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     Y.K. Zaiko     (BERC) */
/*     B.V. Semenov   (NAIF) */
/*     F.S. Turner    (JPL) */

/* $ Version */

/* -    Toolkit Version 6.1.0, 27-JUN-2014 (BVS) */

/*        BUG FIX: changed logic to make a combination of -a and an ID */
/*        specified on the command line work in all cases. */

/* -    Toolkit Version 6.0.0, 28-APR-2014 (BVS) (NJB) */

/*        Modified to treat all files as a single file (-a). */

/*        Changed SCLKD display format to include 6 decimal */
/*        places. */

/*        Increased MAXBOD to 1,000,000 (from 100,000) and CMDSIZ to */
/*        50,000 (from 25,000). */

/*        Added support for CK type 6. */

/* -    Toolkit Version 5.0.0, 11-FEB-2009 (BVS) */

/*        Modified to display names of the frames associated with CK */
/*        IDs (new option -n). */

/*        Updated help display. */

/*        Bug fix: hanging up when no value was provided after -f. */

/*        Bug fix: replaced warning message and incomplete summary in */
/*        the case of insufficient buffer space (FULBUF) with an error */
/*        message. */

/* -    Toolkit Version 4.0.0, 13-JAN-2008 (BVS) */

/*        Bug fix: GDSP is now initialized to .FALSE. Uninitialized GDSP */
/*        caused spontaneous, unsolicited grouping displays on some C */
/*        environments. */

/*        Updated to summarize CKs provided in meta-kernels. Updated to */
/*        recognize and complain about kernels that should be */
/*        summarized other utility programs. Updated to display shorter */
/*        usage message when ran with a blank command line. */

/*        Changed format of the displayed version string. */

/*        Added CHKIN/CHKOUT and set ERRACT to ABORT (which removed */
/*        "Oh, by the way ..." from the errors). */

/*        Increased MAXBOD to 100,000 (from 10,000) and CMDSIZ to */
/*        25,000 (from 4,000) */

/*        SAVEed all variables. */

/* -    Toolkit Version 3.2.0, 02-NOV-2006 (BVS) */

/*        Replaced LDPOOL with FURNSH. */

/* -    Toolkit Version 3.1.0, 08-NOV-2005 (BVS) */

/*        Updated usage display to describe what kind of summary */
/*        is generated by default. */

/* -    Toolkit Version 3.0.0, 28-AUG-2002 (FST) */

/*        Added support for the -dump option for type 4 and 5 CK */
/*        segments. */

/*        Modified ZZCKCVR3 calling sequence to agree with changes */
/*        as delivered to support. */

/* -    Toolkit Version 2.0.0, 15-MAY-2001 (BVS) */

/*        Added option to dump interpolation interval information */
/*        from type 2 & 3 CK segments. */

/* -    Toolkit Version 1.1.2, 09-APR-2001 (BVS) */

/*        Fixed usage display and User's Guide. */

/* -    Toolkit Version 1.1.1, 03-NOV-2000 (EDW) */

/*        Added a BYEBYE( 'SUCCESS' ) call at program's end. */

/* -    Toolkit Version 1.1.0, 14-OCT-1999 (WLT) */

/*        Commented out EXTERNALS in source code.  See note in the */
/*        code for details. */

/* -    Toolkit Version 1.0.0, 26-Mar-1999 (WLT) */

/*        Added code to skip attempting to load binary kernels into */
/*        the kernel pool. */

/* -    Beta Version 1.0.0,  24-MAR-1999  ( YKZ ) ( BVS ) */

/*        See CKBRIEF.INC for version information. */

/* -& */

/*     User adjustable parameters. */


/*     Passed Functions */

/*     The lines below have been commented out as they do not appear */
/*     to be needed for anything.  BIGGER and INC are not referenced */
/*     anywhere else in this file.  (WLT) 14-OCT-1999 */

/*      EXTERNAL              BIGGER */
/*      EXTERNAL              INC */

/*     SPICELIB Functions */

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

/*     Y.K. Zaiko     (BERC) */
/*     B.V. Semenov   (NAIF) */

/* $ Version */

/* -    Toolkit Version 6.1.0, 27-JUN-2014 (BVS) */

/*        BUG FIX: changed logic to make a combination of -a and an ID */
/*        specified on the command line work in all cases. */

/* -    CKBRIEF Version 6.0.0, 2014-04-28 (BVS) (NJB) */

/*        Modified to treat all files as a single file (-a). */

/*        Changed SCLKD display format to include 6 decimal */
/*        places. */

/*        Increased MAXBOD to 1,000,000 (from 100,000) and CMDSIZ to */
/*        50,000 (from 25,000). */

/*        Added support for CK type 6. */

/* -    CKBRIEF Version 5.0.0, 2009-02-11 (BVS) */

/*        Updated version. */

/* -    CKBRIEF Version 4.0.0, 2008-01-13 (BVS) */

/*        Increased MAXBOD to 100,000 (from 10,000). */

/*        Increased CMDSIZ to 25,000 (from 4,000). */

/*        Updated version string and changed its format to */
/*        '#.#.#, Month DD, YYYY' (from '#.#.#, YYYY-MM-DD'). */

/* -    CKBRIEF Version 3.2.0, 2006-11-02 (BVS) */

/*        Updated version string. */

/* -    CKBRIEF Version 3.1.0, 2005-11-08 (BVS) */

/*        Updated version string. */

/* -    CKBRIEF Version 2.0.0, 2001-05-16 (BVS) */

/*        Increased MAXBOD to 10000 (from 4000). Set LRGWIN to be */
/*        MAXBOD*2 (was MAXBOD). Changed version string. */

/* -    CKBRIEF Version 1.1.2, 2001-04-09 (BVS) */

/*        Changed version parameter. */

/* -    CKBRIEF Version 1.0.0 beta, 1999-02-17 (YKZ)(BVS) */

/*        Initial release. */

/* -& */

/*     The Version is stored as a string. */


/*     The maximum number of segments or interpolation intervals */
/*     that can be summarized is stored in the parameter MAXBOD. */
/*     This is THE LIMIT that should be increased if window */
/*     routines called by CKBRIEF fail. */


/*     The largest expected window -- must be twice the size of */
/*     MAXBOD for consistency. */


/*     The longest command line that can be accommodated is */
/*     given by CMDSIZ. */


/*     MAXUSE is the maximum number of objects that can be explicitly */
/*     specified on the command line for ckbrief summaries. */


/*     Generic line size for all modules. */


/*     Time type keys. */


/*     Output time format pictures. */


/*     Local parameters */


/*     Command line options as parameters. */


/*     Lower cell size. */


/*     CK DAF summary component counts. */


/*     SPICE Kernel types as recognized by GETFAT. */


/*     Usage and help message sizes. */


/*     Local variables. */


/*     Help message holder. */


/*     Usage message holder. */


/*     Ancillary line and file names holder line. */


/*     Command line and parsing variables. */


/*     Parsed Control Values. */


/*     Internal flags explanation: FNDSUM is .TRUE. if we have */
/*     something found to display in CK-file we are considering. */


/*     DAF Variables */


/*     Arrays to store segment descriptors data for an objects with */
/*     a single intervals of coverage in a given CK-file */


/*     Arrays to store all segment descriptors data for a given CK-file */


/*     Arrays to store all segment descriptors data for one object in */
/*     a given CK-file */


/*     Utility Variables */


/*     Variable for dealing with FURNSH'ed CKs. */


/*     Save all variables. */


/*     SPICELIB error handling. */

    chkin_("CKBRIEF", (ftnlen)7);
    erract_("SET", "ABORT", (ftnlen)3, (ftnlen)5);

/*     The first thing we do is to display version. */

    s_copy(line, "CKBRIEF -- Version # -- Toolkit Version #", (ftnlen)256, (
	    ftnlen)41);
    tostdo_(" ", (ftnlen)1);
    repmc_(line, "#", "6.1.0, June 27, 2014", line, (ftnlen)256, (ftnlen)1, (
	    ftnlen)20, (ftnlen)256);
    tkvrsn_("TOOLKIT", hword, (ftnlen)7, (ftnlen)256);
    repmc_(line, "#", hword, line, (ftnlen)256, (ftnlen)1, (ftnlen)256, (
	    ftnlen)256);
    tostdo_(line, (ftnlen)256);

/*     Set up the usage text. */

    s_copy(utext, "   CKBRIEF is a command-line utility program that display"
	    "s a summary for", (ftnlen)256, (ftnlen)72);
    s_copy(utext + 256, "   one or more binary CK files. The program usage i"
	    "s:", (ftnlen)256, (ftnlen)53);
    s_copy(utext + 512, " ", (ftnlen)256, (ftnlen)1);
    s_copy(utext + 768, "      % ckbrief [-options] file [file ...]", (ftnlen)
	    256, (ftnlen)42);
    s_copy(utext + 1024, " ", (ftnlen)256, (ftnlen)1);
    s_copy(utext + 1280, "   The most useful options are shown below. For th"
	    "e complete set of", (ftnlen)256, (ftnlen)67);
    s_copy(utext + 1536, "   options, run CKBRIEF with the -h option. The or"
	    "der of options is not", (ftnlen)256, (ftnlen)71);
    s_copy(utext + 1792, "   significant. The option keys must be lowercase "
	    "as shown below.", (ftnlen)256, (ftnlen)65);
    s_copy(utext + 2048, " ", (ftnlen)256, (ftnlen)1);
    s_copy(utext + 2304, "      -dump        display interpolation intervals",
	     (ftnlen)256, (ftnlen)50);
    s_copy(utext + 2560, "      -rel         display relative-to frames (may"
	    " need FK)", (ftnlen)256, (ftnlen)59);
    s_copy(utext + 2816, "      -n           display frames associated with "
	    "CK IDs (may need FK)", (ftnlen)256, (ftnlen)70);
    s_copy(utext + 3072, "      -t           display summary in a tabular fo"
	    "rmat", (ftnlen)256, (ftnlen)54);
    s_copy(utext + 3328, "      -a           treat all files as a single file"
	    , (ftnlen)256, (ftnlen)51);
    s_copy(utext + 3584, "      -utc         display times in UTC calendar d"
	    "ate format (needs LSK&SCLK)", (ftnlen)256, (ftnlen)77);
    s_copy(utext + 3840, "      -utcdoy      display times in UTC day-of-yea"
	    "r format (needs LSK&SCLK)", (ftnlen)256, (ftnlen)75);
    s_copy(utext + 4096, "      -sclk        display times as SCLK strings ("
	    "needs SCLK)", (ftnlen)256, (ftnlen)61);
    s_copy(utext + 4352, " ", (ftnlen)256, (ftnlen)1);
    s_copy(utext + 4608, "   LSK and SCLK files must be provided on the comm"
	    "and line to display times", (ftnlen)256, (ftnlen)75);
    s_copy(utext + 4864, "   in UTC, ET, or SCLK formats. FK file(s) must be"
	    " provided on the command", (ftnlen)256, (ftnlen)74);
    s_copy(utext + 5120, "   line to display names of any frames that are no"
	    "t built into the Toolkit.", (ftnlen)256, (ftnlen)75);

/*     Set up the help text. */

    s_copy(text, "   CKBRIEF is a command line program that displays a summa"
	    "ry of the", (ftnlen)256, (ftnlen)67);
    s_copy(text + 256, "   contents and time coverage for one or more binary"
	    " CK files. The program", (ftnlen)256, (ftnlen)74);
    s_copy(text + 512, "   displays a summary for each CK file listed on the"
	    " command line, in a", (ftnlen)256, (ftnlen)71);
    s_copy(text + 768, "   list file, and/or a meta-kernel. It can display c"
	    "overage boundaries as", (ftnlen)256, (ftnlen)73);
    s_copy(text + 1024, "   ephemeris time (ET) in calendar format, as UTC t"
	    "ime in", (ftnlen)256, (ftnlen)57);
    s_copy(text + 1280, "   ``year-month-day'' or ``day-of-year'' format, or"
	    " as on-board clock", (ftnlen)256, (ftnlen)69);
    s_copy(text + 1536, "   (SCLK) time in string or encoded format. It can "
	    "display the summary in a", (ftnlen)256, (ftnlen)75);
    s_copy(text + 1792, "   variety of formats, with or without showing the "
	    "names of the frames", (ftnlen)256, (ftnlen)70);
    s_copy(text + 2048, "   associated with spacecraft or structure IDs or t"
	    "he names/IDs of the", (ftnlen)256, (ftnlen)70);
    s_copy(text + 2304, "   frames with respect to which orientation is prov"
	    "ided.", (ftnlen)256, (ftnlen)56);
    s_copy(text + 2560, " ", (ftnlen)256, (ftnlen)1);
    s_copy(text + 2816, "   The program usage is:", (ftnlen)256, (ftnlen)24);
    s_copy(text + 3072, " ", (ftnlen)256, (ftnlen)1);
    s_copy(text + 3328, "      ckbrief [-options] file [file ...]", (ftnlen)
	    256, (ftnlen)40);
    s_copy(text + 3584, " ", (ftnlen)256, (ftnlen)1);
    s_copy(text + 3840, "   where [file]s are binary CK files and text kerne"
	    "ls needed to support", (ftnlen)256, (ftnlen)71);
    s_copy(text + 4096, "   time conversion (LSK and SCLKs), or containing f"
	    "rame definitions (FKs),", (ftnlen)256, (ftnlen)74);
    s_copy(text + 4352, "   provided in any order. The options are:", (ftnlen)
	    256, (ftnlen)42);
    s_copy(text + 4608, " ", (ftnlen)256, (ftnlen)1);
    s_copy(text + 4864, "      -dump        display interpolation intervals "
	    "boundaries", (ftnlen)256, (ftnlen)61);
    s_copy(text + 5120, "      -nm          display segment boundaries", (
	    ftnlen)256, (ftnlen)45);
    s_copy(text + 5376, " ", (ftnlen)256, (ftnlen)1);
    s_copy(text + 5632, "      -rel         display relative-to frames", (
	    ftnlen)256, (ftnlen)45);
    s_copy(text + 5888, "      -n           display frames associated with s"
	    "tructure IDs", (ftnlen)256, (ftnlen)63);
    s_copy(text + 6144, " ", (ftnlen)256, (ftnlen)1);
    s_copy(text + 6400, "      -t           display summary in a tabular for"
	    "mat", (ftnlen)256, (ftnlen)54);
    s_copy(text + 6656, "      -a           treat all files as a single file",
	     (ftnlen)256, (ftnlen)51);
    s_copy(text + 6912, "      -g           display summary grouped by cover"
	    "age", (ftnlen)256, (ftnlen)54);
    s_copy(text + 7168, " ", (ftnlen)256, (ftnlen)1);
    s_copy(text + 7424, "      -utc         display times in UTC calendar da"
	    "te format", (ftnlen)256, (ftnlen)60);
    s_copy(text + 7680, "      -utcdoy      display times in UTC day-of-year"
	    " format", (ftnlen)256, (ftnlen)58);
    s_copy(text + 7936, "      -sclk        display times as SCLK strings", (
	    ftnlen)256, (ftnlen)48);
    s_copy(text + 8192, "      -dpsclk      display times as SCLK ticks", (
	    ftnlen)256, (ftnlen)46);
    s_copy(text + 8448, " ", (ftnlen)256, (ftnlen)1);
    s_copy(text + 8704, "      [ID]         display summary for structure wi"
	    "th [ID]", (ftnlen)256, (ftnlen)58);
    s_copy(text + 8960, " ", (ftnlen)256, (ftnlen)1);
    s_copy(text + 9216, "      -f [list]    summarize kernels listed in the "
	    "[list] file", (ftnlen)256, (ftnlen)62);
    s_copy(text + 9472, " ", (ftnlen)256, (ftnlen)1);
    s_copy(text + 9728, "      -h           display help", (ftnlen)256, (
	    ftnlen)31);
    s_copy(text + 9984, "      -v           display version", (ftnlen)256, (
	    ftnlen)34);
    s_copy(text + 10240, " ", (ftnlen)256, (ftnlen)1);
    s_copy(text + 10496, "   The options can be provided in any order and ca"
	    "n appear before, after,", (ftnlen)256, (ftnlen)73);
    s_copy(text + 10752, "   or intermixed with file names. The case of opti"
	    "on keys is significant:", (ftnlen)256, (ftnlen)73);
    s_copy(text + 11008, "   they must be lowercase as shown above. While an"
	    "y option can be provided", (ftnlen)256, (ftnlen)74);
    s_copy(text + 11264, "   together with any other option or a combination"
	    " of options, some options", (ftnlen)256, (ftnlen)75);
    s_copy(text + 11520, "   will override others.", (ftnlen)256, (ftnlen)24);
    s_copy(text + 11776, " ", (ftnlen)256, (ftnlen)1);

/*     Next step is to parse the command line. This is done in two */
/*     passes. The first pass determines the options specified. The */
/*     second pass gets file names. */

/*     Pass one.  The defaults are: */

    obnam = FALSE_;
    tdsp = FALSE_;
    gdsp = FALSE_;
    help = FALSE_;
    vrsn = FALSE_;
    havfil = FALSE_;
    fdsp = FALSE_;
    ndsp = FALSE_;
    sfile = FALSE_;
    nomerg = FALSE_;
    idump = FALSE_;
    doall = FALSE_;
    s_copy(tout, "ET", (ftnlen)256, (ftnlen)2);

/*     Reset ancillary strings that will be used in parsing of the */
/*     command line. */

    s_copy(pass1, " ", (ftnlen)50000, (ftnlen)1);
    s_copy(pass2, " ", (ftnlen)50000, (ftnlen)1);
    s_copy(pass3, " ", (ftnlen)50000, (ftnlen)1);

/*     Get command line. */

    getcml_(pass1, (ftnlen)50000);

/*     If command line is blank display brief usage. */

    if (s_cmp(pass1, " ", (ftnlen)50000, (ftnlen)1) == 0) {
	tostdo_(" ", (ftnlen)1);
	for (i__ = 1; i__ <= 21; ++i__) {
	    tostdo_(utext + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : 
		    s_rnge("utext", i__1, "ckbrief_", (ftnlen)618)) << 8), (
		    ftnlen)256);
	}
	tostdo_(" ", (ftnlen)1);
	s_stop("", (ftnlen)0);
    }

/*     Now locate the first word in the command line. */

    numobj = 0;
    start = 1;
    fndnwd_(pass1, &start, &beg, &end, (ftnlen)50000);
    while(beg > 0) {

/*        All options begin with a '-'. */

	if (*(unsigned char *)&pass1[beg - 1] != '-') {

/*           This item doesn't have dash as the first character, so */
/*           it is presumed to be a file name which we will append */
/*           to the string containing only file names. */

	    suffix_(pass1 + (beg - 1), &c__1, pass2, end - (beg - 1), (ftnlen)
		    50000);
	    havfil = TRUE_;
	} else if (s_cmp(pass1 + (beg - 1), "-t", end - (beg - 1), (ftnlen)2) 
		== 0) {

/*           Display summary in tabular format. */

	    tdsp = TRUE_;
	} else if (s_cmp(pass1 + (beg - 1), "-g", end - (beg - 1), (ftnlen)2) 
		== 0) {

/*           Group summaries for objects with the same coverage */

	    gdsp = TRUE_;
	} else if (s_cmp(pass1 + (beg - 1), "-rel", end - (beg - 1), (ftnlen)
		4) == 0) {

/*           Display names of base frames */

	    fdsp = TRUE_;
	} else if (s_cmp(pass1 + (beg - 1), "-n", end - (beg - 1), (ftnlen)2) 
		== 0) {

/*           Display names of reference frames associated with CK IDs. */

	    ndsp = TRUE_;
	} else if (s_cmp(pass1 + (beg - 1), "-h", end - (beg - 1), (ftnlen)2) 
		== 0) {

/*           Print the help. */

	    help = TRUE_;
	} else if (s_cmp(pass1 + (beg - 1), "-v", end - (beg - 1), (ftnlen)2) 
		== 0) {

/*           Print the version. */

	    vrsn = TRUE_;
	} else if (s_cmp(pass1 + (beg - 1), "-f", end - (beg - 1), (ftnlen)2) 
		== 0) {

/*           We have got a file containing names of the CK, LSK */
/*           or/and SCLK files. */

	    start = end + 1;
	    fndnwd_(pass1, &start, &beg, &end, (ftnlen)50000);
	    if (beg > 0) {
		sfile = TRUE_;
		s_copy(srcfil, pass1 + (beg - 1), (ftnlen)256, end - (beg - 1)
			);
	    } else {
		end = start - 1;
	    }
	} else if (s_cmp(pass1 + (beg - 1), "-utc", end - (beg - 1), (ftnlen)
		4) == 0) {

/*           Display times as UTC times. */

	    s_copy(tout, "UTC", (ftnlen)256, (ftnlen)3);
	} else if (s_cmp(pass1 + (beg - 1), "-sclk", end - (beg - 1), (ftnlen)
		5) == 0) {

/*           Display times as string SCLK times. */

	    s_copy(tout, "SCLK", (ftnlen)256, (ftnlen)4);
	} else if (s_cmp(pass1 + (beg - 1), "-dpsclk", end - (beg - 1), (
		ftnlen)7) == 0) {

/*           Display times as DP SCLK or SCLK ticks. */

	    s_copy(tout, "TICKS", (ftnlen)256, (ftnlen)5);
	} else if (s_cmp(pass1 + (beg - 1), "-utcdoy", end - (beg - 1), (
		ftnlen)7) == 0) {

/*           Display times as UTC/DOY times. */

	    s_copy(tout, "UTC/DOY", (ftnlen)256, (ftnlen)7);
	} else if (s_cmp(pass1 + (beg - 1), "-nm", end - (beg - 1), (ftnlen)3)
		 == 0) {

/*           Set "no-merge" flag. */

	    nomerg = TRUE_;
	} else if (s_cmp(pass1 + (beg - 1), "-dump", end - (beg - 1), (ftnlen)
		5) == 0) {

/*           Set "dump-intervals" flag. */

	    idump = TRUE_;
	} else if (s_cmp(pass1 + (beg - 1), "-a", end - (beg - 1), (ftnlen)2) 
		== 0) {

/*           Set "treat-all-as-one" flag. */

	    doall = TRUE_;
	} else {

/*           Items that begin with '-' that are not one of the previous */
/*           cases are regarded as a request to summarize only for */
/*           particular items. This item must an integer, so we try to */
/*           parse it as integer. If we were successful, we add to the */
/*           integer array containing IDs "to restrict relative to" :) */

	    if (beint_(pass1 + (beg - 1), end - (beg - 1))) {
		nparsi_(pass1 + (beg - 1), &value, error, &ptr, end - (beg - 
			1), (ftnlen)256);

/*              INSLAI is a great routine but it's our responsibility */
/*              to check whether our ID will fit into OBJLIS array. */

		if (numobj < 100) {

/*                 This ID will fit into our OBJLIS array, lets */
/*                 insert it there. */

		    i__1 = numobj + 1;
		    inslai_(&value, &c__1, &i__1, objlis, &numobj);
		} else {

/*                 There is no space for this ID in our array. Lets */
/*                 report about it. */

		    tostdo_(" Object number ", (ftnlen)15);
		    tostdo_(pass1 + (beg - 1), end - (beg - 1));
		    tostdo_(" doesn't fit into the buffer", (ftnlen)28);
		    tostdo_(" which can hold no more than", (ftnlen)28);
		    tostdo_(" MAXUSE elements.", (ftnlen)17);
		}
	    } else {

/*              Unrecognized option specified. */

		tostdo_(" ", (ftnlen)1);
		s_copy(hword, "'#' is not a recognized option.", (ftnlen)256, 
			(ftnlen)31);
		repmc_(hword, "#", pass1 + (beg - 1), hword, (ftnlen)256, (
			ftnlen)1, end - (beg - 1), (ftnlen)256);
		tostdo_(hword, (ftnlen)256);
		tostdo_(" ", (ftnlen)1);
		help = TRUE_;
	    }
	}

/*        Locate the next word. */

	start = end + 1;
	fndnwd_(pass1, &start, &beg, &end, (ftnlen)50000);
    }

/*     Handle any exceptions that might have been occurred. First, */
/*     display version if version key was the only one on the */
/*     command line. */

    if (vrsn && ! havfil && ! sfile && ! help) {
	tostdo_(" ", (ftnlen)1);
	s_stop("", (ftnlen)0);
    }

/*     Display help only if it was requested or if we didn't get any */
/*     file names on the command line and we didn't get the name of a */
/*     file containing these names. */

    help = help || ! (havfil || sfile);
    if (help) {
	tostdo_(" ", (ftnlen)1);
	for (i__ = 1; i__ <= 47; ++i__) {
	    tostdo_(text + (((i__1 = i__ - 1) < 47 && 0 <= i__1 ? i__1 : 
		    s_rnge("text", i__1, "ckbrief_", (ftnlen)827)) << 8), (
		    ftnlen)256);
	}
	tostdo_(" ", (ftnlen)1);
	s_stop("", (ftnlen)0);
    }

/*     Reset contradicting flags. If interval dump was requested we */
/*     cannot do grouping (-g), filtering by object ID ([obj]), */
/*     displaying of segment boundaries (-nm), or treating all as one */
/*     (-a). */

    if (idump) {
	nomerg = FALSE_;
	gdsp = FALSE_;
	doall = FALSE_;
	numobj = 0;
    }

/*     Now finish up determining what the user wants us to do. It is */
/*     necessary to process string PASS2 and file SRCFIL, if it was */
/*     provided, to check files the names of which provided and */
/*     load/process these files correspondingly. */

/*     User can provide 3 types of input files: */

/*      - LSK file(s); */
/*      - SCLK file(s); */
/*      - binary CK file(s). */

/*     First, let check if we have a file name provided using -f */
/*     command line flag */

    if (sfile) {
	eof = FALSE_;
	while(! eof) {
	    rdnbl_(srcfil, strn, &eof, (ftnlen)256, (ftnlen)256);
	    if (! eof) {

/*              We need to check whether we have enough space in our */
/*              string to append another file name to it. If we do, */
/*              we append it. Other wise we report about it. */

		if (rtrim_(strn, (ftnlen)256) + rtrim_(pass2, (ftnlen)50000) <
			 50000) {
		    suffix_(strn, &c__1, pass2, (ftnlen)256, (ftnlen)50000);
		    havfil = TRUE_;
		} else {
		    s_copy(hword, "No space in the file name buffer for the "
			    "name '#' obtained from the input file '#'", (
			    ftnlen)256, (ftnlen)82);
		    repmc_(hword, "#", strn, hword, (ftnlen)256, (ftnlen)1, (
			    ftnlen)256, (ftnlen)256);
		    repmc_(hword, "#", srcfil, hword, (ftnlen)256, (ftnlen)1, 
			    (ftnlen)256, (ftnlen)256);
		    tostdo_(hword, (ftnlen)256);
		}
	    }
	}
    }

/*     Start processing of the file names in string PASS2. */

    if (havfil) {
	start = 1;
	fndnwd_(pass2, &start, &beg, &end, (ftnlen)50000);
	while(beg > 0) {
	    getfat_(pass2 + (beg - 1), arch, type__, end - (beg - 1), (ftnlen)
		    3, (ftnlen)3);
	    if (s_cmp(type__, "CK", (ftnlen)3, (ftnlen)2) == 0 && s_cmp(arch, 
		    "DAF", (ftnlen)3, (ftnlen)3) == 0) {

/*              We don't need to check whether we have enough space */
/*              in the PASS3 because PASS2 and PASS3 are of the same */
/*              size. */

		suffix_(pass2 + (beg - 1), &c__1, pass3, end - (beg - 1), (
			ftnlen)50000);
	    } else if (s_cmp(arch, "DAF", (ftnlen)3, (ftnlen)3) == 0 && s_cmp(
		    type__, "SPK", (ftnlen)3, (ftnlen)3) == 0) {

/*              Signal an error, reporting that this is a SPK file. */

		setmsg_("Kernel '#' is a binary SPK and, therefore, cannot b"
			"e summarized using CKBRIEF. Use BRIEF or SPACIT to s"
			"ummarize this file.", (ftnlen)122);
		errch_("#", pass2 + (beg - 1), (ftnlen)1, end - (beg - 1));
		sigerr_("SPICE(SPKFILE)", (ftnlen)14);
	    } else if (s_cmp(arch, "DAF", (ftnlen)3, (ftnlen)3) == 0 && s_cmp(
		    type__, "PCK", (ftnlen)3, (ftnlen)3) == 0) {

/*              Signal an error, reporting that this is a PCK file. */

		setmsg_("Kernel '#' is a binary PCK and, therefore, cannot b"
			"e summarized using CKBRIEF. Use BRIEF or SPACIT to s"
			"ummarize this file.", (ftnlen)122);
		errch_("#", pass2 + (beg - 1), (ftnlen)1, end - (beg - 1));
		sigerr_("SPICE(PCKFILE)", (ftnlen)14);
	    } else if (s_cmp(arch, "DAS", (ftnlen)3, (ftnlen)3) == 0 && s_cmp(
		    type__, "EK", (ftnlen)3, (ftnlen)2) == 0) {

/*              Signal an error, reporting that this is an EK file. */

		setmsg_("Kernel '#' is a binary EK and, therefore, cannot be"
			" summarized using BRIEF. Use SPACIT or INSPEKT to su"
			"mmarize this file.", (ftnlen)121);
		errch_("#", pass2 + (beg - 1), (ftnlen)1, end - (beg - 1));
		sigerr_("SPICE(EKFILE)", (ftnlen)13);
	    } else if (s_cmp(arch, "XFR", (ftnlen)3, (ftnlen)3) == 0 || s_cmp(
		    arch, "DEC", (ftnlen)3, (ftnlen)3) == 0) {

/*              Signal an error, reporting that this is a transfer file. */

		setmsg_("Kernel '#' is in transfer format. It must be conver"
			"ted to binary format to be summarized using CKBRIEF.",
			 (ftnlen)103);
		errch_("#", pass2 + (beg - 1), (ftnlen)1, end - (beg - 1));
		sigerr_("SPICE(TRANSFERFORMAT)", (ftnlen)21);
	    } else if (s_cmp(arch, "DAS", (ftnlen)3, (ftnlen)3) == 0 || s_cmp(
		    arch, "DAF", (ftnlen)3, (ftnlen)3) == 0 || s_cmp(arch, 
		    "TE1", (ftnlen)3, (ftnlen)3) == 0) {

/*              This is a ``catch all'' branch for all other kernels */
/*              that are not binary CKs. Signal an error, reporting that */
/*              this is a file with a wrong architecture and type. */

		setmsg_("Kernel '#' is not a binary CK and, therefore, canno"
			"t be summarized using CKBRIEF. It is a '#/#' file.", (
			ftnlen)101);
		errch_("#", pass2 + (beg - 1), (ftnlen)1, end - (beg - 1));
		errch_("#", arch, (ftnlen)1, (ftnlen)3);
		errch_("#", type__, (ftnlen)1, (ftnlen)3);
		sigerr_("SPICE(BADARCHITECTURE)", (ftnlen)22);
	    } else {

/*              This is probably a text kernel. To simplify our */
/*              lives we load this file into the kernel pool without */
/*              checking of what type this file is. If it's LSK or */
/*              SCLK, it's going to be processed just fine. If it's */
/*              something else, kernel pool subsystem won't find any */
/*              data in it ... it's just going to take a long time to */
/*              load if somebody provided the name of 20MB file :). */

		furnsh_(pass2 + (beg - 1), end - (beg - 1));
	    }
	    start = end + 1;
	    fndnwd_(pass2, &start, &beg, &end, (ftnlen)50000);
	}
    }

/*     See if any CKs were loaded via meta-kernels. If so, add them */
/*     to the list of CK files to be summarized. */

    ktotal_("CK", &fcount, (ftnlen)2);
    if (fcount != 0) {
	i__1 = fcount;
	for (findex = 1; findex <= i__1; ++findex) {
	    kdata_(&findex, "CK", strn, filtyp, source, &handle, &found, (
		    ftnlen)2, (ftnlen)256, (ftnlen)256, (ftnlen)256);
	    if (found) {

/*              We need to check whether we have enough space in our */
/*              string to append another file name to it. If we do, */
/*              we append it. Otherwise we report about it. */

		if (rtrim_(strn, (ftnlen)256) + rtrim_(pass3, (ftnlen)50000) <
			 50000) {
		    suffix_(strn, &c__1, pass3, (ftnlen)256, (ftnlen)50000);
		} else {
		    s_copy(hword, "No space in the file name buffer for the "
			    "name '#' obtained from the meta-kernel(s).", (
			    ftnlen)256, (ftnlen)83);
		    repmc_(hword, "#", strn, hword, (ftnlen)256, (ftnlen)1, (
			    ftnlen)256, (ftnlen)256);
		    tostdo_(hword, (ftnlen)256);
		}
	    }
	}
    }

/*     Check whether at least one CK file was provided. If not, */
/*     complain about it. */

    if (s_cmp(pass3, " ", (ftnlen)50000, (ftnlen)1) == 0) {
	tostdo_("No CK files were provided -- no summary will be displayed.", 
		(ftnlen)58);
	tostdo_(" ", (ftnlen)1);
	tostdo_("Run CKBRIEF without command line options to see program usa"
		"ge.", (ftnlen)62);
	tostdo_(" ", (ftnlen)1);
	s_stop("", (ftnlen)0);
    }

/*     Below is the main loop processing sequentially all CK-files those */
/*     names are stored in string PASS3. */

    start = 1;
    fndnwd_(pass3, &start, &beg, &end, (ftnlen)50000);
    while(beg != 0) {

/*        Were we asked to treat all CK files as one? */

	if (! doall) {

/*           We were not asked to treat all files as one. So open the */
/*           next file and collect information from it. */

	    dafopr_(pass3 + (beg - 1), &handle, end - (beg - 1));
	    fndsum = FALSE_;
	    dafbfs_(&handle);
	    daffna_(&fnd);

/*           These arrays will hold summary data from segment */
/*           descriptors from the current CK file. At the end of */
/*           collection process these arrays will have the same size */
/*           equal to NOBJ: */

/*           IDA      contains IDs of object. */
/*           AVFA     contains AV flags. */
/*           FRAMEA   contains reference frame IDs */
/*           TYPEA    contains segment types */
/*           BADDRA   contains segment begin addresses */
/*           EADDRA   contains segment end addresses */
/*           TSTRTA   contains start time for the */
/*                    objects coverage. */
/*           TENDA    contains end time for the */
/*                    objects coverage. */

	    nobj = 1;
	    fulbuf = FALSE_;
	    while(fnd) {
		dafgs_(segsum);
		dafus_(segsum, &c__2, &c__6, dc, ic);

/*              If we are summarizing from a list of input object, */
/*              find out if this object is in the list to summarize. */

		if (numobj > 0) {
		    if (isrchi_(ic, &numobj, objlis) > 0) {

/*                    We need to check whether we have space in the */
/*                    buffer. Note that we check against MAXBOD-1 */
/*                    because we will use the element IDA(MAXBOD) as */
/*                    loop terminator flag further in the program. */

			if (nobj <= 999999) {
			    fndsum = TRUE_;
			    ida[(i__1 = nobj - 1) < 1000000 && 0 <= i__1 ? 
				    i__1 : s_rnge("ida", i__1, "ckbrief_", (
				    ftnlen)1131)] = ic[0];
			    framea[(i__1 = nobj - 1) < 1000000 && 0 <= i__1 ? 
				    i__1 : s_rnge("framea", i__1, "ckbrief_", 
				    (ftnlen)1132)] = ic[1];
			    typea[(i__1 = nobj - 1) < 1000000 && 0 <= i__1 ? 
				    i__1 : s_rnge("typea", i__1, "ckbrief_", (
				    ftnlen)1133)] = ic[2];
			    avfa[(i__1 = nobj - 1) < 1000000 && 0 <= i__1 ? 
				    i__1 : s_rnge("avfa", i__1, "ckbrief_", (
				    ftnlen)1134)] = ic[3];
			    baddra[(i__1 = nobj - 1) < 1000000 && 0 <= i__1 ? 
				    i__1 : s_rnge("baddra", i__1, "ckbrief_", 
				    (ftnlen)1135)] = ic[4];
			    eaddra[(i__1 = nobj - 1) < 1000000 && 0 <= i__1 ? 
				    i__1 : s_rnge("eaddra", i__1, "ckbrief_", 
				    (ftnlen)1136)] = ic[5];
			    tstrta[(i__1 = nobj - 1) < 1000000 && 0 <= i__1 ? 
				    i__1 : s_rnge("tstrta", i__1, "ckbrief_", 
				    (ftnlen)1137)] = dc[0];
			    tenda[(i__1 = nobj - 1) < 1000000 && 0 <= i__1 ? 
				    i__1 : s_rnge("tenda", i__1, "ckbrief_", (
				    ftnlen)1138)] = dc[1];
			    ++nobj;
			} else {
			    fulbuf = TRUE_;
			}
		    }
		} else {

/*                 Same as above -- check whether we have space in the */
/*                 buffer. Note that we check against MAXBOD-1 because */
/*                 we will use the element IDA(MAXBOD) as loop */
/*                 terminator flag further in the program. */

		    if (nobj <= 999999) {
			fndsum = TRUE_;
			ida[(i__1 = nobj - 1) < 1000000 && 0 <= i__1 ? i__1 : 
				s_rnge("ida", i__1, "ckbrief_", (ftnlen)1162)]
				 = ic[0];
			framea[(i__1 = nobj - 1) < 1000000 && 0 <= i__1 ? 
				i__1 : s_rnge("framea", i__1, "ckbrief_", (
				ftnlen)1163)] = ic[1];
			typea[(i__1 = nobj - 1) < 1000000 && 0 <= i__1 ? i__1 
				: s_rnge("typea", i__1, "ckbrief_", (ftnlen)
				1164)] = ic[2];
			avfa[(i__1 = nobj - 1) < 1000000 && 0 <= i__1 ? i__1 :
				 s_rnge("avfa", i__1, "ckbrief_", (ftnlen)
				1165)] = ic[3];
			baddra[(i__1 = nobj - 1) < 1000000 && 0 <= i__1 ? 
				i__1 : s_rnge("baddra", i__1, "ckbrief_", (
				ftnlen)1166)] = ic[4];
			eaddra[(i__1 = nobj - 1) < 1000000 && 0 <= i__1 ? 
				i__1 : s_rnge("eaddra", i__1, "ckbrief_", (
				ftnlen)1167)] = ic[5];
			tstrta[(i__1 = nobj - 1) < 1000000 && 0 <= i__1 ? 
				i__1 : s_rnge("tstrta", i__1, "ckbrief_", (
				ftnlen)1168)] = dc[0];
			tenda[(i__1 = nobj - 1) < 1000000 && 0 <= i__1 ? i__1 
				: s_rnge("tenda", i__1, "ckbrief_", (ftnlen)
				1169)] = dc[1];
			++nobj;
		    } else {
			fulbuf = TRUE_;
		    }
		}

/*              Go to the next segment */

		daffna_(&fnd);
	    }
	} else {

/*           We were asked to treat all CK files as one. Collect */
/*           information from all of them in the same buffers. */

	    nobj = 1;
	    fulbuf = FALSE_;
	    fndsum = FALSE_;
	    while(beg != 0) {

/*              Open the next CK file. */

		dafopr_(pass3 + (beg - 1), &handle, end - (beg - 1));
		dafbfs_(&handle);
		daffna_(&fnd);

/*              These arrays will hold summary data from segment */
/*              descriptors from all CK files. At the end of collection */
/*              process these arrays will have the same size equal to */
/*              NOBJ: */

/*              IDA      contains IDs of object. */
/*              AVFA     contains AV flags. */
/*              FRAMEA   contains reference frame IDs */
/*              TYPEA    contains segment types */
/*              BADDRA   contains segment begin addresses */
/*              EADDRA   contains segment end addresses */
/*              TSTRTA   contains start time for the */
/*                       objects coverage. */
/*              TENDA    contains end time for the */
/*                       objects coverage. */

		while(fnd) {
		    dafgs_(segsum);
		    dafus_(segsum, &c__2, &c__6, dc, ic);

/*                 If we are summarizing from a list of input object, */
/*                 find out if this object is in the list to summarize. */

		    if (numobj > 0) {
			if (isrchi_(ic, &numobj, objlis) > 0) {

/*                       We need to check whether we have space in the */
/*                       buffer. Note that we check against MAXBOD-1 */
/*                       because we will use the element IDA(MAXBOD) as */
/*                       loop terminator flag further in the program. */

			    if (nobj <= 999999) {
				fndsum = TRUE_;
				ida[(i__1 = nobj - 1) < 1000000 && 0 <= i__1 ?
					 i__1 : s_rnge("ida", i__1, "ckbrief_"
					, (ftnlen)1246)] = ic[0];
				framea[(i__1 = nobj - 1) < 1000000 && 0 <= 
					i__1 ? i__1 : s_rnge("framea", i__1, 
					"ckbrief_", (ftnlen)1247)] = ic[1];
				typea[(i__1 = nobj - 1) < 1000000 && 0 <= 
					i__1 ? i__1 : s_rnge("typea", i__1, 
					"ckbrief_", (ftnlen)1248)] = ic[2];
				avfa[(i__1 = nobj - 1) < 1000000 && 0 <= i__1 
					? i__1 : s_rnge("avfa", i__1, "ckbri"
					"ef_", (ftnlen)1249)] = ic[3];
				baddra[(i__1 = nobj - 1) < 1000000 && 0 <= 
					i__1 ? i__1 : s_rnge("baddra", i__1, 
					"ckbrief_", (ftnlen)1250)] = ic[4];
				eaddra[(i__1 = nobj - 1) < 1000000 && 0 <= 
					i__1 ? i__1 : s_rnge("eaddra", i__1, 
					"ckbrief_", (ftnlen)1251)] = ic[5];
				tstrta[(i__1 = nobj - 1) < 1000000 && 0 <= 
					i__1 ? i__1 : s_rnge("tstrta", i__1, 
					"ckbrief_", (ftnlen)1252)] = dc[0];
				tenda[(i__1 = nobj - 1) < 1000000 && 0 <= 
					i__1 ? i__1 : s_rnge("tenda", i__1, 
					"ckbrief_", (ftnlen)1253)] = dc[1];
				++nobj;
			    } else {
				fulbuf = TRUE_;
			    }
			}
		    } else {

/*                    Same as above -- check whether we have space in */
/*                    the buffer. Note that we check against MAXBOD-1 */
/*                    because we will use the element IDA(MAXBOD) as */
/*                    loop terminator flag further in the program. */

			if (nobj <= 999999) {
			    fndsum = TRUE_;
			    ida[(i__1 = nobj - 1) < 1000000 && 0 <= i__1 ? 
				    i__1 : s_rnge("ida", i__1, "ckbrief_", (
				    ftnlen)1277)] = ic[0];
			    framea[(i__1 = nobj - 1) < 1000000 && 0 <= i__1 ? 
				    i__1 : s_rnge("framea", i__1, "ckbrief_", 
				    (ftnlen)1278)] = ic[1];
			    typea[(i__1 = nobj - 1) < 1000000 && 0 <= i__1 ? 
				    i__1 : s_rnge("typea", i__1, "ckbrief_", (
				    ftnlen)1279)] = ic[2];
			    avfa[(i__1 = nobj - 1) < 1000000 && 0 <= i__1 ? 
				    i__1 : s_rnge("avfa", i__1, "ckbrief_", (
				    ftnlen)1280)] = ic[3];
			    baddra[(i__1 = nobj - 1) < 1000000 && 0 <= i__1 ? 
				    i__1 : s_rnge("baddra", i__1, "ckbrief_", 
				    (ftnlen)1281)] = ic[4];
			    eaddra[(i__1 = nobj - 1) < 1000000 && 0 <= i__1 ? 
				    i__1 : s_rnge("eaddra", i__1, "ckbrief_", 
				    (ftnlen)1282)] = ic[5];
			    tstrta[(i__1 = nobj - 1) < 1000000 && 0 <= i__1 ? 
				    i__1 : s_rnge("tstrta", i__1, "ckbrief_", 
				    (ftnlen)1283)] = dc[0];
			    tenda[(i__1 = nobj - 1) < 1000000 && 0 <= i__1 ? 
				    i__1 : s_rnge("tenda", i__1, "ckbrief_", (
				    ftnlen)1284)] = dc[1];
			    ++nobj;
			} else {
			    fulbuf = TRUE_;
			}
		    }

/*                 Go to the next segment */

		    daffna_(&fnd);
		}

/*              Current CK-file processing is over - close it and move */
/*              on to the next file. */

		dafcls_(&handle);
		start = end + 1;
		fndnwd_(pass3, &start, &beg, &end, (ftnlen)50000);
	    }

/*           We buffered information from all CKs. Re-open the first CK */
/*           to have a valid HANDLE (to make DAFCLS at the end of the */
/*           main loop happy). Reset the list of CKs to a generic phrase */
/*           "all CK files". Reset BEG and END to the beginning and end */
/*           character positions of this phrase. Setting END to the */
/*           last character will make the outer loop terminate. */

	    fndnwd_(pass3, &c__1, &beg, &end, (ftnlen)50000);
	    dafopr_(pass3 + (beg - 1), &handle, end - (beg - 1));
	    s_copy(pass3, "all CK files", (ftnlen)50000, (ftnlen)12);
	    beg = 1;
	    end = rtrim_(pass3, (ftnlen)50000);
	}

/*        The NOBJ is currently greater that the number of collected */
/*        objects by one. We need to fix this. */

	--nobj;

/*        If any segment information was collected, display the name of */
/*        the file being summarized. */

	if (nobj != 0) {

/*           First, check whether we had a buffer overflow. If yes, */
/*           signal an error. Do slightly different messages for */
/*           all files and single file cases. */

	    if (! doall) {
		if (fulbuf) {
		    if (numobj > 0) {
			setmsg_("The number of segments containing data for "
				"the specified IDs in the CK file '#' was gre"
				"ater than the program can buffer (#). CKBRIE"
				"F cannot summarize this file.", (ftnlen)160);
			errch_("#", pass3 + (beg - 1), (ftnlen)1, end - (beg 
				- 1));
			errint_("#", &c_b243, (ftnlen)1);
			sigerr_("SPICE(BUFFEROVERRUN1)", (ftnlen)21);
		    } else {
			setmsg_("The number of segments in the CK file '#' w"
				"as greater than the program can buffer (#). "
				"CKBRIEF cannot summarize this file.", (ftnlen)
				122);
			errch_("#", pass3 + (beg - 1), (ftnlen)1, end - (beg 
				- 1));
			errint_("#", &c_b243, (ftnlen)1);
			sigerr_("SPICE(BUFFEROVERRUN2)", (ftnlen)21);
		    }
		}
	    } else {
		if (fulbuf) {
		    if (numobj > 0) {
			setmsg_("The number of segments containing data for "
				"the specified IDs in all CK files was greate"
				"r than the program can buffer (#).", (ftnlen)
				121);
			errint_("#", &c_b243, (ftnlen)1);
			sigerr_("SPICE(BUFFEROVERRUN3)", (ftnlen)21);
		    } else {
			setmsg_("The number of segments in all CK files was "
				"greater than the program can buffer (#).", (
				ftnlen)83);
			errint_("#", &c_b243, (ftnlen)1);
			sigerr_("SPICE(BUFFEROVERRUN4)", (ftnlen)21);
		    }
		}
	    }

/*           No buffer overrun. Display the name of the file. */

	    tostdo_("  ", (ftnlen)2);
	    tostdo_("  ", (ftnlen)2);
/* Writing concatenation */
	    i__2[0] = 13, a__1[0] = "Summary for: ";
	    i__2[1] = end - (beg - 1), a__1[1] = pass3 + (beg - 1);
	    s_cat(ch__1, a__1, i__2, &c__2, (ftnlen)50013);
	    tostdo_(ch__1, end - (beg - 1) + 13);
	} else {

/*           No segment data was collected. Report this fact. */

	    tostdo_("  ", (ftnlen)2);
	    tostdo_("  ", (ftnlen)2);
	    if (numobj > 0) {
/* Writing concatenation */
		i__2[0] = 33, a__1[0] = "No data for specified objects in ";
		i__2[1] = end - (beg - 1), a__1[1] = pass3 + (beg - 1);
		s_cat(ch__2, a__1, i__2, &c__2, (ftnlen)50033);
		tostdo_(ch__2, end - (beg - 1) + 33);
	    } else {
/* Writing concatenation */
		i__2[0] = 20, a__1[0] = "No data segments in ";
		i__2[1] = end - (beg - 1), a__1[1] = pass3 + (beg - 1);
		s_cat(ch__3, a__1, i__2, &c__2, (ftnlen)50020);
		tostdo_(ch__3, end - (beg - 1) + 20);
	    }
	}

/*        If we were asked to dump intervals, we have got everything */
/*        we need for displaying them. Otherwise, we have a lot of work */
/*        do. */

	if (idump) {

/*           All we need to do to dump intervals is to call Bills */
/*           ZZCKCRVs and copy output schedule to the buffers that */
/*           DISPSM expects. (BTW, if NOBJ is 0 this DO will never */
/*           DO :-) */

	    i__1 = nobj;
	    for (i__ = 1; i__ <= i__1; ++i__) {

/*              Display segment order number. */

		s_copy(hword, "Segment No.: #", (ftnlen)256, (ftnlen)14);
		repmi_(hword, "#", &i__, hword, (ftnlen)256, (ftnlen)1, (
			ftnlen)256);
		tostdo_("  ", (ftnlen)2);
		tostdo_(hword, (ftnlen)256);

/*              Does looking for interval for a segment of this */
/*              type make sense? */

		if (typea[(i__3 = i__ - 1) < 1000000 && 0 <= i__3 ? i__3 : 
			s_rnge("typea", i__3, "ckbrief_", (ftnlen)1463)] == 2 
			|| typea[(i__4 = i__ - 1) < 1000000 && 0 <= i__4 ? 
			i__4 : s_rnge("typea", i__4, "ckbrief_", (ftnlen)1463)
			] == 3 || typea[(i__5 = i__ - 1) < 1000000 && 0 <= 
			i__5 ? i__5 : s_rnge("typea", i__5, "ckbrief_", (
			ftnlen)1463)] == 4 || typea[(i__6 = i__ - 1) < 
			1000000 && 0 <= i__6 ? i__6 : s_rnge("typea", i__6, 
			"ckbrief_", (ftnlen)1463)] == 5 || typea[(i__7 = i__ 
			- 1) < 1000000 && 0 <= i__7 ? i__7 : s_rnge("typea", 
			i__7, "ckbrief_", (ftnlen)1463)] == 6) {

/*                 Sure does! Initialize window and call corresponding */
/*                 ZZCKCRVx depending on what type the segment is. */

		    ssized_(&c_b281, wind);
		    if (typea[(i__3 = i__ - 1) < 1000000 && 0 <= i__3 ? i__3 :
			     s_rnge("typea", i__3, "ckbrief_", (ftnlen)1475)] 
			    == 2) {
			zzckcvr2_(&handle, &baddra[(i__3 = i__ - 1) < 1000000 
				&& 0 <= i__3 ? i__3 : s_rnge("baddra", i__3, 
				"ckbrief_", (ftnlen)1477)], &eaddra[(i__4 = 
				i__ - 1) < 1000000 && 0 <= i__4 ? i__4 : 
				s_rnge("eaddra", i__4, "ckbrief_", (ftnlen)
				1477)], wind);
		    } else if (typea[(i__3 = i__ - 1) < 1000000 && 0 <= i__3 ?
			     i__3 : s_rnge("typea", i__3, "ckbrief_", (ftnlen)
			    1479)] == 3) {
			zzckcvr3_(&handle, &baddra[(i__3 = i__ - 1) < 1000000 
				&& 0 <= i__3 ? i__3 : s_rnge("baddra", i__3, 
				"ckbrief_", (ftnlen)1481)], &eaddra[(i__4 = 
				i__ - 1) < 1000000 && 0 <= i__4 ? i__4 : 
				s_rnge("eaddra", i__4, "ckbrief_", (ftnlen)
				1481)], wind);
		    } else if (typea[(i__3 = i__ - 1) < 1000000 && 0 <= i__3 ?
			     i__3 : s_rnge("typea", i__3, "ckbrief_", (ftnlen)
			    1483)] == 4) {
			zzckcvr4_(&handle, &baddra[(i__3 = i__ - 1) < 1000000 
				&& 0 <= i__3 ? i__3 : s_rnge("baddra", i__3, 
				"ckbrief_", (ftnlen)1485)], &eaddra[(i__4 = 
				i__ - 1) < 1000000 && 0 <= i__4 ? i__4 : 
				s_rnge("eaddra", i__4, "ckbrief_", (ftnlen)
				1485)], wind);
		    } else if (typea[(i__3 = i__ - 1) < 1000000 && 0 <= i__3 ?
			     i__3 : s_rnge("typea", i__3, "ckbrief_", (ftnlen)
			    1487)] == 5) {
			zzckcvr5_(&handle, &baddra[(i__3 = i__ - 1) < 1000000 
				&& 0 <= i__3 ? i__3 : s_rnge("baddra", i__3, 
				"ckbrief_", (ftnlen)1489)], &eaddra[(i__4 = 
				i__ - 1) < 1000000 && 0 <= i__4 ? i__4 : 
				s_rnge("eaddra", i__4, "ckbrief_", (ftnlen)
				1489)], wind);
		    } else if (typea[(i__3 = i__ - 1) < 1000000 && 0 <= i__3 ?
			     i__3 : s_rnge("typea", i__3, "ckbrief_", (ftnlen)
			    1491)] == 6) {

/*                    Set the SCLK ID to 0, since this input is unused. */
/*                    The lookup tolerance is zero; the time system is */
/*                    SCLK. */

			dc[0] = tstrta[(i__3 = i__ - 1) < 1000000 && 0 <= 
				i__3 ? i__3 : s_rnge("tstrta", i__3, "ckbrie"
				"f_", (ftnlen)1497)];
			dc[1] = tenda[(i__3 = i__ - 1) < 1000000 && 0 <= i__3 
				? i__3 : s_rnge("tenda", i__3, "ckbrief_", (
				ftnlen)1498)];
			zzckcv06_(&handle, &baddra[(i__3 = i__ - 1) < 1000000 
				&& 0 <= i__3 ? i__3 : s_rnge("baddra", i__3, 
				"ckbrief_", (ftnlen)1500)], &eaddra[(i__4 = 
				i__ - 1) < 1000000 && 0 <= i__4 ? i__4 : 
				s_rnge("eaddra", i__4, "ckbrief_", (ftnlen)
				1500)], &c__0, dc, &c_b313, "SCLK", wind, (
				ftnlen)4);
		    }

/*                 We fill have got a schedule for this segment. Copy */
/*                 it to buffers and call DISPSM. */

		    if (cardd_(wind) != 0) {
			nobgs = cardd_(wind) / 2;
			i__3 = nobgs - 1;
			for (l = 0; l <= i__3; ++l) {
			    ids[(i__4 = l) < 1000000 && 0 <= i__4 ? i__4 : 
				    s_rnge("ids", i__4, "ckbrief_", (ftnlen)
				    1514)] = ida[(i__5 = i__ - 1) < 1000000 &&
				     0 <= i__5 ? i__5 : s_rnge("ida", i__5, 
				    "ckbrief_", (ftnlen)1514)];
			    tstrts[(i__4 = l) < 1000000 && 0 <= i__4 ? i__4 : 
				    s_rnge("tstrts", i__4, "ckbrief_", (
				    ftnlen)1515)] = wind[(i__5 = (l << 1) + 6)
				     < 2000006 && 0 <= i__5 ? i__5 : s_rnge(
				    "wind", i__5, "ckbrief_", (ftnlen)1515)];
			    tends[(i__4 = l) < 1000000 && 0 <= i__4 ? i__4 : 
				    s_rnge("tends", i__4, "ckbrief_", (ftnlen)
				    1516)] = wind[(i__5 = (l << 1) + 7) < 
				    2000006 && 0 <= i__5 ? i__5 : s_rnge(
				    "wind", i__5, "ckbrief_", (ftnlen)1516)];
			    avfs[(i__4 = l) < 1000000 && 0 <= i__4 ? i__4 : 
				    s_rnge("avfs", i__4, "ckbrief_", (ftnlen)
				    1517)] = avfa[(i__5 = i__ - 1) < 1000000 
				    && 0 <= i__5 ? i__5 : s_rnge("avfa", i__5,
				     "ckbrief_", (ftnlen)1517)];
			    frames[(i__4 = l) < 1000000 && 0 <= i__4 ? i__4 : 
				    s_rnge("frames", i__4, "ckbrief_", (
				    ftnlen)1518)] = framea[(i__5 = i__ - 1) < 
				    1000000 && 0 <= i__5 ? i__5 : s_rnge(
				    "framea", i__5, "ckbrief_", (ftnlen)1518)]
				    ;
			}
			dispsm_(&nobgs, ids, tstrts, tends, avfs, frames, 
				tout, &fdsp, &tdsp, &gdsp, &ndsp, (ftnlen)256)
				;
		    }
		} else {

/*                 Well, this segment is not eligible for having */
/*                 intervals. Tell user about it. */

		    s_copy(hword, "This segment is a type # segment for whic"
			    "h interpolation is not supported.", (ftnlen)256, (
			    ftnlen)74);
		    repmi_(hword, "#", &typea[(i__3 = i__ - 1) < 1000000 && 0 
			    <= i__3 ? i__3 : s_rnge("typea", i__3, "ckbrief_",
			     (ftnlen)1534)], hword, (ftnlen)256, (ftnlen)1, (
			    ftnlen)256);
		    tostdo_("  ", (ftnlen)2);
		    tostdo_(hword, (ftnlen)256);
		}
	    }
	} else {

/*           We are here because we weren't asked to do interval dump */
/*           but were rather asked to do something intelligent (that */
/*           intelligent work is what CKRBIEF was written to do :-) */

/*           Data from all segment descriptors in the current file are */
/*           stored in arrays. Now it will be distributed to two */
/*           different groups of arrays. */

/*           First group - for objects which have multiple coverage */
/*           intervals in current CK-file. */

/*           Second group - for objects which have single coverage */
/*           interval in current CK-file. */

/*           This distribution is done to provide a possibility to merge */
/*           time intervals for a particular object if intervals */
/*           overlap. */

/*           Initialization of array pointers */

	    nobgs = 1;
	    nobgo = 1;
	    i__ = 1;

/*           Now we set IDA(NOBJ+1) to 0 we can safely do that because */
/*           we checked against MAXBOD-1 in the loop filling the */
/*           buffers. */

	    ida[(i__1 = nobj) < 1000000 && 0 <= i__1 ? i__1 : s_rnge("ida", 
		    i__1, "ckbrief_", (ftnlen)1574)] = 0;

/*           Here is the cycle to get through all descriptors to */
/*           distribute them to two arrays. */

	    while(fndsum && i__ <= nobj) {

/*              Skip all records with IDs set to zero -- these were */
/*              already taken into account. */

		while(ida[(i__1 = i__ - 1) < 1000000 && 0 <= i__1 ? i__1 : 
			s_rnge("ida", i__1, "ckbrief_", (ftnlen)1586)] == 0 &&
			 i__ <= nobj) {
		    ++i__;
		}
		if (i__ <= nobj) {

/*                 Current segment is the first segment found for this */
/*                 ID in the buffer. Set counter of segments for current */
/*                 ID to 1 and copy complete summary for current segment */
/*                 to IDO, etc. buffers. */

		    cnt = 1;
		    ido[(i__1 = nobgo - 1) < 1000000 && 0 <= i__1 ? i__1 : 
			    s_rnge("ido", i__1, "ckbrief_", (ftnlen)1600)] = 
			    ida[(i__3 = i__ - 1) < 1000000 && 0 <= i__3 ? 
			    i__3 : s_rnge("ida", i__3, "ckbrief_", (ftnlen)
			    1600)];
		    avfo[(i__1 = nobgo - 1) < 1000000 && 0 <= i__1 ? i__1 : 
			    s_rnge("avfo", i__1, "ckbrief_", (ftnlen)1601)] = 
			    avfa[(i__3 = i__ - 1) < 1000000 && 0 <= i__3 ? 
			    i__3 : s_rnge("avfa", i__3, "ckbrief_", (ftnlen)
			    1601)];
		    frameo[(i__1 = nobgo - 1) < 1000000 && 0 <= i__1 ? i__1 : 
			    s_rnge("frameo", i__1, "ckbrief_", (ftnlen)1602)] 
			    = framea[(i__3 = i__ - 1) < 1000000 && 0 <= i__3 ?
			     i__3 : s_rnge("framea", i__3, "ckbrief_", (
			    ftnlen)1602)];
		    tstrto[(i__1 = nobgo - 1) < 1000000 && 0 <= i__1 ? i__1 : 
			    s_rnge("tstrto", i__1, "ckbrief_", (ftnlen)1603)] 
			    = tstrta[(i__3 = i__ - 1) < 1000000 && 0 <= i__3 ?
			     i__3 : s_rnge("tstrta", i__3, "ckbrief_", (
			    ftnlen)1603)];
		    tendo[(i__1 = nobgo - 1) < 1000000 && 0 <= i__1 ? i__1 : 
			    s_rnge("tendo", i__1, "ckbrief_", (ftnlen)1604)] =
			     tenda[(i__3 = i__ - 1) < 1000000 && 0 <= i__3 ? 
			    i__3 : s_rnge("tenda", i__3, "ckbrief_", (ftnlen)
			    1604)];

/*                 Increase counter of segments for current ID and */
/*                 "mark" current segment as "processed" in IDA buffer */
/*                 by setting its IDA to zero. */

		    ++nobgo;
		    ida[(i__1 = i__ - 1) < 1000000 && 0 <= i__1 ? i__1 : 
			    s_rnge("ida", i__1, "ckbrief_", (ftnlen)1612)] = 
			    0;

/*                 Loop to find all segments with the same ID in the IDA */
/*                 buffer. */

		    i__4 = nobj - i__;
		    loc = isrchi_(&ido[(i__1 = nobgo - 2) < 1000000 && 0 <= 
			    i__1 ? i__1 : s_rnge("ido", i__1, "ckbrief_", (
			    ftnlen)1618)], &i__4, &ida[(i__3 = i__) < 1000000 
			    && 0 <= i__3 ? i__3 : s_rnge("ida", i__3, "ckbri"
			    "ef_", (ftnlen)1618)]);
		    k = loc + i__;
		    while(loc != 0) {

/*                    We found one more -- increase counter, save */
/*                    summary in IDO, etc. array and reset its IDS to 0. */

			++cnt;
			ido[(i__1 = nobgo - 1) < 1000000 && 0 <= i__1 ? i__1 :
				 s_rnge("ido", i__1, "ckbrief_", (ftnlen)1629)
				] = ida[(i__3 = k - 1) < 1000000 && 0 <= i__3 
				? i__3 : s_rnge("ida", i__3, "ckbrief_", (
				ftnlen)1629)];
			avfo[(i__1 = nobgo - 1) < 1000000 && 0 <= i__1 ? i__1 
				: s_rnge("avfo", i__1, "ckbrief_", (ftnlen)
				1630)] = avfa[(i__3 = k - 1) < 1000000 && 0 <=
				 i__3 ? i__3 : s_rnge("avfa", i__3, "ckbrief_"
				, (ftnlen)1630)];
			frameo[(i__1 = nobgo - 1) < 1000000 && 0 <= i__1 ? 
				i__1 : s_rnge("frameo", i__1, "ckbrief_", (
				ftnlen)1631)] = framea[(i__3 = k - 1) < 
				1000000 && 0 <= i__3 ? i__3 : s_rnge("framea",
				 i__3, "ckbrief_", (ftnlen)1631)];
			tstrto[(i__1 = nobgo - 1) < 1000000 && 0 <= i__1 ? 
				i__1 : s_rnge("tstrto", i__1, "ckbrief_", (
				ftnlen)1632)] = tstrta[(i__3 = k - 1) < 
				1000000 && 0 <= i__3 ? i__3 : s_rnge("tstrta",
				 i__3, "ckbrief_", (ftnlen)1632)];
			tendo[(i__1 = nobgo - 1) < 1000000 && 0 <= i__1 ? 
				i__1 : s_rnge("tendo", i__1, "ckbrief_", (
				ftnlen)1633)] = tenda[(i__3 = k - 1) < 
				1000000 && 0 <= i__3 ? i__3 : s_rnge("tenda", 
				i__3, "ckbrief_", (ftnlen)1633)];
			++nobgo;
			ida[(i__1 = k - 1) < 1000000 && 0 <= i__1 ? i__1 : 
				s_rnge("ida", i__1, "ckbrief_", (ftnlen)1636)]
				 = 0;
			i__4 = nobj - k;
			loc = isrchi_(&ido[(i__1 = nobgo - 2) < 1000000 && 0 
				<= i__1 ? i__1 : s_rnge("ido", i__1, "ckbrie"
				"f_", (ftnlen)1638)], &i__4, &ida[(i__3 = k) < 
				1000000 && 0 <= i__3 ? i__3 : s_rnge("ida", 
				i__3, "ckbrief_", (ftnlen)1638)]);
			k += loc;
		    }

/*                 Did we find more than one segment for current ID? */

		    if (cnt == 1) {

/*                    No, we didn't. Then we include this segment into */
/*                    IDS buffer. But first we reset NOBGO back to */
/*                    remove it from IDO buffer. */

			--nobgo;
			ids[(i__1 = nobgs - 1) < 1000000 && 0 <= i__1 ? i__1 :
				 s_rnge("ids", i__1, "ckbrief_", (ftnlen)1654)
				] = ido[(i__3 = nobgo - 1) < 1000000 && 0 <= 
				i__3 ? i__3 : s_rnge("ido", i__3, "ckbrief_", 
				(ftnlen)1654)];
			avfs[(i__1 = nobgs - 1) < 1000000 && 0 <= i__1 ? i__1 
				: s_rnge("avfs", i__1, "ckbrief_", (ftnlen)
				1655)] = avfo[(i__3 = nobgo - 1) < 1000000 && 
				0 <= i__3 ? i__3 : s_rnge("avfo", i__3, "ckb"
				"rief_", (ftnlen)1655)];
			frames[(i__1 = nobgs - 1) < 1000000 && 0 <= i__1 ? 
				i__1 : s_rnge("frames", i__1, "ckbrief_", (
				ftnlen)1656)] = frameo[(i__3 = nobgo - 1) < 
				1000000 && 0 <= i__3 ? i__3 : s_rnge("frameo",
				 i__3, "ckbrief_", (ftnlen)1656)];
			tstrts[(i__1 = nobgs - 1) < 1000000 && 0 <= i__1 ? 
				i__1 : s_rnge("tstrts", i__1, "ckbrief_", (
				ftnlen)1657)] = tstrto[(i__3 = nobgo - 1) < 
				1000000 && 0 <= i__3 ? i__3 : s_rnge("tstrto",
				 i__3, "ckbrief_", (ftnlen)1657)];
			tends[(i__1 = nobgs - 1) < 1000000 && 0 <= i__1 ? 
				i__1 : s_rnge("tends", i__1, "ckbrief_", (
				ftnlen)1658)] = tendo[(i__3 = nobgo - 1) < 
				1000000 && 0 <= i__3 ? i__3 : s_rnge("tendo", 
				i__3, "ckbrief_", (ftnlen)1658)];
			++nobgs;
		    }
		}
	    }
	    --nobgo;

/*           At this point we have two buffers -- xxxO and xxxS -- */
/*           filled with appropriate segment descriptors. There is no */
/*           need to do any more processing for xxxS buffer. But there */
/*           is work to be done on xxxO buffer: we need to merge */
/*           overlapping time intervals for each ID. */

	    if (nobgo > 1) {
		i__ = 1;
		k = 1;
		while(i__ <= nobgo - 1) {

/*                 All segments for the same ID are already grouped */
/*                 together. We need to find out what is the first and */
/*                 last of them */

		    while(ido[(i__1 = i__ - 1) < 1000000 && 0 <= i__1 ? i__1 :
			     s_rnge("ido", i__1, "ckbrief_", (ftnlen)1689)] ==
			     ido[(i__3 = k - 1) < 1000000 && 0 <= i__3 ? i__3 
			    : s_rnge("ido", i__3, "ckbrief_", (ftnlen)1689)] 
			    && k <= nobgo) {
			++k;
		    }

/*                 After we know it, we copy start and stop times to */
/*                 a WINDOW array. */

		    ssized_(&c_b281, wind);
		    i__1 = k - i__ << 1;
		    scardd_(&i__1, wind);
		    i__1 = k - i__ - 1;
		    for (l = 0; l <= i__1; ++l) {
			wind[(i__3 = (l << 1) + 6) < 2000006 && 0 <= i__3 ? 
				i__3 : s_rnge("wind", i__3, "ckbrief_", (
				ftnlen)1701)] = tstrto[(i__4 = l + i__ - 1) < 
				1000000 && 0 <= i__4 ? i__4 : s_rnge("tstrto",
				 i__4, "ckbrief_", (ftnlen)1701)];
			wind[(i__3 = (l << 1) + 7) < 2000006 && 0 <= i__3 ? 
				i__3 : s_rnge("wind", i__3, "ckbrief_", (
				ftnlen)1702)] = tendo[(i__4 = l + i__ - 1) < 
				1000000 && 0 <= i__4 ? i__4 : s_rnge("tendo", 
				i__4, "ckbrief_", (ftnlen)1702)];
		    }

/*                 Here is important point of algorithm. We'll validate */
/*                 window of time intervals. But we do it only if NOMERG */
/*                 flag is not set. */

		    if (! nomerg) {
			i__1 = k - i__ << 1;
			wnvald_(&c_b281, &i__1, wind);
		    }

/*                 Now we can check whether our window validating */
/*                 brought any results. */

		    if (cardd_(wind) == k - i__ << 1) {

/*                    Nothing happened with our windows. Well, the best */
/*                    we can do then is to order coverages for this ID */
/*                    by start time (and this order in xxxO all arrays.) */

			i__3 = k - i__;
			orderd_(&tstrto[(i__1 = i__ - 1) < 1000000 && 0 <= 
				i__1 ? i__1 : s_rnge("tstrto", i__1, "ckbrie"
				"f_", (ftnlen)1725)], &i__3, iorder);
			i__3 = k - i__;
			reordd_(iorder, &i__3, &tstrto[(i__1 = i__ - 1) < 
				1000000 && 0 <= i__1 ? i__1 : s_rnge("tstrto",
				 i__1, "ckbrief_", (ftnlen)1727)]);
			i__3 = k - i__;
			reordd_(iorder, &i__3, &tendo[(i__1 = i__ - 1) < 
				1000000 && 0 <= i__1 ? i__1 : s_rnge("tendo", 
				i__1, "ckbrief_", (ftnlen)1728)]);
			i__3 = k - i__;
			reordi_(iorder, &i__3, &avfo[(i__1 = i__ - 1) < 
				1000000 && 0 <= i__1 ? i__1 : s_rnge("avfo", 
				i__1, "ckbrief_", (ftnlen)1729)]);
			i__3 = k - i__;
			reordi_(iorder, &i__3, &frameo[(i__1 = i__ - 1) < 
				1000000 && 0 <= i__1 ? i__1 : s_rnge("frameo",
				 i__1, "ckbrief_", (ftnlen)1730)]);

/*                    Now we can copy sorted data back to xxxS buffer. */

			i__4 = k - i__;
			movei_(&ido[(i__1 = i__ - 1) < 1000000 && 0 <= i__1 ? 
				i__1 : s_rnge("ido", i__1, "ckbrief_", (
				ftnlen)1735)], &i__4, &ids[(i__3 = nobgs - 1) 
				< 1000000 && 0 <= i__3 ? i__3 : s_rnge("ids", 
				i__3, "ckbrief_", (ftnlen)1735)]);
			i__4 = k - i__;
			moved_(&tstrto[(i__1 = i__ - 1) < 1000000 && 0 <= 
				i__1 ? i__1 : s_rnge("tstrto", i__1, "ckbrie"
				"f_", (ftnlen)1736)], &i__4, &tstrts[(i__3 = 
				nobgs - 1) < 1000000 && 0 <= i__3 ? i__3 : 
				s_rnge("tstrts", i__3, "ckbrief_", (ftnlen)
				1736)]);
			i__4 = k - i__;
			moved_(&tendo[(i__1 = i__ - 1) < 1000000 && 0 <= i__1 
				? i__1 : s_rnge("tendo", i__1, "ckbrief_", (
				ftnlen)1737)], &i__4, &tends[(i__3 = nobgs - 
				1) < 1000000 && 0 <= i__3 ? i__3 : s_rnge(
				"tends", i__3, "ckbrief_", (ftnlen)1737)]);
			i__4 = k - i__;
			movei_(&avfo[(i__1 = i__ - 1) < 1000000 && 0 <= i__1 ?
				 i__1 : s_rnge("avfo", i__1, "ckbrief_", (
				ftnlen)1738)], &i__4, &avfs[(i__3 = nobgs - 1)
				 < 1000000 && 0 <= i__3 ? i__3 : s_rnge("avfs"
				, i__3, "ckbrief_", (ftnlen)1738)]);
			i__4 = k - i__;
			movei_(&frameo[(i__1 = i__ - 1) < 1000000 && 0 <= 
				i__1 ? i__1 : s_rnge("frameo", i__1, "ckbrie"
				"f_", (ftnlen)1739)], &i__4, &frames[(i__3 = 
				nobgs - 1) < 1000000 && 0 <= i__3 ? i__3 : 
				s_rnge("frames", i__3, "ckbrief_", (ftnlen)
				1739)]);
			nobgs = nobgs + k - i__;
		    } else {

/*                    Some of the windows got merged. It means that by */
/*                    validating we broke connection between them and */
/*                    AVFS and FRAMES for the original segments. And */
/*                    what if original segments used in window */
/*                    validating had a different AV flags of were */
/*                    relative to different frames? We have to check */
/*                    that and assign right values to indicate this -- */
/*                    set AVS to 2 and FRAMES to 0. */

			mixdav = FALSE_;
			mixdfr = FALSE_;
			l = i__;
			while(l < k - 1) {
			    if (avfo[(i__1 = l - 1) < 1000000 && 0 <= i__1 ? 
				    i__1 : s_rnge("avfo", i__1, "ckbrief_", (
				    ftnlen)1760)] != avfo[(i__3 = l) < 
				    1000000 && 0 <= i__3 ? i__3 : s_rnge(
				    "avfo", i__3, "ckbrief_", (ftnlen)1760)]) 
				    {
				mixdav = TRUE_;
			    }
			    if (frameo[(i__1 = l - 1) < 1000000 && 0 <= i__1 ?
				     i__1 : s_rnge("frameo", i__1, "ckbrief_",
				     (ftnlen)1764)] != frameo[(i__3 = l) < 
				    1000000 && 0 <= i__3 ? i__3 : s_rnge(
				    "frameo", i__3, "ckbrief_", (ftnlen)1764)]
				    ) {
				mixdfr = TRUE_;
			    }
			    ++l;
			}

/*                    If we had mixed AV flags, we set all AVFO to 2. */

			if (mixdav) {
			    i__1 = k - 1;
			    for (l = i__; l <= i__1; ++l) {
				avfo[(i__3 = l - 1) < 1000000 && 0 <= i__3 ? 
					i__3 : s_rnge("avfo", i__3, "ckbrief_"
					, (ftnlen)1777)] = 2;
			    }
			}

/*                    If we had mixed reference frames, we set all */
/*                    FRAMEO to 0. */

			if (mixdfr) {
			    i__1 = k - 1;
			    for (l = i__; l <= i__1; ++l) {
				frameo[(i__3 = l - 1) < 1000000 && 0 <= i__3 ?
					 i__3 : s_rnge("frameo", i__3, "ckbr"
					"ief_", (ftnlen)1787)] = 0;
			    }
			}

/*                    What if after validation we have only 1 time */
/*                    interval? Then data for the object have to be */
/*                    moved from array for multiple intervals to array */
/*                    for single intervals. */

			if (cardd_(wind) == 2) {

/*                       Copy summaries to IDS, etc. arrays. */

			    ids[(i__1 = nobgs - 1) < 1000000 && 0 <= i__1 ? 
				    i__1 : s_rnge("ids", i__1, "ckbrief_", (
				    ftnlen)1802)] = ido[(i__3 = i__ - 1) < 
				    1000000 && 0 <= i__3 ? i__3 : s_rnge(
				    "ido", i__3, "ckbrief_", (ftnlen)1802)];
			    avfs[(i__1 = nobgs - 1) < 1000000 && 0 <= i__1 ? 
				    i__1 : s_rnge("avfs", i__1, "ckbrief_", (
				    ftnlen)1803)] = avfo[(i__3 = i__ - 1) < 
				    1000000 && 0 <= i__3 ? i__3 : s_rnge(
				    "avfo", i__3, "ckbrief_", (ftnlen)1803)];
			    frames[(i__1 = nobgs - 1) < 1000000 && 0 <= i__1 ?
				     i__1 : s_rnge("frames", i__1, "ckbrief_",
				     (ftnlen)1804)] = frameo[(i__3 = i__ - 1) 
				    < 1000000 && 0 <= i__3 ? i__3 : s_rnge(
				    "frameo", i__3, "ckbrief_", (ftnlen)1804)]
				    ;
			    tstrts[(i__1 = nobgs - 1) < 1000000 && 0 <= i__1 ?
				     i__1 : s_rnge("tstrts", i__1, "ckbrief_",
				     (ftnlen)1805)] = wind[6];
			    tends[(i__1 = nobgs - 1) < 1000000 && 0 <= i__1 ? 
				    i__1 : s_rnge("tends", i__1, "ckbrief_", (
				    ftnlen)1806)] = wind[7];

/*                       Increment number of elements in AVS, etc. */
/*                       arrays. */

			    ++nobgs;
			} else {

/*                       It looks like we have more than one interval. */

			    i__1 = cardd_(wind) / 2 - 1;
			    for (l = 0; l <= i__1; ++l) {
				temps[(i__3 = l) < 1000000 && 0 <= i__3 ? 
					i__3 : s_rnge("temps", i__3, "ckbrie"
					"f_", (ftnlen)1820)] = wind[(i__4 = (l 
					<< 1) + 6) < 2000006 && 0 <= i__4 ? 
					i__4 : s_rnge("wind", i__4, "ckbrief_"
					, (ftnlen)1820)];
				tempe[(i__3 = l) < 1000000 && 0 <= i__3 ? 
					i__3 : s_rnge("tempe", i__3, "ckbrie"
					"f_", (ftnlen)1821)] = wind[(i__4 = (l 
					<< 1) + 7) < 2000006 && 0 <= i__4 ? 
					i__4 : s_rnge("wind", i__4, "ckbrief_"
					, (ftnlen)1821)];
			    }
			    l = cardd_(wind) / 2;

/*                       Copy objects with multiple time intervals to */
/*                       xxxS buffer. */

			    movei_(&ido[(i__1 = i__ - 1) < 1000000 && 0 <= 
				    i__1 ? i__1 : s_rnge("ido", i__1, "ckbri"
				    "ef_", (ftnlen)1829)], &l, &ids[(i__3 = 
				    nobgs - 1) < 1000000 && 0 <= i__3 ? i__3 :
				     s_rnge("ids", i__3, "ckbrief_", (ftnlen)
				    1829)]);
			    moved_(temps, &l, &tstrts[(i__1 = nobgs - 1) < 
				    1000000 && 0 <= i__1 ? i__1 : s_rnge(
				    "tstrts", i__1, "ckbrief_", (ftnlen)1830)]
				    );
			    moved_(tempe, &l, &tends[(i__1 = nobgs - 1) < 
				    1000000 && 0 <= i__1 ? i__1 : s_rnge(
				    "tends", i__1, "ckbrief_", (ftnlen)1831)])
				    ;
			    movei_(&avfo[(i__1 = i__ - 1) < 1000000 && 0 <= 
				    i__1 ? i__1 : s_rnge("avfo", i__1, "ckbr"
				    "ief_", (ftnlen)1832)], &l, &avfs[(i__3 = 
				    nobgs - 1) < 1000000 && 0 <= i__3 ? i__3 :
				     s_rnge("avfs", i__3, "ckbrief_", (ftnlen)
				    1832)]);
			    movei_(&frameo[(i__1 = i__ - 1) < 1000000 && 0 <= 
				    i__1 ? i__1 : s_rnge("frameo", i__1, 
				    "ckbrief_", (ftnlen)1833)], &l, &frames[(
				    i__3 = nobgs - 1) < 1000000 && 0 <= i__3 ?
				     i__3 : s_rnge("frames", i__3, "ckbrief_",
				     (ftnlen)1833)]);
			    nobgs += l;
			}
		    }

/*                 We are done with current ID. Lets move to the next */
/*                 one. */

		    i__ = k;
		}
	    }

/*           OK, we are done with processing of the xxxO buffer. All */
/*           data sorted/merged and stored in a single buffer -- xxxS. */
/*           Lets display summary then .... */

	    --nobgs;
	    if (nobgs != 0) {
		dispsm_(&nobgs, ids, tstrts, tends, avfs, frames, tout, &fdsp,
			 &tdsp, &gdsp, &ndsp, (ftnlen)256);
	    }
	}

/*        Current CK-file processing is over - close it and move on to */
/*        the next file. */

	dafcls_(&handle);
	start = end + 1;
	fndnwd_(pass3, &start, &beg, &end, (ftnlen)50000);
    }

/*     Add one more blank line at the end of the summary report. */

    tostdo_(" ", (ftnlen)1);

/*     Check out. */

    chkout_("CKBRIEF", (ftnlen)7);

/*     Return with success status. */

    byebye_("SUCCESS", (ftnlen)7);
    s_stop("", (ftnlen)0);
    return 0;
} /* MAIN__ */

/* Main program alias */ int ckbrief_ () { MAIN__ (); return 0; }
