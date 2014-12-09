/* chwcml.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__1 = 1;
static integer c_b114 = 1000000;
static integer c__1000 = 1000;
static integer c__6 = 6;
static integer c__17 = 17;
static integer c__14 = 14;
static integer c__25 = 25;
static integer c__3 = 3;
static integer c__100 = 100;
static integer c__5 = 5;
static integer c__4 = 4;
static integer c__0 = 0;
static doublereal c_b849 = 0.;
static doublereal c_b1112 = 1e-8;

/* $Procedure      CHWCML ( Extract arguments from FRMDIFF command line ) */
/* Subroutine */ int chwcml_(char *line, char *kernam, char *ffrnam, integer *
	ffrid, char *tfrnam, integer *tfrid, logical *avflg, logical *avfflg, 
	doublereal *cmpwin, integer *nitr, doublereal *step, char *diftyp, 
	char *timfmt, char *kernls, integer *sclkid, integer *axes, char *
	aunits, integer *sigdig, ftnlen line_len, ftnlen kernam_len, ftnlen 
	ffrnam_len, ftnlen tfrnam_len, ftnlen diftyp_len, ftnlen timfmt_len, 
	ftnlen kernls_len, ftnlen aunits_len)
{
    /* Initialized data */

    static char clkeys[32*25] = "-k                              " "-t1     "
	    "                        " "-f1                             " 
	    "-c1                             " "-k1                         "
	    "    " "-t2                             " "-f2                   "
	    "          " "-c2                             " "-k2             "
	    "                " "-a                              " "-m        "
	    "                      " "-b                              " "-e  "
	    "                            " "-n                              " 
	    "-s                              " "-f                          "
	    "    " "-t                              " "-o                    "
	    "          " "-x                              " "-d              "
	    "                " "-usage                          " "-u        "
	    "                      " "-help                           " "-h  "
	    "                            " "-v                              ";

    /* System generated locals */
    address a__1[2], a__2[3], a__3[5], a__4[4];
    integer i__1, i__2, i__3[2], i__4, i__5, i__6, i__7[3], i__8[5], i__9[4];
    char ch__1[93], ch__2[133], ch__3[136];

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen), s_cat(char *,
	     char **, integer *, integer *, ftnlen), s_stop(char *, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static char arch[3*2], time[1024*2];
    static integer hint;
    static char type__[3*2];
    static integer i__;
    extern /* Subroutine */ int dafgs_(doublereal *), kdata_(integer *, char *
	    , char *, char *, char *, integer *, logical *, ftnlen, ftnlen, 
	    ftnlen, ftnlen);
    static integer cfrid[2];
    extern /* Subroutine */ int ckobj_(char *, integer *, ftnlen);
    extern logical elemi_(integer *, integer *);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static char hline[1024];
    static doublereal descr[5];
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen), 
	    errch_(char *, char *, ftnlen, ftnlen), repmc_(char *, char *, 
	    char *, char *, ftnlen, ftnlen, ftnlen, ftnlen);
    extern doublereal dpmin_(void), dpmax_(void);
    static integer nargs;
    static logical found;
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen), dafus_(doublereal *, integer *, integer *
	    , doublereal *, integer *);
    extern integer wdcnt_(char *, ftnlen);
    extern /* Subroutine */ int copyd_(doublereal *, doublereal *);
    static char hword[32];
    extern /* Subroutine */ int nthwd_(char *, integer *, char *, integer *, 
	    ftnlen, ftnlen), errdp_(char *, doublereal *, ftnlen);
    static integer count;
    static char error[1024];
    extern integer rtrim_(char *, ftnlen);
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    static char hline2[1024];
    static integer flids1[106], flids2[106];
    static doublereal cover1[1000006], cover2[1000006], dc[2];
    static integer ic[6];
    extern /* Subroutine */ int dafbbs_(integer *), str2et_(char *, 
	    doublereal *, ftnlen), daffpa_(logical *);
    static doublereal et[2];
    static integer cfrcid[2], handle;
    static logical clflag[25];
    extern /* Subroutine */ int dafcls_(integer *), kclear_(void);
    static integer frcode;
    static logical cfrfnd[2];
    extern /* Subroutine */ int ccifrm_(integer *, integer *, integer *, char 
	    *, integer *, logical *, ftnlen);
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen), 
	    wncard_(doublereal *);
    extern logical exists_(char *, ftnlen);
    static char arctyp[7*2], hartyp[7], hlpmsg[80*28], usgmsg[80*33], vermsg[
	    80*3], clvals[1024*25], unprsd[1024], hlline[5120], hkrnam[1024], 
	    clkeyu[32*25], cfrnam[32*2], covdsc[1024*2], timdsc[1024];
    static doublereal covera[1000006], coverb[1000006], coverc[8];
    static integer tfrcid[2], tfrcls[2], cfrcls[2], clssid;
    static logical iscpck[2];
    extern /* Subroutine */ int tkvrsn_(char *, char *, ftnlen, ftnlen), 
	    parcml_(char *, integer *, char *, logical *, char *, logical *, 
	    char *, ftnlen, ftnlen, ftnlen, ftnlen), tostdo_(char *, ftnlen), 
	    getfat_(char *, char *, char *, ftnlen, ftnlen, ftnlen), setmsg_(
	    char *, ftnlen), sigerr_(char *, ftnlen), ssizei_(integer *, 
	    integer *), pckfrm_(char *, integer *, ftnlen), nextwd_(char *, 
	    char *, char *, ftnlen, ftnlen, ftnlen), ldklst_(char *, ftnlen), 
	    nparsi_(char *, integer *, char *, integer *, ftnlen, ftnlen), 
	    namfrm_(char *, integer *, ftnlen), frmnam_(integer *, char *, 
	    ftnlen), frinfo_(integer *, integer *, integer *, integer *, 
	    logical *), errint_(char *, integer *, ftnlen), dafopr_(char *, 
	    integer *, ftnlen), ssized_(integer *, doublereal *), scardd_(
	    integer *, doublereal *), ckcovr_(char *, integer *, logical *, 
	    char *, doublereal *, doublereal *, ftnlen, ftnlen), pckcov_(char 
	    *, integer *, doublereal *, ftnlen), ktotal_(char *, integer *, 
	    ftnlen), wnunid_(doublereal *, doublereal *, doublereal *), 
	    wninsd_(doublereal *, doublereal *, doublereal *), wnintd_(
	    doublereal *, doublereal *, doublereal *), nparsd_(char *, 
	    doublereal *, char *, integer *, ftnlen, ftnlen), wnfetd_(
	    doublereal *, integer *, doublereal *, doublereal *), ckmeta_(
	    integer *, char *, integer *, ftnlen), chkout_(char *, ftnlen);
    static integer ptr;
    static doublereal hdp1, hdp2;

/* $ Abstract */

/*     Extract arguments from FRMDIFF command line and return them */
/*     and/or default values via individual variables. If input command */
/*     line is incomplete or if specific command line keys requesting */
/*     help are specified, this routine displays usage and stops the */
/*     program. */

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

/*     FRMDIFF User's Guide. */

/* $ Keywords */

/*     TBD. */

/* $ Declarations */
/* $ Abstract */

/*     Include Section:  FRMDIFF Global Parameters */

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

/*     B.V. Semenov   (JPL) */

/* $ Version */

/* -    Version 2.1.0, 25-MAR-2014 (BVS). */

/*        Updated version. */

/* -    Version 2.0.0, 27-FEB-2012 (BVS). */

/*        Updated version. */

/* -    Version 1.0.0, 09-DEC-2008 (BVS). */

/* -& */

/*     Program name and version. */


/*     Command line keys. */


/*     Command line key values. */


/*     Max and min number orientations that the program can handle. */


/*     Default number orientations. */


/*     Maximum number of IDs in a CK or a binary PCK file */


/*     Line size parameters. */


/*     Version, help, usage and header display parameters. */


/*     DAF descriptor size and component counts. */


/*     Cell lower boundary. */


/*     Maximum allowed number of coverage windows. */


/*     Smallest allowed step. */


/*     Fraction of step to be used as pad at the end of intervals. */


/*     Default, minimum, and maximum numbers of significant digits */
/*     allowed for numbers in dump reports. */


/*     End of FRMDIFF parameters. */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     LINE       I   FRMDIFF command line */
/*     KERNAM     O   1st and 2nd kernel file names */
/*     FFRNAM     O   1st and 2nd ``from'' frame names */
/*     FFRID      O   1st and 2nd ``from'' frame IDs */
/*     TFRNAM     O   1st and 2nd ``to'' frame names */
/*     TFRID      O   1st and 2nd ``to'' frame IDs */
/*     AVFLG      O   Angular velocity comparison flag */
/*     AVFFLG     O   Angular velocity frame flag */
/*     CMPWIN     O   Time window for comparison */
/*     NITR       O   Number of points to be used in comparison. */
/*     STEP       O   Time step in seconds. */
/*     DIFTYP     O   Type of summary to be generated by FRMDIFF. */
/*     TIMFMT     O   Output time format string "dump" summaries. */
/*     KERNLS     O   Lists of additional kernels. */
/*     SCLKID     O   IDs for ET to SCLK conversions. */
/*     AXES       O   Rotation axes for output Euler angles. */
/*     AUNITS     O   Units for output Euler angles. */
/*     SIGDIG     O   Number of significant digits */

/* $ Detailed_Input */

/*     LINE        is the command line provided to FRMDIFF. See FRMDIFF */
/*                 User's Guide for the command line syntax and detailed */
/*                 specification of allowed arguments. */

/* $ Detailed_Output */

/*     KERNAM      is a two element array containing the names of the */
/*                 first and second kernel files. */

/*     FFRNAM      is a two element array containing the names of the */
/*                 first and second ``from'' frames. */

/*     FFRID       is a two element array containing the ID of the first */
/*                 and second ``from'' frames. */

/*     TFRNAM      is a two element array containing the names of the */
/*                 first and second ``to'' frames. */

/*     TFRID       is a two element array containing the IDs of the first */
/*                 and second ``to'' frames. */

/*     AVFLG       is a flag indicating whether angular velocities */
/*                 should be compared: .FALSE. for no, .TRUE. for yes. */

/*     AVFFLG      is a flag indicating whether angular velocities */
/*                 should be computed with respect to ``from'' frame */
/*                 (.FALSE.) or ``to'' frame (.TRUE.). */

/*     CMPWIN      is a time window for comparison. */

/*     NITR        is the number of points to be used in comparison. */

/*     STEP        is the time step in seconds. STEP is returned as zero */
/*                 if it was not provided on the command line. */

/*     DIFTYP      is a string indicating the type of summary to be */
/*                 generated by FRMDIFF. */

/*     TIMFMT      is a string containing output time format picture for */
/*                 "dump"-type summaries. */

/*     KERNLS      is an array of three strings containing the lists of */
/*                 additional kernels provided on the command line. */
/*                 KERNLS(1) contains the list applicable to the first */
/*                 kernel to be examined. KERNLS(2) contains the list */
/*                 applicable to the second kernel to be examined. */
/*                 KERNLS(3) contains the list applicable both kernels */
/*                 to be examined. */

/*     SCLKID      is a two element array containing ID to be used in */
/*                 SCLK routine calls for converting ET to SCLKs for */
/*                 output. Both elements are returned as zero if SCLK */
/*                 output was not requested using corresponding command */
/*                 line switch. If SCLK output was requested, only one */
/*                 of the elements will be set to SCLK ID, depending on */
/*                 which of the ``to''/coverage frames were CK frames. */
/*                 Which element is set will indicate which kernels */
/*                 should be loaded for SCLK conversions: if SCLK(1) is */
/*                 not zero, kernels applicable to first attitude set, */
/*                 if SCLK(2) is non zero, kernels applicable to second */
/*                 attitude set. */

/*     AXES        is a three element array specifying rotation axes for */
/*                 output Euler angles. */

/*     AUNITS      is a string specifying units for output Euler angles. */

/*     SIGDIG      is the number of significant digits in the output */
/*                 numbers in scientific format included in the dump */
/*                 reports. */

/* $ Parameters */

/*     See include file. */

/* $ Exceptions */

/*     TBD. */

/* $ Files */

/*     TBD. */

/* $ Particulars */

/*     TBD. */

/* $ Examples */

/*     TBD. */

/* $ Restrictions */

/*     TBD. */

/* $ Literature_References */

/*     TBD. */

/* $ Author_and_Institution */

/*     B.V. Semenov   (JPL) */

/* $ Version */

/* -    Version 2.0.0, 29-MAR-2012 (BVS). */

/*        Changed calling sequence to include additional SIGDIG output. */

/*        Changed fetch the number of significant digits specified with */
/*        DIGKEY. */

/*        Updated to fetch coverage from non-primary kernels when no */
/*        primary kernels were specified. */

/* -    Version 1.1.0, 28-OCT-2011 (BVS). */

/*        Moved PARCML to support. Updated its calling sequence. */

/* -    Version 1.0.0, 18-APR-2006 (BVS). */

/* -& */

/*     SPICELIB functions */


/*     Local variables. */


/*     Save everything to prevent potential memory problems in f2c'ed */
/*     version. */


/*     Initialize command line keys. */


/*     Check in. */

    chkin_("CHWCML", (ftnlen)6);

/*     Generate uppercase version of command lines keys. */

    for (i__ = 1; i__ <= 25; ++i__) {
	ucase_(clkeys + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge(
		"clkeys", i__1, "chwcml_", (ftnlen)344)) << 5), clkeyu + (((
		i__2 = i__ - 1) < 25 && 0 <= i__2 ? i__2 : s_rnge("clkeyu", 
		i__2, "chwcml_", (ftnlen)344)) << 5), (ftnlen)32, (ftnlen)32);
    }

/*     Initialize version display. */

    tkvrsn_("TOOLKIT", hword, (ftnlen)7, (ftnlen)32);
    s_copy(vermsg, " ", (ftnlen)80, (ftnlen)1);
/* Writing concatenation */
    i__3[0] = 60, a__1[0] = "frmdiff -- Version 2.1.0, March 25, 2014 -- Too"
	    "lkit Version ";
    i__3[1] = rtrim_(hword, (ftnlen)32), a__1[1] = hword;
    s_cat(vermsg + 80, a__1, i__3, &c__2, (ftnlen)80);
    s_copy(vermsg + 160, " ", (ftnlen)80, (ftnlen)1);

/*     Initialize help display. */

    s_copy(hlpmsg, "   # provides means for sampling orientation of a single"
	    " reference", (ftnlen)80, (ftnlen)66);
    s_copy(hlpmsg + 80, "   frame, or for comparing orientations of two refe"
	    "rence frames known to", (ftnlen)80, (ftnlen)72);
    s_copy(hlpmsg + 160, "   SPICE and supported by data from SPICE kernels.",
	     (ftnlen)80, (ftnlen)50);
    s_copy(hlpmsg + 240, " ", (ftnlen)80, (ftnlen)1);
    s_copy(hlpmsg + 320, "   To sample orientation of a single frame the pro"
	    "gram computes a set of", (ftnlen)80, (ftnlen)72);
    s_copy(hlpmsg + 400, "   transformations from one frame (the ``from'' fr"
	    "ame) to another frame", (ftnlen)80, (ftnlen)71);
    s_copy(hlpmsg + 480, "   (the ``to'' frame) over an interval of time wit"
	    "h fixed or variable time", (ftnlen)80, (ftnlen)74);
    s_copy(hlpmsg + 560, "   step, using a given set of kernel files. Then, "
	    "depending on the", (ftnlen)80, (ftnlen)66);
    s_copy(hlpmsg + 640, "   requested type of output report, it prints to t"
	    "he screen a table", (ftnlen)80, (ftnlen)67);
    s_copy(hlpmsg + 720, "   containing these transformations expressed as t"
	    "otal rotation angles and", (ftnlen)80, (ftnlen)74);
    s_copy(hlpmsg + 800, "   axes, quaternions, matrices or Euler angles and"
	    " angular velocities, or", (ftnlen)80, (ftnlen)73);
    s_copy(hlpmsg + 880, "   only magnitude of the maximum rotation and angu"
	    "lar velocity, or results", (ftnlen)80, (ftnlen)74);
    s_copy(hlpmsg + 960, "   of a simple statistical analysis of rotations.", 
	    (ftnlen)80, (ftnlen)49);
    s_copy(hlpmsg + 1040, " ", (ftnlen)80, (ftnlen)1);
    s_copy(hlpmsg + 1120, "   To compare orientations of two frames the prog"
	    "ram computes a set of", (ftnlen)80, (ftnlen)70);
    s_copy(hlpmsg + 1200, "   transformations from one frame (the ``from'' f"
	    "rame) to another frame", (ftnlen)80, (ftnlen)71);
    s_copy(hlpmsg + 1280, "   (the ``to'' frame) over an interval of time wi"
	    "th a fixed or variable", (ftnlen)80, (ftnlen)71);
    s_copy(hlpmsg + 1360, "   time step using one set of kernels, computes a"
	    " set of transformations", (ftnlen)80, (ftnlen)72);
    s_copy(hlpmsg + 1440, "   for the same or a different ``from''-``to'' fr"
	    "ame combination at the", (ftnlen)80, (ftnlen)71);
    s_copy(hlpmsg + 1520, "   same times using the same or a different set o"
	    "f kernels, and then", (ftnlen)80, (ftnlen)68);
    s_copy(hlpmsg + 1600, "   computes the difference in rotation and angula"
	    "r velocity between the", (ftnlen)80, (ftnlen)71);
    s_copy(hlpmsg + 1680, "   corresponding transformations. Depending on th"
	    "e requested type of", (ftnlen)80, (ftnlen)68);
    s_copy(hlpmsg + 1760, "   output report the program prints to the screen"
	    " only maximum differences", (ftnlen)80, (ftnlen)74);
    s_copy(hlpmsg + 1840, "   in rotation and angular velocity, or a complet"
	    "e table of rotation and", (ftnlen)80, (ftnlen)72);
    s_copy(hlpmsg + 1920, "   angular velocity differences expressed as tota"
	    "l rotation angles and", (ftnlen)80, (ftnlen)70);
    s_copy(hlpmsg + 2000, "   axes, matrices, quaternions, or Euler angles, "
	    "or results of a simple", (ftnlen)80, (ftnlen)71);
    s_copy(hlpmsg + 2080, "   statistical analysis of the rotation differenc"
	    "es.", (ftnlen)80, (ftnlen)52);
    s_copy(hlpmsg + 2160, " ", (ftnlen)80, (ftnlen)1);
    repmc_(hlpmsg, "#", "frmdiff", hlpmsg, (ftnlen)80, (ftnlen)1, (ftnlen)7, (
	    ftnlen)80);

/*     Initialize usage display. */

    s_copy(usgmsg, "   # provides a simple way of sampling orientation of a "
	    "frame or", (ftnlen)80, (ftnlen)64);
    s_copy(usgmsg + 80, "   comparing orientations of two frames known to SP"
	    "ICE and supported by", (ftnlen)80, (ftnlen)71);
    s_copy(usgmsg + 160, "   data from SPICE kernels. The program usage is:", 
	    (ftnlen)80, (ftnlen)49);
    s_copy(usgmsg + 240, " ", (ftnlen)80, (ftnlen)1);
    s_copy(usgmsg + 320, "      % # [options] <first kernel name> <second ke"
	    "rnel name>", (ftnlen)80, (ftnlen)60);
    s_copy(usgmsg + 400, "      % # [options] <kernel name>", (ftnlen)80, (
	    ftnlen)33);
    s_copy(usgmsg + 480, "      % # [options]", (ftnlen)80, (ftnlen)19);
    s_copy(usgmsg + 560, " ", (ftnlen)80, (ftnlen)1);
    s_copy(usgmsg + 640, "   where kernel can be a CK, an FK, or a PCK. Opti"
	    "ons are shown below.", (ftnlen)80, (ftnlen)70);
    s_copy(usgmsg + 720, "   Order and case of keys are not significant. Val"
	    "ues must be", (ftnlen)80, (ftnlen)61);
    s_copy(usgmsg + 800, "   space-separated from keys, i.e. '-n 10', not '-"
	    "n10'.", (ftnlen)80, (ftnlen)55);
    s_copy(usgmsg + 880, " ", (ftnlen)80, (ftnlen)1);
    s_copy(usgmsg + 960, "      #  <supporting kernel(s) name(s)>", (ftnlen)
	    80, (ftnlen)39);
    s_copy(usgmsg + 1040, "      # <first ``from'' frame, name or ID>", (
	    ftnlen)80, (ftnlen)42);
    s_copy(usgmsg + 1120, "      # <first ``to'' frame, name or ID>", (ftnlen)
	    80, (ftnlen)40);
    s_copy(usgmsg + 1200, "      # <first frame for coverage look up, name o"
	    "r ID>", (ftnlen)80, (ftnlen)54);
    s_copy(usgmsg + 1280, "      # <additional supporting kernel(s) for firs"
	    "t file>", (ftnlen)80, (ftnlen)56);
    s_copy(usgmsg + 1360, "      # <second ``from'' frame, name or ID>", (
	    ftnlen)80, (ftnlen)43);
    s_copy(usgmsg + 1440, "      # <second ``to'' frame, name or ID>", (
	    ftnlen)80, (ftnlen)41);
    s_copy(usgmsg + 1520, "      # <second frame for coverage look up, name "
	    "or ID>", (ftnlen)80, (ftnlen)55);
    s_copy(usgmsg + 1600, "      # <additional supporting kernel(s) for seco"
	    "nd file>", (ftnlen)80, (ftnlen)57);
    s_copy(usgmsg + 1680, "      #  <compare angular velocities: #|# (defaul"
	    "t: #)>", (ftnlen)80, (ftnlen)55);
    s_copy(usgmsg + 1760, "      #  <frame for angular velocities: #|# (defa"
	    "ult: #)>", (ftnlen)80, (ftnlen)57);
    s_copy(usgmsg + 1840, "      #  <interval start time>", (ftnlen)80, (
	    ftnlen)30);
    s_copy(usgmsg + 1920, "      #  <interval stop time>", (ftnlen)80, (
	    ftnlen)29);
    s_copy(usgmsg + 2000, "      #  <number of points: # to # (default: #)>", 
	    (ftnlen)80, (ftnlen)48);
    s_copy(usgmsg + 2080, "      #  <time step in seconds>", (ftnlen)80, (
	    ftnlen)31);
    s_copy(usgmsg + 2160, "      #  <time format: #|#|#|#|picture_for_TIMOUT"
	    " (default: #)>", (ftnlen)80, (ftnlen)63);
    s_copy(usgmsg + 2240, "      #  <report: #|#|#|#|#|#|#|#|#>", (ftnlen)80, 
	    (ftnlen)36);
    s_copy(usgmsg + 2320, "      #  <rotation axes order (default: # # #)>", (
	    ftnlen)80, (ftnlen)47);
    s_copy(usgmsg + 2400, "      #  <units for output angles> (only for # # "
	    "and # #)", (ftnlen)80, (ftnlen)57);
    s_copy(usgmsg + 2480, "      #  <number of significant digits: # to # (d"
	    "efault: #)>", (ftnlen)80, (ftnlen)60);
    s_copy(usgmsg + 2560, " ", (ftnlen)80, (ftnlen)1);
    repmc_(usgmsg, "#", "frmdiff", usgmsg, (ftnlen)80, (ftnlen)1, (ftnlen)7, (
	    ftnlen)80);
    repmc_(usgmsg + 320, "#", "frmdiff", usgmsg + 320, (ftnlen)80, (ftnlen)1, 
	    (ftnlen)7, (ftnlen)80);
    repmc_(usgmsg + 400, "#", "frmdiff", usgmsg + 400, (ftnlen)80, (ftnlen)1, 
	    (ftnlen)7, (ftnlen)80);
    repmc_(usgmsg + 480, "#", "frmdiff", usgmsg + 480, (ftnlen)80, (ftnlen)1, 
	    (ftnlen)7, (ftnlen)80);
    repmc_(usgmsg + 960, "#", "-k", usgmsg + 960, (ftnlen)80, (ftnlen)1, (
	    ftnlen)2, (ftnlen)80);
    repmc_(usgmsg + 1040, "#", "-f1", usgmsg + 1040, (ftnlen)80, (ftnlen)1, (
	    ftnlen)3, (ftnlen)80);
    repmc_(usgmsg + 1120, "#", "-t1", usgmsg + 1120, (ftnlen)80, (ftnlen)1, (
	    ftnlen)3, (ftnlen)80);
    repmc_(usgmsg + 1200, "#", "-c1", usgmsg + 1200, (ftnlen)80, (ftnlen)1, (
	    ftnlen)3, (ftnlen)80);
    repmc_(usgmsg + 1280, "#", "-k1", usgmsg + 1280, (ftnlen)80, (ftnlen)1, (
	    ftnlen)3, (ftnlen)80);
    repmc_(usgmsg + 1360, "#", "-f2", usgmsg + 1360, (ftnlen)80, (ftnlen)1, (
	    ftnlen)3, (ftnlen)80);
    repmc_(usgmsg + 1440, "#", "-t2", usgmsg + 1440, (ftnlen)80, (ftnlen)1, (
	    ftnlen)3, (ftnlen)80);
    repmc_(usgmsg + 1520, "#", "-c2", usgmsg + 1520, (ftnlen)80, (ftnlen)1, (
	    ftnlen)3, (ftnlen)80);
    repmc_(usgmsg + 1600, "#", "-k2", usgmsg + 1600, (ftnlen)80, (ftnlen)1, (
	    ftnlen)3, (ftnlen)80);
    repmc_(usgmsg + 1680, "#", "-a", usgmsg + 1680, (ftnlen)80, (ftnlen)1, (
	    ftnlen)2, (ftnlen)80);
    repmc_(usgmsg + 1760, "#", "-m", usgmsg + 1760, (ftnlen)80, (ftnlen)1, (
	    ftnlen)2, (ftnlen)80);
    repmc_(usgmsg + 1840, "#", "-b", usgmsg + 1840, (ftnlen)80, (ftnlen)1, (
	    ftnlen)2, (ftnlen)80);
    repmc_(usgmsg + 1920, "#", "-e", usgmsg + 1920, (ftnlen)80, (ftnlen)1, (
	    ftnlen)2, (ftnlen)80);
    repmc_(usgmsg + 2000, "#", "-n", usgmsg + 2000, (ftnlen)80, (ftnlen)1, (
	    ftnlen)2, (ftnlen)80);
    repmi_(usgmsg + 2000, "#", &c__1, usgmsg + 2000, (ftnlen)80, (ftnlen)1, (
	    ftnlen)80);
    repmi_(usgmsg + 2000, "#", &c_b114, usgmsg + 2000, (ftnlen)80, (ftnlen)1, 
	    (ftnlen)80);
    repmi_(usgmsg + 2000, "#", &c__1000, usgmsg + 2000, (ftnlen)80, (ftnlen)1,
	     (ftnlen)80);
    repmc_(usgmsg + 2080, "#", "-s", usgmsg + 2080, (ftnlen)80, (ftnlen)1, (
	    ftnlen)2, (ftnlen)80);
    repmc_(usgmsg + 2160, "#", "-f", usgmsg + 2160, (ftnlen)80, (ftnlen)1, (
	    ftnlen)2, (ftnlen)80);
    repmc_(usgmsg + 2240, "#", "-t", usgmsg + 2240, (ftnlen)80, (ftnlen)1, (
	    ftnlen)2, (ftnlen)80);
    repmc_(usgmsg + 2320, "#", "-o", usgmsg + 2320, (ftnlen)80, (ftnlen)1, (
	    ftnlen)2, (ftnlen)80);
    repmc_(usgmsg + 2400, "#", "-x", usgmsg + 2400, (ftnlen)80, (ftnlen)1, (
	    ftnlen)2, (ftnlen)80);
    repmc_(usgmsg + 1680, "#", "yes", usgmsg + 1680, (ftnlen)80, (ftnlen)1, (
	    ftnlen)3, (ftnlen)80);
    repmc_(usgmsg + 1680, "#", "no", usgmsg + 1680, (ftnlen)80, (ftnlen)1, (
	    ftnlen)2, (ftnlen)80);
    repmc_(usgmsg + 1680, "#", "no", usgmsg + 1680, (ftnlen)80, (ftnlen)1, (
	    ftnlen)2, (ftnlen)80);
    repmc_(usgmsg + 1760, "#", "from", usgmsg + 1760, (ftnlen)80, (ftnlen)1, (
	    ftnlen)4, (ftnlen)80);
    repmc_(usgmsg + 1760, "#", "to", usgmsg + 1760, (ftnlen)80, (ftnlen)1, (
	    ftnlen)2, (ftnlen)80);
    repmc_(usgmsg + 1760, "#", "from", usgmsg + 1760, (ftnlen)80, (ftnlen)1, (
	    ftnlen)4, (ftnlen)80);
    repmc_(usgmsg + 2160, "#", "et", usgmsg + 2160, (ftnlen)80, (ftnlen)1, (
	    ftnlen)2, (ftnlen)80);
    repmc_(usgmsg + 2160, "#", "sclk", usgmsg + 2160, (ftnlen)80, (ftnlen)1, (
	    ftnlen)4, (ftnlen)80);
    repmc_(usgmsg + 2160, "#", "sclkd", usgmsg + 2160, (ftnlen)80, (ftnlen)1, 
	    (ftnlen)5, (ftnlen)80);
    repmc_(usgmsg + 2160, "#", "ticks", usgmsg + 2160, (ftnlen)80, (ftnlen)1, 
	    (ftnlen)5, (ftnlen)80);
    repmc_(usgmsg + 2160, "#", "et", usgmsg + 2160, (ftnlen)80, (ftnlen)1, (
	    ftnlen)2, (ftnlen)80);
    repmc_(usgmsg + 2160, "#", "et", usgmsg + 2160, (ftnlen)80, (ftnlen)1, (
	    ftnlen)2, (ftnlen)80);
    repmc_(usgmsg + 2240, "#", "basic", usgmsg + 2240, (ftnlen)80, (ftnlen)1, 
	    (ftnlen)5, (ftnlen)80);
    repmc_(usgmsg + 2240, "#", "stats", usgmsg + 2240, (ftnlen)80, (ftnlen)1, 
	    (ftnlen)5, (ftnlen)80);
    repmc_(usgmsg + 2240, "#", "dumpaa", usgmsg + 2240, (ftnlen)80, (ftnlen)1,
	     (ftnlen)6, (ftnlen)80);
    repmc_(usgmsg + 2240, "#", "dumpm", usgmsg + 2240, (ftnlen)80, (ftnlen)1, 
	    (ftnlen)5, (ftnlen)80);
    repmc_(usgmsg + 2240, "#", "dumpqs", usgmsg + 2240, (ftnlen)80, (ftnlen)1,
	     (ftnlen)6, (ftnlen)80);
    repmc_(usgmsg + 2240, "#", "dumpqo", usgmsg + 2240, (ftnlen)80, (ftnlen)1,
	     (ftnlen)6, (ftnlen)80);
    repmc_(usgmsg + 2240, "#", "dumpea", usgmsg + 2240, (ftnlen)80, (ftnlen)1,
	     (ftnlen)6, (ftnlen)80);
    repmc_(usgmsg + 2240, "#", "dumpc", usgmsg + 2240, (ftnlen)80, (ftnlen)1, 
	    (ftnlen)5, (ftnlen)80);
    repmc_(usgmsg + 2240, "#", "dumpg", usgmsg + 2240, (ftnlen)80, (ftnlen)1, 
	    (ftnlen)5, (ftnlen)80);
    repmc_(usgmsg + 2240, "#", "basic", usgmsg + 2240, (ftnlen)80, (ftnlen)1, 
	    (ftnlen)5, (ftnlen)80);
    repmc_(usgmsg + 2320, "#", "z", usgmsg + 2320, (ftnlen)80, (ftnlen)1, (
	    ftnlen)1, (ftnlen)80);
    repmc_(usgmsg + 2320, "#", "y", usgmsg + 2320, (ftnlen)80, (ftnlen)1, (
	    ftnlen)1, (ftnlen)80);
    repmc_(usgmsg + 2320, "#", "x", usgmsg + 2320, (ftnlen)80, (ftnlen)1, (
	    ftnlen)1, (ftnlen)80);
    repmc_(usgmsg + 2400, "#", "-t", usgmsg + 2400, (ftnlen)80, (ftnlen)1, (
	    ftnlen)2, (ftnlen)80);
    repmc_(usgmsg + 2400, "#", "dumpaa", usgmsg + 2400, (ftnlen)80, (ftnlen)1,
	     (ftnlen)6, (ftnlen)80);
    repmc_(usgmsg + 2400, "#", "-t", usgmsg + 2400, (ftnlen)80, (ftnlen)1, (
	    ftnlen)2, (ftnlen)80);
    repmc_(usgmsg + 2400, "#", "dumpea", usgmsg + 2400, (ftnlen)80, (ftnlen)1,
	     (ftnlen)6, (ftnlen)80);
    repmc_(usgmsg + 2480, "#", "-d", usgmsg + 2480, (ftnlen)80, (ftnlen)1, (
	    ftnlen)2, (ftnlen)80);
    repmi_(usgmsg + 2480, "#", &c__6, usgmsg + 2480, (ftnlen)80, (ftnlen)1, (
	    ftnlen)80);
    repmi_(usgmsg + 2480, "#", &c__17, usgmsg + 2480, (ftnlen)80, (ftnlen)1, (
	    ftnlen)80);
    repmi_(usgmsg + 2480, "#", &c__14, usgmsg + 2480, (ftnlen)80, (ftnlen)1, (
	    ftnlen)80);

/*     Get command line and do first attempt at parsing. All we need to */
/*     find out in this try is if one of the help/usage/version key */
/*     variations was present. */

    s_copy(hline, line, (ftnlen)1024, line_len);
    parcml_(hline, &c__25, clkeyu, clflag, clvals, &found, unprsd, (ftnlen)
	    1024, (ftnlen)32, (ftnlen)1024, (ftnlen)1024);

/*     Was command line blank? Is one of the version, usage, or help */
/*     keys present? Display usage, help, or version and stop. */

    nargs = wdcnt_(line, line_len);
    if (nargs == 0 || clflag[(i__1 = isrchc_("-v", &c__25, clkeys, (ftnlen)2, 
	    (ftnlen)32) - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge("clflag", i__1,
	     "chwcml_", (ftnlen)550)] || clflag[(i__2 = isrchc_("-help", &
	    c__25, clkeys, (ftnlen)5, (ftnlen)32) - 1) < 25 && 0 <= i__2 ? 
	    i__2 : s_rnge("clflag", i__2, "chwcml_", (ftnlen)550)] || clflag[(
	    i__4 = isrchc_("-h", &c__25, clkeys, (ftnlen)2, (ftnlen)32) - 1) <
	     25 && 0 <= i__4 ? i__4 : s_rnge("clflag", i__4, "chwcml_", (
	    ftnlen)550)] || clflag[(i__5 = isrchc_("-usage", &c__25, clkeys, (
	    ftnlen)6, (ftnlen)32) - 1) < 25 && 0 <= i__5 ? i__5 : s_rnge(
	    "clflag", i__5, "chwcml_", (ftnlen)550)] || clflag[(i__6 = 
	    isrchc_("-u", &c__25, clkeys, (ftnlen)2, (ftnlen)32) - 1) < 25 && 
	    0 <= i__6 ? i__6 : s_rnge("clflag", i__6, "chwcml_", (ftnlen)550)]
	    ) {

/*        Display version in all cases. */

	for (i__ = 1; i__ <= 3; ++i__) {
	    tostdo_(vermsg + ((i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : 
		    s_rnge("vermsg", i__1, "chwcml_", (ftnlen)561)) * 80, (
		    ftnlen)80);
	}

/*        Depending on what other information was requested, display */
/*        usage, help, or nothing and stop. */

	if (nargs == 0 || clflag[(i__1 = isrchc_("-usage", &c__25, clkeys, (
		ftnlen)6, (ftnlen)32) - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge(
		"clflag", i__1, "chwcml_", (ftnlen)568)] || clflag[(i__2 = 
		isrchc_("-u", &c__25, clkeys, (ftnlen)2, (ftnlen)32) - 1) < 
		25 && 0 <= i__2 ? i__2 : s_rnge("clflag", i__2, "chwcml_", (
		ftnlen)568)]) {
	    for (i__ = 1; i__ <= 33; ++i__) {
		tostdo_(usgmsg + ((i__1 = i__ - 1) < 33 && 0 <= i__1 ? i__1 : 
			s_rnge("usgmsg", i__1, "chwcml_", (ftnlen)573)) * 80, 
			(ftnlen)80);
	    }
	} else if (clflag[(i__1 = isrchc_("-help", &c__25, clkeys, (ftnlen)5, 
		(ftnlen)32) - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge("clflag", 
		i__1, "chwcml_", (ftnlen)576)] || clflag[(i__2 = isrchc_(
		"-h", &c__25, clkeys, (ftnlen)2, (ftnlen)32) - 1) < 25 && 0 <=
		 i__2 ? i__2 : s_rnge("clflag", i__2, "chwcml_", (ftnlen)576)]
		) {
	    for (i__ = 1; i__ <= 28; ++i__) {
		tostdo_(hlpmsg + ((i__1 = i__ - 1) < 28 && 0 <= i__1 ? i__1 : 
			s_rnge("hlpmsg", i__1, "chwcml_", (ftnlen)581)) * 80, 
			(ftnlen)80);
	    }
	}
	s_stop("", (ftnlen)0);
    }

/*     Pull the last word from the command line and see if it's the name */
/*     of a file. If it is, check if it's one of the kernel files that */
/*     may be pulled in by the frames subsystem (Hint: any DAF or KPL */
/*     file can be while DAS and all others can't.) */

    nthwd_(line, &nargs, kernam + kernam_len, &ptr, line_len, kernam_len);
    if (exists_(kernam + kernam_len, kernam_len)) {
	getfat_(kernam + kernam_len, arch + 3, type__ + 3, kernam_len, (
		ftnlen)3, (ftnlen)3);
	if (s_cmp(arch + 3, "?", (ftnlen)3, (ftnlen)1) == 0) {
	    setmsg_("File '#' specified as the last argument on the command "
		    "line is not a SPICE kernel file.", (ftnlen)87);
	    errch_("#", kernam + kernam_len, (ftnlen)1, kernam_len);
	    sigerr_("SPICE(NOTAKERNELFILE2)", (ftnlen)22);
	} else if (! (s_cmp(arch + 3, "DAF", (ftnlen)3, (ftnlen)3) == 0 || 
		s_cmp(arch + 3, "KPL", (ftnlen)3, (ftnlen)3) == 0)) {
	    setmsg_("File '#' specified as the last argument on the command "
		    "line is not one of the kernel types that can be given as"
		    " the last or second to last argument to this program. Th"
		    "is file's architecture/type were '#/#'.", (ftnlen)206);
	    errch_("#", kernam + kernam_len, (ftnlen)1, kernam_len);
	    errch_("#", arch + 3, (ftnlen)1, (ftnlen)3);
	    errch_("#", type__ + 3, (ftnlen)1, (ftnlen)3);
	    sigerr_("SPICE(NONAPPLICABLETYPE2)", (ftnlen)25);
	}
/* Writing concatenation */
	i__7[0] = 3, a__2[0] = arch + 3;
	i__7[1] = 1, a__2[1] = "/";
	i__7[2] = 3, a__2[2] = type__ + 3;
	s_cat(arctyp + 7, a__2, i__7, &c__3, (ftnlen)7);

/*        OK, it looks like the last item on the command line was a */
/*        file that we can process. Let's chop it off and check the */
/*        second to last item. */

	s_copy(line + (ptr - 1), " ", line_len - (ptr - 1), (ftnlen)1);
	nargs = wdcnt_(line, line_len);
	if (nargs != 0) {
	    nthwd_(line, &nargs, kernam, &ptr, line_len, kernam_len);
	    if (exists_(kernam, kernam_len)) {
		getfat_(kernam, arch, type__, kernam_len, (ftnlen)3, (ftnlen)
			3);
		if (s_cmp(arch, "?", (ftnlen)3, (ftnlen)1) == 0) {
		    setmsg_("File '#' specified as the second to last argume"
			    "nt on the command line is not a SPICE kernel fil"
			    "e.", (ftnlen)97);
		    errch_("#", kernam, (ftnlen)1, kernam_len);
		    sigerr_("SPICE(NOTAKERNELFILE1)", (ftnlen)22);
		} else if (! (s_cmp(arch, "DAF", (ftnlen)3, (ftnlen)3) == 0 ||
			 s_cmp(arch, "KPL", (ftnlen)3, (ftnlen)3) == 0)) {
		    setmsg_("File '#' specified as the second to last  argum"
			    "ent on the command line is not one of the kernel"
			    " types that can be given as the last or second t"
			    "o last argument to this program. This file's arc"
			    "hitecture/type were '#/#'.", (ftnlen)217);
		    errch_("#", kernam, (ftnlen)1, kernam_len);
		    errch_("#", arch, (ftnlen)1, (ftnlen)3);
		    errch_("#", type__, (ftnlen)1, (ftnlen)3);
		    sigerr_("SPICE(NONAPPLICABLETYPE1)", (ftnlen)25);
		}
/* Writing concatenation */
		i__7[0] = 3, a__2[0] = arch;
		i__7[1] = 1, a__2[1] = "/";
		i__7[2] = 3, a__2[2] = type__;
		s_cat(arctyp, a__2, i__7, &c__3, (ftnlen)7);

/*              Aren't we lucky?! The second to last item on the command */
/*              line is also a kernel that we can process. Wipe it off */
/*              the tail of the line before moving on. */

		s_copy(line + (ptr - 1), " ", line_len - (ptr - 1), (ftnlen)1)
			;
		nargs = wdcnt_(line, line_len);
	    } else {
		s_copy(kernam, " ", kernam_len, (ftnlen)1);
		s_copy(arctyp, " ", (ftnlen)7, (ftnlen)1);
	    }
	}
    } else {
	s_copy(kernam, " ", kernam_len, (ftnlen)1);
	s_copy(arctyp, " ", (ftnlen)7, (ftnlen)1);
	s_copy(kernam + kernam_len, " ", kernam_len, (ftnlen)1);
	s_copy(arctyp + 7, " ", (ftnlen)7, (ftnlen)1);
    }

/*     If the files are CKs or PCKs, fetch IDs for later use. */

    iscpck[0] = FALSE_;
    iscpck[1] = FALSE_;
    ssizei_(&c__100, flids1);
    ssizei_(&c__100, flids2);
    if (s_cmp(arctyp, "DAF/CK", (ftnlen)7, (ftnlen)6) == 0) {
	ckobj_(kernam, flids1, kernam_len);
	iscpck[0] = TRUE_;
    }
    if (s_cmp(arctyp + 7, "DAF/CK", (ftnlen)7, (ftnlen)6) == 0) {
	ckobj_(kernam + kernam_len, flids2, kernam_len);
	iscpck[1] = TRUE_;
    }
    if (s_cmp(arctyp, "DAF/PCK", (ftnlen)7, (ftnlen)7) == 0) {
	pckfrm_(kernam, flids1, kernam_len);
	iscpck[0] = TRUE_;
    }
    if (s_cmp(arctyp + 7, "DAF/PCK", (ftnlen)7, (ftnlen)7) == 0) {
	pckfrm_(kernam + kernam_len, flids2, kernam_len);
	iscpck[1] = TRUE_;
    }

/*     Parse the command line again because one or two last items may */
/*     have been removed from it. */

    s_copy(hline, line, (ftnlen)1024, line_len);
    parcml_(hline, &c__25, clkeyu, clflag, clvals, &found, unprsd, (ftnlen)
	    1024, (ftnlen)32, (ftnlen)1024, (ftnlen)1024);

/*     Go on processing the rest of the command line. All other */
/*     arguments are optional and, if not present, will have to be set */
/*     to some default values. */


/*     First, get additional kernels provided on the command line */
/*     applicable to both files and/or each specific file. We need to */
/*     deal with them first because some of them may define frames that */
/*     are needed to process the rest of command line arguments. Start */
/*     with looking for kernels applicable to the first attitude set. */

    i__ = isrchc_("-k1", &c__25, clkeys, (ftnlen)3, (ftnlen)32);
    if (clflag[(i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge("clflag", 
	    i__1, "chwcml_", (ftnlen)741)]) {
	if (s_cmp(clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : 
		s_rnge("clvals", i__1, "chwcml_", (ftnlen)743)) << 10), " ", (
		ftnlen)1024, (ftnlen)1) != 0) {
	    s_copy(kernls, clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? 
		    i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)745)) << 
		    10), kernls_len, (ftnlen)1024);
	    s_copy(hline, kernls, (ftnlen)1024, kernls_len);
	    while(s_cmp(hline, " ", (ftnlen)1024, (ftnlen)1) != 0) {
		nextwd_(hline, hline2, hline, (ftnlen)1024, (ftnlen)1024, (
			ftnlen)1024);
		if (! exists_(hline2, (ftnlen)1024)) {
		    setmsg_("File '#' listed after '#' key does not exist.", (
			    ftnlen)45);
		    errch_("#", hline2, (ftnlen)1, (ftnlen)1024);
		    errch_("#", "-k1", (ftnlen)1, (ftnlen)3);
		    sigerr_("SPICE(FILEDOESNTEXIST1)", (ftnlen)23);
		}
	    }
	} else {
	    setmsg_("Although '#' key was provided on the command line no ke"
		    "rnel file names were following it.", (ftnlen)89);
	    errch_("#", "-k1", (ftnlen)1, (ftnlen)3);
	    sigerr_("SPICE(MISSINGFILENAMES1)", (ftnlen)24);
	}
    } else {
	s_copy(kernls, " ", kernls_len, (ftnlen)1);
    }

/*     Second, look for kernels applicable to the second attitude set. */

    i__ = isrchc_("-k2", &c__25, clkeys, (ftnlen)3, (ftnlen)32);
    if (clflag[(i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge("clflag", 
	    i__1, "chwcml_", (ftnlen)782)]) {
	if (s_cmp(clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : 
		s_rnge("clvals", i__1, "chwcml_", (ftnlen)784)) << 10), " ", (
		ftnlen)1024, (ftnlen)1) != 0) {
	    s_copy(kernls + kernls_len, clvals + (((i__1 = i__ - 1) < 25 && 0 
		    <= i__1 ? i__1 : s_rnge("clvals", i__1, "chwcml_", (
		    ftnlen)786)) << 10), kernls_len, (ftnlen)1024);
	    s_copy(hline, kernls + kernls_len, (ftnlen)1024, kernls_len);
	    while(s_cmp(hline, " ", (ftnlen)1024, (ftnlen)1) != 0) {
		nextwd_(hline, hline2, hline, (ftnlen)1024, (ftnlen)1024, (
			ftnlen)1024);
		if (! exists_(hline2, (ftnlen)1024)) {
		    setmsg_("File '#' listed after '#' key does not exist.", (
			    ftnlen)45);
		    errch_("#", hline2, (ftnlen)1, (ftnlen)1024);
		    errch_("#", "-k2", (ftnlen)1, (ftnlen)3);
		    sigerr_("SPICE(FILEDOESNTEXIST2)", (ftnlen)23);
		}
	    }
	} else {
	    setmsg_("Although '#' key was provided on the command line no ke"
		    "rnel file names were following it.", (ftnlen)89);
	    errch_("#", "-k2", (ftnlen)1, (ftnlen)3);
	    sigerr_("SPICE(MISSINGFILENAMES2)", (ftnlen)24);
	}
    } else {
	s_copy(kernls + kernls_len, " ", kernls_len, (ftnlen)1);
    }

/*     Last, look for kernels applicable to both attitude sets. */

    i__ = isrchc_("-k", &c__25, clkeys, (ftnlen)2, (ftnlen)32);
    if (clflag[(i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge("clflag", 
	    i__1, "chwcml_", (ftnlen)823)]) {
	if (s_cmp(clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : 
		s_rnge("clvals", i__1, "chwcml_", (ftnlen)825)) << 10), " ", (
		ftnlen)1024, (ftnlen)1) != 0) {
	    s_copy(kernls + (kernls_len << 1), clvals + (((i__1 = i__ - 1) < 
		    25 && 0 <= i__1 ? i__1 : s_rnge("clvals", i__1, "chwcml_",
		     (ftnlen)827)) << 10), kernls_len, (ftnlen)1024);
	    s_copy(hline, kernls + (kernls_len << 1), (ftnlen)1024, 
		    kernls_len);
	    while(s_cmp(hline, " ", (ftnlen)1024, (ftnlen)1) != 0) {
		nextwd_(hline, hline2, hline, (ftnlen)1024, (ftnlen)1024, (
			ftnlen)1024);
		if (! exists_(hline2, (ftnlen)1024)) {
		    setmsg_("File '#' listed after '#' key does not exist.", (
			    ftnlen)45);
		    errch_("#", hline2, (ftnlen)1, (ftnlen)1024);
		    errch_("#", "-k", (ftnlen)1, (ftnlen)2);
		    sigerr_("SPICE(FILEDOESNTEXIST3)", (ftnlen)23);
		}
	    }
	} else {
	    setmsg_("Although '#' key was provided on the command line no ke"
		    "rnel file names were following it.", (ftnlen)89);
	    errch_("#", "-k", (ftnlen)1, (ftnlen)2);
	    sigerr_("SPICE(MISSINGFILENAMES3)", (ftnlen)24);
	}
    } else {
	s_copy(kernls + (kernls_len << 1), " ", kernls_len, (ftnlen)1);
    }

/*     Do a sanity check to verify that we have enough kernels to do */
/*     anything. We will attempt to do something only if at least one of */
/*     the kernels or kernel lists is non-blank. */

/*     This check is currently inactive to allow comparisons with */
/*     only built-in frames. */

/*      IF ( .NOT. ( KERNAM(1) .NE. ' ' .OR. */
/*     .             KERNAM(2) .NE. ' ' .OR. */
/*     .             KERNLS(1) .NE. ' ' .OR. */
/*     .             KERNLS(2) .NE. ' ' .OR. */
/*     .             KERNLS(3) .NE. ' '       )  ) THEN */

/*            CALL SETMSG ( 'No kernels were provided on the '         // */
/*     .                    'line.'                                    ) */
/*            CALL SIGERR ( 'SPICE(NODATA)'                            ) */

/*      END IF */


/*     If the first kernel is provided, load it. If not but the */
/*     second kernel is provided, load the second kernel (recall the */
/*     program can work on one kernel). Also load additional kernels */
/*     supplied for the first kernel and kernels applicable for both */
/*     kernels. */

    if (s_cmp(kernam, " ", kernam_len, (ftnlen)1) != 0) {
/* Writing concatenation */
	i__8[0] = kernls_len, a__3[0] = kernls + (kernls_len << 1);
	i__8[1] = 1, a__3[1] = " ";
	i__8[2] = kernls_len, a__3[2] = kernls;
	i__8[3] = 1, a__3[3] = " ";
	i__8[4] = kernam_len, a__3[4] = kernam;
	s_cat(hlline, a__3, i__8, &c__5, (ftnlen)5120);
	ldklst_(hlline, (ftnlen)5120);
    } else if (s_cmp(kernam + kernam_len, " ", kernam_len, (ftnlen)1) != 0) {
/* Writing concatenation */
	i__8[0] = kernls_len, a__3[0] = kernls + (kernls_len << 1);
	i__8[1] = 1, a__3[1] = " ";
	i__8[2] = kernls_len, a__3[2] = kernls;
	i__8[3] = 1, a__3[3] = " ";
	i__8[4] = kernam_len, a__3[4] = kernam + kernam_len;
	s_cat(hlline, a__3, i__8, &c__5, (ftnlen)5120);
	ldklst_(hlline, (ftnlen)5120);
    } else {
/* Writing concatenation */
	i__7[0] = kernls_len, a__2[0] = kernls + (kernls_len << 1);
	i__7[1] = 1, a__2[1] = " ";
	i__7[2] = kernls_len, a__2[2] = kernls;
	s_cat(hlline, a__2, i__7, &c__3, (ftnlen)5120);
	ldklst_(hlline, (ftnlen)5120);
    }

/*     After loading all kernels provided for computing the first */
/*     attitude set we can move on to getting command line inputs */
/*     defining it. */

/*     Start with getting the first ``from'' frame. */

    s_copy(ffrnam, " ", ffrnam_len, (ftnlen)1);
    ffrid[0] = 0;
    i__ = isrchc_("-f1", &c__25, clkeys, (ftnlen)3, (ftnlen)32);
    if (clflag[(i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge("clflag", 
	    i__1, "chwcml_", (ftnlen)910)]) {

/*        The first ``from'' frame was provided on the command line. Was */
/*        it given as a name or as an ID? Try to convert it to integer */
/*        as see if it worked. */

	s_copy(ffrnam, clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : 
		s_rnge("clvals", i__1, "chwcml_", (ftnlen)917)) << 10), 
		ffrnam_len, (ftnlen)1024);
	nparsi_(clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge(
		"clvals", i__1, "chwcml_", (ftnlen)918)) << 10), ffrid, error,
		 &ptr, (ftnlen)1024, (ftnlen)1024);
	if (ptr != 0) {

/*           It's not an integer. Assume that it's the frame name and */
/*           try to convert it to ID. If we can't, report an error. */

	    namfrm_(clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : 
		    s_rnge("clvals", i__1, "chwcml_", (ftnlen)926)) << 10), 
		    ffrid, (ftnlen)1024);
	    if (ffrid[0] == 0) {
		setmsg_("'#' specified after '#' key is neither an integer n"
			"umber representing a legitimate frame ID nor a frame"
			" name recognized in SPICE.", (ftnlen)129);
		errch_("#", clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? 
			i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)934))
			 << 10), (ftnlen)1, (ftnlen)1024);
		errch_("#", "-f1", (ftnlen)1, (ftnlen)3);
		sigerr_("SPICE(BADFROMFRAME1SP1)", (ftnlen)23);
	    }
	} else {

/*           It's an integer. Assume that it's the frame ID and try to */
/*           convert it to name. If we can't, report an error. */

	    frmnam_(ffrid, ffrnam, ffrnam_len);
	    if (s_cmp(ffrnam, " ", ffrnam_len, (ftnlen)1) == 0) {
		setmsg_("'#' specified after '#' key is neither an integer n"
			"umber representing a legitimate frame ID nor a frame"
			" name recognized in SPICE.", (ftnlen)129);
		errch_("#", clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? 
			i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)953))
			 << 10), (ftnlen)1, (ftnlen)1024);
		errch_("#", "-f1", (ftnlen)1, (ftnlen)3);
		sigerr_("SPICE(BADFROMFRAME1SP2)", (ftnlen)23);
	    }
	}
    }

/*     Next, get the first ``to'' frame. */

    s_copy(tfrnam, " ", tfrnam_len, (ftnlen)1);
    tfrid[0] = 0;
    tfrcid[0] = 0;
    tfrcls[0] = 0;
    i__ = isrchc_("-t1", &c__25, clkeys, (ftnlen)3, (ftnlen)32);
    if (clflag[(i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge("clflag", 
	    i__1, "chwcml_", (ftnlen)972)]) {

/*        The first ``to'' frame was provided on the command line. Was */
/*        it given as a name or as an ID? Try to convert it to integer */
/*        and see if it worked. */

	s_copy(tfrnam, clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : 
		s_rnge("clvals", i__1, "chwcml_", (ftnlen)979)) << 10), 
		tfrnam_len, (ftnlen)1024);
	nparsi_(clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge(
		"clvals", i__1, "chwcml_", (ftnlen)980)) << 10), tfrid, error,
		 &ptr, (ftnlen)1024, (ftnlen)1024);
	if (ptr != 0) {

/*           It's not an integer. Assume that it's the frame name and */
/*           try to convert it to ID. If we can't, report an error. */

	    namfrm_(clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : 
		    s_rnge("clvals", i__1, "chwcml_", (ftnlen)988)) << 10), 
		    tfrid, (ftnlen)1024);
	    if (tfrid[0] == 0) {
		setmsg_("'#' specified after '#' key is neither an integer n"
			"umber representing a legitimate frame ID nor a frame"
			" name recognized in SPICE.", (ftnlen)129);
		errch_("#", clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? 
			i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)996))
			 << 10), (ftnlen)1, (ftnlen)1024);
		errch_("#", "-t1", (ftnlen)1, (ftnlen)3);
		sigerr_("SPICE(BADTOFRAME1SPEC1)", (ftnlen)23);
	    }
	} else {

/*           It's an integer; treat it as a frame ID or a frame class ID */
/*           depending on context and try to map it to frame name. */

/*           If the first or only file is a CK and this ID is one of the */
/*           IDs present in it, treat it as a CK class ID and try to map */
/*           it to frame as such. */

/*           If the first or only file is a binary PCK and this ID is */
/*           one of the IDs present in it, treat it as a PCK class ID */
/*           and try to map it to frame as such. */

/*           If the first or only file is not a CK or a binary PCK, or */
/*           if it is BUT the ID could not be mapped to frame as a class */
/*           ID, try to map it to frame by treating it as a frame ID. If */
/*           it can't be mapped, report an error. */

	    if (s_cmp(kernam, " ", kernam_len, (ftnlen)1) != 0) {
		s_copy(hartyp, arctyp, (ftnlen)7, (ftnlen)7);
		found = elemi_(tfrid, flids1);
	    } else if (s_cmp(kernam + kernam_len, " ", kernam_len, (ftnlen)1) 
		    != 0) {
		s_copy(hartyp, arctyp + 7, (ftnlen)7, (ftnlen)7);
		found = elemi_(tfrid, flids2);
	    } else {
		s_copy(hartyp, " ", (ftnlen)7, (ftnlen)1);
		found = FALSE_;
	    }
	    if (s_cmp(hartyp, "DAF/CK", (ftnlen)7, (ftnlen)6) == 0 && found) {
		ccifrm_(&c__3, tfrid, &frcode, tfrnam, &hint, &found, 
			tfrnam_len);
	    } else if (s_cmp(hartyp, "DAF/PCK", (ftnlen)7, (ftnlen)7) == 0 && 
		    found) {
		ccifrm_(&c__2, tfrid, &frcode, tfrnam, &hint, &found, 
			tfrnam_len);
	    }
	    if (found) {
		tfrcid[0] = tfrid[0];
		tfrid[0] = frcode;
	    } else {
		s_copy(tfrnam, " ", tfrnam_len, (ftnlen)1);
	    }
	    if (s_cmp(tfrnam, " ", tfrnam_len, (ftnlen)1) == 0) {
		frmnam_(tfrid, tfrnam, tfrnam_len);
		if (s_cmp(tfrnam, " ", tfrnam_len, (ftnlen)1) == 0) {
		    setmsg_("'#' specified after '#' key is neither an integ"
			    "er number representing a legitimate frame ID nor"
			    " a frame name recognized in SPICE.", (ftnlen)129);
		    errch_("#", clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 
			    ? i__1 : s_rnge("clvals", i__1, "chwcml_", (
			    ftnlen)1058)) << 10), (ftnlen)1, (ftnlen)1024);
		    errch_("#", "-t1", (ftnlen)1, (ftnlen)3);
		    sigerr_("SPICE(BADTOFRAME1SPEC2)", (ftnlen)23);
		}
	    }
	}

/*        If we are here, we were able to resolve this frame. Get and */
/*        buffer its class and class ID; we may need these later */
/*        to set the coverage frame. */

	frinfo_(tfrid, &hint, tfrcls, tfrcid, &found);
    }

/*     Next, get the first ``coverage'' frame name or class ID. */

    s_copy(cfrnam, " ", (ftnlen)32, (ftnlen)1);
    cfrcid[0] = 0;
    cfrcls[0] = 0;
    cfrfnd[0] = FALSE_;
    i__ = isrchc_("-c1", &c__25, clkeys, (ftnlen)3, (ftnlen)32);
    if (clflag[(i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge("clflag", 
	    i__1, "chwcml_", (ftnlen)1086)] && (s_cmp(kernam, " ", kernam_len,
	     (ftnlen)1) != 0 && iscpck[0] || s_cmp(kernam, " ", kernam_len, (
	    ftnlen)1) == 0 && iscpck[1])) {

/*        This case -- specifying the first coverage frame and providing */
/*        the first or only primary file that is CK or PCK -- was used */
/*        in the version 1.0.0 of the program which did not allow */
/*        specifying coverage frames and fetching coverage without CK or */
/*        PCK primary kernels. */

/*        The coverage frame applies only to CK and binary PCKs files. */
/*        If it was specified for any other kernels or if no kernels */
/*        were specified, we report an error. */

/*        If two files were given, check the first file. If one file was */
/*        given, check that file. If no files were given, report an */
/*        error right away. */

	if (s_cmp(kernam, " ", kernam_len, (ftnlen)1) != 0) {
	    s_copy(hkrnam, kernam, (ftnlen)1024, kernam_len);
	    s_copy(hartyp, arctyp, (ftnlen)7, (ftnlen)7);
	} else {
	    s_copy(hkrnam, kernam + kernam_len, (ftnlen)1024, kernam_len);
	    s_copy(hartyp, arctyp + 7, (ftnlen)7, (ftnlen)7);
	}

/*        The two errors below will never execute in versions 1.2.+ */
/*        because we can get here only with a non-blank file name and a */
/*        CK or PCK file type. This block is left here for ease of */
/*        comparison with the previous versions of this routine. */

	if (s_cmp(hkrnam, " ", (ftnlen)1024, (ftnlen)1) != 0) {
	    if (! (s_cmp(hartyp, "DAF/CK", (ftnlen)7, (ftnlen)6) == 0 || 
		    s_cmp(hartyp, "DAF/PCK", (ftnlen)7, (ftnlen)7) == 0)) {
		setmsg_("'#' option cannot be used in this run because the f"
			"ile '#' is not a CK or binary PCK.", (ftnlen)85);
		errch_("#", "-c1", (ftnlen)1, (ftnlen)3);
		errch_("#", hkrnam, (ftnlen)1, (ftnlen)1024);
		sigerr_("SPICE(BADCOVFRAME1SPEC1)", (ftnlen)24);
	    }
	} else {
	    setmsg_("'#' option cannot be used in this run because no files "
		    "were specified on the command line.", (ftnlen)90);
	    errch_("#", "-c1", (ftnlen)1, (ftnlen)3);
	    sigerr_("SPICE(BADCOVFRAME1SPEC2)", (ftnlen)24);
	}

/*        OK, we do deal with a CK or a binary PCK. Let's see if we can */
/*        map this frame to an ID and if there is data for this ID in */
/*        this file. */

	s_copy(cfrnam, clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : 
		s_rnge("clvals", i__1, "chwcml_", (ftnlen)1148)) << 10), (
		ftnlen)32, (ftnlen)1024);
	nparsi_(clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge(
		"clvals", i__1, "chwcml_", (ftnlen)1149)) << 10), cfrcid, 
		error, &ptr, (ftnlen)1024, (ftnlen)1024);
	if (ptr != 0) {

/*           It's not an integer. Assume that it's a frame name and */
/*           try to get its ID. */

	    namfrm_(clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : 
		    s_rnge("clvals", i__1, "chwcml_", (ftnlen)1157)) << 10), 
		    cfrcid, (ftnlen)1024);
	    if (cfrcid[0] == 0) {
		setmsg_("'#' specified after '#' key is neither an integer n"
			"umber representing a legitimate frame ID nor a frame"
			" name recognized in SPICE.", (ftnlen)129);
		errch_("#", clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? 
			i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)1165)
			) << 10), (ftnlen)1, (ftnlen)1024);
		errch_("#", "-c1", (ftnlen)1, (ftnlen)3);
		sigerr_("SPICE(BADCOVFRAME1SPEC4)", (ftnlen)24);
	    }

/*           Get class and class ID for this frame. Check if class */
/*           matches the file type and if the class ID is one of the */
/*           IDs present in the file. Again, we need to check this */
/*           either for the first or for the second file, depending on */
/*           how many files were provided. */

	    frinfo_(cfrcid, &hint, cfrcls, &clssid, &found);
	    if (s_cmp(kernam, " ", kernam_len, (ftnlen)1) != 0) {
		s_copy(hkrnam, kernam, (ftnlen)1024, kernam_len);
		s_copy(hartyp, arctyp, (ftnlen)7, (ftnlen)7);
		found = elemi_(&clssid, flids1);
	    } else {
		s_copy(hkrnam, kernam + kernam_len, (ftnlen)1024, kernam_len);
		s_copy(hartyp, arctyp + 7, (ftnlen)7, (ftnlen)7);
		found = elemi_(&clssid, flids2);
	    }
	    if (s_cmp(hartyp, "DAF/PCK", (ftnlen)7, (ftnlen)7) == 0 && cfrcls[
		    0] == 2 || s_cmp(hartyp, "DAF/CK", (ftnlen)7, (ftnlen)6) 
		    == 0 && cfrcls[0] == 3) {
		if (found) {
		    cfrcid[0] = clssid;
		    cfrfnd[0] = found;
		} else {
		    setmsg_("The file '#' does not contain any data for the "
			    "frame '#' (frame class ID '#') specified after '"
			    "#' key.", (ftnlen)102);
		    errch_("#", hkrnam, (ftnlen)1, (ftnlen)1024);
		    errch_("#", clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 
			    ? i__1 : s_rnge("clvals", i__1, "chwcml_", (
			    ftnlen)1204)) << 10), (ftnlen)1, (ftnlen)1024);
		    errint_("#", &clssid, (ftnlen)1);
		    errch_("#", "-c1", (ftnlen)1, (ftnlen)3);
		    sigerr_("SPICE(COVFRAME1NODATA1)", (ftnlen)23);
		}
	    } else {
		setmsg_("The class of the frame '#' provided after '#' key d"
			"oes not match the kernel type of the file '#', to wh"
			"ich the frame applies. The class was '#'; the kernel"
			" architecture/type were '#'.", (ftnlen)183);
		errch_("#", clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? 
			i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)1220)
			) << 10), (ftnlen)1, (ftnlen)1024);
		errch_("#", "-c1", (ftnlen)1, (ftnlen)3);
		errch_("#", hkrnam, (ftnlen)1, (ftnlen)1024);
		errint_("#", cfrcls, (ftnlen)1);
		errch_("#", hartyp, (ftnlen)1, (ftnlen)7);
		sigerr_("SPICE(COVFRAME1MISMATCH)", (ftnlen)24);
	    }
	} else {

/*           It's an integer. We just need to check that the file in */
/*           question contains data for this ID. If not, report an */
/*           error. */

	    if (s_cmp(kernam, " ", kernam_len, (ftnlen)1) != 0) {
		s_copy(hkrnam, kernam, (ftnlen)1024, kernam_len);
		s_copy(hartyp, arctyp, (ftnlen)7, (ftnlen)7);
		cfrfnd[0] = elemi_(cfrcid, flids1);
	    } else {
		s_copy(hkrnam, kernam + kernam_len, (ftnlen)1024, kernam_len);
		s_copy(hartyp, arctyp + 7, (ftnlen)7, (ftnlen)7);
		cfrfnd[0] = elemi_(cfrcid, flids2);
	    }
	    if (! cfrfnd[0]) {
		setmsg_("The file '#' does not contain any data for the ID '"
			"#'  specified after '#' key.", (ftnlen)79);
		errch_("#", hkrnam, (ftnlen)1, (ftnlen)1024);
		errch_("#", clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? 
			i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)1251)
			) << 10), (ftnlen)1, (ftnlen)1024);
		errch_("#", "-c1", (ftnlen)1, (ftnlen)3);
		sigerr_("SPICE(COVFRAME1NODATA2)", (ftnlen)23);
	    }

/*           OK, this is a stretch but we have to do it to not require */
/*           SPICE to know the frame associated with this class ID. */
/*           We will set coverage frame class simply based on the kernel */
/*           type. */

	    if (s_cmp(hartyp, "DAF/CK", (ftnlen)7, (ftnlen)6) == 0) {
		cfrcls[0] = 3;
	    } else if (s_cmp(hartyp, "DAF/PCK", (ftnlen)7, (ftnlen)7) == 0) {
		cfrcls[0] = 2;
	    }
	}
    } else if (clflag[(i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge(
	    "clflag", i__1, "chwcml_", (ftnlen)1270)]) {

/*        If we are here, the first coverage frame was provided on the */
/*        command line but either no primary kernels files were given or */
/*        the primary kernel to which this coverage frame applies is not */
/*        a CK or a PCK. Was the frame given as a name or as an ID? Try */
/*        to convert it to integer and see if it worked. */

	s_copy(cfrnam, clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : 
		s_rnge("clvals", i__1, "chwcml_", (ftnlen)1279)) << 10), (
		ftnlen)32, (ftnlen)1024);
	nparsi_(clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge(
		"clvals", i__1, "chwcml_", (ftnlen)1280)) << 10), cfrid, 
		error, &ptr, (ftnlen)1024, (ftnlen)1024);
	if (ptr != 0) {

/*           It's not an integer. Assume that it's the frame name and */
/*           try to convert it to ID. If we can't, report an error. */

	    namfrm_(clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : 
		    s_rnge("clvals", i__1, "chwcml_", (ftnlen)1288)) << 10), 
		    cfrid, (ftnlen)1024);
	    if (cfrid[0] == 0) {
		setmsg_("'#' specified after '#' key is neither an integer n"
			"umber representing a legitimate frame ID nor a frame"
			" name recognized in SPICE.", (ftnlen)129);
		errch_("#", clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? 
			i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)1296)
			) << 10), (ftnlen)1, (ftnlen)1024);
		errch_("#", "-c1", (ftnlen)1, (ftnlen)3);
		sigerr_("SPICE(BADCOVFRAME1SPEC5)", (ftnlen)24);
	    }
	} else {

/*           It's an integer. Assume that it's the frame ID and try to */
/*           convert it to name. If we can't, report an error. */

	    frmnam_(cfrid, cfrnam, (ftnlen)32);
	    if (s_cmp(cfrnam, " ", (ftnlen)32, (ftnlen)1) == 0) {
		setmsg_("'#' specified after '#' key is neither an integer n"
			"umber representing a legitimate frame ID nor a frame"
			" name recognized in SPICE.", (ftnlen)129);
		errch_("#", clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? 
			i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)1315)
			) << 10), (ftnlen)1, (ftnlen)1024);
		errch_("#", "-c1", (ftnlen)1, (ftnlen)3);
		sigerr_("SPICE(BADCOVFRAME1SPEC6)", (ftnlen)24);
	    }
	}

/*        OK, we could recognize this frame. Get its class and class */
/*        ID. Signal an error if its class is not 2 or 3 as we can */
/*        get coverage only for PCK- or CK-based frames. */

	frinfo_(cfrid, &hint, cfrcls, cfrcid, cfrfnd);
	if (! (cfrcls[0] == 2 || cfrcls[0] == 3)) {
	    setmsg_("Frame '#' (frame ID #) specified after '#' key is neith"
		    "er a PCK (class 2) or a CK (class 3) frame. Its class is"
		    " #.", (ftnlen)114);
	    errch_("#", cfrnam, (ftnlen)1, (ftnlen)32);
	    errint_("#", cfrid, (ftnlen)1);
	    errch_("#", "-c1", (ftnlen)1, (ftnlen)3);
	    errint_("#", cfrcls, (ftnlen)1);
	    sigerr_("SPICE(BADCOVFRAME1SPEC5)", (ftnlen)24);
	}

/*        Set data found flag to FALSE because it is applicable only */
/*        availability of data in the first or only CK or PCK file. */

	cfrfnd[0] = FALSE_;
    }

/*     We are done with command line keys defining the first attitude */
/*     set. We will unload all kernels, load kernels applicable to the */
/*     second attitude set and move on to processing keys for it. */

    kclear_();

/*     If the second kernel is provided, load it. Also load */
/*     additional kernels supplied for the second kernel and kernels */
/*     applicable for both kernels. */

/* Writing concatenation */
    i__8[0] = kernls_len, a__3[0] = kernls + (kernls_len << 1);
    i__8[1] = 1, a__3[1] = " ";
    i__8[2] = kernls_len, a__3[2] = kernls + kernls_len;
    i__8[3] = 1, a__3[3] = " ";
    i__8[4] = kernam_len, a__3[4] = kernam + kernam_len;
    s_cat(hlline, a__3, i__8, &c__5, (ftnlen)5120);
    ldklst_(hlline, (ftnlen)5120);

/*     After loading all kernels provided for computing the second */
/*     attitude set we can move on to getting command line inputs */
/*     defining it. */

/*     Start with getting the second ``from'' frame. */

    s_copy(ffrnam + ffrnam_len, " ", ffrnam_len, (ftnlen)1);
    ffrid[1] = 0;
    i__ = isrchc_("-f2", &c__25, clkeys, (ftnlen)3, (ftnlen)32);
    if (clflag[(i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge("clflag", 
	    i__1, "chwcml_", (ftnlen)1376)]) {

/*        The second ``from'' frame was provided on the command line. */
/*        Was it given as a name or as an ID? Try to convert it to */
/*        integer and see if it worked. */

	s_copy(ffrnam + ffrnam_len, clvals + (((i__1 = i__ - 1) < 25 && 0 <= 
		i__1 ? i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)1383))
		 << 10), ffrnam_len, (ftnlen)1024);
	nparsi_(clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge(
		"clvals", i__1, "chwcml_", (ftnlen)1384)) << 10), &ffrid[1], 
		error, &ptr, (ftnlen)1024, (ftnlen)1024);
	if (ptr != 0) {

/*           It's not an integer. Assume that it's the frame name and */
/*           try to convert it to ID. If we can't, report an error. */

	    namfrm_(clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : 
		    s_rnge("clvals", i__1, "chwcml_", (ftnlen)1392)) << 10), &
		    ffrid[1], (ftnlen)1024);
	    if (ffrid[1] == 0) {
		setmsg_("'#' specified after '#' key is neither an integer n"
			"umber representing a legitimate frame ID nor a frame"
			" name recognized in SPICE.", (ftnlen)129);
		errch_("#", clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? 
			i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)1400)
			) << 10), (ftnlen)1, (ftnlen)1024);
		errch_("#", "-f2", (ftnlen)1, (ftnlen)3);
		sigerr_("SPICE(BADFROMFRAME2SP1)", (ftnlen)23);
	    }
	} else {

/*           It's an integer. Assume that it's the frame ID and try to */
/*           convert it to name. If we can't, report an error. */

	    frmnam_(&ffrid[1], ffrnam + ffrnam_len, ffrnam_len);
	    if (s_cmp(ffrnam + ffrnam_len, " ", ffrnam_len, (ftnlen)1) == 0) {
		setmsg_("'#' specified after '#' key is neither an integer n"
			"umber representing a legitimate frame ID nor a frame"
			" name recognized in SPICE.", (ftnlen)129);
		errch_("#", clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? 
			i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)1419)
			) << 10), (ftnlen)1, (ftnlen)1024);
		errch_("#", "-f2", (ftnlen)1, (ftnlen)3);
		sigerr_("SPICE(BADFROMFRAME2SP2)", (ftnlen)23);
	    }
	}
    }

/*     Next, get the second ``to'' frame. */

    s_copy(tfrnam + tfrnam_len, " ", tfrnam_len, (ftnlen)1);
    tfrid[1] = 0;
    tfrcid[1] = 0;
    tfrcls[1] = 0;
    i__ = isrchc_("-t2", &c__25, clkeys, (ftnlen)3, (ftnlen)32);
    if (clflag[(i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge("clflag", 
	    i__1, "chwcml_", (ftnlen)1438)]) {

/*        The second ``to'' frame was provided on the command line. Was */
/*        it given as a name or as an ID? Try to convert it to integer */
/*        and see if it worked. */

	s_copy(tfrnam + tfrnam_len, clvals + (((i__1 = i__ - 1) < 25 && 0 <= 
		i__1 ? i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)1445))
		 << 10), tfrnam_len, (ftnlen)1024);
	nparsi_(clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge(
		"clvals", i__1, "chwcml_", (ftnlen)1446)) << 10), &tfrid[1], 
		error, &ptr, (ftnlen)1024, (ftnlen)1024);
	if (ptr != 0) {

/*           It's not an integer. Assume that it's the frame name and */
/*           try to convert it to ID. If we can't, report an error. */

	    namfrm_(clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : 
		    s_rnge("clvals", i__1, "chwcml_", (ftnlen)1454)) << 10), &
		    tfrid[1], (ftnlen)1024);
	    if (tfrid[1] == 0) {
		setmsg_("'#' specified after '#' key is neither an integer n"
			"umber representing a legitimate frame ID nor a frame"
			" name recognized in SPICE.", (ftnlen)129);
		errch_("#", clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? 
			i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)1462)
			) << 10), (ftnlen)1, (ftnlen)1024);
		errch_("#", "-t2", (ftnlen)1, (ftnlen)3);
		sigerr_("SPICE(BADTOFRAME2SPEC1)", (ftnlen)23);
	    }
	} else {

/*           It's an integer; treat it as a frame ID or a frame class ID */
/*           depending on context and try to map it to frame name. */

/*           If the second or only file is a CK and this ID is one of */
/*           the IDs present in it, treat it as a CK class ID and try to */
/*           map it to frame as such. */

/*           If the second or only file is a binary PCK and this ID is */
/*           one of the IDs present in it, treat it as a PCK class ID */
/*           and try to map it to frame as such. */

/*           If the second or only file is not a CK or a binary PCK, or */
/*           if it is BUT the ID could not be mapped to frame as a class */
/*           ID, try to map it to frame by treating it as a frame ID. If */
/*           it can't be mapped, report an error. */

	    if (s_cmp(kernam + kernam_len, " ", kernam_len, (ftnlen)1) != 0) {
		s_copy(hartyp, arctyp + 7, (ftnlen)7, (ftnlen)7);
		found = elemi_(&tfrid[1], flids2);
	    } else {
		s_copy(hartyp, " ", (ftnlen)7, (ftnlen)1);
		found = FALSE_;
	    }
	    if (s_cmp(hartyp, "DAF/CK", (ftnlen)7, (ftnlen)6) == 0 && found) {
		ccifrm_(&c__3, &tfrid[1], &frcode, tfrnam + tfrnam_len, &hint,
			 &found, tfrnam_len);
	    } else if (s_cmp(hartyp, "DAF/PCK", (ftnlen)7, (ftnlen)7) == 0 && 
		    found) {
		ccifrm_(&c__2, &tfrid[1], &frcode, tfrnam + tfrnam_len, &hint,
			 &found, tfrnam_len);
	    }
	    if (found) {
		tfrcid[1] = tfrid[1];
		tfrid[1] = frcode;
	    } else {
		s_copy(tfrnam + tfrnam_len, " ", tfrnam_len, (ftnlen)1);
	    }
	    if (s_cmp(tfrnam + tfrnam_len, " ", tfrnam_len, (ftnlen)1) == 0) {
		frmnam_(&tfrid[1], tfrnam + tfrnam_len, tfrnam_len);
		if (s_cmp(tfrnam + tfrnam_len, " ", tfrnam_len, (ftnlen)1) == 
			0) {
		    setmsg_("'#' specified after '#' key is neither an integ"
			    "er number representing a legitimate frame ID nor"
			    " a frame name recognized in SPICE.", (ftnlen)129);
		    errch_("#", clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 
			    ? i__1 : s_rnge("clvals", i__1, "chwcml_", (
			    ftnlen)1521)) << 10), (ftnlen)1, (ftnlen)1024);
		    errch_("#", "-t2", (ftnlen)1, (ftnlen)3);
		    sigerr_("SPICE(BADTOFRAME2SPEC2)", (ftnlen)23);
		}
	    }
	}

/*        If we are here, we were able to resolve this frame. Get and */
/*        buffer its class and class ID; we may need these later */
/*        to set the coverage frame. */

	frinfo_(&tfrid[1], &hint, &tfrcls[1], &tfrcid[1], &found);
    }

/*     Next, get the second ``coverage'' frame name or class ID. */

    s_copy(cfrnam + 32, " ", (ftnlen)32, (ftnlen)1);
    cfrcid[1] = 0;
    cfrcls[1] = 0;
    cfrfnd[1] = FALSE_;
    i__ = isrchc_("-c2", &c__25, clkeys, (ftnlen)3, (ftnlen)32);
    if (clflag[(i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge("clflag", 
	    i__1, "chwcml_", (ftnlen)1549)] && (s_cmp(kernam + kernam_len, 
	    " ", kernam_len, (ftnlen)1) != 0 && iscpck[1])) {

/*        This case -- specifying the second coverage frame and */
/*        providing the second or only primary file that is CK or PCK -- */
/*        was used in the version 1.0.0 of the program which did not */
/*        allow specifying coverage frames and fetching coverage without */
/*        CK or PCK primary kernels. */

/*        The coverage frame applies only to CK and binary PCKs files. */
/*        If it was specified for any other kernels or if no kernels */
/*        were specified, we report an error. */

/*        If the file was given, check it. If no file was given, report */
/*        an error right away. */

	s_copy(hkrnam, kernam + kernam_len, (ftnlen)1024, kernam_len);
	s_copy(hartyp, arctyp + 7, (ftnlen)7, (ftnlen)7);

/*        The two errors below will never execute in versions 1.2.+ */
/*        because we can get here only with a non-blank file name and a */
/*        CK or PCK file type. This block is left here for ease of */
/*        comparison with the previous versions of this routine. */

	if (s_cmp(hkrnam, " ", (ftnlen)1024, (ftnlen)1) != 0) {
	    if (! (s_cmp(hartyp, "DAF/CK", (ftnlen)7, (ftnlen)6) == 0 || 
		    s_cmp(hartyp, "DAF/PCK", (ftnlen)7, (ftnlen)7) == 0)) {
		setmsg_("'#' option cannot be used in this run because the f"
			"ile '#' is not a CK or binary PCK.", (ftnlen)85);
		errch_("#", "-c2", (ftnlen)1, (ftnlen)3);
		errch_("#", hkrnam, (ftnlen)1, (ftnlen)1024);
		sigerr_("SPICE(BADCOVFRAME2SPEC1)", (ftnlen)24);
	    }
	} else {
	    setmsg_("'#' option cannot be used in this run because no files "
		    "were specified on the command line.", (ftnlen)90);
	    errch_("#", "-c2", (ftnlen)1, (ftnlen)3);
	    sigerr_("SPICE(BADCOVFRAME2SPEC2)", (ftnlen)24);
	}

/*        OK, we do deal with a CK or a binary PCK. Let's see if we can */
/*        map this frame to an ID and if there is data for this ID in */
/*        this file. */

	s_copy(cfrnam + 32, clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? 
		i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)1604)) << 10)
		, (ftnlen)32, (ftnlen)1024);
	nparsi_(clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge(
		"clvals", i__1, "chwcml_", (ftnlen)1605)) << 10), &cfrcid[1], 
		error, &ptr, (ftnlen)1024, (ftnlen)1024);
	if (ptr != 0) {

/*           It's not an integer. Assume that it's a frame name and */
/*           try to get its ID. */

	    namfrm_(clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : 
		    s_rnge("clvals", i__1, "chwcml_", (ftnlen)1613)) << 10), &
		    cfrcid[1], (ftnlen)1024);
	    if (cfrcid[1] == 0) {
		setmsg_("'#' specified after '#' key is neither an integer n"
			"umber representing a legitimate frame ID nor a frame"
			" name recognized in SPICE.", (ftnlen)129);
		errch_("#", clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? 
			i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)1621)
			) << 10), (ftnlen)1, (ftnlen)1024);
		errch_("#", "-c2", (ftnlen)1, (ftnlen)3);
		sigerr_("SPICE(BADCOVFRAME2SPEC4)", (ftnlen)24);
	    }

/*           Get class and class ID for this frame. Check if class */
/*           matches the file type and if the class ID is one of the */
/*           IDs present in the file. */

	    frinfo_(&cfrcid[1], &hint, &cfrcls[1], &clssid, &found);
	    s_copy(hkrnam, kernam + kernam_len, (ftnlen)1024, kernam_len);
	    s_copy(hartyp, arctyp + 7, (ftnlen)7, (ftnlen)7);
	    found = elemi_(&clssid, flids2);
	    if (s_cmp(hartyp, "DAF/PCK", (ftnlen)7, (ftnlen)7) == 0 && cfrcls[
		    1] == 2 || s_cmp(hartyp, "DAF/CK", (ftnlen)7, (ftnlen)6) 
		    == 0 && cfrcls[1] == 3) {
		if (found) {
		    cfrcid[1] = clssid;
		    cfrfnd[1] = found;
		} else {
		    setmsg_("The file '#' does not contain any data for the "
			    "frame '#' (frame class ID '#') specified after '"
			    "#' key.", (ftnlen)102);
		    errch_("#", hkrnam, (ftnlen)1, (ftnlen)1024);
		    errch_("#", clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 
			    ? i__1 : s_rnge("clvals", i__1, "chwcml_", (
			    ftnlen)1652)) << 10), (ftnlen)1, (ftnlen)1024);
		    errint_("#", &clssid, (ftnlen)1);
		    errch_("#", "-c2", (ftnlen)1, (ftnlen)3);
		    sigerr_("SPICE(COVFRAME2NODATA1)", (ftnlen)23);
		}
	    } else {
		setmsg_("The class of the frame '#' provided after '#' key d"
			"oes not match the kernel type of the file '#', to wh"
			"ich the frame applies. The class was '#'; the kernel"
			" architecture/type were '#'.", (ftnlen)183);
		errch_("#", clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? 
			i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)1668)
			) << 10), (ftnlen)1, (ftnlen)1024);
		errch_("#", "-c2", (ftnlen)1, (ftnlen)3);
		errch_("#", hkrnam, (ftnlen)1, (ftnlen)1024);
		errint_("#", &cfrcls[1], (ftnlen)1);
		errch_("#", hartyp, (ftnlen)1, (ftnlen)7);
		sigerr_("SPICE(COVFRAME2MISMATCH)", (ftnlen)24);
	    }
	} else {

/*           It's an integer. We just need to check that the file in */
/*           question contains data for this ID. If not, report an */
/*           error. */

	    s_copy(hkrnam, kernam + kernam_len, (ftnlen)1024, kernam_len);
	    s_copy(hartyp, arctyp + 7, (ftnlen)7, (ftnlen)7);
	    cfrfnd[1] = elemi_(&cfrcid[1], flids2);
	    if (! cfrfnd[1]) {
		setmsg_("The file '#' does not contain any data for the ID '"
			"#'  specified after '#' key.", (ftnlen)79);
		errch_("#", hkrnam, (ftnlen)1, (ftnlen)1024);
		errch_("#", clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? 
			i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)1693)
			) << 10), (ftnlen)1, (ftnlen)1024);
		errch_("#", "-c2", (ftnlen)1, (ftnlen)3);
		sigerr_("SPICE(COVFRAME2NODATA2)", (ftnlen)23);
	    }

/*           OK, this is a stretch but we have to do it to not require */
/*           SPICE to know the frame associated with this class ID. */
/*           We will set coverage frame class simply based on the kernel */
/*           type. */

	    if (s_cmp(hartyp, "DAF/CK", (ftnlen)7, (ftnlen)6) == 0) {
		cfrcls[1] = 3;
	    } else if (s_cmp(hartyp, "DAF/PCK", (ftnlen)7, (ftnlen)7) == 0) {
		cfrcls[1] = 2;
	    }
	}
    } else if (clflag[(i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge(
	    "clflag", i__1, "chwcml_", (ftnlen)1712)]) {

/*        If we are here, the second coverage frame was provided on the */
/*        command line but either no primary kernels files were given or */
/*        the second or only primary kernel is not a CK or a PCK. Was */
/*        the frame given as a name or as an ID? Try to convert it to */
/*        integer and see if it worked. */

	s_copy(cfrnam + 32, clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? 
		i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)1721)) << 10)
		, (ftnlen)32, (ftnlen)1024);
	nparsi_(clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge(
		"clvals", i__1, "chwcml_", (ftnlen)1722)) << 10), &cfrid[1], 
		error, &ptr, (ftnlen)1024, (ftnlen)1024);
	if (ptr != 0) {

/*           It's not an integer. Assume that it's the frame name and */
/*           try to convert it to ID. If we can't, report an error. */

	    namfrm_(clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : 
		    s_rnge("clvals", i__1, "chwcml_", (ftnlen)1730)) << 10), &
		    cfrid[1], (ftnlen)1024);
	    if (cfrid[1] == 0) {
		setmsg_("'#' specified after '#' key is neither an integer n"
			"umber representing a legitimate frame ID nor a frame"
			" name recognized in SPICE.", (ftnlen)129);
		errch_("#", clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? 
			i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)1738)
			) << 10), (ftnlen)1, (ftnlen)1024);
		errch_("#", "-c2", (ftnlen)1, (ftnlen)3);
		sigerr_("SPICE(BADCOVFRAME2SPEC5)", (ftnlen)24);
	    }
	} else {

/*           It's an integer. Assume that it's the frame ID and try to */
/*           convert it to name. If we can't, report an error. */

	    frmnam_(&cfrid[1], cfrnam + 32, (ftnlen)32);
	    if (s_cmp(cfrnam + 32, " ", (ftnlen)32, (ftnlen)1) == 0) {
		setmsg_("'#' specified after '#' key is neither an integer n"
			"umber representing a legitimate frame ID nor a frame"
			" name recognized in SPICE.", (ftnlen)129);
		errch_("#", clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? 
			i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)1757)
			) << 10), (ftnlen)1, (ftnlen)1024);
		errch_("#", "-c2", (ftnlen)1, (ftnlen)3);
		sigerr_("SPICE(BADCOVFRAME2SPEC6)", (ftnlen)24);
	    }
	}

/*        OK, we could recognize this frame. Get its class and class */
/*        ID. Signal an error if its class is not 2 or 3 as we can */
/*        get coverage only for PCK- or CK-based frames. */

	frinfo_(&cfrid[1], &hint, &cfrcls[1], &cfrcid[1], &cfrfnd[1]);
	if (! (cfrcls[1] == 2 || cfrcls[1] == 3)) {
	    setmsg_("Frame '#' (frame ID #) specified after '#' key is neith"
		    "er a PCK (class 2) or a CK (class 3) frame. Its class is"
		    " #.", (ftnlen)114);
	    errch_("#", cfrnam + 32, (ftnlen)1, (ftnlen)32);
	    errint_("#", &cfrid[1], (ftnlen)1);
	    errch_("#", "-c2", (ftnlen)1, (ftnlen)3);
	    errint_("#", &cfrcls[1], (ftnlen)1);
	    sigerr_("SPICE(BADCOVFRAME2SPEC5)", (ftnlen)24);
	}

/*        Set data found flag to FALSE because it is applicable only */
/*        availability of data in the second or only CK or PCK file. */

	cfrfnd[1] = FALSE_;
    }

/*     Process angular velocity comparison command line key and set */
/*     the output angular velocity flag (AVFLG) based on its value. */

    i__ = isrchc_("-a", &c__25, clkeys, (ftnlen)2, (ftnlen)32);
    *avflg = FALSE_;
    if (clflag[(i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge("clflag", 
	    i__1, "chwcml_", (ftnlen)1799)]) {
	if (eqstr_(clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : 
		s_rnge("clvals", i__1, "chwcml_", (ftnlen)1801)) << 10), 
		"yes", (ftnlen)1024, (ftnlen)3)) {
	    *avflg = TRUE_;
	} else if (eqstr_(clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? 
		i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)1805)) << 10)
		, "no", (ftnlen)1024, (ftnlen)2)) {
	    *avflg = FALSE_;
	} else {
	    setmsg_("Angular velocity comparison flag '#' specified after '#"
		    "' key is not recognized. Recognized values are '#' and '"
		    "#'.", (ftnlen)114);
	    errch_("#", clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 :
		     s_rnge("clvals", i__1, "chwcml_", (ftnlen)1815)) << 10), 
		    (ftnlen)1, (ftnlen)1024);
	    errch_("#", "-a", (ftnlen)1, (ftnlen)2);
	    errch_("#", "yes", (ftnlen)1, (ftnlen)3);
	    errch_("#", "no", (ftnlen)1, (ftnlen)2);
	    sigerr_("SPICE(BADAVFLAG)", (ftnlen)16);
	}
    }

/*     Process angular velocity frame command line key and set the */
/*     output angular velocity frame flag (AVFFLG) based on its value. */

    i__ = isrchc_("-m", &c__25, clkeys, (ftnlen)2, (ftnlen)32);
    if (clflag[(i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge("clflag", 
	    i__1, "chwcml_", (ftnlen)1831)]) {
	if (eqstr_(clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : 
		s_rnge("clvals", i__1, "chwcml_", (ftnlen)1833)) << 10), 
		"from", (ftnlen)1024, (ftnlen)4)) {
	    *avfflg = FALSE_;
	} else if (eqstr_(clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? 
		i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)1837)) << 10)
		, "to", (ftnlen)1024, (ftnlen)2)) {
	    *avfflg = TRUE_;
	} else {
	    setmsg_("Angular velocity frame flag '#' specified after '#' key"
		    " is not recognized. Recognized values are '#' and '#'.", (
		    ftnlen)109);
	    errch_("#", clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 :
		     s_rnge("clvals", i__1, "chwcml_", (ftnlen)1847)) << 10), 
		    (ftnlen)1, (ftnlen)1024);
	    errch_("#", "-m", (ftnlen)1, (ftnlen)2);
	    errch_("#", "from", (ftnlen)1, (ftnlen)4);
	    errch_("#", "to", (ftnlen)1, (ftnlen)2);
	    sigerr_("SPICE(BADAVFRAMEFLAG)", (ftnlen)21);
	}
    } else {
	*avfflg = FALSE_;
    }

/*     Process begin and end time arguments. As these need to be */
/*     converted to ET, we will need LSK data, which can come in any of */
/*     the five file sets on the command line -- first or second file, */
/*     kernels specific for first of second file or kernels applicable */
/*     to both file. Note that second file, kernels applicable for it */
/*     and kernels applicable for both file are already loaded. We will */
/*     load first file and kernels specific to it to make sure that we */
/*     have loaded all kernels that we could. */

/* Writing concatenation */
    i__7[0] = kernls_len, a__2[0] = kernls;
    i__7[1] = 1, a__2[1] = " ";
    i__7[2] = kernam_len, a__2[2] = kernam;
    s_cat(hlline, a__2, i__7, &c__3, (ftnlen)5120);
    ldklst_(hlline, (ftnlen)5120);
    s_copy(time, " ", (ftnlen)1024, (ftnlen)1);
    et[0] = dpmin_();
    i__ = isrchc_("-b", &c__25, clkeys, (ftnlen)2, (ftnlen)32);
    if (clflag[(i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge("clflag", 
	    i__1, "chwcml_", (ftnlen)1879)]) {
	s_copy(time, clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : 
		s_rnge("clvals", i__1, "chwcml_", (ftnlen)1880)) << 10), (
		ftnlen)1024, (ftnlen)1024);
	str2et_(clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge(
		"clvals", i__1, "chwcml_", (ftnlen)1881)) << 10), et, (ftnlen)
		1024);
    }
    s_copy(time + 1024, " ", (ftnlen)1024, (ftnlen)1);
    et[1] = dpmax_();
    i__ = isrchc_("-e", &c__25, clkeys, (ftnlen)2, (ftnlen)32);
    if (clflag[(i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge("clflag", 
	    i__1, "chwcml_", (ftnlen)1889)]) {
	s_copy(time + 1024, clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? 
		i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)1890)) << 10)
		, (ftnlen)1024, (ftnlen)1024);
	str2et_(clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge(
		"clvals", i__1, "chwcml_", (ftnlen)1891)) << 10), &et[1], (
		ftnlen)1024);
    }

/*     Check that begin time is less than the end time. */

    if (s_cmp(time, " ", (ftnlen)1024, (ftnlen)1) != 0 && s_cmp(time + 1024, 
	    " ", (ftnlen)1024, (ftnlen)1) != 0) {
	if (et[1] < et[0]) {
	    setmsg_("Specified start time '#' is greater than specified stop"
		    " time '#'.", (ftnlen)65);
	    errch_("#", time, (ftnlen)1, (ftnlen)1024);
	    errch_("#", time + 1024, (ftnlen)1, (ftnlen)1024);
	    sigerr_("SPICE(INCONSISTENTTIMES)", (ftnlen)24);
	}
    }

/*     Make a string describing time bounds provided on the command */
/*     line to be used later in error messages. */

    if (s_cmp(time, " ", (ftnlen)1024, (ftnlen)1) != 0 && s_cmp(time + 1024, 
	    " ", (ftnlen)1024, (ftnlen)1) != 0) {
	s_copy(timdsc, "Comparison window specified on the command line was "
		"from '#' to '#'.", (ftnlen)1024, (ftnlen)68);
	repmc_(timdsc, "#", time, timdsc, (ftnlen)1024, (ftnlen)1, (ftnlen)
		1024, (ftnlen)1024);
	repmc_(timdsc, "#", time + 1024, timdsc, (ftnlen)1024, (ftnlen)1, (
		ftnlen)1024, (ftnlen)1024);
    } else if (s_cmp(time, " ", (ftnlen)1024, (ftnlen)1) != 0) {
	s_copy(timdsc, "Comparison window specified on the command line was "
		"all times after '#'.", (ftnlen)1024, (ftnlen)72);
	repmc_(timdsc, "#", time, timdsc, (ftnlen)1024, (ftnlen)1, (ftnlen)
		1024, (ftnlen)1024);
    } else if (s_cmp(time + 1024, " ", (ftnlen)1024, (ftnlen)1) != 0) {
	s_copy(timdsc, "Comparison window specified on the command line was "
		"all times before '#'.", (ftnlen)1024, (ftnlen)73);
	repmc_(timdsc, "#", time + 1024, timdsc, (ftnlen)1024, (ftnlen)1, (
		ftnlen)1024, (ftnlen)1024);
    } else {
	s_copy(timdsc, " ", (ftnlen)1024, (ftnlen)1);
    }

/*     At this point we processed all command line keys defining */
/*     attributes of the two attitude sets as well as those restricting */
/*     time boundaries. It is likely that some of the items we need were */
/*     not provided (``from'', ``to'', ``cov'' frame) or could not be */
/*     provided (the final comparison window). Let go ahead and fill in */
/*     these ``blanks''. */


/*     If only one ``from'' name/ID was provided, set the other one to */
/*     be the same. */

    if (s_cmp(ffrnam, " ", ffrnam_len, (ftnlen)1) == 0 && s_cmp(ffrnam + 
	    ffrnam_len, " ", ffrnam_len, (ftnlen)1) != 0) {
	ffrid[0] = ffrid[1];
	s_copy(ffrnam, ffrnam + ffrnam_len, ffrnam_len, ffrnam_len);
    } else if (s_cmp(ffrnam + ffrnam_len, " ", ffrnam_len, (ftnlen)1) == 0 && 
	    s_cmp(ffrnam, " ", ffrnam_len, (ftnlen)1) != 0) {
	ffrid[1] = ffrid[0];
	s_copy(ffrnam + ffrnam_len, ffrnam, ffrnam_len, ffrnam_len);
    }

/*     If only one ``to'' name/ID was provided, set the other one to be */
/*     the same. */

    if (s_cmp(tfrnam, " ", tfrnam_len, (ftnlen)1) == 0 && s_cmp(tfrnam + 
	    tfrnam_len, " ", tfrnam_len, (ftnlen)1) != 0) {
	tfrid[0] = tfrid[1];
	tfrcid[0] = tfrcid[1];
	s_copy(tfrnam, tfrnam + tfrnam_len, tfrnam_len, tfrnam_len);
	tfrcls[0] = tfrcls[1];
    } else if (s_cmp(tfrnam + tfrnam_len, " ", tfrnam_len, (ftnlen)1) == 0 && 
	    s_cmp(tfrnam, " ", tfrnam_len, (ftnlen)1) != 0) {
	tfrid[1] = tfrid[0];
	tfrcid[1] = tfrcid[0];
	s_copy(tfrnam + tfrnam_len, tfrnam, tfrnam_len, tfrnam_len);
	tfrcls[1] = tfrcls[0];
    }

/*     If after doing this cross-fill either ``from'' or ``to'' frames */
/*     are still undefined, we need to dig trough the files to get */
/*     defaults for them. */

    if (s_cmp(ffrnam, " ", ffrnam_len, (ftnlen)1) == 0 && s_cmp(ffrnam + 
	    ffrnam_len, " ", ffrnam_len, (ftnlen)1) == 0 || s_cmp(tfrnam, 
	    " ", tfrnam_len, (ftnlen)1) == 0 && s_cmp(tfrnam + tfrnam_len, 
	    " ", tfrnam_len, (ftnlen)1) == 0) {

/*        First, check if we got at least one file on the command line. */
/*        If not, report an error. */

	if (s_cmp(kernam, " ", kernam_len, (ftnlen)1) == 0 && s_cmp(kernam + 
		kernam_len, " ", kernam_len, (ftnlen)1) == 0) {
	    setmsg_("Cannot pick default values for '#' frames because no ke"
		    "rnels were provided as the last and/or second to last it"
		    "em on the command line.", (ftnlen)134);
	    if (s_cmp(ffrnam, " ", ffrnam_len, (ftnlen)1) == 0) {
		errch_("#", "from", (ftnlen)1, (ftnlen)4);
	    } else {
		errch_("#", "to", (ftnlen)1, (ftnlen)2);
	    }
	    sigerr_("SPICE(CANTPICKDEFAULTS1)", (ftnlen)24);
	}

/*        Next, check if the file that we are supposed to examine to get */
/*        defaults is a CK or binary PCK file. If not, report an error. */

	if (s_cmp(kernam, " ", kernam_len, (ftnlen)1) != 0) {
	    s_copy(hkrnam, kernam, (ftnlen)1024, kernam_len);
	    s_copy(hartyp, arctyp, (ftnlen)7, (ftnlen)7);
	} else {
	    s_copy(hkrnam, kernam + kernam_len, (ftnlen)1024, kernam_len);
	    s_copy(hartyp, arctyp + 7, (ftnlen)7, (ftnlen)7);
	}
	if (! (s_cmp(hartyp, "DAF/CK", (ftnlen)7, (ftnlen)6) == 0 || s_cmp(
		hartyp, "DAF/PCK", (ftnlen)7, (ftnlen)7) == 0)) {
	    setmsg_("Cannot pick default values for '#' frames from the '#' "
		    "file because it is not a CK or binary PCK file.", (ftnlen)
		    102);
	    if (s_cmp(ffrnam, " ", ffrnam_len, (ftnlen)1) == 0) {
		errch_("#", "from", (ftnlen)1, (ftnlen)4);
	    } else {
		errch_("#", "to", (ftnlen)1, (ftnlen)2);
	    }
	    errch_("#", hkrnam, (ftnlen)1, (ftnlen)1024);
	    sigerr_("SPICE(CANTPICKDEFAULTS2)", (ftnlen)24);
	}

/*        OK, it looks like we can go in and try to get defaults from */
/*        this file. Open it as a DAF file and fetch descriptor of the */
/*        last segment. Depending on whether this file is CK or PCK, */
/*        unpack descriptor using appropriate ND/NI. */

	dafopr_(hkrnam, &handle, (ftnlen)1024);
	dafbbs_(&handle);
	daffpa_(&found);
	if (! found) {
/* Writing concatenation */
	    i__8[0] = 4, a__3[0] = "The ";
	    i__8[1] = 3, a__3[1] = hartyp + 4;
	    i__8[2] = 10, a__3[2] = " file '#' ";
	    i__8[3] = 38, a__3[3] = "does not contain any segments. Cannot ";
	    i__8[4] = 38, a__3[4] = "pick default '#' frame from this file.";
	    s_cat(ch__1, a__3, i__8, &c__5, (ftnlen)93);
	    setmsg_(ch__1, (ftnlen)93);
	    errch_("#", hkrnam, (ftnlen)1, (ftnlen)1024);
	    if (s_cmp(ffrnam, " ", ffrnam_len, (ftnlen)1) == 0) {
		errch_("#", "from", (ftnlen)1, (ftnlen)4);
	    } else {
		errch_("#", "to", (ftnlen)1, (ftnlen)2);
	    }
	    sigerr_("SPICE(NOSEGMENTSFOUND)", (ftnlen)22);
	}
	dafgs_(descr);
	dafcls_(&handle);
	if (s_cmp(hartyp, "DAF/CK", (ftnlen)7, (ftnlen)6) == 0) {
	    dafus_(descr, &c__2, &c__6, dc, ic);
	} else {
	    dafus_(descr, &c__2, &c__5, dc, ic);
	}

/*        Since we will need to do the frame name-ID mapping, we need to */
/*        reload kernels. Currently we have all kernels that have been */
/*        provided loaded into the program; what we want is only kernels */
/*        applicable to the first attitude set. */

	kclear_();
/* Writing concatenation */
	i__8[0] = kernls_len, a__3[0] = kernls + (kernls_len << 1);
	i__8[1] = 1, a__3[1] = " ";
	i__8[2] = kernls_len, a__3[2] = kernls;
	i__8[3] = 1, a__3[3] = " ";
	i__8[4] = 1024, a__3[4] = hkrnam;
	s_cat(hlline, a__3, i__8, &c__5, (ftnlen)5120);
	ldklst_(hlline, (ftnlen)5120);

/*        If we need to pick default for ``from'' frames, try to map */
/*        IC(2) frame ID to frame name. */

	if (s_cmp(ffrnam, " ", ffrnam_len, (ftnlen)1) == 0) {
	    frmnam_(&ic[1], ffrnam, ffrnam_len);
	    if (s_cmp(ffrnam, " ", ffrnam_len, (ftnlen)1) == 0) {
/* Writing concatenation */
		i__8[0] = 87, a__3[0] = "Cannot pick default 'from' frame be"
			"cause the frame ID '#' from the last segment of the ";
		i__8[1] = 3, a__3[1] = hartyp + 4;
		i__8[2] = 6, a__3[2] = " file ";
		i__8[3] = 32, a__3[3] = "'#' cannot be mapped to a frame ";
		i__8[4] = 5, a__3[4] = "name.";
		s_cat(ch__2, a__3, i__8, &c__5, (ftnlen)133);
		setmsg_(ch__2, (ftnlen)133);
		errint_("#", &ic[1], (ftnlen)1);
		errch_("#", hkrnam, (ftnlen)1, (ftnlen)1024);
		sigerr_("SPICE(CANTPICKDEFAULTS3)", (ftnlen)24);
	    }
	    s_copy(ffrnam + ffrnam_len, ffrnam, ffrnam_len, ffrnam_len);
	    ffrid[0] = ic[1];
	    ffrid[1] = ic[1];
	}

/*        If we need to pick default for ``to'' frames, try to map IC(1) */
/*        frame class ID to frame name. */

	if (s_cmp(tfrnam, " ", tfrnam_len, (ftnlen)1) == 0) {
	    if (s_cmp(hartyp, "DAF/CK", (ftnlen)7, (ftnlen)6) == 0) {
		tfrcls[0] = 3;
		ccifrm_(&c__3, ic, tfrid, tfrnam, &hint, &found, tfrnam_len);
	    } else {
		tfrcls[0] = 2;
		ccifrm_(&c__2, ic, tfrid, tfrnam, &hint, &found, tfrnam_len);
	    }
	    if (! found) {
/* Writing concatenation */
		i__9[0] = 91, a__4[0] = "Cannot pick default 'to' frame beca"
			"use the frame class ID '#' from the last segment of "
			"the ";
		i__9[1] = 3, a__4[1] = hartyp + 4;
		i__9[2] = 31, a__4[2] = "file '#' cannot be mapped to a ";
		i__9[3] = 11, a__4[3] = "frame name.";
		s_cat(ch__3, a__4, i__9, &c__4, (ftnlen)136);
		setmsg_(ch__3, (ftnlen)136);
		errint_("#", ic, (ftnlen)1);
		errch_("#", hkrnam, (ftnlen)1, (ftnlen)1024);
		sigerr_("SPICE(CANTPICKDEFAULTS4)", (ftnlen)24);
	    }
	    s_copy(tfrnam + tfrnam_len, tfrnam, tfrnam_len, tfrnam_len);
	    tfrid[1] = tfrid[0];
	    tfrcls[1] = tfrcls[0];
	    tfrcid[0] = ic[0];
	    tfrcid[1] = ic[0];
	}
    }

/*     OK, we now have filled all ``from'' and ``to'' frame attributes. */
/*     If coverage frame attributes were not set yet, set then to those */
/*     of the ``to'' frames but only if the ``to'' frames are of CK or */
/*     PCK types. */

    if (s_cmp(cfrnam, " ", (ftnlen)32, (ftnlen)1) == 0) {
	if (s_cmp(arctyp, "DAF/CK", (ftnlen)7, (ftnlen)6) == 0 && tfrcls[0] ==
		 3 || s_cmp(arctyp, "DAF/PCK", (ftnlen)7, (ftnlen)7) == 0 && 
		tfrcls[0] == 2) {
	    s_copy(cfrnam, tfrnam, (ftnlen)32, tfrnam_len);
	    cfrcid[0] = tfrcid[0];
	    cfrcls[0] = tfrcls[0];
	    cfrfnd[0] = elemi_(cfrcid, flids1);
	} else if (s_cmp(kernam, " ", kernam_len, (ftnlen)1) == 0 && s_cmp(
		arctyp + 7, "DAF/CK", (ftnlen)7, (ftnlen)6) == 0 && tfrcls[0] 
		== 3 || s_cmp(kernam, " ", kernam_len, (ftnlen)1) == 0 && 
		s_cmp(arctyp + 7, "DAF/PCK", (ftnlen)7, (ftnlen)7) == 0 && 
		tfrcls[0] == 2) {
	    s_copy(cfrnam, tfrnam, (ftnlen)32, tfrnam_len);
	    cfrcid[0] = tfrcid[0];
	    cfrcls[0] = tfrcls[0];
	    cfrfnd[0] = elemi_(cfrcid, flids2);
	} else if (tfrcls[0] == 2 || tfrcls[0] == 3) {
	    s_copy(cfrnam, tfrnam, (ftnlen)32, tfrnam_len);
	    cfrcid[0] = tfrcid[0];
	    cfrcls[0] = tfrcls[0];
	    cfrfnd[0] = FALSE_;
	}
    }
    if (s_cmp(cfrnam + 32, " ", (ftnlen)32, (ftnlen)1) == 0) {
	if (s_cmp(arctyp + 7, "DAF/CK", (ftnlen)7, (ftnlen)6) == 0 && tfrcls[
		1] == 3 || s_cmp(arctyp + 7, "DAF/PCK", (ftnlen)7, (ftnlen)7) 
		== 0 && tfrcls[1] == 2) {
	    s_copy(cfrnam + 32, tfrnam + tfrnam_len, (ftnlen)32, tfrnam_len);
	    cfrcid[1] = tfrcid[1];
	    cfrcls[1] = tfrcls[1];
	    cfrfnd[1] = elemi_(&cfrcid[1], flids2);
	} else if (tfrcls[1] == 2 || tfrcls[1] == 3) {
	    s_copy(cfrnam + 32, tfrnam + tfrnam_len, (ftnlen)32, tfrnam_len);
	    cfrcid[1] = tfrcid[1];
	    cfrcls[1] = tfrcls[1];
	    cfrfnd[1] = FALSE_;
	}
    }

/*     Now we need to determine the comparison window. */

/*     If we got no coverage frames specified or picked by default from */
/*     given CK or PCK files, the user must have provided start and stop */
/*     times on the command line. If not, we stop. */

    if (s_cmp(cfrnam, " ", (ftnlen)32, (ftnlen)1) == 0 && s_cmp(cfrnam + 32, 
	    " ", (ftnlen)32, (ftnlen)1) == 0 && (s_cmp(time, " ", (ftnlen)
	    1024, (ftnlen)1) == 0 || s_cmp(time + 1024, " ", (ftnlen)1024, (
	    ftnlen)1) == 0)) {
	setmsg_("Cannot determine time range for comparison. Both start time"
		" and stop time must be provided using '#' and '#' keys when "
		"no coverage look up frames and kernels from which coverage c"
		"an be obtained automatically are given on the command line.", 
		(ftnlen)238);
	errch_("#", "-b", (ftnlen)1, (ftnlen)2);
	errch_("#", "-e", (ftnlen)1, (ftnlen)2);
	sigerr_("SPICE(NOTIMEBOUNDS1)", (ftnlen)20);
    }

/*     We are done checking the obvious error cases. Since we know our */
/*     coverage frames, let's try to get coverage windows for each of */
/*     them and do more checks after that. */

/*     For the first coverage frame we may get coverage either from the */
/*     first or the only file or from the combination of kernels */
/*     applicable to both orientations and the first orientation. */

/*     Initialize the first coverage window and description string. */

    ssized_(&c_b114, cover1);
    scardd_(&c__0, cover1);
    ssized_(&c_b114, covera);
    scardd_(&c__0, covera);
    ssized_(&c_b114, coverb);
    scardd_(&c__0, coverb);
    s_copy(covdsc, " ", (ftnlen)1024, (ftnlen)1);
    if (s_cmp(kernam, " ", kernam_len, (ftnlen)1) != 0 && s_cmp(arctyp, "DAF"
	    "/CK", (ftnlen)7, (ftnlen)6) == 0 && cfrcls[0] == 3 && cfrfnd[0] ||
	     s_cmp(kernam, " ", kernam_len, (ftnlen)1) == 0 && s_cmp(arctyp + 
	    7, "DAF/CK", (ftnlen)7, (ftnlen)6) == 0 && cfrcls[0] == 3 && 
	    cfrfnd[0]) {

/*        Try to get coverage for the first coverage frame from the */
/*        first or only CK if */

/*           the first file is present, */
/*           the first file is a CK, */
/*           the frame is a CK frame, and */
/*           data for frame's class ID is present in the file */

/*        OR if */

/*           the first file is not present, */
/*           the second file is a CK, */
/*           the frame is a CK frame, and */
/*           data for frame's class ID is present in the file. */

/*        Since CKCOVR call will need SCLK and LSK data to do time */
/*        conversions, clear loaded kernels and reload the file we will */
/*        look at and all kernels applicable to the first attitude set. */


/*        Set the file we will work with. */

	if (s_cmp(kernam, " ", kernam_len, (ftnlen)1) != 0) {
	    s_copy(hkrnam, kernam, (ftnlen)1024, kernam_len);
	} else {
	    s_copy(hkrnam, kernam + kernam_len, (ftnlen)1024, kernam_len);
	}

/*        Load appropriate kernels. */

	kclear_();
/* Writing concatenation */
	i__8[0] = kernls_len, a__3[0] = kernls + (kernls_len << 1);
	i__8[1] = 1, a__3[1] = " ";
	i__8[2] = kernls_len, a__3[2] = kernls;
	i__8[3] = 1, a__3[3] = " ";
	i__8[4] = 1024, a__3[4] = hkrnam;
	s_cat(hlline, a__3, i__8, &c__5, (ftnlen)5120);
	ldklst_(hlline, (ftnlen)5120);

/*        Get ET coverage window adjusted for round off, at interval */
/*        level, with zero tolerance, and angular rate flag set earlier. */

	ckcovr_(hkrnam, cfrcid, avflg, "INTERVAL", &c_b849, cover1, (ftnlen)
		1024, (ftnlen)8);

/*        If we got an empty window back, report an error. If not, */
/*        make an information string that we may have to use in the */
/*        error message(s) later. */

	if (wncard_(cover1) == 0) {
	    setmsg_("Cannot determine time range for comparison because CK f"
		    "ile '#' does not provide any roundoff-adjusted coverage "
		    "for the CK frame with class ID '#'.", (ftnlen)146);
	    errch_("#", hkrnam, (ftnlen)1, (ftnlen)1024);
	    errint_("#", cfrcid, (ftnlen)1);
	    sigerr_("SPICE(NOTIMEBOUNDS4)", (ftnlen)20);
	} else {

/*           The frame name not being an integer means that we know this */
/*           frame. If so, include frame name and class ID in the */
/*           string. Otherwise, use only frame class ID. */

	    nparsi_(cfrnam, &hint, error, &ptr, (ftnlen)32, (ftnlen)1024);
	    if (ptr != 0) {
		s_copy(covdsc, "The first coverage window, for the CK frame "
			"'#' (class ID '#'), was determined from the CK file "
			"'#'.", (ftnlen)1024, (ftnlen)100);
		repmc_(covdsc, "#", cfrnam, covdsc, (ftnlen)1024, (ftnlen)1, (
			ftnlen)32, (ftnlen)1024);
		repmi_(covdsc, "#", cfrcid, covdsc, (ftnlen)1024, (ftnlen)1, (
			ftnlen)1024);
		repmc_(covdsc, "#", hkrnam, covdsc, (ftnlen)1024, (ftnlen)1, (
			ftnlen)1024, (ftnlen)1024);
	    } else {
		s_copy(covdsc, "The first coverage window, for the CK frame "
			"with class ID '#', was determined from the CK file '"
			"#'.", (ftnlen)1024, (ftnlen)99);
		repmi_(covdsc, "#", cfrcid, covdsc, (ftnlen)1024, (ftnlen)1, (
			ftnlen)1024);
		repmc_(covdsc, "#", hkrnam, covdsc, (ftnlen)1024, (ftnlen)1, (
			ftnlen)1024, (ftnlen)1024);
	    }
	}
    } else if (s_cmp(kernam, " ", kernam_len, (ftnlen)1) != 0 && s_cmp(arctyp,
	     "DAF/PCK", (ftnlen)7, (ftnlen)7) == 0 && cfrcls[0] == 2 && 
	    cfrfnd[0] || s_cmp(kernam, " ", kernam_len, (ftnlen)1) == 0 && 
	    s_cmp(arctyp + 7, "DAF/PCK", (ftnlen)7, (ftnlen)7) == 0 && cfrcls[
	    0] == 2 && cfrfnd[0]) {

/*        Try to get coverage for the first coverage frame from the */
/*        first or only PCK if */

/*           the first file is present, */
/*           the first file is a PCK, */
/*           the frame is a PCK frame, and */
/*           data for frame's class ID is present in the file */

/*        OR if */

/*           the first file is not present, */
/*           the second file is a PCK, */
/*           the frame is a PCK frame, and */
/*           data for frame's class ID is present in the file. */

/*        No need to reload any kernels for PCK coverage look up. */


/*        Set the file we will work with. */

	if (s_cmp(kernam, " ", kernam_len, (ftnlen)1) != 0) {
	    s_copy(hkrnam, kernam, (ftnlen)1024, kernam_len);
	} else {
	    s_copy(hkrnam, kernam + kernam_len, (ftnlen)1024, kernam_len);
	}

/*        Get coverage. */

	pckcov_(hkrnam, cfrcid, cover1, (ftnlen)1024);

/*        I can't think of a way to get empty window out of the */
/*        previous call, but we will check for it anyway. */

/*        If we got an empty window back, report an error. If not, */
/*        make an information string that we may have to use in the */
/*        error message(s) later. */

	if (wncard_(cover1) == 0) {
	    setmsg_("Cannot determine time range for comparison because PCK "
		    "file '#' does not provide any coverage for the PCK frame"
		    " with class ID '#'.", (ftnlen)130);
	    errch_("#", hkrnam, (ftnlen)1, (ftnlen)1024);
	    errint_("#", cfrcid, (ftnlen)1);
	    sigerr_("SPICE(NOTIMEBOUNDS5)", (ftnlen)20);
	} else {

/*           The frame name not being an integer means that we know this */
/*           frame. If so, include frame name and class ID in the */
/*           string. Otherwise, use only frame class ID. */

	    nparsi_(cfrnam, &hint, error, &ptr, (ftnlen)32, (ftnlen)1024);
	    if (ptr != 0) {
		s_copy(covdsc, "The first coverage window, for the PCK frame"
			" '#' (class ID '#'), was determined from PCK file '#"
			"'.", (ftnlen)1024, (ftnlen)98);
		repmc_(covdsc, "#", cfrnam, covdsc, (ftnlen)1024, (ftnlen)1, (
			ftnlen)32, (ftnlen)1024);
		repmi_(covdsc, "#", cfrcid, covdsc, (ftnlen)1024, (ftnlen)1, (
			ftnlen)1024);
		repmc_(covdsc, "#", hkrnam, covdsc, (ftnlen)1024, (ftnlen)1, (
			ftnlen)1024, (ftnlen)1024);
	    } else {
		s_copy(covdsc, "The first coverage window, for the PCK frame"
			" with class ID '#', was determined from PCK file '#'."
			, (ftnlen)1024, (ftnlen)97);
		repmi_(covdsc, "#", cfrcid, covdsc, (ftnlen)1024, (ftnlen)1, (
			ftnlen)1024);
		repmc_(covdsc, "#", hkrnam, covdsc, (ftnlen)1024, (ftnlen)1, (
			ftnlen)1024, (ftnlen)1024);
	    }
	}
    } else if (s_cmp(kernam, " ", kernam_len, (ftnlen)1) != 0 && ! iscpck[0] 
	    && cfrcls[0] == 3 || s_cmp(kernam, " ", kernam_len, (ftnlen)1) == 
	    0 && s_cmp(kernam + kernam_len, " ", kernam_len, (ftnlen)1) != 0 
	    && ! iscpck[1] && cfrcls[0] == 3 || s_cmp(kernam, " ", kernam_len,
	     (ftnlen)1) == 0 && s_cmp(kernam + kernam_len, " ", kernam_len, (
	    ftnlen)1) == 0 && cfrcls[0] == 3) {

/*        Try to get coverage for the first coverage frame from all CKs */
/*        provided for both orientations and the first orientation if */

/*           the first file is present, */
/*           the first file is not a CK or PCK, and */
/*           the frame is a CK frame, */

/*        OR if */

/*           the first file is not present, */
/*           the second file is present, */
/*           the second file is not a CK or PCK, and */
/*           the frame is a CK frame */

/*        OR if */

/*           the first file is not present, */
/*           the second file is not present, and */
/*           the frame is a CK frame */

/*        We need to reload all kernels applicable the first orientation */
/*        to do this look up. First, set the primary file we will work */
/*        with. */

	if (s_cmp(kernam, " ", kernam_len, (ftnlen)1) != 0) {
	    s_copy(hkrnam, kernam, (ftnlen)1024, kernam_len);
	} else {
	    s_copy(hkrnam, kernam + kernam_len, (ftnlen)1024, kernam_len);
	}
	kclear_();
/* Writing concatenation */
	i__8[0] = kernls_len, a__3[0] = kernls + (kernls_len << 1);
	i__8[1] = 1, a__3[1] = " ";
	i__8[2] = kernls_len, a__3[2] = kernls;
	i__8[3] = 1, a__3[3] = " ";
	i__8[4] = 1024, a__3[4] = hkrnam;
	s_cat(hlline, a__3, i__8, &c__5, (ftnlen)5120);
	ldklst_(hlline, (ftnlen)5120);

/*        Get the total count of loaded CKs and, if the count is */
/*        non-zero, proceed to fetch coverage. */

/*        Getting the zero count is not an error at this point as we may */
/*        still come up with a comparison window using the other */
/*        coverage frame and/or begin and end times specified on the */
/*        command line. Still we need to make an information string that */
/*        we may have to use in the error message(s) later. */

	ktotal_("CK", &count, (ftnlen)2);
	if (count == 0) {
	    s_copy(covdsc, "The first coverage window, for #, could not be d"
		    "etermined because no CK files were provided using the '#"
		    "' and/or '#' keys. ", (ftnlen)1024, (ftnlen)123);
	} else {

/*           Loop through all CK files and collect coverage information */
/*           for this class ID. */

	    i__1 = count;
	    for (i__ = 1; i__ <= i__1; ++i__) {

/*              Get the name of the next CK file. */

		kdata_(&i__, "CK", hkrnam, hartyp, hline2, &handle, &found, (
			ftnlen)2, (ftnlen)1024, (ftnlen)7, (ftnlen)1024);
		if (! found) {
		    setmsg_("There is a bug in the program. Please, contact "
			    "NAIF.", (ftnlen)52);
		    sigerr_("SPICE(FRMDIFFBUG3)", (ftnlen)18);
		}

/*              Reset auxiliary coverage window and get ET coverage */
/*              window adjusted for round off, at interval level, with */
/*              zero tolerance, and angular rate flag set earlier for */
/*              this CK. */

		scardd_(&c__0, covera);
		ckcovr_(hkrnam, cfrcid, avflg, "INTERVAL", &c_b849, covera, (
			ftnlen)1024, (ftnlen)8);

/*              Merge auxiliary coverage with the total coverage for */
/*              this frame for all CKs. */

		wnunid_(cover1, covera, coverb);
		copyd_(coverb, cover1);
	    }

/*           Getting an empty window back is still not an error at this */
/*           point. Whether it is empty or not, we need to make an */
/*           information string that we may have to use in the error */
/*           message(s) later. */

	    if (wncard_(cover1) == 0) {
		s_copy(covdsc, "The first coverage window, for #, could not "
			"be determined because the CK files provided using th"
			"e '#' and/or '#' keys contain no data for this frame."
			, (ftnlen)1024, (ftnlen)149);
	    } else {
		s_copy(covdsc, "The first coverage window, for #, was determ"
			"ined from the CK files provided using the '#' and/or"
			" '#' keys.", (ftnlen)1024, (ftnlen)106);
	    }
	}

/*        Finish by making the information string by substituting either */
/*        the frame name and class ID and the kernel command line keys. */

	repmc_(covdsc, "#", "the CK frame '#' (class ID '#')", covdsc, (
		ftnlen)1024, (ftnlen)1, (ftnlen)31, (ftnlen)1024);
	repmc_(covdsc, "#", cfrnam, covdsc, (ftnlen)1024, (ftnlen)1, (ftnlen)
		32, (ftnlen)1024);
	repmi_(covdsc, "#", cfrcid, covdsc, (ftnlen)1024, (ftnlen)1, (ftnlen)
		1024);
	repmc_(covdsc, "#", "-k", covdsc, (ftnlen)1024, (ftnlen)1, (ftnlen)2, 
		(ftnlen)1024);
	repmc_(covdsc, "#", "-k1", covdsc, (ftnlen)1024, (ftnlen)1, (ftnlen)3,
		 (ftnlen)1024);
    } else if (s_cmp(kernam, " ", kernam_len, (ftnlen)1) != 0 && ! iscpck[0] 
	    && cfrcls[0] == 2 || s_cmp(kernam, " ", kernam_len, (ftnlen)1) == 
	    0 && s_cmp(kernam + kernam_len, " ", kernam_len, (ftnlen)1) != 0 
	    && ! iscpck[1] && cfrcls[0] == 2 || s_cmp(kernam, " ", kernam_len,
	     (ftnlen)1) == 0 && s_cmp(kernam + kernam_len, " ", kernam_len, (
	    ftnlen)1) == 0 && cfrcls[0] == 2) {

/*        Try to get coverage for the first coverage frame from all PCKs */
/*        provided for both orientations and the first orientation if */

/*           the first file is present, */
/*           the first file is not a CK or PCK, and */
/*           the frame is a PCK frame, */

/*        OR if */

/*           the first file is not present, */
/*           the second file is present, */
/*           the second file is not a CK or PCK, and */
/*           the frame is a PCK frame */

/*        OR if */

/*           the first file is not present, */
/*           the second file is not present, and */
/*           the frame is a PCK frame */

/*        We need to reload all kernels applicable the first orientation */
/*        to do this look up. */

	if (s_cmp(kernam, " ", kernam_len, (ftnlen)1) != 0) {
	    s_copy(hkrnam, kernam, (ftnlen)1024, kernam_len);
	} else {
	    s_copy(hkrnam, kernam + kernam_len, (ftnlen)1024, kernam_len);
	}
	kclear_();
/* Writing concatenation */
	i__8[0] = kernls_len, a__3[0] = kernls + (kernls_len << 1);
	i__8[1] = 1, a__3[1] = " ";
	i__8[2] = kernls_len, a__3[2] = kernls;
	i__8[3] = 1, a__3[3] = " ";
	i__8[4] = 1024, a__3[4] = hkrnam;
	s_cat(hlline, a__3, i__8, &c__5, (ftnlen)5120);
	ldklst_(hlline, (ftnlen)5120);

/*        Get the total count of loaded binary PCKs and, if the count is */
/*        non-zero, proceed to fetch coverage. */

/*        Getting the zero count is not an error at this point as we may */
/*        still come up with a comparison window using the other */
/*        coverage frame and/or begin and end times specified on the */
/*        command line. Still we need to make an information string that */
/*        we may have to use in the error message(s) later. */

	ktotal_("PCK", &count, (ftnlen)3);
	if (count == 0) {
	    s_copy(covdsc, "The first coverage window, for #, could not be d"
		    "etermined because no binary PCK files were provided usin"
		    "g the '#' and/or '#' keys. ", (ftnlen)1024, (ftnlen)131);
	} else {

/*           Loop through all binary PCK files and collect coverage */
/*           information for this class ID. */

	    i__1 = count;
	    for (i__ = 1; i__ <= i__1; ++i__) {

/*              Get the name of the next binary PCK file. */

		kdata_(&i__, "PCK", hkrnam, hartyp, hline2, &handle, &found, (
			ftnlen)3, (ftnlen)1024, (ftnlen)7, (ftnlen)1024);
		if (! found) {
		    setmsg_("There is a bug in the program. Please, contact "
			    "NAIF.", (ftnlen)52);
		    sigerr_("SPICE(FRMDIFFBUG4)", (ftnlen)18);
		}

/*              Add coverage for this frame from this binary PCK to */
/*              the total coverage from all binary PCKs */

		pckcov_(hkrnam, cfrcid, cover1, (ftnlen)1024);
	    }

/*           Getting an empty window back is still not an error at this */
/*           point. Whether it is empty or not, we need to make an */
/*           information string that we may have to use in the error */
/*           message(s) later. */

	    if (wncard_(cover1) == 0) {
		s_copy(covdsc, "The first coverage window, for #, could not "
			"be determined because binary PCK files provided usin"
			"g the '#' and/or '#' keys contain no data for this f"
			"rame.", (ftnlen)1024, (ftnlen)153);
	    } else {
		s_copy(covdsc, "The first coverage window, for #, was determ"
			"ined from binary PCK files provided using the '#' an"
			"d/or '#' keys.", (ftnlen)1024, (ftnlen)110);
	    }
	}

/*        Finish by making the information string by substituting either */
/*        the frame name and class ID and the kernel command line keys. */

	repmc_(covdsc, "#", "the PCK frame '#' (class ID '#')", covdsc, (
		ftnlen)1024, (ftnlen)1, (ftnlen)32, (ftnlen)1024);
	repmc_(covdsc, "#", cfrnam, covdsc, (ftnlen)1024, (ftnlen)1, (ftnlen)
		32, (ftnlen)1024);
	repmi_(covdsc, "#", cfrcid, covdsc, (ftnlen)1024, (ftnlen)1, (ftnlen)
		1024);
	repmc_(covdsc, "#", "-k", covdsc, (ftnlen)1024, (ftnlen)1, (ftnlen)2, 
		(ftnlen)1024);
	repmc_(covdsc, "#", "-k1", covdsc, (ftnlen)1024, (ftnlen)1, (ftnlen)3,
		 (ftnlen)1024);
    }

/*     For the second coverage frame we will try to get coverage from */
/*     the second or only file or from the combination of kernels */
/*     applicable to both orientations and the second orientation. */

/*     Initialize the second coverage window and description string. */

    ssized_(&c_b114, cover2);
    scardd_(&c__0, cover2);
    scardd_(&c__0, covera);
    scardd_(&c__0, coverb);
    s_copy(covdsc + 1024, " ", (ftnlen)1024, (ftnlen)1);
    if (s_cmp(arctyp + 7, "DAF/CK", (ftnlen)7, (ftnlen)6) == 0 && cfrcls[1] ==
	     3 && cfrfnd[1]) {

/*        Get coverage for the second coverage frame from the second CK */
/*        file if */

/*           the second file is a CK, */
/*           the frame is a CK frame, and */
/*           data for frame's class ID is present in the file. */

/*        Since CKCOV call will need SCLK and LSK data to do time */
/*        conversions, clear loaded kernels and reload the second file */
/*        and all kernels applicable to the second attitude set. */


/*        Load appropriate kernels. */

	kclear_();
/* Writing concatenation */
	i__8[0] = kernls_len, a__3[0] = kernls + (kernls_len << 1);
	i__8[1] = 1, a__3[1] = " ";
	i__8[2] = kernls_len, a__3[2] = kernls + kernls_len;
	i__8[3] = 1, a__3[3] = " ";
	i__8[4] = kernam_len, a__3[4] = kernam + kernam_len;
	s_cat(hlline, a__3, i__8, &c__5, (ftnlen)5120);
	ldklst_(hlline, (ftnlen)5120);

/*        Get ET coverage window adjusted for round off, at interval */
/*        level, with zero tolerance, and angular rate flag set earlier. */

	ckcovr_(kernam + kernam_len, &cfrcid[1], avflg, "INTERVAL", &c_b849, 
		cover2, kernam_len, (ftnlen)8);

/*        If we got an empty window back, report an error. If not, */
/*        make an information string that we may have to use in the */
/*        error message(s) later. */

	if (wncard_(cover2) == 0) {
	    setmsg_("Cannot determine time range for comparison because CK f"
		    "ile '#' does not provide any roundoff-adjusted coverage "
		    "for the CK frame with class ID '#'.", (ftnlen)146);
	    errch_("#", kernam + kernam_len, (ftnlen)1, kernam_len);
	    errint_("#", &cfrcid[1], (ftnlen)1);
	    sigerr_("SPICE(NOTIMEBOUNDS6)", (ftnlen)20);
	} else {

/*           The frame name not being an integer means that we know this */
/*           frame. If so, include frame name and class ID in the */
/*           string. Otherwise, use only frame class ID. */

	    nparsi_(cfrnam + 32, &hint, error, &ptr, (ftnlen)32, (ftnlen)1024)
		    ;
	    if (ptr != 0) {
		s_copy(covdsc + 1024, "The second coverage window, for the C"
			"K frame '#' (class ID '#'), was determined from the "
			"CK file '#'.", (ftnlen)1024, (ftnlen)101);
		repmc_(covdsc + 1024, "#", cfrnam + 32, covdsc + 1024, (
			ftnlen)1024, (ftnlen)1, (ftnlen)32, (ftnlen)1024);
		repmi_(covdsc + 1024, "#", &cfrcid[1], covdsc + 1024, (ftnlen)
			1024, (ftnlen)1, (ftnlen)1024);
		repmc_(covdsc + 1024, "#", kernam + kernam_len, covdsc + 1024,
			 (ftnlen)1024, (ftnlen)1, kernam_len, (ftnlen)1024);
	    } else {
		s_copy(covdsc + 1024, "The second coverage window, for the C"
			"K frame with class ID '#', was determined from the C"
			"K file '#'.", (ftnlen)1024, (ftnlen)100);
		repmi_(covdsc + 1024, "#", &cfrcid[1], covdsc + 1024, (ftnlen)
			1024, (ftnlen)1, (ftnlen)1024);
		repmc_(covdsc + 1024, "#", kernam + kernam_len, covdsc + 1024,
			 (ftnlen)1024, (ftnlen)1, kernam_len, (ftnlen)1024);
	    }
	}
    } else if (s_cmp(arctyp + 7, "DAF/PCK", (ftnlen)7, (ftnlen)7) == 0 && 
	    cfrcls[1] == 2 && cfrfnd[1]) {

/*        Get coverage for the second coverage frame from the second PCK */
/*        file if */

/*           the second file is is a PCK, */
/*           the frame is a PCK frame, and */
/*           data for frame's class ID is present in the file. */

/*        No need to reload any kernels for PCK coverage look up. */

/*        Get coverage. */

	pckcov_(kernam + kernam_len, &cfrcid[1], cover2, kernam_len);

/*        I can't think of a way to get empty window out of the */
/*        previous call, but we will check for it anyway. */

/*        If we got an empty window back, report an error. If not, */
/*        make an information string that we may have to use in the */
/*        error message(s) later. */

	if (wncard_(cover2) == 0) {
	    setmsg_("Cannot determine time range for comparison because PCK "
		    "file '#' does not provide any coverage for the PCK frame"
		    " with class ID '#'.", (ftnlen)130);
	    errch_("#", kernam + kernam_len, (ftnlen)1, kernam_len);
	    errint_("#", &cfrcid[1], (ftnlen)1);
	    sigerr_("SPICE(NOTIMEBOUNDS7)", (ftnlen)20);
	} else {

/*           The frame name not being an integer means that we know this */
/*           frame. If so, include frame name and class ID in the */
/*           string. Otherwise, use only frame class ID. */

	    nparsi_(cfrnam + 32, &hint, error, &ptr, (ftnlen)32, (ftnlen)1024)
		    ;
	    if (ptr != 0) {
		s_copy(covdsc + 1024, "The second coverage window, for the P"
			"CK frame '#' (class ID '#'), was determined from the"
			" PCK file '#'.", (ftnlen)1024, (ftnlen)103);
		repmc_(covdsc + 1024, "#", cfrnam + 32, covdsc + 1024, (
			ftnlen)1024, (ftnlen)1, (ftnlen)32, (ftnlen)1024);
		repmi_(covdsc + 1024, "#", &cfrcid[1], covdsc + 1024, (ftnlen)
			1024, (ftnlen)1, (ftnlen)1024);
		repmc_(covdsc + 1024, "#", kernam + kernam_len, covdsc + 1024,
			 (ftnlen)1024, (ftnlen)1, kernam_len, (ftnlen)1024);
	    } else {
		s_copy(covdsc + 1024, "The second coverage window, for the P"
			"CK frame with class ID  '#', was determined from the"
			" PCK file '#'.", (ftnlen)1024, (ftnlen)103);
		repmi_(covdsc + 1024, "#", &cfrcid[1], covdsc + 1024, (ftnlen)
			1024, (ftnlen)1, (ftnlen)1024);
		repmc_(covdsc + 1024, "#", kernam + kernam_len, covdsc + 1024,
			 (ftnlen)1024, (ftnlen)1, kernam_len, (ftnlen)1024);
	    }
	}
    } else if (s_cmp(kernam + kernam_len, " ", kernam_len, (ftnlen)1) != 0 && 
	    ! iscpck[1] && cfrcls[1] == 3 || s_cmp(kernam + kernam_len, " ", 
	    kernam_len, (ftnlen)1) == 0 && cfrcls[1] == 3) {

/*        Try to get coverage for the second coverage frame from all */
/*        CKs provided for both orientations and the second orientation */
/*        if */

/*           the second file is present, */
/*           the second file is not a CK or PCK, and */
/*           the frame is a CK frame, */

/*        OR if */

/*           the second file is not present, */
/*           the frame is a CK frame */

/*        We need to reload all kernels applicable the second orientation */
/*        to do this look up. */

	kclear_();
/* Writing concatenation */
	i__8[0] = kernls_len, a__3[0] = kernls + (kernls_len << 1);
	i__8[1] = 1, a__3[1] = " ";
	i__8[2] = kernls_len, a__3[2] = kernls + kernls_len;
	i__8[3] = 1, a__3[3] = " ";
	i__8[4] = kernam_len, a__3[4] = kernam + kernam_len;
	s_cat(hlline, a__3, i__8, &c__5, (ftnlen)5120);
	ldklst_(hlline, (ftnlen)5120);

/*        Get the total count of loaded CKs and, if the count is */
/*        non-zero, proceed to fetch coverage. */

/*        Getting the zero count is not an error at this point as we may */
/*        still come up with a comparison window using the other */
/*        coverage frame and/or begin and end times specified on the */
/*        command line. Still we need to make an information string that */
/*        we may have to use in the error message(s) later. */

	ktotal_("CK", &count, (ftnlen)2);
	if (count == 0) {
	    s_copy(covdsc + 1024, "The second coverage window, for #, could "
		    "not be determined because no CK files were provided usin"
		    "g the '#' and/or '#' keys. ", (ftnlen)1024, (ftnlen)124);
	} else {

/*           Loop through all CK files and collect coverage information */
/*           for this class ID. */

	    i__1 = count;
	    for (i__ = 1; i__ <= i__1; ++i__) {

/*              Get the name of the next CK file. */

		kdata_(&i__, "CK", hkrnam, hartyp, hline2, &handle, &found, (
			ftnlen)2, (ftnlen)1024, (ftnlen)7, (ftnlen)1024);
		if (! found) {
		    setmsg_("There is a bug in the program. Please, contact "
			    "NAIF.", (ftnlen)52);
		    sigerr_("SPICE(FRMDIFFBUG5)", (ftnlen)18);
		}

/*              Reset auxiliary coverage window and get ET coverage */
/*              window adjusted for round off, at interval level, with */
/*              zero tolerance, and angular rate flag set earlier for */
/*              this CK. */

		scardd_(&c__0, covera);
		ckcovr_(hkrnam, &cfrcid[1], avflg, "INTERVAL", &c_b849, 
			covera, (ftnlen)1024, (ftnlen)8);

/*              Merge auxiliary coverage with the total coverage for */
/*              this frame for all CKs. */

		wnunid_(cover2, covera, coverb);
		copyd_(coverb, cover2);
	    }

/*           Getting an empty window back is still not an error at this */
/*           point. Whether it is empty or not, we need to make an */
/*           information string that we may have to use in the error */
/*           message(s) later. */

	    if (wncard_(cover2) == 0) {
		s_copy(covdsc + 1024, "The second coverage window, for #, co"
			"uld not be determined because the CK files provided "
			"using the '#' and/or '#' keys contain no data for th"
			"is frame.", (ftnlen)1024, (ftnlen)150);
	    } else {
		s_copy(covdsc + 1024, "The second coverage window, for #, wa"
			"s determined from the CK files provided using the '#"
			"' and/or '#' keys.", (ftnlen)1024, (ftnlen)107);
	    }
	}

/*        Finish by making the information string by substituting either */
/*        the frame name and class ID and the kernel command line keys. */

	repmc_(covdsc + 1024, "#", "the CK frame '#' (class ID '#')", covdsc 
		+ 1024, (ftnlen)1024, (ftnlen)1, (ftnlen)31, (ftnlen)1024);
	repmc_(covdsc + 1024, "#", cfrnam + 32, covdsc + 1024, (ftnlen)1024, (
		ftnlen)1, (ftnlen)32, (ftnlen)1024);
	repmi_(covdsc + 1024, "#", &cfrcid[1], covdsc + 1024, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024);
	repmc_(covdsc + 1024, "#", "-k", covdsc + 1024, (ftnlen)1024, (ftnlen)
		1, (ftnlen)2, (ftnlen)1024);
	repmc_(covdsc + 1024, "#", "-k2", covdsc + 1024, (ftnlen)1024, (
		ftnlen)1, (ftnlen)3, (ftnlen)1024);
    } else if (s_cmp(kernam + kernam_len, " ", kernam_len, (ftnlen)1) != 0 && 
	    ! iscpck[1] && cfrcls[1] == 2 || s_cmp(kernam + kernam_len, " ", 
	    kernam_len, (ftnlen)1) == 0 && cfrcls[1] == 2) {

/*        Try to get coverage for the second coverage frame from all */
/*        PCKs provided for both orientations and the second orientation */
/*        if */

/*           the second file is present, */
/*           the second file is not a CK or PCK, and */
/*           the frame is a PCK frame, */

/*        OR if */

/*           the second file is not present, */
/*           the frame is a PCK frame */

/*        We need to reload all kernels applicable the second orientation */
/*        to do this look up. */

	kclear_();
/* Writing concatenation */
	i__8[0] = kernls_len, a__3[0] = kernls + (kernls_len << 1);
	i__8[1] = 1, a__3[1] = " ";
	i__8[2] = kernls_len, a__3[2] = kernls + kernls_len;
	i__8[3] = 1, a__3[3] = " ";
	i__8[4] = kernam_len, a__3[4] = kernam + kernam_len;
	s_cat(hlline, a__3, i__8, &c__5, (ftnlen)5120);
	ldklst_(hlline, (ftnlen)5120);

/*        Get the total count of loaded binary PCKs and, if the count is */
/*        non-zero, proceed to fetch coverage. */

/*        Getting the zero count is not an error at this point as we may */
/*        still come up with a comparison window using the other */
/*        coverage frame and/or begin and end times specified on the */
/*        command line. Still we need to make an information string that */
/*        we may have to use in the error message(s) later. */

	ktotal_("PCK", &count, (ftnlen)3);
	if (count == 0) {
	    s_copy(covdsc + 1024, "The second coverage window, for #, could "
		    "not be determined because no binary PCK files were provi"
		    "ded using the '#' and/or '#' keys. ", (ftnlen)1024, (
		    ftnlen)132);
	} else {

/*           Loop through all binary PCK files and collect coverage */
/*           information for this class ID. */

	    i__1 = count;
	    for (i__ = 1; i__ <= i__1; ++i__) {

/*              Get the name of the next binary PCK file. */

		kdata_(&i__, "PCK", hkrnam, hartyp, hline2, &handle, &found, (
			ftnlen)3, (ftnlen)1024, (ftnlen)7, (ftnlen)1024);
		if (! found) {
		    setmsg_("There is a bug in the program. Please, contact "
			    "NAIF.", (ftnlen)52);
		    sigerr_("SPICE(FRMDIFFBUG6)", (ftnlen)18);
		}

/*              Add coverage for this frame from this binary PCK to */
/*              the total coverage from all binary PCKs */

		pckcov_(hkrnam, &cfrcid[1], cover2, (ftnlen)1024);
	    }

/*           Getting an empty window back is still not an error at this */
/*           point. Whether it is empty or not, we need to make an */
/*           information string that we may have to use in the error */
/*           message(s) later. */

	    if (wncard_(cover2) == 0) {
		s_copy(covdsc + 1024, "The second coverage window, for #, co"
			"uld not be determined because binary PCK files provi"
			"ded using the '#' and/or '#' keys contain no data fo"
			"r this frame.", (ftnlen)1024, (ftnlen)154);
	    } else {
		s_copy(covdsc + 1024, "The second coverage window, for #, wa"
			"s determined from binary PCK files provided using th"
			"e '#' and/or '#' keys.", (ftnlen)1024, (ftnlen)111);
	    }
	}

/*        Finish by making the information string by substituting either */
/*        the frame name and class ID and the kernel command line keys. */

	repmc_(covdsc + 1024, "#", "the PCK frame '#' (class ID '#')", covdsc 
		+ 1024, (ftnlen)1024, (ftnlen)1, (ftnlen)32, (ftnlen)1024);
	repmc_(covdsc + 1024, "#", cfrnam + 32, covdsc + 1024, (ftnlen)1024, (
		ftnlen)1, (ftnlen)32, (ftnlen)1024);
	repmi_(covdsc + 1024, "#", &cfrcid[1], covdsc + 1024, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024);
	repmc_(covdsc + 1024, "#", "-k", covdsc + 1024, (ftnlen)1024, (ftnlen)
		1, (ftnlen)2, (ftnlen)1024);
	repmc_(covdsc + 1024, "#", "-k2", covdsc + 1024, (ftnlen)1024, (
		ftnlen)1, (ftnlen)3, (ftnlen)1024);
    }

/*     Check that either both begin and end time are specified or that */
/*     at least one of the coverage windows is not empty. If not, report */
/*     a bug because at this point we should have had enough information */
/*     to determine final comparison window. (At the moment I can think */
/*     only these cases when this can happen: either or both ``to'' */
/*     frames were were given and are non-CK/PCK frames or are CK/PCK */
/*     frames but with no data in the files.) */

    if (wncard_(cover1) == 0 && wncard_(cover2) == 0 && (s_cmp(time, " ", (
	    ftnlen)1024, (ftnlen)1) == 0 || s_cmp(time + 1024, " ", (ftnlen)
	    1024, (ftnlen)1) == 0)) {
	setmsg_("Insufficient information was provided on the command line t"
		"o determine time range for comparison. If the program is run"
		" with kernel sets that include CKs and/or binary PCKs, the '"
		"#' and/or '#' keys can be used to direct the program to dete"
		"rmine default coverage using data from the files. Otherwise,"
		" both of the '#' and '#' keys must be used to specify the st"
		"art and stop times of the comparison window.", (ftnlen)403);
	errch_("#", "-c1", (ftnlen)1, (ftnlen)3);
	errch_("#", "-c2", (ftnlen)1, (ftnlen)3);
	errch_("#", "-b", (ftnlen)1, (ftnlen)2);
	errch_("#", "-e", (ftnlen)1, (ftnlen)2);
	sigerr_("SPICE(NOTIMEBOUNDS8)", (ftnlen)20);
    }

/*     Wrap up determining the final comparison window by intersecting */
/*     the two coverage windows and the window made using begin and end */
/*     time given on the command line. We will do it in a rather long */
/*     "if" block instead of a simple way -- setting undefined windows */
/*     to (DPMIN,DPMAX), intersecting all three of them, and reporting */
/*     error if there is no intersection -- because we want report more */
/*     meaningful errors. First, make a constraint window using start */
/*     and stop time provided on the command line. Note that ET(1) is */
/*     either a real time provided on the command line or DPMIN while */
/*     ET(2) is either a real time provided on the command line or */
/*     DPMAX. */

    ssized_(&c__2, coverc);
    scardd_(&c__0, coverc);
    wninsd_(et, &et[1], coverc);

/*     Now check if we have determined both, one, or no coverage windows */
/*     from the file(s). */

    if (wncard_(cover1) != 0 && wncard_(cover2) != 0) {

/*        Both coverage windows are not empty. Intersect them and report */
/*        an error if intersection is empty. */

	wnintd_(cover1, cover2, cmpwin);
	if (wncard_(cmpwin) == 0) {
	    setmsg_("Cannot determine time range for comparison because cove"
		    "rage windows obtained by examining kernels provided on t"
		    "he command line do not overlap. # #", (ftnlen)146);
	    errch_("#", covdsc, (ftnlen)1, (ftnlen)1024);
	    if (s_cmp(covdsc, covdsc + 1024, (ftnlen)1024, (ftnlen)1024) != 0)
		     {
		errch_("#", covdsc + 1024, (ftnlen)1, (ftnlen)1024);
	    } else {
		errch_("#", " ", (ftnlen)1, (ftnlen)1);
	    }
	    sigerr_("SPICE(NOTIMEBOUNDS9)", (ftnlen)20);
	}

/*        Intersect result window with the start/stop window and place */
/*        it in a temporary window. Report an error if intersection is */
/*        empty. */

	wnintd_(cmpwin, coverc, cover1);
	if (wncard_(cover1) == 0) {
	    setmsg_("Cannot determine time range for comparison because the "
		    "intersection of the coverage windows obtained by examini"
		    "ng kernels provided on the command line does not overlap"
		    " the time range specified in the command line using '#' "
		    "and/or '#' keys. # # # ", (ftnlen)246);
	    errch_("#", "-b", (ftnlen)1, (ftnlen)2);
	    errch_("#", "-e", (ftnlen)1, (ftnlen)2);
	    errch_("#", covdsc, (ftnlen)1, (ftnlen)1024);
	    if (s_cmp(covdsc, covdsc + 1024, (ftnlen)1024, (ftnlen)1024) != 0)
		     {
		errch_("#", covdsc + 1024, (ftnlen)1, (ftnlen)1024);
	    } else {
		errch_("#", " ", (ftnlen)1, (ftnlen)1);
	    }
	    errch_("#", timdsc, (ftnlen)1, (ftnlen)1024);
	    sigerr_("SPICE(NOTIMEBOUNDS10)", (ftnlen)21);
	}

/*        Copy temporary window to final window. */

	copyd_(cover1, cmpwin);
    } else if (wncard_(cover1) != 0) {

/*        Only first coverage window is not empty. Intersect it with the */
/*        start/stop window. Report an error if intersection is empty. */

	wnintd_(cover1, coverc, cmpwin);
	if (wncard_(cmpwin) == 0) {
	    setmsg_("Cannot determine time range for comparison because the "
		    "coverage window obtained by examining the kernels provid"
		    "ed on the command line does not overlap the time range s"
		    "pecified in the command line using '#' and/or '#' keys. "
		    "# # #", (ftnlen)228);
	    errch_("#", "-b", (ftnlen)1, (ftnlen)2);
	    errch_("#", "-e", (ftnlen)1, (ftnlen)2);
	    errch_("#", covdsc, (ftnlen)1, (ftnlen)1024);
	    errch_("#", covdsc + 1024, (ftnlen)1, (ftnlen)1024);
	    errch_("#", timdsc, (ftnlen)1, (ftnlen)1024);
	    sigerr_("SPICE(NOTIMEBOUNDS11)", (ftnlen)21);
	}
    } else if (wncard_(cover2) != 0) {

/*        Only second coverage window is not empty. Intersect it with */
/*        the start/stop window. Report an error if intersection is */
/*        empty. */

	wnintd_(cover2, coverc, cmpwin);
	if (wncard_(cmpwin) == 0) {
	    setmsg_("Cannot determine time range for comparison because the "
		    "coverage window obtained by examining the kernels provid"
		    "ed on the command line does not overlap the time range s"
		    "pecified in the command line using '#' and/or '#' keys. "
		    "# # #", (ftnlen)228);
	    errch_("#", "-b", (ftnlen)1, (ftnlen)2);
	    errch_("#", "-e", (ftnlen)1, (ftnlen)2);
	    errch_("#", covdsc, (ftnlen)1, (ftnlen)1024);
	    errch_("#", covdsc + 1024, (ftnlen)1, (ftnlen)1024);
	    errch_("#", timdsc, (ftnlen)1, (ftnlen)1024);
	    sigerr_("SPICE(NOTIMEBOUNDS12)", (ftnlen)21);
	}
    } else {

/*        Both coverage windows are empty. Copy the window set using */
/*        start and stop time to final comparison window. */

	copyd_(coverc, cmpwin);

/*        Sanity check. If either start or stop time is blank, something */
/*        is wrong with the code determining coverages. Report a bug. */

	if (s_cmp(time, " ", (ftnlen)1024, (ftnlen)1) == 0 || s_cmp(time + 
		1024, " ", (ftnlen)1024, (ftnlen)1) == 0) {
	    setmsg_("There is a bug in the program. Please, contact NAIF.", (
		    ftnlen)52);
	    sigerr_("SPICE(FRMDIFFBUG1)", (ftnlen)18);
	}
    }

/*     Check command line for the rest of arguments. Unlike the */
/*     attributes of the first and second attitude sets and comparison */
/*     window, the rest of argument are truly optional. */

/*     Start with the type of output. Default type is basic. */

    s_copy(diftyp, "basic", diftyp_len, (ftnlen)5);
    i__ = isrchc_("-t", &c__25, clkeys, (ftnlen)2, (ftnlen)32);
    if (clflag[(i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge("clflag", 
	    i__1, "chwcml_", (ftnlen)3297)]) {
	s_copy(diftyp, clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : 
		s_rnge("clvals", i__1, "chwcml_", (ftnlen)3299)) << 10), 
		diftyp_len, (ftnlen)1024);
	if (! (eqstr_(diftyp, "basic", diftyp_len, (ftnlen)5) || eqstr_(
		diftyp, "stats", diftyp_len, (ftnlen)5) || eqstr_(diftyp, 
		"dumpm", diftyp_len, (ftnlen)5) || eqstr_(diftyp, "dumpqs", 
		diftyp_len, (ftnlen)6) || eqstr_(diftyp, "dumpqo", diftyp_len,
		 (ftnlen)6) || eqstr_(diftyp, "dumpea", diftyp_len, (ftnlen)6)
		 || eqstr_(diftyp, "dumpaa", diftyp_len, (ftnlen)6) || eqstr_(
		diftyp, "dumpc", diftyp_len, (ftnlen)5) || eqstr_(diftyp, 
		"dumpg", diftyp_len, (ftnlen)5))) {
	    setmsg_("Output type '#' specified after '#' key is not recogniz"
		    "ed. Recognized output types are '#', '#', '#', '#', '#',"
		    " '#', '#', '#', and '#'.", (ftnlen)135);
	    errch_("#", clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 :
		     s_rnge("clvals", i__1, "chwcml_", (ftnlen)3316)) << 10), 
		    (ftnlen)1, (ftnlen)1024);
	    errch_("#", "-t", (ftnlen)1, (ftnlen)2);
	    errch_("#", "basic", (ftnlen)1, (ftnlen)5);
	    errch_("#", "stats", (ftnlen)1, (ftnlen)5);
	    errch_("#", "dumpaa", (ftnlen)1, (ftnlen)6);
	    errch_("#", "dumpm", (ftnlen)1, (ftnlen)5);
	    errch_("#", "dumpqs", (ftnlen)1, (ftnlen)6);
	    errch_("#", "dumpqo", (ftnlen)1, (ftnlen)6);
	    errch_("#", "dumpea", (ftnlen)1, (ftnlen)6);
	    errch_("#", "dumpc", (ftnlen)1, (ftnlen)5);
	    errch_("#", "dumpg", (ftnlen)1, (ftnlen)5);
	    sigerr_("SPICE(BADOUTPUTTYPE)", (ftnlen)20);
	}
    }

/*     Next, get time step or number of steps. If both are specified, */
/*     time step has higher priority and, for this reason, should be */
/*     processed first. Default step is zero (meaning */
/*     "not set") and default number of points is a parameter set */
/*     in the include file. */

    *step = 0.;
    *nitr = 1000;
    i__ = isrchc_("-s", &c__25, clkeys, (ftnlen)2, (ftnlen)32);
    if (clflag[(i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge("clflag", 
	    i__1, "chwcml_", (ftnlen)3345)]) {

/*        Is the step a DP number? */

	nparsd_(clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge(
		"clvals", i__1, "chwcml_", (ftnlen)3350)) << 10), step, error,
		 &ptr, (ftnlen)1024, (ftnlen)1024);
	if (ptr == 0) {

/*           Check that step is a positive number and is greater that */
/*           the smallest step we can allow. */

	    if (*step < 1e-8) {
		setmsg_("Time step '#' specified after '#' key is smaller th"
			"an # seconds.", (ftnlen)64);
		errch_("#", clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? 
			i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)3362)
			) << 10), (ftnlen)1, (ftnlen)1024);
		errch_("#", "-s", (ftnlen)1, (ftnlen)2);
		errdp_("#", &c_b1112, (ftnlen)1);
		sigerr_("SPICE(STEPTOOSMALL1)", (ftnlen)20);
	    }

/*           Compute the number of steps that will be required to */
/*           step over the comparison window with this step. To do */
/*           that, we loop over the window and increment the total */
/*           step count by 2 (one interval's start and one for stop) */
/*           plus whatever number of steps fits within the interval. */

	    *nitr = 0;
	    i__1 = wncard_(cmpwin);
	    for (i__ = 1; i__ <= i__1; ++i__) {

/*              Fetch endpoints of the next interval. */

		wnfetd_(cmpwin, &i__, et, &et[1]);

/*              Add one step for start of the interval. */

		++(*nitr);

/*              Add one step for each point between endpoints */
/*              up to the end of the interval minus padding. */

		hdp2 = et[1] - *step * .5;
		count = 1;
		hdp1 = et[0] + *step * count;
		while(hdp1 < hdp2 && *nitr < 1000000) {
		    ++(*nitr);
		    ++count;
		    hdp1 = et[0] + *step * count;
		}

/*              If interval begin time is not equal to interval end */
/*              time add one step for the end of the interval. If we */
/*              terminated the loop with NITR equal to MAXITR this */
/*              will take us over the allowed maximum and will */
/*              trigger the error message that follows. */

		if (et[0] != et[1]) {
		    ++(*nitr);
		}
	    }

/*           Check if this number of points fits into the buffer. */

	    if (*nitr > 1000000) {
		setmsg_("The number of points within the comparison window d"
			"etermined using step of '#' seconds specified after "
			"the command line key '#' is greater than can fit int"
			"o program's buffers (# epochs maximum.) Increase the"
			" step or use command line keys '#' and '#' to make t"
			"he time window smaller in order to run the program. "
			"The comparison window was #. # # #", (ftnlen)345);
		errdp_("#", step, (ftnlen)1);
		errch_("#", "-s", (ftnlen)1, (ftnlen)2);
		errint_("#", &c_b114, (ftnlen)1);
		errch_("#", "-b", (ftnlen)1, (ftnlen)2);
		errch_("#", "-e", (ftnlen)1, (ftnlen)2);
		if (s_cmp(timdsc, " ", (ftnlen)1024, (ftnlen)1) != 0) {
		    if (s_cmp(covdsc, " ", (ftnlen)1024, (ftnlen)1) != 0 && 
			    s_cmp(covdsc + 1024, " ", (ftnlen)1024, (ftnlen)1)
			     != 0) {
			errch_("#", "determined by applying constraints spec"
				"ified on the command line to the intersectio"
				"n of coverages obtained from the files", (
				ftnlen)1, (ftnlen)121);
		    } else if (s_cmp(covdsc, " ", (ftnlen)1024, (ftnlen)1) != 
			    0 || s_cmp(covdsc + 1024, " ", (ftnlen)1024, (
			    ftnlen)1) != 0) {
			errch_("#", "determined by applying constraints spec"
				"ified on the command line to the coverage ob"
				"tained from the file", (ftnlen)1, (ftnlen)103)
				;
		    } else {
			errch_("#", "specified on the command line", (ftnlen)
				1, (ftnlen)29);
		    }
		} else {
		    if (s_cmp(covdsc, " ", (ftnlen)1024, (ftnlen)1) != 0 && 
			    s_cmp(covdsc + 1024, " ", (ftnlen)1024, (ftnlen)1)
			     != 0) {
			errch_("#", "determined by intersecting coverages ob"
				"tained from the files", (ftnlen)1, (ftnlen)60)
				;
		    } else if (s_cmp(covdsc, " ", (ftnlen)1024, (ftnlen)1) != 
			    0 || s_cmp(covdsc + 1024, " ", (ftnlen)1024, (
			    ftnlen)1) != 0) {
			errch_("#", "determined from the file", (ftnlen)1, (
				ftnlen)24);
		    } else {

/*                    We can never hit this branch. Set replacement */
/*                    string to indicate a bug. */

			errch_("#", "not determined properly due to a bug in"
				" the program. Please, contact NAIF.", (ftnlen)
				1, (ftnlen)74);
		    }
		}
		errch_("#", covdsc, (ftnlen)1, (ftnlen)1024);
		if (s_cmp(covdsc, covdsc + 1024, (ftnlen)1024, (ftnlen)1024) 
			!= 0) {
		    errch_("#", covdsc + 1024, (ftnlen)1, (ftnlen)1024);
		} else {
		    errch_("#", " ", (ftnlen)1, (ftnlen)1);
		}
		errch_("#", timdsc, (ftnlen)1, (ftnlen)1024);
		sigerr_("SPICE(STEPTOOSMALL2)", (ftnlen)20);
	    }
	} else {
	    setmsg_("Time step '#' specified after '#' key is not a DP numbe"
		    "r.", (ftnlen)57);
	    errch_("#", clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 :
		     s_rnge("clvals", i__1, "chwcml_", (ftnlen)3489)) << 10), 
		    (ftnlen)1, (ftnlen)1024);
	    errch_("#", "-s", (ftnlen)1, (ftnlen)2);
	    sigerr_("SPICE(NOTANDPNUMBER)", (ftnlen)20);
	}
    } else {

/*        Step was not provided on the command line. What about the */
/*        number of steps? */

	i__ = isrchc_("-n", &c__25, clkeys, (ftnlen)2, (ftnlen)32);
	if (clflag[(i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge("clflag"
		, i__1, "chwcml_", (ftnlen)3502)]) {

/*           Is the number of step an integer number? */

	    nparsi_(clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : 
		    s_rnge("clvals", i__1, "chwcml_", (ftnlen)3507)) << 10), 
		    nitr, error, &ptr, (ftnlen)1024, (ftnlen)1024);
	    if (ptr == 0) {
		if (*nitr < 1 || *nitr > 1000000) {
		    setmsg_("Number of points must be an integer number betw"
			    "een # and #. The number of points specified afte"
			    "r the '#'  key was #.", (ftnlen)116);
		    errint_("#", &c__1, (ftnlen)1);
		    errint_("#", &c_b114, (ftnlen)1);
		    errch_("#", "-n", (ftnlen)1, (ftnlen)2);
		    errint_("#", nitr, (ftnlen)1);
		    sigerr_("SPICE(BADNUMBEROFPOINTS)", (ftnlen)24);
		}
	    } else {
		setmsg_("Number of points '#' specified after '#'  key is no"
			"t an integer number.", (ftnlen)71);
		errch_("#", clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? 
			i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)3528)
			) << 10), (ftnlen)1, (ftnlen)1024);
		errch_("#", "-n", (ftnlen)1, (ftnlen)2);
		sigerr_("SPICE(NOTANINTEGERNUMBER)", (ftnlen)25);
	    }
	}
    }

/*     If any of the dump reports was requested we need to check whether */
/*     output time format was provided on the command line. Default */
/*     format is ET seconds. Default SCLK ID is zero (means "not set"). */

    s_copy(timfmt, "et", timfmt_len, (ftnlen)2);
    sclkid[0] = 0;
    sclkid[1] = 0;
    if (eqstr_(diftyp, "dumpm", diftyp_len, (ftnlen)5) || eqstr_(diftyp, 
	    "dumpqs", diftyp_len, (ftnlen)6) || eqstr_(diftyp, "dumpqo", 
	    diftyp_len, (ftnlen)6) || eqstr_(diftyp, "dumpea", diftyp_len, (
	    ftnlen)6) || eqstr_(diftyp, "dumpaa", diftyp_len, (ftnlen)6) || 
	    eqstr_(diftyp, "dumpc", diftyp_len, (ftnlen)5) || eqstr_(diftyp, 
	    "dumpg", diftyp_len, (ftnlen)5)) {
	i__ = isrchc_("-f", &c__25, clkeys, (ftnlen)2, (ftnlen)32);
	if (clflag[(i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge("clflag"
		, i__1, "chwcml_", (ftnlen)3556)]) {
	    s_copy(timfmt, clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? 
		    i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)3558)) <<
		     10), timfmt_len, (ftnlen)1024);

/*           Time format should be either one of the recognized values */
/*           or a valid picture for TIMOUT. */

	    if (eqstr_(timfmt, "et", timfmt_len, (ftnlen)2)) {

/*              It's ET seconds. No checks are needed. */

	    } else if (eqstr_(timfmt, "sclk", timfmt_len, (ftnlen)4) || 
		    eqstr_(timfmt, "sclkd", timfmt_len, (ftnlen)5) || eqstr_(
		    timfmt, "ticks", timfmt_len, (ftnlen)5)) {

/*              It's one of the SCLK formats. Check if it applies, i.e. */
/*              if we can figure out the SCLK ID to use to convert to */
/*              these SCLKs. Set it based on the first CK frame in this */
/*              list: first ``to'' frame, first ``cov'' frame, second */
/*              ``to'' frame, second ``cov'' frame. If nether of these */
/*              frames is a CK frame, report an error. In each case, */
/*              we should load all applicable kernels to make sure that */
/*              CKMETA checks in the pool for information applicable to */
/*              the frame of interest. */

		if (tfrcls[0] == 3) {
		    kclear_();
		    if (s_cmp(kernam, " ", kernam_len, (ftnlen)1) != 0) {
/* Writing concatenation */
			i__8[0] = kernls_len, a__3[0] = kernls + (kernls_len 
				<< 1);
			i__8[1] = 1, a__3[1] = " ";
			i__8[2] = kernls_len, a__3[2] = kernls;
			i__8[3] = 1, a__3[3] = " ";
			i__8[4] = kernam_len, a__3[4] = kernam;
			s_cat(hlline, a__3, i__8, &c__5, (ftnlen)5120);
			ldklst_(hlline, (ftnlen)5120);
		    } else {
/* Writing concatenation */
			i__8[0] = kernls_len, a__3[0] = kernls + (kernls_len 
				<< 1);
			i__8[1] = 1, a__3[1] = " ";
			i__8[2] = kernls_len, a__3[2] = kernls;
			i__8[3] = 1, a__3[3] = " ";
			i__8[4] = kernam_len, a__3[4] = kernam + kernam_len;
			s_cat(hlline, a__3, i__8, &c__5, (ftnlen)5120);
			ldklst_(hlline, (ftnlen)5120);
		    }
		    ckmeta_(tfrcid, "SCLK", sclkid, (ftnlen)4);
		} else if (cfrcls[0] == 3) {
		    kclear_();
		    if (s_cmp(kernam, " ", kernam_len, (ftnlen)1) != 0) {
/* Writing concatenation */
			i__8[0] = kernls_len, a__3[0] = kernls + (kernls_len 
				<< 1);
			i__8[1] = 1, a__3[1] = " ";
			i__8[2] = kernls_len, a__3[2] = kernls;
			i__8[3] = 1, a__3[3] = " ";
			i__8[4] = kernam_len, a__3[4] = kernam;
			s_cat(hlline, a__3, i__8, &c__5, (ftnlen)5120);
			ldklst_(hlline, (ftnlen)5120);
		    } else {
/* Writing concatenation */
			i__8[0] = kernls_len, a__3[0] = kernls + (kernls_len 
				<< 1);
			i__8[1] = 1, a__3[1] = " ";
			i__8[2] = kernls_len, a__3[2] = kernls;
			i__8[3] = 1, a__3[3] = " ";
			i__8[4] = kernam_len, a__3[4] = kernam + kernam_len;
			s_cat(hlline, a__3, i__8, &c__5, (ftnlen)5120);
			ldklst_(hlline, (ftnlen)5120);
		    }
		    ckmeta_(cfrcid, "SCLK", sclkid, (ftnlen)4);
		} else if (tfrcls[1] == 3) {
		    kclear_();
/* Writing concatenation */
		    i__8[0] = kernls_len, a__3[0] = kernls + (kernls_len << 1)
			    ;
		    i__8[1] = 1, a__3[1] = " ";
		    i__8[2] = kernls_len, a__3[2] = kernls + kernls_len;
		    i__8[3] = 1, a__3[3] = " ";
		    i__8[4] = kernam_len, a__3[4] = kernam + kernam_len;
		    s_cat(hlline, a__3, i__8, &c__5, (ftnlen)5120);
		    ldklst_(hlline, (ftnlen)5120);
		    ckmeta_(&tfrcid[1], "SCLK", &sclkid[1], (ftnlen)4);
		} else if (cfrcls[1] == 3) {
		    kclear_();
/* Writing concatenation */
		    i__8[0] = kernls_len, a__3[0] = kernls + (kernls_len << 1)
			    ;
		    i__8[1] = 1, a__3[1] = " ";
		    i__8[2] = kernls_len, a__3[2] = kernls + kernls_len;
		    i__8[3] = 1, a__3[3] = " ";
		    i__8[4] = kernam_len, a__3[4] = kernam + kernam_len;
		    s_cat(hlline, a__3, i__8, &c__5, (ftnlen)5120);
		    ldklst_(hlline, (ftnlen)5120);
		    ckmeta_(&cfrcid[1], "SCLK", &sclkid[1], (ftnlen)4);
		} else {
		    setmsg_("The '#' time format specified after the '#' key"
			    " cannot be used when neither of the 'to' frames "
			    "('#' and '#') # is a CK-based (class 3) frame.", (
			    ftnlen)141);
		    errch_("#", clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 
			    ? i__1 : s_rnge("clvals", i__1, "chwcml_", (
			    ftnlen)3634)) << 10), (ftnlen)1, (ftnlen)1024);
		    errch_("#", "-f", (ftnlen)1, (ftnlen)2);
		    errch_("#", tfrnam, (ftnlen)1, tfrnam_len);
		    errch_("#", tfrnam + tfrnam_len, (ftnlen)1, tfrnam_len);
		    if (s_cmp(cfrnam, " ", (ftnlen)32, (ftnlen)1) != 0 && 
			    s_cmp(cfrnam + 32, " ", (ftnlen)32, (ftnlen)1) != 
			    0) {
			s_copy(hline2, "and coverage look up frames (PCK fra"
				"mes with class IDs # and #)", (ftnlen)1024, (
				ftnlen)63);
			repmi_(hline2, "#", cfrcid, hline2, (ftnlen)1024, (
				ftnlen)1, (ftnlen)1024);
			repmi_(hline2, "#", &cfrcid[1], hline2, (ftnlen)1024, 
				(ftnlen)1, (ftnlen)1024);
			errch_("#", hline2, (ftnlen)1, (ftnlen)1024);
		    } else if (s_cmp(cfrnam, " ", (ftnlen)32, (ftnlen)1) != 0)
			     {
			s_copy(hline2, "and coverage look up frame (PCK fram"
				"e with class ID #)", (ftnlen)1024, (ftnlen)54)
				;
			repmi_(hline2, "#", cfrcid, hline2, (ftnlen)1024, (
				ftnlen)1, (ftnlen)1024);
			errch_("#", hline2, (ftnlen)1, (ftnlen)1024);
		    } else if (s_cmp(cfrnam + 32, " ", (ftnlen)32, (ftnlen)1) 
			    != 0) {
			s_copy(hline2, "and coverage look up frame (PCK fram"
				"e with class ID #)", (ftnlen)1024, (ftnlen)54)
				;
			repmi_(hline2, "#", &cfrcid[1], hline2, (ftnlen)1024, 
				(ftnlen)1, (ftnlen)1024);
			errch_("#", hline2, (ftnlen)1, (ftnlen)1024);
		    } else {
			errch_("#", " ", (ftnlen)1, (ftnlen)1);
		    }
		    sigerr_("SPICE(FORMATDOESNTAPPLY)", (ftnlen)24);
		}

/*              At this point one of the SCLKID elements should be */
/*              set. If not, there is problem in the logic upstream. */
/*              Put in a safety check indicating a bug. */

		if (sclkid[0] == 0 && sclkid[1] == 0) {
		    setmsg_("There is a bug in the program. Please, contact "
			    "NAIF.", (ftnlen)52);
		    sigerr_("SPICE(FRMDIFFBUG2)", (ftnlen)18);
		}
	    } else {

/*              This must be a TIMOUT format picture. We will check only */
/*              one thing about, that it is not blank. */

		if (s_cmp(timfmt, " ", timfmt_len, (ftnlen)1) == 0) {
		    setmsg_("The time format specified after the '#' key was"
			    " blank.", (ftnlen)54);
		    errch_("#", "-f", (ftnlen)1, (ftnlen)2);
		    sigerr_("SPICE(BLANKTIMEFORMAT)", (ftnlen)22);
		}

/*              The very elaborate check is below turned out to be */
/*              completely useless because TIMOUT allows pretty much */
/*              any string as a picture without complaints :(. This is */
/*              why it's commented out. */

/*              This must be a TIMOUT format picture. We will use the */
/*              following "sledgehammer" approach to verify that this is */
/*              a legitimate picture: 1) load all kernels, 2) set error */
/*              handing to RETURN and messages to NONE, 3) call TIMOUT */
/*              to get out for zero ET, 4) check FAILED to see if */
/*              conversion went well, 5a) if yes, clear errors and reset */
/*              error handing response and messages back to what they */
/*              were, or 5b) if not, retrieve format description error */
/*              and report it to the user. */

/*               CALL KCLEAR */
/*               HLLINE = KERNLS(3)//' '//KERNLS(1)//' '//KERNAM(1)// */
/*     .                             ' '//KERNLS(2)//' '//KERNAM(2) */
/*               CALL LDKLST( HLLINE ) */

/*               CALL ERRACT ( 'GET', HLINE    ) */
/*               CALL ERRACT ( 'SET', 'RETURN' ) */

/*               CALL ERRPRT ( 'GET', HLINE2   ) */
/*               CALL ERRPRT ( 'SET', 'NONE'   ) */

/*               CALL TIMOUT ( 0.D0, TIMFMT, ERROR ) */

/*               IF ( .NOT. FAILED() ) THEN */

/*                  CALL ERRACT ( 'SET', HLINE  ) */
/*                  CALL ERRPRT ( 'SET', HLINE2 ) */

/*               ELSE */

/*                  CALL GETMSG ( 'LONG', ERROR ) */
/*                  CALL RESET  ( ) */
/*                  CALL ERRACT ( 'SET', HLINE  ) */
/*                  CALL ERRPRT ( 'SET', HLINE2 ) */

/*                  CALL SETMSG ( 'The ''#'' time format specified '   // */
/*     .                          'after the ''#'' is neither one of ' // */
/*     .                          'the preset formats (''#'', ''#'', ' // */
/*     .                          '''#'', ''#'') nor a valid TIMOUT '  // */
/*     .                          'format specification string. The '  // */
/*     .                          'error from parsing this value '     // */
/*     .                          'as a TIMOUT format specification '  // */
/*     .                          'string was: #'                      ) */
/*                  CALL ERRCH  ( '#', CLVALS(I)                       ) */
/*                  CALL ERRCH  ( '#', FMTKEY                          ) */
/*                  CALL ERRCH  ( '#', ETVAL                           ) */
/*                  CALL ERRCH  ( '#', SCSVAL                          ) */
/*                  CALL ERRCH  ( '#', SCDVAL                          ) */
/*                  CALL ERRCH  ( '#', SCTVAL                          ) */
/*                  CALL ERRCH  ( '#', ERROR                           ) */
/*                  CALL SIGERR ( 'SPICE(BADFORMATSTRING)'             ) */


/*               END IF */
	    }
	}
    }

/*     Finally, if angle dump was requested, check whether the rotation */
/*     order and angle units were provided on the command line. Default */
/*     order is 3, 2, 1. Default units are radians. */

    axes[0] = 3;
    axes[1] = 2;
    axes[2] = 1;
    s_copy(aunits, "radians", aunits_len, (ftnlen)7);
    if (eqstr_(diftyp, "dumpea", diftyp_len, (ftnlen)6) || eqstr_(diftyp, 
	    "dumpaa", diftyp_len, (ftnlen)6)) {

/*        Check if rotation order was provided. */

	i__ = isrchc_("-o", &c__25, clkeys, (ftnlen)2, (ftnlen)32);
	if (clflag[(i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge("clflag"
		, i__1, "chwcml_", (ftnlen)3771)]) {
	    s_copy(hline, clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? 
		    i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)3773)) <<
		     10), (ftnlen)1024, (ftnlen)1024);

/*           Check if the order is a list of three space-delimited */
/*           items. If not, report an error. */

	    if (wdcnt_(hline, (ftnlen)1024) == 3) {
		for (i__ = 1; i__ <= 3; ++i__) {

/*                 Check each of the items in the order and set */
/*                 corresponding element of axes array. If we can't */
/*                 recognize the token, report an error. */

		    nthwd_(hline, &i__, hword, &hint, (ftnlen)1024, (ftnlen)
			    32);
		    if (eqstr_(hword, "x", (ftnlen)32, (ftnlen)1)) {
			axes[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : 
				s_rnge("axes", i__1, "chwcml_", (ftnlen)3791)]
				 = 1;
		    } else if (eqstr_(hword, "y", (ftnlen)32, (ftnlen)1)) {
			axes[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : 
				s_rnge("axes", i__1, "chwcml_", (ftnlen)3793)]
				 = 2;
		    } else if (eqstr_(hword, "z", (ftnlen)32, (ftnlen)1)) {
			axes[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : 
				s_rnge("axes", i__1, "chwcml_", (ftnlen)3795)]
				 = 3;
		    } else {
			setmsg_("Token '#' in the rotation order  '#' specif"
				"ied after after the '#' key is not one of th"
				"e recognized axis identifiers (#, #, #).", (
				ftnlen)127);
			errch_("#", hword, (ftnlen)1, (ftnlen)32);
			errch_("#", hline, (ftnlen)1, (ftnlen)1024);
			errch_("#", "-o", (ftnlen)1, (ftnlen)2);
			errch_("#", "x", (ftnlen)1, (ftnlen)1);
			errch_("#", "y", (ftnlen)1, (ftnlen)1);
			errch_("#", "z", (ftnlen)1, (ftnlen)1);
			sigerr_("SPICE(BADROTATIONORDER1)", (ftnlen)24);
		    }
		}

/*              Check that neighbor elements in the list are distinct. */
/*              If not, report an error. */

		if (axes[0] == axes[1] || axes[1] == axes[2]) {
		    setmsg_("Middle axis in the rotation order '#' specified"
			    " after the '#' key matches one of the neighbors.",
			     (ftnlen)95);
		    errch_("#", hline, (ftnlen)1, (ftnlen)1024);
		    errch_("#", "-o", (ftnlen)1, (ftnlen)2);
		    sigerr_("SPICE(BADROTATIONORDER2)", (ftnlen)24);
		}
	    } else {
		setmsg_("The rotation order '#' specified after the '#' key "
			"is not a set of three space-delimited axes identifie"
			"rs (#, #, #).", (ftnlen)116);
		errch_("#", hline, (ftnlen)1, (ftnlen)1024);
		errch_("#", "-o", (ftnlen)1, (ftnlen)2);
		errch_("#", "x", (ftnlen)1, (ftnlen)1);
		errch_("#", "y", (ftnlen)1, (ftnlen)1);
		errch_("#", "z", (ftnlen)1, (ftnlen)1);
		sigerr_("SPICE(BADROTATIONORDER3)", (ftnlen)24);
	    }
	}

/*        Check if angle units were provided. */

	i__ = isrchc_("-x", &c__25, clkeys, (ftnlen)2, (ftnlen)32);
	if (clflag[(i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge("clflag"
		, i__1, "chwcml_", (ftnlen)3848)]) {
	    s_copy(aunits, clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? 
		    i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)3850)) <<
		     10), aunits_len, (ftnlen)1024);

/*           Check against the following set of units supported by */
/*           CONVRT (circa N0062). If input units don't match any of */
/*           these, report an error. */

	    if (! (eqstr_(aunits, "RADIANS", aunits_len, (ftnlen)7) || eqstr_(
		    aunits, "DEGREES", aunits_len, (ftnlen)7) || eqstr_(
		    aunits, "ARCMINUTES", aunits_len, (ftnlen)10) || eqstr_(
		    aunits, "ARCSECONDS", aunits_len, (ftnlen)10) || eqstr_(
		    aunits, "HOURANGLE", aunits_len, (ftnlen)9) || eqstr_(
		    aunits, "MINUTEANGLE", aunits_len, (ftnlen)11) || eqstr_(
		    aunits, "SECONDANGLE", aunits_len, (ftnlen)11))) {
		setmsg_("The angle units '#' specified after the '#' key are"
			" not recognized. Refer to the program's User's Guide"
			" for the list of supported units.", (ftnlen)136);
		errch_("#", aunits, (ftnlen)1, aunits_len);
		errch_("#", "-x", (ftnlen)1, (ftnlen)2);
		sigerr_("SPICE(BADANGLEUNITS)", (ftnlen)20);
	    }
	}
    }

/*     If any of the dump reports was requested we need to check whether */
/*     the number significant digits for output was provided on the */
/*     command line. */

    *sigdig = 14;
    if (eqstr_(diftyp, "dumpm", diftyp_len, (ftnlen)5) || eqstr_(diftyp, 
	    "dumpqs", diftyp_len, (ftnlen)6) || eqstr_(diftyp, "dumpqo", 
	    diftyp_len, (ftnlen)6) || eqstr_(diftyp, "dumpea", diftyp_len, (
	    ftnlen)6) || eqstr_(diftyp, "dumpaa", diftyp_len, (ftnlen)6) || 
	    eqstr_(diftyp, "dumpc", diftyp_len, (ftnlen)5) || eqstr_(diftyp, 
	    "dumpg", diftyp_len, (ftnlen)5)) {
	i__ = isrchc_("-d", &c__25, clkeys, (ftnlen)2, (ftnlen)32);
	if (clflag[(i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : s_rnge("clflag"
		, i__1, "chwcml_", (ftnlen)3897)]) {

/*           Is the number of digits an integer number? */

	    nparsi_(clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? i__1 : 
		    s_rnge("clvals", i__1, "chwcml_", (ftnlen)3902)) << 10), 
		    sigdig, error, &ptr, (ftnlen)1024, (ftnlen)1024);
	    if (ptr == 0) {
		if (*sigdig < 6 || *sigdig > 17) {
		    setmsg_("The number of significant digits specified usin"
			    "g the '#' key must be an integer number between "
			    "# and #. It was #.", (ftnlen)113);
		    errch_("#", "-d", (ftnlen)1, (ftnlen)2);
		    errint_("#", &c__6, (ftnlen)1);
		    errint_("#", &c__17, (ftnlen)1);
		    errint_("#", sigdig, (ftnlen)1);
		    sigerr_("SPICE(BADNOFDIGITS)", (ftnlen)19);
		}
	    } else {
		setmsg_("The number of significant digits '#' specified afte"
			"r '#'  key is not an integer number.", (ftnlen)87);
		errch_("#", clvals + (((i__1 = i__ - 1) < 25 && 0 <= i__1 ? 
			i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)3922)
			) << 10), (ftnlen)1, (ftnlen)1024);
		errch_("#", "-d", (ftnlen)1, (ftnlen)2);
		sigerr_("SPICE(NOTINTEGERNUMBER2)", (ftnlen)24);
	    }
	}
    }

/*     Check out. */

    chkout_("CHWCML", (ftnlen)6);
    return 0;
} /* chwcml_ */

