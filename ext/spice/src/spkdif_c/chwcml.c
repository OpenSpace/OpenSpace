/* chwcml.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c_b107 = 1000000;
static integer c__1000 = 1000;
static integer c__6 = 6;
static integer c__17 = 17;
static integer c__14 = 14;
static integer c__21 = 21;
static integer c__50000 = 50000;
static integer c__0 = 0;
static integer c__3 = 3;
static integer c__1 = 1;
static doublereal c_b698 = 1e-8;

/* $Procedure      CHWCML ( Extract arguments from SPKDIFF command line ) */
/* Subroutine */ int chwcml_(char *line, char *spk, char *bodnam, integer *
	bodid, char *cennam, integer *cenid, char *frame, doublereal *cmpwin, 
	integer *nitr, doublereal *step, char *diftyp, char *timfmt, char *
	kernls, logical *sample, integer *sigdig, ftnlen line_len, ftnlen 
	spk_len, ftnlen bodnam_len, ftnlen cennam_len, ftnlen frame_len, 
	ftnlen diftyp_len, ftnlen timfmt_len, ftnlen kernls_len)
{
    /* Initialized data */

    static char clkeys[32*21] = "-b1                             " "-c1     "
	    "                        " "-r1                             " 
	    "-k1                             " "-b2                         "
	    "    " "-c2                             " "-r2                   "
	    "          " "-k2                             " "-k              "
	    "                " "-b                              " "-e        "
	    "                      " "-n                              " "-s  "
	    "                            " "-f                              " 
	    "-t                              " "-d                          "
	    "    " "-usage                          " "-u                    "
	    "          " "-help                           " "-h              "
	    "                " "-v                              ";

    /* System generated locals */
    address a__1[2], a__2[3];
    integer i__1, i__2, i__3[2], i__4, i__5, i__6, i__7[3];
    doublereal d__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen), s_cat(char *,
	     char **, integer *, integer *, ftnlen), s_stop(char *, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), i_dnnt(doublereal *);

    /* Local variables */
    static char arch[3], time[1024*2], type__[3];
    static integer i__;
    extern /* Subroutine */ int kdata_(integer *, char *, char *, char *, 
	    char *, integer *, logical *, ftnlen, ftnlen, ftnlen, ftnlen), 
	    dafgs_(doublereal *), etcal_(doublereal *, char *, ftnlen);
    extern logical elemi_(integer *, integer *);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static char hline[1024];
    static doublereal descr[5];
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen), 
	    filli_(integer *, integer *, integer *), errch_(char *, char *, 
	    ftnlen, ftnlen), dafus_(doublereal *, integer *, integer *, 
	    doublereal *, integer *), repmc_(char *, char *, char *, char *, 
	    ftnlen, ftnlen, ftnlen, ftnlen);
    extern doublereal dpmin_(void), dpmax_(void);
    static integer nargs;
    static logical found;
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen), errdp_(char *, doublereal *, ftnlen), 
	    copyd_(doublereal *, doublereal *);
    extern integer wdcnt_(char *, ftnlen);
    extern /* Subroutine */ int movei_(integer *, integer *, integer *);
    static char hword[32];
    extern /* Subroutine */ int nthwd_(char *, integer *, char *, integer *, 
	    ftnlen, ftnlen);
    static integer count;
    static char error[1024];
    extern integer rtrim_(char *, ftnlen);
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int bodn2c_(char *, integer *, logical *, ftnlen),
	     bodc2n_(integer *, char *, logical *, ftnlen);
    static char hline2[1024];
    static integer flids1[50006], flids2[50006];
    static doublereal cover1[1000006], cover2[1000006];
    static integer spids1[50006];
    static doublereal dc[2];
    static integer spids2[50006], sp1cnt, sp2cnt, ic[6];
    extern /* Subroutine */ int dafbbs_(integer *), str2et_(char *, 
	    doublereal *, ftnlen), daffpa_(logical *);
    static logical clflag[21];
    static doublereal et[2];
    static integer handle;
    extern /* Subroutine */ int dafcls_(integer *);
    static integer framid;
    extern /* Subroutine */ int kclear_(void), scardd_(integer *, doublereal *
	    ), scardi_(integer *, integer *);
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen), 
	    wncard_(doublereal *), intmax_(void);
    extern logical exists_(char *, ftnlen);
    static char hlline[5120], hlpmsg[80*23], usgmsg[80*29], vermsg[80*3], 
	    clvals[1024*21], covdsc[1024*2], hkrnam[1024], timdsc[1024], 
	    unprsd[1024], clkeyu[32*21];
    static doublereal coverc[8];
    static integer iclstb[6], iclstn[6], iclsts[6];
    extern /* Subroutine */ int tkvrsn_(char *, char *, ftnlen, ftnlen), 
	    parcml_(char *, integer *, char *, logical *, char *, logical *, 
	    char *, ftnlen, ftnlen, ftnlen, ftnlen), tostdo_(char *, ftnlen), 
	    getfat_(char *, char *, char *, ftnlen, ftnlen, ftnlen), ssizei_(
	    integer *, integer *), spkobj_(char *, integer *, ftnlen), 
	    setmsg_(char *, ftnlen), sigerr_(char *, ftnlen), nextwd_(char *, 
	    char *, char *, ftnlen, ftnlen, ftnlen), ldklst_(char *, ftnlen), 
	    ktotal_(char *, integer *, ftnlen), nparsi_(char *, integer *, 
	    char *, integer *, ftnlen, ftnlen), namfrm_(char *, integer *, 
	    ftnlen), errint_(char *, integer *, ftnlen), dafopr_(char *, 
	    integer *, ftnlen), intstr_(integer *, char *, ftnlen), frmnam_(
	    integer *, char *, ftnlen), ssized_(integer *, doublereal *), 
	    spkcov_(char *, integer *, doublereal *, ftnlen), wninsd_(
	    doublereal *, doublereal *, doublereal *), wnintd_(doublereal *, 
	    doublereal *, doublereal *), wnfetd_(doublereal *, integer *, 
	    doublereal *, doublereal *), nparsd_(char *, doublereal *, char *,
	     integer *, ftnlen, ftnlen), rmaind_(doublereal *, doublereal *, 
	    doublereal *, doublereal *), chkout_(char *, ftnlen);
    static doublereal ett[2];
    static integer ptr;
    static doublereal hdp1, hdp2;

/* $ Abstract */

/*     Extract arguments from SPKDIFF command line and return them */
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

/*     SPKDIFF User's Guide. */

/* $ Keywords */

/*     TBD. */

/* $ Declarations */
/* $ Abstract */

/*     Include Section:  SPKDIFF Global Parameters */

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

/* -    Version 2.0.0, 25-MAR-2014 (BVS). */

/*        Updated for extended functionality (sampling, non-continuous */
/*        comparison window, coverage and gap display, number of */
/*        significant digits in the output). */

/* -    Version 1.0.0, 09-DEC-2008 (BVS). */

/* -& */

/*     Program name and version. */


/*     Command line keys. */


/*     Command line key values. */


/*     Max and min number states that the program can handle. */


/*     Default number states. */


/*     Maximum number of IDs in an SPK file */


/*     Line size parameters. */


/*     Version, help, usage, and header display parameters. */


/*     DAF descriptor size and component counts. */


/*     Cell lower boundary. */


/*     Maximum allowed number of coverage windows. */


/*     Smallest allowed step. */


/*     Fraction of step to be used as pad at the end of intervals. */


/*     Default, minimum, and maximum numbers of significant digits */
/*     allowed for numbers in dump reports. */


/*     End of SPKDIFF parameters. */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     LINE       I   SPKDIFF command line */
/*     SPK        O   1st and 2nd SPK file names */
/*     BODNAM     O   1st and 2nd body names */
/*     BODID      O   1st and 2nd body IDs */
/*     CENNAM     O   1st and 2nd center names */
/*     CENID      O   1st and 2nd center IDs */
/*     FRAME      O   1st and 2nd frame names */
/*     CMPWIN     O   Time window for comparison */
/*     NITR       O   Number of points to be used in comparison. */
/*     STEP       O   Time step in seconds. */
/*     DIFTYP     O   Type of summary to be generated by SPKDIFF. */
/*     TIMFMT     O   Output time format string "dump" summaries. */
/*     KERNLS     O   Lists of additional kernels. */
/*     SAMPLE     O   Flag indicating a sampling run. */
/*     SIGDIG     O   Number of significant digits */

/* $ Detailed_Input */

/*     LINE        is the command line provided to SPKDIFF. See SPKDIFF */
/*                 User's Guide for the command line syntax and detailed */
/*                 specification of allowed arguments. */

/* $ Detailed_Output */

/*     SPK         is a two element array containing the names of the */
/*                 first and second SPK files. */

/*     BODNAM      is a two element array containing the names of the */
/*                 first and second bodies, position of which is to be */
/*                 computed. */

/*     BODID       is a two element array containing the IDs of the */
/*                 first and second bodies, position of which is to be */
/*                 computed. */

/*     CENNAM      is a two element array containing the names of the */
/*                 first and second bodies, position with respect to */
/*                 which is to be computed. */

/*     CENID       is a two element array containing the IDs of the first */
/*                 and second bodies, position with respect to which is */
/*                 to be computed. */

/*     FRAME       is a two element array containing the names of the */
/*                 first and second frames, in which positions are to be */
/*                 computed. */

/*     CMPWIN      is a time window for comparison. */

/*     NITR        is the number of points to be used in comparison. */

/*     STEP        is the time step in seconds. Depending on the number */
/*                 of intervals in CMPWIN, STEP may be returned as zero */
/*                 if it was not provided on the command line. */

/*     DIFTYP      is a string indicating the type of summary to be */
/*                 generated by SPKDIFF. */

/*     TIMFMT      is a string containing output time format picture for */
/*                 "dump"-type summaries. */

/*     KERNLS      is an array of three strings containing the lists of */
/*                 additional kernels provided on the command line. */
/*                 KERNLS(1) contains the list applicable to the first */
/*                 SPK to be examined. KERNLS(2) contains the list */
/*                 applicable to the second SPK to be examined. */
/*                 KERNLS(3) contains the list applicable both SPKs */
/*                 to be examined. */

/*     SAMPLE      is a logical flag indicating whether it is a sampling */
/*                 run. */

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

/* -    Version 2.1.0, 04-JUL-2014 (BVS) */

/*        Changed short error messages SPICE(NOTANINTEGERNUMBER) and */
/*        SPICE(NOTANINTEGERNUMBER2) to avoid truncation. */

/* -    Version 2.0.0, 21-OCT-2011 (BVS) */

/*        Updated for majors functionality additions (sampling, window */
/*        with gaps, etc). */

/* -    Version 1.0.0, 18-APR-2006 (BVS) */

/* -& */

/*     SPICELIB functions */


/*     Local variables. */


/*     Save everything to prevent potential memory problems in f2c'ed */
/*     version. */


/*     Initialize command line keys. */


/*     Check in. */

    chkin_("CHWCML", (ftnlen)6);

/*     Generate uppercase version of command lines keys. */

    for (i__ = 1; i__ <= 21; ++i__) {
	ucase_(clkeys + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge(
		"clkeys", i__1, "chwcml_", (ftnlen)310)) << 5), clkeyu + (((
		i__2 = i__ - 1) < 21 && 0 <= i__2 ? i__2 : s_rnge("clkeyu", 
		i__2, "chwcml_", (ftnlen)310)) << 5), (ftnlen)32, (ftnlen)32);
    }

/*     Initialize version display. */

    tkvrsn_("TOOLKIT", hword, (ftnlen)7, (ftnlen)32);
    s_copy(vermsg, " ", (ftnlen)80, (ftnlen)1);
/* Writing concatenation */
    i__3[0] = 60, a__1[0] = "spkdiff -- Version 2.0.0, March 25, 2014 -- Too"
	    "lkit Version ";
    i__3[1] = rtrim_(hword, (ftnlen)32), a__1[1] = hword;
    s_cat(vermsg + 80, a__1, i__3, &c__2, (ftnlen)80);
    s_copy(vermsg + 160, " ", (ftnlen)80, (ftnlen)1);

/*     Initialize help display. */

    s_copy(hlpmsg, "   # provides means for comparing the trajectories of tw"
	    "o bodies or", (ftnlen)80, (ftnlen)67);
    s_copy(hlpmsg + 80, "   sampling the trajectory of a single body known t"
	    "o SPICE and", (ftnlen)80, (ftnlen)62);
    s_copy(hlpmsg + 160, "   supported by data from SPICE kernels.", (ftnlen)
	    80, (ftnlen)40);
    s_copy(hlpmsg + 240, " ", (ftnlen)80, (ftnlen)1);
    s_copy(hlpmsg + 320, "   To compare trajectories the program computes a "
	    "set of geometric", (ftnlen)80, (ftnlen)66);
    s_copy(hlpmsg + 400, "   states of a body as seen from a center in a ref"
	    "erence frame over an", (ftnlen)80, (ftnlen)70);
    s_copy(hlpmsg + 480, "   interval of time with a fixed or variable time "
	    "step using one SPK", (ftnlen)80, (ftnlen)68);
    s_copy(hlpmsg + 560, "   file, computes a set of geometric states for th"
	    "e same or different", (ftnlen)80, (ftnlen)69);
    s_copy(hlpmsg + 640, "   body-center-frame combination at the same times"
	    " using the other SPK", (ftnlen)80, (ftnlen)70);
    s_copy(hlpmsg + 720, "   file, and then subtracts the corresponding stat"
	    "es from each other.", (ftnlen)80, (ftnlen)69);
    s_copy(hlpmsg + 800, "   Depending on the type of output report requeste"
	    "d the program prints", (ftnlen)80, (ftnlen)70);
    s_copy(hlpmsg + 880, "   to the screen only the maximum differences in p"
	    "osition and velocity,", (ftnlen)80, (ftnlen)71);
    s_copy(hlpmsg + 960, "   or a complete table of position and velocity di"
	    "fferences, or a", (ftnlen)80, (ftnlen)65);
    s_copy(hlpmsg + 1040, "   complete table of differences expressed in ``v"
	    "iew frame''", (ftnlen)80, (ftnlen)60);
    s_copy(hlpmsg + 1120, "   coordinates, or results of a simple statistica"
	    "l analysis of the", (ftnlen)80, (ftnlen)66);
    s_copy(hlpmsg + 1200, "   differences expressed in ``view frame'' coordi"
	    "nates.", (ftnlen)80, (ftnlen)55);
    s_copy(hlpmsg + 1280, " ", (ftnlen)80, (ftnlen)1);
    s_copy(hlpmsg + 1360, "   To sample trajectory of a body the program com"
	    "putes a set of", (ftnlen)80, (ftnlen)63);
    s_copy(hlpmsg + 1440, "   geometric states of a body as seen from a cent"
	    "er in a reference", (ftnlen)80, (ftnlen)66);
    s_copy(hlpmsg + 1520, "   frame over an interval of time with fixed or v"
	    "ariable time step,", (ftnlen)80, (ftnlen)67);
    s_copy(hlpmsg + 1600, "   using a given set of kernel files, and prints "
	    "to the screen a table", (ftnlen)80, (ftnlen)70);
    s_copy(hlpmsg + 1680, "   containing these states.", (ftnlen)80, (ftnlen)
	    27);
    s_copy(hlpmsg + 1760, " ", (ftnlen)80, (ftnlen)1);
    repmc_(hlpmsg, "#", "spkdiff", hlpmsg, (ftnlen)80, (ftnlen)1, (ftnlen)7, (
	    ftnlen)80);

/*     Initialize usage display. */

    s_copy(usgmsg, "   # provides means for comparing the trajectories of tw"
	    "o bodies", (ftnlen)80, (ftnlen)64);
    s_copy(usgmsg + 80, "   or sampling the trajectory of a single body usin"
	    "g data from SPICE", (ftnlen)80, (ftnlen)68);
    s_copy(usgmsg + 160, "   kernels (see the User''s Guide for more details"
	    ".). The program usage", (ftnlen)80, (ftnlen)71);
    s_copy(usgmsg + 240, "   is:", (ftnlen)80, (ftnlen)6);
    s_copy(usgmsg + 320, " ", (ftnlen)80, (ftnlen)1);
    s_copy(usgmsg + 400, "      % # [options] <first SPK name> <second SPK n"
	    "ame>", (ftnlen)80, (ftnlen)54);
    s_copy(usgmsg + 480, "      % # [options] <SPK name>", (ftnlen)80, (
	    ftnlen)30);
    s_copy(usgmsg + 560, "      % # [options]", (ftnlen)80, (ftnlen)19);
    s_copy(usgmsg + 640, " ", (ftnlen)80, (ftnlen)1);
    s_copy(usgmsg + 720, "   Options are shown below. Order and case of keys"
	    " are not significant.", (ftnlen)80, (ftnlen)71);
    s_copy(usgmsg + 800, "   Values must be space-separated from keys, i.e. "
	    "'# 10', not '#10'.", (ftnlen)80, (ftnlen)68);
    s_copy(usgmsg + 880, " ", (ftnlen)80, (ftnlen)1);
    s_copy(usgmsg + 960, "      #  <supporting kernel(s) name(s)>", (ftnlen)
	    80, (ftnlen)39);
    s_copy(usgmsg + 1040, "      # <first body name or ID>", (ftnlen)80, (
	    ftnlen)31);
    s_copy(usgmsg + 1120, "      # <first center name or ID>", (ftnlen)80, (
	    ftnlen)33);
    s_copy(usgmsg + 1200, "      # <first reference frame name>", (ftnlen)80, 
	    (ftnlen)36);
    s_copy(usgmsg + 1280, "      # <additional supporting kernel(s) for firs"
	    "t SPK>", (ftnlen)80, (ftnlen)55);
    s_copy(usgmsg + 1360, "      # <second body name or ID>", (ftnlen)80, (
	    ftnlen)32);
    s_copy(usgmsg + 1440, "      # <second center name or ID>", (ftnlen)80, (
	    ftnlen)34);
    s_copy(usgmsg + 1520, "      # <second reference frame name>", (ftnlen)80,
	     (ftnlen)37);
    s_copy(usgmsg + 1600, "      # <additional supporting kernel(s) for seco"
	    "nd SPK>", (ftnlen)80, (ftnlen)56);
    s_copy(usgmsg + 1680, "      #  <interval start time>", (ftnlen)80, (
	    ftnlen)30);
    s_copy(usgmsg + 1760, "      #  <interval stop time>", (ftnlen)80, (
	    ftnlen)29);
    s_copy(usgmsg + 1840, "      #  <time step in seconds>", (ftnlen)80, (
	    ftnlen)31);
    s_copy(usgmsg + 1920, "      #  <number of states: # to # (default: #)>", 
	    (ftnlen)80, (ftnlen)48);
    s_copy(usgmsg + 2000, "      #  <output time format (default: TDB second"
	    "s past J2000)>", (ftnlen)80, (ftnlen)63);
    s_copy(usgmsg + 2080, "      #  <number of significant digits: # to # (d"
	    "efault: #)>", (ftnlen)80, (ftnlen)60);
    s_copy(usgmsg + 2160, "      #  <report type: #|#|#|#|#|# (def.: #|#)>", (
	    ftnlen)80, (ftnlen)47);
    s_copy(usgmsg + 2240, " ", (ftnlen)80, (ftnlen)1);
    repmc_(usgmsg, "#", "spkdiff", usgmsg, (ftnlen)80, (ftnlen)1, (ftnlen)7, (
	    ftnlen)80);
    repmc_(usgmsg + 400, "#", "spkdiff", usgmsg + 400, (ftnlen)80, (ftnlen)1, 
	    (ftnlen)7, (ftnlen)80);
    repmc_(usgmsg + 480, "#", "spkdiff", usgmsg + 480, (ftnlen)80, (ftnlen)1, 
	    (ftnlen)7, (ftnlen)80);
    repmc_(usgmsg + 560, "#", "spkdiff", usgmsg + 560, (ftnlen)80, (ftnlen)1, 
	    (ftnlen)7, (ftnlen)80);
    repmc_(usgmsg + 800, "#", "-n", usgmsg + 800, (ftnlen)80, (ftnlen)1, (
	    ftnlen)2, (ftnlen)80);
    repmc_(usgmsg + 800, "#", "-n", usgmsg + 800, (ftnlen)80, (ftnlen)1, (
	    ftnlen)2, (ftnlen)80);
    repmc_(usgmsg + 960, "#", "-k", usgmsg + 960, (ftnlen)80, (ftnlen)1, (
	    ftnlen)2, (ftnlen)80);
    repmc_(usgmsg + 1040, "#", "-b1", usgmsg + 1040, (ftnlen)80, (ftnlen)1, (
	    ftnlen)3, (ftnlen)80);
    repmc_(usgmsg + 1120, "#", "-c1", usgmsg + 1120, (ftnlen)80, (ftnlen)1, (
	    ftnlen)3, (ftnlen)80);
    repmc_(usgmsg + 1200, "#", "-r1", usgmsg + 1200, (ftnlen)80, (ftnlen)1, (
	    ftnlen)3, (ftnlen)80);
    repmc_(usgmsg + 1280, "#", "-k1", usgmsg + 1280, (ftnlen)80, (ftnlen)1, (
	    ftnlen)3, (ftnlen)80);
    repmc_(usgmsg + 1360, "#", "-b2", usgmsg + 1360, (ftnlen)80, (ftnlen)1, (
	    ftnlen)3, (ftnlen)80);
    repmc_(usgmsg + 1440, "#", "-c2", usgmsg + 1440, (ftnlen)80, (ftnlen)1, (
	    ftnlen)3, (ftnlen)80);
    repmc_(usgmsg + 1520, "#", "-r2", usgmsg + 1520, (ftnlen)80, (ftnlen)1, (
	    ftnlen)3, (ftnlen)80);
    repmc_(usgmsg + 1600, "#", "-k2", usgmsg + 1600, (ftnlen)80, (ftnlen)1, (
	    ftnlen)3, (ftnlen)80);
    repmc_(usgmsg + 1680, "#", "-b", usgmsg + 1680, (ftnlen)80, (ftnlen)1, (
	    ftnlen)2, (ftnlen)80);
    repmc_(usgmsg + 1760, "#", "-e", usgmsg + 1760, (ftnlen)80, (ftnlen)1, (
	    ftnlen)2, (ftnlen)80);
    repmc_(usgmsg + 1840, "#", "-s", usgmsg + 1840, (ftnlen)80, (ftnlen)1, (
	    ftnlen)2, (ftnlen)80);
    repmc_(usgmsg + 1920, "#", "-n", usgmsg + 1920, (ftnlen)80, (ftnlen)1, (
	    ftnlen)2, (ftnlen)80);
    repmi_(usgmsg + 1920, "#", &c__2, usgmsg + 1920, (ftnlen)80, (ftnlen)1, (
	    ftnlen)80);
    repmi_(usgmsg + 1920, "#", &c_b107, usgmsg + 1920, (ftnlen)80, (ftnlen)1, 
	    (ftnlen)80);
    repmi_(usgmsg + 1920, "#", &c__1000, usgmsg + 1920, (ftnlen)80, (ftnlen)1,
	     (ftnlen)80);
    repmc_(usgmsg + 2000, "#", "-f", usgmsg + 2000, (ftnlen)80, (ftnlen)1, (
	    ftnlen)2, (ftnlen)80);
    repmc_(usgmsg + 2080, "#", "-d", usgmsg + 2080, (ftnlen)80, (ftnlen)1, (
	    ftnlen)2, (ftnlen)80);
    repmi_(usgmsg + 2080, "#", &c__6, usgmsg + 2080, (ftnlen)80, (ftnlen)1, (
	    ftnlen)80);
    repmi_(usgmsg + 2080, "#", &c__17, usgmsg + 2080, (ftnlen)80, (ftnlen)1, (
	    ftnlen)80);
    repmi_(usgmsg + 2080, "#", &c__14, usgmsg + 2080, (ftnlen)80, (ftnlen)1, (
	    ftnlen)80);
    repmc_(usgmsg + 2160, "#", "-t", usgmsg + 2160, (ftnlen)80, (ftnlen)1, (
	    ftnlen)2, (ftnlen)80);
    repmc_(usgmsg + 2160, "#", "basic", usgmsg + 2160, (ftnlen)80, (ftnlen)1, 
	    (ftnlen)5, (ftnlen)80);
    repmc_(usgmsg + 2160, "#", "stats", usgmsg + 2160, (ftnlen)80, (ftnlen)1, 
	    (ftnlen)5, (ftnlen)80);
    repmc_(usgmsg + 2160, "#", "dump", usgmsg + 2160, (ftnlen)80, (ftnlen)1, (
	    ftnlen)4, (ftnlen)80);
    repmc_(usgmsg + 2160, "#", "dumpvf", usgmsg + 2160, (ftnlen)80, (ftnlen)1,
	     (ftnlen)6, (ftnlen)80);
    repmc_(usgmsg + 2160, "#", "dumpc", usgmsg + 2160, (ftnlen)80, (ftnlen)1, 
	    (ftnlen)5, (ftnlen)80);
    repmc_(usgmsg + 2160, "#", "dumpg", usgmsg + 2160, (ftnlen)80, (ftnlen)1, 
	    (ftnlen)5, (ftnlen)80);
    repmc_(usgmsg + 2160, "#", "basic", usgmsg + 2160, (ftnlen)80, (ftnlen)1, 
	    (ftnlen)5, (ftnlen)80);
    repmc_(usgmsg + 2160, "#", "dump", usgmsg + 2160, (ftnlen)80, (ftnlen)1, (
	    ftnlen)4, (ftnlen)80);

/*     Get command line and do first attempt at parsing. All we need to */
/*     find out in this try is if one of the help/usage/version key */
/*     variations was present. */

    s_copy(hline, line, (ftnlen)1024, line_len);
    parcml_(hline, &c__21, clkeyu, clflag, clvals, &found, unprsd, (ftnlen)
	    1024, (ftnlen)32, (ftnlen)1024, (ftnlen)1024);

/*     Was command line blank? Is one of the version, usage, or help */
/*     keys present? Display usage, help, or version and stop. */

    nargs = wdcnt_(line, line_len);
    if (nargs == 0 || clflag[(i__1 = isrchc_("-v", &c__21, clkeys, (ftnlen)2, 
	    (ftnlen)32) - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("clflag", i__1,
	     "chwcml_", (ftnlen)468)] || clflag[(i__2 = isrchc_("-help", &
	    c__21, clkeys, (ftnlen)5, (ftnlen)32) - 1) < 21 && 0 <= i__2 ? 
	    i__2 : s_rnge("clflag", i__2, "chwcml_", (ftnlen)468)] || clflag[(
	    i__4 = isrchc_("-h", &c__21, clkeys, (ftnlen)2, (ftnlen)32) - 1) <
	     21 && 0 <= i__4 ? i__4 : s_rnge("clflag", i__4, "chwcml_", (
	    ftnlen)468)] || clflag[(i__5 = isrchc_("-usage", &c__21, clkeys, (
	    ftnlen)6, (ftnlen)32) - 1) < 21 && 0 <= i__5 ? i__5 : s_rnge(
	    "clflag", i__5, "chwcml_", (ftnlen)468)] || clflag[(i__6 = 
	    isrchc_("-u", &c__21, clkeys, (ftnlen)2, (ftnlen)32) - 1) < 21 && 
	    0 <= i__6 ? i__6 : s_rnge("clflag", i__6, "chwcml_", (ftnlen)468)]
	    ) {

/*        Display version in all cases. */

	for (i__ = 1; i__ <= 3; ++i__) {
	    tostdo_(vermsg + ((i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : 
		    s_rnge("vermsg", i__1, "chwcml_", (ftnlen)479)) * 80, (
		    ftnlen)80);
	}

/*        Depending on what other information was requested, display */
/*        usage, help, or nothing and stop. */

	if (nargs == 0 || clflag[(i__1 = isrchc_("-usage", &c__21, clkeys, (
		ftnlen)6, (ftnlen)32) - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge(
		"clflag", i__1, "chwcml_", (ftnlen)486)] || clflag[(i__2 = 
		isrchc_("-u", &c__21, clkeys, (ftnlen)2, (ftnlen)32) - 1) < 
		21 && 0 <= i__2 ? i__2 : s_rnge("clflag", i__2, "chwcml_", (
		ftnlen)486)]) {
	    for (i__ = 1; i__ <= 29; ++i__) {
		tostdo_(usgmsg + ((i__1 = i__ - 1) < 29 && 0 <= i__1 ? i__1 : 
			s_rnge("usgmsg", i__1, "chwcml_", (ftnlen)491)) * 80, 
			(ftnlen)80);
	    }
	} else if (clflag[(i__1 = isrchc_("-help", &c__21, clkeys, (ftnlen)5, 
		(ftnlen)32) - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("clflag", 
		i__1, "chwcml_", (ftnlen)494)] || clflag[(i__2 = isrchc_(
		"-h", &c__21, clkeys, (ftnlen)2, (ftnlen)32) - 1) < 21 && 0 <=
		 i__2 ? i__2 : s_rnge("clflag", i__2, "chwcml_", (ftnlen)494)]
		) {
	    for (i__ = 1; i__ <= 23; ++i__) {
		tostdo_(hlpmsg + ((i__1 = i__ - 1) < 23 && 0 <= i__1 ? i__1 : 
			s_rnge("hlpmsg", i__1, "chwcml_", (ftnlen)499)) * 80, 
			(ftnlen)80);
	    }
	}
	s_stop("", (ftnlen)0);
    }

/*     Pull the last word from the command line and see if it's the name */
/*     of a file. If it is, check if it's an SPK file. */

    nthwd_(line, &nargs, spk + spk_len, &ptr, line_len, spk_len);
    if (exists_(spk + spk_len, spk_len)) {
	getfat_(spk + spk_len, arch, type__, spk_len, (ftnlen)3, (ftnlen)3);
	if (s_cmp(arch, "DAF", (ftnlen)3, (ftnlen)3) == 0 && s_cmp(type__, 
		"SPK", (ftnlen)3, (ftnlen)3) == 0) {

/*           OK, it looks like the last item on the command line was an */
/*           SPK file. Let's chop it off and check the second to last */
/*           item. */

	    s_copy(line + (ptr - 1), " ", line_len - (ptr - 1), (ftnlen)1);
	    nargs = wdcnt_(line, line_len);
	    if (nargs != 0) {
		nthwd_(line, &nargs, spk, &ptr, line_len, spk_len);
		if (exists_(spk, spk_len)) {
		    getfat_(spk, arch, type__, spk_len, (ftnlen)3, (ftnlen)3);
		    if (s_cmp(arch, "DAF", (ftnlen)3, (ftnlen)3) == 0 && 
			    s_cmp(type__, "SPK", (ftnlen)3, (ftnlen)3) == 0) {

/*                    Aren't we lucky?! The second to last item on the */
/*                    command line is also an SPK file. Wipe it off the */
/*                    tail of the line before moving on. */

			s_copy(line + (ptr - 1), " ", line_len - (ptr - 1), (
				ftnlen)1);
			nargs = wdcnt_(line, line_len);
		    } else {

/*                    The second to last item on the command line was a */
/*                    file but not an SPK file. Set the name of the */
/*                    first SPK to blank. */

			s_copy(spk, " ", spk_len, (ftnlen)1);
		    }
		} else {

/*                 The second to last item on the command line is not a */
/*                 file name. Set the name of the first SPK to blank. */

		    s_copy(spk, " ", spk_len, (ftnlen)1);
		}
	    } else {

/*              We have only one argument on the command line -- the */
/*              second SPK. Obviously the name the first SPK should be */
/*              set to blank. */

		s_copy(spk, " ", spk_len, (ftnlen)1);
	    }
	} else {

/*           The last item on the command line was a file but not an SPK */
/*           file. Set both SPK names to blanks. */

	    s_copy(spk, " ", spk_len, (ftnlen)1);
	    s_copy(spk + spk_len, " ", spk_len, (ftnlen)1);
	}
    } else {

/*        The last items on the command line was not a file. Set both */
/*        SPK names to blanks. */

	s_copy(spk, " ", spk_len, (ftnlen)1);
	s_copy(spk + spk_len, " ", spk_len, (ftnlen)1);
    }

/*     If the files were provided and are SPKs, fetch IDs of all boides */
/*     covered by them for later use. */

    ssizei_(&c__50000, flids1);
    ssizei_(&c__50000, flids2);
    scardi_(&c__0, flids1);
    scardi_(&c__0, flids2);
    if (s_cmp(spk, " ", spk_len, (ftnlen)1) != 0) {
	spkobj_(spk, flids1, spk_len);
    }
    if (s_cmp(spk + spk_len, " ", spk_len, (ftnlen)1) != 0) {
	spkobj_(spk + spk_len, flids2, spk_len);
    }

/*     Parse the command line again because one or two last items may */
/*     have been removed from it. */

    s_copy(hline, line, (ftnlen)1024, line_len);
    parcml_(hline, &c__21, clkeyu, clflag, clvals, &found, unprsd, (ftnlen)
	    1024, (ftnlen)32, (ftnlen)1024, (ftnlen)1024);

/*     Was there any un-recognized stuff at the beginning of the command */
/*     line? If yes, complain and exit. */

    if (s_cmp(unprsd, " ", (ftnlen)1024, (ftnlen)1) != 0) {
	setmsg_("'#' specified at the beginning of the command line is not r"
		"ecognized as a valid command line key or option or as the na"
		"me of an existing SPK (for runs when only one SPK name is to"
		" be provided on the command line) or as the names of two exi"
		"sting SPKs (for runs when two SPK names are to be provided o"
		"n the command line.) See the program usage for the proper co"
		"mmand line syntax and the full list of recognized options.", (
		ftnlen)417);
	errch_("#", unprsd, (ftnlen)1, (ftnlen)1024);
	sigerr_("SPICE(INCORRECTUSAGE)", (ftnlen)21);
    }

/*     Go on processing the rest of the command line. All other */
/*     arguments are optional and, if not present, will have to be set */
/*     to some default values. */


/*     First, get additional kernels provided on the command line */
/*     applicable to both files and/or each specific file. We need to */
/*     deal with them first because some of them may define frames or */
/*     name-ID mappings that are needed to process the rest of command */
/*     line arguments. Start with looking for kernels applicable to the */
/*     first trajectory set. */

    i__ = isrchc_("-k1", &c__21, clkeys, (ftnlen)3, (ftnlen)32);
    if (clflag[(i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("clflag", 
	    i__1, "chwcml_", (ftnlen)662)]) {
	if (s_cmp(clvals + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : 
		s_rnge("clvals", i__1, "chwcml_", (ftnlen)664)) << 10), " ", (
		ftnlen)1024, (ftnlen)1) != 0) {
	    s_copy(kernls, clvals + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? 
		    i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)666)) << 
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

/*     Second, look for kernels applicable to the second trajectory set. */

    i__ = isrchc_("-k2", &c__21, clkeys, (ftnlen)3, (ftnlen)32);
    if (clflag[(i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("clflag", 
	    i__1, "chwcml_", (ftnlen)703)]) {
	if (s_cmp(clvals + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : 
		s_rnge("clvals", i__1, "chwcml_", (ftnlen)705)) << 10), " ", (
		ftnlen)1024, (ftnlen)1) != 0) {
	    s_copy(kernls + kernls_len, clvals + (((i__1 = i__ - 1) < 21 && 0 
		    <= i__1 ? i__1 : s_rnge("clvals", i__1, "chwcml_", (
		    ftnlen)707)) << 10), kernls_len, (ftnlen)1024);
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

/*     Last, look for kernels applicable to both trajectory sets. */

    i__ = isrchc_("-k", &c__21, clkeys, (ftnlen)2, (ftnlen)32);
    if (clflag[(i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("clflag", 
	    i__1, "chwcml_", (ftnlen)744)]) {
	if (s_cmp(clvals + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : 
		s_rnge("clvals", i__1, "chwcml_", (ftnlen)746)) << 10), " ", (
		ftnlen)1024, (ftnlen)1) != 0) {
	    s_copy(kernls + (kernls_len << 1), clvals + (((i__1 = i__ - 1) < 
		    21 && 0 <= i__1 ? i__1 : s_rnge("clvals", i__1, "chwcml_",
		     (ftnlen)748)) << 10), kernls_len, (ftnlen)1024);
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

/*     Get total count of SPKs and all all IDs for both complete */
/*     supporting kernel sets (each set is common kernels + specific */
/*     kernels but not the corresponding SPK). These will be needed to */
/*     get default coverage windows if no SPKs were provided but */
/*     body(ies) were explicitly given on the command line. */

    ssizei_(&c__50000, spids1);
    ssizei_(&c__50000, spids2);
    scardi_(&c__0, spids1);
    scardi_(&c__0, spids2);
/* Writing concatenation */
    i__7[0] = kernls_len, a__2[0] = kernls + (kernls_len << 1);
    i__7[1] = 1, a__2[1] = " ";
    i__7[2] = kernls_len, a__2[2] = kernls;
    s_cat(hlline, a__2, i__7, &c__3, (ftnlen)5120);
    kclear_();
    ldklst_(hlline, (ftnlen)5120);
    ktotal_("SPK", &sp1cnt, (ftnlen)3);
    i__1 = sp1cnt;
    for (i__ = 1; i__ <= i__1; ++i__) {
	kdata_(&i__, "SPK", hkrnam, hline, hline2, &handle, &found, (ftnlen)3,
		 (ftnlen)1024, (ftnlen)1024, (ftnlen)1024);
	if (found) {
	    spkobj_(hkrnam, spids1, (ftnlen)1024);
	} else {
	    setmsg_("There is a bug in the program. Please, contact NAIF.", (
		    ftnlen)52);
	    sigerr_("SPICE(SPKDIFFBUG5)", (ftnlen)18);
	}
    }
    kclear_();
/* Writing concatenation */
    i__7[0] = kernls_len, a__2[0] = kernls + (kernls_len << 1);
    i__7[1] = 1, a__2[1] = " ";
    i__7[2] = kernls_len, a__2[2] = kernls + kernls_len;
    s_cat(hlline, a__2, i__7, &c__3, (ftnlen)5120);
    ldklst_(hlline, (ftnlen)5120);
    ktotal_("SPK", &sp2cnt, (ftnlen)3);
    i__1 = sp2cnt;
    for (i__ = 1; i__ <= i__1; ++i__) {
	kdata_(&i__, "SPK", hkrnam, hline, hline2, &handle, &found, (ftnlen)3,
		 (ftnlen)1024, (ftnlen)1024, (ftnlen)1024);
	if (found) {
	    spkobj_(hkrnam, spids2, (ftnlen)1024);
	} else {
	    setmsg_("There is a bug in the program. Please, contact NAIF.", (
		    ftnlen)52);
	    sigerr_("SPICE(SPKDIFFBUG6)", (ftnlen)18);
	}
    }

/*     Load additional kernels supplied for the first SPK and kernels */
/*     applicable for both SPKs. */

    kclear_();
/* Writing concatenation */
    i__7[0] = kernls_len, a__2[0] = kernls + (kernls_len << 1);
    i__7[1] = 1, a__2[1] = " ";
    i__7[2] = kernls_len, a__2[2] = kernls;
    s_cat(hlline, a__2, i__7, &c__3, (ftnlen)5120);
    ldklst_(hlline, (ftnlen)5120);

/*     After loading all kernels provided for computing the first */
/*     trajectory set we can move on to getting command line inputs */
/*     defining it. */


/*     Was the center name or ID for the first SPK provided on the */
/*     command line? */

    s_copy(cennam, " ", cennam_len, (ftnlen)1);
    i__ = isrchc_("-c1", &c__21, clkeys, (ftnlen)3, (ftnlen)32);
    if (clflag[(i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("clflag", 
	    i__1, "chwcml_", (ftnlen)847)]) {
	s_copy(cennam, clvals + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : 
		s_rnge("clvals", i__1, "chwcml_", (ftnlen)849)) << 10), 
		cennam_len, (ftnlen)1024);
	nparsi_(clvals + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge(
		"clvals", i__1, "chwcml_", (ftnlen)850)) << 10), cenid, error,
		 &ptr, (ftnlen)1024, (ftnlen)1024);
	if (ptr != 0) {
	    bodn2c_(clvals + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : 
		    s_rnge("clvals", i__1, "chwcml_", (ftnlen)854)) << 10), 
		    cenid, &found, (ftnlen)1024);
	    if (! found) {
		setmsg_("'#' specified after '#' key is neither an integer n"
			"umber representing a legitimate NAIF ID nor an objec"
			"t name recognized in SPICE.", (ftnlen)130);
		errch_("#", clvals + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? 
			i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)862))
			 << 10), (ftnlen)1, (ftnlen)1024);
		errch_("#", "-c1", (ftnlen)1, (ftnlen)3);
		sigerr_("SPICE(BADCENTER1SPEC)", (ftnlen)21);
	    }
	} else {
	    bodc2n_(cenid, hword, &found, (ftnlen)32);
	    if (found) {
		s_copy(cennam, hword, cennam_len, (ftnlen)32);
	    }
	}
    }

/*     Was the body name or ID for the first SPK provided on the */
/*     command line? */

    s_copy(bodnam, " ", bodnam_len, (ftnlen)1);
    i__ = isrchc_("-b1", &c__21, clkeys, (ftnlen)3, (ftnlen)32);
    if (clflag[(i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("clflag", 
	    i__1, "chwcml_", (ftnlen)886)]) {
	s_copy(bodnam, clvals + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : 
		s_rnge("clvals", i__1, "chwcml_", (ftnlen)888)) << 10), 
		bodnam_len, (ftnlen)1024);
	nparsi_(clvals + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge(
		"clvals", i__1, "chwcml_", (ftnlen)889)) << 10), bodid, error,
		 &ptr, (ftnlen)1024, (ftnlen)1024);
	if (ptr != 0) {
	    bodn2c_(clvals + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : 
		    s_rnge("clvals", i__1, "chwcml_", (ftnlen)893)) << 10), 
		    bodid, &found, (ftnlen)1024);
	    if (! found) {
		setmsg_("'#' specified after '#' key is neither an integer n"
			"umber representing a legitimate NAIF ID nor an objec"
			"t name recognized in SPICE.", (ftnlen)130);
		errch_("#", clvals + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? 
			i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)901))
			 << 10), (ftnlen)1, (ftnlen)1024);
		errch_("#", "-b1", (ftnlen)1, (ftnlen)3);
		sigerr_("SPICE(BADBODY1SPEC)", (ftnlen)19);
	    }
	} else {
	    bodc2n_(bodid, hword, &found, (ftnlen)32);
	    if (found) {
		s_copy(bodnam, hword, bodnam_len, (ftnlen)32);
	    }
	}
    }

/*     Was the first frame name provided on the command line? */

    s_copy(frame, " ", frame_len, (ftnlen)1);
    i__ = isrchc_("-r1", &c__21, clkeys, (ftnlen)3, (ftnlen)32);
    if (clflag[(i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("clflag", 
	    i__1, "chwcml_", (ftnlen)924)]) {
	s_copy(frame, clvals + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : 
		s_rnge("clvals", i__1, "chwcml_", (ftnlen)926)) << 10), 
		frame_len, (ftnlen)1024);
	namfrm_(frame, &framid, frame_len);
	if (framid == 0) {
	    setmsg_("Cannot recognize frame '#' provided on the command line"
		    " after '#' key.", (ftnlen)70);
	    errch_("#", frame, (ftnlen)1, frame_len);
	    errch_("#", "-r1", (ftnlen)1, (ftnlen)3);
	    sigerr_("SPICE(BADFRAME1NAME)", (ftnlen)20);
	}
    }

/*     We are done with command line keys defining the first trajectory */
/*     set. We will unload all kernels, load kernels applicable to the */
/*     second attitude set and move on to processing keys for it. */

    kclear_();

/*     Load additional kernels supplied for the second kernel and */
/*     kernels applicable for both kernels. */

/* Writing concatenation */
    i__7[0] = kernls_len, a__2[0] = kernls + (kernls_len << 1);
    i__7[1] = 1, a__2[1] = " ";
    i__7[2] = kernls_len, a__2[2] = kernls + kernls_len;
    s_cat(hlline, a__2, i__7, &c__3, (ftnlen)5120);
    ldklst_(hlline, (ftnlen)5120);

/*     After loading all kernels provided for computing the second */
/*     trajectory set we can move on to getting command line inputs */
/*     defining it. */


/*     Was the center name or ID for the second SPK provided on the */
/*     command line? */

    s_copy(cennam + cennam_len, " ", cennam_len, (ftnlen)1);
    i__ = isrchc_("-c2", &c__21, clkeys, (ftnlen)3, (ftnlen)32);
    if (clflag[(i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("clflag", 
	    i__1, "chwcml_", (ftnlen)966)]) {
	s_copy(cennam + cennam_len, clvals + (((i__1 = i__ - 1) < 21 && 0 <= 
		i__1 ? i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)968)) 
		<< 10), cennam_len, (ftnlen)1024);
	nparsi_(clvals + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge(
		"clvals", i__1, "chwcml_", (ftnlen)969)) << 10), &cenid[1], 
		error, &ptr, (ftnlen)1024, (ftnlen)1024);
	if (ptr != 0) {
	    bodn2c_(clvals + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : 
		    s_rnge("clvals", i__1, "chwcml_", (ftnlen)973)) << 10), &
		    cenid[1], &found, (ftnlen)1024);
	    if (! found) {
		setmsg_("'#' specified after '#' key is neither an integer n"
			"umber representing  a legitimate NAIF ID nor an obje"
			"ct name recognized in SPICE.", (ftnlen)131);
		errch_("#", clvals + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? 
			i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)981))
			 << 10), (ftnlen)1, (ftnlen)1024);
		errch_("#", "-c2", (ftnlen)1, (ftnlen)3);
		sigerr_("SPICE(BADCENTER2SPEC)", (ftnlen)21);
	    }
	} else {
	    bodc2n_(&cenid[1], hword, &found, (ftnlen)32);
	    if (found) {
		s_copy(cennam + cennam_len, hword, cennam_len, (ftnlen)32);
	    }
	}
    }

/*     Was the body name or ID for the second SPK provided on the */
/*     command line? */

    s_copy(bodnam + bodnam_len, " ", bodnam_len, (ftnlen)1);
    i__ = isrchc_("-b2", &c__21, clkeys, (ftnlen)3, (ftnlen)32);
    if (clflag[(i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("clflag", 
	    i__1, "chwcml_", (ftnlen)1005)]) {
	s_copy(bodnam + bodnam_len, clvals + (((i__1 = i__ - 1) < 21 && 0 <= 
		i__1 ? i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)1007))
		 << 10), bodnam_len, (ftnlen)1024);
	nparsi_(clvals + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge(
		"clvals", i__1, "chwcml_", (ftnlen)1008)) << 10), &bodid[1], 
		error, &ptr, (ftnlen)1024, (ftnlen)1024);
	if (ptr != 0) {
	    bodn2c_(clvals + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : 
		    s_rnge("clvals", i__1, "chwcml_", (ftnlen)1012)) << 10), &
		    bodid[1], &found, (ftnlen)1024);
	    if (! found) {
		setmsg_("'#' specified after '#' key is neither an integer n"
			"umber representing a legitimate NAIF ID nor an objec"
			"t name recognized in SPICE.", (ftnlen)130);
		errch_("#", clvals + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? 
			i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)1020)
			) << 10), (ftnlen)1, (ftnlen)1024);
		errch_("#", "-b2", (ftnlen)1, (ftnlen)3);
		sigerr_("SPICE(BADBODY2SPEC)", (ftnlen)19);
	    }
	} else {
	    bodc2n_(&bodid[1], hword, &found, (ftnlen)32);
	    if (found) {
		s_copy(bodnam + bodnam_len, hword, bodnam_len, (ftnlen)32);
	    }
	}
    }

/*     Was the second frame name provided on the command line? */

    s_copy(frame + frame_len, " ", frame_len, (ftnlen)1);
    i__ = isrchc_("-r2", &c__21, clkeys, (ftnlen)3, (ftnlen)32);
    if (clflag[(i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("clflag", 
	    i__1, "chwcml_", (ftnlen)1043)]) {
	s_copy(frame + frame_len, clvals + (((i__1 = i__ - 1) < 21 && 0 <= 
		i__1 ? i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)1045))
		 << 10), frame_len, (ftnlen)1024);
	namfrm_(frame + frame_len, &framid, frame_len);
	if (framid == 0) {
	    setmsg_("Cannot recognize frame '#' provided on the command line"
		    " after '#' key.", (ftnlen)70);
	    errch_("#", frame + frame_len, (ftnlen)1, frame_len);
	    errch_("#", "-r2", (ftnlen)1, (ftnlen)3);
	    sigerr_("SPICE(BADFRAME2NAME)", (ftnlen)20);
	}
    }

/*     Process begin and end time arguments. As these need to be */
/*     converted to ET, we will need LSK data, which can come in any of */
/*     the three file sets on the command line -- kernels specific for */
/*     first of second file or kernels applicable to both files. Note */
/*     that kernels applicable for the second file and kernels */
/*     applicable for both file are already loaded. We will only kernels */
/*     specific to the first file to make sure that we have loaded all */
/*     kernels that we could. */

    s_copy(hlline, kernls, (ftnlen)5120, kernls_len);
    ldklst_(hlline, (ftnlen)5120);
    s_copy(time, " ", (ftnlen)1024, (ftnlen)1);
    et[0] = dpmin_();
    i__ = isrchc_("-b", &c__21, clkeys, (ftnlen)2, (ftnlen)32);
    if (clflag[(i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("clflag", 
	    i__1, "chwcml_", (ftnlen)1076)]) {
	s_copy(time, clvals + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : 
		s_rnge("clvals", i__1, "chwcml_", (ftnlen)1077)) << 10), (
		ftnlen)1024, (ftnlen)1024);
	str2et_(clvals + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge(
		"clvals", i__1, "chwcml_", (ftnlen)1078)) << 10), et, (ftnlen)
		1024);
    }
    s_copy(time + 1024, " ", (ftnlen)1024, (ftnlen)1);
    et[1] = dpmax_();
    i__ = isrchc_("-e", &c__21, clkeys, (ftnlen)2, (ftnlen)32);
    if (clflag[(i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("clflag", 
	    i__1, "chwcml_", (ftnlen)1086)]) {
	s_copy(time + 1024, clvals + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? 
		i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)1087)) << 10)
		, (ftnlen)1024, (ftnlen)1024);
	str2et_(clvals + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge(
		"clvals", i__1, "chwcml_", (ftnlen)1088)) << 10), &et[1], (
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
	s_copy(timdsc, "Time window specified on the command line was from '"
		"#' to '#'.", (ftnlen)1024, (ftnlen)62);
	repmc_(timdsc, "#", time, timdsc, (ftnlen)1024, (ftnlen)1, (ftnlen)
		1024, (ftnlen)1024);
	repmc_(timdsc, "#", time + 1024, timdsc, (ftnlen)1024, (ftnlen)1, (
		ftnlen)1024, (ftnlen)1024);
    } else if (s_cmp(time, " ", (ftnlen)1024, (ftnlen)1) != 0) {
	s_copy(timdsc, "Time window specified on the command line was all ti"
		"mes after '#'.", (ftnlen)1024, (ftnlen)66);
	repmc_(timdsc, "#", time, timdsc, (ftnlen)1024, (ftnlen)1, (ftnlen)
		1024, (ftnlen)1024);
    } else if (s_cmp(time + 1024, " ", (ftnlen)1024, (ftnlen)1) != 0) {
	s_copy(timdsc, "Time window specified on the command line was all ti"
		"mes before '#'.", (ftnlen)1024, (ftnlen)67);
	repmc_(timdsc, "#", time + 1024, timdsc, (ftnlen)1024, (ftnlen)1, (
		ftnlen)1024, (ftnlen)1024);
    } else {
	s_copy(timdsc, " ", (ftnlen)1024, (ftnlen)1);
    }

/*     Sanity check: body should be distinct from center for all possible */
/*     input combinations (-b1/-c1, -b2/-c2, -b1/-c2 and -b2/-c1). */

    if (s_cmp(bodnam, " ", bodnam_len, (ftnlen)1) != 0 && s_cmp(cennam, " ", 
	    cennam_len, (ftnlen)1) != 0) {
	if (bodid[0] == cenid[0]) {
	    setmsg_("Body and center specified on the command line line must"
		    " be distinct. They were '#'(#) and '#'(#).", (ftnlen)97);
	    errch_("#", bodnam, (ftnlen)1, bodnam_len);
	    errint_("#", bodid, (ftnlen)1);
	    errch_("#", cennam, (ftnlen)1, cennam_len);
	    errint_("#", cenid, (ftnlen)1);
	    sigerr_("SPICE(SAMEBODY1CENTER1)", (ftnlen)23);
	}
    } else if (s_cmp(bodnam + bodnam_len, " ", bodnam_len, (ftnlen)1) != 0 && 
	    s_cmp(cennam + cennam_len, " ", cennam_len, (ftnlen)1) != 0) {
	if (bodid[1] == cenid[1]) {
	    setmsg_("Body and center specified on the command line line must"
		    " be distinct.  They were '#'(#) and '#'(#).", (ftnlen)98);
	    errch_("#", bodnam + bodnam_len, (ftnlen)1, bodnam_len);
	    errint_("#", &bodid[1], (ftnlen)1);
	    errch_("#", cennam + cennam_len, (ftnlen)1, cennam_len);
	    errint_("#", &cenid[1], (ftnlen)1);
	    sigerr_("SPICE(SAMEBODY2CENTER2)", (ftnlen)23);
	}
    } else if (s_cmp(bodnam, " ", bodnam_len, (ftnlen)1) != 0 && s_cmp(cennam 
	    + cennam_len, " ", cennam_len, (ftnlen)1) != 0 && s_cmp(cennam, 
	    " ", cennam_len, (ftnlen)1) == 0) {
	if (bodid[0] == cenid[1]) {
	    setmsg_("Body and center specified on the command line line must"
		    " be distinct.  They were '#'(#) and '#'(#).", (ftnlen)98);
	    errch_("#", bodnam, (ftnlen)1, bodnam_len);
	    errint_("#", bodid, (ftnlen)1);
	    errch_("#", cennam + cennam_len, (ftnlen)1, cennam_len);
	    errint_("#", &cenid[1], (ftnlen)1);
	    sigerr_("SPICE(SAMEBODY1CENTER2)", (ftnlen)23);
	}
    } else if (s_cmp(bodnam + bodnam_len, " ", bodnam_len, (ftnlen)1) != 0 && 
	    s_cmp(cennam, " ", cennam_len, (ftnlen)1) != 0 && s_cmp(cennam + 
	    cennam_len, " ", cennam_len, (ftnlen)1) == 0) {
	if (bodid[1] == cenid[0]) {
	    setmsg_("Body and center specified on the command line line must"
		    " be distinct.  They were '#'(#) and '#'(#).", (ftnlen)98);
	    errch_("#", bodnam + bodnam_len, (ftnlen)1, bodnam_len);
	    errint_("#", &bodid[1], (ftnlen)1);
	    errch_("#", cennam, (ftnlen)1, cennam_len);
	    errint_("#", cenid, (ftnlen)1);
	    sigerr_("SPICE(SAMEBODY2CENTER1)", (ftnlen)23);
	}
    }

/*     At this point we processed all command line keys defining */
/*     attributes of the two trajectory sets as well as those */
/*     restricting time boundaries. It is likely that some of the items */
/*     we need were not provided (``body'', ``center'', ``frame'') or */
/*     could not be provided (the final comparison window). Let go ahead */
/*     and fill in these ``blanks''. */


/*     If only one body name/ID was provided, set the other one to be */
/*     the same. */

    if (s_cmp(bodnam, " ", bodnam_len, (ftnlen)1) == 0 && s_cmp(bodnam + 
	    bodnam_len, " ", bodnam_len, (ftnlen)1) != 0) {
	bodid[0] = bodid[1];
	s_copy(bodnam, bodnam + bodnam_len, bodnam_len, bodnam_len);
    } else if (s_cmp(bodnam + bodnam_len, " ", bodnam_len, (ftnlen)1) == 0 && 
	    s_cmp(bodnam, " ", bodnam_len, (ftnlen)1) != 0) {
	bodid[1] = bodid[0];
	s_copy(bodnam + bodnam_len, bodnam, bodnam_len, bodnam_len);
    }

/*     If only one center name/ID was provided, set the other one to be */
/*     the same. */

    if (s_cmp(cennam, " ", cennam_len, (ftnlen)1) == 0 && s_cmp(cennam + 
	    cennam_len, " ", cennam_len, (ftnlen)1) != 0) {
	cenid[0] = cenid[1];
	s_copy(cennam, cennam + cennam_len, cennam_len, cennam_len);
    } else if (s_cmp(cennam + cennam_len, " ", cennam_len, (ftnlen)1) == 0 && 
	    s_cmp(cennam, " ", cennam_len, (ftnlen)1) != 0) {
	cenid[1] = cenid[0];
	s_copy(cennam + cennam_len, cennam, cennam_len, cennam_len);
    }

/*     If only one frame name was provided, set the other one to be the */
/*     same. */

    if (s_cmp(frame, " ", frame_len, (ftnlen)1) == 0 && s_cmp(frame + 
	    frame_len, " ", frame_len, (ftnlen)1) != 0) {
	s_copy(frame, frame + frame_len, frame_len, frame_len);
    } else if (s_cmp(frame + frame_len, " ", frame_len, (ftnlen)1) == 0 && 
	    s_cmp(frame, " ", frame_len, (ftnlen)1) != 0) {
	s_copy(frame + frame_len, frame, frame_len, frame_len);
    }

/*     Check if at least one body, center, and frame were provided on */
/*     the command line. If not, obtain default values by looking at one */
/*     of the SPK files. */

    if (s_cmp(bodnam, " ", bodnam_len, (ftnlen)1) == 0 && s_cmp(bodnam + 
	    bodnam_len, " ", bodnam_len, (ftnlen)1) == 0 || s_cmp(cennam, 
	    " ", cennam_len, (ftnlen)1) == 0 && s_cmp(cennam + cennam_len, 
	    " ", cennam_len, (ftnlen)1) == 0 || s_cmp(frame, " ", frame_len, (
	    ftnlen)1) == 0 && s_cmp(frame + frame_len, " ", frame_len, (
	    ftnlen)1) == 0) {

/*        We don't have complete body-center-frame triplets on the */
/*        command line and need to get some defaults from the SPKs */
/*        provided at the end of the command line. */

/*        First check if we have at least one SPK provided on the */
/*        command line to fetch the missing defaults from. If not, */
/*        complain and stop. If yes, set the SPK to be examined to the */
/*        first file if two files were provided or to the second file if */
/*        only one SPK was provided. */

	if (s_cmp(spk, " ", spk_len, (ftnlen)1) != 0) {
	    s_copy(hkrnam, spk, (ftnlen)1024, spk_len);
	} else if (s_cmp(spk + spk_len, " ", spk_len, (ftnlen)1) != 0) {
	    s_copy(hkrnam, spk + spk_len, (ftnlen)1024, spk_len);
	} else {
	    setmsg_("Cannot pick default values for bodies, centers, or fram"
		    "es because insufficient information was given using the "
		    "relevant command line keys and the last item (or the las"
		    "t two items) on the command line are not the names of SP"
		    "K files.", (ftnlen)231);
	    sigerr_("SPICE(CANNOTGETDEFAULTS1)", (ftnlen)25);
	}

/*        If first, second, or both body IDs were specified on the */
/*        command line, then center and frame from the segment for this */
/*        body closest to the end of the file will be picked as default */
/*        values. */

/*        If neither first nor second body ID was specified on the */
/*        command line and SPK contains segments for one or more */
/*        spacecraft, the body, center, and frame from the spacecraft */
/*        segment closest to the end of the file will be picked as */
/*        default values. */

/*        If neither first nor second body ID was specified on the */
/*        command line and SPK contains no spacecraft segments, the */
/*        body, center, and frame from the very last segment of the file */
/*        will be picked as default values. */


/*        Zero out descriptor buffers for last segment, last s/c segment */
/*        and last segment for specified body. Note that SPK type */
/*        element (ICXXXX(4)) cannot be 0 for any real SPK segment; this */
/*        property will be relied upon in checks in the loop below to */
/*        determine if any of these descriptors have already been set. */

	filli_(&c__0, &c__6, iclstn);
	filli_(&c__0, &c__6, iclsts);
	filli_(&c__0, &c__6, iclstb);

/*        Open the SPK file that we picked and search it in backward */
/*        order. */

	dafopr_(hkrnam, &handle, (ftnlen)1024);
	dafbbs_(&handle);
	daffpa_(&found);
	while(found) {

/*           Fetch and unpack the segment descriptor. */

	    dafgs_(descr);
	    dafus_(descr, &c__2, &c__6, dc, ic);

/*           Save integer components of the last descriptor. */

	    if (iclstn[3] == 0) {
		movei_(ic, &c__6, iclstn);
	    }

/*           Save integer components of the last descriptor for a */
/*           spacecraft. */

	    if (iclsts[3] == 0 && ic[0] < 0) {
		movei_(ic, &c__6, iclsts);
	    }

/*           Save integer components of the data descriptor for */
/*           the specified body. */

	    if (iclstb[3] == 0 && s_cmp(bodnam, " ", bodnam_len, (ftnlen)1) !=
		     0 && bodid[0] == ic[0]) {
		movei_(ic, &c__6, iclstb);
	    }

/*           Find next segment. */

	    daffpa_(&found);
	}

/*        Release the file. */

	dafcls_(&handle);

/*        Set default values based on priorities described above and the */
/*        descriptor data collected in the loop. */

	if (s_cmp(bodnam, " ", bodnam_len, (ftnlen)1) != 0) {

/*           Check if any segments for specified body were found. If */
/*           yes, set defaults. If no, complain and stop. */

	    if (iclstb[3] != 0) {
		movei_(iclstb, &c__6, ic);
	    } else {
		setmsg_("SPK file '#' does not contain any data for body '#'"
			"(#) specified on the command line,", (ftnlen)85);
		errch_("#", hkrnam, (ftnlen)1, (ftnlen)1024);
		errch_("#", bodnam, (ftnlen)1, bodnam_len);
		errint_("#", bodid, (ftnlen)1);
		sigerr_("SPICE(1NODATAFORBODY)", (ftnlen)21);
	    }
	} else if (iclsts[3] != 0) {

/*           Set defaults to the values from the last segment for a */
/*           spacecraft. */

	    movei_(iclsts, &c__6, ic);
	} else {

/*           Set defaults to the values from the last segment */

	    movei_(iclstn, &c__6, ic);
	}

/*        Do a sanity check. At this point descriptor containing defaults */
/*        (IC) should have been set to something meaningful therefore */
/*        IC(4) should be non-zero. */

	if (ic[3] == 0) {
	    setmsg_("Cannot retrieve default values from SPK file '#'. It ei"
		    "ther is damaged or contains no data segments.", (ftnlen)
		    100);
	    errch_("#", hkrnam, (ftnlen)1, (ftnlen)1024);
	    sigerr_("SPICE(CANNOTGETDEFAULTS2)", (ftnlen)25);
	}

/*        Set default body. */

	if (s_cmp(bodnam, " ", bodnam_len, (ftnlen)1) == 0) {
	    bodid[0] = ic[0];
	    bodid[1] = ic[0];
	    bodc2n_(bodid, bodnam, &found, bodnam_len);
	    if (! found) {
		intstr_(ic, bodnam, bodnam_len);
	    }
	    s_copy(bodnam + bodnam_len, bodnam, bodnam_len, bodnam_len);
	}

/*        Set default center. */

	if (s_cmp(cennam, " ", cennam_len, (ftnlen)1) == 0) {
	    cenid[0] = ic[1];
	    cenid[1] = ic[1];
	    bodc2n_(cenid, cennam, &found, cennam_len);
	    if (! found) {
		intstr_(&ic[1], cennam, cennam_len);
	    }
	    s_copy(cennam + cennam_len, cennam, cennam_len, cennam_len);
	}

/*        Set default frame. */

	if (s_cmp(frame, " ", frame_len, (ftnlen)1) == 0) {
	    frmnam_(&ic[2], frame, frame_len);
	    if (s_cmp(frame, " ", frame_len, (ftnlen)1) == 0) {
		setmsg_("Cannot pick default frame for this run becasue no f"
			"rame names were specified on the command line and th"
			"e frame ID # picked from the SPK '#' could not be ma"
			"pped to a frame name. Please, specify the frame rela"
			"tive to which states should be computed using '#' or"
			" '#' command line keys or provide a text kernel defi"
			"ning the frame with the above ID.", (ftnlen)344);
		errint_("#", &ic[2], (ftnlen)1);
		errch_("#", hkrnam, (ftnlen)1, (ftnlen)1024);
		errch_("#", "-r1", (ftnlen)1, (ftnlen)3);
		errch_("#", "-r2", (ftnlen)1, (ftnlen)3);
		sigerr_("SPICE(CANNOTPICKFRAME)", (ftnlen)22);
	    }
	    s_copy(frame + frame_len, frame, frame_len, frame_len);
	}
    }

/*     Now that we have body and center for each pair we can repeat the */
/*     same sanity check: body should be distinct from center. */

    if (bodid[0] == cenid[0]) {
	setmsg_("Body and center picked from SPK and/or command line line mu"
		"st be distinct. They were '#'(#) and '#'(#).", (ftnlen)103);
	errch_("#", bodnam, (ftnlen)1, bodnam_len);
	errint_("#", bodid, (ftnlen)1);
	errch_("#", cennam, (ftnlen)1, cennam_len);
	errint_("#", cenid, (ftnlen)1);
	sigerr_("SPICE(SAMEBODYANDCENTER3)", (ftnlen)25);
    } else if (bodid[1] == cenid[1]) {
	setmsg_("Body and center picked from SPK and/or command line line mu"
		"st be distinct. They were '#'(#) and '#'(#).", (ftnlen)103);
	errch_("#", bodnam + bodnam_len, (ftnlen)1, bodnam_len);
	errint_("#", &bodid[1], (ftnlen)1);
	errch_("#", cennam + cennam_len, (ftnlen)1, cennam_len);
	errint_("#", &cenid[1], (ftnlen)1);
	sigerr_("SPICE(SAMEBODYANDCENTER4)", (ftnlen)25);
    }

/*     Now we need to determine the comparison window. */

/*     If we got no SPK file(s) on the command line, the user must have */
/*     provided start and stop times on the command line. If not, we */
/*     stop. */

/*     If one SPK file was provided, we will try to get coverage window */
/*     from it and further restrict this window using start and stop */
/*     times if they were provided. */

/*     If both SPK files were provided, we will try to get coverage */
/*     window from each of them, intersect these windows and them and */
/*     further restrict the resulting window using start and stop times */
/*     if they were provided. */


/*     First check all cases when we should have had start and stop */
/*     times given on the command line. First check is for no files and */
/*     missing start or stop. */

/*     This check was commented out because the program was updated */
/*     to attempt to get coverage from supporting kernels if no SPKs */
/*     were provided. */

/*      IF (   SPK(1) .EQ. ' '       .AND. */
/*     .       SPK(2) .EQ. ' '       .AND. */
/*     .     ( TIME(1)   .EQ. ' ' .OR. */
/*     .       TIME(2)   .EQ. ' '      )      ) THEN */

/*         CALL SETMSG ( 'Cannot determine time range for comparison. '// */
/*     .                 'Both start time and stop time must be '      // */
/*     .                 'provided using ''#'' and ''#'' keys when '   // */
/*     .                 'no SPK files are given as the second to '    // */
/*     .                 'and the last items on the command line.'     ) */
/*         CALL ERRCH  ( '#', BEGKEY                                   ) */
/*         CALL ERRCH  ( '#', ENDKEY                                   ) */
/*         CALL SIGERR ( 'SPICE(NOTIMEBOUNDS1)'                        ) */

/*      END IF */

/*     We are done checking the obvious error cases. Since we know our */
/*     bodies, let's try to get coverage windows for each of them and do */
/*     more checks after that. For the first body we may get coverage */
/*     either from the first or the only file. For the second body we */
/*     will try to get coverage from the second or only file. But first, */
/*     let's initialize our coverage windows. */

    ssized_(&c_b107, cover1);
    scardd_(&c__0, cover1);
    ssized_(&c_b107, cover2);
    scardd_(&c__0, cover2);

/*     Try to get coverage for the first body from first or second SPK */
/*     if */

/*              the first SPK is present */
/*           AND */
/*              data for the first body is present in the first SPK */
/*        OR if */
/*              the second SPK is present */
/*           AND */
/*              data for the first body is present in the second SPK. */

/*     OR try to get coverage from the common kernel set + the first */
/*     supporting kernel set if */

/*              no SPKs were provided, */
/*           AND */
/*              data for the first body is present in the first complete */
/*              set of supporting kernels */

    s_copy(covdsc, " ", (ftnlen)1024, (ftnlen)1);
    if (s_cmp(spk, " ", spk_len, (ftnlen)1) != 0 && elemi_(bodid, flids1) || 
	    s_cmp(spk + spk_len, " ", spk_len, (ftnlen)1) != 0 && elemi_(
	    bodid, flids2)) {

/*        We have to look up coverage from one of the SPKs specifically */
/*        given on the command line. Set the file we will work with. */

	if (s_cmp(spk, " ", spk_len, (ftnlen)1) != 0 && elemi_(bodid, flids1))
		 {
	    s_copy(hkrnam, spk, (ftnlen)1024, spk_len);
	} else {
	    s_copy(hkrnam, spk + spk_len, (ftnlen)1024, spk_len);
	}

/*        Get ET coverage window for the first body from the picked */
/*        SPK. */

	spkcov_(hkrnam, bodid, cover1, (ftnlen)1024);

/*        If we got an empty window back, it's a bug because the ID was */
/*        in the list of IDs for which SPK provides coverage. If not, */
/*        make an information string that we may have to use in the */
/*        error message(s) later. */

	if (wncard_(cover1) == 0) {
	    setmsg_("There is a bug in the program. Please, contact NAIF.", (
		    ftnlen)52);
	    sigerr_("SPICE(SPKDIFFBUG7)", (ftnlen)18);
	} else {
	    s_copy(covdsc, "Coverage for body with ID '#' was determined fro"
		    "m SPK file '#'.", (ftnlen)1024, (ftnlen)63);
	    repmi_(covdsc, "#", bodid, covdsc, (ftnlen)1024, (ftnlen)1, (
		    ftnlen)1024);
	    repmc_(covdsc, "#", hkrnam, covdsc, (ftnlen)1024, (ftnlen)1, (
		    ftnlen)1024, (ftnlen)1024);
	}

/*        Done determining coverage from one of the SPKs specifically */
/*        given on the command line. */

    } else if (s_cmp(spk, " ", spk_len, (ftnlen)1) == 0 && s_cmp(spk + 
	    spk_len, " ", spk_len, (ftnlen)1) == 0 && elemi_(bodid, spids1)) {

/*        We have to look up coverage from the first set of supporting */
/*        kernels given on the command line. */

	kclear_();
/* Writing concatenation */
	i__7[0] = kernls_len, a__2[0] = kernls + (kernls_len << 1);
	i__7[1] = 1, a__2[1] = " ";
	i__7[2] = kernls_len, a__2[2] = kernls;
	s_cat(hlline, a__2, i__7, &c__3, (ftnlen)5120);
	ldklst_(hlline, (ftnlen)5120);
	i__1 = sp1cnt;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    kdata_(&i__, "SPK", hkrnam, hline, hline2, &handle, &found, (
		    ftnlen)3, (ftnlen)1024, (ftnlen)1024, (ftnlen)1024);
	    if (found) {
		spkcov_(hkrnam, bodid, cover1, (ftnlen)1024);
	    } else {
		setmsg_("There is a bug in the program. Please, contact NAIF."
			, (ftnlen)52);
		sigerr_("SPICE(SPKDIFFBUG8)", (ftnlen)18);
	    }
	}

/*        If we got an empty window back, it's a bug because the ID was */
/*        in the list of IDs for which supporting kernels provide */
/*        coverage. If not, make an information string that we may have */
/*        to use in the error message(s) later. */

	if (wncard_(cover1) == 0) {
	    setmsg_("There is a bug in the program. Please, contact NAIF.", (
		    ftnlen)52);
	    sigerr_("SPICE(SPKDIFFBUG9)", (ftnlen)18);
	} else {
	    s_copy(covdsc, "Coverage for body with ID '#' was determined fro"
		    "m supporting kernels '#' and '#'.", (ftnlen)1024, (ftnlen)
		    81);
	    repmi_(covdsc, "#", bodid, covdsc, (ftnlen)1024, (ftnlen)1, (
		    ftnlen)1024);
	    repmc_(covdsc, "#", kernls + (kernls_len << 1), covdsc, (ftnlen)
		    1024, (ftnlen)1, kernls_len, (ftnlen)1024);
	    repmc_(covdsc, "#", kernls, covdsc, (ftnlen)1024, (ftnlen)1, 
		    kernls_len, (ftnlen)1024);
	}

/*        Done determining coverage from supporting kernels. */

    }

/*     Get coverage for the second body if */

/*           the second SPK is present */
/*        AND */
/*           data for the second body is present in the second SPK. */

/*     OR try to get coverage from the common kernel set + the first */
/*     supporting kernel set if */

/*           no SPKs were provided */
/*        AND */
/*           data for the second body is present in the supporting */
/*           kernels */

    s_copy(covdsc + 1024, " ", (ftnlen)1024, (ftnlen)1);
    if (s_cmp(spk + spk_len, " ", spk_len, (ftnlen)1) != 0 && elemi_(&bodid[1]
	    , flids2)) {

/*        Get ET coverage window for the second body from the second */
/*        SPK. */

	spkcov_(spk + spk_len, &bodid[1], cover2, spk_len);

/*        If we got an empty window back, report an error. If not, */
/*        make an information string that we may have to use in the */
/*        error message(s) later. */

	if (wncard_(cover2) == 0) {
	    setmsg_("There is a bug in the program. Please, contact NAIF.", (
		    ftnlen)52);
	    sigerr_("SPICE(SPKDIFFBUG10)", (ftnlen)19);
	} else {
	    s_copy(covdsc + 1024, "Coverage for body with ID '#' was determi"
		    "ned from SPK file '#'.", (ftnlen)1024, (ftnlen)63);
	    repmi_(covdsc + 1024, "#", &bodid[1], covdsc + 1024, (ftnlen)1024,
		     (ftnlen)1, (ftnlen)1024);
	    repmc_(covdsc + 1024, "#", spk + spk_len, covdsc + 1024, (ftnlen)
		    1024, (ftnlen)1, spk_len, (ftnlen)1024);
	}

/*        Done determining coverage from one of the SPKs specifically */
/*        given on the command line. */

    } else if (s_cmp(spk, " ", spk_len, (ftnlen)1) == 0 && s_cmp(spk + 
	    spk_len, " ", spk_len, (ftnlen)1) == 0 && elemi_(&bodid[1], 
	    spids2)) {

/*        We have to look up coverage from the second set of supporting */
/*        kernels given on the command line. */

	kclear_();
/* Writing concatenation */
	i__7[0] = kernls_len, a__2[0] = kernls + (kernls_len << 1);
	i__7[1] = 1, a__2[1] = " ";
	i__7[2] = kernls_len, a__2[2] = kernls + kernls_len;
	s_cat(hlline, a__2, i__7, &c__3, (ftnlen)5120);
	ldklst_(hlline, (ftnlen)5120);
	i__1 = sp2cnt;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    kdata_(&i__, "SPK", hkrnam, hline, hline2, &handle, &found, (
		    ftnlen)3, (ftnlen)1024, (ftnlen)1024, (ftnlen)1024);
	    if (found) {
		spkcov_(hkrnam, &bodid[1], cover2, (ftnlen)1024);
	    } else {
		setmsg_("There is a bug in the program. Please, contact NAIF."
			, (ftnlen)52);
		sigerr_("SPICE(SPKDIFFBUG11)", (ftnlen)19);
	    }
	}

/*        If we got an empty window back, it's a bug because the ID was */
/*        in the list of IDs for which supporting kernels provide */
/*        coverage. If not, make an information string that we may have */
/*        to use in the error message(s) later. */

	if (wncard_(cover2) == 0) {
	    setmsg_("There is a bug in the program. Please, contact NAIF.", (
		    ftnlen)52);
	    sigerr_("SPICE(SPKDIFFBUG12)", (ftnlen)19);
	} else {
	    s_copy(covdsc + 1024, "Coverage for body with ID '#' was determi"
		    "ned from supporting kernels '#' and '#'.", (ftnlen)1024, (
		    ftnlen)81);
	    repmi_(covdsc + 1024, "#", bodid, covdsc + 1024, (ftnlen)1024, (
		    ftnlen)1, (ftnlen)1024);
	    repmc_(covdsc + 1024, "#", kernls + (kernls_len << 1), covdsc + 
		    1024, (ftnlen)1024, (ftnlen)1, kernls_len, (ftnlen)1024);
	    repmc_(covdsc + 1024, "#", kernls + kernls_len, covdsc + 1024, (
		    ftnlen)1024, (ftnlen)1, kernls_len, (ftnlen)1024);
	}

/*        Done determining coverage from supporting kernels. */

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
		"o determine time window of interest. The window can be deter"
		"mined by examining the SPK files specified as the second to "
		"last and the last items on the command line or by examining "
		"supporting kernels if no SPKs were provided and/or given exp"
		"licitly using the '#' and '#' keys specifying the start and "
		"stop times. When no SPKs are provided at the end of the comm"
		"and line, supporting kernels must provide some coverage for "
		"the specified bodies. When one or two SPKs are provided but "
		"do not contain coverage for the specified bodies, both the '"
		"#' and'#' keys must be used to define the time window of int"
		"erest. ", (ftnlen)666);
	errch_("#", "-b", (ftnlen)1, (ftnlen)2);
	errch_("#", "-e", (ftnlen)1, (ftnlen)2);
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
		    "rage windows obtained by examining # provided on the com"
		    "mand line do not overlap. # #", (ftnlen)140);
	    if (s_cmp(spk, " ", spk_len, (ftnlen)1) != 0) {
		errch_("#", "the SPKs", (ftnlen)1, (ftnlen)8);
	    } else {
		errch_("#", "the SPK", (ftnlen)1, (ftnlen)7);
	    }
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
		    "ng # provided on the command line does not overlap the t"
		    "ime range specified in the command line using '#' and/or"
		    " '#' keys. # # # ", (ftnlen)240);
	    if (s_cmp(spk, " ", spk_len, (ftnlen)1) != 0) {
		errch_("#", "the SPKs", (ftnlen)1, (ftnlen)8);
	    } else {
		errch_("#", "the SPK", (ftnlen)1, (ftnlen)7);
	    }
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
		    "coverage window obtained by examining the SPK provided o"
		    "n the command line does not overlap the time range speci"
		    "fied in the command line using '#' and/or '#' keys. # #", 
		    (ftnlen)222);
	    errch_("#", "-b", (ftnlen)1, (ftnlen)2);
	    errch_("#", "-e", (ftnlen)1, (ftnlen)2);
	    errch_("#", covdsc, (ftnlen)1, (ftnlen)1024);
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
		    "coverage window obtained by examining the SPK provided o"
		    "n the command line does not overlap the time range speci"
		    "fied in the command line using '#' and/or '#' keys. # #", 
		    (ftnlen)222);
	    errch_("#", "-b", (ftnlen)1, (ftnlen)2);
	    errch_("#", "-e", (ftnlen)1, (ftnlen)2);
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
	    sigerr_("SPICE(SPKDIFFBUG1)", (ftnlen)18);
	}
    }

/*     Reset ET(1) and ET(2) to be ends of the actual comparison window. */

    wnfetd_(cmpwin, &c__1, et, &hdp1);
    i__1 = wncard_(cmpwin);
    wnfetd_(cmpwin, &i__1, &hdp1, &et[1]);
    etcal_(et, time, (ftnlen)1024);
    etcal_(&et[1], time + 1024, (ftnlen)1024);
/* Writing concatenation */
    i__3[0] = rtrim_(time, (ftnlen)1024), a__1[0] = time;
    i__3[1] = 4, a__1[1] = " TDB";
    s_cat(time, a__1, i__3, &c__2, (ftnlen)1024);
/* Writing concatenation */
    i__3[0] = rtrim_(time + 1024, (ftnlen)1024), a__1[0] = time + 1024;
    i__3[1] = 4, a__1[1] = " TDB";
    s_cat(time + 1024, a__1, i__3, &c__2, (ftnlen)1024);

/*     Set flag indicating whether we are running in comparison mode or */
/*     in sampling mode. When two SPKs were provided, we will do */
/*     sampling if centers are the same, bodies are the same, frames are */
/*     the same, SPK-specific kernels are the same, and files are the */
/*     same. When one or no SPKs were provided, we will do sampling if */
/*     centers are the same, bodies are the same, frames are the same, */
/*     and SPK-specific kernels are the same. */

    if (s_cmp(spk, " ", spk_len, (ftnlen)1) != 0 && s_cmp(spk + spk_len, 
	    " ", spk_len, (ftnlen)1) != 0) {

/*        For backward compatibility with verison 1.0.0 we will always */
/*        run in comparison mode when two SPKs were provided. */

/*         SAMPLE = EQSTR( CENNAM(1), CENNAM(2) ) .AND. */
/*     .            EQSTR( BODNAM(1), BODNAM(2) ) .AND. */
/*     .            EQSTR( FRAME(1),  FRAME(2)  ) .AND. */
/*     .            KERNLS(1) .EQ. KERNLS(2)      .AND. */
/*     .            SPK(1) .EQ. SPK(2) */

	*sample = FALSE_;
    } else {
	*sample = eqstr_(cennam, cennam + cennam_len, cennam_len, cennam_len) 
		&& eqstr_(bodnam, bodnam + bodnam_len, bodnam_len, bodnam_len)
		 && eqstr_(frame, frame + frame_len, frame_len, frame_len) && 
		s_cmp(kernls, kernls + kernls_len, kernls_len, kernls_len) == 
		0;
    }

/*     Check command line for the rest of arguments. Unlike the */
/*     attributes of the first and second trajectory sets and comparison */
/*     window, the rest of arguments are truly optional. */

/*     Start with the type of output. Default type is basic. */

    i__ = isrchc_("-t", &c__21, clkeys, (ftnlen)2, (ftnlen)32);
    if (clflag[(i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("clflag", 
	    i__1, "chwcml_", (ftnlen)2036)]) {
	s_copy(diftyp, clvals + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : 
		s_rnge("clvals", i__1, "chwcml_", (ftnlen)2038)) << 10), 
		diftyp_len, (ftnlen)1024);
	if (! (eqstr_(diftyp, "stats", diftyp_len, (ftnlen)5) || eqstr_(
		diftyp, "dump", diftyp_len, (ftnlen)4) || eqstr_(diftyp, 
		"dumpvf", diftyp_len, (ftnlen)6) || eqstr_(diftyp, "dumpc", 
		diftyp_len, (ftnlen)5) || eqstr_(diftyp, "dumpg", diftyp_len, 
		(ftnlen)5) || eqstr_(diftyp, "basic", diftyp_len, (ftnlen)5)))
		 {
	    setmsg_("Output type '#' specified after '#' key is not recogniz"
		    "ed. Recognized output types are '#', '#', '#', '#', '#',"
		    " and '#'.", (ftnlen)120);
	    errch_("#", clvals + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 :
		     s_rnge("clvals", i__1, "chwcml_", (ftnlen)2051)) << 10), 
		    (ftnlen)1, (ftnlen)1024);
	    errch_("#", "-t", (ftnlen)1, (ftnlen)2);
	    errch_("#", "basic", (ftnlen)1, (ftnlen)5);
	    errch_("#", "stats", (ftnlen)1, (ftnlen)5);
	    errch_("#", "dump", (ftnlen)1, (ftnlen)4);
	    errch_("#", "dumpvf", (ftnlen)1, (ftnlen)6);
	    errch_("#", "dumpc", (ftnlen)1, (ftnlen)5);
	    errch_("#", "dumpg", (ftnlen)1, (ftnlen)5);
	    sigerr_("SPICE(BADOUTPUTTYPE)", (ftnlen)20);
	}
	if (*sample && (eqstr_(diftyp, "stats", diftyp_len, (ftnlen)5) || 
		eqstr_(diftyp, "dumpvf", diftyp_len, (ftnlen)6) || eqstr_(
		diftyp, "basic", diftyp_len, (ftnlen)5))) {
	    setmsg_("Output type '#' specified after '#' key is not applicab"
		    "le for sampling runs. Only output types '#', '#', and '#"
		    "' are applicable.", (ftnlen)128);
	    errch_("#", clvals + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 :
		     s_rnge("clvals", i__1, "chwcml_", (ftnlen)2073)) << 10), 
		    (ftnlen)1, (ftnlen)1024);
	    errch_("#", "-t", (ftnlen)1, (ftnlen)2);
	    errch_("#", "dump", (ftnlen)1, (ftnlen)4);
	    errch_("#", "dumpc", (ftnlen)1, (ftnlen)5);
	    errch_("#", "dumpg", (ftnlen)1, (ftnlen)5);
	    sigerr_("SPICE(BADOUTPUTTYPE)", (ftnlen)20);
	}
    } else {

/*        If output type was not specified, set it based on whether this */
/*        is a sampling or comparison run. */

	if (*sample) {
	    s_copy(diftyp, "dump", diftyp_len, (ftnlen)4);
	} else {
	    s_copy(diftyp, "basic", diftyp_len, (ftnlen)5);
	}
    }

/*     Next, get time step or number of steps. If both are specified, */
/*     time step has higher priority and, for this reason, should be */
/*     processed first. Default step is zero (meaning */
/*     "not set") and default number of points is a parameter set */
/*     in the include file. */

    *step = 0.;
    *nitr = 1000;
    i__ = isrchc_("-s", &c__21, clkeys, (ftnlen)2, (ftnlen)32);
    if (clflag[(i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("clflag", 
	    i__1, "chwcml_", (ftnlen)2108)]) {

/*        Is the step a DP number? */

	nparsd_(clvals + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge(
		"clvals", i__1, "chwcml_", (ftnlen)2113)) << 10), step, error,
		 &ptr, (ftnlen)1024, (ftnlen)1024);
	if (ptr == 0) {

/*           Check that step is a positive number and is greater that */
/*           the smallest step we can allow. */

	    if (*step < 1e-8) {
		setmsg_("Time step '#' specified after '#' key is smaller th"
			"an # seconds.", (ftnlen)64);
		errch_("#", clvals + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? 
			i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)2125)
			) << 10), (ftnlen)1, (ftnlen)1024);
		errch_("#", "-s", (ftnlen)1, (ftnlen)2);
		errdp_("#", &c_b698, (ftnlen)1);
		sigerr_("SPICE(STEPTOOSMALL1)", (ftnlen)20);
	    }

/*           Fork for backwards compatibility with the first version of */
/*           SPKDIFF that could work only on continuous intervals if the */
/*           comparison window is a single interval. */

	    if (wncard_(cmpwin) == 1 && ! (*sample)) {

/*              Compute the number of steps that will be required to */
/*              step over the time interval with this step. */

		d__1 = et[1] - et[0];
		rmaind_(&d__1, step, &hdp1, &hdp2);

/*              If step is greater than time range, we will have only */
/*              two points. If step overflows maximum integer we reset */
/*              it to INTMAX-2 and error out later. If remainder is */
/*              closer than tolerance to zero, we will not introduce */
/*              "extra" step for the end of the interval. If otherwise */
/*              we will add an extra step for end of the interval. */

		if (hdp1 == 0.) {
		    *nitr = 2;
		} else if (hdp1 > (doublereal) (intmax_() - 2)) {
		    *nitr = intmax_() - 2;
		} else if (hdp2 <= 1e-8) {
		    *nitr = i_dnnt(&hdp1) + 1;
		    hdp1 += 1;
		} else {
		    *nitr = i_dnnt(&hdp1) + 2;
		    hdp1 += 2;
		}

/*              Check that this number of states will fit into the */
/*              buffer. */

		if (*nitr > 1000000) {
		    setmsg_("The number of states, #, computed using step of"
			    " # seconds within time interval from '#'(# TDB s"
			    "econds) to '#'(# TDB seconds) is greater than ca"
			    "n fit into program's buffers (# states maximum.)"
			    " Increase step or make the time window smaller i"
			    "n order to run the program.", (ftnlen)266);
		    errdp_("#", &hdp1, (ftnlen)1);
		    errdp_("#", step, (ftnlen)1);
		    errch_("#", time, (ftnlen)1, (ftnlen)1024);
		    errdp_("#", et, (ftnlen)1);
		    errch_("#", time + 1024, (ftnlen)1, (ftnlen)1024);
		    errdp_("#", &et[1], (ftnlen)1);
		    errint_("#", &c_b107, (ftnlen)1);
		    sigerr_("SPICE(STEPTOOSMALL2)", (ftnlen)20);
		}
	    } else {

/*              Compute the number of steps that will be required to */
/*              step over the comparison window with this step. To do */
/*              that, we loop over the window and increment the total */
/*              step count by 2 (one interval's start and one for stop) */
/*              plus whatever number of steps fits within the interval. */

		*nitr = 0;
		i__1 = wncard_(cmpwin);
		for (i__ = 1; i__ <= i__1; ++i__) {

/*                 Fetch endpoints of the next interval. */

		    wnfetd_(cmpwin, &i__, ett, &ett[1]);

/*                 Add one step for start of the interval. */

		    ++(*nitr);

/*                 Add one step for each point between endpoints */
/*                 up to the end of the interval minus padding. */

		    hdp2 = ett[1] - *step * .5;
		    count = 1;
		    hdp1 = ett[0] + *step * count;
		    while(hdp1 < hdp2 && *nitr < 1000000) {
			++(*nitr);
			++count;
			hdp1 = ett[0] + *step * count;
		    }

/*                 If interval begin time is not equal to interval end */
/*                 time add one step for the end of the interval. If we */
/*                 terminated the loop with NITR equal to MAXITR this */
/*                 will take us over the allowed maximum and will */
/*                 trigger the error message that follows. */

		    if (ett[0] != ett[1]) {
			++(*nitr);
		    }
		}

/*              Check if this number of points fits into the buffer. */

		if (*nitr > 1000000) {
		    setmsg_("The number of points within the # window determ"
			    "ined using step of '#' seconds specified after t"
			    "he command line key '#' is greater than can fit "
			    "into program's buffers (# epochs maximum.) Incre"
			    "ase the step or use command line keys '#' and '#"
			    "' to make the time window smaller in order to ru"
			    "n the program. The comparison window was #. # # #"
			    , (ftnlen)336);
		    if (*sample) {
			errch_("#", "sampling", (ftnlen)1, (ftnlen)8);
		    } else {
			errch_("#", "comparison", (ftnlen)1, (ftnlen)10);
		    }
		    errdp_("#", step, (ftnlen)1);
		    errch_("#", "-s", (ftnlen)1, (ftnlen)2);
		    errint_("#", &c_b107, (ftnlen)1);
		    errch_("#", "-b", (ftnlen)1, (ftnlen)2);
		    errch_("#", "-e", (ftnlen)1, (ftnlen)2);
		    if (s_cmp(timdsc, " ", (ftnlen)1024, (ftnlen)1) != 0) {
			if (s_cmp(covdsc, " ", (ftnlen)1024, (ftnlen)1) != 0 
				&& s_cmp(covdsc + 1024, " ", (ftnlen)1024, (
				ftnlen)1) != 0) {
			    errch_("#", "determined by applying constraints "
				    "specified on the command line to the int"
				    "ersection of coverages obtained from the"
				    " files", (ftnlen)1, (ftnlen)121);
			} else if (s_cmp(covdsc, " ", (ftnlen)1024, (ftnlen)1)
				 != 0 || s_cmp(covdsc + 1024, " ", (ftnlen)
				1024, (ftnlen)1) != 0) {
			    errch_("#", "determined by applying constraints "
				    "specified on the command line to the cov"
				    "erage obtained from the file", (ftnlen)1, 
				    (ftnlen)103);
			} else {
			    errch_("#", "specified on the command line", (
				    ftnlen)1, (ftnlen)29);
			}
		    } else {
			if (s_cmp(covdsc, " ", (ftnlen)1024, (ftnlen)1) != 0 
				&& s_cmp(covdsc + 1024, " ", (ftnlen)1024, (
				ftnlen)1) != 0) {
			    errch_("#", "determined by intersecting coverage"
				    "s obtained from the files", (ftnlen)1, (
				    ftnlen)60);
			} else if (s_cmp(covdsc, " ", (ftnlen)1024, (ftnlen)1)
				 != 0 || s_cmp(covdsc + 1024, " ", (ftnlen)
				1024, (ftnlen)1) != 0) {
			    errch_("#", "determined from the file", (ftnlen)1,
				     (ftnlen)24);
			} else {

/*                       We can never hit this branch. Set replacement */
/*                       string to indicate a bug. */

			    errch_("#", "not determined properly due to a bu"
				    "g in the program. Please, contact NAIF.", 
				    (ftnlen)1, (ftnlen)74);
			}
		    }
		    errch_("#", covdsc, (ftnlen)1, (ftnlen)1024);
		    if (s_cmp(covdsc, covdsc + 1024, (ftnlen)1024, (ftnlen)
			    1024) != 0) {
			errch_("#", covdsc + 1024, (ftnlen)1, (ftnlen)1024);
		    } else {
			errch_("#", " ", (ftnlen)1, (ftnlen)1);
		    }
		    errch_("#", timdsc, (ftnlen)1, (ftnlen)1024);
		    sigerr_("SPICE(STEPTOOSMALL2)", (ftnlen)20);
		}
	    }
	} else {
	    setmsg_("Time step '#' specified after '#' key is not a DP numbe"
		    "r.", (ftnlen)57);
	    errch_("#", clvals + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 :
		     s_rnge("clvals", i__1, "chwcml_", (ftnlen)2318)) << 10), 
		    (ftnlen)1, (ftnlen)1024);
	    errch_("#", "-s", (ftnlen)1, (ftnlen)2);
	    sigerr_("SPICE(NOTANDPNUMBER)", (ftnlen)20);
	}
    } else {

/*        Step was not provided on the command line. What about the */
/*        number of steps? */

	i__ = isrchc_("-n", &c__21, clkeys, (ftnlen)2, (ftnlen)32);
	if (clflag[(i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("clflag"
		, i__1, "chwcml_", (ftnlen)2331)]) {

/*           Is the number of steps an integer number? */

	    nparsi_(clvals + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : 
		    s_rnge("clvals", i__1, "chwcml_", (ftnlen)2336)) << 10), 
		    nitr, error, &ptr, (ftnlen)1024, (ftnlen)1024);
	    if (ptr == 0) {
		if (*nitr < 2 || *nitr > 1000000) {
		    setmsg_("Number of states must be an integer number betw"
			    "een # and #. It was #.", (ftnlen)69);
		    errint_("#", &c__2, (ftnlen)1);
		    errint_("#", &c_b107, (ftnlen)1);
		    errint_("#", nitr, (ftnlen)1);
		    sigerr_("SPICE(BADNOFSTATES)", (ftnlen)19);
		}
	    } else {
		setmsg_("Number of states '#' specified after '#'  key is no"
			"t an integer number.", (ftnlen)71);
		errch_("#", clvals + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? 
			i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)2354)
			) << 10), (ftnlen)1, (ftnlen)1024);
		errch_("#", "-n", (ftnlen)1, (ftnlen)2);
		sigerr_("SPICE(NOTANINTNUMBER)", (ftnlen)21);
	    }
	} else {

/*           For compatibility with the version 1, set number of states */
/*           to the default number or the number of minimum iterations */
/*           as was done in the verison 1 if the comparison window is a */
/*           single interval. */

	    if (wncard_(cmpwin) == 1) {
		if (et[1] == et[0]) {
		    *nitr = 2;
		} else {
		    *nitr = 1000;
		}
	    }
	}

/*        For compatibility with the version 1, calculate step for this */
/*        number of steps as was done in the version 1 if the comparison */
/*        window is a single interval. */

	if (wncard_(cmpwin) == 1 && ! (*sample)) {
	    *step = (et[1] - et[0]) / (doublereal) (*nitr - 1);
	}
    }

/*     If any of the dump reports was requested we need to check whether */
/*     output time format was provided on the command line. Default */
/*     format is ET seconds. */

    s_copy(timfmt, " ", timfmt_len, (ftnlen)1);
    if (eqstr_(diftyp, "dump", diftyp_len, (ftnlen)4) || eqstr_(diftyp, "dum"
	    "pvf", diftyp_len, (ftnlen)6) || eqstr_(diftyp, "dumpc", 
	    diftyp_len, (ftnlen)5) || eqstr_(diftyp, "dumpg", diftyp_len, (
	    ftnlen)5)) {
	i__ = isrchc_("-f", &c__21, clkeys, (ftnlen)2, (ftnlen)32);
	if (clflag[(i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("clflag"
		, i__1, "chwcml_", (ftnlen)2406)]) {
	    s_copy(timfmt, clvals + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? 
		    i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)2408)) <<
		     10), timfmt_len, (ftnlen)1024);

/*           In this incarnation of the program we are not going to try */
/*           to verify that the format is OK at the time when we process */
/*           command line. We will let TIMOUT fail when it's called for */
/*           the first time down stream from here. The only thing that */
/*           makes sense to check for is if the format string is non */
/*           blank. */

	    if (s_cmp(timfmt, " ", timfmt_len, (ftnlen)1) == 0) {
		setmsg_("Output time format specified after '#' key is blank."
			, (ftnlen)52);
		errch_("#", "-f", (ftnlen)1, (ftnlen)2);
		sigerr_("SPICE(BLANKTIMEFORMAT)", (ftnlen)22);
	    }
	}
    }

/*     If any of the dump reports was requested we need to check whether */
/*     the number significant digits for output was provided on the */
/*     command line. */

    *sigdig = 14;
    if (eqstr_(diftyp, "dump", diftyp_len, (ftnlen)4) || eqstr_(diftyp, "dum"
	    "pvf", diftyp_len, (ftnlen)6) || eqstr_(diftyp, "dumpc", 
	    diftyp_len, (ftnlen)5) || eqstr_(diftyp, "dumpg", diftyp_len, (
	    ftnlen)5)) {
	i__ = isrchc_("-d", &c__21, clkeys, (ftnlen)2, (ftnlen)32);
	if (clflag[(i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : s_rnge("clflag"
		, i__1, "chwcml_", (ftnlen)2443)]) {

/*           Is the number of digits an integer number? */

	    nparsi_(clvals + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? i__1 : 
		    s_rnge("clvals", i__1, "chwcml_", (ftnlen)2448)) << 10), 
		    sigdig, error, &ptr, (ftnlen)1024, (ftnlen)1024);
	    if (ptr == 0) {
		if (*sigdig < 6 || *sigdig > 17) {
		    setmsg_("Number of digits must be an integer number betw"
			    "een # and #. It was #.", (ftnlen)69);
		    errint_("#", &c__6, (ftnlen)1);
		    errint_("#", &c__17, (ftnlen)1);
		    errint_("#", sigdig, (ftnlen)1);
		    sigerr_("SPICE(BADNOFDIGITS)", (ftnlen)19);
		}
	    } else {
		setmsg_("Number of digits '#' specified after '#'  key is no"
			"t an integer number.", (ftnlen)71);
		errch_("#", clvals + (((i__1 = i__ - 1) < 21 && 0 <= i__1 ? 
			i__1 : s_rnge("clvals", i__1, "chwcml_", (ftnlen)2466)
			) << 10), (ftnlen)1, (ftnlen)1024);
		errch_("#", "-d", (ftnlen)1, (ftnlen)2);
		sigerr_("SPICE(NOTANINTNUMBER2)", (ftnlen)22);
	    }
	}
    }

/*     Check out. */

    chkout_("CHWCML", (ftnlen)6);
    return 0;
} /* chwcml_ */

