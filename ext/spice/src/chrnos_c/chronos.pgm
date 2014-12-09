/* chronos.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__19 = 19;
static integer c__2 = 2;
static integer c__4 = 4;
static integer c__10 = 10;
static integer c__5 = 5;
static integer c__1 = 1;
static integer c__34 = 34;
static integer c__6 = 6;

/* $Program      CHRONOS ( SPICE Time Conversion Tool) */
/* Main program */ MAIN__(void)
{
    /* System generated locals */
    address a__1[2], a__2[5], a__3[34], a__4[6];
    integer i__1, i__2, i__3[2], i__4, i__5, i__6[5], i__7[34], i__8[6];
    char ch__1[2070], ch__2[288], ch__3[281], ch__4[131];

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char file[256], ckfn[256];
    integer scid, hndl;
    logical done;
    char line[2048], fsrc[256], ftyp[32], tots[32], tott[32];
    extern /* Subroutine */ int zzgetenv_(char *, char *, ftnlen, ftnlen);
    integer i__, j, n;
    extern /* Subroutine */ int kdata_(integer *, char *, char *, char *, 
	    char *, integer *, logical *, ftnlen, ftnlen, ftnlen, ftnlen);
    char frafn[256];
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    char hline[2048];
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen);
    char pckfn[256];
    extern /* Subroutine */ int speak_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    logical found;
    char lskfn[256], spkfn[256];
    integer stdin;
    char hword[32];
    extern /* Subroutine */ int stdio_(char *, integer *, ftnlen);
    integer count;
    char error[256];
    extern integer rtrim_(char *, ftnlen);
    char scstr[32], types[32*10];
    logical clflag[19], ok;
    char hfname[256];
    extern /* Subroutine */ int readln_(integer *, char *, logical *, ftnlen);
    char cmdlin[2048];
    logical batmod, nolabl;
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    integer bodyid;
    char sclkfn[256];
    extern /* Subroutine */ int getcml_(char *, ftnlen);
    char clvals[2048*19], intime[256], fmtpic[64*4*10], clkeys[32*19], deftyp[
	    32*4];
    extern logical exists_(char *, ftnlen);
    char outtim[256], tofrmt[256], unprsd[2048], setpfn[256], censtr[32], 
	    curkey[32], fromts[32], fromtt[32], lsttim[32], solids[32], 
	    systms[32*4];
    integer frsidx, frtidx, solzer, tosidx, totidx;
    logical fmtted[40]	/* was [4][10] */, systyp[40]	/* was [4][10] */, 
	    envstp;
    extern /* Subroutine */ int errprt_(char *, char *, ftnlen, ftnlen), 
	    crcnst_(char *, char *, char *, logical *, logical *, char *, 
	    char *, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen), parcml_(char *, 
	    integer *, char *, logical *, char *, logical *, char *, ftnlen, 
	    ftnlen, ftnlen, ftnlen), dsplay_(char *, char *, ftnlen, ftnlen), 
	    spekon_(void), nparsi_(char *, integer *, char *, integer *, 
	    ftnlen, ftnlen), setmsg_(char *, ftnlen), sigerr_(char *, ftnlen),
	     nextwd_(char *, char *, char *, ftnlen, ftnlen, ftnlen), furnsh_(
	    char *, ftnlen), gcpool_(char *, integer *, integer *, integer *, 
	    char *, logical *, ftnlen, ftnlen), gipool_(char *, integer *, 
	    integer *, integer *, integer *, logical *, ftnlen), intstr_(
	    integer *, char *, ftnlen), dtpool_(char *, logical *, integer *, 
	    char *, ftnlen, ftnlen);
    char hwd[32];
    extern /* Subroutine */ int spekst_(char *, ftnlen), ktotal_(char *, 
	    integer *, ftnlen), cronos_(char *, integer *, char *, char *, 
	    ftnlen, ftnlen, ftnlen), tostdo_(char *, ftnlen), chkout_(char *, 
	    ftnlen), byebye_(char *, ftnlen);
    integer ptr;

/* $ Abstract */

/*     CHRONOS is a program converting time between various time systems */
/*     and types. Currently it supports the following time systems/ */
/*     types: */

/*        System         Type(s) */
/*       ---------      ------------------------ */
/*        UTC            SCET, ERT, ETT, LT */
/*        ET             SCET, ERT, ETT, LT, SECONDS */
/*        SCLK           SCLK, HEX, TICKS */
/*        LST            LST, LSUN */

/*     To convert time from one supported system/type to another */
/*     CHRONOS program usage is: */

/*           % CHRONOS  -SETUP <setup file name OR kernel file name(s)> */
/*                      -FROM <"from" time system> */
/*                     [-FROMTYPE <"from" time type>] */
/*                      -TO <"to" time system> */
/*                     [-TOTYPE <"to" time type>] */
/*                     [-FORMAT <output time format picture>] */
/*                      -TIME <input time> | -BATCH */
/*                     [-SC <sc ID>] */
/*                     [-CENTER <central body ID>] */
/*                     [-LANDINGTIME <UTC time of the landing>] */
/*                     [-SOL1INDEX <index of the first SOL>] */
/*                     [-NOLABEL] */
/*                     [-TRACE] */

/*     To display help: */

/*           % CHRONOS [-HELP|-H] */

/*     To display usage: */

/*           % CHRONOS -USAGE|-U */

/*     To display setup file template: */

/*           % CHRONOS  -TEMPLATE */

/*     Command line parameters specified in [] are optional. Command */
/*     line keys '-XXXXXXX' are case insensitive. Any of the command */
/*     line options can be present in the command line multiple times. */
/*     In such cases the last appearance takes precedence. Order in */
/*     which options are specified is insignificant. */

/*     Although not required, the program allows certain parameters to */
/*     be provided in a setup file. Setup file name is usually provided */
/*     on the command line. If it's not present there the program looks */
/*     for such name in environment variable CHRONOS_SETUP_FILE. */

/*     The setup file format should correspond to the SPICE Kernel Text */
/*     file format specification, i.e. it must contain data formatted as */
/*     a set of KEYWORD=VALUE assignments enclosed between */

/*        \begindata */
/*        \begintext */

/*     markers. Each assignment and marker must be on a line by itself. */

/*     The following parameters may be provided in a setup file: */

/*        \begindata */
/*           LEAPSECONDS_FILE = 'name of a LSK file' */
/*           SCLK_FILE = 'name of a SCLK file for the mission' */
/*           PCK_FILE = 'name of a PCK file' */
/*           SPK_FILES = ( 'name of an SPK file', '...' ) */
/*           CK_FILES = ( 'name of a CK file', '...' ) */
/*           FRAMES_FILE = 'name of a frame definitions file' */
/*           SPACECRAFT_ID = NAIF ID for the spacecraft */
/*           CENTER_ID = NAIF ID for the center body */
/*           LANDING_TIME = 'UTC time of the landing' */
/*           LANDING_SOL_INDEX = SOL index of the landing */
/*        \begintext */

/*     Note that either or all of the SPACECRAFT_ID, CENTER_ID, */
/*     LANDING_TIME, and LANDING_SOL_INDEX parameters can also be */
/*     provided using the command line switches. If done so, the setup */
/*     file value corresponding to a command line value is not needed, */
/*     and, if present, is ignored by the program. */

/*     Similarly, the kernels files to be loaded can be provided using */
/*     the standard SPICE interface -- with the KERNELS_TO_LOAD */
/*     parameter: */

/*        \begindata */
/*           KERNELS_TO_LOAD = ( */
/*                         'name of a LSK file', */
/*                         'name of a SCLK file ', */
/*                         'name of a PCK file', */
/*                         'name of an SPK file', */
/*                         '...', */
/*                         'name of a CK file', */
/*                         '...', */
/*                         'name of an FK file' */
/*                             ) */
/*        \begintext */

/*     or even by simply listing them after the -SETUP command line */
/*     switch. In either of these two cases, specifying the */
/*     LEAPSECONDS_FILE, SCLK_FILE, PCK_FILE, SPK_FILES, CK_FILES, and */
/*     FRAMES_FILE setup file parameters is not necessary. */

/*     Note that if any other KEYWORD=VALUE definitions (such as LSK or */
/*     PCK values) are present in the setup file, they get loaded into */
/*     the program automatically. In such cases programs checks whether */
/*     required LSK values are already loaded and if so it's doesn't */
/*     load external LSK file(s). */

/*     Custom format for a particular time system/type pair can be */
/*     specified in a setup file using keywords [SYSTEM]_{TYPE}_FORMAT, */
/*     where [SYSTEM] and {TYPE} are corresponding character */
/*     designations (for example: UTC_SCET_FORMAT = 'YYYY-DOY//HR:MN') */

/*     See CHRONOS User's Guide for more information. */

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

/*     CHRONOS.UG */
/*     TIME.REQ */
/*     SCLK.REQ */

/* $ Author_and_Institution */

/*     B.V.Semenov      (JPL) */

/* $ Version */

/* -    CHRONOS Version 2.2.0, 28-OCT-2011 (BVS) */

/*        Moved PARCML to support. Updated its calling sequence. */

/* -    CHRONOS Version 2.1.0, 18-AUG-2006 (BVS) */

/*        The CROHOS subroutine was updated to fix the bug causing */
/*        incorrect computation of the local solar time second length. */
/*        This change has also resulted in conversions from LST/LTST to */
/*        any other time system/type to produce results that differ from */
/*        those produced by the previous version by up to one second. */

/*        The LSTMID subroutine was updated to fix the bug that */
/*        prevented termination of the midnight search loop for input */
/*        local times at the end of a local day (23:59:xx). */

/*        No change in overall program functionality. */

/* -    CHRONOS Version 2.0.2, 07-OCT-2005 (BVS) */

/*        CROHOS subroutine was updated to remove non-standard use of */
/*        duplicate arguments in subroutine calls (LSTMID and LTIME). No */
/*        change in overall program functionality. */

/* -    CHRONOS Version 2.0.1, 17-MAR-2004 (BVS) */

/*        Small infrastructure-related tweak in CRONOS. No change */
/*        in functionality. */

/* -    CHRONOS Version 2.0.0, 29-JUL-2003 (BVS) */

/*        The program was re-written to call CRONOS routine to do the */
/*        conversions. Command line was extended to allow specification */
/*        of the s/c id, center id, landing time, the first SOL */
/*        index, and list of the kernel files in place of a single */
/*        setup file. */

/* -    CHRONOS Version 1.4.0, October 16, 2002 (BVS) */

/*        Fixed LSTMID bug causing midnight search loop not terminate */
/*        in some obscure cases. */

/* -    CHRONOS Version 1.3.0, September 8, 2002 (NJB) */

/*        File name declarations now use system-dependent name */
/*        length declared in chronos.inc. */

/* -    CHRONOS Version 1.2.3, 25-NOV-2001 (BVS) */

/*        Replaced INT with IDNINT in the last LSTMID call in the input */
/*        LST processing branch to prevent "loosing" second due to round */
/*        off on some platforms. */

/* -    CHRONOS Version 1.2.2, 03-MAY-2001 (BVS)(EDW) */

/*        Fixed DSPLAY to show backslash in template display on all */
/*        platforms. Added a BYEBYE( 'SUCCESS' ) call at program's end. */

/* -    CHRONOS Version 1.2.1, 01-NOV-1999 (WLT) */

/*        Declared LTIME to be an external routine. */

/* -    CHRONOS Version 1.2.0, 14-OCT-1999 (WLT) */

/*        Made chronos.pgm into chronos.pgm.mst to get around VMS */
/*        Command line interpreter problems of converting everything to */
/*        upper case (or lower case if we use GETCML).  This is a */
/*        temporary fix for version N0050 of the SPICE Toolkit. No other */
/*        environments are affected. */

/* -    CHRONOS Version 1.1.0, 17-FEB-1999 (BVS) */

/*        Changed output SCLK/TICKS conversion to use SCE2C (continuous */
/*        ticks.) Enabled output formatting for this system/type. */

/* -    CHRONOS Version 1.0.0, 14-MAY-1998 (BVS) */

/* -& */

/*     CHRONOS Include file. */


/*     SPICELIB functions. */

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


/*     CHRONOS Include file. */


/*     CHRONOS Version. */


/*     Environment variable that contains CHRONOS setup file name. */


/*     LST SOL marker. */


/*     Command lines keys and total number of them. */


/*     Setup file variables. */


/*     Time system indentifier strings and total number of time */
/*     systems. */


/*     Time types identitifier strings and total number of time types. */


/*     Line size parameters. */


/*     File name length parameters. */


/*     Variables. */


/*     Standard SPICE error handling. */

    chkin_("CHRONOS", (ftnlen)7);

/*     Display only short and long error messages. */

    errprt_("SET", "NONE, SHORT, LONG, TRACEBACK", (ftnlen)3, (ftnlen)28);

/*     Get STDIN unit. */

    stdio_("STDIN", &stdin, (ftnlen)5);

/*     Get the command line. */

    getcml_(line, (ftnlen)2048);
    s_copy(hline, line, (ftnlen)2048, (ftnlen)2048);

/*     Get parameter arrays -- time systems and types, format pictures */
/*     and flags, and command line keys. */

    crcnst_(systms, types, deftyp, systyp, fmtted, fmtpic, clkeys, (ftnlen)32,
	     (ftnlen)32, (ftnlen)32, (ftnlen)64, (ftnlen)32);

/*     Parse command line. */

    parcml_(hline, &c__19, clkeys, clflag, clvals, &found, unprsd, (ftnlen)
	    2048, (ftnlen)32, (ftnlen)2048, (ftnlen)2048);

/*     Are there any keys on the command line? Is one of the usage keys */
/*     present? Display USAGE and STOP if yes. */

    if (! found || clflag[(i__1 = isrchc_("-USAGE", &c__19, clkeys, (ftnlen)6,
	     (ftnlen)32) - 1) < 19 && 0 <= i__1 ? i__1 : s_rnge("clflag", 
	    i__1, "chronos_", (ftnlen)360)] || clflag[(i__2 = isrchc_("-U", &
	    c__19, clkeys, (ftnlen)2, (ftnlen)32) - 1) < 19 && 0 <= i__2 ? 
	    i__2 : s_rnge("clflag", i__2, "chronos_", (ftnlen)360)]) {
	dsplay_("USAGE", "STOP", (ftnlen)5, (ftnlen)4);
    }

/*     Are there any help keys? Display HELP and STOP if yes. */

    if (clflag[(i__1 = isrchc_("-HELP", &c__19, clkeys, (ftnlen)5, (ftnlen)32)
	     - 1) < 19 && 0 <= i__1 ? i__1 : s_rnge("clflag", i__1, "chronos_"
	    , (ftnlen)371)] || clflag[(i__2 = isrchc_("-H", &c__19, clkeys, (
	    ftnlen)2, (ftnlen)32) - 1) < 19 && 0 <= i__2 ? i__2 : s_rnge(
	    "clflag", i__2, "chronos_", (ftnlen)371)]) {
	dsplay_("HELP", "STOP", (ftnlen)4, (ftnlen)4);
    }

/*     What about template key? Display TEMPLATE and STOP if it's */
/*     present. */

    if (clflag[(i__1 = isrchc_("-TEMPLATE", &c__19, clkeys, (ftnlen)9, (
	    ftnlen)32) - 1) < 19 && 0 <= i__1 ? i__1 : s_rnge("clflag", i__1, 
	    "chronos_", (ftnlen)382)]) {
	dsplay_("TEMPLATE", "STOP", (ftnlen)8, (ftnlen)4);
    }

/*     If batch mode was requested, set the flag. */

    batmod = clflag[(i__1 = isrchc_("-BATCH", &c__19, clkeys, (ftnlen)6, (
	    ftnlen)32) - 1) < 19 && 0 <= i__1 ? i__1 : s_rnge("clflag", i__1, 
	    "chronos_", (ftnlen)391)];

/*     If no label printing was requested, set the flag. */

    nolabl = clflag[(i__1 = isrchc_("-NOLABEL", &c__19, clkeys, (ftnlen)8, (
	    ftnlen)32) - 1) < 19 && 0 <= i__1 ? i__1 : s_rnge("clflag", i__1, 
	    "chronos_", (ftnlen)396)];

/*     If printing trace was requested, set "speaker" ON. Also, change */
/*     label setting -- when trace ON label should be printed. */

    if (clflag[(i__1 = isrchc_("-TRACE", &c__19, clkeys, (ftnlen)6, (ftnlen)
	    32) - 1) < 19 && 0 <= i__1 ? i__1 : s_rnge("clflag", i__1, "chro"
	    "nos_", (ftnlen)402)]) {
	spekon_();
	dsplay_("VERSION", "PROCEED", (ftnlen)7, (ftnlen)7);
/* Writing concatenation */
	i__3[0] = 22, a__1[0] = " Actual command line: ";
	i__3[1] = rtrim_(line, (ftnlen)2048), a__1[1] = line;
	s_cat(ch__1, a__1, i__3, &c__2, (ftnlen)2070);
	speak_(ch__1, rtrim_(line, (ftnlen)2048) + 22);
	speak_(" ", (ftnlen)1);
	nolabl = FALSE_;
    }

/*     Well, nobody wants information about the program, they just want */
/*     to run it. So, check what other stuff we got on the command line. */
/*     But first, let set all values to blanks. */

    s_copy(setpfn, " ", (ftnlen)256, (ftnlen)1);
    s_copy(fromts, " ", (ftnlen)32, (ftnlen)1);
    s_copy(fromtt, " ", (ftnlen)32, (ftnlen)1);
    s_copy(tots, " ", (ftnlen)32, (ftnlen)1);
    s_copy(tott, " ", (ftnlen)32, (ftnlen)1);
    s_copy(tofrmt, " ", (ftnlen)256, (ftnlen)1);
    s_copy(intime, " ", (ftnlen)256, (ftnlen)1);
    s_copy(scstr, " ", (ftnlen)32, (ftnlen)1);
    s_copy(censtr, " ", (ftnlen)32, (ftnlen)1);
    s_copy(lsttim, " ", (ftnlen)32, (ftnlen)1);
    s_copy(solids, " ", (ftnlen)32, (ftnlen)1);
    for (i__ = 1; i__ <= 19; ++i__) {
	if (clflag[(i__1 = i__ - 1) < 19 && 0 <= i__1 ? i__1 : s_rnge("clflag"
		, i__1, "chronos_", (ftnlen)431)]) {
	    s_copy(curkey, clkeys + (((i__1 = i__ - 1) < 19 && 0 <= i__1 ? 
		    i__1 : s_rnge("clkeys", i__1, "chronos_", (ftnlen)433)) <<
		     5), (ftnlen)32, (ftnlen)32);
	    if (s_cmp(curkey, "-SETUP", (ftnlen)32, (ftnlen)6) == 0) {

/*              It's a setup file name. */

		s_copy(setpfn, clvals + (((i__1 = i__ - 1) < 19 && 0 <= i__1 ?
			 i__1 : s_rnge("clvals", i__1, "chronos_", (ftnlen)
			439)) << 11), (ftnlen)256, rtrim_(clvals + (((i__2 = 
			i__ - 1) < 19 && 0 <= i__2 ? i__2 : s_rnge("clvals", 
			i__2, "chronos_", (ftnlen)439)) << 11), (ftnlen)2048))
			;
	    } else if (s_cmp(curkey, "-FROM", (ftnlen)32, (ftnlen)5) == 0) {

/*              It's "from" time system. */

		s_copy(fromts, clvals + (((i__1 = i__ - 1) < 19 && 0 <= i__1 ?
			 i__1 : s_rnge("clvals", i__1, "chronos_", (ftnlen)
			445)) << 11), (ftnlen)32, rtrim_(clvals + (((i__2 = 
			i__ - 1) < 19 && 0 <= i__2 ? i__2 : s_rnge("clvals", 
			i__2, "chronos_", (ftnlen)445)) << 11), (ftnlen)2048))
			;
		ucase_(fromts, fromts, (ftnlen)32, (ftnlen)32);
	    } else if (s_cmp(curkey, "-FROMTYPE", (ftnlen)32, (ftnlen)9) == 0)
		     {

/*              It's "from" time type. */

		s_copy(fromtt, clvals + (((i__1 = i__ - 1) < 19 && 0 <= i__1 ?
			 i__1 : s_rnge("clvals", i__1, "chronos_", (ftnlen)
			452)) << 11), (ftnlen)32, rtrim_(clvals + (((i__2 = 
			i__ - 1) < 19 && 0 <= i__2 ? i__2 : s_rnge("clvals", 
			i__2, "chronos_", (ftnlen)452)) << 11), (ftnlen)2048))
			;
		ucase_(fromtt, fromtt, (ftnlen)32, (ftnlen)32);
	    } else if (s_cmp(curkey, "-TO", (ftnlen)32, (ftnlen)3) == 0) {

/*              It's "to" time system. */

		s_copy(tots, clvals + (((i__1 = i__ - 1) < 19 && 0 <= i__1 ? 
			i__1 : s_rnge("clvals", i__1, "chronos_", (ftnlen)459)
			) << 11), (ftnlen)32, rtrim_(clvals + (((i__2 = i__ - 
			1) < 19 && 0 <= i__2 ? i__2 : s_rnge("clvals", i__2, 
			"chronos_", (ftnlen)459)) << 11), (ftnlen)2048));
		ucase_(tots, tots, (ftnlen)32, (ftnlen)32);
	    } else if (s_cmp(curkey, "-TOTYPE", (ftnlen)32, (ftnlen)7) == 0) {

/*              It's "to" time type. */

		s_copy(tott, clvals + (((i__1 = i__ - 1) < 19 && 0 <= i__1 ? 
			i__1 : s_rnge("clvals", i__1, "chronos_", (ftnlen)466)
			) << 11), (ftnlen)32, rtrim_(clvals + (((i__2 = i__ - 
			1) < 19 && 0 <= i__2 ? i__2 : s_rnge("clvals", i__2, 
			"chronos_", (ftnlen)466)) << 11), (ftnlen)2048));
		ucase_(tott, tott, (ftnlen)32, (ftnlen)32);
	    } else if (s_cmp(curkey, "-FORMAT", (ftnlen)32, (ftnlen)7) == 0) {

/*              It's "to" time format. */

		s_copy(tofrmt, clvals + (((i__1 = i__ - 1) < 19 && 0 <= i__1 ?
			 i__1 : s_rnge("clvals", i__1, "chronos_", (ftnlen)
			473)) << 11), (ftnlen)256, rtrim_(clvals + (((i__2 = 
			i__ - 1) < 19 && 0 <= i__2 ? i__2 : s_rnge("clvals", 
			i__2, "chronos_", (ftnlen)473)) << 11), (ftnlen)2048))
			;
	    } else if (s_cmp(curkey, "-TIME", (ftnlen)32, (ftnlen)5) == 0) {

/*              It's input time string. */

		s_copy(intime, clvals + (((i__1 = i__ - 1) < 19 && 0 <= i__1 ?
			 i__1 : s_rnge("clvals", i__1, "chronos_", (ftnlen)
			479)) << 11), (ftnlen)256, rtrim_(clvals + (((i__2 = 
			i__ - 1) < 19 && 0 <= i__2 ? i__2 : s_rnge("clvals", 
			i__2, "chronos_", (ftnlen)479)) << 11), (ftnlen)2048))
			;
	    } else if (s_cmp(curkey, "-SC", (ftnlen)32, (ftnlen)3) == 0) {

/*              It's spacecraft ID; save it in string. */

		s_copy(scstr, clvals + (((i__1 = i__ - 1) < 19 && 0 <= i__1 ? 
			i__1 : s_rnge("clvals", i__1, "chronos_", (ftnlen)485)
			) << 11), (ftnlen)32, rtrim_(clvals + (((i__2 = i__ - 
			1) < 19 && 0 <= i__2 ? i__2 : s_rnge("clvals", i__2, 
			"chronos_", (ftnlen)485)) << 11), (ftnlen)2048));
	    } else if (s_cmp(curkey, "-CENTER", (ftnlen)32, (ftnlen)7) == 0) {

/*              It's body ID; save it in string. */

		s_copy(censtr, clvals + (((i__1 = i__ - 1) < 19 && 0 <= i__1 ?
			 i__1 : s_rnge("clvals", i__1, "chronos_", (ftnlen)
			491)) << 11), (ftnlen)32, rtrim_(clvals + (((i__2 = 
			i__ - 1) < 19 && 0 <= i__2 ? i__2 : s_rnge("clvals", 
			i__2, "chronos_", (ftnlen)491)) << 11), (ftnlen)2048))
			;
	    } else if (s_cmp(curkey, "-LANDINGTIME", (ftnlen)32, (ftnlen)12) 
		    == 0) {

/*              It's landing time. */

		s_copy(lsttim, clvals + (((i__1 = i__ - 1) < 19 && 0 <= i__1 ?
			 i__1 : s_rnge("clvals", i__1, "chronos_", (ftnlen)
			497)) << 11), (ftnlen)32, rtrim_(clvals + (((i__2 = 
			i__ - 1) < 19 && 0 <= i__2 ? i__2 : s_rnge("clvals", 
			i__2, "chronos_", (ftnlen)497)) << 11), (ftnlen)2048))
			;
	    } else if (s_cmp(curkey, "-SOL1INDEX", (ftnlen)32, (ftnlen)10) == 
		    0) {

/*              It's SOL 1 index; save it in string and integer. */

		s_copy(solids, clvals + (((i__1 = i__ - 1) < 19 && 0 <= i__1 ?
			 i__1 : s_rnge("clvals", i__1, "chronos_", (ftnlen)
			503)) << 11), (ftnlen)32, rtrim_(clvals + (((i__2 = 
			i__ - 1) < 19 && 0 <= i__2 ? i__2 : s_rnge("clvals", 
			i__2, "chronos_", (ftnlen)503)) << 11), (ftnlen)2048))
			;
		nparsi_(solids, &solzer, error, &ptr, (ftnlen)32, (ftnlen)256)
			;
		if (ptr != 0) {
		    setmsg_("The first SOL index provided on the command lin"
			    "e, #, is not an integer number.", (ftnlen)78);
		    errch_("#", solids, (ftnlen)1, (ftnlen)32);
		    sigerr_("SPICE(BADSOLINDEX)", (ftnlen)18);
		}
	    }
	}
    }

/*     Although CRONOS routine will throughly check consistency of the */
/*     inputs, from/to-system/types should be verified in this module */
/*     too. They are used to process other setups that can be provided */
/*     through the setup file. Start with "FROM" time system. */

    frsidx = isrchc_(fromts, &c__4, systms, (ftnlen)32, (ftnlen)32);
    if (s_cmp(fromts, " ", (ftnlen)32, (ftnlen)1) != 0) {
	if (frsidx == 0) {
	    setmsg_("Time system '#' from which the time must be converted i"
		    "s not one of the supported systems.", (ftnlen)90);
	    errch_("#", fromts, (ftnlen)1, (ftnlen)32);
	    sigerr_("SPICE(BADFROMTIMESYSTEM)", (ftnlen)24);
	}
    } else {
	setmsg_("Time system from which the time must be converted wasn't sp"
		"ecified on the command line using # switch.", (ftnlen)102);
	errch_("#", "-FROM", (ftnlen)1, (ftnlen)5);
	sigerr_("SPICE(NOFROMTIMESYSTEM)", (ftnlen)23);
    }

/*     "TO" time system goes second. */

    tosidx = isrchc_(tots, &c__4, systms, (ftnlen)32, (ftnlen)32);
    if (s_cmp(tots, " ", (ftnlen)32, (ftnlen)1) != 0) {
	if (tosidx == 0) {
	    setmsg_("Time system '#' to which the time must be converted is "
		    "not one of the supported systems.", (ftnlen)88);
	    errch_("#", tots, (ftnlen)1, (ftnlen)32);
	    sigerr_("SPICE(BADTOTIMESYSTEM)", (ftnlen)22);
	}
    } else {
	setmsg_("Time system to which the time must be converted wasn't spec"
		"ified on the command line using # switch.", (ftnlen)100);
	errch_("#", "-TO", (ftnlen)1, (ftnlen)3);
	sigerr_("SPICE(NOTOTIMESYSTEM)", (ftnlen)21);
    }

/*     "FROM" time type goes third. */

    frtidx = isrchc_(fromtt, &c__10, types, (ftnlen)32, (ftnlen)32);
    if (frtidx != 0) {
	if (! systyp[(i__1 = frsidx + (frtidx << 2) - 5) < 40 && 0 <= i__1 ? 
		i__1 : s_rnge("systyp", i__1, "chronos_", (ftnlen)568)]) {
	    setmsg_("Time type '#' is not applicable for time system '#' fro"
		    "m which the time must be converted.", (ftnlen)90);
	    errch_("#", fromtt, (ftnlen)1, (ftnlen)32);
	    errch_("#", fromts, (ftnlen)1, (ftnlen)32);
	    sigerr_("SPICE(MISMATCHFROMTIMETYPE)", (ftnlen)27);
	}
    } else {
	if (s_cmp(fromtt, " ", (ftnlen)32, (ftnlen)1) != 0) {
	    setmsg_("Time type '#' from which the time must be converted is "
		    "not one of the supported types.", (ftnlen)86);
	    errch_("#", fromtt, (ftnlen)1, (ftnlen)32);
	    sigerr_("SPICE(BADFROMTIMETYPE)", (ftnlen)22);
	} else {

/*           Set "from" type to default type for that system if the */
/*           value that we have got is blank. */

	    s_copy(fromtt, deftyp + (((i__1 = frsidx - 1) < 4 && 0 <= i__1 ? 
		    i__1 : s_rnge("deftyp", i__1, "chronos_", (ftnlen)588)) <<
		     5), (ftnlen)32, (ftnlen)32);
	    frtidx = isrchc_(fromtt, &c__10, types, (ftnlen)32, (ftnlen)32);
	}
    }

/*     "TO" time type goes fourth. */

    totidx = isrchc_(tott, &c__10, types, (ftnlen)32, (ftnlen)32);
    if (totidx != 0) {
	if (! systyp[(i__1 = tosidx + (totidx << 2) - 5) < 40 && 0 <= i__1 ? 
		i__1 : s_rnge("systyp", i__1, "chronos_", (ftnlen)598)]) {
	    setmsg_("Time type '#' is not applicable for time system '#' to "
		    "which the time must be converted.", (ftnlen)88);
	    errch_("#", tott, (ftnlen)1, (ftnlen)32);
	    errch_("#", tots, (ftnlen)1, (ftnlen)32);
	    sigerr_("SPICE(MISMATCHTOTIMETYPE)", (ftnlen)25);
	}
    } else {
	if (s_cmp(tott, " ", (ftnlen)32, (ftnlen)1) != 0) {
	    setmsg_("Time type '#' to which the time must be converted is no"
		    "t one of the supported types.", (ftnlen)84);
	    errch_("#", tott, (ftnlen)1, (ftnlen)32);
	    sigerr_("SPICE(BADTOTIMETYPE)", (ftnlen)20);
	} else {

/*           Set "to" type to default type for that system if the value */
/*           that we have got is blank. */

	    s_copy(tott, deftyp + (((i__1 = tosidx - 1) < 4 && 0 <= i__1 ? 
		    i__1 : s_rnge("deftyp", i__1, "chronos_", (ftnlen)618)) <<
		     5), (ftnlen)32, (ftnlen)32);
	    totidx = isrchc_(tott, &c__10, types, (ftnlen)32, (ftnlen)32);
	}
    }

/*     If setup file name was not provided on the command line or if it */
/*     was blank, look for environment variable. */

    if (s_cmp(setpfn, " ", (ftnlen)256, (ftnlen)1) == 0) {
	zzgetenv_("CHRONOS_SETUP_FILE", setpfn, (ftnlen)18, (ftnlen)256);
	if (s_cmp(setpfn, " ", (ftnlen)256, (ftnlen)1) == 0) {
	    setmsg_("Setup file name or kernel file names weren't provided n"
		    "either on the command line using # switch nor via enviro"
		    "nment variable '#'.", (ftnlen)130);
	    errch_("#", "-SETUP", (ftnlen)1, (ftnlen)6);
	    errch_("#", "CHRONOS_SETUP_FILE", (ftnlen)1, (ftnlen)18);
	    sigerr_("SPICE(NOFILENAMES)", (ftnlen)18);
	}
	envstp = TRUE_;
    } else {
	envstp = FALSE_;
    }

/*     The value provided with the setup file command line key or */
/*     environment variable can contain not just one but a list of file */
/*     names. This way the user can provide all kernel names on the */
/*     command line without even bothering to make a setup file. */
/*     Therefore, the files from SETPFN value must be loaded in a loop. */

    while(s_cmp(setpfn, " ", (ftnlen)256, (ftnlen)1) != 0) {

/*        Pick the next file name in the list. */

	nextwd_(setpfn, hfname, setpfn, (ftnlen)256, (ftnlen)256, (ftnlen)256)
		;
	if (! exists_(hfname, (ftnlen)256)) {
	    if (envstp) {
		setmsg_("The file '#' listed in the environment variable '#'"
			" doesn't exist.", (ftnlen)66);
		errch_("#", hfname, (ftnlen)1, (ftnlen)256);
		errch_("#", "CHRONOS_SETUP_FILE", (ftnlen)1, (ftnlen)18);
		sigerr_("SPICE(FILEDOESNTEXIST1)", (ftnlen)23);
	    } else {
		setmsg_("The file '#' listed on the command line doesn't exi"
			"st.", (ftnlen)54);
		errch_("#", hfname, (ftnlen)1, (ftnlen)256);
		sigerr_("SPICE(FILEDOESNTEXIST2)", (ftnlen)23);
	    }
	}

/*        Load the file using FURNSH */

	furnsh_(hfname, (ftnlen)256);
    }

/*     First, process all inputs that could have been provided on the */
/*     command line or in the old-style setup file, namely: output time */
/*     format, s/c and body IDs, landing time, and first SOL index. The */
/*     rule is that command line value, if present, overwrites setup */
/*     file value. */


/*     If output format string has not been provided on the command */
/*     line, try to get it from the pool. */

    if (s_cmp(tofrmt, " ", (ftnlen)256, (ftnlen)1) == 0) {

/*        Make up a KEYWORD corresponding to this system/type format and */
/*        see if it was loaded. */

/* Writing concatenation */
	i__6[0] = rtrim_(systms + (((i__2 = tosidx - 1) < 4 && 0 <= i__2 ? 
		i__2 : s_rnge("systms", i__2, "chronos_", (ftnlen)705)) << 5),
		 (ftnlen)32), a__2[0] = systms + (((i__1 = tosidx - 1) < 4 && 
		0 <= i__1 ? i__1 : s_rnge("systms", i__1, "chronos_", (ftnlen)
		705)) << 5);
	i__6[1] = 1, a__2[1] = "_";
	i__6[2] = rtrim_(types + (((i__5 = totidx - 1) < 10 && 0 <= i__5 ? 
		i__5 : s_rnge("types", i__5, "chronos_", (ftnlen)705)) << 5), 
		(ftnlen)32), a__2[2] = types + (((i__4 = totidx - 1) < 10 && 
		0 <= i__4 ? i__4 : s_rnge("types", i__4, "chronos_", (ftnlen)
		705)) << 5);
	i__6[3] = 1, a__2[3] = "_";
	i__6[4] = 6, a__2[4] = "FORMAT";
	s_cat(hword, a__2, i__6, &c__5, (ftnlen)32);
	gcpool_(hword, &c__1, &c__1, &n, tofrmt, &found, (ftnlen)32, (ftnlen)
		256);
	if (! found) {

/*           If it wasn't provided in the setup file or on the command */
/*           line, leave it blank. */

	    s_copy(tofrmt, " ", (ftnlen)256, (ftnlen)1);
	}
    }

/*     If s/c ID has not been provided on the command line, try to get */
/*     it for the pool. */

    if (s_cmp(scstr, " ", (ftnlen)32, (ftnlen)1) == 0) {

/*        S/C ID is only needed if input/output system is SCLK (used in */
/*        SCLK routine calls) or if input/output type is LST, ERT, ETT, */
/*        or and LT (used in SPK/LTIME calls.) */

	if (s_cmp(fromts, "SCLK", (ftnlen)32, (ftnlen)4) == 0 || s_cmp(tots, 
		"SCLK", (ftnlen)32, (ftnlen)4) == 0 || s_cmp(fromts, "LST", (
		ftnlen)32, (ftnlen)3) == 0 || s_cmp(tots, "LST", (ftnlen)32, (
		ftnlen)3) == 0 || s_cmp(fromtt, "ERT", (ftnlen)32, (ftnlen)3) 
		== 0 || s_cmp(tott, "ERT", (ftnlen)32, (ftnlen)3) == 0 || 
		s_cmp(fromtt, "ETT", (ftnlen)32, (ftnlen)3) == 0 || s_cmp(
		tott, "ETT", (ftnlen)32, (ftnlen)3) == 0 || s_cmp(fromtt, 
		"LT", (ftnlen)32, (ftnlen)2) == 0 || s_cmp(tott, "LT", (
		ftnlen)32, (ftnlen)2) == 0) {
	    gipool_("SPACECRAFT_ID", &c__1, &c__1, &n, &scid, &found, (ftnlen)
		    13);
	    if (found) {
		intstr_(&scid, scstr, (ftnlen)32);
	    } else {
		setmsg_("S/C ID needed for time conversion that has been req"
			"uested was not provided on the command line of in th"
			"e setup file.", (ftnlen)116);
		sigerr_("SPICE(NOSCID)", (ftnlen)13);
	    }
	}
    }

/*     If center ID has not been provided on the command line, try to */
/*     get it from the pool. */

    if (s_cmp(censtr, " ", (ftnlen)32, (ftnlen)1) == 0) {

/*        Body ID is only needed if input or output time system is LST. */

	if (s_cmp(fromts, "LST", (ftnlen)32, (ftnlen)3) == 0 || s_cmp(tots, 
		"LST", (ftnlen)32, (ftnlen)3) == 0) {
	    gipool_("CENTER_ID", &c__1, &c__1, &n, &bodyid, &found, (ftnlen)9)
		    ;
	    if (found) {
		intstr_(&bodyid, censtr, (ftnlen)32);
	    } else {
		setmsg_("Body ID needed for time conversion that has been re"
			"quested was not provided on the command line of in t"
			"he setup file.", (ftnlen)117);
		sigerr_("SPICE(NOBODYID)", (ftnlen)15);
	    }
	}
    }

/*     If landing time has not been provided on the command line, try to */
/*     get it from the pool. */

    if (s_cmp(lsttim, " ", (ftnlen)32, (ftnlen)1) == 0) {

/*        Landing time is only needed if input or output time system is */
/*        LST. */

	if (s_cmp(fromts, "LST", (ftnlen)32, (ftnlen)3) == 0 || s_cmp(tots, 
		"LST", (ftnlen)32, (ftnlen)3) == 0) {
	    gcpool_("LANDING_TIME", &c__1, &c__1, &n, lsttim, &found, (ftnlen)
		    12, (ftnlen)32);

/*           If LST hasn't been provided in either way, it's OK. It only */
/*           means that CRONOS will not count local days. */

	    if (! found) {
		s_copy(lsttim, " ", (ftnlen)32, (ftnlen)1);
	    }
	}
    }

/*     If first Sol index has not been provided on the command line, try */
/*     to get it from the pool. */

    if (s_cmp(solids, " ", (ftnlen)32, (ftnlen)1) == 0) {

/*        First SOL index is only needed if input or output time system */
/*        is LST. */

	if (s_cmp(fromts, "LST", (ftnlen)32, (ftnlen)3) == 0 || s_cmp(tots, 
		"LST", (ftnlen)32, (ftnlen)3) == 0) {
	    gipool_("LANDING_SOL_INDEX", &c__1, &c__1, &n, &solzer, &found, (
		    ftnlen)17);

/*           If it has not been found, it's OK. It will be set to the */
/*           default value in CRONOS. */

	    if (found) {
		intstr_(&solzer, solids, (ftnlen)32);
	    } else {
		s_copy(solids, " ", (ftnlen)32, (ftnlen)1);
	    }
	}
    }

/*     Time to process kernels files. Some of them may have been already */
/*     loaded by an earlier call to FURNSH (both directly or through */
/*     KERNELS_TO_LOAD keyword.) */


/*     We always need LSK data and LSK values are probably the only we */
/*     can safely check on. */

    ok = TRUE_;
    dtpool_("DELTET/DELTA_T_A", &found, &n, hwd, (ftnlen)16, (ftnlen)32);
    ok = ok && found && n >= 1 && s_cmp(hwd, "N", rtrim_(hwd, (ftnlen)32), (
	    ftnlen)1) == 0;
    dtpool_("DELTET/K", &found, &n, hwd, (ftnlen)8, (ftnlen)32);
    ok = ok && found && n >= 1 && s_cmp(hwd, "N", rtrim_(hwd, (ftnlen)32), (
	    ftnlen)1) == 0;
    dtpool_("DELTET/EB", &found, &n, hwd, (ftnlen)9, (ftnlen)32);
    ok = ok && found && n >= 1 && s_cmp(hwd, "N", rtrim_(hwd, (ftnlen)32), (
	    ftnlen)1) == 0;
    dtpool_("DELTET/M", &found, &n, hwd, (ftnlen)8, (ftnlen)32);
    ok = ok && found && n >= 1 && s_cmp(hwd, "N", rtrim_(hwd, (ftnlen)32), (
	    ftnlen)1) == 0;
    dtpool_("DELTET/DELTA_AT", &found, &n, hwd, (ftnlen)15, (ftnlen)32);
    ok = ok && found && n >= 2 && s_cmp(hwd, "N", rtrim_(hwd, (ftnlen)32), (
	    ftnlen)1) == 0;
    if (! ok) {

/*        Well, one or more of the LSK parameters wasn't found. Look for */
/*        LSK keyword; it would be in the POOL if the setup file was an */
/*        old fashion setup. */

	gcpool_("LEAPSECONDS_FILE", &c__1, &c__1, &n, lskfn, &found, (ftnlen)
		16, (ftnlen)256);
	if (found) {
	    if (exists_(lskfn, (ftnlen)256)) {
		furnsh_(lskfn, (ftnlen)256);
	    } else {
		setmsg_("LSK file '#' provided in the setup file using '#' k"
			"eyworddoesn't exist.", (ftnlen)71);
		errch_("#", lskfn, (ftnlen)1, (ftnlen)256);
		errch_("#", "LEAPSECONDS_FILE", (ftnlen)1, (ftnlen)16);
		sigerr_("SPICE(LSKDOESNTEXIST)", (ftnlen)21);
	    }
	} else {
	    setmsg_("LSK file name wasn't provided in the setup file using '"
		    "#' keyword or KERNELS_TO_LOAD keyword. LSK data has also"
		    " not been provided as part of the setup file.", (ftnlen)
		    156);
	    errch_("#", "LEAPSECONDS_FILE", (ftnlen)1, (ftnlen)16);
	    sigerr_("SPICE(NOLSKFILENAME)", (ftnlen)20);
	}
    }

/*     SCLK file may have been loaded already, but for backward */
/*     compatibility we will also look for SCLK keyword and will try to */
/*     load SCLK provided in it. Note that SCLK file is needed only if */
/*     input/output system is SCLK or if input/output type is LST, ERT, */
/*     ETT, or and LT (these require position computation, which may */
/*     include CK calls somewhere in the chain.) */

    if (s_cmp(fromts, "SCLK", (ftnlen)32, (ftnlen)4) == 0 || s_cmp(tots, 
	    "SCLK", (ftnlen)32, (ftnlen)4) == 0 || s_cmp(fromts, "LST", (
	    ftnlen)32, (ftnlen)3) == 0 || s_cmp(tots, "LST", (ftnlen)32, (
	    ftnlen)3) == 0 || s_cmp(fromtt, "ERT", (ftnlen)32, (ftnlen)3) == 
	    0 || s_cmp(tott, "ERT", (ftnlen)32, (ftnlen)3) == 0 || s_cmp(
	    fromtt, "ETT", (ftnlen)32, (ftnlen)3) == 0 || s_cmp(tott, "ETT", (
	    ftnlen)32, (ftnlen)3) == 0 || s_cmp(fromtt, "LT", (ftnlen)32, (
	    ftnlen)2) == 0 || s_cmp(tott, "LT", (ftnlen)32, (ftnlen)2) == 0) {

/*        No attempt to check for already loaded SCLK constants -- too */
/*        much hassle. Simply get SCLK name from the POOL and try to */
/*        load the file provided in it (if exists.) */

	gcpool_("SCLK_FILE", &c__1, &c__1, &n, sclkfn, &found, (ftnlen)9, (
		ftnlen)256);
	if (found) {
	    if (exists_(sclkfn, (ftnlen)256)) {
		furnsh_(sclkfn, (ftnlen)256);
	    } else {
		setmsg_("SCLK file '#' provided in the setup file using '#' "
			"keyworddoesn't exist.", (ftnlen)72);
		errch_("#", sclkfn, (ftnlen)1, (ftnlen)256);
		errch_("#", "SCLK_FILE", (ftnlen)1, (ftnlen)9);
		sigerr_("SPICE(SCLKDOESNTEXIST)", (ftnlen)22);
	    }
	}
    }

/*     Same deal with SPK, CK, PCK and Frame files -- they may have been */
/*     loaded already, but we need to be backward compatible. therefore, */
/*     if on of the time types is LST, LSN, ERT, ETT or LT we will try */
/*     to look for appropriate, old-style-setup keywords. */

    if (s_cmp(fromts, "LST", (ftnlen)32, (ftnlen)3) == 0 || s_cmp(tots, "LST",
	     (ftnlen)32, (ftnlen)3) == 0 || s_cmp(fromtt, "ERT", (ftnlen)32, (
	    ftnlen)3) == 0 || s_cmp(tott, "ERT", (ftnlen)32, (ftnlen)3) == 0 
	    || s_cmp(fromtt, "ETT", (ftnlen)32, (ftnlen)3) == 0 || s_cmp(tott,
	     "ETT", (ftnlen)32, (ftnlen)3) == 0 || s_cmp(fromtt, "LT", (
	    ftnlen)32, (ftnlen)2) == 0 || s_cmp(tott, "LT", (ftnlen)32, (
	    ftnlen)2) == 0) {

/*        Look for and load SPKs, if any are provided through the SPK */
/*        keyword. */

	dtpool_("SPK_FILES", &found, &n, hwd, (ftnlen)9, (ftnlen)32);
	if (found && n >= 1 && *(unsigned char *)hwd == 'C') {
	    i__1 = n;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		gcpool_("SPK_FILES", &i__, &c__1, &j, spkfn, &found, (ftnlen)
			9, (ftnlen)256);
		if (exists_(spkfn, (ftnlen)256)) {
		    furnsh_(spkfn, (ftnlen)256);
		} else {
		    setmsg_("SPK file '#' provided in the setup file using '"
			    "#' keyword doesn't exist.", (ftnlen)72);
		    errch_("#", spkfn, (ftnlen)1, (ftnlen)256);
		    errch_("#", "SPK_FILES", (ftnlen)1, (ftnlen)9);
		    sigerr_("SPICE(SPKDOESNTEXIST)", (ftnlen)21);
		}
	    }
	}

/*        Look for and load CKs, if any are provided through the CK */
/*        keyword. */

	dtpool_("CK_FILES", &found, &n, hwd, (ftnlen)8, (ftnlen)32);
	if (found && n >= 1 && *(unsigned char *)hwd == 'C') {
	    i__1 = n;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		gcpool_("CK_FILES", &i__, &c__1, &j, ckfn, &found, (ftnlen)8, 
			(ftnlen)256);
		if (exists_(ckfn, (ftnlen)256)) {
		    furnsh_(ckfn, (ftnlen)256);
		} else {
		    setmsg_("CK file '#' provided in the setup file using '#"
			    "' keyword doesn't exist.", (ftnlen)71);
		    errch_("#", ckfn, (ftnlen)1, (ftnlen)256);
		    errch_("#", "CK_FILES", (ftnlen)1, (ftnlen)8);
		    sigerr_("SPICE(CKDOESNTEXIST)", (ftnlen)20);
		}
	    }
	}

/*        Look for and load FKs, if any are provided through the FK */
/*        keyword. */

	dtpool_("FRAMES_FILE", &found, &n, hwd, (ftnlen)11, (ftnlen)32);
	if (found && n >= 1 && *(unsigned char *)hwd == 'C') {
	    i__1 = n;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		gcpool_("FRAMES_FILE", &i__, &c__1, &j, frafn, &found, (
			ftnlen)11, (ftnlen)256);
		if (exists_(frafn, (ftnlen)256)) {
		    furnsh_(frafn, (ftnlen)256);
		} else {
		    setmsg_("FK file '#' provided in the setup file using '#"
			    "' keyword doesn't exist.", (ftnlen)71);
		    errch_("#", frafn, (ftnlen)1, (ftnlen)256);
		    errch_("#", "FRAMES_FILE", (ftnlen)1, (ftnlen)11);
		    sigerr_("SPICE(FKDOESNTEXIST)", (ftnlen)20);
		}
	    }
	}

/*        PCK at last. We could have tried to check for whether PCK */
/*        stuff is already in the POOL, but it's too much hassle. Simply */
/*        get PCK name from the POOL and try to load the file provided */
/*        in it (if exists.) */


/*        Look for and load FKs, if any are provided through the FK */
/*        keyword. */

	dtpool_("PCK_FILE", &found, &n, hwd, (ftnlen)8, (ftnlen)32);
	if (found && n >= 1 && *(unsigned char *)hwd == 'C') {
	    i__1 = n;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		gcpool_("PCK_FILE", &i__, &c__1, &j, pckfn, &found, (ftnlen)8,
			 (ftnlen)256);
		if (exists_(pckfn, (ftnlen)256)) {
		    furnsh_(pckfn, (ftnlen)256);
		} else {
		    setmsg_("PCK file '#' provided in the setup file using '"
			    "#' keyword doesn't exist.", (ftnlen)72);
		    errch_("#", pckfn, (ftnlen)1, (ftnlen)256);
		    errch_("#", "PCK_FILE", (ftnlen)1, (ftnlen)8);
		    sigerr_("SPICE(PCKDOESNTEXIST)", (ftnlen)21);
		}
	    }
	}
    }

/*     If trace is ON, report on loaded kernels. */

    spekst_(hword, (ftnlen)32);
    if (s_cmp(hword, "ON", (ftnlen)2, (ftnlen)2) == 0) {

/*        Display list of names provided with command line option or */
/*        through the environment variable. */

	if (envstp) {
	    s_copy(hword, "CHRONOS_SETUP_FILE list: ", (ftnlen)32, (ftnlen)25)
		    ;
	} else {
	    s_copy(hword, "            -SETUP list: ", (ftnlen)32, (ftnlen)25)
		    ;
	}
/* Writing concatenation */
	i__3[0] = 32, a__1[0] = hword;
	i__3[1] = rtrim_(setpfn, (ftnlen)256), a__1[1] = setpfn;
	s_cat(ch__2, a__1, i__3, &c__2, (ftnlen)288);
	speak_(ch__2, rtrim_(setpfn, (ftnlen)256) + 32);
	speak_(" ", (ftnlen)1);
	speak_("   Loaded SPICE kernels: ", (ftnlen)25);

/*        Display the list of loaded text kernels. */

	ktotal_("TEXT", &count, (ftnlen)4);
	if (count == 0) {
	    speak_("                   TEXT: (no files loaded) ", (ftnlen)43);
	} else {
	    i__1 = count;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		kdata_(&i__, "TEXT", file, ftyp, fsrc, &hndl, &found, (ftnlen)
			4, (ftnlen)256, (ftnlen)32, (ftnlen)256);
		if (i__ == 1) {
/* Writing concatenation */
		    i__3[0] = 25, a__1[0] = "                   TEXT: ";
		    i__3[1] = 256, a__1[1] = file;
		    s_cat(ch__3, a__1, i__3, &c__2, (ftnlen)281);
		    speak_(ch__3, (ftnlen)281);
		} else {
/* Writing concatenation */
		    i__3[0] = 25, a__1[0] = "                         ";
		    i__3[1] = 256, a__1[1] = file;
		    s_cat(ch__3, a__1, i__3, &c__2, (ftnlen)281);
		    speak_(ch__3, (ftnlen)281);
		}
	    }
	}

/*        Display the list of loaded SPKs. */

	ktotal_("SPK", &count, (ftnlen)3);
	if (count == 0) {
	    speak_("                    SPK: (no files loaded) ", (ftnlen)43);
	} else {
	    i__1 = count;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		kdata_(&i__, "SPK", file, ftyp, fsrc, &hndl, &found, (ftnlen)
			3, (ftnlen)256, (ftnlen)32, (ftnlen)256);
		if (i__ == 1) {
/* Writing concatenation */
		    i__3[0] = 25, a__1[0] = "                    SPK: ";
		    i__3[1] = 256, a__1[1] = file;
		    s_cat(ch__3, a__1, i__3, &c__2, (ftnlen)281);
		    speak_(ch__3, (ftnlen)281);
		} else {
/* Writing concatenation */
		    i__3[0] = 25, a__1[0] = "                         ";
		    i__3[1] = 256, a__1[1] = file;
		    s_cat(ch__3, a__1, i__3, &c__2, (ftnlen)281);
		    speak_(ch__3, (ftnlen)281);
		}
	    }
	}

/*        Display the list of loaded CKs. */

	ktotal_("CK", &count, (ftnlen)2);
	if (count == 0) {
	    speak_("                     CK: (no files loaded) ", (ftnlen)43);
	} else {
	    i__1 = count;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		kdata_(&i__, "CK", file, ftyp, fsrc, &hndl, &found, (ftnlen)2,
			 (ftnlen)256, (ftnlen)32, (ftnlen)256);
		if (i__ == 1) {
/* Writing concatenation */
		    i__3[0] = 25, a__1[0] = "                     CK: ";
		    i__3[1] = 256, a__1[1] = file;
		    s_cat(ch__3, a__1, i__3, &c__2, (ftnlen)281);
		    speak_(ch__3, (ftnlen)281);
		} else {
/* Writing concatenation */
		    i__3[0] = 25, a__1[0] = "                         ";
		    i__3[1] = 256, a__1[1] = file;
		    s_cat(ch__3, a__1, i__3, &c__2, (ftnlen)281);
		    speak_(ch__3, (ftnlen)281);
		}
	    }
	}

/*        Display the list of loaded binary PCKs. */

	ktotal_("PCK", &count, (ftnlen)3);
	if (count == 0) {
	    speak_("             Binary PCK: (no files loaded) ", (ftnlen)43);
	} else {
	    i__1 = count;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		kdata_(&i__, "PCK", file, ftyp, fsrc, &hndl, &found, (ftnlen)
			3, (ftnlen)256, (ftnlen)32, (ftnlen)256);
		if (i__ == 1) {
/* Writing concatenation */
		    i__3[0] = 25, a__1[0] = "             Binary PCK: ";
		    i__3[1] = 256, a__1[1] = file;
		    s_cat(ch__3, a__1, i__3, &c__2, (ftnlen)281);
		    speak_(ch__3, (ftnlen)281);
		} else {
/* Writing concatenation */
		    i__3[0] = 25, a__1[0] = "                         ";
		    i__3[1] = 256, a__1[1] = file;
		    s_cat(ch__3, a__1, i__3, &c__2, (ftnlen)281);
		    speak_(ch__3, (ftnlen)281);
		}
	    }
	}
    }

/*     Re-assemble command line for CRONOS input. */

    intstr_(&solzer, solids, (ftnlen)32);
/* Writing concatenation */
    i__7[0] = 6, a__3[0] = "-FROM ";
    i__7[1] = rtrim_(fromts, (ftnlen)32), a__3[1] = fromts;
    i__7[2] = 1, a__3[2] = " ";
    i__7[3] = 9, a__3[3] = "-FROMTYPE";
    i__7[4] = 1, a__3[4] = " ";
    i__7[5] = rtrim_(fromtt, (ftnlen)32), a__3[5] = fromtt;
    i__7[6] = 1, a__3[6] = " ";
    i__7[7] = 3, a__3[7] = "-TO";
    i__7[8] = 1, a__3[8] = " ";
    i__7[9] = rtrim_(tots, (ftnlen)32), a__3[9] = tots;
    i__7[10] = 1, a__3[10] = " ";
    i__7[11] = 7, a__3[11] = "-TOTYPE";
    i__7[12] = 1, a__3[12] = " ";
    i__7[13] = rtrim_(tott, (ftnlen)32), a__3[13] = tott;
    i__7[14] = 1, a__3[14] = " ";
    i__7[15] = 7, a__3[15] = "-FORMAT";
    i__7[16] = 1, a__3[16] = " ";
    i__7[17] = rtrim_(tofrmt, (ftnlen)256), a__3[17] = tofrmt;
    i__7[18] = 1, a__3[18] = " ";
    i__7[19] = 3, a__3[19] = "-SC";
    i__7[20] = 1, a__3[20] = " ";
    i__7[21] = rtrim_(scstr, (ftnlen)32), a__3[21] = scstr;
    i__7[22] = 1, a__3[22] = " ";
    i__7[23] = 7, a__3[23] = "-CENTER";
    i__7[24] = 1, a__3[24] = " ";
    i__7[25] = rtrim_(censtr, (ftnlen)32), a__3[25] = censtr;
    i__7[26] = 1, a__3[26] = " ";
    i__7[27] = 12, a__3[27] = "-LANDINGTIME";
    i__7[28] = 1, a__3[28] = " ";
    i__7[29] = rtrim_(lsttim, (ftnlen)32), a__3[29] = lsttim;
    i__7[30] = 1, a__3[30] = " ";
    i__7[31] = 10, a__3[31] = "-SOL1INDEX";
    i__7[32] = 1, a__3[32] = " ";
    i__7[33] = rtrim_(solids, (ftnlen)32), a__3[33] = solids;
    s_cat(cmdlin, a__3, i__7, &c__34, (ftnlen)2048);

/*     Get input times from STDIN if we are in the batch mode. */

    if (batmod) {
	readln_(&stdin, intime, &done, (ftnlen)256);
    } else {
	done = FALSE_;
    }

/*     Loop until no more input times (just once for command line mode */
/*     :). */

    while(! done) {

/*        Call CRONOS routine. */

	cronos_(cmdlin, &c__1, intime, outtim, (ftnlen)2048, (ftnlen)256, (
		ftnlen)256);

/*        Display the result based on requested tracing and labeling. */

/* Writing concatenation */
	i__8[0] = 64, a__4[0] = intime;
	i__8[1] = 1, a__4[1] = "(";
	i__8[2] = rtrim_(fromts, (ftnlen)32), a__4[2] = fromts;
	i__8[3] = 1, a__4[3] = "/";
	i__8[4] = rtrim_(fromtt, (ftnlen)32), a__4[4] = fromtt;
	i__8[5] = 1, a__4[5] = ")";
	s_cat(ch__4, a__4, i__8, &c__6, (ftnlen)131);
	speak_(ch__4, rtrim_(fromts, (ftnlen)32) + 66 + rtrim_(fromtt, (
		ftnlen)32) + 1);
	if (nolabl) {
	    tostdo_(outtim, (ftnlen)64);
	} else {
/* Writing concatenation */
	    i__8[0] = 64, a__4[0] = outtim;
	    i__8[1] = 1, a__4[1] = "(";
	    i__8[2] = rtrim_(tots, (ftnlen)32), a__4[2] = tots;
	    i__8[3] = 1, a__4[3] = "/";
	    i__8[4] = rtrim_(tott, (ftnlen)32), a__4[4] = tott;
	    i__8[5] = 1, a__4[5] = ")";
	    s_cat(ch__4, a__4, i__8, &c__6, (ftnlen)131);
	    tostdo_(ch__4, rtrim_(tots, (ftnlen)32) + 66 + rtrim_(tott, (
		    ftnlen)32) + 1);
	}
	speak_(" ", (ftnlen)1);

/*        Get next input time from STDIN if we are in the batch mode. */

	if (batmod) {
	    readln_(&stdin, intime, &done, (ftnlen)256);
	} else {
	    done = TRUE_;
	}

/*        End of processing input times. */

    }

/*     All done. */

    chkout_("CHRONOS", (ftnlen)7);
    byebye_("SUCCESS", (ftnlen)7);
    return 0;
} /* MAIN__ */

/* Main program alias */ int chronos_ () { MAIN__ (); return 0; }
