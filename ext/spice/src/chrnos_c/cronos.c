/* cronos.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__19 = 19;
static integer c__4 = 4;
static integer c__10 = 10;
static integer c__2 = 2;
static integer c__1 = 1;
static integer c__0 = 0;
static integer c__3 = 3;
static integer c__14 = 14;
static integer c__399 = 399;
static doublereal c_b400 = 60.;
static doublereal c_b511 = 1.;

/* $Procedure      CRONOS ( SPICE Time Conversion Umbrella ) */
/* Subroutine */ int cronos_(char *cmdlin, integer *ntimes, char *inptim, 
	char *outtim, ftnlen cmdlin_len, ftnlen inptim_len, ftnlen outtim_len)
{
    /* Initialized data */

    static integer nloops = 0;
    static logical first = TRUE_;

    /* System generated locals */
    address a__1[4], a__2[2], a__3[3];
    integer i__1, i__2[4], i__3[2], i__4, i__5, i__6[3];
    doublereal d__1;
    char ch__1[87], ch__2[278], ch__3[54];

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer);
    double sqrt(doublereal);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);
    double pow_di(doublereal *, integer *);
    integer i_dnnt(doublereal *);
    double d_mod(doublereal *, doublereal *);

    /* Local variables */
    static integer scid;
    static char line[256];
    static doublereal tvec[10];
    static logical mods;
    static doublereal elts[8];
    static char type__[5], tots[32], tott[32];
    extern /* Subroutine */ int sce2c_(integer *, doublereal *, doublereal *),
	     sct2e_(integer *, doublereal *, doublereal *), sce2t_(integer *, 
	    doublereal *, doublereal *);
    extern integer zzbodbry_(integer *);
    static integer i__, j, n;
    static doublereal r__, t;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static char hline[256];
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen), 
	    errch_(char *, char *, ftnlen, ftnlen), speak_(char *, ftnlen), 
	    repmc_(char *, char *, char *, char *, ftnlen, ftnlen, ftnlen, 
	    ftnlen), repmd_(char *, char *, doublereal *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen);
    static doublereal lsecs;
    extern /* Subroutine */ int ltime_(doublereal *, integer *, char *, 
	    integer *, doublereal *, doublereal *, ftnlen);
    static logical found;
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen), dpfmt_(doublereal *, char *, char *, 
	    ftnlen, ftnlen);
    static integer ntvec;
    static doublereal state[6];
    static char hword[32];
    static integer count;
    static char error[256];
    static doublereal sungm, lstet;
    extern integer rtrim_(char *, ftnlen);
    static char scstr[32];
    extern /* Subroutine */ int spkez_(integer *, doublereal *, char *, char *
	    , integer *, doublereal *, doublereal *, ftnlen, ftnlen);
    static doublereal etout;
    extern doublereal twopi_(void);
    static char types[32*10];
    extern /* Subroutine */ int bodc2n_(integer *, char *, logical *, ftnlen),
	     et2utc_(doublereal *, char *, integer *, char *, ftnlen, ftnlen),
	     hx2int_(char *, integer *, logical *, char *, ftnlen, ftnlen), 
	    int2hx_(integer *, char *, integer *, ftnlen), et2lst_(doublereal 
	    *, integer *, doublereal *, char *, integer *, integer *, integer 
	    *, char *, char *, ftnlen, ftnlen, ftnlen), str2et_(char *, 
	    doublereal *, ftnlen), scdecd_(integer *, doublereal *, char *, 
	    ftnlen);
    static integer sc;
    static logical clflag[19];
    static doublereal et;
    static integer hr, mn;
    static logical ok;
    extern doublereal ls_(integer *, doublereal *, char *, ftnlen);
    static doublereal lt;
    extern /* Subroutine */ int tcheck_(doublereal *, char *, logical *, char 
	    *, logical *, char *, ftnlen, ftnlen, ftnlen);
    static integer frcode;
    extern /* Subroutine */ int scencd_(integer *, char *, doublereal *, 
	    ftnlen), cidfrm_(integer *, integer *, char *, logical *, ftnlen);
    static char fields[32*10];
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    static char cmdsav[2048];
    static doublereal midnet;
    static char cmdtmp[2048], modify[8*5], clvals[256*19], intime[256];
    extern logical return_(void);
    static char unprsd[2048], outime[256], tofrmt[256], tofrmh[256], clkeys[
	    32*19], curkey[32], deftyp[32*4], fromts[32], fromtt[32], lsttim[
	    32], systms[32*4], censtr[32], solids[32], fmtpic[64*4*10];
    static doublereal ettemp, pmmotn, sclkdp, scrate;
    static integer bodyid, frsidx, frtidx;
    static doublereal hdp;
    static integer solday, solzer, tosidx, totidx;
    static doublereal lat;
    static char hwd[32];
    static logical bad, fmtted[40]	/* was [4][10] */;
    extern doublereal dpr_(void), spd_(void);
    static logical systyp[40]	/* was [4][10] */;
    static doublereal lon;
    static logical yabbrv;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), crcnst_(char *, char *, char *, 
	    logical *, logical *, char *, char *, ftnlen, ftnlen, ftnlen, 
	    ftnlen, ftnlen), parcml_(char *, integer *, char *, logical *, 
	    char *, logical *, char *, ftnlen, ftnlen, ftnlen, ftnlen), 
	    tpictr_(char *, char *, logical *, char *, ftnlen, ftnlen, ftnlen)
	    ;
    extern integer pos_(char *, char *, integer *, ftnlen, ftnlen);
    extern /* Subroutine */ int nparsi_(char *, integer *, char *, integer *, 
	    ftnlen, ftnlen), errint_(char *, integer *, ftnlen), reclat_(
	    doublereal *, doublereal *, doublereal *, doublereal *), gdpool_(
	    char *, integer *, integer *, integer *, doublereal *, logical *, 
	    ftnlen), dtpool_(char *, logical *, integer *, char *, ftnlen, 
	    ftnlen), bodvrd_(char *, char *, integer *, integer *, doublereal 
	    *, ftnlen, ftnlen), oscelt_(doublereal *, doublereal *, 
	    doublereal *, doublereal *), lstmid_(doublereal *, integer *, 
	    doublereal *, doublereal *, char *, integer *, doublereal *, 
	    ftnlen), spekst_(char *, ftnlen), tpartv_(char *, doublereal *, 
	    integer *, char *, char *, logical *, logical *, logical *, char *
	    , char *, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen), nparsd_(char *,
	     doublereal *, char *, integer *, ftnlen, ftnlen), lparse_(char *,
	     char *, integer *, integer *, char *, ftnlen, ftnlen, ftnlen), 
	    lparsm_(char *, char *, integer *, integer *, char *, ftnlen, 
	    ftnlen, ftnlen), intstr_(integer *, char *, ftnlen), nextwd_(char 
	    *, char *, char *, ftnlen, ftnlen, ftnlen), timout_(doublereal *, 
	    char *, char *, ftnlen, ftnlen);

/* $ Abstract */

/*     Convert time between various time systems and types supported by */
/*     lower level SPICE time conversion subsystems. */

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

/*     CHRONOS User's Guide */
/*     TIME */
/*     SCLK */

/* $ Keywords */

/*     TIME */
/*     SCLK */
/*     CONVERSION */

/* $ Declarations */
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

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     CMDLIN     I   Command line. */
/*     NTIMES     I   Number of input/output times. */
/*     INPTIM     I   Array of input times. */
/*     OUTTIM     O   Array of output times. */

/* $ Detailed_Input */

/*     CMDLIN     is the command line. See Particulars section for the */
/*                command line syntax details. */

/*     NTIMES     is the number of input/output times. Must be greater */
/*                than zero. */

/*     INPTIM     is an array of input times. All elements of this array */
/*                must be legitimate time tags for the input time */
/*                system/type as defined by the CMDLIN argument. No blank */
/*                elements are permitted. */

/* $ Detailed_Output */

/*     OUTTIM     is an array of output times. */

/* $ Parameters */

/*     Essential parameters defined in the include file: */

/*     LLNSIZ     is the maximum number of characters from the input */
/*                argument CMDLIN that will be accepted by this routine. */
/*                All characters past LLNSIZ are truncated. */

/*     LINSIZ     is the maximum number of characters that is taken */
/*                from each element of the INPTIM and is assigned to */
/*                each element of the OUTTIM. */

/* $ Exceptions */

/*     1) TBD */

/* $ Files */

/*     Enough SPICE data to do the requested conversion should be loaded */
/*     prior to calling this routine. */

/*     An LSK file should be loaded to support any time conversion. */

/*     An SCLK file for the specified spacecraft should be loaded to */
/*     support any time conversion involving SCLK time system. */

/*     A set of SPK, PCK, and possibly CK and FK files sufficient to */
/*     compute the specified spacecraft position with respect the */
/*     specified central body and the Sun should be loaded to support */
/*     any conversions involving LSK time system. */

/*     A set of SPK, PCK, and possibly CK and FK files sufficient to */
/*     compute the specified spacecraft position with respect the Earth */
/*     should be loaded to support any conversions involving ERT, ETT, */
/*     and LT time types. */

/* $ Particulars */

/*     This section of the header provides a brief overview of the */
/*     routine's interface. Refer to the CHRONOS User's Guide for the */
/*     complete specification */

/*     The CMDLIN input argument, specifying the time systems/types from */
/*     which and to which the conversion should be done along with a set */
/*     of the other parameters needed to perform the converison, */
/*     incorporates these specification in a single line, similar to a */
/*     command line for an executable. The command line has the following */
/*     syntax: */

/*        'switch <value> switch <value> switch <value> ...' */

/*     The following command line swtches are recognized and used by */
/*     the routine: */

/*        switch        optional?  value */
/*        ------------  ---------  -------------------------------- */
/*        -FROM           no       "from" time system */
/*        -FROMTYPE       yes      "from" time type */
/*        -TO             no       "to" time system */
/*        -TOTYPE         yes      "to" time type */
/*        -FORMAT         yes      output time format picture */
/*        -SC             yes(*)   NAIF ID of the spacecraft */
/*        -CENTER         yes(*)   NAIF ID of the cental body */
/*        -LANDINGTIME    yes(*)   UTC time of the landing */
/*        -SOL1INDEX      yes(*)   index of the first SOL */

/*     (*) these inputs are required for some input/output time systems/ */
/*         types and aren't required for the others. */

/*     The following command line switches are recongnized but ignored */
/*     by the routine: */

/*        switch */
/*        --------- */
/*        -SETUP */
/*        -TIME */
/*        -BATCH */
/*        -NOLABEL */
/*        -TRACE */
/*        -HELP */
/*        -H */
/*        -USAGE */
/*        -U */
/*        -TEMPLATE */

/*     The recognition of these switches is implemented to provide */
/*     compatibilty between this routine's and the CHRONOS utility */
/*     program interfaces, such that a command line setup for the */
/*     CHRONOS program could be used 'as is' with this routine. */

/*     Any other tokens starting with '-' are not recognized and */
/*     interpreted as part of the value of the preceding command line */
/*     switch. */

/*     The case of command line switches is insignificant. */

/*     This routine supports converison between the following time */
/*     systems, that can be specified after the -FROM and -TO command */
/*     line switches: */

/*        System   Description */
/*        ------   --------------------------------- */
/*        UTC      Universal Time Coordinated */
/*        ET       Ephemeris Time */
/*        SCLK     Spacecraft On-board Clock Time */
/*        LST      Local True Solar Time */

/*     For each of them it suppports the following input types */
/*     (specified with the -FROMTYPE switch): */

/*        System   Input Types */
/*        ------   --------------------------------- */
/*        UTC      SCET, ERT, ETT */
/*        ET       SCET, ERT, ETT, SECONDS */
/*        SCLK     SCLK, HEX, TICKS */
/*        LST      LST */

/*     and the following output types (specified with the -TOTYPE */
/*     switch): */

/*        System   Input Types */
/*        ------   --------------------------------- */
/*        UTC      SCET, ERT, ETT, LT */
/*        ET       SCET, ERT, ETT, SECONDS, LT */
/*        SCLK     SCLK, HEX, TICKS */
/*        LST      LST, LSUN */

/*     where the type identifiers correspond to: */

/*        Type     Description */
/*        ------   --------------------------------- */
/*        SCET     Spacecraft Event Time */
/*        ERT      Earth Received Time */
/*        ETT      Earth Transmit Time */
/*        LT       One-way Light Time */
/*        SECONDS  Ephemeris Seconds past epoch J2000 */
/*        SCLK     String Spacecraft Clock */
/*        HEX      HEX Spacecraft Clock */
/*        TICKS    Spacecraft Clock Ticks (or encoded SPICE SCLK) */
/*        LST      Local Solar Time */
/*        LSUN     Longitude of the Sun */

/*     The input and output type specifications are optional. If either */
/*     of of the types is absent or set to blank, the default value */
/*     specified in this table is assumed: */

/*        System   Default Input Type   Default Output Type */
/*        ------   ------------------   ------------------- */
/*        UTC      SCET                 SCET */
/*        ET       SCET                 SCET */
/*        SCLK     SCLK                 SCLK */
/*        LST      LST                  LST */

/*     The -FORMAT command line switch is used to modify, if applicable, */
/*     the format of the output time tags. This switch is optional and */
/*     is applicable only for the following output time system/type */
/*     combinations: */

/*        System   Type      Default Format */
/*        ------   -------   -------------------------------- */
/*        UTC      SCET      YYYY-MM-DD HR:MN:SC.### ::RND */
/*        UTC      ERT       YYYY-MM-DD HR:MN:SC.### ::RND */
/*        UTC      ETT       YYYY-MM-DD HR:MN:SC.### ::RND */
/*        UTC      LT        xxxxxxxxxxxx.xxx */
/*        ET       SCET      YYYY-MM-DD, HR:MN:SC.### ::TDB ::RND */
/*        ET       ERT       YYYY-MM-DD, HR:MN:SC.### ::TDB ::RND */
/*        ET       ETT       YYYY-MM-DD, HR:MN:SC.### ::TDB ::RND */
/*        ET       LT        xxxxxxxxxxxx.xxx */
/*        ET       SECONDS   xxxxxxxxxxxxxxx.xxx */
/*        SCLK     TICKS     xxxxxxxxxxxxxxxx */
/*        LST      LSUN      xxxxxx.xxx */

/*     As with the types, if this switch is not specified or its value */
/*     is blank, then the default value, given in the right column of */
/*     the table above, is used. */

/*     The spacecraft ID, specified with the -SC command line switch, is */
/*     required if the input, output, or both time system is SCLK and if */
/*     the input, output, or both time type is LST, ERT, ETT, or LT. */

/*     The central body ID, specified with the -CENTER command line */
/*     switch, is required if the input, output, or both time system is */
/*     LST. */

/*     The UTC time of the landing, specified with the -LANDINGTIME */
/*     switch, and the index of the first SOL, specified with the */
/*     -SOL1INDEX switch, are required if the input or output */
/*     system/type pair is LST/LST. */

/* $ Examples */

/*     These examples assume that the complete set of kernel files */
/*     required to support time conversions for MER-A at the landing */
/*     site EP55A2 has been loaded prior to calling CRONOS routine: */

/*     1) Converting from UTC to ET (note it's sufficient to specify */
/*        only -from and -to command line options for this conversion): */

/*           CMDLIN    = '-from UTC -to ET' */
/*           INTIME(1) = '2004-01-05 01:55:46.175' */
/*           CALL CRONOS( CMDLIN, 1, INTIME, OUTIME ) */

/*        returned value of OUTIME is: */

/*           '2004-01-05, 01:56:50.359' */

/*     2) Converting from SCLK to UTC (note that the s/c ID must be */
/*        specified for this conversion): */

/*           CMDLIN    = '-from SCLK -to UTC -sc -253' */
/*           INTIME(1) = '0126539810.092' */
/*           CALL CRONOS( CMDLIN, 1, INTIME, OUTIME ) */

/*        returned value of OUTIME is: */

/*           '2004-01-05 01:55:46.175' */

/*     3) Converting from LST to UTC (note that the IDs for the s/c */
/*        and Mars and landing time and first SOL index must be */
/*        specified for this conversion as for any other conversion */
/*        involving LST): */

/*           CMDLIN    = '-from LST ' */
/*          .//          '-to UTC ' */
/*          .//          '-sc -253 ' */
/*          .//          '-center 499 ' */
/*          .//          '-landingtime 2004-01-04 12:00 ' */
/*          .//          '-sol1index 1 ' */
/*           INTIME(1) = 'SOL 2 11:11:11' */
/*           CALL CRONOS( CMDLIN, 1, INTIME, OUTIME ) */

/*        returned value of OUTIME is: */

/*           '2004-01-05 01:55:46.175' */

/*     4) Computing one-way light time for a given UTC epoch (note */
/*        that one-way light time expressed in seconds is returned */
/*        as a string): */

/*           CMDLIN    = '-from UTC -to UTC -totype LT -sc -253' */
/*           INTIME(1) = '2004-01-05 01:55:46.175' */
/*           CALL CRONOS( CMDLIN, 1, INTIME, OUTIME ) */

/*        returned value of OUTIME is: */

/*           '572.658' */

/*     5) Computing Earth Receive Time (ERT) in Pacific Standard */
/*        Time (PST) for a given LST (note that the format string */
/*        modified ::UTC-8 is used to tell the routine to output */
/*        time as PST): */

/*           CMDLIN    = '-from LST ' */
/*          .//          '-to UTC ' */
/*          .//          '-totype ERT ' */
/*          .//          '-sc -253 ' */
/*          .//          '-center 499 ' */
/*          .//          '-landingtime 2004-01-04 12:00 ' */
/*          .//          '-sol1index 1 ' */
/*          .//          '-format YYYY-MM-DD HR:MN:SC.### ::UTC-8 PST ' */
/*           INTIME(1) = 'SOL 2 11:11:11' */
/*           CALL CRONOS( CMDLIN, 1, INTIME, OUTIME ) */

/*        returned value of OUTIME is: */

/*           '2004-01-04 18:05:18.833 PST' */

/*     6) Re-using CHRONOS executable command line (note that the */
/*        command line switches -setup, -time, -nolabel and the values */
/*        that follows them are ignored by the routine): */

/*           CMDLIN    = '-setup chronos.setup ' */
/*          .//          '-from UTC ' */
/*          .//          '-to ET ' */
/*          .//          '-time 2004-01-05 01:55:46.175 ' */
/*          .//          '-nolabel ' */
/*           INTIME(1) = '-time 2004-01-05 01:55:46.175' */
/*           CALL CRONOS( CMDLIN, 1, INTIME, OUTIME ) */

/*        returned value of OUTIME is: */

/*           '2004-01-05, 01:56:50.359' */

/* $ Restrictions */

/*     TBD. Enough SPICE data to do the conversion should be loaded */
/*     prior to calling this routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.3.0, 28-OCT-2011 (BVS) */

/*        Moved PARCML to support. Updated its calling sequence. */

/* -    SPICELIB Version 1.2.0, 18-AUG-2006 (BVS) */

/*        Fixed the bug that caused incorrect computation of the orbital */
/*        period used in computing the local second length (the orbital */
/*        period formula had e**2 instead of e). */

/* -    SPICELIB Version 1.1.0, 07-OCT-2005 (BVS) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in subroutine calls (LSTMID and LTIME). Replaced BODVAR with */
/*        BODVRD. */

/* -    SPICELIB Version 1.0.1, 17-MAR-2004 (BVS) */

/*        Planet ID is now computed from barycenter ID returned by */
/*        ZZBODBRY rather than one determined by division by a hundred. */

/* -    SPICELIB Version 1.0.0, 13-DEC-2001 (BVS) */

/*        The guts are from CHRONOS Ver 1.2.3 (25-NOV-2001) */

/* -& */
/* $ Index_Entries */

/*     convert between ET, UTC, SCLK and LST times */

/* -& */

/*     SPICELIB functions. */


/*     Parameters. */


/*     Variables. */


/*     Save all variables. */


/*     Number of iteration that have been done with each command line is */
/*     initially set to zero. */


/*     First call flag. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("CRONOS", (ftnlen)6);
    }

/*     The very first thing to check is whether the count of input */
/*     times make sense? */

    if (*ntimes <= 0) {
	setmsg_("The count of input times was less or equal to zero. This is"
		" a NO-NO.", (ftnlen)68);
	sigerr_("SPICE(BADTIMECOUNT)", (ftnlen)19);
	chkout_("CRONOS", (ftnlen)6);
	return 0;
    }

/*     If the command line haven't changed from the previous call, there */
/*     is no need to parse it. */

    if (s_cmp(cmdlin, cmdsav, cmdlin_len, (ftnlen)2048) != 0) {

/*        If this is the first call, get parameter arrays -- time */
/*        systems and types, format pictures and flags, and command line */
/*        keys. */

	if (first) {
	    crcnst_(systms, types, deftyp, systyp, fmtted, fmtpic, clkeys, (
		    ftnlen)32, (ftnlen)32, (ftnlen)32, (ftnlen)64, (ftnlen)32)
		    ;
	    first = FALSE_;
	}

/*        Re-assign command line to a temporary variable and parse it. */

	s_copy(cmdtmp, cmdlin, (ftnlen)2048, cmdlin_len);
	parcml_(cmdtmp, &c__19, clkeys, clflag, clvals, &found, unprsd, (
		ftnlen)2048, (ftnlen)32, (ftnlen)256, (ftnlen)2048);

/*        Are there any keys on the command line? If not, complain and */
/*        exit. */

	if (! found) {
	    setmsg_("No recognized tokens were found in the command line arg"
		    "ument passed into this routine.", (ftnlen)86);
	    sigerr_("SPICE(BLANKCOMMANDLINE)", (ftnlen)23);
	    chkout_("CRONOS", (ftnlen)6);
	    return 0;
	}

/*        Process command line. First, set all values to blanks. */

	s_copy(fromts, " ", (ftnlen)32, (ftnlen)1);
	s_copy(fromtt, " ", (ftnlen)32, (ftnlen)1);
	s_copy(tots, " ", (ftnlen)32, (ftnlen)1);
	s_copy(tott, " ", (ftnlen)32, (ftnlen)1);
	s_copy(tofrmt, " ", (ftnlen)256, (ftnlen)1);
	s_copy(scstr, " ", (ftnlen)32, (ftnlen)1);
	s_copy(censtr, " ", (ftnlen)32, (ftnlen)1);
	s_copy(lsttim, " ", (ftnlen)32, (ftnlen)1);
	s_copy(solids, " ", (ftnlen)32, (ftnlen)1);
	for (i__ = 1; i__ <= 19; ++i__) {
	    if (clflag[(i__1 = i__ - 1) < 19 && 0 <= i__1 ? i__1 : s_rnge(
		    "clflag", i__1, "cronos_", (ftnlen)614)]) {
		s_copy(curkey, clkeys + (((i__1 = i__ - 1) < 19 && 0 <= i__1 ?
			 i__1 : s_rnge("clkeys", i__1, "cronos_", (ftnlen)616)
			) << 5), (ftnlen)32, (ftnlen)32);
		if (s_cmp(curkey, "-FROM", (ftnlen)32, (ftnlen)5) == 0) {

/*                 It's "from" time system. */

		    s_copy(fromts, clvals + (((i__1 = i__ - 1) < 19 && 0 <= 
			    i__1 ? i__1 : s_rnge("clvals", i__1, "cronos_", (
			    ftnlen)622)) << 8), (ftnlen)32, (ftnlen)256);
		    ucase_(fromts, fromts, (ftnlen)32, (ftnlen)32);
		} else if (s_cmp(curkey, "-FROMTYPE", (ftnlen)32, (ftnlen)9) 
			== 0) {

/*                 It's "from" time type. */

		    s_copy(fromtt, clvals + (((i__1 = i__ - 1) < 19 && 0 <= 
			    i__1 ? i__1 : s_rnge("clvals", i__1, "cronos_", (
			    ftnlen)629)) << 8), (ftnlen)32, (ftnlen)256);
		    ucase_(fromtt, fromtt, (ftnlen)32, (ftnlen)32);
		} else if (s_cmp(curkey, "-TO", (ftnlen)32, (ftnlen)3) == 0) {

/*                 It's "to" time system. */

		    s_copy(tots, clvals + (((i__1 = i__ - 1) < 19 && 0 <= 
			    i__1 ? i__1 : s_rnge("clvals", i__1, "cronos_", (
			    ftnlen)636)) << 8), (ftnlen)32, (ftnlen)256);
		    ucase_(tots, tots, (ftnlen)32, (ftnlen)32);
		} else if (s_cmp(curkey, "-TOTYPE", (ftnlen)32, (ftnlen)7) == 
			0) {

/*                 It's "to" time type. */

		    s_copy(tott, clvals + (((i__1 = i__ - 1) < 19 && 0 <= 
			    i__1 ? i__1 : s_rnge("clvals", i__1, "cronos_", (
			    ftnlen)643)) << 8), (ftnlen)32, (ftnlen)256);
		    ucase_(tott, tott, (ftnlen)32, (ftnlen)32);
		} else if (s_cmp(curkey, "-FORMAT", (ftnlen)32, (ftnlen)7) == 
			0) {

/*                 It's "to" time format. */

		    s_copy(tofrmt, clvals + (((i__1 = i__ - 1) < 19 && 0 <= 
			    i__1 ? i__1 : s_rnge("clvals", i__1, "cronos_", (
			    ftnlen)650)) << 8), (ftnlen)256, (ftnlen)256);
		} else if (s_cmp(curkey, "-SC", (ftnlen)32, (ftnlen)3) == 0) {

/*                 It's spacecraft ID; save it in string. */

		    s_copy(scstr, clvals + (((i__1 = i__ - 1) < 19 && 0 <= 
			    i__1 ? i__1 : s_rnge("clvals", i__1, "cronos_", (
			    ftnlen)656)) << 8), (ftnlen)32, (ftnlen)256);
		} else if (s_cmp(curkey, "-CENTER", (ftnlen)32, (ftnlen)7) == 
			0) {

/*                 It's body ID; save it in string. */

		    s_copy(censtr, clvals + (((i__1 = i__ - 1) < 19 && 0 <= 
			    i__1 ? i__1 : s_rnge("clvals", i__1, "cronos_", (
			    ftnlen)662)) << 8), (ftnlen)32, (ftnlen)256);
		} else if (s_cmp(curkey, "-LANDINGTIME", (ftnlen)32, (ftnlen)
			12) == 0) {

/*                 It's landing time. */

		    s_copy(lsttim, clvals + (((i__1 = i__ - 1) < 19 && 0 <= 
			    i__1 ? i__1 : s_rnge("clvals", i__1, "cronos_", (
			    ftnlen)668)) << 8), (ftnlen)32, (ftnlen)256);
		} else if (s_cmp(curkey, "-SOL1INDEX", (ftnlen)32, (ftnlen)10)
			 == 0) {

/*                 It's SOL 1 index; save it in string. */

		    s_copy(solids, clvals + (((i__1 = i__ - 1) < 19 && 0 <= 
			    i__1 ? i__1 : s_rnge("clvals", i__1, "cronos_", (
			    ftnlen)674)) << 8), (ftnlen)32, (ftnlen)256);
		}
	    }
	}

/*        Done with parsing. Now, check whether the stuff that we have */
/*        got makes any sense. We start with systems, types, format and */
/*        input time. "FROM" time system goes first. */

	frsidx = isrchc_(fromts, &c__4, systms, (ftnlen)32, (ftnlen)32);
	if (s_cmp(fromts, " ", (ftnlen)32, (ftnlen)1) != 0) {
	    if (frsidx == 0) {
		setmsg_("Time system '#' from which the time must be convert"
			"ed is not one of the supported systems.", (ftnlen)90);
		errch_("#", fromts, (ftnlen)1, (ftnlen)32);
		sigerr_("SPICE(BADFROMTIMESYSTEM)", (ftnlen)24);
		chkout_("CRONOS", (ftnlen)6);
		return 0;
	    }
	} else {
	    setmsg_("Time system from which the time must be converted wasn'"
		    "t specified on the command line using # switch.", (ftnlen)
		    102);
	    errch_("#", "-FROM", (ftnlen)1, (ftnlen)5);
	    sigerr_("SPICE(NOFROMTIMESYSTEM)", (ftnlen)23);
	    chkout_("CRONOS", (ftnlen)6);
	    return 0;
	}

/*        "TO" time system goes second. */

	tosidx = isrchc_(tots, &c__4, systms, (ftnlen)32, (ftnlen)32);
	if (s_cmp(tots, " ", (ftnlen)32, (ftnlen)1) != 0) {
	    if (tosidx == 0) {
		setmsg_("Time system '#' to which the time must be converted"
			" is not one of the supported systems.", (ftnlen)88);
		errch_("#", tots, (ftnlen)1, (ftnlen)32);
		sigerr_("SPICE(BADTOTIMESYSTEM)", (ftnlen)22);
		chkout_("CRONOS", (ftnlen)6);
		return 0;
	    }
	} else {
	    setmsg_("Time system to which the time must be converted wasn't "
		    "specified on the command line using # switch.", (ftnlen)
		    100);
	    errch_("#", "-TO", (ftnlen)1, (ftnlen)3);
	    sigerr_("SPICE(NOTOTIMESYSTEM)", (ftnlen)21);
	    chkout_("CRONOS", (ftnlen)6);
	    return 0;
	}

/*        "FROM" time type goes third. */

	frtidx = isrchc_(fromtt, &c__10, types, (ftnlen)32, (ftnlen)32);
	if (frtidx != 0) {
	    if (! systyp[(i__1 = frsidx + (frtidx << 2) - 5) < 40 && 0 <= 
		    i__1 ? i__1 : s_rnge("systyp", i__1, "cronos_", (ftnlen)
		    737)]) {
		setmsg_("Time type '#' is not applicable for time system '#'"
			" from which the time must be converted.", (ftnlen)90);
		errch_("#", fromtt, (ftnlen)1, (ftnlen)32);
		errch_("#", fromts, (ftnlen)1, (ftnlen)32);
		sigerr_("SPICE(MISMATCHFROMTIMETYPE)", (ftnlen)27);
		chkout_("CRONOS", (ftnlen)6);
		return 0;
	    }
	} else {
	    if (s_cmp(fromtt, " ", (ftnlen)32, (ftnlen)1) != 0) {
		setmsg_("Time type '#' from which the time must be converted"
			" is not one of the supported types.", (ftnlen)86);
		errch_("#", fromtt, (ftnlen)1, (ftnlen)32);
		sigerr_("SPICE(BADFROMTIMETYPE)", (ftnlen)22);
		chkout_("CRONOS", (ftnlen)6);
		return 0;
	    } else {

/*              Set "from" type to default type for that system if */
/*              the value that we have got is blank. */

		s_copy(fromtt, deftyp + (((i__1 = frsidx - 1) < 4 && 0 <= 
			i__1 ? i__1 : s_rnge("deftyp", i__1, "cronos_", (
			ftnlen)761)) << 5), (ftnlen)32, (ftnlen)32);
		frtidx = isrchc_(fromtt, &c__10, types, (ftnlen)32, (ftnlen)
			32);
	    }
	}

/*        "TO" time type goes fourth. */

	totidx = isrchc_(tott, &c__10, types, (ftnlen)32, (ftnlen)32);
	if (totidx != 0) {
	    if (! systyp[(i__1 = tosidx + (totidx << 2) - 5) < 40 && 0 <= 
		    i__1 ? i__1 : s_rnge("systyp", i__1, "cronos_", (ftnlen)
		    771)]) {
		setmsg_("Time type '#' is not applicable for time system '#'"
			" to which the time must be converted.", (ftnlen)88);
		errch_("#", tott, (ftnlen)1, (ftnlen)32);
		errch_("#", tots, (ftnlen)1, (ftnlen)32);
		sigerr_("SPICE(MISMATCHTOTIMETYPE)", (ftnlen)25);
		chkout_("CRONOS", (ftnlen)6);
		return 0;
	    }
	} else {
	    if (s_cmp(tott, " ", (ftnlen)32, (ftnlen)1) != 0) {
		setmsg_("Time type '#' to which the time must be converted i"
			"s not one of the supported types.", (ftnlen)84);
		errch_("#", tott, (ftnlen)1, (ftnlen)32);
		sigerr_("SPICE(BADTOTIMETYPE)", (ftnlen)20);
		chkout_("CRONOS", (ftnlen)6);
		return 0;
	    } else {

/*              Set "to" type to default type for that system if */
/*              the value that we have got is blank. */

		s_copy(tott, deftyp + (((i__1 = tosidx - 1) < 4 && 0 <= i__1 ?
			 i__1 : s_rnge("deftyp", i__1, "cronos_", (ftnlen)795)
			) << 5), (ftnlen)32, (ftnlen)32);
		totidx = isrchc_(tott, &c__10, types, (ftnlen)32, (ftnlen)32);
	    }
	}

/*        Output format string goes next. */

	if (s_cmp(tofrmt, " ", (ftnlen)256, (ftnlen)1) != 0) {

/*           Is formating applicable to time system/type? */

	    if (fmtted[(i__1 = tosidx + (totidx << 2) - 5) < 40 && 0 <= i__1 ?
		     i__1 : s_rnge("fmtted", i__1, "cronos_", (ftnlen)808)]) {

/*              For UTC and ET we may need to do some check on the */
/*              formats. for other systems we will just leave blank */
/*              cases, maybe check will be added later. */

		if (s_cmp(tots, "UTC", (ftnlen)32, (ftnlen)3) == 0 || s_cmp(
			tots, "ET", (ftnlen)32, (ftnlen)2) == 0) {

/*                 We run TPICTR and use Bill logic -- if it fails, then */
/*                 provided string is a format picture already, if it */
/*                 succeeds -- then input was a times string and we have */
/*                 our picture as the output :). */

		    tpictr_(tofrmt, hline, &ok, error, (ftnlen)256, (ftnlen)
			    256, (ftnlen)256);
		    if (ok) {
			s_copy(tofrmt, hline, (ftnlen)256, (ftnlen)256);
		    }
		} else if (s_cmp(tots, "SCLK", (ftnlen)32, (ftnlen)4) == 0) {

/*                 No checks for now. */

		} else if (s_cmp(tots, "LST", (ftnlen)32, (ftnlen)3) == 0) {

/*                 No checks for now. */

		}
	    } else {

/*              Formatting is not applicable to this system/type. */

		setmsg_("Specification of a format for the  system '#'/type "
			"'#' to which the time must be converted is not suppo"
			"rted.", (ftnlen)108);
		errch_("#", tots, (ftnlen)1, (ftnlen)32);
		errch_("#", tott, (ftnlen)1, (ftnlen)32);
		sigerr_("SPICE(FORMATNOTAPPLICABLE)", (ftnlen)26);
		chkout_("CRONOS", (ftnlen)6);
		return 0;
	    }
	} else {

/*           Format has not been provided. Use default. */

	    s_copy(tofrmt, fmtpic + (((i__1 = tosidx + (totidx << 2) - 5) < 
		    40 && 0 <= i__1 ? i__1 : s_rnge("fmtpic", i__1, "cronos_",
		     (ftnlen)863)) << 6), (ftnlen)256, (ftnlen)64);
	}

/*        Process the s/c ID. It is required for all types in the SCLK */
/*        time system, LST type in the LST time system, and for ERT, ETT */
/*        and LT types in the UTC and ET systems. */

	if (s_cmp(fromts, "SCLK", (ftnlen)32, (ftnlen)4) == 0 || s_cmp(tots, 
		"SCLK", (ftnlen)32, (ftnlen)4) == 0 || s_cmp(fromts, "LST", (
		ftnlen)32, (ftnlen)3) == 0 || s_cmp(tots, "LST", (ftnlen)32, (
		ftnlen)3) == 0 || s_cmp(fromtt, "ERT", (ftnlen)32, (ftnlen)3) 
		== 0 || s_cmp(tott, "ERT", (ftnlen)32, (ftnlen)3) == 0 || 
		s_cmp(fromtt, "ETT", (ftnlen)32, (ftnlen)3) == 0 || s_cmp(
		tott, "ETT", (ftnlen)32, (ftnlen)3) == 0 || s_cmp(fromtt, 
		"LT", (ftnlen)32, (ftnlen)2) == 0 || s_cmp(tott, "LT", (
		ftnlen)32, (ftnlen)2) == 0) {

/*           Check that the s/c ID has been provided. */

	    if (s_cmp(scstr, " ", (ftnlen)32, (ftnlen)1) == 0) {
		setmsg_("S/c ID required to do conversion between specified "
			"time systems and types has not been provided after #"
			" switch.", (ftnlen)111);
		errch_("#", "-SC", (ftnlen)1, (ftnlen)3);
		sigerr_("SPICE(NOSPACECRAFTID)", (ftnlen)21);
		chkout_("CRONOS", (ftnlen)6);
		return 0;
	    }

/*           Check that s/c ID is an integer. */

	    nparsi_(scstr, &scid, error, &n, (ftnlen)32, (ftnlen)256);
	    if (n != 0) {
		setmsg_("NAIF ID for the spacecraft, '#', is not an integer "
			"number.", (ftnlen)58);
		errch_("#", scstr, (ftnlen)1, (ftnlen)32);
		sigerr_("SPICE(BADSCID)", (ftnlen)14);
		chkout_("CRONOS", (ftnlen)6);
		return 0;
	    }
	}

/*        Process the center body ID. It is required only for  LST */
/*        time system. */

	if (s_cmp(fromts, "LST", (ftnlen)32, (ftnlen)3) == 0 || s_cmp(tots, 
		"LST", (ftnlen)32, (ftnlen)3) == 0 || s_cmp(fromtt, "LSUN", (
		ftnlen)32, (ftnlen)4) == 0 || s_cmp(tott, "LSUN", (ftnlen)32, 
		(ftnlen)4) == 0) {

/*           Check that the body ID has been provided. */

	    if (s_cmp(censtr, " ", (ftnlen)32, (ftnlen)1) == 0) {
		setmsg_("Body ID required to do conversion between specified"
			" time systems and types has not been provided after "
			"# switch.", (ftnlen)112);
		errch_("#", "-CENTER", (ftnlen)1, (ftnlen)7);
		sigerr_("SPICE(NOSPACECRAFTID)", (ftnlen)21);
		chkout_("CRONOS", (ftnlen)6);
		return 0;
	    }

/*           Check that center body ID is an integer. */

	    nparsi_(censtr, &bodyid, error, &n, (ftnlen)32, (ftnlen)256);
	    if (n != 0) {
		setmsg_("NAIF ID for the center body, '#', is not an integer"
			" number.", (ftnlen)59);
		errch_("#", censtr, (ftnlen)1, (ftnlen)32);
		sigerr_("SPICE(BADBODYID1)", (ftnlen)17);
		chkout_("CRONOS", (ftnlen)6);
		return 0;
	    }

/*           Check that center body ID is for a planet or a satellite. */

	    if (bodyid <= 0 || bodyid >= 1000) {
		setmsg_("Center body must be planet or satellite. Provided I"
			"D '#'  designates neither of these.", (ftnlen)86);
		errint_("#", &bodyid, (ftnlen)1);
		sigerr_("SPICE(BADBODYID2)", (ftnlen)17);
		chkout_("CRONOS", (ftnlen)6);
		return 0;
	    }
	}

/*        Check whether SOL index, if provided, is an integer. */

	if (s_cmp(solids, " ", (ftnlen)32, (ftnlen)1) != 0) {
	    nparsi_(solids, &solzer, error, &n, (ftnlen)32, (ftnlen)256);
	    if (n != 0) {
		setmsg_("First SOL index, '#', is not an integer number.", (
			ftnlen)47);
		errch_("#", censtr, (ftnlen)1, (ftnlen)32);
		sigerr_("SPICE(BADSOLINDEX)", (ftnlen)18);
		chkout_("CRONOS", (ftnlen)6);
		return 0;
	    }
	} else {
	    solzer = 1;
	}

/*        Parsing was successfully completed; re-assign saved command */
/*        line. */

	s_copy(cmdsav, cmdlin, (ftnlen)2048, cmdlin_len);

/*        Reset the number of iteration that have been done with this */
/*        command line */

	nloops = 0;
    }

/*     For LST time we need compute UTC for the first local midnight and */
/*     the duration of the average local second. Unfortunately, this */
/*     part relies on the actual loaded data (from all SPICE */
/*     subsystems), and, therefore, it's hard to determine whether it */
/*     had to be re-computed even if the command line didn't change. */

    if (s_cmp(fromts, "LST", (ftnlen)32, (ftnlen)3) == 0 || s_cmp(tots, "LST",
	     (ftnlen)32, (ftnlen)3) == 0) {
	if (s_cmp(lsttim, " ", (ftnlen)32, (ftnlen)1) != 0) {

/*           Landing time was provided. So we can compute SOL days and */
/*           for that we need to get our first midnight time. First, we */
/*           convert given time to ET. */

	    str2et_(lsttim, &lstet, (ftnlen)32);
	    midnet = lstet;

/*           Second we compute longitude of the s/c in the body-fixed */
/*           rotating frame at the landing time. */

	    cidfrm_(&bodyid, &frcode, hline, &found, (ftnlen)256);
	    if (! found) {
		setmsg_("Cannot determine body-fixed frame name for the body"
			" with ID #.", (ftnlen)62);
		errint_("#", &bodyid, (ftnlen)1);
		sigerr_("SPICE(FRAMENOTFOUND)", (ftnlen)20);
		chkout_("CRONOS", (ftnlen)6);
		return 0;
	    }
	    spkez_(&scid, &midnet, hline, "NONE", &bodyid, state, &lt, (
		    ftnlen)256, (ftnlen)4);
	    reclat_(state, &r__, &lon, &lat);

/*           Now we need compute average number of ET seconds in one */
/*           body second. */

	    s_copy(hword, "BODY#_PM", (ftnlen)32, (ftnlen)8);
	    repmi_(hword, "#", &bodyid, hword, (ftnlen)32, (ftnlen)1, (ftnlen)
		    32);
	    gdpool_(hword, &c__2, &c__1, &n, &pmmotn, &found, (ftnlen)32);
	    if (pmmotn == 0.) {
		setmsg_("Body # prime meridian motion, specified in the PCK "
			"keyword #, is zero, which doesn't make any sense.", (
			ftnlen)100);
		errint_("#", &bodyid, (ftnlen)1);
		errch_("#", hline, (ftnlen)1, (ftnlen)256);
		sigerr_("SPICE(BADPCKVALUE)", (ftnlen)18);
		chkout_("CRONOS", (ftnlen)6);
		return 0;
	    }

/*           If our body is satellite, we compute period of orbit around */
/*           Sun for the planet which this satellite is orbiting. */

	    i__ = zzbodbry_(&bodyid);
	    if (i__ >= 1 && i__ <= 9) {
		i__ = i__ * 100 + 99;
	    } else {
		i__ = bodyid;
	    }

/*           To compute period of the orbit around the Sun, we need Sun */
/*           GM. */

	    dtpool_("BODY10_GM", &found, &n, hwd, (ftnlen)9, (ftnlen)32);
	    if (! found || n < 1 || *(unsigned char *)hwd != 'N') {
		setmsg_("GM of the Sun required to compute heliocentric orbi"
			"t period for the body with NAIF ID # wasn't found in"
			" the pool. Check whether BODY10_GM parameter is pres"
			"ent in the loaded PCK files.", (ftnlen)183);
		errint_("#", &i__, (ftnlen)1);
		sigerr_("SPICE(NOSUNGM)", (ftnlen)14);
		chkout_("CRONOS", (ftnlen)6);
		return 0;
	    }
	    bodvrd_("10", "GM", &c__1, &n, &sungm, (ftnlen)2, (ftnlen)2);

/*           Get state of center body relative to the Sun, compute */
/*           elements and period. */

	    spkez_(&i__, &midnet, "J2000", "NONE", &c__10, state, &lt, (
		    ftnlen)5, (ftnlen)4);
	    oscelt_(state, &midnet, &sungm, elts);
/* Computing 3rd power */
	    d__1 = elts[0] / (1. - elts[1]);
	    t = twopi_() * sqrt(d__1 * (d__1 * d__1) / sungm);
	    if (t == 0.) {
		setmsg_("Body's # heliocentric orbital period is computed as"
			" zero, which doesn't make any sense.", (ftnlen)87);
		errint_("#", &i__, (ftnlen)1);
		sigerr_("SPICE(BADORBITALPERIOD)", (ftnlen)23);
		chkout_("CRONOS", (ftnlen)6);
		return 0;
	    }

/*           Compute average local second duration (thanks to Bill */
/*           for the formula!) */

	    hdp = 1. / (spd_() * 360. / pmmotn) - 1. / t;
	    if (hdp == 0.) {
		setmsg_("Duration of the local second on the body # is infin"
			"ity. So, local solar time is always the same there.", 
			(ftnlen)102);
		errint_("#", &bodyid, (ftnlen)1);
		sigerr_("SPICE(INDEFINITELOCALSECOND)", (ftnlen)28);
		chkout_("CRONOS", (ftnlen)6);
		return 0;
	    }
	    scrate = 1. / hdp / spd_();
	    if (scrate < 0.) {
		scrate = -scrate;
	    }

/*           Now we compute previous local solar time midnight at this */
/*           longitude. We will use it as the basic for SOL computation */
/*           for input and output LST. */

	    lstmid_(&midnet, &bodyid, &lon, &scrate, "PREVIOUS", &c__0, &
		    ettemp, (ftnlen)8);
	    midnet = ettemp;
	}
    } else {

/*        Landing date knowledge is not needed because neither input nor */
/*        output system is LST. */

	s_copy(lsttim, " ", (ftnlen)32, (ftnlen)1);
    }

/*     If speaker is ON, print trace information, but only for the first */
/*     time around with this command line. (This "hook" to STDOUT is */
/*     ugly, but we must keep it for backwards compatibility with the */
/*     old CHRONOS program interface.) */

/*     The "speaker" can be set on by calling SPEKON entry of the SPEAKR */
/*     umbrella. But it should never be done if this routine used */
/*     anywhere but in CHRONOS executable. */

    spekst_(hword, (ftnlen)32);
    if (s_cmp(hword, "ON", (ftnlen)2, (ftnlen)2) == 0 && nloops == 0) {
	speak_(" ", (ftnlen)1);
/* Writing concatenation */
	i__2[0] = 22, a__1[0] = "Converting time from: ";
	i__2[1] = rtrim_(fromts, (ftnlen)32), a__1[1] = fromts;
	i__2[2] = 1, a__1[2] = "/";
	i__2[3] = rtrim_(fromtt, (ftnlen)32), a__1[3] = fromtt;
	s_cat(ch__1, a__1, i__2, &c__4, (ftnlen)87);
	speak_(ch__1, rtrim_(fromts, (ftnlen)32) + 23 + rtrim_(fromtt, (
		ftnlen)32));
/* Writing concatenation */
	i__2[0] = 22, a__1[0] = "                  to: ";
	i__2[1] = rtrim_(tots, (ftnlen)32), a__1[1] = tots;
	i__2[2] = 1, a__1[2] = "/";
	i__2[3] = rtrim_(tott, (ftnlen)32), a__1[3] = tott;
	s_cat(ch__1, a__1, i__2, &c__4, (ftnlen)87);
	speak_(ch__1, rtrim_(tots, (ftnlen)32) + 23 + rtrim_(tott, (ftnlen)32)
		);
	if (s_cmp(tofrmt, " ", (ftnlen)256, (ftnlen)1) != 0) {
/* Writing concatenation */
	    i__3[0] = 22, a__2[0] = "  Output time format: ";
	    i__3[1] = rtrim_(tofrmt, (ftnlen)256), a__2[1] = tofrmt;
	    s_cat(ch__2, a__2, i__3, &c__2, (ftnlen)278);
	    speak_(ch__2, rtrim_(tofrmt, (ftnlen)256) + 22);
	} else {
	    speak_("  Output time format: NOT APPLICABLE", (ftnlen)36);
	}
	if (s_cmp(fromts, "SCLK", (ftnlen)32, (ftnlen)4) == 0 || s_cmp(tots, 
		"SCLK", (ftnlen)32, (ftnlen)4) == 0 || s_cmp(fromts, "LST", (
		ftnlen)32, (ftnlen)3) == 0 || s_cmp(tots, "LST", (ftnlen)32, (
		ftnlen)3) == 0 || s_cmp(fromtt, "ERT", (ftnlen)32, (ftnlen)3) 
		== 0 || s_cmp(tott, "ERT", (ftnlen)32, (ftnlen)3) == 0 || 
		s_cmp(fromtt, "ETT", (ftnlen)32, (ftnlen)3) == 0 || s_cmp(
		tott, "ETT", (ftnlen)32, (ftnlen)3) == 0 || s_cmp(fromtt, 
		"LT", (ftnlen)32, (ftnlen)2) == 0 || s_cmp(tott, "LT", (
		ftnlen)32, (ftnlen)2) == 0) {
	    s_copy(hline, "  Spacecraft NAIF ID: #", (ftnlen)256, (ftnlen)23);
	    repmi_(hline, "#", &scid, hline, (ftnlen)256, (ftnlen)1, (ftnlen)
		    256);
	    speak_(hline, (ftnlen)256);
	}
	if (s_cmp(fromts, "LST", (ftnlen)32, (ftnlen)3) == 0 || s_cmp(tots, 
		"LST", (ftnlen)32, (ftnlen)3) == 0 || s_cmp(fromtt, "LSUN", (
		ftnlen)32, (ftnlen)4) == 0 || s_cmp(tott, "LSUN", (ftnlen)32, 
		(ftnlen)4) == 0 || s_cmp(fromtt, "ERT", (ftnlen)32, (ftnlen)3)
		 == 0 || s_cmp(tott, "ERT", (ftnlen)32, (ftnlen)3) == 0 || 
		s_cmp(fromtt, "ETT", (ftnlen)32, (ftnlen)3) == 0 || s_cmp(
		tott, "ETT", (ftnlen)32, (ftnlen)3) == 0 || s_cmp(fromtt, 
		"LT", (ftnlen)32, (ftnlen)2) == 0 || s_cmp(tott, "LT", (
		ftnlen)32, (ftnlen)2) == 0) {
	    s_copy(hline, " Center body NAIF ID: #", (ftnlen)256, (ftnlen)23);
	    repmi_(hline, "#", &bodyid, hline, (ftnlen)256, (ftnlen)1, (
		    ftnlen)256);
	    speak_(hline, (ftnlen)256);
	}
	if (s_cmp(fromts, "LST", (ftnlen)32, (ftnlen)3) == 0 || s_cmp(tots, 
		"LST", (ftnlen)32, (ftnlen)3) == 0) {
	    if (s_cmp(lsttim, " ", (ftnlen)32, (ftnlen)1) != 0) {
/* Writing concatenation */
		i__3[0] = 22, a__2[0] = "   Landing date/time: ";
		i__3[1] = rtrim_(lsttim, (ftnlen)32), a__2[1] = lsttim;
		s_cat(ch__3, a__2, i__3, &c__2, (ftnlen)54);
		speak_(ch__3, rtrim_(lsttim, (ftnlen)32) + 22);
		et2utc_(&midnet, "C", &c__3, hword, (ftnlen)1, (ftnlen)32);
		s_copy(hline, "   Landing SOL index: #", (ftnlen)256, (ftnlen)
			23);
		repmi_(hline, "#", &solzer, hline, (ftnlen)256, (ftnlen)1, (
			ftnlen)256);
		speak_(hline, (ftnlen)256);
/* Writing concatenation */
		i__3[0] = 22, a__2[0] = "Landing SOL midnight: ";
		i__3[1] = rtrim_(hword, (ftnlen)32), a__2[1] = hword;
		s_cat(ch__3, a__2, i__3, &c__2, (ftnlen)54);
		speak_(ch__3, rtrim_(hword, (ftnlen)32) + 22);
		s_copy(hline, "Average local second: # ET seconds", (ftnlen)
			256, (ftnlen)34);
		repmd_(hline, "#", &scrate, &c__14, hline, (ftnlen)256, (
			ftnlen)1, (ftnlen)256);
		speak_(hline, (ftnlen)256);
	    } else {
		speak_("   Landing date/time: NOT SPECIFIED", (ftnlen)35);
	    }
	}
	speak_(" ", (ftnlen)1);
    }

/*     Loop for each time in the input times array. */

    i__1 = *ntimes;
    for (count = 1; count <= i__1; ++count) {
	s_copy(intime, inptim + (count - 1) * inptim_len, (ftnlen)256, 
		inptim_len);

/*        Is input time blank? */

	if (s_cmp(intime, " ", (ftnlen)256, (ftnlen)1) == 0) {
	    setmsg_("The #-th input time is blank.", (ftnlen)29);
	    errint_("#", &count, (ftnlen)1);
	    sigerr_("SPICE(BLANKINPUTTIME)", (ftnlen)21);
	}

/*        Let's do real work. First we need to convert from whatever */
/*        "from" system we have got to spacecraft ET. UTC case is first. */

	if (s_cmp(fromts, "UTC", (ftnlen)32, (ftnlen)3) == 0) {

/*           If we have got time string, we need to check whether it's */
/*           parsable. And if it is, convert it to ET. */

	    if (s_cmp(fromtt, "ERT", (ftnlen)32, (ftnlen)3) == 0 || s_cmp(
		    fromtt, "ETT", (ftnlen)32, (ftnlen)3) == 0 || s_cmp(
		    fromtt, "SCET", (ftnlen)32, (ftnlen)4) == 0) {

/*              Convert UTC to ET. Nothing can be simpler if you have */
/*              SPICELIB time subsystem. But first check whether there */
/*              is some key in the input string that will force SPICE */
/*              time system to interpret it as TDB or TDT instead of */
/*              UTC. */

		ucase_(intime, line, (ftnlen)256, (ftnlen)256);
		if (pos_(line, "TDB", &c__1, (ftnlen)256, (ftnlen)3) != 0 || 
			pos_(line, "TDT", &c__1, (ftnlen)256, (ftnlen)3) != 0)
			 {
		    setmsg_("Input time which is supposed to be a UTC time c"
			    "ontains TDB or TDT flag that will make it interp"
			    "reted as Barycentric Dynamical Time system (TDB "
			    "or ET ) or the Terrestrial Dynamical Time system"
			    " (TDT) time.", (ftnlen)203);
		    sigerr_("SPICE(BADINPUTUTCTIME)", (ftnlen)22);
		    chkout_("CRONOS", (ftnlen)6);
		    return 0;
		}

/*              Well, let's convert our time to ET using STR2ET. Run */
/*              TPARTV/TCHECK before it to generate more meaningful */
/*              message in case if input time cannot be parsed. */

		tpartv_(intime, tvec, &ntvec, type__, modify, &mods, &yabbrv, 
			&ok, hword, error, (ftnlen)256, (ftnlen)5, (ftnlen)8, 
			(ftnlen)32, (ftnlen)256);
		if (ok) {
		    tcheck_(tvec, type__, &mods, modify, &ok, error, (ftnlen)
			    5, (ftnlen)8, (ftnlen)256);
		}
		if (! ok) {
		    setmsg_("Input time doesn't seem to represent a UTC time"
			    " correctly for the following reason: # ", (ftnlen)
			    86);
		    errch_("#", error, (ftnlen)1, (ftnlen)256);
		    sigerr_("SPICE(BADINPUTUTCTIME)", (ftnlen)22);
		    chkout_("CRONOS", (ftnlen)6);
		    return 0;
		}
		str2et_(intime, &et, (ftnlen)256);
	    }

/*           What else depending on the type do we need to do with it? */

	    if (s_cmp(fromtt, "ERT", (ftnlen)32, (ftnlen)3) == 0) {

/*              For the ERT type, we need to adjust our ET time to be on */
/*              the spacecraft instead of the Earth. Call LTIME to do */
/*              it. */

		ltime_(&et, &c__399, "<-", &scid, &ettemp, &lt, (ftnlen)2);
		et = ettemp;
	    } else if (s_cmp(fromtt, "ETT", (ftnlen)32, (ftnlen)3) == 0) {

/*              For the ETT type, we need to adjust our ET time to be on */
/*              the spacecraft instead of the Earth. Call LTIME to do it */
/*              with direction opposite to ERT. */

		ltime_(&et, &c__399, "->", &scid, &ettemp, &lt, (ftnlen)2);
		et = ettemp;
	    } else if (s_cmp(fromtt, "LT", (ftnlen)32, (ftnlen)2) == 0) {

/*              Light time as an input??? Whoever specified it must be */
/*              crazy! */

		setmsg_("Input time type '#' doesn't make any sense. Cannot "
			"process it.", (ftnlen)62);
		errch_("#", "LT", (ftnlen)1, (ftnlen)2);
		sigerr_("SPICE(BADINPUTTYPE)", (ftnlen)19);
		chkout_("CRONOS", (ftnlen)6);
		return 0;
	    } else if (s_cmp(fromtt, "SCET", (ftnlen)32, (ftnlen)4) == 0) {

/*              We don't need to do any additional work for SCET type. */
/*              Therefore, this "if case" is empty. */

	    } else {
		setmsg_("How did you manage to get to this place in the prog"
			"ram?", (ftnlen)55);
		sigerr_("SPICE(CHRONOSBUG1)", (ftnlen)18);
		chkout_("CRONOS", (ftnlen)6);
		return 0;
	    }

/*        We are done with UTC case. The next case is ET. */

	} else if (s_cmp(fromts, "ET", (ftnlen)32, (ftnlen)2) == 0) {

/*           Convert ET to ET. Silly, huh? :) Maybe not so silly given */
/*           that ET is provided as a string and quite it's possibly in */
/*           some calendar format. If we have got time string, we need */
/*           to check whether it's parsable. */

	    if (s_cmp(fromtt, "ERT", (ftnlen)32, (ftnlen)3) == 0 || s_cmp(
		    fromtt, "ETT", (ftnlen)32, (ftnlen)3) == 0 || s_cmp(
		    fromtt, "SCET", (ftnlen)32, (ftnlen)4) == 0) {

/*              Add TDB and run TPARTV/TCHECK to see if input ET time is */
/*              parsable. */

		ucase_(intime, line, (ftnlen)256, (ftnlen)256);
		if (pos_(line, "TDB", &c__1, (ftnlen)256, (ftnlen)3) == 0) {
		    s_copy(line, intime, (ftnlen)256, (ftnlen)256);
/* Writing concatenation */
		    i__3[0] = rtrim_(line, (ftnlen)256), a__2[0] = line;
		    i__3[1] = 4, a__2[1] = " TDB";
		    s_cat(line, a__2, i__3, &c__2, (ftnlen)256);
		}
		tpartv_(line, tvec, &ntvec, type__, modify, &mods, &yabbrv, &
			ok, hword, error, (ftnlen)256, (ftnlen)5, (ftnlen)8, (
			ftnlen)32, (ftnlen)256);
		if (ok) {
		    tcheck_(tvec, type__, &mods, modify, &ok, error, (ftnlen)
			    5, (ftnlen)8, (ftnlen)256);
		}
		if (! ok) {
		    setmsg_("Input time doesn't seem to represent a ET time "
			    "correctly for the following reason: # ", (ftnlen)
			    85);
		    errch_("#", error, (ftnlen)1, (ftnlen)256);
		    sigerr_("SPICE(BADINPUTETTIME)", (ftnlen)21);
		    chkout_("CRONOS", (ftnlen)6);
		    return 0;
		}
		str2et_(line, &et, (ftnlen)256);
	    } else if (s_cmp(fromtt, "SECONDS", (ftnlen)32, (ftnlen)7) == 0) {

/*              DP number of seconds is stored in the input string. Run */
/*              NPARSD. */

		nparsd_(intime, &et, error, &n, (ftnlen)256, (ftnlen)256);
		if (n != 0) {
		    setmsg_("Input time doesn't seem to be a DP number repre"
			    "senting a ET seconds past J2000 for the followin"
			    "g reason: # ", (ftnlen)107);
		    errch_("#", error, (ftnlen)1, (ftnlen)256);
		    sigerr_("SPICE(BADINPUTETTIME)", (ftnlen)21);
		    chkout_("CRONOS", (ftnlen)6);
		    return 0;
		}
	    }

/*           What else depending on the type do we need to do with it? */

	    if (s_cmp(fromtt, "ERT", (ftnlen)32, (ftnlen)3) == 0) {

/*              For the ERT type, we need to adjust our ET time to be on */
/*              the spacecraft instead of the Earth. Call LTIME to do */
/*              it. */

		ltime_(&et, &c__399, "<-", &scid, &ettemp, &lt, (ftnlen)2);
		et = ettemp;
	    } else if (s_cmp(fromtt, "ETT", (ftnlen)32, (ftnlen)3) == 0) {

/*              For the ETT type, we need to adjust our ET time to be on */
/*              the spacecraft instead of the Earth. Call LTIME to do it */
/*              with direction opposite to ERT. */

		ltime_(&et, &c__399, "->", &scid, &ettemp, &lt, (ftnlen)2);
		et = ettemp;
	    } else if (s_cmp(fromtt, "LT", (ftnlen)32, (ftnlen)2) == 0) {

/*              Light time as an input??? Whoever specified it must be */
/*              crazy! */

		setmsg_("Input time type '#' doesn't make any sense. Cannot "
			"process it.", (ftnlen)62);
		errch_("#", "LT", (ftnlen)1, (ftnlen)2);
		sigerr_("SPICE(BADINPUTTYPE)", (ftnlen)19);
		chkout_("CRONOS", (ftnlen)6);
		return 0;
	    } else if (s_cmp(fromtt, "SECONDS", (ftnlen)32, (ftnlen)7) == 0) {

/*              Hey, we don't need to do anything, we already parsed the */
/*              string containing DP number. */

	    } else if (s_cmp(fromtt, "SCET", (ftnlen)32, (ftnlen)4) == 0) {

/*              Also, nothing more to do for SCET type */

	    } else {
		setmsg_("How did you manage to get to this place in the prog"
			"ram?", (ftnlen)55);
		sigerr_("SPICE(CHRONOSBUG2)", (ftnlen)18);
		chkout_("CRONOS", (ftnlen)6);
		return 0;
	    }

/*        We are done with ET case also. Now SCLK system */

	} else if (s_cmp(fromts, "SCLK", (ftnlen)32, (ftnlen)4) == 0) {

/*           Convert SCLK to ET. First, convert whatever string we have */
/*           got to SCLK ticks. */

	    if (s_cmp(fromtt, "SCLK", (ftnlen)32, (ftnlen)4) == 0) {

/*              For string SCLK we just pull it though SCENCD and see */
/*              what happens. */

		scencd_(&scid, intime, &sclkdp, (ftnlen)256);
	    } else if (s_cmp(fromtt, "HEX", (ftnlen)32, (ftnlen)3) == 0) {

/*              Well, for hex we need to convert each field (except */
/*              partition :) to integer before calling SCENCD. First we */
/*              determine whether input time contains no more than one */
/*              slash and that it's not the last char. */

		lparse_(intime, "/", &c__10, &n, fields, (ftnlen)256, (ftnlen)
			1, (ftnlen)32);
		if (n > 2 || s_cmp(fields + (((i__4 = n - 1) < 10 && 0 <= 
			i__4 ? i__4 : s_rnge("fields", i__4, "cronos_", (
			ftnlen)1506)) << 5), " ", (ftnlen)32, (ftnlen)1) == 0)
			 {
		    setmsg_("Input HEX SCLK time '#' contains more than one "
			    "'/' designating partitions and/or it's the last "
			    "non-blank character in the time.", (ftnlen)127);
		    errch_("#", intime, (ftnlen)1, (ftnlen)256);
		    sigerr_("SPICE(BADINPUTETTIME)", (ftnlen)21);
		    chkout_("CRONOS", (ftnlen)6);
		    return 0;
		} else if (n == 2) {

/*                 There is a slash in the input time. Save all before */
/*                 slash and slash to use a prefix for "integer" SCLK */
/*                 string that we are going to make. */

		    s_copy(hline, intime, (ftnlen)256, pos_(intime, "/", &
			    c__1, (ftnlen)256, (ftnlen)1));
		} else {

/*                 No slash -- no prefix for "integer" SCLK string that */
/*                 we are going to make. */

		    s_copy(hline, " ", (ftnlen)256, (ftnlen)1);
		}

/*              Now we parse portion of input time after slash using */
/*              delimiter list from type 1 SCLK. */

		i__4 = pos_(intime, "/", &c__1, (ftnlen)256, (ftnlen)1);
		lparsm_(intime + i__4, " -.,:", &c__10, &n, fields, 256 - 
			i__4, (ftnlen)5, (ftnlen)32);

/*              Convert each field from HEX to integer and "rebuild" */
/*              "integer" SCLK. */

		i__4 = n;
		for (i__ = 1; i__ <= i__4; ++i__) {
		    hx2int_(fields + (((i__5 = i__ - 1) < 10 && 0 <= i__5 ? 
			    i__5 : s_rnge("fields", i__5, "cronos_", (ftnlen)
			    1548)) << 5), &j, &bad, error, (ftnlen)32, (
			    ftnlen)256);
		    if (bad) {
			setmsg_("#th field of input HEX SCLK string '#' does"
				"n't represent HEX number.", (ftnlen)68);
			errint_("#", &i__, (ftnlen)1);
			errch_("#", intime, (ftnlen)1, (ftnlen)256);
			sigerr_("SPICE(BADINPUTETTIME)", (ftnlen)21);
			chkout_("CRONOS", (ftnlen)6);
			return 0;
		    }
		    intstr_(&j, hword, (ftnlen)32);
/* Writing concatenation */
		    i__6[0] = rtrim_(hline, (ftnlen)256), a__3[0] = hline;
		    i__6[1] = 1, a__3[1] = " ";
		    i__6[2] = 32, a__3[2] = hword;
		    s_cat(hline, a__3, i__6, &c__3, (ftnlen)256);
		}

/*              Convert SCLK string to ticks. */

		scencd_(&scid, hline, &sclkdp, (ftnlen)256);
	    } else if (s_cmp(fromtt, "TICKS", (ftnlen)32, (ftnlen)5) == 0) {

/*              For SCLK ticks we just need to convert string to DP. */

		nparsd_(intime, &sclkdp, error, &n, (ftnlen)256, (ftnlen)256);
		if (n != 0) {
		    setmsg_("Input time doesn't seem to be a DP number repre"
			    "senting a SCLK ticks for the following reason: # "
			    , (ftnlen)96);
		    errch_("#", error, (ftnlen)1, (ftnlen)256);
		    sigerr_("SPICE(BADINPUTETTIME)", (ftnlen)21);
		    chkout_("CRONOS", (ftnlen)6);
		    return 0;
		}
	    } else {
		setmsg_("How did you manage to get to this place in the prog"
			"ram?", (ftnlen)55);
		sigerr_("SPICE(CHRONOSBUG3)", (ftnlen)18);
		chkout_("CRONOS", (ftnlen)6);
		return 0;
	    }

/*           Here we have encoded SCLK. Simply convert it to ET. */

	    sct2e_(&scid, &sclkdp, &et);

/*        The last is LST case. */

	} else if (s_cmp(fromts, "LST", (ftnlen)32, (ftnlen)3) == 0) {

/*           Again, processing depend on a time type. */

	    if (s_cmp(fromtt, "LST", (ftnlen)32, (ftnlen)3) == 0) {

/*              Convert local true solar time to ET. Check if there was */
/*              landing date or not. */

		if (s_cmp(lsttim, " ", (ftnlen)32, (ftnlen)1) != 0) {

/*                 Yes, there was. Let's look for SOL day number. It */
/*                 must be the next word after SOL marker. */

		    ucase_(intime, hline, (ftnlen)256, (ftnlen)256);
		    if (pos_(hline, "SOL", &c__1, (ftnlen)256, (ftnlen)3) == 
			    0) {
			setmsg_("\"#\" key is not present in the input LST t"
				"ime string \"#\". Cannot process this input "
				"time. ", (ftnlen)89);
			errch_("#", "SOL", (ftnlen)1, (ftnlen)3);
			errch_("#", intime, (ftnlen)1, (ftnlen)256);
			sigerr_("SPICE(NOSOLMARKER)", (ftnlen)18);
			chkout_("CRONOS", (ftnlen)6);
			return 0;
		    }
		    i__4 = pos_(hline, "SOL", &c__1, (ftnlen)256, (ftnlen)3) 
			    + rtrim_("SOL", (ftnlen)3) - 1;
		    nextwd_(intime + i__4, hword, line, 256 - i__4, (ftnlen)
			    32, (ftnlen)256);
		    nparsi_(hword, &solday, error, &n, (ftnlen)32, (ftnlen)
			    256);
		    if (n != 0) {
			setmsg_("SOL day specified after \"#\" key in the in"
				"put LST time string \"#\" is not an integer "
				"number. ", (ftnlen)91);
			errch_("#", "SOL", (ftnlen)1, (ftnlen)3);
			errch_("#", intime, (ftnlen)1, (ftnlen)256);
			sigerr_("SPICE(BADSOLDAY)", (ftnlen)16);
			chkout_("CRONOS", (ftnlen)6);
			return 0;
		    }
		    if (solday < solzer) {
			setmsg_("SOL day specified after \"#\" key in the in"
				"put LST time string \"#\" is less than landi"
				"ng SOL #. ", (ftnlen)93);
			errch_("#", "SOL", (ftnlen)1, (ftnlen)3);
			errch_("#", intime, (ftnlen)1, (ftnlen)256);
			errint_("#", &solzer, (ftnlen)1);
			sigerr_("SPICE(BADSOLDAY)", (ftnlen)16);
			chkout_("CRONOS", (ftnlen)6);
			return 0;
		    }

/*                 Now we can compute midnight ET corresponding to this */
/*                 SOL day. We will iterate to get this ET as good as */
/*                 possible. */

		    et = midnet + (solday - solzer) * spd_() * scrate;

/*                 "Precise" this rough estimate of the local solar */
/*                 midnight of this day of interest. */

		    lstmid_(&et, &bodyid, &lon, &scrate, "NEAREST", &c__0, &
			    ettemp, (ftnlen)7);
		    et = ettemp;

/*                 So, know when was the midnight of this SOL day. so we */
/*                 just need to add our local HR:MN:SC.### converted to */
/*                 ET seconds. These HR:MN:SC.### are currently */
/*                 contained the LINE variable. */

		    lparsm_(line, " :", &c__10, &n, fields, (ftnlen)256, (
			    ftnlen)2, (ftnlen)32);
		    if (n > 3) {
			setmsg_("Clock part of the input LST time string \""
				"#\" contains more than 3 fields.", (ftnlen)72)
				;
			errch_("#", intime, (ftnlen)1, (ftnlen)256);
			sigerr_("SPICE(BADSOLTIME)", (ftnlen)17);
			chkout_("CRONOS", (ftnlen)6);
			return 0;
		    }
		    lsecs = 0.;
		    i__4 = n;
		    for (i__ = 1; i__ <= i__4; ++i__) {
			nparsd_(fields + (((i__5 = i__ - 1) < 10 && 0 <= i__5 
				? i__5 : s_rnge("fields", i__5, "cronos_", (
				ftnlen)1694)) << 5), &hdp, error, &j, (ftnlen)
				32, (ftnlen)256);
			if (j != 0) {
			    setmsg_("#th token in the clock part of the inpu"
				    "t LST time string \"#\" is not a number.",
				     (ftnlen)77);
			    errint_("#", &i__, (ftnlen)1);
			    errch_("#", intime, (ftnlen)1, (ftnlen)256);
			    sigerr_("SPICE(BADSOLTIME)", (ftnlen)17);
			    chkout_("CRONOS", (ftnlen)6);
			    return 0;
			}
			i__5 = 3 - i__;
			lsecs += hdp * scrate * pow_di(&c_b400, &i__5);
		    }
		    if (lsecs < 0. || lsecs >= spd_() * scrate) {
			setmsg_("Number of local seconds represented by the "
				"clock part of the input LST time string \""
				"#\" is negative of greater than 86000.", (
				ftnlen)121);
			errch_("#", intime, (ftnlen)1, (ftnlen)256);
			sigerr_("SPICE(BADSOLTIME)", (ftnlen)17);
			chkout_("CRONOS", (ftnlen)6);
			return 0;
		    }
		    et += lsecs;

/*                 And, as last, we "precise" this estimate of the */
/*                 computed ET. */

		    d__1 = lsecs / scrate;
		    i__4 = i_dnnt(&d__1);
		    lstmid_(&et, &bodyid, &lon, &scrate, "NEAREST", &i__4, &
			    ettemp, (ftnlen)7);
		    et = ettemp;
		} else {

/*                 There were no landing date. Well, we cannot process */
/*                 LST as input than. */

		    setmsg_("Since landing time wasn't provided in the setup"
			    " # time cannot be used as \"from\" time system.", 
			    (ftnlen)92);
		    sigerr_("SPICE(NOLANDINGTIME)", (ftnlen)20);
		    chkout_("CRONOS", (ftnlen)6);
		    return 0;
		}
	    } else if (s_cmp(fromtt, "LSUN", (ftnlen)32, (ftnlen)4) == 0) {

/*              Longitude of the Sun as an input??? Whoever specified */
/*              it must be crazy! */

		setmsg_("Input time type '#' doesn't make any sense. Cannot "
			"process it.", (ftnlen)62);
		errch_("#", "LSUN", (ftnlen)1, (ftnlen)4);
		sigerr_("SPICE(BADINPUTTYPE)", (ftnlen)19);
		chkout_("CRONOS", (ftnlen)6);
		return 0;
	    } else {
		setmsg_("How did you manage to get to this place in the prog"
			"ram?", (ftnlen)55);
		sigerr_("SPICE(CHRONOSBUG9)", (ftnlen)18);
		chkout_("CRONOS", (ftnlen)6);
		return 0;
	    }
	} else {
	    setmsg_("How did you manage to get to this place in the program?",
		     (ftnlen)55);
	    sigerr_("SPICE(CHRONOSBUG4)", (ftnlen)18);
	    chkout_("CRONOS", (ftnlen)6);
	    return 0;
	}

/*        Congratulations!!! 1/2 is done: we have ET time corresponding */
/*        to our input time (whatever it was:). Let do the second half */
/*        and convert this ET to the output system/type. UTC case goes */
/*        first (again). */

	if (s_cmp(tots, "UTC", (ftnlen)32, (ftnlen)3) == 0) {

/*           Convert UTC to ET. Do we need any computation depending on */
/*           type? */

	    if (s_cmp(tott, "ERT", (ftnlen)32, (ftnlen)3) == 0) {

/*              For the ERT type, we need to adjust our ET time to be on */
/*              the Earth instead of the spacecraft. Call LTIME to do */
/*              it. */

		ltime_(&et, &scid, "->", &c__399, &ettemp, &lt, (ftnlen)2);
		et = ettemp;
	    } else if (s_cmp(tott, "ETT", (ftnlen)32, (ftnlen)3) == 0) {

/*              For the ETT type, we need to adjust our ET time to be on */
/*              the Earth instead of the spacecraft. Call LTIME to do it */
/*              but use light direction opposite to ERT. */

		ltime_(&et, &scid, "<-", &c__399, &ettemp, &lt, (ftnlen)2);
		et = ettemp;
	    } else if (s_cmp(tott, "LT", (ftnlen)32, (ftnlen)2) == 0) {

/*              For the LT type, we need to compute light time. Call */
/*              LTIME for that. */

		ltime_(&et, &scid, "->", &c__399, &ettemp, &lt, (ftnlen)2);
		et = ettemp;
	    } else if (s_cmp(tott, "SCET", (ftnlen)32, (ftnlen)4) == 0) {

/*              Nothing for SCET, thank you. */

	    } else {
		setmsg_("How did you manage to get to this place in the prog"
			"ram?", (ftnlen)55);
		sigerr_("SPICE(CHRONOSBUG5)", (ftnlen)18);
		chkout_("CRONOS", (ftnlen)6);
		return 0;
	    }

/*           Do we need any additional formatting depending on type? */

	    if (s_cmp(tott, "SCET", (ftnlen)32, (ftnlen)4) == 0 || s_cmp(tott,
		     "ETT", (ftnlen)32, (ftnlen)3) == 0 || s_cmp(tott, "ERT", 
		    (ftnlen)32, (ftnlen)3) == 0) {

/*              For SCET and ERT output is a time string. So we need a */
/*              stupidity check for output format. What if somebody put */
/*              ::TDB, ::TDT or some other strange subsystem */
/*              specification in it? (No need to uppercase when check -- */
/*              it was already done by TPICTR.) It's not clear whether */
/*              time zone token must be checked also. So, we don't check */
/*              it, because it leave more flexibility for output while */
/*              still filters completely erroneous sys-spec tokens. */

		if (pos_(tofrmt, "::TDB", &c__1, (ftnlen)256, (ftnlen)5) != 0 
			|| pos_(tofrmt, "::GCAL", &c__1, (ftnlen)256, (ftnlen)
			6) != 0 || pos_(tofrmt, "::JCAL", &c__1, (ftnlen)256, 
			(ftnlen)6) != 0 || pos_(tofrmt, "::MCAL", &c__1, (
			ftnlen)256, (ftnlen)6) != 0 || pos_(tofrmt, "::TDT", &
			c__1, (ftnlen)256, (ftnlen)5) != 0) {
		    setmsg_("Output format picture '#' which is supposed to "
			    "be a suitable for formating time as UTC contains"
			    " TDB, TDT, GCAL, JCAL or MCAL token which tells "
			    "SPICE time subsystem to output time in the corre"
			    "sponding non-UTC time system.", (ftnlen)220);
		    sigerr_("SPICE(MISMATCHOUTPUTFORMAT)", (ftnlen)27);
		    chkout_("CRONOS", (ftnlen)6);
		    return 0;
		}
		timout_(&et, tofrmt, outime, (ftnlen)256, (ftnlen)256);
	    } else if (s_cmp(tott, "LT", (ftnlen)32, (ftnlen)2) == 0) {

/*              For LT output is a DP number ... but still stored in */
/*              string. */

		dpfmt_(&lt, tofrmt, outime, (ftnlen)256, (ftnlen)256);
	    }

/*        Second case is ET. */

	} else if (s_cmp(tots, "ET", (ftnlen)32, (ftnlen)2) == 0) {

/*           Convert ET to ET. Silly, huh? :) Not at all :) Sometime we */
/*           even need to compute something depending on type. */

	    if (s_cmp(tott, "ERT", (ftnlen)32, (ftnlen)3) == 0) {

/*              For the ERT type, we need to adjust our ET time to be on */
/*              the Earth instead of the spacecraft. Call LTIME to do */
/*              it. */

		ltime_(&et, &scid, "->", &c__399, &ettemp, &lt, (ftnlen)2);
		et = ettemp;
	    } else if (s_cmp(tott, "ETT", (ftnlen)32, (ftnlen)3) == 0) {

/*              For the ETT type, we need to adjust our ET time to be on */
/*              the Earth instead of the spacecraft. Call LTIME to do it */
/*              but use light direction opposite to ERT. */

		ltime_(&et, &scid, "<-", &c__399, &ettemp, &lt, (ftnlen)2);
		et = ettemp;
	    } else if (s_cmp(tott, "LT", (ftnlen)32, (ftnlen)2) == 0) {

/*              For the LT type, we need to compute it. Call LTIME for */
/*              that. */

		ltime_(&et, &scid, "->", &c__399, &ettemp, &lt, (ftnlen)2);
		et = ettemp;
	    } else if (s_cmp(tott, "SECONDS", (ftnlen)32, (ftnlen)7) == 0) {

/*              Nothing for the seconds. */

	    } else if (s_cmp(tott, "SCET", (ftnlen)32, (ftnlen)4) == 0) {

/*              And also nothing for ET-SCET. */

	    } else {
		setmsg_("How did you manage to get to this place in the prog"
			"ram?", (ftnlen)55);
		sigerr_("SPICE(CHRONOSBUG6)", (ftnlen)18);
		chkout_("CRONOS", (ftnlen)6);
		return 0;
	    }

/*           What about formatting output? */

	    if (s_cmp(tott, "SCET", (ftnlen)32, (ftnlen)4) == 0 || s_cmp(tott,
		     "ETT", (ftnlen)32, (ftnlen)3) == 0 || s_cmp(tott, "ERT", 
		    (ftnlen)32, (ftnlen)3) == 0) {

/*              SCET, ERT and ETT are to be formatted as time string. */

/*              Same story as for output UTC above -- check format for */
/*              stupid errors and go ahead with TIMOUT. */

		if (pos_(tofrmt, "::UTC", &c__1, (ftnlen)256, (ftnlen)5) != 0 
			|| pos_(tofrmt, "::GCAL", &c__1, (ftnlen)256, (ftnlen)
			6) != 0 || pos_(tofrmt, "::JCAL", &c__1, (ftnlen)256, 
			(ftnlen)6) != 0 || pos_(tofrmt, "::MCAL", &c__1, (
			ftnlen)256, (ftnlen)6) != 0 || pos_(tofrmt, "::TDT", &
			c__1, (ftnlen)256, (ftnlen)5) != 0) {
		    setmsg_("Output format picture '#' which is supposed to "
			    "be a suitable for formating time as ET contains "
			    "UTC, TDT, GCAL, JCAL or MCAL token which will te"
			    "ll SPICE time subsystem to output time in the co"
			    "rresponding non-ET time system.", (ftnlen)222);
		    sigerr_("SPICE(MISMATCHOUTPUTFORMAT)", (ftnlen)27);
		    chkout_("CRONOS", (ftnlen)6);
		    return 0;
		}
		if (pos_(tofrmt, "::TDB", &c__1, (ftnlen)256, (ftnlen)5) == 0)
			 {
/* Writing concatenation */
		    i__3[0] = rtrim_(tofrmt, (ftnlen)256), a__2[0] = tofrmt;
		    i__3[1] = 6, a__2[1] = " ::TDB";
		    s_cat(tofrmh, a__2, i__3, &c__2, (ftnlen)256);
		} else {
		    s_copy(tofrmh, tofrmt, (ftnlen)256, rtrim_(tofrmt, (
			    ftnlen)256));
		}
		timout_(&et, tofrmh, outime, (ftnlen)256, (ftnlen)256);
	    } else if (s_cmp(tott, "LT", (ftnlen)32, (ftnlen)2) == 0) {

/*              Store DP number in output string for LT. */

		dpfmt_(&lt, tofrmt, outime, (ftnlen)256, (ftnlen)256);
	    } else if (s_cmp(tott, "SECONDS", (ftnlen)32, (ftnlen)7) == 0) {

/*              Do the same as for ET seconds. */

		dpfmt_(&et, tofrmt, outime, (ftnlen)256, (ftnlen)256);
	    }

/*        SCLK case goes third. */

	} else if (s_cmp(tots, "SCLK", (ftnlen)32, (ftnlen)4) == 0) {

/*           Convert SCLK to ET. Any additional computations/ */
/*           formatting? */

	    if (s_cmp(tott, "SCLK", (ftnlen)32, (ftnlen)4) == 0) {

/*              Output is plain SCLK. This is the easiest one. */

		sce2t_(&scid, &et, &sclkdp);
		scdecd_(&scid, &sclkdp, outime, (ftnlen)256);
	    } else if (s_cmp(tott, "HEX", (ftnlen)32, (ftnlen)3) == 0) {

/*              It's a little more headache with outputting HEX. But */
/*              it's definitely doable :) */

		sce2t_(&scid, &et, &sclkdp);
		scdecd_(&scid, &sclkdp, outime, (ftnlen)256);

/*              Slash is always these and in the right place, no checks */
/*              for that. Just go ahead -- parse, convert to HEX and */
/*              rebuild the string */

		i__4 = pos_(outime, "/", &c__1, (ftnlen)256, (ftnlen)1);
		lparsm_(outime + i__4, " -.,:", &c__10, &n, fields, 256 - 
			i__4, (ftnlen)5, (ftnlen)32);
		s_copy(hline, outime, (ftnlen)256, pos_(outime, "/", &c__1, (
			ftnlen)256, (ftnlen)1));
		i__4 = n;
		for (i__ = 1; i__ <= i__4; ++i__) {
		    nparsi_(fields + (((i__5 = i__ - 1) < 10 && 0 <= i__5 ? 
			    i__5 : s_rnge("fields", i__5, "cronos_", (ftnlen)
			    2022)) << 5), &j, error, &n, (ftnlen)32, (ftnlen)
			    256);
		    int2hx_(&j, hword, &n, (ftnlen)32);
		    if (i__ == 1) {
/* Writing concatenation */
			i__3[0] = rtrim_(hline, (ftnlen)256), a__2[0] = hline;
			i__3[1] = 32, a__2[1] = hword;
			s_cat(hline, a__2, i__3, &c__2, (ftnlen)256);
		    } else {
/* Writing concatenation */
			i__6[0] = rtrim_(hline, (ftnlen)256), a__3[0] = hline;
			i__6[1] = 1, a__3[1] = ".";
			i__6[2] = 32, a__3[2] = hword;
			s_cat(hline, a__3, i__6, &c__3, (ftnlen)256);
		    }
		}
		s_copy(outime, hline, (ftnlen)256, (ftnlen)256);
	    } else if (s_cmp(tott, "TICKS", (ftnlen)32, (ftnlen)5) == 0) {

/*              Ticks is easy too. But let's do them as integer. */

		sce2c_(&scid, &et, &sclkdp);
		dpfmt_(&sclkdp, tofrmt, outime, (ftnlen)256, (ftnlen)256);
	    } else {
		setmsg_("How did you manage to get to this place in the prog"
			"ram?", (ftnlen)55);
		sigerr_("SPICE(CHRONOSBUG7)", (ftnlen)18);
	    }

/*        At last, output LST case. */

	} else if (s_cmp(tots, "LST", (ftnlen)32, (ftnlen)3) == 0) {

/*           Any additional computation depending on type? */

	    if (s_cmp(tott, "LST", (ftnlen)32, (ftnlen)3) == 0) {

/*              Convert LST to ET. */

		if (s_cmp(lsttim, " ", (ftnlen)32, (ftnlen)1) != 0 && et >= 
			lstet) {

/*                 We can compute SOL day number if we know landing time */
/*                 and our ET after the landing time. We find midnight, */
/*                 previous to this ET. */

		    lstmid_(&et, &bodyid, &lon, &scrate, "PREVIOUS", &c__0, &
			    etout, (ftnlen)8);

/*                 How many days since first SOL midnight? */

		    hdp = (etout - midnet) / (spd_() * scrate);
		    if (d_mod(&hdp, &c_b511) > .5) {
			solday = (integer) hdp + solzer + 1;
		    } else {
			solday = (integer) hdp + solzer;
		    }

/*                 Put SOL day number into the output string. */

		    s_copy(outime, "# # #", (ftnlen)256, (ftnlen)5);
		    repmc_(outime, "#", "SOL", outime, (ftnlen)256, (ftnlen)1,
			     (ftnlen)3, (ftnlen)256);
		    repmi_(outime, "#", &solday, outime, (ftnlen)256, (ftnlen)
			    1, (ftnlen)256);
		} else {

/*                 No SOL day ... how pity :). Let prepare all things to */
/*                 compute at least relative LST is sub-spacecraft */
/*                 point. */

		    bodc2n_(&bodyid, hword, &found, (ftnlen)32);
		    if (! found) {
			setmsg_("Cannot recognize body ID #.", (ftnlen)27);
			errint_("#", &bodyid, (ftnlen)1);
			sigerr_("SPICE(BADBODYID)", (ftnlen)16);
			chkout_("CRONOS", (ftnlen)6);
			return 0;
		    }
/* Writing concatenation */
		    i__3[0] = 4, a__2[0] = "IAU_";
		    i__3[1] = 32, a__2[1] = hword;
		    s_cat(hline, a__2, i__3, &c__2, (ftnlen)256);
		    spkez_(&scid, &et, hline, "LT+S", &bodyid, state, &lt, (
			    ftnlen)256, (ftnlen)4);
		    reclat_(state, &r__, &lon, &lat);

/*                 And set initial string for output time. */

		    s_copy(outime, "#", (ftnlen)256, (ftnlen)1);
		}

/*              Now, get 24 hours clock readout from ET2LST. */

		et2lst_(&et, &bodyid, &lon, "PLANETOCENTRIC", &hr, &mn, &sc, 
			hword, hline, (ftnlen)14, (ftnlen)32, (ftnlen)256);

/*              Final touch -- inserting local clock to output string */
/*              :). */

		repmc_(outime, "#", hword, outime, (ftnlen)256, (ftnlen)1, (
			ftnlen)32, (ftnlen)256);
	    } else if (s_cmp(tott, "LSUN", (ftnlen)32, (ftnlen)4) == 0) {

/*              We need to compute longitude if the Sun -- measure of */
/*              the seasons on a planet. We just call Nat's routine to */
/*              do that. */

		hdp = ls_(&bodyid, &et, "LT+S", (ftnlen)4) * dpr_();

/*              Store DP longitude of the Sun in output string. */

		dpfmt_(&hdp, tofrmt, outime, (ftnlen)256, (ftnlen)256);
	    } else {
		setmsg_("How did you manage to get to this place in the prog"
			"ram?", (ftnlen)55);
		sigerr_("SPICE(CHRONOSBUG10)", (ftnlen)19);
		chkout_("CRONOS", (ftnlen)6);
		return 0;
	    }
	} else {
	    setmsg_("How did you manage to get to this place in the program?",
		     (ftnlen)55);
	    sigerr_("SPICE(CHRONOSBUG8)", (ftnlen)18);
	    chkout_("CRONOS", (ftnlen)6);
	    return 0;
	}

/*        Conversion is done. Assign corresponding element of the output */
/*        times array. */

	s_copy(outtim + (count - 1) * outtim_len, outime, outtim_len, (ftnlen)
		256);

/*        Increment the number of iterations with this command line. */

	++nloops;

/*        End of the main loop processing input times. */

    }

/*     All done. */

    chkout_("CRONOS", (ftnlen)6);
    return 0;
} /* cronos_ */

