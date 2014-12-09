/* dsplay.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__4 = 4;
static integer c__26 = 26;
static integer c__18 = 18;
static integer c__54 = 54;

/* $Procedure      DSPLAY( Display static descriptive information ) */
/* Subroutine */ int dsplay_(char *what, char *action, ftnlen what_len, 
	ftnlen action_len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_stop(char *, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), repmc_(char *, char *,
	     char *, char *, ftnlen, ftnlen, ftnlen, ftnlen), stdio_(char *, 
	    integer *, ftnlen);
    char hlpmsg[80*18], vermsg[80*4], usgmsg[80*26], tmlmsg[80*54];
    integer stdout;
    extern logical return_(void);
    extern /* Subroutine */ int writla_(integer *, char *, integer *, ftnlen),
	     chkout_(char *, ftnlen);

/* $ Abstract */

/*     Displays CHRONOS usage, help or template information and stops */
/*     the program if needed. */

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

/*     None */

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
/*     WHAT       I   "What should be displayed" key. */
/*     ACTION     I   Stop/continue key. */

/* $ Detailed_Input */

/*     WHAT           Key specifying what should be displayed. The value */
/*                    can be 'VERSION', 'USAGE', 'HELP' or 'TEMPLATE'. */

/*     ACTION         Key specifying whether program should proceed */
/*                    or stop ('STOP', 'PROCEED') */

/* $ Detailed_Output */

/*     None. */

/*     The routine prints requested message to STDOUT and stops (or not) */
/*     depending on the requested action. */

/* $ Parameters */

/*     See the include file. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Particulars */

/*     If WHAT value is not one of the values listed in Detailed_Input, */
/*     then no message is displayed. */

/*     If ACTION value is not 'STOP', then the routine does not stop the */
/*     program. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     B.V.Semenov      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    CHRONOS Version 2.0.0, 03-JAN-2002 (BVS) */

/*        Updated usage and template displays to reflect addition of */
/*        the new command line switches and FURNSH capability in the */
/*        calling program. */

/* -    CHRONOS Version 1.2.2, 03-MAY-2001 (BVS) */

/*        Fixed "disappearing" backslashes in template output. */

/* -    CHRONOS Version 1.0.0, 13-MAY-1998 (BVS) */

/* -& */

/*     Local parameters. */


/*     Backslash character ord. */


/*     Local variables. */


/*     SPICELIB function. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DSPLAY", (ftnlen)6);
    }

/*     Version display. */

    s_copy(vermsg, " ", (ftnlen)80, (ftnlen)1);
    s_copy(vermsg + 80, "CHRONOS -- Universal Time Conversion Tool", (ftnlen)
	    80, (ftnlen)41);
    s_copy(vermsg + 160, "Version 2.2.0, October 28, 2011", (ftnlen)80, (
	    ftnlen)31);
    s_copy(vermsg + 240, " ", (ftnlen)80, (ftnlen)1);

/*     Usage display. */

    s_copy(usgmsg, "CHRONOS Usage", (ftnlen)80, (ftnlen)13);
    s_copy(usgmsg + 80, "---------------------------------------------------"
	    "---------------", (ftnlen)80, (ftnlen)66);
    s_copy(usgmsg + 160, " ", (ftnlen)80, (ftnlen)1);
    s_copy(usgmsg + 240, "   To convert time from one supported system/type "
	    "to another:", (ftnlen)80, (ftnlen)61);
    s_copy(usgmsg + 320, " ", (ftnlen)80, (ftnlen)1);
    s_copy(usgmsg + 400, "      % CHRONOS # <setup file name OR kernel file "
	    "name(s)>", (ftnlen)80, (ftnlen)58);
    s_copy(usgmsg + 480, "                # <\"from\" time system>", (ftnlen)
	    80, (ftnlen)38);
    s_copy(usgmsg + 560, "               [# <\"from\" time type>]", (ftnlen)
	    80, (ftnlen)37);
    s_copy(usgmsg + 640, "                # <\"to\" time system>", (ftnlen)80,
	     (ftnlen)36);
    s_copy(usgmsg + 720, "               [# <\"to\" time type>]", (ftnlen)80, 
	    (ftnlen)35);
    s_copy(usgmsg + 800, "               [# <output time format picture>]", (
	    ftnlen)80, (ftnlen)47);
    s_copy(usgmsg + 880, "                # <input time> | #", (ftnlen)80, (
	    ftnlen)34);
    s_copy(usgmsg + 960, "               [# <sc ID>]", (ftnlen)80, (ftnlen)26)
	    ;
    s_copy(usgmsg + 1040, "               [# <cental body ID>]", (ftnlen)80, (
	    ftnlen)35);
    s_copy(usgmsg + 1120, "               [# <UTC time of the landing>]", (
	    ftnlen)80, (ftnlen)44);
    s_copy(usgmsg + 1200, "               [# <index of the first SOL>]", (
	    ftnlen)80, (ftnlen)43);
    s_copy(usgmsg + 1280, "               [#]", (ftnlen)80, (ftnlen)18);
    s_copy(usgmsg + 1360, "               [#]", (ftnlen)80, (ftnlen)18);
    s_copy(usgmsg + 1440, " ", (ftnlen)80, (ftnlen)1);
    s_copy(usgmsg + 1520, "   To display usage:               % CHRONOS [#|#]"
	    , (ftnlen)80, (ftnlen)50);
    s_copy(usgmsg + 1600, "   To display help:                % CHRONOS #|#", 
	    (ftnlen)80, (ftnlen)48);
    s_copy(usgmsg + 1680, "   To display setup file template: % CHRONOS #", (
	    ftnlen)80, (ftnlen)46);
    s_copy(usgmsg + 1760, " ", (ftnlen)80, (ftnlen)1);
    s_copy(usgmsg + 1840, "   The case of command line switches is insignifi"
	    "cant. Switches shown within ", (ftnlen)80, (ftnlen)77);
    s_copy(usgmsg + 1920, "   square braces [] are optional. See User's Guid"
	    "e for details on # usage.", (ftnlen)80, (ftnlen)74);
    s_copy(usgmsg + 2000, " ", (ftnlen)80, (ftnlen)1);
    repmc_(usgmsg + 400, "#", "-SETUP", usgmsg + 400, (ftnlen)80, (ftnlen)1, (
	    ftnlen)6, (ftnlen)80);
    repmc_(usgmsg + 480, "#", "-FROM", usgmsg + 480, (ftnlen)80, (ftnlen)1, (
	    ftnlen)5, (ftnlen)80);
    repmc_(usgmsg + 560, "#", "-FROMTYPE", usgmsg + 560, (ftnlen)80, (ftnlen)
	    1, (ftnlen)9, (ftnlen)80);
    repmc_(usgmsg + 640, "#", "-TO", usgmsg + 640, (ftnlen)80, (ftnlen)1, (
	    ftnlen)3, (ftnlen)80);
    repmc_(usgmsg + 720, "#", "-TOTYPE", usgmsg + 720, (ftnlen)80, (ftnlen)1, 
	    (ftnlen)7, (ftnlen)80);
    repmc_(usgmsg + 800, "#", "-FORMAT", usgmsg + 800, (ftnlen)80, (ftnlen)1, 
	    (ftnlen)7, (ftnlen)80);
    repmc_(usgmsg + 880, "#", "-TIME", usgmsg + 880, (ftnlen)80, (ftnlen)1, (
	    ftnlen)5, (ftnlen)80);
    repmc_(usgmsg + 880, "#", "-BATCH", usgmsg + 880, (ftnlen)80, (ftnlen)1, (
	    ftnlen)6, (ftnlen)80);
    repmc_(usgmsg + 960, "#", "-SC", usgmsg + 960, (ftnlen)80, (ftnlen)1, (
	    ftnlen)3, (ftnlen)80);
    repmc_(usgmsg + 1040, "#", "-CENTER", usgmsg + 1040, (ftnlen)80, (ftnlen)
	    1, (ftnlen)7, (ftnlen)80);
    repmc_(usgmsg + 1120, "#", "-LANDINGTIME", usgmsg + 1120, (ftnlen)80, (
	    ftnlen)1, (ftnlen)12, (ftnlen)80);
    repmc_(usgmsg + 1200, "#", "-SOL1INDEX", usgmsg + 1200, (ftnlen)80, (
	    ftnlen)1, (ftnlen)10, (ftnlen)80);
    repmc_(usgmsg + 1280, "#", "-NOLABEL", usgmsg + 1280, (ftnlen)80, (ftnlen)
	    1, (ftnlen)8, (ftnlen)80);
    repmc_(usgmsg + 1360, "#", "-TRACE", usgmsg + 1360, (ftnlen)80, (ftnlen)1,
	     (ftnlen)6, (ftnlen)80);
    repmc_(usgmsg + 1520, "#", "-HELP", usgmsg + 1520, (ftnlen)80, (ftnlen)1, 
	    (ftnlen)5, (ftnlen)80);
    repmc_(usgmsg + 1520, "#", "-H", usgmsg + 1520, (ftnlen)80, (ftnlen)1, (
	    ftnlen)2, (ftnlen)80);
    repmc_(usgmsg + 1600, "#", "-USAGE", usgmsg + 1600, (ftnlen)80, (ftnlen)1,
	     (ftnlen)6, (ftnlen)80);
    repmc_(usgmsg + 1600, "#", "-U", usgmsg + 1600, (ftnlen)80, (ftnlen)1, (
	    ftnlen)2, (ftnlen)80);
    repmc_(usgmsg + 1680, "#", "-TEMPLATE", usgmsg + 1680, (ftnlen)80, (
	    ftnlen)1, (ftnlen)9, (ftnlen)80);
    repmc_(usgmsg + 1920, "#", "-BATCH", usgmsg + 1920, (ftnlen)80, (ftnlen)1,
	     (ftnlen)6, (ftnlen)80);

/*     Help display. */

    s_copy(hlpmsg, "CHRONOS Help", (ftnlen)80, (ftnlen)12);
    s_copy(hlpmsg + 80, "---------------------------------------------------"
	    "---------------", (ftnlen)80, (ftnlen)66);
    s_copy(hlpmsg + 160, " ", (ftnlen)80, (ftnlen)1);
    s_copy(hlpmsg + 240, "CHRONOS is a time conversion tool capable of conve"
	    "rting times", (ftnlen)80, (ftnlen)61);
    s_copy(hlpmsg + 320, "between the following time systems:", (ftnlen)80, (
	    ftnlen)35);
    s_copy(hlpmsg + 400, " ", (ftnlen)80, (ftnlen)1);
    s_copy(hlpmsg + 480, "    # -- Universal Time Coordinated (types: #, #, "
	    "#, #)", (ftnlen)80, (ftnlen)55);
    s_copy(hlpmsg + 560, "    # -- Ephemeris Time (types: #, #, #, #, #)", (
	    ftnlen)80, (ftnlen)46);
    s_copy(hlpmsg + 640, "    # -- Spacecraft On-board Clock Time (types: #,"
	    " #, #)", (ftnlen)80, (ftnlen)56);
    s_copy(hlpmsg + 720, "    # -- Local True Solar Time (types: #, #)", (
	    ftnlen)80, (ftnlen)44);
    s_copy(hlpmsg + 800, " ", (ftnlen)80, (ftnlen)1);
    s_copy(hlpmsg + 880, "CHRONOS takes inputs from the command line. Run CH"
	    "RONOS with the ", (ftnlen)80, (ftnlen)65);
    s_copy(hlpmsg + 960, "\"#\" switch to display usage information.", (
	    ftnlen)80, (ftnlen)40);
    s_copy(hlpmsg + 1040, " ", (ftnlen)80, (ftnlen)1);
    s_copy(hlpmsg + 1120, "Although not required, CHRONOS allows certain par"
	    "ameters to be ", (ftnlen)80, (ftnlen)63);
    s_copy(hlpmsg + 1200, "provided via a setup file. Run CHRONOS with the"
	    " \"#\" switch ", (ftnlen)80, (ftnlen)59);
    s_copy(hlpmsg + 1280, "to display setup file template. ", (ftnlen)80, (
	    ftnlen)32);
    s_copy(hlpmsg + 1360, " ", (ftnlen)80, (ftnlen)1);
    repmc_(hlpmsg + 480, "#", "UTC", hlpmsg + 480, (ftnlen)80, (ftnlen)1, (
	    ftnlen)3, (ftnlen)80);
    repmc_(hlpmsg + 480, "#", "SCET", hlpmsg + 480, (ftnlen)80, (ftnlen)1, (
	    ftnlen)4, (ftnlen)80);
    repmc_(hlpmsg + 480, "#", "ERT", hlpmsg + 480, (ftnlen)80, (ftnlen)1, (
	    ftnlen)3, (ftnlen)80);
    repmc_(hlpmsg + 480, "#", "ETT", hlpmsg + 480, (ftnlen)80, (ftnlen)1, (
	    ftnlen)3, (ftnlen)80);
    repmc_(hlpmsg + 480, "#", "LT", hlpmsg + 480, (ftnlen)80, (ftnlen)1, (
	    ftnlen)2, (ftnlen)80);
    repmc_(hlpmsg + 560, "#", "ET", hlpmsg + 560, (ftnlen)80, (ftnlen)1, (
	    ftnlen)2, (ftnlen)80);
    repmc_(hlpmsg + 560, "#", "SCET", hlpmsg + 560, (ftnlen)80, (ftnlen)1, (
	    ftnlen)4, (ftnlen)80);
    repmc_(hlpmsg + 560, "#", "ERT", hlpmsg + 560, (ftnlen)80, (ftnlen)1, (
	    ftnlen)3, (ftnlen)80);
    repmc_(hlpmsg + 560, "#", "ETT", hlpmsg + 560, (ftnlen)80, (ftnlen)1, (
	    ftnlen)3, (ftnlen)80);
    repmc_(hlpmsg + 560, "#", "LT", hlpmsg + 560, (ftnlen)80, (ftnlen)1, (
	    ftnlen)2, (ftnlen)80);
    repmc_(hlpmsg + 560, "#", "SECONDS", hlpmsg + 560, (ftnlen)80, (ftnlen)1, 
	    (ftnlen)7, (ftnlen)80);
    repmc_(hlpmsg + 640, "#", "SCLK", hlpmsg + 640, (ftnlen)80, (ftnlen)1, (
	    ftnlen)4, (ftnlen)80);
    repmc_(hlpmsg + 640, "#", "SCLK", hlpmsg + 640, (ftnlen)80, (ftnlen)1, (
	    ftnlen)4, (ftnlen)80);
    repmc_(hlpmsg + 640, "#", "HEX", hlpmsg + 640, (ftnlen)80, (ftnlen)1, (
	    ftnlen)3, (ftnlen)80);
    repmc_(hlpmsg + 640, "#", "TICKS", hlpmsg + 640, (ftnlen)80, (ftnlen)1, (
	    ftnlen)5, (ftnlen)80);
    repmc_(hlpmsg + 720, "#", "LST", hlpmsg + 720, (ftnlen)80, (ftnlen)1, (
	    ftnlen)3, (ftnlen)80);
    repmc_(hlpmsg + 720, "#", "LST", hlpmsg + 720, (ftnlen)80, (ftnlen)1, (
	    ftnlen)3, (ftnlen)80);
    repmc_(hlpmsg + 720, "#", "LSUN", hlpmsg + 720, (ftnlen)80, (ftnlen)1, (
	    ftnlen)4, (ftnlen)80);
    repmc_(hlpmsg + 960, "#", "-USAGE", hlpmsg + 960, (ftnlen)80, (ftnlen)1, (
	    ftnlen)6, (ftnlen)80);
    repmc_(hlpmsg + 1200, "#", "-TEMPLATE", hlpmsg + 1200, (ftnlen)80, (
	    ftnlen)1, (ftnlen)9, (ftnlen)80);

/*     Template diplay */

    s_copy(tmlmsg, "CHRONOS Setup File", (ftnlen)80, (ftnlen)18);
    s_copy(tmlmsg + 80, "---------------------------------------------------"
	    "---------------", (ftnlen)80, (ftnlen)66);
    s_copy(tmlmsg + 160, " ", (ftnlen)80, (ftnlen)1);
    s_copy(tmlmsg + 240, "CHRONOS allows a few parameters to be provided in "
	    "a setup file.", (ftnlen)80, (ftnlen)63);
    s_copy(tmlmsg + 320, "The setup file format should correspond to the SPI"
	    "CE Kernel Text", (ftnlen)80, (ftnlen)64);
    s_copy(tmlmsg + 400, "file format specification, i.e. it must contain da"
	    "ta formatted as", (ftnlen)80, (ftnlen)65);
    s_copy(tmlmsg + 480, "a set of KEYWORD=VALUE assignments enclosed between"
	    , (ftnlen)80, (ftnlen)51);
    s_copy(tmlmsg + 560, " ", (ftnlen)80, (ftnlen)1);
    s_copy(tmlmsg + 640, "   \\begindata", (ftnlen)80, (ftnlen)13);
    s_copy(tmlmsg + 720, "   \\begintext", (ftnlen)80, (ftnlen)13);
    s_copy(tmlmsg + 800, " ", (ftnlen)80, (ftnlen)1);
    s_copy(tmlmsg + 880, "markers. Each assignment and marker must be on a l"
	    "ine by itself.", (ftnlen)80, (ftnlen)64);
    s_copy(tmlmsg + 960, " ", (ftnlen)80, (ftnlen)1);
    s_copy(tmlmsg + 1040, "The following parameters may be provided in a set"
	    "up file:", (ftnlen)80, (ftnlen)57);
    s_copy(tmlmsg + 1120, " ", (ftnlen)80, (ftnlen)1);
    s_copy(tmlmsg + 1200, "   \\begindata", (ftnlen)80, (ftnlen)13);
    s_copy(tmlmsg + 1280, "      # = 'name of a LSK file'", (ftnlen)80, (
	    ftnlen)30);
    s_copy(tmlmsg + 1360, "      # = 'name of a SCLK file for the mission'", (
	    ftnlen)80, (ftnlen)47);
    s_copy(tmlmsg + 1440, "      # = 'name of a PCK file'", (ftnlen)80, (
	    ftnlen)30);
    s_copy(tmlmsg + 1520, "      # = ( 'name of an SPK file', '...' )", (
	    ftnlen)80, (ftnlen)42);
    s_copy(tmlmsg + 1600, "      # = ( 'name of a CK file', '...' )", (ftnlen)
	    80, (ftnlen)40);
    s_copy(tmlmsg + 1680, "      # = 'name of a frame definitions file'", (
	    ftnlen)80, (ftnlen)44);
    s_copy(tmlmsg + 1760, "      # = NAIF ID for the spacecraft", (ftnlen)80, 
	    (ftnlen)36);
    s_copy(tmlmsg + 1840, "      # = NAIF ID for the center body", (ftnlen)80,
	     (ftnlen)37);
    s_copy(tmlmsg + 1920, "      # = 'UTC time of the landing'", (ftnlen)80, (
	    ftnlen)35);
    s_copy(tmlmsg + 2000, "      # = SOL index of the landing", (ftnlen)80, (
	    ftnlen)34);
    s_copy(tmlmsg + 2080, "   \\begintext", (ftnlen)80, (ftnlen)13);
    s_copy(tmlmsg + 2160, " ", (ftnlen)80, (ftnlen)1);
    s_copy(tmlmsg + 2240, "Note that either or all of the #, #, #, ", (ftnlen)
	    80, (ftnlen)40);
    s_copy(tmlmsg + 2320, "and # parameters can also be provided using the c"
	    "ommand", (ftnlen)80, (ftnlen)55);
    s_copy(tmlmsg + 2400, "line switches. If done so, the setup file value c"
	    "orresponding to a command ", (ftnlen)80, (ftnlen)75);
    s_copy(tmlmsg + 2480, "line value is not needed, and, if present, is ign"
	    "ored by the program.", (ftnlen)80, (ftnlen)69);
    s_copy(tmlmsg + 2560, " ", (ftnlen)80, (ftnlen)1);
    s_copy(tmlmsg + 2640, "Similarly, the kernels files to be loaded can be "
	    "provided using the ", (ftnlen)80, (ftnlen)68);
    s_copy(tmlmsg + 2720, "standard SPICE interface -- with the KERNELS_TO_L"
	    "OAD parameter:", (ftnlen)80, (ftnlen)63);
    s_copy(tmlmsg + 2800, " ", (ftnlen)80, (ftnlen)1);
    s_copy(tmlmsg + 2880, "   \\begindata", (ftnlen)80, (ftnlen)13);
    s_copy(tmlmsg + 2960, "      KERNELS_TO_LOAD = ( ", (ftnlen)80, (ftnlen)
	    26);
    s_copy(tmlmsg + 3040, "                    'name of a LSK file',", (
	    ftnlen)80, (ftnlen)41);
    s_copy(tmlmsg + 3120, "                    'name of a SCLK file ',", (
	    ftnlen)80, (ftnlen)43);
    s_copy(tmlmsg + 3200, "                    'name of a PCK file',", (
	    ftnlen)80, (ftnlen)41);
    s_copy(tmlmsg + 3280, "                    'name of an SPK file',", (
	    ftnlen)80, (ftnlen)42);
    s_copy(tmlmsg + 3360, "                    '...',", (ftnlen)80, (ftnlen)
	    26);
    s_copy(tmlmsg + 3440, "                    'name of a CK file',", (ftnlen)
	    80, (ftnlen)40);
    s_copy(tmlmsg + 3520, "                    '...',", (ftnlen)80, (ftnlen)
	    26);
    s_copy(tmlmsg + 3600, "                    'name of an FK file'", (ftnlen)
	    80, (ftnlen)40);
    s_copy(tmlmsg + 3680, "                        )", (ftnlen)80, (ftnlen)25)
	    ;
    s_copy(tmlmsg + 3760, "   \\begintext", (ftnlen)80, (ftnlen)13);
    s_copy(tmlmsg + 3840, " ", (ftnlen)80, (ftnlen)1);
    s_copy(tmlmsg + 3920, "or even by simply listing them after the # comman"
	    "d line switch. In", (ftnlen)80, (ftnlen)66);
    s_copy(tmlmsg + 4000, "either of these two cases, specifying the #, #, ", 
	    (ftnlen)80, (ftnlen)48);
    s_copy(tmlmsg + 4080, "#, #, #, and # setup file parameters ", (ftnlen)80,
	     (ftnlen)37);
    s_copy(tmlmsg + 4160, "is not necessary.", (ftnlen)80, (ftnlen)17);
    s_copy(tmlmsg + 4240, " ", (ftnlen)80, (ftnlen)1);
    repmc_(tmlmsg + 1280, "#", "LEAPSECONDS_FILE", tmlmsg + 1280, (ftnlen)80, 
	    (ftnlen)1, (ftnlen)16, (ftnlen)80);
    repmc_(tmlmsg + 1360, "#", "SCLK_FILE", tmlmsg + 1360, (ftnlen)80, (
	    ftnlen)1, (ftnlen)9, (ftnlen)80);
    repmc_(tmlmsg + 1440, "#", "PCK_FILE", tmlmsg + 1440, (ftnlen)80, (ftnlen)
	    1, (ftnlen)8, (ftnlen)80);
    repmc_(tmlmsg + 1520, "#", "SPK_FILES", tmlmsg + 1520, (ftnlen)80, (
	    ftnlen)1, (ftnlen)9, (ftnlen)80);
    repmc_(tmlmsg + 1600, "#", "CK_FILES", tmlmsg + 1600, (ftnlen)80, (ftnlen)
	    1, (ftnlen)8, (ftnlen)80);
    repmc_(tmlmsg + 1680, "#", "FRAMES_FILE", tmlmsg + 1680, (ftnlen)80, (
	    ftnlen)1, (ftnlen)11, (ftnlen)80);
    repmc_(tmlmsg + 1760, "#", "SPACECRAFT_ID", tmlmsg + 1760, (ftnlen)80, (
	    ftnlen)1, (ftnlen)13, (ftnlen)80);
    repmc_(tmlmsg + 1840, "#", "CENTER_ID", tmlmsg + 1840, (ftnlen)80, (
	    ftnlen)1, (ftnlen)9, (ftnlen)80);
    repmc_(tmlmsg + 1920, "#", "LANDING_TIME", tmlmsg + 1920, (ftnlen)80, (
	    ftnlen)1, (ftnlen)12, (ftnlen)80);
    repmc_(tmlmsg + 2000, "#", "LANDING_SOL_INDEX", tmlmsg + 2000, (ftnlen)80,
	     (ftnlen)1, (ftnlen)17, (ftnlen)80);
    repmc_(tmlmsg + 2240, "#", "SPACECRAFT_ID", tmlmsg + 2240, (ftnlen)80, (
	    ftnlen)1, (ftnlen)13, (ftnlen)80);
    repmc_(tmlmsg + 2240, "#", "CENTER_ID", tmlmsg + 2240, (ftnlen)80, (
	    ftnlen)1, (ftnlen)9, (ftnlen)80);
    repmc_(tmlmsg + 2240, "#", "LANDING_TIME", tmlmsg + 2240, (ftnlen)80, (
	    ftnlen)1, (ftnlen)12, (ftnlen)80);
    repmc_(tmlmsg + 2320, "#", "LANDING_SOL_INDEX", tmlmsg + 2320, (ftnlen)80,
	     (ftnlen)1, (ftnlen)17, (ftnlen)80);
    repmc_(tmlmsg + 3920, "#", "-SETUP", tmlmsg + 3920, (ftnlen)80, (ftnlen)1,
	     (ftnlen)6, (ftnlen)80);
    repmc_(tmlmsg + 4000, "#", "LEAPSECONDS_FILE", tmlmsg + 4000, (ftnlen)80, 
	    (ftnlen)1, (ftnlen)16, (ftnlen)80);
    repmc_(tmlmsg + 4000, "#", "SCLK_FILE", tmlmsg + 4000, (ftnlen)80, (
	    ftnlen)1, (ftnlen)9, (ftnlen)80);
    repmc_(tmlmsg + 4080, "#", "PCK_FILE", tmlmsg + 4080, (ftnlen)80, (ftnlen)
	    1, (ftnlen)8, (ftnlen)80);
    repmc_(tmlmsg + 4080, "#", "SPK_FILES", tmlmsg + 4080, (ftnlen)80, (
	    ftnlen)1, (ftnlen)9, (ftnlen)80);
    repmc_(tmlmsg + 4080, "#", "CK_FILES", tmlmsg + 4080, (ftnlen)80, (ftnlen)
	    1, (ftnlen)8, (ftnlen)80);
    repmc_(tmlmsg + 4080, "#", "FRAMES_FILE", tmlmsg + 4080, (ftnlen)80, (
	    ftnlen)1, (ftnlen)11, (ftnlen)80);

/*     Get STDOUT */

    stdio_("STDOUT", &stdout, (ftnlen)6);

/*     What was requested for display? */

    if (s_cmp(what, "VERSION", what_len, (ftnlen)7) == 0) {

/*        Display version. */

	writla_(&c__4, vermsg, &stdout, (ftnlen)80);
    } else if (s_cmp(what, "USAGE", what_len, (ftnlen)5) == 0) {

/*        Display usage. */

	writla_(&c__4, vermsg, &stdout, (ftnlen)80);
	writla_(&c__26, usgmsg, &stdout, (ftnlen)80);
    } else if (s_cmp(what, "HELP", what_len, (ftnlen)4) == 0) {

/*        Display help. */

	writla_(&c__4, vermsg, &stdout, (ftnlen)80);
	writla_(&c__18, hlpmsg, &stdout, (ftnlen)80);
    } else if (s_cmp(what, "TEMPLATE", what_len, (ftnlen)8) == 0) {

/*        Display help. */

	writla_(&c__4, vermsg, &stdout, (ftnlen)80);
	writla_(&c__54, tmlmsg, &stdout, (ftnlen)80);
    }

/*     Should we stop the program? */

    if (s_cmp(action, "STOP", action_len, (ftnlen)4) == 0) {
	s_stop("", (ftnlen)0);
    }
    chkout_("DSPLAY", (ftnlen)6);
    return 0;
} /* dsplay_ */

