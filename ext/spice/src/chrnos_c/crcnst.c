/* crcnst.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $ Procedure     CRCNST ( CHRONOS Parameter Arrays ) */
/* Subroutine */ int crcnst_(char *systms, char *types, char *deftyp, logical 
	*systyp, logical *fmtted, char *fmtpic, char *clkeys, ftnlen 
	systms_len, ftnlen types_len, ftnlen deftyp_len, ftnlen fmtpic_len, 
	ftnlen clkeys_len)
{
    /* Initialized data */

    static char lsystm[32*4] = "UTC                             " "ET       "
	    "                       " "SCLK                            " "LST"
	    "                             ";
    static char ltypes[32*10] = "SCET                            " "ERT     "
	    "                        " "SCLK                            " 
	    "HEX                             " "TICKS                       "
	    "    " "LT                              " "SECONDS               "
	    "          " "LST                             " "ETT             "
	    "                " "LSUN                            ";
    static char ldefty[32*4] = "SCET                            " "SCET     "
	    "                       " "SCLK                            " "LST"
	    "                             ";
    static logical lsysty[40]	/* was [4][10] */ = { TRUE_,TRUE_,FALSE_,
	    FALSE_,TRUE_,TRUE_,FALSE_,FALSE_,FALSE_,FALSE_,TRUE_,FALSE_,
	    FALSE_,FALSE_,TRUE_,FALSE_,FALSE_,FALSE_,TRUE_,FALSE_,TRUE_,TRUE_,
	    FALSE_,FALSE_,FALSE_,TRUE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,
	    TRUE_,TRUE_,TRUE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,TRUE_ };
    static logical lfmtte[40]	/* was [4][10] */ = { TRUE_,TRUE_,FALSE_,
	    FALSE_,TRUE_,TRUE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,
	    FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,TRUE_,FALSE_,TRUE_,
	    TRUE_,FALSE_,FALSE_,FALSE_,TRUE_,FALSE_,FALSE_,FALSE_,FALSE_,
	    FALSE_,FALSE_,TRUE_,TRUE_,FALSE_,FALSE_,FALSE_,FALSE_,FALSE_,
	    TRUE_ };
    static char lfmtpi[64*4*10] = "YYYY-MM-DD HR:MN:SC.### ::RND            "
	    "                       " "YYYY-MM-DD, HR:MN:SC.### ::TDB ::RND  "
	    "                          " "                                   "
	    "                             " "                                "
	    "                                " "YYYY-MM-DD HR:MN:SC.### ::RND"
	    "                                   " "YYYY-MM-DD, HR:MN:SC.### :"
	    ":TDB ::RND                            " "                       "
	    "                                         " "                    "
	    "                                            " "                 "
	    "                                               " "              "
	    "                                                  " "           "
	    "                                                     " "        "
	    "                                                        " "     "
	    "                                                           " 
	    "                                                                "
	     "                                                              "
	    "  " "                                                           "
	    "     " "                                                        "
	    "        " "                                                     "
	    "           " "xxxxxxxxxxxxxxxx                                  "
	    "              " "                                               "
	    "                 " "xxxxxxxxxxxx.xxx                            "
	    "                    " "xxxxxxxxxxxx.xxx                         "
	    "                       " "                                      "
	    "                          " "                                   "
	    "                             " "                                "
	    "                                " "xxxxxxxxxxxxxxx.xxx          "
	    "                                   " "                          "
	    "                                      " "                       "
	    "                                         " "                    "
	    "                                            " "                 "
	    "                                               " "              "
	    "                                                  " "           "
	    "                                                     " "YYYY-MM-"
	    "DD HR:MN:SC.### ::RND                                   " "YYYY-"
	    "MM-DD, HR:MN:SC.### ::TDB ::RND                            " 
	    "                                                                "
	     "                                                              "
	    "  " "                                                           "
	    "     " "                                                        "
	    "        " "                                                     "
	    "           " "xxxxxx.xxx                                        "
	    "              ";
    static char lclkey[32*19] = "-SETUP                          " "-FROM   "
	    "                        " "-FROMTYPE                       " 
	    "-TO                             " "-TOTYPE                     "
	    "    " "-FORMAT                         " "-TIME                 "
	    "          " "-HELP                           " "-H              "
	    "                " "-USAGE                          " "-U        "
	    "                      " "-TEMPLATE                       " "-BAT"
	    "CH                          " "-NOLABEL                        " 
	    "-TRACE                          " "-SC                         "
	    "    " "-CENTER                         " "-LANDINGTIME          "
	    "          " "-SOL1INDEX                      ";

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static integer i__, j;

/* $ Abstract */

/*     Returns arrays with parameterized time systems/types, output */
/*     system/type formats, and command line keys used by CRONOS routine */
/*     and CHRONOS executable. */

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
/*     SYSTMS     O   Array of supported time systems. */
/*     TYPES      O   Array of supported time types. */
/*     DEFTYP     O   Array of default time types. */
/*     SYSTYP     O   Time system-type matrix. */
/*     FMTTED     O   Output format applicability matrix. */
/*     FMTPIC     O   Array of default output formats. */
/*     CLKEYS     O   Array of recongnized command line keys. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     SYSTMS         is the array of supported time systems. */

/*     TYPES          is the array of supported time types. */

/*     DEFTYP         is the array of default time types. */

/*     SYSTYP         is the time system-type matrix. */

/*     FMTTED         is the output format applicability matrix. */

/*     FMTPIC         is the array of default output formats. */

/*     CLKEYS         is the array of recongnized command line keys. */

/* $ Parameters */

/*     See CHRONOS include file. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Particulars */

/*     This routine provides consistent parameter arrays to CRONOS */
/*     routine and CHRONOS main module. In essence, it's just an */
/*     augmentation to the CHRONOS include file. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    CHRONOS Version 1.0.0, 17-DEC-2001 (BVS) */

/* -& */

/*     Local array declarations. */


/*     Save all local arrays. */


/*     The data. */


/*     Time systems */


/*     Types within systems. */


/*     Default types for the systems. */


/*     This array defines whether TYPE is applicable for a system. */


/*     This array defines whether an output of SYSTEM/TYPE can be */
/*     formatted. */


/*     This array defines default picture for output format */
/*     for a combination of SYSTEM/TYPE. */


/*     Command line keys. */


/*     Copy local arrays to outputs and return. */

    for (i__ = 1; i__ <= 4; ++i__) {
	s_copy(systms + (((i__1 = i__ - 1) < 4 && 0 <= i__1 ? i__1 : s_rnge(
		"systms", i__1, "crcnst_", (ftnlen)279)) << 5), lsystm + (((
		i__2 = i__ - 1) < 4 && 0 <= i__2 ? i__2 : s_rnge("lsystm", 
		i__2, "crcnst_", (ftnlen)279)) << 5), (ftnlen)32, (ftnlen)32);
	s_copy(deftyp + (((i__1 = i__ - 1) < 4 && 0 <= i__1 ? i__1 : s_rnge(
		"deftyp", i__1, "crcnst_", (ftnlen)280)) << 5), ldefty + (((
		i__2 = i__ - 1) < 4 && 0 <= i__2 ? i__2 : s_rnge("ldefty", 
		i__2, "crcnst_", (ftnlen)280)) << 5), (ftnlen)32, (ftnlen)32);
	for (j = 1; j <= 10; ++j) {
	    systyp[(i__1 = i__ + (j << 2) - 5) < 40 && 0 <= i__1 ? i__1 : 
		    s_rnge("systyp", i__1, "crcnst_", (ftnlen)282)] = lsysty[(
		    i__2 = i__ + (j << 2) - 5) < 40 && 0 <= i__2 ? i__2 : 
		    s_rnge("lsysty", i__2, "crcnst_", (ftnlen)282)];
	    fmtted[(i__1 = i__ + (j << 2) - 5) < 40 && 0 <= i__1 ? i__1 : 
		    s_rnge("fmtted", i__1, "crcnst_", (ftnlen)283)] = lfmtte[(
		    i__2 = i__ + (j << 2) - 5) < 40 && 0 <= i__2 ? i__2 : 
		    s_rnge("lfmtte", i__2, "crcnst_", (ftnlen)283)];
	    s_copy(fmtpic + (((i__1 = i__ + (j << 2) - 5) < 40 && 0 <= i__1 ? 
		    i__1 : s_rnge("fmtpic", i__1, "crcnst_", (ftnlen)284)) << 
		    6), lfmtpi + (((i__2 = i__ + (j << 2) - 5) < 40 && 0 <= 
		    i__2 ? i__2 : s_rnge("lfmtpi", i__2, "crcnst_", (ftnlen)
		    284)) << 6), (ftnlen)64, (ftnlen)64);
	}
    }
    for (j = 1; j <= 10; ++j) {
	s_copy(types + (((i__1 = j - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge(
		"types", i__1, "crcnst_", (ftnlen)289)) << 5), ltypes + (((
		i__2 = j - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge("ltypes", 
		i__2, "crcnst_", (ftnlen)289)) << 5), (ftnlen)32, (ftnlen)32);
    }
    for (i__ = 1; i__ <= 19; ++i__) {
	s_copy(clkeys + (((i__1 = i__ - 1) < 19 && 0 <= i__1 ? i__1 : s_rnge(
		"clkeys", i__1, "crcnst_", (ftnlen)293)) << 5), lclkey + (((
		i__2 = i__ - 1) < 19 && 0 <= i__2 ? i__2 : s_rnge("lclkey", 
		i__2, "crcnst_", (ftnlen)293)) << 5), (ftnlen)32, (ftnlen)32);
    }
    return 0;
} /* crcnst_ */

