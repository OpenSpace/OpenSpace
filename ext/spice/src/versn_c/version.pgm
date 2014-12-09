/* version.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__2 = 2;
static integer c__23 = 23;
static integer c__4 = 4;

/* $Procedure VERSION ( Print library version information ) */
/* Main program */ MAIN__(void)
{
    /* System generated locals */
    address a__1[2], a__2[4];
    integer i__1[2], i__2, i__3[4], i__4;
    doublereal d__1;
    char ch__1[25], ch__2[99];

    /* Builtin functions */
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen),
	     s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_stop(char *, ftnlen);

    /* Local variables */
    char line[80], vrsn[6];
    extern /* Subroutine */ int zzplatfm_(char *, char *, ftnlen, ftnlen);
    integer i__;
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen);
    extern doublereal dpmin_(void);
    extern /* Subroutine */ int repmd_(char *, char *, doublereal *, integer *
	    , char *, ftnlen, ftnlen, ftnlen);
    extern doublereal dpmax_(void);
    char fform[80];
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen);
    char cmplr[80];
    extern integer wdcnt_(char *, ftnlen);
    char tform[80];
    extern integer rtrim_(char *, ftnlen);
    char os[80];
    extern /* Subroutine */ int getcml_(char *, ftnlen), byebye_(char *, 
	    ftnlen);
    extern integer intmin_(void), intmax_(void);
    char linout[80*6];
    extern /* Subroutine */ int tostdo_(char *, ftnlen), tkvrsn_(char *, char 
	    *, ftnlen, ftnlen);
    extern integer pos_(char *, char *, integer *, ftnlen, ftnlen);
    char sys[80];

/* $ Abstract */

/*     This program prints to standard output the current SPICE */
/*     distribution version number, hardware system ID, operating */
/*     system ID, compiler name, the format of double precision */
/*     numbers for the hardware architecture, and the max and min */
/*     values for double precision and integer numbers. */

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

/* $ Keyword */

/*     VERSION */
/*     UTILITY */

/* $ Parameters */

/*     LINELN            length of line output string, set to 80. */

/*     DATEID            update version time string, set to 20. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The version utility may use 3 different command line arguments. */
/*     The default (no arguments) returns the Toolkit version string. */

/*     Usage: $ version [OPTION] */

/* $ Description */

/*     None. */

/* $ Examples */


/*     Default behavior: */

/*     $ version */
/*     N0051 */

/*     Display all (-a) information: */

/*     $version -a */

/*     Toolkit version  : N0051 */
/*     System           : PC */
/*     Operating System : LINUX */
/*     Compiler         : LINUX G77 */
/*     File Format      : LTL-IEEE */
/*     MAX DP           :  1.7976931348623E+308 */
/*     MIN DP           : -1.7976931348623E+308 */
/*     MAX INT          :  2147483647 */
/*     MIN INT          : -2147483647 */

/*     Display version (-v) information: */

/*     $version -v */

/*     Version Utility for SPICE Toolkit edition N0051, */
/*     last update: 1.1.0, 05-OCT-2001 */

/*     Display help (-h) information: */

/*     $version -h */

/*     Usage: version [OPTION] */
/*     no arguments   output only the SPICE toolkit version string. */
/*     -a(ll)         output all environment variables; SPICE toolkit */
/*                    version, system ID, operating system, compiler, */
/*                    binary file format, max and min values for */
/*                    double precision and integer numbers. */
/*     -v(ersion)     output the version of the utility. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */
/*     F.S. Turner     (JPL) */
/*     E.D. Wright     (JPL) */

/* $ Version */

/*     SPICELIB Version 1.1.0 26-SEP-2001 (FST) (EDW) */

/*        Added TEXT_FORMAT output. */

/*        Included options for SYSTEM, O/S, COMPILER, FILE_FORMAT, */
/*        max/min DPs & integers, outputs, version, and help. */

/*        Added proper SPICE header. */

/*     SPICELIB Version 1.0.0 13-NOV-2001 (WLT) */

/*        First version, Thu NOV 13 10:04:41 PST 1997 W.L. Taber */

/* -& */

/*     SPICELIB functions. */


/*     Local Parameters. */


/*     Local Variables. */


/*     Get command line. */

    getcml_(line, (ftnlen)80);
    ucase_(line, line, (ftnlen)80, (ftnlen)80);
    tkvrsn_("TOOLKIT", vrsn, (ftnlen)7, (ftnlen)6);

/*     Parse the command line for arguments. Appropriately respond. */

    if (wdcnt_(line, (ftnlen)80) == 0) {

/*        No arguments, default to the toolkit version string. */

	tostdo_(vrsn, rtrim_(vrsn, (ftnlen)6));
    } else if (pos_(line, "-A", &c__1, (ftnlen)80, (ftnlen)2) == 1) {

/*        All. Output everything. */

	tostdo_(" ", (ftnlen)1);
/* Writing concatenation */
	i__1[0] = 19, a__1[0] = "Toolkit version  : ";
	i__1[1] = 6, a__1[1] = vrsn;
	s_cat(ch__1, a__1, i__1, &c__2, (ftnlen)25);
	tostdo_(ch__1, (ftnlen)25);
	zzplatfm_("SYSTEM", sys, (ftnlen)6, (ftnlen)80);
/* Writing concatenation */
	i__1[0] = 19, a__1[0] = "System           : ";
	i__1[1] = 80, a__1[1] = sys;
	s_cat(ch__2, a__1, i__1, &c__2, (ftnlen)99);
	tostdo_(ch__2, (ftnlen)99);
	zzplatfm_("O/S", os, (ftnlen)3, (ftnlen)80);
/* Writing concatenation */
	i__1[0] = 19, a__1[0] = "Operating System : ";
	i__1[1] = 80, a__1[1] = os;
	s_cat(ch__2, a__1, i__1, &c__2, (ftnlen)99);
	tostdo_(ch__2, (ftnlen)99);
	zzplatfm_("COMPILER", cmplr, (ftnlen)8, (ftnlen)80);
/* Writing concatenation */
	i__1[0] = 19, a__1[0] = "Compiler         : ";
	i__1[1] = 80, a__1[1] = cmplr;
	s_cat(ch__2, a__1, i__1, &c__2, (ftnlen)99);
	tostdo_(ch__2, (ftnlen)99);
	zzplatfm_("FILE_FORMAT", fform, (ftnlen)11, (ftnlen)80);
/* Writing concatenation */
	i__1[0] = 19, a__1[0] = "File Format      : ";
	i__1[1] = 80, a__1[1] = fform;
	s_cat(ch__2, a__1, i__1, &c__2, (ftnlen)99);
	tostdo_(ch__2, (ftnlen)99);
	zzplatfm_("TEXT_FORMAT", tform, (ftnlen)11, (ftnlen)80);
/* Writing concatenation */
	i__1[0] = 19, a__1[0] = "Text File Format : ";
	i__1[1] = 80, a__1[1] = tform;
	s_cat(ch__2, a__1, i__1, &c__2, (ftnlen)99);
	tostdo_(ch__2, (ftnlen)99);
	s_copy(linout, "MAX DP           :  #", (ftnlen)80, (ftnlen)21);
	d__1 = dpmax_();
	repmd_(linout, "#", &d__1, &c__23, linout, (ftnlen)80, (ftnlen)1, (
		ftnlen)80);
	tostdo_(linout, (ftnlen)80);
	s_copy(linout + 80, "MIN DP           : #", (ftnlen)80, (ftnlen)20);
	d__1 = dpmin_();
	repmd_(linout + 80, "#", &d__1, &c__23, linout + 80, (ftnlen)80, (
		ftnlen)1, (ftnlen)80);
	tostdo_(linout + 80, (ftnlen)80);
	s_copy(linout + 160, "MAX INT          :  #", (ftnlen)80, (ftnlen)21);
	i__2 = intmax_();
	repmi_(linout + 160, "#", &i__2, linout + 160, (ftnlen)80, (ftnlen)1, 
		(ftnlen)80);
	tostdo_(linout + 160, (ftnlen)80);
	s_copy(linout + 240, "MIN INT          : #", (ftnlen)80, (ftnlen)20);
	i__2 = intmin_();
	repmi_(linout + 240, "#", &i__2, linout + 240, (ftnlen)80, (ftnlen)1, 
		(ftnlen)80);
	tostdo_(linout + 240, (ftnlen)80);
	tostdo_(" ", (ftnlen)1);
    } else if (pos_(line, "-V", &c__1, (ftnlen)80, (ftnlen)2) == 1) {

/*        Version. Output the utility version string. */

/* Writing concatenation */
	i__3[0] = 42, a__2[0] = "Version Utility for SPICE Toolkit edition ";
	i__3[1] = rtrim_(vrsn, (ftnlen)6), a__2[1] = vrsn;
	i__3[2] = 15, a__2[2] = ", last update: ";
	i__3[3] = 18, a__2[3] = "1.1.0, 07-JAN-2002  ";
	s_cat(linout, a__2, i__3, &c__4, (ftnlen)80);
	tostdo_(" ", (ftnlen)1);
	tostdo_(linout, (ftnlen)80);
	tostdo_(" ", (ftnlen)1);
    } else if (pos_(line, "-H", &c__1, (ftnlen)80, (ftnlen)2) == 1) {

/*        Help. How does does one use this perplexing routine? */

	s_copy(linout, "Usage: version [OPTION]", (ftnlen)80, (ftnlen)23);
	s_copy(linout + 80, " no arguments   output only the SPICE toolkit v"
		"ersion string.", (ftnlen)80, (ftnlen)61);
	s_copy(linout + 160, " -a(ll)         output all environment variabl"
		"es; SPICE toolkit version, system", (ftnlen)80, (ftnlen)79);
	s_copy(linout + 240, "                ID, operating system, compiler"
		", and binary file format, ", (ftnlen)80, (ftnlen)72);
	s_copy(linout + 320, "                max and min values for double "
		"precision and integer numbers.", (ftnlen)80, (ftnlen)76);
	s_copy(linout + 400, " -v(ersion)     output the version of the util"
		"ity.", (ftnlen)80, (ftnlen)50);
	tostdo_(" ", (ftnlen)1);
	for (i__ = 1; i__ <= 6; ++i__) {
	    tostdo_(linout + ((i__2 = i__ - 1) < 6 && 0 <= i__2 ? i__2 : 
		    s_rnge("linout", i__2, "version_", (ftnlen)272)) * 80, 
		    rtrim_(linout + ((i__4 = i__ - 1) < 6 && 0 <= i__4 ? i__4 
		    : s_rnge("linout", i__4, "version_", (ftnlen)272)) * 80, (
		    ftnlen)80));
	}
	tostdo_(" ", (ftnlen)1);
    } else {

/*        The user put something on the command line, but nothing */
/*        known. Return the toolkit version string. */

	tostdo_(vrsn, rtrim_(vrsn, (ftnlen)6));
    }

/*     Done. Indicate as much. Say bye. */

    byebye_("SUCCESS", (ftnlen)7);
    s_stop("", (ftnlen)0);
    return 0;
} /* MAIN__ */

/* Main program alias */ int version_ () { MAIN__ (); return 0; }
