/* nspint.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__20 = 20;
static integer c__80 = 80;
static integer c__0 = 0;
static integer c__100 = 100;

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

/* Subroutine */ int nspint_(char *versn, ftnlen versn_len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char sclk[127], leaps[127];
    extern /* Subroutine */ int nsphi_(char *, ftnlen), m2serr_(char *, char *
	    , char *, ftnlen, ftnlen, ftnlen);
    char leapnm[32], sclknm[32];
    extern /* Subroutine */ int ldpool_(char *, ftnlen), builto_(char *, 
	    ftnlen), hlpint_(void), bbclr_1__(void);
    extern logical exists_(char *, ftnlen);
    extern /* Subroutine */ int bbputc_1__(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen), bbputi_1__(char *, char *, integer *, 
	    integer *, ftnlen, ftnlen), expfnm_1__(char *, char *, ftnlen, 
	    ftnlen);


/*     This routine handles the task of initializing the various */
/*     items needed by INSPEKT at startup. */

/*     March 26, 2003  - WLT */

/*     Version 6.0  Added the DELIMITER and QUOTE specification required */
/*     by the "DELIMITED" report format. */


/*     Version 5.0  Added the override call (BUILTO) that overrides */
/*     the generic environment reporting capability of the command */
/*     loop code. */

/*     Version 4.0  Added the default floating and integer formats */
/*     to be used by Inspekt. */

/*     Version 3.0  Help waiting is set to off by default.  Users */
/*     can reset it by typing "SET HELP WAIT" */

/*     Version 2.0  The environment variable that points to leapseconds */
/*     is now called LEAPSECONDS and not LEAPSCND as it was before. */


/*     Spicelib functions */


/*     Local Parameters. */

/*     FILSIZ   is the number of characters allowed an a filename */
/*     LEAPNM   is the logical/environment variable that points */
/*              to a leapsecond kernel. */
/*     SCLKNM   is the logical/environment variable that points */
/*              to an sclk kernel. */


/*     The first thing we need to do is clear the bulletin board */
/*     and open a log file. */

    bbclr_1__();
/*     CALL TRCOFF */

/*     Set up the default formats, page size etc. */

    bbputi_1__("POST", "PAGEHEIGHT", &c__1, &c__20, (ftnlen)4, (ftnlen)10);
    bbputi_1__("POST", "PAGEWIDTH", &c__1, &c__80, (ftnlen)4, (ftnlen)9);
    bbputi_1__("POST", "TITLEFREQUENCY", &c__1, &c__0, (ftnlen)4, (ftnlen)14);
    bbputi_1__("POST", "HEADERFREQUENCY", &c__1, &c__0, (ftnlen)4, (ftnlen)15)
	    ;
    bbputi_1__("POST", "REPORTLIMIT", &c__1, &c__100, (ftnlen)4, (ftnlen)11);
    bbputi_1__("POST", "AUTOADJUST", &c__1, &c__1, (ftnlen)4, (ftnlen)10);
    bbputi_1__("POST", "HELPPROMPT", &c__1, &c__0, (ftnlen)4, (ftnlen)10);
    bbputc_1__("POST", "FORMAT", &c__1, "FLAGGED PRESERVED", (ftnlen)4, (
	    ftnlen)6, (ftnlen)17);
    bbputc_1__("POST", "FMTMARK", &c__1, ">", (ftnlen)4, (ftnlen)7, (ftnlen)1)
	    ;
    bbputc_1__("POST", "PAGETITLE", &c__1, "Inspekt Report", (ftnlen)4, (
	    ftnlen)9, (ftnlen)14);
    bbputc_1__("POST", "TITLEJUSTIFICATION", &c__1, "LEFT", (ftnlen)4, (
	    ftnlen)18, (ftnlen)4);
    bbputc_1__("POST", "TIMEFMT", &c__1, "YYYY MON DD HR:MN:SC::UTC::RND", (
	    ftnlen)4, (ftnlen)7, (ftnlen)30);
    bbputc_1__("POST", "INTFMT", &c__1, "###########", (ftnlen)4, (ftnlen)6, (
	    ftnlen)11);
    bbputc_1__("POST", "DPFMT", &c__1, "#########.####", (ftnlen)4, (ftnlen)5,
	     (ftnlen)14);
    bbputc_1__("POST", "CHFMT", &c__1, ".............", (ftnlen)4, (ftnlen)5, 
	    (ftnlen)13);
    bbputc_1__("POST", "QUOTE", &c__1, "\"", (ftnlen)4, (ftnlen)5, (ftnlen)1);
    bbputc_1__("POST", "DELIMITER", &c__1, "TAB", (ftnlen)4, (ftnlen)9, (
	    ftnlen)3);

/*     Override the default SHOW ENVIRONMENT command that is */
/*     available from the command loop code. */

    builto_("ENVIRONMENT", (ftnlen)11);

/*     Initialize the error reporting style used by META/2 */

    m2serr_(" /cr(3:3)/cr ", "/vt...", ".../vt", (ftnlen)13, (ftnlen)6, (
	    ftnlen)6);

/*     Set up the help system. */

    hlpint_();

/*     Now look for any environment variables that might be around */
/*     such as LEAPSECONDS, SCLK, etc. */

    s_copy(leapnm, "LEAPSECONDS", (ftnlen)32, (ftnlen)11);
    s_copy(sclknm, "SCLK", (ftnlen)32, (ftnlen)4);
    s_copy(leaps, " ", (ftnlen)127, (ftnlen)1);
    s_copy(sclk, " ", (ftnlen)127, (ftnlen)1);
    expfnm_1__(leapnm, leaps, (ftnlen)32, (ftnlen)127);
    expfnm_1__(sclknm, sclk, (ftnlen)32, (ftnlen)127);
    if (s_cmp(leaps, " ", (ftnlen)127, (ftnlen)1) != 0) {
	if (exists_(leaps, (ftnlen)127)) {
	    ldpool_(leaps, (ftnlen)127);
	    bbputc_1__("POST", "LEAPSECONDS", &c__1, leaps, (ftnlen)4, (
		    ftnlen)11, (ftnlen)127);
	}
    }
    if (s_cmp(sclk, " ", (ftnlen)127, (ftnlen)1) != 0) {
	if (exists_(sclk, (ftnlen)127)) {
	    ldpool_(sclk, (ftnlen)127);
	    bbputc_1__("POST", "SCLK", &c__1, sclk, (ftnlen)4, (ftnlen)4, (
		    ftnlen)127);
	}
    }

/*     Finally present a greeting to the user. */

    nsphi_(versn, versn_len);
    return 0;
} /* nspint_ */

