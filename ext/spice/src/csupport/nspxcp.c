/* nspxcp.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

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

/* Subroutine */ int nspxcp_0_(int n__, char *string, char *error, char *
	screen, char *logfil, ftnlen string_len, ftnlen error_len, ftnlen 
	screen_len, ftnlen logfil_len)
{
    /* Initialized data */

    static char lstyle[128] = "LEFT 1 RIGHT 78                              "
	    "                                                                "
	    "                   ";
    static char sstyle[128] = "LEFT 1 RIGHT 78                              "
	    "                                                                "
	    "                   ";

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_rnge(char *, integer, 
	    char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static integer i__;
    extern /* Subroutine */ int prclr_(void);
    static char margin[128];
    extern /* Subroutine */ int nspioa_(char *, ftnlen), nspioh_(char *, 
	    ftnlen), suffix_(char *, integer *, char *, ftnlen, ftnlen), 
	    trnlat_(char *, char *, ftnlen, ftnlen), nspmrg_(char *, ftnlen), 
	    nspgst_(char *, logical *, ftnlen);
    extern /* Subroutine */ int nspwln_();
    static logical scrstt[3], savstt[3];
    extern /* Subroutine */ int nsppst_(char *, logical *, ftnlen), 
	    nicepr_1__(char *, char *, U_fp, ftnlen, ftnlen);


/* $ Version */

/* -     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 4, 1994 */

    /* Parameter adjustments */
    if (error) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_nsperr;
	case 2: goto L_nspsty;
	}

    return 0;

L_nsperr:
/* $ Version */

/* -     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 4, 1994 */


/*     This entry point is intended to be called once for */
/*     brief error diagnostics and a second time for more detailed */
/*     diagnostics.  We can tell which is which by examining the */
/*     first entry of the error array.  If it is non-blank this */
/*     must be the first such call (because we set it to blank */
/*     after we get done doing something with it).  A second */
/*     call can only happen if the special command */
/*     was entered by the user ('?').  In this case the command */
/*     manager will not reset the error array and not pass the */
/*     command to any other routines.  Instead it returns immediately */
/*     so that this routine can process the second part of the */
/*     error message. */

    if (s_cmp(error, " ", error_len, (ftnlen)1) != 0) {

/*        We automatically clear the procedure stack whenever */
/*        an error occurs. */

	prclr_();

/*        First inhibit writing to the log file. */

	nspioh_("LOG", (ftnlen)3);

/*        Now write out only the first component of the error message. */

	nspmrg_(margin, (ftnlen)128);
	suffix_(sstyle, &c__1, margin, (ftnlen)128, (ftnlen)128);
	nicepr_1__(error, margin, (U_fp)nspwln_, error_len, (ftnlen)128);

/*        Now inhibit writing to the screen or the save file. But */
/*        fetch their current state so that we can reset them */
/*        to exactly their current states. */

	nspgst_("SCREEN", scrstt, (ftnlen)6);
	nspgst_("SAVE", savstt, (ftnlen)4);
	nspioh_("SCREEN", (ftnlen)6);
	nspioh_("SAVE", (ftnlen)4);

/*        Reactivate the log file. */

	nspioa_("LOG", (ftnlen)3);
	for (i__ = 1; i__ <= 2; ++i__) {
	    nicepr_1__(error + ((i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : 
		    s_rnge("error", i__1, "nspxcp_", (ftnlen)127)) * 
		    error_len, lstyle, (U_fp)nspwln_, error_len, (ftnlen)128);
	}
	nsppst_("SCREEN", scrstt, (ftnlen)6);
	nsppst_("SAVE", savstt, (ftnlen)4);
	s_copy(error, " ", error_len, (ftnlen)1);
	return 0;
    }

/*     The only way to get here is for the user to have processed */
/*     the first half of an error and typed a question mark or */
/*     blank command. (This relies on all kinds of side effects. */
/*     Better talk to Bill if you want to be able to figure this out). */

    if (s_cmp(string, "?", string_len, (ftnlen)1) == 0) {
	if (s_cmp(error + error_len, " ", error_len, (ftnlen)1) == 0) {
	    trnlat_("NOMOREDIAGNOSTICS", error + error_len, (ftnlen)17, 
		    error_len);
	}

/*        We've already written the second part of the error */
/*        message to the log file, so we shall inhibit writing */
/*        there now. */

	nspioh_("LOG", (ftnlen)3);
	nspmrg_(margin, (ftnlen)128);
	suffix_(sstyle, &c__1, margin, (ftnlen)128, (ftnlen)128);
	nicepr_1__(error + error_len, margin, (U_fp)nspwln_, error_len, (
		ftnlen)128);

/*        Now re-activate the log file. */

	nspioa_("LOG", (ftnlen)3);
	s_copy(error + error_len, " ", error_len, (ftnlen)1);
    }
    return 0;

/*     Set the style string that shall be used for printing */
/*     errors. */


L_nspsty:

/* $ Version */

/* -     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 4, 1994 */

    s_copy(sstyle, screen, (ftnlen)128, screen_len);
    s_copy(lstyle, logfil, (ftnlen)128, logfil_len);
    return 0;
} /* nspxcp_ */

/* Subroutine */ int nspxcp_(char *string, char *error, char *screen, char *
	logfil, ftnlen string_len, ftnlen error_len, ftnlen screen_len, 
	ftnlen logfil_len)
{
    return nspxcp_0_(0, string, error, screen, logfil, string_len, error_len, 
	    screen_len, logfil_len);
    }

/* Subroutine */ int nsperr_(char *string, char *error, ftnlen string_len, 
	ftnlen error_len)
{
    return nspxcp_0_(1, string, error, (char *)0, (char *)0, string_len, 
	    error_len, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int nspsty_(char *screen, char *logfil, ftnlen screen_len, 
	ftnlen logfil_len)
{
    return nspxcp_0_(2, (char *)0, (char *)0, screen, logfil, (ftnint)0, (
	    ftnint)0, screen_len, logfil_len);
    }

