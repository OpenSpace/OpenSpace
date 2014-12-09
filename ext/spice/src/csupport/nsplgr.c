/* nsplgr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;

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

/* Subroutine */ int nsplg_0_(int n__, char *commnd, logical *hidden, char *
	vstyle, char *hstyle, char *cdelim, ftnlen commnd_len, ftnlen 
	vstyle_len, ftnlen hstyle_len, ftnlen cdelim_len)
{
    /* Initialized data */

    static char mystr[1025] = "                                             "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                                                                "
	    "                    ";
    static char seen[120] = "LEFT 1 RIGHT 78                                "
	    "                                                                "
	    "         ";
    static char hide[120] = "LEADER ;^ LEFT 1 RIGHT 78 HARDSPACE ^          "
	    "                                                                "
	    "         ";
    static char delim[1] = ";";

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int nspioa_(char *, ftnlen), nspioh_(char *, 
	    ftnlen), suffix_(char *, integer *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int nspwln_();
    extern /* Subroutine */ int nicepr_1__(char *, char *, U_fp, ftnlen, 
	    ftnlen);


/* $ Version */

/* -     Command Loop Configured Version 1.1.0, 21-JUN-1999 (WLT) */

/*         Placed RETURN before first entry point. */

/* -     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 4, 1994 */


/*     Save the contents of the command to a log file and any save */
/*     file that might be open and active. */

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

/* $ Version */

/* -     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 4, 1994 */


/*     The following parameters are the system parameters required */
/*     by PERCY.  Be sure to read any notes before adjusting these */


/*     The maximum number of commands that can be buffered is */
/*     determined by the value of MAXCOM.  This parameter is */
/*     used primarily by NXTCOM. */


/*     The parameter FILEN is the maximum length of a file name */
/*     on a particular system. */


/*     The parameter COMSIZ is the maximum length allowed for a */
/*     command. */


/*     The parameter ERRSIZ is the maximum length allowed for */
/*     error messages. */


/*     The parameter STYSIZ is the maximum length expected for */
/*     a NICEPR style string. */

    switch(n__) {
	case 1: goto L_nsplog;
	case 2: goto L_nsplgs;
	case 3: goto L_nspgls;
	}

    return 0;

/*     This entry point handles the logging of commands. */


L_nsplog:

/* $ Version */

/* -     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 4, 1994 */

    s_copy(mystr, commnd, (ftnlen)1025, commnd_len);

/*     Inhibit writing to the screen. */

    nspioh_("SCREEN", (ftnlen)6);
    nspioa_("LOG", (ftnlen)3);
    if (*hidden) {
	nicepr_1__(commnd, hide, (U_fp)nspwln_, commnd_len, (ftnlen)120);
    } else {
	s_copy(mystr, commnd, (ftnlen)1025, commnd_len);
	suffix_(delim, &c__0, mystr, (ftnlen)1, (ftnlen)1025);
	nicepr_1__(mystr, seen, (U_fp)nspwln_, (ftnlen)1025, (ftnlen)120);
    }

/*     Re-activate the screen for writing output. */

    nspioa_("SCREEN", (ftnlen)6);
    nspioh_("LOG", (ftnlen)3);
    return 0;

/*     This entry point allows users to set the style used for */
/*     logging hidden and visible commands. */


L_nsplgs:

/* $ Version */

/* -     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 4, 1994 */

    s_copy(seen, vstyle, (ftnlen)120, vstyle_len);
    s_copy(hide, hstyle, (ftnlen)120, hstyle_len);
    s_copy(delim, cdelim, (ftnlen)1, cdelim_len);
    return 0;

/*     This entry point allows users to get the style used for */
/*     logging hidden and visible commands. */


L_nspgls:
    s_copy(vstyle, seen, vstyle_len, (ftnlen)120);
    s_copy(hstyle, hide, hstyle_len, (ftnlen)120);
    s_copy(cdelim, delim, cdelim_len, (ftnlen)1);
    return 0;
} /* nsplg_ */

/* Subroutine */ int nsplg_(char *commnd, logical *hidden, char *vstyle, char 
	*hstyle, char *cdelim, ftnlen commnd_len, ftnlen vstyle_len, ftnlen 
	hstyle_len, ftnlen cdelim_len)
{
    return nsplg_0_(0, commnd, hidden, vstyle, hstyle, cdelim, commnd_len, 
	    vstyle_len, hstyle_len, cdelim_len);
    }

/* Subroutine */ int nsplog_(char *commnd, logical *hidden, ftnlen commnd_len)
{
    return nsplg_0_(1, commnd, hidden, (char *)0, (char *)0, (char *)0, 
	    commnd_len, (ftnint)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int nsplgs_(char *vstyle, char *hstyle, char *cdelim, ftnlen 
	vstyle_len, ftnlen hstyle_len, ftnlen cdelim_len)
{
    return nsplg_0_(2, (char *)0, (logical *)0, vstyle, hstyle, cdelim, (
	    ftnint)0, vstyle_len, hstyle_len, cdelim_len);
    }

/* Subroutine */ int nspgls_(char *vstyle, char *hstyle, char *cdelim, ftnlen 
	vstyle_len, ftnlen hstyle_len, ftnlen cdelim_len)
{
    return nsplg_0_(3, (char *)0, (logical *)0, vstyle, hstyle, cdelim, (
	    ftnint)0, vstyle_len, hstyle_len, cdelim_len);
    }

