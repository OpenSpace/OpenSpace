/* nspopl.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__2 = 2;
static logical c_true = TRUE_;

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

/* Subroutine */ int nspopl_(char *lognam, char *versn, ftnlen lognam_len, 
	ftnlen versn_len)
{
    /* System generated locals */
    address a__1[2];
    integer i__1, i__2[2];

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);

    /* Local variables */
    extern logical have_(char *, ftnlen);
    char time[32], warn[32], attr[32*2], rest[800];
    integer i__, n;
    logical found;
    char value[32];
    extern integer rtrim_(char *, ftnlen);
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    char myerr[800*2], style[80];
    integer start;
    char io[32], logfil[128];
    extern /* Subroutine */ int newfil_(char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen), dcyphr_(integer *, logical *, char *, ftnlen), 
	    nparsi_(char *, integer *, char *, integer *, ftnlen, ftnlen), 
	    prefix_(char *, integer *, char *, ftnlen, ftnlen), nsplog_(char *
	    , logical *, ftnlen), curtim_(char *, ftnlen), trnlat_(char *, 
	    char *, ftnlen, ftnlen), nextwd_(char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen), pltfrm_(integer *, integer *, char *, ftnlen), 
	    suffix_(char *, integer *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int nspwln_();
    extern /* Subroutine */ int tkvrsn_(char *, char *, ftnlen, ftnlen);
    char env[80], err[80], was[32];
    extern integer pos_(char *, char *, integer *, ftnlen, ftnlen);
    char tkv[80];
    integer ptr;
    extern /* Subroutine */ int nicepr_1__(char *, char *, U_fp, ftnlen, 
	    ftnlen);


/* $ Version */

/* -     Command Loop Configured Version 2.0.0, 10-SEP-1998 (WLT) */

/*         The routine now logs the version of SPICELIB that the */
/*         program was linked against. */

/* -     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 4, 1994 */


/*     This routine opens the log file that will be used for loging */
/*     commands.  It should only be called once.  If a log file */
/*     cannot be opened, the routine will issue a warning message */
/*     to the default output device. */

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


/*     Empty out the internal error buffers. */

    s_copy(myerr, " ", (ftnlen)800, (ftnlen)1);
    s_copy(myerr + 800, " ", (ftnlen)800, (ftnlen)1);
    for (i__ = 1; i__ <= 2; ++i__) {
	s_copy(attr + (((i__1 = i__ - 1) < 2 && 0 <= i__1 ? i__1 : s_rnge(
		"attr", i__1, "nspopl_", (ftnlen)101)) << 5), " ", (ftnlen)32,
		 (ftnlen)1);
    }
    newfil_(lognam, "LOG", logfil, lognam_len, (ftnlen)3, (ftnlen)128);
    if (have_(myerr, (ftnlen)800)) {

/*        See if we can parse the error message as having the */
/*        string IOSTAT was value imbedded in it.  This isn't */
/*        pretty, but we can possibly get a better idea of */
/*        what went wrong this way. */

	start = pos_(myerr, "IOSTAT", &c__1, (ftnlen)800, (ftnlen)6);
	if (start > 0) {
	    s_copy(rest, myerr + (start - 1), (ftnlen)800, 800 - (start - 1));
	    nextwd_(rest, io, rest, (ftnlen)800, (ftnlen)32, (ftnlen)800);
	    nextwd_(rest, was, rest, (ftnlen)800, (ftnlen)32, (ftnlen)800);
	    nextwd_(rest, value, rest, (ftnlen)800, (ftnlen)32, (ftnlen)800);
	    if (eqstr_(was, "was", (ftnlen)32, (ftnlen)3) && s_cmp(value, 
		    " ", (ftnlen)32, (ftnlen)1) != 0) {
		s_copy(err, " ", (ftnlen)80, (ftnlen)1);
		nparsi_(value, &i__, err, &ptr, (ftnlen)32, (ftnlen)80);
		if (s_cmp(err, " ", (ftnlen)80, (ftnlen)1) == 0) {
		    dcyphr_(&i__, &found, rest, (ftnlen)800);
		    if (found) {
			s_copy(myerr + (start - 1), rest, 800 - (start - 1), (
				ftnlen)800);
		    }
		}
	    }
	}
	s_copy(rest, myerr, (ftnlen)800, (ftnlen)800);
	s_copy(warn, " ", (ftnlen)32, (ftnlen)1);
	trnlat_("WARNING", warn, (ftnlen)7, (ftnlen)32);
	trnlat_("CANNOTOPENLOG", myerr + 800, (ftnlen)13, (ftnlen)800);
	start = rtrim_(myerr + 800, (ftnlen)800);
	prefix_(myerr + 800, &c__1, rest, start, (ftnlen)800);
/* Writing concatenation */
	i__2[0] = 33, a__1[0] = "LEFT 1 RIGHT 78 NEWLINE /cr FLAG ";
	i__2[1] = 32, a__1[1] = warn;
	s_cat(style, a__1, i__2, &c__2, (ftnlen)80);
	nicepr_1__(rest, style, (U_fp)nspwln_, (ftnlen)800, (ftnlen)80);
    } else {
	curtim_(time, (ftnlen)32);
	pltfrm_(&c__2, &n, attr, (ftnlen)32);
	tkvrsn_("TOOLKIT", tkv, (ftnlen)7, (ftnlen)80);
	s_copy(env, attr, (ftnlen)80, (ftnlen)32);
	suffix_("---", &c__1, env, (ftnlen)3, (ftnlen)80);
	suffix_(attr + 32, &c__1, env, (ftnlen)32, (ftnlen)80);
	prefix_("SPICE Toolkit ", &c__1, tkv, (ftnlen)14, (ftnlen)80);
	nsplog_(env, &c_true, (ftnlen)80);
	nsplog_(versn, &c_true, versn_len);
	nsplog_(tkv, &c_true, (ftnlen)80);
	nsplog_(time, &c_true, (ftnlen)32);
    }
    return 0;
} /* nspopl_ */

