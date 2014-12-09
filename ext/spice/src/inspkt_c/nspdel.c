/* nspdel.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;

/* Subroutine */ int nspdel_(integer *idlist, integer *n, integer *from, 
	integer *to, integer *every)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char name__[80];
    integer skip, i__, j, k;
    extern /* Subroutine */ int cladv_(logical *);
    char delim[4];
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    logical merge, found;
    char title[16000];
    integer count;
    char quote[4];
    extern /* Subroutine */ int clgqal_(integer *, char *, ftnlen);
    extern integer lastnb_(char *, ftnlen);
    extern /* Subroutine */ int setchr_(char *, integer *, char *, ftnlen, 
	    ftnlen), chkout_(char *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int nspwln_(char *, ftnlen), wrtnpr_(integer *, 
	    integer *, char *, char *, ftnlen, ftnlen), wrtprs_(integer *, 
	    integer *, char *, char *, ftnlen, ftnlen);
    char del[4], fmt[80];
    extern integer pos_(char *, char *, integer *, ftnlen, ftnlen);
    integer put;
    extern /* Subroutine */ int bbgetc_1__(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen);


/* $ Abstract */

/*     Print a delimited report from Inspekt */

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

/*     INSPEKT */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     IDLIST     I   Idcodes for the columns to be reported */
/*     N          I   Number of matching rows */
/*     FROM       I   First row to print */
/*     TO         I   Last  row to print */
/*     EVERY      I   Increment between rows */


/* $ Detailed_Input */

/*     IDLIST     is an array of id codes for the various columns that */
/*                are to be printed in the report. */

/*     N          is the number of columns to appear in the report. */

/*     FROM       is the first row of the set of matching rows to */
/*                display */

/*     TO         is the last row of the set of matching rows to */
/*                display. */

/*     EVERY      is the number of matching rows to skip between the */
/*                rows that are displayed. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     None. */


/* $ Particulars */

/*     This routine prints the results of an Inspekt query in a delimited */
/*     format suitable for importing into excel  or other program. */

/* $ Examples */

/*    Sorry Charlie, you're on your own here. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    Inspekt Version 1.0.0, 26-MAR-2003 */


/* -& */

/*     Spicelib Functions */

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


/*     This file contains the parameter LNGSIZ which specifies the */
/*     longest character string that can be declared in all of the */
/*     FORTRAN environments supported by the SPICE system. */


/*     Parameters */

    if (return_()) {
	return 0;
    }
    chkin_("NSPDEL", (ftnlen)6);

/*     Look up the delimiter to use as well as the character to use to */
/*     quote strings. */

    bbgetc_1__("COPY", "DELIMITER", &k, delim, (ftnlen)4, (ftnlen)9, (ftnlen)
	    4);
    bbgetc_1__("COPY", "QUOTE", &k, quote, (ftnlen)4, (ftnlen)5, (ftnlen)4);
    bbgetc_1__("COPY", "FORMAT", &k, fmt, (ftnlen)4, (ftnlen)6, (ftnlen)80);

/*     Determine whether or not to put each componenent in */
/*     a separate row. */
    merge = pos_(fmt, "PRESERVED", &c__0, (ftnlen)80, (ftnlen)9) == 0;
    if (s_cmp(delim, "TAB", (ftnlen)4, (ftnlen)3) == 0) {
	s_copy(del, "\t", (ftnlen)4, (ftnlen)1);
    } else if (s_cmp(delim, "SPACE", (ftnlen)4, (ftnlen)5) == 0) {
	s_copy(del, " ", (ftnlen)4, (ftnlen)1);
    } else {
	s_copy(del, delim, (ftnlen)4, (ftnlen)4);
    }

/*     Write out the line with the column headings. */

    s_copy(title, " ", (ftnlen)16000, (ftnlen)1);
    put = 1;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	clgqal_(&idlist[i__], name__, (ftnlen)80);
	setchr_(quote, &put, title, (ftnlen)4, (ftnlen)16000);
	i__2 = lastnb_(name__, (ftnlen)80);
	for (j = 1; j <= i__2; ++j) {
	    setchr_(name__ + (j - 1), &put, title, (ftnlen)1, (ftnlen)16000);
	    if (s_cmp(name__ + (j - 1), quote, (ftnlen)1, (ftnlen)4) == 0) {
		setchr_(quote, &put, title, (ftnlen)4, (ftnlen)16000);
	    }
	}
	setchr_(quote, &put, title, (ftnlen)4, (ftnlen)16000);
	if (i__ != *n) {
	    setchr_(del, &put, title, (ftnlen)4, (ftnlen)16000);
	}
    }
    nspwln_(title, (ftnlen)16000);

/*     Advance to the first row of the current scope of the query. */

    i__1 = *from;
    for (i__ = 1; i__ <= i__1; ++i__) {
	cladv_(&found);
    }
    count = *from;
    while(found) {
	if (! merge) {
	    wrtprs_(idlist, n, del, quote, (ftnlen)4, (ftnlen)4);
	} else {
	    wrtnpr_(idlist, n, del, quote, (ftnlen)4, (ftnlen)4);
	}
	skip = 0;
	while(found && skip < *every) {
	    cladv_(&found);
	    ++skip;
	    ++count;
	}
	if (count > *to) {
	    found = FALSE_;
	}
    }
    chkout_("NSPDEL", (ftnlen)6);
    return 0;
} /* nspdel_ */

