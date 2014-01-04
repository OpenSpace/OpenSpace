/* flgrpt.f -- translated by f2c (version 19980913).
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

/* Subroutine */ int flgrpt_(integer *nitems, char *names, char *values, U_fp 
	myio, ftnlen names_len, ftnlen values_len)
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer), i_len(char *, ftnlen), 
	    s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char hard[1];
    logical free[129];
    integer i__, j, k, l;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer width;
    extern integer rtrim_(char *, ftnlen);
    char style[200];
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    char letter[1];
    extern /* Subroutine */ int suffix_(char *, integer *, char *, ftnlen, 
	    ftnlen), nspmrg_(char *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int nicepr_1__(char *, char *, U_fp, ftnlen, 
	    ftnlen);


/*     This routine takes an array of names and an array of associated */
/*     value strings and produces a flagged set of outputs.  This */
/*     routine signals no errors. */


/*     The routine MYIO is a routine that is supplied by the user */
/*     that can handle io of text lines without any action by the */
/*     routine that calls it. */

/* $ Version */

/*     Inspekt Routine version 2.0.0, 7-APR-1995 (WLT) */

/*        Unused variables LEFT and RIGHT were removed. */


/*     Spicelib functions */

    if (return_()) {
	return 0;
    }
    chkin_("FLGRPT", (ftnlen)6);

/*     First find the widest of the names: */

    width = 0;
    i__1 = *nitems;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (rtrim_(names + (i__ - 1) * names_len, names_len) > width) {
	    width = rtrim_(names + (i__ - 1) * names_len, names_len);
	}
    }

/*     Now for each of the NAME/VALUE pairs construct a style */
/*     string using NAMES and run the VALUES through NICEPR_1. */

    i__1 = *nitems;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        First we need to find a character that is not used */
/*        in the NAMES(I)/VALUES(I) pair.  We will use this as */
/*        a hardspace in our style string. */

	for (j = 33; j <= 127; ++j) {
	    free[(i__2 = j) < 129 && 0 <= i__2 ? i__2 : s_rnge("free", i__2, 
		    "flgrpt_", (ftnlen)102)] = TRUE_;
	}
	i__2 = width;
	for (j = 1; j <= i__2; ++j) {
	    free[(i__3 = *(unsigned char *)&names[(i__ - 1) * names_len + (j 
		    - 1)]) < 129 && 0 <= i__3 ? i__3 : s_rnge("free", i__3, 
		    "flgrpt_", (ftnlen)106)] = FALSE_;
	}
	i__2 = i_len(values, values_len);
	for (j = 1; j <= i__2; ++j) {
	    free[(i__3 = *(unsigned char *)&values[(i__ - 1) * values_len + (
		    j - 1)]) < 129 && 0 <= i__3 ? i__3 : s_rnge("free", i__3, 
		    "flgrpt_", (ftnlen)110)] = FALSE_;
	}
	j = 33;
	while(! free[(i__2 = j) < 129 && 0 <= i__2 ? i__2 : s_rnge("free", 
		i__2, "flgrpt_", (ftnlen)114)] && j < 127) {
	    ++j;
	}
	*(unsigned char *)hard = (char) j;

/*        Set up the style we are going to use for this */
/*        value */

	nspmrg_(style, (ftnlen)200);
	suffix_("HARDSPACE", &c__1, style, (ftnlen)9, (ftnlen)200);
	suffix_(hard, &c__1, style, (ftnlen)1, (ftnlen)200);
	suffix_("FLAG", &c__1, style, (ftnlen)4, (ftnlen)200);
	l = rtrim_(style, (ftnlen)200) + 2;
	i__2 = width;
	for (k = 1; k <= i__2; ++k) {
	    *(unsigned char *)letter = *(unsigned char *)&names[(i__ - 1) * 
		    names_len + (k - 1)];
	    if (*(unsigned char *)letter == ' ') {
		*(unsigned char *)&style[l - 1] = *(unsigned char *)hard;
	    } else {
		*(unsigned char *)&style[l - 1] = *(unsigned char *)letter;
	    }
	    ++l;
	}
	*(unsigned char *)&style[l - 1] = ':';
	++l;
	*(unsigned char *)&style[l - 1] = *(unsigned char *)hard;

/*        Ok.  Now just ship the stuff to the output routines. */

	if (s_cmp(names + (i__ - 1) * names_len, " ", names_len, (ftnlen)1) ==
		 0 && s_cmp(values + (i__ - 1) * values_len, " ", values_len, 
		(ftnlen)1) == 0) {
	    i__2 = l - 2;
	    s_copy(style + i__2, hard, l - 1 - i__2, (ftnlen)1);
	    nicepr_1__(hard, style, (U_fp)myio, (ftnlen)1, l);
	} else if (s_cmp(values + (i__ - 1) * values_len, " ", values_len, (
		ftnlen)1) == 0) {
	    i__2 = l - 2;
	    s_copy(style + i__2, hard, l - 1 - i__2, (ftnlen)1);
	    nicepr_1__(hard, style, (U_fp)myio, (ftnlen)1, l);
	} else {
	    nicepr_1__(values + (i__ - 1) * values_len, style, (U_fp)myio, 
		    values_len, l);
	}
    }
    chkout_("FLGRPT", (ftnlen)6);
    return 0;
} /* flgrpt_ */

