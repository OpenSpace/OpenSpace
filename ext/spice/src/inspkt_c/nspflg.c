/* nspflg.f -- translated by f2c (version 19980913).
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

/* Subroutine */ int nspflg_(integer *idlist, integer *n, integer *from, 
	integer *to, integer *every, char *format, ftnlen format_len)
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char name__[32];
    integer skip, wdth, size[3], i__, k, r__;
    extern /* Subroutine */ int cladv_(logical *);
    integer space;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer freqf, freqh;
    logical found;
    integer shift, width[3];
    char title[132];
    integer freqt, count;
    extern integer rtrim_(char *, ftnlen);
    logical justr[3];
    integer id[3];
    char lr[32];
    extern /* Subroutine */ int clgqal_(integer *, char *, ftnlen);
    integer lmarge, pageht, pagewd;
    char spcial[32*3];
    extern /* Subroutine */ int pagscn_(char *, ftnlen), clncmp_(integer *, 
	    integer *), pagset_(char *, integer *, ftnlen), tabrpt_(integer *,
	     integer *, integer *, integer *, logical *, logical *, char *, 
	    integer *, integer *, U_fp, ftnlen), chkout_(char *, ftnlen);
    integer nitems;
    logical dotitl;
    extern /* Subroutine */ int shiftr_(char *, integer *, char *, char *, 
	    ftnlen, ftnlen, ftnlen), pagrst_(void), pagput_(char *, ftnlen);
    extern /* Subroutine */ int nspfrp_();
    extern logical return_(void);
    extern /* Subroutine */ int nspfrw_(integer *);
    logical presrv[3], prserv;
    extern /* Subroutine */ int bbgetc_1__(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen), bbgeti_1__(char *, char *, integer *, 
	    integer *, ftnlen, ftnlen);


/*     Functions to pass on to others. */


/*     Spicelib Functions */


/*     Local Parameters */


/*     Local Variables */

    if (return_()) {
	return 0;
    }
    chkin_("NSPFLG", (ftnlen)6);

/*     Determine whether we are in FLAGGED or FLAGGED PRESERVED format */

    if (s_cmp(format, "FLAGGED", format_len, (ftnlen)7) == 0) {
	prserv = FALSE_;
    } else {
	prserv = TRUE_;
    }
    wdth = 0;

/*     First  set up the page parameters. */

/*     If still, here, the page is wide enough to hold everything. */
/*     It is time to fetch and set all of the page attributes. */

    bbgeti_1__("COPY", "PAGEWIDTH", &k, &pagewd, (ftnlen)4, (ftnlen)9);
    bbgeti_1__("COPY", "PAGEHEIGHT", &k, &pageht, (ftnlen)4, (ftnlen)10);
    bbgeti_1__("COPY", "TITLEFREQUENCY", &k, &freqt, (ftnlen)4, (ftnlen)14);
    bbgeti_1__("COPY", "HEADERFREQUENCY", &k, &freqh, (ftnlen)4, (ftnlen)15);
    freqf = -1;
    bbgetc_1__("COPY", "PAGETITLE", &k, title, (ftnlen)4, (ftnlen)9, (ftnlen)
	    132);
    bbgetc_1__("COPY", "TITLEJUSTIFICATION", &k, lr, (ftnlen)4, (ftnlen)18, (
	    ftnlen)32);
    r__ = rtrim_(title, (ftnlen)132);
    dotitl = s_cmp(title, " ", (ftnlen)132, (ftnlen)1) != 0;
    if (s_cmp(lr, "LEFT", (ftnlen)32, (ftnlen)4) == 0) {
	shift = 0;
    } else if (s_cmp(lr, "RIGHT", (ftnlen)32, (ftnlen)5) == 0) {
	shift = pagewd - r__;
    } else if (s_cmp(lr, "CENTER", (ftnlen)32, (ftnlen)6) == 0) {
	shift = (pagewd - r__) / 2;
    }
    shiftr_(title, &shift, " ", title, (ftnlen)132, (ftnlen)1, (ftnlen)132);
    pagrst_();
    pagset_("TITLEFREQUENCY", &freqt, (ftnlen)14);
    pagset_("HEADERFREQUENCY", &freqh, (ftnlen)15);
    pagset_("FOOTERFREQUENCY", &freqt, (ftnlen)15);
    pagset_("PAGEWIDTH", &pagewd, (ftnlen)9);
    pagset_("PAGEHEIGHT", &pageht, (ftnlen)10);
    if (dotitl) {
	pagscn_("TITLE", (ftnlen)5);
	pagput_(" ", (ftnlen)1);
	pagput_(" ", (ftnlen)1);
	pagput_(title, (ftnlen)132);
	pagput_(" ", (ftnlen)1);
    }

/*     Now set the output section to be the body of the report. */

    pagscn_("BODY", (ftnlen)4);

/*     Next set up the non-volatile portions of the report. */

/*     We shall leave no spaces between adjacent columns in the */
/*     table. */

    space = 0;
    wdth = 0;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	clgqal_(&idlist[i__], name__, (ftnlen)32);
/* Computing MAX */
	i__2 = wdth, i__3 = rtrim_(name__, (ftnlen)32);
	wdth = max(i__2,i__3);
    }
    wdth = min(32,wdth);

/*     Pass the width of the widest column alias onto the */
/*     routine NSPFRP which when given an ID and component */

    nspfrw_(&wdth);
    nitems = 3;
    width[0] = wdth;
    width[1] = 2;
    width[2] = pagewd - width[0] - width[1] - (space << 1);
    justr[0] = FALSE_;
    justr[1] = FALSE_;
    justr[2] = FALSE_;
    presrv[0] = prserv;
    presrv[1] = prserv;
    presrv[2] = prserv;
    s_copy(spcial, " ", (ftnlen)32, (ftnlen)1);
    s_copy(spcial + 32, " ", (ftnlen)32, (ftnlen)1);
    s_copy(spcial + 64, " ", (ftnlen)32, (ftnlen)1);
    size[0] = 1;
    size[1] = 1;
    lmarge = 1;

/*     Advance to the first row of the current scope of the query. */

    i__1 = *from;
    for (i__ = 1; i__ <= i__1; ++i__) {
	cladv_(&found);
    }
    count = *from;
    while(found) {

/*        For each column, we pass the opposite of its ID, */
/*        zero and the ID. This will cause NSPFRP to fetch */
/*        the column alias, a separator and the column value. */

	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    id[0] = -idlist[i__];
	    id[1] = 0;
	    id[2] = idlist[i__];
	    clncmp_(&idlist[i__], &size[2]);
	    tabrpt_(&nitems, id, size, width, justr, presrv, spcial, &lmarge, 
		    &space, (U_fp)nspfrp_, (ftnlen)32);
	}

/*        Separate lines of the report by blank lines. */

	pagput_(" ", (ftnlen)1);

/*        Now fetch the next row within the scope of the current */
/*        query. */

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

/*     Finally fill out the rest of the page. */

    pagset_("TITLEFREQUENCY", &c__0, (ftnlen)14);
    pagset_("HEADERFREQUENCY", &c__0, (ftnlen)15);
    pagput_(" ", (ftnlen)1);
    pagput_(" ", (ftnlen)1);
    chkout_("NSPFLG", (ftnlen)6);
    return 0;
} /* nspflg_ */

