/* nspvrb.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
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

/* Subroutine */ int nspvrb_(integer *idlist, integer *n, integer *from, 
	integer *to, integer *every)
{
    /* System generated locals */
    address a__1[3];
    integer i__1, i__2, i__3, i__4[3];

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen),
	     s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char name__[32];
    integer skip, wdth, i__, j, k, r__;
    extern /* Subroutine */ int cladv_(logical *), chkin_(char *, ftnlen);
    integer freqf, freqh;
    logical found;
    integer shift, width;
    char title[132];
    integer freqt, count;
    extern integer rtrim_(char *, ftnlen);
    char lr[32];
    extern /* Subroutine */ int clgqal_(integer *, char *, ftnlen);
    integer pagewd, pageht;
    extern /* Subroutine */ int pagscn_(char *, ftnlen), clncmp_(integer *, 
	    integer *), clpval_(integer *, integer *, char *, integer *, 
	    ftnlen), pagset_(char *, integer *, ftnlen);
    char myline[132];
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    logical dotitl;
    extern /* Subroutine */ int shiftr_(char *, integer *, char *, char *, 
	    ftnlen, ftnlen, ftnlen), pagrst_(void), pagput_(char *, ftnlen);
    extern logical return_(void);
    integer num;
    extern /* Subroutine */ int bbgetc_1__(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen), bbgeti_1__(char *, char *, integer *, 
	    integer *, ftnlen, ftnlen);


/*     Version 1.1  27-MAR-1998  W.L. Taber */

/*        Modified the original version to remove an unneeded */
/*        EXTERNAL declaration of NSPWLN. */


/*     Spicelib Functions */


/*     Local Parameters */

/*     Local Variables */

    if (return_()) {
	return 0;
    }
    chkin_("NSPVRB", (ftnlen)6);

/*     First  set up the page parameters. */

/*     If still, here, the page is wide enough to hold everything. */
/*     It is time to fetch and set all of the page attributes. */

    bbgeti_1__("COPY", "PAGEWIDTH", &k, &pagewd, (ftnlen)4, (ftnlen)9);
    bbgeti_1__("COPY", "PAGEHEIGHT", &k, &pageht, (ftnlen)4, (ftnlen)10);
    bbgeti_1__("COPY", "TITLEFREQUENCY", &k, &freqt, (ftnlen)4, (ftnlen)14);
    bbgeti_1__("COPY", "HEADERFREQUENCY", &k, &freqh, (ftnlen)4, (ftnlen)15);
    freqf = -1;

/*     Set up the title section of the report. */

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
	pagput_("==========================================================="
		"============================================================"
		"=================================================", (ftnlen)
		168);
	pagput_(" ", (ftnlen)1);
    }
    pagscn_("BODY", (ftnlen)4);

/*     Find the width of the widest column name. */

    wdth = 0;
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	clgqal_(&idlist[i__], name__, (ftnlen)32);
/* Computing MAX */
	i__2 = wdth, i__3 = rtrim_(name__, (ftnlen)32);
	wdth = max(i__2,i__3);
    }

/*     Advance to the first row of the current scope of the query. */

    i__1 = *from;
    for (i__ = 1; i__ <= i__1; ++i__) {
	cladv_(&found);
    }
    count = *from;
    while(found) {
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {

/*           Get the name of this column, spruce it up a bit */
/*           and send it to the page manager. */

	    clgqal_(&idlist[i__], name__, (ftnlen)32);
/* Writing concatenation */
	    i__4[0] = 4, a__1[0] = "--- ";
	    i__4[1] = wdth, a__1[1] = name__;
	    i__4[2] = 5, a__1[2] = " --- ";
	    s_cat(myline, a__1, i__4, &c__3, (ftnlen)132);
	    pagput_(myline, (ftnlen)132);

/*           Now send every component of this column directly to */
/*           output.  No formatting. */

	    clncmp_(&idlist[i__], &num);
	    i__2 = num;
	    for (j = 1; j <= i__2; ++j) {
		clpval_(&idlist[i__], &j, myline, &width, (ftnlen)132);
		pagput_(myline, (ftnlen)132);
	    }
	}

/*        We always put a blank line after completing a row. */

	pagput_(" ", (ftnlen)1);

/*        Now fetch the next row within the scope of the current */
/*        query.  If we find something, we output a marker to */
/*        indicate the beginning of another row of the E-kernel */
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
	if (found) {
	    s_copy(myline, "------------------------------------------------"
		    "--------------------------------------------------------"
		    "--------------------------------------------------------"
		    "--------", (ftnlen)132, (ftnlen)168);
	    pagput_(myline, (ftnlen)132);
	}
    }

/*     Finally fill out the rest of the page. */

    pagset_("TITLEFREQUENCY", &c__0, (ftnlen)14);
    pagset_("HEADERFREQUENCY", &c__0, (ftnlen)15);
    pagput_(" ", (ftnlen)1);
    pagput_(" ", (ftnlen)1);
    chkout_("NSPVRB", (ftnlen)6);
    return 0;
} /* nspvrb_ */

