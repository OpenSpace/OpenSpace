/* nsptab.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__60 = 60;
static integer c__1 = 1;
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

/* Subroutine */ int nsptab_(integer *idlist, integer *n, integer *from, 
	integer *to, integer *every, char *format, ftnlen format_len)
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5, i__6;
    doublereal d__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen), i_dnnt(doublereal *);

    /* Local variables */
    char name__[32], line[170];
    integer myid, last, skip, size[61], reqw[3];
    char type__[32], text[1920];
    integer i__;
    extern /* Subroutine */ int clgac_(integer *, char *, char *, ftnlen, 
	    ftnlen);
    integer k, m;
    extern /* Subroutine */ int clgai_(integer *, char *, integer *, integer *
	    , ftnlen);
    extern logical batch_(void);
    integer r__, w;
    extern /* Subroutine */ int cladv_(logical *);
    integer space;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer freqf;
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen);
    integer freqh;
    logical found;
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen);
    integer shift, width[61];
    char title[132];
    integer freqt, total;
    logical lstat[3];
    integer count, wmods[60];
    extern integer rtrim_(char *, ftnlen);
    integer start;
    logical sstat[3];
    char style[132];
    logical justr[61];
    integer w1, w2;
    extern /* Subroutine */ int nsptv_();
    integer w3;
    extern /* Subroutine */ int clq2id_(integer *, integer *);
    integer id;
    extern /* Subroutine */ int fetcha_();
    char lr[32];
    logical spaced;
    integer adjval;
    logical marked;
    integer lmarge, pageht, pagewd;
    char spcial[32*61];
    extern /* Subroutine */ int pagscn_(char *, ftnlen), clncmp_(integer *, 
	    integer *), pagset_(char *, integer *, ftnlen);
    integer iorder[60];
    logical adjust;
    extern /* Subroutine */ int sigerr_(char *, ftnlen);
    integer mwidth[60];
    char myline[132];
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    integer nitems;
    logical dotitl;
    extern /* Subroutine */ int nspioh_(char *, ftnlen), setmsg_(char *, 
	    ftnlen), errint_(char *, integer *, ftnlen), suffix_(char *, 
	    integer *, char *, ftnlen, ftnlen), nspmrg_(char *, ftnlen), 
	    orderi_(integer *, integer *, integer *), shiftr_(char *, integer 
	    *, char *, char *, ftnlen, ftnlen, ftnlen), pagrst_(void), 
	    pagput_(char *, ftnlen), tabrpt_(integer *, integer *, integer *, 
	    integer *, logical *, logical *, char *, integer *, integer *, 
	    U_fp, ftnlen), nspgst_(char *, logical *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int nspwln_(char *, ftnlen);
    logical presrv[61];
    extern /* Subroutine */ int nsppst_(char *, logical *, ftnlen), 
	    bbgetc_1__(char *, char *, integer *, char *, ftnlen, ftnlen, 
	    ftnlen), bbgeti_1__(char *, char *, integer *, integer *, ftnlen, 
	    ftnlen), cnfirm_1__(char *, logical *, ftnlen), nicepr_1__(char *,
	     char *, S_fp, ftnlen, ftnlen);


/* $ Version */

/*      Inspekt Routine Version 1.3.0 22-APR-1997 (WLT) */

/*         Changed call to SETERR regarding unknown FORMAT so that */
/*         the value of format is set using ERRCH instead of */
/*         concatenation. */

/*      Inspekt Routine Version 1.1.0  4-AUG-1995 (WLT) */

/*       Added a blank line to the header of a report and */
/*       removed one from the bottom of the title of the */
/*       report */




/*     Functions to pass on to others. */


/*     Spicelib Functions */


/*     Local Parameters */


/*     Local Variables */


/*     The function RSCALE is used to adjust the widths of columns */
/*     in cases where they will not all fit on the output page.  This */
/*     is strictly a hueristic function.  It just seems to look right */
/*     The scaling is linear as a function of the width of the column */
/*     Columns 8 wide will be scaled to a width of 6 (a factor of .75) */
/*     Columns that are 80 wide will be scaled to a width of 36 ( a */
/*     factor of 0.45 )  The max and min are just there to make */
/*     sure that things don't get out of control if repeated scalings */
/*     are applied). */

    if (return_()) {
	return 0;
    }
    chkin_("NSPTAB", (ftnlen)6);
    s_copy(line, "=========================================================="
	    "================================================================"
	    "==============================================", (ftnlen)170, (
	    ftnlen)168);
    if (*n >= 60) {
	setmsg_("You have requested # or more columns for in the current rep"
		"ort. You must keep the total number of columns requested to "
		"less than # columns. ", (ftnlen)140);
	errint_("#", &c__60, (ftnlen)1);
	errint_("#", &c__60, (ftnlen)1);
	chkout_("NSPTAB", (ftnlen)6);
	return 0;
    }

/*     We shall leave two spaces between adjacent columns in the */
/*     table. */

    space = 2;

/*     The id list we pass to the report generator will */
/*     begin either at 0 (marked reports) or 1 (unmarked). */
/*     We will adjust the start point if we find out we have */
/*     a marked report. */

    start = 1;

/*     Fetch the widths of all of the columns */

    width[0] = 1;
    justr[0] = FALSE_;
    size[0] = 1;
    s_copy(spcial, " ", (ftnlen)32, (ftnlen)1);
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	clq2id_(&idlist[i__], &myid);
	clgai_(&myid, "WIDTH", &m, &width[(i__2 = i__) < 61 && 0 <= i__2 ? 
		i__2 : s_rnge("width", i__2, "nsptab_", (ftnlen)222)], (
		ftnlen)5);
	clgac_(&myid, "JUSTIFICATION", lr, (ftnlen)13, (ftnlen)32);
	justr[(i__2 = i__) < 61 && 0 <= i__2 ? i__2 : s_rnge("justr", i__2, 
		"nsptab_", (ftnlen)225)] = s_cmp(lr, "RIGHT", (ftnlen)32, (
		ftnlen)5) == 0;
	size[(i__2 = i__) < 61 && 0 <= i__2 ? i__2 : s_rnge("size", i__2, 
		"nsptab_", (ftnlen)226)] = 1;
	s_copy(spcial + (((i__2 = i__) < 61 && 0 <= i__2 ? i__2 : s_rnge(
		"spcial", i__2, "nsptab_", (ftnlen)227)) << 5), " ", (ftnlen)
		32, (ftnlen)1);
    }

/*     Although it should never happen, it is easier to figure it */
/*     out here than somewhere else.  See if any of the WIDTHS */
/*     are non-positive */

    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	clq2id_(&idlist[i__], &myid);
	if (width[(i__2 = i__) < 61 && 0 <= i__2 ? i__2 : s_rnge("width", 
		i__2, "nsptab_", (ftnlen)239)] <= 0) {
	    setmsg_("The column, #, has a non-positive width. The width repo"
		    "rted by the column manager was: #", (ftnlen)88);
	    clgac_(&myid, "NAME", name__, (ftnlen)4, (ftnlen)32);
	    errch_("#", name__, (ftnlen)1, (ftnlen)32);
	    errint_("#", &width[(i__2 = i__) < 61 && 0 <= i__2 ? i__2 : 
		    s_rnge("width", i__2, "nsptab_", (ftnlen)247)], (ftnlen)1)
		    ;
	    sigerr_("INSPEKT(BADWIDTH)", (ftnlen)17);
	    chkout_("NSPTAB", (ftnlen)6);
	    return 0;
	}
    }
    if (s_cmp(format, "MARKED TABULAR", format_len, (ftnlen)14) == 0) {
	marked = TRUE_;
	start = 0;
	idlist[0] = 0;
	nitems = *n + 1;
	spaced = FALSE_;
	lmarge = 4;
	i__1 = *n;
	for (i__ = start; i__ <= i__1; ++i__) {
	    presrv[(i__2 = i__) < 61 && 0 <= i__2 ? i__2 : s_rnge("presrv", 
		    i__2, "nsptab_", (ftnlen)265)] = FALSE_;
	}
    } else if (s_cmp(format, "MARKED TABULAR PRESERVED", format_len, (ftnlen)
	    24) == 0) {
	marked = TRUE_;
	start = 0;
	idlist[0] = 0;
	nitems = *n + 1;
	spaced = FALSE_;
	lmarge = 4;
	i__1 = *n;
	for (i__ = start; i__ <= i__1; ++i__) {
	    presrv[(i__2 = i__) < 61 && 0 <= i__2 ? i__2 : s_rnge("presrv", 
		    i__2, "nsptab_", (ftnlen)279)] = TRUE_;
	}
    } else if (s_cmp(format, "TABULAR PRESERVED", format_len, (ftnlen)17) == 
	    0) {
	marked = FALSE_;
	start = 1;
	idlist[0] = 0;
	nitems = *n;
	spaced = FALSE_;
	lmarge = 1;
	i__1 = *n;
	for (i__ = start; i__ <= i__1; ++i__) {
	    presrv[(i__2 = i__) < 61 && 0 <= i__2 ? i__2 : s_rnge("presrv", 
		    i__2, "nsptab_", (ftnlen)292)] = TRUE_;
	}
    } else if (s_cmp(format, "TABULAR", format_len, (ftnlen)7) == 0) {
	marked = FALSE_;
	start = 1;
	idlist[0] = 0;
	nitems = *n;
	spaced = FALSE_;
	lmarge = 1;
	i__1 = *n;
	for (i__ = start; i__ <= i__1; ++i__) {
	    presrv[(i__2 = i__) < 61 && 0 <= i__2 ? i__2 : s_rnge("presrv", 
		    i__2, "nsptab_", (ftnlen)305)] = FALSE_;
	}
    } else if (s_cmp(format, "SPACED TABULAR", format_len, (ftnlen)14) == 0) {
	marked = FALSE_;
	start = 1;
	idlist[0] = 0;
	nitems = *n;
	spaced = TRUE_;
	lmarge = 1;
	i__1 = *n;
	for (i__ = start; i__ <= i__1; ++i__) {
	    presrv[(i__2 = i__) < 61 && 0 <= i__2 ? i__2 : s_rnge("presrv", 
		    i__2, "nsptab_", (ftnlen)318)] = FALSE_;
	}
    } else if (s_cmp(format, "SPACED TABULAR PRESERVED", format_len, (ftnlen)
	    24) == 0) {
	marked = FALSE_;
	start = 1;
	idlist[0] = 0;
	nitems = *n;
	spaced = TRUE_;
	lmarge = 1;
	i__1 = *n;
	for (i__ = start; i__ <= i__1; ++i__) {
	    presrv[(i__2 = i__) < 61 && 0 <= i__2 ? i__2 : s_rnge("presrv", 
		    i__2, "nsptab_", (ftnlen)332)] = TRUE_;
	}
    } else {
	setmsg_("The format supplied to NSPTAB was not one that it is prepar"
		"ed to accept. The format requested was: \"#\".", (ftnlen)103);
	errch_("#", format, (ftnlen)1, format_len);
	sigerr_("INSPEKT(UNKNOWNFORMAT)", (ftnlen)22);
	chkout_("NSPTAB", (ftnlen)6);
	return 0;
    }

/*     See if the page is wide enough to hold all of the columns. */

    bbgeti_1__("COPY", "PAGEWIDTH", &k, &pagewd, (ftnlen)4, (ftnlen)9);
    total = -space;
    i__1 = *n;
    for (i__ = start; i__ <= i__1; ++i__) {
	total = total + width[(i__2 = i__) < 61 && 0 <= i__2 ? i__2 : s_rnge(
		"width", i__2, "nsptab_", (ftnlen)356)] + space;
    }
    if (total > pagewd) {
	bbgeti_1__("COPY", "AUTOADJUST", &k, &adjval, (ftnlen)4, (ftnlen)10);

/*        First see if we can adjust anything so that everything */
/*        will fit on the screen. We shall adjust character column */
/*        widths by scaling them.  The scale facter shall be given */
/*        by (-1/240)*WIDTH + .783333334 */

/*        This function Scales 8 to 6 and  80 to 36 */

	reqw[0] = -space;
	reqw[1] = -space;
	reqw[2] = -space;
	if (start <= 0) {
	    reqw[0] = width[0] + space;
	    reqw[1] = width[0] + space;
	    reqw[2] = width[0] + space;
	}
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    reqw[0] += space;
	    reqw[1] += space;
	    reqw[2] += space;
	    clq2id_(&idlist[i__], &myid);
	    clgac_(&myid, "TYPE", type__, (ftnlen)4, (ftnlen)32);
	    if (s_cmp(type__, "CHAR", (ftnlen)4, (ftnlen)4) != 0) {
		reqw[0] += width[(i__2 = i__) < 61 && 0 <= i__2 ? i__2 : 
			s_rnge("width", i__2, "nsptab_", (ftnlen)394)];
		reqw[1] += width[(i__2 = i__) < 61 && 0 <= i__2 ? i__2 : 
			s_rnge("width", i__2, "nsptab_", (ftnlen)395)];
		reqw[2] += width[(i__2 = i__) < 61 && 0 <= i__2 ? i__2 : 
			s_rnge("width", i__2, "nsptab_", (ftnlen)396)];
	    } else {
		w = width[(i__2 = i__) < 61 && 0 <= i__2 ? i__2 : s_rnge(
			"width", i__2, "nsptab_", (ftnlen)398)];
/* Computing MAX */
/* Computing MIN */
		d__1 = (.78333334 - (doublereal) w / 240.) * (doublereal) w;
		i__4 = 40, i__5 = i_dnnt(&d__1);
		i__2 = 6, i__3 = min(i__4,i__5);
		w1 = max(i__2,i__3);
/* Computing MAX */
/* Computing MIN */
		d__1 = (.78333334 - (doublereal) w1 / 240.) * (doublereal) w1;
		i__4 = 40, i__5 = i_dnnt(&d__1);
		i__2 = 6, i__3 = min(i__4,i__5);
		w2 = max(i__2,i__3);
/* Computing MAX */
/* Computing MIN */
		d__1 = (.78333334 - (doublereal) w2 / 240.) * (doublereal) w2;
		i__4 = 40, i__5 = i_dnnt(&d__1);
		i__2 = 6, i__3 = min(i__4,i__5);
		w3 = max(i__2,i__3);
		reqw[0] += w1;
		reqw[1] += w2;
		reqw[2] += w3;
	    }
	}
	if (reqw[2] > pagewd || adjval == 0 || batch_() && adjval == 1) {
	    setmsg_("The data requested will not fit within the space availa"
		    "ble on a page.  The page width is set at #.  The report "
		    "specified would require a width of #. You will need to a"
		    "djust some combination of column selection, column width"
		    "s, and page width.  Alternatively, you can set report fo"
		    "rmat to FLAGGED or VERBATIM. ", (ftnlen)308);
	    errint_("#", &pagewd, (ftnlen)1);
	    errint_("#", &total, (ftnlen)1);
	    sigerr_("INSPEKT(REPORTTOOWIDE)", (ftnlen)22);
	    chkout_("NSPTAB", (ftnlen)6);
	    return 0;
	} else if (adjval == 1) {
	    nspgst_("SAVE", sstat, (ftnlen)4);
	    nspgst_("LOG", lstat, (ftnlen)3);
	    nspioh_("SAVE", (ftnlen)4);
	    nspioh_("LOG", (ftnlen)3);
	    s_copy(text, "The data requested will not fit within the current"
		    " page width. The page width is set at #.  The report spe"
		    "cified would require a width of #. /cr/cr However, by te"
		    "mporarily adjusting the column widths for character colu"
		    "mns (for this report only) I can fit all of the data on "
		    "the page. /cr/cr", (ftnlen)1920, (ftnlen)290);
	    repmi_(text, "#", &pagewd, text, (ftnlen)1920, (ftnlen)1, (ftnlen)
		    1920);
	    repmi_(text, "#", &total, text, (ftnlen)1920, (ftnlen)1, (ftnlen)
		    1920);
	    nspmrg_(style, (ftnlen)132);
	    suffix_("NEWLINE /cr", &c__1, style, (ftnlen)11, (ftnlen)132);
	    nspwln_(" ", (ftnlen)1);
	    nicepr_1__(text, style, (S_fp)nspwln_, (ftnlen)1920, (ftnlen)132);
	    nspwln_(" ", (ftnlen)1);
	    cnfirm_1__("Should I adjust columns widths (Y/N)? :", &adjust, (
		    ftnlen)39);
	    nsppst_("SAVE", sstat, (ftnlen)4);
	    nsppst_("LOG", lstat, (ftnlen)3);
	    if (! adjust) {
		chkout_("NSPTAB", (ftnlen)6);
		return 0;
	    }
	}

/*        We are going to adjust the widths of columns but we are going */
/*        to attempt to do this in a way that will favor reducing wide */
/*        columns instead of those that are already narrow.  (Sort of a */
/*        progressive tax system). However, once a column is adjusted we */
/*        are not going to adjust it again until we've determined that */
/*        it still has more than its fair share of the screen.  The */
/*        adjustment is done using the RSCALE function.  So when a */
/*        column ID is adjusted its width, W,  is replaced by RSCALE(W). */
/*        But, the modified width is replaced by */
/*        RSCALE(RSCALE(W))-4*MODS that way we wont be adjusting it for */
/*        a while unless it is so wide that even with a second */
/*        adjustment it would end up being the widest column in the */
/*        table.  This way once a column has been adjusted twice every */
/*        other column will have to be adjusted at least once before it */
/*        will be adjusted a third time.  No column will be adjusted */
/*        more than three times. */

	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    wmods[(i__2 = i__ - 1) < 60 && 0 <= i__2 ? i__2 : s_rnge("wmods", 
		    i__2, "nsptab_", (ftnlen)491)] = 0;
	    mwidth[(i__2 = i__ - 1) < 60 && 0 <= i__2 ? i__2 : s_rnge("mwidth"
		    , i__2, "nsptab_", (ftnlen)492)] = width[(i__3 = i__) < 
		    61 && 0 <= i__3 ? i__3 : s_rnge("width", i__3, "nsptab_", 
		    (ftnlen)492)];
	}
	while(total > pagewd) {
	    orderi_(mwidth, n, iorder);
	    last = *n;
	    id = iorder[(i__1 = last - 1) < 60 && 0 <= i__1 ? i__1 : s_rnge(
		    "iorder", i__1, "nsptab_", (ftnlen)501)];
	    clq2id_(&idlist[id], &myid);
	    clgac_(&myid, "TYPE", type__, (ftnlen)4, (ftnlen)32);
	    while(s_cmp(type__, "CHAR", (ftnlen)4, (ftnlen)4) != 0 || wmods[(
		    i__1 = id - 1) < 60 && 0 <= i__1 ? i__1 : s_rnge("wmods", 
		    i__1, "nsptab_", (ftnlen)507)] >= 3) {
		--last;
		id = iorder[(i__1 = last - 1) < 60 && 0 <= i__1 ? i__1 : 
			s_rnge("iorder", i__1, "nsptab_", (ftnlen)511)];
		clq2id_(&idlist[id], &myid);
		clgac_(&myid, "TYPE", type__, (ftnlen)4, (ftnlen)32);
	    }
	    w = width[(i__1 = id) < 61 && 0 <= i__1 ? i__1 : s_rnge("width", 
		    i__1, "nsptab_", (ftnlen)517)];
/* Computing MAX */
/* Computing MIN */
	    d__1 = (.78333334 - (doublereal) w / 240.) * (doublereal) w;
	    i__3 = 40, i__4 = i_dnnt(&d__1);
	    i__1 = 6, i__2 = min(i__3,i__4);
	    w = max(i__1,i__2);
	    total = total + w - width[(i__1 = id) < 61 && 0 <= i__1 ? i__1 : 
		    s_rnge("width", i__1, "nsptab_", (ftnlen)519)];
	    width[(i__1 = id) < 61 && 0 <= i__1 ? i__1 : s_rnge("width", i__1,
		     "nsptab_", (ftnlen)520)] = w;
	    wmods[(i__1 = id - 1) < 60 && 0 <= i__1 ? i__1 : s_rnge("wmods", 
		    i__1, "nsptab_", (ftnlen)521)] = wmods[(i__2 = id - 1) < 
		    60 && 0 <= i__2 ? i__2 : s_rnge("wmods", i__2, "nsptab_", 
		    (ftnlen)521)] + 1;
/* Computing MAX */
/* Computing MIN */
	    d__1 = (.78333334 - (doublereal) w / 240.) * (doublereal) w;
	    i__5 = 40, i__6 = i_dnnt(&d__1);
	    i__3 = 6, i__4 = min(i__5,i__6);
	    mwidth[(i__1 = id - 1) < 60 && 0 <= i__1 ? i__1 : s_rnge("mwidth",
		     i__1, "nsptab_", (ftnlen)522)] = max(i__3,i__4) - (wmods[
		    (i__2 = id - 1) < 60 && 0 <= i__2 ? i__2 : s_rnge("wmods",
		     i__2, "nsptab_", (ftnlen)522)] << 2);
	    orderi_(mwidth, n, iorder);
	}
    }

/*     If still, here, the page is wide enough to hold everything. */
/*     It is time to fetch and set all of the page attributes. */

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
	shift = total - r__;
    } else if (s_cmp(lr, "CENTER", (ftnlen)32, (ftnlen)6) == 0) {
	shift = (total - r__) / 2;
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
    }

/*     Now set up the header. */

    pagscn_("HEADER", (ftnlen)6);
    pagput_(" ", (ftnlen)1);
    tabrpt_(n, &idlist[1], &size[1], &width[1], &justr[1], &presrv[1], spcial 
	    + 32, &lmarge, &space, (U_fp)fetcha_, (ftnlen)32);
    s_copy(myline, line, total, (ftnlen)170);
    if (total < 132) {
	i__1 = total;
	s_copy(myline + i__1, " ", 132 - i__1, (ftnlen)1);
    }
    pagput_(myline, (ftnlen)132);

/*     Now reset the left margin to 1 since reports begin in */
/*     column 1 regardless of where their headers start. */

    lmarge = 1;

/*     Finally, begin fetching and printing output from the */
/*     E-kernel. */

    pagscn_("BODY", (ftnlen)4);
    i__1 = *from;
    for (i__ = 1; i__ <= i__1; ++i__) {
	cladv_(&found);
    }
    count = *from;
    while(found) {

/*        We need to get the size of each of the columns for this */
/*        row. (Note if IDLIST(0) is active we don't need to adjust */
/*        it since it is used only for the report marker and has */
/*        a fixed size of 1.) */

	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    clncmp_(&idlist[i__], &size[(i__2 = i__) < 61 && 0 <= i__2 ? i__2 
		    : s_rnge("size", i__2, "nsptab_", (ftnlen)624)]);
	}
	tabrpt_(&nitems, &idlist[start], &size[(i__1 = start) < 61 && 0 <= 
		i__1 ? i__1 : s_rnge("size", i__1, "nsptab_", (ftnlen)627)], &
		width[(i__2 = start) < 61 && 0 <= i__2 ? i__2 : s_rnge("width"
		, i__2, "nsptab_", (ftnlen)627)], &justr[(i__3 = start) < 61 
		&& 0 <= i__3 ? i__3 : s_rnge("justr", i__3, "nsptab_", (
		ftnlen)627)], &presrv[(i__4 = start) < 61 && 0 <= i__4 ? i__4 
		: s_rnge("presrv", i__4, "nsptab_", (ftnlen)627)], spcial + ((
		(i__5 = start) < 61 && 0 <= i__5 ? i__5 : s_rnge("spcial", 
		i__5, "nsptab_", (ftnlen)627)) << 5), &lmarge, &space, (U_fp)
		nsptv_, (ftnlen)32);
	if (spaced) {
	    pagput_(" ", (ftnlen)1);
	}

/*        Advance to the next row of the query. */

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
    chkout_("NSPTAB", (ftnlen)6);
    return 0;
} /* nsptab_ */

