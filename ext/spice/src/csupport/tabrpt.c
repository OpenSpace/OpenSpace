/* tabrpt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__260 = 260;

/* $Procedure      TABRPT ( Table Format Report ) */
/* Subroutine */ int tabrpt_0_(int n__, integer *nitems, integer *item, 
	integer *size, integer *width, logical *justr, logical *presrv, char *
	spcial, integer *lmarge, integer *space, S_fp fetch, ftnlen 
	spcial_len)
{
    /* Initialized data */

    static char key[32] = "abort                           ";
    static char hrd[60] = "                                                 "
	    "           ";
    static logical dohrd = FALSE_;

    /* System generated locals */
    integer i__1, i__2, i__3, i__4;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static char page[132*520];
    static logical done[60];
    static integer left;
    static char long__[1024];
    static logical full;
    static integer last, wdth, room, i__, j;
    extern integer cardc_(char *, ftnlen);
    static integer l, r__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static char value[32];
    static integer right;
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen);
    static integer count[60], putat;
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    static char style[80*60];
    static integer nrows;
    extern /* Subroutine */ int rjust_(char *, char *, ftnlen, ftnlen);
    static integer id;
    extern logical failed_(void);
    static logical filled;
    static char buffer[132*266];
    extern /* Subroutine */ int replch_(char *, char *, char *, char *, 
	    ftnlen, ftnlen, ftnlen, ftnlen);
    static logical finish;
    extern /* Subroutine */ int pagpmt_(integer *, char *, ftnlen), chkout_(
	    char *, ftnlen), ssizec_(integer *, char *, ftnlen), pagput_(char 
	    *, ftnlen);
    extern integer qlstnb_(char *, ftnlen);
    extern /* Subroutine */ int suffix_(char *, integer *, char *, ftnlen, 
	    ftnlen);
    static integer toship;
    static char getstr[1024];
    static logical noroom;
    static integer maxrow;
    extern logical return_(void);
    static integer did, row[60];
    extern /* Subroutine */ int nicebt_1__(char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen);

/* $ Abstract */

/*     This routine creates a tabular report using the parameters */
/*     supplied for the arrangement of the report and the user */
/*     supplied routine that fetches the items to be placed in */
/*     the report. */

/* $ Required_Reading */

/*     REPORTS */

/* $ Keywords */

/*     IO */
/*     REPORTING */
/*     TABLE */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*      --------  ---  -------------------------------------------------- */
/*     NITEMS     I   The number of columns that should appear */
/*     ITEM       I   An array of item codes */
/*     SIZE       I   The number of components associated with the items */
/*     WIDTH      I   The room to allow for each item */
/*     JUSTR      I   Justify right */
/*     SPACE      I   The amount of space to place between columns */
/*     LMARGE     I   Location of the left margin */
/*     PRESRV     I   Logical indicating whether to preserve components */
/*     SPCIAL     I   Special characters to us/recognize in a column */
/*     FETCH      I   Name of a routine that will fetch data for an item. */
/*     MAXWDTH    P   The maximum width for the report. */
/*     MAXCOL     P   Maximum number of columns that can be supported. */

/* $ Detailed_Input */

/*     NITEMS     The number of columns that should appear in this */
/*                this block of the report. */

/*     ITEM       An array of id codes that can be used to fetch */
/*                the data strings that will be formatted into the */
/*                columns of this block of the report. */

/*     SIZE       The number of components associated with each item. */

/*     WIDTH      The maximum number of characters that may appear */
/*                across a column */

/*     JUSTR      A logical array.  If JUSTR(I) is true, then the */
/*                data for a column will be right justified. Otherwise */
/*                it will be left justified. */

/*     SPACE      The amount of space to place between columns */

/*     LMARGE     Location of the left margin */

/*     PRESRV     Logical indicating whether to preserve components */
/*                by starting each new component on a new line in */
/*                its column. */

/*     SPCIAL     Special instructions that may be used to alter the */
/*                style of output in a column.  For example you might */
/*                want to have leaders or a trailer so that the */
/*                report will have vertical bars between columns. */
/*                Or if the column has preserved spacing you might */
/*                choose to use a flag with each component (especially */
/*                if it is likely to wrap over several lines. */

/*     FETCH      Name of a routine that will fetch data for an item. */

/* $ Detailed_Output */


/* $ Parameters */

/*     MXWDTH    is the maximum width page that is supported for */
/*               report generation.  This parameter should never */
/*               be larger than the same parameter that is used */
/*               in the PAGE MANAGER routine PAGMAN. */

/*     MAXCOL    is the maximum number of columns that can appear */
/*               in a report */

/* $ Exceptions */

/*     1) If NITEMS is larger than MAXCOL the error */
/*        SPICE(TOOMANYCOLUMNS) will be signalled. */

/*     2) If the space required implied by WIDTHS, SPACE and LMARGE */
/*        is greater than MXWDTH the error SPICE(REPORTTOOWIDE) will */
/*        be signalled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine allows you to "easily" create nicely formatted */
/*     reports for output by your programs.  By setting the parameters */
/*     supplied on input together with the parameters that control */
/*     page layout as used by PAGMAN you can produce a wide variety of */
/*     report formats without having to deal with the details of */
/*     arranging the output on the screen. */

/* $ Examples */

/*     copy required reading examples here. */

/* $ Restrictions */

/*     This routine works in conjunction with the routine PAGMAN */
/*     and its entry points.  You need to be sure that PAGMAN has */
/*     been properly initialized before you begin using this routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */

/* $ Version */

/* -    Beta Version 3.0.0, 2-OCT-1996 (WLT) */

/*        Increased the internal buffer sizes and modified */
/*        the fetching logic so that the buffer will not fill */
/*        up and inadvertantly cut off data with no warning. */

/* -    Beta Version 2.0.0, 9-Aug-1995 (WLT) */

/*        Increased several buffer parameters and put in a check */
/*        for FAILED so that we can quit this thing if we need to. */

/* -    Beta Version 1.0.0, 1-JAN-1994 (WLT) */

/* -& */
/* $ Index_Entries */


/*     Arrange data in columns */

/* -& */

/*     SPICELIB functions */


/*     Other functions */


/*     Local parameters */


/*     The arrays below are used to store attributes on a column */
/*     by column basis. */

/*     STYLE  is the style to be used when formating text for an */
/*            individual column */

/*     COUNT  is a counter that is used to indicate how many components */
/*            have been processed for an individual column */

/*     ROW    keeps track of the last row in the local page where */
/*            formatted text was placed. */

/*     DONE   is a logical that indicates whether we have formatted */
/*            all of the data for a column. */


/*     Local variables */


/*     Saved variables */

    /* Parameter adjustments */
    if (item) {
	}
    if (size) {
	}
    if (width) {
	}
    if (justr) {
	}
    if (presrv) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_tababt;
	case 2: goto L_tabhrd;
	}


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("TABRPT", (ftnlen)6);

/*     Initialize the cell that is used by NICEBT and make sure */
/*     the page is completely blank */

    for (i__ = 1; i__ <= 520; ++i__) {
	s_copy(page + ((i__1 = i__ - 1) < 520 && 0 <= i__1 ? i__1 : s_rnge(
		"page", i__1, "tabrpt_", (ftnlen)294)) * 132, " ", (ftnlen)
		132, (ftnlen)1);
    }

/*     Initialize the local page and set the column parameters. */

    i__1 = *nitems;
    for (i__ = 1; i__ <= i__1; ++i__) {
	done[(i__2 = i__ - 1) < 60 && 0 <= i__2 ? i__2 : s_rnge("done", i__2, 
		"tabrpt_", (ftnlen)301)] = FALSE_;
	count[(i__2 = i__ - 1) < 60 && 0 <= i__2 ? i__2 : s_rnge("count", 
		i__2, "tabrpt_", (ftnlen)302)] = 0;
	row[(i__2 = i__ - 1) < 60 && 0 <= i__2 ? i__2 : s_rnge("row", i__2, 
		"tabrpt_", (ftnlen)303)] = 0;
	repmi_("LEFT 1 RIGHT #", "#", &width[i__ - 1], style + ((i__2 = i__ - 
		1) < 60 && 0 <= i__2 ? i__2 : s_rnge("style", i__2, "tabrpt_",
		 (ftnlen)304)) * 80, (ftnlen)14, (ftnlen)1, (ftnlen)80);
	suffix_(spcial + (i__ - 1) * spcial_len, &c__1, style + ((i__2 = i__ 
		- 1) < 60 && 0 <= i__2 ? i__2 : s_rnge("style", i__2, "tabrp"
		"t_", (ftnlen)305)) * 80, spcial_len, (ftnlen)80);
    }

/*     The logical FINISH is used to keep track of whether or not */
/*     we have finished processing all items.  Certainly we haven't */
/*     done so yet.  It will be the value of the expression given */
/*     by DONE(1) .AND. DONE(2) .AND. ... .AND. DONE(NITEMS) */

    finish = FALSE_;
    while(! finish) {

/*        We need to reset the left margin of the page. */

	left = *lmarge;
	i__1 = *nitems;
	for (id = 1; id <= i__1; ++id) {

/*           We are going to format items for output one at a time. */
/*           We will either fetch all of the components, or we */
/*           will fill up the room allotted for this item in the */
/*           buffer that will hold the data. */

/*           Thus at the end of this loop, we will have filled */
/*           up as much room as there is for this part of the */
/*           report and be ready to send that stuff to the */
/*           printer. */

/*           Set the right margin and determine whether or not */
/*           the  COLUMN that holds the text to be formatted is */
/*           already filled up. */

	    filled = row[(i__2 = id - 1) < 60 && 0 <= i__2 ? i__2 : s_rnge(
		    "row", i__2, "tabrpt_", (ftnlen)337)] >= 260 || done[(
		    i__3 = id - 1) < 60 && 0 <= i__3 ? i__3 : s_rnge("done", 
		    i__3, "tabrpt_", (ftnlen)337)];
	    right = left + width[id - 1] - 1;
	    while(! filled) {

/*              Put data into the long string for output until */
/*              it becomes full or it is appropriate to stop doing */
/*              so (there's no more data, or the PRESRV flag tells */
/*              us to stop). */
		putat = 1;
		full = FALSE_;
/* Computing MIN */
		i__2 = 1024, i__3 = width[id - 1] * 130;
		room = min(i__2,i__3);
		s_copy(long__, " ", (ftnlen)1024, (ftnlen)1);
		while(! done[(i__2 = id - 1) < 60 && 0 <= i__2 ? i__2 : 
			s_rnge("done", i__2, "tabrpt_", (ftnlen)353)] && ! 
			full) {

/*                 Increment COUNT so that we can fetch the next */
/*                 component of this item. */

		    count[(i__2 = id - 1) < 60 && 0 <= i__2 ? i__2 : s_rnge(
			    "count", i__2, "tabrpt_", (ftnlen)359)] = count[(
			    i__3 = id - 1) < 60 && 0 <= i__3 ? i__3 : s_rnge(
			    "count", i__3, "tabrpt_", (ftnlen)359)] + 1;
		    (*fetch)(&item[id - 1], &count[(i__2 = id - 1) < 60 && 0 
			    <= i__2 ? i__2 : s_rnge("count", i__2, "tabrpt_", 
			    (ftnlen)361)], getstr, &wdth, (ftnlen)1024);
		    if (failed_()) {
			chkout_("TABRPT", (ftnlen)6);
			return 0;
		    }

/*                 Determine the next place to add on to this string */
/*                 and see if adding on at that point would fill up */
/*                 the available space in our string. */

		    l = qlstnb_(getstr, (ftnlen)1024);
		    last = max(l,1);
		    if (putat + l < room) {
			s_copy(long__ + (putat - 1), getstr, 1024 - (putat - 
				1), last);
/* Computing MIN */
			i__2 = putat + l + 2;
			putat = min(i__2,1024);

/*                    If the input was a blank, we step back to */
/*                    the beginning of the string. */

			if (putat == 2) {
			    putat = 1;
			}
			noroom = putat + width[id - 1] >= room;
		    } else if (putat == 1) {

/*                    This case is very funky.  We are at the very */
/*                    beginning of the output buffer, but there still */
/*                    isn't room.  This means the user requested */
/*                    a width such that HLFHLD * WIDTH(ID)  is smaller */
/*                    than the size of the data in the column. */
/*                    In other words, the width must be less than */
/*                    the value DATA_LENGTH/HLFHLD.  Since the */
/*                    maximum data length is 1024 and HLFHLD is */
/*                    at last look 130, this means they have asked */
/*                    to fit data that is very long into a very */
/*                    column that is less than 8 characters wide. */
/*                    Sorry but there doesn't seem to be a morally */
/*                    compelling reason to handle this case */
/*                    robustly.  We just put some dots at the end */
/*                    of the output to indicate there's more stuff */
/*                    that can't be printed. */

			s_copy(long__, getstr, (ftnlen)1024, (ftnlen)1024);
			noroom = TRUE_;
			i__2 = room - 8;
			s_copy(long__ + i__2, "........", room - i__2, (
				ftnlen)8);
			putat = room;
		    } else {

/*                    There isn't room to append GETSTR to the end */
/*                    of LONG.  Adjust the counter back by 1 and */
/*                    set NOROOM to .TRUE. */

			count[(i__2 = id - 1) < 60 && 0 <= i__2 ? i__2 : 
				s_rnge("count", i__2, "tabrpt_", (ftnlen)421)]
				 = count[(i__3 = id - 1) < 60 && 0 <= i__3 ? 
				i__3 : s_rnge("count", i__3, "tabrpt_", (
				ftnlen)421)] - 1;
			noroom = TRUE_;
		    }
		    done[(i__2 = id - 1) < 60 && 0 <= i__2 ? i__2 : s_rnge(
			    "done", i__2, "tabrpt_", (ftnlen)426)] = count[(
			    i__3 = id - 1) < 60 && 0 <= i__3 ? i__3 : s_rnge(
			    "count", i__3, "tabrpt_", (ftnlen)426)] >= size[
			    id - 1];
		    full = presrv[id - 1] || noroom;
		}

/*              Format the string into the holding buffer. */

		ssizec_(&c__260, buffer, (ftnlen)132);
		nicebt_1__(long__, style + ((i__2 = id - 1) < 60 && 0 <= i__2 
			? i__2 : s_rnge("style", i__2, "tabrpt_", (ftnlen)435)
			) * 80, buffer, putat, (ftnlen)80, (ftnlen)132);
		if (failed_()) {
		    chkout_("TABRPT", (ftnlen)6);
		    return 0;
		}
		nrows = cardc_(buffer, (ftnlen)132);

/*              Transfer the data from the holding buffer */
/*              to the page layout buffer. */

		i__2 = nrows;
		for (j = 1; j <= i__2; ++j) {
		    row[(i__3 = id - 1) < 60 && 0 <= i__3 ? i__3 : s_rnge(
			    "row", i__3, "tabrpt_", (ftnlen)448)] = row[(i__4 
			    = id - 1) < 60 && 0 <= i__4 ? i__4 : s_rnge("row",
			     i__4, "tabrpt_", (ftnlen)448)] + 1;
		    r__ = row[(i__3 = id - 1) < 60 && 0 <= i__3 ? i__3 : 
			    s_rnge("row", i__3, "tabrpt_", (ftnlen)449)];
		    s_copy(page + (((i__3 = r__ - 1) < 520 && 0 <= i__3 ? 
			    i__3 : s_rnge("page", i__3, "tabrpt_", (ftnlen)
			    451)) * 132 + (left - 1)), buffer + ((i__4 = j + 
			    5) < 266 && 0 <= i__4 ? i__4 : s_rnge("buffer", 
			    i__4, "tabrpt_", (ftnlen)451)) * 132, right - (
			    left - 1), (ftnlen)132);
		    if (justr[id - 1]) {
			rjust_(page + (((i__3 = r__ - 1) < 520 && 0 <= i__3 ? 
				i__3 : s_rnge("page", i__3, "tabrpt_", (
				ftnlen)454)) * 132 + (left - 1)), page + (((
				i__4 = r__ - 1) < 520 && 0 <= i__4 ? i__4 : 
				s_rnge("page", i__4, "tabrpt_", (ftnlen)454)) 
				* 132 + (left - 1)), right - (left - 1), 
				right - (left - 1));
		    }

/*                 Replace any "hardspaces" by blanks. */

		    if (dohrd) {
			if (*(unsigned char *)&hrd[id - 1] != ' ') {
			    replch_(page + (((i__3 = r__ - 1) < 520 && 0 <= 
				    i__3 ? i__3 : s_rnge("page", i__3, "tabr"
				    "pt_", (ftnlen)462)) * 132 + (left - 1)), 
				    hrd + (id - 1), " ", page + (((i__4 = r__ 
				    - 1) < 520 && 0 <= i__4 ? i__4 : s_rnge(
				    "page", i__4, "tabrpt_", (ftnlen)462)) * 
				    132 + (left - 1)), right - (left - 1), (
				    ftnlen)1, (ftnlen)1, right - (left - 1));
			}
		    }
		}

/*              Determine whether this column has been sufficiently */
/*              filled up. */

		done[(i__2 = id - 1) < 60 && 0 <= i__2 ? i__2 : s_rnge("done",
			 i__2, "tabrpt_", (ftnlen)474)] = count[(i__3 = id - 
			1) < 60 && 0 <= i__3 ? i__3 : s_rnge("count", i__3, 
			"tabrpt_", (ftnlen)474)] >= size[id - 1];
		filled = done[(i__2 = id - 1) < 60 && 0 <= i__2 ? i__2 : 
			s_rnge("done", i__2, "tabrpt_", (ftnlen)475)] || row[(
			i__3 = id - 1) < 60 && 0 <= i__3 ? i__3 : s_rnge(
			"row", i__3, "tabrpt_", (ftnlen)475)] >= 260;
	    }

/*           Once you get to this point, the current column has */
/*           been filled as much as is possible.   We need to */
/*           Set the left margin for the next item to process */

	    left = right + *space + 1;
	}

/*        By the time you get to this point, every column has either */
/*        filled up or there's nothing left to print. */

/*        In either case we need to ship out the rows from */
/*        1 to MIN ( MAX{ROW(1) ... ROW(NITEMS)}, THRSHOLD ) */
/*        and shift the rest of the stuff up in the buffer. */

	maxrow = 0;
	i__1 = *nitems;
	for (i__ = 1; i__ <= i__1; ++i__) {
/* Computing MAX */
	    i__3 = maxrow, i__4 = row[(i__2 = i__ - 1) < 60 && 0 <= i__2 ? 
		    i__2 : s_rnge("row", i__2, "tabrpt_", (ftnlen)501)];
	    maxrow = max(i__3,i__4);
	}
	toship = min(maxrow,260);

/*        Ship out the rows that are ready to go. */

	i__1 = toship;
	for (r__ = 1; r__ <= i__1; ++r__) {
	    pagput_(page + ((i__2 = r__ - 1) < 520 && 0 <= i__2 ? i__2 : 
		    s_rnge("page", i__2, "tabrpt_", (ftnlen)509)) * 132, (
		    ftnlen)132);
	    pagpmt_(&did, value, (ftnlen)32);
	    if (did != 0) {
		if (eqstr_(value, key, (ftnlen)32, (ftnlen)32)) {
		    chkout_("TABRPT", (ftnlen)6);
		    return 0;
		}
	    }
	}

/*        Shift the remaining rows up to the top of the page */

	for (r__ = toship + 1; r__ <= 520; ++r__) {
	    s_copy(page + ((i__1 = r__ - toship - 1) < 520 && 0 <= i__1 ? 
		    i__1 : s_rnge("page", i__1, "tabrpt_", (ftnlen)526)) * 
		    132, page + ((i__2 = r__ - 1) < 520 && 0 <= i__2 ? i__2 : 
		    s_rnge("page", i__2, "tabrpt_", (ftnlen)526)) * 132, (
		    ftnlen)132, (ftnlen)132);
	}

/*        Blank out the last TOSHIP rows. */

	for (r__ = 520 - toship + 1; r__ <= 520; ++r__) {
	    s_copy(page + ((i__1 = r__ - 1) < 520 && 0 <= i__1 ? i__1 : 
		    s_rnge("page", i__1, "tabrpt_", (ftnlen)533)) * 132, 
		    " ", (ftnlen)132, (ftnlen)1);
	}

/*        Finally adjust the positions where each column should begin */
/*        filling in more data. */

	i__1 = *nitems;
	for (j = 1; j <= i__1; ++j) {
/* Computing MAX */
	    i__4 = row[(i__3 = j - 1) < 60 && 0 <= i__3 ? i__3 : s_rnge("row",
		     i__3, "tabrpt_", (ftnlen)540)] - toship;
	    row[(i__2 = j - 1) < 60 && 0 <= i__2 ? i__2 : s_rnge("row", i__2, 
		    "tabrpt_", (ftnlen)540)] = max(i__4,0);
	}

/*        Now examine each of the ID's to see if we are done */
/*        processing all items. */

	finish = TRUE_;
	i__1 = *nitems;
	for (id = 1; id <= i__1; ++id) {
	    finish = finish && done[(i__2 = id - 1) < 60 && 0 <= i__2 ? i__2 :
		     s_rnge("done", i__2, "tabrpt_", (ftnlen)550)];
	}
    }

/*     Send any remaining rows out to the page manager. */

    maxrow = 0;
    i__1 = *nitems;
    for (i__ = 1; i__ <= i__1; ++i__) {
/* Computing MAX */
	i__3 = maxrow, i__4 = row[(i__2 = i__ - 1) < 60 && 0 <= i__2 ? i__2 : 
		s_rnge("row", i__2, "tabrpt_", (ftnlen)561)];
	maxrow = max(i__3,i__4);
    }
    i__1 = maxrow;
    for (r__ = 1; r__ <= i__1; ++r__) {
	pagput_(page + ((i__2 = r__ - 1) < 520 && 0 <= i__2 ? i__2 : s_rnge(
		"page", i__2, "tabrpt_", (ftnlen)565)) * 132, (ftnlen)132);
	s_copy(page + ((i__2 = r__ - 1) < 520 && 0 <= i__2 ? i__2 : s_rnge(
		"page", i__2, "tabrpt_", (ftnlen)566)) * 132, " ", (ftnlen)
		132, (ftnlen)1);
	pagpmt_(&did, value, (ftnlen)32);
	if (did != 0) {
	    if (eqstr_(value, key, (ftnlen)32, (ftnlen)32)) {
		chkout_("TABRPT", (ftnlen)6);
		return 0;
	    }
	}
    }
    chkout_("TABRPT", (ftnlen)6);
    return 0;
/* $Procedure      TABABT ( Tabular Report Abort Key ) */

L_tababt:
/* $ Abstract */

/*     Set the abort string to use if the page manager prompt has */
/*     been set. */

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

/*      None. */

/* $ Keywords */

/*      REPORTS */

/* $ Declarations */

/*     IMPLICIT NONE */

/*     CHARACTER*(*)         SPCIAL */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     SPCIAL     I   String used to indicate report should be aborted. */

/* $ Detailed_Input */

/*     SPCIAL     is an array of strings.  Only the first entry is used. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Particulars */

/*     This entry point is used to set the KEY that is used to */
/*     determine whether or not a report should be aborted */

/* $ Examples */

/*     Suppose that you plan to ask the user whether or not */
/*     a report should be continued. And that the user should */
/*     type 'N' if the report should not be continued. */

/*     CALL TABABT ( 'N' ) */

/*     DO WHILE ( MOREDATA ) */

/*        CALL TABRPT ( .... ) */

/*        CALL PAGPMT ( DIDPMT, RESPNS ) */
/*        IF ( DIDPMT .EQ. 1 ) THEN */
/*           QUIT = EQSTR( RESPNS, 'N' ) */
/*        END IF */

/*        IF ( .NOT. QUIT ) THEN */

/*           see if there is more data */

/*        ELSE */

/*           MOREDATA = .FALSE. */

/*        END IF */

/*     END DO */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 10-SEP-1998 (WLT) */


/* -& */
    s_copy(key, spcial, (ftnlen)32, spcial_len);
    return 0;
/* $Procedure      TABHRD ( Tabular Report Hard Space ) */

L_tabhrd:
/* $ Abstract */

/*    Set the hard space to be used in reports. */

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

/*     REPORTS */

/* $ Declarations */

/*     IMPLICIT NONE */
/*     INTEGER               NITEMS */
/*     CHARACTER*(*)         SPCIAL ( * ) */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     NITEMS     I   Number of items to check in a report. */
/*     SPCIAL     I   SPCIAL(I)(1:1) contains that hardspace character */

/*     The function returns */

/* $ Detailed_Input */

/*     NITEMS      Number of items to appear in a report. */

/*     SPCIAL      The string SPCIAL(I) contains the character that */
/*                 should be filtered from the Ith entry and converted */
/*                 to a space after all justifications and formatting */
/*                 have been performed. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Particulars */

/*     This entry point allows you to specify some character that */
/*     should be converted to a blank character after all column */
/*     settings and justifications have been performed. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 23-SEP-1998 (WLT) */


/* -& */
/* $ Index_Entries */

/*     set a hard space character */

/* -& */
    s_copy(hrd, " ", (ftnlen)60, (ftnlen)1);
    dohrd = FALSE_;
    i__1 = *nitems;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s_copy(hrd + (i__ - 1), spcial + (i__ - 1) * spcial_len, (ftnlen)1, 
		spcial_len);
	dohrd = dohrd || *(unsigned char *)&hrd[i__ - 1] != ' ';
    }
    return 0;
} /* tabrpt_ */

/* Subroutine */ int tabrpt_(integer *nitems, integer *item, integer *size, 
	integer *width, logical *justr, logical *presrv, char *spcial, 
	integer *lmarge, integer *space, S_fp fetch, ftnlen spcial_len)
{
    return tabrpt_0_(0, nitems, item, size, width, justr, presrv, spcial, 
	    lmarge, space, fetch, spcial_len);
    }

/* Subroutine */ int tababt_(char *spcial, ftnlen spcial_len)
{
    return tabrpt_0_(1, (integer *)0, (integer *)0, (integer *)0, (integer *)
	    0, (logical *)0, (logical *)0, spcial, (integer *)0, (integer *)0,
	     (S_fp)0, spcial_len);
    }

/* Subroutine */ int tabhrd_(integer *nitems, char *spcial, ftnlen spcial_len)
{
    return tabrpt_0_(2, nitems, (integer *)0, (integer *)0, (integer *)0, (
	    logical *)0, (logical *)0, spcial, (integer *)0, (integer *)0, (
	    S_fp)0, spcial_len);
    }

