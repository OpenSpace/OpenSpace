/* nspsho.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__9 = 9;
static integer c__1 = 1;
static integer c__2 = 2;
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

/* Subroutine */ int nspsho_(char *commnd, char *error, ftnlen commnd_len, 
	ftnlen error_len)
{
    /* Initialized data */

    static logical first = TRUE_;
    static char synval[100*15] = "                                          "
	    "                                                          " "   "
	    "                                                                "
	    "                                 " "                            "
	    "                                                                "
	    "        " "                                                     "
	    "                                               " "              "
	    "                                                                "
	    "                      " "                                       "
	    "                                                             " 
	    "FORMAT[format]                                                 "
	    "                                     " "PAGE[page]              "
	    "                                                                "
	    "            " "KERNELS[kernels]                                 "
	    "                                                   " "COLUMN[col"
	    "umn] #word[colnam]                                              "
	    "                          " "INDEXES[indexed]                   "
	    "                                                                 "
	     "SUMMARY[summary]                                              "
	    "                                      " "ENVIRONMENT[env]       "
	    "                                                                "
	    "             " "COMMENTS[comment] #word[file]                   "
	    "                                                    " "COMMENTS["
	    "comment]                                                        "
	    "                           ";
    static char freq[64*3] = "No page                                       "
	    "                  " "First page only                            "
	    "                     " "Every page                              "
	    "                        ";
    static char offon[3*3] = "OFF" "ASK" "ON ";

    /* System generated locals */
    address a__1[2], a__2[3];
    integer i__1, i__2, i__3[2], i__4[3], i__5;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);
    integer i_indx(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static char name__[64];
    extern logical have_(char *, ftnlen);
    static char dfmt[80], mark[64], ifmt[80], tfmt[80], word[64], rest[80], 
	    type__[64], just[64];
    static integer i__;
    extern /* Subroutine */ int clgac_(integer *, char *, char *, ftnlen, 
	    ftnlen);
    static integer k, n;
    extern /* Subroutine */ int clgai_(integer *, char *, integer *, integer *
	    , ftnlen);
    static integer p;
    static char table[64], alias[80];
    extern /* Subroutine */ int clnid_(integer *, integer *, logical *), 
	    chkin_(char *, ftnlen), ucase_(char *, char *, ftnlen, ftnlen);
    static char names[64*500];
    extern /* Subroutine */ int repmc_(char *, char *, char *, char *, ftnlen,
	     ftnlen, ftnlen, ftnlen);
    static integer useid;
    static logical found;
    static integer limit;
    extern /* Subroutine */ int clnum_(integer *);
    extern integer rtrim_(char *, ftnlen);
    static integer r1, r2;
    extern /* Subroutine */ int m2chck_(char *, char *, integer *, char *, 
	    char *, ftnlen, ftnlen, ftnlen, ftnlen), cln2id_(char *, integer *
	    , logical *, ftnlen), m2getc_(char *, char *, logical *, char *, 
	    ftnlen, ftnlen, ftnlen), m2ints_(integer *, char *, integer *, 
	    char *, ftnlen, ftnlen);
    static integer id;
    static char bs[1];
    extern logical m2xist_(char *, ftnlen);
    static char dq[64*2];
    static logical header;
    static char lr[64], filnam[80];
    static integer pagewd, pageht;
    static char colnam[64];
    static integer hdfreq;
    static char indexd[64], pagetl[80];
    extern /* Subroutine */ int replch_(char *, char *, char *, char *, 
	    ftnlen, ftnlen, ftnlen, ftnlen), getedt_(char *, ftnlen), gtecho_(
	    char *, ftnlen), shellc_(integer *, char *, ftnlen);
    static integer cwidth;
    extern /* Subroutine */ int nspekc_(char *, ftnlen);
    static char litnam[64];
    static integer numfnd;
    static char format[80];
    static integer adjust, tlfreq;
    extern /* Subroutine */ int prefix_(char *, integer *, char *, ftnlen, 
	    ftnlen);
    static char values[80*500];
    static integer nitems;
    extern /* Subroutine */ int chkout_(char *, ftnlen), nsppfl_(char *, char 
	    *, ftnlen, ftnlen);
    static char cmpnts[64];
    extern /* Subroutine */ int flgrpt_(integer *, char *, char *, S_fp, 
	    ftnlen, ftnlen), nextwd_(char *, char *, char *, ftnlen, ftnlen, 
	    ftnlen), repmot_(char *, char *, integer *, char *, char *, 
	    ftnlen, ftnlen, ftnlen, ftnlen), nspgst_(char *, logical *, 
	    ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int nspeks_(void), nspwln_(char *, ftnlen);
    static char synkey[64*15];
    static logical status[3];
    extern /* Subroutine */ int intstr_(integer *, char *, ftnlen), suffix_(
	    char *, integer *, char *, ftnlen, ftnlen), namxpn_(char *, char *
	    , char *, ftnlen, ftnlen, ftnlen);
    static integer synptr[15];
    extern /* Subroutine */ int bbfndc_1__(char *, integer *, ftnlen), 
	    bbgetc_1__(char *, char *, integer *, char *, ftnlen, ftnlen, 
	    ftnlen), bbgeti_1__(char *, char *, integer *, integer *, ftnlen, 
	    ftnlen);


/*     Nov 21, 1995 */

/*        Removed the show symbol capability since it is now supported */
/*        by the generic command loop capability. */

/*     Nov 2, 1995 */

/*        Added the ability to use templates instead of full */
/*        names when requesting information about a column. */

/*     August 14, 1995 */

/*     Increased the declared length of syntax templates from 64 to 100 */

/*     March 2, 1995. */

/*     Corrected error message generated by SHOW COLUMN x  when */
/*     x is not the name of a known column. */


/*     SPICELIB functions */


/*     Meta/2 Functions */


/*     Interface to the SPICELIB error handling. */


/*     Functions/routines that are passed on to someplace else */


/*     Meta/2 syntax definition variables. */


/*     Local Variables */


/*     Save everything. */


/*     Standard Spicelib error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("NSPSHO", (ftnlen)6);

/*     On the first pass establish the syntax that this routine */
/*     is responsible for recognizing. */

    if (first) {
	first = FALSE_;
	*(unsigned char *)bs = '@';
	for (i__ = 1; i__ <= 9; ++i__) {
	    replch_(synval + ((i__1 = i__ + 5) < 15 && 0 <= i__1 ? i__1 : 
		    s_rnge("synval", i__1, "nspsho_", (ftnlen)218)) * 100, 
		    "#", bs, synval + ((i__2 = i__ + 5) < 15 && 0 <= i__2 ? 
		    i__2 : s_rnge("synval", i__2, "nspsho_", (ftnlen)218)) * 
		    100, (ftnlen)100, (ftnlen)1, (ftnlen)1, (ftnlen)100);
	}
	m2ints_(&c__9, synkey, synptr, synval, (ftnlen)64, (ftnlen)100);
    }

/*     See if this command matches a known syntax.  If it doesn't */
/*     there is no point in hanging around. */

    m2chck_(commnd, synkey, synptr, synval, error, commnd_len, (ftnlen)64, (
	    ftnlen)100, error_len);
    if (have_(error, error_len)) {
	prefix_("NSPSHO:", &c__1, error + error_len, (ftnlen)7, error_len);
	chkout_("NSPSHO", (ftnlen)6);
	return 0;
    }

/*     Determine what the user wants to see. */

    if (m2xist_("format", (ftnlen)6)) {
	s_copy(format, " ", (ftnlen)80, (ftnlen)1);
	s_copy(mark, "(not applicable)", (ftnlen)64, (ftnlen)16);
	bbgetc_1__("COPY", "FORMAT", &n, format, (ftnlen)4, (ftnlen)6, (
		ftnlen)80);
	bbgetc_1__("COPY", "TIMEFMT", &n, tfmt, (ftnlen)4, (ftnlen)7, (ftnlen)
		80);
	bbgetc_1__("COPY", "INTFMT", &n, ifmt, (ftnlen)4, (ftnlen)6, (ftnlen)
		80);
	bbgetc_1__("COPY", "DPFMT", &n, dfmt, (ftnlen)4, (ftnlen)5, (ftnlen)
		80);
	bbgeti_1__("COPY", "REPORTLIMIT", &n, &limit, (ftnlen)4, (ftnlen)11);
	bbgeti_1__("COPY", "AUTOADJUST", &n, &adjust, (ftnlen)4, (ftnlen)10);
	nextwd_(format, word, rest, (ftnlen)80, (ftnlen)64, (ftnlen)80);
	if (s_cmp(word, "MARKED", (ftnlen)64, (ftnlen)6) == 0) {

/*           Put the mark in MARK starting at the second character so */
/*           that we preserve the quote that's already in the first */
/*           character. */

	    s_copy(mark, "'", (ftnlen)64, (ftnlen)1);
	    bbgetc_1__("COPY", "FMTMARK", &n, mark + 1, (ftnlen)4, (ftnlen)7, 
		    (ftnlen)63);
	    s_copy(mark + 2, "'", (ftnlen)62, (ftnlen)1);
	}
	s_copy(names, " ", (ftnlen)64, (ftnlen)1);
	s_copy(names + 64, "Report Format", (ftnlen)64, (ftnlen)13);
	s_copy(names + 128, "Report Mark", (ftnlen)64, (ftnlen)11);
	s_copy(names + 192, "Default Time     Format", (ftnlen)64, (ftnlen)23)
		;
	s_copy(names + 256, "Default Integer  Format", (ftnlen)64, (ftnlen)23)
		;
	s_copy(names + 320, "Default Floating Format", (ftnlen)64, (ftnlen)23)
		;
	s_copy(names + 384, "Deluge Warning ", (ftnlen)64, (ftnlen)15);
	s_copy(names + 448, "Auto Adjust ", (ftnlen)64, (ftnlen)12);
	s_copy(names + 512, " ", (ftnlen)64, (ftnlen)1);
	s_copy(values, " ", (ftnlen)80, (ftnlen)1);
	s_copy(values + 80, format, (ftnlen)80, (ftnlen)80);
	s_copy(values + 160, mark, (ftnlen)80, (ftnlen)64);
	s_copy(values + 240, tfmt, (ftnlen)80, (ftnlen)80);
	s_copy(values + 320, ifmt, (ftnlen)80, (ftnlen)80);
	s_copy(values + 400, dfmt, (ftnlen)80, (ftnlen)80);
	intstr_(&limit, values + 480, (ftnlen)80);
/* Writing concatenation */
	i__3[0] = 3, a__1[0] = offon + ((i__1 = adjust) < 3 && 0 <= i__1 ? 
		i__1 : s_rnge("offon", i__1, "nspsho_", (ftnlen)286)) * 3;
	i__3[1] = 34, a__1[1] = " (applies only to tabular formats)";
	s_cat(values + 560, a__1, i__3, &c__2, (ftnlen)80);
	s_copy(values + 640, " ", (ftnlen)80, (ftnlen)1);
	nitems = 9;
	if (s_cmp(format, "DELIMITED", (ftnlen)80, (ftnlen)9) == 0 || s_cmp(
		format, "DELIMITED PRESERVED", (ftnlen)80, (ftnlen)19) == 0) {
	    bbgetc_1__("COPY", "DELIMITER", &n, dq, (ftnlen)4, (ftnlen)9, (
		    ftnlen)64);
	    bbgetc_1__("COPY", "QUOTE", &n, dq + 64, (ftnlen)4, (ftnlen)5, (
		    ftnlen)64);
	    r1 = rtrim_(dq, (ftnlen)64);
	    r2 = rtrim_(dq + 64, (ftnlen)64);
	    s_copy(names + 128, "Delimiter / Quote ", (ftnlen)64, (ftnlen)18);
/* Writing concatenation */
	    i__4[0] = r1, a__2[0] = dq;
	    i__4[1] = 3, a__2[1] = " / ";
	    i__4[2] = r2, a__2[2] = dq + 64;
	    s_cat(values + 160, a__2, i__4, &c__3, (ftnlen)80);
	}
    } else if (m2xist_("env", (ftnlen)3)) {
	s_copy(names, "Editor", (ftnlen)64, (ftnlen)6);
	s_copy(names + 64, "Echoing Commands", (ftnlen)64, (ftnlen)16);
	s_copy(names + 128, "Screen Output File", (ftnlen)64, (ftnlen)18);
	getedt_(values, (ftnlen)80);
	gtecho_(values + 80, (ftnlen)80);
	nspgst_("SAVE", status, (ftnlen)4);
	if (status[0] && status[1] && ! status[2]) {
	    nsppfl_("SAVE", values + 160, (ftnlen)4, (ftnlen)80);
	} else {
	    s_copy(values + 160, "No Current Screen Save File", (ftnlen)80, (
		    ftnlen)27);
	}
	s_copy(names + 192, "Help Waiting", (ftnlen)64, (ftnlen)12);
	bbgeti_1__("COPY", "HELPPROMPT", &n, &k, (ftnlen)4, (ftnlen)10);
	if (k == 0) {
	    s_copy(values + 240, "DISABLED", (ftnlen)80, (ftnlen)8);
	} else {
	    s_copy(values + 240, "ENABLED", (ftnlen)80, (ftnlen)7);
	}
	s_copy(names + 256, "Leapseconds Kernel", (ftnlen)64, (ftnlen)18);
	bbfndc_1__("LEAPSECONDS", &n, (ftnlen)11);
	if (n == 0) {
	    s_copy(values + 320, "<None Loaded>", (ftnlen)80, (ftnlen)13);
	} else {
	    bbgetc_1__("COPY", "LEAPSECONDS", &n, values + 320, (ftnlen)4, (
		    ftnlen)11, (ftnlen)80);
	}
	s_copy(names + 320, "Spacecraft Clock Kernel", (ftnlen)64, (ftnlen)23)
		;
	bbfndc_1__("SCLK", &n, (ftnlen)4);
	if (n == 0) {
	    s_copy(values + 400, "<None Loaded>", (ftnlen)80, (ftnlen)13);
	    nitems = 6;
	} else {
	    bbgetc_1__("COPY", "SCLK", &n, values + 400, (ftnlen)4, (ftnlen)4,
		     (ftnlen)80);
	    i__1 = n;
	    for (i__ = 2; i__ <= i__1; ++i__) {
		s_copy(names + (((i__2 = i__ + 4) < 500 && 0 <= i__2 ? i__2 : 
			s_rnge("names", i__2, "nspsho_", (ftnlen)351)) << 6), 
			" ", (ftnlen)64, (ftnlen)1);
	    }
	    nitems = n + 5;
	    if (n > 1) {
		s_copy(names + 320, "Spacecraft Clock Kernels", (ftnlen)64, (
			ftnlen)24);
	    }
	}
	nspwln_(" ", (ftnlen)1);
	nspwln_("Current Inspekt Environment", (ftnlen)27);
	nspwln_(" ", (ftnlen)1);
	flgrpt_(&nitems, names, values, (S_fp)nspwln_, (ftnlen)64, (ftnlen)80)
		;
	nspwln_(" ", (ftnlen)1);
	chkout_("NSPSHO", (ftnlen)6);
	return 0;
    } else if (m2xist_("page", (ftnlen)4)) {
	s_copy(pagetl, " ", (ftnlen)80, (ftnlen)1);
	bbgeti_1__("COPY", "PAGEHEIGHT", &n, &pageht, (ftnlen)4, (ftnlen)10);
	bbgeti_1__("COPY", "PAGEWIDTH", &n, &pagewd, (ftnlen)4, (ftnlen)9);
	bbgeti_1__("COPY", "TITLEFREQUENCY", &n, &tlfreq, (ftnlen)4, (ftnlen)
		14);
	bbgeti_1__("COPY", "HEADERFREQUENCY", &n, &hdfreq, (ftnlen)4, (ftnlen)
		15);
	bbgetc_1__("COPY", "TITLEJUSTIFICATION", &n, just, (ftnlen)4, (ftnlen)
		18, (ftnlen)64);
	bbgetc_1__("COPY", "PAGETITLE", &n, pagetl, (ftnlen)4, (ftnlen)9, (
		ftnlen)80);
	bbgetc_1__("COPY", "FORMAT", &n, format, (ftnlen)4, (ftnlen)6, (
		ftnlen)80);
	header = i_indx(format, "TABULAR", (ftnlen)80, (ftnlen)7) > 0;
	s_copy(names, " ", (ftnlen)64, (ftnlen)1);
	s_copy(values, " ", (ftnlen)80, (ftnlen)1);
	s_copy(names + 64, "Page height (rows)", (ftnlen)64, (ftnlen)18);
	s_copy(names + 128, "Page width  (columns)", (ftnlen)64, (ftnlen)21);
	s_copy(names + 192, " ", (ftnlen)64, (ftnlen)1);
	intstr_(&pageht, values + 80, (ftnlen)80);
	intstr_(&pagewd, values + 160, (ftnlen)80);
	s_copy(values + 240, " ", (ftnlen)80, (ftnlen)1);
	nitems = 4;

/*        If a title has not been supplied for reports, we don't bother */
/*        saying anything about whether or not it will be centered and */
/*        how often it will be displayed. */

	if (s_cmp(pagetl, " ", (ftnlen)80, (ftnlen)1) == 0) {
	    s_copy(names + 256, " ", (ftnlen)64, (ftnlen)1);
	    s_copy(values + 320, "(No title has been supplied)", (ftnlen)80, (
		    ftnlen)28);
	    s_copy(names + 320, " ", (ftnlen)64, (ftnlen)1);
	    s_copy(values + 400, " ", (ftnlen)80, (ftnlen)1);
	    nitems = 6;
	} else {
	    s_copy(names + 256, "Page Title", (ftnlen)64, (ftnlen)10);
	    s_copy(values + 320, pagetl, (ftnlen)80, (ftnlen)80);
	    s_copy(names + 320, "Title Justification", (ftnlen)64, (ftnlen)19)
		    ;
	    s_copy(values + 400, just, (ftnlen)80, (ftnlen)64);
	    s_copy(names + 384, "Title Appears on", (ftnlen)64, (ftnlen)16);
	    if (tlfreq < 2) {
		s_copy(values + 480, freq + (((i__1 = tlfreq + 1) < 3 && 0 <= 
			i__1 ? i__1 : s_rnge("freq", i__1, "nspsho_", (ftnlen)
			422)) << 6), (ftnlen)80, (ftnlen)64);
	    } else {
		s_copy(values + 480, "First page and every # page thereafter."
			, (ftnlen)80, (ftnlen)39);
		repmot_(values + 480, "#", &tlfreq, "L", values + 480, (
			ftnlen)80, (ftnlen)1, (ftnlen)1, (ftnlen)80);
	    }
	    s_copy(names + 448, " ", (ftnlen)64, (ftnlen)1);
	    s_copy(values + 560, " ", (ftnlen)80, (ftnlen)1);
	    nitems = 8;
	}

/*        If a header applies to the current report format, then */
/*        we might as well state how often it shall be printed. */

	if (header) {
	    ++nitems;
	    s_copy(names + (((i__1 = nitems - 1) < 500 && 0 <= i__1 ? i__1 : 
		    s_rnge("names", i__1, "nspsho_", (ftnlen)448)) << 6), 
		    "Header Appears on", (ftnlen)64, (ftnlen)17);
	    if (hdfreq < 2) {
		s_copy(values + ((i__1 = nitems - 1) < 500 && 0 <= i__1 ? 
			i__1 : s_rnge("values", i__1, "nspsho_", (ftnlen)452))
			 * 80, freq + (((i__2 = hdfreq + 1) < 3 && 0 <= i__2 ?
			 i__2 : s_rnge("freq", i__2, "nspsho_", (ftnlen)452)) 
			<< 6), (ftnlen)80, (ftnlen)64);
	    } else {
		s_copy(values + ((i__1 = nitems - 1) < 500 && 0 <= i__1 ? 
			i__1 : s_rnge("values", i__1, "nspsho_", (ftnlen)456))
			 * 80, "First page and every # page thereafter.", (
			ftnlen)80, (ftnlen)39);
		repmot_(values + ((i__1 = nitems - 1) < 500 && 0 <= i__1 ? 
			i__1 : s_rnge("values", i__1, "nspsho_", (ftnlen)459))
			 * 80, "#", &hdfreq, "L", values + ((i__2 = nitems - 
			1) < 500 && 0 <= i__2 ? i__2 : s_rnge("values", i__2, 
			"nspsho_", (ftnlen)459)) * 80, (ftnlen)80, (ftnlen)1, 
			(ftnlen)1, (ftnlen)80);
	    }
	    ++nitems;
	    s_copy(names + (((i__1 = nitems - 1) < 500 && 0 <= i__1 ? i__1 : 
		    s_rnge("names", i__1, "nspsho_", (ftnlen)465)) << 6), 
		    " ", (ftnlen)64, (ftnlen)1);
	    s_copy(values + ((i__1 = nitems - 1) < 500 && 0 <= i__1 ? i__1 : 
		    s_rnge("values", i__1, "nspsho_", (ftnlen)466)) * 80, 
		    " ", (ftnlen)80, (ftnlen)1);
	}
    } else if (m2xist_("kernels", (ftnlen)7)) {
	nspeks_();
	chkout_("NSPSHO", (ftnlen)6);
	return 0;
    } else if (m2xist_("comment", (ftnlen)7)) {
	s_copy(filnam, " ", (ftnlen)80, (ftnlen)1);
	m2getc_("file", commnd, &found, filnam, (ftnlen)4, commnd_len, (
		ftnlen)80);
	suffix_("*", &c__0, filnam, (ftnlen)1, (ftnlen)80);
	nspekc_(filnam, (ftnlen)80);
	chkout_("NSPSHO", (ftnlen)6);
	return 0;
    } else if (m2xist_("column", (ftnlen)6)) {

/*        For now we shall only display the user adjustable */
/*        items. */

	m2getc_("colnam", commnd, &found, litnam, (ftnlen)6, commnd_len, (
		ftnlen)64);
	ucase_(litnam, colnam, (ftnlen)64, (ftnlen)64);
	namxpn_(colnam, "COLUMN", error, (ftnlen)64, (ftnlen)6, error_len);
	if (have_(error, error_len)) {
	    prefix_("NSPSHO:", &c__1, error + error_len, (ftnlen)7, error_len)
		    ;
	    chkout_("NSPSHO", (ftnlen)6);
	    return 0;
	}

/*        First we see if the requested column is recognized. */

	cln2id_(colnam, &id, &found, (ftnlen)64);
	if (found) {

/*           Yes.  The requested column is recognized so we just */
/*           need to set a couple of values before further processing. */

	    useid = id;
	    numfnd = 1;
	    s_copy(name__, colnam, (ftnlen)64, (ftnlen)64);
	} else {

/*           Ooops.  The column wasn't qualified with a table name. */
/*           See if the column is in some table.  If it's in just */
/*           one table, we will show it.  If it's in more than one */
/*           say so.  If it's not in any of them say so. */

	    numfnd = 0;
	    clnum_(&n);
	    i__1 = n;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		clnid_(&i__, &id, &found);
		if (found) {
		    clgac_(&id, "COLNAM", name__, (ftnlen)6, (ftnlen)64);
/* Computing MAX */
		    i__2 = 1, i__5 = i_indx(name__, ".", (ftnlen)64, (ftnlen)
			    1);
		    p = max(i__2,i__5);
		    if (s_cmp(name__ + (p - 1), colnam, 64 - (p - 1), (ftnlen)
			    64) == 0) {
			useid = id;
			++numfnd;
		    }
		}
	    }
	}
	if (numfnd == 0) {
	    s_copy(error, "There is currently no column having the name '#'."
		    "  To obtain a list of the columns that are available you"
		    " can use the either ofthe commands: SHOW SUMMARY or SHOW"
		    " KERNELS", error_len, (ftnlen)169);
	    repmc_(error, "#", litnam, error, error_len, (ftnlen)1, (ftnlen)
		    64, error_len);
	    chkout_("NSPSHO", (ftnlen)6);
	    return 0;
	} else if (numfnd > 1) {
	    s_copy(error, "The column requested, '#', appears in more than o"
		    "ne table. To specify the table and column of interest su"
		    "pply both the table and column names separated by a peri"
		    "od as in 'TABLE.COLUMN'. ", error_len, (ftnlen)186);
	    repmc_(error, "#", litnam, error, error_len, (ftnlen)1, (ftnlen)
		    64, error_len);
	    chkout_("NSPSHO", (ftnlen)6);
	    return 0;
	} else {
	    id = useid;
	    clgac_(&id, "COLNAM", name__, (ftnlen)6, (ftnlen)64);
	}
	clgac_(&id, "TABLE", table, (ftnlen)5, (ftnlen)64);
	clgac_(&id, "JUSTIFICATION", lr, (ftnlen)13, (ftnlen)64);
	clgac_(&id, "INDEXED", indexd, (ftnlen)7, (ftnlen)64);
	clgac_(&id, "SIZE", cmpnts, (ftnlen)4, (ftnlen)64);
	clgac_(&id, "ALIAS", alias, (ftnlen)5, (ftnlen)80);
	clgai_(&id, "WIDTH", &n, &cwidth, (ftnlen)5);
	clgac_(&id, "TYPE", type__, (ftnlen)4, (ftnlen)64);
	clgac_(&id, "FORMAT", format, (ftnlen)6, (ftnlen)80);
	suffix_(".", &c__0, table, (ftnlen)1, (ftnlen)64);
	prefix_(table, &c__0, name__, (ftnlen)64, (ftnlen)64);
	s_copy(names, " ", (ftnlen)64, (ftnlen)1);
	s_copy(values, " ", (ftnlen)80, (ftnlen)1);
	s_copy(names + 64, "Attributes of column: ", (ftnlen)64, (ftnlen)22);
	s_copy(values + 80, name__, (ftnlen)80, (ftnlen)64);
	s_copy(names + 128, " ", (ftnlen)64, (ftnlen)1);
	s_copy(values + 160, " ", (ftnlen)80, (ftnlen)1);
	s_copy(names + 192, "Type", (ftnlen)64, (ftnlen)4);
	s_copy(values + 240, type__, (ftnlen)80, (ftnlen)64);
	s_copy(names + 256, "Indexed", (ftnlen)64, (ftnlen)7);
	s_copy(values + 320, indexd, (ftnlen)80, (ftnlen)64);
	s_copy(names + 320, "Number of Components", (ftnlen)64, (ftnlen)20);
	s_copy(values + 400, cmpnts, (ftnlen)80, (ftnlen)64);
	s_copy(names + 384, " ", (ftnlen)64, (ftnlen)1);
	s_copy(values + 480, " ", (ftnlen)80, (ftnlen)1);
	s_copy(names + 448, "User Adjustable Attributes", (ftnlen)64, (ftnlen)
		26);
	s_copy(values + 560, " ", (ftnlen)80, (ftnlen)1);
	s_copy(names + 512, " ", (ftnlen)64, (ftnlen)1);
	s_copy(values + 640, " ", (ftnlen)80, (ftnlen)1);
	s_copy(names + 576, "Column justification", (ftnlen)64, (ftnlen)20);
	s_copy(values + 720, lr, (ftnlen)80, (ftnlen)64);
	s_copy(names + 640, "Column width", (ftnlen)64, (ftnlen)12);
	intstr_(&cwidth, values + 800, (ftnlen)80);
	s_copy(names + 704, "Column heading", (ftnlen)64, (ftnlen)14);
	s_copy(values + 880, alias, (ftnlen)80, (ftnlen)80);
	if (s_cmp(type__, "TIME", (ftnlen)64, (ftnlen)4) == 0 || s_cmp(type__,
		 "INTEGER", (ftnlen)64, (ftnlen)7) == 0 || s_cmp(type__, 
		"DOUBLE PRECISION", (ftnlen)64, (ftnlen)16) == 0) {
	    s_copy(names + 768, "Format of output", (ftnlen)64, (ftnlen)16);
	    s_copy(values + 960, format, (ftnlen)80, (ftnlen)80);
	    s_copy(names + 832, " ", (ftnlen)64, (ftnlen)1);
	    s_copy(values + 1040, " ", (ftnlen)80, (ftnlen)1);
	    nitems = 14;
	} else {
	    s_copy(names + 768, " ", (ftnlen)64, (ftnlen)1);
	    s_copy(values + 960, " ", (ftnlen)80, (ftnlen)1);
	    nitems = 13;
	}
    } else if (m2xist_("indexed", (ftnlen)7)) {
	clnum_(&nitems);
	if (nitems == 0) {
	    nspwln_(" ", (ftnlen)1);
	    nspwln_("There are currently no columns. ", (ftnlen)32);
	    nspwln_(" ", (ftnlen)1);
	    chkout_("NSPSHO", (ftnlen)6);
	    return 0;
	}
	n = 0;
	i__1 = nitems;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    clnid_(&i__, &id, &found);
	    clgac_(&id, "INDEXED", indexd, (ftnlen)7, (ftnlen)64);
	    if (s_cmp(indexd, "YES", (ftnlen)64, (ftnlen)3) == 0) {
		++n;
		clgac_(&id, "NAME", names + (((i__2 = n - 1) < 500 && 0 <= 
			i__2 ? i__2 : s_rnge("names", i__2, "nspsho_", (
			ftnlen)666)) << 6), (ftnlen)4, (ftnlen)64);
	    }
	}
	shellc_(&n, names, (ftnlen)64);
	nspwln_(" ", (ftnlen)1);
	nspwln_("Indexed Columns and their Types", (ftnlen)31);
	nspwln_(" ", (ftnlen)1);
	i__1 = n;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    cln2id_(names + (((i__2 = i__ - 1) < 500 && 0 <= i__2 ? i__2 : 
		    s_rnge("names", i__2, "nspsho_", (ftnlen)679)) << 6), &id,
		     &found, (ftnlen)64);
	    clgac_(&id, "TYPE", values + ((i__2 = i__ - 1) < 500 && 0 <= i__2 
		    ? i__2 : s_rnge("values", i__2, "nspsho_", (ftnlen)680)) *
		     80, (ftnlen)4, (ftnlen)80);
	}
	flgrpt_(&n, names, values, (S_fp)nspwln_, (ftnlen)64, (ftnlen)80);
	nspwln_(" ", (ftnlen)1);
	chkout_("NSPSHO", (ftnlen)6);
	return 0;
    } else if (m2xist_("summary", (ftnlen)7)) {
	s_copy(names, "Column", (ftnlen)64, (ftnlen)6);
	s_copy(values, "Description", (ftnlen)80, (ftnlen)11);
	s_copy(names + 64, " ", (ftnlen)64, (ftnlen)1);
	s_copy(values + 80, " ", (ftnlen)80, (ftnlen)1);
	clnum_(&nitems);
	if (nitems == 0) {
	    nspwln_(" ", (ftnlen)1);
	    nspwln_("There are currently no columns. ", (ftnlen)32);
	    nspwln_(" ", (ftnlen)1);
	    chkout_("NSPSHO", (ftnlen)6);
	    return 0;
	}
	i__1 = nitems;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    clnid_(&i__, &id, &found);
	    clgac_(&id, "NAME", names + (((i__2 = i__ + 1) < 500 && 0 <= i__2 
		    ? i__2 : s_rnge("names", i__2, "nspsho_", (ftnlen)713)) <<
		     6), (ftnlen)4, (ftnlen)64);
	}
	n = nitems + 2;

/*        Sort only the items from the 3rd index on. */

	shellc_(&nitems, names + 128, (ftnlen)64);
	i__1 = nitems + 2;
	for (i__ = 3; i__ <= i__1; ++i__) {
	    s_copy(values + ((i__2 = i__ - 1) < 500 && 0 <= i__2 ? i__2 : 
		    s_rnge("values", i__2, "nspsho_", (ftnlen)723)) * 80, 
		    " ", (ftnlen)80, (ftnlen)1);
	    cln2id_(names + (((i__2 = i__ - 1) < 500 && 0 <= i__2 ? i__2 : 
		    s_rnge("names", i__2, "nspsho_", (ftnlen)724)) << 6), &id,
		     &found, (ftnlen)64);
	    clgac_(&id, "TYPE", values + ((i__2 = i__ - 1) < 500 && 0 <= i__2 
		    ? i__2 : s_rnge("values", i__2, "nspsho_", (ftnlen)725)) *
		     80, (ftnlen)4, (ftnlen)80);
	    clgac_(&id, "INDEXED", indexd, (ftnlen)7, (ftnlen)64);
	    if (s_cmp(indexd, "YES", (ftnlen)64, (ftnlen)3) == 0) {
		s_copy(values + (((i__2 = i__ - 1) < 500 && 0 <= i__2 ? i__2 :
			 s_rnge("values", i__2, "nspsho_", (ftnlen)730)) * 80 
			+ 19), "(Indexed)", (ftnlen)61, (ftnlen)9);
	    }
	}
	nspwln_(" ", (ftnlen)1);
	flgrpt_(&n, names, values, (S_fp)nspwln_, (ftnlen)64, (ftnlen)80);
	nspwln_(" ", (ftnlen)1);
	chkout_("NSPSHO", (ftnlen)6);
	return 0;
    }

/*     Now all we have to do is ship out these lines. */

    flgrpt_(&nitems, names, values, (S_fp)nspwln_, (ftnlen)64, (ftnlen)80);
    chkout_("NSPSHO", (ftnlen)6);
    return 0;
} /* nspsho_ */

