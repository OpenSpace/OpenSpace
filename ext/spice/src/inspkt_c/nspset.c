/* nspset.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__26 = 26;
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

/* Subroutine */ int nspset_(char *commnd, char *error, ftnlen commnd_len, 
	ftnlen error_len)
{
    /* Initialized data */

    static logical first = TRUE_;
    static char synval[150*32] = "                                          "
	    "                                                                "
	    "                                            " "                 "
	    "                                                                "
	    "                                                                "
	    "     " "                                                        "
	    "                                                                "
	    "                              " "                               "
	    "                                                                "
	    "                                                       " "      "
	    "                                                                "
	    "                                                                "
	    "                " "                                             "
	    "                                                                "
	    "                                         ";
    static char fmt[80*8] = "dict                                           "
	    "                                 " "flag                        "
	    "                                                    " "verbat   "
	    "                                                                "
	    "       " "spaced                                                "
	    "                          " "marked                             "
	    "                                             " "plain           "
	    "                                                                " 
	    "delimited                                                      "
	    "                 " "delimited                                   "
	    "                                    ";
    static char form[80*8] = "FLAGGED                                       "
	    "                                  " "FLAGGED PRESERVED          "
	    "                                                     " "VERBATIM"
	    "                                                                "
	    "        " "SPACED TABULAR                                       "
	    "                           " "MARKED TABULAR                    "
	    "                                              " "TABULAR        "
	    "                                                                 "
	     "DELIMITED                                                     "
	    "                  " "DELIMITED                                  "
	    "                                     ";

    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen), i_indx(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static char name__[80];
    extern logical have_(char *, ftnlen);
    static char item[80], temp[80], attr[80];
    static integer i__;
    extern /* Subroutine */ int clgac_(integer *, char *, char *, ftnlen, 
	    ftnlen);
    static integer n, p;
    extern /* Subroutine */ int clpac_(integer *, char *, char *, ftnlen, 
	    ftnlen);
    static integer w;
    extern /* Subroutine */ int clpai_(integer *, char *, integer *, ftnlen);
    static char delim[80];
    extern /* Subroutine */ int clnid_(integer *, integer *, logical *), 
	    chkin_(char *, ftnlen), ucase_(char *, char *, ftnlen, ftnlen), 
	    repmc_(char *, char *, char *, char *, ftnlen, ftnlen, ftnlen, 
	    ftnlen);
    static integer useid;
    static logical found;
    extern /* Subroutine */ int clnum_(integer *);
    static integer atype;
    static char quote[80];
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int m2chck_(char *, char *, integer *, char *, 
	    char *, ftnlen, ftnlen, ftnlen, ftnlen), cln2id_(char *, integer *
	    , logical *, ftnlen), m2geta_(char *, char *, logical *, char *, 
	    ftnlen, ftnlen, ftnlen), m2getc_(char *, char *, logical *, char *
	    , ftnlen, ftnlen, ftnlen), m2marg_(char *, ftnlen), m2geti_(char *
	    , char *, logical *, integer *, ftnlen, ftnlen), m2ints_(integer *
	    , char *, integer *, char *, ftnlen, ftnlen);
    static integer id;
    static char bs[1];
    extern logical m2xist_(char *, ftnlen);
    static char colnam[80], margin[80];
    static logical colcom;
    extern /* Subroutine */ int replch_(char *, char *, char *, char *, 
	    ftnlen, ftnlen, ftnlen, ftnlen);
    static char litnam[80];
    static integer ivalue, numfnd;
    static char svalue[80];
    extern /* Subroutine */ int prefix_(char *, integer *, char *, ftnlen, 
	    ftnlen), chkout_(char *, ftnlen), namxpn_(char *, char *, char *, 
	    ftnlen, ftnlen, ftnlen), suffix_(char *, integer *, char *, 
	    ftnlen, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int nspcht_(char *, integer *, ftnlen), nspslr_(
	    integer *, integer *);
    static char synkey[80*32];
    extern /* Subroutine */ int nspmrg_(char *, ftnlen);
    static integer synptr[32];
    extern /* Subroutine */ int bbputc_1__(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen), bbputi_1__(char *, char *, integer *, 
	    integer *, ftnlen, ftnlen);

/*     March 26, 2003 */

/*        Added the SET FORMAT DELIMITED command */

/*     August 29, 1996 */

/*        Increase the size of a word from 32 to 80 characters. */

/*     Nov 21, 1995 */

/*        Removed the SET EDITOR command since its already supported */
/*        by the built in command editor code. */

/*     Nov 2, 1995 */

/*        Added the ability to use templates instead of full */
/*        names when requesting information about a column. */

/*     Sep 7, 1995 */

/*        Added    DEFAULT INTEGER FORMAT */
/*                 DEFAULT FLOATING FORMAT */

/*        Synstax and actions to the routine. */

/*     Sep 6, 1995 */

/*       Changed the syntax */

/*       from SET TIME FORMAT */
/*       to   SET DEFAULT TIME FORMAT */

/*     Aug 29, 1995 */

/*       Added the syntax SET HELP NO WAIT (two words) in addition to */
/*       the original SET HELP NOWAIT. */

/*     Aug 15, 1995 */

/*     Increase the declared length of syntax templates from 120 to 150 */


/*     This routine allows users to set parameters that will be used */
/*     to establish the appearance of reports. */


/*     SPICELIB functions */


/*     Interface to the SPICELIB error handling. */


/*     Meta/2 functions */


/*     Parameters used in parsing. */


/*     Meta/2 syntax definition variables. */


/*     DICTNY */
/*     FLAGGD */
/*     VRBATM */
/*     SPACED */
/*     MARKED */
/*     PLAIN */
/*     FRMMRK */
/*     COLWID */
/*     COLJST */
/*     ALIAS */
/*     COLFMT */
/*     TIMFMT */
/*     PAGEHT */
/*     PAGEWD */
/*     PAGETL */
/*     TLFREQ */
/*     TLJUST */
/*     HDFREQ */
/*     RPTLIM */
/*     AUTOAD */


/*     There are seven different formats supported.  The names */
/*     associated with these formats shall be regarded as */
/*     global variables.  We need arrays to hold these names. */


/*     Local Variables */


/*     Save everything. */


/*     Standard Spicelib error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("NSPSET", (ftnlen)6);

/*     On the first pass establish the syntax that this routine */
/*     is responsible for recognizing. */

    if (first) {
	first = FALSE_;

/*     The syntax definitions follow. */

	s_copy(synval + 900, "FORMAT[fmt] FLAGGED[dict] ", (ftnlen)150, (
		ftnlen)26);
	s_copy(synval + 1050, "FORMAT[fmt] FLAGGED PRESERVED[flag] ", (ftnlen)
		150, (ftnlen)36);
	s_copy(synval + 1200, "FORMAT[fmt] VERBATIM[verbat]", (ftnlen)150, (
		ftnlen)28);
	s_copy(synval + 1350, "FORMAT[fmt] SPACED[spaced] TABULAR (0:1){ PRE"
		"SERVED[preserved] } ", (ftnlen)150, (ftnlen)65);
	s_copy(synval + 1500, "FORMAT[fmt] MARKED[marked] TABULAR (0:1){ PRE"
		"SERVED[preserved] } ", (ftnlen)150, (ftnlen)65);
	s_copy(synval + 1650, "FORMAT[fmt] TABULAR[plain] (0:1){ PRESERVED[p"
		"reserved] } ", (ftnlen)150, (ftnlen)57);
	s_copy(synval + 1800, "FORMAT[fmt] DELIMITED[delimited] (0:3){ DELIM"
		"ITER #word(%)[delimiter]     | QUOTE #word(%)[quote]      | "
		"PRESERVED[preserved] } ", (ftnlen)150, (ftnlen)128);
	s_copy(synval + 1950, "FORMAT[fmt] DELIMITED[delimited] (0:3){ DELIM"
		"ITER SPACE[delimiter]      | QUOTE #word(%)[quote]      | PR"
		"ESERVED[preserved] } ", (ftnlen)150, (ftnlen)126);

/*     The syntax below allows the user to change the leadoff */
/*     character that is used for the MARKED TABULAR reports. */

	s_copy(synval + 2100, "FORMAT MARK #word(%)[fmtmark] ", (ftnlen)150, (
		ftnlen)30);

/*     Below is the syntax for column attributes that the use */
/*     can control in reports. */

	s_copy(synval + 2250, "COLUMN #word[colnam] WIDTH #int(8:)[columnwdt"
		"h] ", (ftnlen)150, (ftnlen)48);
	s_copy(synval + 2400, "COLUMN #word[colnam] JUSTIFICATION (1:1){ LEF"
		"T[left]      | RIGHT[right] } ", (ftnlen)150, (ftnlen)75);
	s_copy(synval + 2550, "COLUMN #word[colnam] HEADING (1:)#word[alias]",
		 (ftnlen)150, (ftnlen)45);
	s_copy(synval + 2700, "COLUMN #word[colnam] FORMAT (1:)#word[colfmt]",
		 (ftnlen)150, (ftnlen)45);

/*     The TIME column is always present and has special formatting */
/*     requirements.  Users may set these using the command below. */

	s_copy(synval + 2850, "DEFAULT TIME FORMAT (1:)#word[timefmt] ", (
		ftnlen)150, (ftnlen)39);

/*     The next set of command syntax definitions give the user */
/*     control over the dimensions of the output. */

	s_copy(synval + 3000, "PAGE HEIGHT #int(20:)[pageht] ", (ftnlen)150, (
		ftnlen)30);
	s_copy(synval + 3150, "PAGE WIDTH #int(40:132)[pagewdth] ", (ftnlen)
		150, (ftnlen)34);
	s_copy(synval + 3300, "PAGE (1:1){ TITLE NONE[notitle] | TITLE (1:)#"
		"word[pagetitle] }", (ftnlen)150, (ftnlen)62);

/*     The next set of syntax definitions allow the user control */
/*     over titles and their positions within reports. */

	s_copy(synval + 3450, "TITLE FREQUENCY[titlefreq] (1:1){ 0[zero] | 1"
		"ST[first] | FIRST[first] | ALL[all] | EVERY #int(1:)[every] "
		"} ", (ftnlen)150, (ftnlen)107);
	s_copy(synval + 3600, "TITLE JUSTIFICATION[titlejustify] (1:1){ LEFT"
		"[left] | RIGHT[right] | CENTER[center] } ", (ftnlen)150, (
		ftnlen)86);

/*     The user may use the syntax given below to control the */
/*     frequency with which headers are printed with tabular */
/*     format reports. */

	s_copy(synval + 3750, "HEADER FREQUENCY[headerfreq] (1:1){ 0[zero] |"
		" 1ST[first] | FIRST[first] | ALL[all] | EVERY #int(1:)[every"
		"] } ", (ftnlen)150, (ftnlen)109);
	s_copy(synval + 3900, "DELUGE WARNING #int(1:)[limit]", (ftnlen)150, (
		ftnlen)30);
	s_copy(synval + 4050, "AUTOADJUST[auto] (1:1){ OFF[off]  | ASK[ask] "
		" | ON[on] } ", (ftnlen)150, (ftnlen)57);
	s_copy(synval + 4200, "HELP[help] (1:1){ WAIT[wait]      | NO WAIT[n"
		"owait]       | NOWAIT[nowait]  } ", (ftnlen)150, (ftnlen)78);
	s_copy(synval + 4350, "DEFAULT INTEGER FORMAT #word[intfmt] ", (
		ftnlen)150, (ftnlen)37);
	s_copy(synval + 4500, "DEFAULT FLOATING FORMAT #word[dpfmt] ", (
		ftnlen)150, (ftnlen)37);
	*(unsigned char *)bs = '@';
	for (i__ = 1; i__ <= 26; ++i__) {
	    replch_(synval + ((i__1 = i__ + 5) < 32 && 0 <= i__1 ? i__1 : 
		    s_rnge("synval", i__1, "nspset_", (ftnlen)465)) * 150, 
		    "#", bs, synval + ((i__2 = i__ + 5) < 32 && 0 <= i__2 ? 
		    i__2 : s_rnge("synval", i__2, "nspset_", (ftnlen)465)) * 
		    150, (ftnlen)150, (ftnlen)1, (ftnlen)1, (ftnlen)150);
	}
	m2ints_(&c__26, synkey, synptr, synval, (ftnlen)80, (ftnlen)150);
    }

/*     See if this command matches a known syntax.  If it doesn't */
/*     there is no point in hanging around. */

    m2chck_(commnd, synkey, synptr, synval, error, commnd_len, (ftnlen)80, (
	    ftnlen)150, error_len);
    if (have_(error, error_len)) {
	prefix_("NSPSET:", &c__1, error + error_len, (ftnlen)7, error_len);
	chkout_("NSPSET", (ftnlen)6);
	return 0;
    }

/*     If we get to this point, we have a legitimate command. */
/*     See if the user is trying to set a report attribute or a column */
/*     attribute.  For column commands, we extract the column name */
/*     and the attribute of that column that the user is allowed to */
/*     set.  The variables used for this are COLNAM, ATTR, and SVALUE */
/*     or IVALUE depending upon whether the attribute is represented */
/*     as a string or integer. */

/*     The other attributes control shape and other global */
/*     characteristics of reports.  In these cases, we extract the */
/*     characteristics name and value.  The variables used here */
/*     are ITEM to hold the name of the control item and SVALUE or */
/*     IVALUE to contain the value of the control item. */

    colcom = m2xist_("colnam", (ftnlen)6);
    if (colcom) {
	m2getc_("colnam", commnd, &found, litnam, (ftnlen)6, commnd_len, (
		ftnlen)80);
	ucase_(litnam, colnam, (ftnlen)80, (ftnlen)80);
	namxpn_(colnam, "COLUMN", error, (ftnlen)80, (ftnlen)6, error_len);
	if (have_(error, error_len)) {
	    prefix_("NSPSET:", &c__1, error + error_len, (ftnlen)7, error_len)
		    ;
	    chkout_("NSPSET", (ftnlen)6);
	    return 0;
	}
	if (m2xist_("columnwdth", (ftnlen)10)) {
	    s_copy(attr, "WIDTH", (ftnlen)80, (ftnlen)5);
	    atype = 2;
	    m2geti_("columnwdth", commnd, &found, &ivalue, (ftnlen)10, 
		    commnd_len);
	} else if (m2xist_("left", (ftnlen)4)) {
	    s_copy(attr, "JUSTIFICATION", (ftnlen)80, (ftnlen)13);
	    atype = 1;
	    s_copy(svalue, "LEFT", (ftnlen)80, (ftnlen)4);
	} else if (m2xist_("right", (ftnlen)5)) {
	    s_copy(attr, "JUSTIFICATION", (ftnlen)80, (ftnlen)13);
	    atype = 1;
	    s_copy(svalue, "RIGHT", (ftnlen)80, (ftnlen)5);
	} else if (m2xist_("alias", (ftnlen)5)) {
	    s_copy(attr, "ALIAS", (ftnlen)80, (ftnlen)5);
	    atype = 1;
	    m2geta_("alias", commnd, &found, svalue, (ftnlen)5, commnd_len, (
		    ftnlen)80);
	} else if (m2xist_("colfmt", (ftnlen)6)) {
	    s_copy(attr, "FORMAT", (ftnlen)80, (ftnlen)6);
	    atype = 1;
	    m2geta_("colfmt", commnd, &found, svalue, (ftnlen)6, commnd_len, (
		    ftnlen)80);
	}
    } else if (m2xist_("help", (ftnlen)4)) {
	s_copy(item, "HELPPROMPT", (ftnlen)80, (ftnlen)10);
	atype = 2;
	if (m2xist_("wait", (ftnlen)4)) {
	    ivalue = 1;
	} else {
	    ivalue = 0;
	}
    } else if (m2xist_("fmt", (ftnlen)3)) {
	s_copy(item, "FORMAT", (ftnlen)80, (ftnlen)6);
	atype = 1;
	i__ = 1;
	while(i__ <= 8 && ! m2xist_(fmt + ((i__1 = i__ - 1) < 8 && 0 <= i__1 ?
		 i__1 : s_rnge("fmt", i__1, "nspset_", (ftnlen)562)) * 80, (
		ftnlen)80)) {
	    ++i__;
	}
	s_copy(svalue, form + ((i__1 = i__ - 1) < 8 && 0 <= i__1 ? i__1 : 
		s_rnge("form", i__1, "nspset_", (ftnlen)567)) * 80, (ftnlen)
		80, (ftnlen)80);
	if (m2xist_("preserved", (ftnlen)9)) {
	    suffix_("PRESERVED", &c__1, svalue, (ftnlen)9, (ftnlen)80);
	}
	if (m2xist_("delimited", (ftnlen)9)) {
	    s_copy(delim, "TAB", (ftnlen)80, (ftnlen)3);
	    s_copy(quote, "\"", (ftnlen)80, (ftnlen)1);
	    m2getc_("delimiter", commnd, &found, delim, (ftnlen)9, commnd_len,
		     (ftnlen)80);
	    m2getc_("quote", commnd, &found, quote, (ftnlen)5, commnd_len, (
		    ftnlen)80);
	    if (eqstr_("SPACE", delim, (ftnlen)5, (ftnlen)80)) {
		s_copy(delim, "SPACE", (ftnlen)80, (ftnlen)5);
	    }
	    bbputc_1__("POST", "QUOTE", &c__1, quote, (ftnlen)4, (ftnlen)5, (
		    ftnlen)80);
	    bbputc_1__("POST", "DELIMITER", &c__1, delim, (ftnlen)4, (ftnlen)
		    9, (ftnlen)80);
	}
    } else if (m2xist_("fmtmark", (ftnlen)7)) {
	s_copy(item, "FMTMARK", (ftnlen)80, (ftnlen)7);
	atype = 1;
	m2getc_("fmtmark", commnd, &found, svalue, (ftnlen)7, commnd_len, (
		ftnlen)80);
    } else if (m2xist_("timefmt", (ftnlen)7)) {
	s_copy(item, "TIMEFMT", (ftnlen)80, (ftnlen)7);
	atype = 1;
	m2geta_("timefmt", commnd, &found, svalue, (ftnlen)7, commnd_len, (
		ftnlen)80);

/*        If the format entered is one of the SCLK formats we need */
/*        to make sure that the SCLK kernel is loaded for */
/*        that SCLK. */

	nspcht_(svalue, &w, (ftnlen)80);
	if (have_(error, error_len)) {
	    prefix_("NSPSET:", &c__1, error + error_len, (ftnlen)7, error_len)
		    ;
	    chkout_("NSPSET", (ftnlen)6);
	    return 0;
	}
    } else if (m2xist_("intfmt", (ftnlen)6)) {
	s_copy(item, "INTFMT", (ftnlen)80, (ftnlen)6);
	atype = 1;
	m2geta_("intfmt", commnd, &found, svalue, (ftnlen)6, commnd_len, (
		ftnlen)80);
	ucase_(svalue, temp, (ftnlen)80, (ftnlen)80);
	if (s_cmp(temp, "DEFAULT", (ftnlen)80, (ftnlen)7) == 0) {
	    s_copy(svalue, temp, (ftnlen)80, (ftnlen)80);
	}
    } else if (m2xist_("dpfmt", (ftnlen)5)) {
	s_copy(item, "DPFMT", (ftnlen)80, (ftnlen)5);
	atype = 1;
	m2geta_("dpfmt", commnd, &found, svalue, (ftnlen)5, commnd_len, (
		ftnlen)80);
	ucase_(svalue, temp, (ftnlen)80, (ftnlen)80);
	if (s_cmp(temp, "DEFAULT", (ftnlen)80, (ftnlen)7) == 0) {
	    s_copy(svalue, temp, (ftnlen)80, (ftnlen)80);
	}
    } else if (m2xist_("pageht", (ftnlen)6)) {
	s_copy(item, "PAGEHEIGHT", (ftnlen)80, (ftnlen)10);
	atype = 2;
	m2geti_("pageht", commnd, &found, &ivalue, (ftnlen)6, commnd_len);
    } else if (m2xist_("pagewdth", (ftnlen)8)) {
	s_copy(item, "PAGEWIDTH", (ftnlen)80, (ftnlen)9);
	atype = 2;
	m2geti_("pagewdth", commnd, &found, &ivalue, (ftnlen)8, commnd_len);

/*        We need to notify the command loop "page margins" routine */
/*        that the pagewidth has been modified. */

	nspslr_(&c__1, &ivalue);
    } else if (m2xist_("pagetitle", (ftnlen)9)) {
	s_copy(item, "PAGETITLE", (ftnlen)80, (ftnlen)9);
	atype = 1;
	m2geta_("pagetitle", commnd, &found, svalue, (ftnlen)9, commnd_len, (
		ftnlen)80);
    } else if (m2xist_("notitle", (ftnlen)7)) {
	s_copy(item, "PAGETITLE", (ftnlen)80, (ftnlen)9);
	atype = 1;
	s_copy(svalue, " ", (ftnlen)80, (ftnlen)1);
    } else if (m2xist_("titlefreq", (ftnlen)9)) {
	s_copy(item, "TITLEFREQUENCY", (ftnlen)80, (ftnlen)14);
	atype = 2;
	if (m2xist_("zero", (ftnlen)4)) {
	    ivalue = -1;
	} else if (m2xist_("first", (ftnlen)5)) {
	    ivalue = 0;
	} else if (m2xist_("all", (ftnlen)3)) {
	    ivalue = 1;
	} else if (m2xist_("every", (ftnlen)5)) {
	    m2geti_("every", commnd, &found, &ivalue, (ftnlen)5, commnd_len);
	}
    } else if (m2xist_("limit", (ftnlen)5)) {
	s_copy(item, "REPORTLIMIT", (ftnlen)80, (ftnlen)11);
	atype = 2;
	m2geti_("limit", commnd, &found, &ivalue, (ftnlen)5, commnd_len);
    } else if (m2xist_("titlejustify", (ftnlen)12)) {
	s_copy(item, "TITLEJUSTIFICATION", (ftnlen)80, (ftnlen)18);
	atype = 1;
	if (m2xist_("left", (ftnlen)4)) {
	    s_copy(svalue, "LEFT", (ftnlen)80, (ftnlen)4);
	} else if (m2xist_("right", (ftnlen)5)) {
	    s_copy(svalue, "RIGHT", (ftnlen)80, (ftnlen)5);
	} else if (m2xist_("center", (ftnlen)6)) {
	    s_copy(svalue, "CENTER", (ftnlen)80, (ftnlen)6);
	}
    } else if (m2xist_("headerfreq", (ftnlen)10)) {
	s_copy(item, "HEADERFREQUENCY", (ftnlen)80, (ftnlen)15);
	atype = 2;
	if (m2xist_("zero", (ftnlen)4)) {
	    ivalue = -1;
	} else if (m2xist_("first", (ftnlen)5)) {
	    ivalue = 0;
	} else if (m2xist_("all", (ftnlen)3)) {
	    ivalue = 1;
	} else {
	    m2geti_("every", commnd, &found, &ivalue, (ftnlen)5, commnd_len);
	}
    } else if (m2xist_("auto", (ftnlen)4)) {
	s_copy(item, "AUTOADJUST", (ftnlen)80, (ftnlen)10);
	atype = 2;
	if (m2xist_("off", (ftnlen)3)) {
	    ivalue = 0;
	} else if (m2xist_("ask", (ftnlen)3)) {
	    ivalue = 1;
	} else if (m2xist_("on", (ftnlen)2)) {
	    ivalue = 2;
	}
    }

/*     Now depending upon the type of object we just snagged, */
/*     hand it to the appropriate buffering routine. */

    if (colcom) {
	cln2id_(colnam, &id, &found, (ftnlen)80);
	if (found) {

/*           Yes.  The requested column is recognized so we just */
/*           need to set a couple of values before further processing. */

	    useid = id;
	    numfnd = 1;
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
		    clgac_(&id, "COLNAM", name__, (ftnlen)6, (ftnlen)80);
/* Computing MAX */
		    i__2 = 1, i__3 = i_indx(name__, ".", (ftnlen)80, (ftnlen)
			    1);
		    p = max(i__2,i__3);
		    if (s_cmp(name__ + (p - 1), colnam, 80 - (p - 1), (ftnlen)
			    80) == 0) {
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
		    80, error_len);
	    chkout_("NSPSET", (ftnlen)6);
	    return 0;
	} else if (numfnd > 1) {
	    s_copy(error, "The column requested, '#', appears in more than o"
		    "ne table. To specify the table and column of interest su"
		    "pply both the table and column names separated by a peri"
		    "od as in 'TABLE.COLUMN'. ", error_len, (ftnlen)186);
	    repmc_(error, "#", litnam, error, error_len, (ftnlen)1, (ftnlen)
		    80, error_len);
	    chkout_("NSPSET", (ftnlen)6);
	    return 0;
	} else {
	    id = useid;
	}
	if (atype == 2) {
	    clpai_(&id, attr, &ivalue, (ftnlen)80);
	} else {
	    clpac_(&id, attr, svalue, (ftnlen)80, (ftnlen)80);
	}
    } else {
	if (atype == 2) {
	    bbputi_1__("POST", item, &c__1, &ivalue, (ftnlen)4, (ftnlen)80);
	} else {
	    bbputc_1__("POST", item, &c__1, svalue, (ftnlen)4, (ftnlen)80, (
		    ftnlen)80);
	}
    }

/*     If we adjusted the page width, we shall want to adjust the */
/*     margins used by META/2 for reporting spelling errors.  Every */
/*     place else in Inspekt, looks this up directly.  Meta/2 on the */
/*     other hand doesn't know about Inspekt and thus needs to be */
/*     told directly what the margins are. */

    if (m2xist_("pagewdth", (ftnlen)8)) {
	nspmrg_(margin, (ftnlen)80);
	m2marg_(margin, (ftnlen)80);
    }

/*     One final error check, and then we are all done. */

    if (have_(error, error_len)) {
	prefix_("NSPSET:", &c__1, error + error_len, (ftnlen)7, error_len);
	chkout_("NSPSET", (ftnlen)6);
	return 0;
    }
    chkout_("NSPSET", (ftnlen)6);
    return 0;
} /* nspset_ */

