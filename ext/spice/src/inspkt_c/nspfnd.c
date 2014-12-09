/* nspfnd.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__5 = 5;
static integer c__1 = 1;
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

/* Subroutine */ int nspfnd_(char *commnd, char *error, ftnlen commnd_len, 
	ftnlen error_len)
{
    /* Initialized data */

    static logical first = TRUE_;
    static integer thshld = 200;
    static char rname[6] = "NSPFND";
    static char synval[700*11] = "                                          "
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
	    "                  " "                                           "
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
	    "                 " "                                            "
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
	    "                " "                                             "
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
	    "               " "                                              "
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
	    "              " "                                               "
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
	    "             ";

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int getopt_2__(char *, integer *, integer *, char 
	    *, char *, integer *, integer *, ftnlen, ftnlen, ftnlen);
    extern logical have_(char *, ftnlen);
    static char cols[1024];
    static integer from, last;
    static char copy[1024];
    static integer nopt, b, e, i__, k, n;
    extern logical batch_(void);
    static integer r__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static logical found;
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen);
    static char title[80];
    static integer count, every;
    static logical lstat[3];
    extern integer rtrim_(char *, ftnlen);
    static logical sstat[3];
    static char style[80], query[1024];
    extern /* Subroutine */ int m2chck_(char *, char *, integer *, char *, 
	    char *, ftnlen, ftnlen, ftnlen, ftnlen), m2geta_(char *, char *, 
	    logical *, char *, ftnlen, ftnlen, ftnlen);
    extern integer m2have_(char *, ftnlen);
    extern /* Subroutine */ int m2geti_(char *, char *, logical *, integer *, 
	    ftnlen, ftnlen), m2vget_(char *, integer *, logical *, integer *, 
	    integer *, ftnlen), m2ints_(integer *, char *, integer *, char *, 
	    ftnlen, ftnlen);
    static char bs[1];
    extern logical m2xist_(char *, ftnlen);
    static integer to;
    static logical select;
    static integer center;
    static char messge[800];
    static logical sample;
    extern /* Subroutine */ int replch_(char *, char *, char *, char *, 
	    ftnlen, ftnlen, ftnlen, ftnlen);
    static char format[32];
    static integer idlist[101], lradus, oindnt;
    static char optnam[1*3];
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    static integer tindnt;
    extern /* Subroutine */ int namxpn_(char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen), repsub_(char *, integer *, integer *, char *, 
	    char *, ftnlen, ftnlen, ftnlen);
    static integer uradus, option;
    static logical disply;
    extern /* Subroutine */ int nsplog_(char *, logical *, ftnlen), clscop_(
	    char *, char *, integer *, integer *, integer *, char *, ftnlen, 
	    ftnlen, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int nspgst_(char *, logical *, ftnlen), nspwln_(
	    char *, ftnlen);
    static char synkey[32*11];
    extern /* Subroutine */ int nspioh_(char *, ftnlen), nspmrg_(char *, 
	    ftnlen), suffix_(char *, integer *, char *, ftnlen, ftnlen), 
	    nspdel_(integer *, integer *, integer *, integer *, integer *), 
	    nsppst_(char *, logical *, ftnlen), nspflg_(integer *, integer *, 
	    integer *, integer *, integer *, char *, ftnlen), nspvrb_(integer 
	    *, integer *, integer *, integer *, integer *), nsptab_(integer *,
	     integer *, integer *, integer *, integer *, char *, ftnlen);
    static logical summrz;
    static integer synptr[11];
    static char opttxt[80*3];
    static integer num;
    extern /* Subroutine */ int bbgetc_1__(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen), bbgeti_1__(char *, char *, integer *, 
	    integer *, ftnlen, ftnlen), nicepr_1__(char *, char *, S_fp, 
	    ftnlen, ftnlen);


/*     This routine handles the task of setting up the information */
/*     needed to format the results of an E-kernel query and */
/*     passes this information on to the appropriate formatting */
/*     routines. */

/* $ Version */

/*     Inspekt Routine Version 4.1.0 21-Nov-1995 (WLT) */

/*       Fixed bug in table name expansion. */

/*     Inspekt Routine Version 4.0.0 2-NOV-1995 (WLT) */

/*       The ability to recognized patterns for column names */
/*       and tables was added.  Patterns when encountered are */
/*       now replaced by the matching name.  If a pattern occurs */
/*       that is not uniquely matched an error is set and the */
/*       routine returns. */

/*     Inspekt Routine Version 3.1.0 4-AUG-1995 (WLT) */

/*       Fixed a counting problem in the sample counting */
/*       and select subsample so that it counts correctly */
/*       on output. */

/*     Inspekt Routine Version 3.0.0 3-AUG-1995 (WLT) */

/*       Modified what is sent to CLMGR to reflect change */
/*       in the EK interface routines.  Actually, Nat is */
/*       responsible for this change. */

/*     Inspekt Routine Version 2.0.0 7-APR-1995 (WLT) */

/*        The unused variable RNAMEC was removed. */


/*     Inspekt's output function */


/*     SPICELIB functions */


/*     Meta/2 Functions */


/*     Interface to the SPICELIB error handling. */


/*     Meta/2 syntax definition variables. */


/*     SELWLD */
/*     SELCOL */
/*     SAMFLL */
/*     SAMFOL */
/*     SAMPRC */


/*     Local Variables */


/*     Save everything. */


/*     Standard Spicelib error handling. */

    if (return_()) {
	return 0;
    }
    chkin_(rname, (ftnlen)6);

/*     On the first pass establish the syntax that this routine */
/*     is responsible for recognizing. */

    if (first) {
	first = FALSE_;
	*(unsigned char *)bs = '@';
	s_copy(synval + 4200, "SELECT[select] *[wildcard] FROM[f] (1:)#word["
		"query] (0:2){ ORDER BY (1:100)@word[by] | WHERE (1:)#word[wh"
		"ere] } ", (ftnlen)700, (ftnlen)112);
	s_copy(synval + 4900, "SELECT[select] (1:100)#word[columns] FROM[f] "
		"(1:)#word[query] (0:2){ ORDER BY (1:100)@word[by]   | WHERE "
		"(1:)#word[where] } ", (ftnlen)700, (ftnlen)124);


/*        Below are the full set of SAMPLE commands. */

/*        SAMPLE[sample] @int(1:)[count] */
/*        (1:1){ SELECT[select]              (1:100)@word[columns] */
/*             | SELECT[select] *[wildcard] */
/*             } */
/*        FROM[f] (1:)@word[query] */
/*        (0:1){ ORDER BY (1:100)@word[by] } */



/*        SAMPLE[sample] */
/*        (1:1){ FIRST[first] @int(1:)[count] */
/*             | LAST[last]   @int(1:)[count] */
/*             } */
/*        (1:1){ SELECT[select]              (1:100)@word[columns] */
/*             | SELECT[select] *[wildcard] */
/*             } */
/*        FROM[f]    (1:)@word[query] */
/*        (0:1){ ORDER BY (1:100)@word[by] } */



/*        SAMPLE[sample] @int(1:)[count] */
/*        (1:1){ UP TO       @int(0:100)[upto]     EVERY @int(1:)[every] */
/*             | UP TO       @int(0:100)[upto] */
/*             | STARTING AT @int(0:100)[starting] EVERY @int(1:)[every] */
/*             | STARTING AT @int(0:100)[starting] */
/*             | CENTER   AT @int(0:100)[center]   EVERY @int(1:)[every] */
/*             | CENTER   AT @int(0:100)[center] */
/*             | FROM @int(0:100)[from] TO @int(0:100)[to] */
/*             } */
/*        (1:1){ SELECT[select]              (1:100)@word[columns] */
/*             | SELECT[select] *[wildcard] */
/*             } */
/*        FROM[f]    (1:)@word[query] */
/*        (0:1){ ORDER BY (1:100)@word[by] } */




	s_copy(synval + 5600, "SAMPLE[sample] @int(1:)[count] (1:1){ SELECT["
		"select] (1:100)@word[columns] | SELECT[select] *[wildcard] }"
		" FROM[f] (1:)@word[query] (0:2){ ORDER BY (1:100)@word[by] |"
		" WHERE (1:)#word[where] } ", (ftnlen)700, (ftnlen)191);
	s_copy(synval + 6300, "SAMPLE[sample] (1:1){ FIRST[first] @int(1:)[c"
		"ount] | LAST[last] @int(1:)[count] } (1:1){ SELECT[select] ("
		"1:100)@word[columns] | SELECT[select] *[wildcard] } FROM[f] "
		"(1:)@word[query] (0:2){ ORDER BY (1:100)@word[by] | WHERE (1"
		":)#word[where] } ", (ftnlen)700, (ftnlen)242);
	s_copy(synval + 7000, "SAMPLE[sample] @int(1:)[count] (1:1){ UP TO @"
		"int(0:100)[upto] EVERY @int(1:)[every] | UP TO @int(0:100)[u"
		"pto] | STARTING AT @int(0:100)[starting] EVERY @int(1:)[ever"
		"y] | STARTING AT @int(0:100)[starting] | CENTER AT @int(0:10"
		"0)[center] EVERY @int(1:)[every] | CENTER AT @int(0:100)[cen"
		"ter] | FROM @int(0:100)[from] TO @int(0:100)[to] } (1:1){ SE"
		"LECT[select] (1:100)@word[columns] | SELECT[select] *[wildca"
		"rd] } FROM[f] (1:)@word[query] (0:2){ ORDER BY (1:100)@word["
		"by] | WHERE (1:)#word[where] } ", (ftnlen)700, (ftnlen)496);
	for (i__ = 1; i__ <= 5; ++i__) {
	    replch_(synval + ((i__1 = i__ + 5) < 11 && 0 <= i__1 ? i__1 : 
		    s_rnge("synval", i__1, "nspfnd_", (ftnlen)346)) * 700, 
		    "#", bs, synval + ((i__2 = i__ + 5) < 11 && 0 <= i__2 ? 
		    i__2 : s_rnge("synval", i__2, "nspfnd_", (ftnlen)346)) * 
		    700, (ftnlen)700, (ftnlen)1, (ftnlen)1, (ftnlen)700);
	}
	m2ints_(&c__5, synkey, synptr, synval, (ftnlen)32, (ftnlen)700);
    }

/*     See if this command matches a known syntax.  If it doesn't */
/*     there is no point in hanging around. */

    m2chck_(commnd, synkey, synptr, synval, error, commnd_len, (ftnlen)32, (
	    ftnlen)700, error_len);
    if (have_(error, error_len)) {
	chkout_(rname, (ftnlen)6);
	return 0;
    }

/*     Now expand the various parts of the command.  Table */
/*     templates first. Get the portion of the input command */
/*     that corresponds to the "query" portion. */

    last = m2have_("query", (ftnlen)5);
    m2vget_("query", &c__1, &found, &b, &i__, (ftnlen)5);
    m2vget_("query", &last, &found, &i__, &e, (ftnlen)5);
    s_copy(copy, commnd, (ftnlen)1024, commnd_len);
    s_copy(query, commnd + (b - 1), (ftnlen)1024, e - (b - 1));
    namxpn_(query, "TABLE", error, (ftnlen)1024, (ftnlen)5, error_len);
    if (have_(error, error_len)) {
	chkout_(rname, (ftnlen)6);
	return 0;
    }

/*     Put the resolved version of QUERY back into the command. */
/*     Next expand column names. */

    r__ = rtrim_(query, (ftnlen)1024);
    repsub_(commnd, &b, &e, query, commnd, commnd_len, r__, commnd_len);
    namxpn_(commnd, "COLUMN", error, commnd_len, (ftnlen)6, error_len);
    if (have_(error, error_len)) {
	chkout_(rname, (ftnlen)6);
	return 0;
    }

/*     If we ended up changing the command as a result of */
/*     name expansion, we log that in the log file.  Also */
/*     we re-check the syntax of the command.  The syntax */
/*     should still be ok.  But parts of the commmand have */
/*     probably moved so that we need to re-determine where */
/*     all the parts are located. */

    if (s_cmp(copy, commnd, (ftnlen)1024, commnd_len) != 0) {
	nsplog_(commnd, &c_true, commnd_len);
	m2chck_(commnd, synkey, synptr, synval, error, commnd_len, (ftnlen)32,
		 (ftnlen)700, error_len);
	if (have_(error, error_len)) {
	    chkout_(rname, (ftnlen)6);
	    return 0;
	}
    }

/*     If we get here, we have what may pass for a legitimate command. */
/*     We are going to need to construct the list of columns that */
/*     will be output in the report. */

/*     The wildcard case is the easiest.  Just fetch all of the */
/*     column names from the column manager.  If columns have */
/*     actually been specified, we are going check that the columns */
/*     actually exist before going on.  If a column doesn't exist */
/*     we'll try to correct the spelling.  If all that fails, we */
/*     let the user know that a column isn't on the list of known */
/*     columns.  NOTE: we do not check to see if the columns in */
/*     the query are recognized.  It would be friendlier to do */
/*     so, but not in the prototype. */


/*     Next look up the report format since it will decide where */
/*     things are going to go from here. */

    bbgetc_1__("COPY", "FORMAT", &n, format, (ftnlen)4, (ftnlen)6, (ftnlen)32)
	    ;
    if (m2xist_("wildcard", (ftnlen)8)) {
	s_copy(cols, "*", (ftnlen)1024, (ftnlen)1);
	m2vget_("query", &c__1, &found, &b, &e, (ftnlen)5);
	s_copy(query, commnd + (b - 1), (ftnlen)1024, e - (b - 1));
    } else {
	m2geta_("columns", commnd, &found, cols, (ftnlen)7, commnd_len, (
		ftnlen)1024);
	m2vget_("query", &c__1, &found, &b, &e, (ftnlen)5);
	s_copy(query, commnd + (b - 1), (ftnlen)1024, commnd_len - (b - 1));
    }

/*     Issue the query. Keep in mind that IDLIST has some extra */
/*     room at the beginning for "special columns" needed by the */
/*     various reporting functions. */

    clscop_(cols, commnd, &n, &idlist[1], &num, error, (ftnlen)1024, 
	    commnd_len, error_len);
    if (have_(error, error_len)) {
	chkout_(rname, (ftnlen)6);
	return 0;
    }
    if (num == 0) {
	nspwln_(" ", (ftnlen)1);
	nspwln_("  There were no matches to the supplied query.", (ftnlen)46);
	nspwln_(" ", (ftnlen)1);
	chkout_(rname, (ftnlen)6);
	return 0;
    }

/*     Determine what kind of query we have.  Is it a subsample, */
/*     summarize or straigh selection. */

    sample = m2xist_("sample", (ftnlen)6);
    summrz = m2xist_("summarize", (ftnlen)9);
    select = ! (sample || summrz);
    if (sample) {

/*        If it is a subsample, we have a lot of options to check. */
/*        All subsamples require the user to supply a number of */
/*        items to choose from the selection match. */

	m2geti_("count", commnd, &found, &count, (ftnlen)5, commnd_len);
	if (m2xist_("last", (ftnlen)4)) {

/*           In this case we show the last count items. */

	    every = 1;
/* Computing MAX */
	    i__1 = num - count + 1;
	    from = max(i__1,1);
	    to = num;
	} else if (m2xist_("first", (ftnlen)5)) {

/*           Here we show the first count. */

	    every = 1;
	    from = 1;
	    to = min(count,num);
	} else if (m2xist_("from", (ftnlen)4)) {

/*           In this case we use the percentiles specified */
/*           by from and to determine the bounds and compute */
/*           the skip factor using the count supplied. */

	    m2geti_("from", commnd, &found, &from, (ftnlen)4, commnd_len);
	    m2geti_("to", commnd, &found, &to, (ftnlen)2, commnd_len);
	    from = (num - 1) * from / 100 + 1;
	    to = (num - 1) * to / 100 + 1;
/* Computing MAX */
	    i__1 = 1, i__2 = (to - from) / count;
	    every = max(i__1,i__2);
	} else if (m2xist_("upto", (ftnlen)4)) {

/*           In this case, we have want to show some subset */
/*           up to a specified percentile. */

	    every = 1;
	    m2geti_("upto", commnd, &found, &to, (ftnlen)4, commnd_len);
	    m2geti_("every", commnd, &found, &every, (ftnlen)5, commnd_len);
	    to = (num - 1) * to / 100 + 1;
/* Computing MAX */
	    i__1 = 1, i__2 = to - (count - 1) * every;
	    from = max(i__1,i__2);
	} else if (m2xist_("starting", (ftnlen)8)) {

/*           Here we want to show samples starting at a specified */
/*           percentile. */

	    every = 1;
	    m2geti_("starting", commnd, &found, &from, (ftnlen)8, commnd_len);
	    m2geti_("every", commnd, &found, &every, (ftnlen)5, commnd_len);
	    from = (num - 1) * from / 100 + 1;
/* Computing MIN */
	    i__1 = num, i__2 = from + (count - 1) * every;
	    to = min(i__1,i__2);
	} else if (m2xist_("center", (ftnlen)6)) {

/*           In this case we want to show samples centered about */
/*           a user specified percentile. */

	    every = 1;
	    m2geti_("center", commnd, &found, &center, (ftnlen)6, commnd_len);
	    m2geti_("every", commnd, &found, &every, (ftnlen)5, commnd_len);
	    center = (num - 1) * center / 100 + 1;
	    lradus = (count - 1) / 2;
	    uradus = count - lradus;
	    from = center - lradus * every;
	    to = center + uradus * every;
	    from = max(1,from);
	    to = min(num,to);
	} else {

/*           This is the simplest to specify.  Start at the beginning */
/*           and go to the end skipping just enough in between to */
/*           get the number of samples the user requested. */

/* Computing MAX */
	    i__1 = 1, i__2 = num / count;
	    every = max(i__1,i__2);
	    from = 1;
	    to = every * (count - 1) + 1;
	}
    } else if (select) {

/*        This is a straight selection.  We need to see if we */
/*        are going to exceed the deluge warning limit. */

	bbgeti_1__("COPY", "REPORTLIMIT", &k, &thshld, (ftnlen)4, (ftnlen)11);

/*        And until we have other information, we are going to */
/*        just assume we are printing everything. */

	from = 1;
	to = num;
	every = 1;
    }
    if (num > thshld && select) {

/*        We are going to give the user the option to turn this */
/*        selection request into a SAMPLE DELUGE LIMIT SELECT ... */
/*        request. */

	s_copy(messge, "There were # matches to the supplied query. The thre"
		"shold for automatic display is #. ", (ftnlen)800, (ftnlen)86);
	repmi_(messge, "#", &num, messge, (ftnlen)800, (ftnlen)1, (ftnlen)800)
		;
	repmi_(messge, "#", &thshld, messge, (ftnlen)800, (ftnlen)1, (ftnlen)
		800);
	nspgst_("LOG", lstat, (ftnlen)3);
	nspioh_("LOG", (ftnlen)3);
	nspgst_("SAVE", sstat, (ftnlen)4);
	nspioh_("SAVE", (ftnlen)4);
	nspmrg_(style, (ftnlen)80);
	nicepr_1__(messge, style, (S_fp)nspwln_, (ftnlen)800, (ftnlen)80);
	s_copy(title, "Do you want to display all, some or none?", (ftnlen)80,
		 (ftnlen)41);
	tindnt = 2;
	nopt = 3;
	oindnt = 5;
	*(unsigned char *)&optnam[0] = 'A';
	s_copy(opttxt, "All.  Display all requested items", (ftnlen)80, (
		ftnlen)33);
	*(unsigned char *)&optnam[1] = 'S';
	s_copy(opttxt + 80, "Some. Display a sub-sample of #.", (ftnlen)80, (
		ftnlen)32);
	*(unsigned char *)&optnam[2] = 'N';
	s_copy(opttxt + 160, "None. Discard this query.", (ftnlen)80, (ftnlen)
		25);
	repmi_(opttxt + 80, "#", &thshld, opttxt + 80, (ftnlen)80, (ftnlen)1, 
		(ftnlen)80);
	if (! batch_()) {
	    getopt_2__(title, &tindnt, &nopt, optnam, opttxt, &oindnt, &
		    option, (ftnlen)80, (ftnlen)1, (ftnlen)80);
	    disply = option != 3;
	    sample = option == 2;
	    select = option == 1;
	} else {
	    disply = TRUE_;
	    sample = FALSE_;
	    select = TRUE_;
	}
	if (sample) {
/* Computing MAX */
	    i__1 = 1, i__2 = num / thshld;
	    every = max(i__1,i__2);
	    from = 1;
	    to = every * (thshld - 1) + 1;
	}
	nsppst_("LOG", lstat, (ftnlen)3);
	nsppst_("SAVE", sstat, (ftnlen)4);
	if (! disply) {
	    suffix_("No matching items were displayed. ", &c__1, messge, (
		    ftnlen)34, (ftnlen)800);
	    nspgst_("SCREEN", sstat, (ftnlen)6);
	    nspioh_("SCREEN", (ftnlen)6);
	    nspwln_(" ", (ftnlen)1);
	    nspmrg_(style, (ftnlen)80);
	    nicepr_1__(messge, style, (S_fp)nspwln_, (ftnlen)800, (ftnlen)80);
	    nsppst_("SCREEN", sstat, (ftnlen)6);
	    chkout_(rname, (ftnlen)6);
	    return 0;
	}
    }
    if (s_cmp(format, "DELIMITED", (ftnlen)32, (ftnlen)9) == 0) {
	nspdel_(idlist, &n, &from, &to, &every);
    } else if (s_cmp(format, "DELIMITED PRESERVED", (ftnlen)32, (ftnlen)19) ==
	     0) {
	nspdel_(idlist, &n, &from, &to, &every);
    } else if (s_cmp(format, "FLAGGED", (ftnlen)32, (ftnlen)7) == 0) {
	nspflg_(idlist, &n, &from, &to, &every, format, (ftnlen)32);
    } else if (s_cmp(format, "FLAGGED PRESERVED", (ftnlen)32, (ftnlen)17) == 
	    0) {
	nspflg_(idlist, &n, &from, &to, &every, format, (ftnlen)32);
    } else if (s_cmp(format, "VERBATIM", (ftnlen)32, (ftnlen)8) == 0) {
	nspvrb_(idlist, &n, &from, &to, &every);
    } else {
	nsptab_(idlist, &n, &from, &to, &every, format, (ftnlen)32);
    }
    chkout_(rname, (ftnlen)6);
    return 0;
} /* nspfnd_ */

