/* inspkn.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__9 = 9;
static integer c__2 = 2;
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

/* Subroutine */ int inspkn_(char *commnd, char *error, ftnlen commnd_len, 
	ftnlen error_len)
{
    /* Initialized data */

    static logical first = TRUE_;
    static struct {
	char fill_1[480];
	char e_2[720];
	} equiv_14 = { {0}, "SET[set]        (1:)#word[rest]                "
		"                                 SHOW[show]      (1:)#word[r"
		"est]                                                 LOAD[lo"
		"ad]      (1:)#word[rest]                                    "
		"             UNLOAD[unload]  (1:)#word[rest]                "
		"                                 HELP[help]                 "
		"                                                     HELP[he"
		"lp]      (1:)#word[rest]                                    "
		"             SELECT[find]    (1:)#word[rest]                "
		"                                 SAMPLE[find]    (1:)#word[r"
		"est]                                                 TRACEOF"
		"F[traceoff]                                                 "
		"             " };

#define synval ((char *)&equiv_14)

    static char spcial[8*2] = "        " "?       ";
    static logical off = FALSE_;

    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_wsle(void);

    /* Local variables */
    extern logical have_(char *, ftnlen);
    static integer rest, e, i__, l;
    static logical found;
    extern /* Subroutine */ int nspld_(char *, char *, ftnlen, ftnlen);
    extern integer ltrim_(char *, ftnlen), rtrim_(char *, ftnlen);
    extern /* Subroutine */ int m2chck_(char *, char *, integer *, char *, 
	    char *, ftnlen, ftnlen, ftnlen, ftnlen), m2vget_(char *, integer *
	    , logical *, integer *, integer *, ftnlen), m2ints_(integer *, 
	    char *, integer *, char *, ftnlen, ftnlen);
    static char bs[1];
    extern logical m2xist_(char *, ftnlen);
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int replch_(char *, char *, char *, char *, 
	    ftnlen, ftnlen, ftnlen, ftnlen), trcoff_(void), nspfnd_(char *, 
	    char *, ftnlen, ftnlen), prefix_(char *, integer *, char *, 
	    ftnlen, ftnlen), nsphlp_(char *, char *, ftnlen, ftnlen), nspuld_(
	    char *, char *, ftnlen, ftnlen), nspsho_(char *, char *, ftnlen, 
	    ftnlen), nspset_(char *, char *, ftnlen, ftnlen);
    static char synkey[32*15];
    static integer synptr[15];

    /* Fortran I/O blocks */
    static cilist io___13 = { 0, 6, 0, 0, 0 };


/*     Nov 21, 1995 (WLT) */

/*     Removed SAVE and DISCARD commands as these are handled */
/*     by the generic Command loop code. */

/*     Oct 18, 1995 (WLT) */

/*     Added the ability to turn tracing off. */

/*     Aug 21, 1995 (WLT) */

/*     Modified the syntax of HELP to allow for multiple word help */
/*     requests. */

/*     This routine handles the delegation of tasks within the */
/*     the INSPEKT program.  It makes a preliminary check of the */
/*     syntax to see if it remotely resembles a recognizable */
/*     command.  It then passes the command (or the untested */
/*     portion of it on to a routine that is set up to deal */
/*     with the details of the command. */

/*     Spicelib functions */


/*     Error handling interface routines. */


/*     META/2 Functions */


/*     Variables needed for syntax declarations. */


/*     The following are for special commands that will not be */
/*     processed by INSPKTN. */


/*     Other Local Variables */


/*     Save everything */


/*     On the first pass set up the collection of recognized */
/*     syntax statements. */

    if (first) {
	first = FALSE_;
	*(unsigned char *)bs = '@';
	for (i__ = 1; i__ <= 9; ++i__) {
	    replch_(synval + ((i__1 = i__ + 5) < 15 && 0 <= i__1 ? i__1 : 
		    s_rnge("synval", i__1, "inspkn_", (ftnlen)145)) * 80, 
		    "#", bs, synval + ((i__2 = i__ + 5) < 15 && 0 <= i__2 ? 
		    i__2 : s_rnge("synval", i__2, "inspkn_", (ftnlen)145)) * 
		    80, (ftnlen)80, (ftnlen)1, (ftnlen)1, (ftnlen)80);
	}
	m2ints_(&c__9, synkey, synptr, synval, (ftnlen)32, (ftnlen)80);
    }

/*     Special commands are stopped here.  Right now there are */
/*     only two such commands: '?' and ' '.  Note, the error values */
/*     are NOT reset. */

    l = ltrim_(commnd, commnd_len);
    rest = rtrim_(commnd, commnd_len) + 1;
    if (isrchc_(commnd + (l - 1), &c__2, spcial, rest - (l - 1), (ftnlen)8) > 
	    0) {
	return 0;
    }

/*     There are no errors yet. */

    s_copy(error, " ", error_len, (ftnlen)1);
    s_copy(error + error_len, " ", error_len, (ftnlen)1);

/*     Check the input command to see if it is recognizable */

    m2chck_(commnd, synkey, synptr, synval, error, commnd_len, (ftnlen)32, (
	    ftnlen)80, error_len);
    if (have_(error, error_len)) {
	prefix_("INSPKN:", &c__1, error, (ftnlen)7, error_len);
	return 0;
    }

/*     If we get this far, we have a potentially legitimate */
/*     command.  We simply pass the command to someone responsible. */

    m2vget_("rest", &c__1, &found, &rest, &e, (ftnlen)4);
    if (m2xist_("set", (ftnlen)3)) {
	nspset_(commnd + (rest - 1), error, commnd_len - (rest - 1), 
		error_len);
    } else if (m2xist_("show", (ftnlen)4)) {
	nspsho_(commnd + (rest - 1), error, commnd_len - (rest - 1), 
		error_len);
    } else if (m2xist_("load", (ftnlen)4)) {
	nspld_(commnd + (rest - 1), error, commnd_len - (rest - 1), error_len)
		;
    } else if (m2xist_("unload", (ftnlen)6)) {
	nspuld_(commnd + (rest - 1), error, commnd_len - (rest - 1), 
		error_len);
    } else if (m2xist_("help", (ftnlen)4)) {
	nsphlp_(commnd + (rest - 1), error, commnd_len - (rest - 1), 
		error_len);
    } else if (m2xist_("find", (ftnlen)4)) {
	nspfnd_(commnd, error, commnd_len, error_len);
    } else if (m2xist_("traceoff", (ftnlen)8)) {
	if (! off) {
	    off = TRUE_;
	    trcoff_();
	}
    } else {
	s_wsle(&io___13);
	do_lio(&c__9, &c__1, "Unrecognized or unimplemented", (ftnlen)29);
	e_wsle();
	s_copy(error, "The input command is not yet supported.", error_len, (
		ftnlen)39);
    }
    found = have_(error, error_len);
    return 0;
} /* inspkn_ */

#undef synval


