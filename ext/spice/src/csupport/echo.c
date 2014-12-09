/* echo.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__2 = 2;
static integer c__3 = 3;
static logical c_true = TRUE_;

/* $Procedure      ECHO ( Echo the translation of a string ) */
/* Subroutine */ int echo_0_(int n__, char *string, char *transl, ftnlen 
	string_len, ftnlen transl_len)
{
    /* Initialized data */

    static logical doit = FALSE_;
    static logical first = TRUE_;

    /* System generated locals */
    address a__1[3], a__2[2];
    integer i__1[3], i__2[2];

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen),
	     s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static char lead[3], hide[80], flag__[3], seen[80], dont[32];
    static logical wipe, stat[3];
    static char delim[1];
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen), 
	    nthwd_(char *, integer *, char *, integer *, ftnlen, ftnlen);
    static char style[80], cdelim[1];
    extern /* Subroutine */ int getdel_(char *, ftnlen);
    static char repeat[32], scndwd[32], thrdwd[32];
    extern /* Subroutine */ int prefix_(char *, integer *, char *, ftnlen, 
	    ftnlen), nspioh_(char *, ftnlen), nsplog_(char *, logical *, 
	    ftnlen), trnlat_(char *, char *, ftnlen, ftnlen);
    static char hstyle[80], frstwd[32];
    extern /* Subroutine */ int nspmrg_(char *, ftnlen), nspgls_(char *, char 
	    *, char *, ftnlen, ftnlen, ftnlen), nsplgs_(char *, char *, char *
	    , ftnlen, ftnlen, ftnlen), nspgst_(char *, logical *, ftnlen);
    extern /* Subroutine */ int nspwln_();
    extern /* Subroutine */ int nsppst_(char *, logical *, ftnlen);
    static integer loc;
    extern /* Subroutine */ int nicepr_1__(char *, char *, U_fp, ftnlen, 
	    ftnlen);

/* $ Abstract */

/*    Echo a string if echoing is enabled and a string has been */
/*    translated from its original value. */

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

/*     Command Loop */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   is a string */
/*     TRANSL     I   is string after some kind of processing */

/* $ Detailed_Input */

/*     STRING     is a string.  The intent is that this is some string */
/*                that the user has specified as a command to a program */
/*                and that may be subject to some kind of preprocessing */
/*                such as symbol resolution. */

/*     TRANSL     is the string that results from some user's action on */
/*                the input STRING. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Particulars */

/*     This is a utility routine for the command loop system. */

/*     If as the result of preprocessing a command, some modificactions */
/*     are created it is sometimes helpful to see the result */
/*     of these translations. */

/*     If the echoing is enabled (via the entry point DOECHO) and */
/*     TRANSL is not the same as STRING.  The translation will */
/*     be echoed to the user's output device and to the user's log */
/*     file. */

/*     This routine has 3 companion entry points. */

/*     DOECHO  ---  enables echoing of commands. */
/*     NOECHO  ---  disables echoing of commands. */
/*     GTECHO  ---  returns 'YES' if echoing is enabled 'NO' */
/*                  otherwise. */

/*     By default echoing is disabled. */

/* $ Examples */

/*     Suppose that as a result of symbol resolution the */
/*     command */

/*        "DOIT" */

/*     becomes */

/*          SELECT A, B, C, FROM TABLE WHERE A < B AND B < C */
/*          AND C < A ORDER BY A B C */


/*     If echoing has been enabled the text below will be sent */
/*     to the user's screen and log file: */

/*     ;;; SELECT A, B, C, FROM TABLE WHERE A < B AND B < C AND */
/*     ;   C < A ORDER BY A B C ' */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*       W.L. Taber      (JPL) */

/* $ Literature_References */

/*       None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 28-JUL-1995 (WLT) */


/* -& */
/* $ Index_Entries */

/*     Echo translated commands. */

/* -& */
    switch(n__) {
	case 1: goto L_doecho;
	case 2: goto L_noecho;
	case 3: goto L_gtecho;
	}

    if (first) {

/*        Find out what the words for NO and ECHO are */
/*        in the current language. */

	first = FALSE_;
	trnlat_("DONT", dont, (ftnlen)4, (ftnlen)32);
	trnlat_("ECHO", repeat, (ftnlen)4, (ftnlen)32);
    }
    nthwd_(transl, &c__1, frstwd, &loc, transl_len, (ftnlen)32);
    nthwd_(transl, &c__2, scndwd, &loc, transl_len, (ftnlen)32);
    nthwd_(transl, &c__3, thrdwd, &loc, transl_len, (ftnlen)32);
    ucase_(frstwd, frstwd, (ftnlen)32, (ftnlen)32);
    ucase_(scndwd, scndwd, (ftnlen)32, (ftnlen)32);
    ucase_(thrdwd, thrdwd, (ftnlen)32, (ftnlen)32);
    if (s_cmp(frstwd, repeat, (ftnlen)32, (ftnlen)32) == 0 && s_cmp(scndwd, 
	    " ", (ftnlen)32, (ftnlen)1) == 0) {
	wipe = TRUE_;
	doit = TRUE_;
    } else if (s_cmp(frstwd, dont, (ftnlen)32, (ftnlen)32) == 0 && s_cmp(
	    scndwd, repeat, (ftnlen)32, (ftnlen)32) == 0 && s_cmp(thrdwd, 
	    " ", (ftnlen)32, (ftnlen)1) == 0) {
	wipe = TRUE_;
	doit = FALSE_;
    } else {
	wipe = FALSE_;
    }
    if (doit) {
	if (s_cmp(string, transl, string_len, transl_len) != 0) {

/*           Get the current margins and the delimiter. */

	    nspmrg_(style, (ftnlen)80);
	    getdel_(delim, (ftnlen)1);

/*           Create the NICEIO style string it will be of the form */

/*              LEFT 1 RIGHT margin FLAG ;;; LEADER  ; */

/*           (provided of course that ';' is the command */

/* Writing concatenation */
	    i__1[0] = 1, a__1[0] = delim;
	    i__1[1] = 1, a__1[1] = delim;
	    i__1[2] = 1, a__1[2] = delim;
	    s_cat(flag__, a__1, i__1, &c__3, (ftnlen)3);
/* Writing concatenation */
	    i__2[0] = 1, a__2[0] = delim;
	    i__2[1] = 2, a__2[1] = "++";
	    s_cat(lead, a__2, i__2, &c__2, (ftnlen)3);
	    prefix_(lead, &c__1, style, (ftnlen)3, (ftnlen)80);
	    prefix_("LEADER ", &c__1, style, (ftnlen)7, (ftnlen)80);
	    prefix_(flag__, &c__1, style, (ftnlen)3, (ftnlen)80);
	    prefix_("FLAG", &c__1, style, (ftnlen)4, (ftnlen)80);

/*           Get the current status of the "log" port and */
/*           for the moment inhibit writing to that port. */

	    nspgst_("LOG", stat, (ftnlen)3);
	    nspioh_("LOG", (ftnlen)3);

/*           Display the translated string. */

	    nicepr_1__(transl, style, (U_fp)nspwln_, transl_len, (ftnlen)80);

/*           Now re-establish the status of the log port. */

	    nsppst_("LOG", stat, (ftnlen)3);

/*           Send the translated string to the log file and */
/*           do it so that it is a comment in the log file. */
/*           Note that we use a special logging style for */
/*           echoing the symbol translation. */

	    s_copy(hstyle, "LEFT 1 RIGHT 78 ", (ftnlen)80, (ftnlen)16);
	    prefix_(lead, &c__1, hstyle, (ftnlen)3, (ftnlen)80);
	    prefix_("LEADER ", &c__1, hstyle, (ftnlen)7, (ftnlen)80);
	    prefix_(flag__, &c__1, hstyle, (ftnlen)3, (ftnlen)80);
	    prefix_("FLAG", &c__1, hstyle, (ftnlen)4, (ftnlen)80);
	    nspgls_(seen, hide, cdelim, (ftnlen)80, (ftnlen)80, (ftnlen)1);
	    nsplgs_(seen, hstyle, cdelim, (ftnlen)80, (ftnlen)80, (ftnlen)1);
	    nsplog_(transl, &c_true, transl_len);
	    nsplgs_(seen, hide, cdelim, (ftnlen)80, (ftnlen)80, (ftnlen)1);
	}
    }
    if (wipe) {
	s_copy(transl, " ", transl_len, (ftnlen)1);
    }
    return 0;

/*     The following entry points allow you to */

/*        1) Enable echoing of translations */
/*        2) Disable echoing of translations */
/*        3) Find out the current status of echoing. */

/*     Since the code in each case is trivial, we aren't */
/*     going to set up those big old nasty NAIF headers. */
/*     (What a rebel!) */


L_doecho:
    doit = TRUE_;
    return 0;

L_noecho:
    doit = FALSE_;
    return 0;

L_gtecho:
    if (doit) {
	s_copy(string, "ENABLED", string_len, (ftnlen)7);
    } else {
	s_copy(string, "DISABLED", string_len, (ftnlen)8);
    }
    return 0;
} /* echo_ */

/* Subroutine */ int echo_(char *string, char *transl, ftnlen string_len, 
	ftnlen transl_len)
{
    return echo_0_(0, string, transl, string_len, transl_len);
    }

/* Subroutine */ int doecho_(void)
{
    return echo_0_(1, (char *)0, (char *)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int noecho_(void)
{
    return echo_0_(2, (char *)0, (char *)0, (ftnint)0, (ftnint)0);
    }

/* Subroutine */ int gtecho_(char *string, ftnlen string_len)
{
    return echo_0_(3, string, (char *)0, string_len, (ftnint)0);
    }

