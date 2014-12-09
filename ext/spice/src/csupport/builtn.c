/* builtn.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__5 = 5;
static integer c__2 = 2;
static integer c__1 = 1;

/* $Procedure      BUILTN ( Built in Commands ) */
/* Subroutine */ int builtn_0_(int n__, char *commnd, logical *hit, char *
	error, ftnlen commnd_len, ftnlen error_len)
{
    /* Initialized data */

    static logical first = TRUE_;
    static logical dosav = TRUE_;
    static logical dodisc = TRUE_;
    static logical doedit = TRUE_;
    static logical dosym = TRUE_;
    static logical doenv = TRUE_;
    static struct {
	char fill_1[480];
	char e_2[400];
	} equiv_22 = { {0}, "SET[set]   EDITOR[editor] (1:)@word[rest]      "
		"                                 SHOW[show] SYMBOL[symbol] @"
		"word[def]                                            SHOW[sh"
		"ow] ENVIRONMENT[env]                                        "
		"             SAVE[save] TO  @word[rest]                     "
		"                                 DISCARD[discard]           "
		"                                                     " };

#define synval ((char *)&equiv_22)

    static char spcial[8*2] = "        " "?       ";

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), i_indx(char *, char *, 
	    ftnlen, ftnlen);

    /* Local variables */
    extern logical have_(char *, ftnlen);
    static integer rest, e, i__, l;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static char names[32*3];
    static logical found;
    extern integer ltrim_(char *, ftnlen), rtrim_(char *, ftnlen);
    static char myerr[512*2];
    extern /* Subroutine */ int m2chck_(char *, char *, integer *, char *, 
	    char *, ftnlen, ftnlen, ftnlen, ftnlen), m2getc_(char *, char *, 
	    logical *, char *, ftnlen, ftnlen, ftnlen), m2vget_(char *, 
	    integer *, logical *, integer *, integer *, ftnlen), m2ints_(
	    integer *, char *, integer *, char *, ftnlen, ftnlen);
    extern logical m2xist_(char *, ftnlen);
    extern /* Subroutine */ int gtecho_(char *, ftnlen);
    extern integer isrchc_(char *, integer *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int getedt_(char *, ftnlen), setedt_(char *, 
	    ftnlen);
    extern integer touchi_(integer *);
    extern /* Subroutine */ int nspioc_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    static char values[512*3];
    static integer nitems;
    extern /* Subroutine */ int flgrpt_(integer *, char *, char *, S_fp, 
	    ftnlen, ftnlen), nsppfl_(char *, char *, ftnlen, ftnlen);
    static char templt[80];
    extern /* Subroutine */ int nspsav_(char *, char *, ftnlen, ftnlen), 
	    nspgst_(char *, logical *, ftnlen), nspwln_(char *, ftnlen);
    static char synkey[32*11];
    static logical status[3];
    extern /* Subroutine */ int shosym_(char *, ftnlen);
    static integer synptr[11];

/* $ Abstract */

/*     This routine handles the normal commands that every */
/*     command line based program will support if you */
/*     use the command loop software. */

/* $ Required_Reading */

/*      None. */

/* $ Keywords */

/*       Command Loop */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      COMMND     I   A command */
/*      HIT        O   Indicates if the command was a built in command */
/*      ERROR      O   Indicates any problems that occurred. */

/* $ Detailed_Input */

/*     COMMND      is a command that is to be processed to see if it */
/*                 is one of the command loop built in commands. */

/* $ Detailed_Output */

/*     HIT         is a logical variable.  If the input command is */
/*                 recognized and acted on, HIT is returned as .TRUE. */
/*                 Otherwise it is returned as .FALSE. */

/*     ERROR       is blank unless a built in command is recognized */
/*                 and causes an error to be triggered.  In the later */
/*                 case ERROR will contain the diagnostics associated */
/*                 with the error. */

/* $ Parameters */

/*      None. */

/* $ Files */

/*      None. */

/* $ Exceptions */

/*     Error free. */

/*     1) If a problem is detected, it is diagnosed and returned */
/*        in the variable ERROR */

/* $ Particulars */

/*     This routine handles the "built in" commands that are */
/*     automatically available with every command loop routine these */
/*     are: */

/*        SET  EDITOR (1:)@word */
/*        SHOW SYMBOL @word */
/*        SHOW ENVIRONMENT */
/*        SAVE TO @word */
/*        DISCARD */

/*     These built in functions can be overridden (turned off) through */
/*     the companion entry point BUILTO */

/* $ Examples */

/*     See the routine CMLOOP */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*      W.L. Taber      (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 22-APR-1997 (WLT) */

/*        Declares NSPWLN as external */

/* -    SPICELIB Version 1.0.0, 5-DEC-1995 (WLT) */


/* -& */
/* $ Index_Entries */

/*     Command Loop Built in Commands. */

/* -& */

/*     Spicelib functions */


/*     Error handling interface routines. */


/*     META/2 Functions */


/*     Inspekt External Routines */


/*     Variables needed for syntax declarations. */


/*     The following are for special commands that will not be */
/*     processed by BUILTN. */


/*     Other Local Variables */


/*     Save everything */

    /* Parameter adjustments */
    if (error) {
	}

    /* Function Body */
    switch(n__) {
	case 1: goto L_builto;
	}

    chkin_("BUILTN", (ftnlen)6);
    if (first) {
	first = FALSE_;
	i__ = 0;
	i__ = touchi_(&i__);
	m2ints_(&c__5, synkey, synptr, synval, (ftnlen)32, (ftnlen)80);
    }
    l = ltrim_(commnd, commnd_len);
    rest = rtrim_(commnd, commnd_len) + 1;
    if (isrchc_(commnd + (l - 1), &c__2, spcial, rest - (l - 1), (ftnlen)8) > 
	    0) {
	chkout_("BUILTN", (ftnlen)6);
	return 0;
    }

/*     There are no errors yet. */

    s_copy(error, " ", error_len, (ftnlen)1);
    s_copy(error + error_len, " ", error_len, (ftnlen)1);
    *hit = FALSE_;

/*     Check the input command to see if it is recognizable */

    m2chck_(commnd, synkey, synptr, synval, myerr, commnd_len, (ftnlen)32, (
	    ftnlen)80, (ftnlen)512);
    if (s_cmp(myerr, " ", (ftnlen)512, (ftnlen)1) != 0) {
	chkout_("BUILTN", (ftnlen)6);
	return 0;
    }
    if (m2xist_("set", (ftnlen)3) && doedit) {
	m2vget_("rest", &c__1, &found, &rest, &e, (ftnlen)4);
	setedt_(commnd + (rest - 1), commnd_len - (rest - 1));
	*hit = TRUE_;
    } else if (m2xist_("symbol", (ftnlen)6) && dosym) {
	m2getc_("def", commnd, &found, templt, (ftnlen)3, commnd_len, (ftnlen)
		80);
	shosym_(templt, (ftnlen)80);
	*hit = TRUE_;
    } else if (m2xist_("env", (ftnlen)3) && doenv) {
	nitems = 3;
	s_copy(names, "Editor", (ftnlen)32, (ftnlen)6);
	s_copy(names + 32, "Echoing Commands", (ftnlen)32, (ftnlen)16);
	s_copy(names + 64, "Screen Output File", (ftnlen)32, (ftnlen)18);
	getedt_(values, (ftnlen)512);
	gtecho_(values + 512, (ftnlen)512);
	nspgst_("SAVE", status, (ftnlen)4);
	if (status[0] && status[1] && ! status[2]) {
	    nsppfl_("SAVE", values + 1024, (ftnlen)4, (ftnlen)512);
	} else {
	    s_copy(values + 1024, "No Current Screen Save File", (ftnlen)512, 
		    (ftnlen)27);
	}
	nspwln_(" ", (ftnlen)1);
	nspwln_("Current Environment", (ftnlen)19);
	nspwln_(" ", (ftnlen)1);
	flgrpt_(&nitems, names, values, (S_fp)nspwln_, (ftnlen)32, (ftnlen)
		512);
	nspwln_(" ", (ftnlen)1);
	*hit = TRUE_;
    } else if (m2xist_("save", (ftnlen)4) && dosav) {
	m2vget_("rest", &c__1, &found, &rest, &e, (ftnlen)4);
	nspsav_(commnd + (rest - 1), error, commnd_len - (rest - 1), 
		error_len);
	*hit = TRUE_;
    } else if (m2xist_("discard", (ftnlen)7) && dodisc) {
	nspioc_("SAVE", (ftnlen)4);
	*hit = TRUE_;
    }
    found = have_(error, error_len);
    chkout_("BUILTN", (ftnlen)6);
    return 0;
/* $Procedure      BUILTO ( Built in commands off ) */

L_builto:
/* $ Abstract */

/*    Turn off built-in command loop commands. */

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

/*     COMMAND LOOP */


/* $ Declarations */

/*     CHARACTER*(*)         COMMND */

/* $ Brief_I/O */
/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     COMMND     I   A list of commands to turn off. */

/* $ Detailed_Input */

/*     COMMND     is a list of words that describes which built-in */
/*                commands to disable.  The words and commands */
/*                they turn off are: */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1) No errors are detected. */

/* $ Particulars */

/*     This routine allows you to turn off selected built in commands */
/*     commands available through command loop programs. */

/* $ Examples */

/*     Suppose you want to turn off the SHOW ENVIRONMENT and */
/*     SET EDITOR commands. */

/*     Do this: */

/*     COMMAND = 'EDITOR ENVIRONMENT' */


/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*       W.L. Taber      (JPL) */

/* $ Literature_References */

/*       None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 5-DEC-1995 (WLT) */


/* -& */
/* $ Index_Entries */

/*     Disable built in command loop commmands */


/* -& */

/*     We just look at command to see which of the built in */
/*     command should be disabled. */

    dosav = i_indx(commnd, "SAVE", commnd_len, (ftnlen)4) == 0;
    doenv = i_indx(commnd, "ENVIRONMENT", commnd_len, (ftnlen)11) == 0;
    doedit = i_indx(commnd, "EDITOR", commnd_len, (ftnlen)6) == 0;
    dosym = i_indx(commnd, "SYMBOL", commnd_len, (ftnlen)6) == 0;
    dodisc = i_indx(commnd, "DISCARD", commnd_len, (ftnlen)7) == 0;
    return 0;
} /* builtn_ */

#undef synval


/* Subroutine */ int builtn_(char *commnd, logical *hit, char *error, ftnlen 
	commnd_len, ftnlen error_len)
{
    return builtn_0_(0, commnd, hit, error, commnd_len, error_len);
    }

/* Subroutine */ int builto_(char *commnd, ftnlen commnd_len)
{
    return builtn_0_(1, commnd, (logical *)0, (char *)0, commnd_len, (ftnint)
	    0);
    }

