/* logchk.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      LOGCHK ( Log file check ) */
/* Subroutine */ int logchk_(char *defalt, char *usenam, logical *dolog, 
	ftnlen defalt_len, ftnlen usenam_len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char line[900];
    integer b, e, start;
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    extern /* Subroutine */ int getcml_(char *, ftnlen), fndnwd_(char *, 
	    integer *, integer *, integer *, ftnlen);

/* $ Abstract */

/*     Determine whether to use a log file, and if so what name */
/*     pattern to use. */

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

/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     DEFALT     I   Default logfile name pattern */
/*     USENAM     O   Acutal logfile name pattern that will be used. */
/*     DOLOG      O   Flag indicating whether or not to use a log file. */

/* $ Detailed_Input */

/*     DEFALT     is a default pattern to use if nothing is specified */
/*                on the command line. */

/* $ Detailed_Output */

/*     USENAM     is the name to use for the log file or blank */
/*                if the -nolog flag is supplied on the command line. */

/*     DOLOG      is a logical flag that indicates whether or not */
/*                to create a log file. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Particulars */

/*     This is a utility routine for use by the "Command Loop" routines */
/*     so that one can specify a custom name for a log file (or */
/*     specify that no log file be used at all. */

/*     The options examined from the command line are: */

/*       -nolog */
/*       -log <filename> */

/*      This routine does not judge the "fitness" of the name of */
/*      the logfile, if one is specified on the command line.  Checking */
/*      for suitability is left to other portions of the system. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 28-DEC-2001 (WLT) */


/* -& */

/*     Spicelib Functions. */


/*     Until we know otherwise, we set the logname to the default */
/*     value and set action to "use a log file". */

    s_copy(usenam, defalt, usenam_len, defalt_len);
    *dolog = TRUE_;
    start = 1;
    getcml_(line, (ftnlen)900);
    fndnwd_(line, &start, &b, &e, (ftnlen)900);
    while(b > 0) {
	start = e + 1;
	if (eqstr_(line + (b - 1), "-nolog", e - (b - 1), (ftnlen)6)) {
	    s_copy(usenam, " ", usenam_len, (ftnlen)1);
	    *dolog = FALSE_;
	    return 0;
	} else if (eqstr_(line + (b - 1), "-log", e - (b - 1), (ftnlen)4)) {
	    fndnwd_(line, &start, &b, &e, (ftnlen)900);
	    if (e > b) {
		s_copy(usenam, line + (b - 1), usenam_len, e - (b - 1));
	    }
	    return 0;
	}
	fndnwd_(line, &start, &b, &e, (ftnlen)900);
    }
    return 0;
} /* logchk_ */

