/* spalog.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__6 = 6;

/* $Procedure      SPALOG ( SPACIT, read comments from binary file ) */
/* Subroutine */ int spalog_(char *versn, logical *logfil, integer *loglun, 
	ftnlen versn_len)
{
    /* System generated locals */
    cllist cl__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer f_clos(cllist *);

    /* Local variables */
    char line[255];
    extern /* Subroutine */ int chkin_(char *, ftnlen), repmc_(char *, char *,
	     char *, char *, ftnlen, ftnlen, ftnlen, ftnlen);
    char prmpt[80];
    extern logical failed_(void);
    logical fileok;
    char logfnm[128];
    extern /* Subroutine */ int errfnm_(char *, integer *, ftnlen), sigerr_(
	    char *, ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int writln_(char *, integer *, ftnlen), txtopn_(
	    char *, integer *, ftnlen), getfnm_1__(char *, char *, char *, 
	    logical *, ftnlen, ftnlen, ftnlen);

/* $ Abstract */

/*     SPACIT utility subroutine used to open a log file. This subroutine */
/*     is for use only be the SPACIT program. Use it at your own risk. */

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

/*     None. */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*      LOGFIL    O    Logical flag indicating a log file was opened. */
/*      LOGLUN    O    The logical unit of the log file. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*      LOGFIL   Logical flag indicating a log file was opened. This */
/*               Variable has the value of .TRUE. if a log file is being */
/*               written, and a value of .FALSE. otherwise. */

/*      LOGLUN   The logical unit of the log file. If LOGFIL has the */
/*               value .TRUE. then LOGLUN will be the Fortran logical */
/*               unit of the log file. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     xxx */

/* $ Examples */

/*     xxx */

/* $ Restrictions */

/*     xxx */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     K.R. Gehringer     (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 11-JUL-1995 (KRG) */



/* -& */
/* $ Index_Entries */

/*     spacit convert binary to transfer */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */

/*     Set a value for the logical unit which represents the standard */
/*     output device, commonly a terminal. A value of 6 is widely used, */
/*     but the Fortran standard does not specify a value, so it may be */
/*     different for different Fortran implementations. */


/*     Set a value for the length of an input text line. */


/*     Set a value for the length of a filename. */


/*     Set a length for the prompt. */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SPALOG", (ftnlen)6);
    }

/*     Get the filename. */

    fileok = FALSE_;
    s_copy(prmpt, "   Log file: ", (ftnlen)80, (ftnlen)13);
    getfnm_1__(prmpt, "NEW", logfnm, &fileok, (ftnlen)80, (ftnlen)3, (ftnlen)
	    128);
    if (failed_()) {
	chkout_("SPALOG", (ftnlen)6);
	return 0;
    }
    if (*logfil) {

/*        If a log file has already been opened, then display */
/*        a message to that effect and go get the next */
/*        option. */

	setmsg_("A log file with the name '#' has already been opened. Only "
		"one log file is allowed per SPACIT session.", (ftnlen)102);
	errfnm_("#", loglun, (ftnlen)1);
	sigerr_("SPACIT(LOGFILEISOPEN)", (ftnlen)21);
	chkout_("SPALOG", (ftnlen)6);
	return 0;
    }

/*     Open the SPACIT log file here */

    txtopn_(logfnm, loglun, (ftnlen)128);
    if (failed_()) {
	*logfil = FALSE_;
	chkout_("SPALOG", (ftnlen)6);
	return 0;
    }
    *logfil = TRUE_;
    s_copy(line, "   Opening log file: #", (ftnlen)255, (ftnlen)22);
    repmc_(line, "#", logfnm, line, (ftnlen)255, (ftnlen)1, (ftnlen)128, (
	    ftnlen)255);
    writln_(" ", &c__6, (ftnlen)1);
    writln_(line, &c__6, (ftnlen)255);
    writln_(" ", &c__6, (ftnlen)1);
    s_copy(line, "   The log file '#' was opened successfully.", (ftnlen)255, 
	    (ftnlen)44);
    repmc_(line, "#", logfnm, line, (ftnlen)255, (ftnlen)1, (ftnlen)128, (
	    ftnlen)255);
    writln_(line, &c__6, (ftnlen)255);
    writln_(" ", &c__6, (ftnlen)1);

/*     Now put a short header into the log file. */
/*     Eventually this should contain the date that */
/*     log file was created and possible some other */
/*     things as well. */

    s_copy(line, "Log file for SPACIT Version #.", (ftnlen)255, (ftnlen)30);
    repmc_(line, "#", versn, line, (ftnlen)255, (ftnlen)1, versn_len, (ftnlen)
	    255);
    writln_(" ", loglun, (ftnlen)1);
    writln_(line, loglun, (ftnlen)255);
    writln_(" ", loglun, (ftnlen)1);
    if (failed_()) {
	cl__1.cerr = 0;
	cl__1.cunit = *loglun;
	cl__1.csta = 0;
	f_clos(&cl__1);
	*logfil = FALSE_;
	*loglun = -1;
    }
    chkout_("SPALOG", (ftnlen)6);
    return 0;
} /* spalog_ */

