/* cmstup.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure      CMSTUP ( Command Loop Startup ) */
/* Subroutine */ int cmstup_(void)
{
    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char file[255];
    integer b, e;
    logical havgo;
    integer start;
    logical dobtch, havfil;
    extern /* Subroutine */ int getcml_(char *, ftnlen);
    char commnd[255];
    extern logical setbat_(void);
    char comlin[255];
    extern /* Subroutine */ int fndnwd_(char *, integer *, integer *, integer 
	    *, ftnlen), trnlat_(char *, char *, ftnlen, ftnlen), suffix_(char 
	    *, integer *, char *, ftnlen, ftnlen), putcom_(char *, integer *, 
	    ftnlen);

/* $ Abstract */

/*     This routine performs command loop start ups associated */
/*     with information on the command line when the user */
/*     activated the program. */

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

/*       Command Loop */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      None. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*      None. */

/* $ Files */

/*      None. */

/* $ Exceptions */

/*     Error free. */

/* $ Particulars */

/*     This routine examines the information supplied on the command */
/*     line when a program was started and sets the symbols indicating */
/*     whether or not the program is in batch mode and if appropriate */
/*     sets up to start a command procedure. */

/*     This routine works entirely by side effect. */

/*     Recognized flags are: */

/*     -b                for batch mode */
/*     -start filename   for starting a startup file. */

/*     Unrecognized options are ignored. */

/* $ Examples */

/*     See the command loop documentation */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*      W.L. Taber      (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 20-NOV-1995 (WLT) */


/* -& */
/* $ Index_Entries */

/*     Command loop set up. */

/* -& */

/*     Command loop fucntions */


/*     Below are the various sources from which */
/*     commands might come. */

/*     NONE */
/*     COMBUF */
/*     KEYBRD */
/*     INPFIL */

    getcml_(comlin, (ftnlen)255);
    start = 1;
    havgo = FALSE_;
    dobtch = FALSE_;
    havfil = FALSE_;
    fndnwd_(comlin, &start, &b, &e, (ftnlen)255);
    while(b > 0) {
	if (s_cmp(comlin + (b - 1), "-b", e - (b - 1), (ftnlen)2) == 0) {
	    dobtch = TRUE_;
	} else if (s_cmp(comlin + (b - 1), "-start", e - (b - 1), (ftnlen)6) 
		== 0) {
	    havgo = TRUE_;
	} else if (havgo && ! havfil) {
	    s_copy(file, comlin + (b - 1), (ftnlen)255, e - (b - 1));
	    havfil = TRUE_;
	}
	start = e + 1;
	fndnwd_(comlin, &start, &b, &e, (ftnlen)255);
    }

/*     If we have a batch flag, notify NXTCOM */

    if (dobtch) {
	dobtch = setbat_();
    }
    if (havgo && havfil) {
	trnlat_("START", commnd, (ftnlen)5, (ftnlen)255);
	suffix_(file, &c__1, commnd, (ftnlen)255, (ftnlen)255);
	putcom_(commnd, &c__1, (ftnlen)255);
    }
    return 0;
} /* cmstup_ */

