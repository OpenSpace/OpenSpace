/* getsta.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__14 = 14;
static integer c__6 = 6;

/* $Procedure      GETSTA ( Compute/not-compute states ) */
/* Subroutine */ int getsta_(logical *check, integer *bodid, integer *cenid, 
	char *frame, doublereal *epoch, integer *n, doublereal *state, 
	logical *ok, char *error, integer *erridx, ftnlen frame_len, ftnlen 
	error_len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), repmc_(char *, char *,
	     char *, char *, ftnlen, ftnlen, ftnlen, ftnlen), repmf_(char *, 
	    char *, doublereal *, integer *, char *, char *, ftnlen, ftnlen, 
	    ftnlen, ftnlen), moved_(doublereal *, integer *, doublereal *), 
	    repmi_(char *, char *, integer *, char *, ftnlen, ftnlen, ftnlen),
	     reset_(void);
    extern logical failed_(void);
    static doublereal lt;
    extern /* Subroutine */ int erract_(char *, char *, ftnlen, ftnlen);
    static char savact[80];
    extern /* Subroutine */ int getmsg_(char *, char *, ftnlen, ftnlen);
    static doublereal hstate[6];
    extern /* Subroutine */ int spkgeo_(integer *, doublereal *, char *, 
	    integer *, doublereal *, doublereal *, ftnlen), chkout_(char *, 
	    ftnlen);
    static char longms[1840];
    extern /* Subroutine */ int errprt_(char *, char *, ftnlen, ftnlen);
    static char savrpt[80];
    extern logical return_(void);

/* $ Abstract */

/*     This routine either computes geometric states of a given body */
/*     relative to a given center in a given reference frame at given */
/*     epochs or verifies that they cannot be computed. */

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


/*     Include File:  SPICELIB Error Handling Parameters */

/*        errhnd.inc  Version 2    18-JUN-1997 (WLT) */

/*           The size of the long error message was */
/*           reduced from 25*80 to 23*80 so that it */
/*           will be accepted by the Microsoft Power Station */
/*           FORTRAN compiler which has an upper bound */
/*           of 1900 for the length of a character string. */

/*        errhnd.inc  Version 1    29-JUL-1997 (NJB) */



/*     Maximum length of the long error message: */


/*     Maximum length of the short error message: */


/*     End Include File:  SPICELIB Error Handling Parameters */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     CHECK      I   Run mode: .TRUE. for check, .FALSE. for compute */
/*     BODID      I   Body ID. */
/*     CENID      I   Center ID. */
/*     FRAME      I   Frame name. */
/*     EPOCH      I   Buffer of epochs (ETs). */
/*     N          I   Number of epochs. */
/*     STATE      O   Output state buffer */
/*     OK         O   Success flag */
/*     ERROR      O   Error message */
/*     ERRIDX     O   Index of the epoch at which error occured. */

/* $ Detailed_Input */

/*     CHECK       is the run mode: .TRUE. for check, .FALSE. for */
/*                 compute. */

/*     BODID       is the body ID. */

/*     CENID       is the center ID. */

/*     FRAME       is the frame name. */

/*     EPOCH       is the buffer of epochs (ETs). */

/*     N           is the number of epochs. */

/* $ Detailed_Output */

/*     STATE       is the array of output states of the given body */
/*                 relative to the given center in the given reference */
/*                 frame at each of the epochs. Undefined if routine is */
/*                 run in check mode. */

/*     OK          is the success flag: .TRUE. for success, .FALSE. for */
/*                 failure. */

/*     ERROR       is the error message. Blank if OK is .TRUE. */

/*     ERRIDX      is the index of the epoch from the EPOCH buffer at */
/*                 which error occured. Set to 0 if no error. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     The routine uses error/report mode RETURN/NONE and returns */
/*     success/failure flag and error message as outputs. */

/* $ Files */

/*     All applicable kernels must be loaded prior to calling this */
/*     routine. */

/* $ Particulars */

/*     This routine resets error handling to RETURN and error report to */
/*     NONE and attempts to compute the state of the given body relative */
/*     to the given center in the given reference frame at each of the */
/*     epochs. */

/*     If this routine is called in check mode, it verifies that this */
/*     attempt failes for all epochs. If SPKGEO call succeeded for even */
/*     one point, the routine returns failure status and description of */
/*     the error. */

/*     If this routine is called in compute mode, it verifies that this */
/*     attempt succeeds for all epochs. If yes, it returns with success */
/*     status and STATE buffers filled with valid states. If SPKGEO did */
/*     not succeed for even one point, the routine returns failure */
/*     status and description of the error. */

/*     In either case, before returning the routine resets error handing */
/*     mode and report type to what they were before the call. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     All applicable kernels must loaded prior to calling this routine. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Version */

/* -    Version 1.0.0, 25-MAR-2014 (BVS) */

/* -& */

/*     Local parameters. */


/*     Local variables. */


/*     Save everything to prevent potential memory problems in f2c'ed */
/*     version. */


/*     SPICELIB functions. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("GETSTA", (ftnlen)6);
    }

/*     Save previous error and report modes and reset them to RETURN and */
/*     NONE. */

    erract_("GET", savact, (ftnlen)3, (ftnlen)80);
    erract_("SET", "RETURN", (ftnlen)3, (ftnlen)6);
    errprt_("GET", savrpt, (ftnlen)3, (ftnlen)80);
    errprt_("SET", "NONE", (ftnlen)3, (ftnlen)4);

/*     Loop over epochs and compute rotation for each of them. */

    *ok = TRUE_;
    s_copy(error, " ", error_len, (ftnlen)1);
    *erridx = 0;
    i__ = 1;
    while(i__ <= *n && *ok) {

/*        Try to compute state. */

	spkgeo_(bodid, &epoch[i__ - 1], frame, cenid, hstate, &lt, frame_len);

/*        Did SPKGEO fail or succeed? Are we checking or */
/*        computing? */

	if (*check && ! failed_()) {

/*           Bad: we are checking and it did not fail. Set OK flag to */
/*           failure and put together an error description. */

	    *ok = FALSE_;
	    s_copy(error, "Geometric state of # relative to # in '#' frame c"
		    "ould be computed at ET #", error_len, (ftnlen)73);
	    repmi_(error, "#", bodid, error, error_len, (ftnlen)1, error_len);
	    repmi_(error, "#", cenid, error, error_len, (ftnlen)1, error_len);
	    repmc_(error, "#", frame, error, error_len, (ftnlen)1, frame_len, 
		    error_len);
	    repmf_(error, "#", &epoch[i__ - 1], &c__14, "E", error, error_len,
		     (ftnlen)1, (ftnlen)1, error_len);
	    *erridx = i__;
	} else if (! (*check) && failed_()) {

/*           Bad: we are computing and it failed. Set OK flag to failure */
/*           and put together an error description. Reset error handling. */

	    *ok = FALSE_;
	    s_copy(error, "Geometric state of # relative to # in '#' frame c"
		    "ould not be computed at ET #. SPKGEO error was: #", 
		    error_len, (ftnlen)98);
	    repmi_(error, "#", bodid, error, error_len, (ftnlen)1, error_len);
	    repmi_(error, "#", cenid, error, error_len, (ftnlen)1, error_len);
	    repmc_(error, "#", frame, error, error_len, (ftnlen)1, frame_len, 
		    error_len);
	    repmf_(error, "#", &epoch[i__ - 1], &c__14, "E", error, error_len,
		     (ftnlen)1, (ftnlen)1, error_len);
	    getmsg_("LONG", longms, (ftnlen)4, (ftnlen)1840);
	    repmc_(error, "#", longms, error, error_len, (ftnlen)1, (ftnlen)
		    1840, error_len);
	    *erridx = i__;
	    reset_();
	} else if (*check && failed_()) {

/*           Good: we are checking and it failed. Reset error handling */
/*           and move on to the next epoch. */

	    reset_();
	} else if (! (*check) && ! failed_()) {

/*           Good: we are computing and it did not fail. Buffer this */
/*           state and move on to the next epoch. */

	    moved_(hstate, &c__6, &state[i__ * 6 - 6]);
	}

/*        Move on to the next epoch. */

	++i__;
    }

/*     Restore original error and report modes. */

    erract_("SET", savact, (ftnlen)3, (ftnlen)80);
    errprt_("SET", savrpt, (ftnlen)3, (ftnlen)80);

/*     All done. */

    chkout_("GETSTA", (ftnlen)6);
    return 0;
} /* getsta_ */

