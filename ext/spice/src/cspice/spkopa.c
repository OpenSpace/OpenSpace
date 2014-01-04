/* spkopa.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      SPKOPA ( SPK open for addition ) */
/* Subroutine */ int spkopa_(char *file, integer *handle, ftnlen file_len)
{
    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char arch[8], type__[8];
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    extern logical failed_(void);
    extern /* Subroutine */ int getfat_(char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen), dafopw_(char *, integer *, ftnlen), sigerr_(char 
	    *, ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen);
    extern logical exists_(char *, ftnlen), return_(void);

/* $ Abstract */

/*    Open an existing SPK file for subsequent write. */

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

/*     SPK */

/* $ Keywords */

/*     SPK */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     FILE       I   The name of an existing SPK file. */
/*     HANDLE     O   A handle attached to the SPK file opened for write. */

/* $ Detailed_Input */

/*     FILE       is the name of an existing SPK file to which */
/*                you wish to append additional SPK segments. */

/* $ Detailed_Output */

/*     HANDLE     is the DAF handle attached to the file required */
/*                by any of the SPK writing routines.  If any exceptions */
/*                arise that prevent opening of the specified file for */
/*                writing, HANDLE will be returned with the value 0. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1)  If the file specified does not exist the error */
/*         'SPICE(FILENOTFOUND)' will be signalled. */

/*     2)  If the file specified is not an SPK file, the error */
/*         'SPICE(FILEISNOTSPK)' will be signalled. */

/*     All other exceptions are determined by routines in the */
/*     call tree of this routine. */

/* $ Particulars */

/*     This file provides an interface for opening existing SPK */
/*     files for the addition of SPK segments.  If you need */
/*     to open an new SPK file for writing, call the routine SPKOPN. */

/* $ Examples */

/*     Suppose you have collected data for a type 05 spk segment and */
/*     wish to place the new segment in an existing SPK file.  The */
/*     code fragment below shows one set of calls that you could perform */
/*     to make the addition.  (Note that you could add segments of */
/*     other data types by replacing the call to SPKW05 with a suitably */
/*     modified call to another SPKWxx routine.) */

/*     We assume that the following variables have already been */
/*     assigned the proper values: */

/*        BODY   (integer)  Body code for ephemeris object. */
/*        CENTER (integer)  Body code for the center of motion */
/*                          of the body. */
/*        FRAME  (string)   The reference frame of the states. */
/*        FIRST  (d.p.)     First valid time for which states can be */
/*                          computed in seconds past 2000. */
/*        LAST   (d.p.)     Last valid time for which states can */
/*                          be computed in seconds past 2000. */
/*        GM     (d.p.)     Gravitational mass of central body. */
/*        N      (integer)  Number of states and epochs. */
/*        STATES (d.p.)     Array of states (x,y,z,dx,dy,dz). */
/*        EPOCHS (d.p.)     Array of epochs (seconds past 2000.) */
/*        SEGID  (string)   Segment identifier */


/*     Begin by opening the file. */

/*     CALL SPKOPA ( FILE, HANDLE ) */

/*     Now add the collected data as a new segment. */

/*     CALL SPKW05 ( HANDLE, BODY,  CENTER, FRAME,  FIRST, LAST, SEGID, */
/*    .              GM,    N,      STATES, EPOCHS      ) */

/*     Finally, close the file. */

/*     CALL SPKCLS ( HANDLE ) */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 10-MAR-1999 (WLT) */


/* -& */
/* $ Index_Entries */

/*     Open an existing SPK file for adding segments */

/* -& */

/*     SPICELIB Functions */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SPKOPA", (ftnlen)6);

/*     Until we get a legitimate handle we set HANDLE to zero. */

    *handle = 0;

/*     First make sure the file exists. */

    if (! exists_(file, file_len)) {
	setmsg_("The file '#' is not recognized as an existing file. ", (
		ftnlen)52);
	errch_("#", file, (ftnlen)1, file_len);
	sigerr_("SPICE(FILENOTFOUND)", (ftnlen)19);
	chkout_("SPKOPA", (ftnlen)6);
	return 0;
    }

/*     Next make sure it is an SPK file. */

    getfat_(file, arch, type__, file_len, (ftnlen)8, (ftnlen)8);
    if (failed_()) {
	chkout_("SPKOPA", (ftnlen)6);
	return 0;
    }
    if (s_cmp(arch, "DAF", (ftnlen)8, (ftnlen)3) != 0 || s_cmp(type__, "SPK", 
	    (ftnlen)8, (ftnlen)3) != 0) {
	setmsg_("The file '#' was not an SPK file.  The architecture and typ"
		"e of the file were found to be '#' and '#' respectively. ", (
		ftnlen)116);
	errch_("#", file, (ftnlen)1, file_len);
	errch_("#", arch, (ftnlen)1, (ftnlen)8);
	errch_("#", type__, (ftnlen)1, (ftnlen)8);
	sigerr_("SPICE(FILEISNOTSPK)", (ftnlen)19);
	chkout_("SPKOPA", (ftnlen)6);
	return 0;
    }

/*     That's the limit of the checks performed here.  We let DAFOPW */
/*     handle the remaining checks. */

    dafopw_(file, handle, file_len);
    if (failed_()) {
	*handle = 0;
    }
    chkout_("SPKOPA", (ftnlen)6);
    return 0;
} /* spkopa_ */

