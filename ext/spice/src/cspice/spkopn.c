/* spkopn.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__6 = 6;

/* $Procedure      SPKOPN ( SPK, open new file. ) */
/* Subroutine */ int spkopn_(char *name__, char *ifname, integer *ncomch, 
	integer *handle, ftnlen name_len, ftnlen ifname_len)
{
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer ncomr;
    extern logical failed_(void);
    extern /* Subroutine */ int dafonw_(char *, char *, integer *, integer *, 
	    char *, integer *, integer *, ftnlen, ftnlen, ftnlen), chkout_(
	    char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Create a new SPK file, returning the handle of the opened file. */

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
/*     NAME       I   The name of the new SPK file to be created. */
/*     IFNAME     I   The internal filename for the SPK file. */
/*     NCOMCH     I   The number of characters to reserve for comments. */
/*     HANDLE     O   The handle of the opened SPK file. */

/* $ Detailed_Input */

/*     NAME     The name of the new SPK file to be created. */

/*     IFNAME   The internal filename for the SPK file that is being */
/*              created. The internal filename may be up to 60 characters */
/*              long. If you do not have any conventions for tagging your */
/*              files, an internal filename of 'SPK_file' is perfectly */
/*              acceptable. You may also leave it blank if you like. */

/*     NCOMCH   This is the space, measured in characters, to be */
/*              initially set aside for the comment area when a new SPK */
/*              file is opened. The amount of space actually set aside */
/*              may be greater than the amount requested, due to the */
/*              manner in which comment records are allocated in an SPK */
/*              file. However, the amount of space set aside for comments */
/*              will always be at least the amount that was requested. */

/*              The value of NCOMCH should be greater than or equal to */
/*              zero, i.e., 0 <= NCOMCH. A negative value, should one */
/*              occur, will be assumed to be zero. */

/* $ Detailed_Output */

/*     HANDLE   The handle of the opened SPK file. If an error occurs */
/*              when opening the file, the value of this variable should */
/*              not be used, as it will not represent a valid handle. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the value of NCOMCH is negative, a value of zero (0) will */
/*        be used for the number of comment characters to be set aside */
/*        for comments. */

/*     2) If an error occurs while attempting to open a CK file the */
/*        value of HANDLE will not represent a valid file handle. */

/* $ Files */

/*     See NAME and HANDLE. */

/* $ Particulars */

/*     Open a new SPK file, reserving room for comments if requested. */

/* $ Examples */

/*     Suppose that you want to create a new SPK file called 'new.spk' */
/*     that contains a single type 5 SPK segment and has room for at */
/*     least 5000 comment characters. The following code fragment should */
/*     take care of this for you, assuming that all of the variables */
/*     passed to the SPK type 5 segment writer have appropriate values */
/*     and no errors occur. */

/*        NAME   = 'new.spk' */
/*        IFNAME = 'Test SPK file' */

/*        CALL SPKOPN ( NAME, IFNAME, 5000,  HANDLE ) */
/*        CALL SPKW05 ( HANDLE, OBJID, CNTRID, CFRAME, ETBEG, */
/*       .              ETEND, SEGMID, CNTRGM, NSTATE, STATE, */
/*       .              EPOCH                                 ) */
/*        CALL SPKCLS ( HANDLE ) */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman      (JPL) */
/*     K.R. Gehringer    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 09-NOV-2006 (NJB) */

/*        Routine has been upgraded to support comment */
/*        area allocation using NCOMCH. */

/* -    SPICELIB Version 1.0.0, 26-JAN-1995 (KRG) */

/* -& */
/* $ Index_Entries */

/*     open a new spk file */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     DAF ND and NI values for SPK files. */


/*     Length of a DAF comment record, in characters. */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SPKOPN", (ftnlen)6);

/*     Compute the number of comment records that we want to allocate, if */
/*     the number of comment characters requested is greater than zero, */
/*     we always allocate an extra record to account for the end of line */
/*     marks in the comment area. */


    if (*ncomch > 0) {
	ncomr = (*ncomch - 1) / 1000 + 1;
    } else {
	ncomr = 0;
    }

/*     Just do it. All of the error handling is taken care of for us. */

    dafonw_(name__, "SPK", &c__2, &c__6, ifname, &ncomr, handle, name_len, (
	    ftnlen)3, ifname_len);
    if (failed_()) {

/*        If we failed, make sure that HANDLE does not contain a value */
/*        that represents a valid DAF file handle. */

	*handle = 0;
    }
    chkout_("SPKOPN", (ftnlen)6);
    return 0;
} /* spkopn_ */

