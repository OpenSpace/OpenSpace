/* ekopr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure   EKOPR ( EK, open file for reading ) */
/* Subroutine */ int ekopr_(char *fname, integer *handle, ftnlen fname_len)
{
    extern /* Subroutine */ int zzekpgch_(integer *, char *, ftnlen), chkin_(
	    char *, ftnlen);
    extern logical failed_(void);
    extern /* Subroutine */ int dasopr_(char *, integer *, ftnlen), chkout_(
	    char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Open an existing E-kernel file for reading. */

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

/*     EK */

/* $ Keywords */

/*     EK */
/*     FILES */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     FNAME      I   Name of EK file. */
/*     HANDLE     O   Handle attached to EK file. */

/* $ Detailed_Input */

/*     FNAME          is the name of an existing E-kernel file to be */
/*                    opened for read access. */

/* $ Detailed_Output */

/*     HANDLE         is the EK file handle of the file designated by */
/*                    FNAME.  This handle is used to identify the file */
/*                    to other EK routines. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)  If the indicated file cannot be opened, the error will be */
/*         diagnosed by routines called by this routine.  The new file */
/*         will be deleted. */

/*     2)  If the indicated file has the wrong architecture version, the */
/*         error will be diagnosed by routines called by this routine. */

/*     3)  If an I/O error occurs while reading the indicated file, the */
/*         error will be diagnosed by routines called by this routine. */

/* $ Files */

/*     See the EK Required Reading for a discussion of the EK file */
/*     format. */

/* $ Particulars */

/*     This routine should be used to open an EK file for read access. */
/*     EKs opened for read access may not be modified. */

/*     Opening an EK file with this routine makes the EK accessible to */
/*     the SPICELIB EK readers */

/*        EKRCEC */
/*        EKRCED */
/*        EKRCEI */

/*     all of which expect an EK file handle as an input argument.  These */
/*     readers allow a caller to read individual EK column entries. */

/*     To make an EK available to the EK query system, the file must be */
/*     loaded via EKLEF, rather than by this routine.  See the EK */
/*     Required Reading for further information. */

/* $ Examples */

/*     1)  Open the file MY.EK for read access: */

/*            CALL EKOPR ( 'MY.EK', HANDLE ) */

/* $ Restrictions */

/*     1)  No more than FTSIZE DAS files may be opened simultaneously. */
/*         See DASFM for the value of FTSIZE. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 26-AUG-1995 (NJB) */

/* -& */
/* $ Index_Entries */

/*     open EK for reading */

/* -& */

/*     SPICELIB functions */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("EKOPR", (ftnlen)5);
    }

/*     Open the file as a DAS file. */

    dasopr_(fname, handle, fname_len);
    if (failed_()) {
	chkout_("EKOPR", (ftnlen)5);
	return 0;
    }

/*     Nothing doing unless the architecture is correct.  This file */
/*     should be a paged DAS EK. */

    zzekpgch_(handle, "READ", (ftnlen)4);
    chkout_("EKOPR", (ftnlen)5);
    return 0;
} /* ekopr_ */

