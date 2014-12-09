/* subtex.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      SUBTEX ( Subset TeX ) */
/* Subroutine */ int subtex_(char *buffer, ftnlen buffer_len)
{
    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer card, last;
    extern /* Subroutine */ int proc_(char *, integer *, integer *, ftnlen), 
	    prep_(char *, ftnlen);
    extern integer cardc_(char *, ftnlen);
    extern /* Subroutine */ int chkin_(char *, ftnlen), chunk_(char *, 
	    integer *, integer *, ftnlen);
    integer first;
    extern /* Subroutine */ int expand_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Convert a buffer containing TeX source lines to an equivalent */
/*     buffer containing ASCII text. */

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

/* $ Declarations */
/* $ Author_and_Institution */

/*     I.M. Underwood (JPL) */

/* $ Version */

/*     Beta Version 1.0.0, 11-JUN-1988 (IMU) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling */

    if (return_()) {
	return 0;
    } else {
	chkin_("SUBTEX", (ftnlen)6);
    }

/*     Expand @input commands. */

    expand_(buffer, buffer_len);

/*     Preprocess the original buffer (to remove TABs and other */
/*     odd characters). */

    prep_(buffer, buffer_len);

/*     Chunks are processed sequentially. PROC writes to standard */
/*     output, instead of returning the buffer. */

    card = cardc_(buffer, buffer_len);
    first = 1;
    while(first <= card) {
	chunk_(buffer, &first, &last, buffer_len);
	if (s_cmp(buffer + (first + 5) * buffer_len, " ", buffer_len, (ftnlen)
		1) != 0) {
	    proc_(buffer, &first, &last, buffer_len);
	}
	first = last + 1;
    }
    chkout_("SUBTEX", (ftnlen)6);
    return 0;
} /* subtex_ */

