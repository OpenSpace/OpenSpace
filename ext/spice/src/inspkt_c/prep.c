/* prep.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      PREP ( Preprocess the input SUBTeX buffer ) */
/* Subroutine */ int prep_(char *buffer, ftnlen buffer_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer card, i__;
    extern integer cardc_(char *, ftnlen);
    extern /* Subroutine */ int chkin_(char *, ftnlen), scardc_(integer *, 
	    char *, ftnlen), replch_(char *, char *, char *, char *, ftnlen, 
	    ftnlen, ftnlen, ftnlen);
    extern logical matchw_(char *, char *, char *, char *, ftnlen, ftnlen, 
	    ftnlen, ftnlen);
    extern /* Subroutine */ int chkout_(char *, ftnlen), astrip_(char *, char 
	    *, char *, char *, ftnlen, ftnlen, ftnlen, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Preprocess a SUBTeX buffer. That is, replace tab characters */
/*     with spaces and remove all other non-printing characters. */
/*     Also remove index lines. */

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

/*     SUBTeX */

/* $ Keywords */

/*     SUBTeX */

/* $ Declarations */
/* $ Detailed_Input */

/*     BUFFER      on input contains lines of SUBTeX source text. */

/* $ Detailed_Output */

/*     BUFFER      on output contains the same source text, with */
/*                 all tab characters replaced by spaces, and with */
/*                 all other non-printing characters removed. */
/*                 Index lines are also removed. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     BUFFER    I/O  Input source, output source. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Particulars */



/* $ Examples */




/* $ Version */

/*     Beta Version 1.0.0, 11-JUN-1988 (IMU) */

/* -& */

/*     Entry points */


/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling */

    if (return_()) {
	return 0;
    } else {
	chkin_("PREP", (ftnlen)4);
    }

/*     Replace tabs first, else they'll be stripped. Replace index */
/*     lines by blank lines. */

    card = cardc_(buffer, buffer_len);
    i__1 = card;
    for (i__ = 1; i__ <= i__1; ++i__) {
	replch_(buffer + (i__ + 5) * buffer_len, "\t", " ", buffer + (i__ + 5)
		 * buffer_len, buffer_len, (ftnlen)1, (ftnlen)1, buffer_len);
	astrip_(buffer + (i__ + 5) * buffer_len, "\000", "\037", buffer + (
		i__ + 5) * buffer_len, buffer_len, (ftnlen)1, (ftnlen)1, 
		buffer_len);
	astrip_(buffer + (i__ + 5) * buffer_len, "\177", "\177", buffer + (
		i__ + 5) * buffer_len, buffer_len, (ftnlen)1, (ftnlen)1, 
		buffer_len);
	if (matchw_(buffer + (i__ + 5) * buffer_len, "@Index*", "*", "%", 
		buffer_len, (ftnlen)7, (ftnlen)1, (ftnlen)1)) {
	    s_copy(buffer + (i__ + 5) * buffer_len, " ", buffer_len, (ftnlen)
		    1);
	}
    }

/*     Remove trailing blank lines. */

    while(s_cmp(buffer + (card + 5) * buffer_len, " ", buffer_len, (ftnlen)1) 
	    == 0) {
	--card;
    }
    scardc_(&card, buffer, buffer_len);
    chkout_("PREP", (ftnlen)4);
    return 0;
} /* prep_ */

