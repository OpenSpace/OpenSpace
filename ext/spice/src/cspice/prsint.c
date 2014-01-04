/* prsint.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure   PRSINT   ( Parse integer with error checking ) */
/* Subroutine */ int prsint_(char *string, integer *intval, ftnlen string_len)
{
    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), nparsi_(char *, integer *, char *, integer *, ftnlen, 
	    ftnlen), chkout_(char *, ftnlen);
    char errmsg[320];
    extern /* Subroutine */ int setmsg_(char *, ftnlen);
    integer ptr;

/* $ Abstract */

/*     Parse a string as an integer, encapsulating error handling. */

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

/*     INTEGER */
/*     PARSING */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   String representing an integer. */
/*     INTVAL     O   Integer value obtained by parsing STRING. */

/* $ Detailed_Input */

/*     STRING         is a string representing an integer.  Any string */
/*                    acceptable to the SPICELIB routine NPARSI is */
/*                    allowed. */

/* $ Detailed_Output */

/*     INTVAL         is the integer obtained by parsing STRING. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the input string cannot be parsed, the error */
/*        SPICE(NOTANINTEGER) is signalled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The purpose of this routine is to enable safe parsing of integers */
/*     without the necessity of in-line error checking.  This routine is */
/*     based on the SPICELIB routine NPARSI. */

/* $ Examples */

/*     See the routine NPARSI for an examples of allowed strings. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman       (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 22-JUL-1997 (NJB) */

/* -& */
/* $ Index_Entries */

/*     parse integer with encapsulated error handling */

/* -& */

/*     Local parameters */


/*     Local variables */


/*     Use discovery check-in. */

    nparsi_(string, intval, errmsg, &ptr, string_len, (ftnlen)320);
    if (s_cmp(errmsg, " ", (ftnlen)320, (ftnlen)1) != 0) {
	chkin_("PRSINT", (ftnlen)6);
	setmsg_(errmsg, (ftnlen)320);
	sigerr_("SPICE(NOTANINTEGER)", (ftnlen)19);
	chkout_("PRSINT", (ftnlen)6);
	return 0;
    }
    return 0;
} /* prsint_ */

