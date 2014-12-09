/* dpstrp.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      DPSTRP ( DP Number to Character, With Plus ) */
/* Subroutine */ int dpstrp_(doublereal *x, integer *sigdig, char *string, 
	ftnlen string_len)
{
    extern /* Subroutine */ int dpstre_(doublereal *, integer *, char *, 
	    ftnlen);

/* $ Abstract */

/*     This routine is a wrapper around DPSTRE. It passes all inputs */
/*     directly to DPSTRE and does only one thing to its output -- */
/*     replaces the first character of the output string with '+' */
/*     for positive numbers. */

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

/*     CHARACTER */
/*     CONVERSION */
/*     PARSING */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     X          I   A double precision number */
/*     SIGDIG     I   The number of significant digits placed in output */
/*     STRING     O   A character string representation of X */

/* $ Detailed_Input */

/*     See DPSTRE. */

/* $ Detailed_Output */

/*     See DPSTRE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     See DPSTRE. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     See DPSTRE. */

/* $ Examples */

/*     See DPSTRE. */

/* $ Restrictions */

/*     See DPSTRE. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov    (JPL) */

/* $ Version */

/* -    FRMDIFF Version 2.0.0, 27-FEB-2012 (BVS) */

/*        Replaced the call to SPICELIB's DPSTR with the call to */
/*        SUPPORT's DPSTRE which does not limit the number of */
/*        significant digits to 14. */

/* -    FRMDIFF Version 1.0.0, 10-SEP-2008 (BVS) */

/* -& */
/* $ Index_Entries */

/*     d.p. number to character */

/* -& */

/*     Call DPSTRE. */

    dpstre_(x, sigdig, string, string_len);

/*     If the first character is blank, replace it with '+'. */

    if (*(unsigned char *)string == ' ') {
	*(unsigned char *)string = '+';
    }

/*     That's all folks. */

    return 0;
} /* dpstrp_ */

