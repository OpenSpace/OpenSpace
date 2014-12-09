/* m2thnq.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure      M2THNQ ( Find a META/2 qualified @then directive ) */
/* Subroutine */ int m2thnq_(char *string, integer *positn, char *label, 
	ftnlen string_len, ftnlen label_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer i_len(char *, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern integer upto_(char *, char *, integer *, ftnlen, ftnlen);
    static integer i__, j;
    extern /* Subroutine */ int fndnwd_(char *, integer *, integer *, integer 
	    *, ftnlen);
    static integer length;

/* $ Abstract */

/*      This utility routine locates a META/2 qualified @then directive */
/*      and returns the position in the string immediately preceeding */
/*      the directive as well as the label portion of the directive. */

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

/*      The META/2 book. */

/* $ Keywords */

/*     PARSING */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   A META/2 language specication string. */
/*     POSITN     O   The position of the last character before @then(%*) */
/*     LABEL      O   The label portion of the @then directive. */

/* $ Detailed_Input */

/*     STRING     A META/2 language specication string. */

/* $ Detailed_Output */

/*     POSITN     The index of the last character before a word */
/*                that begins with '@then('.  If there is no such word */
/*                POSITN is assigned the index of the last character */
/*                of the string. */

/*     LABEL      The label portion of the @then directive. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1)  If there is no qualified @then, POSITN is set to the index of */
/*         the last character of the string and LABEL is set to ' '. */

/* $ Particulars */

/*     This is a utility routine that locates the first character */
/*     before the first occurance of a substring of the form '@then(%*)'. */

/*     It is intended for use only by META/2. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */

/* $ Version */

/* -     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 9, 1994 */


/* -     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of META/2 */
/*         software as of May 3, 1994 */


/*     Beta Version 1.0.0, 18-MAY-1988 (WLT) (IMU) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Get the lengtH of the string. */

    length = i_len(string, string_len);

/*     See if there is a qualified @then. */

    *positn = upto_(string, "@then(", &c__1, string_len, (ftnlen)6);
    if (*positn == length) {
	s_copy(label, " ", label_len, (ftnlen)1);
    } else {
	fndnwd_(string, positn, &i__, &j, string_len);
	if (j <= i__ + 6) {
	    *positn = length;
	    s_copy(label, " ", label_len, (ftnlen)1);
	} else {
	    i__1 = i__ + 5;
	    s_copy(label, string + i__1, label_len, j - 1 - i__1);
	}
    }
    return 0;
} /* m2thnq_ */

