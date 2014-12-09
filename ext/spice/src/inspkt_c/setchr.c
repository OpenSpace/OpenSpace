/* setchr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Subroutine */ int setchr_(char *chr, integer *at, char *string, ftnlen 
	chr_len, ftnlen string_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer i_len(char *, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen), sigerr_(char *, 
	    ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen), 
	    errint_(char *, integer *, ftnlen);
    extern logical return_(void);


/* $ Abstract */

/*     Set a particular location in a string to be a specified */
/*     character. */

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

/*     String */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     CHR        I   character to put in a specified location of string */
/*     AT        I/O   place in string to put CHR */
/*     STRING    I/O  string to be modified by overwriting a character */

/* $ Detailed_Input */

/*     CHR        A character to overwrite a specified  character of */
/*                the input string. */

/*     AT         Location in the input string to  overwrite. */

/*     STRING     String that will have one character modified. */

/* $ Detailed_Output */

/*     AT         Incremented by  1 from its input value. */

/*     STRING     The input string after having set STRING(AT:AT) = CHR */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) If AT is before or after the end of the  string.  The */
/*        error 'SPICE(INDEXOUTOFRANGE)' will be signalled.  The */
/*        string will  not  be modified. */

/* $ Particulars */

/*     This is a "macro" subroutine that encapulates the operations: */

/*      1)  check to make sure AT is in range */
/*      2)  Overwrite STRING(AT:AT) with CHR */
/*      3)  Increment AT by 1. */

/* $ Examples */

/*     Here's how you can use this routine to copy the text from one */
/*     string into another a character at a time..  Variations can be */
/*     made on this example to handle specific tasks based upon the */
/*     value of the  character to be copied.  The example assumes */
/*     that INPUT and OUTPUT occupy  distinct memory. */

/*     GET = 1 */
/*     AT  = 1 */

/*     DO GET = 1, LEN(INPUT) */
/*        CALL SETCHR( INPUT(GET:GET), AT, OUTPUT ) */
/*     END DO */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 28-MAR-2003 (WLT) */


/* -& */
/* $ Index_Entries */

/*     Overwrite a character in a string */

/* -& */
    if (return_()) {
	return 0;
    }
    if (*at < 0 || *at > i_len(string, string_len)) {
	chkin_("SETCHR", (ftnlen)6);
	setmsg_("A request has been made to set the #'th character of a stri"
		"ng.  However the valid range of characters is from 0 to #.", (
		ftnlen)117);
	errint_("#", at, (ftnlen)1);
	i__1 = i_len(string, string_len);
	errint_("#", &i__1, (ftnlen)1);
	sigerr_("SPICE(INDEXOUTOFRANGE)", (ftnlen)22);
	chkout_("SETCHR", (ftnlen)6);
	return 0;
    }
    *(unsigned char *)&string[*at - 1] = *(unsigned char *)chr;
    ++(*at);
    return 0;
} /* setchr_ */

