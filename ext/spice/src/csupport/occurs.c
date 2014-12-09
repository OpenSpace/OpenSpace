/* occurs.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure OCCURS ( Count occurrences of a substring in a string ) */
integer occurs_(char *str, char *sub, ftnlen str_len, ftnlen sub_len)
{
    /* System generated locals */
    integer ret_val, i__1;

    /* Builtin functions */
    integer i_len(char *, ftnlen), s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer lsub, lstr, i__;

/* $ Abstract */

/*     Count the number of times that a substring occurs within */
/*     a character string. */

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

/*     STRING */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STR        I   Character string. */
/*     C          I   Target substring. */

/* $ Detailed_Input */

/*     STR         is an arbitrary character string. */

/*     SUB         is an arbitrary character string. */

/* $ Detailed_Output */

/*     The function returns the number of occurrences of the substring */
/*     within the string. */

/* $ Exceptions. */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     Typically, this would be used to count the number of times */
/*     that a single character occurs within a string: for example, */
/*     to determine whether the number of left parentheses in an */
/*     expression matches the number of right parentheses. */

/*     The occurrences found by OCCURS are independent: that is, */
/*     the number of occurrences of 'XXX' in 'XXXXXXXX' is two, */
/*     and not six. */

/* $ Examples */

/*     The following code fragment checks to make sure that the */
/*     number of left parentheses in an expression matches the number */
/*     of right delimiters in the same expression. */

/*       IF ( OCCURS ( EXPR, '(' ) - OCCURS ( EXPR, ')' ) .NE. 0 ) THEN */
/*         WRITE (6,*) 'Parenthesis mismatch.' */
/*       END IF */

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


/*     Version B1.0.0, 29-APR-1988 (WLT) (IMU) */

/* -& */

/*     Local variables */

    lstr = i_len(str, str_len);
    lsub = i_len(sub, sub_len);
    i__ = 0;
    ret_val = 0;
    while(i__ <= lstr - lsub) {
	i__1 = i__;
	if (s_cmp(str + i__1, sub, i__ + lsub - i__1, sub_len) == 0) {
	    ++ret_val;
	    i__ += lsub;
	} else {
	    ++i__;
	}
    }
    return ret_val;
} /* occurs_ */

