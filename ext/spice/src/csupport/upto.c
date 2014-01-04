/* upto.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure UPTO ( Up to the next index of a substring ) */
integer upto_(char *string, char *substr, integer *start, ftnlen string_len, 
	ftnlen substr_len)
{
    /* System generated locals */
    integer ret_val;

    /* Builtin functions */
    integer i_len(char *, ftnlen), i_indx(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer b, i__, strlen;

/* $ Abstract */

/*     Return up to (but not including) the index of the next occurrence */
/*     of a substring within a string, after some initial offset. */

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

/*     CHARACTER, PARSING, SEARCH, STRING, TEXT */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   Input string. */
/*     SUBSTR     I   Target substring. */
/*     START      I   Begin searching here. */

/* $ Detailed_Input */

/*     STRING      is an arbitrary input string. */

/*     SUBSTR      is the target substring to be located. */

/*     START       is the location at which to begin searching. That is, */
/*                 the search is confined to STRING(START: ). */

/* $ Detailed_Output */

/*     The function returns one less than the next location of the */
/*     target substring within the string, or the length of the string */
/*     if the substring is not found. */

/* $ Exceptions */

/*     1) If START is greater than the length of the string, the */
/*        function returns zero. */

/*     2) If START is less than one it is treated as if were one. */

/* $ Particulars */

/*     UPTO is used primarily for extracting substrings bounded by */
/*     a delimiter. Because the function returns the length of the */
/*     string when the target substring is not found, the reference */

/*        NEXT = STRING ( START : UPTO ( STRING, SUBSTR, START ) ) */

/*     is always legal. */

/* $ Examples */

/*     The following code fragment extracts (and prints) substrings */
/*     bounded by slash (/) characters. */

/*        BEGIN = 1 */
/*        END   = BEGIN */

/*        DO WHILE ( END .NE. 0 ) */
/*           END   = UPTO ( STR, '/', BEGIN ) */

/*           WRITE (6,*) 'Next token is ', STR(BEGIN:END) */

/*           BEGIN = END + 2 */
/*        END DO */

/*     Notice that the loop ends when BEGIN is greater than the length */
/*     of the string, causing the function to return zero. */

/*     Notice also that the last token in the string is printed whether */
/*     or not the string ends with a slash. */

/*     If STRING is */

/*        'first/second/third/fourth' */

/*     the output from the fragment is */

/*        Next token is first */
/*        Next token is second */
/*        Next token is third */
/*        Next token is fourth */

/*     Contrast this with the following fragment, written using the */
/*     intrinsic function INDEX. */

/*        BEGIN = 1 */
/*        END   = BEGIN */

/*        DO WHILE ( END .NE. 0 ) */
/*           I = INDEX ( STR(BEGIN: ), '/' ) */

/*           IF ( I .GT. 0 ) THEN */
/*              END = BEGIN + I - 1 */
/*           ELSE */
/*              END = LEN ( STR ) */
/*           END IF */

/*           WRITE (6,*) 'Next token is ', STR(BEGIN:END) */

/*           BEGIN = END + 2 */

/*           IF ( BEGIN .GT. LEN ( STR ) ) THEN */
/*              END = 0 */
/*           END IF */
/*        END DO */
/* $ Files */

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


/*     Version B1.0.0, 4-APR-1988, (IMU) (WLT) */

/* -& */

/*     Local variables */


/*     Just like it says in the header. */

    strlen = i_len(string, string_len);
    b = max(1,*start);
    if (b > strlen) {
	ret_val = 0;
    } else {
	i__ = i_indx(string + (b - 1), substr, string_len - (b - 1), 
		substr_len);
	if (i__ > 0) {
	    ret_val = b + i__ - 2;
	} else {
	    ret_val = strlen;
	}
    }
    return ret_val;
} /* upto_ */

