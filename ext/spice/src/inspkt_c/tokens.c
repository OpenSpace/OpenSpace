/* tokens.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      TOKENS ( Find tokens in SUBTeX input ) */
/* Subroutine */ int tokens_(char *action, char *source, integer *n, char *
	token, integer *length, ftnlen action_len, ftnlen source_len, ftnlen 
	token_len)
{
    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer i_len(char *, ftnlen);

    /* Local variables */
    static integer line;
    char what[5];
    extern integer cpos_(char *, char *, integer *, ftnlen, ftnlen);
    integer begin;
    extern /* Subroutine */ int chkin_(char *, ftnlen), ucase_(char *, char *,
	     ftnlen, ftnlen);
    extern integer ncpos_(char *, char *, integer *, ftnlen, ftnlen);
    integer maxlen, wrdlen;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int cutstr_(char *, integer *, integer *, char *, 
	    integer *, integer *, ftnlen, ftnlen);
    integer end;
    static integer col;

/* $ Abstract */

/*     Find the next token in one or more lines of SUBTeX input. */

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

/*     ACTION      determines whether the search begins at the first */
/*                 character of the first line ('NEW') or just after */
/*                 the end of the latest token found ('NEXT'). */

/*     SOURCE      are one or more of SUBTeX source lines. */

/*     N           is the number of source lines. */

/* $ Detailed_Output */

/*     TOKEN       is a SUBTeX token. Currently, tokens are defined */
/*                 as words (in the SPICELIB sense), although a word */
/*                 may be broken into more than one token if its */
/*                 length exceeds LEN ( TOKEN ). Words are broken */
/*                 at delimiters recognized by the SPICELIB routine */
/*                 CUTSTR. */

/*     LENGTH      is the length of the token. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     ACTION     I   'NEW' or 'NEXT'. */
/*     SOURCE     I   SUBTeX source lines. */
/*     N          I   Number of source lines. */
/*     TOKEN      O   SUBTeX token. */
/*     LENGTH     O   Length of token. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) If ACTION is not recognized, the error 'SUBTeX(BADTOKENACTION)' */
/*        is signalled. */

/* $ Particulars */

/*     This routine allows several input lines to be processed as */
/*     a single line, with no breaks. */

/* $ Examples */

/*     Let SOURCE contain the following lines. */

/*        '@chapter In Which Pooh Goes' */
/*        'Visiting and @tbr Gets Into' */
/*        'a Tight Place' */

/*     Then the following code fragment */

/*        CHARACTER*132      TOKEN */
/*          . */
/*          . */

/*        CALL TOKENS ( 'NEW', SOURCE, 3, TOKEN, L ) */

/*        DO WHILE ( TOKEN .NE. ' ' ) */
/*           WRITE (6,*) TOKEN, L */

/*           CALL TOKENS ( 'NEXT', SOURCE, 3, TOKEN, L ) */
/*        END DO */

/*     produces the following output. */

/*        @chapter       8 */
/*        In             2 */
/*        Which          5 */
/*        Pooh           4 */
/*        Goes           4 */
/*        Visiting       8 */
/*        and            3 */
/*        @tbr           4 */
/*        Gets           4 */
/*        Into           4 */
/*        a              1 */
/*        Tight          5 */
/*        Place          5 */

/*     If the declaration of TOKEN is changed to */

/*        CHARACTER*6        TOKEN */

/*     produces the following output. */

/*        @chapt         6 */
/*        er             2 */
/*        In             2 */
/*        Which          5 */
/*        Pooh           4 */
/*        Goes           4 */
/*        Visiti         6 */
/*        ng             2 */
/*        and            3 */
/*        @tbr           4 */
/*        Gets           4 */
/*        Into           4 */
/*        a              1 */
/*        Tight          5 */
/*        Place          5 */

/* $ Restrictions */

/*        None. */

/* $ Literature_References */

/* $Include SUBTeX.REFS */

/* $ Author_and_Institution */

/*     I.M. Underwood (JPL) */

/* $ Version */

/*     Beta Version 1.0.1, 22-APR-1997 (WLT) */

/*       replaced backslashes '\' with ats '@'  (only affects comments). */

/*     Beta Version 1.0.0, 11-JUN-1988 (IMU) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Saved variables */


/*     Standard SPICE error handling */

    if (return_()) {
	return 0;
    } else {
	chkin_("TOKENS", (ftnlen)6);
    }

/*     Shake or bake? All that changes is the starting point. */

    ucase_(action, what, action_len, (ftnlen)5);
    if (s_cmp(what, "NEW", (ftnlen)5, (ftnlen)3) == 0) {
	line = 1;
	col = 1;
    } else if (s_cmp(what, "NEXT", (ftnlen)5, (ftnlen)4) != 0) {
	sigerr_("SUBTeX(BADTOKENACTION)", (ftnlen)22);
	chkout_("TOKENS", (ftnlen)6);
	return 0;
    } else if (line > *n) {
	s_copy(token, " ", token_len, (ftnlen)1);
	*length = 0;
	chkout_("TOKENS", (ftnlen)6);
	return 0;
    }

/*     Anything left in the current line? */

    begin = ncpos_(source + (line - 1) * source_len, " ", &col, source_len, (
	    ftnlen)1);

/*     If not, try the next one (or two, or three...). */

    while(begin == 0) {
	++line;
	col = 1;
	if (line <= *n) {
	    begin = ncpos_(source + (line - 1) * source_len, " ", &col, 
		    source_len, (ftnlen)1);
	} else {
	    s_copy(token, " ", token_len, (ftnlen)1);
	    *length = 0;
	    chkout_("TOKENS", (ftnlen)6);
	    return 0;
	}
    }

/*     We must have found something, or we wouldn't be here. */

    end = cpos_(source + (line - 1) * source_len, " ", &begin, source_len, (
	    ftnlen)1) - 1;
    if (end < 0) {
	end = i_len(source + (line - 1) * source_len, source_len);
    }

/*     We may have to break the word to fit within the space allotted */
/*     for the token. */

    maxlen = i_len(token, token_len);
    wrdlen = end - begin + 1;
    if (maxlen < wrdlen) {
	cutstr_(source + (line - 1) * source_len, &begin, &maxlen, " ", &
		begin, &end, source_len, (ftnlen)1);
    }
    s_copy(token, source + ((line - 1) * source_len + (begin - 1)), token_len,
	     end - (begin - 1));
    *length = end - begin + 1;

/*     Next time, pick up where we left off this time. */

    if (end == i_len(source + (line - 1) * source_len, source_len)) {
	++line;
	col = 1;
    } else {
	col = end + 1;
    }
    chkout_("TOKENS", (ftnlen)6);
    return 0;
} /* tokens_ */

