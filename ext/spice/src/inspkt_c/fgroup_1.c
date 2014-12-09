/* fgroup_1.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure FGROUP_1 ( First simple group of a string ) */
/* Subroutine */ int fgroup_1__(char *string, char *lgr, char *rgr, integer *
	beg, integer *end, integer *depth, ftnlen string_len, ftnlen lgr_len, 
	ftnlen rgr_len)
{
    /* Builtin functions */
    integer i_len(char *, ftnlen), s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer nest, i__, l, r__;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer lgoff, lglen, rgoff, rglen, length, minlen;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Return the endpoints of the first simple group of a string. */

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

/*     GROUPING */

/* $ Keywords */

/*     PARSING */
/*     SEARCH */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   Any character string */
/*     LGR        I   A string that marks the start of a group */
/*     RGR        I   A string that marks the end   of a group */
/*     BEG        O   Index of the left  character of first full group */
/*     END        O   Index of the right character of first full group */
/*     DEPTH      O   Nesting depth of the first full group */

/* $ Detailed_Input */

/*     STRING     Any character string. */

/*     LGR        A character or string of characters that marks the */
/*                start of a group. Typically this will be a left */
/*                parenthesis, left square bracket, or left brace. */

/*     RGR        A character or string of characters that marks the */
/*                end of a group. Usually this will be a right */
/*                parenthesis, right square bracket, or right brace. */

/* $ Detailed_Output */

/*     BEG        Index of the left character of the first simple group. */

/*     END        Index of the last character of the first simple group. */

/*     DEPTH      Integer giving the nesting depth of the simple group */
/*                delimited by BEG and END. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) If the input string is not balanced (unpaired groupers), */
/*        prior to the detection of the first simple group, */
/*        the error 'SPICE(UNBALANCEDGROUP)' will be signalled. */
/*        The values of BEG, END and DEPTH will be set to zero. */

/*     2) If no left or right groupers occur in the string, BEG and END */
/*        will point to the first and last characters of the string */
/*        respectively. (The last character has index equal to the */
/*        declared length of the string.)  DEPTH will be set to zero. */

/*     3) If either grouper is an introductory substring of the other */
/*        the error 'SPICE(NONDISTINCTPAIR)' will be signalled. */
/*        The values of BEG, END and DEPTH will be set to zero. */


/* $ Particulars */

/*     One symbol begins a group, another distinct symbol ends it. */

/*     Associated with each character of the string is a depth number. */
/*     The depth of a character is determined via the following */
/*     proceedure. */

/*        Initially assume that each character has depth zero. */

/*        Read the string from left to right. */

/*        If a left grouper is encountered */

/*           the depth is incremented by one and all characters of the */
/*           left grouper are assigned this depth. */

/*        If a right grouper is encountered */

/*           the depth is decremented by one and all characters of the */
/*           right grouper are assigned this depth. */

/*        Otherwise */

/*           The character is given a depth equal to the last depth. */

/*     For example, suppose that the groupers are '(' and ')'.  Then */
/*     the depths of each character of the string below is given */
/*     immediately beneath it. */

/*              1 + 2 * ( (3+3)*4 +(2+(3*4 + 3) ) ) + 27 */
/*              00000000                          000000 */
/*                      11    11111             11 */
/*                        2222     222        22 */
/*                                    33333333 */

/*     The group of depth zero is simply the entire string. A group of */
/*     depth n begins at left grouper of depth n and ends on the */
/*     first right grouper of depth n-1. Taking the above example */
/*     again the groups of depth 2 are underlined. */

/*              1 + 2 * ( (3+3)*4 +(2+(3*4 + 3) ) ) + 27 */
/*                        -----    -------------- */

/*     A simple group, is a group that contains no groups of greater */
/*     depth, i.e. a substring that begins with a left grouper, */
/*     ends with a right grouper, and contains neither type of */
/*     grouper between them.  The simple groups of our example */
/*     are underlined below. */

/*              1 + 2 * ( (3+3)*4 +(2+(3*4 + 3) ) ) + 27 */
/*                        -----       --------- */

/*     There are three routines available for locating the */
/*     groups of a string.  They are: */

/*        FGROUP --- Find the first simple group of a string. */
/*        DGROUP --- Find the deepest group of a string. */
/*        NGROUP --- Find the kth group of depth n. */

/*     Related routines are */

/*        SGROUP --- Resolve super groupers */
/*        BGROUP --- Determine how balanced a group is. */

/*     This routine finds the endpoints of the first simple group of */
/*     a string and its depth. */

/* $ Examples */

/*     The tables below lists a collection of sample input strings. */
/*     The substring STRING(BEG:END) is underlined.  The depth that */
/*     would be return for the nesting level is listed under the column */
/*     DEPTH. */

/*     E X A M P L E     I N P U T S    A N D    O U T P U T S */

/*     This first example illustrates how the routine works when */
/*     standard grouping symbols are used. */

/*           LGR     = '(' */
/*           RGR     = ')' */

/*           STRING                                    DEPTH */
/*           ==============================            ===== */

/*          '( ( 2 + 4 ) * 7 ) + 19        '             2 */
/*             --------- */

/*          '.NOT. ( ( A .OR B ) .AND. C ) '             2 */
/*                   ----------- */

/*          '( ( (( ((( X + Y )            '             7 */
/*                    --------- */

/*          'THIS HAS NO DELIMITERS        '             0 */
/*           ------------------------------ */

/*          'THE MAN ( JIM ) WENT HOME     '             1 */
/*                   ------- */

/*          '( 12*[3 + 5] )                '             1 */
/*           -------------- */



/*     The following example illustrate the need for care when choosing */
/*     left and right groupers of more than one character. */


/*           LGR    = '\beg' */
/*           RGR    = '\end' */

/*           STRING                                    DEPTH */
/*           ==============================            ===== */

/*          'Data: \begin 1 2  3 4 \enddata'              1 */
/*                 -------------------- */

/*          '\begin \beg time \endit \end  '              2 */
/*                  -------------- */



/*     Finally the next example shows that you need to be careful */
/*     about passing trailing blanks attached to the right and left */
/*     groupers. */


/*           LGR    = ' [ ' */
/*           RGR    = '] ' */

/*           STRING                                    DEPTH */
/*           ==============================            ===== */

/*          'A + [B+7]*32                  '             0 */
/*           ------------------------------ */

/*          'A + [ B + 7 ]*32              '       results in error */
/*                                                 SPICE(UNBALANCEDGROUP) */

/*          'A + [ B + 7] *32              '             1 */
/*              ---------- */


/*     =============================================================== */


/*     A    C O D E     E X A M P L E */


/*     The following loop shows how one might use this routine to */
/*     parse an arithmetic expression. */

/*        See if the string is BALANCED */

/*        DO WHILE ( BGROUP( STRING, LGR, RGR, SGR ) .NE. 0 ) */

/*           Attempt to balance the string. */

/*        END DO */

/*        CALL FGROUP ( STRING, '(', ')', BEG, END, LEVEL ) */

/*        DO WHILE ( LEVEL .GT. 0 ) */

/*           Simplyfy the expression from BEG to END */

/*           Insert the simplified expression in place of the previous */
/*           one in STRING. */

/*           CALL FGROUP ( STRING, '(', ')', BEG, END, LEVEL ) */

/*        END DO */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */

/* $ Version */

/*     Beta Version 1.0.0, 24-OCT-1988 (WLT) (IMU) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */

/* %&END_DECLARATIONS */

/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("FGROUP_1", (ftnlen)8);
    }

/*     Get the lengths of the left and right groupers as well as */
/*     the input string. */

    length = i_len(string, string_len);
    lglen = i_len(lgr, lgr_len);
    rglen = i_len(rgr, rgr_len);
    lgoff = lglen - 1;
    rgoff = rglen - 1;
    minlen = min(lglen,rglen);

/*     Check for dumb errors */

    if (s_cmp(rgr, lgr, rgr_len, minlen) == 0 || s_cmp(lgr, rgr, lgr_len, 
	    minlen) == 0) {
	sigerr_("SPICE(NONDISTINCTPAIR)", (ftnlen)22);
	chkout_("FGROUP_1", (ftnlen)8);
	return 0;
    }

/*     Now we have the expected case.  Initialize the items to be used */
/*     for the search loop. */

    nest = 0;
    *beg = 1;
    *end = length;
    i__ = 1;
    while(i__ <= length) {

/*        Figure out where the ends of the groupers might appear in */
/*        the input string. */

	r__ = i__ + rgoff;
	l = i__ + lgoff;
	r__ = min(r__,length);
	l = min(l,length);

/*        If this is a right grouper, we are done one way or the other. */

	if (s_cmp(string + (i__ - 1), rgr, r__ - (i__ - 1), rgr_len) == 0) {
	    if (nest > 0) {
		*end = r__;
		*depth = nest;
	    } else {
		*beg = 0;
		*end = 0;
		*depth = 0;
		sigerr_("SPICE(UNBALANCEDGROUP)", (ftnlen)22);
	    }
	    chkout_("FGROUP_1", (ftnlen)8);
	    return 0;
	} else if (s_cmp(string + (i__ - 1), lgr, l - (i__ - 1), lgr_len) == 
		0) {
	    ++nest;
	    *beg = i__;
	    i__ = l + 1;
	} else {
	    ++i__;
	}
    }

/*     If the routine makes it this far, we had better have a string */
/*     with no groupers, i.e. current nesting zero.  Otherwise we */
/*     have an error. */

    if (nest != 0) {
	*beg = 0;
	*end = 0;
	*depth = 0;
	sigerr_("SPICE(UNBALANCEDGROUP)", (ftnlen)22);
    } else {
	*depth = 0;
	*beg = 1;
	*end = length;
    }
    chkout_("FGROUP_1", (ftnlen)8);
    return 0;
} /* fgroup_1__ */

