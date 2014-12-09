/* bgroup_1.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure BGROUP_1 ( Determine if groups are balanced ) */
integer bgroup_1__(char *string, char *lgr, char *rgr, char *sgr, ftnlen 
	string_len, ftnlen lgr_len, ftnlen rgr_len, ftnlen sgr_len)
{
    /* System generated locals */
    integer ret_val;

    /* Builtin functions */
    integer i_len(char *, ftnlen), s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer nest, i__, l, r__, s;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer lgoff, lglen, rgoff, sgoff, rglen, sglen, minlr, minsl, minrs;
    logical super;
    integer length;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Determine the extent to which a string with grouping characters */
/*     or strings is balanced. */

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
/*     SGR        I   A string that marks the end of all groups. */

/*     The function returns the depth of the last character of the string */
/*     or minus the position of the first negatively nested character. */

/* $ Detailed_Input */

/*     STRING     Any character string. */

/*     LGR        A character or string of characters that marks the */
/*                start of a group. Typically this will be a left */
/*                parenthesis, left square bracket, or left brace. */

/*     RGR        A character or string of characters that marks the */
/*                end of a group. Usually this will be a right */
/*                parenthesis, right square bracket, or right brace. */

/*     SGR        A character or string of characters that is used as a */
/*                super grouper.  If SGR is a blank string, it will be */
/*                assumed that no super grouper symbols are in the */
/*                input string. */

/* $ Detailed_Output */

/*     BGROUP_1   If the depth of characters in the string never becomes */
/*                negative, BGROUP_1 will be returned as the depth of the */
/*                last character of the input string.  I.E. if the string */
/*                is balanced BGROUP_1 will be zero, if not balanced (and */
/*                the depth is not negative for any character of the */
/*                string) it will be the minus the number of right */
/*                groupers that need to be appended to the string to */
/*                balance it. */

/*                If the depth ever becomes negative, BGROUP_1 will be */
/*                returned as the the index of the first */
/*                character in the string of negative depth. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*      1) If the super grouper symbol is not blank and any of the */
/*         grouping strings are initial substrings of one of the others */
/*         the error 'SPICE(NONDISTINCTPAIR)' will be signalled. */

/*      2) If the super grouper symbol is blank and either of the left */
/*         or right groupers is an initial substring of the other, */
/*         the error 'SPICE(NONDISTINCTPAIR)' will be signalled. */

/* $ Particulars */

/*     One symbol begins a group, another distinct symbol ends it. */
/*     The super grouper symbol ends all current groups. */

/*     Associated with each character of the string is a depth number. */
/*     The depth of a token is determined via the following */
/*     proceedure. */

/*        Initially assume that each character has depth zero. */

/*        Read the string from left to right. */

/*        If a left grouper is encountered */

/*           the depth is incremented by one and all characters of the */
/*           left grouper are assigned this depth. */

/*        If a right grouper is encountered */

/*           the depth is decremented by one and all characters of the */
/*           left grouper are assigned this depth. */

/*        If a super grouper is encountered */

/*           the depth is set to zero, all characters of the super */
/*           grouper are given the depth zero. */

/*        Otherwise */

/*           The character is given a depth equal to the last depth. */

/*     For example, suppose that the left and right groupers are '(' */
/*     and ')' respectively and that ']' is the super grouper symbol. */
/*     Then the depths of each character of the string below is given */
/*     immediately beneath it. */

/*              1 + 2 * ( (3+3)*4 +(2+(3*4 + 3 ] + 27 */
/*              00000000                       000000 */
/*                      11    11111 */
/*                        2222     222 */
/*                                    333333333 */

/*     This routine determines the depth of the last character of the */
/*     input string (provided the depth never bocomes negative. */
/*     If the depth should become negative, he value returned is minus */
/*     the index of the first character of the string having negative */
/*     depth.  By using this routine, one can repair bad input string */
/*     before they are processed by the group locating routines. */

/*     There are three routines available for locating the delimited */
/*     groups of a string.  They are: */

/*        FGROUP_1 --- Find the first simple group of a string. */
/*        DGROUP_1 --- Find the deepest group of a string. */
/*        NGROUP_1 --- Find the kth group of depth n. */

/*     Related routines are */

/*        SGROUP_1 --- Resolve super groupers */
/*        BGROUP_1 --- Determine how balanced a string is. */

/*     This routine determines how balanced a string is. */

/* $ Examples */

/*     The tables below lists a collection of sample input strings and */
/*     output values that should be expected for BGROUP. */

/*     E X A M P L E     I N P U T S    A N D    O U T P U T S */

/*     This first example illustrates how the routine works when */
/*     standard grouping symbols are used. */

/*           LGR     = '(' */
/*           RGR     = ')' */
/*           SGR     = ' ' */

/*           STRING                                    BGROUP */
/*           ==================================        ====== */
/*          'A + ( B + C )*A + ( D + E         '           1 */
/*          '1 + 2*( 3 + 4*( 5 + 6*( 7 + 8*( 9 '           4 */
/*          '1 + 2*( 3 + 4*( 5 + 6*( 7 )))     '           0 */
/*          '1 + 2*) 3 + 4*( 5                 '          -7 */
/*                 - */



/*           LGR     = '(' */
/*           RGR     = ')' */
/*           SGR     = ']' */

/*           STRING                                    BGROUP */
/*           ==================================        ====== */
/*          'A + ( B + C )*A + ( D + E         '           1 */
/*          'A + ( B + C )*A + ( D + E    ]    '           0 */
/*          '1 + 2*( 3 + 4*( 5 + 6*( 7 + 8*( 9]'           0 */
/*          '1 + 2*( 3 + 4*( 5 + 6*( 7] )))     '        -28 */
/*                                      - */
/*          '1 + 2*] 3 + 4*( 5                 '           1 */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */
/*     I.M. Underwood (JPL) */

/* $ Version */

/*     Beta Version 1.1.0, 28-Dec-1994 (WLT) */

/*        Gave BGROUP_1 an initial value of zero so that the function */
/*        will have a value when it returns even if an error is */
/*        signalled. */

/*     Beta Version 1.0.0, 18-OCT-1988 (WLT) (IMU) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */

/* %&END_DECLARATIONS */

/*     We assume to start with that the string is balanced. */

    ret_val = 0;

/*     Standard SPICE error handling. */

    if (return_()) {
	return ret_val;
    } else {
	chkin_("BGROUP_1", (ftnlen)8);
    }

/*     Get the lengths of the left and right groupers as well as */
/*     the input string. */

    length = i_len(string, string_len);
    lglen = i_len(lgr, lgr_len);
    rglen = i_len(rgr, rgr_len);
    sglen = i_len(sgr, sgr_len);
    lgoff = lglen - 1;
    rgoff = rglen - 1;
    sgoff = sglen - 1;
    minlr = min(lglen,rglen);
    minrs = min(sglen,rglen);
    minsl = min(sglen,lglen);

/*     Check for dumb errors */

    if (s_cmp(sgr, " ", sgr_len, (ftnlen)1) == 0) {
	super = FALSE_;
	if (s_cmp(rgr, lgr, rgr_len, minlr) == 0 || s_cmp(lgr, rgr, lgr_len, 
		minlr) == 0) {
	    sigerr_("SPICE(NONDISTINCTPAIR)", (ftnlen)22);
	    chkout_("BGROUP_1", (ftnlen)8);
	    return ret_val;
	}
    } else {
	super = TRUE_;
	if (s_cmp(rgr, lgr, rgr_len, minlr) == 0 || s_cmp(lgr, rgr, lgr_len, 
		minlr) == 0 || s_cmp(rgr, sgr, rgr_len, minrs) == 0 || s_cmp(
		sgr, rgr, sgr_len, minrs) == 0 || s_cmp(lgr, sgr, lgr_len, 
		minsl) == 0 || s_cmp(sgr, lgr, sgr_len, minsl) == 0) {
	    sigerr_("SPICE(NONDISTINCTPAIR)", (ftnlen)22);
	    chkout_("BGROUP_1", (ftnlen)8);
	    return ret_val;
	}
    }
    nest = 0;
    i__ = 1;
    while(i__ <= length) {

/*        Figure out where the ends of the groupers might appear in */
/*        the input string. */

	r__ = i__ + rgoff;
	l = i__ + lgoff;
	s = i__ + sgoff;
	r__ = min(r__,length);
	l = min(l,length);
	s = min(s,length);

/*        If this is a right grouper, we might be done. */

	if (s_cmp(string + (i__ - 1), rgr, r__ - (i__ - 1), rgr_len) == 0) {
	    if (nest == 0) {
		ret_val = i__;
		chkout_("BGROUP_1", (ftnlen)8);
		return ret_val;
	    }
	    ++nest;
	    i__ = r__ + 1;
	} else if (s_cmp(string + (i__ - 1), lgr, l - (i__ - 1), lgr_len) == 
		0) {
	    --nest;
	    i__ = l + 1;
	} else if (super && s_cmp(string + (i__ - 1), sgr, s - (i__ - 1), 
		sgr_len) == 0) {
	    nest = 0;
	    i__ = l + 1;
	} else {
	    ++i__;
	}
    }
    ret_val = nest;
    chkout_("BGROUP_1", (ftnlen)8);
    return ret_val;
} /* bgroup_1__ */

