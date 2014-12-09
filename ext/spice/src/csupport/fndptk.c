/* fndptk.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      FNDPTK ( Find the previous token in a string ) */
/* Subroutine */ int fndptk_(char *string, char *delims, integer *start, 
	integer *beg, integer *end, ftnlen string_len, ftnlen delims_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer i_len(char *, ftnlen), i_indx(char *, char *, ftnlen, ftnlen), 
	    s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer last, b;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer ncpos_(char *, char *, integer *, ftnlen, ftnlen), cposr_(
	    char *, char *, integer *, ftnlen, ftnlen);
    logical atdelm;
    extern /* Subroutine */ int fndntk_(char *, char *, integer *, integer *, 
	    integer *, ftnlen, ftnlen);
    logical onspce;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern integer ncposr_(char *, char *, integer *, ftnlen, ftnlen);
    extern logical return_(void);
    integer eol;

/* $ Abstract */

/*      Find the previous token in a string delimited by multiple */
/*      delimiters. */

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

/*      None. */

/* $ Keywords */

/*      CHARACTER,  STRING,  PARSING */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      STRING     I    String of items delimited by DELIMS. */
/*      DELIMS     I    Single characters which delimit items. */
/*      START      I    Point to start looking for a token in the string. */
/*      BEG        O    Beginning index of the token. */
/*      END        O    End index of the token. */

/* $ Detailed_Input */

/*      STRING      is a character string containing tokens delimited */
/*                  by any one of the characters in the string DELIMS. */
/*                  Consecutive delimiters, and delimiters at the */
/*                  beginning and end of the string, are considered to */
/*                  delimit null items. A blank string is considered to */
/*                  contain a single (blank) item. */

/*      DELIMS      contains the individual characters which delimit */
/*                  the items in the string. These may be any ASCII */
/*                  characters, including blanks. */

/*                  However, by definition, consecutive blanks are NOT */
/*                  considered to be consecutive delimiters. Nor is */
/*                  a blank and any other delimiter considered to be */
/*                  consecutive delimiters. In addition, leading and */
/*                  trailing blanks are ignored.  (See "Particulars" */
/*                  for a discussion of blanks and how they are treated.) */

/*      START       is the point in the string to begin looking for the */
/*                  previous token in the string.  To search for tokens */
/*                  in a string begin with START = LEN(STRING) + 2 and */
/*                  for subsequent calls set START to BEG, where BEG */
/*                  was returned by the previous call. */

/* $ Detailed_Output */

/*      BEG         is the beginning of the substring containing the */
/*                  token. */

/*      END         is the end of the substring containing the token. */

/* $ Parameters */

/*     None. */


/* $ Exceptions */

/*      1.  If START is more than two greater than the length of the */
/*          string it will be treated as though its length is two more */
/*          than the length of the string.  Then if there is a null */
/*          string at the end of the string BEG will point to */
/*          LEN(STRING) + 1, otherwise it will point to the beginning */
/*          of the last token in the string. */

/*      2.  If START is LEN(STRING) + 1, BEG will point to the beginning */
/*          of the last token that preceeds the end of the string. */

/*      2.  If START is at less than or equal to 1, BEG and END will be */
/*          returned as zero. */


/* $ Particulars */


/*      For the purposes of discussion, we regard STRING to be a */
/*      substring of the string that begins with a meta-delimiter */
/*      followed by STRING and ends with a meta-delimiter.  The */
/*      meta-delimiters have indexes 0 and LEN(STRING)+1. */
/*      Meta-delimiters are non-blank delimiters. */

/*      A token is a substring that */

/*         1.  begins with a non-blank character, */
/*         2.  ends   with a non-blank character, */
/*         3.  contains no delimiters */
/*         4.  cannot be extended on either end without violating */
/*             on of the first 3 conditions. */

/*      A good question to ask at this point is: */

/*         "Suppose that ',' is a delimiter and the string contains */
/*          the substring ',    ,' .  Is there a delimiter between */
/*          the two commas? */

/*      Our answer to this question is "Yes".  But from the rules */
/*      1 through 4 above, whatever it is can contain no characters. */
/*      We call such a token a null token.  Another question: */

/*         "Ok. There's a token. Where does it begin and end?" */

/*      Now we have to adopt some convention.  The only consistent */
/*      one we could think of was this:  The null token begins at */
/*      the second delimiter and ends at the previous character. */

/*      Beginning at the second delimiter seems reasonable.  The */
/*      only consistent way to define the end is to give an index */
/*      such that the length computation END - BEG + 1 yields 0. */
/*      So whatever, we do for the beginning, end must be BEG - 1. */

/*      Choosing the beginning to be the second of the two delimiters */
/*      makes it possible to easily move on to the next delimiter. */
/*      If the assignment START = END + 2 is made after a call to */
/*      the routine, then START will always point beyond the end */
/*      of the token just found and will always point no further */
/*      than the beginning of the next token ( if there is one). */
/*      If we keep in mind that there are meta-delimiters at the ends */
/*      of the string then a string that begins with ',    ...' */
/*      begins with a null token. A string that ends with ...   ,  ' */
/*      ends with a null token.  In the first case the beginning */
/*      of the null token is at character 1 of the string. In the second */
/*      case the null token begins at LEN(STRING) + 1,  i.e. at the */
/*      meta-delimiter past the end of the string. */

/*      Using these conventions, this routine finds the beginning and */
/*      end of the last token that ends strictly before the input */
/*      START position in the string.  If no tokens preceeded the input */
/*      index, then both BEG and END will be returned as zero.  This is */
/*      the only case in which BEG will be returned as non-positive. */

/* $ Examples */

/*      STRING = */

/*      'A FEW OF US, THE BAD-BOYS, WENT TO TOWN IN 8//1984-' */

/*                1         2         3         4         5 */
/*       123456789012345678901234567890123456789012345678901 */

/*       If DELIMS = ' ,-/' */

/*          Tokens      BEG      END */
/*          ------      ---      --- */
/*          'A'          1        1 */
/*          'FEW'        3        5 */
/*          'OF'         7        8 */
/*          'US'        10       11 */
/*          'THE'       14       16 */
/*          'BAD'       18       20 */
/*          'BOYS'      22       24 */
/*          'WENT'      28       31 */
/*          'TO'        33       34 */
/*          'TOWN'      36       39 */
/*          'IN'        41       42 */
/*          '8'         44       44 */
/*           null       46       45 */
/*          '1984'      47       50 */
/*           null       52       51 */


/*       If DELIMS = ',/' */

/*          Tokens                BEG      END */
/*          ------                ---      --- */
/*          'A FEW OF US'           1       11 */
/*          'THE BAD-BOYS'         18       25 */
/*          'WENT TO TOWN IN 8'    28       44 */
/*           null                  46       45 */
/*          '1984-'                47       51 */


/*       To get all of the tokens in a string the following loop of code */
/*       will suffice */


/*             BEG   = 1 */
/*             START = LEN ( STRING ) + 2 */

/*             DO WHILE ( BEG .NE. 0 ) */

/*                CALL FNDPTK ( STRING, DELIMS, START, BEG, END ) */

/*                   do something with the token STRING(BEG:END) taking */
/*                   appropriate care of the null tokens. */

/*                START = BEG */

/*             END DO */


/* $ Restrictions */

/*      None. */

/* $ Files */

/*      None. */

/* $ Author_and_Institution */

/*      I.M. Underwood (JPL) */
/*      W.L. Taber     (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 9, 1994 */


/* -     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of META/2 */
/*         software as of May 3, 1994 */


/*     Version B1.0.0, 3-MAY-1988 (WLT) (IMU) */

/* -& */
/* $ Index_Entries */

/*     Find a token preceding a location in a string */

/* -& */

/*     SPICE funtions. */


/*     Local variables */

/* %&END_DECLARATIONS */

/*     Standard SPICE error handling */

    if (return_()) {
	return 0;
    } else {
	chkin_("FNDPTK", (ftnlen)6);
    }

/*     First we gather some data regarding the input string and */
/*     delimiters */

    last = i_len(string, string_len);
    eol = last + 1;
/* Computing MIN */
    i__1 = eol + 1;
    b = min(i__1,*start);

/*     We don't have to do anything if we are starting past the end of */
/*     the string. */

    if (b < 1) {
	*beg = 0;
	*end = 0;
	chkout_("FNDPTK", (ftnlen)6);
	return 0;
    }
    if (b < eol) {
	onspce = *(unsigned char *)&string[b - 1] == ' ';
    } else {
	onspce = FALSE_;
    }

/*     Are we currently pointing at a delimiter? */

    if (b > eol) {
	atdelm = FALSE_;
    } else if (b == eol) {
	atdelm = TRUE_;
    } else if (i_indx(delims, string + (b - 1), delims_len, (ftnlen)1) != 0) {
	atdelm = TRUE_;
    } else {
	atdelm = FALSE_;
    }
    if (atdelm) {

/*        Yes.  Move left to a non-blank character */

	i__1 = b - 1;
	b = ncposr_(string, " ", &i__1, string_len, (ftnlen)1);

/*        If we didn't find a non-blank, then there is not a previous */
/*        token. */

	if (b == 0) {
	    *beg = 0;
	    *end = 0;
	    chkout_("FNDPTK", (ftnlen)6);
	    return 0;
	}

/*        Still here? Are we currently pointing at a delimiter? */

	if (i_indx(delims, string + (b - 1), delims_len, (ftnlen)1) != 0) {

/*           Yes. Move left to a non-blank. */

	    i__1 = b - 1;
	    b = ncposr_(string, " ", &i__1, string_len, (ftnlen)1);
	}

/*        Move left to a delimiter, then Move right 1 */

	b = cposr_(string, delims, &b, string_len, delims_len) + 1;

/*     Are we on a space? */

    } else if (onspce) {

/*        Yes.  (note: space is not a delimiter ) Find the next */
/*        non-blank to the right. */

	b = ncpos_(string, " ", &b, string_len, (ftnlen)1);

/*        Is this a delimiter? */

	if (b == 0) {

/*           it was all blanks to the end of the string.  Make the */
/*           B point to the end + 1, that is a delimiter */

	    b = eol;
	    b = cposr_(string, delims, &b, string_len, delims_len);
	} else if (i_indx(delims, string + (b - 1), delims_len, (ftnlen)1) == 
		0) {

/*           No.   Move left to the first delimiter. */

	    b = cposr_(string, delims, &b, string_len, delims_len);

/*           If we ran off the front of the string without hitting a */
/*           delimiter, there isn't a previous token.  Checkout and */
/*           head for home. */

	    if (b == 0) {
		*beg = 0;
		*end = 0;
		chkout_("FNDPTK", (ftnlen)6);
		return 0;
	    }
	}

/*        Move left to the first delimiter. */
/*        Move right 1 */

	i__1 = b - 1;
	b = cposr_(string, delims, &i__1, string_len, delims_len) + 1;
    } else {

/*     Otherwise */

/*        Move left to the first delimiter. */

	if (b > eol) {
	    b = eol;
	} else {
	    b = cposr_(string, delims, &b, string_len, delims_len);

/*           B is now pointing at a delimiter. */

	}
/* ---------- */
	if (b == 0) {
	    *beg = 0;
	    *end = 0;
	    chkout_("FNDPTK", (ftnlen)6);
	    return 0;
	}

/*        Move left to the first non-blank  (here or to the left) */

	if (b < eol) {
	    b = ncposr_(string, " ", &b, string_len, (ftnlen)1);

/*           B is now pointing to the first non-blank character to the */
/*           left of the token we started in. */

	    i__1 = b - 2;
	    if (i_indx(delims, string + (b - 1), delims_len, (ftnlen)1) != 0 
		    && i_indx(delims, " ", delims_len, (ftnlen)1) != 0 && 
		    s_cmp(string + i__1, " ", b - 1 - i__1, (ftnlen)1) == 0) {

/*              Move backwards to the true delimiter for the token */
/*              that ends here. */

		i__1 = b - 1;
		b = ncposr_(string, " ", &i__1, string_len, (ftnlen)1) + 1;
	    }
	} else {

/*           If we were at or beyond the EOL position, we need to */
/*           know if backing up to a non-blank puts us on a delimiter */
/*           or not.  If it does reset B to EOL. */

	    b = ncposr_(string, " ", &b, string_len, (ftnlen)1);
	    if (i_indx(delims, string + (b - 1), delims_len, (ftnlen)1) != 0) 
		    {
		b = eol;
	    }
	}

/*        Move left to the first deliter, and then move right 1. */

	i__1 = b - 1;
	b = cposr_(string, delims, &i__1, string_len, delims_len) + 1;
    }
    fndntk_(string, delims, &b, beg, end, string_len, delims_len);
    chkout_("FNDPTK", (ftnlen)6);
    return 0;
} /* fndptk_ */

