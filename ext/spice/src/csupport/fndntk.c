/* fndntk.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      FNDNTK ( Find the next token in a string ) */
/* Subroutine */ int fndntk_(char *string, char *delims, integer *start, 
	integer *beg, integer *end, ftnlen string_len, ftnlen delims_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer i_indx(char *, char *, ftnlen, ftnlen), i_len(char *, ftnlen);

    /* Local variables */
    integer last, b;
    logical blank, space, deliml, delimr, nodelm;
    integer nbl, eol, nbr;

/* $ Abstract */

/*      Find the next token in a string delimited by multiple delimiters. */

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
/*                  next token in the string.  To search for tokens in */
/*                  a string begin with START = 1, and for subsequent */
/*                  calls set START to END + 2, where END was returned */
/*                  by the previous call. */

/* $ Detailed_Output */

/*      BEG         is the beginning of the substring containing the */
/*                  token. */

/*      END         is the end of the substring containing the token. */

/* $ Exceptions */

/*      1.  If START is less than 1 it will be treated as though it were */
/*          1. */

/*      2.  If START is the declared length of the string plus 1 and the */
/*          last non-blank character is a delimiter (or the string is */
/*          blank) START will be regarded as pointing at a null token. */
/*          BEG = LEN(STRING) + 1, END = LEN(STRING). */

/*      3.  If START is at least two greater than the declared length of */
/*          the string, BEG and END will be returned as zero. */


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
/*          the substring ',    ,' .  Is there a token between */
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
/*      end of the first token that begins at or following the input */
/*      START position in the string.  If no tokens follow the input */
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
/*             START = 1 */

/*             DO WHILE ( BEG .NE. 0 ) */

/*                CALL FNDNTK ( STRING, DELIMS, START, BEG, END ) */

/*                   do something with the token STRING(BEG:END) taking */
/*                   appropriate care of the null tokens. */

/*                START = END + 2 */

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

/*     Local variables */

/* %&END_DECLARATIONS */

/*     First we gather some data regarding the input string and */
/*     delimiters */

    space = i_indx(delims, " ", delims_len, (ftnlen)1) != 0;
    last = i_len(string, string_len);
    eol = last + 1;
    b = max(1,*start);

/*     We don't have to do anything if we are starting past the end of */
/*     the string. */

    if (b > eol) {
	*beg = 0;
	*end = 0;
	return 0;
    }

/*     Find the first non-blank character at or to the right of where */
/*     we are starting. */

    blank = TRUE_;
    nbr = b;
    while(blank) {
	if (nbr >= eol) {
	    blank = FALSE_;
	} else if (*(unsigned char *)&string[nbr - 1] != ' ') {
	    blank = FALSE_;
	} else {
	    ++nbr;
	}
    }

/*     Find the first non-blank character and first non-blank delimiter */
/*     to the left of the starting point. */

    blank = TRUE_;
    nbl = b - 1;
    while(blank) {
	if (nbl <= 0) {
	    blank = FALSE_;
	} else if (*(unsigned char *)&string[nbl - 1] != ' ') {
	    blank = FALSE_;
	} else {
	    --nbl;
	}
    }

/*     If both the preceeding non-blank character and the following */
/*     non-blank character are delimiters, we have a null item. */

    if (nbr >= eol) {
	delimr = TRUE_;
    } else {
	delimr = i_indx(delims, string + (nbr - 1), delims_len, (ftnlen)1) != 
		0;
    }
    if (nbl <= 0) {
	deliml = TRUE_;
    } else {
	deliml = i_indx(delims, string + (nbl - 1), delims_len, (ftnlen)1) != 
		0;
    }
    if (delimr && deliml) {
	*beg = nbr;
	*end = *beg - 1;
	return 0;
    }

/*     Still here? See if we were past the last delimiter. */

    if (nbr >= eol && ! deliml) {
	*beg = 0;
	*end = 0;
	return 0;
    }

/*     If the left most non-blank is a delimiter OR a blank is a */
/*     delimiter and the non-blank character to the left is at least */
/*     two characters away from the right non-blank character, then */
/*     we have a token beginning at the right non-blank. We just need */
/*     to find the right boundary. */

    if (deliml || nbr - nbl >= 2 && space && ! delimr) {
	*beg = nbr;
	*end = *beg;

/*        Note: DELIMR is already .FALSE. or else we couldn't get to */
/*        this point. */

	while(! delimr) {
	    if (*end + 1 >= eol) {
		delimr = TRUE_;
	    } else /* if(complicated condition) */ {
		i__1 = *end;
		if (i_indx(delims, string + i__1, delims_len, *end + 1 - i__1)
			 != 0) {
		    delimr = TRUE_;
		} else {
		    ++(*end);
		}
	    }
	}

/*        Back up END to the first non-blank that precedes it. */

	while(*(unsigned char *)&string[*end - 1] == ' ') {
	    --(*end);
	}
	return 0;
    }

/*     Still here? In that case we were in the middle of something */
/*     to start with.  Move the pointer forward until we reach a */
/*     delimiter. */

/*     Keep in mind that DELIMR still has the information as to whether */
/*     or not NBR points to a non-blank delimiter. We are going to use */
/*     this information to determine whether to look for a delimiter */
/*     first or not. */

    if (! delimr) {
	nodelm = TRUE_;
	b = nbr;
	while(nodelm) {
	    ++nbr;
	    if (nbr >= eol) {
		nodelm = FALSE_;
	    } else {
		nodelm = i_indx(delims, string + (nbr - 1), delims_len, (
			ftnlen)1) == 0;
	    }
	}

/*        If a space is a delimiter and we happen to have landed on one, */
/*        we want to continue until we hit a non-blank delimiter or just */
/*        before a non-blank character. */

	if (space && nbr < eol) {
	    nodelm = *(unsigned char *)&string[nbr - 1] == ' ';
	    while(nodelm) {
		++nbr;
		if (nbr == eol) {
		    nodelm = FALSE_;
		} else if (i_indx(delims, string + (nbr - 1), delims_len, (
			ftnlen)1) != 0) {
		    nodelm = *(unsigned char *)&string[nbr - 1] == ' ';
		} else if (*(unsigned char *)&string[nbr - 1] != ' ') {
		    nodelm = FALSE_;

/*                 Back up one, to just before the non-blank character */

		    --nbr;
		}
	    }
	}

/*        Since we did not start on a delimiter if we reached the end of */
/*        the string before hitting one, then there is no token to find */
/*        here. */

	if (nbr >= eol) {
	    *beg = 0;
	    *end = 0;
	    return 0;
	}
    }

/*     Still here?  Then starting at the first character to the right of */
/*     the delimiter, find the next non-blank character, and the next */
/*     right delimiter after that. */

    nbl = nbr;
    blank = TRUE_;
    while(blank) {
	++nbl;
	if (nbl >= eol) {
	    blank = FALSE_;
	} else {
	    blank = *(unsigned char *)&string[nbl - 1] == ' ';
	}
    }

/*     Now locate the next delimiter. */

    nbr = nbl - 1;
    delimr = FALSE_;
    while(! delimr) {
	++nbr;
	if (nbr >= eol) {
	    delimr = TRUE_;
	} else {
	    delimr = i_indx(delims, string + (nbr - 1), delims_len, (ftnlen)1)
		     != 0;
	}
    }
    *beg = nbl;
    *end = nbr - 1;
    if (*end > *beg) {

/*        Backup until we are at a non-space. */

	while(*(unsigned char *)&string[*end - 1] == ' ' && *end > *beg) {
	    --(*end);
	}
    }
    return 0;
} /* fndntk_ */

