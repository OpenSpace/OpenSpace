/* cutstr.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      CUTSTR ( Cut a long string into substrings ) */
/* Subroutine */ int cutstr_(char *string, integer *start, integer *width, 
	char *breaks, integer *beg, integer *end, ftnlen string_len, ftnlen 
	breaks_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer i_len(char *, ftnlen), s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer i_indx(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer here;
    extern logical even_(integer *);
    integer long__, pass;
    char this__[1], next[1];
    integer a, b, p, blank;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer there;
    char other[15], dtype[15], punct[15], quote[15], otype[15], ptype[15], 
	    qtype[15];
    integer length;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    extern integer occurs_(char *, char *, ftnlen, ftnlen);
    extern logical return_(void);
    char def[15];

/* $ Abstract */

/*     Cut a long string into substrings, breaking at "good" points */
/*     whenever possible. */

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
/*     STRING     I   Long string. */
/*     START      I   Nominal beginning of substring. */
/*     WIDTH      I   Maximum width of substrings. */
/*     BREAKS     I   Characters indicating good break points. */
/*     BEG        I   Beginning of substring. */
/*     END        O   End of substring. */

/* $ Detailed_Input */

/*     STRING      is an arbitrary character string. Typically, this */
/*                 is too wide to fit into an area of limited width: */
/*                 an element of a character array, for instance, or */
/*                 an area on a terminal screen. */

/*     START       is the nominal beginning of the next substring. */
/*                 (STRING(START:START) is the first character that */
/*                 can appear in the substring.) It is used to skip */
/*                 past substrings returned by previous calls. */

/*     WIDTH       is the width (in characters) of the limited area. */
/*                 Thus, it is the maximum width of the substrings */
/*                 to be returned. */

/*     BREAKS      is a collection of characters indicating preferred */
/*                 places to break the string into substrings: commas, */
/*                 colons, and periods, for instance. BREAKS is always */
/*                 treated as though it contains a space, whether it */
/*                 does or not. (That is, '+-=' is treated as ' +-='.) */

/* $ Detailed_Output */

/*     BEG, */
/*     END         are the endpoints of a substring no wider than */
/*                 WIDTH. Substrings always begin and end with non-blank */
/*                 characters. BEG is zero if no non-blank substring */
/*                 was found. */

/* $ Exceptions. */

/*     1) If STRING(START:) is blank or BEG is greater than the declared */
/*        length of STRING, both BEG and END are zero. */

/*     2) If START is less than one, the error 'SPICE(BEFOREBEGSTR)' */
/*        is signalled. */

/*     4) If WIDTH is less than one, the error 'SPICE(WIDTHTOOSMALL)' */
/*        is signalled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*      This routine is useful primarily for displaying messages on */
/*      terminal screens or within log files. Since messages can run */
/*      to hundreds of characters, while most output devices cannot */
/*      handle more than 80 or 132 characters at a time, it is necessary */
/*      to break the strings. The friendliness of a message is enhanced */
/*      if these breaks occur at "natural" places within the message, */
/*      rather than at rigid intervals. */

/*      The most natural breaks occur before spaces. Slightly less */
/*      natural breaks occur at the characters */

/*         Comma             , */
/*         Period            . */
/*         Semicolon         ; */
/*         Colon             : */
/*         Question          ? */
/*         Exclamation       ! */
/*         End parenthesis   ) */
/*         End bracket       ] */
/*         End brace         } */
/*         End angle         > */

/*      or before the characters */

/*         Begin parenthesis   ( */
/*         Begin bracket       [ */
/*         Begin brace         { */
/*         Begin angle         < */

/*      At any rate, breaks should occur between adjacent letters or */
/*      numeric characters only as a last resort. */

/*      In the absence of other instructions, CUTSTR tries to break: */

/*         1) before      space */

/*         2) at           , . ; : - ) ] } > */
/*            or before              ( [ { < */

/*         3) at           ' "                   (even occurrence) */
/*            or before    ' "                   (odd  occurrence) */

/*         4) at           ? ! = _ % */
/*            or before    \ ~ $ @ ^ * / | & + */

/*      before forcing a break at an aribitrary location. */

/*      You may override these rules by supplying a set of preferred */
/*      characters in BREAKS. Before applying the rules shown above, */
/*      CUTSTR will try to break AT these characters. (However, breaks */
/*      always occur BEFORE spaces.) */

/* $ Examples */

/*      CUTSTR might typically be used to display a long diagnostic */
/*      messages on a terminal screen. For example, suppose that the */
/*      following message has been returned by a subroutine. */

/*         'I believe you have made an significant error by requesting */
/*          that I send to the printer a file containing 250 megabytes */
/*          of text information. The system manager is likely to be */
/*          very unhappy with such a request. I suggest you reconsider */
/*          your intended action.' */

/*      and that this needs to be displayed on a 40-character monitor. */
/*      The following code fragment */

/*         WIDTH  = 40 */
/*         BREAKS = ' ,.' */

/*         CALL CUTSTR ( MSSG, 1, WIDTH, BREAKS, BEG, END ) */

/*         DO WHILE ( BEG .NE. 0 ) */
/*            WRITE (6,*) MSSG (BEG:END) */

/*            START = END + 1 */
/*            CALL CUTSTR ( MSSG, START, WIDTH, BREAKS, BEG, END ) */
/*         END DO */

/*      would display something like */

/*         I believe you have made an significant */
/*         error by requesting that I send to the */
/*         printer a file containing 250 megabytes */
/*         of text information. The system manager */
/*         is likely to be very unhappy with such a */
/*         request.  I suggest you reconsider your */
/*         intended action. */

/*      On a more whimsical note, you could indent each successive lines */
/*      by three characters: the code fragment */

/*         WIDTH  = 40 */
/*         BREAKS = ' ,.' */
/*         INDENT = 1 */

/*         CALL CUTSTR ( MSSG, 1, WIDTH, BREAKS, BEG, END ) */

/*         DO WHILE ( BEG .NE. 0 ) */
/*            TEMP           = ' ' */
/*            TEMP(INDENT: ) = MSSG(BEG:END) */
/*            WRITE (6,*) TEMP */

/*            INDENT = INDENT + 3 */
/*            WIDTH  = MAX ( WIDTH-3, 9 ) */

/*            START = END + 1 */
/*            CALL CUTSTR ( MSSG, START, WIDTH, BREAKS, BEG, END ) */
/*         END DO */

/*      would display something like */

/*         I believe you have made an significant */
/*            error by requesting that I send to */
/*               the printer a file containing 250 */
/*                  megabytes of text information. */
/*                     The system manager is likely */
/*                        to be very unhappy with */
/*                           such a request. I */
/*                              suggest you */
/*                                 reconsider your */
/*                                    intended */
/*                                       action. */

/*     Note that both loops terminate when BEG is zero. This indicates */
/*     that no substring was found (and that none will be returned by */
/*     subsequent calls). If the string is full, the loop will terminate */
/*     normally when START becomes greater than the length of the string. */

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

/*     SPICELIB functions */


/*     Local variables */


/*     Parameters used to simulate an enumerated type for */
/*     the various passes required to break the string at */
/*     good places.  Note that the order is important. */
/*     This forces the routine to try spaces first, user */
/*     supplied preferences next, etc.    It is also */
/*     critical that these be defined to be a sequence */
/*     of consecutive integers. */


/*     The ASCII character value for the backslash is needed for */
/*     uniformity of porting this routine (Some platforms treat the */
/*     backslah as a special character and so we can't just use */
/*     the character in strings.) */


/*     Standard error handling. */

    if (return_()) {
	return 0;
    }

/*     Exceptions first. Is START outside the bounds of the string? */

    length = i_len(string, string_len);
    if (*start > length) {
	*beg = 0;
	*end = 0;
	return 0;
    } else if (*start < 1) {
	chkin_("CUTSTR", (ftnlen)6);
	sigerr_("SPICE(BEFOREBEGSTR)", (ftnlen)19);
	chkout_("CUTSTR", (ftnlen)6);
	return 0;
    }

/*     Is the width reasonable? */

    if (*width < 1) {
	chkin_("CUTSTR", (ftnlen)6);
	sigerr_("SPICE(WIDTHTOOSMALL)", (ftnlen)20);
	chkout_("CUTSTR", (ftnlen)6);
	return 0;
    }

/*     Does the remainder of the string contain anything besides blanks? */

    if (s_cmp(string + (*start - 1), " ", string_len - (*start - 1), (ftnlen)
	    1) == 0) {
	*beg = 0;
	*end = 0;
	return 0;
    }

/*     Obviously, we should try to get the longest possible substring. */


    *beg = *start;
    blank = ' ';
    while(*(unsigned char *)&string[*beg - 1] == blank) {
	++(*beg);
    }
    long__ = *beg + *width - 1;

/*     The remainder of the substring may fit without a trim. */
/*     But drop trailing blanks anyway. */

    if (length <= long__) {
	*end = length;
	while(*(unsigned char *)&string[*end - 1] == blank) {
	    --(*end);
	}
	return 0;
    }

/*     Assign the default break characters. Each character in PUNCT, */
/*     QUOTE, or OTHER indicates a good place to break. The associated */
/*     type indicates whether the break should occur at or before the */
/*     the character: */

/*        Type         Break occurs */
/*        ----         ------------------------------------------------ */
/*         A           At the character. */
/*         B           Before the character. */
/*         P           At an EVEN occurrence, or */
/*                     Before an ODD occurrence. */


    s_copy(punct, ",.;:-)]}>([{<", (ftnlen)15, (ftnlen)13);
    s_copy(ptype, "AAAAAAAAABBBB", (ftnlen)15, (ftnlen)13);
    s_copy(quote, "\"'", (ftnlen)15, (ftnlen)2);
    s_copy(qtype, "PP", (ftnlen)15, (ftnlen)2);
    s_copy(other, "?!~$@^=_%*/|&+\\", (ftnlen)15, (ftnlen)15);
    s_copy(otype, "AABBBBAAABBBBBB", (ftnlen)15, (ftnlen)15);

/*     We will do this in five passes. During the first pass, we will */
/*     try to break before a space. During the second pass, we will try */
/*     to break at one of the preferred characters. During the third, */
/*     fourth, and fifth passes, we will try to break at or before one */
/*     of the quotation, punctuation, or other default characters. */

    pass = 1;
    a = 'A';
    p = 'P';
    b = 'B';
    while(pass != 6) {
	*end = long__;
	while(*end >= *beg) {
	    *(unsigned char *)this__ = *(unsigned char *)&string[*end - 1];
	    i__1 = *end;
	    s_copy(next, string + i__1, (ftnlen)1, *end + 1 - i__1);

/*           Always break BEFORE a space. */

	    if (pass == 1) {
		if (*(unsigned char *)next == blank) {
		    pass = 7;
		}

/*           Always break AT a preferred character. */

	    } else if (pass == 2) {
		if (i_indx(breaks, this__, breaks_len, (ftnlen)1) > 0) {
		    pass = 7;
		}

/*           But with default characters, some break at, some */
/*           before, and some depend on the parity of strangers. */

	    } else {
		here = i_indx(def, this__, (ftnlen)15, (ftnlen)1);
		there = i_indx(def, next, (ftnlen)15, (ftnlen)1);
		if (here > 0) {
		    if (*(unsigned char *)&dtype[here - 1] == a) {
			pass = 7;
		    } else if (*(unsigned char *)&dtype[here - 1] == p) {
			i__1 = occurs_(string, this__, (*end), (ftnlen)1);
			if (even_(&i__1)) {
			    pass = 7;
			}
		    }
		}
		if (there > 0 && pass != 7) {
		    if (*(unsigned char *)&dtype[there - 1] == b) {
			pass = 7;
		    } else if (*(unsigned char *)&dtype[there - 1] == p) {
			i__1 = occurs_(string, next, (*end), (ftnlen)1);
			if (even_(&i__1)) {
			    pass = 7;
			}
		    }
		}
	    }

/*           If we've found a break point, remove any trailing blanks */
/*           before returning. */

	    if (pass == 7) {
		while(*(unsigned char *)&string[*end - 1] == blank) {
		    --(*end);
		}
		return 0;
	    } else {
		--(*end);
	    }
	}

/*        We may have to try another pass. */

	++pass;

/*        In the final passes, only the character set changes. */

	if (pass == 3) {
	    s_copy(def, punct, (ftnlen)15, (ftnlen)15);
	    s_copy(dtype, ptype, (ftnlen)15, (ftnlen)15);
	} else if (pass == 4) {
	    s_copy(def, quote, (ftnlen)15, (ftnlen)15);
	    s_copy(dtype, qtype, (ftnlen)15, (ftnlen)15);
	} else if (pass == 5) {
	    s_copy(def, other, (ftnlen)15, (ftnlen)15);
	    s_copy(dtype, otype, (ftnlen)15, (ftnlen)15);
	}
    }

/*     Looks like we'll have to do this the hard way. */

    *end = long__;
    return 0;
} /* cutstr_ */

