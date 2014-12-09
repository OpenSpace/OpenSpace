/* m2trim.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      M2TRIM ( META/2 trim the name portion from a word ) */
/* Subroutine */ int m2trim_(char *word, char *root, ftnlen word_len, ftnlen 
	root_len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer i_len(char *, ftnlen);

    /* Local variables */
    static integer b, e, blank, lbrace, rbrace;
    extern integer qrtrim_(char *, ftnlen);

/* $ Abstract */

/*     Extract the "root" of a META/2 template word.  That is trim off */
/*     the name portion of a template word. */

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

/*    META/2 */

/* $ Keywords */

/*     META1 */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     WORD       I   A word from a META/2 template. */
/*     ROOT       O   The input word trimmed of any name specification. */

/* $ Detailed_Input */

/*     WORD       is a word from a META/2 template.  It may or may not */
/*                looklike   ROOT // '[name]' */

/* $ Detailed_Output */

/*     ROOT       is the portion of the input word that precedes the */
/*                name portion of the input WORD.  ROOT may overwrite */
/*                WORD. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     If ROOT is not sufficiently large to contain all of the output, */
/*     it will be truncated on the right. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     META/2 template words can have appended to them "variable" names */
/*     that will be used to store substring boundaries of STRINGS matched */
/*     against META/2 templates.  For example */

/*           FIND @name[WINDOW] */
/*           SEPARATION  (2:2){ OF   @int[BODY1] @int[BODY2] */
/*                            | FROM @int[OBSERVER]          } */

/*     the words */

/*         @name[WINDOW], @int[BODY1], @int[BODY2], @int[OBSERVER] */

/*     all have "varialbe" name substrings.  They are: */

/*         WINDOW, BODY1, BODY2, and OBSERVER respectively. */

/*     The routine removes variable names and associated brackets in WORD */
/*     if they exist. */

/* $ Examples */

/*     Below is a table descibing sample inputs and outputs. */

/*         WORD                ROOT */
/*         ---------------     ------------------ */
/*         @int[SPUD]          @int */
/*         @name[WINDOW]       @name */
/*         SEARCH[GET]         SEARCH */
/*         @name               @name */
/*         @body(2:4)[LIST]    @body(2:4) */

/* $ Restrictions */

/*     None. */


/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */

/* $ Version */

/* -     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 9, 1994 */


/* -     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of META/2 */
/*         software as of May 3, 1994 */


/* -    Beta Version 1.0.0, 21-NOV-1991 (WLT) */

/* -& */

/* $ Index_Entry */

/*     Extract the root of a META/2 template word. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */

    s_copy(root, word, root_len, word_len);
    lbrace = '[';
    rbrace = ']';
    blank = ' ';
    e = i_len(word, word_len);

/*     This loop is the same as RTRIM only faster. */

    e = qrtrim_(word, word_len);

/*     If the length is not at least 4 or the last character is not */
/*     a right brace, there is no name associated with this word. */

    if (*(unsigned char *)&word[e - 1] == rbrace && e >= 4) {

/*        Ok. We have a chance at getting a name.  Look for */
/*        a left brace and if found blank out the end portion of */
/*        ROOT. */

	b = 2;
	while(b < e - 1) {
	    if (*(unsigned char *)&word[b - 1] == lbrace) {

/*              We've found the beginning of the name portion */
/*              of the word.  Record the end of the meta-2 */
/*              word and then reset L so that we exit this loop. */

		s_copy(root + (b - 1), " ", root_len - (b - 1), (ftnlen)1);
		b = e;
	    }
	    ++b;
	}
    }
    return 0;
} /* m2trim_ */

