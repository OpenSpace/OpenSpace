/* prepsn.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;

/* $Procedure      PREPSN (Pretty print syntax definition) */
/* Subroutine */ int prepsn_(char *string, ftnlen string_len)
{
    /* System generated locals */
    address a__1[2];
    integer i__1[2];

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen), s_cat(char *,
	     char **, integer *, integer *, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char long__[2000], word[63];
    integer b, e, r__;
    logical begin;
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen);
    integer start;
    extern integer rtrim_(char *, ftnlen);
    extern /* Subroutine */ int fndnwd_(char *, integer *, integer *, integer 
	    *, ftnlen);
    logical indent;
    integer indnby;
    logical crlast;
    char outdnt[63];
    integer end;

/* $ Abstract */

/*     This routine prepares a string having a META/2 syntax description */
/*     for printing via NICEIO, NICEPR or NICEBT */

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

/*       FORMATTING */

/* $ Declarations */
/* $ Brief_I/O */

/*      VARIABLE  I/O  DESCRIPTION */
/*      --------  ---  -------------------------------------------------- */
/*      STRING    I/O  a string to be prepare for display */

/* $ Detailed_Input */

/*     STRING     is a string that contains a META/2 syntax description. */

/* $ Detailed_Output */

/*     STRING     is the same string after having carriage return */
/*                markers inserted into it for use by display routines */
/*                NICEIO, NICEPR or NICEBT */

/* $ Parameters */

/*      MAXLEN    is the maximum length string that can be supported */
/*                for pretty printing. */

/* $ Files */

/*      None. */

/* $ Exceptions */

/*     Error free. */

/* $ Particulars */

/*     This routine allows you to easily prepare a META/2 syntax */
/*     specification for display using one of the routines NICEIO */
/*     NICEPR or NICEBT.  The routine steps through the input */
/*     routine a word at a time to locate the markers used in */
/*     META/2 switches.  It assumes the string '/cr' is used for */
/*     the new line marker within a string. */

/*     Newlines are always inserted at the beginning of a switch (x:y){, */
/*     after a switch separator '|' and after the end of a switch }. */
/*     Care is taken so that the construct */

/*       } (x:y){ */

/*     becomes */

/*       }/cr (x:y){ */

/*     and not */

/*       }/cr(x:y){ */

/*     or */

/*       }/cr/cr (x:y){ */

/* $ Examples */

/*     This routine is meant for internal use by the routine */
/*     META_2.  However, if you have a sequence of strings that */
/*     you would like to prepare for display in documentation */
/*     you might want to use this routine together with */
/*     NICEIO or one of its cousins for preparing your documentation. */

/*        DO I = 1, NSYN */

/*           TEMP = SYNTAX */

/*           CALL PREPSN ( TEMP ) */
/*           CALL NICEIO ( TEMP, UNIT, 'LEFT 1 RIGHT 78' ) */
/*           WRITE (UNIT,*) ' ' */

/*        END DO */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*      W.L. Taber      (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */

/* -     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 9, 1994 */


/* -     META/2 Configured Version 1.0.0, 9-MAY-1994 (WLT) */

/*         This is the configured version of META/2 */
/*         software as of May 9, 1994 */


/* -& */
/* $ Index_Entries */

/*     «We need a permuted index entry */

/* -& */

/*     Set the initial states. */

/*        START  we start looking at the string at the first character */
/*        E      end of the first word (we have to start somewhere) */
/*        END    is the end of the local buffer LONG. */
/*        INDBY  is the amount we've indented things. */
/*        LONG   is our local string for creating the pretty print string */
/*        OUTDNT is the string for controlling out-denting */
/*        BEGIN  we have not begun processing a swithc */
/*        INDENT we have not indented */
/*        CRLAST we did not put a '/cr' in the last word we processed. */

    start = 1;
    e = 1;
    end = 1;
    indnby = 0;
    s_copy(long__, " ", (ftnlen)2000, (ftnlen)1);
    s_copy(outdnt, " ", (ftnlen)63, (ftnlen)1);
    begin = FALSE_;
    indent = FALSE_;
    crlast = FALSE_;

/*     Process the string a word at a time  untill we've seen it all. */

    while(e != 0) {
	fndnwd_(string, &start, &b, &e, string_len);
	if (e > 0) {
	    if (*(unsigned char *)&string[e - 1] == '{') {

/*              There was a word left in the string.  The beginning */
/*              of a switch ends with '{' */

		begin = TRUE_;
		indent = FALSE_;
		if (crlast) {
		    crlast = FALSE_;
/* Writing concatenation */
		    i__1[0] = 1, a__1[0] = " ";
		    i__1[1] = e - (b - 1), a__1[1] = string + (b - 1);
		    s_cat(word, a__1, i__1, &c__2, (ftnlen)63);
		} else {
		    s_copy(word, "/cr(:1) ", (ftnlen)8, (ftnlen)8);
		    s_copy(word + 8, string + (b - 1), (ftnlen)55, e - (b - 1)
			    );
		}

/*              We shall indent (if we do at all) by the number */
/*              of characters that precede the left bracket '{' */

		indnby = e - b;
	    } else if (s_cmp(string + (b - 1), "|", e - (b - 1), (ftnlen)1) ==
		     0) {

/*              Switch separators appear all by themselves a words. */

		if (begin) {

/*                 This is the first separator of this switch, we */
/*                 are probably going to indent.  And we are no */
/*                 longer in the beginning simple template of the */
/*                 switch. */

		    begin = FALSE_;
		    indent = TRUE_;
		    if (indnby > 0) {

/*                    Create the indent and outdent strings. */

			s_copy(word, "/cr(#:)|", (ftnlen)63, (ftnlen)8);
			s_copy(outdnt, "/cr(-#:)", (ftnlen)63, (ftnlen)8);
			repmi_(word, "#", &indnby, word, (ftnlen)63, (ftnlen)
				1, (ftnlen)63);
			repmi_(outdnt, "#", &indnby, outdnt, (ftnlen)63, (
				ftnlen)1, (ftnlen)63);
		    } else {
			s_copy(word, "/cr|", (ftnlen)63, (ftnlen)4);
			s_copy(outdnt, "/cr(0:0)", (ftnlen)63, (ftnlen)8);
		    }
		} else {

/*                 We are not at the beginning so there is no */
/*                 need to indent. */

		    s_copy(word, "/cr|", (ftnlen)63, (ftnlen)4);
		}
	    } else if (*(unsigned char *)&string[b - 1] == '}') {

/*              We are at the end of a switch (there might be some */
/*              other stuff such as user punctuation in the string */
/*              so we don't require STRING(B:E) .EQ. '}' */

		begin = FALSE_;
		if (indent) {
		    indent = FALSE_;
/* Writing concatenation */
		    i__1[0] = e - (b - 1), a__1[0] = string + (b - 1);
		    i__1[1] = 63, a__1[1] = outdnt;
		    s_cat(word, a__1, i__1, &c__2, (ftnlen)63);
		} else {
/* Writing concatenation */
		    i__1[0] = e - (b - 1), a__1[0] = string + (b - 1);
		    i__1[1] = 8, a__1[1] = "/cr(0:0)";
		    s_cat(word, a__1, i__1, &c__2, (ftnlen)63);
		}

/*              We just put in a carriage return at the end of a switch. */
/*              Set our logical flag that says we did this. */

		crlast = TRUE_;
	    } else {

/*              This word is to be treated as an ordinatry word. */

		s_copy(word, string + (b - 1), (ftnlen)63, e - (b - 1));
		crlast = FALSE_;
	    }
	    r__ = rtrim_(word, (ftnlen)63);
	    s_copy(long__ + (end - 1), word, end + r__ - (end - 1), (ftnlen)
		    63);
	    end = end + r__ + 1;
	}
	start = e + 1;
    }

/*     That's all folks.  Move our long string into STRING and */
/*     return. */

    s_copy(string, long__, string_len, end);
    return 0;
} /* prepsn_ */

