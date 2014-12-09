/* liter.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure      LITER ( Process a SUBTeX literal block ) */
/* Subroutine */ int liter_(char *source, integer *n, ftnlen source_len)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer need;
    char line[132], cseq[32];
    integer more, i__, j, begin;
    extern /* Subroutine */ int chkin_(char *, ftnlen), tempb_(char *, char *,
	     ftnlen, ftnlen);
    integer width, lskip;
    extern /* Subroutine */ int nthwd_(char *, integer *, char *, integer *, 
	    ftnlen, ftnlen);
    extern integer ltrim_(char *, ftnlen), rtrim_(char *, ftnlen);
    char ltype[32];
    integer indent, allowd;
    char ignore[32];
    extern /* Subroutine */ int params_(char *, char *, integer *, ftnlen, 
	    ftnlen), chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Process a block of literal text delimited by @literal* and */
/*     |endliteral or @exliteral* and !endliteral. */

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

/*     SOURCE      are the source lines containing a block or literal */
/*                 text delimited byby @literal and |endliteral or */
/*                 @exliteral and !endliteral. */

/*     N           is the number of source lines. */

/* $ Detailed_Output */

/*     Processed lines are saved in the temporary buffer. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     SOURCE     I   Source lines. */
/*     N          I   Number of source lines. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Particulars */

/*     Literal sections are left unformatted. They are indented to */
/*     account for LEFTSKIP and LITERALINDENT. Lines are truncated */
/*     on the right if too long. */

/* $ Examples */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/* $Include SUBTeX.REFS */

/* $ Author_and_Institution */

/*     I.M. Underwood   (JPL) */
/*     H.A. Neilan      (JPL) */

/* $ Version */

/* -    Beta Version 3.0.0, 18-AUG-1995 (WLT) */

/*         Removed the code Hester added to write warning messages */
/*         Instead the characters >! appear in the rightmost two */
/*         columns if text won't fit. */

/* -    Beta Version 2.0.0, 24-MAY-1994 (HAN) */

/*         Added code to write warning messages if the literal */
/*         section lines are longer than 67 characters. They are */
/*         truncated in that case, and util now there was no */
/*         indication that truncation occurred. */

/* -    Beta Version 1.0.0, 11-JUN-1988 (IMU) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling */

    if (return_()) {
	return 0;
    } else {
	chkin_("LITER", (ftnlen)5);
    }

/*     What kind of literal section is this? (Normal, item, parameter, */
/*     or variable.) */

    nthwd_(source, &c__1, cseq, &i__, source_len, (ftnlen)32);
    if (s_cmp(cseq, "@literal", (ftnlen)32, (ftnlen)8) == 0 || s_cmp(cseq, 
	    "@exliteral", (ftnlen)32, (ftnlen)10) == 0) {
	s_copy(ltype, "NORMAL", (ftnlen)32, (ftnlen)6);
    } else if (s_cmp(cseq, "@literalitem", (ftnlen)32, (ftnlen)12) == 0 || 
	    s_cmp(cseq, "@exliteralitem", (ftnlen)32, (ftnlen)14) == 0) {
	s_copy(ltype, "ITEM", (ftnlen)32, (ftnlen)4);
    } else if (s_cmp(cseq, "@literalparam", (ftnlen)32, (ftnlen)13) == 0 || 
	    s_cmp(cseq, "@exliteralparam", (ftnlen)32, (ftnlen)15) == 0) {
	s_copy(ltype, "PARAMETER", (ftnlen)32, (ftnlen)9);
    } else if (s_cmp(cseq, "@literalvar", (ftnlen)32, (ftnlen)11) == 0 || 
	    s_cmp(cseq, "@exliteralvar", (ftnlen)32, (ftnlen)13) == 0) {
	s_copy(ltype, "VARIABLE", (ftnlen)32, (ftnlen)8);
    }

/*     Determine what kind of |newpage or !newpage to ignore. */

    if (s_cmp(cseq, "@li", (ftnlen)3, (ftnlen)3) == 0) {
	s_copy(ignore, "|newpage", (ftnlen)32, (ftnlen)8);
    } else {
	s_copy(ignore, "!newpage", (ftnlen)32, (ftnlen)8);
    }

/*     Retrieve the required parameters, and compute the total */
/*     indentation required (as explained below). */

    params_("GET", "PAGEWIDTH", &width, (ftnlen)3, (ftnlen)9);
    params_("GET", "LEFTSKIP", &lskip, (ftnlen)3, (ftnlen)8);
    params_("GET", "LITERALINDENT", &indent, (ftnlen)3, (ftnlen)13);
    begin = lskip + indent + 1;
    if (s_cmp(ltype, "ITEM", (ftnlen)32, (ftnlen)4) == 0) {
	params_("GET", "ITEMINDENT", &more, (ftnlen)3, (ftnlen)10);
	begin += more;
	params_("GET", "ITEMSKIP", &more, (ftnlen)3, (ftnlen)8);
	begin += more;
    } else if (s_cmp(ltype, "PARAMETER", (ftnlen)32, (ftnlen)9) == 0) {
	params_("GET", "ITEMINDENT", &more, (ftnlen)3, (ftnlen)10);
	begin += more;
	params_("GET", "PARAMNAMESIZE", &more, (ftnlen)3, (ftnlen)13);
	begin += more;
	params_("GET", "PARAMNAMESKIP", &more, (ftnlen)3, (ftnlen)13);
	begin += more;
    } else if (s_cmp(ltype, "VARIABLE", (ftnlen)32, (ftnlen)8) == 0) {
	params_("GET", "ITEMINDENT", &more, (ftnlen)3, (ftnlen)10);
	begin += more;
	params_("GET", "VARNAMESIZE", &more, (ftnlen)3, (ftnlen)11);
	begin += more;
	params_("GET", "VARNAMESKIP", &more, (ftnlen)3, (ftnlen)11);
	begin += more;
	params_("GET", "VARTYPESIZE", &more, (ftnlen)3, (ftnlen)11);
	begin += more;
	params_("GET", "VARTYPESKIP", &more, (ftnlen)3, (ftnlen)11);
	begin += more;
    }

/*     Ignore the first and last (delimiting) lines. All but the */
/*     exceptional remaining lines (including blank lines) remain */
/*     unchanged, and begin in column */

/*        LEFTSKIP + LITERALINDENT + 1 */

/*     for normal literal sections. For literal items, add */

/*        ITEMINDENT + ITEMSKIP */

/*     For literal parameters, add */

/*        PARAMINDENT + PARAMNAMESIZE + PARAMNAMESKIP */

/*     For literal variables, add */

/*        VARINDENT + VARNAMESIZE + VARNAMESKIP */
/*                  + VARTYPESIZE + VARTYPESKIP */


/*     The exceptional lines are those that begin with |newline or */
/*     !newline.  These turn into blank lines. */

/*     If this is a plain old literal area, we'll check to see if */
/*     truncation is going to occur. If it is, print out a warning */
/*     message. */

    s_copy(line, " ", (ftnlen)132, (ftnlen)1);
    i__1 = *n - 1;
    for (i__ = 2; i__ <= i__1; ++i__) {
	j = ltrim_(source + (i__ - 1) * source_len, source_len);
	allowd = width - begin + 1;
	need = rtrim_(source + (i__ - 1) * source_len, source_len);
	if (s_cmp(source + ((i__ - 1) * source_len + (j - 1)), ignore, 
		source_len - (j - 1), (ftnlen)32) == 0) {
	    s_copy(line, " ", (ftnlen)132, (ftnlen)1);
	} else {
	    s_copy(line + (begin - 1), source + (i__ - 1) * source_len, width 
		    - (begin - 1), source_len);
	    if (need > allowd) {
		i__2 = width - 2;
		s_copy(line + i__2, ">!", width - i__2, (ftnlen)2);
	    }
	}
	tempb_("ADD", line, (ftnlen)3, (ftnlen)132);
    }

/*     Every literal block is followed by a blank line. */

    tempb_("ADD", " ", (ftnlen)3, (ftnlen)1);
    chkout_("LITER", (ftnlen)5);
    return 0;
} /* liter_ */

