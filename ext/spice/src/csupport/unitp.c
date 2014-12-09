/* unitp.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__32 = 32;

/* $Procedure UNITP ( Determine whether a string represents units) */
logical unitp_(char *string, ftnlen string_len)
{
    /* Initialized data */

    static logical first = TRUE_;
    static integer nop = 6;
    static char op[2*6] = "  " "( " ") " "* " "**" "/ ";

    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    logical ret_val;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    extern /* Subroutine */ int scan_(char *, char *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    ftnlen, ftnlen);
    integer nest;
    static integer mult;
    integer b, e, i__;
    static integer blank;
    integer ident[32], class__;
    doublereal value;
    static integer oplen[6];
    logical known;
    integer start;
    static integer opptr[20];
    extern integer bsrchc_(char *, integer *, char *, ftnlen, ftnlen);
    static integer lparen;
    extern /* Subroutine */ int fnducv_(char *, logical *, integer *, 
	    doublereal *, ftnlen), scanpr_(integer *, char *, integer *, 
	    integer *, ftnlen);
    static integer rparen;
    integer lasttk, explev;
    logical physcl, expgrp;
    integer ntokns, beg[32], end[32];
    static integer div, exp__;

/* $ Abstract */

/*     Determine whether or not a string represents the units associated */
/*     with a physical quantity. */

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

/*     PARSING */
/*     UNITS */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     STRING     I   potentially, units describing a physical quantity */

/*     The function returns .TRUE. if the string represents some physical */
/*     units.  Otherwise it returns false. */

/* $ Detailed_Input */

/*     STRING     a string that potentially represents the units */
/*                associated with some physical quantity.  For */
/*                example KM/SEC.  A string represents a unit if */
/*                it consists of numbers and recognized */
/*                primitive units (of length, angle, mass, time or */
/*                charge) connected in a "sensible" way with */
/*                operations of multiplication, division and */
/*                exponentiation. */

/* $ Detailed_Output */

/*     UNITP      returns as TRUE if the string satisfies the following */
/*                rules. */

/*                1) All maximal substrings of STRING that do not contain */
/*                   any of the character '(', ')', '*', '/' are */
/*                   recognized as numbers or units of angle, length, */
/*                   time, mass or charge. */

/*                2) The string is a properly formed multiplicative */
/*                   expression. */

/*                3) At least one physical unit is present in the string. */

/*                4) No physical units appear in an exponent */
/*                   subexpression. */

/*                If these conditions are not met, the function returns */
/*                FALSE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine examines a string to determine whether or not */
/*     it represents the units attached to a physical quantity. */

/*     Units are created by multiplicatively combining primitive units */
/*     of time, length, angle, charge and mass together with numeric */
/*     constants. */

/* $ Examples */

/*     Below are some sample strings and the response of UNITP */
/*     when applied to these strings. */

/*     String                 Value of UNITP */
/*     ----------------       -------------- */
/*     KM                     T */
/*     KM/SEC                 T */
/*     KM**3/SEC**2           T */
/*     (KM**(SEC**-2))        F   ( a unit appears in the exponent ) */
/*     (KM)/SEC               T */
/*     (KM/SEC                F   ( parentheses are unbalanced ) */
/*     (KM/+(7*DAYS)          F   ( /+ is not a legitimate operation ) */
/*     12*7                   F   ( no physical units appear ) */
/*     3*KG                   T */
/*     AU/(100*DAYS)          T */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 10-APR-1990 (WLT) */

/* -& */

/*     SPICELIB functions */


/*     Parameters */


/*     Local variables */


/*       Here is the range of       Character      ASCII code */
/*       initial characters that    ---------      ---------- */
/*       will be used by the        ' '             32 */
/*       "known" marks.             '('             40 */
/*                                  ')'             41 */
/*                                  '*'             42 */
/*                                  '/'             47 */

/*     So the required number of pointers is 47 - 32 + 5 = 20. */


/*     Saved variables */


/*     Initial values */


/*     On the first pass through this routine, set up the stuff */
/*     required for scanning the input string. */

    if (first) {
	first = FALSE_;
	scanpr_(&nop, op, oplen, opptr, (ftnlen)2);
	blank = bsrchc_(" ", &nop, op, (ftnlen)1, (ftnlen)2);
	lparen = bsrchc_("(", &nop, op, (ftnlen)1, (ftnlen)2);
	rparen = bsrchc_(")", &nop, op, (ftnlen)1, (ftnlen)2);
	mult = bsrchc_("*", &nop, op, (ftnlen)1, (ftnlen)2);
	exp__ = bsrchc_("**", &nop, op, (ftnlen)2, (ftnlen)2);
	div = bsrchc_("/", &nop, op, (ftnlen)1, (ftnlen)2);
    }

/*     To get started we will assume that the last token (before we */
/*     started looking at the string) was an introductory left */
/*     parenthesis. */

    lasttk = lparen;
    nest = 0;
    physcl = FALSE_;
    expgrp = FALSE_;
    start = 1;
    scan_(string, op, oplen, opptr, &c__32, &start, &ntokns, ident, beg, end, 
	    string_len, (ftnlen)2);
    while(ntokns > 0) {
	i__1 = ntokns;
	for (i__ = 1; i__ <= i__1; ++i__) {

/*           Look at the identity of the next token ... */

	    if (ident[(i__2 = i__ - 1) < 32 && 0 <= i__2 ? i__2 : s_rnge(
		    "ident", i__2, "unitp_", (ftnlen)282)] == 0) {

/*              A non-recognized item cannot follow a right parenthesis */
/*              or a non-recognized item. */

		if (lasttk == rparen || lasttk == 0) {
		    ret_val = FALSE_;
		    return ret_val;
		}

/*              So far, so good.  Determine whether this object is */
/*              a recognized unit or number. */

		b = beg[(i__2 = i__ - 1) < 32 && 0 <= i__2 ? i__2 : s_rnge(
			"beg", i__2, "unitp_", (ftnlen)298)];
		e = end[(i__2 = i__ - 1) < 32 && 0 <= i__2 ? i__2 : s_rnge(
			"end", i__2, "unitp_", (ftnlen)299)];
		fnducv_(string + (b - 1), &known, &class__, &value, e - (b - 
			1));

/*              If it wasn't recognized we don't have a unit. */

		if (! known) {
		    ret_val = FALSE_;
		    return ret_val;
		}

/*              We also need to make sure we don't have anything of */
/*              the form **UNIT or **( ... UNIT ... ) where UNIT is a */
/*              physical unit. */

		if (class__ > 0) {
		    if (lasttk == exp__ || expgrp) {
			ret_val = FALSE_;
			return ret_val;
		    }
		}

/*              Finally, we need to keep track of whether or not */
/*              we've seen a physical unit. */

		physcl = physcl || class__ > 0;
	    } else if (ident[(i__2 = i__ - 1) < 32 && 0 <= i__2 ? i__2 : 
		    s_rnge("ident", i__2, "unitp_", (ftnlen)334)] == rparen) {

/*              A right parenthesis can only follow a right parenthesis, */
/*              a unit or a number. */

		if (lasttk != 0 && lasttk != rparen) {
		    ret_val = FALSE_;
		    return ret_val;
		}
		--nest;
	    } else if (ident[(i__2 = i__ - 1) < 32 && 0 <= i__2 ? i__2 : 
		    s_rnge("ident", i__2, "unitp_", (ftnlen)350)] == exp__ || 
		    ident[(i__3 = i__ - 1) < 32 && 0 <= i__3 ? i__3 : s_rnge(
		    "ident", i__3, "unitp_", (ftnlen)350)] == mult || ident[(
		    i__4 = i__ - 1) < 32 && 0 <= i__4 ? i__4 : s_rnge("ident",
		     i__4, "unitp_", (ftnlen)350)] == div) {

/*              An arithmetic operation can only follow a right */
/*              parenthesis, a unit or a number. */

		if (lasttk != rparen && lasttk != 0) {
		    ret_val = FALSE_;
		    return ret_val;
		}
	    } else if (ident[(i__2 = i__ - 1) < 32 && 0 <= i__2 ? i__2 : 
		    s_rnge("ident", i__2, "unitp_", (ftnlen)364)] == lparen) {

/*              A left parenthesis must be the first thing in the */
/*              string or follow one of the following: */

/*                    '(', '*', '**', '/' */

/*              (Note by construction the last token prior to the */
/*              beginning of the string was '(' ).  If this is _not_ */
/*              the case then this is not a unit. */

		if (lasttk != lparen && lasttk != mult && lasttk != div && 
			lasttk != exp__) {
		    ret_val = FALSE_;
		    return ret_val;
		}

/*              If the last token was exponentiation (and we were not */
/*              already in some exponentiation group), we can't have */
/*              anything but numbers until the nesting level returns */
/*              to the current level. */

		if (lasttk == exp__ && ! expgrp) {
		    explev = nest;
		    expgrp = TRUE_;
		}

/*              Increase the nesting level of the expression. */

		++nest;
	    } else if (ident[(i__2 = i__ - 1) < 32 && 0 <= i__2 ? i__2 : 
		    s_rnge("ident", i__2, "unitp_", (ftnlen)405)] == blank) {

/*              Don't do anything. */

	    }

/*           Copy the identity of this token. */

	    lasttk = ident[(i__2 = i__ - 1) < 32 && 0 <= i__2 ? i__2 : s_rnge(
		    "ident", i__2, "unitp_", (ftnlen)416)];

/*           Now for a few quick checks.  If the nesting level ever drops */
/*           below zero, we don't have a unit. */

	    if (nest < 0) {
		ret_val = FALSE_;
		return ret_val;
	    }

/*           We need to see if its ok to relax the restriction on the */
/*           use of physical units. */

	    if (expgrp) {
		expgrp = nest > explev;
	    }
	}

/*        Just in case we didn't get everything the first time, */
/*        scan the string again. */

	scan_(string, op, oplen, opptr, &c__32, &start, &ntokns, ident, beg, 
		end, string_len, (ftnlen)2);
    }

/*     One last check.  If we didn't get a physical unit somewhere in */
/*     the string or if the nesting did not return to zero, we don't */
/*     have a unit. */

    if (nest == 0) {
	ret_val = physcl;
    } else {
	ret_val = FALSE_;
    }
    return ret_val;
} /* unitp_ */

