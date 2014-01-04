/* m2chck.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__10 = 10;
static integer c__1 = 1;

/* $Procedure      M2CHCK ( Meta-2, check a table of syntax definitions ) */
/* Subroutine */ int m2chck_(char *statmn, char *synkey, integer *synptr, 
	char *synval, char *error, ftnlen statmn_len, ftnlen synkey_len, 
	ftnlen synval_len, ftnlen error_len)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);

    /* Local variables */
    integer best[16];
    char mssg[160];
    integer b, e, i__, n;
    extern integer cardi_(integer *);
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen);
    logical found;
    extern /* Subroutine */ int meta_2__(char *, char *, integer *, char *, 
	    integer *, char *, ftnlen, ftnlen, ftnlen, ftnlen), fndnwd_(char *
	    , integer *, integer *, integer *, ftnlen);
    integer cutoff;
    extern /* Subroutine */ int bestwd_(char *, char *, integer *, integer *, 
	    integer *, char *, ftnlen, ftnlen, ftnlen);
    integer lookat;
    extern /* Subroutine */ int prefix_(char *, integer *, char *, ftnlen, 
	    ftnlen);
    integer scores[16];
    extern /* Subroutine */ int suffix_(char *, integer *, char *, ftnlen, 
	    ftnlen);
    char keywrd[32];
    extern /* Subroutine */ int ssizei_(integer *, integer *);
    integer mxscor;
    extern logical return_(void);
    logical unknwn;
    extern /* Subroutine */ int syptrc_(char *, char *, integer *, char *, 
	    integer *, integer *, logical *, ftnlen, ftnlen, ftnlen);
    integer bst, ptr;

/* $ Abstract */

/*     Using a symbol table of syntax definition statement indexed by */
/*     initial keyword, determine if the input statement is syntactically */
/*     correct. */

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

/*     META-2 A command definition language and parser. */

/* $ Keywords */

/*     META-2 */
/*     PARSING */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     STATMN     I   A statement to check for syntactic correctness. */
/*     SYNKEY     I   A symbol table of syntax definitions. */
/*     SYNPTR */
/*     SYNVAL */
/*     ERROR      O   Blank if STATMN correct, diagnosis otherwise. */

/* $ Detailed_Input */

/*     STATMN     is a string that is a candidate for a syntactically */
/*                correct statement. */

/*     SYNKEY     is a symbol table.  It is indexed by the initial */
/*     SYNPTR     keywords of META-2 syntax definition statements. */
/*     SYNVAL     This table is best prepared using the routine */
/*                M2INTS. */

/* $ Detailed_Output */

/*     ERROR      is an array of character strings that are used to */
/*                diagnose how well a STATMN matches one of the */
/*                syntax specificiations in the input symbol table. */
/*                If the STATMN is syntactically correct ERROR(1) */
/*                is returned as a blank.  Otherwise it is returned */
/*                with a diagnosis of why STATMN failed to be */
/*                syntactically correct. */

/*                Parsing of STATMN is usually accomplished by using */
/*                the various M2GET routines. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine can be used to compare a statement with a large */
/*     collection of syntax definitions provided all of the definitions */
/*     begin with a keyword.  To make use of this routine, you must first */
/*     prepare the symbol table.  The easiest way to to this is to use */
/*     the routine M2INTS. */

/*     To parse the input statement once it has been determine that it */
/*     is syntactically correct, one can use the M2GET routines to locate */
/*     the various substring corresponding to the meaning of STATMN. */

/* $ Examples */

/*     Typical useage looks like this: */

/*           IF ( FIRST ) THEN */

/*              CALL     M2INTS ( NSYN, SYNKEY, SYNPTR, SYNVAL ) */
/*              FIRST = .FALSE. */

/*           END IF */

/*           CALL M2CHCK ( STATMN, SYNKEY, SYNPTR, SYNVAL, ERROR ) */

/*           IF ( ERROR(1) .NE. ' ' ) THEN */
/*              CALL PREFIX ( 'MYNAME:', 1, ERROR(1) ) */
/*              RETURN */
/*           END IF */

/*           Still here?  Determine what the string actually meant. */

/* $ Restrictions */

/*     To make use of STATMN for parsing with the M2GET routines, you */
/*     should not alter it after the call to M2CHCK until you have */
/*     finished parsing it. */

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


/* -    Beta Version 1.0.0, 4-MAY-1992 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Check a statement against a set of syntax definitions */

/* -& */

/*     Spicelib functions */


/*     Local Variables */

    if (return_()) {
	s_copy(error, "M2CHCK: The function RETURN was set to .TRUE. This si"
		"tuation is not supposed to happen.", error_len, (ftnlen)87);
	return 0;
    }

/*     Initialize the cell BEST and SCORES. */

    ssizei_(&c__10, best);
    ssizei_(&c__10, scores);

/*     Get the first word of the input string. */

    fndnwd_(statmn, &c__1, &b, &e, statmn_len);
    ucase_(statmn + (b - 1), keywrd, e - (b - 1), (ftnlen)32);

/*     Find the syntax templates that match the first word of the */
/*     command. */

    syptrc_(keywrd, synkey, synptr, synval, &ptr, &n, &found, (ftnlen)32, 
	    synkey_len, synval_len);

/*     If we didn't find our word, then we look for a word that */
/*     comes close spelling-wise */

    if (! found) {
	cutoff = 70;
	bestwd_(keywrd, synkey, &cutoff, best, scores, mssg, (ftnlen)32, 
		synkey_len, (ftnlen)160);
	if (cardi_(best) == 0) {
	    unknwn = TRUE_;
	} else if (scores[6] < 50) {
	    unknwn = TRUE_;
	} else {
	    unknwn = FALSE_;
	}
	if (unknwn) {
	    s_copy(error, "Sorry but I didn't recognize the word", error_len, 
		    (ftnlen)37);
	    suffix_(keywrd, &c__1, error, (ftnlen)32, error_len);
	    suffix_("as the beginning of any valid statement. ", &c__1, error,
		     (ftnlen)41, error_len);
	    return 0;
	}

/*        Still here? fetch the set of likely syntax statements to check. */

	mxscor = 0;
	i__1 = cardi_(best);
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (scores[(i__2 = i__ + 5) < 16 && 0 <= i__2 ? i__2 : s_rnge(
		    "scores", i__2, "m2chck_", (ftnlen)269)] > mxscor) {
		mxscor = scores[(i__2 = i__ + 5) < 16 && 0 <= i__2 ? i__2 : 
			s_rnge("scores", i__2, "m2chck_", (ftnlen)270)];
		lookat = i__;
	    }
	}
	s_copy(keywrd, synkey + (best[(i__1 = lookat + 5) < 16 && 0 <= i__1 ? 
		i__1 : s_rnge("best", i__1, "m2chck_", (ftnlen)275)] + 5) * 
		synkey_len, (ftnlen)32, synkey_len);
	syptrc_(keywrd, synkey, synptr, synval, &ptr, &n, &found, (ftnlen)32, 
		synkey_len, synval_len);
    }

/*     Until we find out otherwise, we shall assume that we have */
/*     a syntactically correct input statement. */

    meta_2__(statmn, synval + (ptr + 5) * synval_len, &n, synval, &bst, error,
	     statmn_len, synval_len, synval_len, error_len);
    if (s_cmp(error, " ", error_len, (ftnlen)1) != 0) {
	prefix_("M2CHCK:", &c__1, error + error_len, (ftnlen)7, error_len);
    }
    return 0;
} /* m2chck_ */

