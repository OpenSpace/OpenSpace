/* m2ints.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;

/* $Procedure      M2INTS (Meta 2 --- initialize syntax table) */
/* Subroutine */ int m2ints_(integer *nsyn, char *synkey, integer *synptr, 
	char *synval, ftnlen synkey_len, ftnlen synval_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer b, e, i__;
    extern integer cardc_(char *, ftnlen);
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen), 
	    ljust_(char *, char *, ftnlen, ftnlen), m2shll_(integer *, char *,
	     ftnlen), m2trim_(char *, char *, ftnlen, ftnlen), scardc_(
	    integer *, char *, ftnlen), scardi_(integer *, integer *), 
	    fndnwd_(char *, integer *, integer *, integer *, ftnlen), ssizec_(
	    integer *, char *, ftnlen);
    char keywrd[32];
    extern /* Subroutine */ int ssizei_(integer *, integer *);
    char lstkey[32];
    integer put;

/* $ Abstract */

/*     Construct a symbol table that uses the initial keywords of */
/*     META-2 syntax definitions as the keys to the same a set of */
/*     META-2 syntax definitions. */

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

/*     META-2 A language definition language and parser. */

/* $ Keywords */

/*     INITIALIZATION */
/*     PARSING */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     NSYN       I   The number of syntax definition statements. */
/*     SYNKEY     O   The key (names) portion of a symbol table. */
/*     SYNPTR     O   The pointer portion of a symbol table. */
/*     SYNVAL    I/O  The Meta-2 syntax statements. */

/* $ Detailed_Input */

/*     NSYN       is the number of syntax statements that will be */
/*                organized into an initial keyword based symbol table */

/*     SYNVAL     is a cell containing syntax definintion statements. */
/*                The defitions should be located at indices 1 through */
/*                NSYN. */

/* $ Detailed_Output */

/*     SYNKEY     is the names portion of a symbol table.  The names */
/*                in this array will be the initial keywords of the */
/*                syntax definition statments stored in SYNVAL.  Each */
/*                initial keyword will be associated with those */
/*                collection of definitions that begin with that keyword. */

/*     SYNPTR     is the pointer cell of the symbol table */
/*                SYNKEY, SYNPTR, SYNVAL */

/*     SYNVAL     is the input cell organized now as the values cell */
/*                of the symbol table SYNKEY, SYNPTR, SYNVAL. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     None. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine serves to initialize a syntax cell and list of */
/*     initial known keywords.  This is useful primarily for META2 */
/*     languages that have all syntax definitions beginning with a */
/*     diverse set of keywords.  It is anticipated that users will */
/*     use this once in a module that accepts language statements. */

/*           if ( first ) then */

/*              first = .false. */
/*              call m2intp ( nsyn, synkey, synptr, synval ) */
/*           end if */


/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     This routine is intended only for use with META-2 derived */
/*     languages whose syntax statements all begin with keywords. */
/*     It is assumed that all keywords are 32 or fewer characters */
/*     in length. */

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


/*     Initialize an intial keyword based META-2 syntax table */
/* -& */

/*     Spicelib functions. */


/*     Local variables. */


/*     Initialize the symbol table size attributes. */

    ssizec_(nsyn, synkey, synkey_len);
    ssizei_(nsyn, synptr);
    ssizec_(nsyn, synval, synval_len);

/*     Just in case, left justify everything in the values cell */
/*     and set all of the pointer values to 0. */

    i__1 = *nsyn;
    for (i__ = 1; i__ <= i__1; ++i__) {
	ljust_(synval + (i__ + 5) * synval_len, synval + (i__ + 5) * 
		synval_len, synval_len, synval_len);
	synptr[i__ + 5] = 0;
    }

/*     Turn the collection of syntax definitions into an array ordered */
/*     by initial keyword (minus any labels). */

    m2shll_(nsyn, synval + synval_len * 6, synval_len);

/*     Remove any duplicates including a blank at the beginning if */
/*     there is one. */

    put = 0;
    s_copy(synval + synval_len * 5, " ", synval_len, (ftnlen)1);
    i__1 = *nsyn;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (s_cmp(synval + (i__ + 5) * synval_len, synval + (i__ + 4) * 
		synval_len, synval_len, synval_len) != 0) {
	    ++put;
	    s_copy(synval + (put + 5) * synval_len, synval + (i__ + 5) * 
		    synval_len, synval_len, synval_len);
	}
    }
    ssizec_(nsyn, synval, synval_len);
    scardc_(&put, synval, synval_len);

/*     Now we will construct the symbol table to go with this collection */
/*     of syntax definitions. */

    s_copy(lstkey, " ", (ftnlen)32, (ftnlen)1);
    put = 0;
    i__1 = cardc_(synval, synval_len);
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Get the first word, and trim off any attached label.  Note that */
/*        since this is supposed to be a keyword, there are no range */
/*        templates or qualifiers attached. */

	fndnwd_(synval + (i__ + 5) * synval_len, &c__1, &b, &e, synval_len);
	m2trim_(synval + ((i__ + 5) * synval_len + (b - 1)), keywrd, e - (b - 
		1), (ftnlen)32);
	ucase_(keywrd, keywrd, (ftnlen)32, (ftnlen)32);

/*        If this is a new keyword, put it into the list of keywords and */
/*        change the last keyword. */

	if (s_cmp(keywrd, lstkey, (ftnlen)32, (ftnlen)32) != 0) {
	    ++put;
	    s_copy(synkey + (put + 5) * synkey_len, keywrd, synkey_len, (
		    ftnlen)32);
	    s_copy(lstkey, keywrd, (ftnlen)32, (ftnlen)32);
	}

/*        Increment the value in the pointer array. */

	++synptr[put + 5];
    }

/*     Set the cardinality of the name and pointer cells. */

    scardc_(&put, synkey, synkey_len);
    scardi_(&put, synptr);

/*     Finally, blank out all of the non-used parts of the values cell. */

    for (i__ = -5; i__ <= -2; ++i__) {
	s_copy(synval + (i__ + 5) * synval_len, " ", synval_len, (ftnlen)1);
    }
    return 0;
} /* m2ints_ */

