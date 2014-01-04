/* syptrc.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure       SYPTRC ( Symbol table, fetch pointers, generic ) */
/* Subroutine */ int syptrc_(char *name__, char *symnam, integer *symptr, 
	char *symval, integer *ptr, integer *n, logical *found, ftnlen 
	name_len, ftnlen symnam_len, ftnlen symval_len)
{
    /* System generated locals */
    integer i__1;
    char ch__1[1];

    /* Local variables */
    extern integer cardc_(char *, ftnlen);
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern integer sumai_(integer *, integer *), bsrchc_(char *, integer *, 
	    char *, ftnlen, ftnlen);
    extern /* Character */ VOID touchc_(char *, ftnlen, char *, ftnlen);
    integer number;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);
    integer loc;

/* $ Abstract */

/*     Return the address of the first value associated with a symbol */
/*     and the number of values associated with the symbol. */

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

/*     SYMBOLS */

/* $ Keywords */

/*     SYMBOLS */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     NAME       I   The name of a symbol. */
/*     SYMNAM     I   The name cell of symbol table. */
/*     SYMPTR     I   The pointer cell of a symbol table. */
/*     SYMVAL     I   The value cell of a symbol table. */
/*     PTR        O   The index of the first value associated with NAME. */
/*     N          O   The number of values associated with NAME. */
/*     FOUND      O   TRUE if NAME is in the symbol table, else FALSE */

/* $ Detailed_Input */

/*     NAME       is a string representing the name of some symbol that */
/*                might be in the symbol table SYMNAM, SYMPTR, ... */

/*     SYMNAM     is a symbol table. */
/*     SYMPTR */
/*     SYMVAL */


/* $ Detailed_Output */

/*     PTR        is the location in the values cell of the symbol table */
/*                where the values associated with NAME begin. */

/*     N          is the number of values in the symbol table */
/*                associated with NAME. */

/*     FOUND      is TRUE if NAME is the name of a symbol. Otherwise, */
/*                it is FALSE. */


/* $ Parameters */


/*     None. */

/* $ Exceptions */

/*     1) If NAME is not present in the symbol table, N and PTR will */
/*        both be returned with the value 0. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine returns the index of the first value associated with */
/*     a particular name in a symbol table.  It also returns the number */
/*     of values associated with the name.  In this way, routines that */
/*     "read" the values associated with a symbol table name, can read */
/*     them directly without having to declare local storage for these */
/*     values. */

/* $ Examples */

/*     Suppose that you need to count the number of values associated */
/*     with NAME that satisfy some property (computed by a logical */
/*     function PROP that you have written). The following block of code */
/*     would do the job. */

/*           COUNT = 0 */

/*           CALL SYPTRC ( NAME, SYMNAM, SYMPTR, SYMVAL, PTR, N, FOUND ) */

/*           DO I = PTR, PTR + N - 1 */

/*              IF ( PROP(SYMVAL(I)) ) THEN */
/*                 COUNT = COUNT + 1 */
/*              END IF */

/*           END DO */


/* $ Restrictions */

/*     User's should not attempt to access values beyond those in the */
/*     range returned returned by this routine. Also, any action that is */
/*     to be performed with the values associated with NAME should */
/*     be performed within a scope in which the symbol table cannot */
/*     be altered by other calls to symbol table routine. */

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

/*     Find pointers to values in a symbol table. */

/* -& */
    if (return_()) {
	return 0;
    } else {
	chkin_("SYPTRC", (ftnlen)6);
    }

/*     We don't use the values of the symbol table in this routine */
/*     but it is passed for the sake of uniformity in the symbol */
/*     table routine calling sequences.  However, some compilers */
/*     generate warnings if a variable isn't used.  So we touch */
/*     the values cell to fake out the compiler. */

    touchc_(ch__1, (ftnlen)1, symval, symval_len);
    *(unsigned char *)&symval[0] = *(unsigned char *)&ch__1[0];

/*     Now for the real work of this routine. */

    number = cardc_(symnam, symnam_len);
    loc = bsrchc_(name__, &number, symnam + symnam_len * 6, name_len, 
	    symnam_len);
    if (loc == 0) {
	*found = FALSE_;
	*ptr = 0;
	*n = 0;
	chkout_("SYPTRC", (ftnlen)6);
	return 0;
    }
    i__1 = loc - 1;
    *ptr = sumai_(&symptr[6], &i__1) + 1;
    *n = symptr[loc + 5];
    *found = TRUE_;
    chkout_("SYPTRC", (ftnlen)6);
    return 0;
} /* syptrc_ */

