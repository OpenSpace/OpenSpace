/* match.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure MATCH ( Match string against multiple wildcard templates ) */
logical match_(char *string, char *templ, ftnlen string_len, ftnlen templ_len)
{
    /* System generated locals */
    logical ret_val;

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    extern logical matchm_(char *, char *, char *, char *, char *, char *, 
	    ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen);
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*      Determines whether or not a string matches any of a */
/*      collection of templates containing wildcard characters. */

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

/* $ Keywords */

/*      SEARCH */
/*      UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*      Variable  I/O  Description */
/*      --------  ---  -------------------------------------------------- */
/*      STRING     I   String to be matched against templates. */
/*      TEMPL      I   Collection of templates. */

/* $ Detailed_Input */

/*      STRING     is a character string to be checked for a match */
/*                 against the specified collection of templates. */
/*                 Leading and trailing blanks are ignored. */

/*      TEMPL      is a collection of individual templates to be */
/*                 compared against the specified string. Leading */
/*                 and trailing blanks are ignored. An empty (blank) */
/*                 template collection matches only an empty (blank) */
/*                 string. */

/* $ Detailed_Output */

/*      The function is TRUE whenever the string matches the collection */
/*      of templates, and is FALSE otherwise. */

/* $ Exceptions */

/*      None. */

/* $ Particulars */

/*      MATCH is exactly equivalent to MATCHM with the special characters */
/*      defined as follows. */

/*            WCHR    = '%' */
/*            WSTR    = '*' */
/*            NOTCHR  = '~' */
/*            ORCHR   = '|' */

/* $ Examples */

/*      1. Normal Templates */
/*      ------------------- */

/*      Consider the following string */

/*         '  ABCDEFGHIJKLMNOPQRSTUVWXYZ ' */

/*      and the following templates. */

/*         Template         Matches STRING? */
/*         ---------------  --------------- */
/*         '*A*'            Yes */
/*         'A%D*'           No */
/*         'A%C*'           Yes */
/*         '%A*'            No */
/*         ' A*   '         Yes */

/*         '%%CD*Z'         Yes */
/*         '%%CD'           No */
/*         'A*MN*Y*Z'       Yes */
/*         'A*MN*Y%Z'       No */
/*         '*BCD*Z*'        Yes */
/*         '*bcd*z*'        No */


/*      2. Negated Templates */
/*      -------------------- */

/*      Consider the same string, and the following templates. */

/*         Template         Matches STRING? */
/*         ---------------  --------------- */
/*         '~%B*D'          Yes */
/*         '~%B*D*'         No */
/*         '~ABC'           Yes */
/*         '~ABC*'          No */
/*         '~~B*'           Yes */

/*      Note that in the final example, the second '~' is treated not as */
/*      a second negation but as an ordinary character. */


/*      3. Combining Templates */
/*      ---------------------- */

/*      Consider the following strings and templates. */

/*           String          Template             Matches? */
/*           --------------  -------------------  -------- */
/*           AKRON           *A*|*B*              Yes */
/*           BELOIT          *B*|*I*              Yes */
/*           CHAMPAGNE       *B*|*I*              No */


/*      4. Combining Negated Templates */
/*      ------------------------------ */

/*      Consider the following strings and templates. */

/*           String          Template             Matches? */
/*           --------------  -------------------  -------- */
/*           SEQUIOA         ~*A*|~*E*|~*I*       No */
/*           SAINT PAUL      ~*A*|~*E*|~*I*       Yes */
/*           HOUSTON         ~*A*|~*E*|~*I*       Yes */


/*      5. Negating Combined Templates */
/*      ------------------------------ */

/*      Consider the following strings and templates. */

/*           String          Template             Matches? */
/*           --------------  -------------------  -------- */
/*           DETROIT         ~|B*|D*              No */
/*           EUGENE          ~|B*|D*              Yes */
/*           FAIRBANKS       ~|*A*|*I*|*O*|*U*    No */
/*           GREENBELT       ~|*A*|*I*|*O*|*U*    Yes */

/* $ Restrictions */

/*      None. */

/* $ Common_Variables */

/*      None. */

/* $ Author_and_Institution */

/*      W.L. Taber     (JPL) */
/*      I.M. Underwood (JPL) */

/* $ Literature_References */

/*      None. */

/* $ Version */


/*      META/2 Configured Version 2.1.0, 28-DEC-1994 (WLT) */

/*         An initial value of FALSE is assigned to MATCH so */
/*         that if we are running in RETURN mode the function */
/*         will have a value. */

/* -     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT) */

/*         This is the configured version of the Command Loop */
/*         software as of May 9, 1994 */


/* -     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT) */

/*         This is the configured version of META/2 */
/*         software as of May 3, 1994 */


/*      Version B 1.0.0, 15-MAY-1988 */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Give the function an intial value of FALSE */

    ret_val = FALSE_;

/*     Standard SPICE error handling */

    if (return_()) {
	return ret_val;
    } else {
	chkin_("MATCH", (ftnlen)5);
    }
    ret_val = matchm_(string, templ, "*", "%", "~", "|", string_len, 
	    templ_len, (ftnlen)1, (ftnlen)1, (ftnlen)1, (ftnlen)1);
    chkout_("MATCH", (ftnlen)5);
    return ret_val;
} /* match_ */

