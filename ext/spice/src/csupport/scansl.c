/* scansl.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      SCANSL ( Scan --- select tokens ) */
/* Subroutine */ int scansl_(integer *ids, integer *n, integer *ntokns, 
	integer *ident, integer *beg, integer *end)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    integer i__, j;
    extern integer isrchi_(integer *, integer *, integer *);

/* $ Abstract */

/*     Select those tokens descripters whose identities are belong */
/*     to a specific list of identities. */

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

/*     SCANNING */

/* $ Keywords */

/*     SEARCH */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     IDS        I   value of id's of tokens that should be kept. */
/*     N          I   number of id's. */
/*     NTOKNS    I/O  input: number of tokens input, output: number kept. */
/*     IDENT     I/O  identity of each of the tokens. */
/*     BEG       I/O  beginning indices of the tokens. */
/*     END       I/O  ending indices of the tokens. */

/* $ Detailed_Input */

/*     IDS       is a list of the identity codes for tokens that we */
/*               will want to keep. */

/*     N         is the number of identity codes for keepers. */

/*     NTOKNS    is the number of tokens to consider. */

/*     IDENT     holds the identities of each token that is up for */
/*               consideration. */

/*     BEG       holds the beginning indices of each token being */
/*               considered. */

/*     END       holds the ending indicies of each token being */
/*               considered. */

/* $ Detailed_Output */

/*     NTOKNS    is the number of tokens remaining after the selection */
/*               process has been completed. */

/*     IDENT     holds the identities of each token remaining. */

/*     BEG       holds the beginning indices of each token remaining. */

/*     END       holds the ending indices of each token remaining. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine serves as a macro for the selection process that */
/*     is typically performed to select tokens whose ID's fall into */
/*     some set. */

/* $ Examples */

/*     Suppose you wished to scan a string to locate the beginning and */
/*     endings of words in normal text.  The following code fragment */
/*     illustrates how you could use this routine to find the words. */

/*     Words will be delimited by spaces, periods, commas, colons, */
/*     question marks, exclamation marks, semicolons, parentheses, */
/*     m-dashes, and quotes. */

/*     MARKS(1)  = ' ' */
/*     MARKS(2)  = '.' */
/*     MARKS(3)  = ',' */
/*     MARKS(4)  = '?' */
/*     MARKS(5)  = '!' */
/*     MARKS(6)  = '---' */
/*     MARKS(7)  = ':' */
/*     MARKS(8)  = ';' */
/*     MARKS(9)  = '(' */
/*     MARKS(10) = ')' */
/*     MARKS(11) = '"' */

/*     NMARKS    = 11 */

/*     IDS(1)    = 0 */
/*     N         = 1 */


/*     CALL SCANPR ( NMARKS, MARKS,  MRKLEN, MRKPTR ) */

/*     CALL SCAN   ( STRING, MARKS,  MRKLEN, MRKPTR, */
/*    .              ROOM,   NTOKNS, IDENT,  BEG,    END ) */

/*     CALL SCANSL ( IDS, N, NTOKNS, IDENT,  BEG,    END ) */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 28-MAR-1991 (WLT) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     There's not much to do, shift forward the token attributes for */
/*     tokens whose identities belong to the selection list. */

    j = 0;
    i__1 = *ntokns;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (isrchi_(&ident[i__ - 1], n, ids) > 0) {
	    ++j;
	    ident[j - 1] = ident[i__ - 1];
	    beg[j - 1] = beg[i__ - 1];
	    end[j - 1] = end[i__ - 1];
	}
    }
    *ntokns = j;
    return 0;
} /* scansl_ */

