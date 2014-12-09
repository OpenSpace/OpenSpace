/* m2bodini.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure      M2BODINI () */
/* Subroutine */ int m2bodini_(char *names, integer *nnam, integer *codes, 
	integer *ncod, integer *ordnam, integer *ordcod, ftnlen names_len)
{
    integer i__, n;
    extern /* Subroutine */ int orderc_(char *, integer *, integer *, ftnlen),
	     orderi_(integer *, integer *, integer *);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Initialize the two order vectors. This routine should be called */
/*     by M2BODTRN only. */

/*     This routine can not graduate as it is without modifying the */
/*     specification of BSCHOI and BSCHOC. (WLT) */

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


/* $ Keywords */

/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     NAMES     I/O  Array of recognized names */
/*     CODES     I/O  Id-codes to associated with NAMES */
/*     NNAM      I/O  Number of names */
/*     NCOD      I/O  Number if id-codes */
/*     ORDNAM     O   An order vector for NAMES */
/*     ORDCOD     O   An ordered vector for CODES */

/* $ Detailed_Input */

/*     NAMES     is an array of names for whick there is an */
/*               id-code. */

/*     CODES     is an array of id-codes for the items in NAMES.  The */
/*               correspondence is: CODES(I) is the id-code of the body */
/*               named in NAMES(I) */

/*     NNAM      Number of names */

/* $ Detailed_Output */

/*     NCOD      is the number pointers in the ordered pointer array */
/*               ORDCOD */

/*     ORDNAM    is an order vector of integers for NAMES.  The set of */
/*               values NAMES(ORDNAM(1)), NAMES(ORDNAM(2),  ... forms */
/*               an increasing list of names. */

/*     ORDCOD    is an ordering array of integers (as opposed to an */
/*               order vector).  The list CODES(ORDNAM(1)), */
/*               CODES(ORDNAM(2)), ... CODES(ORDNAM(NCOD)) forms an */
/*               increasing non-repeating list of integers.  Moreover, */
/*               every value in CODES is listed exactly once in this */
/*               sequence. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This is a utitility routine used for initializing the ordering */
/*     vectors that point to the recognized names and codes usde by */
/*     the private routine M2BODTRN */

/* $ Examples */

/*     See the routine M2BODTRN. */

/* $ Restrictions */

/*     This routine is intended only for use by M2BODTRN. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov       (JPL) */
/*     M.J. Spencer       (JPL) */
/*     W.L. Taber         (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 25-SEP-1995 (BVS) (WLT) */

/*        Renamed to M2BODINI and filled out the comments on what this */
/*        routine does and how it works. */

/* -& */
/* $ Index_Entries */


/* -& */

/*     Local variables */


/*     Create order vectors ORDNAM and ORDCOD */

    orderc_(names, nnam, ordnam, names_len);
    orderi_(codes, nnam, ordcod);

/*     Remove duplicate entries in the code order table. The entry that */
/*     points to the highest entry in CODES should remain. */

    n = 1;
    i__ = 2;

/*     Now for some very funky manuevering.  We are going to take our */
/*     order vector for the id-codes and modify it! */

/*     Here's what is true now. */

/*     CODES(ORDCOD(1)) <= CODES(ORDCOD(2)) <=...<= CODES(ORDCOD(NNAM) */

/*     For each element such that CODES(ORDCOD(I)) = CODES(ORDCOD(I+1)) */
/*     we are going to "shift" the items ORDCOD(I+1), ORDCOD(I+2), ... */
/*     left by one.  We will then repeat the test and shift as needed. */
/*     When we get done we will have a possibly shorter array ORDCOD */
/*     and the array will satisfy */

/*        CODES(ORDCOD(1)) < CODES(ORDCOD(2)) < ... < CODES(ORDCOD(NNAM) */

/*     We can still use the resulting "ordered vector" (as opposed to */
/*     order vector) in the BSCHOI routine because it only relies */
/*     upon the indexes to ORDCOD and not to CODES itself.  This is */
/*     making very heavy use of the implementation of BSCHOI but we */
/*     are going to let it go for the momemt because this is a private */
/*     routine. */

    while(i__ <= *nnam) {
	if (codes[ordcod[i__ - 1] - 1] == codes[ordcod[n - 1] - 1]) {
	    if (ordcod[i__ - 1] > ordcod[n - 1]) {
		ordcod[n - 1] = ordcod[i__ - 1];
	    }
	} else {
	    ++n;
	    ordcod[n - 1] = ordcod[i__ - 1];
	}
	++i__;
    }
    *ncod = n;
    return 0;
} /* m2bodini_ */

