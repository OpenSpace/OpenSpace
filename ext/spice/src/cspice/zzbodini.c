/* zzbodini.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZBODINI ( Private --- Body-Code Initialization ) */
/* Subroutine */ int zzbodini_(char *names, char *nornam, integer *codes, 
	integer *nvals, integer *ordnom, integer *ordcod, integer *nocds, 
	ftnlen names_len, ftnlen nornam_len)
{
    integer i__, n;
    extern /* Subroutine */ int orderc_(char *, integer *, integer *, ftnlen),
	     orderi_(integer *, integer *, integer *);

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Initialize the two order vectors. This routine should be called */
/*     by ZZBODTRN only. */

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

/*     UTILITY */

/* $ Declarations */
/* $ Abstract */

/*     This include file lists the parameter collection */
/*     defining the number of SPICE ID -> NAME mappings. */

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

/*     naif_ids.req */

/* $ Keywords */

/*     Body mappings. */

/* $ Author_and_Institution */

/*     E.D. Wright (JPL) */

/* $ Version */

/*     SPICELIB 1.0.0 Thu May 20 07:57:58 2010 (EDW) */


/*     A script generates this file. Do not edit by hand. */
/*     Edit the creation script to modify the contents of */
/*     ZZBODTRN.INC. */


/*     Maximum size of a NAME string */


/*     Count of default SPICE mapping assignments. */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     NAMES      I   Array of kernel pool assigned names. */
/*     NORNAM     I   Array of normalized kernel pool assigned names. */
/*     CODES      I   Array of ID codes for NAMES/NORNAM. */
/*     NVALS      I   Length of NAMES, NORNAM, CODES, and ORDNOM arrays. */
/*     ORDNOM     O   Order vector for NORNAM. */
/*     ORDCOD     O   Modified order vector for CODES. */
/*     NOCDS      O   Length of ORDCOD array. */
/*     MAXL       P   Maximum length of body name strings. */

/* $ Detailed_Input */

/*     NAMES     the array of highest precedent names extracted */
/*               from the kernel pool vector NAIF_BODY_NAME.  This */
/*               array is parallel to NORNAM and CODES. */

/*     NORNAM    the array of highest precedent names extracted */
/*               from the kernel pool vector NAIF_BODY_NAME.  After */
/*               extraction, each entry is converted to uppercase, */
/*               and groups of spaces are compressed to a single */
/*               space.  This represents the canonical member of the */
/*               equivalence class each parallel entry in NAMES */
/*               belongs. */

/*     CODES     the array of highest precedent codes extracted */
/*               from the kernel pool vector NAIF_BODY_CODE.  This */
/*               array is parallel to NAMES and NORNAM. */

/*     NVALS     the number of items contained in NAMES, NORNAM, */
/*               CODES and ORDNOM. */

/* $ Detailed_Output */

/*     ORDNOM    the order vector of indexes for NORNAM.  The set */
/*               of values NORNAM( ORDNOM(1) ), NORNAM( ORDNOM(2) ), */
/*               ... forms an increasing list of name values. */

/*     ORDCOD    the modified ordering vector of indexes into */
/*               CODES.  The list CODES( ORDCOD(1) ), */
/*               CODES( ORDCOD(2) ), ... , CODES( ORDCOD(NOCDS) ) */
/*               forms an increasing non-repeating list of integers. */
/*               Moreover, every value in CODES is listed exactly */
/*               once in this sequence. */

/*     NOCDS     the number of indexes listed in ORDCOD.  This */
/*               value will never exceed NVALS.C */

/* $ Parameters */

/*     MAXL        is the maximum length of a body name.  Defined in */
/*                 the include file 'zzbodtrn.inc'. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This is a utility routine used for initializing the ordering */
/*     vectors that point to the recognized names and codes used by */
/*     the private routine ZZBODTRN. */

/* $ Examples */

/*     See the routine ZZBODTRN. */

/* $ Restrictions */

/*     1) This routine is intended only for use by ZZBODTRN. */

/*     2) NAMES and NORNAM must contain only unique entries. */
/*        If duplicate entries exist, this routine may not */
/*        perform as expected. */

/*     3) This routine relies rather heavily on the implementation of */
/*        BSCHOI.  The specification of BSCHOI requires an order vector */
/*        as input, however it turns out that a generalization of an */
/*        order vector (as defined by this routine) will work as well. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov       (JPL) */
/*     M.J. Spencer       (JPL) */
/*     W.L. Taber         (JPL) */
/*     F.S. Turner        (JPL) */
/*     E.D. Wright        (JPL) */

/* $ Version */

/* -    SPICELIB Version 3.0.0, 23-AUG-2002 (FST) */

/*        Implemented changes to support the new precedence */
/*        system. */

/*        Altered the calling sequence of ZZBODINI to remove */
/*        unused arguments.  This routine also no longer computes */
/*        NORNAM from NAMES, since it is used in a more general */
/*        capacity. */

/*        Updated module header and comments to document additional */
/*        assumptions this module now makes about its inputs. */

/*        This routine is now error free. */

/* -    SPICELIB Version 2.1.1, 07-MAR-2002 (EDW) */

/*        Modified error logic to allow duplicate */
/*        NAME -> CODE mappings without signaling an error. */
/*        The mapping operation is a no-op, but might */
/*        cause a user problems if an error signals. */

/* -    SPICELIB Version 2.1.0, 12-AUG-2001 (EDW) */

/*        Modified logic for all ZZBOD routines to function with */
/*        equivalence class concept. A body name now exists */
/*        as a member of an equivalence class named by the */
/*        normalized form of the body name. To facilitate this */
/*        concept, an addition name vector, NORNAM, and */
/*        order vector, ORDNOM, now exist. */

/* -    SPICELIB Version 1.0.0, 25-SEP-1995 (BVS) (WLT) */

/*        Renamed to ZZBODINI and filled out the comments on what this */
/*        routine does and how it works. */

/* -& */

/*     Local Variables */


/*     Create the order vectors ORDCOD and ORDNOM. */

    orderc_(nornam, nvals, ordnom, (ftnlen)36);
    orderi_(codes, nvals, ordcod);

/*     Remove duplicate entries in the code order table. The entry that */
/*     points to the highest entry in CODES should remain. */

    n = 1;
    i__ = 2;

/*     Now for some very funky maneuvering.  We are going to take our */
/*     order vector for the id-codes and modify it! */

/*     Here's what is true now. */

/*       CODES(ORDCOD(1)) <= CODES(ORDCOD(2)) <=...<= CODES(ORDCOD(NVALS) */

/*     For each element such that CODES(ORDCOD(I)) = CODES(ORDCOD(I+1)) */
/*     we are going to "shift" the items ORDCOD(I+1), ORDCOD(I+2), ... */
/*     left by one.  We will then repeat the test and shift as needed. */
/*     When we get done we will have a possibly shorter array ORDCOD */
/*     and the array will satisfy */

/*       CODES(ORDCOD(1)) < CODES(ORDCOD(2)) < ... < CODES(ORDCOD(NVALS) */

/*     We can still use the resulting "ordered vector" (as opposed to */
/*     order vector) in the BSCHOI routine because it only relies */
/*     upon the indexes to ORDCOD and not to CODES itself.  This is */
/*     making very heavy use of the implementation of BSCHOI but we */
/*     are going to let it go for the moment because this is a private */
/*     routine. */

    while(i__ <= *nvals) {
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
    *nocds = n;
    return 0;
} /* zzbodini_ */

