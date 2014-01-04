/* zzbodker.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__2000 = 2000;

/* $Procedure ZZBODKER ( Private --- Process Body-Name Kernel Pool Maps ) */
/* Subroutine */ int zzbodker_(char *names, char *nornam, integer *codes, 
	integer *nvals, integer *ordnom, integer *ordcod, integer *nocds, 
	logical *extker, ftnlen names_len, ftnlen nornam_len)
{
    /* Initialized data */

    static char nbc[32] = "NAIF_BODY_CODE                  ";
    static char nbn[32] = "NAIF_BODY_NAME                  ";

    /* System generated locals */
    integer i__1, i__2, i__3, i__4, i__5;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    logical drop[2000];
    char type__[1*2];
    integer nsiz[2];
    extern /* Subroutine */ int zzbodini_(char *, char *, integer *, integer *
	    , integer *, integer *, integer *, ftnlen, ftnlen);
    integer i__, j;
    extern /* Subroutine */ int chkin_(char *, ftnlen), ucase_(char *, char *,
	     ftnlen, ftnlen), errch_(char *, char *, ftnlen, ftnlen);
    logical found;
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    logical plfind[2];
    extern /* Subroutine */ int orderc_(char *, integer *, integer *, ftnlen),
	     gcpool_(char *, integer *, integer *, integer *, char *, logical 
	    *, ftnlen, ftnlen), gipool_(char *, integer *, integer *, integer 
	    *, integer *, logical *, ftnlen), sigerr_(char *, ftnlen);
    logical remdup;
    extern /* Subroutine */ int chkout_(char *, ftnlen), dtpool_(char *, 
	    logical *, integer *, char *, ftnlen, ftnlen), setmsg_(char *, 
	    ftnlen), errint_(char *, integer *, ftnlen), cmprss_(char *, 
	    integer *, char *, char *, ftnlen, ftnlen, ftnlen);
    extern logical return_(void);
    integer num[2];

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     This routine processes the kernel pool vectors NAIF_BODY_NAME */
/*     and NAIF_BODY_CODE into the formatted lists required by ZZBODTRN */
/*     to successfully compute code-name mappings. */

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

/*     NAIF_IDS */

/* $ Keywords */

/*     BODY */

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
/*     NAMES      O   Array of kernel pool assigned names. */
/*     NORNAM     O   Array of normalized kernel pool assigned names. */
/*     CODES      O   Array of ID codes for NAMES/NORNAM. */
/*     NVALS      O   Length of NAMES, NORNAM, CODES, and ORDNOM arrays. */
/*     ORDNOM     O   Order vector for NORNAM. */
/*     ORDCOD     O   Modified order vector for CODES. */
/*     NOCDS      O   Length of ORDCOD array. */
/*     EXTKER     O   Logical indicating presence of kernel pool names. */
/*     MAXL       P   Maximum length of body name strings. */
/*     NROOM      P   Maximum length of kernel pool data vectors. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

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
/*               value will never exceed NVALS. */

/*     EXTKER    is a logical that indicates to the caller whether */
/*               any kernel pool name-code maps have been defined. */
/*               If EXTKER is .FALSE., then the kernel pool variables */
/*               NAIF_BODY_CODE and NAIF_BODY_NAME are empty and */
/*               only the built-in and ZZBODDEF code-name mappings */
/*               need consideration.  If .TRUE., then the values */
/*               returned by this module need consideration. */

/* $ Parameters */

/*     MAXL        is the maximum length of a body name.  Defined in */
/*                 the include file 'zzbodtrn.inc'. */

/*     NROOM       is the maximum number of kernel pool data items */
/*                 that can be processed from the NAIF_BODY_CODE */
/*                 and NAIF_BODY_NAME lists. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) The error SPICE(MISSINGKPV) is signaled when one of the */
/*        NAIF_BODY_CODE and NAIF_BODY_NAME keywords is present in the */
/*        kernel pool and the other is not. */

/*     2) The error SPICE(KERVARTOOBIG) is signaled if one or both of */
/*        the NAIF_BODY_CODE and NAIF_BODY_NAME kernel pool vectors */
/*        have a cardinality that exceeds NROOM. */

/*     3) The error SPICE(BADDIMENSIONS) is signaled if the cardinality */
/*        of the NAIF_BODY_CODE and NAIF_BODY_NAME kernel pool vectors do */
/*        not match. */

/*     4) The error SPICE(BLANKNAMEASSIGNED) is signaled if an entry */
/*        in the NAIF_BODY_NAME kernel pool vector is a blank string. */
/*        ID codes may not be assigned to a blank string. */

/* $ Particulars */

/*     This routine examines the contents of the kernel pool, ingests */
/*     the contents of the NAIF_BODY_CODE and NAIF_BODY_NAME keywords, */
/*     and produces the order vectors and name/code lists that ZZBODTRN */
/*     requires to resolve code to name and name to code mappings. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     F.S. Turner    (JPL) */
/*     E.D. Wright    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 23-AUG-2002 (EDW) (FST) */

/* -& */

/*     SPICELIB Functions */


/*     Local Parameters */


/*     Local Variables */


/*     Saved Variables */


/*     Data Statements */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZBODKER", (ftnlen)8);
    }

/*     Until the code below proves otherwise, we shall assume */
/*     we lack kernel pool name/code mappings. */

    *extker = FALSE_;

/*     Check for the external body ID variables in the kernel pool. */

    gcpool_(nbn, &c__1, &c__2000, num, names, plfind, (ftnlen)32, (ftnlen)36);
    gipool_(nbc, &c__1, &c__2000, &num[1], codes, &plfind[1], (ftnlen)32);

/*     Examine PLFIND(1) and PLFIND(2) for problems. */

    if (plfind[0] != plfind[1]) {

/*        If they are not both present or absent, signal an error. */

	setmsg_("The kernel pool vector, #, used in mapping between names an"
		"d ID-codes is absent, while # is not.  This is often due to "
		"an improperly constructed text kernel.  Check loaded kernels"
		" for these keywords.", (ftnlen)199);
	if (plfind[0]) {
	    errch_("#", nbc, (ftnlen)1, (ftnlen)32);
	    errch_("#", nbn, (ftnlen)1, (ftnlen)32);
	} else {
	    errch_("#", nbn, (ftnlen)1, (ftnlen)32);
	    errch_("#", nbc, (ftnlen)1, (ftnlen)32);
	}
	sigerr_("SPICE(MISSINGKPV)", (ftnlen)17);
	chkout_("ZZBODKER", (ftnlen)8);
	return 0;
    } else if (! plfind[0]) {

/*        Return if both keywords are absent. */

	chkout_("ZZBODKER", (ftnlen)8);
	return 0;
    }

/*     If we reach here, then both kernel pool variables are present. */
/*     Perform some simple sanity checks on their lengths. */

    dtpool_(nbn, &found, nsiz, type__, (ftnlen)32, (ftnlen)1);
    dtpool_(nbc, &found, &nsiz[1], type__ + 1, (ftnlen)32, (ftnlen)1);
    if (nsiz[0] > 2000 || nsiz[1] > 2000) {
	setmsg_("The kernel pool vectors used to define the names/ID-codes m"
		"appingexceeds the max size. The size of the NAME vector is #"
		"1. The size of the CODE vector is #2. The max number allowed"
		" of elements is #3.", (ftnlen)198);
	errint_("#1", nsiz, (ftnlen)2);
	errint_("#2", &nsiz[1], (ftnlen)2);
	errint_("#3", &c__2000, (ftnlen)2);
	sigerr_("SPICE(KERVARTOOBIG)", (ftnlen)19);
	chkout_("ZZBODKER", (ftnlen)8);
	return 0;
    } else if (nsiz[0] != nsiz[1]) {
	setmsg_("The kernel pool vectors used for mapping between names and "
		"ID-codes are not the same size.  The size of the name vector"
		", NAIF_BODY_NAME is #. The size of the ID-code vector, NAIF_"
		"BODY_CODE is #. You need to examine the ID-code kernel you l"
		"oaded and correct the mismatch.", (ftnlen)270);
	errint_("#", nsiz, (ftnlen)1);
	errint_("#", &nsiz[1], (ftnlen)1);
	sigerr_("SPICE(BADDIMENSIONS)", (ftnlen)20);
	chkout_("ZZBODKER", (ftnlen)8);
	return 0;
    }

/*     Compute the canonical member of the equivalence class of NAMES, */
/*     NORNAM.  This normalization compresses groups of spaces into a */
/*     single space, left justifies the string, and uppercases the */
/*     contents.  While passing through the NAMES array, look for any */
/*     blank strings and signal an appropriate error. */

    *nvals = num[0];
    i__1 = *nvals;
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Check for blank strings. */

	if (s_cmp(names + ((i__2 = i__ - 1) < 2000 && 0 <= i__2 ? i__2 : 
		s_rnge("names", i__2, "zzbodker_", (ftnlen)345)) * 36, " ", (
		ftnlen)36, (ftnlen)1) == 0) {
	    setmsg_("An attempt to assign the code, #, to a blank string was"
		    " made.  Check loaded text kernels for a blank string in "
		    "the NAIF_BODY_NAME array.", (ftnlen)136);
	    errint_("#", &i__, (ftnlen)1);
	    sigerr_("SPICE(BLANKNAMEASSIGNED)", (ftnlen)24);
	    chkout_("ZZBODKER", (ftnlen)8);
	    return 0;
	}

/*        Compute the canonical member of the equivalence class. */

	ljust_(names + ((i__2 = i__ - 1) < 2000 && 0 <= i__2 ? i__2 : s_rnge(
		"names", i__2, "zzbodker_", (ftnlen)361)) * 36, nornam + ((
		i__3 = i__ - 1) < 2000 && 0 <= i__3 ? i__3 : s_rnge("nornam", 
		i__3, "zzbodker_", (ftnlen)361)) * 36, (ftnlen)36, (ftnlen)36)
		;
	ucase_(nornam + ((i__2 = i__ - 1) < 2000 && 0 <= i__2 ? i__2 : s_rnge(
		"nornam", i__2, "zzbodker_", (ftnlen)362)) * 36, nornam + ((
		i__3 = i__ - 1) < 2000 && 0 <= i__3 ? i__3 : s_rnge("nornam", 
		i__3, "zzbodker_", (ftnlen)362)) * 36, (ftnlen)36, (ftnlen)36)
		;
	cmprss_(" ", &c__1, nornam + ((i__2 = i__ - 1) < 2000 && 0 <= i__2 ? 
		i__2 : s_rnge("nornam", i__2, "zzbodker_", (ftnlen)363)) * 36,
		 nornam + ((i__3 = i__ - 1) < 2000 && 0 <= i__3 ? i__3 : 
		s_rnge("nornam", i__3, "zzbodker_", (ftnlen)363)) * 36, (
		ftnlen)1, (ftnlen)36, (ftnlen)36);
    }

/*     Determine a preliminary order vector for NORNAM. */

    orderc_(nornam, nvals, ordnom, (ftnlen)36);

/*     We are about to remove duplicates.  Make some initial */
/*     assumptions, no duplicates exist in NORNAM. */

    i__1 = *nvals;
    for (i__ = 1; i__ <= i__1; ++i__) {
	drop[(i__2 = i__ - 1) < 2000 && 0 <= i__2 ? i__2 : s_rnge("drop", 
		i__2, "zzbodker_", (ftnlen)377)] = FALSE_;
    }
    remdup = FALSE_;

/*     ORDERC clusters duplicate entries in NORNAM together. */
/*     Use this fact to locate duplicates on one pass through */
/*     NORNAM. */

    i__1 = *nvals - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (s_cmp(nornam + ((i__3 = ordnom[(i__2 = i__ - 1) < 2000 && 0 <= 
		i__2 ? i__2 : s_rnge("ordnom", i__2, "zzbodker_", (ftnlen)389)
		] - 1) < 2000 && 0 <= i__3 ? i__3 : s_rnge("nornam", i__3, 
		"zzbodker_", (ftnlen)389)) * 36, nornam + ((i__5 = ordnom[(
		i__4 = i__) < 2000 && 0 <= i__4 ? i__4 : s_rnge("ordnom", 
		i__4, "zzbodker_", (ftnlen)389)] - 1) < 2000 && 0 <= i__5 ? 
		i__5 : s_rnge("nornam", i__5, "zzbodker_", (ftnlen)389)) * 36,
		 (ftnlen)36, (ftnlen)36) == 0) {

/*           We have at least one duplicate to remove. */

	    remdup = TRUE_;

/*           If the normalized entries are equal, drop the one with */
/*           the lower index in the NAMES array.  Entries defined */
/*           later in the kernel pool have higher precedence. */

	    if (ordnom[(i__2 = i__ - 1) < 2000 && 0 <= i__2 ? i__2 : s_rnge(
		    "ordnom", i__2, "zzbodker_", (ftnlen)401)] < ordnom[(i__3 
		    = i__) < 2000 && 0 <= i__3 ? i__3 : s_rnge("ordnom", i__3,
		     "zzbodker_", (ftnlen)401)]) {
		drop[(i__3 = ordnom[(i__2 = i__ - 1) < 2000 && 0 <= i__2 ? 
			i__2 : s_rnge("ordnom", i__2, "zzbodker_", (ftnlen)
			402)] - 1) < 2000 && 0 <= i__3 ? i__3 : s_rnge("drop",
			 i__3, "zzbodker_", (ftnlen)402)] = TRUE_;
	    } else {
		drop[(i__3 = ordnom[(i__2 = i__) < 2000 && 0 <= i__2 ? i__2 : 
			s_rnge("ordnom", i__2, "zzbodker_", (ftnlen)404)] - 1)
			 < 2000 && 0 <= i__3 ? i__3 : s_rnge("drop", i__3, 
			"zzbodker_", (ftnlen)404)] = TRUE_;
	    }
	}
    }

/*     If necessary, remove duplicates. */

    if (remdup) {

/*        Sweep through the DROP array, compressing off any elements */
/*        that are to be dropped. */

	j = 0;
	i__1 = *nvals;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (! drop[(i__2 = i__ - 1) < 2000 && 0 <= i__2 ? i__2 : s_rnge(
		    "drop", i__2, "zzbodker_", (ftnlen)423)]) {
		++j;
		s_copy(names + ((i__2 = j - 1) < 2000 && 0 <= i__2 ? i__2 : 
			s_rnge("names", i__2, "zzbodker_", (ftnlen)425)) * 36,
			 names + ((i__3 = i__ - 1) < 2000 && 0 <= i__3 ? i__3 
			: s_rnge("names", i__3, "zzbodker_", (ftnlen)425)) * 
			36, (ftnlen)36, (ftnlen)36);
		s_copy(nornam + ((i__2 = j - 1) < 2000 && 0 <= i__2 ? i__2 : 
			s_rnge("nornam", i__2, "zzbodker_", (ftnlen)426)) * 
			36, nornam + ((i__3 = i__ - 1) < 2000 && 0 <= i__3 ? 
			i__3 : s_rnge("nornam", i__3, "zzbodker_", (ftnlen)
			426)) * 36, (ftnlen)36, (ftnlen)36);
		codes[(i__2 = j - 1) < 2000 && 0 <= i__2 ? i__2 : s_rnge(
			"codes", i__2, "zzbodker_", (ftnlen)427)] = codes[(
			i__3 = i__ - 1) < 2000 && 0 <= i__3 ? i__3 : s_rnge(
			"codes", i__3, "zzbodker_", (ftnlen)427)];
	    }
	}

/*        Adjust NVALS to compensate for the number of elements that */
/*        were compressed off the list. */

	*nvals = j;
    }

/*     Compute the order vectors that ZZBODTRN requires. */

    zzbodini_(names, nornam, codes, nvals, ordnom, ordcod, nocds, (ftnlen)36, 
	    (ftnlen)36);

/*     We're on the home stretch if we make it to this point. */
/*     Set EXTKER to .TRUE., check out and return. */

    *extker = TRUE_;
    chkout_("ZZBODKER", (ftnlen)8);
    return 0;
} /* zzbodker_ */

