/* zzgftreb.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;

/* $Procedure ZZGFTREB ( Geometry finder: return body axes ) */
/* Subroutine */ int zzgftreb_(integer *body, doublereal *axes)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer i__, n;
    extern /* Subroutine */ int chkin_(char *, ftnlen), bodvcd_(integer *, 
	    char *, integer *, integer *, doublereal *, ftnlen), sigerr_(char 
	    *, ftnlen), chkout_(char *, ftnlen), setmsg_(char *, ftnlen), 
	    errint_(char *, integer *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Return the values of the triaxial radii for any body in the */
/*     kernel pool. */

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

/*     CONSTANTS */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     BODY       I   NAIF ID code of body. */
/*     AXES       O   Length of axes of body (1,2,3, as defined below). */

/* $ Detailed_Input */

/*     BODY       is the NAIF ID code of the body for which the axes are */
/*                requested. Bodies are numbered according to the */
/*                standard NAIF numbering scheme described in the */
/*                required reading (naif_ids.req) document. */

/* $ Detailed_Output */

/*     AXES       are the lengths of the axes of the body, in km. */

/*                      AXES(1)  is the longest equatorial radius of */
/*                               the body. For satellites, this axis is */
/*                               typically pointed toward the primary */
/*                               planet. */

/*                      AXES(2)  is the shortest equatorial radius of */
/*                               the body. */

/*                      AXES(3)  is the polar radius of the body. */


/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1)   If the body specified does not have 3 axes defined, */
/*          then the error SPICE(ZEROAXISLENGTH) is signaled. */

/* $ Files */

/*     PCK data:  triaxial radii for the target body must be loaded */
/*     into the kernel pool.  Typically this is done by loading a */
/*     text PCK file via LDPOOL or a general kernel loader */
/*     such as FURNSH. */

/* $ Particulars */

/*     ZZGFTREB returns the lengths of the axes of the target body. */
/*     Appropriate SPK and PCK data must be available to the calling */
/*     program before this routine is called. */

/* $ Examples */

/*     The call */

/*         CALL ZZGFTREB ( 399,  VALUE ) */

/*     returns the values associated with the variable 'BODY399_RADII', */
/*     for example, */

/*          VALUE(1) = 6378.140 */
/*          VALUE(2) = 6378.140 */
/*          VALUE(3) = 6356.755 */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     1) Refer to the SPK required reading file for a complete list of */
/*     the NAIF integer ID codes for bodies. */

/*     2) ''Report of the IAU/IAG/COSPAR Working Group on Cartographic */
/*     Coordinates and Rotational Elements of the Planets and */
/*     Satellites: 1991,'' March 3, 1992. */

/* $ Author_and_Institution */

/*     I.M. Underwood (JPL) */
/*     W.L. Taber     (JPL) */
/*     L.S. Elson     (JPL) */

/* $ Version */

/* -    SPICELIB version 1.0.0  05-MAR-2003 (EDW) */

/* -& */
/* $ Index_Entries */

/*     Return the values of the triaxial radii */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZGFTREB", (ftnlen)8);
    }

/*     Look it up in the kernel pool. */

    bodvcd_(body, "RADII", &c__3, &n, axes, (ftnlen)5);
    if (n != 3) {
	setmsg_("Only # axes were found  for ID # . Three axes are needed.", (
		ftnlen)57);
	errint_("#", &n, (ftnlen)1);
	errint_("#", body, (ftnlen)1);
	sigerr_("SPICE(ZEROAXISLENGTH)", (ftnlen)21);
	chkout_("ZZGFTREB", (ftnlen)8);
	return 0;
    } else {
	for (i__ = 1; i__ <= 3; ++i__) {
	    if (axes[(i__1 = i__ - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("axes",
		     i__1, "zzgftreb_", (ftnlen)185)] < 0.) {
		setmsg_("The # axis of body # is negative.  Please check you"
			"r text PCK file. You should fix the  # component of "
			"the kernel pool variable  BODY#_RADII. ", (ftnlen)142)
			;
		errint_("#", &i__, (ftnlen)1);
		errint_("#", body, (ftnlen)1);
		errint_("#", &i__, (ftnlen)1);
		errint_("#", body, (ftnlen)1);
		sigerr_("SPICE(BADAXISNUMBERS)", (ftnlen)21);
		chkout_("ZZGFTREB", (ftnlen)8);
		return 0;
	    }
	}
    }
    chkout_("ZZGFTREB", (ftnlen)8);
    return 0;
} /* zzgftreb_ */

