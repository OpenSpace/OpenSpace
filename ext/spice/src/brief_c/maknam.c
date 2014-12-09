/* maknam.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__1 = 1;

/* $Procedure MAKNAM ( GET body NAMe for BRIEF display ) */
/* Subroutine */ int maknam_(integer *object, integer *objsiz, logical *
	namord, char *kertyp, char *objnam, ftnlen kertyp_len, ftnlen 
	objnam_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char name__[48];
    integer cent, i__;
    doublereal x;
    extern /* Subroutine */ int dpfmt_(doublereal *, char *, char *, ftnlen, 
	    ftnlen);
    logical found;
    extern /* Subroutine */ int bodc2n_(integer *, char *, logical *, ftnlen);
    integer frcode;
    extern /* Subroutine */ int ccifrm_(integer *, integer *, integer *, char 
	    *, integer *, logical *, ftnlen), replch_(char *, char *, char *, 
	    char *, ftnlen, ftnlen, ftnlen, ftnlen);
    integer clssid;
    extern /* Subroutine */ int frmnam_(integer *, char *, ftnlen), frinfo_(
	    integer *, integer *, integer *, integer *, logical *);
    integer frclss;
    extern /* Subroutine */ int suffix_(char *, integer *, char *, ftnlen, 
	    ftnlen);

/* $ Abstract */

/*     Construct object name for use in BRIEF's symbol table string */
/*     coverages. */

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

/*     None. */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     OBJECT     I   IDs: body+frameclass or body+center+frameclass */
/*     OBJSIZ     I   Number of elements in OBJECT */
/*     NAMORD     I   Flag indicating whether name ordering is needed */
/*     KERTYP     I   Kernel type: 'SPK', 'PCK' */
/*     NAME       O   Body for use in BRIEF's symbol table for coverages. */

/* $ Detailed_Input */

/*     See Brief_I/O. */

/* $ Detailed_Output */

/*     See Brief_I/O. */

/* $ Parameters */

/*     TBD. */

/* $ Exceptions */

/*     TBD. */

/* $ Files */

/*     TBD. */

/* $ Particulars */

/*     TBD. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     TBD. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov   (JPL) */
/*     W.L. Taber     (JPL) */

/* $ Version */

/* -    BRIEF Version 2.0.0, 05-NOV-2007 (BVS) */

/*        Changed calling sequence to include KERTYP. Changed code to */
/*        map SPK ID for physical object names and PCK frame IDs and */
/*        frame class IDs to frame names. Added header. */

/* -    BRIEF Version 1.0.0, 14-MAR-1996 (WLT) */

/*        Bill's initial version. */

/* -& */
/* $ Index_Entries */

/*     make body name for BRIEF coverage symbol table */

/* -& */

/*     Local parameters. */


/*     Local variables. */

    s_copy(objnam, " ", objnam_len, (ftnlen)1);
    i__1 = *objsiz - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	s_copy(name__, " ", (ftnlen)48, (ftnlen)1);

/*        If name ordering was requested, try to get a name. */

	if (*namord) {

/*           Attemp to map ID to name. */

/*           For SPK 'object' and 'center' IDs, use BODN2C as they are */
/*           IDs of physical objects. */

/*           For PCK 'object' IDs (OBJECT(1)), use CCIFRM as they are */
/*           frame class IDs. */

/*           For PCK 'center' IDs (OBJECT(2)), use FRINFO/FRMNAM as they */
/*           are frame IDs. */

	    if (s_cmp(kertyp, "SPK", kertyp_len, (ftnlen)3) == 0) {
		bodc2n_(&object[i__ - 1], name__, &found, (ftnlen)48);
	    } else if (s_cmp(kertyp, "PCK", kertyp_len, (ftnlen)3) == 0 && 
		    i__ == 1) {
		ccifrm_(&c__2, &object[i__ - 1], &frcode, name__, &cent, &
			found, (ftnlen)48);
	    } else if (s_cmp(kertyp, "PCK", kertyp_len, (ftnlen)3) == 0 && 
		    i__ == 2) {
		frinfo_(&object[i__ - 1], &cent, &frclss, &clssid, &found);
		if (found) {
		    frmnam_(&object[i__ - 1], name__, (ftnlen)48);
		}
	    } else {
		found = FALSE_;
	    }

/*           If ID could not be mapped to name, turn ID into a string. */

	    if (! found) {
		x = (doublereal) object[i__ - 1];
		dpfmt_(&x, "+0XXXXXXXXXXX", name__, (ftnlen)13, (ftnlen)48);
		replch_(name__, "-", "$", name__, (ftnlen)48, (ftnlen)1, (
			ftnlen)1, (ftnlen)48);
	    }
	} else {

/*           If name ordering was not requested, turn ID into a string. */

	    x = (doublereal) object[i__ - 1];
	    dpfmt_(&x, "+0XXXXXXXXXXX", name__, (ftnlen)13, (ftnlen)48);
	    replch_(name__, "-", "$", name__, (ftnlen)48, (ftnlen)1, (ftnlen)
		    1, (ftnlen)48);
	}
	suffix_(name__, &c__1, objnam, (ftnlen)48, objnam_len);
    }
    return 0;
} /* maknam_ */

