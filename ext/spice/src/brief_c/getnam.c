/* getnam.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__1 = 1;

/* $Procedure GETNAM ( GET body NAMe for BRIEF display ) */
/* Subroutine */ int getnam_(integer *idcode, char *pattrn, char *kertyp, 
	char *idtype, char *name__, ftnlen pattrn_len, ftnlen kertyp_len, 
	ftnlen idtype_len, ftnlen name_len)
{
    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer cent;
    logical found;
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen), bodc2n_(integer *, char *, logical *, 
	    ftnlen);
    integer frcode;
    extern /* Subroutine */ int ccifrm_(integer *, integer *, integer *, char 
	    *, integer *, logical *, ftnlen), frmnam_(integer *, char *, 
	    ftnlen);
    integer clssid;
    extern /* Subroutine */ int frinfo_(integer *, integer *, integer *, 
	    integer *, logical *);
    integer frclss;
    extern /* Subroutine */ int prefix_(char *, integer *, char *, ftnlen, 
	    ftnlen), suffix_(char *, integer *, char *, ftnlen, ftnlen);
    char string[64];
    extern /* Subroutine */ int intstr_(integer *, char *, ftnlen);

/* $ Abstract */

/*     Construct partial body name for BRIEF display. */

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
/*     IDCODE     I   ID code to be mapped to name. */
/*     PATTRN     I   Pattern string: p1, p2, etc. (see brief.pgm) */
/*     KERTYP     I   Kernel type string: 'SPK' or 'PCK' */
/*     IDTYPE     I   Is ID for object ('OBJECT') or center ('CENTER') */
/*     NAME       O   Name corresponding to ID. */

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

/*        Changed calling sequence to include KERTYP and IDTYPE. Changed */
/*        code to map SPK ID for physical object names and PCK frame IDs */
/*        and frame class IDs to frame names. Added header. */

/* -    BRIEF Version 1.0.0, 14-MAR-1996 (WLT) */

/*        Bill's initial version. */

/* -& */
/* $ Index_Entries */

/*     get partial body name for BRIEF display */

/* -& */

/*     Local parameters. */


/*     Local variables. */


/*     Attemp to map ID to name. */

/*     For SPK 'object' and 'center' IDs, use BODN2C as they are IDs of */
/*     physical objects. */

/*     For PCK 'object' IDs, use CCIFRM as they are frame class IDs. */

/*     For PCK 'center' IDs, use FRINFO/FRMNAM as they are frame IDs. */

    if (s_cmp(kertyp, "SPK", kertyp_len, (ftnlen)3) == 0) {
	bodc2n_(idcode, string, &found, (ftnlen)64);
    } else if (s_cmp(kertyp, "PCK", kertyp_len, (ftnlen)3) == 0 && s_cmp(
	    idtype, "OBJECT", idtype_len, (ftnlen)6) == 0) {
	ccifrm_(&c__2, idcode, &frcode, string, &cent, &found, (ftnlen)64);
    } else if (s_cmp(kertyp, "PCK", kertyp_len, (ftnlen)3) == 0 && s_cmp(
	    idtype, "CENTER", idtype_len, (ftnlen)6) == 0) {
	frinfo_(idcode, &cent, &frclss, &clssid, &found);
	if (found) {
	    frmnam_(idcode, string, (ftnlen)64);
	}
    } else {
	found = FALSE_;
    }

/*     If ID could not be mapped to name, turn ID into a string and */
/*     return. */

    if (! found) {
	intstr_(idcode, name__, name_len);
	return 0;
    }

/*     Depending on requested pattern, append ID to the name. */

    if (s_cmp(pattrn, "p1", pattrn_len, (ftnlen)2) == 0) {
	suffix_("(#)", &c__1, string, (ftnlen)3, (ftnlen)64);
	repmi_(string, "#", idcode, string, (ftnlen)64, (ftnlen)1, (ftnlen)64)
		;
	s_copy(name__, string, name_len, (ftnlen)64);
    } else if (s_cmp(pattrn, "p2", pattrn_len, (ftnlen)2) == 0) {
	intstr_(idcode, name__, name_len);
    } else {
	prefix_("#", &c__1, string, (ftnlen)1, (ftnlen)64);
	repmi_(string, "#", idcode, string, (ftnlen)64, (ftnlen)1, (ftnlen)64)
		;
	s_copy(name__, string, name_len, (ftnlen)64);
    }
    return 0;
} /* getnam_ */

