/* prname.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static integer c__1 = 1;

/* $Procedure PRNAME ( PRintable body NAME for BRIEF display ) */
/* Subroutine */ int prname_(integer *object, integer *sobj, char *p1, char *
	wd, char *p2, integer *size, char *kertyp, char *name__, ftnlen 
	p1_len, ftnlen wd_len, ftnlen p2_len, ftnlen kertyp_len, ftnlen 
	name_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer), i_len(char *, ftnlen);

    /* Local variables */
    integer r__;
    extern integer rtrim_(char *, ftnlen);
    extern /* Subroutine */ int getnam_(integer *, char *, char *, char *, 
	    char *, ftnlen, ftnlen, ftnlen, ftnlen), suffix_(char *, integer *
	    , char *, ftnlen, ftnlen);

/* $ Abstract */

/*     Construct complete printable body name for BRIEF display. */

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

/*     Construct the printname for an object. */

/* $ Required_Reading */

/*     None. */

/* $ Keywords */

/*     None. */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     OBJECT     I   IDs: body+frameclass or body+center+frameclass */
/*     SOBJ       I   Number of elements in OBJECT */
/*     P1         I   Pattern string for body: p1, p2, .. (see brief.pgm) */
/*     WD         I   Separator string (e.g. 'w.r.t') */
/*     P2         I   Pattern string for center: p1, .. (see brief.pgm) */
/*     SIZE       I   1 (if only P1 is set) or 3 (if P1, WD, P2 are set) */
/*     KERTYP     I   Kernel type: 'SPK', 'PCK' */
/*     NAME       O   Complete printable name. */

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

/*     get complete printable body name for BRIEF display */

/* -& */

/*     Get name of the body. */

    s_copy(name__, " ", name_len, (ftnlen)1);
    getnam_(object, p1, kertyp, "OBJECT", name__, p1_len, kertyp_len, (ftnlen)
	    6, name_len);

/*     Indicate non-inertial frame. */

    if (object[(i__1 = *sobj - 1) < 3 && 0 <= i__1 ? i__1 : s_rnge("object", 
	    i__1, "prname_", (ftnlen)145)] != 1) {
	suffix_("*", &c__0, name__, (ftnlen)1, name_len);
    }

/*     If center is included, add center name to the printanle name. */

    if (*size > 1) {
	suffix_(wd, &c__1, name__, wd_len, name_len);
	r__ = rtrim_(name__, name_len) + 2;
	if (r__ < i_len(name__, name_len)) {
	    getnam_(&object[1], p2, kertyp, "CENTER", name__ + (r__ - 1), 
		    p2_len, kertyp_len, (ftnlen)6, name_len - (r__ - 1));
	}
    }
    return 0;
} /* prname_ */

