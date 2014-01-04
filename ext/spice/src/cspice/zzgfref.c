/* zzgfref.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZGFREF ( Private - GF, update REFVAL ) */
/* Subroutine */ int zzgfref_(doublereal *refval)
{
    extern /* Subroutine */ int zzholdd_(char *, doublereal *, ftnlen);

/* $ Abstract */

/*     SPICE private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     Set reference value in the GF sub-system. */

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

/*     GF */

/* $ Keywords */

/*     STORE_VALUE */
/*     GEOMETRY */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     REFVAL     I   The value to set as the reference value */

/* $ Detailed_Input */

/*     REFVAL     the double precision scalar value to set as the */
/*                reference value for a geometry finder search. */

/* $ Detailed_Output */

/*    None. */

/* $ Parameters */

/*    None. */

/* $ Exceptions */

/*    None. */

/* $ Files */

/*    None. */

/* $ Particulars */

/*    This routine wraps a 'PUT' call to ZZHOLDD. */

/* $ Examples */

/*    None. */

/* $ Restrictions */

/*    None. */

/* $ Literature_References */

/*    None. */

/* $ Author_and_Institution */

/*    E.D. Wright    (JPL) */

/* $ Version */

/* -   SPICELIB Version 1.0.0  28-NOV-2009 (EDW) */

/* -& */
/* $ Index_Entries */

/*    store a double precision reference value */

/* -& */

/*     Store the REFVAL value for use in ZZGFUDLT. */

    zzholdd_("PUT", refval, (ftnlen)3);
    return 0;
} /* zzgfref_ */

