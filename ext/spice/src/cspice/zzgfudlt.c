/* zzgfudlt.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZGFUDLT ( Private --- GF, scalar function < ref value ) */
/* Subroutine */ int zzgfudlt_(S_fp udfunc, doublereal *et, logical *isless)
{
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal udval;
    extern logical failed_(void);
    doublereal refval;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);
    extern /* Subroutine */ int zzholdd_(char *, doublereal *, ftnlen);

/* $ Abstract */

/*     SPICE private routine intended solely for the support of SPICE */
/*     routines. Users should not call this routine directly due to the */
/*     volatile nature of this routine. */

/*     This routine determines if the value of the scalar quantity */
/*     function is less than a previously defined reference value. */

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

/*     GEOMETRY */
/*     MATH */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     UDFUNC     I   Name of the routine that computes the scalar value */
/*                    of interest. */
/*     ET         I   Time in TDB seconds for which to evaluate UDFUNC. */
/*     ISLESS     O   Boolean indicating if the scalar value is less than */
/*                    reference value. */

/* $ Detailed_Input */

/*     UDFUNC     the routine that returns the value of the scalar */
/*                quantity of interest at time ET. The calling sequence */
/*                for UDFUNC is: */

/*                   CALL UDFUNC ( ET, VALUE ) */

/*                where: */

/*                   ET      a double precision value representing */
/*                           ephemeris time, expressed as seconds past */
/*                           J2000 TDB, at which to determine the scalar */
/*                           value. */

/*                   VALUE   is the value of the geometric quantity */
/*                           at ET. */

/*     ET         a double precision value representing ephemeris time, */
/*                expressed as seconds past J2000 TDB at which to */
/*                determine the value of UDFUNC. */

/* $ Detailed_Output */

/*     ISLESS     a scalar boolean indicating if the value of UDFUNC at */
/*                ET is less than REFVAL (true) or not (false). */

/*                 Functionally: */

/*                   ISLESS = UDFUNC( ET )  <  REFVAL */

/* $ Parameters */

/*    None. */

/* $ Exceptions */

/*     1) ZZHOLDD will signal the error SPICE(ZZHOLDNOPUT) if this */
/*        routine is called prior to storing a reference value */
/*        using a ZZHOLDD "PUT" operation. */

/* $ Files */

/*    None. */

/* $ Particulars */

/*     A ZZHOLDD "PUT" stored the reference value used in the logical */
/*     operation. A ZZHOLDD "GET" retrieves the value. */

/* $ Examples */

/*    See GFUDS. */

/* $ Restrictions */

/*    None. */

/* $ Literature_References */

/*    None. */

/* $ Author_and_Institution */

/*    N.J. Bachman   (JPL) */
/*    E.D. Wright    (JPL) */

/* $ Version */

/* -   SPICELIB Version 1.0.0, 16-FEB-2010 (EDW) */

/* -& */
/* $ Index_Entries */

/*    function less than reference value */

/* -& */

/*     SPICELIB functions */


/*     Local Variables */

    if (return_()) {
	return 0;
    }
    chkin_("ZZGFUDLT", (ftnlen)8);
    *isless = FALSE_;
    (*udfunc)(et, &udval);
    if (failed_()) {
	chkout_("ZZGFUDLT", (ftnlen)8);
	return 0;
    }

/*     Retrieve the reference value. */

    zzholdd_("GET", &refval, (ftnlen)3);
    *isless = udval < refval;
    chkout_("ZZGFUDLT", (ftnlen)8);
    return 0;
} /* zzgfudlt_ */

