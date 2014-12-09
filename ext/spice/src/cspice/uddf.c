/* uddf.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure UDDF ( First derivative of a function, df(x)/dx ) */
/* Subroutine */ int uddf_(S_fp udfunc, doublereal *x, doublereal *dx, 
	doublereal *deriv)
{
    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    doublereal dfdx[1];
    integer n;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal udval[2];
    extern logical failed_(void);
    extern /* Subroutine */ int qderiv_(integer *, doublereal *, doublereal *,
	     doublereal *, doublereal *), chkout_(char *, ftnlen);
    extern logical return_(void);

/* $ Abstract */

/*     Routine to calculate the first derivative of a caller-specified */
/*     scalar function using a three-point estimation. */

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

/*    None. */

/* $ Keywords */

/*    MATH */
/*    DERIVATIVE */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     UDFUNC     I   The routine that computes the scalar value */
/*                    of interest. */
/*     X          I   Independent variable of UDFUNC. */
/*     DX         I   Interval from X for derivative calculation. */
/*     DERIV      O   Approximate derivative of UDFUNC at X. */

/* $ Detailed_Input */

/*     UDFUNC     the routine that returns the value of the scalar */
/*                quantity function of interest at X. The calling */
/*                sequence for UDFUNC is: */

/*                   CALL UDFUNC ( X, VALUE ) */

/*                where: */

/*                   X       the double precision value of the */
/*                           independent variable of the function */
/*                           at which to determine the scalar value. */

/*                   VALUE   the double precision value returned by */
/*                           UDFUNC at X. */

/*                Functionally: */

/*                   VALUE = UDFUNC ( X ) */

/*     X          a scalar double precision value at which to determine */
/*                the derivative of UDFUNC. */

/*                For many SPICE uses, X will represent ephemeris time, */
/*                expressed as seconds past J2000 TDB. */

/*     DX         a scalar double precision value representing half the */
/*                interval in units of X separating the evaluation */
/*                values of UDFUNC; the evaluations occur at (X + DX) */
/*                and (X - DX). */

/*                DX may be negative but must be non-zero. */

/* $ Detailed_Output */

/*     DERIV      the scalar double precision approximate value of the */
/*                first derivative of UDFUNC with respect to X. */

/*                Functionally: */

/*                            d UDFUNC ( x ) | */
/*                   DERIV =  --             | */
/*                            dx             | */
/*                                            X */

/* $ Parameters */

/*    None. */

/* $ Exceptions */

/*     1) A routine in the call tree of this routine signals */
/*        SPICE(DIVIDEBYZERO) if DX has a value of zero. */

/* $ Files */

/*     If the evaluation of UDFUNC requires SPICE kernel data, the */
/*     appropriate kernels must be loaded before calling this routine. */

/*        - SPK data: the calling application must load ephemeris data */
/*          for the targets, observer, and any intermediate objects in */
/*          a chain connecting the targets and observer for the time */
/*          used in the evaluation. If aberration corrections are */
/*          used, the states of target and observer relative to the */
/*          solar system barycenter must be calculable from the */
/*          available ephemeris data. */

/*        - If non-inertial reference frames are used, then PCK */
/*          files, frame kernels, C-kernels, and SCLK kernels may be */
/*          needed. */

/*     Such kernel data are normally loaded once per program run, NOT */
/*     every time this routine is called. */

/* $ Particulars */

/*     This routine provides a simple interface to numerically calculate */
/*     the first derivative of a scalar quantity function, UDFUNC. */
/*     UDFUNC is expected to be "well behaved" across at the evaluation */
/*     interval [ X - DX, X + DX ]. This means a linear approximation to */
/*     the function over the interval is sufficiently accurate to */
/*     calculate the approximate derivative at X. */

/*     The routine QDERIV performs the differentiation using a three */
/*     point estimation. */

/* $ Examples */

/*     The numerical results shown for these examples may differ across */
/*     platforms. The results depend on the SPICE kernels used as */
/*     input, the compiler and supporting libraries, and the machine */
/*     specific arithmetic implementation. */

/*     Calculate the time derivative of the light time corresponding to */
/*     the apparent position of Mercury relative to the Moon at */
/*     time "JAN 1 2009." */

/*           PROGRAM UDDF_T */

/*           EXTERNAL                 UDFUNC */

/*           DOUBLE PRECISION         ET */
/*           DOUBLE PRECISION         DT */
/*           DOUBLE PRECISION         DERIV */

/*     C */
/*     C     Load leapsecond and SPK kernels. The name of the */
/*     C     meta kernel file shown here is fictitious; you */
/*     C     must supply the name of a file available */
/*     C     on your own computer system. */
/*     C */
/*           CALL FURNSH ( 'standard.tm' ) */

/*     C */
/*     C     Use a shift of one second off the epoch of interest. */
/*     C */
/*           DT = 1.D0 */

/*     C */
/*     C     Convert the epoch date string to ephemeris seconds. */
/*     C */
/*           CALL STR2ET ( 'JAN 1 2009', ET ) */

/*     C */
/*     C     Calculate the derivative of UDFUNC at ET. */
/*     C */
/*           CALL UDDF ( UDFUNC, ET, DT, DERIV ) */

/*     C */
/*     C     Output the calculated derivative. */
/*     C */
/*           WRITE(*,*) DERIV */

/*           END */

/*     C */
/*     C     A scalar quantity function that returns the light-time */
/*     C     between the Moon and Mercury at ET. */
/*     C */
/*           SUBROUTINE UDFUNC ( ET, VALUE ) */

/*           IMPLICIT NONE */

/*           DOUBLE PRECISION         ET */
/*           DOUBLE PRECISION         VALUE */

/*           DOUBLE PRECISION         POS  (3) */
/*           DOUBLE PRECISION         LT */

/*     C */
/*     C     Evaluate the apparent position of Mercury with respect */
/*     C     to the Moon at ET. */
/*     C */
/*           CALL SPKPOS ( 'MERCURY', ET, 'J2000', 'LT+S', 'MOON', */
/*          .               POS,   LT ) */

/*     C */
/*     C     Return the light-time value as the scalar quantity. */
/*     C */
/*           VALUE = LT */

/*           END */

/*     The program outputs (OS X Intel run): */

/*         -0.00013567094 */

/* $ Restrictions */

/*    The function UDFUNC must exist everywhere within [X - DX, X + DX]. */

/* $ Literature_References */

/*    See QDERIV header */

/* $ Author_and_Institution */

/*    N.J. Bachman   (JPL) */
/*    E.D. Wright    (JPL) */

/* $ Version */

/* -   SPICELIB Version 1.0.0  31-MAR-2010 (EDW) */

/* -& */
/* $ Index_Entries */

/*    first derivative of a user-defined scalar function */

/* -& */

/*     SPICELIB functions */


/*     Local Variables */

    if (return_()) {
	return 0;
    }
    chkin_("UDDF", (ftnlen)4);

/*     Apply a three-point estimation of the derivative for */
/*     UDFUNC at X by evaluating UDFUNC at [X-DX, X+DX]. */

/*     The QDERIV call returns a single value in the DFDX array. */

    n = 1;

/*     Evaluate the scalar function at the interval boundaries. */
/*     Check for a FAILED event. */

    d__1 = *x - *dx;
    (*udfunc)(&d__1, udval);
    if (failed_()) {
	chkout_("UDDF", (ftnlen)4);
	return 0;
    }
    d__1 = *x + *dx;
    (*udfunc)(&d__1, &udval[1]);
    if (failed_()) {
	chkout_("UDDF", (ftnlen)4);
	return 0;
    }

/*     Estimate the derivative at X. */

    qderiv_(&n, udval, &udval[1], dx, dfdx);
    if (failed_()) {
	chkout_("UDDF", (ftnlen)4);
	return 0;
    }
    *deriv = dfdx[0];
    chkout_("UDDF", (ftnlen)4);
    return 0;
} /* uddf_ */

