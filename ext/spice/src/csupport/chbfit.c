/* chbfit.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__25 = 25;
static integer c__625 = 625;
static integer c__15625 = 15625;

/* $Procedure      CHBFIT ( Chebyshev fit ) */
/* Subroutine */ int chbfit_(D_fp func, doublereal *left, doublereal *right, 
	integer *n, doublereal *work, doublereal *coeffs)
{
    /* Initialized data */

    static logical pass1 = TRUE_;

    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    double cos(doublereal);

    /* Local variables */
    static doublereal rtab[625]	/* was [25][25] */, ttab[15625]	/* was [25][
	    25][25] */;
    integer i__, j, k;
    doublereal x;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errdp_(char *, 
	    doublereal *, ftnlen);
    doublereal midpt;
    extern doublereal pi_(void);
    extern /* Subroutine */ int cleard_(integer *, doublereal *);
    doublereal radius;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen);
    extern logical return_(void);
    doublereal arg;

/* $ Abstract */

/*     Return the Chebyshev coefficients for a Chebyshev expansion */
/*     of a specified function. */

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

/*     INTERPOLATION */
/*     MATH */
/*     POLYNOMIAL */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     MAXSIZ     P   Maximum number of terms in expansion. */
/*     FUNC       I   Function to be approximated. */
/*     LEFT       I   Left endpoint of approximation interval. */
/*     RIGHT      I   Right endpoint of approximation interval. */
/*     N          I   Number of terms in Chebyshev expansion. */
/*     WORK       I   Work space array of dimension N. */
/*     COEFFS     O   Coefficients of Chebyshev expansion. */

/* $ Detailed_Input */

/*     FUNC           is the function to be approximated.  FUNC must */
/*                    accept a single, double precision input argument */
/*                    and must return a double precision value.  FUNC */
/*                    should be declared EXTERNAL in the caller of this */
/*                    routine. */

/*     LEFT, */
/*     RIGHT          are, respectively, the left and right endpoints */
/*                    of the interval on which the input function is */
/*                    to be approximated. */

/*     N              is the number of terms in the desired Chebyshev */
/*                    expansion.  The degree of the highest-order */
/*                    Chebyshev polynomial in the expansion is N-1. */

/*     WORK           is a work space array of dimension N. */


/* $ Detailed_Output */

/*     COEFFS         is an array containing the coefficients of */
/*                    the N-term Chebyshev expansion of the input */
/*                    function. */

/*                    Let */

/*                       T (x)   =  cos ( j arccos(x) ) */
/*                        j */

/*                    be the Chebyshev polynomial of degree j; then */
/*                    COEFFS are computed such that the expansion */

/*                        N */
/*                       ___ */
/*                       \    COEFFS(j)  T   (x) */
/*                       /__              j-1 */

/*                       j=1 */

/*                    is the Chebyshev expansion of F(Y) on the */
/*                    interval [-1,1], where */

/*                       F(Y) =  FUNC(X) */

/*                    and */

/*                               X  -  (LEFT+RIGHT)/2 */
/*                       Y    =  --------------------- */
/*                                 (LEFT-RIGHT) / 2 */

/*                    The coefficients computed by this routine are */
/*                    compatible with the SPICELIB routines CHBINT, */
/*                    CHBVAL, and CHBDER. */

/*                    See the $Particulars section for further details */
/*                    on the specification of this routine. */

/* $ Parameters */

/*     MAXSIZ         is the maximum number of terms in the Chebyshev */
/*                    expansion.  This is the maximum allowed value of */
/*                    N. */

/* $ Exceptions */

/*     1)  If N is less than 1, the error SPICE(INVALIDSIZE) is */
/*         signaled.  The function will return the value 0.D0. */

/*     2)  If N is greater than MAXSIZ, the error SPICE(INVALIDSIZE) is */
/*         signaled.  The function will return the value 0.D0. */

/*     3)  This routine does not attempt to ward off or diagnose */
/*         arithmetic overflows. */

/*     4)  If the endpoints LEFT and RIGHT are not in strictly */
/*         increasing order, the error SPICE(INVALIDENDPTS) */
/*         is signaled. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     The coefficient set produced by this routine is described below: */

/*        Let */

/*           x ,    k = 1, ... , N */
/*            k */

/*        be the roots of the Chebyshev polynomial */

/*           T (x)   =  cos ( N arccos(x) ) */
/*            N */

/*        These roots are */

/*           cos ( (k-1/2)*PI/N ),    k = 1, ..., N. */


/*        For a function f(x) defined on the closed */
/*        interval [-1,1], the N-term Chebyshev expansion */
/*        is */

/*            N */
/*           ___ */
/*           \    C  T   (x) */
/*           /__   j  j-1 */

/*           j=1 */

/*        where */
/*                         N */
/*                        ___ */
/*           C  =  (2/N)  \   f(x ) T   (x ),  j = 2, ...,N, */
/*            j           /__    k   j-1  k */

/*                        k=1 */

/*                         N */
/*                        ___ */
/*           C  =  (1/N)  \   f(x ) */
/*            1           /__    k */

/*                        k=1 */


/*        The definition of */

/*           C */
/*            1 */

/*        used differs from that used in reference [1]; */
/*        our value is half theirs, and yields the simpler */
/*        expression for the expansion of f(x) shown above. */

/*        When the function f(x) to be approximated is */
/*        defined on the interval [LEFT,RIGHT], the mapping */

/*                     x  -  (LEFT+RIGHT)/2 */
/*           y(x)  =  --------------------- */
/*                       (LEFT-RIGHT) / 2 */

/*        can be used to define a new function F such that */
/*        F(y) = f(x).  F has domain [-1,1] and hence admits */
/*        a Chebyshev expansion. */

/*        In this routine, the above mapping is used to */
/*        transform the domain of the input function to the */
/*        interval [-1,1]. */


/* $ Examples */

/*     1)  Recover coefficients from a function whose Chebyshev */
/*         expansion is known.  Suppose */

/*            f(x) = 1*T (x) + 2*T (x) + 3*T (x) + 4*T (x). */
/*                      0         1         2         3 */

/*         The following small program produces the Chebyshev */
/*         coefficients of f: */


/*                  PROGRAM TSTCHB */
/*                  IMPLICIT NONE */
/*            C */
/*            C     Test Chebyshev fitting for a simple function. */
/*            C */
/*                  INTEGER               NCOEFF */
/*                  PARAMETER           ( NCOEFF = 4 ) */

/*                  DOUBLE PRECISION      FUNC */
/*                  EXTERNAL              FUNC */

/*                  DOUBLE PRECISION      COEFFS ( NCOEFF ) */
/*                  DOUBLE PRECISION      WORK   ( NCOEFF ) */
/*                  INTEGER               I */


/*                  CALL CHBFIT ( FUNC,   -1.D0,  1.D0, */
/*                 .              NCOEFF,  WORK,  COEFFS ) */

/*                  WRITE (*,*) 'Coefficients follow:' */

/*                  DO I = 1, NCOEFF */
/*                     WRITE (*,*) 'DEGREE: ', I-1, ' = ', COEFFS(I) */
/*                  END DO */

/*                  END */


/*                  DOUBLE PRECISION FUNCTION FUNC ( X ) */
/*                  IMPLICIT NONE */
/*            C */
/*            C     Return */
/*            C */
/*            C        f(x) = 1*T (x) + 2*T (x) + 3*T (x) + 4*T (x). */
/*            C                  0         1         2         3 */
/*            C */
/*                  DOUBLE PRECISION      X */

/*                  INTEGER               NCOEFF */
/*                  PARAMETER           ( NCOEFF = 4 ) */

/*                  DOUBLE PRECISION      CP  ( NCOEFF ) */
/*                  DOUBLE PRECISION      X2S ( 2 ) */
/*                  INTEGER               I */

/*                  DO I = 1, NCOEFF */
/*                     CP(I) =  DBLE(I) */
/*                  END DO */

/*                  X2S(1) = 0.D0 */
/*                  X2S(2) = 1.D0 */

/*                  CALL CHBVAL ( CP, NCOEFF-1, X2S, X, FUNC ) */
/*                  END */

/* $ Restrictions */

/*     1)  Maximum number of terms in the expansion is limited by the */
/*         parameter MAXSIZ. */

/* $ Literature_References */

/*     [1]  "Numerical Recipes---The Art of Scientific Computing" by */
/*           William H. Press, Brian P. Flannery, Saul A. Teukolsky, */
/*           William T. Vetterling (see section 5.6). */

/* $ Author_and_Institution */

/*     N.J. Bachman   (JPL) */

/* $ Version */

/* -    SUPPORT Version 2.0.0, 14-SEP-2007 (NJB) */

/*        Now pre-computes Chebyvshev polynomial values.  Maximum */
/*        number of terms in the expansion is limited by the */
/*        parameter MAXSIZ. */

/* -    SUPPORT Version 1.0.0, 16-JUN-1996 (NJB) */

/* -& */
/* $ Index_Entries */

/*     fit Chebyshev expansion to a function */
/*     determine Chebyshev coefficients of a function */

/* -& */

/*     SPICELIB functions */


/*     Local parameters */


/*     Local variables */


/*     Saved variables */


/*     Initial values */


/*     Check in only if an error is detected. */

    if (return_()) {
	return 0;
    }

/*     Make sure the requested expansion order is not too large. */

    if (*n > 25) {
	chkin_("CHBFIT", (ftnlen)6);
	setmsg_("The requested expansion order # exceeds the maximum support"
		"ed order #.", (ftnlen)70);
	errint_("#", n, (ftnlen)1);
	errint_("#", &c__25, (ftnlen)1);
	errint_("#", n, (ftnlen)1);
	sigerr_("SPICE(INVALIDSIZE)", (ftnlen)18);
	chkout_("CHBFIT", (ftnlen)6);
	return 0;
    }

/*     No data, no interpolation. */

    if (*n < 1) {
	chkin_("CHBFIT", (ftnlen)6);
	setmsg_("Array size must be positive; was #.", (ftnlen)35);
	errint_("#", n, (ftnlen)1);
	sigerr_("SPICE(INVALIDSIZE)", (ftnlen)18);
	chkout_("CHBFIT", (ftnlen)6);
	return 0;
    }

/*     Make sure the input interval is OK. */

    if (*left >= *right) {
	chkin_("CHBFIT", (ftnlen)6);
	setmsg_("Left endpoint = #; right endpoint = #.", (ftnlen)38);
	errdp_("#", left, (ftnlen)1);
	errdp_("#", right, (ftnlen)1);
	sigerr_("SPICE(INVALIDENDPTS)", (ftnlen)20);
	chkout_("CHBFIT", (ftnlen)6);
	return 0;
    }
    if (pass1) {

/*        On the first pass, compute a table of roots of all */
/*        Cheby polynomials from degree 1 to degree N.  The Ith */
/*        column of the table contains roots of the Ith polynomial. */

	cleard_(&c__625, rtab);
	for (i__ = 1; i__ <= 25; ++i__) {
	    i__1 = i__;
	    for (k = 1; k <= i__1; ++k) {
		rtab[(i__2 = k + i__ * 25 - 26) < 625 && 0 <= i__2 ? i__2 : 
			s_rnge("rtab", i__2, "chbfit_", (ftnlen)439)] = cos(
			pi_() * (k - .5) / i__);
	    }
	}

/*        Also compute a table of Chebyshev function values.  For */
/*        each expansion size J from 1 to N, we compute the values */
/*        of */

/*           T   (x ) ... T   ( x ) */
/*            0    1       0     J */

/*                    . */
/*                    . */
/*                    . */

/*           T   (x ) ... T   ( x ) */
/*            J-1  1       J-1   J */

/*        where */

/*           x */
/*            K */

/*        is the Kth root of */

/*           T */
/*            J */

/*        In our 3-dimensional table, the (K,I,J) entry is the value */
/*        of */

/*           T    ( x  ) */
/*            I-1    K */

/*        where */

/*           x */
/*            K */

/*        is the Kth root of */

/*           T */
/*            J */

	cleard_(&c__15625, ttab);
	for (j = 1; j <= 25; ++j) {

/*           Compute Cheby values needed to implement an expansion */
/*           of size J. */

	    i__1 = j;
	    for (i__ = 1; i__ <= i__1; ++i__) {

/*              Compute values of */

/*                 T */
/*                  I-1 */

/*              on the roots of */

/*                 T */
/*                  J */


		i__2 = j;
		for (k = 1; k <= i__2; ++k) {

/*                 Evaluate */

/*                    T */
/*                     I-1 */

/*                 at the Kth root of */

/*                    T */
/*                     J */

		    arg = pi_() * (k - .5) / j;
		    ttab[(i__3 = k + (i__ + j * 25) * 25 - 651) < 15625 && 0 
			    <= i__3 ? i__3 : s_rnge("ttab", i__3, "chbfit_", (
			    ftnlen)522)] = cos((i__ - 1) * arg);
		}
	    }
	}
	pass1 = FALSE_;
    }

/*     Find the transformation parameters. */

    midpt = (*right + *left) / 2.;
    radius = (*right - *left) / 2.;

/*     Compute the input function values at the transformed Chebyshev */
/*     roots. */

    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {
	x = radius * rtab[(i__2 = k + *n * 25 - 26) < 625 && 0 <= i__2 ? i__2 
		: s_rnge("rtab", i__2, "chbfit_", (ftnlen)550)] + midpt;
	work[k - 1] = (*func)(&x);
    }

/*     Compute the coefficients. */

    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	coeffs[j - 1] = 0.;
	i__2 = *n;
	for (k = 1; k <= i__2; ++k) {
	    coeffs[j - 1] = work[k - 1] * ttab[(i__3 = k + (j + *n * 25) * 25 
		    - 651) < 15625 && 0 <= i__3 ? i__3 : s_rnge("ttab", i__3, 
		    "chbfit_", (ftnlen)565)] + coeffs[j - 1];
	}
	coeffs[j - 1] = coeffs[j - 1] * 2. / *n;
    }

/*     Scale the zero-order coefficient to simplify the form of the */
/*     Chebyshev expansion. */

    coeffs[0] *= .5;
    return 0;
} /* chbfit_ */

