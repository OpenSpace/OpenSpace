/* dpspce.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b19 = .66666666666666663;
static doublereal c_b20 = 3.5;
static doublereal c_b22 = 1.5;
static doublereal c_b23 = 1.;
static doublereal c_b25 = 0.;

/* $Procedure DPSPCE ( Propagate a two line element set for deep space ) */
/* Subroutine */ int dpspce_(doublereal *time, doublereal *geophs, doublereal 
	*elems, doublereal *state)
{
    /* Initialized data */

    static logical doinit = TRUE_;
    static logical first = TRUE_;

    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1, d__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    double pow_dd(doublereal *, doublereal *), cos(doublereal), sqrt(
	    doublereal), sin(doublereal), d_mod(doublereal *, doublereal *), 
	    atan2(doublereal, doublereal);

    /* Local variables */
    static doublereal coef, eeta, aodp, delo, capu, uang, xmdf, xinc, xmam, 
	    aynl, elsq, temp;
    static logical cont;
    static doublereal rdot, cosu, sinu, coef1, t2cof, temp1, temp2, temp3, 
	    temp4, temp5, cos2u, temp6;
    extern /* Subroutine */ int zzdpinit_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *);
    static doublereal sin2u, a, e;
    static integer i__;
    static doublereal m[3], n[3], s, u[3], v[3], betal, scale, betao;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static doublereal epoch, ecose, aycof, esine, a3ovk2, tempa, tempe, bstar,
	     cosio, xincl, etasq, rfdot, sinio, a1, rdotk, c1, c2, cosuk, c4, 
	    qoms24, sinuk, templ, x1m5th, x1mth2, x3thm1, x7thm1, psisq, 
	    xinck, xlcof, xmdot, xnode, xnodp;
    extern doublereal twopi_(void);
    static doublereal s4;
    extern /* Subroutine */ int vlcom_(doublereal *, doublereal *, doublereal 
	    *, doublereal *, doublereal *);
    static doublereal betao2, theta2, ae, xhdot1, ao, em, eo, qoms2t, pl, 
	    omgadf, rk, qo, uk, so;
    extern doublereal halfpi_(void);
    static doublereal xl, xn, omegao;
    extern /* Subroutine */ int latrec_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    static doublereal perige, xnodcf, xnoddf, tsince, xnodek, omgdot, rfdotk, 
	    xnodeo;
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    static doublereal ck2, lstelm[10], ck4, cosepw, sinepw, xkmper, xnodot, 
	    lstphs[8];
    extern logical return_(void);
    static doublereal pinvsq, xj2, xj3, xj4, eta, axn, xke, ayn, epw, tsi, 
	    xll, xmo, xno, tsq, xlt, del1;
    extern /* Subroutine */ int zzdpsec_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    static doublereal pio2;
    extern /* Subroutine */ int zzdpper_(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *);
    static doublereal pix2;

/* $ Abstract */

/*     This routine propagates NORAD two-line element data for */
/*     earth orbiting deep space vehicles (a vehicle with an */
/*     orbital period more than 225 minutes). */

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

/*     EPHEMERIS */
/*     TWO LINE ELEMENTS */
/*     DEEP SPACE PROPAGATOR */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     TIME       I   Time for state evaluation in seconds past ephemeris */
/*                    epoch J2000. */
/*     GEOPHS     I   The array of geophysical constants */
/*     ELEMS      I   Array of orbit elements */
/*     STATE      O   State vector at TIME */

/* $ Detailed_Input */

/*     TIME        is the epoch in seconds past ephemeris epoch J2000 */
/*                 to produced a state from the input elements. */

/*     GEOPHS      is a collection of 8 geophysical constants needed */
/*                 for computing a state.  The order of these */
/*                 constants must be: */

/*                 GEOPHS(1) = J2 gravitational harmonic for earth */
/*                 GEOPHS(2) = J3 gravitational harmonic for earth */
/*                 GEOPHS(3) = J4 gravitational harmonic for earth */

/*                 These first three constants are dimensionless. */

/*                 GEOPHS(4) = KE: Square root of the GM for earth where */
/*                             GM is expressed in earth radii cubed per */
/*                             minutes squared. */

/*                 GEOPHS(5) = QO: Low altitude bound for atmospheric */
/*                             model in km. */

/*                 GEOPHS(6) = SO: High altitude bound for atmospheric */
/*                             model in km. */


/*                 GEOPHS(7) = RE: Equatorial radius of the earth in km. */


/*                 GEOPHS(8) = AE: Distance units/earth radius */
/*                             (normally 1) */

/*                 Below are currently recommended values for these */
/*                 items: */

/*                   J2 =    1.082616D-3 */
/*                   J3 =   -2.53881D-6 */
/*                   J4 =   -1.65597D-6 */

/*                 The next item is the square root of GM for the */
/*                 earth given in units of earth-radii**1.5/Minute */

/*                   KE =    7.43669161D-2 */

/*                 The next two items define the top and */
/*                 bottom of the atmospheric drag model */
/*                 used by the type 10 ephemeris type. */
/*                 Don't adjust these unless you understand */
/*                 the full implications of such changes. */

/*                   QO =  120.0D0 */
/*                   SO =   78.0D0 */

/*                 The ER value is the equatorial radius in km */
/*                 of the earth as used by NORAD. */

/*                   ER = 6378.135D0 */

/*                 The value of AE is the number of */
/*                 distance units per earth radii used by */
/*                 the NORAD state propagation software. */
/*                 The value is 1 unless you've got */
/*                 a very good understanding of the NORAD */
/*                 routine SGP4 and the affect of changing */
/*                 this value.. */

/*                   AE =    1.0D0 */

/*     ELEMS       is an array containing two-line element data */
/*                 as prescribed below. The elements XNDD6O and BSTAR */
/*                 must have been scaled by the proper exponent stored */
/*                 in the two line elements set.  Moreover, the */
/*                 various items must be converted to the units shown */
/*                 here. */

/*                    ELEMS (  1 ) = XNDT2O in radians/minute**2 */
/*                    ELEMS (  2 ) = XNDD6O in radians/minute**3 */
/*                    ELEMS (  3 ) = BSTAR */
/*                    ELEMS (  4 ) = XINCL  in radians */
/*                    ELEMS (  5 ) = XNODEO in radians */
/*                    ELEMS (  6 ) = EO */
/*                    ELEMS (  7 ) = OMEGAO in radians */
/*                    ELEMS (  8 ) = XMO    in radians */
/*                    ELEMS (  9 ) = XNO    in radians/minute */
/*                    ELEMS ( 10 ) = EPOCH of the elements in seconds */
/*                                   past ephemeris epoch J2000. */

/* $ Detailed_Output */

/*     STATE       A 6 vector containing the X, Y, Z, Vx, Vy, Vz */
/*                 coordinates in the inertial frame (double */
/*                 precision). */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This subroutine is an extensive rewrite of the SDP4 */
/*     routine as described in the Spacetrack 3 report.  All common */
/*     blocks were removed and all variables are explicitly defined. */

/*     The removal of common blocks causes the set of routines to */
/*     execute slower than the original version of SDP4.  However the */
/*     stability improves especially as concerns memory and */
/*     expanded internal documentation. */

/*     Trivial or redundant variables have been eliminated. */

/*       R         removed, occurrence replaced with RK */
/*       E6A       renamed TOL */
/*       THETA4    removed, relevant equation recast in Horner's form */
/*                 i.e. something like x^4 + x^2 -> x^2 ( x^2 + 1 ) */
/*       U         renamed UANG, U is now a euclidean 3 vector. */
/*       Ux,Uy,Uz  removed, replaced with 3-vector U */
/*       Vx,Vy,Vz  removed, replaced with 3-vector V */
/*       OMEGAQ    removed, usage replaced with OMEGAO */
/*       OMGDT     removed, same variable as OMGDOT, so all occurrences */
/*                 replaced with OMGDOT */
/*       SSL,SSG   replaced with the 5-vector SSX */
/*       SSH,SSE */
/*       SSI */

/*     Three functions present in the original Spacetrack report, ACTAN, */
/*     FMOD2P and THETAG, have been either replaced with an intrinsic */
/*     FORTRAN function (ACTAN -> DATAN2, FMOD2P -> DMOD) or recoded */
/*     using SPICELIB calls (THETAG). */

/*     The code at the end of this subroutine which calculates */
/*     orientation vectors, was replaced with a set of calls to */
/*     SPICELIB vector routines. */

/*     A direct comparison of output from the original Spacetrack 3 code */
/*     and these NAIF routines for the same elements and time parameters */
/*     will produce unacceptably different results. */

/* $ Examples */


/*   C---  Load the geophysical constants kernel and the leapsecond */
/*         kernel */
/*         CALL FURNSH( '/Users/ewright/lib/geophysical.ker' ) */
/*         CALL FURNSH( '/kernels/gen/lsk/naif0008.tls' ) */


/*   C---  Define a vehicle element array, TDRS 4 Geosynch */
/*         LINES( 1 ) = '1 19883U 89021B   97133.05943164 -.00000277  ' */
/*        .//           '00000-0  10000-3 0  3315' */
/*         LINES( 2 ) = '2 19883   0.5548  86.7278 0001786 312.2904 ' */
/*        .//           '172.2391  1.00269108202415' */


/*   C---  Identify the earliest first year for the elements */
/*         FRSTYR = 1988 */


/*   C---  Parse the elements to something SPICE can use */
/*         CALL GETELM ( FRSTYR, LINES, EPOCH, ELEMS ) */


/*   C---  Final time past epoch, 1400 mins (in seconds) */
/*         TF     = 1440.D0 * 60.D0 */

/*   C---  Step size for elements output 360 mins (in seconds) */
/*         DELT   = 360.D0  * 60.D0 */

/*   C---  Start time keyed off epoch */
/*         TIME   = EPOCH - 2.D0 * DELT */

/*         DO WHILE ( DABS(TIME - EPOCH) .LE. DABS(TF) ) */

/*            CALL DPSPCE ( TIME, GEOPHS, ELEMS, STATE ) */

/*            WRITE(*, FMT ='(7F17.8)' ) (TIME-EPOCH)/60.D0, */
/*        .                              (STATE(I),I=1,6) */

/*            TIME = TIME + DELT */

/*         END DO */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     Hoots, Felix R., Ronald L. Roehrich (31 December 1988). "Models */
/*     for Propagation of NORAD Element Sets". United States Department */
/*     of Defense Spacetrack Report (3). */

/* $ Author_and_Institution */

/*     E.D. Wright      (JPL) */

/* $ Version */

/* -    SPICELIB Version 2.0.0, 23-JAN-2013 (EDW) */

/*        Corrected initialization block error. The ZZDPINIT call */
/*        causes a side-effect required for each DPSPCE call. */
/*        The ZZDPINIT call now occurs outside the initialization */
/*        block. Note from designer, side-effects are bad. */

/*        Added proper citation for Hoots paper. */

/* -    SPICELIB Version 1.2.2, 22-AUG-2006 (EDW) */

/*        Replaced references to LDPOOL with references */
/*        to FURNSH. */

/* -    SPICELIB Version 1.2.1, DEC-27-2000 (EDW) */

/*       Corrected error in header documentation. Horner's Rule */
/*       not Butcher's. */

/* -    SPICELIB Version 1.2.0, MAR-24-1999 (EDW) */

/*       Documentation expanded to include modifications made */
/*       to private routines.  Some english errors corrected. */

/*       Alphabetized variable declaration lists. */

/*       Temporary variable TEMP removed.  OMGDOT argument added to */
/*       ZZDPSEC call. */

/* -    SPICELIB Version 1.1.0, OCT-05-1998 (WLT) */

/*        Forced initialization section until we can figure out */
/*        why it doesn't work on SUNs. */

/* -    SPICELIB Version 1.0.1, MAR-11-1998 (EDW) */

/*       Corrected error in header describing GEOPHS array. */

/* -    SPICELIB Version 1.0.0, NOV-11-1998 (EDW) */

/* -& */
/* $ Index_Entries */

/*     NORAD two line elements deep space evaluator */

/* -& */

/*     Local variables */


/*     Define parameters for convergence tolerance and the value for 2/3, */
/*     0 and 1. */


/*     The geophysical Quantities */


/*     Elements */


/*     Other quantities */


/*     SPICELIB routines */


/*     Save everything. */


/*     Set initialization flags */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("DPSPCE", (ftnlen)6);
    }

/*     If this is the very first time into this routine, set these */
/*     values. */

    if (first) {
	pix2 = twopi_();
	pio2 = halfpi_();
	first = FALSE_;
    }

/*     If initialization flag is FALSE, then this is not the first */
/*     call to this routine.  Check the stuff. */

    if (! doinit) {

/*        Check whether the current and last constants and elements */
/*        match.  If not, we need to reinitialize everything */
/*        since the propagation is dependent on the value of these */
/*        arrays. */

	for (i__ = 1; i__ <= 8; ++i__) {
	    if (lstphs[(i__1 = i__ - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge(
		    "lstphs", i__1, "dpspce_", (ftnlen)547)] != geophs[(i__2 =
		     i__ - 1) < 8 && 0 <= i__2 ? i__2 : s_rnge("geophs", i__2,
		     "dpspce_", (ftnlen)547)]) {
		doinit = TRUE_;
	    }
	}
	for (i__ = 1; i__ <= 10; ++i__) {
	    if (lstelm[(i__1 = i__ - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge(
		    "lstelm", i__1, "dpspce_", (ftnlen)556)] != elems[(i__2 = 
		    i__ - 1) < 10 && 0 <= i__2 ? i__2 : s_rnge("elems", i__2, 
		    "dpspce_", (ftnlen)556)]) {
		doinit = TRUE_;
	    }
	}
    }

/*     Initialization block.  Always called on the initial entry and */
/*     anytime the geophysical or elements array changes. */

    if (doinit) {
	doinit = FALSE_;

/*        Retrieve the geophysical constants from the GEOPHS array */

	xj2 = geophs[0];
	xj3 = geophs[1];
	xj4 = geophs[2];
	xke = geophs[3];
	qo = geophs[4];
	so = geophs[5];
	xkmper = geophs[6];
	ae = geophs[7];

/*        Save the geophysical constants for later comparison */

	for (i__ = 1; i__ <= 8; ++i__) {
	    lstphs[(i__1 = i__ - 1) < 8 && 0 <= i__1 ? i__1 : s_rnge("lstphs",
		     i__1, "dpspce_", (ftnlen)590)] = geophs[(i__2 = i__ - 1) 
		    < 8 && 0 <= i__2 ? i__2 : s_rnge("geophs", i__2, "dpspce_"
		    , (ftnlen)590)];
	}

/*        Unpack the elements array. */

	bstar = elems[2];
	xincl = elems[3];
	xnodeo = elems[4];
	eo = elems[5];
	omegao = elems[6];
	xmo = elems[7];
	xno = elems[8];
	epoch = elems[9];

/*        Save the elements for later comparison */

	for (i__ = 1; i__ <= 10; ++i__) {
	    lstelm[(i__1 = i__ - 1) < 10 && 0 <= i__1 ? i__1 : s_rnge("lstelm"
		    , i__1, "dpspce_", (ftnlen)610)] = elems[(i__2 = i__ - 1) 
		    < 10 && 0 <= i__2 ? i__2 : s_rnge("elems", i__2, "dpspce_"
		    , (ftnlen)610)];
	}

/*        Set common variables, the init flag and calculate the */
/*        WGS-72 physical and geopotential constants */

/*        CK2 =  0.5   * J2 * AE^2 */
/*        CK4 = -0.375 * J4 * AE^4 */

/*        These are values calculated only once and then saved for */
/*        future access. */

/* Computing 2nd power */
	d__1 = ae;
	ck2 = xj2 * .5 * (d__1 * d__1);
/* Computing 4th power */
	d__1 = ae, d__1 *= d__1;
	ck4 = xj4 * -.375 * (d__1 * d__1);
/* Computing 4th power */
	d__1 = (qo - so) * ae / xkmper, d__1 *= d__1;
	qoms2t = d__1 * d__1;
	s = ae * (so / xkmper + 1.);

/*        Recover original mean motion (XNODP) and semimajor axis (AODP) */
/*        from input elements */

	d__1 = xke / xno;
	a1 = pow_dd(&d__1, &c_b19);
	cosio = cos(xincl);
/* Computing 2nd power */
	d__1 = cosio;
	theta2 = d__1 * d__1;
	x3thm1 = theta2 * 3. - 1.;
/* Computing 2nd power */
	d__1 = eo;
	betao2 = 1. - d__1 * d__1;
	betao = sqrt(betao2);
/* Computing 2nd power */
	d__1 = a1;
	del1 = ck2 * 1.5 * x3thm1 / (d__1 * d__1 * betao * betao2);
	ao = a1 * (1. - del1 * (del1 * (del1 * 1.654320987654321 + 1.) + 
		.33333333333333331));
/* Computing 2nd power */
	d__1 = ao;
	delo = ck2 * 1.5 * x3thm1 / (d__1 * d__1 * betao * betao2);
	xnodp = xno / (delo + 1.);
	aodp = ao / (1. - delo);

/*        For perigee below 156 km, the values of S and QOMS2T are */
/*        altered */

	s4 = s;
	qoms24 = qoms2t;
	perige = (aodp * (1. - eo) - ae) * xkmper;
	if (perige < 156.) {
	    s4 = perige - 78.;
	    if (perige > 98.) {
/* Computing 4th power */
		d__1 = (120. - s4) * ae / xkmper, d__1 *= d__1;
		qoms24 = d__1 * d__1;
		s4 = s4 / xkmper + ae;
	    } else {
		s4 = 20.;
	    }
	}
/* Computing 2nd power */
	d__1 = aodp;
/* Computing 2nd power */
	d__2 = betao2;
	pinvsq = 1. / (d__1 * d__1 * (d__2 * d__2));
	tsi = 1. / (aodp - s4);
	eta = aodp * eo * tsi;
/* Computing 2nd power */
	d__1 = eta;
	etasq = d__1 * d__1;
	eeta = eo * eta;
	psisq = (d__1 = 1. - etasq, abs(d__1));
/* Computing 4th power */
	d__1 = tsi, d__1 *= d__1;
	coef = qoms24 * (d__1 * d__1);
	coef1 = coef / pow_dd(&psisq, &c_b20);
	c2 = coef1 * xnodp * (aodp * (etasq * 1.5 + 1. + eeta * (etasq + 4.)) 
		+ ck2 * .75 * tsi / psisq * x3thm1 * (etasq * 3. * (etasq + 
		8.) + 8.));
	c1 = bstar * c2;
	sinio = sin(xincl);
/* Computing 3rd power */
	d__1 = ae;
	a3ovk2 = -xj3 / ck2 * (d__1 * (d__1 * d__1));
	x1mth2 = 1. - theta2;
	c4 = xnodp * 2. * coef1 * aodp * betao2 * (eta * (etasq * .5 + 2.) + 
		eo * (etasq * 2. + .5) - ck2 * 2. * tsi / (aodp * psisq) * (
		x3thm1 * -3. * (1. - eeta * 2. + etasq * (1.5 - eeta * .5)) + 
		x1mth2 * .75 * (etasq * 2. - eeta * (etasq + 1.)) * cos(
		omegao * 2.)));
	temp1 = ck2 * 3. * pinvsq * xnodp;
	temp2 = temp1 * ck2 * pinvsq;
	temp3 = ck4 * 1.25 * pinvsq * pinvsq * xnodp;
	xmdot = xnodp + temp1 * .5 * betao * x3thm1 + temp2 * .0625 * betao * 
		(theta2 * (theta2 * 137. - 78.) + 13.);
	x1m5th = 1. - theta2 * 5.;
	omgdot = temp1 * -.5 * x1m5th + temp2 * .0625 * (theta2 * (theta2 * 
		395. - 114.) + 7.) + temp3 * (theta2 * (theta2 * 49. - 36.) + 
		3.);
	xhdot1 = -temp1 * cosio;
	xnodot = xhdot1 + (temp2 * .5 * (4. - theta2 * 19.) + temp3 * 2. * (
		3. - theta2 * 7.)) * cosio;
	xnodcf = betao2 * 3.5 * xhdot1 * c1;
	t2cof = c1 * 1.5;
	xlcof = a3ovk2 * .125 * sinio * (cosio * 5. + 3.) / (cosio + 1.);
	aycof = a3ovk2 * .25 * sinio;
	x7thm1 = theta2 * 7. - 1.;
    }
    zzdpinit_(&aodp, &xmdot, &omgdot, &xnodot, &xnodp, elems);

/*     Get the time since the EPOCH in minutes. */

    tsince = (*time - epoch) / 60.;

/*     Update for secular gravity and atmospheric drag */

    xmdf = xmo + xmdot * tsince;
    omgadf = omegao + omgdot * tsince;
    xnoddf = xnodeo + xnodot * tsince;
    tsq = tsince * tsince;
    xnode = xnoddf + xnodcf * tsq;
    tempa = 1. - c1 * tsince;
    tempe = bstar * c4 * tsince;
    templ = t2cof * tsq;
    xn = xnodp;

/*     Calculate the secular terms. */

    zzdpsec_(&xmdf, &omgadf, &xnode, &em, &xinc, &xn, &tsince, elems, &omgdot)
	    ;
    d__1 = xke / xn;
/* Computing 2nd power */
    d__2 = tempa;
    a = pow_dd(&d__1, &c_b19) * (d__2 * d__2);
    e = em - tempe;
    xmam = xmdf + xnodp * templ;

/*     Calculate the periodic terms. */

    zzdpper_(&tsince, &e, &xinc, &omgadf, &xnode, &xmam);
    xl = xmam + omgadf + xnode;
    xn = xke / pow_dd(&a, &c_b22);

/*      Long period periodics */

    axn = e * cos(omgadf);
/* Computing 2nd power */
    d__1 = e;
    temp = 1. / (a * (1. - d__1 * d__1));
    xll = temp * xlcof * axn;
    aynl = temp * aycof;
    xlt = xl + xll;
    ayn = e * sin(omgadf) + aynl;

/*     Solve Kepler's equation */

/*           U = EPW - AXN * SIN(EPW)  +  AYN * COS(EPW) */

/*     Where */

/*        AYN  = E * SIN(OMEGA)  +   AYNL */
/*        AXN  = E * COS(OMEGA) */

/*     And */

/*        AYNL =  -0.50D0 * SINIO * AE * J3 / (J2 * A * (1.0D0  -  E^2)) */


/*     Get the mod division of CAPU with 2 Pi */

    d__1 = xlt - xnode;
    capu = d_mod(&d__1, &pix2);
    if (capu < 0.) {
	capu += pix2;
    }

/*     Set initial states for the Kepler solution */

    epw = capu;
    cont = TRUE_;
    while(cont) {
	temp2 = epw;
	sinepw = sin(temp2);
	cosepw = cos(temp2);
	temp3 = axn * sinepw;
	temp4 = ayn * cosepw;
	temp5 = axn * cosepw;
	temp6 = ayn * sinepw;
	epw = (capu - temp4 + temp3 - temp2) / (1. - temp5 - temp6) + temp2;

/*        Test for convergence against the defined tolerance */

	if ((d__1 = epw - temp2, abs(d__1)) <= 1e-6) {
	    cont = FALSE_;
	}
    }

/*     Short period preliminary quantities */

    ecose = temp5 + temp6;
    esine = temp3 - temp4;
    elsq = axn * axn + ayn * ayn;
    temp = 1. - elsq;
    pl = a * temp;
    rk = a * (1. - ecose);
    temp1 = 1. / rk;
    rdot = xke * sqrt(a) * esine * temp1;
    rfdot = xke * sqrt(pl) * temp1;
    temp2 = a * temp1;
    betal = sqrt(temp);
    temp3 = 1. / (betal + 1.);
    cosu = temp2 * (cosepw - axn + ayn * esine * temp3);
    sinu = temp2 * (sinepw - ayn - axn * esine * temp3);

/*     Compute the angle from the x-axis of the point ( COSU, SINU ) */

    if (sinu != 0. || cosu != 0.) {
	uang = atan2(sinu, cosu);
	if (uang < 0.) {
	    uang += pix2;
	}
    } else {
	uang = 0.;
    }
    sin2u = sinu * 2. * cosu;
    cos2u = cosu * 2. * cosu - 1.;
    temp1 = ck2 * (1. / pl);
    temp2 = temp1 * (1. / pl);

/*     Update for short periodics */

    rk = rk * (1. - temp2 * 1.5 * betal * x3thm1) + temp1 * .5 * x1mth2 * 
	    cos2u;
    uk = uang - temp2 * .25 * x7thm1 * sin2u;
    xnodek = xnode + temp2 * 1.5 * cosio * sin2u;
    xinck = xinc + temp2 * 1.5 * cosio * sinio * cos2u;
    rdotk = rdot - xn * temp1 * x1mth2 * sin2u;
    rfdotk = rfdot + xn * temp1 * (x1mth2 * cos2u + x3thm1 * 1.5);

/*     Orientation vectors are calculated by */

/*     U = M sin(uk) + N cos(uk) */
/*     V = M cos(uk) - N sin(uk) */

/*     Where M and N are euclidean 3 vectors */

/*     M = (-sin(xnodek)cos(xinck), cos(xnodek)cos(xinck), sin(xinck) ) */
/*     N = (           cos(xnodek), sin(xnodek)          , 0          ) */

    sinuk = sin(uk);
    cosuk = cos(uk);

/*     Use LATREC to generate M and N.  M is a latitude to rectangle */
/*     conversion of a unit vector where PI/2 + XNODEK is the longitude */

    d__1 = pio2 + xnodek;
    latrec_(&c_b23, &d__1, &xinck, m);
    latrec_(&c_b23, &xnodek, &c_b25, n);

/*     Sum the components to obtain U and V */

    vlcom_(&sinuk, m, &cosuk, n, u);
    d__1 = -sinuk;
    vlcom_(&cosuk, m, &d__1, n, v);

/*     Determine the position and velocity then pack the STATE vector */
/*     with value scaled to KM and KPS. */

/*     R = RK    U +        0 V */
/*     V = RKDOT U + RK RFDOT V */

    scale = xkmper / ae;
    d__1 = rk * scale;
    vlcom_(&d__1, u, &c_b25, v, state);

/*     Now scale to KPS for the velocity component */

    scale /= 60.;
    d__1 = rdotk * scale;
    d__2 = rfdotk * scale;
    vlcom_(&d__1, u, &d__2, v, &state[3]);

/*     All done now.... */

    chkout_("DPSPCE", (ftnlen)6);
    return 0;
} /* dpspce_ */

