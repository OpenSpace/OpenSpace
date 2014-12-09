/* setelm.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__10 = 10;
static integer c__8 = 8;
static integer c__12 = 12;
static integer c__11 = 11;
static integer c__16 = 16;
static integer c__17 = 17;
static integer c__18 = 18;
static integer c__19 = 19;
static integer c__20 = 20;

/* $Procedure      SETELM ( Calculate set of SPICE conic elements ) */
/* Subroutine */ int setelm_(doublereal *cngmvl, doublereal *dvl, doublereal *
	dstcof, doublereal *angcof, integer *param, integer *nparam, 
	doublereal *elts, logical *errflg, char *errtxt, ftnlen errtxt_len)
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    double sqrt(doublereal), pow_dd(doublereal *, doublereal *), sin(
	    doublereal), sinh(doublereal), cos(doublereal), acos(doublereal), 
	    tan(doublereal);

    /* Local variables */
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal meanm, sense, wrkel;
    extern doublereal twopi_(void), pi_(void), dacosh_(doublereal *);
    extern integer isrchi_(integer *, integer *, integer *);
    extern /* Subroutine */ int chkout_(char *, ftnlen);
    extern logical return_(void);
    doublereal twothr;

/* $ Abstract */

/*     Calculate set of standard conic elements used by SPICELIB */
/*     procedure CONIC from a set of different parameters allowed for */
/*     the input data type ELEMENTS. */

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

/*     MKSPK User's Guide */

/* $ Keywords */

/*     None. */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     CNGMVL     I   Gravitational parameter (GM) of primary body */
/*     DVL        I   Array of input parameters values */
/*     PARAM      I   Array of parameter indexes */
/*     NPARAM     I   Number elements in PARAM */
/*     ANGCOF     I   Angle unit coefficient */
/*     DSTCOF     I   Distance unit coefficient */
/*     ELTS       O   Set of conic elements */
/*     ERRFLG     O   Logical error flag */
/*     ERRTXT     O   Error explanation text */

/* $ Detailed_Input */

/*                                                       3    2 */
/*     CNGMVL      is the gravitational parameter (GM, km /sec ) of */
/*                 the primary body. */

/*     DVL         is a set of parameter values obtained by parsing */
/*                 input text data of type ELEMENTS according */
/*                 to the specified DATA_ORDER. The parameters in the */
/*                 arrays are the following order (see array DOVAL in */
/*                 the main module): */

/*                    'EPOCH', 'X', 'Y', 'Z', 'VX' , 'VY', 'VZ', */
/*                    'A', 'E', 'RP', 'T', 'P', 'INC', 'PER', 'NOD', */
/*                    'MEAN', 'EXAN', 'TRAN', 'EPOCHP', 'TAU', */
/*                    'EQ_A', 'EQ_H', 'EQ_K', 'EQ_ML', 'EQ_P', 'EQ_Q', */
/*                    'DPER/DT', 'DMPN/DT', 'DNOD/DT' , 'SKIP' */

/*     PARAM       is an array of input parameter indexes in DVL */
/*                 arranged in accordance with the specified DATA_ORDER */
/*                 value. Index of each parameter is as ordinal number */
/*                 of the corresponding word in array DOVAL. */

/*     NPARAM      is the number of elements in array PARAM. */

/*     ANGCOF      is the coefficient that converts input angle units */
/*                 to radians. */

/*     DSTCOF      is the coefficient that converts input distance units */
/*                 to kilometers. */

/* $ Detailed_Output */

/*     ELTS        are conic elements describing the orbit of an object */
/*                 around a primary body. The elements are, in order: */

/*                            Perifocal distance. */
/*                            Eccentricity. */
/*                            Inclination. */
/*                            Longitude of the ascending node. */
/*                            Argument of periapsis. */
/*                            Mean anomaly at epoch. */
/*                            Epoch. */
/*                            Gravitational parameter. */

/*                 The epoch of the elements is the epoch of the input */
/*                 state. Units are km, rad, rad/sec. The same elements */
/*                 are used to describe all three types (elliptic, */
/*                 hyperbolic, and parabolic) of conic orbit. */

/*     ERRFLG      is the logical flag of the error. Set to .TRUE. value */
/*                 if calculation of the comic elements from a given */
/*                 input parameters wasn't possible. */

/*     ERRTXT      is the error explanation text. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     If input elements are inconsistent or incomplete, this routine */
/*     doesn't signal an error but rather returns long anf short */
/*     error messages through its output parameters. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.G. Khavenson (IKI RAS, Russia) */
/*     B.V. Semenov   (NAIF, JPL) */

/* $ Version */

/* -    Version 1.0.4, 18-JAN-2000 (BVS). */

/*        Added CHKOUT calls before each RETURN. */

/* -    Version 1.0.3, 19-MAR-1999 (BVS). */

/*        Corrected comments. */

/* -    Version 1.0.2, 13-JAN-1999 (BVS). */

/*        Modified error messages. */

/* -    Version 1.0.1, 22-NOV-1998 (NGK). */


/* -    Version 1.0.0, 8-SEP-1998 (NGK). */

/* -& */
/* $ Index_Entries */

/*     Calculate set of conic elements from MKSPK input parameters. */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SETELM", (ftnlen)6);
    }

/*     Initialize error flag. */

    *errflg = FALSE_;

/*     Assign constant value. */

    twothr = .66666666666666663;

/*     All listed below DVL are present in the input data. We already */
/*     checked this when we checked specified DATA_ORDER against input */
/*     output type in CHCKDO. */

    elts[1] = dvl[8];
    elts[2] = dvl[12] * *angcof;
    elts[3] = dvl[14] * *angcof;
    elts[4] = dvl[13] * *angcof;
    elts[6] = dvl[0];
    elts[7] = *cngmvl;

/*     First we need to compute the distance of the periapsis. */

    if (isrchi_(&c__10, nparam, param) != 0) {

/*        Distance of the peripsis is defined in input file. */

	elts[0] = dvl[9] * *dstcof;
    } else if (isrchi_(&c__8, nparam, param) != 0) {

/*        Semi-major axis is defined in input file. */

	if (dvl[8] != 1.) {
/*           Case of elliptic or hyperbolic orbits. */

	    elts[0] = (d__1 = (dvl[8] - 1.) * dvl[7], abs(d__1)) * *dstcof;
	} else {

/*           Case of parabolic orbit. Complain and exit. */

	    s_copy(errtxt, "Semi-major axis can not be used as input paramet"
		    "er in case of a parabolic orbit (ecc = 1.0).", errtxt_len,
		     (ftnlen)92);
	    s_copy(errtxt + errtxt_len, "SPICE(INCONSISTELEMENTS)", 
		    errtxt_len, (ftnlen)24);
	    *errflg = TRUE_;
	    chkout_("SETELM", (ftnlen)6);
	    return 0;
	}
    } else if (isrchi_(&c__12, nparam, param) != 0) {

/*        Semi-latus rectum is defined in input file. */

	elts[0] = dvl[11] / (dvl[8] + 1.) * *dstcof;
    } else if (isrchi_(&c__11, nparam, param) != 0) {

/*        Orbital period is defined in input file. */

	if (dvl[8] < 1.) {

/*           Case of elliptic orbit. */

	    d__1 = dvl[10] * sqrt(*cngmvl) / twopi_();
	    elts[0] = (1. - dvl[8]) * pow_dd(&d__1, &twothr) * *dstcof;
	} else {

/*           Case of parabolic or hyperbolic orbit. Complain and exit. */

	    s_copy(errtxt, "Orbital period can not be used as input paramete"
		    "r in case of a parabolic (ecc = 1.0) or hyperbolic (ecc "
		    "> 1.0) orbit.", errtxt_len, (ftnlen)117);
	    s_copy(errtxt + errtxt_len, "SPICE(INCONSISTELEMENTS)", 
		    errtxt_len, (ftnlen)24);
	    *errflg = TRUE_;
	    chkout_("SETELM", (ftnlen)6);
	    return 0;
	}
    } else {

/*        Not enough input data to calculate the periapsis distance. */
/*        Complain and exit. */

	s_copy(errtxt, "Perifocal distance can not be computed from given se"
		"t of orbital elements.", errtxt_len, (ftnlen)74);
	s_copy(errtxt + errtxt_len, "SPICE(INCOMPLETEELEMENTS)", errtxt_len, (
		ftnlen)25);
	*errflg = TRUE_;
	chkout_("SETELM", (ftnlen)6);
	return 0;
    }

/*     Second we need to compute the mean anomaly. */

    if (isrchi_(&c__16, nparam, param) != 0) {

/*        Mean anomaly is defined in the input file. */

	elts[5] = dvl[15] * *angcof;
    } else if (isrchi_(&c__17, nparam, param) != 0) {

/*        Eccentric anomaly is defined in the input file. */

	wrkel = dvl[16] * *angcof;
	if (dvl[8] < 1.) {

/*           Case of elliptic orbit. */

	    elts[5] = wrkel - dvl[8] * sin(wrkel);
	} else if (dvl[8] > 1.) {

/*           Case of hyperbolic orbit. */

	    elts[5] = dvl[8] * sinh(wrkel) - wrkel;
	} else {

/*           Case of parabolic  orbit. Complain and exit. */

	    s_copy(errtxt, "Eccentric anomaly can not be used as input param"
		    "eter in case of a parabolic orbit (ecc = 1.0).", 
		    errtxt_len, (ftnlen)94);
	    s_copy(errtxt + errtxt_len, "SPICE(INCONSISTELEMENTS)", 
		    errtxt_len, (ftnlen)24);
	    *errflg = TRUE_;
	    chkout_("SETELM", (ftnlen)6);
	    return 0;
	}
    } else if (isrchi_(&c__18, nparam, param) != 0) {

/*        True anomaly is defined in input file. */

	wrkel = dvl[17] * *angcof;
	if (wrkel < pi_()) {
	    sense = 1.;
	} else {
	    sense = -1.;
	}
	if (dvl[8] < 1.) {

/*           Case of elliptic orbit. */

	    wrkel = acos((dvl[8] + cos(wrkel)) / (dvl[8] * cos(wrkel) + 1.));
	    elts[5] = sense * (wrkel - dvl[8] * sin(wrkel));
	    if (elts[5] < 0.) {
		elts[5] += twopi_();
	    }
	} else if (dvl[8] > 1.) {

/*           Case of hyperbolic orbit. */

	    d__1 = (dvl[8] + cos(wrkel)) / (dvl[8] * cos(wrkel) + 1.);
	    wrkel = dacosh_(&d__1);
	    elts[5] = sense * (dvl[8] * sinh(wrkel) - wrkel);
	} else {

/*           Case of parabolic orbit. */

	    wrkel = tan(wrkel / 2.);
/* Computing 3rd power */
	    d__1 = wrkel;
	    elts[5] = sense * (wrkel + d__1 * (d__1 * d__1) / 3.);
	}
    } else if (isrchi_(&c__19, nparam, param) != 0) {

/*        Epoch of periapsis is defined in input file. */

	if (dvl[8] < 1.) {

/*           Case of elliptic orbit. */

	    wrkel = (1. - dvl[8]) / elts[0];
	    meanm = sqrt(elts[7] * wrkel) * wrkel;
	    elts[5] = meanm * (dvl[0] - dvl[18]);
	} else if (dvl[8] > 1.) {

/*           Case of hyperbolic orbit. */

	    wrkel = (dvl[8] - 1.) / elts[0];
	    meanm = sqrt(elts[7] * wrkel) * wrkel;
	    elts[5] = meanm * (dvl[0] - dvl[18]);
	} else {

/*           Case of parabolic orbit. */

	    meanm = sqrt(elts[7] / 2. / elts[0]) / elts[0];
	    elts[5] = meanm * (dvl[0] - dvl[18]);
	}
    } else if (isrchi_(&c__20, nparam, param) != 0) {

/*        Time interval between current epoch and periapsis epoch */
/*        is defined in input file. */

	if (dvl[8] < 1.) {

/*           Case of elliptic orbit. */

	    wrkel = (1. - dvl[8]) / elts[0];
	    meanm = sqrt(elts[7] * wrkel) * wrkel;
	    elts[5] = meanm * dvl[19];
	} else if (dvl[8] > 1.) {

/*           Case of hyperbolic orbit. */

	    wrkel = (dvl[8] - 1.) / elts[0];
	    meanm = sqrt(elts[7] * wrkel) * wrkel;
	    elts[5] = meanm * dvl[19];
	} else {

/*           Case of parabolic orbit. */

	    meanm = sqrt(elts[7] / 2. / elts[0]) / elts[0];
	    elts[5] = meanm * dvl[19];
	}
    } else {

/*        Not enough input data to calculate mean anomaly. */
/*        Complain and exit. */

	s_copy(errtxt, "Mean anomaly can not be computed from given set of o"
		"rbital elements.", errtxt_len, (ftnlen)68);
	s_copy(errtxt + errtxt_len, "SPICE(INCOMPLETEELEMENTS)", errtxt_len, (
		ftnlen)25);
	*errflg = TRUE_;
	chkout_("SETELM", (ftnlen)6);
	return 0;
    }
    chkout_("SETELM", (ftnlen)6);
    return 0;
} /* setelm_ */

