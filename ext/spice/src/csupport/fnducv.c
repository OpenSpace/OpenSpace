/* fnducv.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;

/* $Procedure      FNDUCV ( Find unit, class and value. ) */
/* Subroutine */ int fnducv_(char *unin, logical *known, integer *class__, 
	doublereal *value, ftnlen unin_len)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Builtin functions */
    double sin(doublereal);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer), s_cmp(char *, char *, 
	    ftnlen, ftnlen);

    /* Local variables */
    static doublereal lsec, lday, lmin;
    static integer iaus, i__, j;
    static char candp[33];
    static doublereal scale;
    static char cands[32];
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static doublereal hrang;
    extern /* Subroutine */ int ucase_(char *, char *, ftnlen, ftnlen);
    static char names[8*1];
    extern logical benum_(char *, ftnlen);
    static doublereal light;
    static logical found;
    static doublereal lyear;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen);
    static integer count;
    static char error[32];
    static doublereal lhour;
    extern /* Subroutine */ int ljust_(char *, char *, ftnlen, ftnlen);
    extern doublereal twopi_(void);
    static char units[32*84];
    static doublereal au;
    extern doublereal pi_(void);
    static doublereal degree, arcsec, secang;
    extern integer bsrchc_(char *, integer *, char *, ftnlen, ftnlen);
    static doublereal arcmin;
    extern doublereal clight_(void);
    static doublereal minang, parsec;
    extern /* Subroutine */ int orderc_(char *, integer *, integer *, ftnlen),
	     reordc_(integer *, integer *, char *, ftnlen), reordd_(integer *,
	     integer *, doublereal *);
    static logical update;
    static integer iparsc, nnames, ordvec[84];
    extern /* Subroutine */ int reordi_(integer *, integer *, integer *);
    static integer uclass[84];
    extern /* Subroutine */ int nparsd_(char *, doublereal *, char *, integer 
	    *, ftnlen, ftnlen), sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    static doublereal uvalue[84];
    extern /* Subroutine */ int cvpool_(char *, logical *, ftnlen), suffix_(
	    char *, integer *, char *, ftnlen, ftnlen), setmsg_(char *, 
	    ftnlen), rtpool_(char *, integer *, doublereal *, logical *, 
	    ftnlen), swpool_(char *, integer *, char *, ftnlen, ftnlen);
    static integer iau;
    static doublereal rev;
    static integer ptr;

/* $ Abstract */

/*     Find the class (length, time, angle, mass, charge) and value of */
/*     1 unit relative to the reference set of units ( radian, km, sec, */
/*     kg, coulomb). */

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

/*     CONSTANTS */
/*     CONVERSION */
/*     PARSING */
/*     UNITS */
/*     UTILITY */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     UNIN       I   string that may be a primitive unit. */
/*     KNOWN      O   indicates whether UNIN was recognized. */
/*     CLASS      O   type of unit (angle, time, length, mass, charge). */
/*     VALUE      O   the number of these units in 1 reference unit. */

/* $ Detailed_Input */

/*     UNIN       is a string that may be a number or one of the */
/*                primitive units of angle, time, length, mass or */
/*                charge.  A list of recognized units are given below. */
/*                The case of UNIN (upper or lower) is insignificant. */

/* $ Detailed_Output */

/*     KNOWN      is true if UNIN is recognized as a primitive unit, */
/*                or number.  Otherwise it is .FALSE. */

/*     CLASS      is the type of UNIN if it is recognized.  The class */
/*                values are: */

/*                   0  for a number */
/*                   1  for an angle */
/*                   2  for length */
/*                   3  for time */
/*                   4  for mass */
/*                   5  for charge */

/*                if UNIN is not recognized as belonging to any of these */
/*                classes, CLASS is assigned the value of -1. */

/*     VALUE      is the value of 1 UNIN in reference units. */
/*                The reference units are: */

/*                   Number           1 */
/*                   Angle            radians */
/*                   length           kilometers */
/*                   time             second */
/*                   mass             kilogram */
/*                   charge           coulomb */

/*                if UNIN is not recognized as belonging to any of these */
/*                classes, VALUE is set to 0.0d0. */
/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/*     1) This routine is NOT case sensitive. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine examines UNIN and determines if it is a number or */
/*     recognized unit of angle, length, time, mass or charge.  If */
/*     it is recognized it sets a logical variable to .TRUE. to */
/*     indicate the recognition.  In addition, it returns the type of */
/*     object as an integer code: 0 for number, 1 for angle, */
/*     2 for length, 3 for time and 5 for charge.  Finally it returns */
/*     the number of fundamental units 1 UNIN is equal to.  The */
/*     fundamental units for each class of object are: */

/*        number  ---  1 */
/*        angle   ---  radians */
/*        length  ---  kilometers */
/*        time    ---  seconds */
/*        mass    ---  kilograms */
/*        charge  ---  coulombs */

/*      The routine does not recognize any compound units such as */
/*      newtons or joules. */

/* $ Examples */

/*      This routine is intended primarily as a utility routine for */
/*      a more general units conversion routine. */

/* $ Restrictions */

/*      None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 24-MAY-1991 (WLT) */

/* -& */

/*     SPICELIB Functions */



/*     Local parameters */


/*     These are the various classes of recognized objects. */


/*     The reference values for length will be kilometers */
/*                          for time   will be seconds */
/*                          for angles will be radians */
/*                          for mass   will be kilograms */
/*                          for charge will be coulombs */


/*     This value will be computed at run time or default to the */
/*     value given here. */


/*      Some of the units are not "defined" quantities.  In such a case */
/*      a best estimate is provided as of the date of the current version */
/*      of this routine.  Those estimated quantities are: */

/*         1 AU    --- the astronomical unit  is taken from the JPL */
/*                     ephemeris DE200.  It is believed to be accurate to */
/*                     about 40 meters. */

/*         The tropical year is the time from equinox to equinox.  This */
/*         varies slightly with time. */

/*         1 PARSEC --- is dependent upon the value of the astronomical */
/*                      unit. */


/*     1.0d0 divided by the sin of 1 arc second */


/*     Local variables */


/*     Conversion values. */


/*     Initial values */


/*     This next block of code sets up the constants, names, values */
/*     and classes for all the recognized strings.  We do this here */
/*     because FORTRAN just doesn't do this kind of stuff in a */
/*     convenient manner. */

    if (first) {
	first = FALSE_;
	degree = pi_() / 180.;
	arcmin = degree / 60.;
	arcsec = arcmin / 60.;
	scale = 1. / sin(arcsec);
	secang = arcsec * 15.;
	minang = arcmin * 15.;
	hrang = degree * 15.;
	rev = twopi_();
	light = clight_();
	lsec = light * 1.;
	lmin = light * 60.;
	lhour = light * 3600.;
	lday = light * 86400.;
	lyear = light * 31557600.;
	nnames = 1;
	s_copy(names, "AU", (ftnlen)8, (ftnlen)2);

/*        If available and the value of the AU is reasonable, we fetch */
/*        it from the kernel pool.  Otherwise we use the value in */
/*        DE200. */

	swpool_("FNDUCV", &nnames, names, (ftnlen)6, (ftnlen)8);
	cvpool_("FNDUCV", &update, (ftnlen)6);
	rtpool_("AU", &i__, &au, &found, (ftnlen)2);
	if (! found) {
	    au = 149597870.66;
	} else if ((d__1 = au - 149597870.66, abs(d__1)) > 10.) {
	    au = 149597870.66;
	}
	parsec = scale * au;
	i__ = 0;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)433)) << 5), "METERS", (
		ftnlen)32, (ftnlen)6);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)434)] = .001;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)435)] = 2;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)438)) << 5), "CM", (ftnlen)
		32, (ftnlen)2);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)439)] = 1.0000000000000001e-5;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)440)] = 2;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)443)) << 5), "KM", (ftnlen)
		32, (ftnlen)2);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)444)] = 1.;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)445)] = 2;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)448)) << 5), "KMS", (ftnlen)
		32, (ftnlen)3);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)449)] = 1.;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)450)] = 2;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)453)) << 5), "CENTIMETERS", 
		(ftnlen)32, (ftnlen)11);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)454)] = 1.0000000000000001e-5;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)455)] = 2;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)458)) << 5), "KILOMETERS", (
		ftnlen)32, (ftnlen)10);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)459)] = 1.;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)460)] = 2;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)463)) << 5), "INCH", (
		ftnlen)32, (ftnlen)4);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)464)] = 2.5400000000000001e-5;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)465)] = 2;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)468)) << 5), "INCHES", (
		ftnlen)32, (ftnlen)6);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)469)] = 2.5400000000000001e-5;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)470)] = 2;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)473)) << 5), "FOOT", (
		ftnlen)32, (ftnlen)4);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)474)] = 3.0480000000000004e-4;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)475)] = 2;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)478)) << 5), "FEET", (
		ftnlen)32, (ftnlen)4);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)479)] = 3.0480000000000004e-4;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)480)] = 2;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)483)) << 5), "YARDS", (
		ftnlen)32, (ftnlen)5);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)484)] = 9.1440000000000011e-4;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)485)] = 2;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)488)) << 5), "AU", (ftnlen)
		32, (ftnlen)2);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)489)] = au;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)490)] = 2;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)493)) << 5), "AUS", (ftnlen)
		32, (ftnlen)3);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)494)] = au;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)495)] = 2;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)498)) << 5), "MILES", (
		ftnlen)32, (ftnlen)5);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)499)] = 1.6093440000000001;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)500)] = 2;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)503)) << 5), "STATUTE_MILES"
		, (ftnlen)32, (ftnlen)13);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)504)] = 1.6093440000000001;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)505)] = 2;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)508)) << 5), "LIGHTSECONDS",
		 (ftnlen)32, (ftnlen)12);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)509)] = lsec;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)510)] = 2;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)513)) << 5), "LIGHTYEAR", (
		ftnlen)32, (ftnlen)9);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)514)] = lyear;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)515)] = 2;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)518)) << 5), "SECS", (
		ftnlen)32, (ftnlen)4);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)519)] = 1.;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)520)] = 3;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)523)) << 5), "SECONDS", (
		ftnlen)32, (ftnlen)7);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)524)] = 1.;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)525)] = 3;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)528)) << 5), "MINS", (
		ftnlen)32, (ftnlen)4);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)529)] = 60.;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)530)] = 3;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)533)) << 5), "MINUTES", (
		ftnlen)32, (ftnlen)7);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)534)] = 60.;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)535)] = 3;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)538)) << 5), "HRS", (ftnlen)
		32, (ftnlen)3);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)539)] = 3600.;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)540)] = 3;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)543)) << 5), "HOURS", (
		ftnlen)32, (ftnlen)5);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)544)] = 3600.;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)545)] = 3;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)548)) << 5), "DAYS", (
		ftnlen)32, (ftnlen)4);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)549)] = 86400.;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)550)] = 3;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)553)) << 5), "WEEKS", (
		ftnlen)32, (ftnlen)5);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)554)] = 604800.;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)555)] = 3;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)558)) << 5), "JYEARS", (
		ftnlen)32, (ftnlen)6);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)559)] = 31557600.;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)560)] = 3;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)563)) << 5), "JULIAN_YEARS",
		 (ftnlen)32, (ftnlen)12);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)564)] = 31557600.;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)565)] = 3;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)568)) << 5), "CENTURY", (
		ftnlen)32, (ftnlen)7);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)569)] = 3.15576e9;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)570)] = 3;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)573)) << 5), "CENTURIES", (
		ftnlen)32, (ftnlen)9);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)574)] = 3.15576e9;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)575)] = 3;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)578)) << 5), "JULIAN_CENTU"
		"RIES", (ftnlen)32, (ftnlen)16);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)579)] = 3.15576e9;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)580)] = 3;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)583)) << 5), "JULIAN_CENTU"
		"RY", (ftnlen)32, (ftnlen)14);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)584)] = 3.15576e9;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)585)] = 3;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)588)) << 5), "LIGHTDAYS", (
		ftnlen)32, (ftnlen)9);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)589)] = lday;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)590)] = 2;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)593)) << 5), "LIGHTYEARS", (
		ftnlen)32, (ftnlen)10);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)594)] = lyear;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)595)] = 2;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)598)) << 5), "RADIANS", (
		ftnlen)32, (ftnlen)7);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)599)] = 1.;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)600)] = 1;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)603)) << 5), "MILLIRADIANS",
		 (ftnlen)32, (ftnlen)12);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)604)] = .001;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)605)] = 1;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)608)) << 5), "MICRORADIANS",
		 (ftnlen)32, (ftnlen)12);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)609)] = 9.9999999999999995e-7;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)610)] = 1;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)613)) << 5), "NANORADIANS", 
		(ftnlen)32, (ftnlen)11);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)614)] = 1.0000000000000001e-9;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)615)] = 1;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)618)) << 5), "DEGREES", (
		ftnlen)32, (ftnlen)7);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)619)] = degree;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)620)] = 1;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)623)) << 5), "DEGS", (
		ftnlen)32, (ftnlen)4);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)624)] = degree;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)625)] = 1;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)628)) << 5), "ARCSECONDS", (
		ftnlen)32, (ftnlen)10);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)629)] = arcsec;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)630)] = 1;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)633)) << 5), "ARCMINUTES", (
		ftnlen)32, (ftnlen)10);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)634)] = arcmin;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)635)] = 1;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)638)) << 5), "SECONDANGLES",
		 (ftnlen)32, (ftnlen)12);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)639)] = secang;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)640)] = 1;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)643)) << 5), "MINUTEANGLES",
		 (ftnlen)32, (ftnlen)12);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)644)] = minang;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)645)] = 1;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)648)) << 5), "HOURANGLES", (
		ftnlen)32, (ftnlen)10);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)649)] = hrang;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)650)] = 1;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)653)) << 5), "KILOGRAMS", (
		ftnlen)32, (ftnlen)9);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)654)] = 1.;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)655)] = 4;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)658)) << 5), "KGS", (ftnlen)
		32, (ftnlen)3);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)659)] = 1.;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)660)] = 4;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)663)) << 5), "GRAMS", (
		ftnlen)32, (ftnlen)5);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)664)] = .001;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)665)] = 4;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)668)) << 5), "POUNDS", (
		ftnlen)32, (ftnlen)6);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)669)] = .45359237000000002;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)670)] = 4;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)673)) << 5), "OUNCES", (
		ftnlen)32, (ftnlen)6);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)674)] = .028349523125000001;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)675)] = 4;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)678)) << 5), "PARSECS", (
		ftnlen)32, (ftnlen)7);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)679)] = parsec;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)680)] = 2;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)683)) << 5), "YEARS", (
		ftnlen)32, (ftnlen)5);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)684)] = 31557600.;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)685)] = 3;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)688)) << 5), "JULIANYEARS", 
		(ftnlen)32, (ftnlen)11);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)689)] = 31557600.;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)690)] = 3;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)693)) << 5), "TROPICALYEARS"
		, (ftnlen)32, (ftnlen)13);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)694)] = 31556925.976319999;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)695)] = 3;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)698)) << 5), "TROPICAL_YEA"
		"RS", (ftnlen)32, (ftnlen)14);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)699)] = 31556925.976319999;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)700)] = 3;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)703)) << 5), "STATUTEMILES",
		 (ftnlen)32, (ftnlen)12);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)704)] = 1.6093440000000001;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)705)] = 2;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)708)) << 5), "NAUTICALMILES"
		, (ftnlen)32, (ftnlen)13);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)709)] = 1.8520000000000001;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)710)] = 2;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)713)) << 5), "NAUTICAL_MIL"
		"ES", (ftnlen)32, (ftnlen)14);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)714)] = 1.8520000000000001;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)715)] = 2;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)718)) << 5), "MMS", (ftnlen)
		32, (ftnlen)3);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)719)] = 1.0000000000000002e-6;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)720)] = 2;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)723)) << 5), "MILLIMETERS", 
		(ftnlen)32, (ftnlen)11);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)724)] = 1.0000000000000002e-6;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)725)] = 2;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)728)) << 5), "REVOLUTIONS", 
		(ftnlen)32, (ftnlen)11);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)729)] = rev;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)730)] = 1;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)733)) << 5), "REVS", (
		ftnlen)32, (ftnlen)4);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)734)] = rev;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)735)] = 1;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)738)) << 5), "LIGHTHOURS", (
		ftnlen)32, (ftnlen)10);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)739)] = lhour;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)740)] = 2;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)743)) << 5), "LIGHTMINUTES",
		 (ftnlen)32, (ftnlen)12);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)744)] = lmin;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)745)] = 2;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)748)) << 5), "COULOMBS", (
		ftnlen)32, (ftnlen)8);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)749)] = 1.;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)750)] = 5;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)753)) << 5), "ELECTRON_CHA"
		"RGES", (ftnlen)32, (ftnlen)16);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)754)] = 1.6020608911303502e-19;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)755)] = 5;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)758)) << 5), "STATCOULOMBS",
		 (ftnlen)32, (ftnlen)12);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)759)] = 2.99793e9;
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)760)] = 5;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)763)) << 5), "PI", (ftnlen)
		32, (ftnlen)2);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)764)] = pi_();
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)765)] = 0;
	++i__;
	s_copy(units + (((i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"units", i__1, "fnducv_", (ftnlen)768)) << 5), "-PI", (ftnlen)
		32, (ftnlen)3);
	uvalue[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue", 
		i__1, "fnducv_", (ftnlen)769)] = -pi_();
	uclass[(i__1 = i__ - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uclass", 
		i__1, "fnducv_", (ftnlen)770)] = 0;

/*        I         = I + 1 */
/*        UNITS(I)  = */
/*        UVALUE(I) = */
/*        UCLASS(I) = */

	count = i__;

/*        Sort everything for quick lookup. */

	orderc_(units, &count, ordvec, (ftnlen)32);
	reordc_(ordvec, &count, units, (ftnlen)32);
	reordd_(ordvec, &count, uvalue);
	reordi_(ordvec, &count, uclass);
    }
    cvpool_("FNDUCV", &update, (ftnlen)6);
    if (update) {
	iau = bsrchc_("AU", &count, units, (ftnlen)2, (ftnlen)32);
	iaus = bsrchc_("AUS", &count, units, (ftnlen)3, (ftnlen)32);
	iparsc = bsrchc_("PARSECS", &count, units, (ftnlen)7, (ftnlen)32);
	rtpool_("AU", &i__, &au, &found, (ftnlen)2);
	if ((d__1 = au - 149597870.66, abs(d__1)) < 10.) {
	    uvalue[(i__1 = iau - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uvalue"
		    , i__1, "fnducv_", (ftnlen)802)] = au;
	    uvalue[(i__1 = iaus - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge("uval"
		    "ue", i__1, "fnducv_", (ftnlen)803)] = au;
	    uvalue[(i__1 = iparsc - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		    "uvalue", i__1, "fnducv_", (ftnlen)804)] = scale * au;
	}
    }

/*     Left justify, convert to upper case and form a "plural" version */
/*     of UNIN */

    ljust_(unin, cands, unin_len, (ftnlen)32);
    ucase_(cands, cands, (ftnlen)32, (ftnlen)32);
    s_copy(candp, cands, (ftnlen)33, (ftnlen)32);
    suffix_("S", &c__0, candp, (ftnlen)1, (ftnlen)33);

/*     Look for the "singular" version first. */

    j = bsrchc_(cands, &count, units, (ftnlen)32, (ftnlen)32);

/*     If we didn't have any luck with the singular version, */
/*     look for the plural form. */

    if (j == 0) {
	j = bsrchc_(candp, &count, units, (ftnlen)33, (ftnlen)32);
    }

/*     If we got something, just copy the class and value. */

    if (j > 0) {
	*known = TRUE_;
	*class__ = uclass[(i__1 = j - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"uclass", i__1, "fnducv_", (ftnlen)840)];
	*value = uvalue[(i__1 = j - 1) < 84 && 0 <= i__1 ? i__1 : s_rnge(
		"uvalue", i__1, "fnducv_", (ftnlen)841)];
    } else {

/*        We don't have a unit.  Get ready to return... */

	*known = FALSE_;
	*class__ = -1;
	*value = 0.;

/*        ... but before we do, see if we've got a number. */

	if (benum_(cands, (ftnlen)32)) {
	    nparsd_(cands, value, error, &ptr, (ftnlen)32, (ftnlen)32);
	    if (s_cmp(error, " ", (ftnlen)32, (ftnlen)1) == 0) {
		*known = TRUE_;
		*class__ = 0;
	    }
	}
    }

/*     Since the user can potentially enter a bad value for the AU */
/*     via the kernel pool, we will signal an error.  However we */
/*     wait until this point so that routines that need to have */
/*     an AU value in order to continue functioning, */

    if ((d__1 = au - 149597870.66, abs(d__1)) > 10.) {
	chkin_("FNDUCV", (ftnlen)6);
	setmsg_("The value of the astronomical unit extracted from the kerne"
		"l pool varies from the well trusted value used in DE200 (149"
		",597,870.660 km) by more than 10 km. The value in DE200 is b"
		"elieved to be good to 60 meters or so.  The value in the ker"
		"nel pool was #. ", (ftnlen)255);
	errdp_("#", &au, (ftnlen)1);
	sigerr_("SPICE(BADAUVALUE)", (ftnlen)17);
	chkout_("FNDUCV", (ftnlen)6);

/*        Reset the value of the AU back to the DE200 value so that */
/*        the next time we hit this without doing a kernel pool read */
/*        we will not get this error message again. */

	au = 149597870.66;
	return 0;
    }
    return 0;
} /* fnducv_ */

