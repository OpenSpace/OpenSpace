/* changu.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__1 = 1;
static integer c__0 = 0;
static integer c__128 = 128;

/* $Procedure      CHANGU ( Change units ) */
/* Subroutine */ int changu_0_(int n__, char *angle, char *length, char *time,
	 char *mass, char *charge, char *in, char *out, char *error, ftnlen 
	angle_len, ftnlen length_len, ftnlen time_len, ftnlen mass_len, 
	ftnlen charge_len, ftnlen in_len, ftnlen out_len, ftnlen error_len)
{
    /* Initialized data */

    static char tclass[8*5] = "ANGLE   " "LENGTH  " "TIME    " "MASS    " 
	    "CHARGE  ";
    static logical first = TRUE_;
    static integer nop = 6;
    static char op[2*6] = "  " "( " ") " "* " "**" "/ ";

    /* System generated locals */
    address a__1[2];
    integer i__1, i__2[2], i__3;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int scan_(char *, char *, integer *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, integer *, 
	    ftnlen, ftnlen);
    static integer pass, nest, size[6];
    static char type__[32*6];
    static integer mult, b, e, f, i__;
    static char o[256];
    static integer s, blank, ident[128], class__;
    static logical found;
    static doublereal value;
    static integer oplen[6], start, opptr[20];
    extern integer bsrchc_(char *, integer *, char *, ftnlen, ftnlen);
    static integer lparen;
    extern integer lastnb_(char *, ftnlen);
    extern /* Subroutine */ int fnducv_(char *, logical *, integer *, 
	    doublereal *, ftnlen), scanpr_(integer *, char *, integer *, 
	    integer *, ftnlen);
    static integer rparen;
    extern /* Subroutine */ int suffix_(char *, integer *, char *, ftnlen, 
	    ftnlen);
    static char string[256];
    static integer ntokns, beg[128], end[128], div, exp__;

/* $ Abstract */

/*     Determine units having the same dimensions of angle, length, */
/*     time, mass and charge as some set of input units, but with */
/*     respect to a "standard" set of units of the user's choosing. */

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
/*     ANGLE      I   Default unit to use for angles (see OUNITS). */
/*     LENGTH     I   Default unit to use for lengths (see OUNITS). */
/*     TIME       I   Default unit to use for time (see OUNITS). */
/*     MASS       I   Default unit to use for mass (see OUNITS). */
/*     CHARGE     I   Default unit to use for charge (see OUNITS). */
/*     IN         I   Units to be transformed to the "standard". */
/*     OUT        O   Units that the input will be transformed to. */
/*     ERROR      O   Contains a description of a problem if one occurs. */
/*     ROOM       P   Maximum number of components in a compound unit. */

/* $ Detailed_Input */

/*     See individual entry points */

/*     ANGLE      is a string indicating which angle unit should be */
/*                used for outputs. */

/*     LENGTH     is a string indicating which distance unit should */
/*                be used for outputs. */

/*     TIME       is a string indicating which time unit should be */
/*                used for outputs. */

/*     MASS       is a string indicating which mass unit should be */
/*                used for outputs. */

/*     CHARGE     is a string indicating which charge unit should be */
/*                used for outputs. */

/*     IN         is the set of units associated with some measurment. */
/*                The dimensionally equivalent "standard" units are */
/*                returned in OUT. */

/* $ Detailed_Output */

/*     See individual entry points. */

/*     OUT        is the set of "standard" units that are dimensionally */
/*                equivalent to the input units given by IN. */

/*     ERROR      Contains a descriptive error message if the */
/*                subroutine call can not be executed successfully. */

/* $ Parameters */

/*     ROOM       This routine uses internal storage to construct */
/*                the output for TRANSU.  ROOM is the parameter that */
/*                describes the maximum number of components that */
/*                are expected for any compound unit.  The components */
/*                of a compound unit are */

/*                   Left parenthesis  --- '(' */
/*                   Right parenthesis --- ')' */
/*                   Exponentiation    --- '**' */
/*                   Multiplication    --- '*' */
/*                   Division          --- '/' */
/*                   Numbers */
/*                   Reconized units of angle, distance, time, mass or */
/*                   charge. */

/*                Thus  ((10**12*KG)*(10**9*KM)**3)/((2/3)*SEC**2) */
/*                      ^^ ^ ^ ^^ ^^^^ ^ ^^^ ^^ ^^^^^^^^^^^  ^ ^^^ */

/*                Has 31 components. (Each '^' points to the end of a */
/*                component). */

/*                At the time this routine was written, it was assumed */
/*                that compound units would have fewer than 128 */
/*                components. */

/* $ Exceptions */

/*     1) The units used as the "standard" set must be recognized. */
/*        If they are not the error 'SPICE(UNKNOWNUNITS)' is signalled */
/*        by the entry point OUNITS. */

/*     2) If the input string IN can not be parsed as a unit, the error */
/*        'SPICE(INVALIDUNITS)' is signalled by the entry point TRANSU. */
/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine (and its entry points) are utilities that work */
/*     in conjunction with the general units conversion routine */
/*     CONVRT_2. */

/*     Here's why it is needed. */

/*        For many applications it is convenient to have command */
/*        driven programs.  Such commands might look like */

/*           SET FIELD OF VIEW  <number> [units] */

/*        Where "<number>" is some number that represents the size of */
/*        the field of view and must be supplied for the command to */
/*        mean anything.  The field "[units]" is an optional argument */
/*        that specifies the units to associate with the numeric */
/*        part of the command.  For example you might type any of the */
/*        following: */

/*           SET FIELD OF VIEW 12 DEGREES */

/*           SET FIELD OF VIEW 5  10E-3*RADIANS */

/*           SET FIELD OF VIEW 12 NANORADIANS */

/*           SET FIELD OF VIEW 6 ARCSECONDS */

/*        Allowing this kind of flexibility for inputs, gives user's */
/*        a friendlier interface to the program.  Instead of spending */
/*        time converting to some standard set of inputs, the program */
/*        "understands" many different units. */

/*        Ultimately, the measurements written in these expressions */
/*        must be converted to a set of units that the program */
/*        "understands."  If the above command were the only one */
/*        recognized by the program, the problem of converting to */
/*        internal units would be relatively simple.  You could just */
/*        list the collection of recognized units and translate them. */
/*        For this command such a would probably not contain more than */
/*        30 different units.  However, when compound units are */
/*        allowed such as: */

/*           KM/SEC**2,  MILES/HOUR/DAY,  AU/(100*DAYS)**2, etc. */

/*        it is no longer practical to simply list all of the possible */
/*        compound expressions.  Instead it is much simpler to select a */
/*        set of primitive units in which all compound units will be */
/*        expressed and used internally.  For example you might decide */
/*        that the fundamental units best suited to your application are: */

/*           For angles   ---  Degrees */
/*           For distance ---  Astronomical Units (AU) */
/*           For time     ---  DAYS */
/*           For mass     ---  KG */
/*           For Charge   ---  ELECTRON_CHARGES */

/*        When a measurment is encountered, your program would convert */
/*        it to this set of standard units automatically.  For example */
/*        If an input had the form */

/*           3 KM/SEC */

/*        the program would automatically convert it to the appropriate */
/*        number of */

/*             AU/DAYS. */

/*        In terms of the primitive units of angle, length, time, mass */
/*        and charge.  These two quantities are dimensionally equivalent. */


/*        This routine serves as the umbrella for two functions: */

/*        1) Establishing what units to use as "standard" for the */
/*           fundamental quanities of angle, distance, time, mass and */
/*           charge.  (OUNITS) */

/*        2) Computing the standard units that are dimensionally */
/*           equivalent to any given input units. */

/*        With the dimensionally equivalent standard units in hand, */
/*        it is an easy matter (as the example below illustrates) */
/*        to convert inputs measurments to the standard units your */
/*        program needs. */

/* $ Examples */

/*        To set up your default units as above: */

/*           IF ( FIRST ) THEN */

/*              CALL OUNITS ( 'DEGREES',         'AU', 'DAYS', 'KG', */
/*          .                 'ELECTRON_CHARGES'                    ) */

/*              FIRST = .FALSE. */

/*           END IF */

/*        To translate a measurement X UNITS to the default units. */

/*           CALL TRANSU   (    UNITS, MINE       ) */
/*           CALL CONVRT_2 ( X, UNITS, MINE, MY_X ) */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber     (JPL) */

/* $ Version */

/* -    Beta Version 1.0.0, 29-MAY-1991 (WLT) */

/* -& */

/*     SPICELIB functions. */


/*     Local Parameters */


/*     Local Variables */


/*       Here is the range of       Character      ASCII code */
/*       initial characters that    ---------      ---------- */
/*       will be used by the        ' '             32 */
/*       "known" marks.             '('             40 */
/*                                  ')'             41 */
/*                                  '*'             42 */
/*                                  '/'             47 */

/*     So the required number of pointers is 47 - 32 + 5 = 20. */


/*     Saved variables */


/*     Initial Values */

    switch(n__) {
	case 1: goto L_ounits;
	case 2: goto L_transu;
	}

    return 0;

L_ounits:

/*        On the first pass through this routine, set up the stuff */
/*        required for scanning the input string. */

    if (first) {
	first = FALSE_;
	scanpr_(&nop, op, oplen, opptr, (ftnlen)2);
	blank = bsrchc_(" ", &nop, op, (ftnlen)1, (ftnlen)2);
	lparen = bsrchc_("(", &nop, op, (ftnlen)1, (ftnlen)2);
	rparen = bsrchc_(")", &nop, op, (ftnlen)1, (ftnlen)2);
	mult = bsrchc_("*", &nop, op, (ftnlen)1, (ftnlen)2);
	exp__ = bsrchc_("**", &nop, op, (ftnlen)2, (ftnlen)2);
	div = bsrchc_("/", &nop, op, (ftnlen)1, (ftnlen)2);
    }
    s_copy(type__, "1", (ftnlen)32, (ftnlen)1);
    s_copy(type__ + 32, angle, (ftnlen)32, angle_len);
    s_copy(type__ + 64, length, (ftnlen)32, length_len);
    s_copy(type__ + 96, time, (ftnlen)32, time_len);
    s_copy(type__ + 128, mass, (ftnlen)32, mass_len);
    s_copy(type__ + 160, charge, (ftnlen)32, charge_len);
    i__ = 1;
    s_copy(error, " ", error_len, (ftnlen)1);
    while(i__ <= 5) {
	fnducv_(type__ + (((i__1 = i__) < 6 && 0 <= i__1 ? i__1 : s_rnge(
		"type", i__1, "changu_", (ftnlen)391)) << 5), &found, &
		class__, &value, (ftnlen)32);
	if (! found) {
/* Writing concatenation */
	    i__2[0] = 19, a__1[0] = "Unrecognized unit: ";
	    i__2[1] = 32, a__1[1] = type__ + (((i__1 = i__) < 6 && 0 <= i__1 ?
		     i__1 : s_rnge("type", i__1, "changu_", (ftnlen)394)) << 
		    5);
	    s_cat(error, a__1, i__2, &c__2, error_len);
	} else if (class__ != i__) {
	    suffix_("The", &c__1, error, (ftnlen)3, error_len);
	    suffix_(tclass + (((i__1 = i__ - 1) < 5 && 0 <= i__1 ? i__1 : 
		    s_rnge("tclass", i__1, "changu_", (ftnlen)398)) << 3), &
		    c__1, error, (ftnlen)8, error_len);
	    suffix_("argument is '", &c__1, error, (ftnlen)13, error_len);
	    suffix_(type__ + (((i__1 = i__) < 6 && 0 <= i__1 ? i__1 : s_rnge(
		    "type", i__1, "changu_", (ftnlen)400)) << 5), &c__1, 
		    error, (ftnlen)32, error_len);
	    suffix_("'. This is not a unit ", &c__0, error, (ftnlen)22, 
		    error_len);
	    suffix_("of type", &c__1, error, (ftnlen)7, error_len);
	    suffix_(tclass + (((i__1 = i__ - 1) < 5 && 0 <= i__1 ? i__1 : 
		    s_rnge("tclass", i__1, "changu_", (ftnlen)403)) << 3), &
		    c__1, error, (ftnlen)8, error_len);
	    suffix_(".", &c__0, error, (ftnlen)1, error_len);
	}
	++i__;
    }
    for (i__ = 0; i__ <= 5; ++i__) {
	size[(i__1 = i__) < 6 && 0 <= i__1 ? i__1 : s_rnge("size", i__1, 
		"changu_", (ftnlen)413)] = lastnb_(type__ + (((i__3 = i__) < 
		6 && 0 <= i__3 ? i__3 : s_rnge("type", i__3, "changu_", (
		ftnlen)413)) << 5), (ftnlen)32);
    }
    return 0;

/*     Construct the units having the same dimensions as the input */
/*     but that have fundamentals (angle, length, time, ... ) in the */
/*     form that are expected by the calling program. */


L_transu:
    s_copy(string, in, (ftnlen)256, in_len);
    s_copy(o, " ", (ftnlen)256, (ftnlen)1);
    nest = 0;
    start = 1;
    f = 0;
    scan_(string, op, oplen, opptr, &c__128, &start, &ntokns, ident, beg, end,
	     (ftnlen)256, (ftnlen)2);
    i__ = 1;
    while(i__ <= ntokns) {
	b = beg[(i__1 = i__ - 1) < 128 && 0 <= i__1 ? i__1 : s_rnge("beg", 
		i__1, "changu_", (ftnlen)440)];
	e = end[(i__1 = i__ - 1) < 128 && 0 <= i__1 ? i__1 : s_rnge("end", 
		i__1, "changu_", (ftnlen)441)];
	if (ident[(i__1 = i__ - 1) < 128 && 0 <= i__1 ? i__1 : s_rnge("ident",
		 i__1, "changu_", (ftnlen)443)] == blank) {

/*              Don't do anything.... */

	} else if (ident[(i__1 = i__ - 1) < 128 && 0 <= i__1 ? i__1 : s_rnge(
		"ident", i__1, "changu_", (ftnlen)448)] != 0) {
	    s = f + 1;
	    f = s + e - b;
	    s_copy(o + (s - 1), string + (b - 1), f - (s - 1), e - (b - 1));

/*              We have to excercise a bit of caution.  If this */
/*              is an exponentiation operation, we need to just copy */
/*              the exponent to the output string. */

	    if (ident[(i__1 = i__ - 1) < 128 && 0 <= i__1 ? i__1 : s_rnge(
		    "ident", i__1, "changu_", (ftnlen)458)] == exp__) {
		nest = 0;
		pass = 0;
		while(pass < 1 || nest > 0) {
		    ++i__;
		    ++pass;
		    b = beg[(i__1 = i__ - 1) < 128 && 0 <= i__1 ? i__1 : 
			    s_rnge("beg", i__1, "changu_", (ftnlen)469)];
		    e = end[(i__1 = i__ - 1) < 128 && 0 <= i__1 ? i__1 : 
			    s_rnge("end", i__1, "changu_", (ftnlen)470)];
		    s = f + 1;
		    f = s + b - e;
		    s_copy(o + (s - 1), string + (b - 1), f - (s - 1), e - (b 
			    - 1));
		    if (ident[(i__1 = i__ - 1) < 128 && 0 <= i__1 ? i__1 : 
			    s_rnge("ident", i__1, "changu_", (ftnlen)476)] == 
			    rparen) {
			--nest;
		    } else if (ident[(i__1 = i__ - 1) < 128 && 0 <= i__1 ? 
			    i__1 : s_rnge("ident", i__1, "changu_", (ftnlen)
			    480)] == lparen) {
			++nest;
		    }
		}
	    }
	} else {

/*              If you get to this point, just copy the units */
/*              associated with the class of this token. */

	    fnducv_(string + (b - 1), &found, &class__, &value, e - (b - 1));
	    s = f + 1;
	    f = size[(i__1 = class__) < 6 && 0 <= i__1 ? i__1 : s_rnge("size",
		     i__1, "changu_", (ftnlen)499)] - 1 + s;
	    s_copy(o + (s - 1), type__ + (((i__1 = class__) < 6 && 0 <= i__1 ?
		     i__1 : s_rnge("type", i__1, "changu_", (ftnlen)500)) << 
		    5), f - (s - 1), (ftnlen)32);
	}
	++i__;
    }
    s_copy(out, o, out_len, (ftnlen)256);
    return 0;
} /* changu_ */

/* Subroutine */ int changu_(char *angle, char *length, char *time, char *
	mass, char *charge, char *in, char *out, char *error, ftnlen 
	angle_len, ftnlen length_len, ftnlen time_len, ftnlen mass_len, 
	ftnlen charge_len, ftnlen in_len, ftnlen out_len, ftnlen error_len)
{
    return changu_0_(0, angle, length, time, mass, charge, in, out, error, 
	    angle_len, length_len, time_len, mass_len, charge_len, in_len, 
	    out_len, error_len);
    }

/* Subroutine */ int ounits_(char *angle, char *length, char *time, char *
	mass, char *charge, char *error, ftnlen angle_len, ftnlen length_len, 
	ftnlen time_len, ftnlen mass_len, ftnlen charge_len, ftnlen error_len)
{
    return changu_0_(1, angle, length, time, mass, charge, (char *)0, (char *)
	    0, error, angle_len, length_len, time_len, mass_len, charge_len, (
	    ftnint)0, (ftnint)0, error_len);
    }

/* Subroutine */ int transu_(char *in, char *out, ftnlen in_len, ftnlen 
	out_len)
{
    return changu_0_(2, (char *)0, (char *)0, (char *)0, (char *)0, (char *)0,
	     in, out, (char *)0, (ftnint)0, (ftnint)0, (ftnint)0, (ftnint)0, (
	    ftnint)0, in_len, out_len, (ftnint)0);
    }

