/* spkw17.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__9 = 9;
static integer c__17 = 17;
static integer c__12 = 12;

/* $Procedure      SPKW17 ( SPK, write a type 17 segment ) */
/* Subroutine */ int spkw17_(integer *handle, integer *body, integer *center, 
	char *frame, doublereal *first, doublereal *last, char *segid, 
	doublereal *epoch, doublereal *eqel, doublereal *rapol, doublereal *
	decpol, ftnlen frame_len, ftnlen segid_len)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    doublereal a, h__;
    integer i__;
    doublereal k;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal descr[5];
    extern /* Subroutine */ int moved_(doublereal *, integer *, doublereal *);
    integer value;
    extern /* Subroutine */ int errdp_(char *, doublereal *, ftnlen), dafada_(
	    doublereal *, integer *), dafbna_(integer *, doublereal *, char *,
	     ftnlen), dafena_(void);
    extern logical failed_(void);
    doublereal record[12];
    extern integer lastnb_(char *, ftnlen);
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, integer *, 
	    ftnlen), spkpds_(integer *, integer *, char *, integer *, 
	    doublereal *, doublereal *, doublereal *, ftnlen);
    extern logical return_(void);
    doublereal ecc;

/* $ Abstract */

/*     Write an SPK segment of type 17 given a type 17 data record. */

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

/*     SPK */

/* $ Keywords */

/*     EPHEMERIS */

/* $ Declarations */
/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of an SPK file open for writing. */
/*     BODY       I   Body code for ephemeris object. */
/*     CENTER     I   Body code for the center of motion of the body. */
/*     FRAME      I   The reference frame of the states. */
/*     FIRST      I   First valid time for which states can be computed. */
/*     LAST       I   Last valid time for which states can be computed. */
/*     SEGID      I   Segment identifier. */
/*     EPOCH      I   Epoch of elements in seconds past J2000 */
/*     EQEL       I   Array of equinoctial elements */
/*     RAPOL      I   Right Ascension of the pole of the reference plane */
/*     DECPOL     I   Declination of the pole of the reference plane */

/* $ Detailed_Input */

/*     HANDLE      is the file handle of an SPK file that has been */
/*                 opened for writing. */

/*     BODY        is the NAIF ID for the body whose states are */
/*                 to be recorded in an SPK file. */

/*     CENTER      is the NAIF ID for the center of motion associated */
/*                 with BODY. */

/*     FRAME       is the reference frame that states are referenced to, */
/*                 for example 'J2000'. */

/*     FIRST       are the bounds on the ephemeris times, expressed as */
/*     LAST        seconds past J2000. */

/*     SEGID       is the segment identifier. An SPK segment identifier */
/*                 may contain up to 40 characters. */

/*     EPOCH      is the epoch of equinoctial elements in seconds */
/*                past the J2000 epoch. */

/*     EQEL       is an array of 9 double precision numbers that */
/*                are the equinoctial elements for some orbit relative */
/*                to the equatorial frame of a central body. */

/*                ( The z-axis of the equatorial frame is the direction */
/*                  of the pole of the central body relative to FRAME. */
/*                  The x-axis is given by the cross product of the */
/*                  Z-axis of FRAME with the direction of the pole of */
/*                  the central body.  The Y-axis completes a right */
/*                  handed frame. ) */

/*                The specific arrangement of the elements is spelled */
/*                out below.  The following terms are used in the */
/*                discussion of elements of EQEL */

/*                    INC  --- inclination of the orbit */
/*                    ARGP --- argument of periapse */
/*                    NODE --- longitude of the ascending node */
/*                    E    --- eccentricity of the orbit */

/*                EQEL(1) is the semi-major axis (A) of the orbit in km. */

/*                EQEL(2) is the value of H at the specified epoch. */
/*                        ( E*SIN(ARGP+NODE) ). */

/*                EQEL(3) is the value of K at the specified epoch */
/*                        ( E*COS(ARGP+NODE) ). */

/*                EQEL(4) is the mean longitude (MEAN0+ARGP+NODE)at */
/*                        the epoch of the elements measured in radians. */

/*                EQEL(5) is the value of P (TAN(INC/2)*SIN(NODE))at */
/*                        the specified epoch. */

/*                EQEL(6) is the value of Q (TAN(INC/2)*COS(NODE))at */
/*                        the specified epoch. */

/*                EQEL(7) is the rate of the longitude of periapse */
/*                        (dARGP/dt + dNODE/dt ) at the epoch of */
/*                        the elements.  This rate is assumed to hold */
/*                        for all time. The rate is measured in */
/*                        radians per second. */

/*                EQEL(8) is the derivative of the mean longitude */
/*                        ( dM/dt + dARGP/dt + dNODE/dt ).  This */
/*                        rate is assumed to be constant and is */
/*                        measured in radians/second. */

/*                EQEL(9) is the rate of the longitude of the ascending */
/*                        node ( dNODE/dt).  This rate is measured */
/*                        in radians per second. */

/*     RAPOL      Right Ascension of the pole of the reference plane */
/*                relative to FRAME measured in radians. */

/*     DECPOL     Declination of the pole of the reference plane */
/*                relative to FRAME measured in radians. */

/* $ Detailed_Output */

/*     None.  A type 17 segment is written to the file attached */
/*     to HANDLE. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If the semi-major axis is less than or equal to zero, the error */
/*        'SPICE(BADSEMIAXIS)' is signalled. */

/*     2) If the eccentricity of the orbit corresponding to the values */
/*        of H and K ( EQEL(2) and EQEL(3) ) is greater than 0.9 the */
/*        error 'SPICE(ECCOUTOFRANGE)' is signalled. */

/*     3) If the segment identifier has more than 40 non-blank characters */
/*        the error 'SPICE(SEGIDTOOLONG)' is signalled. */

/*     4) If the segment identifier contains non-printing characters */
/*        the error 'SPICE(NONPRINTABLECHARS)' is signalled. */

/*     5) If there are inconsistencies in the BODY, CENTER, FRAME or */
/*        FIRST and LAST times, the problem will be diagnosed by */
/*        a routine in the call tree of this routine. */

/* $ Files */

/*     A new type 17 SPK segment is written to the SPK file attached */
/*     to HANDLE. */

/* $ Particulars */

/*     This routine writes an SPK type 17 data segment to the open SPK */
/*     file according to the format described in the type 17 section of */
/*     the SPK Required Reading. The SPK file must have been opened with */
/*     write access. */

/* $ Examples */

/*     Suppose that at time EPOCH you have the classical elements */
/*     of some BODY relative to the equatorial frame of some central */
/*     body CENTER. These can be converted to equinoctial elements */
/*     and stored in an SPK file as a type 17 segment so that this */
/*     body can be used within the SPK subsystem of the SPICE system. */

/*     Below is a list of the variables used to represent the */
/*     classical elements */

/*           Variable     Meaning */
/*           --------     ---------------------------------- */
/*           A            Semi-major axis in km */
/*           ECC          Eccentricity of orbit */
/*           INC          Inclination of orbit */
/*           NODE         Longitude of the ascending node at epoch */
/*           OMEGA        Argument of periapse at epoch */
/*           M            Mean anomaly at epoch */
/*           DMDT         Mean anomaly rate in radians/second */
/*           DNODE        Rate of change of longitude of ascending node */
/*                        in radians/second */
/*           DOMEGA       Rate of change of argument of periapse in */
/*                        radians/second */
/*           EPOCH        is the epoch of the elements in seconds past */
/*                        the J2000 epoch. */


/*        These elements are converted to equinoctial elements (in */
/*        the order compatible with type 17) as shown below. */

/*           EQEL(1) = A */
/*           EQEL(2) = ECC * DSIN ( OMEGA + NODE ) */
/*           EQEL(3) = ECC * DCOS ( OMEGA + NODE ) */

/*           EQEL(4) = M + OMEGA + NODE */

/*           EQEL(5) = TAN(INC/2.0D0) * DSIN(NODE) */
/*           EQEL(6) = TAN(INC/2.0D0) * DCOS(NODE) */

/*           EQEL(7) = DOMEGA */
/*           EQEL(8) = DOMEGA + DMDT + DNODE */
/*           EQEL(9) = DNODE */


/*     C */
/*     C     Now add the segment. */
/*     C */

/*           CALL SPKW17 ( HANDLE, BODY,  CENTER, FRAME,  FIRST, LAST, */
/*          .              SEGID,  EPOCH, EQEL,   RAPOL,  DECPOL ) */


/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.0.1, 24-Jun-1999 (WLT) */

/*        Corrected typographical errors in the header. */

/* -    SPICELIB Version 1.0.0, 8-Jan-1997 (WLT) */

/* -& */
/* $ Index_Entries */

/*     Write a type 17 spk segment */

/* -& */

/*     SPICELIB Functions */


/*     Local Variables */


/*     Segment descriptor size */


/*     Segment identifier size */


/*     SPK data type */


/*     Range of printing characters */


/*     Number of items in a segment */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    }
    chkin_("SPKW17", (ftnlen)6);

/*     Fetch the various entities from the inputs and put them into */
/*     the data record, first the epoch. */

    record[0] = *epoch;

/*     The trajectory pole vector. */

    moved_(eqel, &c__9, &record[1]);
    record[10] = *rapol;
    record[11] = *decpol;
    a = record[1];
    h__ = record[2];
    k = record[3];
    ecc = sqrt(h__ * h__ + k * k);

/*     Check all the inputs here for obvious failures.  It's much */
/*     better to check them now and quit than it is to get a bogus */
/*     segment into an SPK file and diagnose it later. */

    if (a <= 0.) {
	setmsg_("The semimajor axis supplied to the SPK type 17 evaluator wa"
		"s non-positive.  This value must be positive. The value supp"
		"lied was #.", (ftnlen)130);
	errdp_("#", &a, (ftnlen)1);
	sigerr_("SPICE(BADSEMIAXIS)", (ftnlen)18);
	chkout_("SPKW17", (ftnlen)6);
	return 0;
    } else if (ecc > .9) {
	setmsg_("The eccentricity supplied for a type 17 segment is greater "
		"than 0.9.  It must be less than 0.9.The value supplied to th"
		"e type 17 evaluator was #. ", (ftnlen)146);
	errdp_("#", &ecc, (ftnlen)1);
	sigerr_("SPICE(BADECCENTRICITY)", (ftnlen)22);
	chkout_("SPKW17", (ftnlen)6);
	return 0;
    }

/*     Make sure the segment identifier is not too long. */

    if (lastnb_(segid, segid_len) > 40) {
	setmsg_("Segment identifier contains more than 40 characters.", (
		ftnlen)52);
	sigerr_("SPICE(SEGIDTOOLONG)", (ftnlen)19);
	chkout_("SPKW17", (ftnlen)6);
	return 0;
    }

/*     Make sure the segment identifier has only printing characters. */

    i__1 = lastnb_(segid, segid_len);
    for (i__ = 1; i__ <= i__1; ++i__) {
	value = *(unsigned char *)&segid[i__ - 1];
	if (value < 32 || value > 126) {
	    setmsg_("The segment identifier contains the nonprintable charac"
		    "ter having ascii code #.", (ftnlen)79);
	    errint_("#", &value, (ftnlen)1);
	    sigerr_("SPICE(NONPRINTABLECHARS)", (ftnlen)24);
	    chkout_("SPKW17", (ftnlen)6);
	    return 0;
	}
    }

/*     All of the obvious checks have been performed on the input */
/*     record.  Create the segment descriptor. (FIRST and LAST are */
/*     checked by SPKPDS as well as consistency between BODY and CENTER). */

    spkpds_(body, center, frame, &c__17, first, last, descr, frame_len);
    if (failed_()) {
	chkout_("SPKW17", (ftnlen)6);
	return 0;
    }

/*     Begin a new segment. */

    dafbna_(handle, descr, segid, segid_len);
    if (failed_()) {
	chkout_("SPKW17", (ftnlen)6);
	return 0;
    }
    dafada_(record, &c__12);
    if (! failed_()) {
	dafena_();
    }
    chkout_("SPKW17", (ftnlen)6);
    return 0;
} /* spkw17_ */

