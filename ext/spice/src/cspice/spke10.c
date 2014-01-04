/* spke10.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c__3 = 3;
static integer c__6 = 6;

/* $Procedure SPKE10 ( Evaluate SPK record, type 10 ) */
/* Subroutine */ int spke10_(doublereal *et, doublereal *record, doublereal *
	state)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    double cos(doublereal), sin(doublereal);

    /* Local variables */
    extern /* Subroutine */ int vadd_(doublereal *, doublereal *, doublereal *
	    );
    static doublereal dmob, dwdt, mypi;
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *), mtxv_(
	    doublereal *, doublereal *, doublereal *), zzmobliq_(doublereal *,
	     doublereal *, doublereal *), eul2m_(doublereal *, doublereal *, 
	    doublereal *, integer *, integer *, integer *, doublereal *);
    static doublereal my2pi;
    extern /* Subroutine */ int zzeprcss_(doublereal *, doublereal *);
    static doublereal m[9]	/* was [3][3] */, w;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static doublereal denom, precm[9]	/* was [3][3] */;
    extern /* Subroutine */ int moved_(doublereal *, integer *, doublereal *);
    static doublereal nuobl;
    extern /* Subroutine */ int vlcom_(doublereal *, doublereal *, doublereal 
	    *, doublereal *, doublereal *);
    static doublereal vcomp[3], numer, nulon, n0;
    extern doublereal twopi_(void);
    static doublereal s1[6], s2[6], t1, t2;
    extern /* Subroutine */ int ev2lin_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    static doublereal nuobl1, nuobl2, nulon1, nulon2;
    extern doublereal pi_(void);
    static doublereal dargdt;
    extern doublereal brcktd_(doublereal *, doublereal *, doublereal *);
    extern /* Subroutine */ int dpspce_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    static doublereal fivday, mnrate;
    extern /* Subroutine */ int vlcomg_(integer *, doublereal *, doublereal *,
	     doublereal *, doublereal *, doublereal *), chkout_(char *, 
	    ftnlen);
    static logical loworb;
    static doublereal dt1, dt2, tmpsta[6];
    extern logical return_(void);
    static doublereal arg, mob;
    extern doublereal spd_(void);

/* $ Abstract */

/*     Evaluate a single SPK data record from a segment of type 10 */
/*     (NORAD two-line element sets.). */

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
/*     ET         I   Target epoch. */
/*     RECORD     I   Data record. */
/*     STATE      O   State (position and velocity). */

/* $ Detailed_Input */

/*     ET          is a target epoch, specified as ephemeris seconds past */
/*                 J2000, at which a state vector is to be computed. */

/*     RECORD      is a data record which, when evaluated at epoch ET, */
/*                 will give the state (position and velocity) of some */
/*                 body, relative to some center, in some inertial */
/*                 reference frame. */

/*                 The structure of RECORD is: */

/*                     RECORD(1) */
/*                        .            Geophysical Constants such as */
/*                        .            GM, J2, J3, J4, etc. */
/*                        . */
/*                     RECORD(NGEOCN) */

/*                     RECORD(NGEOCN + 1) */
/*                        . */
/*                        .            elements and epoch for the body */
/*                        .            at epoch 1. */
/*                        . */
/*                     RECORD(NGEOCN + NELEMN ) */

/*                     RECORD(NGEOCN + NELEMN + 1) */
/*                        . */
/*                        .            elements and epoch for the body */
/*                        .            at epoch 2. */
/*                        . */
/*                     RECORD(NGEOCN + 2*NELEMN ) */

/*                 Epoch 1 and epoch 2 are the times in the segment that */
/*                 bracket ET.  If ET is less than the first time in the */
/*                 segment then both epochs 1 and 2 are equal to the */
/*                 first time.  And if ET is greater than the last time */
/*                 then, epochs 1 and 2 are set equal to this last time. */

/* $ Detailed_Output */

/*     STATE       is the state produced by evaluating RECORD at ET. */
/*                 Units are km and km/sec. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     1) If there is a problem evaluating the two-line elements, */
/*     the error will be diagnosed by EV2LIN. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     This routine interpolates a state from the two reference sets */
/*     of two-line element sets contained in RECORD. */

/*     It is assumed that this routine is used in conjunction with */
/*     the routine SPKR10 as shown here: */

/*        CALL SPKR10 ( HANDLE, DESCR, ET, RECORD         ) */
/*        CALL SPKE10 (                ET, RECORD, STATE  ) */

/*     Where it is known in advance that the HANDLE, DESCR pair points */
/*     to a type 10 data segment. */

/* $ Examples */

/*     The SPKEnn routines are almost always used in conjunction with */
/*     the corresponding SPKRnn routines, which read the records from */
/*     SPK files. */

/*     The data returned by the SPKRnn routine is in its rawest form, */
/*     taken directly from the segment.  As such, it will be meaningless */
/*     to a user unless he/she understands the structure of the data type */
/*     completely.  Given that understanding, however, the SPKRnn */
/*     routines might be used to examine raw segment data before */
/*     evaluating it with the SPKEnn routines. */


/*     C */
/*     C     Get a segment applicable to a specified body and epoch. */
/*     C */
/*           CALL SPKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND ) */

/*     C */
/*     C     Look at parts of the descriptor. */
/*     C */
/*           CALL DAFUS ( DESCR, 2, 6, DCD, ICD ) */
/*           CENTER = ICD( 2 ) */
/*           REF    = ICD( 3 ) */
/*           TYPE   = ICD( 4 ) */

/*           IF ( TYPE .EQ. 10 ) THEN */

/*              CALL SPKR10 ( HANDLE, DESCR, ET, RECORD ) */
/*                  . */
/*                  .  Look at the RECORD data. */
/*                  . */
/*              CALL SPKE10 ( ET, RECORD, STATE ) */
/*                  . */
/*                  .  Check out the evaluated state. */
/*                  . */
/*           END IF */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman    (JPL) */
/*     W.L. Taber      (JPL) */

/* $ Version */

/* -    SPICELIB Version 1.1.0, 01-SEP-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in MTXV and VADD calls. */

/* -    SPICELIB Version 1.0.0 18-JUL-1997 (WLT) */

/* -& */
/* $ Index_Entries */

/*     evaluate type_10 spk segment */

/* -& */
/* $ Revisions */

/* -    SPICELIB Version 1.1.0, 01-SEP-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in MTXV and VADD calls. */

/* -& */

/*     SPICELIB functions */


/*     Local Parameters */



/*     The following parameters give the location of the various */
/*     geophysical parameters needed for the two line element */
/*     sets.  We need these only so that we can count how many there */
/*     are (NGEOCN). */

/*     KJ2  --- location of J2 */
/*     KJ3  --- location of J3 */
/*     KJ4  --- location if J4 */
/*     KKE  --- location of KE = sqrt(GM) in eart-radii**1.5/MIN */
/*     KQO  --- upper bound of atmospheric model in KM */
/*     KSO  --- lower bound of atmospheric model in KM */
/*     KER  --- earth equatorial radius in KM. */
/*     KAE  --- distance units/earth radius */



/*     An enumeration of the various components of the */
/*     a two-line element set.  These are needed so that we */
/*     can locate the epochs in the two sets and so that */
/*     we can count the number of elements in a two-line */
/*     element set. */

/*     KNDT20 */
/*     KNDD60 */
/*     KBSTAR */
/*     KINCL */
/*     KNODE0 */
/*     KECC */
/*     KOMEGA */
/*     KMO */
/*     KNO */
/*     KEPOCH */


/*     The nutation in obliquity and longitude as well as their rates */
/*     follow the elements.  So we've got four angles/angle rates */
/*     following the elements */


/*     The locations of the epochs and the starts of the element */
/*     sets are given below. */


/*     The parameters below give the location of the nutation terms */
/*     and their rates for the first packet. */


/*     The parameters below give the location of the nutation terms */
/*     and their rates for the second packet. */


/*     Local variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("SPKE10", (ftnlen)6);
    }
    if (first) {
	first = FALSE_;
	mypi = pi_();
	my2pi = twopi_();
	fivday = spd_() * 5.;
    }

/*     Fetch the mean motion from the first set of two-line elements */
/*     stored in the record. */

    n0 = record[16];
    mnrate = my2pi / 225.;
    loworb = n0 >= mnrate;

/*     Fetch the two epochs stored in the record. */

    t1 = record[17];
    t2 = record[31];

/*     Evaluate the two states. Call them s_1(t) and s_2(t). */
/*     Let the position and velocity components be: p_1, v_1, p_2, v_2. */

/*     The final position is a weighted average. */

/*     Let */

/*        W(t) =  0.5 + 0.5*COS( PI*(t-t1)/(t2-t1) ) */

/*     then */

/*        p  = W(t)*p_1(t) + (1 - W(t))*p_2(t) */
/*        v  = W(t)*v_1(t) + (1 - W(t))*v_2(t) + W'(t)*(p_1(t) - p_2(t)) */

/*     If t1 = t2, the state is just s(t1). */


/*     Note: there are a number of weighting schemes we could have */
/*     used.  This one has the nice property that */

/*     The graph of W is symmetric about the point */


/*        ( (t1+t2)/2,  W( (t1+t2)/2 ) */

/*     The range of W is from 1 to 0.  And the derivative of W is */
/*     symmetric and zero at both t1 and t2. */

    if (t1 != t2) {
	if (loworb) {
	    ev2lin_(et, record, &record[8], s1);
	    ev2lin_(et, record, &record[22], s2);
	} else {
	    dpspce_(et, record, &record[8], s1);
	    dpspce_(et, record, &record[22], s2);
	}

/*        Using the different element packets compute nutations in */
/*        obliquity and longitude.  Note to avoid run-away nutation */
/*        interpolation we bracket DT1 and DT2 to be between -5 and 5 */
/*        days. */

	d__1 = *et - t1;
	d__2 = -fivday;
	dt1 = brcktd_(&d__1, &d__2, &fivday);
	nuobl1 = record[18] + dt1 * record[20];
	nulon1 = record[19] + dt1 * record[21];
	d__1 = *et - t2;
	d__2 = -fivday;
	dt2 = brcktd_(&d__1, &d__2, &fivday);
	nuobl2 = record[32] + dt2 * record[34];
	nulon2 = record[33] + dt2 * record[35];

/*        Compute the weighting function that we'll need later */
/*        when we combine states 1 and 2. */

	numer = *et - t1;
	denom = t2 - t1;
	arg = numer * mypi / denom;
	dargdt = mypi / denom;
	w = cos(arg) * .5 + .5;
	dwdt = sin(arg) * -.5 * dargdt;

/*        Use the weighting function to compute the nutation in obliquity */
/*        longitude. */

	nuobl = w * nuobl1 + (1. - w) * nuobl2;
	nulon = w * nulon1 + (1. - w) * nulon2;

/*        Compute the mean obliquity at the epoch ET. */

	zzmobliq_(et, &mob, &dmob);

/*        Construct the transformation from mean of date to true of date. */

	d__1 = -mob - nuobl;
	d__2 = -nulon;
	eul2m_(&d__1, &d__2, &mob, &c__1, &c__3, &c__1, m);

/*        Use the transpose of the matrix just computed to convert */
/*        S1 and S2 from true of date to mean of date. */

	mtxv_(m, s1, tmpsta);
	mtxv_(m, &s1[3], &tmpsta[3]);
	moved_(tmpsta, &c__6, s1);
	mtxv_(m, s2, tmpsta);
	mtxv_(m, &s2[3], &tmpsta[3]);
	moved_(tmpsta, &c__6, s2);

/*        Now compute the weighted average of the two true of date */
/*        states. */

	d__1 = 1. - w;
	vlcomg_(&c__6, &w, s1, &d__1, s2, state);
	d__1 = -dwdt;
	vlcom_(&dwdt, s1, &d__1, s2, vcomp);
	vadd_(&state[3], vcomp, &tmpsta[3]);
	vequ_(&tmpsta[3], &state[3]);
    } else {
	if (loworb) {
	    ev2lin_(et, record, &record[8], state);
	} else {
	    dpspce_(et, record, &record[8], state);
	}

/*        Interpolate the nutation in longitude and obliquity.  Note */
/*        that to avoid run-away linear interpolation of the nutation */
/*        angles, we bracket DT1 to be between -5 and 5 days. */

	d__1 = *et - t1;
	d__2 = -fivday;
	dt1 = brcktd_(&d__1, &d__2, &fivday);
	nuobl = record[18] + dt1 * record[20];
	nulon = record[19] + dt1 * record[21];

/*        Get the current obliquity. */

	zzmobliq_(et, &mob, &dmob);
	d__1 = -mob - nuobl;
	d__2 = -nulon;
	eul2m_(&d__1, &d__2, &mob, &c__1, &c__3, &c__1, m);
	mtxv_(m, state, tmpsta);
	mtxv_(m, &state[3], &tmpsta[3]);
	moved_(tmpsta, &c__6, state);
    }

/*     Finally, convert the mean of date state to J2000.  First get */
/*     the rotation from J2000 to mean of date. */

    zzeprcss_(et, precm);

/*     Now convert STATE to J2000. */

    mtxv_(precm, state, tmpsta);
    mtxv_(precm, &state[3], &tmpsta[3]);
    moved_(tmpsta, &c__6, state);
    chkout_("SPKE10", (ftnlen)6);
    return 0;
} /* spke10_ */

