/* spke10.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__6 = 6;

/* $Procedure SPKE10 ( Evaluate SPK record, type 10 ) */
/* Subroutine */ int spke10_(doublereal *et, doublereal *record, doublereal *
	state)
{
    /* Initialized data */

    static logical first = TRUE_;

    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double cos(doublereal), sin(doublereal);

    /* Local variables */
    extern /* Subroutine */ int vadd_(doublereal *, doublereal *, doublereal *
	    );
    static doublereal dwdt, mypi;
    extern /* Subroutine */ int vequ_(doublereal *, doublereal *), mxvg_(
	    doublereal *, doublereal *, integer *, integer *, doublereal *);
    static doublereal my2pi, w;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static doublereal denom, precm[36]	/* was [6][6] */;
    extern /* Subroutine */ int moved_(doublereal *, integer *, doublereal *),
	     vlcom_(doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *);
    static doublereal vcomp[3], numer, n0;
    extern doublereal twopi_(void);
    static doublereal s1[6], s2[6], t1, t2;
    extern /* Subroutine */ int ev2lin_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    extern doublereal pi_(void);
    static doublereal dargdt;
    extern /* Subroutine */ int dpspce_(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    static doublereal mnrate;
    extern /* Subroutine */ int vlcomg_(integer *, doublereal *, doublereal *,
	     doublereal *, doublereal *, doublereal *), chkout_(char *, 
	    ftnlen);
    static doublereal invprc[36]	/* was [6][6] */;
    static logical loworb;
    static doublereal tmpsta[6];
    extern /* Subroutine */ int zzteme_(doublereal *, doublereal *);
    extern logical return_(void);
    extern /* Subroutine */ int invstm_(doublereal *, doublereal *);
    static doublereal arg;

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

/* -    SPICELIB Version 2.0.0, 01-JAN-2011 (EDW) */

/*        Correction of state transformation calculation. Algorithm */
/*        now computes state transformation as from TEME to J2000. */
/*        The previous version of this routine calculated TETE to */
/*        J2000. */

/* -    SPICELIB Version 1.1.0, 01-SEP-2005 (NJB) */

/*        Updated to remove non-standard use of duplicate arguments */
/*        in MTXV and VADD calls. */

/* -    SPICELIB Version 1.0.0 18-JUL-1997 (WLT) */

/* -& */
/* $ Index_Entries */

/*     evaluate type_10 spk segment */

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
/*     KKE  --- location of KE = sqrt(GM) in earth-radii**1.5/MIN */
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


/*        ( (t1+t2)/2,  W( (t1+t2)/2 ) ) */

/*     The range of W is from 1 to 0. The derivative of W is */
/*     symmetric and zero at both t1 and t2. */

    if (t1 != t2) {
	if (loworb) {
	    ev2lin_(et, record, &record[8], s1);
	    ev2lin_(et, record, &record[22], s2);
	} else {
	    dpspce_(et, record, &record[8], s1);
	    dpspce_(et, record, &record[22], s2);
	}

/*        Compute the weighting function that we'll need later */
/*        when we combine states 1 and 2. */

	numer = *et - t1;
	denom = t2 - t1;
	arg = numer * mypi / denom;
	dargdt = mypi / denom;
	w = cos(arg) * .5 + .5;
	dwdt = sin(arg) * -.5 * dargdt;

/*        Now compute the weighted average of the two states. */

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
    }

/*     Finally, convert the TEME state to J2000.  First get */
/*     the rotation from J2000 to TEME... */

    zzteme_(et, precm);

/*     ...now convert STATE to J2000. Invert the state transformation */
/*     operator (important to correctly do this). */

    invstm_(precm, invprc);

/*     Map STATE to the corresponding expression in J2000. */

    mxvg_(invprc, state, &c__6, &c__6, tmpsta);
    moved_(tmpsta, &c__6, state);
    chkout_("SPKE10", (ftnlen)6);
    return 0;
} /* spke10_ */

