/* timecn.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $ Procedure TIMECN (Convert and round times) */
/* Subroutine */ int timecn_(doublereal *tconv, integer *ids, char *tout, 
	char *linet, ftnlen tout_len, ftnlen linet_len)
{
    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    extern /* Subroutine */ int sct2e_(integer *, doublereal *, doublereal *),
	     dpfmt_(doublereal *, char *, char *, ftnlen, ftnlen), reset_(
	    void);
    extern logical failed_(void);
    extern /* Subroutine */ int scdecd_(integer *, doublereal *, char *, 
	    ftnlen);
    integer sc;
    logical ok;
    extern /* Subroutine */ int ckmeta_(integer *, char *, integer *, ftnlen),
	     erract_(char *, char *, ftnlen, ftnlen);
    doublereal ettime;
    extern /* Subroutine */ int fixuni_(void), errprt_(char *, char *, ftnlen,
	     ftnlen), timout_(doublereal *, char *, char *, ftnlen, ftnlen);

/* $ Abstract */

/*     This is internal subroutine for CKBRIEF program. It converts */
/*     time between encoded SCLK, SCLK string, ET, UTC or UTC/DOY. */

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

/* $ Keywords */

/*     SUMMARY */
/*     C KERNEL */

/* $ Declarations */
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

/* $ Author_and_Institution */

/*     Y.K. Zaiko     (BERC) */
/*     B.V. Semenov   (NAIF) */

/* $ Version */

/* -    Toolkit Version 6.1.0, 27-JUN-2014 (BVS) */

/*        BUG FIX: changed logic to make a combination of -a and an ID */
/*        specified on the command line work in all cases. */

/* -    CKBRIEF Version 6.0.0, 2014-04-28 (BVS) (NJB) */

/*        Modified to treat all files as a single file (-a). */

/*        Changed SCLKD display format to include 6 decimal */
/*        places. */

/*        Increased MAXBOD to 1,000,000 (from 100,000) and CMDSIZ to */
/*        50,000 (from 25,000). */

/*        Added support for CK type 6. */

/* -    CKBRIEF Version 5.0.0, 2009-02-11 (BVS) */

/*        Updated version. */

/* -    CKBRIEF Version 4.0.0, 2008-01-13 (BVS) */

/*        Increased MAXBOD to 100,000 (from 10,000). */

/*        Increased CMDSIZ to 25,000 (from 4,000). */

/*        Updated version string and changed its format to */
/*        '#.#.#, Month DD, YYYY' (from '#.#.#, YYYY-MM-DD'). */

/* -    CKBRIEF Version 3.2.0, 2006-11-02 (BVS) */

/*        Updated version string. */

/* -    CKBRIEF Version 3.1.0, 2005-11-08 (BVS) */

/*        Updated version string. */

/* -    CKBRIEF Version 2.0.0, 2001-05-16 (BVS) */

/*        Increased MAXBOD to 10000 (from 4000). Set LRGWIN to be */
/*        MAXBOD*2 (was MAXBOD). Changed version string. */

/* -    CKBRIEF Version 1.1.2, 2001-04-09 (BVS) */

/*        Changed version parameter. */

/* -    CKBRIEF Version 1.0.0 beta, 1999-02-17 (YKZ)(BVS) */

/*        Initial release. */

/* -& */

/*     The Version is stored as a string. */


/*     The maximum number of segments or interpolation intervals */
/*     that can be summarized is stored in the parameter MAXBOD. */
/*     This is THE LIMIT that should be increased if window */
/*     routines called by CKBRIEF fail. */


/*     The largest expected window -- must be twice the size of */
/*     MAXBOD for consistency. */


/*     The longest command line that can be accommodated is */
/*     given by CMDSIZ. */


/*     MAXUSE is the maximum number of objects that can be explicitly */
/*     specified on the command line for ckbrief summaries. */


/*     Generic line size for all modules. */


/*     Time type keys. */


/*     Output time format pictures. */

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     TCONV      I   Encoded SCLK time */
/*     IDS        I   NAIF ID code of object */
/*     TOUT       I   Form of time representation on output */
/*     LINET      O   Text presentation of time */

/* $ Detailed Input */

/*     TCONV          Encoded SCLK time to be converted, rounded */
/*                    and decoded to character string */

/*     IDS            Integer NAIF ID code found in summary from which */
/*                    TCONV was obtained. */

/*     TOUT           Key specifying time presentation on output: */
/*                    SCLK string, encoded SCLK, ET, UTC or DOY UTC. */

/* $ Detailed Output */

/*     LINET          Character string which contains time converted */
/*                    to requested representation or NOTIME flag if */
/*                    conversion was not possible. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     Y.K. Zaiko      (BERC) */
/*     B.V. Semenov    (NAIF) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    CKBRIEF Beta Version 1.0.0, 17-FEB-1999 (YKZ)(BVS) */

/* -& */

/*     SPICELIB functions */


/*     Local variables */


/*     Reset output time string. */

    s_copy(linet, " ", linet_len, (ftnlen)1);

/*     It is necessary to use real spacecraft ID in SCLK<->ET */
/*     conversion routines. CKMETA is providing it. */

    ckmeta_(ids, "SCLK", &sc, (ftnlen)4);

/*     TIMECN is the special routine to be used in CKBRIEF */
/*     utility to convert times in accordance to user request. If user */
/*     haven't provided ancillary files to perform this conversion, the */
/*     program shouldn't stop. To achieve this we'll forbid TIMECN to */
/*     be aborted by SPICELIB standard error processing if it can't */
/*     convert times. On the exit from TIMECN, SPICE error handling */
/*     is restored to its original state. */

    erract_("SET", "RETURN", (ftnlen)3, (ftnlen)6);
    errprt_("SET", "NONE", (ftnlen)3, (ftnlen)4);

/*     We do appropriate conversion depending on the requested output */
/*     time representation. If SCLK for the s/c of interest and(!) */
/*     LSK file weren't loaded, conversions to string SCLK, ET, UTC */
/*     and UTC/DOY are not possible. The output time set to NOTIME */
/*     flag. */

    if (s_cmp(tout, "TICKS", tout_len, (ftnlen)5) == 0) {

/*        DP SLCKs should be simply converted to string. */

	dpfmt_(tconv, "xxxxxxxxxxxxxx.xxxxxx", linet, (ftnlen)21, linet_len);
    } else if (s_cmp(tout, "SCLK", tout_len, (ftnlen)4) == 0) {

/*        SCLK string is computed from DP SCLK if it's possible. */

	scdecd_(&sc, tconv, linet, linet_len);
	if (failed_()) {
	    s_copy(linet, "NEED LSK AND SCLK FILES", linet_len, (ftnlen)23);
	}
    } else if (s_cmp(tout, "ET", tout_len, (ftnlen)2) == 0) {

/*        Calendar ET is computed by converting DP SCLK to ET seconds */
/*        and converting them further to ET calendar string */

	sct2e_(&sc, tconv, &ettime);
	if (! failed_()) {
	    timout_(&ettime, "YYYY-MON-DD HR:MN:SC.### ::TDB", linet, (ftnlen)
		    30, linet_len);
	    if (failed_()) {
		s_copy(linet, "NEED LSK AND SCLK FILES", linet_len, (ftnlen)
			23);
	    }
	} else {
	    s_copy(linet, "NEED LSK AND SCLK FILES", linet_len, (ftnlen)23);
	}
    } else if (s_cmp(tout, "UTC", tout_len, (ftnlen)3) == 0) {

/*        UTC time is computed by converting DP SCLK to ET seconds, */
/*        which after that converted to UTC string. */

	sct2e_(&sc, tconv, &ettime);
	if (! failed_()) {
	    timout_(&ettime, "YYYY-MON-DD HR:MN:SC.###", linet, (ftnlen)24, 
		    linet_len);
	    if (failed_()) {
		s_copy(linet, "NEED LSK AND SCLK FILES", linet_len, (ftnlen)
			23);
	    }
	} else {
	    s_copy(linet, "NEED LSK AND SCLK FILES", linet_len, (ftnlen)23);
	}
    } else if (s_cmp(tout, "UTC/DOY", tout_len, (ftnlen)7) == 0) {

/*        UTCDOY time is computed by converting DP SCLK to ET seconds, */
/*        which after that converted to UTC string. */

	sct2e_(&sc, tconv, &ettime);
	if (! failed_()) {
	    timout_(&ettime, "YYYY-DOY // HR:MN:SC.###", linet, (ftnlen)24, 
		    linet_len);
	    if (failed_()) {
		s_copy(linet, "NEED LSK AND SCLK FILES", linet_len, (ftnlen)
			23);
	    }
	} else {
	    s_copy(linet, "NEED LSK AND SCLK FILES", linet_len, (ftnlen)23);
	}
    }
    ok = ! failed_();

/*     Now we can reset SPICE error handling mechanism back to its */
/*     original state. */

    reset_();
    erract_("SET", "ABORT", (ftnlen)3, (ftnlen)5);
    errprt_("SET", "DEFAULT", (ftnlen)3, (ftnlen)7);

/*     There is a bug in UNITIM (trace: SCT2E --> SCTE01 --> UNITIM) */
/*     that has to be temporarily fixed before UNITIM officially fixed */
/*     in N0049 delivery. Call to a specially written routine FIXUNI */
/*     does that. */

    if (! ok) {
	fixuni_();
    }
    return 0;
} /* timecn_ */

