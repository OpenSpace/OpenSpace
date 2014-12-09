/* lstmid.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__100 = 100;

/* $Procedure      LSTMID ( Find ET corresponding to LST ) */
/* Subroutine */ int lstmid_(doublereal *inpet, integer *bodyid, doublereal *
	lon, doublereal *scrate, char *mntype, integer *secoff, doublereal *
	mnet, ftnlen mntype_len)
{
    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    char ampm[6], time[24];
    integer i__;
    extern /* Subroutine */ int chkin_(char *, ftnlen), errch_(char *, char *,
	     ftnlen, ftnlen);
    integer lsecs;
    extern /* Subroutine */ int et2lst_(doublereal *, integer *, doublereal *,
	     char *, integer *, integer *, integer *, char *, char *, ftnlen, 
	    ftnlen, ftnlen);
    integer sc;
    doublereal et;
    integer hr, mn, daysec;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen);
    integer intspd;
    extern /* Subroutine */ int setmsg_(char *, ftnlen), errint_(char *, 
	    integer *, ftnlen);
    extern logical return_(void);
    extern doublereal spd_(void);

/* $ Abstract */

/*     This routine finds ET corresponding the local solar time, */
/*     specified as an integer number of seconds past midnight of */
/*     current local solar day, that is nearest, previous, or next to a */
/*     given ET. */

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

/*     None. */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     INPET      I   Input ET. */
/*     BODYID     I   NAIF ID of the body. */
/*     LON        I   Planetocentric longitude on the body. */
/*     SCRATE     I   Number of ET seconds in one local second. */
/*     MNTYPE     I   Time to look for: nearest, previous or next. */
/*     SECOFF     I   Offset from previous midnight in local seconds. */
/*     MNET       O   Output ET corresponding to the local time. */

/* $ Detailed_Input */

/*     ET             Input ephemeris time. */

/*     BODYID         NAIF ID of the body of interest. */

/*     LON            Planetocentric longitude on the body of interest. */

/*     SCRATE         Local time rate, i.e. number of ET seconds */
/*                    in one local second. */

/*     MNTYPE         Time to look for -- 'NEAREST', */
/*                    'PREVIOUS' or 'NEXT'. */

/*     SECOFF         Offset from previous midnight in local seconds. */
/*                    Must be between 0 and SPD(). */

/* $ Detailed_Output */

/*     MNET           Ephemeris time of the nearest, previous or next */
/*                    given local time to a given input ET time. */

/* $ Parameters */

/*     MAXITR         Maximum number of iteration allowed in the loop. */
/*                    Currently set to 100. Usually there is no more */
/*                    than 3-5 iterations are need to get to the */
/*                    midnight time. */

/* $ Files */

/*     None. */

/* $ Exceptions */

/*     1) If number of iteration exceeds MAXITR, routine reports */
/*        SPICE(TOOMANYITERATIONS) error. */

/*     2) If value of MNTYPE is not one of the 'NEAREST', 'PREVIOUS' */
/*        or 'NEXT', routine reports SPICE(BADMIDNIGHTTYPE) error. */

/*     3) If value of SECOFF is not between 0 and 86000, */
/*        routine reports SPICE(BADTIMEOFFSET) error. */

/* $ Particulars */

/*     This routine is required to implement local solar days (SOLs) */
/*     counting for the local solar time (LST). It's essential that */
/*     caller provides correct SCRATE for a body. Also the routine */
/*     assumes that there is enough SPICE kernel data loaded into */
/*     the program to compute LST within +/- two local solar days */
/*     of an input ET. */

/* $ Examples */

/*     Let our inputs be: */

/*        ETSTR  = '1997 AUG 12 12:00:12 TDB' */
/*        BODYID = 499 */
/*        LON    = - 33.1D0 */
/*        SCRATE = 1.0277116753731D0 */

/*        CALL STR2ET( ETSTR, ET ) */
/*        LON = LON * RPD() */

/*     then to compute nearest local midnight LSTMID must be called */
/*     follows: */

/*        CALL LSTMID( ET, BODYID, LON, SCRATE, 'NEAREST', 0, MNET ) */

/*     to compute previous local noon: */

/*        CALL LSTMID( ET, BODYID, LON, SCRATE, 'PREVIOUS', */
/*       .                                   INT( SPD()) / 2, MNET ) */

/*     to compute next local 6:00 a.m.: */

/*        CALL LSTMID( ET, BODYID, LON, SCRATE, 'NEXT', */
/*       .                                   INT( SPD()) / 4, MNET ) */

/* $ Restrictions */

/*     Sufficient SPICE kernel data loaded into the calling program to */
/*     compute LST within +/- two local solar days of an input ET. */

/* $ Author_and_Institution */

/*     B.V.Semenov      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    CHRONOS Version 1.2.0, May 10, 2006 (BVS) */

/*        Bug fix: changed logic computing iteration loop convergence */
/*        parameter to ensure that the loop terminates successfully for */
/*        input local times at the end of the local day (23:59:xx). */
/*        Fixed/improved header sections and in-line comments in a few */
/*        places. */

/* -    CHRONOS Version 1.1.0, October 16, 2002 (BVS) */

/*        Bug fix: the search loop now forces termination is the */
/*        the delta value at MAXITR is 1 or -1. This has to be done */
/*        because of the integer output from ET2LST. */

/* -    CHRONOS Version 1.0.0, May 14, 1998 (BVS) */


/* -& */

/*     Local parameters. */


/*     Local variables. */


/*     SPICELIB functions. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("LSTMID", (ftnlen)6);
    }
    et = *inpet;
    intspd = (integer) spd_();

/*     Check if input offset within the right bounds. */

    if (*secoff < 0 || *secoff > intspd) {
	setmsg_("Input offset expressed as count of local seconds must be be"
		"tween 0 and 86400.", (ftnlen)77);
	sigerr_("SPICE(BADTIMEOFFSET)", (ftnlen)20);
    }

/*     Set initial ET for our iterations depending on whether we need */
/*     compute ET for nearest, previous, or next given LST. */

    if (s_cmp(mntype, "NEAREST", mntype_len, (ftnlen)7) == 0) {

/*        Leave initial ET unchanged and assign non-zero LSECS just to */
/*        start iterations loop. */

	lsecs = 1;
    } else if (s_cmp(mntype, "PREVIOUS", mntype_len, (ftnlen)8) == 0) {

/*        Adjust current ET by the number of ET second between current */
/*        local time and previous desired local time. */

	et2lst_(&et, bodyid, lon, "PLANETOCENTRIC", &hr, &mn, &sc, time, ampm,
		 (ftnlen)14, (ftnlen)24, (ftnlen)6);
	lsecs = hr * 3600 + mn * 60 + sc - *secoff;
	if (lsecs < 0) {
	    lsecs = intspd + lsecs;
	}
	et -= *scrate * lsecs;
    } else if (s_cmp(mntype, "NEXT", mntype_len, (ftnlen)4) == 0) {

/*        Adjust current ET by the number of ET second between current */
/*        local time and following desired local time. */

	et2lst_(&et, bodyid, lon, "PLANETOCENTRIC", &hr, &mn, &sc, time, ampm,
		 (ftnlen)14, (ftnlen)24, (ftnlen)6);
	lsecs = intspd - (hr * 3600 + mn * 60 + sc - *secoff);
	if (lsecs >= intspd) {
	    lsecs -= intspd;
	}
	et += *scrate * lsecs;
    } else {

/*        Unrecognizable MNTYPE. Complain and exit. */

	setmsg_("Cannot recognize specification of the kind of local midnigh"
		"t to compute '#'. Recognizable values are 'NEAREST', 'PREVIO"
		"US' and 'NEXT'.", (ftnlen)134);
	errch_("#", mntype, (ftnlen)1, mntype_len);
	sigerr_("SPICE(BADMIDNIGHTTYPE)", (ftnlen)22);
    }

/*     We stop iterations when LSECS is exactly 0. We can do it because */
/*     ET2LST returns integer number of local hours, minutes and seconds */
/*     :). Before iterating we set counter to 0. */

    i__ = 0;
    while(lsecs != 0) {

/*        Get local time at current ET. */

	et2lst_(&et, bodyid, lon, "PLANETOCENTRIC", &hr, &mn, &sc, time, ampm,
		 (ftnlen)14, (ftnlen)24, (ftnlen)6);

/*        Compute the number of local seconds since last midnight. */

	daysec = hr * 3600 + mn * 60 + sc;

/*        Calculate the difference (in local seconds) between local */
/*        seconds since midnight given on the input and computed during */
/*        this iteration. Adjust it by the number of seconds in the day */
/*        if it happens to be greater than a half a day to make sure */
/*        that the loop does not "run away" into previous or next day. */

	if (daysec - *secoff > intspd / 2) {
	    lsecs = daysec - intspd - *secoff;
	} else if (*secoff - daysec > intspd / 2) {
	    lsecs = daysec + intspd - *secoff;
	} else {
	    lsecs = hr * 3600 + mn * 60 + sc - *secoff;
	}

/*        Adjust current ET for the next iteration. */

	et -= lsecs * *scrate;

/*        Increase iterations counter and bail out if it's over the */
/*        limit. */

	++i__;
	if (i__ > 100) {

/*           Check if the last difference value is 1 or -1 local second. */
/*           If so, force the loop termination because it has probably */
/*           been this way for many iteration already. Otherwise */
/*           complain and exit. */

	    if (lsecs == 1 || lsecs == -1) {
		lsecs = 0;
	    } else {
		setmsg_("More than # iterations in the loop determining loca"
			"l solar time midnight ET.", (ftnlen)76);
		errint_("#", &c__100, (ftnlen)1);
		sigerr_("SPICE(TOOMANYITERATIONS)", (ftnlen)24);
	    }
	}
    }

/*     Assign output value and return. */

    *mnet = et;
    chkout_("LSTMID", (ftnlen)6);
    return 0;
} /* lstmid_ */

