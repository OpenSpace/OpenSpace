/* frmdiff.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c_b4 = 1000000;
static integer c__3 = 3;
static logical c_true = TRUE_;
static logical c_false = FALSE_;
static integer c__5 = 5;
static integer c__9 = 9;
static integer c__15 = 15;
static integer c__0 = 0;

/* $Procedure   FRMDIFF ( Compare two rotations. ) */
/* Main program */ MAIN__(void)
{
    /* System generated locals */
    address a__1[3], a__2[5], a__3[9];
    integer i__1, i__2, i__3, i__4[3], i__5[5], i__6[9];
    doublereal d__1, d__2;

    /* Builtin functions */
    integer s_cmp(char *, char *, ftnlen, ftnlen), s_rnge(char *, integer, 
	    char *, integer);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen),
	     s_copy(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    static char line[1024], time[1024*2];
    static integer axes[3];
    static doublereal step;
    static integer nitr, i__;
    extern integer cardd_(doublereal *);
    extern /* Subroutine */ int etcal_(doublereal *, char *, ftnlen);
    static integer ffrid[2];
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static doublereal epoch[1000000];
    static logical avflg;
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen), 
	    repmc_(char *, char *, char *, char *, ftnlen, ftnlen, ftnlen, 
	    ftnlen);
    static integer tfrid[2];
    extern /* Subroutine */ int repmf_(char *, char *, doublereal *, integer *
	    , char *, char *, ftnlen, ftnlen, ftnlen, ftnlen), repmi_(char *, 
	    char *, integer *, char *, ftnlen, ftnlen, ftnlen);
    static char hword[32];
    static integer tlidx, count;
    static char error[1840];
    extern integer rtrim_(char *, ftnlen);
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    static char avstr[1024];
    static doublereal q1[4000000]	/* was [4][1000000] */, q2[4000000]	
	    /* was [4][1000000] */;
    extern /* Subroutine */ int dr2str_(doublereal *, char *, ftnlen);
    static doublereal et[2];
    static logical ok;
    extern /* Subroutine */ int kclear_(void);
    static logical avfflg;
    static integer sigdig;
    static char ffrnam[32*2];
    static integer sclkid[2];
    static char hlline[5120], infmsg[1024*32], kernls[1024*3], gapstr[1024], 
	    kernam[1024*2], timfmt[1024], tfrnam[32*2], diftyp[32], stpstr[
	    1024], aunits[32];
    static doublereal cmpwin[1000006], av1[3000000]	/* was [3][1000000] */
	    , av2[3000000]	/* was [3][1000000] */, winmes;
    static integer infcnt, myscid, sgtcnt, nsgcnt;
    static logical infprt[32], sample, eponly;
    extern integer wncard_(doublereal *);
    extern /* Subroutine */ int errprt_(char *, char *, ftnlen, ftnlen), 
	    ssized_(integer *, doublereal *), getcml_(char *, ftnlen), 
	    chwcml_(char *, char *, char *, integer *, char *, integer *, 
	    logical *, logical *, doublereal *, integer *, doublereal *, char 
	    *, char *, char *, integer *, integer *, char *, integer *, 
	    ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen, ftnlen), 
	    wnfetd_(doublereal *, integer *, doublereal *, doublereal *), 
	    setmsg_(char *, ftnlen), sigerr_(char *, ftnlen), ldklst_(char *, 
	    ftnlen), getqav_(logical *, char *, char *, doublereal *, integer 
	    *, logical *, doublereal *, doublereal *, logical *, char *, 
	    ftnlen, ftnlen, ftnlen), prefix_(char *, integer *, char *, 
	    ftnlen, ftnlen), suffix_(char *, integer *, char *, ftnlen, 
	    ftnlen), tostdo_(char *, ftnlen), rtdiff_(doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, integer *,
	     doublereal *, char *, logical *, logical *, char *, integer *, 
	    integer *, char *, logical *, integer *, ftnlen, ftnlen, ftnlen), 
	    chkout_(char *, ftnlen);
    static doublereal hdp1, hdp2;

/* $ Abstract */

/*     FRMDIFF is a program that samples orientation of a reference */
/*     frame known to SPICE or computes differences between orientations */
/*     of two reference frames known to SPICE, and either displays this */
/*     orientation or these differences, or shows statistics about it or */
/*     them. */

/*     For complete information about the program see FRMDIFF User's */
/*     Guide. */

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

/*     FRMDIFF.UG */

/* $ Keywords */

/*     FRAMES */
/*     CK */
/*     ROTATIONS */

/* $ Declarations */
/* $ Abstract */

/*     Include Section:  FRMDIFF Global Parameters */

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

/*     B.V. Semenov   (JPL) */

/* $ Version */

/* -    Version 2.1.0, 25-MAR-2014 (BVS). */

/*        Updated version. */

/* -    Version 2.0.0, 27-FEB-2012 (BVS). */

/*        Updated version. */

/* -    Version 1.0.0, 09-DEC-2008 (BVS). */

/* -& */

/*     Program name and version. */


/*     Command line keys. */


/*     Command line key values. */


/*     Max and min number orientations that the program can handle. */


/*     Default number orientations. */


/*     Maximum number of IDs in a CK or a binary PCK file */


/*     Line size parameters. */


/*     Version, help, usage and header display parameters. */


/*     DAF descriptor size and component counts. */


/*     Cell lower boundary. */


/*     Maximum allowed number of coverage windows. */


/*     Smallest allowed step. */


/*     Fraction of step to be used as pad at the end of intervals. */


/*     Default, minimum, and maximum numbers of significant digits */
/*     allowed for numbers in dump reports. */


/*     End of FRMDIFF parameters. */

/* $ Abstract */

/*     This file contains the number of inertial reference */
/*     frames that are currently known by the SPICE toolkit */
/*     software. */

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

/*     FRAMES */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     NINERT     P   Number of known inertial reference frames. */

/* $ Parameters */

/*     NINERT     is the number of recognized inertial reference */
/*                frames.  This value is needed by both CHGIRF */
/*                ZZFDAT, and FRAMEX. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 10-OCT-1996 (WLT) */

/* -& */
/* $ Brief_I/O */

/*     See User's Guide. */

/* $ Detailed_Input */

/*     See User's Guide. */

/* $ Detailed_Output */

/*     See User's Guide. */

/* $ Parameters */

/*     See include files in Declarations section. */

/* $ Exceptions */

/*     TBD. */

/* $ Files */

/*     See User's Guide. */

/* $ Particulars */

/*     See User's Guide. */

/* $ Examples */

/*     See User's Guide. */

/* $ Restrictions */

/*     See User's Guide. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov   (JPL) */

/* $ Version */

/* -    Version 2.1.0, 25-MAR-2014 (BVS). */

/*        BUG FIX: added exception for non-rotation cases in GETQAV. */

/*        Included 'errhnd.inc' and used LMSGLN to declare ERROR. */

/* -    Version 2.0.0, 27-FEB-2012 (BVS). */

/*        Updated to support a new command line option for specifying */
/*        the number of significant digits in dump outputs. */

/*        Updated to fetch coverage from non-primary kernels when */
/*        no primary kernels were specified. */

/*        BUG FIX: updated to load SCLK kernel before calling RTDIFF for */
/*        runs when it is the only primary kernel given on the command */
/*        line. */

/* -    Version 1.1.0, 28-OCT-2011 (BVS). */

/*        Moved PARCML to support. */

/* -    Version 1.0.0, 10-FEB-2009 (BVS). */

/* -& */

/*     Local variables */

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


/*     Include File:  SPICELIB Error Handling Parameters */

/*        errhnd.inc  Version 2    18-JUN-1997 (WLT) */

/*           The size of the long error message was */
/*           reduced from 25*80 to 23*80 so that it */
/*           will be accepted by the Microsoft Power Station */
/*           FORTRAN compiler which has an upper bound */
/*           of 1900 for the length of a character string. */

/*        errhnd.inc  Version 1    29-JUL-1997 (NJB) */



/*     Maximum length of the long error message: */


/*     Maximum length of the short error message: */


/*     End Include File:  SPICELIB Error Handling Parameters */


/*     SPICELIB functions. */


/*     Save everything to prevent potential memory problems in f2c'ed */
/*     version. */


/*     In-line function definitions. */


/*     Check in. */

    chkin_("frmdiff", (ftnlen)7);

/*     Reset default error messages. */

    errprt_("SET", "NONE, SHORT, LONG, TRACEBACK", (ftnlen)3, (ftnlen)28);

/*     Initialize comparison window. */

    ssized_(&c_b4, cmpwin);

/*     Get command line and call the "big kahuna" deal-with-command-line */
/*     routine to get back all needed setups. */

    getcml_(line, (ftnlen)1024);
    chwcml_(line, kernam, ffrnam, ffrid, tfrnam, tfrid, &avflg, &avfflg, 
	    cmpwin, &nitr, &step, diftyp, timfmt, kernls, sclkid, axes, 
	    aunits, &sigdig, (ftnlen)1024, (ftnlen)1024, (ftnlen)32, (ftnlen)
	    32, (ftnlen)32, (ftnlen)1024, (ftnlen)1024, (ftnlen)32);

/*     Set flag indicating whether we are running in comparison mode or */
/*     in sampling mode. When two files were provided, we will do */
/*     sampling if ``from'' frames are the same, ``to'' frames are the */
/*     same, file-specific kernels are the same, and files are the same. */
/*     When one of no files were provided, we will do sampling if */
/*     ``from'' frames are the same, ``to'' frames are the same, and */
/*     file-specific kernels are the same. */

    if (s_cmp(kernam, " ", (ftnlen)1024, (ftnlen)1) != 0 && s_cmp(kernam + 
	    1024, " ", (ftnlen)1024, (ftnlen)1) != 0) {
	sample = eqstr_(ffrnam, ffrnam + 32, (ftnlen)32, (ftnlen)32) && 
		eqstr_(tfrnam, tfrnam + 32, (ftnlen)32, (ftnlen)32) && s_cmp(
		kernls, kernls + 1024, (ftnlen)1024, (ftnlen)1024) == 0 && 
		s_cmp(kernam, kernam + 1024, (ftnlen)1024, (ftnlen)1024) == 0;
    } else {
	sample = eqstr_(ffrnam, ffrnam + 32, (ftnlen)32, (ftnlen)32) && 
		eqstr_(tfrnam, tfrnam + 32, (ftnlen)32, (ftnlen)32) && s_cmp(
		kernls, kernls + 1024, (ftnlen)1024, (ftnlen)1024) == 0;
    }

/*     Count how many singleton and non-singleton intervals comparison */
/*     window contains and get the measure of the window. */

    winmes = 0.;
    sgtcnt = 0;
    nsgcnt = 0;
    i__1 = cardd_(cmpwin);
    for (i__ = 1; i__ <= i__1; i__ += 2) {
	winmes += cmpwin[(i__2 = i__ + 6) < 1000006 && 0 <= i__2 ? i__2 : 
		s_rnge("cmpwin", i__2, "frmdiff_", (ftnlen)269)] - cmpwin[(
		i__3 = i__ + 5) < 1000006 && 0 <= i__3 ? i__3 : s_rnge("cmpw"
		"in", i__3, "frmdiff_", (ftnlen)269)];
	if (cmpwin[(i__2 = i__ + 6) < 1000006 && 0 <= i__2 ? i__2 : s_rnge(
		"cmpwin", i__2, "frmdiff_", (ftnlen)271)] == cmpwin[(i__3 = 
		i__ + 5) < 1000006 && 0 <= i__3 ? i__3 : s_rnge("cmpwin", 
		i__3, "frmdiff_", (ftnlen)271)]) {
	    ++sgtcnt;
	} else {
	    ++nsgcnt;
	}
    }

/*     Determine time step if it was not given on the command line. */

    if (step == 0.) {

/*        Check how the number of points for comparison returned by */
/*        CHWCML relates to these singleton/non-singleton interval */
/*        counts. Recall that for each singleton interval the time */
/*        array will contain one point while for each non-singleton */
/*        it will contain at least two points -- one for each */
/*        interval end. */

	if (nitr <= sgtcnt + (nsgcnt << 1)) {

/*           The requested number of points for comparison is less or */
/*           equal to the minimum number that we should have for the */
/*           intervals in our comparison window. In this case, we set */
/*           step to a value that won't produce any points within */
/*           intervals. NITR will be reset to the minimum number */
/*           later, in the loop that computes epochs. */

/* Computing MAX */
	    d__1 = 1., d__2 = winmes + 1.;
	    step = max(d__1,d__2);
	} else {

/*           The requested number of points for comparison is greater */
/*           than the minimum number that we should have for the */
/*           intervals in our comparison window. In this case, we */
/*           have some points to distribute within non-singleton */
/*           intervals and to do that we need to compute the step */
/*           using which they should be distributed. We do this by */
/*           dividing the window measure by the difference between */
/*           requested number of points and the minimum number of */
/*           intervals. Note that the step computed this way is */
/*           larger than the step that we could have gotten if we */
/*           have divided by the difference between requested number */
/*           and the total number of intervals. But, unfortunately, */
/*           the smaller step computed using the latter approach may */
/*           cause epoch buffer overflow due to round off. */

	    step = winmes / (nitr - (sgtcnt + (nsgcnt << 1)));
	}
    }

/*     Using step generate array of times at which attitudes will be */
/*     computed using the same algorithm as was used in CHWCML. Note */
/*     that although the step should not cause overflow of the epoch */
/*     buffer (the input was verified to and the computed step was */
/*     set to not to do so), we will still check for overflow and */
/*     generate bug-type errors if we see it. */

    nitr = 0;
    i__1 = wncard_(cmpwin);
    for (i__ = 1; i__ <= i__1; ++i__) {

/*        Fetch endpoints of the next interval. */

	wnfetd_(cmpwin, &i__, et, &et[1]);

/*        Add one step for start of the interval. */

	if (nitr + 1 <= 1000000) {
	    ++nitr;
	    epoch[(i__2 = nitr - 1) < 1000000 && 0 <= i__2 ? i__2 : s_rnge(
		    "epoch", i__2, "frmdiff_", (ftnlen)349)] = et[0];
	} else {
	    setmsg_("There is a bug in the program. Please, contact NAIF.", (
		    ftnlen)52);
	    sigerr_("SPICE(FRMDIFFBUG3)", (ftnlen)18);
	}

/*        Add one step for each point between endpoints up to the end */
/*        of the interval minus padding. */

	hdp2 = et[1] - step * .5;
	count = 1;
	hdp1 = et[0] + step * count;
	while(hdp1 < hdp2 && nitr < 1000000) {
	    ++nitr;
	    epoch[(i__2 = nitr - 1) < 1000000 && 0 <= i__2 ? i__2 : s_rnge(
		    "epoch", i__2, "frmdiff_", (ftnlen)368)] = hdp1;
	    ++count;
	    hdp1 = et[0] + step * count;
	}

/*        If interval begin time is not equal to interval end time */
/*        add one step for the end of the interval. */

	if (et[0] != et[1]) {
	    if (nitr + 1 <= 1000000) {
		++nitr;
		epoch[(i__2 = nitr - 1) < 1000000 && 0 <= i__2 ? i__2 : 
			s_rnge("epoch", i__2, "frmdiff_", (ftnlen)383)] = et[
			1];
	    } else {
		setmsg_("There is a bug in the program. Please, contact NAIF."
			, (ftnlen)52);
		sigerr_("SPICE(FRMDIFFBUG4)", (ftnlen)18);
	    }
	}
    }

/*     Set flag indicating that the number of steps is equal to the */
/*     number of end points. */

    eponly = nitr == sgtcnt + (nsgcnt << 1);

/*     With the first and second attitude set attributes and epochs */
/*     in hand we can now compute the attitude. How we do this */
/*     depends on how many files were provided on the command line. */

    if (s_cmp(kernam, " ", (ftnlen)1024, (ftnlen)1) != 0 && s_cmp(kernam + 
	    1024, " ", (ftnlen)1024, (ftnlen)1) != 0) {

/*        Compute one or two attitude sets depending on sampling */
/*        flag. */

	if (sample) {

/*           Compute one attitude set. */

/*           Unload everything; load additional kernels for first */
/*           attitude; check that first attitude cannot be computed */
/*           without the first file at all epochs (except for */
/*           inertial frames); load the first file; compute and */
/*           buffer attitude (and AV if requested) for all epochs. */

	    kclear_();
/* Writing concatenation */
	    i__4[0] = rtrim_(kernls + 2048, (ftnlen)1024), a__1[0] = kernls + 
		    2048;
	    i__4[1] = 1, a__1[1] = " ";
	    i__4[2] = 1024, a__1[2] = kernls;
	    s_cat(hlline, a__1, i__4, &c__3, (ftnlen)5120);
	    ldklst_(hlline, (ftnlen)5120);
	    getqav_(&c_true, ffrnam, tfrnam, epoch, &nitr, &avflg, q1, av1, &
		    ok, error, (ftnlen)32, (ftnlen)32, (ftnlen)1840);
	    if (! ok && ! (tfrid[0] > 0 && tfrid[0] <= 21 && ffrid[0] > 0 && 
		    ffrid[0] <= 21)) {
		setmsg_("The attitude must not be computable using only supp"
			"orting kernels '#' provided after the keys '#' and '"
			"#' without loading the first kernel '#'. #.", (ftnlen)
			146);
		errch_("#", hlline, (ftnlen)1, (ftnlen)5120);
		errch_("#", "-k", (ftnlen)1, (ftnlen)2);
		errch_("#", "-k1", (ftnlen)1, (ftnlen)3);
		errch_("#", kernam, (ftnlen)1, (ftnlen)1024);
		errch_("#", error, (ftnlen)1, (ftnlen)1840);
		sigerr_("SPICE(JEOPARDIZEDRUN0)", (ftnlen)22);
	    }
	    ldklst_(kernam, (ftnlen)1024);
	    getqav_(&c_false, ffrnam, tfrnam, epoch, &nitr, &avflg, q1, av1, &
		    ok, error, (ftnlen)32, (ftnlen)32, (ftnlen)1840);
	    if (! ok) {
		setmsg_("The attitude must be computable using the first ker"
			"nel '#' and supporting kernels '#' provided after th"
			"e keys '#' and '#'. #", (ftnlen)124);
		errch_("#", kernam, (ftnlen)1, (ftnlen)1024);
		errch_("#", hlline, (ftnlen)1, (ftnlen)5120);
		errch_("#", "-k", (ftnlen)1, (ftnlen)2);
		errch_("#", "-k1", (ftnlen)1, (ftnlen)3);
		errch_("#", error, (ftnlen)1, (ftnlen)1840);
		sigerr_("SPICE(NOTENOUGHDATA0)", (ftnlen)21);
	    }

/*           Set second attitude to identity and AV to zero. */

	    i__1 = nitr;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		q2[(i__2 = (i__ << 2) - 4) < 4000000 && 0 <= i__2 ? i__2 : 
			s_rnge("q2", i__2, "frmdiff_", (ftnlen)470)] = 1.;
		q2[(i__2 = (i__ << 2) - 3) < 4000000 && 0 <= i__2 ? i__2 : 
			s_rnge("q2", i__2, "frmdiff_", (ftnlen)471)] = 0.;
		q2[(i__2 = (i__ << 2) - 2) < 4000000 && 0 <= i__2 ? i__2 : 
			s_rnge("q2", i__2, "frmdiff_", (ftnlen)472)] = 0.;
		q2[(i__2 = (i__ << 2) - 1) < 4000000 && 0 <= i__2 ? i__2 : 
			s_rnge("q2", i__2, "frmdiff_", (ftnlen)473)] = 0.;
		av2[(i__2 = i__ * 3 - 3) < 3000000 && 0 <= i__2 ? i__2 : 
			s_rnge("av2", i__2, "frmdiff_", (ftnlen)474)] = 0.;
		av2[(i__2 = i__ * 3 - 2) < 3000000 && 0 <= i__2 ? i__2 : 
			s_rnge("av2", i__2, "frmdiff_", (ftnlen)475)] = 0.;
		av2[(i__2 = i__ * 3 - 1) < 3000000 && 0 <= i__2 ? i__2 : 
			s_rnge("av2", i__2, "frmdiff_", (ftnlen)476)] = 0.;
	    }
	} else {

/*           Compute two attitude sets. */

/*           Unload everything; load additional kernels for first */
/*           attitude; check that first attitude cannot be computed */
/*           without first file at all epochs (except for inertial */
/*           frames); load first file; compute and buffer attitude */
/*           (and AV if requested) for all epochs. */

	    kclear_();
/* Writing concatenation */
	    i__4[0] = rtrim_(kernls + 2048, (ftnlen)1024), a__1[0] = kernls + 
		    2048;
	    i__4[1] = 1, a__1[1] = " ";
	    i__4[2] = 1024, a__1[2] = kernls;
	    s_cat(hlline, a__1, i__4, &c__3, (ftnlen)5120);
	    ldklst_(hlline, (ftnlen)5120);
	    getqav_(&c_true, ffrnam, tfrnam, epoch, &nitr, &avflg, q1, av1, &
		    ok, error, (ftnlen)32, (ftnlen)32, (ftnlen)1840);
	    if (! ok && ! (tfrid[0] > 0 && tfrid[0] <= 21 && ffrid[0] > 0 && 
		    ffrid[0] <= 21)) {
		setmsg_("The first attitude to be compared must not be compu"
			"table using only supporting kernels '#' provided aft"
			"er the keys '#' and '#' without loading the first ke"
			"rnel '#'. #.", (ftnlen)167);
		errch_("#", hlline, (ftnlen)1, (ftnlen)5120);
		errch_("#", "-k", (ftnlen)1, (ftnlen)2);
		errch_("#", "-k1", (ftnlen)1, (ftnlen)3);
		errch_("#", kernam, (ftnlen)1, (ftnlen)1024);
		errch_("#", error, (ftnlen)1, (ftnlen)1840);
		sigerr_("SPICE(JEOPARDIZEDRUN1)", (ftnlen)22);
	    }
	    ldklst_(kernam, (ftnlen)1024);
	    getqav_(&c_false, ffrnam, tfrnam, epoch, &nitr, &avflg, q1, av1, &
		    ok, error, (ftnlen)32, (ftnlen)32, (ftnlen)1840);
	    if (! ok) {
		setmsg_("The first attitude to be compared must be computabl"
			"e using the first kernel '#' and supporting  kernels"
			" '#' provided after the keys '#' and '#'. #", (ftnlen)
			146);
		errch_("#", kernam, (ftnlen)1, (ftnlen)1024);
		errch_("#", hlline, (ftnlen)1, (ftnlen)5120);
		errch_("#", "-k", (ftnlen)1, (ftnlen)2);
		errch_("#", "-k1", (ftnlen)1, (ftnlen)3);
		errch_("#", error, (ftnlen)1, (ftnlen)1840);
		sigerr_("SPICE(NOTENOUGHDATA1)", (ftnlen)21);
	    }

/*           Unload everything; load additional kernels for second */
/*           attitude; check that second attitude cannot be computed */
/*           without second file at all epochs (except for inertial */
/*           frames); load second file; compute and buffer attitude */
/*           (and AV if requested) for all epochs. */

	    kclear_();
/* Writing concatenation */
	    i__4[0] = rtrim_(kernls + 2048, (ftnlen)1024), a__1[0] = kernls + 
		    2048;
	    i__4[1] = 1, a__1[1] = " ";
	    i__4[2] = 1024, a__1[2] = kernls + 1024;
	    s_cat(hlline, a__1, i__4, &c__3, (ftnlen)5120);
	    ldklst_(hlline, (ftnlen)5120);
	    getqav_(&c_true, ffrnam + 32, tfrnam + 32, epoch, &nitr, &avflg, 
		    q2, av2, &ok, error, (ftnlen)32, (ftnlen)32, (ftnlen)1840)
		    ;
	    if (! ok && ! (tfrid[1] > 0 && tfrid[1] <= 21 && ffrid[1] > 0 && 
		    ffrid[1] <= 21)) {
		setmsg_("The second attitude to be compared must not be comp"
			"utable using only supporting kernels '#' provided af"
			"ter the keys '#' and '#' without loading the second "
			"kernel '#'. #.", (ftnlen)169);
		errch_("#", hlline, (ftnlen)1, (ftnlen)5120);
		errch_("#", "-k", (ftnlen)1, (ftnlen)2);
		errch_("#", "-k2", (ftnlen)1, (ftnlen)3);
		errch_("#", kernam + 1024, (ftnlen)1, (ftnlen)1024);
		errch_("#", error, (ftnlen)1, (ftnlen)1840);
		sigerr_("SPICE(JEOPARDIZEDRUN2)", (ftnlen)22);
	    }
	    ldklst_(kernam + 1024, (ftnlen)1024);
	    getqav_(&c_false, ffrnam + 32, tfrnam + 32, epoch, &nitr, &avflg, 
		    q2, av2, &ok, error, (ftnlen)32, (ftnlen)32, (ftnlen)1840)
		    ;
	    if (! ok) {
		setmsg_("The second attitude to be compared must be computab"
			"le using the second kernel '#' and supporting kernel"
			"s '#' provided after the keys '#' and '#'. #", (
			ftnlen)147);
		errch_("#", kernam + 1024, (ftnlen)1, (ftnlen)1024);
		errch_("#", hlline, (ftnlen)1, (ftnlen)5120);
		errch_("#", "-k", (ftnlen)1, (ftnlen)2);
		errch_("#", "-k2", (ftnlen)1, (ftnlen)3);
		errch_("#", error, (ftnlen)1, (ftnlen)1840);
		sigerr_("SPICE(NOTENOUGHDATA2)", (ftnlen)21);
	    }
	}
    } else if (s_cmp(kernam + 1024, " ", (ftnlen)1024, (ftnlen)1) != 0) {

/*        Only one kernel was provided on the command line. Compute */
/*        one or two attitude sets depending on sampling flag. */

	if (sample) {

/*           Compute one attitude set. */

/*           Unload everything; load additional kernels for first */
/*           attitude; check that first attitude cannot be computed */
/*           without the input file at all epochs (except for */
/*           inertial frames); load the file; compute and buffer */
/*           attitude (and AV if requested) for all epochs. */

	    kclear_();
/* Writing concatenation */
	    i__4[0] = rtrim_(kernls + 2048, (ftnlen)1024), a__1[0] = kernls + 
		    2048;
	    i__4[1] = 1, a__1[1] = " ";
	    i__4[2] = 1024, a__1[2] = kernls;
	    s_cat(hlline, a__1, i__4, &c__3, (ftnlen)5120);
	    ldklst_(hlline, (ftnlen)5120);
	    getqav_(&c_true, ffrnam, tfrnam, epoch, &nitr, &avflg, q1, av1, &
		    ok, error, (ftnlen)32, (ftnlen)32, (ftnlen)1840);
	    if (! ok && ! (tfrid[0] > 0 && tfrid[0] <= 21 && ffrid[0] > 0 && 
		    ffrid[0] <= 21)) {
		setmsg_("The attitude must not be computable using only supp"
			"orting kernels '#' provided after the keys '#' and '"
			"#' without loading the kernel '#'. #.", (ftnlen)140);
		errch_("#", hlline, (ftnlen)1, (ftnlen)5120);
		errch_("#", "-k", (ftnlen)1, (ftnlen)2);
		errch_("#", "-k1", (ftnlen)1, (ftnlen)3);
		errch_("#", kernam + 1024, (ftnlen)1, (ftnlen)1024);
		errch_("#", error, (ftnlen)1, (ftnlen)1840);
		sigerr_("SPICE(JEOPARDIZEDRUN3)", (ftnlen)22);
	    }
	    ldklst_(kernam + 1024, (ftnlen)1024);
	    getqav_(&c_false, ffrnam, tfrnam, epoch, &nitr, &avflg, q1, av1, &
		    ok, error, (ftnlen)32, (ftnlen)32, (ftnlen)1840);
	    if (! ok) {
		setmsg_("The attitude must be computable using the kernel '#"
			"' and supporting kernels '#' provided after the keys"
			" '#' and '#'. #", (ftnlen)118);
		errch_("#", kernam + 1024, (ftnlen)1, (ftnlen)1024);
		errch_("#", hlline, (ftnlen)1, (ftnlen)5120);
		errch_("#", "-k", (ftnlen)1, (ftnlen)2);
		errch_("#", "-k1", (ftnlen)1, (ftnlen)3);
		errch_("#", error, (ftnlen)1, (ftnlen)1840);
		sigerr_("SPICE(NOTENOUGHDATA3)", (ftnlen)21);
	    }

/*           Set second attitude to identity and AV to zero. */

	    i__1 = nitr;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		q2[(i__2 = (i__ << 2) - 4) < 4000000 && 0 <= i__2 ? i__2 : 
			s_rnge("q2", i__2, "frmdiff_", (ftnlen)651)] = 1.;
		q2[(i__2 = (i__ << 2) - 3) < 4000000 && 0 <= i__2 ? i__2 : 
			s_rnge("q2", i__2, "frmdiff_", (ftnlen)652)] = 0.;
		q2[(i__2 = (i__ << 2) - 2) < 4000000 && 0 <= i__2 ? i__2 : 
			s_rnge("q2", i__2, "frmdiff_", (ftnlen)653)] = 0.;
		q2[(i__2 = (i__ << 2) - 1) < 4000000 && 0 <= i__2 ? i__2 : 
			s_rnge("q2", i__2, "frmdiff_", (ftnlen)654)] = 0.;
		av2[(i__2 = i__ * 3 - 3) < 3000000 && 0 <= i__2 ? i__2 : 
			s_rnge("av2", i__2, "frmdiff_", (ftnlen)655)] = 0.;
		av2[(i__2 = i__ * 3 - 2) < 3000000 && 0 <= i__2 ? i__2 : 
			s_rnge("av2", i__2, "frmdiff_", (ftnlen)656)] = 0.;
		av2[(i__2 = i__ * 3 - 1) < 3000000 && 0 <= i__2 ? i__2 : 
			s_rnge("av2", i__2, "frmdiff_", (ftnlen)657)] = 0.;
	    }
	} else {

/*           Compute two attitude sets. */

/*           Unload everything; load additional kernels for first */
/*           attitude; check that first attitude cannot be computed */
/*           without the input file at all epochs (except for */
/*           inertial frames); load the file; compute and buffer */
/*           attitude (and AV if requested) for all epochs */

	    kclear_();
/* Writing concatenation */
	    i__4[0] = rtrim_(kernls + 2048, (ftnlen)1024), a__1[0] = kernls + 
		    2048;
	    i__4[1] = 1, a__1[1] = " ";
	    i__4[2] = 1024, a__1[2] = kernls;
	    s_cat(hlline, a__1, i__4, &c__3, (ftnlen)5120);
	    ldklst_(hlline, (ftnlen)5120);
	    getqav_(&c_true, ffrnam, tfrnam, epoch, &nitr, &avflg, q1, av1, &
		    ok, error, (ftnlen)32, (ftnlen)32, (ftnlen)1840);
	    if (! ok && ! (tfrid[0] > 0 && tfrid[0] <= 21 && ffrid[0] > 0 && 
		    ffrid[0] <= 21)) {
		setmsg_("The first attitude to be compared must not be compu"
			"table using only supporting kernels '#' provided aft"
			"er the keys '#' and '#' without loading the kernel '"
			"#'. #.", (ftnlen)161);
		errch_("#", hlline, (ftnlen)1, (ftnlen)5120);
		errch_("#", "-k", (ftnlen)1, (ftnlen)2);
		errch_("#", "-k1", (ftnlen)1, (ftnlen)3);
		errch_("#", kernam + 1024, (ftnlen)1, (ftnlen)1024);
		errch_("#", error, (ftnlen)1, (ftnlen)1840);
		sigerr_("SPICE(JEOPARDIZEDRUN4)", (ftnlen)22);
	    }
	    ldklst_(kernam + 1024, (ftnlen)1024);
	    getqav_(&c_false, ffrnam, tfrnam, epoch, &nitr, &avflg, q1, av1, &
		    ok, error, (ftnlen)32, (ftnlen)32, (ftnlen)1840);
	    if (! ok) {
		setmsg_("The first attitude to be compared must be computabl"
			"e using the kernel '#' and supporting kernels '#' pr"
			"ovided after the keys '#' and '#'. #", (ftnlen)139);
		errch_("#", kernam + 1024, (ftnlen)1, (ftnlen)1024);
		errch_("#", hlline, (ftnlen)1, (ftnlen)5120);
		errch_("#", "-k", (ftnlen)1, (ftnlen)2);
		errch_("#", "-k1", (ftnlen)1, (ftnlen)3);
		errch_("#", error, (ftnlen)1, (ftnlen)1840);
		sigerr_("SPICE(NOTENOUGHDATA4)", (ftnlen)21);
	    }

/*           Unload everything; load additional kernels for second */
/*           attitude; check that second attitude cannot be computed */
/*           without the input file at all epochs (except for */
/*           inertial frames); load the file; compute and buffer */
/*           attitude (and AV if requested) for all epochs */

	    kclear_();
/* Writing concatenation */
	    i__4[0] = rtrim_(kernls + 2048, (ftnlen)1024), a__1[0] = kernls + 
		    2048;
	    i__4[1] = 1, a__1[1] = " ";
	    i__4[2] = 1024, a__1[2] = kernls + 1024;
	    s_cat(hlline, a__1, i__4, &c__3, (ftnlen)5120);
	    ldklst_(hlline, (ftnlen)5120);
	    getqav_(&c_true, ffrnam + 32, tfrnam + 32, epoch, &nitr, &avflg, 
		    q2, av2, &ok, error, (ftnlen)32, (ftnlen)32, (ftnlen)1840)
		    ;
	    if (! ok && ! (tfrid[1] > 0 && tfrid[1] <= 21 && ffrid[1] > 0 && 
		    ffrid[1] <= 21)) {
		setmsg_("The second attitude to be compared must not be comp"
			"utable using only supporting kernels '#' provided af"
			"ter the keys '#' and '#' without loading the kernel "
			"'#'. #.", (ftnlen)162);
		errch_("#", hlline, (ftnlen)1, (ftnlen)5120);
		errch_("#", "-k", (ftnlen)1, (ftnlen)2);
		errch_("#", "-k2", (ftnlen)1, (ftnlen)3);
		errch_("#", kernam + 1024, (ftnlen)1, (ftnlen)1024);
		errch_("#", error, (ftnlen)1, (ftnlen)1840);
		sigerr_("SPICE(JEOPARDIZEDRUN5)", (ftnlen)22);
	    }
	    ldklst_(kernam + 1024, (ftnlen)1024);
	    getqav_(&c_false, ffrnam + 32, tfrnam + 32, epoch, &nitr, &avflg, 
		    q2, av2, &ok, error, (ftnlen)32, (ftnlen)32, (ftnlen)1840)
		    ;
	    if (! ok) {
		setmsg_("The second attitude to be compared must be computab"
			"le using the kernel '#' and supporting kernels '#' p"
			"rovided after the keys '#' and '#'. #", (ftnlen)140);
		errch_("#", kernam + 1024, (ftnlen)1, (ftnlen)1024);
		errch_("#", hlline, (ftnlen)1, (ftnlen)5120);
		errch_("#", "-k", (ftnlen)1, (ftnlen)2);
		errch_("#", "-k2", (ftnlen)1, (ftnlen)3);
		errch_("#", error, (ftnlen)1, (ftnlen)1840);
		sigerr_("SPICE(NOTENOUGHDATA5)", (ftnlen)21);
	    }
	}
    } else {

/*        No kernels were provided on the command line.   Compute */
/*        one or two attitude sets depending on sampling flag. */

	if (sample) {

/*           Compute one attitude set. */

/*           Unload everything; load additional kernels for first */
/*           attitude; compute and buffer attitude (and AV if */
/*           requested) for all epochs */

	    kclear_();
/* Writing concatenation */
	    i__4[0] = rtrim_(kernls + 2048, (ftnlen)1024), a__1[0] = kernls + 
		    2048;
	    i__4[1] = 1, a__1[1] = " ";
	    i__4[2] = 1024, a__1[2] = kernls;
	    s_cat(hlline, a__1, i__4, &c__3, (ftnlen)5120);
	    ldklst_(hlline, (ftnlen)5120);
	    getqav_(&c_false, ffrnam, tfrnam, epoch, &nitr, &avflg, q1, av1, &
		    ok, error, (ftnlen)32, (ftnlen)32, (ftnlen)1840);
	    if (! ok) {
		setmsg_("The attitude must be computable using the supportin"
			"g kernels '#' provided after the keys '#' and '#'. #",
			 (ftnlen)103);
		errch_("#", hlline, (ftnlen)1, (ftnlen)5120);
		errch_("#", "-k", (ftnlen)1, (ftnlen)2);
		errch_("#", "-k1", (ftnlen)1, (ftnlen)3);
		errch_("#", error, (ftnlen)1, (ftnlen)1840);
		sigerr_("SPICE(NOTENOUGHDATA6)", (ftnlen)21);
	    }

/*           Set second attitude to identity. */

	    i__1 = nitr;
	    for (i__ = 1; i__ <= i__1; ++i__) {
		q2[(i__2 = (i__ << 2) - 4) < 4000000 && 0 <= i__2 ? i__2 : 
			s_rnge("q2", i__2, "frmdiff_", (ftnlen)808)] = 1.;
		q2[(i__2 = (i__ << 2) - 3) < 4000000 && 0 <= i__2 ? i__2 : 
			s_rnge("q2", i__2, "frmdiff_", (ftnlen)809)] = 0.;
		q2[(i__2 = (i__ << 2) - 2) < 4000000 && 0 <= i__2 ? i__2 : 
			s_rnge("q2", i__2, "frmdiff_", (ftnlen)810)] = 0.;
		q2[(i__2 = (i__ << 2) - 1) < 4000000 && 0 <= i__2 ? i__2 : 
			s_rnge("q2", i__2, "frmdiff_", (ftnlen)811)] = 0.;
		av2[(i__2 = i__ * 3 - 3) < 3000000 && 0 <= i__2 ? i__2 : 
			s_rnge("av2", i__2, "frmdiff_", (ftnlen)812)] = 0.;
		av2[(i__2 = i__ * 3 - 2) < 3000000 && 0 <= i__2 ? i__2 : 
			s_rnge("av2", i__2, "frmdiff_", (ftnlen)813)] = 0.;
		av2[(i__2 = i__ * 3 - 1) < 3000000 && 0 <= i__2 ? i__2 : 
			s_rnge("av2", i__2, "frmdiff_", (ftnlen)814)] = 0.;
	    }
	} else {

/*           Compute two attitude sets. */

/*           Unload everything; load additional kernels for first */
/*           attitude; compute and buffer attitude (and AV if */
/*           requested) for all epochs. */

	    kclear_();
/* Writing concatenation */
	    i__4[0] = rtrim_(kernls + 2048, (ftnlen)1024), a__1[0] = kernls + 
		    2048;
	    i__4[1] = 1, a__1[1] = " ";
	    i__4[2] = 1024, a__1[2] = kernls;
	    s_cat(hlline, a__1, i__4, &c__3, (ftnlen)5120);
	    ldklst_(hlline, (ftnlen)5120);
	    getqav_(&c_false, ffrnam, tfrnam, epoch, &nitr, &avflg, q1, av1, &
		    ok, error, (ftnlen)32, (ftnlen)32, (ftnlen)1840);
	    if (! ok) {
		setmsg_("The first attitude to be compared must be computabl"
			"e using the supporting kernels '#' provided after th"
			"e keys '#' and '#'. #", (ftnlen)124);
		errch_("#", hlline, (ftnlen)1, (ftnlen)5120);
		errch_("#", "-k", (ftnlen)1, (ftnlen)2);
		errch_("#", "-k1", (ftnlen)1, (ftnlen)3);
		errch_("#", error, (ftnlen)1, (ftnlen)1840);
		sigerr_("SPICE(NOTENOUGHDATA7)", (ftnlen)21);
	    }

/*           Unload everything; load additional kernels for second */
/*           attitude; compute and buffer attitude (and AV if */
/*           requested) for all epochs */

	    kclear_();
/* Writing concatenation */
	    i__4[0] = rtrim_(kernls + 2048, (ftnlen)1024), a__1[0] = kernls + 
		    2048;
	    i__4[1] = 1, a__1[1] = " ";
	    i__4[2] = 1024, a__1[2] = kernls + 1024;
	    s_cat(hlline, a__1, i__4, &c__3, (ftnlen)5120);
	    ldklst_(hlline, (ftnlen)5120);
	    getqav_(&c_false, ffrnam + 32, tfrnam + 32, epoch, &nitr, &avflg, 
		    q2, av2, &ok, error, (ftnlen)32, (ftnlen)32, (ftnlen)1840)
		    ;
	    if (! ok) {
		setmsg_("The second attitude to be compared must be computab"
			"le using the supporting kernels '#' provided after t"
			"he keys '#' and '#'. #", (ftnlen)125);
		errch_("#", hlline, (ftnlen)1, (ftnlen)5120);
		errch_("#", "-k", (ftnlen)1, (ftnlen)2);
		errch_("#", "-k2", (ftnlen)1, (ftnlen)3);
		errch_("#", error, (ftnlen)1, (ftnlen)1840);
		sigerr_("SPICE(NOTENOUGHDATA8)", (ftnlen)21);
	    }
	}
    }

/*     Load kernels that may be needed for time conversions in the */
/*     routine that does the rest of display. The set of kernels that we */
/*     will load depends on the time format. For SCLK output we will */
/*     load kernels applicable for the SCLK ID that is set (either */
/*     SCLKID(1) or SCLKID(2)). For all other formats we will first load */
/*     all kernels applicable to both attitude sets, then all kernels */
/*     applicable to second attitude set, and finally all kernels */
/*     applicable to first attitude set, just like we did in CHWCML */
/*     before processing begin and end time command line arguments. */

    if (eqstr_(timfmt, "sclk", (ftnlen)1024, (ftnlen)4) || eqstr_(timfmt, 
	    "sclkd", (ftnlen)1024, (ftnlen)5) || eqstr_(timfmt, "ticks", (
	    ftnlen)1024, (ftnlen)5)) {
	if (sclkid[0] != 0) {
	    if (s_cmp(kernam, " ", (ftnlen)1024, (ftnlen)1) != 0) {
/* Writing concatenation */
		i__5[0] = rtrim_(kernls + 2048, (ftnlen)1024), a__2[0] = 
			kernls + 2048;
		i__5[1] = 1, a__2[1] = " ";
		i__5[2] = rtrim_(kernls, (ftnlen)1024), a__2[2] = kernls;
		i__5[3] = 1, a__2[3] = " ";
		i__5[4] = 1024, a__2[4] = kernam;
		s_cat(hlline, a__2, i__5, &c__5, (ftnlen)5120);
	    } else {
/* Writing concatenation */
		i__5[0] = rtrim_(kernls + 2048, (ftnlen)1024), a__2[0] = 
			kernls + 2048;
		i__5[1] = 1, a__2[1] = " ";
		i__5[2] = rtrim_(kernls, (ftnlen)1024), a__2[2] = kernls;
		i__5[3] = 1, a__2[3] = " ";
		i__5[4] = 1024, a__2[4] = kernam + 1024;
		s_cat(hlline, a__2, i__5, &c__5, (ftnlen)5120);
	    }
	    myscid = sclkid[0];
	} else if (sclkid[1] != 0) {
/* Writing concatenation */
	    i__5[0] = rtrim_(kernls + 2048, (ftnlen)1024), a__2[0] = kernls + 
		    2048;
	    i__5[1] = 1, a__2[1] = " ";
	    i__5[2] = rtrim_(kernls + 1024, (ftnlen)1024), a__2[2] = kernls + 
		    1024;
	    i__5[3] = 1, a__2[3] = " ";
	    i__5[4] = 1024, a__2[4] = kernam + 1024;
	    s_cat(hlline, a__2, i__5, &c__5, (ftnlen)5120);
	    myscid = sclkid[1];
	} else {
	    setmsg_("There is a bug in the program. Please contact NAIF.", (
		    ftnlen)51);
	    sigerr_("SPICE(FRMDIFFBUG5)", (ftnlen)18);
	}
    } else {
/* Writing concatenation */
	i__6[0] = rtrim_(kernls + 2048, (ftnlen)1024), a__3[0] = kernls + 
		2048;
	i__6[1] = 1, a__3[1] = " ";
	i__6[2] = rtrim_(kernls + 1024, (ftnlen)1024), a__3[2] = kernls + 
		1024;
	i__6[3] = 1, a__3[3] = " ";
	i__6[4] = rtrim_(kernam + 1024, (ftnlen)1024), a__3[4] = kernam + 
		1024;
	i__6[5] = 1, a__3[5] = " ";
	i__6[6] = rtrim_(kernls, (ftnlen)1024), a__3[6] = kernls;
	i__6[7] = 1, a__3[7] = " ";
	i__6[8] = 1024, a__3[8] = kernam;
	s_cat(hlline, a__3, i__6, &c__9, (ftnlen)5120);
	myscid = 0;
    }
    kclear_();
    ldklst_(hlline, (ftnlen)5120);

/*     Generate and display report header. The header for comparison */
/*     run is different from the header for sampling run. First, prepare */
/*     strings that will appear in both headers. */

/*     Make string representing step. Step is included in the report as */
/*     the number of seconds and as DDDAYS:HR:MN:SC.MSECND. */

    s_copy(stpstr, "# second (#)", (ftnlen)1024, (ftnlen)12);
    repmf_(stpstr, "#", &step, &c__15, "F", stpstr, (ftnlen)1024, (ftnlen)1, (
	    ftnlen)1, (ftnlen)1024);
    dr2str_(&step, hword, (ftnlen)32);
    repmc_(stpstr, "#", hword, stpstr, (ftnlen)1024, (ftnlen)1, (ftnlen)32, (
	    ftnlen)1024);

/*     Make strings representing start and stop times. */

    etcal_(&cmpwin[6], time, (ftnlen)1024);
    etcal_(&cmpwin[(i__1 = cardd_(cmpwin) + 5) < 1000006 && 0 <= i__1 ? i__1 :
	     s_rnge("cmpwin", i__1, "frmdiff_", (ftnlen)952)], time + 1024, (
	    ftnlen)1024);
    prefix_("'", &c__0, time, (ftnlen)1, (ftnlen)1024);
    prefix_("'", &c__0, time + 1024, (ftnlen)1, (ftnlen)1024);
    suffix_("' TDB (# TDB seconds)", &c__0, time, (ftnlen)21, (ftnlen)1024);
    suffix_("' TDB (# TDB seconds)", &c__0, time + 1024, (ftnlen)21, (ftnlen)
	    1024);
    repmf_(time, "#", &cmpwin[6], &c__15, "F", time, (ftnlen)1024, (ftnlen)1, 
	    (ftnlen)1, (ftnlen)1024);
    repmf_(time + 1024, "#", &cmpwin[(i__1 = cardd_(cmpwin) + 5) < 1000006 && 
	    0 <= i__1 ? i__1 : s_rnge("cmpwin", i__1, "frmdiff_", (ftnlen)961)
	    ], &c__15, "F", time + 1024, (ftnlen)1024, (ftnlen)1, (ftnlen)1, (
	    ftnlen)1024);

/*     Make string indicating whether comparison window has gaps. */

    if (wncard_(cmpwin) == 1) {
	s_copy(gapstr, "continuous", (ftnlen)1024, (ftnlen)10);
    } else {
	s_copy(gapstr, "non-continuous (with # gaps)", (ftnlen)1024, (ftnlen)
		28);
	i__1 = wncard_(cmpwin) - 1;
	repmi_(gapstr, "#", &i__1, gapstr, (ftnlen)1024, (ftnlen)1, (ftnlen)
		1024);
    }

/*     Make string indicating if angular velocities were computed */
/*     and, if yes, in which frame(s). */

    if (avflg) {
	if (avfflg) {
	    s_copy(avstr, "'to'", (ftnlen)1024, (ftnlen)4);
	} else {
	    s_copy(avstr, "'from'", (ftnlen)1024, (ftnlen)6);
	}
    } else {
	s_copy(avstr, " ", (ftnlen)1024, (ftnlen)1);
    }

/*     Fill in report header. */

    if (sample) {

/*        Set header template for sampling case. */

	s_copy(infmsg, "#", (ftnlen)1024, (ftnlen)1);
	s_copy(infmsg + 1024, "# Sampling of $ rotations", (ftnlen)1024, (
		ftnlen)25);
	s_copy(infmsg + 2048, "#", (ftnlen)1024, (ftnlen)1);
	s_copy(infmsg + 3072, "#    from '$' ($) to '$' ($)", (ftnlen)1024, (
		ftnlen)28);
	s_copy(infmsg + 4096, "#    computed using", (ftnlen)1024, (ftnlen)19)
		;
	s_copy(infmsg + 5120, "#", (ftnlen)1024, (ftnlen)1);
	s_copy(infmsg + 6144, "#       $", (ftnlen)1024, (ftnlen)9);
	s_copy(infmsg + 7168, "#       $", (ftnlen)1024, (ftnlen)9);
	s_copy(infmsg + 8192, "#       $", (ftnlen)1024, (ftnlen)9);
	s_copy(infmsg + 9216, "#", (ftnlen)1024, (ftnlen)1);
	s_copy(infmsg + 10240, "# with a $ step size", (ftnlen)1024, (ftnlen)
		20);
	s_copy(infmsg + 11264, "# within the $ time period", (ftnlen)1024, (
		ftnlen)26);
	s_copy(infmsg + 12288, "#", (ftnlen)1024, (ftnlen)1);
	s_copy(infmsg + 13312, "#    from $", (ftnlen)1024, (ftnlen)11);
	s_copy(infmsg + 14336, "#    to   $", (ftnlen)1024, (ftnlen)11);
	s_copy(infmsg + 15360, "#", (ftnlen)1024, (ftnlen)1);
	s_copy(infmsg + 16384, "# including angular velocities relative to $"
		" frame.", (ftnlen)1024, (ftnlen)51);
	s_copy(infmsg + 17408, "#", (ftnlen)1024, (ftnlen)1);
	s_copy(infmsg + 18432, "# Times $.", (ftnlen)1024, (ftnlen)10);
	s_copy(infmsg + 19456, "#", (ftnlen)1024, (ftnlen)1);
	infcnt = 20;

/*        Tag all lines as printable. */

	i__1 = infcnt;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    infprt[(i__2 = i__ - 1) < 32 && 0 <= i__2 ? i__2 : s_rnge("infprt"
		    , i__2, "frmdiff_", (ftnlen)1024)] = TRUE_;
	}

/*        Substitute values. */

	repmi_(infmsg + 1024, "$", &nitr, infmsg + 1024, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024);
	repmc_(infmsg + 3072, "$", ffrnam, infmsg + 3072, (ftnlen)1024, (
		ftnlen)1, (ftnlen)32, (ftnlen)1024);
	repmi_(infmsg + 3072, "$", ffrid, infmsg + 3072, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024);
	repmc_(infmsg + 3072, "$", tfrnam, infmsg + 3072, (ftnlen)1024, (
		ftnlen)1, (ftnlen)32, (ftnlen)1024);
	repmi_(infmsg + 3072, "$", tfrid, infmsg + 3072, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024);
	if (s_cmp(kernls + 2048, " ", (ftnlen)1024, (ftnlen)1) != 0) {
	    repmc_(infmsg + 6144, "$", kernls + 2048, infmsg + 6144, (ftnlen)
		    1024, (ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	} else {
	    infprt[6] = FALSE_;
	}
	if (s_cmp(kernls, " ", (ftnlen)1024, (ftnlen)1) != 0) {
	    repmc_(infmsg + 7168, "$", kernls, infmsg + 7168, (ftnlen)1024, (
		    ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	} else {
	    infprt[7] = FALSE_;
	}
	if (s_cmp(kernam, " ", (ftnlen)1024, (ftnlen)1) != 0) {
	    repmc_(infmsg + 8192, "$", kernam, infmsg + 8192, (ftnlen)1024, (
		    ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	} else if (s_cmp(kernam + 1024, " ", (ftnlen)1024, (ftnlen)1) != 0) {
	    repmc_(infmsg + 8192, "$", kernam + 1024, infmsg + 8192, (ftnlen)
		    1024, (ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	} else {
	    infprt[8] = FALSE_;
	}
	if (! infprt[6] && ! infprt[7] && ! infprt[8]) {
	    infprt[4] = FALSE_;
	    infprt[5] = FALSE_;
	}
	repmc_(infmsg + 10240, "$", stpstr, infmsg + 10240, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	repmc_(infmsg + 11264, "$", gapstr, infmsg + 11264, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	repmc_(infmsg + 13312, "$", time, infmsg + 13312, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	repmc_(infmsg + 14336, "$", time + 1024, infmsg + 14336, (ftnlen)1024,
		 (ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	if (avflg) {
	    repmc_(infmsg + 16384, "$", avstr, infmsg + 16384, (ftnlen)1024, (
		    ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	} else {
	    infprt[16] = FALSE_;
	    infprt[17] = FALSE_;
	}

/*        Set index of the line providing time spec. */

	tlidx = 19;

/*        For coverage reports we need to change some wording and drop */
/*        some lines. */

	if (eqstr_(diftyp, "dumpc", (ftnlen)32, (ftnlen)5) || eqstr_(diftyp, 
		"dumpg", (ftnlen)32, (ftnlen)5)) {
	    s_copy(infmsg + 1024, "# Coverage for rotation", (ftnlen)1024, (
		    ftnlen)23);
	}

/*        For runs checking only endpoints, replace step line. */

	if (eponly) {
	    s_copy(infmsg + 10240, "# at continuous coverage intervals' endp"
		    "oints", (ftnlen)1024, (ftnlen)45);
	}
    } else {

/*        Set header template for comparison case. */

	s_copy(infmsg, "#", (ftnlen)1024, (ftnlen)1);
	s_copy(infmsg + 1024, "# Comparison of $ rotations", (ftnlen)1024, (
		ftnlen)27);
	s_copy(infmsg + 2048, "#", (ftnlen)1024, (ftnlen)1);
	s_copy(infmsg + 3072, "#    from '$' ($) to '$' ($)", (ftnlen)1024, (
		ftnlen)28);
	s_copy(infmsg + 4096, "#    computed using", (ftnlen)1024, (ftnlen)19)
		;
	s_copy(infmsg + 5120, "#", (ftnlen)1024, (ftnlen)1);
	s_copy(infmsg + 6144, "#       $", (ftnlen)1024, (ftnlen)9);
	s_copy(infmsg + 7168, "#       $", (ftnlen)1024, (ftnlen)9);
	s_copy(infmsg + 8192, "#       $", (ftnlen)1024, (ftnlen)9);
	s_copy(infmsg + 9216, "#", (ftnlen)1024, (ftnlen)1);
	s_copy(infmsg + 10240, "# with $ rotations", (ftnlen)1024, (ftnlen)18)
		;
	s_copy(infmsg + 11264, "#", (ftnlen)1024, (ftnlen)1);
	s_copy(infmsg + 12288, "#    from '$' ($) to '$' ($)", (ftnlen)1024, (
		ftnlen)28);
	s_copy(infmsg + 13312, "#    computed using", (ftnlen)1024, (ftnlen)
		19);
	s_copy(infmsg + 14336, "#", (ftnlen)1024, (ftnlen)1);
	s_copy(infmsg + 15360, "#       $", (ftnlen)1024, (ftnlen)9);
	s_copy(infmsg + 16384, "#       $", (ftnlen)1024, (ftnlen)9);
	s_copy(infmsg + 17408, "#       $", (ftnlen)1024, (ftnlen)9);
	s_copy(infmsg + 18432, "#", (ftnlen)1024, (ftnlen)1);
	s_copy(infmsg + 19456, "# with a $ step size", (ftnlen)1024, (ftnlen)
		20);
	s_copy(infmsg + 20480, "# within the $ time period", (ftnlen)1024, (
		ftnlen)26);
	s_copy(infmsg + 21504, "#", (ftnlen)1024, (ftnlen)1);
	s_copy(infmsg + 22528, "#    from $", (ftnlen)1024, (ftnlen)11);
	s_copy(infmsg + 23552, "#    to   $", (ftnlen)1024, (ftnlen)11);
	s_copy(infmsg + 24576, "#", (ftnlen)1024, (ftnlen)1);
	s_copy(infmsg + 25600, "# including differences in angular velocitie"
		"s relative to $ frames.", (ftnlen)1024, (ftnlen)67);
	s_copy(infmsg + 26624, "#", (ftnlen)1024, (ftnlen)1);
	s_copy(infmsg + 27648, "# Times $.", (ftnlen)1024, (ftnlen)10);
	s_copy(infmsg + 28672, "#", (ftnlen)1024, (ftnlen)1);
	infcnt = 29;

/*        Tag all lines as printable. */

	i__1 = infcnt;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    infprt[(i__2 = i__ - 1) < 32 && 0 <= i__2 ? i__2 : s_rnge("infprt"
		    , i__2, "frmdiff_", (ftnlen)1145)] = TRUE_;
	}

/*        Substitute values. */

	repmi_(infmsg + 1024, "$", &nitr, infmsg + 1024, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024);
	repmc_(infmsg + 3072, "$", ffrnam, infmsg + 3072, (ftnlen)1024, (
		ftnlen)1, (ftnlen)32, (ftnlen)1024);
	repmi_(infmsg + 3072, "$", ffrid, infmsg + 3072, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024);
	repmc_(infmsg + 3072, "$", tfrnam, infmsg + 3072, (ftnlen)1024, (
		ftnlen)1, (ftnlen)32, (ftnlen)1024);
	repmi_(infmsg + 3072, "$", tfrid, infmsg + 3072, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024);
	if (s_cmp(kernls + 2048, " ", (ftnlen)1024, (ftnlen)1) != 0) {
	    repmc_(infmsg + 6144, "$", kernls + 2048, infmsg + 6144, (ftnlen)
		    1024, (ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	} else {
	    infprt[6] = FALSE_;
	}
	if (s_cmp(kernls, " ", (ftnlen)1024, (ftnlen)1) != 0) {
	    repmc_(infmsg + 7168, "$", kernls, infmsg + 7168, (ftnlen)1024, (
		    ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	} else {
	    infprt[7] = FALSE_;
	}
	if (s_cmp(kernam, " ", (ftnlen)1024, (ftnlen)1) != 0) {
	    repmc_(infmsg + 8192, "$", kernam, infmsg + 8192, (ftnlen)1024, (
		    ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	} else if (s_cmp(kernam + 1024, " ", (ftnlen)1024, (ftnlen)1) != 0) {
	    repmc_(infmsg + 8192, "$", kernam + 1024, infmsg + 8192, (ftnlen)
		    1024, (ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	} else {
	    infprt[8] = FALSE_;
	}
	if (! infprt[6] && ! infprt[7] && ! infprt[8]) {
	    infprt[4] = FALSE_;
	    infprt[5] = FALSE_;
	}
	repmi_(infmsg + 10240, "$", &nitr, infmsg + 10240, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024);
	repmc_(infmsg + 12288, "$", ffrnam + 32, infmsg + 12288, (ftnlen)1024,
		 (ftnlen)1, (ftnlen)32, (ftnlen)1024);
	repmi_(infmsg + 12288, "$", &ffrid[1], infmsg + 12288, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024);
	repmc_(infmsg + 12288, "$", tfrnam + 32, infmsg + 12288, (ftnlen)1024,
		 (ftnlen)1, (ftnlen)32, (ftnlen)1024);
	repmi_(infmsg + 12288, "$", &tfrid[1], infmsg + 12288, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024);
	if (s_cmp(kernls + 2048, " ", (ftnlen)1024, (ftnlen)1) != 0) {
	    repmc_(infmsg + 15360, "$", kernls + 2048, infmsg + 15360, (
		    ftnlen)1024, (ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	} else {
	    infprt[15] = FALSE_;
	}
	if (s_cmp(kernls + 1024, " ", (ftnlen)1024, (ftnlen)1) != 0) {
	    repmc_(infmsg + 16384, "$", kernls + 1024, infmsg + 16384, (
		    ftnlen)1024, (ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	} else {
	    infprt[16] = FALSE_;
	}
	if (s_cmp(kernam + 1024, " ", (ftnlen)1024, (ftnlen)1) != 0) {
	    repmc_(infmsg + 17408, "$", kernam + 1024, infmsg + 17408, (
		    ftnlen)1024, (ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	} else {
	    infprt[17] = FALSE_;
	}
	if (! infprt[15] && ! infprt[16] && ! infprt[17]) {
	    infprt[13] = FALSE_;
	    infprt[14] = FALSE_;
	}
	repmc_(infmsg + 19456, "$", stpstr, infmsg + 19456, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	repmc_(infmsg + 20480, "$", gapstr, infmsg + 20480, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	repmc_(infmsg + 22528, "$", time, infmsg + 22528, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	repmc_(infmsg + 23552, "$", time + 1024, infmsg + 23552, (ftnlen)1024,
		 (ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	if (avflg) {
	    repmc_(infmsg + 25600, "$", avstr, infmsg + 25600, (ftnlen)1024, (
		    ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	} else {
	    infprt[25] = FALSE_;
	    infprt[26] = FALSE_;
	}

/*        Set index of the line providing time spec. */

	tlidx = 28;

/*        For coverage reports we need to change some wording and drop */
/*        some lines. */

	if (eqstr_(diftyp, "dumpc", (ftnlen)32, (ftnlen)5) || eqstr_(diftyp, 
		"dumpg", (ftnlen)32, (ftnlen)5)) {
	    s_copy(infmsg + 1024, "# Coverage overlap for rotation", (ftnlen)
		    1024, (ftnlen)31);
	    s_copy(infmsg + 10240, "# and rotation", (ftnlen)1024, (ftnlen)14)
		    ;
	}

/*        For runs checking only endpoints, replace step line. */

	if (eponly) {
	    s_copy(infmsg + 19456, "# at continuous coverage intervals' endp"
		    "oints", (ftnlen)1024, (ftnlen)45);
	}
    }

/*     Fill in time type. */

    if (eqstr_(diftyp, "basic", (ftnlen)32, (ftnlen)5) || eqstr_(diftyp, 
	    "stats", (ftnlen)32, (ftnlen)5)) {
	infprt[(i__1 = tlidx - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge("infprt", 
		i__1, "frmdiff_", (ftnlen)1266)] = FALSE_;
	infprt[(i__1 = tlidx) < 32 && 0 <= i__1 ? i__1 : s_rnge("infprt", 
		i__1, "frmdiff_", (ftnlen)1267)] = FALSE_;
    } else {
	if (eqstr_(timfmt, "et", (ftnlen)1024, (ftnlen)2)) {
	    repmc_(infmsg + (((i__1 = tlidx - 1) < 32 && 0 <= i__1 ? i__1 : 
		    s_rnge("infmsg", i__1, "frmdiff_", (ftnlen)1273)) << 10), 
		    "$", "are TDB seconds past J2000", infmsg + (((i__2 = 
		    tlidx - 1) < 32 && 0 <= i__2 ? i__2 : s_rnge("infmsg", 
		    i__2, "frmdiff_", (ftnlen)1273)) << 10), (ftnlen)1024, (
		    ftnlen)1, (ftnlen)26, (ftnlen)1024);
	} else if (eqstr_(timfmt, "ticks", (ftnlen)1024, (ftnlen)5)) {
	    repmc_(infmsg + (((i__1 = tlidx - 1) < 32 && 0 <= i__1 ? i__1 : 
		    s_rnge("infmsg", i__1, "frmdiff_", (ftnlen)1279)) << 10), 
		    "$", "are SCLK ticks computed using SCLK ID $", infmsg + (
		    ((i__2 = tlidx - 1) < 32 && 0 <= i__2 ? i__2 : s_rnge(
		    "infmsg", i__2, "frmdiff_", (ftnlen)1279)) << 10), (
		    ftnlen)1024, (ftnlen)1, (ftnlen)39, (ftnlen)1024);
	    repmi_(infmsg + (((i__1 = tlidx - 1) < 32 && 0 <= i__1 ? i__1 : 
		    s_rnge("infmsg", i__1, "frmdiff_", (ftnlen)1282)) << 10), 
		    "$", &myscid, infmsg + (((i__2 = tlidx - 1) < 32 && 0 <= 
		    i__2 ? i__2 : s_rnge("infmsg", i__2, "frmdiff_", (ftnlen)
		    1282)) << 10), (ftnlen)1024, (ftnlen)1, (ftnlen)1024);
	} else if (eqstr_(timfmt, "sclk", (ftnlen)1024, (ftnlen)4)) {
	    repmc_(infmsg + (((i__1 = tlidx - 1) < 32 && 0 <= i__1 ? i__1 : 
		    s_rnge("infmsg", i__1, "frmdiff_", (ftnlen)1286)) << 10), 
		    "$", "are SCLKs computed using SCLK ID $", infmsg + (((
		    i__2 = tlidx - 1) < 32 && 0 <= i__2 ? i__2 : s_rnge("inf"
		    "msg", i__2, "frmdiff_", (ftnlen)1286)) << 10), (ftnlen)
		    1024, (ftnlen)1, (ftnlen)34, (ftnlen)1024);
	    repmi_(infmsg + (((i__1 = tlidx - 1) < 32 && 0 <= i__1 ? i__1 : 
		    s_rnge("infmsg", i__1, "frmdiff_", (ftnlen)1289)) << 10), 
		    "$", &myscid, infmsg + (((i__2 = tlidx - 1) < 32 && 0 <= 
		    i__2 ? i__2 : s_rnge("infmsg", i__2, "frmdiff_", (ftnlen)
		    1289)) << 10), (ftnlen)1024, (ftnlen)1, (ftnlen)1024);
	} else if (eqstr_(timfmt, "sclkd", (ftnlen)1024, (ftnlen)5)) {
	    repmc_(infmsg + (((i__1 = tlidx - 1) < 32 && 0 <= i__1 ? i__1 : 
		    s_rnge("infmsg", i__1, "frmdiff_", (ftnlen)1293)) << 10), 
		    "$", "are decimal SCLKs computed using SCLK ID $", infmsg 
		    + (((i__2 = tlidx - 1) < 32 && 0 <= i__2 ? i__2 : s_rnge(
		    "infmsg", i__2, "frmdiff_", (ftnlen)1293)) << 10), (
		    ftnlen)1024, (ftnlen)1, (ftnlen)42, (ftnlen)1024);
	    repmi_(infmsg + (((i__1 = tlidx - 1) < 32 && 0 <= i__1 ? i__1 : 
		    s_rnge("infmsg", i__1, "frmdiff_", (ftnlen)1296)) << 10), 
		    "$", &myscid, infmsg + (((i__2 = tlidx - 1) < 32 && 0 <= 
		    i__2 ? i__2 : s_rnge("infmsg", i__2, "frmdiff_", (ftnlen)
		    1296)) << 10), (ftnlen)1024, (ftnlen)1, (ftnlen)1024);
	} else {
	    repmc_(infmsg + (((i__1 = tlidx - 1) < 32 && 0 <= i__1 ? i__1 : 
		    s_rnge("infmsg", i__1, "frmdiff_", (ftnlen)1300)) << 10), 
		    "$", "were generated by TIMOUT using '$' format", infmsg 
		    + (((i__2 = tlidx - 1) < 32 && 0 <= i__2 ? i__2 : s_rnge(
		    "infmsg", i__2, "frmdiff_", (ftnlen)1300)) << 10), (
		    ftnlen)1024, (ftnlen)1, (ftnlen)41, (ftnlen)1024);
	    repmc_(infmsg + (((i__1 = tlidx - 1) < 32 && 0 <= i__1 ? i__1 : 
		    s_rnge("infmsg", i__1, "frmdiff_", (ftnlen)1303)) << 10), 
		    "$", timfmt, infmsg + (((i__2 = tlidx - 1) < 32 && 0 <= 
		    i__2 ? i__2 : s_rnge("infmsg", i__2, "frmdiff_", (ftnlen)
		    1303)) << 10), (ftnlen)1024, (ftnlen)1, (ftnlen)1024, (
		    ftnlen)1024);
	}
    }

/*     Display report header. */

    i__1 = infcnt;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (infprt[(i__2 = i__ - 1) < 32 && 0 <= i__2 ? i__2 : s_rnge("infprt"
		, i__2, "frmdiff_", (ftnlen)1313)]) {
	    tostdo_(infmsg + (((i__2 = i__ - 1) < 32 && 0 <= i__2 ? i__2 : 
		    s_rnge("infmsg", i__2, "frmdiff_", (ftnlen)1314)) << 10), 
		    (ftnlen)1024);
	}
    }

/*     Pass quaternions and AV tables with all the flags to the routine */
/*     that will do analysis of the differences and will print them to */
/*     the screen. */

    rtdiff_(q1, av1, q2, av2, cmpwin, &nitr, epoch, diftyp, &avflg, &avfflg, 
	    timfmt, &myscid, axes, aunits, &sample, &sigdig, (ftnlen)32, (
	    ftnlen)1024, (ftnlen)32);

/*     Check out. */

    chkout_("frmdiff", (ftnlen)7);

/*     We are done. :-) */

    return 0;
} /* MAIN__ */

/* Main program alias */ int frmdiff_ () { MAIN__ (); return 0; }
