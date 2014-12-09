/* spkdiff.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c_b4 = 1000000;
static integer c__1 = 1;
static integer c__15 = 15;
static integer c__0 = 0;
static doublereal c_b147 = 3600.;
static doublereal c_b148 = 60.;
static doublereal c_b149 = 1.;
static integer c__3 = 3;
static logical c_true = TRUE_;
static logical c_false = FALSE_;
static integer c__5 = 5;

/* $Procedure   SPKDIFF ( Compare two SPK files. ) */
/* Main program */ MAIN__(void)
{
    /* System generated locals */
    address a__1[3], a__2[5];
    integer i__1, i__2, i__3, i__4[3], i__5[5];
    doublereal d__1, d__2;

    /* Builtin functions */
    integer s_rnge(char *, integer, char *, integer);
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen), i_dnnt(doublereal *);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);

    /* Local variables */
    static logical done;
    static char line[1024];
    static integer msec;
    static char time[1024*2];
    static doublereal step;
    static integer nitr, hour, i__, j;
    extern integer cardd_(doublereal *);
    static integer bodid[2], cenid[2];
    extern /* Subroutine */ int etcal_(doublereal *, char *, ftnlen);
    static char frame[32*2];
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    static doublereal epoch[1000000];
    extern /* Subroutine */ int errch_(char *, char *, ftnlen, ftnlen), 
	    repmc_(char *, char *, char *, char *, ftnlen, ftnlen, ftnlen, 
	    ftnlen), repmf_(char *, char *, doublereal *, integer *, char *, 
	    char *, ftnlen, ftnlen, ftnlen, ftnlen), repmi_(char *, char *, 
	    integer *, char *, ftnlen, ftnlen, ftnlen), errdp_(char *, 
	    doublereal *, ftnlen);
    static char hword[32];
    static integer count;
    static char error[1840];
    extern integer rtrim_(char *, ftnlen);
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    static doublereal state1[6000000]	/* was [6][1000000] */, state2[
	    6000000]	/* was [6][1000000] */;
    extern /* Subroutine */ int dr2str_(doublereal *, char *, ftnlen);
    static doublereal et[2];
    static logical ok;
    extern /* Subroutine */ int cleard_(integer *, doublereal *);
    static char bodnam[32*2], cennam[32*2];
    extern /* Subroutine */ int kclear_(void);
    static integer sigdig;
    static char hlline[5120];
    extern integer wncard_(doublereal *);
    static char infmsg[1024*27];
    static integer infcnt;
    static char gapstr[1024], kernls[1024*3], timfmt[1024], diftyp[32];
    static doublereal cmpwin[1000006], stepsv, winmes;
    static integer erridx, nsgcnt, sgtcnt, minuts, trycnt;
    static logical eponly;
    static char stpstr[1024];
    static logical infprt[27], ovrflw, sample;
    extern /* Subroutine */ int errprt_(char *, char *, ftnlen, ftnlen), 
	    ssized_(integer *, doublereal *), getcml_(char *, ftnlen), 
	    chwcml_(char *, char *, char *, integer *, char *, integer *, 
	    char *, doublereal *, integer *, doublereal *, char *, char *, 
	    char *, logical *, integer *, ftnlen, ftnlen, ftnlen, ftnlen, 
	    ftnlen, ftnlen, ftnlen, ftnlen), wnfetd_(doublereal *, integer *, 
	    doublereal *, doublereal *), setmsg_(char *, ftnlen), sigerr_(
	    char *, ftnlen), prefix_(char *, integer *, char *, ftnlen, 
	    ftnlen), suffix_(char *, integer *, char *, ftnlen, ftnlen), 
	    rmaind_(doublereal *, doublereal *, doublereal *, doublereal *), 
	    intstr_(integer *, char *, ftnlen), tostdo_(char *, ftnlen), 
	    ldklst_(char *, ftnlen), getsta_(logical *, integer *, integer *, 
	    char *, doublereal *, integer *, doublereal *, logical *, char *, 
	    integer *, ftnlen, ftnlen), errint_(char *, integer *, ftnlen), 
	    stdiff_(doublereal *, doublereal *, doublereal *, integer *, 
	    doublereal *, char *, char *, logical *, integer *, ftnlen, 
	    ftnlen), chkout_(char *, ftnlen);
    static integer sec, day;
    extern doublereal spd_(void);
    static char spk[1024*2];
    static doublereal hdp1, hdp2, hdp3, hdp4;

/* $ Abstract */

/*     SPKDIFF is a program that samples data from an SPK file or finds */
/*     differences between geometric states computed from two SPK files */
/*     and either displays these differences or shows statistics about */
/*     them. */

/*     For complete information about the program see SPKDIFF User's */
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

/*     SPKDIFF.UG */

/* $ Keywords */

/*     SPK */

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

/* -    Version 2.0.0, 25-MAR-2014 (BVS). */

/*        Updated for majors functionality additions (sampling, window */
/*        with gaps, etc). */

/* -    Version 1.0.0, 18-JUL-2006 (BVS). */

/* -& */

/*     SPICELIB functions. */

/* $ Abstract */

/*     Include Section:  SPKDIFF Global Parameters */

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

/* -    Version 2.0.0, 25-MAR-2014 (BVS). */

/*        Updated for extended functionality (sampling, non-continuous */
/*        comparison window, coverage and gap display, number of */
/*        significant digits in the output). */

/* -    Version 1.0.0, 09-DEC-2008 (BVS). */

/* -& */

/*     Program name and version. */


/*     Command line keys. */


/*     Command line key values. */


/*     Max and min number states that the program can handle. */


/*     Default number states. */


/*     Maximum number of IDs in an SPK file */


/*     Line size parameters. */


/*     Version, help, usage, and header display parameters. */


/*     DAF descriptor size and component counts. */


/*     Cell lower boundary. */


/*     Maximum allowed number of coverage windows. */


/*     Smallest allowed step. */


/*     Fraction of step to be used as pad at the end of intervals. */


/*     Default, minimum, and maximum numbers of significant digits */
/*     allowed for numbers in dump reports. */


/*     End of SPKDIFF parameters. */


/*     Local variables */


/*     Save everything to prevent potential memory problems in f2c'ed */
/*     version. */


/*     Check in. */

    chkin_("spkdiff", (ftnlen)7);

/*     Reset default error messages. */

    errprt_("SET", "NONE, SHORT, LONG, TRACEBACK", (ftnlen)3, (ftnlen)28);

/*     Initialize comparison window. */

    ssized_(&c_b4, cmpwin);

/*     Get command line and call the "big kahuna" deal-with-command-line */
/*     routine to get back all needed setups. */

    getcml_(line, (ftnlen)1024);
    chwcml_(line, spk, bodnam, bodid, cennam, cenid, frame, cmpwin, &nitr, &
	    step, diftyp, timfmt, kernls, &sample, &sigdig, (ftnlen)1024, (
	    ftnlen)1024, (ftnlen)32, (ftnlen)32, (ftnlen)32, (ftnlen)32, (
	    ftnlen)1024, (ftnlen)1024);

/*     The fork for single/multiple intervals below is for backwards */
/*     compatibility with the version 1.0.0 of SPKDIFF that could work */
/*     only on continuous intervals. */

    if (wncard_(cmpwin) == 1 && ! sample) {

/*        If our windows contains a single interval, calculate times */
/*        for which we will compute states using the simple loop used */
/*        in the version 1.0.0. */

	wnfetd_(cmpwin, &c__1, et, &et[1]);
	i__1 = nitr - 2;
	for (j = 0; j <= i__1; ++j) {
	    epoch[(i__2 = j) < 1000000 && 0 <= i__2 ? i__2 : s_rnge("epoch", 
		    i__2, "spkdiff_", (ftnlen)223)] = et[0] + (doublereal) j *
		     step;
	}
	epoch[(i__1 = nitr - 1) < 1000000 && 0 <= i__1 ? i__1 : s_rnge("epoch"
		, i__1, "spkdiff_", (ftnlen)226)] = et[1];
	eponly = nitr == 2;
    } else {

/*        For multi-interval windows we used the algorithm developed for */
/*        FRMDIFF. Note that parts of this algorithm must be consistent */
/*        with what's done to compute STEP or and/or NIRT in CHWCML. */

/*        Count how many singleton and non-singleton intervals the */
/*        comparison window contains and get the measure of the window. */

	winmes = 0.;
	sgtcnt = 0;
	nsgcnt = 0;
	i__1 = cardd_(cmpwin);
	for (i__ = 1; i__ <= i__1; i__ += 2) {
	    winmes += cmpwin[(i__2 = i__ + 6) < 1000006 && 0 <= i__2 ? i__2 : 
		    s_rnge("cmpwin", i__2, "spkdiff_", (ftnlen)246)] - cmpwin[
		    (i__3 = i__ + 5) < 1000006 && 0 <= i__3 ? i__3 : s_rnge(
		    "cmpwin", i__3, "spkdiff_", (ftnlen)246)];
	    if (cmpwin[(i__2 = i__ + 6) < 1000006 && 0 <= i__2 ? i__2 : 
		    s_rnge("cmpwin", i__2, "spkdiff_", (ftnlen)248)] == 
		    cmpwin[(i__3 = i__ + 5) < 1000006 && 0 <= i__3 ? i__3 : 
		    s_rnge("cmpwin", i__3, "spkdiff_", (ftnlen)248)]) {
		++sgtcnt;
	    } else {
		++nsgcnt;
	    }
	}

/*        We will make two attempts to come up with a step and */
/*        distribute points within the comparison window. In the first */
/*        attempt we will set step on a smaller side, allocating just */
/*        one extra point for each interval whether it is a singleton or */
/*        not. If that attempt will produce more points than can fit the */
/*        buffer, we will back off a bit and make step bigger by */
/*        allocating two extra points for each non-singleton interval. */

	done = FALSE_;
	trycnt = 1;
	stepsv = step;
	while(! done) {

/*           Set buffer overflow flag to false. */

	    ovrflw = FALSE_;

/*           Determine time step if it was not given on the command */
/*           line. */

	    if (step == 0.) {

/*              Check how the number of points for comparison returned */
/*              by CHWCML relates to these singleton/non-singleton */
/*              interval counts. Recall that for each singleton interval */
/*              the time array will contain one point while for each */
/*              non-singleton it will contain at least two points -- one */
/*              for each interval end. Note that because our coverage */
/*              window is declared to have the same size as the times */
/*              buffer the case with just end points cannot cause an */
/*              overflow. */

		if (nitr <= sgtcnt + (nsgcnt << 1)) {

/*                 The requested number of points for comparison is less */
/*                 or equal to the minimum number that we should have */
/*                 for the intervals in our comparison window. In this */
/*                 case, we set step to a value that won't produce any */
/*                 points within intervals. NITR will be reset to the */
/*                 minimum number later, in the loop that computes */
/*                 epochs. */

/* Computing MAX */
		    d__1 = 1., d__2 = winmes + 1.;
		    step = max(d__1,d__2);
		} else {

/*                 The requested number of points for comparison is */
/*                 greater than the minimum number that we should have */
/*                 for the intervals in our comparison window. In this */
/*                 case, we have some points to distribute within */
/*                 non-singleton intervals and to do that we need to */
/*                 compute the step using which they should be */
/*                 distributed. In the first attempt we will set step on */
/*                 a smaller side, allocating just one extra point for */
/*                 each interval whether it is a singleton or not. On */
/*                 the second try we will back off a bit and make step */
/*                 bigger by allocating two extra points for each */
/*                 non-singleton interval. One of the two approaches */
/*                 should work. */

		    if (trycnt == 1) {
			step = winmes / (nitr - (sgtcnt + nsgcnt));
		    } else if (trycnt == 2) {
			step = winmes / (nitr - (sgtcnt + (nsgcnt << 1)));
		    } else {
			setmsg_("There is a bug in the program. Please, cont"
				"act NAIF.", (ftnlen)52);
			sigerr_("SPICE(SPKDIFFBUG3)", (ftnlen)18);
		    }
		}
	    }

/*           Using step generate array of times at which attitudes will */
/*           be computed using the same algorithm as was used in CHWCML. */
/*           Note that if the step was given on the input, it should not */
/*           cause overflow of the epoch buffer (the input was verified */
/*           not to). But if the step was computed it could cause */
/*           overflow on the first try, so we will check for overflow as */
/*           we go. */

	    nitr = 0;
	    i__1 = wncard_(cmpwin);
	    for (i__ = 1; i__ <= i__1; ++i__) {

/*              Fetch endpoints of the next interval. */

		wnfetd_(cmpwin, &i__, et, &et[1]);

/*              Add one step for start of the interval. */

		if (nitr < 1000000) {
		    ++nitr;
		    epoch[(i__2 = nitr - 1) < 1000000 && 0 <= i__2 ? i__2 : 
			    s_rnge("epoch", i__2, "spkdiff_", (ftnlen)365)] = 
			    et[0];
		} else {
		    ovrflw = TRUE_;
		}

/*              Add one step for each point between endpoints up to the */
/*              end of the interval minus padding. Note that in the case */
/*              of overflow this loop will not execute as NITR will be */
/*              equal to MAXITR. */

		hdp2 = et[1] - step * .5;
		count = 1;
		hdp1 = et[0] + step * count;
		while(nitr < 1000000 && hdp1 < hdp2) {
		    ++nitr;
		    epoch[(i__2 = nitr - 1) < 1000000 && 0 <= i__2 ? i__2 : 
			    s_rnge("epoch", i__2, "spkdiff_", (ftnlen)384)] = 
			    hdp1;
		    ++count;
		    hdp1 = et[0] + step * count;
		}

/*              If interval begin time is not equal to interval end time */
/*              add one step for the end of the interval. */

		if (et[0] != et[1]) {
		    if (nitr < 1000000) {
			++nitr;
			epoch[(i__2 = nitr - 1) < 1000000 && 0 <= i__2 ? i__2 
				: s_rnge("epoch", i__2, "spkdiff_", (ftnlen)
				399)] = et[1];
		    } else {
			ovrflw = TRUE_;
		    }
		}
	    }

/*           If we did not overflow the buffer, we are done. Otherwise */
/*           restore step and do another try. */

	    if (! ovrflw) {
		done = TRUE_;
	    } else {
		++trycnt;
		step = stepsv;
	    }
	}

/*        Set flag indicating that the number of steps is equal to the */
/*        number of end points. */

	eponly = nitr == sgtcnt + (nsgcnt << 1);
    }

/*     Generate and display the report header. First, prepare strings */
/*     that will appear in all variations of headers. */

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
	     s_rnge("cmpwin", i__1, "spkdiff_", (ftnlen)445)], time + 1024, (
	    ftnlen)1024);
    prefix_("'", &c__0, time, (ftnlen)1, (ftnlen)1024);
    prefix_("'", &c__0, time + 1024, (ftnlen)1, (ftnlen)1024);
    suffix_(" TDB' (# TDB seconds)", &c__0, time, (ftnlen)21, (ftnlen)1024);
    suffix_(" TDB' (# TDB seconds)", &c__0, time + 1024, (ftnlen)21, (ftnlen)
	    1024);
    repmf_(time, "#", &cmpwin[6], &c__15, "F", time, (ftnlen)1024, (ftnlen)1, 
	    (ftnlen)1, (ftnlen)1024);
    repmf_(time + 1024, "#", &cmpwin[(i__1 = cardd_(cmpwin) + 5) < 1000006 && 
	    0 <= i__1 ? i__1 : s_rnge("cmpwin", i__1, "spkdiff_", (ftnlen)454)
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

/*     Tag all lines in the header buffers as printable. Those that */
/*     don't need to printed in certain circumstances will be reset to */
/*     FALSE later. */

    for (i__ = 1; i__ <= 27; ++i__) {
	infprt[(i__1 = i__ - 1) < 27 && 0 <= i__1 ? i__1 : s_rnge("infprt", 
		i__1, "spkdiff_", (ftnlen)473)] = TRUE_;
    }

/*     Fill in report header. We will have three different report header */
/*     formats: first for sampling case, second for comparison case when */
/*     we have a single interval, both SPKs, and no SPK-specific kernels */
/*     (for compatibility with version 1.0.0), and third for all other */
/*     comparison cases. */


    if (sample) {

/*        Fill in the sampling report header for all sampling cases. Set */
/*        header template. */

	s_copy(infmsg, "#", (ftnlen)1024, (ftnlen)1);
	s_copy(infmsg + 1024, "# Sampling of $ '$'-referenced geometric stat"
		"es", (ftnlen)1024, (ftnlen)47);
	s_copy(infmsg + 2048, "#", (ftnlen)1024, (ftnlen)1);
	s_copy(infmsg + 3072, "#    of '$' ($) relative to '$' ($)", (ftnlen)
		1024, (ftnlen)35);
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
	s_copy(infmsg + 16384, "# Times $.", (ftnlen)1024, (ftnlen)10);
	s_copy(infmsg + 17408, "#", (ftnlen)1024, (ftnlen)1);
	infcnt = 18;

/*        Substitute values. */

	repmi_(infmsg + 1024, "$", &nitr, infmsg + 1024, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024);
	repmc_(infmsg + 1024, "$", frame, infmsg + 1024, (ftnlen)1024, (
		ftnlen)1, (ftnlen)32, (ftnlen)1024);
	repmc_(infmsg + 3072, "$", bodnam, infmsg + 3072, (ftnlen)1024, (
		ftnlen)1, (ftnlen)32, (ftnlen)1024);
	repmi_(infmsg + 3072, "$", bodid, infmsg + 3072, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024);
	repmc_(infmsg + 3072, "$", cennam, infmsg + 3072, (ftnlen)1024, (
		ftnlen)1, (ftnlen)32, (ftnlen)1024);
	repmi_(infmsg + 3072, "$", cenid, infmsg + 3072, (ftnlen)1024, (
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
	if (s_cmp(spk, " ", (ftnlen)1024, (ftnlen)1) != 0) {
	    repmc_(infmsg + 8192, "$", spk, infmsg + 8192, (ftnlen)1024, (
		    ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	} else if (s_cmp(spk + 1024, " ", (ftnlen)1024, (ftnlen)1) != 0) {
	    repmc_(infmsg + 8192, "$", spk + 1024, infmsg + 8192, (ftnlen)
		    1024, (ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	} else {
	    infprt[8] = FALSE_;
	}
	repmc_(infmsg + 10240, "$", stpstr, infmsg + 10240, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	repmc_(infmsg + 11264, "$", gapstr, infmsg + 11264, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	repmc_(infmsg + 13312, "$", time, infmsg + 13312, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	repmc_(infmsg + 14336, "$", time + 1024, infmsg + 14336, (ftnlen)1024,
		 (ftnlen)1, (ftnlen)1024, (ftnlen)1024);

/*        For coverage reports, change some wording. */

	if (eqstr_(diftyp, "dumpc", (ftnlen)32, (ftnlen)5) || eqstr_(diftyp, 
		"dumpg", (ftnlen)32, (ftnlen)5)) {
	    s_copy(infmsg + 1024, "# Coverage for '$'-referenced geometric s"
		    "tates", (ftnlen)1024, (ftnlen)46);
	    repmc_(infmsg + 1024, "$", frame, infmsg + 1024, (ftnlen)1024, (
		    ftnlen)1, (ftnlen)32, (ftnlen)1024);
	}

/*        For runs checking only endpoints, replace step line. */

	if (eponly) {
	    s_copy(infmsg + 10240, "# at continuous coverage intervals' endp"
		    "oints", (ftnlen)1024, (ftnlen)45);
	}

/*        Finally, fill in time type for dump and coverage reports. */

	if (eqstr_(diftyp, "basic", (ftnlen)32, (ftnlen)5) || eqstr_(diftyp, 
		"stats", (ftnlen)32, (ftnlen)5)) {
	    infcnt = 16;
	} else {
	    if (s_cmp(timfmt, " ", (ftnlen)1024, (ftnlen)1) == 0) {
		repmc_(infmsg + 16384, "$", "are TDB seconds past J2000", 
			infmsg + 16384, (ftnlen)1024, (ftnlen)1, (ftnlen)26, (
			ftnlen)1024);
	    } else {
		repmc_(infmsg + 16384, "$", "were generated by TIMOUT using "
			"'$' format", infmsg + 16384, (ftnlen)1024, (ftnlen)1, 
			(ftnlen)41, (ftnlen)1024);
		repmc_(infmsg + 16384, "$", timfmt, infmsg + 16384, (ftnlen)
			1024, (ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	    }
	}

/*        Done filling out sampling report header. */

    } else if (wncard_(cmpwin) == 1 && s_cmp(spk, " ", (ftnlen)1024, (ftnlen)
	    1) != 0 && s_cmp(spk + 1024, " ", (ftnlen)1024, (ftnlen)1) != 0 &&
	     s_cmp(kernls, " ", (ftnlen)1024, (ftnlen)1) == 0 && s_cmp(kernls 
	    + 1024, " ", (ftnlen)1024, (ftnlen)1) == 0 && ! eqstr_(diftyp, 
	    "dumpc", (ftnlen)32, (ftnlen)5) && ! eqstr_(diftyp, "dumpg", (
	    ftnlen)32, (ftnlen)5)) {

/*        Fill in the comparison report header for comparison case when */
/*        we have a single interval, both SPKs, and no SPK-specific */
/*        kernels (for compatibility with version 1.0.0). Set header */
/*        template. */

	s_copy(infmsg, "# ", (ftnlen)1024, (ftnlen)2);
	s_copy(infmsg + 1024, "# Comparison of $ '$'-referenced geometric st"
		"ates", (ftnlen)1024, (ftnlen)49);
	s_copy(infmsg + 2048, "# ", (ftnlen)1024, (ftnlen)2);
	s_copy(infmsg + 3072, "#    of '$' ($) relative to '$' ($)", (ftnlen)
		1024, (ftnlen)35);
	s_copy(infmsg + 4096, "#    from SPK '$'", (ftnlen)1024, (ftnlen)17);
	s_copy(infmsg + 5120, "# ", (ftnlen)1024, (ftnlen)2);
	s_copy(infmsg + 6144, "# with $ '$'-referenced geometric states", (
		ftnlen)1024, (ftnlen)40);
	s_copy(infmsg + 7168, "# ", (ftnlen)1024, (ftnlen)2);
	s_copy(infmsg + 8192, "#    of '$' ($) relative to '$' ($)", (ftnlen)
		1024, (ftnlen)35);
	s_copy(infmsg + 9216, "#    from SPK '$'", (ftnlen)1024, (ftnlen)17);
	s_copy(infmsg + 10240, "# ", (ftnlen)1024, (ftnlen)2);
	s_copy(infmsg + 11264, "# evenly-spaced with $ second ($) step size", 
		(ftnlen)1024, (ftnlen)43);
	s_copy(infmsg + 12288, "# within the time interval", (ftnlen)1024, (
		ftnlen)26);
	s_copy(infmsg + 13312, "# ", (ftnlen)1024, (ftnlen)2);
	s_copy(infmsg + 14336, "#    from $", (ftnlen)1024, (ftnlen)11);
	s_copy(infmsg + 15360, "#    to   $", (ftnlen)1024, (ftnlen)11);
	s_copy(infmsg + 16384, "# ", (ftnlen)1024, (ftnlen)2);
	s_copy(infmsg + 17408, "# using additional data from these kernels", (
		ftnlen)1024, (ftnlen)42);
	s_copy(infmsg + 18432, "# ", (ftnlen)1024, (ftnlen)2);
	s_copy(infmsg + 19456, "#    '$'", (ftnlen)1024, (ftnlen)8);
	s_copy(infmsg + 20480, "# ", (ftnlen)1024, (ftnlen)2);

/*        Substitute values. */

	repmi_(infmsg + 1024, "$", &nitr, infmsg + 1024, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024);
	repmc_(infmsg + 1024, "$", frame, infmsg + 1024, (ftnlen)1024, (
		ftnlen)1, (ftnlen)32, (ftnlen)1024);
	repmc_(infmsg + 3072, "$", bodnam, infmsg + 3072, (ftnlen)1024, (
		ftnlen)1, (ftnlen)32, (ftnlen)1024);
	repmi_(infmsg + 3072, "$", bodid, infmsg + 3072, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024);
	repmc_(infmsg + 3072, "$", cennam, infmsg + 3072, (ftnlen)1024, (
		ftnlen)1, (ftnlen)32, (ftnlen)1024);
	repmi_(infmsg + 3072, "$", cenid, infmsg + 3072, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024);
	repmc_(infmsg + 4096, "$", spk, infmsg + 4096, (ftnlen)1024, (ftnlen)
		1, (ftnlen)1024, (ftnlen)1024);
	repmi_(infmsg + 6144, "$", &nitr, infmsg + 6144, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024);
	repmc_(infmsg + 6144, "$", frame + 32, infmsg + 6144, (ftnlen)1024, (
		ftnlen)1, (ftnlen)32, (ftnlen)1024);
	repmc_(infmsg + 8192, "$", bodnam + 32, infmsg + 8192, (ftnlen)1024, (
		ftnlen)1, (ftnlen)32, (ftnlen)1024);
	repmi_(infmsg + 8192, "$", &bodid[1], infmsg + 8192, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024);
	repmc_(infmsg + 8192, "$", cennam + 32, infmsg + 8192, (ftnlen)1024, (
		ftnlen)1, (ftnlen)32, (ftnlen)1024);
	repmi_(infmsg + 8192, "$", &cenid[1], infmsg + 8192, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024);
	repmc_(infmsg + 9216, "$", spk + 1024, infmsg + 9216, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024, (ftnlen)1024);

/*        For backward compatibility with version 1.0.0 this in-line */
/*        step formatting is used instead of the STPSTR prepared earlier. */

/*        Step in included into the report as the number of seconds and */
/*        as #d #h #m #.######s (for more clarity.) */

	repmf_(infmsg + 11264, "$", &step, &c__15, "F", infmsg + 11264, (
		ftnlen)1024, (ftnlen)1, (ftnlen)1, (ftnlen)1024);
	d__1 = spd_();
	rmaind_(&step, &d__1, &hdp1, &hdp2);
	day = (integer) hdp1;
	rmaind_(&hdp2, &c_b147, &hdp1, &hdp3);
	hour = (integer) hdp1;
	rmaind_(&hdp3, &c_b148, &hdp1, &hdp4);
	minuts = (integer) hdp1;
	rmaind_(&hdp4, &c_b149, &hdp1, &hdp2);
	sec = (integer) hdp1;
	d__1 = hdp2 * 1e6;
	msec = i_dnnt(&d__1);
	if (msec == 1000000) {
	    msec = 999999;
	}
	intstr_(&msec, hword, (ftnlen)32);
	while(rtrim_(hword, (ftnlen)32) < 6) {
	    prefix_("0", &c__0, hword, (ftnlen)1, (ftnlen)32);
	}
	s_copy(line, "#d #h #m #.#s", (ftnlen)1024, (ftnlen)13);
	repmi_(line, "#", &day, line, (ftnlen)1024, (ftnlen)1, (ftnlen)1024);
	repmi_(line, "#", &hour, line, (ftnlen)1024, (ftnlen)1, (ftnlen)1024);
	repmi_(line, "#", &minuts, line, (ftnlen)1024, (ftnlen)1, (ftnlen)
		1024);
	repmi_(line, "#", &sec, line, (ftnlen)1024, (ftnlen)1, (ftnlen)1024);
	repmc_(line, "#", hword, line, (ftnlen)1024, (ftnlen)1, (ftnlen)32, (
		ftnlen)1024);
	repmc_(infmsg + 11264, "$", line, infmsg + 11264, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	repmc_(infmsg + 11264, "$", stpstr, infmsg + 11264, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	repmc_(infmsg + 14336, "$", time, infmsg + 14336, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	repmc_(infmsg + 15360, "$", time + 1024, infmsg + 15360, (ftnlen)1024,
		 (ftnlen)1, (ftnlen)1024, (ftnlen)1024);

/*        If no additional kernels were provided, don't report them. */

	if (s_cmp(kernls + 2048, " ", (ftnlen)1024, (ftnlen)1) != 0) {
	    repmc_(infmsg + 19456, "$", kernls + 2048, infmsg + 19456, (
		    ftnlen)1024, (ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	    infcnt = 21;
	} else {
	    infcnt = 17;
	}

/*        Done filling out version 1.0.0 compatibility comparison report */
/*        header. */

    } else {

/*        Fill in the comparison report header for all other comparison */
/*        cases. Set header template. */

	s_copy(infmsg, "#", (ftnlen)1024, (ftnlen)1);
	s_copy(infmsg + 1024, "# Comparison of $ '$'-referenced geometric st"
		"ates", (ftnlen)1024, (ftnlen)49);
	s_copy(infmsg + 2048, "#", (ftnlen)1024, (ftnlen)1);
	s_copy(infmsg + 3072, "#    of '$' ($) relative to '$' ($)", (ftnlen)
		1024, (ftnlen)35);
	s_copy(infmsg + 4096, "#    computed using", (ftnlen)1024, (ftnlen)19)
		;
	s_copy(infmsg + 5120, "#", (ftnlen)1024, (ftnlen)1);
	s_copy(infmsg + 6144, "#       $", (ftnlen)1024, (ftnlen)9);
	s_copy(infmsg + 7168, "#       $", (ftnlen)1024, (ftnlen)9);
	s_copy(infmsg + 8192, "#       $", (ftnlen)1024, (ftnlen)9);
	s_copy(infmsg + 9216, "#", (ftnlen)1024, (ftnlen)1);
	s_copy(infmsg + 10240, "# with $ '$'-referenced geometric states", (
		ftnlen)1024, (ftnlen)40);
	s_copy(infmsg + 11264, "#", (ftnlen)1024, (ftnlen)1);
	s_copy(infmsg + 12288, "#    of '$' ($) relative to '$' ($)", (ftnlen)
		1024, (ftnlen)35);
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
	s_copy(infmsg + 25600, "# Times $.", (ftnlen)1024, (ftnlen)10);
	s_copy(infmsg + 26624, "#", (ftnlen)1024, (ftnlen)1);
	infcnt = 27;

/*        Substitute values. */

	repmi_(infmsg + 1024, "$", &nitr, infmsg + 1024, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024);
	repmc_(infmsg + 1024, "$", frame, infmsg + 1024, (ftnlen)1024, (
		ftnlen)1, (ftnlen)32, (ftnlen)1024);
	repmc_(infmsg + 3072, "$", bodnam, infmsg + 3072, (ftnlen)1024, (
		ftnlen)1, (ftnlen)32, (ftnlen)1024);
	repmi_(infmsg + 3072, "$", bodid, infmsg + 3072, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024);
	repmc_(infmsg + 3072, "$", cennam, infmsg + 3072, (ftnlen)1024, (
		ftnlen)1, (ftnlen)32, (ftnlen)1024);
	repmi_(infmsg + 3072, "$", cenid, infmsg + 3072, (ftnlen)1024, (
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
	if (s_cmp(spk, " ", (ftnlen)1024, (ftnlen)1) != 0) {
	    repmc_(infmsg + 8192, "$", spk, infmsg + 8192, (ftnlen)1024, (
		    ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	} else if (s_cmp(spk + 1024, " ", (ftnlen)1024, (ftnlen)1) != 0) {
	    repmc_(infmsg + 8192, "$", spk + 1024, infmsg + 8192, (ftnlen)
		    1024, (ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	} else {
	    infprt[8] = FALSE_;
	}
	repmi_(infmsg + 10240, "$", &nitr, infmsg + 10240, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024);
	repmc_(infmsg + 10240, "$", frame + 32, infmsg + 10240, (ftnlen)1024, 
		(ftnlen)1, (ftnlen)32, (ftnlen)1024);
	repmc_(infmsg + 12288, "$", bodnam + 32, infmsg + 12288, (ftnlen)1024,
		 (ftnlen)1, (ftnlen)32, (ftnlen)1024);
	repmi_(infmsg + 12288, "$", &bodid[1], infmsg + 12288, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024);
	repmc_(infmsg + 12288, "$", cennam + 32, infmsg + 12288, (ftnlen)1024,
		 (ftnlen)1, (ftnlen)32, (ftnlen)1024);
	repmi_(infmsg + 12288, "$", &cenid[1], infmsg + 12288, (ftnlen)1024, (
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
	if (s_cmp(spk + 1024, " ", (ftnlen)1024, (ftnlen)1) != 0) {
	    repmc_(infmsg + 17408, "$", spk + 1024, infmsg + 17408, (ftnlen)
		    1024, (ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	} else {
	    infprt[17] = FALSE_;
	}
	repmc_(infmsg + 19456, "$", stpstr, infmsg + 19456, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	repmc_(infmsg + 20480, "$", gapstr, infmsg + 20480, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	repmc_(infmsg + 22528, "$", time, infmsg + 22528, (ftnlen)1024, (
		ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	repmc_(infmsg + 23552, "$", time + 1024, infmsg + 23552, (ftnlen)1024,
		 (ftnlen)1, (ftnlen)1024, (ftnlen)1024);

/*        For coverage reports, change some wording. */

	if (eqstr_(diftyp, "dumpc", (ftnlen)32, (ftnlen)5) || eqstr_(diftyp, 
		"dumpg", (ftnlen)32, (ftnlen)5)) {
	    s_copy(infmsg + 1024, "# Coverage overlap for '$'-referenced geo"
		    "metric states", (ftnlen)1024, (ftnlen)54);
	    repmc_(infmsg + 1024, "$", frame, infmsg + 1024, (ftnlen)1024, (
		    ftnlen)1, (ftnlen)32, (ftnlen)1024);
	    s_copy(infmsg + 10240, "# and '$'-referenced geometric states", (
		    ftnlen)1024, (ftnlen)37);
	    repmc_(infmsg + 10240, "$", frame + 32, infmsg + 10240, (ftnlen)
		    1024, (ftnlen)1, (ftnlen)32, (ftnlen)1024);
	}

/*        For runs checking only endpoints, replace step line. */

	if (eponly) {
	    s_copy(infmsg + 19456, "# at continuous coverage intervals' endp"
		    "oints", (ftnlen)1024, (ftnlen)45);
	}

/*        Finally, fill in time type for dump and coverage reports */

	if (eqstr_(diftyp, "basic", (ftnlen)32, (ftnlen)5) || eqstr_(diftyp, 
		"stats", (ftnlen)32, (ftnlen)5)) {
	    infcnt = 25;
	} else {
	    if (s_cmp(timfmt, " ", (ftnlen)1024, (ftnlen)1) == 0) {
		repmc_(infmsg + 25600, "$", "are TDB seconds past J2000", 
			infmsg + 25600, (ftnlen)1024, (ftnlen)1, (ftnlen)26, (
			ftnlen)1024);
	    } else {
		repmc_(infmsg + 25600, "$", "were generated by TIMOUT using "
			"'$' format", infmsg + 25600, (ftnlen)1024, (ftnlen)1, 
			(ftnlen)41, (ftnlen)1024);
		repmc_(infmsg + 25600, "$", timfmt, infmsg + 25600, (ftnlen)
			1024, (ftnlen)1, (ftnlen)1024, (ftnlen)1024);
	    }
	}

/*        Done filling out other compatibility comparison report header. */

    }

/*     Display report header. */

    i__1 = infcnt;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if (infprt[(i__2 = i__ - 1) < 27 && 0 <= i__2 ? i__2 : s_rnge("infprt"
		, i__2, "spkdiff_", (ftnlen)886)]) {
	    tostdo_(infmsg + (((i__2 = i__ - 1) < 27 && 0 <= i__2 ? i__2 : 
		    s_rnge("infmsg", i__2, "spkdiff_", (ftnlen)887)) << 10), (
		    ftnlen)1024);
	}
    }

/*     With the first and second trajectory set attributes and epochs in */
/*     hand we can now compute the trajectory sets. How we do this */
/*     depends on how many SPKs were provided on the command line. We */
/*     also set states in the second set to zero which they have to be */
/*     for sampling runs; for comparison runs they fill in as needed. */

    i__1 = nitr * 6;
    cleard_(&i__1, state2);
    if (s_cmp(spk, " ", (ftnlen)1024, (ftnlen)1) != 0 && s_cmp(spk + 1024, 
	    " ", (ftnlen)1024, (ftnlen)1) != 0) {

/*        Two SPKs were provided on the command line. */

/*        Compute the first trajectory set. */

/*        Unload everything; load additional kernels for first */
/*        trajectory; check that first trajectory cannot be computed */
/*        without the first SPK at all epochs; load the first SPK; */
/*        compute and buffer trajectory for all epochs. */

	kclear_();
/* Writing concatenation */
	i__4[0] = rtrim_(kernls + 2048, (ftnlen)1024), a__1[0] = kernls + 
		2048;
	i__4[1] = 1, a__1[1] = " ";
	i__4[2] = 1024, a__1[2] = kernls;
	s_cat(hlline, a__1, i__4, &c__3, (ftnlen)5120);
	ldklst_(hlline, (ftnlen)5120);
	getsta_(&c_true, bodid, cenid, frame, epoch, &nitr, state1, &ok, 
		error, &erridx, (ftnlen)32, (ftnlen)1840);
	if (! ok) {
	    etcal_(&epoch[(i__1 = erridx - 1) < 1000000 && 0 <= i__1 ? i__1 : 
		    s_rnge("epoch", i__1, "spkdiff_", (ftnlen)922)], line, (
		    ftnlen)1024);
	    setmsg_("Oops ... It looks like we can compute geometric state o"
		    "f # with respect to # in '#' frame at '# TDB' (ET #), ev"
		    "en without loading the first SPK file '#'. It means that"
		    " supporting kernel(s) '#' already contain data for this "
		    "center/body pair.", (ftnlen)240);
	    errint_("#", bodid, (ftnlen)1);
	    errint_("#", cenid, (ftnlen)1);
	    errch_("#", frame, (ftnlen)1, (ftnlen)32);
	    errch_("#", line, (ftnlen)1, (ftnlen)1024);
	    errdp_("#", &epoch[(i__1 = erridx - 1) < 1000000 && 0 <= i__1 ? 
		    i__1 : s_rnge("epoch", i__1, "spkdiff_", (ftnlen)935)], (
		    ftnlen)1);
	    errch_("#", spk, (ftnlen)1, (ftnlen)1024);
	    errch_("#", hlline, (ftnlen)1, (ftnlen)5120);
	    sigerr_("SPICE(JEOPARDIZEDRUN1)", (ftnlen)22);
	}
	ldklst_(spk, (ftnlen)1024);
	getsta_(&c_false, bodid, cenid, frame, epoch, &nitr, state1, &ok, 
		error, &erridx, (ftnlen)32, (ftnlen)1840);
	if (! ok) {
	    if (sample) {
		setmsg_("The trajectory that is being sampled must be comput"
			"able using the SPK '#' and supporting kernels '#' pr"
			"ovided after the keys '#' and '#'. #", (ftnlen)139);
	    } else {
		setmsg_("The first trajectory to be compared must be computa"
			"ble using the SPK '#' and supporting kernels '#' pro"
			"vided after the keys '#' and '#'. #", (ftnlen)138);
	    }
	    errch_("#", spk, (ftnlen)1, (ftnlen)1024);
	    errch_("#", hlline, (ftnlen)1, (ftnlen)5120);
	    errch_("#", "-k", (ftnlen)1, (ftnlen)2);
	    errch_("#", "-k1", (ftnlen)1, (ftnlen)3);
	    errch_("#", error, (ftnlen)1, (ftnlen)1840);
	    sigerr_("SPICE(NOTENOUGHDATA1)", (ftnlen)21);
	}

/*        Compute the second trajectory set needed for comparison runs. */

	if (! sample) {

/*           Compute second trajectory set in the same fashion as the */
/*           first one. */

/*           Unload everything; load additional kernels for second */
/*           trajectory; check that second trajectory cannot be computed */
/*           without the second SPK at all epochs; load the second SPK */
/*           file; compute and buffer trajectory for all epochs. */

	    kclear_();
/* Writing concatenation */
	    i__4[0] = rtrim_(kernls + 2048, (ftnlen)1024), a__1[0] = kernls + 
		    2048;
	    i__4[1] = 1, a__1[1] = " ";
	    i__4[2] = 1024, a__1[2] = kernls + 1024;
	    s_cat(hlline, a__1, i__4, &c__3, (ftnlen)5120);
	    ldklst_(hlline, (ftnlen)5120);
	    getsta_(&c_true, &bodid[1], &cenid[1], frame + 32, epoch, &nitr, 
		    state2, &ok, error, &erridx, (ftnlen)32, (ftnlen)1840);
	    if (! ok) {
		etcal_(&epoch[(i__1 = erridx - 1) < 1000000 && 0 <= i__1 ? 
			i__1 : s_rnge("epoch", i__1, "spkdiff_", (ftnlen)994)]
			, line, (ftnlen)1024);
		setmsg_("Oops ... It looks like we can compute geometric sta"
			"te of # with respect to # in '#' frame at '# TDB' (E"
			"T #), even without loading the second SPK file '#'. "
			"It means that supporting kernel(s) '#' already conta"
			"in data for this center/body pair.", (ftnlen)241);
		errint_("#", &bodid[1], (ftnlen)1);
		errint_("#", &cenid[1], (ftnlen)1);
		errch_("#", frame + 32, (ftnlen)1, (ftnlen)32);
		errch_("#", line, (ftnlen)1, (ftnlen)1024);
		errdp_("#", &epoch[(i__1 = erridx - 1) < 1000000 && 0 <= i__1 
			? i__1 : s_rnge("epoch", i__1, "spkdiff_", (ftnlen)
			1007)], (ftnlen)1);
		errch_("#", spk + 1024, (ftnlen)1, (ftnlen)1024);
		errch_("#", hlline, (ftnlen)1, (ftnlen)5120);
		sigerr_("SPICE(JEOPARDIZEDRUN2)", (ftnlen)22);
	    }
	    ldklst_(spk + 1024, (ftnlen)1024);
	    getsta_(&c_false, &bodid[1], &cenid[1], frame + 32, epoch, &nitr, 
		    state2, &ok, error, &erridx, (ftnlen)32, (ftnlen)1840);
	    if (! ok) {
		setmsg_("The second trajectory to be compared must be comput"
			"able using the SPK '#' and supporting kernels '#' pr"
			"ovided after the keys '#' and '#'. #", (ftnlen)139);
		errch_("#", spk + 1024, (ftnlen)1, (ftnlen)1024);
		errch_("#", hlline, (ftnlen)1, (ftnlen)5120);
		errch_("#", "-k", (ftnlen)1, (ftnlen)2);
		errch_("#", "-k2", (ftnlen)1, (ftnlen)3);
		errch_("#", error, (ftnlen)1, (ftnlen)1840);
		sigerr_("SPICE(NOTENOUGHDATA2)", (ftnlen)21);
	    }
	}
    } else if (s_cmp(spk + 1024, " ", (ftnlen)1024, (ftnlen)1) != 0) {

/*        Only one SPK was provided on the command line. */

/*        Compute the first trajectory set. */

/*        Unload everything; load additional kernels for first */
/*        trajectory; check that first trajectory cannot be computed */
/*        without the SPK at all epochs; load the SPK; compute and */
/*        buffer trajectory for all epochs. */

	kclear_();
/* Writing concatenation */
	i__4[0] = rtrim_(kernls + 2048, (ftnlen)1024), a__1[0] = kernls + 
		2048;
	i__4[1] = 1, a__1[1] = " ";
	i__4[2] = 1024, a__1[2] = kernls;
	s_cat(hlline, a__1, i__4, &c__3, (ftnlen)5120);
	ldklst_(hlline, (ftnlen)5120);
	getsta_(&c_true, bodid, cenid, frame, epoch, &nitr, state1, &ok, 
		error, &erridx, (ftnlen)32, (ftnlen)1840);
	if (! ok) {
	    etcal_(&epoch[(i__1 = erridx - 1) < 1000000 && 0 <= i__1 ? i__1 : 
		    s_rnge("epoch", i__1, "spkdiff_", (ftnlen)1058)], line, (
		    ftnlen)1024);
	    setmsg_("Oops ... It looks like we can compute geometric state o"
		    "f # with respect to # in '#' frame at '# TDB' (ET #), ev"
		    "en without loading the SPK file '#'. It means that suppo"
		    "rting kernel(s) '#' already contain data for this center"
		    "/body pair.", (ftnlen)234);
	    errint_("#", bodid, (ftnlen)1);
	    errint_("#", cenid, (ftnlen)1);
	    errch_("#", frame, (ftnlen)1, (ftnlen)32);
	    errch_("#", line, (ftnlen)1, (ftnlen)1024);
	    errdp_("#", &epoch[(i__1 = erridx - 1) < 1000000 && 0 <= i__1 ? 
		    i__1 : s_rnge("epoch", i__1, "spkdiff_", (ftnlen)1071)], (
		    ftnlen)1);
	    errch_("#", spk + 1024, (ftnlen)1, (ftnlen)1024);
	    errch_("#", hlline, (ftnlen)1, (ftnlen)5120);
	    sigerr_("SPICE(JEOPARDIZEDRUN3)", (ftnlen)22);
	}
	ldklst_(spk + 1024, (ftnlen)1024);
	getsta_(&c_false, bodid, cenid, frame, epoch, &nitr, state1, &ok, 
		error, &erridx, (ftnlen)32, (ftnlen)1840);
	if (! ok) {
	    if (sample) {
		setmsg_("The trajectory that is being sampled must be comput"
			"able using the SPK '#' and supporting kernels '#' pr"
			"ovided after the keys '#' and '#'. #", (ftnlen)139);
	    } else {
		setmsg_("The first trajectory to be compared must be computa"
			"ble using the SPK '#' and supporting kernels '#' pro"
			"vided after the keys '#' and '#'. #", (ftnlen)138);
	    }
	    errch_("#", spk + 1024, (ftnlen)1, (ftnlen)1024);
	    errch_("#", hlline, (ftnlen)1, (ftnlen)5120);
	    errch_("#", "-k", (ftnlen)1, (ftnlen)2);
	    errch_("#", "-k1", (ftnlen)1, (ftnlen)3);
	    errch_("#", error, (ftnlen)1, (ftnlen)1840);
	    sigerr_("SPICE(NOTENOUGHDATA3)", (ftnlen)21);
	}

/*        Compute the second trajectory set needed for comparison runs. */

	if (! sample) {

/*           Compute second trajectory set in the same fashion as the */
/*           first one. */

/*           Unload everything; load additional kernels for second */
/*           trajectory; check that second trajectory cannot be computed */
/*           without the SPK at all epochs; load the SPK; compute and */
/*           buffer trajectory for all epochs. */

	    kclear_();
/* Writing concatenation */
	    i__4[0] = rtrim_(kernls + 2048, (ftnlen)1024), a__1[0] = kernls + 
		    2048;
	    i__4[1] = 1, a__1[1] = " ";
	    i__4[2] = 1024, a__1[2] = kernls + 1024;
	    s_cat(hlline, a__1, i__4, &c__3, (ftnlen)5120);
	    ldklst_(hlline, (ftnlen)5120);
	    getsta_(&c_true, &bodid[1], &cenid[1], frame + 32, epoch, &nitr, 
		    state2, &ok, error, &erridx, (ftnlen)32, (ftnlen)1840);
	    if (! ok) {
		etcal_(&epoch[(i__1 = erridx - 1) < 1000000 && 0 <= i__1 ? 
			i__1 : s_rnge("epoch", i__1, "spkdiff_", (ftnlen)1130)
			], line, (ftnlen)1024);
		setmsg_("Oops ... It looks like we can compute geometric sta"
			"te of # with respect to # in '#' frame at '# TDB' (E"
			"T #), even without loading the SPK file '#'. It mean"
			"s that supporting kernel(s) '#' already contain data"
			" for this center/body pair.", (ftnlen)234);
		errint_("#", &bodid[1], (ftnlen)1);
		errint_("#", &cenid[1], (ftnlen)1);
		errch_("#", frame + 32, (ftnlen)1, (ftnlen)32);
		errch_("#", line, (ftnlen)1, (ftnlen)1024);
		errdp_("#", &epoch[(i__1 = erridx - 1) < 1000000 && 0 <= i__1 
			? i__1 : s_rnge("epoch", i__1, "spkdiff_", (ftnlen)
			1143)], (ftnlen)1);
		errch_("#", spk + 1024, (ftnlen)1, (ftnlen)1024);
		errch_("#", hlline, (ftnlen)1, (ftnlen)5120);
		sigerr_("SPICE(JEOPARDIZEDRUN4)", (ftnlen)22);
	    }
	    ldklst_(spk + 1024, (ftnlen)1024);
	    getsta_(&c_false, &bodid[1], &cenid[1], frame + 32, epoch, &nitr, 
		    state2, &ok, error, &erridx, (ftnlen)32, (ftnlen)1840);
	    if (! ok) {
		setmsg_("The second trajectory to be compared must be comput"
			"able using the SPK '#' and supporting kernels '#' pr"
			"ovided after the keys '#' and '#'. #", (ftnlen)139);
		errch_("#", spk + 1024, (ftnlen)1, (ftnlen)1024);
		errch_("#", hlline, (ftnlen)1, (ftnlen)5120);
		errch_("#", "-k", (ftnlen)1, (ftnlen)2);
		errch_("#", "-k2", (ftnlen)1, (ftnlen)3);
		errch_("#", error, (ftnlen)1, (ftnlen)1840);
		sigerr_("SPICE(NOTENOUGHDATA4)", (ftnlen)21);
	    }
	}
    } else {

/*        No kernels were provided on the command line. Compute the */
/*        first trajectory set. */

/*        Unload everything; load additional kernels for first */
/*        trajectory; compute and buffer trajectory for all epochs. */

	kclear_();
/* Writing concatenation */
	i__4[0] = rtrim_(kernls + 2048, (ftnlen)1024), a__1[0] = kernls + 
		2048;
	i__4[1] = 1, a__1[1] = " ";
	i__4[2] = 1024, a__1[2] = kernls;
	s_cat(hlline, a__1, i__4, &c__3, (ftnlen)5120);
	ldklst_(hlline, (ftnlen)5120);
	getsta_(&c_false, bodid, cenid, frame, epoch, &nitr, state1, &ok, 
		error, &erridx, (ftnlen)32, (ftnlen)1840);
	if (! ok) {
	    if (sample) {
		setmsg_("The trajectory that is being sampled must be comput"
			"able using the supporting kernels '#' provided after"
			" the keys '#' and '#'. #", (ftnlen)127);
	    } else {
		setmsg_("The first trajectory to be compared must be computa"
			"ble using the supporting kernels '#' provided after "
			"the keys '#' and  '#'. #", (ftnlen)127);
	    }
	    errch_("#", hlline, (ftnlen)1, (ftnlen)5120);
	    errch_("#", "-k", (ftnlen)1, (ftnlen)2);
	    errch_("#", "-k1", (ftnlen)1, (ftnlen)3);
	    errch_("#", error, (ftnlen)1, (ftnlen)1840);
	    sigerr_("SPICE(NOTENOUGHDATA5)", (ftnlen)21);
	}

/*        Compute the second trajectory set needed for comparison runs. */

	if (! sample) {

/*           Compute second trajectory set in the same fashion as the */
/*           first one. */

/*           Unload everything; load additional kernels for second */
/*           trajectory; compute and buffer trajectory for all epochs. */

	    kclear_();
/* Writing concatenation */
	    i__4[0] = rtrim_(kernls + 2048, (ftnlen)1024), a__1[0] = kernls + 
		    2048;
	    i__4[1] = 1, a__1[1] = " ";
	    i__4[2] = 1024, a__1[2] = kernls + 1024;
	    s_cat(hlline, a__1, i__4, &c__3, (ftnlen)5120);
	    ldklst_(hlline, (ftnlen)5120);
	    getsta_(&c_false, &bodid[1], &cenid[1], frame + 32, epoch, &nitr, 
		    state2, &ok, error, &erridx, (ftnlen)32, (ftnlen)1840);
	    if (! ok) {
		setmsg_("The second trajectory to be compared must be comput"
			"able using the supporting kernels '#' provided after"
			" the keys '#' and '#'. #", (ftnlen)127);
		errch_("#", hlline, (ftnlen)1, (ftnlen)5120);
		errch_("#", "-k", (ftnlen)1, (ftnlen)2);
		errch_("#", "-k2", (ftnlen)1, (ftnlen)3);
		errch_("#", error, (ftnlen)1, (ftnlen)1840);
		sigerr_("SPICE(NOTENOUGHDATA6)", (ftnlen)21);
	    }
	}
    }

/*     Load kernels that may be needed for time conversions in the */
/*     routine that does the rest of display. */

/* Writing concatenation */
    i__5[0] = rtrim_(kernls + 2048, (ftnlen)1024), a__2[0] = kernls + 2048;
    i__5[1] = 1, a__2[1] = " ";
    i__5[2] = rtrim_(kernls + 1024, (ftnlen)1024), a__2[2] = kernls + 1024;
    i__5[3] = 1, a__2[3] = " ";
    i__5[4] = 1024, a__2[4] = kernls;
    s_cat(hlline, a__2, i__5, &c__5, (ftnlen)5120);
    kclear_();
    ldklst_(hlline, (ftnlen)5120);

/*     Pass state tables to the routine that will do analysis of the */
/*     differences and will print them to the screen. */

    stdiff_(state1, state2, cmpwin, &nitr, epoch, diftyp, timfmt, &sample, &
	    sigdig, (ftnlen)32, (ftnlen)1024);

/*     Check out. */

    chkout_("spkdiff", (ftnlen)7);

/*     We are done. :-) */

    return 0;
} /* MAIN__ */

/* Main program alias */ int spkdiff_ () { MAIN__ (); return 0; }
