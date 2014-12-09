/* rtdiff.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;
static integer c__3 = 3;
static integer c__14 = 14;

/* $Procedure      RTDIFF ( Rotation differences ) */
/* Subroutine */ int rtdiff_(doublereal *q1, doublereal *av1, doublereal *q2, 
	doublereal *av2, doublereal *cmpwin, integer *nitr, doublereal *epoch,
	 char *diftyp, logical *avflg, logical *avfflg, char *timfmt, integer 
	*sclkid, integer *axes, char *aunits, logical *sample, integer *
	sigdig, ftnlen diftyp_len, ftnlen timfmt_len, ftnlen aunits_len)
{
    /* Initialized data */

    static char axisnm[1*3] = "X" "Y" "Z";

    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer);
    double sqrt(doublereal);

    /* Local variables */
    extern /* Subroutine */ int vsub_(doublereal *, doublereal *, doublereal *
	    ), mxmt_(doublereal *, doublereal *, doublereal *), m2eul_(
	    doublereal *, integer *, integer *, integer *, doublereal *, 
	    doublereal *, doublereal *);
    static integer i__, j, k;
    static doublereal mdiff[9]	/* was [3][3] */, angle[3];
    extern /* Subroutine */ int etcal_(doublereal *, char *, ftnlen), chkin_(
	    char *, ftnlen);
    static doublereal avdav;
    extern /* Subroutine */ int repmc_(char *, char *, char *, char *, ftnlen,
	     ftnlen, ftnlen, ftnlen);
    extern doublereal dpmin_(void);
    static doublereal ranav;
    extern /* Subroutine */ int dpfmt_(doublereal *, char *, char *, ftnlen, 
	    ftnlen), moved_(doublereal *, integer *, doublereal *);
    static doublereal raxis[3];
    extern integer rtrim_(char *, ftnlen);
    extern logical eqstr_(char *, char *, ftnlen, ftnlen);
    static doublereal m1[9]	/* was [3][3] */, m2[9]	/* was [3][3] */;
    extern doublereal vnorm_(doublereal *);
    extern /* Subroutine */ int dr2str_(doublereal *, char *, ftnlen), 
	    et2str_(doublereal *, char *, integer *, integer *, char *, 
	    ftnlen, ftnlen);
    static doublereal avdiff[3], qdiffo[4], rangle;
    extern logical return_(void);
    extern integer wncard_(doublereal *);
    static char hlpstr[80], dmpstr[1024], datstr[1024], hdrstr[1024];
    static doublereal qdiffs[4], avdtmp[3], avnorm, ranmax, avdmax, ranmet, 
	    avdmet, ranrms, avdrms;
    static integer infcnt;
    static char infmsg[1024*32];
    static logical infprt[32];
    extern /* Subroutine */ int tostdo_(char *, ftnlen), suffix_(char *, 
	    integer *, char *, ftnlen, ftnlen), q2m_(doublereal *, doublereal 
	    *), dpstrp_(doublereal *, integer *, char *, ftnlen), m2q_(
	    doublereal *, doublereal *), convrt_(doublereal *, char *, char *,
	     doublereal *, ftnlen, ftnlen), raxisa_(doublereal *, doublereal *
	    , doublereal *), setmsg_(char *, ftnlen), sigerr_(char *, ftnlen),
	     dpstrf_(doublereal *, integer *, char *, char *, ftnlen, ftnlen),
	     chkout_(char *, ftnlen);
    static doublereal hdp;
    extern /* Subroutine */ int mxv_(doublereal *, doublereal *, doublereal *)
	    ;

/* $ Abstract */

/*     Computes the differences between the rotations and AVs provided */
/*     on the input and writes summary of requested type to the screen. */

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
/*     ROTATIONS */
/*     CK */

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

/* $ Brief_I/O */

/*     Variable  I/O  Description */
/*     --------  ---  -------------------------------------------------- */
/*     Q1         I   First quaternion buffer */
/*     AV1        I   First AV buffer */
/*     Q2         I   Second quaternion buffer */
/*     AV2        I   Second AV buffer */
/*     CMPWIN     I   Comparison window */
/*     NITR       I   Number of point in Q, AV and EPOCH buffers */
/*     EPOCH      I   Epoch buffer */
/*     DIFTYP     I   Type of report to produce */
/*     AVFLG      I   Angular velocity flag */
/*     AVFFLG     I   Angular velocity frame flag */
/*     TIMFMT     I   Output time format */
/*     SCLKID     I   ID to use in SCLK conversions */
/*     AXES       I   Rotation axes for output Euler angles. */
/*     AUNITS     I   Units for output Euler angles. */
/*     SAMPLE     I   Flag indicating a sampling run */
/*     SIGDIG     I   Number of significant digits */

/* $ Detailed_Input */

/*     TBD. */

/* $ Detailed_Output */

/*     None. */

/* $ Parameters */

/*     See include file. */

/* $ Exceptions */

/*     TBD. */

/* $ Files */

/*     All kernels needed for time conversions (ET->UTC, ET->SCLK) must */
/*     be loaded prior to calling this routine. */

/* $ Particulars */

/*     TBD. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     See files. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     B.V. Semenov   (JPL) */

/* $ Version */

/* -    Version 2.0.0, 27-FEB-2012 (BVS) */

/*        Changed the calling sequence to include additional input */
/*        SIGDIG. */

/*        Updated to use SIGDIG to specify the number of significant */
/*        digits in numeric times and numbers in all dump reports. */

/* -    Version 1.0.0, 30-AUG-2008 (BVS) */

/*        Initial version. */

/* -& */

/*     SPICELIB functions */


/*     Local parameters. */


/*     Local variables */


/*     Save everything to prevent potential memory problems in f2c'ed */
/*     version. */


/*     Axis names. */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("RTDIFF", (ftnlen)6);
    }

/*     Generate reports based on requested report type. */

    if (eqstr_(diftyp, "dumpc", diftyp_len, (ftnlen)5)) {

/*        Do coverage interval dump. */

	s_copy(hdrstr, "# interval_start, interval_stop, interval_duration_s"
		"ec, interval_duration_string", (ftnlen)1024, (ftnlen)80);
	s_copy(datstr, "# #", (ftnlen)1024, (ftnlen)3);
	tostdo_(hdrstr, (ftnlen)1024);
	i__1 = wncard_(cmpwin) << 1;
	for (i__ = 1; i__ <= i__1; i__ += 2) {

/*           Reset output line template. */

	    s_copy(dmpstr, datstr, (ftnlen)1024, (ftnlen)1024);

/*           Insert interval start and end. */

	    et2str_(&cmpwin[i__ + 5], timfmt, sclkid, sigdig, hlpstr, 
		    timfmt_len, (ftnlen)80);
	    repmc_(dmpstr, "#", hlpstr, dmpstr, (ftnlen)1024, (ftnlen)1, (
		    ftnlen)80, (ftnlen)1024);
	    et2str_(&cmpwin[i__ + 6], timfmt, sclkid, sigdig, hlpstr, 
		    timfmt_len, (ftnlen)80);
	    repmc_(dmpstr, "#", hlpstr, dmpstr, (ftnlen)1024, (ftnlen)1, (
		    ftnlen)80, (ftnlen)1024);

/*           Append interval duration as seconds and D:H:M:S string. */

	    i__2 = rtrim_(dmpstr, (ftnlen)1024);
	    d__1 = cmpwin[i__ + 6] - cmpwin[i__ + 5];
	    dpfmt_(&d__1, "mmmmmmmmmmmm.mmmmmm", dmpstr + i__2, (ftnlen)19, 
		    1024 - i__2);
	    i__2 = rtrim_(dmpstr, (ftnlen)1024);
	    d__1 = cmpwin[i__ + 6] - cmpwin[i__ + 5];
	    dr2str_(&d__1, dmpstr + i__2, 1024 - i__2);

/*           Print output string. */

	    tostdo_(dmpstr, (ftnlen)1024);
	}

/*        End of coverage interval dump. */

    } else if (eqstr_(diftyp, "dumpg", diftyp_len, (ftnlen)5)) {

/*        Do coverage gap dump. */

	s_copy(hdrstr, "# gap_start, gap_stop, gap_duration_sec, gap_duratio"
		"n_string", (ftnlen)1024, (ftnlen)60);
	s_copy(datstr, "# #", (ftnlen)1024, (ftnlen)3);

/*        Display table header is there is at least one gap. */

	if (! (wncard_(cmpwin) == 1)) {
	    tostdo_(hdrstr, (ftnlen)1024);
	}
	i__1 = wncard_(cmpwin) - 1 << 1;
	for (i__ = 2; i__ <= i__1; i__ += 2) {

/*           Reset output line template. */

	    s_copy(dmpstr, datstr, (ftnlen)1024, (ftnlen)1024);

/*           Insert gap start and end. */

	    et2str_(&cmpwin[i__ + 5], timfmt, sclkid, sigdig, hlpstr, 
		    timfmt_len, (ftnlen)80);
	    repmc_(dmpstr, "#", hlpstr, dmpstr, (ftnlen)1024, (ftnlen)1, (
		    ftnlen)80, (ftnlen)1024);
	    et2str_(&cmpwin[i__ + 6], timfmt, sclkid, sigdig, hlpstr, 
		    timfmt_len, (ftnlen)80);
	    repmc_(dmpstr, "#", hlpstr, dmpstr, (ftnlen)1024, (ftnlen)1, (
		    ftnlen)80, (ftnlen)1024);

/*           Append interval duration as seconds and D:H:M:S string. */

	    i__2 = rtrim_(dmpstr, (ftnlen)1024);
	    d__1 = cmpwin[i__ + 6] - cmpwin[i__ + 5];
	    dpfmt_(&d__1, "mmmmmmmmmmmm.mmmmmm", dmpstr + i__2, (ftnlen)19, 
		    1024 - i__2);
	    i__2 = rtrim_(dmpstr, (ftnlen)1024);
	    d__1 = cmpwin[i__ + 6] - cmpwin[i__ + 5];
	    dr2str_(&d__1, dmpstr + i__2, 1024 - i__2);

/*           Print output string. */

	    tostdo_(dmpstr, (ftnlen)1024);
	}

/*        If there no gaps, report it. */

	if (wncard_(cmpwin) == 1) {
	    tostdo_(" ", (ftnlen)1);
	    tostdo_("There are no gaps in coverage.", (ftnlen)30);
	    tostdo_(" ", (ftnlen)1);
	}

/*        End of coverage gap dump. */

    } else if (eqstr_(diftyp, "dumpm", diftyp_len, (ftnlen)5) || eqstr_(
	    diftyp, "dumpqs", diftyp_len, (ftnlen)6) || eqstr_(diftyp, "dump"
	    "qo", diftyp_len, (ftnlen)6) || eqstr_(diftyp, "dumpaa", 
	    diftyp_len, (ftnlen)6) || eqstr_(diftyp, "dumpea", diftyp_len, (
	    ftnlen)6)) {

/*        Do difference dumps. */


/*        Set header and data line strings depending on requested dump */
/*        type. */

	if (eqstr_(diftyp, "dumpm", diftyp_len, (ftnlen)5)) {
	    s_copy(hdrstr, "# time, m11, m12, m13, m21, m22, m23, m31, m32, "
		    "m33", (ftnlen)1024, (ftnlen)51);
	    s_copy(datstr, "# # # # # # # # # #", (ftnlen)1024, (ftnlen)19);
	} else if (eqstr_(diftyp, "dumpqs", diftyp_len, (ftnlen)6)) {
	    s_copy(hdrstr, "# time, q_cos, q_sin1, q_sin2, q_sin3", (ftnlen)
		    1024, (ftnlen)37);
	    s_copy(datstr, "# # # # #", (ftnlen)1024, (ftnlen)9);
	} else if (eqstr_(diftyp, "dumpqo", diftyp_len, (ftnlen)6)) {
	    s_copy(hdrstr, "# time, q_sin1, q_sin2, q_sin3, q_cos", (ftnlen)
		    1024, (ftnlen)37);
	    s_copy(datstr, "# # # # #", (ftnlen)1024, (ftnlen)9);
	} else if (eqstr_(diftyp, "dumpea", diftyp_len, (ftnlen)6)) {
	    s_copy(hdrstr, "# angles are shown in $.", (ftnlen)1024, (ftnlen)
		    24);
	    repmc_(hdrstr, "$", aunits, hdrstr, (ftnlen)1024, (ftnlen)1, 
		    aunits_len, (ftnlen)1024);
	    tostdo_(hdrstr, (ftnlen)1024);
	    s_copy(hdrstr, "#", (ftnlen)1024, (ftnlen)1);
	    tostdo_(hdrstr, (ftnlen)1024);
	    s_copy(hdrstr, "# time, ang3_about_$, ang2_about_$, ang1_about_$",
		     (ftnlen)1024, (ftnlen)48);
	    for (i__ = 1; i__ <= 3; ++i__) {
		repmc_(hdrstr, "$", axisnm + ((i__2 = axes[(i__1 = i__ - 1) < 
			3 && 0 <= i__1 ? i__1 : s_rnge("axes", i__1, "rtdiff_"
			, (ftnlen)389)] - 1) < 3 && 0 <= i__2 ? i__2 : s_rnge(
			"axisnm", i__2, "rtdiff_", (ftnlen)389)), hdrstr, (
			ftnlen)1024, (ftnlen)1, (ftnlen)1, (ftnlen)1024);
	    }
	    s_copy(datstr, "# # # #", (ftnlen)1024, (ftnlen)7);
	} else if (eqstr_(diftyp, "dumpaa", diftyp_len, (ftnlen)6)) {
	    s_copy(hdrstr, "# angle is shown in $.", (ftnlen)1024, (ftnlen)22)
		    ;
	    repmc_(hdrstr, "$", aunits, hdrstr, (ftnlen)1024, (ftnlen)1, 
		    aunits_len, (ftnlen)1024);
	    tostdo_(hdrstr, (ftnlen)1024);
	    s_copy(hdrstr, "#", (ftnlen)1024, (ftnlen)1);
	    tostdo_(hdrstr, (ftnlen)1024);
	    s_copy(hdrstr, "# time, angle, axis_x, axis_y, axis_z", (ftnlen)
		    1024, (ftnlen)37);
	    s_copy(datstr, "# # # # #", (ftnlen)1024, (ftnlen)9);
	}

/*        Modify header and data line strings for AVs if needed. */

	if (*avflg) {
	    suffix_(", av_x, av_y, av_z", &c__0, hdrstr, (ftnlen)18, (ftnlen)
		    1024);
	    suffix_(" # # #", &c__0, datstr, (ftnlen)6, (ftnlen)1024);
	    if (eqstr_(diftyp, "dumpaa", diftyp_len, (ftnlen)6)) {
		suffix_(", av_magnitude", &c__0, hdrstr, (ftnlen)14, (ftnlen)
			1024);
		suffix_(" #", &c__0, datstr, (ftnlen)2, (ftnlen)1024);
	    }
	}

/*        Print header string. */

	tostdo_(hdrstr, (ftnlen)1024);

/*        Compute and print difference for each point. */

	i__1 = *nitr;
	for (i__ = 1; i__ <= i__1; ++i__) {

/*           Reset output line template. */

	    s_copy(dmpstr, datstr, (ftnlen)1024, (ftnlen)1024);

/*           Put time in the output string. */

	    et2str_(&epoch[i__ - 1], timfmt, sclkid, sigdig, hlpstr, 
		    timfmt_len, (ftnlen)80);
	    repmc_(dmpstr, "#", hlpstr, dmpstr, (ftnlen)1024, (ftnlen)1, (
		    ftnlen)80, (ftnlen)1024);

/*           Compute rotation difference. */

	    q2m_(&q1[(i__ << 2) - 4], m1);
	    q2m_(&q2[(i__ << 2) - 4], m2);
	    mxmt_(m1, m2, mdiff);

/*           Put rotation difference in the output string in requested */
/*           format. */

	    if (eqstr_(diftyp, "dumpm", diftyp_len, (ftnlen)5)) {

/*              Package matrix. */

		for (j = 1; j <= 3; ++j) {
		    for (k = 1; k <= 3; ++k) {
			dpstrp_(&mdiff[(i__2 = j + k * 3 - 4) < 9 && 0 <= 
				i__2 ? i__2 : s_rnge("mdiff", i__2, "rtdiff_",
				 (ftnlen)461)], sigdig, hlpstr, (ftnlen)80);
			repmc_(dmpstr, "#", hlpstr, dmpstr, (ftnlen)1024, (
				ftnlen)1, (ftnlen)80, (ftnlen)1024);
		    }
		}
	    } else if (eqstr_(diftyp, "dumpqs", diftyp_len, (ftnlen)6)) {

/*              Compute and package SPICE quaternion. */

		m2q_(mdiff, qdiffs);
		for (j = 1; j <= 4; ++j) {
		    dpstrp_(&qdiffs[(i__2 = j - 1) < 4 && 0 <= i__2 ? i__2 : 
			    s_rnge("qdiffs", i__2, "rtdiff_", (ftnlen)474)], 
			    sigdig, hlpstr, (ftnlen)80);
		    repmc_(dmpstr, "#", hlpstr, dmpstr, (ftnlen)1024, (ftnlen)
			    1, (ftnlen)80, (ftnlen)1024);
		}
	    } else if (eqstr_(diftyp, "dumpqo", diftyp_len, (ftnlen)6)) {

/*              Compute and package ``other'' quaternion. */

		m2q_(mdiff, qdiffs);
		qdiffo[0] = -qdiffs[1];
		qdiffo[1] = -qdiffs[2];
		qdiffo[2] = -qdiffs[3];
		qdiffo[3] = qdiffs[0];
		for (j = 1; j <= 4; ++j) {
		    dpstrp_(&qdiffo[(i__2 = j - 1) < 4 && 0 <= i__2 ? i__2 : 
			    s_rnge("qdiffo", i__2, "rtdiff_", (ftnlen)491)], 
			    sigdig, hlpstr, (ftnlen)80);
		    repmc_(dmpstr, "#", hlpstr, dmpstr, (ftnlen)1024, (ftnlen)
			    1, (ftnlen)80, (ftnlen)1024);
		}
	    } else if (eqstr_(diftyp, "dumpea", diftyp_len, (ftnlen)6)) {

/*              Compute and package Euler angles. */

		m2eul_(mdiff, axes, &axes[1], &axes[2], angle, &angle[1], &
			angle[2]);
		for (j = 1; j <= 3; ++j) {
		    convrt_(&angle[(i__2 = j - 1) < 3 && 0 <= i__2 ? i__2 : 
			    s_rnge("angle", i__2, "rtdiff_", (ftnlen)504)], 
			    "RADIANS", aunits, &hdp, (ftnlen)7, aunits_len);
		    dpstrp_(&hdp, sigdig, hlpstr, (ftnlen)80);
		    repmc_(dmpstr, "#", hlpstr, dmpstr, (ftnlen)1024, (ftnlen)
			    1, (ftnlen)80, (ftnlen)1024);
		}
	    } else if (eqstr_(diftyp, "dumpaa", diftyp_len, (ftnlen)6)) {

/*              Compute and package angle/axis. */

		raxisa_(mdiff, raxis, &rangle);
		convrt_(&rangle, "RADIANS", aunits, &hdp, (ftnlen)7, 
			aunits_len);
		dpstrp_(&hdp, sigdig, hlpstr, (ftnlen)80);
		repmc_(dmpstr, "#", hlpstr, dmpstr, (ftnlen)1024, (ftnlen)1, (
			ftnlen)80, (ftnlen)1024);
		for (j = 1; j <= 3; ++j) {
		    dpstrp_(&raxis[(i__2 = j - 1) < 3 && 0 <= i__2 ? i__2 : 
			    s_rnge("raxis", i__2, "rtdiff_", (ftnlen)521)], 
			    sigdig, hlpstr, (ftnlen)80);
		    repmc_(dmpstr, "#", hlpstr, dmpstr, (ftnlen)1024, (ftnlen)
			    1, (ftnlen)80, (ftnlen)1024);
		}
	    } else {
		setmsg_("There is a bug in the program. Please, contact NAIF."
			, (ftnlen)52);
		sigerr_("SPICE(FRMDIFFBUG7)", (ftnlen)18);
	    }

/*           Add AV difference if needed. */

	    if (*avflg) {
		vsub_(&av1[i__ * 3 - 3], &av2[i__ * 3 - 3], avdiff);

/*              If requested, rotate AV difference to the first ``to'' */
/*              frame. */

		if (*avfflg) {
		    q2m_(&q1[(i__ << 2) - 4], m1);
		    mxv_(m1, avdiff, avdtmp);
		    moved_(avdtmp, &c__3, avdiff);
		}
		for (j = 1; j <= 3; ++j) {
		    dpstrp_(&avdiff[(i__2 = j - 1) < 3 && 0 <= i__2 ? i__2 : 
			    s_rnge("avdiff", i__2, "rtdiff_", (ftnlen)553)], 
			    sigdig, hlpstr, (ftnlen)80);
		    repmc_(dmpstr, "#", hlpstr, dmpstr, (ftnlen)1024, (ftnlen)
			    1, (ftnlen)80, (ftnlen)1024);
		}

/*              If we dump angle/axis, add AV difference magnitude to */
/*              output. */

		if (eqstr_(diftyp, "dumpaa", diftyp_len, (ftnlen)6)) {
		    d__1 = vnorm_(avdiff);
		    dpstrp_(&d__1, sigdig, hlpstr, (ftnlen)80);
		    repmc_(dmpstr, "#", hlpstr, dmpstr, (ftnlen)1024, (ftnlen)
			    1, (ftnlen)80, (ftnlen)1024);
		}
	    }

/*           Print output string. */

	    tostdo_(dmpstr, (ftnlen)1024);
	}

/*        End of difference dumps. */

    } else if (eqstr_(diftyp, "basic", diftyp_len, (ftnlen)5) || eqstr_(
	    diftyp, "stats", diftyp_len, (ftnlen)5)) {

/*        Do basic and stats dumps. */


/*        Set initial values. */

	ranmax = dpmin_();
	avdmax = dpmin_();
	ranav = 0.;
	avdav = 0.;
	ranrms = 0.;
	avdrms = 0.;

/*        Compute difference for each point and accumulate statistics */
/*        as we go. */

	i__1 = *nitr;
	for (i__ = 1; i__ <= i__1; ++i__) {

/*           Compute rotation difference and angle/axis. Reset maximum */
/*           value. Add current differences and squares to cumulative */
/*           values. */

	    q2m_(&q1[(i__ << 2) - 4], m1);
	    q2m_(&q2[(i__ << 2) - 4], m2);
	    mxmt_(m1, m2, mdiff);
	    raxisa_(mdiff, raxis, &rangle);
	    if (rangle > ranmax) {
		ranmax = rangle;
		ranmet = epoch[i__ - 1];
	    }
	    ranav += rangle;
	    ranrms += rangle * rangle;

/*           Same for AVs, if requested. */

	    if (*avflg) {
		vsub_(&av1[i__ * 3 - 3], &av2[i__ * 3 - 3], avdiff);
		avnorm = vnorm_(avdiff);
		if (avnorm > avdmax) {
		    avdmax = avnorm;
		    avdmet = epoch[i__ - 1];
		}
		avdav += avnorm;
		avdrms += avnorm * avnorm;
	    }
	}

/*        Compute average and RMS. */

	ranav /= (doublereal) (*nitr);
	ranrms = sqrt(ranrms / (doublereal) (*nitr));
	if (*avflg) {
	    avdav /= (doublereal) (*nitr);
	    avdrms = sqrt(avdrms / (doublereal) (*nitr));
	}

/*        Tag all lines as printable. */

	infcnt = 32;
	i__1 = infcnt;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    infprt[(i__2 = i__ - 1) < 32 && 0 <= i__2 ? i__2 : s_rnge("infprt"
		    , i__2, "rtdiff_", (ftnlen)658)] = TRUE_;
	}

/*        Populate report text based report type. */

	if (eqstr_(diftyp, "basic", diftyp_len, (ftnlen)5)) {

/*           Set template for basic report. */

	    s_copy(infmsg, " ", (ftnlen)1024, (ftnlen)1);
	    s_copy(infmsg + 1024, "Absolute difference magnitudes:", (ftnlen)
		    1024, (ftnlen)31);
	    s_copy(infmsg + 2048, " ", (ftnlen)1024, (ftnlen)1);
	    s_copy(infmsg + 3072, "                                     maxi"
		    "mum                 average", (ftnlen)1024, (ftnlen)68);
	    s_copy(infmsg + 4096, " ", (ftnlen)1024, (ftnlen)1);
	    s_copy(infmsg + 5120, "  Rotation (rad):             #      #", (
		    ftnlen)1024, (ftnlen)38);
	    s_copy(infmsg + 6144, "  Angular Velocity (rad/s):   #      #", (
		    ftnlen)1024, (ftnlen)38);
	    s_copy(infmsg + 7168, " ", (ftnlen)1024, (ftnlen)1);
	    infcnt = 8;

/*           Tweak template for sampling case. */

	    if (*sample) {
		s_copy(infmsg + 1024, "Absolute magnitudes:", (ftnlen)1024, (
			ftnlen)20);
	    }

/*           Fill in maximum and average rotation value. */

	    dpstrp_(&ranmax, &c__14, hlpstr, (ftnlen)80);
	    repmc_(infmsg + 5120, "#", hlpstr, infmsg + 5120, (ftnlen)1024, (
		    ftnlen)1, (ftnlen)80, (ftnlen)1024);
	    dpstrp_(&ranav, &c__14, hlpstr, (ftnlen)80);
	    repmc_(infmsg + 5120, "#", hlpstr, infmsg + 5120, (ftnlen)1024, (
		    ftnlen)1, (ftnlen)80, (ftnlen)1024);

/*           If requested, fill in maximum and average AV values. */

	    if (*avflg) {
		dpstrp_(&avdmax, &c__14, hlpstr, (ftnlen)80);
		repmc_(infmsg + 6144, "#", hlpstr, infmsg + 6144, (ftnlen)
			1024, (ftnlen)1, (ftnlen)80, (ftnlen)1024);
		dpstrp_(&avdav, &c__14, hlpstr, (ftnlen)80);
		repmc_(infmsg + 6144, "#", hlpstr, infmsg + 6144, (ftnlen)
			1024, (ftnlen)1, (ftnlen)80, (ftnlen)1024);
	    } else {

/*              Remove AV line from the report. */

		infprt[6] = FALSE_;
	    }
	} else if (eqstr_(diftyp, "stats", diftyp_len, (ftnlen)5)) {

/*           Set template for stats report. */

	    s_copy(infmsg, " ", (ftnlen)1024, (ftnlen)1);
	    s_copy(infmsg + 1024, "1) Average difference", (ftnlen)1024, (
		    ftnlen)21);
	    s_copy(infmsg + 2048, " ", (ftnlen)1024, (ftnlen)1);
	    s_copy(infmsg + 3072, "   1a) Rotation (rad):                   "
		    "            #", (ftnlen)1024, (ftnlen)54);
	    s_copy(infmsg + 4096, " ", (ftnlen)1024, (ftnlen)1);
	    s_copy(infmsg + 5120, "   1b) Angular velocity (rad/s):         "
		    "            #", (ftnlen)1024, (ftnlen)54);
	    s_copy(infmsg + 6144, " ", (ftnlen)1024, (ftnlen)1);
	    s_copy(infmsg + 7168, " ", (ftnlen)1024, (ftnlen)1);
	    s_copy(infmsg + 8192, "2) RMS of difference", (ftnlen)1024, (
		    ftnlen)20);
	    s_copy(infmsg + 9216, " ", (ftnlen)1024, (ftnlen)1);
	    s_copy(infmsg + 10240, "   2a) Rotation (rad):                  "
		    "             #", (ftnlen)1024, (ftnlen)54);
	    s_copy(infmsg + 11264, " ", (ftnlen)1024, (ftnlen)1);
	    s_copy(infmsg + 12288, "   2b) Angular velocity (rad/s):        "
		    "             #", (ftnlen)1024, (ftnlen)54);
	    s_copy(infmsg + 13312, " ", (ftnlen)1024, (ftnlen)1);
	    s_copy(infmsg + 14336, " ", (ftnlen)1024, (ftnlen)1);
	    s_copy(infmsg + 15360, "3) Maximum rotation difference", (ftnlen)
		    1024, (ftnlen)30);
	    s_copy(infmsg + 16384, " ", (ftnlen)1024, (ftnlen)1);
	    s_copy(infmsg + 17408, "   3a) Rotation (rad):                  "
		    "             #", (ftnlen)1024, (ftnlen)54);
	    s_copy(infmsg + 18432, " ", (ftnlen)1024, (ftnlen)1);
	    s_copy(infmsg + 19456, "   3b) Epoch (TDB, seconds past J2000): "
		    "             #", (ftnlen)1024, (ftnlen)54);
	    s_copy(infmsg + 20480, " ", (ftnlen)1024, (ftnlen)1);
	    s_copy(infmsg + 21504, "   3c) Epoch (TDB, calendar format):    "
		    "             #", (ftnlen)1024, (ftnlen)54);
	    s_copy(infmsg + 22528, " ", (ftnlen)1024, (ftnlen)1);
	    s_copy(infmsg + 23552, " ", (ftnlen)1024, (ftnlen)1);
	    s_copy(infmsg + 24576, "4) Maximum angular velocity difference", (
		    ftnlen)1024, (ftnlen)38);
	    s_copy(infmsg + 25600, " ", (ftnlen)1024, (ftnlen)1);
	    s_copy(infmsg + 26624, "   4a) Angular velocity (rad/s):        "
		    "             #", (ftnlen)1024, (ftnlen)54);
	    s_copy(infmsg + 27648, " ", (ftnlen)1024, (ftnlen)1);
	    s_copy(infmsg + 28672, "   4b) Epoch (TDB, seconds past J2000): "
		    "             #", (ftnlen)1024, (ftnlen)54);
	    s_copy(infmsg + 29696, " ", (ftnlen)1024, (ftnlen)1);
	    s_copy(infmsg + 30720, "   4c) Epoch (TDB, calendar format):    "
		    "             #", (ftnlen)1024, (ftnlen)54);
	    s_copy(infmsg + 31744, " ", (ftnlen)1024, (ftnlen)1);
	    infcnt = 32;

/*           Tweak template for sampling case. */

	    if (*sample) {
		s_copy(infmsg + 1024, "1) Average", (ftnlen)1024, (ftnlen)10);
		s_copy(infmsg + 8192, "2) RMS of", (ftnlen)1024, (ftnlen)9);
		s_copy(infmsg + 15360, "3) Maximum rotation", (ftnlen)1024, (
			ftnlen)19);
		s_copy(infmsg + 24576, "4) Maximum angular velocity", (ftnlen)
			1024, (ftnlen)27);
	    }

/*           Fill in rotation values and accompanying times. */

	    dpstrf_(&ranav, &c__14, "F", hlpstr, (ftnlen)1, (ftnlen)80);
	    repmc_(infmsg + 3072, "#", hlpstr, infmsg + 3072, (ftnlen)1024, (
		    ftnlen)1, (ftnlen)80, (ftnlen)1024);
	    dpstrf_(&ranrms, &c__14, "F", hlpstr, (ftnlen)1, (ftnlen)80);
	    repmc_(infmsg + 10240, "#", hlpstr, infmsg + 10240, (ftnlen)1024, 
		    (ftnlen)1, (ftnlen)80, (ftnlen)1024);
	    dpstrf_(&ranmax, &c__14, "F", hlpstr, (ftnlen)1, (ftnlen)80);
	    repmc_(infmsg + 17408, "#", hlpstr, infmsg + 17408, (ftnlen)1024, 
		    (ftnlen)1, (ftnlen)80, (ftnlen)1024);
	    dpstrf_(&ranmet, &c__14, "F", hlpstr, (ftnlen)1, (ftnlen)80);
	    repmc_(infmsg + 19456, "#", hlpstr, infmsg + 19456, (ftnlen)1024, 
		    (ftnlen)1, (ftnlen)80, (ftnlen)1024);
	    etcal_(&ranmet, hlpstr, (ftnlen)80);
	    *(unsigned char *)&hlpstr[4] = '-';
	    *(unsigned char *)&hlpstr[8] = '-';
	    *(unsigned char *)&hlpstr[11] = '-';
	    repmc_(infmsg + 21504, "#", hlpstr, infmsg + 21504, (ftnlen)1024, 
		    (ftnlen)1, (ftnlen)80, (ftnlen)1024);

/*           If requested, fill in AV values and accompanying times. */

	    if (*avflg) {
		dpstrf_(&avdav, &c__14, "F", hlpstr, (ftnlen)1, (ftnlen)80);
		repmc_(infmsg + 5120, "#", hlpstr, infmsg + 5120, (ftnlen)
			1024, (ftnlen)1, (ftnlen)80, (ftnlen)1024);
		dpstrf_(&avdrms, &c__14, "F", hlpstr, (ftnlen)1, (ftnlen)80);
		repmc_(infmsg + 12288, "#", hlpstr, infmsg + 12288, (ftnlen)
			1024, (ftnlen)1, (ftnlen)80, (ftnlen)1024);
		dpstrf_(&avdmax, &c__14, "F", hlpstr, (ftnlen)1, (ftnlen)80);
		repmc_(infmsg + 26624, "#", hlpstr, infmsg + 26624, (ftnlen)
			1024, (ftnlen)1, (ftnlen)80, (ftnlen)1024);
		dpstrf_(&avdmet, &c__14, "F", hlpstr, (ftnlen)1, (ftnlen)80);
		repmc_(infmsg + 28672, "#", hlpstr, infmsg + 28672, (ftnlen)
			1024, (ftnlen)1, (ftnlen)80, (ftnlen)1024);
		etcal_(&avdmet, hlpstr, (ftnlen)80);
		*(unsigned char *)&hlpstr[4] = '-';
		*(unsigned char *)&hlpstr[8] = '-';
		*(unsigned char *)&hlpstr[11] = '-';
		repmc_(infmsg + 30720, "#", hlpstr, infmsg + 30720, (ftnlen)
			1024, (ftnlen)1, (ftnlen)80, (ftnlen)1024);
	    } else {

/*              Remove AV lines from the report. */

		infprt[5] = FALSE_;
		infprt[6] = FALSE_;
		infprt[12] = FALSE_;
		infprt[13] = FALSE_;
		for (i__ = 24; i__ <= 32; ++i__) {
		    infprt[(i__1 = i__ - 1) < 32 && 0 <= i__1 ? i__1 : s_rnge(
			    "infprt", i__1, "rtdiff_", (ftnlen)833)] = FALSE_;
		}
	    }
	} else {
	    setmsg_("There is a bug in the program. Please, contact NAIF.", (
		    ftnlen)52);
	    sigerr_("SPICE(FRMDIFFBUG8)", (ftnlen)18);
	}

/*        Display report. */

	i__1 = infcnt;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    if (infprt[(i__2 = i__ - 1) < 32 && 0 <= i__2 ? i__2 : s_rnge(
		    "infprt", i__2, "rtdiff_", (ftnlen)850)]) {
		tostdo_(infmsg + (((i__2 = i__ - 1) < 32 && 0 <= i__2 ? i__2 :
			 s_rnge("infmsg", i__2, "rtdiff_", (ftnlen)851)) << 
			10), (ftnlen)1024);
	    }
	}

/*        End of basic and stats dumps. */

    } else {
	setmsg_("There is a bug in the program. Please, contact NAIF.", (
		ftnlen)52);
	sigerr_("SPICE(FRMDIFFBUG9)", (ftnlen)18);
    }

/*     All done. */

    chkout_("RTDIFF", (ftnlen)6);
    return 0;
} /* rtdiff_ */

