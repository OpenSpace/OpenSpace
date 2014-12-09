/* dispsm.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure  DISSM ( Write a summary to standard output ) */
/* Subroutine */ int dispsm_(integer *nobj, integer *ids, doublereal *tstrts, 
	doublereal *tends, integer *avfs, integer *frames, char *tout, 
	logical *fdsp, logical *tdsp, logical *gdsp, logical *ndsp, ftnlen 
	tout_len)
{
    /* System generated locals */
    integer ids_dim1, frames_dim1, avfs_dim1, tstrts_dim1, tends_dim1, i__1, 
	    i__2, i__3, i__4, i__5, i__6;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer i__, k;
    extern integer rtrim_(char *, ftnlen);
    char tdsph1[256], tdsph2[256];
    extern /* Subroutine */ int repmcw_(char *, char *, char *, integer *, 
	    char *, ftnlen, ftnlen, ftnlen, ftnlen), tostdo_(char *, ftnlen), 
	    prinsr_(void), prinst_(integer *, doublereal *, doublereal *, 
	    integer *, integer *, char *, logical *, logical *, logical *, 
	    logical *, ftnlen);

/* $ Abstract */

/*     Format and display CK-file data summary on standard output. */

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

/*     CKBRIEF.UG */

/* $ Keywords */

/*     SUMMARY */
/*     CK */

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
/*     NOBJ       I   Number of intervals */
/*     IDS        I   NAIF ID codes of objects */
/*     TSTRTS     I   Begin DP SCLK times of intervals */
/*     TENDS      I   End DP SCLK times of intervals */
/*     AVFS       I   Angular velocity flags */
/*     FRAMES     I   NAIF ID codes of reference frames */
/*     TOUT       I   Key specifying times representation on output */
/*     FDSP       I   Flag defining whether frame's name/id is printed */
/*     TDSP       I   Flag defining tabular/non-tabular summary format */
/*     GDSP       I   Flag requesting object grouping by coverage */
/*     NDSP       I   Flag to display frame assosiated with CK ID */

/* $ Detailed_Input */

/*     NOBJ           Number of different coverage intervals in a */
/*                    CK-file. */

/*     IDS            Integer array of NAIF ID codes corresponding to */
/*                    the coverage intervals. */

/*     TSTRTS         Double precision array of begin DP SCLK times for */
/*                    each interval for a given CK-file. */

/*     TENDS          Double precision array of end DP SCLK times for */
/*                    each interval for a given CK-file. */

/*     AVFS           Integer array of angular velocities flags */
/*                    corresponding to the coverage intervals. */

/*     FRAMES         Integer array of reference frame ID codes */
/*                    corresponding to the coverage intervals. */

/*     TOUT           Key specifying time representation on output: */
/*                    SCLK string, encoded SCLK, ET, UTC or DOY */

/*     FDSP           Flag defining whether name or ID code of the */
/*                    FRAME should appear on output. */

/*     TDSP           Flag defining whether summaries have to be written */
/*                    in tabular or non-tabular format. */

/*     GDSP           Flag defining whether objects with the same */
/*                    coverage must be grouped together. */

/*     NDSP           Flag requesting display of the name of the frame */
/*                    associated with CK ID. */

/* $ Detailed_Output */

/*     No output parameters in this subroutine. It prints summary for */
/*     a given CK-file. */

/* $ Parameters */

/*     None. */

/* $ Exceptions */

/*     Error free. */

/* $ Files */

/*     None. */

/* $ Particulars */

/*     None. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Literature_References */

/*     None. */

/* $ Author_and_Institution */

/*     Y.K. Zaiko      (BERC) */
/*     B.V. Semenov    (NAIF) */

/* $ Version */

/* -    CKBRIEF Beta Version 2.0.0, 13-OCT-2008 (BVS) */

/*        Added NDSP argument. Changed tabular display heading for */
/*        frame name display. */

/* -    CKBRIEF Beta Version 1.1.0, 28-DEC-2001 (NJB) */

/*        Removed extraneous white space at end of file so that */
/*        the final character is a newline.  This was done */
/*        to suppress compiler warnings. */

/* -    CKBRIEF Beta Version 1.0.0, 17-FEB-1999 (YKZ)(BVS) */

/* -& */

/*     SPICELIB functions. */


/*     Local variables */


/*     If table output was requested, substitute correct time type in */
/*     the table header and print it (header) out. */

    /* Parameter adjustments */
    frames_dim1 = *nobj;
    avfs_dim1 = *nobj;
    tends_dim1 = *nobj + 1;
    tstrts_dim1 = *nobj + 1;
    ids_dim1 = *nobj + 1;

    /* Function Body */
    if (*tdsp) {

/*        Set header template for tabular format of summary display. */

	if (*ndsp) {
	    if (*fdsp) {
		s_copy(tdsph1, "Frames                     Interval Begin ##"
			"#####   Interval End #######     AV  Relative to FRA"
			"ME", (ftnlen)256, (ftnlen)98);
		s_copy(tdsph2, "-------------------------- -----------------"
			"------- ------------------------ --- ---------------"
			"--", (ftnlen)256, (ftnlen)98);
	    } else {
		s_copy(tdsph1, "Frames                     Interval Begin ##"
			"#####   Interval End #######     AV  ", (ftnlen)256, (
			ftnlen)81);
		s_copy(tdsph2, "-------------------------- -----------------"
			"------- ------------------------ --- ", (ftnlen)256, (
			ftnlen)81);
	    }
	} else {
	    if (*fdsp) {
		s_copy(tdsph1, "Objects  Interval Begin #######   Interval E"
			"nd #######     AV  Relative to FRAME", (ftnlen)256, (
			ftnlen)80);
		s_copy(tdsph2, "-------- ------------------------ ----------"
			"-------------- --- -----------------", (ftnlen)256, (
			ftnlen)80);
	    } else {
		s_copy(tdsph1, "Objects  Interval Begin #######   Interval E"
			"nd #######     AV  ", (ftnlen)256, (ftnlen)63);
		s_copy(tdsph2, "-------- ------------------------ ----------"
			"-------------- --- ", (ftnlen)256, (ftnlen)63);
	    }
	}
	i__1 = rtrim_("#######", (ftnlen)7);
	repmcw_(tdsph1, "#######", tout, &i__1, tdsph1, (ftnlen)256, (ftnlen)
		7, tout_len, (ftnlen)256);
	i__1 = rtrim_("#######", (ftnlen)7);
	repmcw_(tdsph1, "#######", tout, &i__1, tdsph1, (ftnlen)256, (ftnlen)
		7, tout_len, (ftnlen)256);
	tostdo_(" ", (ftnlen)1);
	tostdo_(tdsph1, (ftnlen)256);
	tostdo_(tdsph2, (ftnlen)256);
    }

/*     If option "group together objects with the same coverage" was not */
/*     specified then objects will be displayed one by one from index */
/*     1 to index NOBJ. */

    if (! (*gdsp)) {
	i__1 = *nobj;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    prinst_(&ids[(i__2 = i__ - 1) < ids_dim1 && 0 <= i__2 ? i__2 : 
		    s_rnge("ids", i__2, "dispsm_", (ftnlen)254)], &tstrts[(
		    i__3 = i__ - 1) < tstrts_dim1 && 0 <= i__3 ? i__3 : 
		    s_rnge("tstrts", i__3, "dispsm_", (ftnlen)254)], &tends[(
		    i__4 = i__ - 1) < tends_dim1 && 0 <= i__4 ? i__4 : s_rnge(
		    "tends", i__4, "dispsm_", (ftnlen)254)], &avfs[(i__5 = 
		    i__ - 1) < avfs_dim1 && 0 <= i__5 ? i__5 : s_rnge("avfs", 
		    i__5, "dispsm_", (ftnlen)254)], &frames[(i__6 = i__ - 1) <
		     frames_dim1 && 0 <= i__6 ? i__6 : s_rnge("frames", i__6, 
		    "dispsm_", (ftnlen)254)], tout, fdsp, tdsp, gdsp, ndsp, 
		    tout_len);
	}
    } else {

/*        Grouping option was specified. But, do we have anything to */
/*        group together (or in other words do we have more that one */
/*        record?) */

	if (*nobj == 1) {

/*           No, we don't. Then we display this one (and only :) record. */

	    prinst_(&ids[(i__1 = 0) < ids_dim1 ? i__1 : s_rnge("ids", i__1, 
		    "dispsm_", (ftnlen)271)], &tstrts[(i__2 = 0) < 
		    tstrts_dim1 ? i__2 : s_rnge("tstrts", i__2, "dispsm_", (
		    ftnlen)271)], &tends[(i__3 = 0) < tends_dim1 ? i__3 : 
		    s_rnge("tends", i__3, "dispsm_", (ftnlen)271)], &avfs[(
		    i__4 = 0) < avfs_dim1 ? i__4 : s_rnge("avfs", i__4, "dis"
		    "psm_", (ftnlen)271)], &frames[(i__5 = 0) < frames_dim1 ? 
		    i__5 : s_rnge("frames", i__5, "dispsm_", (ftnlen)271)], 
		    tout, fdsp, tdsp, gdsp, ndsp, tout_len);
	} else {

/*           We need to group together objects this the same coverage */
/*           in summary display. To provide this, there are two */
/*           loops. Loop for variable I is to find first record */
/*           in source buffer, which was not displayed yet. Loop for */
/*           variable K is to find an index of object with the coverage */
/*           equal to the coverage of previous displayed object (if */
/*           such exists). */

	    i__ = 1;
	    while(i__ < *nobj) {

/*              Look for the next ID that wasn't displayed yet. */

		while(ids[(i__1 = i__ - 1) < ids_dim1 && 0 <= i__1 ? i__1 : 
			s_rnge("ids", i__1, "dispsm_", (ftnlen)292)] == 0 && 
			i__ < *nobj) {
		    ++i__;
		}

/*              Did we reach the end of the buffer? */

		if (i__ == *nobj) {

/*                 We did. Was the last record in the buffer processed */
/*                 already? If not, print in out. */

		    if (ids[(i__1 = i__ - 1) < ids_dim1 && 0 <= i__1 ? i__1 : 
			    s_rnge("ids", i__1, "dispsm_", (ftnlen)305)] != 0)
			     {
			prinst_(&ids[(i__1 = i__ - 1) < ids_dim1 && 0 <= i__1 
				? i__1 : s_rnge("ids", i__1, "dispsm_", (
				ftnlen)307)], &tstrts[(i__2 = i__ - 1) < 
				tstrts_dim1 && 0 <= i__2 ? i__2 : s_rnge(
				"tstrts", i__2, "dispsm_", (ftnlen)307)], &
				tends[(i__3 = i__ - 1) < tends_dim1 && 0 <= 
				i__3 ? i__3 : s_rnge("tends", i__3, "dispsm_",
				 (ftnlen)307)], &avfs[(i__4 = i__ - 1) < 
				avfs_dim1 && 0 <= i__4 ? i__4 : s_rnge("avfs",
				 i__4, "dispsm_", (ftnlen)307)], &frames[(
				i__5 = i__ - 1) < frames_dim1 && 0 <= i__5 ? 
				i__5 : s_rnge("frames", i__5, "dispsm_", (
				ftnlen)307)], tout, fdsp, tdsp, gdsp, ndsp, 
				tout_len);
		    }
		} else {

/*                 Our record is somewhere in the middle of the buffer. */
/*                 Print it first and after that loop over the rest of */
/*                 the buffer to see whether we have more records */
/*                 with the same coverage. */

		    prinst_(&ids[(i__1 = i__ - 1) < ids_dim1 && 0 <= i__1 ? 
			    i__1 : s_rnge("ids", i__1, "dispsm_", (ftnlen)320)
			    ], &tstrts[(i__2 = i__ - 1) < tstrts_dim1 && 0 <= 
			    i__2 ? i__2 : s_rnge("tstrts", i__2, "dispsm_", (
			    ftnlen)320)], &tends[(i__3 = i__ - 1) < 
			    tends_dim1 && 0 <= i__3 ? i__3 : s_rnge("tends", 
			    i__3, "dispsm_", (ftnlen)320)], &avfs[(i__4 = i__ 
			    - 1) < avfs_dim1 && 0 <= i__4 ? i__4 : s_rnge(
			    "avfs", i__4, "dispsm_", (ftnlen)320)], &frames[(
			    i__5 = i__ - 1) < frames_dim1 && 0 <= i__5 ? i__5 
			    : s_rnge("frames", i__5, "dispsm_", (ftnlen)320)],
			     tout, fdsp, tdsp, gdsp, ndsp, tout_len);
		    ids[(i__1 = i__ - 1) < ids_dim1 && 0 <= i__1 ? i__1 : 
			    s_rnge("ids", i__1, "dispsm_", (ftnlen)322)] = 0;
		    k = i__;
		    while(k < *nobj) {
			++k;
			if (tstrts[(i__1 = i__ - 1) < tstrts_dim1 && 0 <= 
				i__1 ? i__1 : s_rnge("tstrts", i__1, "dispsm_"
				, (ftnlen)330)] == tstrts[(i__2 = k - 1) < 
				tstrts_dim1 && 0 <= i__2 ? i__2 : s_rnge(
				"tstrts", i__2, "dispsm_", (ftnlen)330)] && 
				tends[(i__3 = i__ - 1) < tends_dim1 && 0 <= 
				i__3 ? i__3 : s_rnge("tends", i__3, "dispsm_",
				 (ftnlen)330)] == tends[(i__4 = k - 1) < 
				tends_dim1 && 0 <= i__4 ? i__4 : s_rnge("ten"
				"ds", i__4, "dispsm_", (ftnlen)330)]) {

/*                       Print this records and set IDS(K) to 0. */

			    prinst_(&ids[(i__1 = k - 1) < ids_dim1 && 0 <= 
				    i__1 ? i__1 : s_rnge("ids", i__1, "disps"
				    "m_", (ftnlen)336)], &tstrts[(i__2 = k - 1)
				     < tstrts_dim1 && 0 <= i__2 ? i__2 : 
				    s_rnge("tstrts", i__2, "dispsm_", (ftnlen)
				    336)], &tends[(i__3 = k - 1) < tends_dim1 
				    && 0 <= i__3 ? i__3 : s_rnge("tends", 
				    i__3, "dispsm_", (ftnlen)336)], &avfs[(
				    i__4 = k - 1) < avfs_dim1 && 0 <= i__4 ? 
				    i__4 : s_rnge("avfs", i__4, "dispsm_", (
				    ftnlen)336)], &frames[(i__5 = k - 1) < 
				    frames_dim1 && 0 <= i__5 ? i__5 : s_rnge(
				    "frames", i__5, "dispsm_", (ftnlen)336)], 
				    tout, fdsp, tdsp, gdsp, ndsp, tout_len);
			    ids[(i__1 = k - 1) < ids_dim1 && 0 <= i__1 ? i__1 
				    : s_rnge("ids", i__1, "dispsm_", (ftnlen)
				    338)] = 0;
			}
		    }
		}
	    }
	}
    }

/*     Reset variables saved in PRINST to make sure that summary for */
/*     the next CK file will be displayed correctly. */

    prinsr_();
    return 0;
} /* dispsm_ */

