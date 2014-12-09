/* prinst.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__3 = 3;
static integer c__24 = 24;
static integer c__1 = 1;
static integer c__32 = 32;

/* $Procedure PRINST (Display string of CK-file summary) */
/* Subroutine */ int prinst_0_(int n__, integer *id, doublereal *tbegin, 
	doublereal *tend, integer *avflag, integer *frame, char *tout, 
	logical *fdsp, logical *tdsp, logical *gdsp, logical *ndsp, ftnlen 
	tout_len)
{
    /* Initialized data */

    static doublereal tbprev = 0.;
    static doublereal teprev = 0.;
    static integer idprev = 0;

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_cmp(char *, char *, ftnlen, ftnlen);

    /* Local variables */
    integer hint;
    extern /* Subroutine */ int repmc_(char *, char *, char *, char *, ftnlen,
	     ftnlen, ftnlen, ftnlen);
    integer scidw;
    logical found;
    extern /* Subroutine */ int repmi_(char *, char *, integer *, char *, 
	    ftnlen, ftnlen, ftnlen);
    extern integer rtrim_(char *, ftnlen);
    integer frcode;
    extern /* Subroutine */ int ccifrm_(integer *, integer *, integer *, char 
	    *, integer *, logical *, ftnlen);
    char idline[256], fnline[256], tbline[256], avline[256], teline[256];
    extern /* Subroutine */ int timecn_(doublereal *, integer *, char *, char 
	    *, ftnlen, ftnlen), frmnam_(integer *, char *, ftnlen), repmcw_(
	    char *, char *, char *, integer *, char *, ftnlen, ftnlen, ftnlen,
	     ftnlen);
    char outlin[256];
    extern /* Subroutine */ int tostdo_(char *, ftnlen), intstr_(integer *, 
	    char *, ftnlen);

/* $ Abstract */

/*     Write a single CK-file summary record string to standard */
/*     output in requested format. */

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
/*     ID         I   NAIF ID code of object */
/*     TBEGIN     I   Start time of object coverage interval, SCLK ticks */
/*     TEND       I   End time of object coverage interval, SCLK ticks */
/*     AVFLAG     I   Angular velocity flag */
/*     FRAME      I   NAIF ID code of reference frame */
/*     TOUT       I   Key specifying times representation on output */
/*     FDSP       I   Flag defining whether frames name/id is printed */
/*     TDSP       I   Flag defining tabular/non-tabular summary format */
/*     GDSP       I   Flag requesting object grouping by coverage */
/*     NDSP       I   Flag to display frame assosiated with CK ID */

/* $ Detailed_Input */

/*     ID             Integer NAIF ID code found in summaries */
/*                    of CK-file and to be written to standard output. */

/*     TBEGIN         Begin time for object coverage given as DP */
/*                    SCLK ticks. */

/*     TEND           End time for object coverage given as DP */
/*                    SCLK ticks. */

/*     AVFLAG         Angular velocities presence flag: 0 - not present, */
/*                    1 - present, 2 - mixed. */

/*     FRAME          Integer NAIF ID code of reference frame relative */
/*                    to which orientation of the ID was given. */

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

/*     None. This subroutine displays summary line for a CK-file/segment */
/*     for subroutine DISPSM. */

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

/*        Added NDSP argument. Changed to display frame names associated */
/*        with CK IDs when NDSP is .TRUE.. */

/* -    CKBRIEF Beta Version 1.0.0, 17-FEB-1999 (YKZ)(BVS) */

/* -& */

/*     SPICELIB functions */


/*     Local parameters. */


/*     Output fields widths. */


/*     Preset output values. */


/*     Local variables */


/*     Save previous time boundaries and ID code. */


/*     Set initial value to zeros. */

    switch(n__) {
	case 1: goto L_prinsr;
	}


/*     Convert all inputs to strings that will appear on output. */

    if (*ndsp) {
	scidw = 26;
	ccifrm_(&c__3, id, &frcode, idline, &hint, &found, (ftnlen)256);
	if (! found) {
	    s_copy(idline, "NO FRAME FOR #", (ftnlen)256, (ftnlen)14);
	    repmi_(idline, "#", id, idline, (ftnlen)256, (ftnlen)1, (ftnlen)
		    256);
	}
    } else {
	scidw = 8;
	intstr_(id, idline, (ftnlen)256);
    }
    timecn_(tbegin, id, tout, tbline, tout_len, (ftnlen)256);
    timecn_(tend, id, tout, teline, tout_len, (ftnlen)256);
    if (*avflag == 2) {
	s_copy(avline, "*", (ftnlen)256, (ftnlen)1);
    } else if (*avflag == 1) {
	s_copy(avline, "Y", (ftnlen)256, (ftnlen)1);
    } else {
	s_copy(avline, "N", (ftnlen)256, (ftnlen)1);
    }
    frmnam_(frame, fnline, (ftnlen)256);
    if (s_cmp(fnline, " ", (ftnlen)256, (ftnlen)1) == 0) {
	if (*frame == 0) {
	    s_copy(fnline, "MIXED", (ftnlen)256, (ftnlen)5);
	} else {
	    intstr_(frame, fnline, (ftnlen)256);
	}
    }

/*     Make up output string and print them depending on what kind of */
/*     output format was requested. */

    if (*tdsp) {

/*        For table output, set output line template depending on */
/*        whether FRAME display was requested. */

	if (*fdsp) {
	    s_copy(outlin, "# # # #   #", (ftnlen)256, (ftnlen)11);
	} else {
	    s_copy(outlin, "# # # #", (ftnlen)256, (ftnlen)7);
	}

/*        Check whether coverage is the same as previous one and */
/*        reassign begin and end time to 'same' flag if so. */

	if (*tbegin == tbprev && *tend == teprev && s_cmp(tbline, "NEED LSK "
		"AND SCLK FILES", (ftnlen)256, (ftnlen)23) != 0 && s_cmp(
		teline, "NEED LSK AND SCLK FILES", (ftnlen)256, (ftnlen)23) !=
		 0) {
	    s_copy(tbline, "   -- same --", (ftnlen)256, (ftnlen)13);
	    s_copy(teline, "   -- same --", (ftnlen)256, (ftnlen)13);
	}

/*        Substitute string and print out the line. */

	repmcw_(outlin, "#", idline, &scidw, outlin, (ftnlen)256, (ftnlen)1, (
		ftnlen)256, (ftnlen)256);
	repmcw_(outlin, "#", tbline, &c__24, outlin, (ftnlen)256, (ftnlen)1, (
		ftnlen)256, (ftnlen)256);
	repmcw_(outlin, "#", teline, &c__24, outlin, (ftnlen)256, (ftnlen)1, (
		ftnlen)256, (ftnlen)256);
	repmcw_(outlin, "#", avline, &c__1, outlin, (ftnlen)256, (ftnlen)1, (
		ftnlen)256, (ftnlen)256);
	repmcw_(outlin, "#", fnline, &c__32, outlin, (ftnlen)256, (ftnlen)1, (
		ftnlen)256, (ftnlen)256);

/*        Display the line. */

	tostdo_(outlin, rtrim_(outlin, (ftnlen)256));
    } else {

/*        If grouping flag is set, we display single coverage line for */
/*        multiple objects. If it's not set, we display multiple */
/*        coverage lines for a single object. Also when GDSP set we do */
/*        NOT display angular velocity flags or FRAME names/ids. */

	if (*gdsp) {
	    if (*tbegin == tbprev && *tend == teprev) {

/*              This is another object in a group with the same */
/*              coverage. Display just the object ID. */

		s_copy(outlin, "         #", (ftnlen)256, (ftnlen)10);
	    } else {

/*              This is the first object in a group with a different */
/*              coverage. Display blank line, coverage and ID of the */
/*              first object. */

		tostdo_(" ", (ftnlen)1);
		s_copy(outlin, "Begin #: #  End #: # ", (ftnlen)256, (ftnlen)
			21);
		repmc_(outlin, "#", tout, outlin, (ftnlen)256, (ftnlen)1, 
			tout_len, (ftnlen)256);
		repmcw_(outlin, "#", tbline, &c__24, outlin, (ftnlen)256, (
			ftnlen)1, (ftnlen)256, (ftnlen)256);
		repmc_(outlin, "#", tout, outlin, (ftnlen)256, (ftnlen)1, 
			tout_len, (ftnlen)256);
		repmcw_(outlin, "#", teline, &c__24, outlin, (ftnlen)256, (
			ftnlen)1, (ftnlen)256, (ftnlen)256);
		tostdo_(outlin, rtrim_(outlin, (ftnlen)256));
		if (*ndsp) {
		    s_copy(outlin, "Frames:  #", (ftnlen)256, (ftnlen)10);
		} else {
		    s_copy(outlin, "Objects: #", (ftnlen)256, (ftnlen)10);
		}
	    }
	    repmcw_(outlin, "#", idline, &scidw, outlin, (ftnlen)256, (ftnlen)
		    1, (ftnlen)256, (ftnlen)256);
	    tostdo_(outlin, rtrim_(outlin, (ftnlen)256));
	} else {

/*           No grouping by time was requested. So, display contains */
/*           sets of coverage intervals for a particular object. */

	    if (*id == idprev) {

/*              It's the same object. Print out only interval. */

		if (*fdsp) {
		    s_copy(outlin, "  # # #   #", (ftnlen)256, (ftnlen)11);
		} else {
		    s_copy(outlin, "  # # #", (ftnlen)256, (ftnlen)7);
		}
	    } else {

/*              It's another object. Print object ID, header and */
/*              the first interval. */

		tostdo_(" ", (ftnlen)1);
		if (*ndsp) {
		    s_copy(outlin, "Frame:   #", (ftnlen)256, (ftnlen)10);
		} else {
		    s_copy(outlin, "Object:  #", (ftnlen)256, (ftnlen)10);
		}
		repmcw_(outlin, "#", idline, &scidw, outlin, (ftnlen)256, (
			ftnlen)1, (ftnlen)256, (ftnlen)256);
		tostdo_(outlin, rtrim_(outlin, (ftnlen)256));
		if (*fdsp) {
		    s_copy(outlin, "  Interval Begin #######   Interval End "
			    "#######     AV  Relative to FRAME", (ftnlen)256, (
			    ftnlen)73);
		    i__1 = rtrim_("#######", (ftnlen)7);
		    repmcw_(outlin, "#######", tout, &i__1, outlin, (ftnlen)
			    256, (ftnlen)7, tout_len, (ftnlen)256);
		    i__1 = rtrim_("#######", (ftnlen)7);
		    repmcw_(outlin, "#######", tout, &i__1, outlin, (ftnlen)
			    256, (ftnlen)7, tout_len, (ftnlen)256);
		    tostdo_(outlin, rtrim_(outlin, (ftnlen)256));
		    s_copy(outlin, "  ------------------------ -------------"
			    "----------- --- ----------------- ", (ftnlen)256, 
			    (ftnlen)74);
		    tostdo_(outlin, rtrim_(outlin, (ftnlen)256));
		    s_copy(outlin, "  # # #   #", (ftnlen)256, (ftnlen)11);
		} else {
		    s_copy(outlin, "  Interval Begin #######   Interval End "
			    "#######     AV  ", (ftnlen)256, (ftnlen)56);
		    i__1 = rtrim_("#######", (ftnlen)7);
		    repmcw_(outlin, "#######", tout, &i__1, outlin, (ftnlen)
			    256, (ftnlen)7, tout_len, (ftnlen)256);
		    i__1 = rtrim_("#######", (ftnlen)7);
		    repmcw_(outlin, "#######", tout, &i__1, outlin, (ftnlen)
			    256, (ftnlen)7, tout_len, (ftnlen)256);
		    tostdo_(outlin, rtrim_(outlin, (ftnlen)256));
		    s_copy(outlin, "  ------------------------ -------------"
			    "----------- --- ", (ftnlen)256, (ftnlen)56);
		    tostdo_(outlin, rtrim_(outlin, (ftnlen)256));
		    s_copy(outlin, "  # # #", (ftnlen)256, (ftnlen)7);
		}
	    }
	    repmcw_(outlin, "#", tbline, &c__24, outlin, (ftnlen)256, (ftnlen)
		    1, (ftnlen)256, (ftnlen)256);
	    repmcw_(outlin, "#", teline, &c__24, outlin, (ftnlen)256, (ftnlen)
		    1, (ftnlen)256, (ftnlen)256);
	    repmcw_(outlin, "#", avline, &c__1, outlin, (ftnlen)256, (ftnlen)
		    1, (ftnlen)256, (ftnlen)256);
	    repmcw_(outlin, "#", fnline, &c__32, outlin, (ftnlen)256, (ftnlen)
		    1, (ftnlen)256, (ftnlen)256);
	    tostdo_(outlin, rtrim_(outlin, (ftnlen)256));
	}
    }

/*     Reassign saved variables. */

    tbprev = *tbegin;
    teprev = *tend;
    idprev = *id;
    return 0;
/* $Procedure PRINSR (Reset saved variables) */

L_prinsr:
/* $ Abstract */

/*     This entry point resets saved ID and start and stop time) */
/*     to make sure that CKBRIEF generates table headers correctly. */

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

/*     None. */

/* $ Brief_I/O */

/*     None. */

/* $ Detailed_Input */

/*     None. */

/* $ Detailed_Output */

/*     None. */

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

/* -& */
    tbprev = 0.;
    teprev = 0.;
    idprev = 0;
    return 0;
} /* prinst_ */

/* Subroutine */ int prinst_(integer *id, doublereal *tbegin, doublereal *
	tend, integer *avflag, integer *frame, char *tout, logical *fdsp, 
	logical *tdsp, logical *gdsp, logical *ndsp, ftnlen tout_len)
{
    return prinst_0_(0, id, tbegin, tend, avflag, frame, tout, fdsp, tdsp, 
	    gdsp, ndsp, tout_len);
    }

/* Subroutine */ int prinsr_(void)
{
    return prinst_0_(1, (integer *)0, (doublereal *)0, (doublereal *)0, (
	    integer *)0, (integer *)0, (char *)0, (logical *)0, (logical *)0, 
	    (logical *)0, (logical *)0, (ftnint)0);
    }

