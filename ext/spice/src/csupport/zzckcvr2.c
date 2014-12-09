/* zzckcvr2.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZCKCVR2 ( Private --- C-kernel segment coverage, type 02 ) */
/* Subroutine */ int zzckcvr2_(integer *handle, integer *arrbeg, integer *
	arrend, doublereal *schedl)
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1;

    /* Builtin functions */
    integer i_dnnt(doublereal *), s_rnge(char *, integer, char *, integer);

    /* Local variables */
    integer nrec;
    doublereal last[100];
    integer i__, begat, endat;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    doublereal first[100];
    extern /* Subroutine */ int dafgda_(integer *, integer *, integer *, 
	    doublereal *), chkout_(char *, ftnlen), wninsd_(doublereal *, 
	    doublereal *, doublereal *);
    integer arrsiz;
    extern logical return_(void);
    integer get, got;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Determine the "window" of coverage of a type 02 C-kernel segment. */

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

/*     CK */
/*     DAF */

/* $ Keywords */

/*     CK */
/*     UTILITY */
/*     PRIVATE */

/* $ Declarations */
/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of a C-kernel open for read access */
/*     ARRBEG     I   Beginning DAF address */
/*     ARREND     I   Ending DAF address */
/*     SCHEDL    I/O  An initialized window/schedule of interval */

/* $ Detailed_Input */

/*     HANDLE     is the handle of some DAF that is open for reading. */

/*     ARRBEG     is the beginning address of a type 02 segment */

/*     ARREND     is the ending address of a type 02 segment. */

/*     SCHEDL     is a schedule (window) of intervals, to which the */
/*                intervals of coverage for this segment will be added. */

/* $ Detailed_Output */

/*     SCHEDL     the input schedule updated to include the intervals */
/*                of coverage for this segment. */

/* $ Parameters */

/*     None. */

/* $ Files */

/*     This routine reads the contents of the file associated with */
/*     HANDLE to locate coverage intervals. */

/* $ Exceptions */

/*     Routines in the call tree of this routine may signal errors */
/*     if insufficient room in SCHEDL exists or other error */
/*     conditions relating to file access arise. */

/* $ Particulars */

/*     This is a utility routine that determines the intervals */
/*     of coverage for a type 02 C-kernel segment. */

/* $ Examples */

/*     None. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */
/*     F.S. Turner     (JPL) */
/*     B.V. Semenov    (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SUPPORT Version 2.1.0, 13-FEB-2003 (BVS) */

/*        Replaced MAX with MIN in the assignment of GET. This bug */
/*        caused the routine either to look beyond the end of the */
/*        start/stop time blocks of the segment (for NREC < BSIZE) or to */
/*        attempt to fill in internal buffers with more data than they */
/*        were declared to hold (for NREC > BSIZE.) */

/* -    SUPPORT Version 2.0.0, 27-AUG-2002 (FST) */

/*        Updated this routine to use DAFGDA instead of DAFRDA. */
/*        This allows the module to process non-native kernels. */

/*        Header and code clean up for delivery to SUPPORT. */

/* -    SUPPORT Version 1.0.0, 14-Feb-2000 (WLT) */

/*        Happy Valentine's Day. */

/* -& */

/*     SPICELIB Functions */


/*     Local Parameters */


/*     Local Variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZCKCVR2", (ftnlen)8);
    }

/*     Determine the size of the array and the number of records */
/*     in it. */

    arrsiz = *arrend - *arrbeg + 1;
    d__1 = ((doublereal) arrsiz * 100. + 1.) / 1001.;
    nrec = i_dnnt(&d__1);

/*     The variable GOT tells us how many time endpoints we've */
/*     gotten so far. */

    got = 0;
    while(got < nrec) {
/* Computing MIN */
	i__1 = 100, i__2 = nrec - got;
	get = min(i__1,i__2);
	begat = *arrbeg + (nrec << 3) + got;
	endat = *arrbeg + (nrec << 3) + nrec + got;

/*        Retrieve the list next list of windows. */

	i__1 = begat + get - 1;
	dafgda_(handle, &begat, &i__1, first);
	i__1 = endat + get - 1;
	dafgda_(handle, &endat, &i__1, last);

/*        Insert the coverage intervals into the schedule. */

	i__1 = get;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    wninsd_(&first[(i__2 = i__ - 1) < 100 && 0 <= i__2 ? i__2 : 
		    s_rnge("first", i__2, "zzckcvr2_", (ftnlen)214)], &last[(
		    i__3 = i__ - 1) < 100 && 0 <= i__3 ? i__3 : s_rnge("last",
		     i__3, "zzckcvr2_", (ftnlen)214)], schedl);
	}
	got += get;
    }
    chkout_("ZZCKCVR2", (ftnlen)8);
    return 0;
} /* zzckcvr2_ */

