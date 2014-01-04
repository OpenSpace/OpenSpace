/* zzckcvr5.f -- translated by f2c (version 19980913).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* $Procedure ZZCKCVR5 ( Private --- C-kernel segment coverage, type 05 ) */
/* Subroutine */ int zzckcvr5_(integer *handle, integer *arrbeg, integer *
	arrend, doublereal *schedl)
{
    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    integer i_dnnt(doublereal *);

    /* Local variables */
    integer nrec;
    doublereal tick;
    integer ndir;
    doublereal begin;
    extern /* Subroutine */ int chkin_(char *, ftnlen);
    integer intat, invls, rsize;
    doublereal start;
    extern /* Subroutine */ int dafgda_(integer *, integer *, integer *, 
	    doublereal *);
    integer intbeg;
    doublereal buffer[4];
    integer tickat;
    doublereal finish;
    extern /* Subroutine */ int sigerr_(char *, ftnlen), chkout_(char *, 
	    ftnlen), setmsg_(char *, ftnlen), errint_(char *, doublereal *, 
	    ftnlen), wninsd_(doublereal *, doublereal *, doublereal *);
    integer lsttik, lstint;
    extern logical return_(void);
    integer subtyp;

/* $ Abstract */

/*     SPICE Private routine intended solely for the support of SPICE */
/*     routines.  Users should not call this routine directly due */
/*     to the volatile nature of this routine. */

/*     Determine the "window" of coverage of a type 05 C-kernel segment. */

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
/* $ Abstract */

/*     Declare parameters specific to CK type 05. */

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

/* $ Keywords */

/*     CK */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     N.J. Bachman      (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 20-AUG-2002 (NJB) */

/* -& */

/*     CK type 5 subtype codes: */


/*     Subtype 0:  Hermite interpolation, 8-element packets. Quaternion */
/*                 and quaternion derivatives only, no angular velocity */
/*                 vector provided. Quaternion elements are listed */
/*                 first, followed by derivatives. Angular velocity is */
/*                 derived from the quaternions and quaternion */
/*                 derivatives. */


/*     Subtype 1:  Lagrange interpolation, 4-element packets. Quaternion */
/*                 only. Angular velocity is derived by differentiating */
/*                 the interpolating polynomials. */


/*     Subtype 2:  Hermite interpolation, 14-element packets. */
/*                 Quaternion and angular angular velocity vector, as */
/*                 well as derivatives of each, are provided. The */
/*                 quaternion comes first, then quaternion derivatives, */
/*                 then angular velocity and its derivatives. */


/*     Subtype 3:  Lagrange interpolation, 7-element packets. Quaternion */
/*                 and angular velocity vector provided.  The quaternion */
/*                 comes first. */


/*     Packet sizes associated with the various subtypes: */


/*     End of file ck05.inc. */

/* $ Brief_I/O */

/*     VARIABLE  I/O  DESCRIPTION */
/*     --------  ---  -------------------------------------------------- */
/*     HANDLE     I   Handle of a C-kernel open for read access */
/*     ARRBEG     I   Beginning DAF address */
/*     ARREND     I   Ending DAF address */
/*     SCHEDL    I/O  An initialized window/schedule of interval */

/* $ Detailed_Input */

/*     HANDLE     is the handle of some DAF that is open for reading. */

/*     ARRBEG     is the beginning address of a type 05 segment */

/*     ARREND     is the ending address of a type 05 segment. */

/*     SCHEDL     is a schedule (window) of intervals, to which the */
/*                intervals of coverage for this segment will be added. */

/* $ Detailed_Output */

/*     SCHEDL     the input schedule updated to include the intervals */
/*                of coverage for this segment. */

/* $ Parameters */

/*     Several parameters associated with the type 05 C-kernel */
/*     are utilized to compute the packet size of each subtype. */
/*     See the include file 'ck05.inc' for details. */

/* $ Files */

/*     This routine reads the contents of the file associated with */
/*     HANDLE to locate coverage intervals. */

/* $ Exceptions */

/*     1) The error SPICE(NOTSUPPORTED) is signaled if the subtype */
/*        of the CK type 05 segment is not recognized. */

/*     2) Routines in the call tree of this routine may signal errors */
/*        if insufficient room in SCHEDL exists or other error */
/*        conditions relating to file access arise. */

/* $ Particulars */

/*     This is a utility routine that determines the intervals */
/*     of coverage for a type 05 C-kernel segment. */

/* $ Examples */

/*     See CKBRIEF's main driver. */

/* $ Restrictions */

/*     None. */

/* $ Author_and_Institution */

/*     W.L. Taber      (JPL) */
/*     F.S. Turner     (JPL) */

/* $ Literature_References */

/*     None. */

/* $ Version */

/* -    SPICELIB Version 1.0.0, 28-AUG-2002 (FST) */

/* -& */

/*     SPICELIB Functions */


/*     Local Variables */


/*     Standard SPICE error handling. */

    if (return_()) {
	return 0;
    } else {
	chkin_("ZZCKCVR5", (ftnlen)8);
    }

/*     Get the meta-data associated with this segment that we */
/*     require to produce the schedule. */

/*     BUFFER(1) = Subtype Code */
/*     BUFFER(2) = Window Size */
/*     BUFFER(3) = Number of Interpolation Intervals */
/*     BUFFER(4) = Number of Packets */

    i__1 = *arrend - 3;
    dafgda_(handle, &i__1, arrend, buffer);
    subtyp = i_dnnt(buffer);
    invls = i_dnnt(&buffer[2]);
    nrec = i_dnnt(&buffer[3]);
    ndir = (nrec - 1) / 100;

/*     Compute the packet size.  This requires parameters listed */
/*     in the include file 'ck05.inc' and is based on the subtype. */

    if (subtyp == 0) {
	rsize = 8;
    } else if (subtyp == 1) {
	rsize = 4;
    } else if (subtyp == 2) {
	rsize = 14;
    } else if (subtyp == 3) {
	rsize = 7;
    } else {
	setmsg_("CK type 5 subtype <#> is not supported.", (ftnlen)39);
	errint_("#", buffer, (ftnlen)1);
	sigerr_("SPICE(NOTSUPPORTED)", (ftnlen)19);
	chkout_("ZZCKCVR5", (ftnlen)8);
	return 0;
    }

/*     Recall that the segment is layed out as: */


/*       +------------------------------+ */
/*       |                              | */
/*       |  Pointing                    | */
/*       |                              | */
/*       +------------------------------+ */
/*       |                        | */
/*       |  SCLK times            | */
/*       |                        | */
/*       +------------------------+ */
/*       |                        | */
/*       |  SCLK directory        | */
/*       |                        | */
/*       +------------------------+ */
/*       |                        | */
/*       |  Interval start times  | */
/*       |                        | */
/*       +------------------------+ */
/*       |                        | */
/*       |  Start times directory | */
/*       |                        | */
/*       +------------------------+ */
/*       |    Seconds per tick    | */
/*       +------------------------+ */
/*       |      Subtype code      | */
/*       +------------------------+ */
/*       |      Window size       | */
/*       +------------------------+ */
/*       |                        | */
/*       |  Number of intervals   | */
/*       |                        | */
/*       +------------------------+ */
/*       |                        | */
/*       |  Number of pointing    | */
/*       |      instances         | */
/*       |                        | */
/*       +------------------------+ */

    tickat = *arrbeg + rsize * nrec;
    lsttik = tickat + nrec - 1;
    intbeg = *arrbeg + rsize * nrec + nrec + ndir;
    intat = intbeg;
    lstint = intbeg + invls - 1;
    dafgda_(handle, &intat, &intat, &start);
    dafgda_(handle, &tickat, &tickat, &tick);
    while(tick < start && tickat < lsttik) {
	++tickat;
	dafgda_(handle, &tickat, &tickat, &tick);
    }

/*     If we did not find a TICK at least as big as START, we can */
/*     just return now. */

    if (tick < start) {
	chkout_("ZZCKCVR5", (ftnlen)8);
	return 0;
    }
    while(intat <= lstint && tickat <= lsttik) {

/*        At this point, we have an interval that begins at START */
/*        and ends at FINISH (unless of course we never found a "good" */
/*        TICK to start with.) */

	begin = start;

/*        If the the start of the interval was the start of the LAST */
/*        interval available, we can short cut the remainder of the */
/*        reads. */

	if (intat == lstint) {
	    dafgda_(handle, &lsttik, &lsttik, &finish);
	    wninsd_(&start, &finish, schedl);
	    chkout_("ZZCKCVR5", (ftnlen)8);
	    return 0;
	}

/*        This is the expected case.  Get the start of the next */
/*        interval. */

	++intat;
	dafgda_(handle, &intat, &intat, &start);

/*        Read forward from the last tick until we reach the */
/*        START of the next interval or until we run out of TICKS. */

	while(tick < start && tickat < lsttik) {
	    finish = tick;
	    ++tickat;
	    dafgda_(handle, &tickat, &tickat, &tick);
	}

/*        A structurally correct CK-5 segment should never allow */
/*        the next test to pass, but it's just easier to check than */
/*        police the writers of C-kernels.  The only way to get into */
/*        the block below is if TICKAT .EQ. LSTTIK */

	if (tick < start) {
	    finish = tick;
	    ++tickat;
	}

/*        Insert the interval into the window. */

	wninsd_(&begin, &finish, schedl);
    }
    chkout_("ZZCKCVR5", (ftnlen)8);
    return 0;
} /* zzckcvr5_ */

